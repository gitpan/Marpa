package Marpa::Recognizer;

use 5.010;
use warnings;
no warnings 'recursion';
use strict;
use integer;

use English qw( -no_match_vars );

use Marpa::Internal;

# Elements of the EARLEY ITEM structure
# Note that these are Earley items as modified by Aycock & Horspool, with QDFA states instead of
# LR(0) items.

use Marpa::Offset qw(

    :package=Marpa::Internal::Earley_Item

    NAME STATE TOKENS LINKS
    =LAST_EVALUATOR_FIELD

    PARENT SET
    =LAST_FIELD

);

# We don't prune the Earley items because we want PARENT and SET
# around for debugging

# NAME   - unique string describing Earley item
# STATE  - the QDFA state
# PARENT - the number of the Earley set with the parent item(s)
# TOKENS - a list of the links from token scanning
# LINKS  - a list of the links from the completer step
# SET    - the set this item is in, for debugging

# Elements of the RECOGNIZER structure
use Marpa::Offset qw(

    :package=Marpa::Internal::Recognizer

    GRAMMAR
    EARLEY_SETS
    FURTHEST_EARLEME
    LAST_COMPLETED_EARLEME
    TOKENS_BY_EARLEME

    =LAST_EVALUATOR_FIELD

    WANTED
    CURRENT_TERMINALS
    EARLEY_HASH
    EXHAUSTED
    TERMINALS_BY_STATE
    CURRENT_EARLEME
    TOKEN_HASHES_BY_EARLEME

);

# GRAMMAR            - the grammar used
# EARLEY_SETS        - the array of the Earley sets
# EARLEY_HASH        - hash of the Earley items
#                      to build the Earley sets
# FURTHEST_EARLEME   - last earley set with a token
# EXHAUSTED          - parse can't continue?
# EVALUATOR          - the current evaluator for this recognizer
# TERMINALS_BY_STATE - an array, indexed by QDFA state id,
#                      of the terminals belonging in it

package Marpa::Internal::Recognizer;

use Marpa::Internal;

# use Smart::Comments '-ENV';

### Using smart comments <where>...

use Data::Dumper;
use Storable;
use English qw( -no_match_vars );

use constant EARLEME_MASK => ~(0x7fffffff);

my $parse_number = 0;

# Returns the new parse object or throws an exception
sub Marpa::Recognizer::new {
    my $class = shift;
    my $args  = shift;

    my $arg_trace_fh = $args->{trace_file_handle};

    my $self = bless [], $class;

    my $clone_arg = $args->{clone};
    delete $args->{clone};
    my $clone = $clone_arg // 1;

    my $grammar = $args->{grammar};
    if ( not defined $grammar ) {
        my $stringified_grammar = $args->{stringified_grammar};
        Marpa::exception('No grammar specified')
            if not defined $stringified_grammar;
        delete $args->{stringified_grammar};
        my $trace_fh = $arg_trace_fh // (*STDERR);
        $grammar =
            Marpa::Grammar::unstringify( $stringified_grammar, $trace_fh );
        $clone = 0;
    } ## end if ( not defined $grammar )
    else {
        delete $args->{grammar};
    }

    my $grammar_class = ref $grammar;
    Marpa::exception(
        "${class}::new() grammar arg has wrong class: $grammar_class")
        if not $grammar_class eq 'Marpa::Grammar';

    my $tracing = $grammar->[Marpa::Internal::Grammar::TRACING];

    my $problems = $grammar->[Marpa::Internal::Grammar::PROBLEMS];
    if ($problems) {
        Marpa::exception(
            Marpa::Grammar::show_problems($grammar),
            "Attempt to parse grammar with fatal problems\n",
            'Marpa cannot proceed',
        );
    } ## end if ($problems)

    if ( $grammar->[Marpa::Internal::Grammar::ACADEMIC] ) {
        Marpa::exception( "Attempt to parse grammar marked academic\n",
            'Marpa cannot proceed' );
    }

    my $phase = $grammar->[Marpa::Internal::Grammar::PHASE];
    if ( $phase != Marpa::Internal::Phase::PRECOMPUTED ) {
        Marpa::exception(
            'Attempt to parse grammar in inappropriate phase ',
            Marpa::Internal::Phase::description($phase)
        );
    } ## end if ( $phase != Marpa::Internal::Phase::PRECOMPUTED )

    if ($clone) {
        $grammar = $grammar->clone($arg_trace_fh);
        delete $args->{trace_file_handle};
    }

    # options are not set until *AFTER* the grammar is cloned
    Marpa::Grammar::set( $grammar, $args );

    # Pull lookup of terminal flag by symbol ID out of the loop
    # over the QDFA transitions
    my $symbols = $grammar->[Marpa::Internal::Grammar::SYMBOLS];
    my @terminal_ids =
        map  { $_->[Marpa::Internal::Symbol::ID] }
        grep { $_->[Marpa::Internal::Symbol::TERMINAL] } @{$symbols};
    my @terminals_by_id;
    @terminals_by_id[@terminal_ids] = (1) x scalar @terminal_ids;

    my $QDFA        = $grammar->[Marpa::Internal::Grammar::QDFA];
    my $symbol_hash = $grammar->[Marpa::Internal::Grammar::SYMBOL_HASH];
    my @terminals_by_state;
    $#terminals_by_state = $#{$QDFA};

    for my $state ( @{$QDFA} ) {
        my ( $id, $transition ) =
            @{$state}[ Marpa::Internal::QDFA::ID,
            Marpa::Internal::QDFA::TRANSITION, ];
        $terminals_by_state[$id] = [
            grep    { $terminals_by_id[$_] }
                map { $symbol_hash->{$_} }
                keys %{$transition}
        ];
    } ## end for my $state ( @{$QDFA} )

    $self->[Marpa::Internal::Recognizer::TERMINALS_BY_STATE] =
        \@terminals_by_state;

    $grammar->[Marpa::Internal::Grammar::PHASE] =
        Marpa::Internal::Phase::RECOGNIZING;

    my $earley_hash;
    my $earley_set;

    my $start_states = $grammar->[Marpa::Internal::Grammar::START_STATES];
    my %wanted       = ();

    for my $state ( @{$start_states} ) {
        my $state_id = $state->[Marpa::Internal::QDFA::ID];
        my $name     = sprintf
            ## no critic (ValuesAndExpressions::RequireInterpolationOfMetachars)
            'S%d@%d-%d',
            ## use critic
            $state_id, 0, 0;

        my $item;
        $item->[Marpa::Internal::Earley_Item::NAME]   = $name;
        $item->[Marpa::Internal::Earley_Item::STATE]  = $state;
        $item->[Marpa::Internal::Earley_Item::PARENT] = 0;
        $item->[Marpa::Internal::Earley_Item::TOKENS] = [];
        $item->[Marpa::Internal::Earley_Item::LINKS]  = [];
        $item->[Marpa::Internal::Earley_Item::SET]    = 0;

        push @{$earley_set}, $item;
        $earley_hash->{$name} = $item;
        while (
            my ( $wanted_symbol_name, $next_states ) = each %{
                $QDFA->[ $state->[Marpa::Internal::QDFA::ID] ]
                    ->[Marpa::Internal::QDFA::TRANSITION]
            }
            )
        {
            push @{ $wanted{ $wanted_symbol_name . q{@} . '0' } },
                [ $item, $next_states ];
        } ## end while ( my ( $wanted_symbol_name, $next_states ) = each...)

    } ## end for my $state ( @{$start_states} )

    $self->[Marpa::Internal::Recognizer::EARLEY_HASH] = $earley_hash;
    $self->[Marpa::Internal::Recognizer::GRAMMAR]     = $grammar;
    $self->[Marpa::Internal::Recognizer::EARLEY_SETS] = [$earley_set];
    $self->[Marpa::Internal::Recognizer::TOKEN_HASHES_BY_EARLEME] = [];
    $self->[Marpa::Internal::Recognizer::TOKENS_BY_EARLEME]       = [];
    $self->[Marpa::Internal::Recognizer::WANTED]                  = \%wanted;

    $self->[Marpa::Internal::Recognizer::LAST_COMPLETED_EARLEME] = -1;
    $self->[Marpa::Internal::Recognizer::CURRENT_EARLEME] =
        $self->[Marpa::Internal::Recognizer::FURTHEST_EARLEME] = 0;

    Marpa::Recognizer::tokens( $self, 'predict', 'absolute', 0 );

    return $self;
} ## end sub Marpa::Recognizer::new

sub Marpa::Recognizer::check_terminal {
    my ( $recce, $name ) = @_;
    my $grammar = $recce->[Marpa::Internal::Recognizer::GRAMMAR];
    return $grammar->check_terminal($name);
}

sub Marpa::Recognizer::status {
    my ($recce) = @_;
    my $exhausted = $recce->[Marpa::Internal::Recognizer::EXHAUSTED];
    return if $exhausted;
    my $current_earleme =
        $recce->[Marpa::Internal::Recognizer::CURRENT_EARLEME];
    return ( $current_earleme,
        $recce->[Marpa::Internal::Recognizer::CURRENT_TERMINALS] )
        if wantarray;
    return $current_earleme;
} ## end sub Marpa::Recognizer::status

# Convert Recognizer into string form
#
sub Marpa::Recognizer::stringify {
    my $recce   = shift;
    my $grammar = $recce->[Marpa::Internal::Recognizer::GRAMMAR];

    my $tracing = $grammar->[Marpa::Internal::Grammar::TRACING];
    my $trace_fh;
    if ($tracing) {
        $trace_fh = $grammar->[Marpa::Internal::Grammar::TRACE_FILE_HANDLE];
    }
    $grammar->[Marpa::Internal::Grammar::TRACE_FILE_HANDLE] = undef;

    # returns a ref -- dumps can be long
    return \Storable::freeze($recce);

} ## end sub Marpa::Recognizer::stringify

# First arg is stringified recognizer
# Second arg (optional) is trace file handle, either saved and restored
# If not trace file handle supplied, it reverts to the default, STDERR
#
# Returns the unstringified recognizer
sub Marpa::Recognizer::unstringify {
    my $stringified_recce = shift;
    my $trace_fh          = shift;
    $trace_fh //= *STDERR;

    Marpa::exception('Attempt to unstringify undefined recognizer')
        if not defined $stringified_recce;
    Marpa::exception('Arg to unstringify must be ref to SCALAR')
        if ref $stringified_recce ne 'SCALAR';

    my $recce = Storable::unfreeze($stringified_recce);

    my $grammar = $recce->[Marpa::Internal::Recognizer::GRAMMAR];
    $grammar->[Marpa::Internal::Grammar::TRACE_FILE_HANDLE] = $trace_fh;

    return $recce;

} ## end sub Marpa::Recognizer::unstringify

sub Marpa::Recognizer::clone {
    my $recce    = shift;
    my $trace_fh = shift;

    my $grammar = $recce->[Marpa::Internal::Recognizer::GRAMMAR];
    $trace_fh //= $grammar->[Marpa::Internal::Grammar::TRACE_FILE_HANDLE];

    $grammar->[Marpa::Internal::Grammar::TRACE_FILE_HANDLE] = undef;
    my $cloned_recce = Storable::dclone($recce);
    my $cloned_grammar =
        $cloned_recce->[Marpa::Internal::Recognizer::GRAMMAR];
    $cloned_grammar->[Marpa::Internal::Grammar::TRACE_FILE_HANDLE] =
        $grammar->[Marpa::Internal::Grammar::TRACE_FILE_HANDLE] = $trace_fh;

    return $cloned_recce;

} ## end sub Marpa::Recognizer::clone

# Viewing methods, for debugging

sub Marpa::show_token_choice {
    my ($token) = @_;
    my ( $earley_item, $symbol, $value_ref ) = @{$token};
    my $token_dump = Data::Dumper->new( [$value_ref] )->Terse(1)->Dump;
    chomp $token_dump;
    my $symbol_name      = $symbol->[Marpa::Internal::Symbol::NAME];
    my $earley_item_name = $earley_item->[Marpa::Internal::Earley_Item::NAME];
    return "[p=$earley_item_name; s=$symbol_name; t=$token_dump]";
} ## end sub Marpa::show_token_choice

sub Marpa::show_link_choice {
    my ($link) = @_;
    return
          '[p='
        . $link->[0]->[Marpa::Internal::Earley_Item::NAME] . '; c='
        . $link->[1]->[Marpa::Internal::Earley_Item::NAME] . ']';
} ## end sub Marpa::show_link_choice

sub Marpa::show_earley_item {
    my ($item) = @_;
    my $tokens = $item->[Marpa::Internal::Earley_Item::TOKENS];
    my $links  = $item->[Marpa::Internal::Earley_Item::LINKS];
    my $text   = $item->[Marpa::Internal::Earley_Item::NAME];

    if ( defined $tokens and @{$tokens} ) {
        for my $token ( @{$tokens} ) {
            $text .= q{ } . Marpa::show_token_choice($token);
        }
    }
    if ( defined $links and @{$links} ) {
        for my $link ( @{$links} ) {
            $text .= q{ } . Marpa::show_link_choice($link);
        }
    }
    return $text;
} ## end sub Marpa::show_earley_item

sub Marpa::show_earley_set {
    my ($earley_set) = @_;
    my $text = q{};
    for my $earley_item ( @{$earley_set} ) {
        $text .= Marpa::show_earley_item($earley_item) . "\n";
    }
    return $text;
} ## end sub Marpa::show_earley_set

sub Marpa::show_earley_set_list {
    my ($earley_set_list) = @_;
    my $text              = q{};
    my $earley_set_count  = @{$earley_set_list};
    LIST: for my $ix ( 0 .. $earley_set_count - 1 ) {
        my $set = $earley_set_list->[$ix];
        next LIST if not defined $set;
        $text .= "Earley Set $ix\n" . Marpa::show_earley_set($set);
    }
    return $text;
} ## end sub Marpa::show_earley_set_list

sub Marpa::Recognizer::show_earley_sets {
    my ($recce) = @_;
    my $current_earleme = $recce->[CURRENT_EARLEME] // 'stripped';
    my $last_completed_earleme = $recce->[LAST_COMPLETED_EARLEME]
        // 'stripped';
    my $furthest_earleme = $recce->[FURTHEST_EARLEME];
    my $earley_set_list  = $recce->[EARLEY_SETS];

    return
        "Current Earley Set: $current_earleme; Last Completed: $last_completed_earleme; Furthest: $furthest_earleme\n"
        . Marpa::show_earley_set_list($earley_set_list);

} ## end sub Marpa::Recognizer::show_earley_sets

## no critic (Subroutines::RequireArgUnpacking)
sub Marpa::Recognizer::tokens {

    ## use critic

    ## no critic (ControlStructures::ProhibitDeepNests)

    # check class of parse?
    my $recce = shift;
    Marpa::exception('No recognizer object for token call')
        if not defined $recce
            or ref $recce ne 'Marpa::Recognizer';

    my $grammar  = $recce->[Marpa::Internal::Recognizer::GRAMMAR];
    my $phase    = $grammar->[Marpa::Internal::Grammar::PHASE];
    my $trace_fh = $grammar->[Marpa::Internal::Grammar::TRACE_FILE_HANDLE];
    my $too_many_earley_items =
        $grammar->[Marpa::Internal::Grammar::TOO_MANY_EARLEY_ITEMS];

    my $tokens;
    my $predict_earleme;
    my $predict_offset_is_absolute = 0;
    my $predict_flag               = 1;
    my $continue_earleme;
    my $continue_offset_is_absolute = 0;
    my $continue_flag               = 0;

    while ( my $arg = shift @_ ) {
        given ($arg) {
            when ( ref $_ eq 'ARRAY' ) {
                Marpa::exception('More than one tokens arg') if $tokens;
                $tokens = $_;
            }
            when ('predict') {
                Marpa::exception('More than one predict arg')
                    if defined $predict_earleme;
                while ( not defined $predict_earleme ) {
                    given ( shift @_ ) {
                        when (undef) { $predict_earleme = 0 }
                        when ('absolute') {
                            $predict_offset_is_absolute = 1
                        }
                        when ( Scalar::Util::looks_like_number($_) ) {
                            $predict_earleme = $_;
                        }
                        default {
                            Marpa::exception("Bad offset for predict: $_")
                        }
                    } ## end given
                } ## end while ( not defined $predict_earleme )
            } ## end when ('predict')
            when ('continue') {
                Marpa::exception('More than one continue arg')
                    if defined $continue_earleme;
                while ( not defined $continue_earleme ) {
                    given ( shift @_ ) {
                        when (undef)      { $continue_earleme            = 0 }
                        when ('absolute') { $continue_offset_is_absolute = 1 }
                        when ( Scalar::Util::looks_like_number($_) ) {
                            $continue_earleme = $_;
                        }
                        default {
                            Marpa::exception("Bad offset for continue $_")
                        }
                    } ## end given
                } ## end while ( not defined $continue_earleme )
            } ## end when ('continue')
        } ## end given
    } ## end while ( my $arg = shift @_ )

    Marpa::exception('continue and predict options are mutually exclusive')
        if defined $continue_earleme and defined $predict_earleme;

    $tokens //= [];

    Marpa::exception('Attempt to scan tokens after parsing was exhausted')
        if $recce->[Marpa::Internal::Recognizer::EXHAUSTED]
            and scalar @{$tokens};

    # TOKEN PROCESSING PHASE

    my $symbols     = $grammar->[Marpa::Internal::Grammar::SYMBOLS];
    my $symbol_hash = $grammar->[Marpa::Internal::Grammar::SYMBOL_HASH];

    my $next_token_earleme =
        $recce->[Marpa::Internal::Recognizer::CURRENT_EARLEME];
    my $last_completed_earleme =
        $recce->[Marpa::Internal::Recognizer::LAST_COMPLETED_EARLEME];
    my $furthest_earleme =
        $recce->[Marpa::Internal::Recognizer::FURTHEST_EARLEME];
    my $tokens_by_earleme =
        $recce->[Marpa::Internal::Recognizer::TOKENS_BY_EARLEME];

    TOKEN: for my $token ( @{$tokens} ) {
        my ( $symbol_name, $value, $length, $offset ) = @{$token};

        my $current_token_earleme = $next_token_earleme;

        Marpa::exception(
            "Attempt to add token '$symbol_name' at location where processing is complete:\n",
            "  Add attempted at $current_token_earleme\n",
            "  Processing complete to $last_completed_earleme\n"
        ) if $current_token_earleme < $last_completed_earleme;
        my $token_id = $symbol_hash->{$symbol_name};

        if ( not defined $token_id ) {
            Marpa::exception( 'Unknown symbol '
                    . ( defined $symbol_name ? "$symbol_name " : q{} )
                    . 'used as token' );
        }

        my $token = $symbols->[$token_id];

        # Make sure it's an allowed terminal symbol.
        if ( not $token->[Marpa::Internal::Symbol::TERMINAL] ) {
            Marpa::exception( 'Non-terminal '
                    . ( defined $symbol_name ? "$symbol_name " : q{} )
                    . 'supplied as token' );
        }

        my $value_ref = \($value);

        given ($length) {
            when (undef) { $length = 1; }
            when ( $_ & Marpa::Internal::Recognizer::EARLEME_MASK ) {
                Marpa::exception(
                    'Token '
                        . $token->[Marpa::Internal::Symbol::NAME]
                        . " is too long\n",
                    "  Token starts at $last_completed_earleme, and its length is $length\n"
                    )
            } ## end when ( $_ & Marpa::Internal::Recognizer::EARLEME_MASK )
            when ( $_ <= 0 ) {
                Marpa::exception( 'Token '
                        . $token->[Marpa::Internal::Symbol::NAME]
                        . ' has non-positive length '
                        . $length );
            } ## end when ( $_ <= 0 )
        } ## end given

        my $end_earleme = $current_token_earleme + $length;

        Marpa::exception(
            'Token '
                . $token->[Marpa::Internal::Symbol::NAME]
                . " make parse too long\n",
            "  Token starts at $last_completed_earleme, and its length is $length\n"
        ) if $end_earleme & Marpa::Internal::Recognizer::EARLEME_MASK;

        if ( $end_earleme > $furthest_earleme ) {
            $furthest_earleme = $end_earleme;
        }

        $offset //= 1;
        $next_token_earleme += $offset;

        my $token_entry = [ $token, $value_ref, $length ];

        # This logic is arranged so that non-overlapping tokens do not incur the cost
        # of the checks for duplicates
        my $tokens_here = $tokens_by_earleme->[$current_token_earleme];
        if ( not $tokens_here ) {
            $tokens_by_earleme->[$current_token_earleme] = [$token_entry];
            next TOKEN;
        }

        my $token_hashes_by_earleme =
            $recce->[Marpa::Internal::Recognizer::TOKEN_HASHES_BY_EARLEME];
        my $token_hash_here =
            $token_hashes_by_earleme->[$current_token_earleme];

        if ( not $token_hash_here ) {
            $token_hashes_by_earleme->[$current_token_earleme] =
                $token_hash_here =
                { map { ( join q{;}, @{$_}[ 0, 2 ] ) => 1 } @{$tokens_here} };
        }

        my $hash_key = join q{;}, $token, $length;
        Marpa::exception( $token->[Marpa::Internal::Symbol::NAME],
            " already exists with length $length at location $current_token_earleme"
        ) if $token_hash_here->{$hash_key};

        $token_hash_here->{$hash_key} = 1;
        push @{$tokens_here}, $token_entry;

    } ## end for my $token ( @{$tokens} )

    my $current_earleme =
        $recce->[Marpa::Internal::Recognizer::CURRENT_EARLEME] =
        $next_token_earleme;
    $recce->[Marpa::Internal::Recognizer::FURTHEST_EARLEME] =
        $furthest_earleme;

    if ( defined $continue_earleme ) {
        $current_earleme =
              $continue_offset_is_absolute
            ? $continue_earleme
            : $current_earleme + $continue_earleme;
        return $recce->[Marpa::Internal::Recognizer::CURRENT_EARLEME] =
            $current_earleme;
    } ## end if ( defined $continue_earleme )

    my $furthest_earleme_to_complete =
          defined $predict_earleme
        ? $predict_offset_is_absolute
            ? $predict_earleme
            : $current_earleme + $predict_earleme
        : $furthest_earleme;

    $recce->[Marpa::Internal::Recognizer::CURRENT_EARLEME] =
        $current_earleme = $furthest_earleme_to_complete;

    my $terminals_by_state =
        $recce->[Marpa::Internal::Recognizer::TERMINALS_BY_STATE];
    my $earley_set_list = $recce->[Marpa::Internal::Recognizer::EARLEY_SETS];
    my $earley_hash     = $recce->[Marpa::Internal::Recognizer::EARLEY_HASH];
    my $QDFA            = $grammar->[Marpa::Internal::Grammar::QDFA];
    my $wanted          = $recce->[Marpa::Internal::Recognizer::WANTED];
    my $current_terminals;

    COMPLETION: while (1) {

        # ================
        # === SCANNING ===
        # ================

        my $tokens_here = $tokens_by_earleme->[$last_completed_earleme] // [];

        my $earley_set = $earley_set_list->[$last_completed_earleme];

        if ( not defined $earley_set ) {

            $earley_set_list->[$last_completed_earleme] = [];
            return 1;
        }

        # Important: more earley sets can be added in the loop
        my $earley_set_ix = -1;
        EARLEY_ITEM: while (1) {

            my $earley_item = $earley_set->[ ++$earley_set_ix ];
            last EARLEY_ITEM if not defined $earley_item;

            my ( $state, $parent ) = @{$earley_item}[
                Marpa::Internal::Earley_Item::STATE,
                Marpa::Internal::Earley_Item::PARENT
            ];

            ALTERNATIVE: for my $alternative ( @{$tokens_here} ) {
                my ( $token, $value_ref, $length ) = @{$alternative};

                # compute goto(state, token_name)
                my $states =
                    $QDFA->[ $state->[Marpa::Internal::QDFA::ID] ]
                    ->[Marpa::Internal::QDFA::TRANSITION]
                    ->{ $token->[Marpa::Internal::Symbol::NAME] };
                next ALTERNATIVE if not $states;

                # Create the kernel item and its link.
                my $target_ix = $last_completed_earleme + $length;

                my $target_set = ( $earley_set_list->[$target_ix] //= [] );
                STATE: for my $state ( @{$states} ) {
                    my $reset = $state->[Marpa::Internal::QDFA::RESET_ORIGIN];
                    my $origin   = $reset ? $target_ix : $parent;
                    my $state_id = $state->[Marpa::Internal::QDFA::ID];
                    my $name     = sprintf
                        ## no critic (ValuesAndExpressions::RequireInterpolationOfMetachars)
                        'S%d@%d-%d',
                        ## use critic
                        $state_id, $origin, $target_ix;

                    my $target_item = $earley_hash->{$name};
                    if ( not defined $target_item ) {
                        $target_item = [];
                        $target_item->[Marpa::Internal::Earley_Item::NAME] =
                            $name;
                        $target_item->[Marpa::Internal::Earley_Item::STATE] =
                            $state;
                        $target_item->[Marpa::Internal::Earley_Item::PARENT] =
                            $origin;
                        $target_item->[Marpa::Internal::Earley_Item::LINKS] =
                            [];
                        $target_item->[Marpa::Internal::Earley_Item::TOKENS] =
                            [];
                        $target_item->[Marpa::Internal::Earley_Item::SET] =
                            $target_ix;
                        $earley_hash->{$name} = $target_item;
                        push @{$target_set}, $target_item;

                        while (
                            my ( $wanted_symbol_name, $next_states ) = each %{
                                $QDFA->[ $state->[Marpa::Internal::QDFA::ID] ]
                                    ->[Marpa::Internal::QDFA::TRANSITION]
                            }
                            )
                        {
                            push @{
                                $wanted->{
                                    $wanted_symbol_name . q{@} . $target_ix
                                    }
                                },
                                [ $target_item, $next_states ];
                        } ## end while ( my ( $wanted_symbol_name, $next_states ) = ...)
                    } ## end if ( not defined $target_item )

                    next STATE if $reset;

                    push @{ $target_item
                            ->[Marpa::Internal::Earley_Item::TOKENS] },
                        [ $earley_item, $token, $value_ref ];
                }    # for my $state

            }    # ALTERNATIVE

        }    # EARLEY_ITEM

        last COMPLETION
            if $recce->[Marpa::Internal::Recognizer::LAST_COMPLETED_EARLEME]
                >= $furthest_earleme_to_complete;

        $last_completed_earleme =
            ++$recce->[Marpa::Internal::Recognizer::LAST_COMPLETED_EARLEME];

        # ==================
        # === COMPLETING ===
        # ==================

        $earley_set_list->[$last_completed_earleme] //= [];
        $earley_set = $earley_set_list->[$last_completed_earleme];

        my $lexable_seen = [];
        $#{$lexable_seen} = $#{$symbols};

        # Important: more earley sets can be added in the loop
        $earley_set_ix = -1;
        EARLEY_ITEM: while (1) {

            my $earley_item = $earley_set->[ ++$earley_set_ix ];
            last EARLEY_ITEM if not defined $earley_item;

            my ( $state, $parent ) = @{$earley_item}[
                Marpa::Internal::Earley_Item::STATE,
                Marpa::Internal::Earley_Item::PARENT
            ];
            my $state_id = $state->[Marpa::Internal::QDFA::ID];

            for my $lexable ( @{ $terminals_by_state->[$state_id] } ) {
                $lexable_seen->[$lexable] = 1;
            }

            next EARLEY_ITEM if $last_completed_earleme == $parent;

            PARENT_ITEM:
            for my $parent_data (
                map  { @{$_} }
                grep {defined}
                map  { $wanted->{ $_ . q{@} . $parent } }
                @{ $state->[Marpa::Internal::QDFA::COMPLETE_LHS] }
                )
            {
                my ( $parent_item, $states ) = @{$parent_data};
                my $parent_state =
                    $parent_item->[Marpa::Internal::Earley_Item::STATE];

                TRANSITION_STATE:
                for my $transition_state ( @{$states} ) {
                    my $reset = $transition_state
                        ->[Marpa::Internal::QDFA::RESET_ORIGIN];
                    my $origin =
                          $reset
                        ? $last_completed_earleme
                        : $parent_item
                        ->[Marpa::Internal::Earley_Item::PARENT];
                    my $transition_state_id =
                        $transition_state->[Marpa::Internal::QDFA::ID];
                    my $name = sprintf
                        ## no critic (ValuesAndExpressions::RequireInterpolationOfMetachars)
                        'S%d@%d-%d',
                        ## use critic
                        $transition_state_id, $origin,
                        $last_completed_earleme;
                    my $target_item = $earley_hash->{$name};
                    if ( not defined $target_item ) {
                        $target_item = [];
                        @{$target_item}[
                            Marpa::Internal::Earley_Item::NAME,
                            Marpa::Internal::Earley_Item::STATE,
                            Marpa::Internal::Earley_Item::PARENT,
                            Marpa::Internal::Earley_Item::LINKS,
                            Marpa::Internal::Earley_Item::TOKENS,
                            Marpa::Internal::Earley_Item::SET,
                            ]
                            = (
                            $name, $transition_state, $origin, [], [],
                            $last_completed_earleme,
                            );
                        $earley_hash->{$name} = $target_item;
                        push @{$earley_set}, $target_item;

                        while (
                            my ( $wanted_symbol_name, $next_states ) = each %{
                                $QDFA->[
                                    $transition_state
                                    ->[Marpa::Internal::QDFA::ID]
                                    ]->[Marpa::Internal::QDFA::TRANSITION]
                            }
                            )
                        {
                            push @{
                                $wanted->{
                                          $wanted_symbol_name . q{@}
                                        . $last_completed_earleme
                                    }
                                },
                                [ $target_item, $next_states ];
                        } ## end while ( my ( $wanted_symbol_name, $next_states ) = ...)

                    }    # unless defined $target_item
                    next TRANSITION_STATE if $reset;
                    push
                        @{ $target_item->[Marpa::Internal::Earley_Item::LINKS]
                        },
                        [ $parent_item, $earley_item ];
                }    # TRANSITION_STATE

            }    # PARENT_ITEM

        }    # EARLEY_ITEM

        if ( $too_many_earley_items >= 0
            and ( my $item_count = scalar @{$earley_set} )
            >= $too_many_earley_items )
        {
            if ( $grammar->[Marpa::Internal::Grammar::WARNINGS] ) {
                say {$trace_fh}
                    "Very large earley set: $item_count items at location $last_completed_earleme";
            }
        } ## end if ( $too_many_earley_items >= 0 and ( my $item_count...))

        # TODO: Prove that the completion links are UNIQUE
        # Update 2009-Oct-25: Doesn't really matter.
        # I have to remove duplicates anyway
        # because the same rule derivation can result from
        # different states.

        # Smart::Comment: Lexables Predicted: scalar grep { $lexable_seen->[$_] } ( 0 .. $#{$symbols} )

        ### earley set: Marpa'show_earley_set($earley_set)

        $current_terminals = [
            map { $symbols->[$_]->[Marpa::Internal::Symbol::NAME] }
            grep { $lexable_seen->[$_] } ( 0 .. $#{$symbols} )
        ];

    } ## end while (1)

    $recce->[Marpa::Internal::Recognizer::CURRENT_TERMINALS] =
        $current_terminals;

    if ( $last_completed_earleme > $furthest_earleme ) {
        $recce->[Marpa::Internal::Recognizer::EXHAUSTED] = 1;
        return;
    }

    return ( $current_earleme, $current_terminals ) if wantarray;

    return $current_earleme;

} ## end sub Marpa::Recognizer::tokens

1;

__END__

=pod

=head1 NAME

Marpa::Recognizer - Marpa Recognizer Objects

=head1 SYNOPSIS

=begin Marpa::Test::Display:

## next display
in_file($_, 't/equation_s.t');

=end Marpa::Test::Display:

    my $recce = Marpa::Recognizer->new( { grammar => $grammar } );

Z<>

=begin Marpa::Test::Display:

## next 5 displays
in_file($_, 't/equation.t');

=end Marpa::Test::Display:

    my $recce = Marpa::Recognizer->new( { grammar => $grammar } );

    my @tokens = (
        [ $number, 2,    1 ],
        [ $op,     q{-}, 1 ],
        [ $number, 0,    1 ],
        [ $op,     q{*}, 1 ],
        [ $number, 3,    1 ],
        [ $op,     q{+}, 1 ],
        [ $number, 1,    1 ],
    );

    TOKEN: for my $token (@tokens) {
        next TOKEN if $recce->earleme($token);
        Marpa::exception( 'Parsing exhausted at character: ', $token->[1] );
    }

    $recce->end_input();

=head1 DESCRIPTION

Marpa parsing takes place in three major phases: grammar creation, input recognition
and parse evaluation.
Once a grammar object has rules,
a recognizer object can be created from it.
The recognizer accepts input and
can be used to create a Marpa evaluator object.

=head2 Tokens and Earlemes

Marpa allows ambiguous tokens.
Several Marpa tokens can start at a single parsing location.
Marpa tokens can be of various lengths.
Marpa tokens can even overlap.

For most parsers, position is location in a token stream.
To deal with variable-length and overlapping tokens,
Marpa needs a more flexible idea of location.
Marpa's idea of position is location in an B<earleme> stream.
B<Earlemes> are named after Jay Earley, the inventor of the first algorithm
in Marpa's lineage.

While scanning, Marpa keeps track of the B<current earleme>.
Earlemes in an earleme start at earleme 0 and increase numerically.
The earleme immediately following earleme 0 is earleme 1,
the earleme immediately following earleme 1 is earleme 2,
and so on.
The earleme immediately following earleme I<N> is always earleme I<N+1>.

B<Distance> in the earleme stream are what you'd expect.
The distance between earleme I<X> and earleme I<Y> is
the absolute value of the difference between I<X> and I<Y>,
I<|X-Y|>.
The distance from earleme 3 to earleme 6,
for example, is 3 earlemes.

Whenever a token is given to Marpa to be scanned,
it starts at the current earleme.
In addition to the type and value of the token,
Marpa must be told token's B<length> in earlemes.
The length of a Marpa token must be greater than zero.
This earleme length will become
the distance from the start of the
token to the end of the token.

The start of the token is put at the current earleme.
If the length of the token is I<L>,
and the number of the current earleme is I<C>,
the end of the token will be at the earleme number I<C+L>.

=head3 The One-Character-Per-Earleme Model

Many different models of the relationship between tokens and earlemes
are possible, but two are particularly important.
One is the one-token-per-earleme model.
The other is the one-character-per-earleme model.
If you do your lexing with the C<text> method,
you will use a
one-character-per-earleme model.

Using the C<text> method, Marpa receives the input as the series
of strings and string reference.
provided in the one or more calls to the C<text> method.
The B<raw input> can be thought of the concatenation of these
strings,
even though the strings are not physically concatenated.
When the C<text> method is used,
character position in this raw input will 
correspond exactly one-to-one with the earleme position.

Every character will be treated as being exactly one
earleme in length.
Any tokens which are more than one several character in length,
will span earlemes.

It is common, when a one-character-per-earleme model of input is used,
for there to be many earlemes at which no tokens start.
For example,
in a standard implementation
of a grammar for a language which allows
comments,
no tokens will start at
any earlemes which corresponds to character locations inside
a comment.

=head3 Other Models

Marpa is not restricted to the one-character-per-earleme model.
Most parser generators treat location as position in a token stream.
In Marpa, this corresponds to a
one-token-per-earleme model.

If you use the C<earleme> method, you can structure your input in almost any way you like.
There are only four restrictions:

=over 4

=item 1

Scanning always starts at earleme 0.

=item 2

Earleme I<N> is always scanned immediately before earleme I<N+1>.
In other words, the earlemes are scanned one by one in increasing numerical order.

=item 3

When an earleme is scanned, all tokens starting at that earleme must be
added.
It is perfectly acceptable for there to be no tokens
starting at a given earleme.
However, once earleme I<N> is scanned,
it is no longer possible to add a token starting at any of the earlemes
from 0 to I<N>.

=item 4

With every token, a length in earlemes must be given,
and this length cannot be zero or negative.

=back

=head2 Exhaustion

At the start of parsing,
the B<furthest earleme> is earleme 0.
When a token is recognized, its end earleme is determined by
adding the token length to the current earleme.
If the new token's end earleme is after the furthest earleme,
the furthest earleme is set at the new token's end earleme.

If, after scanning all the tokens at an earleme,
the current earleme
has reached the furthest earleme,
no more successful parses are possible.
At this point, the recognizer is said to
be B<exhausted>.
A recognizer is B<active>
if and only if it is not exhausted.

Parsing is said to be exhausted,
when the recognizer is exhausted.
Parsing is said to be active,
when the recognizer is active.

Exhausted parsing does not mean failed parsing.
In particular,
parsing is often exhausted at the point of a successful parse.
An exhausted recognizer
may also contain successful parses
both prior to the current earleme.

Conversely, active parsing does not mean successful parsing.
A recognizer remains active as long as some potential input
I<might> produce a successful parse.
This does not mean that it ever will.

Marpa parsing can remain active even if
no token is found at the current earleme.
In the one-character-per-earleme model,
the current earleme might fall in the middle of a
previously recognized token
and parsing will remain active at least until the end of that
token is reached.
In the one-character-per-earleme model,
stretches where no token either starts or ends
can be many earlemes in length.

=head2 Cloning

The C<new> constructor requires a grammar to be specified in
one of its arguments.
By default, the C<new> constructor clones the grammar object.
This is done so that recognizers do not interfere with each other by
modifying the same data.
Cloning is the default behavior, and is always safe.

While safe, cloning does impose an overhead in memory and time.
This can be avoided by using the C<clone> option with the C<new>
constructor.
Not cloning is safe if you know that the grammar object will not be shared by another recognizer
or used by more than one evaluator.

It is very common for a Marpa program to have simple
flows of data, where no more than one recognizer is created from any grammar,
and no more than one evaluator is created from any recognizer.
When this is the case, cloning is unnecessary.

=head1 METHODS

=head2 new

=begin Marpa::Test::Display:

## start display
## next display
is_file($_, 'author.t/misc.t', 'new Recognizer snippet');

=end Marpa::Test::Display:

    my $recce = Marpa::Recognizer->new(
        {    grammar      => $grammar,
        }
    );

=begin Marpa::Test::Display:

## end display

=end Marpa::Test::Display:

The C<new> method's one, required, argument is a hash reference of named
arguments.
The C<new> method either returns a new parse object or throws an exception.
Either the C<stringified_grammar> or the C<grammar> named argument must be specified, but not both.
A recognizer is created with
the current earleme
set at earleme 0.

If the C<grammar> option is specified, 
its value must be a grammar object with rules defined.
By default, the grammar is cloned for use in the recognizer.

If the C<stringified_grammar> option is specified, 
its value must be a Perl 5 string containing a stringified Marpa grammar,
as produced by L<C<Marpa::Grammar::stringify>|Marpa::Grammar/"stringify">.
It will be unstringified for use in the recognizer.
When the C<stringified_grammar> option is specified, 
the resulting grammar is never cloned,
regardless of the setting of the C<clone> argument.

If the C<clone> argument is set to 1,
and the grammar argument is not in stringified form,
C<new> clones the grammar object.
This prevents
evaluators from interfering with each other's data.
This is the default and is always safe.
If C<clone> is set to 0,
the evaluator will work directly with
the grammar object which was its argument.
See L<above|/"Cloning"> for more detail.

Marpa options can also
be named arguments to C<new>.
For details of the Marpa options,
see L<Marpa::Doc::Options>.

=head2 text (NO LONGER A RECCE METHOD)

=begin Marpa::Test::Display:

## next display
in_file($_, 't/equation_s.t');

=end Marpa::Test::Display:

    my $fail_offset = $lexer->text('2-0*3+1');
    if ( $fail_offset >= 0 ) {
        Marpa::exception("Parse failed at offset $fail_offset");
    }

Extends the parse using the one-character-per-earleme model.
The one, required, argument must be
a string or a
reference to a string which contains text to be parsed.
If all the input was successfully consumed, the C<text> method returns
a negative number.
The return value is C<Marpa::Recognizer::PARSING_EXHAUSTED> if parsing was exhausted after consuming the
entire input.
The return value is C<Marpa::Recognizer::PARSING_STILL_ACTIVE> if parsing was still active after consuming the
entire input.

If parsing was exhausted before all the input was consumed,
the C<text> method returns the number of characters that were
consumed before parsing was exhausted.
If C<text> is called on an exhausted recognizer,
so that none of the input can be consumed,
the return value is 0.
Failures, other than exhausted recognizers, are thrown as exceptions.

Terminals are recognized in the text
using the lexers that were specified in the porcelain
or the plumbing.
The earleme length of each token is
set to the length of the token in characters.
(If a token has a "lex prefix",
the length of the lex prefix counts as part of the token length.)

=for stopwords cth

Subsequent
calls to C<text> on the same recognizer always advance the earleme numbering
monotonically.
The I<c>th character,
where the count I<c> includes
all characters from any previous calls to the C<text> method
for this recognizer,
will start at earleme I<c-1>
and will end at earleme I<c>.

How a string is divided up among calls to the C<text> method
makes no difference in the earleme location of individual characters,
but it can affect the recognition of terminals by the lexers.
If the characters from a single terminal
are split between two C<text> calls,
the lexers will fail to recognize that terminal.
Terminals cannot span calls to the C<text> method.

=head2 earleme

=begin Marpa::Test::Display:

## next display
in_file($_, 't/ah2.t');

=end Marpa::Test::Display:

    $recce->earleme( [ $a, 'a', 1 ] ) or Marpa::exception('Parsing exhausted');

The C<earleme> method takes zero or more arguments.
Each argument represents a token which starts at the B<current earleme>.
Because ambiguous lexing is allowed.
more than one token may start at each earleme,
in which case, there will be one argument per token.
Because tokens can span earlemes,
no tokens may start at an earleme
in which case the call to C<earleme> will have zero arguments.

After adding the tokens to the recognizer,
the C<earleme> method determines whether the recognizer is active or exhausted.
If the recognizer is still active,
the C<earleme> method moves the current earleme forward by one,
and the C<earleme> method returns 1.
If the recognizer is exhausted, the current earleme stays where it is,
and the C<earleme> method returns 0.
The C<earleme> method throws an exception on failure.
Any attempt to add more input to an exhausted recognizer will fail.

Each token argument is a reference to a three element array.
The first element is a "cookie" for the token's symbol,
as returned by the C<Marpa::Grammar::get_symbol> method
or the C<get_symbol> method of a porcelain interface.
The second element is the token's value in the parse,
and may be any value legal in Perl 5, including undefined.
The third is the token's length in earlemes.

While the recognizer is active, 
an earleme remains the current earleme during only one call of the C<earleme> method.
All tokens starting at that earleme must be added in that call.
The first time that the C<earleme> method is called in a recognizer,
the current earleme is at earleme 0.

Once a recognizer is exhausted, the current earleme never moves
and no more input can be added.
It is possible for a call to B<earleme>
with no arguments
to exhaust the recognizer.
This happens if
C<earleme> is called
with zero arguments when the current earleme reaches 
the furthest earleme.

C<earleme> is the low-level token input method.
Unlike C<text>, the C<earleme> method assumes no particular model of the input.
It is up to the user to define the relationship between
tokens and earlemes.

=head2 end_input

=begin Marpa::Test::Display:

## next display
in_file($_, 't/equation.t');

=end Marpa::Test::Display:

    $recce->end_input();

Used to indicate the end of input.
Tells the recognizer that
no new tokens will be added,
or, in other words,
that no tokens will start at
or after the current earleme.
The C<end_input> method takes no arguments.

The C<end_input> method
does not change the location of the furthest earleme.
After a successful call to 
the C<end_input> method,
the current earleme will be positioned at the furthest earleme.
Since positioning the current earleme at the furthest
earleme leaves the recognizer exhausted,
any further calls to C<text> will return 0,
and any further calls to C<earleme> will throw an
exception.

The C<end_input> method returns a Perl true value on success.
On failure, it throws an exception.
The C<end_input> method can only usefully be called once
per recognizer, but the method is idempotent.
Subsequent calls to the C<end_input> method
will have no effect and will return a Perl true.

=head2 stringify

=begin Marpa::Test::Display:

## next display
in_file($_, 'author.t/misc.t');

=end Marpa::Test::Display:

    my $stringified_recce = $recce->stringify();

The C<stringify> method takes as its single argument a recognizer object
and converts it into a string.
It returns a reference to the string.
The string is created 
using L<Data::Dumper>.
On failure, C<stringify> throws an exception.

=head2 unstringify

=begin Marpa::Test::Display:

## start display
## next display
is_file($_, 'author.t/misc.t', 'unstringify Recognizer snippet');

=end Marpa::Test::Display:

    $recce = Marpa::Recognizer::unstringify( $stringified_recce, $trace_fh );

    $recce = Marpa::Recognizer::unstringify($stringified_recce);

=begin Marpa::Test::Display:

## end display

=end Marpa::Test::Display:

The C<unstringify> static method takes a reference to a stringified recognizer as its first
argument.
Its second, optional, argument is a file handle.
The file handle argument will be used both as the unstringified recognizer's trace file handle,
and for any trace messages produced by C<unstringify> itself.
C<unstringify> returns the unstringified recognizer object unless it throws an
exception.

If the trace file handle argument is omitted,
it defaults to C<STDERR>
and the unstringified recognizer's trace file handle reverts to the default for a new
recognizer, which is also C<STDERR>.
The trace file handle argument is necessary because in the course of stringifying,
the recognizer's original trace file handle may have been lost.

=head2 clone

=begin Marpa::Test::Display:

## next 2 displays
in_file($_, 'author.t/misc.t');

=end Marpa::Test::Display:

    my $cloned_recce = $recce->clone();

The C<clone> method creates a useable copy of a recognizer object.
It returns a successfully cloned recognizer object,
or throws an exception.

=head1 SUPPORT

See the L<support section|Marpa/SUPPORT> in the main module.

=head1 AUTHOR

Jeffrey Kegler

=head1 LICENSE AND COPYRIGHT

Copyright 2007 - 2009 Jeffrey Kegler

This program is free software; you can redistribute
it and/or modify it under the same terms as Perl 5.10.0.

=cut
