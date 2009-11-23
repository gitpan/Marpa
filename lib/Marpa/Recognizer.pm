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
    FURTHEST_EARLEME :{ last earley set with something in it }
    FURTHEST_TOKEN :{ furthest end of token }
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
        $self->[Marpa::Internal::Recognizer::FURTHEST_TOKEN] =
        $self->[Marpa::Internal::Recognizer::FURTHEST_EARLEME] = 0;

    Marpa::Recognizer::tokens( $self, 'predict', 'absolute', 0 );

    return $self;
} ## end sub Marpa::Recognizer::new

sub Marpa::Recognizer::check_terminal {
    my ( $recce, $name ) = @_;
    my $grammar = $recce->[Marpa::Internal::Recognizer::GRAMMAR];
    return $grammar->check_terminal($name);
}

sub Marpa::Recognizer::furthest {
    my ($recce) = @_;
    return $recce->[Marpa::Internal::Recognizer::FURTHEST_EARLEME];
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

sub Marpa::Recognizer::find_parse {
    my ( $recce, $match, $first_earley_ix, $direction ) = @_;

    given ($direction) {
        when (undef)    { $direction = -1 }
        when ( $_ > 0 ) { $direction = 1 }
        default { $direction = -1 }    # default direction is last to first
    }
    my $earley_set_list = $recce->[Marpa::Internal::Recognizer::EARLEY_SETS];

    given ($first_earley_ix) {
        when (undef) {
            $first_earley_ix = $direction >= 0 ? 0 : $#{$earley_set_list}
        }
        when ( $direction == -1 and $first_earley_ix < 0 ) {
            $first_earley_ix = 0
        }
        when (      $direction == 1
                and $first_earley_ix > scalar @{$earley_set_list} )
        {
            $first_earley_ix = scalar @{$earley_set_list}
        }
    } ## end given
    my $last_earley_ix = $direction >= 0 ? scalar @{$earley_set_list} : -1;

    my $grammar       = $recce->[Marpa::Internal::Recognizer::GRAMMAR];
    my @sought_states = ();
    if ( not defined $match ) {
        @sought_states =
            map { $_ + 0 }    # convert pointers to numeric
            @{ $grammar->[Marpa::Internal::Grammar::START_STATES] };
    }
    else {
        QDFA: for my $qdfa ( $grammar->[Marpa::Internal::Grammar::QDFA] ) {
            for my $completed_lhs (
                @{ $qdfa->[Marpa::Internal::QDFA::COMPLETE_LHS] } )
            {
                if ( $completed_lhs ~~ $match ) {
                    push @sought_states, $qdfa + 0;
                    next QDFA;
                }
            } ## end for my $completed_lhs ( @{ $qdfa->[...]})
        } ## end for my $qdfa ( $grammar->[Marpa::Internal::Grammar::QDFA...])
    } ## end else [ if ( not defined $match ) ]

    my @found_states  = ();
    my $earley_set_ix = $first_earley_ix;
    EARLEY_SET: while ( $earley_set_ix != $last_earley_ix ) {
        my $earley_set = $earley_set_list->[$earley_set_ix];
        EARLEY_ITEM: for my $earley_item ( @{$earley_set} ) {
            next EARLEY_ITEM
                if $earley_item->[Marpa::Internal::Earley_Item::PARENT] != 0;
            my $state = $earley_item->[Marpa::Internal::Earley_Item::STATE];
            next EARLEY_ITEM if ( $state + 0 ) ~~ \@sought_states;
            push @found_states, $state;
        } ## end for my $earley_item ( @{$earley_set} )
        last EARLEY_SET if @found_states;
        $earley_set_ix += $direction;
    } ## end while ( $earley_set_ix != $last_earley_ix )

    return if not @found_states;

    my %lhs_seen;
    my @found_symbols =
        grep { ( $_ ~~ $match ) and not( $lhs_seen{$_}++ ) }
        map { @{ $_->[Marpa::Internal::QDFA::COMPLETE_LHS] } } @found_states;

    return $earley_set_ix if not wantarray;
    return ( $earley_set_ix, \@found_symbols );
} ## end sub Marpa::Recognizer::find_parse

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

sub Marpa::Recognizer::strip {
    my ($recce) = @_;
    $#{$recce} = Marpa::Internal::Recognizer::LAST_EVALUATOR_FIELD;
    return 1;
}

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
    my $trace_earley_sets =
        $grammar->[Marpa::Internal::Grammar::TRACE_EARLEY_SETS];
    my $trace_terminals =
        $grammar->[Marpa::Internal::Grammar::TRACE_TERMINALS];
    my $too_many_earley_items =
        $grammar->[Marpa::Internal::Grammar::TOO_MANY_EARLEY_ITEMS];

    my $tokens;
    my $predict_earleme;
    my $predict_offset_is_absolute = 0;
    my $continue_earleme;
    my $continue_offset_is_absolute = 0;

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
    my $furthest_token =
        $recce->[Marpa::Internal::Recognizer::FURTHEST_TOKEN];
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

        if ( $end_earleme > $furthest_token ) {
            $furthest_token = $end_earleme;
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
    $recce->[Marpa::Internal::Recognizer::FURTHEST_TOKEN] = $furthest_token;

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
        : $furthest_token;

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

        my $furthest_earleme =
            $recce->[Marpa::Internal::Recognizer::FURTHEST_EARLEME];

        my %accepted = ();    # used only if trace_terminals set

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
                my $token_name = $token->[Marpa::Internal::Symbol::NAME];
                if ($trace_terminals) {
                    $accepted{$token_name} //= 0;
                }

                my $states =
                    $QDFA->[ $state->[Marpa::Internal::QDFA::ID] ]
                    ->[Marpa::Internal::QDFA::TRANSITION]->{$token_name};

                next ALTERNATIVE if not $states;
                if ($trace_terminals) {
                    $accepted{$token_name}++;
                }

                # Create the kernel item and its link.
                my $target_ix = $last_completed_earleme + $length;
                if ( $target_ix > $furthest_earleme ) {
                    $furthest_earleme = $target_ix;
                }

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

        if ($trace_terminals) {
            while ( my ( $token_name, $accepted ) = each %accepted ) {
                say {$trace_fh} +( $accepted ? 'Accepted' : 'Rejected' ),
                    qq{ "$token_name" at $last_completed_earleme};
            }
        } ## end if ($trace_terminals)

        $recce->[Marpa::Internal::Recognizer::FURTHEST_EARLEME] =
            $furthest_earleme;
        if ( $furthest_earleme < $last_completed_earleme ) {
            $recce->[Marpa::Internal::Recognizer::EXHAUSTED] = 1;
            return;
        }

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

        if ($trace_earley_sets) {
            print {$trace_fh} Marpa::show_earley_set($earley_set)
                or Marpa::exception("print failed: $!");
        }

        $current_terminals = [
            map { $symbols->[$_]->[Marpa::Internal::Symbol::NAME] }
            grep { $lexable_seen->[$_] } ( 0 .. $#{$symbols} )
        ];

        if ($trace_terminals) {
            for my $terminal ( @{$current_terminals} ) {
                say {$trace_fh}
                    qq{Expecting "$terminal" at $last_completed_earleme};
            }
        } ## end if ($trace_terminals)

    } ## end while (1)

    $recce->[Marpa::Internal::Recognizer::CURRENT_TERMINALS] =
        $current_terminals;

    return ( $current_earleme, $current_terminals ) if wantarray;

    return $current_earleme;

} ## end sub Marpa::Recognizer::tokens

1;
