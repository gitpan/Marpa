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
    LAST_COMPLETED_EARLEME

    TRACE_FILE_HANDLE

    =LAST_EVALUATOR_FIELD

    WANTED
    CURRENT_TERMINALS
    EARLEY_HASH
    EXHAUSTED
    FINISHED
    TERMINALS_BY_STATE

    TOO_MANY_EARLEY_ITEMS
    TRACE_EARLEY_SETS
    TRACE_TERMINALS
    WARNINGS
    TRACING

    MODE

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

use constant DEFAULT_TOO_MANY_EARLEY_ITEMS => 100;

my $parse_number = 0;

# Returns the new parse object or throws an exception
sub Marpa::Recognizer::new {
    my ( $class, @arg_hashes ) = @_;
    my $self = bless [], $class;

    my $grammar;
    ARG_HASH: for my $arg_hash (@arg_hashes) {
        if ( defined( $grammar = $arg_hash->{grammar} ) ) {
            delete $arg_hash->{grammar};
            last ARG_HASH;
        }
    } ## end for my $arg_hash (@arg_hashes)
    Marpa::exception('No grammar specified') if not defined $grammar;

    $self->[Marpa::Internal::Recognizer::GRAMMAR] = $grammar;

    my $grammar_class = ref $grammar;
    Marpa::exception(
        "${class}::new() grammar arg has wrong class: $grammar_class")
        if not $grammar_class eq 'Marpa::Grammar';

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

    # set the defaults
    local $Marpa::Internal::TRACE_FH = my $trace_fh =
        $self->[Marpa::Internal::Recognizer::TRACE_FILE_HANDLE] =
        $grammar->[Marpa::Internal::Grammar::TRACE_FILE_HANDLE];
    $self->[Marpa::Internal::Recognizer::WARNINGS] = 1;
    $self->[Marpa::Internal::Recognizer::MODE]     = 'default';

    $self->set(@arg_hashes);

    if (not
        defined $self->[Marpa::Internal::Recognizer::TOO_MANY_EARLEY_ITEMS] )
    {
        my $QDFA_size =
            scalar @{ $grammar->[Marpa::Internal::Grammar::QDFA] };
        $self->[Marpa::Internal::Recognizer::TOO_MANY_EARLEY_ITEMS] =
            List::Util::max( ( 2 * $QDFA_size ),
            Marpa::Internal::Recognizer::DEFAULT_TOO_MANY_EARLEY_ITEMS );
    } ## end if ( not defined $self->[...])

    # Pull lookup of terminal flag by symbol ID out of the loop
    # over the QDFA transitions
    my $symbols = $grammar->[Marpa::Internal::Grammar::SYMBOLS];
    my %terminal_names =
        map { $_->[Marpa::Internal::Symbol::NAME] => 1 }
        grep { $_->[Marpa::Internal::Symbol::TERMINAL] } @{$symbols};

    my $QDFA        = $grammar->[Marpa::Internal::Grammar::QDFA];
    my $symbol_hash = $grammar->[Marpa::Internal::Grammar::SYMBOL_HASH];
    my @terminals_by_state;
    $#terminals_by_state = $#{$QDFA};

    for my $state ( @{$QDFA} ) {
        my ( $id, $transition ) =
            @{$state}[ Marpa::Internal::QDFA::ID,
            Marpa::Internal::QDFA::TRANSITION, ];
        $terminals_by_state[$id] = [
            grep { $terminal_names{$_} }
                keys %{$transition}
        ];
    } ## end for my $state ( @{$QDFA} )

    $self->[Marpa::Internal::Recognizer::TERMINALS_BY_STATE] =
        \@terminals_by_state;

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
    $self->[Marpa::Internal::Recognizer::WANTED]      = \%wanted;

    $self->[Marpa::Internal::Recognizer::LAST_COMPLETED_EARLEME] = -1;
    $self->[Marpa::Internal::Recognizer::FURTHEST_EARLEME]       = 0;

    $self->[Marpa::Internal::Recognizer::LAST_COMPLETED_EARLEME] =
        Marpa::Internal::Recognizer::complete($self);

    return $self;
} ## end sub Marpa::Recognizer::new

use constant RECOGNIZER_OPTIONS => [
    qw{
        too_many_earley_items
        trace_earley_sets
        trace_file_handle
        trace_terminals
        warnings
        mode
        }
];

use constant RECOGNIZER_MODES => [qw(default stream)];

sub Marpa::Recognizer::set {
    my ( $recce, @arg_hashes ) = @_;

    # This may get changed below
    my $trace_fh = $recce->[Marpa::Internal::Recognizer::TRACE_FILE_HANDLE];

    for my $args (@arg_hashes) {

        my $ref_type = ref $args;
        if ( not $ref_type or $ref_type ne 'HASH' ) {
            Carp::croak(
                'Marpa Recognizer expects args as ref to HASH, got ',
                ( "ref to $ref_type" || 'non-reference' ),
                ' instead'
            );
        } ## end if ( not $ref_type or $ref_type ne 'HASH' )
        if (my @bad_options =
            grep { not $_ ~~ Marpa::Internal::Recognizer::RECOGNIZER_OPTIONS }
            keys %{$args}
            )
        {
            Carp::croak( 'Unknown option(s) for Marpa Recognizer: ',
                join q{ }, @bad_options );
        } ## end if ( my @bad_options = grep { not $_ ~~ ...})

        if ( defined( my $value = $args->{'mode'} ) ) {
            if ( not $value ~~ Marpa::Internal::Recognizer::RECOGNIZER_MODES )
            {
                Carp::croak( 'Unknown mode for Marpa Recognizer: ', $value );
            }
            $recce->[Marpa::Internal::Recognizer::MODE] = $value;
        } ## end if ( defined( my $value = $args->{'mode'} ) )

        if ( defined( my $value = $args->{'trace_file_handle'} ) ) {
            $trace_fh =
                $recce->[Marpa::Internal::Recognizer::TRACE_FILE_HANDLE] =
                $value;
        }

        if ( defined( my $value = $args->{'trace_terminals'} ) ) {
            $recce->[Marpa::Internal::Recognizer::TRACE_TERMINALS] = $value;
            if ($value) {
                say {$trace_fh} 'Setting trace_terminals option';
                $recce->[Marpa::Internal::Recognizer::TRACING] = 1;
            }
        } ## end if ( defined( my $value = $args->{'trace_terminals'}...))

        if ( defined( my $value = $args->{'trace_earley_sets'} ) ) {
            $recce->[Marpa::Internal::Recognizer::TRACE_EARLEY_SETS] = $value;
            if ($value) {
                say {$trace_fh} 'Setting trace_earley_sets option';
                $recce->[Marpa::Internal::Recognizer::TRACING] = 1;
            }
        } ## end if ( defined( my $value = $args->{'trace_earley_sets'...}))

        if ( defined( my $value = $args->{'warnings'} ) ) {
            $recce->[Marpa::Internal::Recognizer::WARNINGS] = $value;
        }

        if ( defined( my $value = $args->{'too_many_earley_items'} ) ) {
            $recce->[Marpa::Internal::Recognizer::TOO_MANY_EARLEY_ITEMS] =
                $value;
        }

    } ## end for my $args (@arg_hashes)

    return 1;
} ## end sub Marpa::Recognizer::set

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
    my $last_completed_earleme =
        $recce->[Marpa::Internal::Recognizer::LAST_COMPLETED_EARLEME];
    return (
        $last_completed_earleme,
        [   keys %{ $recce->[Marpa::Internal::Recognizer::CURRENT_TERMINALS] }
        ]
    ) if wantarray;
    return $last_completed_earleme;
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

sub Marpa::Recognizer::strip {
    my ($recce) = @_;
    $#{$recce} = Marpa::Internal::Recognizer::LAST_EVALUATOR_FIELD;
    return 1;
}

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
    my $last_completed_earleme = $recce->[LAST_COMPLETED_EARLEME]
        // 'stripped';
    my $furthest_earleme = $recce->[FURTHEST_EARLEME];
    my $earley_set_list  = $recce->[EARLEY_SETS];

    return
          "Last Completed: $last_completed_earleme; "
        . "Furthest: $furthest_earleme\n"
        . Marpa::show_earley_set_list($earley_set_list);

} ## end sub Marpa::Recognizer::show_earley_sets

sub Marpa::Recognizer::tokens {

    my ( $recce, $tokens, $token_ix_ref ) = @_;

    # say STDERR "Calling tokens(): ", Data::Dumper::Dumper($tokens);

    Marpa::exception('No recognizer object for Marpa::Recognizer::tokens')
        if not defined $recce
            or ref $recce ne 'Marpa::Recognizer';

    Marpa::exception('No tokens arg for Marpa::Recognizer::tokens')
        if not defined $tokens;

    my $mode     = $recce->[Marpa::Internal::Recognizer::MODE];
    my $interactive;

    if ( defined $token_ix_ref ) {
        Marpa::exception(
            q{'Tokens index ref for Marpa::Recognizer::tokens allowed only in 'stream' mode}
        ) if $mode ne 'stream';
        $interactive = 1;
    } ## end if ( defined $token_ix_ref )

    my $grammar = $recce->[Marpa::Internal::Recognizer::GRAMMAR];
    local $Marpa::Internal::TRACE_FH = my $trace_fh =
        $recce->[Marpa::Internal::Recognizer::TRACE_FILE_HANDLE];
    my $trace_terminals =
        $recce->[Marpa::Internal::Recognizer::TRACE_TERMINALS];
    my $warnings = $recce->[Marpa::Internal::Recognizer::WARNINGS];

    my $earley_hash = $recce->[Marpa::Internal::Recognizer::EARLEY_HASH];
    my $wanted      = $recce->[Marpa::Internal::Recognizer::WANTED];

    Marpa::exception('Attempt to scan tokens after parsing is finished')
        if $recce->[Marpa::Internal::Recognizer::FINISHED]
            and scalar @{$tokens};

    Marpa::exception('Attempt to scan tokens when parsing is exhausted')
        if $recce->[Marpa::Internal::Recognizer::EXHAUSTED]
            and scalar @{$tokens};

    # TOKEN PROCESSING PHASE

    my $symbols     = $grammar->[Marpa::Internal::Grammar::SYMBOLS];
    my $symbol_hash = $grammar->[Marpa::Internal::Grammar::SYMBOL_HASH];

    my $next_token_earleme = my $last_completed_earleme =
        $recce->[Marpa::Internal::Recognizer::LAST_COMPLETED_EARLEME];
    my $furthest_earleme =
        $recce->[Marpa::Internal::Recognizer::FURTHEST_EARLEME];
    my $earley_set_list = $recce->[Marpa::Internal::Recognizer::EARLEY_SETS];
    my $QDFA            = $grammar->[Marpa::Internal::Grammar::QDFA];

    $token_ix_ref //= \(my $token_ix = 0);

    # say STDERR __LINE__, " last_completed_earleme: $last_completed_earleme";

    my $token_args = $tokens->[${$token_ix_ref}];

    if ( not scalar @{$tokens} ) { $next_token_earleme++ }

    EARLEME: while ( ${$token_ix_ref} < scalar @{$tokens} ) {

        my $tokens_here;
        my $token_hash_here;

        my $current_token_earleme = $last_completed_earleme;

        my $current_terminals =
            $recce->[Marpa::Internal::Recognizer::CURRENT_TERMINALS];

        my $first_ix_of_this_earleme = ${$token_ix_ref};

        TOKEN: while ( $current_token_earleme == $next_token_earleme ) {

            last TOKEN if not my $token_args = $tokens->[ ${$token_ix_ref} ];
            ${$token_ix_ref}++;
            my ( $symbol_name, $value, $length, $offset ) = @{$token_args};

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

            my $earley_items = $current_terminals->{$symbol_name};
            if ( not $earley_items ) {
                if ( not $interactive ) {
                    Marpa::exception(
                        qq{Terminal "$symbol_name" received when not expected}
                    );
                }
                ${$token_ix_ref} = $first_ix_of_this_earleme;
                return (
                    $last_completed_earleme,
                    [   keys %{
                            $recce->[
                                Marpa::Internal::Recognizer::CURRENT_TERMINALS
                            ]
                            }
                    ]
                ) if wantarray;
                return $last_completed_earleme;

            } ## end if ( not $earley_items )

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
                } ## end when ( $_ & Marpa::Internal::Recognizer::EARLEME_MASK)
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
                    . " makes parse too long\n",
                "  Token starts at $last_completed_earleme, and its length is $length\n"
            ) if $end_earleme & Marpa::Internal::Recognizer::EARLEME_MASK;

            $offset //= 1;
            Marpa::exception(
                'Token '
                    . $token->[Marpa::Internal::Symbol::NAME]
                    . " has negative offset\n",
                "  Token starts at $last_completed_earleme, and its length is $length\n",
                "  Tokens are required to in sequence by location\n",
            ) if $offset < 0;
            $next_token_earleme += $offset;

            # say STDERR __LINE__, " offset: $offset";
            # say STDERR __LINE__, " next_token_earleme: $next_token_earleme";

            my $token_entry = [ $token, $value_ref, $length, $earley_items ];

            # This logic is arranged so that non-overlapping tokens do not incur the cost
            # of the checks for duplicates
            if ( not $tokens_here ) {
                $tokens_here = [$token_entry];
                next TOKEN;
            }

            if ( not $token_hash_here ) {
                $token_hash_here =
                    { map { ( join q{;}, @{$_}[ 0, 2 ] ) => 1 }
                        @{$tokens_here} };
            }

            my $hash_key = join q{;}, $token, $length;
            Marpa::exception( $token->[Marpa::Internal::Symbol::NAME],
                " already exists with length $length at location $current_token_earleme"
            ) if $token_hash_here->{$hash_key};

            $token_hash_here->{$hash_key} = 1;
            push @{$tokens_here}, $token_entry;

        } ## end while ( $current_token_earleme == $next_token_earleme )

        $current_token_earleme++;

        $tokens_here //= [];

        $earley_set_list->[$last_completed_earleme] //= [];
        my $earley_set = $earley_set_list->[$last_completed_earleme];

        my %accepted = ();    # used only if trace_terminals set

        ALTERNATIVE: for my $alternative ( @{$tokens_here} ) {
            my ( $token, $value_ref, $length, $earley_items ) =
                @{$alternative};

            # compute goto(state, token_name)
            my $token_name = $token->[Marpa::Internal::Symbol::NAME];
            if ($trace_terminals) {
                $accepted{$token_name} //= 0;
            }

            EARLEY_ITEM: for my $earley_item ( @{$earley_items} ) {

                my ( $state, $parent ) = @{$earley_item}[
                    Marpa::Internal::Earley_Item::STATE,
                    Marpa::Internal::Earley_Item::PARENT
                ];

                my $states =
                    $QDFA->[ $state->[Marpa::Internal::QDFA::ID] ]
                    ->[Marpa::Internal::QDFA::TRANSITION]->{$token_name};

                next EARLEY_ITEM if not $states;
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
            $recce->[Marpa::Internal::Recognizer::FURTHEST_EARLEME] =
                $furthest_earleme;
            $recce->[Marpa::Internal::Recognizer::EXHAUSTED] = 1;
            return;
        } ## end if ( $furthest_earleme < $last_completed_earleme )

        $last_completed_earleme =
            Marpa::Internal::Recognizer::complete($recce);

    } ## end while ( ${$token_ix_ref} < scalar @{$tokens} )

    $recce->[Marpa::Internal::Recognizer::FURTHEST_EARLEME] =
        $furthest_earleme;

    if ( $mode eq 'stream' ) {
        while ( $last_completed_earleme < $next_token_earleme ) {
            $last_completed_earleme =
                Marpa::Internal::Recognizer::complete($recce);
        }
    } ## end if ( $mode eq 'stream' )

    if ( $mode eq 'default' ) {
        while ( $last_completed_earleme < $furthest_earleme ) {
            $last_completed_earleme =
                Marpa::Internal::Recognizer::complete($recce);
        }
        $recce->[Marpa::Internal::Recognizer::FINISHED] = 1;
    } ## end if ( $mode eq 'default' )

    return (
        $last_completed_earleme,
        [   keys %{ $recce->[Marpa::Internal::Recognizer::CURRENT_TERMINALS] }
        ]
    ) if wantarray;

    return $last_completed_earleme;

} ## end sub Marpa::Recognizer::tokens

# Perform the completion step on an earley set

sub Marpa::Recognizer::end_input {
    my ($recce) = @_;
    local $Marpa::Internal::TRACE_FH =
        $recce->[Marpa::Internal::Recognizer::TRACE_FILE_HANDLE];
    my $last_completed_earleme =
        $recce->[Marpa::Internal::Recognizer::LAST_COMPLETED_EARLEME];
    my $furthest_earleme =
        $recce->[Marpa::Internal::Recognizer::FURTHEST_EARLEME];
    while ( $last_completed_earleme < $furthest_earleme ) {
        $last_completed_earleme =
            $recce->[Marpa::Internal::Recognizer::LAST_COMPLETED_EARLEME] =
            Marpa::Internal::Recognizer::complete($recce);
    }
    $recce->[Marpa::Internal::Recognizer::FINISHED] = 1;
    return 1;
} ## end sub Marpa::Recognizer::end_input

sub complete {
    my ($recce) = @_;

    my $grammar         = $recce->[Marpa::Internal::Recognizer::GRAMMAR];
    my $QDFA            = $grammar->[Marpa::Internal::Grammar::QDFA];
    my $earley_set_list = $recce->[Marpa::Internal::Recognizer::EARLEY_SETS];
    my $earley_hash     = $recce->[Marpa::Internal::Recognizer::EARLEY_HASH];
    my $symbols         = $grammar->[Marpa::Internal::Grammar::SYMBOLS];
    my $terminals_by_state =
        $recce->[Marpa::Internal::Recognizer::TERMINALS_BY_STATE];
    my $wanted = $recce->[Marpa::Internal::Recognizer::WANTED];
    my $too_many_earley_items =
        $recce->[Marpa::Internal::Recognizer::TOO_MANY_EARLEY_ITEMS];
    my $trace_earley_sets =
        $recce->[Marpa::Internal::Recognizer::TRACE_EARLEY_SETS];
    my $trace_terminals =
        $recce->[Marpa::Internal::Recognizer::TRACE_TERMINALS];

    my $earleme_to_complete =
        ++$recce->[Marpa::Internal::Recognizer::LAST_COMPLETED_EARLEME];

    $earley_set_list->[$earleme_to_complete] //= [];
    my $earley_set = $earley_set_list->[$earleme_to_complete];

    my %lexable_seen = ();

    # Important: more earley sets can be added in the loop
    my $earley_set_ix = -1;
    EARLEY_ITEM: while (1) {

        my $earley_item = $earley_set->[ ++$earley_set_ix ];
        last EARLEY_ITEM if not defined $earley_item;

        my ( $state, $parent ) = @{$earley_item}[
            Marpa::Internal::Earley_Item::STATE,
            Marpa::Internal::Earley_Item::PARENT
        ];
        my $state_id = $state->[Marpa::Internal::QDFA::ID];

        for my $lexable ( @{ $terminals_by_state->[$state_id] } ) {
            push @{ $lexable_seen{$lexable} }, $earley_item;
        }

        next EARLEY_ITEM if $earleme_to_complete == $parent;

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
                my $reset =
                    $transition_state->[Marpa::Internal::QDFA::RESET_ORIGIN];
                my $origin =
                      $reset
                    ? $earleme_to_complete
                    : $parent_item->[Marpa::Internal::Earley_Item::PARENT];
                my $transition_state_id =
                    $transition_state->[Marpa::Internal::QDFA::ID];
                my $name = sprintf
                    ## no critic (ValuesAndExpressions::RequireInterpolationOfMetachars)
                    'S%d@%d-%d',
                    ## use critic
                    $transition_state_id, $origin, $earleme_to_complete;
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
                        $earleme_to_complete,
                        );
                    $earley_hash->{$name} = $target_item;
                    push @{$earley_set}, $target_item;

                    while (
                        my ( $wanted_symbol_name, $next_states ) = each %{
                            $QDFA->[
                                $transition_state->[Marpa::Internal::QDFA::ID]
                                ]->[Marpa::Internal::QDFA::TRANSITION]
                        }
                        )
                    {
                        push @{
                            $wanted->{
                                      $wanted_symbol_name . q{@}
                                    . $earleme_to_complete
                                }
                            },
                            [ $target_item, $next_states ];
                    } ## end while ( my ( $wanted_symbol_name, $next_states ) = ...)

                }    # unless defined $target_item
                next TRANSITION_STATE if $reset;
                push @{ $target_item->[Marpa::Internal::Earley_Item::LINKS] },
                    [ $parent_item, $earley_item ];
            }    # TRANSITION_STATE

        }    # PARENT_ITEM

    }    # EARLEY_ITEM

    if ( $too_many_earley_items >= 0
        and ( my $item_count = scalar @{$earley_set} )
        >= $too_many_earley_items )
    {
        if ( $recce->[Marpa::Internal::Recognizer::WARNINGS] ) {
            say {$Marpa::Internal::TRACE_FH}
                "Very large earley set: $item_count items at location $earleme_to_complete";
        }
    } ## end if ( $too_many_earley_items >= 0 and ( my $item_count...))

    # Are the completion links unique?
    # It doesn't matter a lot.
    # I have to remove duplicates anyway
    # because the same rule derivation can result from
    # different states.

    if ($trace_earley_sets) {
        print {$Marpa::Internal::TRACE_FH}
            "=== Earley set $earleme_to_complete\n"
            or Marpa::exception("print failed: $!");
        print {$Marpa::Internal::TRACE_FH} Marpa::show_earley_set($earley_set)
            or Marpa::exception("print failed: $!");
    } ## end if ($trace_earley_sets)

    $recce->[Marpa::Internal::Recognizer::CURRENT_TERMINALS] = \%lexable_seen;

    if ($trace_terminals) {
        for my $terminal (
            keys %{ $recce->[Marpa::Internal::Recognizer::CURRENT_TERMINALS] }
            )
        {
            say {$Marpa::Internal::TRACE_FH}
                qq{Expecting "$terminal" at $earleme_to_complete};
        } ## end for my $terminal ( keys %{ $recce->[...]})
    } ## end if ($trace_terminals)

    return $earleme_to_complete;

} ## end sub complete

1;
