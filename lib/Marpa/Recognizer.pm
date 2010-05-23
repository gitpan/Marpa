package Marpa::Recognizer;

use 5.010;
use warnings;
no warnings 'recursion';
use strict;
use integer;

use English qw( -no_match_vars );

use Marpa::Internal;

# Elements of the EARLEY ITEM structure
# Note that these are Earley items as modified by Aycock & Horspool,
# with AHFA states instead of
# LR(0) items.

# We don't prune the Earley items because we want PARENT and SET
# around for debugging.

use Marpa::Offset qw(

    :package=Marpa::Internal::Earley_Item

    NAME { Unique string describing Earley item. }
    STATE { The AHFA state. }
    TOKENS { A list of the links from token scanning. }
    LINKS { A list of the links from the completer step. }
    IS_LEO_ITEM { Am I a Leo item? }
    LEO_LINKS { Leo Links source }

    =LAST_EVALUATOR_FIELD

    PARENT { The number of the Earley set with the parent item(s) }
    SET { The set this item is in. For debugging. }

    =LAST_FIELD

);

# Elements of the RECOGNIZER structure
use Marpa::Offset qw(

    :package=Marpa::Internal::Recognizer

    GRAMMAR { the grammar used }
    EARLEY_SETS { the array of the Earley sets }
    FURTHEST_EARLEME :{ last earley set with something in it }
    LAST_COMPLETED_EARLEME
    FINISHED

    TRACE_FILE_HANDLE

    =LAST_EVALUATOR_FIELD

    EARLEY_HASHES { Hash of the Earley items by Earley set.
    Used to prevent duplicates.  It is an
    array of hashes.  It starts at LAST_COMPLETED_EARLEME.
    Hashes are shifted off of it as the parse proceeds. }

    EXHAUSTED { parse can't continue? }

    LEO_SETS { An array. Indexed by AHFA state id.
    of hashes by symbol name to Leo items. }

    POSTDOT { An array. Indexed by AHFA state id.
    of hashes by symbol name to Earley item and to-states }

    TOO_MANY_EARLEY_ITEMS
    TRACE_EARLEY_SETS
    TRACE_TERMINALS
    WARNINGS
    TRACING

    MODE

);

package Marpa::Internal::Recognizer;

use Marpa::Internal;

use Data::Dumper;
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
        my $AHFA_size =
            scalar @{ $grammar->[Marpa::Internal::Grammar::AHFA] };
        $self->[Marpa::Internal::Recognizer::TOO_MANY_EARLEY_ITEMS] =
            List::Util::max( ( 2 * $AHFA_size ),
            Marpa::Internal::Recognizer::DEFAULT_TOO_MANY_EARLEY_ITEMS );
    } ## end if ( not defined $self->[...])

    # Some of this processing -- to find terminals and Leo symbols
    # by state -- should perhaps be done in the grammar.

    my $terminal_names = $grammar->[Marpa::Internal::Grammar::TERMINAL_NAMES];

    my $AHFA        = $grammar->[Marpa::Internal::Grammar::AHFA];
    my $symbol_hash = $grammar->[Marpa::Internal::Grammar::SYMBOL_HASH];

    my $earley_set;

    my $start_states = $grammar->[Marpa::Internal::Grammar::START_STATES];
    my $postdot = {};

    for my $state ( @{$start_states} ) {
        my $state_id = $state->[Marpa::Internal::AHFA::ID];
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

        while ( my ( $transition_symbol, $to_states ) =
            each %{ $state->[Marpa::Internal::AHFA::TRANSITION] } )
        {
            my @to_states = grep { ref } @{$to_states};
            push @{ $postdot->{0}->{$transition_symbol} },
                [ $item, \@to_states, 0 ];
        }

    } ## end for my $state ( @{$start_states} )

    # The above (maximum two) Earley items do not duplicate,
    # and no more will be added, so there will be no duplicate
    # Earley items in Earley set 0.
    $self->[Marpa::Internal::Recognizer::EARLEY_HASHES] = [];

    $self->[Marpa::Internal::Recognizer::GRAMMAR]     = $grammar;
    $self->[Marpa::Internal::Recognizer::EARLEY_SETS] = [$earley_set];

    $self->[Marpa::Internal::Recognizer::FURTHEST_EARLEME]       = 0;
    $self->[Marpa::Internal::Recognizer::LAST_COMPLETED_EARLEME] = 0;

    $self->[Marpa::Internal::Recognizer::POSTDOT] = $postdot;

    # Don't include the start states in the Leo sets.
    $self->[Marpa::Internal::Recognizer::LEO_SETS] = [];

    if ( $self->[Marpa::Internal::Recognizer::TRACE_TERMINALS] ) {
        for my $terminal (
            grep { $terminal_names->{$_} } keys %{ $postdot->{0} }
            )
        {
            say {$Marpa::Internal::TRACE_FH}
                qq{Expecting "$terminal" at earleme 0}
                or Marpa::exception("Cannot print: $ERRNO");
        } ## end for my $terminal ( keys %{ $self->[...]})
    } ## end if ( $self->[Marpa::Internal::Recognizer::TRACE_TERMINALS...])

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
                say {$trace_fh} 'Setting trace_terminals option'
                    or Marpa::exception("Cannot print: $ERRNO");
                $recce->[Marpa::Internal::Recognizer::TRACING] = 1;
            }
        } ## end if ( defined( my $value = $args->{'trace_terminals'}...))

        if ( defined( my $value = $args->{'trace_earley_sets'} ) ) {
            $recce->[Marpa::Internal::Recognizer::TRACE_EARLEY_SETS] = $value;
            if ($value) {
                say {$trace_fh} 'Setting trace_earley_sets option'
                    or Marpa::exception("Cannot print: $ERRNO");
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

sub Marpa::Recognizer::status {
    my ($recce) = @_;

    my $exhausted = $recce->[Marpa::Internal::Recognizer::EXHAUSTED];
    return if $exhausted;

    my $grammar        = $recce->[Marpa::Internal::Recognizer::GRAMMAR];
    my $terminal_names = $grammar->[Marpa::Internal::Grammar::TERMINAL_NAMES];
    my $last_completed_earleme =
        $recce->[Marpa::Internal::Recognizer::LAST_COMPLETED_EARLEME];

    return (
        $last_completed_earleme,
        [   grep { $terminal_names->{$_} }
                keys %{
                $recce->[Marpa::Internal::Recognizer::POSTDOT]
                    ->{$last_completed_earleme}
                }
        ]
    ) if wantarray;
    return $last_completed_earleme;

} ## end sub Marpa::Recognizer::status

sub Marpa::Recognizer::strip {
    my ($recce) = @_;
    Marpa::exception('Cannot strip recognizer before input is finished')
        if not $recce->[Marpa::Internal::Recognizer::FINISHED];
    $#{$recce} = Marpa::Internal::Recognizer::LAST_EVALUATOR_FIELD;
    return 1;
} ## end sub Marpa::Recognizer::strip

# Viewing methods, for debugging

sub Marpa::show_token_choice {
    my ($token) = @_;
    my ( $earley_item, $symbol_name, $value_ref ) = @{$token};
    my $token_dump = Data::Dumper->new( [$value_ref] )->Terse(1)->Dump;
    chomp $token_dump;
    my $earley_item_name = $earley_item->[Marpa::Internal::Earley_Item::NAME];
    return "[p=$earley_item_name; s=$symbol_name; t=$token_dump]";
} ## end sub Marpa::show_token_choice

sub Marpa::show_link_choice {
    my ($link) = @_;
    my $predecessor = $link->[0];
    my @link_texts = ();
    if ($predecessor) {
        push @link_texts, 'p=' . $predecessor->[Marpa::Internal::Earley_Item::NAME];
    }
    push @link_texts, 'c=' . $link->[1]->[Marpa::Internal::Earley_Item::NAME];
    return '[' . (join '; ', @link_texts) . ']';
} ## end sub Marpa::show_link_choice

sub Marpa::show_leo_link_choice {
    my ($leo_link) = @_;
    my ( $leo_item, $cause, $symbol_name, $value_ref ) = @{$leo_link};
    my @link_texts =
        ( 'l=' . $leo_item->[Marpa::Internal::Earley_Item::NAME] );
    if ($cause) {
        push @link_texts, 'c=' . $cause->[Marpa::Internal::Earley_Item::NAME];
    }
    else {
        my $token_dump = Data::Dumper->new( [$value_ref] )->Terse(1)->Dump;
        chomp $token_dump;
        push @link_texts, "s=$symbol_name", "t=$token_dump";
    }
    return '[' . ( join '; ', @link_texts ) . ']';
} ## end sub Marpa::show_leo_link_choice

sub Marpa::show_earley_item {
    my ($item) = @_;
    my $tokens = $item->[Marpa::Internal::Earley_Item::TOKENS];
    my $links  = $item->[Marpa::Internal::Earley_Item::LINKS];
    my $leo_links  = $item->[Marpa::Internal::Earley_Item::LEO_LINKS];
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
    if ( defined $leo_links and @{$leo_links} ) {
        for my $leo_link ( @{$leo_links} ) {
            $text .= q{ } . Marpa::show_leo_link_choice($leo_link);
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
    my ($earley_set_list, $leo_set_list) = @_;
    my $text              = q{};
    my $earley_set_count  = @{$earley_set_list};
    LIST: for my $ix ( 0 .. $earley_set_count - 1 ) {
        my $set = $earley_set_list->[$ix];
        next LIST if not defined $set;
        $text .= "Earley Set $ix\n" . Marpa::show_earley_set($set);
        my $leo_set = $leo_set_list->[$ix];
        next LIST if not defined $leo_set;
        $text .= Marpa::show_earley_set($leo_set);
    }
    return $text;
} ## end sub Marpa::show_earley_set_list

sub Marpa::Recognizer::show_earley_sets {
    my ($recce) = @_;
    my $last_completed_earleme = $recce->[LAST_COMPLETED_EARLEME]
        // 'stripped';
    my $furthest_earleme = $recce->[FURTHEST_EARLEME];
    my $earley_set_list  = $recce->[EARLEY_SETS];
    my $leo_set_list  = $recce->[LEO_SETS];

    return
          "Last Completed: $last_completed_earleme; "
        . "Furthest: $furthest_earleme\n"
        . Marpa::show_earley_set_list($earley_set_list, $leo_set_list);

} ## end sub Marpa::Recognizer::show_earley_sets

sub Marpa::Recognizer::tokens {

    my ( $recce, $tokens, $token_ix_ref ) = @_;

    Marpa::exception('No recognizer object for Marpa::Recognizer::tokens')
        if not defined $recce
            or ref $recce ne 'Marpa::Recognizer';

    Marpa::exception('No tokens arg for Marpa::Recognizer::tokens')
        if not defined $tokens;

    my $mode = $recce->[Marpa::Internal::Recognizer::MODE];
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

    my $earley_hashes = $recce->[Marpa::Internal::Recognizer::EARLEY_HASHES];

    Marpa::exception('Attempt to scan tokens after parsing is finished')
        if $recce->[Marpa::Internal::Recognizer::FINISHED]
            and scalar @{$tokens};

    Marpa::exception('Attempt to scan tokens when parsing is exhausted')
        if $recce->[Marpa::Internal::Recognizer::EXHAUSTED]
            and scalar @{$tokens};

    # TOKEN PROCESSING PHASE

    my $terminal_names     = $grammar->[Marpa::Internal::Grammar::TERMINAL_NAMES];

    my $next_token_earleme = my $last_completed_earleme =
        $recce->[Marpa::Internal::Recognizer::LAST_COMPLETED_EARLEME];
    my $furthest_earleme =
        $recce->[Marpa::Internal::Recognizer::FURTHEST_EARLEME];
    my $earley_set_list = $recce->[Marpa::Internal::Recognizer::EARLEY_SETS];
    my $AHFA            = $grammar->[Marpa::Internal::Grammar::AHFA];

    $token_ix_ref //= \( my $token_ix = 0 );

    my $token_args = $tokens->[ ${$token_ix_ref} ];

    if ( not scalar @{$tokens} ) { $next_token_earleme++ }

    EARLEME: while ( ${$token_ix_ref} < scalar @{$tokens} ) {

        my $tokens_here;
        my $token_hash_here;

        my $current_token_earleme = $last_completed_earleme;


        my $first_ix_of_this_earleme = ${$token_ix_ref};

        TOKEN: while ( $current_token_earleme == $next_token_earleme ) {

            last TOKEN if not my $token_args = $tokens->[ ${$token_ix_ref} ];
            Marpa::exception(
                'Tokens must be array refs: token #',
                ${$token_ix_ref},
                " is $token_args\n",
            ) if ref $token_args ne 'ARRAY';
            ${$token_ix_ref}++;
            my ( $symbol_name, $value, $length, $offset ) = @{$token_args};

            my $postdot = $recce->[Marpa::Internal::Recognizer::POSTDOT];
            my $postdot_here = $postdot->{$current_token_earleme};

            Marpa::exception(
                "Attempt to add token '$symbol_name' at location where processing is complete:\n",
                "  Add attempted at $current_token_earleme\n",
                "  Processing complete to $last_completed_earleme\n"
            ) if $current_token_earleme < $last_completed_earleme;

            if ( not $terminal_names->{$symbol_name} ) {
                Marpa::exception(
                    qq{Token name "$symbol_name" is not the name of a terminal symbol}
                );
            }

            # Make sure it's an allowed terminal symbol.
            my $postdot_data = $postdot_here->{$symbol_name};
            if ( not $postdot_data ) {
                if ( not $interactive ) {
                    Marpa::exception(
                        qq{Terminal "$symbol_name" received when not expected}
                    );
                }
                ${$token_ix_ref} = $first_ix_of_this_earleme;
                return (
                    $last_completed_earleme,
                    [   grep { $terminal_names->{$_} } keys %{ $postdot->{$last_completed_earleme} }
                    ]
                ) if wantarray;
                return $last_completed_earleme;

            } ## end if ( not $postdot_data )

            my $value_ref = \($value);

            given ($length) {
                when (undef) { $length = 1; }
                when ( $_ & Marpa::Internal::Recognizer::EARLEME_MASK ) {
                    Marpa::exception(
                        'Token ' . $symbol_name . " is too long\n",
                        "  Token starts at $last_completed_earleme, and its length is $length\n"
                        )
                } ## end when ( $_ & Marpa::Internal::Recognizer::EARLEME_MASK)
                when ( $_ <= 0 ) {
                    Marpa::exception( 'Token '
                            . $symbol_name
                            . ' has non-positive length '
                            . $length );
                } ## end when ( $_ <= 0 )
            } ## end given

            my $end_earleme = $current_token_earleme + $length;

            Marpa::exception(
                'Token ' . $symbol_name . " makes parse too long\n",
                "  Token starts at $last_completed_earleme, and its length is $length\n"
            ) if $end_earleme & Marpa::Internal::Recognizer::EARLEME_MASK;

            $offset //= 1;
            Marpa::exception(
                'Token ' . $symbol_name . " has negative offset\n",
                "  Token starts at $last_completed_earleme, and its length is $length\n",
                "  Tokens are required to in sequence by location\n",
            ) if $offset < 0;
            $next_token_earleme += $offset;

            my $token_entry =
                [ $symbol_name, $value_ref, $length, $postdot_data ];

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

            my $hash_key = join q{;}, $symbol_name, $length;
            Marpa::exception(
                qq{"$symbol_name" already scanned with length $length at location $current_token_earleme}
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
            my ( $token_name, $value_ref, $length, $postdot_data ) =
                @{$alternative};

            # compute goto(state, token_name)
            if ($trace_terminals) {
                $accepted{$token_name} //= 0;
            }

            EARLEY_ITEM: for my $postdot_data_entry ( @{$postdot_data} ) {

                my ($earley_item, $to_states, $parent, $leo_item) = @{$postdot_data_entry};

                next EARLEY_ITEM if not $to_states;

                if ($trace_terminals) {
                    $accepted{$token_name}++;
                }

                # Create the kernel item and its link.
                my $target_ix = $last_completed_earleme + $length;
                if ( $target_ix > $furthest_earleme ) {
                    $furthest_earleme = $target_ix;
                }

                my $target_set = ( $earley_set_list->[$target_ix] //= [] );
                TO_STATE: for my $to_state ( grep { ref } @{$to_states} ) {
                    my $reset =
                        $to_state->[Marpa::Internal::AHFA::RESET_ORIGIN];
                    my $origin      = $reset ? $target_ix : $parent;
                    my $to_state_id = $to_state->[Marpa::Internal::AHFA::ID];
                    my $name        = sprintf
                        ## no critic (ValuesAndExpressions::RequireInterpolationOfMetachars)
                        'S%d@%d-%d',
                        ## use critic
                        $to_state_id, $origin, $target_ix;

                    my $target_item =
                        $earley_hashes->[ $length - 1 ]->{$name};
                    if ( not defined $target_item ) {
                        $target_item = [];
                        $target_item->[Marpa::Internal::Earley_Item::NAME] =
                            $name;
                        $target_item->[Marpa::Internal::Earley_Item::STATE] =
                            $to_state;
                        $target_item->[Marpa::Internal::Earley_Item::PARENT] =
                            $origin;
                        $target_item->[Marpa::Internal::Earley_Item::LEO_LINKS] =
                            [];
                        $target_item->[Marpa::Internal::Earley_Item::LINKS] =
                            [];
                        $target_item->[Marpa::Internal::Earley_Item::TOKENS] =
                            [];
                        $target_item->[Marpa::Internal::Earley_Item::SET] =
                            $target_ix;
                        $earley_hashes->[ $length - 1 ]->{$name} =
                            $target_item;
                        push @{$target_set}, $target_item;

                    } ## end if ( not defined $target_item )

                    next TO_STATE if $reset;

                    if ($leo_item) {
                        push @{ $target_item
                                ->[Marpa::Internal::Earley_Item::LEO_LINKS] },
                            [ $leo_item, undef, $token_name, $value_ref ];
                    } else {
                        push @{ $target_item
                                ->[Marpa::Internal::Earley_Item::TOKENS] },
                            [ $earley_item, $token_name, $value_ref ];
                    }
                }    # for my $to_state

            }    # ALTERNATIVE

        }    # EARLEY_ITEM

        if ($trace_terminals) {
            while ( my ( $token_name, $accepted ) = each %accepted ) {
                say {$trace_fh} +( $accepted ? 'Accepted' : 'Rejected' ),
                    qq{ "$token_name" at $last_completed_earleme}
                    or Marpa::exception("Cannot print: $ERRNO");
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
        [   grep { $terminal_names->{$_} }
                keys %{
                $recce->[Marpa::Internal::Recognizer::POSTDOT]
                    ->{$last_completed_earleme}
                }
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
    my $AHFA            = $grammar->[Marpa::Internal::Grammar::AHFA];
    my $earley_set_list = $recce->[Marpa::Internal::Recognizer::EARLEY_SETS];

    my $earley_hash =
        ( shift @{ $recce->[Marpa::Internal::Recognizer::EARLEY_HASHES] } )
        // {};
    my $terminal_names = $grammar->[Marpa::Internal::Grammar::TERMINAL_NAMES];
    my $postdot        = $recce->[Marpa::Internal::Recognizer::POSTDOT];
    my $leo_sets       = $recce->[Marpa::Internal::Recognizer::LEO_SETS];
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

    # Important: more earley sets can be added in the loop
    my $earley_set_ix = -1;
    EARLEY_ITEM: while (1) {

        my $earley_item = $earley_set->[ ++$earley_set_ix ];
        last EARLEY_ITEM if not defined $earley_item;

        my ( $state, $parent ) = @{$earley_item}[
            Marpa::Internal::Earley_Item::STATE,
            Marpa::Internal::Earley_Item::PARENT
        ];
        my $state_id = $state->[Marpa::Internal::AHFA::ID];

        next EARLEY_ITEM if $earleme_to_complete == $parent;

        PARENT_ITEM:
        for my $parent_data (
            map  { @{$_} }
            grep {defined}
            map  { $postdot->{$parent}->{$_} }
            @{ $state->[Marpa::Internal::AHFA::COMPLETE_LHS] }
            )
        {
            my ( $parent_item, $states, $parent, $leo_item ) = @{$parent_data};
            my $parent_state =
                $parent_item->[Marpa::Internal::Earley_Item::STATE];

            TRANSITION_STATE:
            for my $transition_state ( grep { ref } @{$states} ) {
                my $reset =
                    $transition_state->[Marpa::Internal::AHFA::RESET_ORIGIN];
                my $origin =
                      $reset
                    ? $earleme_to_complete
                    : $parent;
                my $transition_state_id =
                    $transition_state->[Marpa::Internal::AHFA::ID];
                my $name = sprintf
                    ## no critic (ValuesAndExpressions::RequireInterpolationOfMetachars)
                    'S%d@%d-%d',
                    ## use critic
                    $transition_state_id, $origin, $earleme_to_complete;
                my $target_item = $earley_hash->{$name};
                if ( not defined $target_item ) {
                    $target_item = [];
                    $target_item->[Marpa::Internal::Earley_Item::NAME] =
                        $name;
                    $target_item->[Marpa::Internal::Earley_Item::STATE] =
                        $transition_state;
                    $target_item->[Marpa::Internal::Earley_Item::PARENT] =
                        $origin;
                    $target_item->[Marpa::Internal::Earley_Item::LEO_LINKS]  = [];
                    $target_item->[Marpa::Internal::Earley_Item::LINKS]  = [];
                    $target_item->[Marpa::Internal::Earley_Item::TOKENS] = [];
                    $target_item->[Marpa::Internal::Earley_Item::SET] =
                        $earleme_to_complete;
                    $earley_hash->{$name} = $target_item;
                    push @{$earley_set}, $target_item;
                }    # unless defined $target_item
                next TRANSITION_STATE if $reset;
                if ($leo_item) {
                    push @{ $target_item->[Marpa::Internal::Earley_Item::LEO_LINKS] },
                        [ $leo_item, $earley_item ];
                } else {
                    push @{ $target_item->[Marpa::Internal::Earley_Item::LINKS] },
                        [ $parent_item, $earley_item ];
                }
            }    # TRANSITION_STATE

        }    # PARENT_ITEM

    }    # EARLEY_ITEM

    if ( $too_many_earley_items >= 0
        and ( my $item_count = scalar @{$earley_set} )
        >= $too_many_earley_items )
    {
        if ( $recce->[Marpa::Internal::Recognizer::WARNINGS] ) {
            say {$Marpa::Internal::TRACE_FH}
                "Very large earley set: $item_count items at location $earleme_to_complete"
                or Marpa::exception("Cannot print: $ERRNO");
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
            or Marpa::exception("Cannot print: $ERRNO");
        print {$Marpa::Internal::TRACE_FH} Marpa::show_earley_set($earley_set)
            or Marpa::exception("Cannot print: $ERRNO");
    } ## end if ($trace_earley_sets)

    my $postdot_here;
    for my $earley_item ( @{$earley_set} ) {
        my $state  = $earley_item->[Marpa::Internal::Earley_Item::STATE];
        my $parent = $earley_item->[Marpa::Internal::Earley_Item::PARENT];
        while ( my ( $postdot_symbol_name, $next_states ) =
            each %{ $state->[Marpa::Internal::AHFA::TRANSITION] } )
        {
            push @{ $postdot_here->{$postdot_symbol_name} },
                [ $earley_item, $next_states, $parent ];
        } ## end while ( my ( $postdot_symbol_name, $next_states ) = each...)
    } ## end for my $earley_item ( @{$earley_set} )

    $leo_sets->[$earleme_to_complete] = [];
    my $leo_set = $leo_sets->[$earleme_to_complete];
    SYMBOL:
    while ( my ( $postdot_symbol, $postdot_data ) = each %{$postdot_here} ) {
        next SYMBOL if scalar @{$postdot_data} != 1;
        my $postdot_0 = $postdot_data->[0];
        my ( $base_earley_item, $next_states, $leo_parent ) = @{$postdot_0};
        my ( $leo_lhs, $leo_state ) = @{$next_states};

        # Only one transition in the Earley set on this symbol,
        # but it is not to a Leo completion.
        next SYMBOL if ref $leo_lhs;

        next SYMBOL if $leo_lhs ne $postdot_symbol;

        my $predecessor_leo_item;
        if ( $leo_parent < $earleme_to_complete ) {
            my $predecessor_postdot = $postdot->{$leo_parent}->{$leo_lhs};
            my ( $predecessor_earley_item, $predecessor_to_states,
                $predecessor_parent );
            (   $predecessor_earley_item, $predecessor_to_states,
                $predecessor_parent,      $predecessor_leo_item
            ) = @{ $predecessor_postdot->[0] };
            if ($predecessor_leo_item) {
                $leo_parent = $predecessor_parent;
                $leo_state =
                    List::Util::first {ref} @{$predecessor_to_states};
            }
        } ## end if ( $leo_parent < $earleme_to_complete )

        my $name     = sprintf
            ## no critic (ValuesAndExpressions::RequireInterpolationOfMetachars)
            'L%d@%d-%d',
            ## use critic
            $leo_state->[Marpa::Internal::AHFA::ID], $leo_parent,
            $earleme_to_complete;
        my $leo_item = [];
        $leo_item->[Marpa::Internal::Earley_Item::NAME]   = $name;
        $leo_item->[Marpa::Internal::Earley_Item::STATE]  = $leo_state;
        $leo_item->[Marpa::Internal::Earley_Item::PARENT] = $leo_parent;
        $leo_item->[Marpa::Internal::Earley_Item::SET] = $earleme_to_complete;
        $leo_item->[Marpa::Internal::Earley_Item::IS_LEO_ITEM] = 1;
        $leo_item->[Marpa::Internal::Earley_Item::TOKENS]      = [];
        $leo_item->[Marpa::Internal::Earley_Item::LINKS] =
            [ [ $predecessor_leo_item, $base_earley_item ] ];

        push @{$leo_set},   $leo_item;
        push @{$postdot_0}, $leo_item;
        $postdot_here->{$postdot_symbol} =
            [ [ $base_earley_item, [$leo_state], $leo_parent, $leo_item ] ];
    } ## end while ( my ( $postdot_symbol, $postdot_data ) = each %{...})

    # Insert code to add Leo items for prediction states here
    # Parent of prediction states is $earleme_to_complete.
    # -- Create a worklist of the symbols above.
    # -- Cycle through, looking for Leo items @ symbol @ $earleme_to_complete
    # -- As found, add Leo lhs to worklist

    $postdot->{$earleme_to_complete} = $postdot_here;

    if ($trace_terminals) {
        for my $terminal ( grep { $terminal_names->{$_} }
            keys %{ $postdot->{$earleme_to_complete} } )
        {
            say {$Marpa::Internal::TRACE_FH}
                qq{Expecting "$terminal" at $earleme_to_complete}
                or Marpa::exception("Cannot print: $ERRNO");
        } ## end for my $terminal ( grep { $terminal_names->{$_} } keys...)
    } ## end if ($trace_terminals)

    return $earleme_to_complete;

} ## end sub complete

1;
