package Marpa::Recognizer;

use 5.010;
use warnings;

# As of 9 Aug 2010 there's a problem with this perlcritic check
## no critic (TestingAndDebugging::ProhibitNoWarnings)
no warnings 'recursion';
## use critic

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
    LINKS { A list of the links from the completer step. }

    LEO_SYMBOL { A symbol name.
    Defined if and only if this is a Leo item. }

    LEO_ACTUAL_STATE { An AHFA state.
    Defined if and only if this is a Leo item. }

    LEO_LINKS { Leo Links source }

    =LAST_EVALUATOR_FIELD

    PARENT { The number of the Earley set with the parent item(s) }
    =ORIGIN { A synonym I prefer over PARENT. }
    SET { The set this item is in. For debugging. }

    =LAST_FIELD

);

# Elements of the RECOGNIZER structure
use Marpa::Offset qw(

    :package=Marpa::Internal::Recognizer

    GRAMMAR { the grammar used }
    EARLEY_SETS { the array of the Earley sets }
    FURTHEST_EARLEME { last earley set with something in it }
    LAST_COMPLETED_EARLEME
    FINISHED
    USE_LEO { Use Leo items? }

    TRACE_FILE_HANDLE

    END
    CLOSURES
    TRACE_ACTIONS
    TRACE_VALUES
    TRACE_TASKS
    TRACING
    MAX_PARSES
    NULL_VALUES
    RANKING_METHOD

    { The following fields must be reinitialized when
    evaluation is reset }

    SINGLE_PARSE_MODE
    PARSE_COUNT :{ number of parses in an ambiguous parse :}

    AND_NODES
    AND_NODE_HASH
    OR_NODES
    OR_NODE_HASH

    ITERATION_STACK

    EVALUATOR_RULES

    { This is the end of the list of fields which
    must be reinitialized when evaluation is reset }

    =LAST_EVALUATOR_FIELD

    EARLEY_HASH { Hash of the Earley items by Earley set.
    Used to prevent duplicates.  It is a hash by name
    to Earley item. }

    EXHAUSTED { parse can't continue? }

    LEO_SETS { An array. Indexed by AHFA state id.
    of hashes by symbol name to Leo items. }

    POSTDOT { An array. Indexed by AHFA state id.
    of hashes by symbol name to Earley item and to-states }

    TOO_MANY_EARLEY_ITEMS
    TRACE_EARLEY_SETS
    TRACE_TERMINALS
    WARNINGS

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
    my $recce = bless [], $class;

    my $grammar;
    ARG_HASH: for my $arg_hash (@arg_hashes) {
        if ( defined( $grammar = $arg_hash->{grammar} ) ) {
            delete $arg_hash->{grammar};
            last ARG_HASH;
        }
    } ## end for my $arg_hash (@arg_hashes)
    Marpa::exception('No grammar specified') if not defined $grammar;

    $recce->[Marpa::Internal::Recognizer::GRAMMAR] = $grammar;

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
        $recce->[Marpa::Internal::Recognizer::TRACE_FILE_HANDLE] =
        $grammar->[Marpa::Internal::Grammar::TRACE_FILE_HANDLE];
    $recce->[Marpa::Internal::Recognizer::WARNINGS] = 1;
    $recce->reset_evaluation();
    $recce->[Marpa::Internal::Recognizer::MODE]           = 'default';
    $recce->[Marpa::Internal::Recognizer::RANKING_METHOD] = 'none';
    $recce->[Marpa::Internal::Recognizer::USE_LEO]        = 1;
    $recce->[Marpa::Internal::Recognizer::MAX_PARSES]     = 0;

    $recce->set(@arg_hashes);

    if (    $grammar->[Marpa::Internal::Grammar::IS_INFINITE]
        and $recce->[Marpa::Internal::Recognizer::RANKING_METHOD] ne 'none'
        and not $grammar->[Marpa::Internal::Grammar::CYCLE_RANKING_ACTION] )
    {
        Marpa::exception(
            "The grammar cycles (is infinitely ambiguous)\n",
            "    but it has no 'cycle_ranking_action'.\n",
            "    Either rewrite the grammar to eliminate cycles\n",
            "    or define a 'cycle ranking action'\n"
        );
    } ## end if ( $grammar->[Marpa::Internal::Grammar::IS_INFINITE...])

    my $trace_terminals =
        $recce->[Marpa::Internal::Recognizer::TRACE_TERMINALS] // 0;
    my $trace_tasks = $recce->[Marpa::Internal::Recognizer::TRACE_TASKS] // 0;

    if (not
        defined $recce->[Marpa::Internal::Recognizer::TOO_MANY_EARLEY_ITEMS] )
    {
        my $AHFA_size =
            scalar @{ $grammar->[Marpa::Internal::Grammar::AHFA] };
        $recce->[Marpa::Internal::Recognizer::TOO_MANY_EARLEY_ITEMS] =
            List::Util::max( ( 2 * $AHFA_size ),
            Marpa::Internal::Recognizer::DEFAULT_TOO_MANY_EARLEY_ITEMS );
    } ## end if ( not defined $recce->[...])

    # Some of this processing -- to find terminals and Leo symbols
    # by state -- should perhaps be done in the grammar.

    my $terminal_names = $grammar->[Marpa::Internal::Grammar::TERMINAL_NAMES];

    my $AHFA        = $grammar->[Marpa::Internal::Grammar::AHFA];
    my $symbol_hash = $grammar->[Marpa::Internal::Grammar::SYMBOL_HASH];

    my $earley_set;

    my $start_states = $grammar->[Marpa::Internal::Grammar::START_STATES];
    my $postdot      = {};

    for my $state ( @{$start_states} ) {
        my $state_id = $state->[Marpa::Internal::AHFA::ID];
        my $name     = sprintf
            'S%d@%d-%d',
            $state_id, 0, 0;

        my $item;
        $item->[Marpa::Internal::Earley_Item::NAME]   = $name;
        $item->[Marpa::Internal::Earley_Item::STATE]  = $state;
        $item->[Marpa::Internal::Earley_Item::PARENT] = 0;
        $item->[Marpa::Internal::Earley_Item::LINKS]  = [];
        $item->[Marpa::Internal::Earley_Item::SET]    = 0;

        push @{$earley_set}, $item;

        while ( my ( $transition_symbol, $to_states ) =
            each %{ $state->[Marpa::Internal::AHFA::TRANSITION] } )
        {
            my @to_states = grep {ref} @{$to_states};
            push @{ $postdot->{0}->{$transition_symbol} },
                [ $item, \@to_states, 0 ];
        } ## end while ( my ( $transition_symbol, $to_states ) = each %{...})

    } ## end for my $state ( @{$start_states} )

    $recce->[Marpa::Internal::Recognizer::EARLEY_HASH] = {};

    $recce->[Marpa::Internal::Recognizer::GRAMMAR]     = $grammar;
    $recce->[Marpa::Internal::Recognizer::EARLEY_SETS] = [$earley_set];

    $recce->[Marpa::Internal::Recognizer::FURTHEST_EARLEME]       = 0;
    $recce->[Marpa::Internal::Recognizer::LAST_COMPLETED_EARLEME] = 0;

    $recce->[Marpa::Internal::Recognizer::POSTDOT] = $postdot;

    # Don't include the start states in the Leo sets.
    $recce->[Marpa::Internal::Recognizer::LEO_SETS] = [];

    if ( $trace_terminals > 1 ) {
        for my $terminal (
            grep { $terminal_names->{$_} }
            keys %{ $postdot->{0} }
            )
        {
            say {$Marpa::Internal::TRACE_FH}
                qq{Expecting "$terminal" at earleme 0}
                or Marpa::exception("Cannot print: $ERRNO");
        } ## end for my $terminal ( grep { $terminal_names->{$_} } keys...)
    } ## end if ( $trace_terminals > 1 )

    return $recce;
} ## end sub Marpa::Recognizer::new

use constant RECOGNIZER_OPTIONS => [
    qw{
        closures
        end
        leo
        max_parses
        mode
        ranking_method
        too_many_earley_items
        trace_actions
        trace_earley_sets
        trace_fh
        trace_file_handle
        trace_tasks
        trace_terminals
        trace_values
        warnings
        }
];

use constant RECOGNIZER_MODES => [qw(default stream)];

sub Marpa::Recognizer::reset_evaluation {
    my ($recce) = @_;
    $recce->[Marpa::Internal::Recognizer::PARSE_COUNT]       = 0;
    $recce->[Marpa::Internal::Recognizer::SINGLE_PARSE_MODE] = undef;
    $recce->[Marpa::Internal::Recognizer::AND_NODES]         = [];
    $recce->[Marpa::Internal::Recognizer::AND_NODE_HASH]     = {};
    $recce->[Marpa::Internal::Recognizer::OR_NODES]          = [];
    $recce->[Marpa::Internal::Recognizer::OR_NODE_HASH]      = {};
    $recce->[Marpa::Internal::Recognizer::ITERATION_STACK]   = [];
    $recce->[Marpa::Internal::Recognizer::EVALUATOR_RULES]   = [];
    return;
} ## end sub Marpa::Recognizer::reset_evaluation

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

        if ( defined( my $value = $args->{'leo'} ) ) {
            $recce->[Marpa::Internal::Recognizer::USE_LEO] = $value ? 1 : 0;
        }

        if ( defined( my $value = $args->{'max_parses'} ) ) {
            $recce->[Marpa::Internal::Recognizer::MAX_PARSES] = $value;
        }

        if ( defined( my $value = $args->{'mode'} ) ) {
            if ( not $value ~~ Marpa::Internal::Recognizer::RECOGNIZER_MODES )
            {
                Carp::croak( 'Unknown mode for Marpa Recognizer: ', $value );
            }
            $recce->[Marpa::Internal::Recognizer::MODE] = $value;
        } ## end if ( defined( my $value = $args->{'mode'} ) )

        if ( defined( my $value = $args->{'ranking_method'} ) ) {
            Marpa::exception(q{ranking_method must be 'constant' or 'none'})
                if not $value ~~ [qw(constant none)];
            $recce->[Marpa::Internal::Recognizer::RANKING_METHOD] = $value;
        }

        if ( defined( my $value = $args->{'trace_fh'} ) ) {
            $trace_fh =
                $recce->[Marpa::Internal::Recognizer::TRACE_FILE_HANDLE] =
                $value;
        }

        if ( defined( my $value = $args->{'trace_file_handle'} ) ) {
            $trace_fh =
                $recce->[Marpa::Internal::Recognizer::TRACE_FILE_HANDLE] =
                $value;
        }

        if ( defined( my $value = $args->{'trace_actions'} ) ) {
            $recce->[Marpa::Internal::Recognizer::TRACE_ACTIONS] = $value;
            ## Do not allow setting this option in recognizer for single parse mode
            $recce->[Marpa::Internal::Recognizer::SINGLE_PARSE_MODE] = 0;
            if ($value) {
                say {$trace_fh} 'Setting trace_actions option'
                    or Marpa::exception("Cannot print: $ERRNO");
                $recce->[Marpa::Internal::Recognizer::TRACING] = 1;
            }
        } ## end if ( defined( my $value = $args->{'trace_actions'} ))

        if ( defined( my $value = $args->{'trace_tasks'} ) ) {
            Marpa::exception('trace_tasks must be set to a number >= 0')
                if $value !~ /\A\d+\z/xms;
            $recce->[Marpa::Internal::Recognizer::TRACE_TASKS] = $value + 0;
            if ($value) {
                say {$trace_fh} "Setting trace_tasks option to $value"
                    or Marpa::exception("Cannot print: $ERRNO");
                $recce->[Marpa::Internal::Recognizer::TRACING] = 1;
            }
        } ## end if ( defined( my $value = $args->{'trace_tasks'} ) )

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

        if ( defined( my $value = $args->{'trace_values'} ) ) {
            $recce->[Marpa::Internal::Recognizer::TRACE_VALUES] = $value;
            ## Do not allow setting this option in recognizer for single parse mode
            $recce->[Marpa::Internal::Recognizer::SINGLE_PARSE_MODE] = 0;
            if ($value) {
                say {$trace_fh} 'Setting trace_values option'
                    or Marpa::exception("Cannot print: $ERRNO");
                $recce->[Marpa::Internal::Recognizer::TRACING] = 1;
            }
        } ## end if ( defined( my $value = $args->{'trace_values'} ) )

        if ( defined( my $value = $args->{'end'} ) ) {

            # Not allowed once parsing is started
            if ( $recce->[Marpa::Internal::Recognizer::PARSE_COUNT] > 0 ) {
                Marpa::exception(
                    q{Cannot reset end once parsing has started});
            }
            $recce->[Marpa::Internal::Recognizer::END] = $value;
            ## Do not allow setting this option in recognizer for single parse mode
            $recce->[Marpa::Internal::Recognizer::SINGLE_PARSE_MODE] = 0;
        } ## end if ( defined( my $value = $args->{'end'} ) )

        if ( defined( my $value = $args->{'closures'} ) ) {

            # Not allowed once parsing is started
            if ( $recce->[Marpa::Internal::Recognizer::PARSE_COUNT] > 0 ) {
                Marpa::exception(
                    q{Cannot reset end once parsing has started});
            }
            my $closures = $recce->[Marpa::Internal::Recognizer::CLOSURES] =
                $value;
            ## Do not allow setting this option in recognizer for single parse mode
            $recce->[Marpa::Internal::Recognizer::SINGLE_PARSE_MODE] = 0;
            while ( my ( $action, $closure ) = each %{$closures} ) {
                Marpa::exception(qq{Bad closure for action "$action"})
                    if ref $closure ne 'CODE';
            }
        } ## end if ( defined( my $value = $args->{'closures'} ) )

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

# Not intended to be documented.
# Returns the size of the lasy completed earley set.
# For testing, especially that the Leo items
# are doing their job.
sub Marpa::Recognizer::earley_set_size {
    my ( $recce, $name ) = @_;
    my $last_completed_earleme =
        $recce->[Marpa::Internal::Recognizer::LAST_COMPLETED_EARLEME];
    return if not defined $last_completed_earleme;
    my $earley_set = $recce->[EARLEY_SETS]->[$last_completed_earleme];
    return if not defined $earley_set;
    return scalar @{$earley_set};
} ## end sub Marpa::Recognizer::earley_set_size

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

sub Marpa::show_link_choice {
    my ($link) = @_;
    my ( $predecessor, $cause, $token_name, $value_ref ) = @{$link};
    my @pieces = ();
    if ($predecessor) {
        push @pieces,
            'p=' . $predecessor->[Marpa::Internal::Earley_Item::NAME];
    }
    if ( not defined $cause ) {
        push @pieces, "s=$token_name";
        my $token_dump = Data::Dumper->new( [$value_ref] )->Terse(1)->Dump;
        chomp $token_dump;
        push @pieces, "t=$token_dump";
    } ## end if ( not defined $cause )
    else {
        push @pieces, 'c=' . $link->[1]->[Marpa::Internal::Earley_Item::NAME];
    }
    return '[' . ( join '; ', @pieces ) . ']';
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
    my ($item)     = @_;
    my $links      = $item->[Marpa::Internal::Earley_Item::LINKS];
    my $leo_links  = $item->[Marpa::Internal::Earley_Item::LEO_LINKS];
    my $leo_symbol = $item->[Marpa::Internal::Earley_Item::LEO_SYMBOL];

    my $text = $item->[Marpa::Internal::Earley_Item::NAME];

    if ( defined $leo_symbol ) {
        my $actual_to_state = my $leo_state_id =
            $item->[Marpa::Internal::Earley_Item::LEO_ACTUAL_STATE]
            ->[Marpa::Internal::AHFA::ID];
        $text .= qq{; actual="$leo_symbol"->$leo_state_id;};
    } ## end if ( defined $leo_symbol )

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
    my ( $earley_set_list, $leo_set_list ) = @_;
    my $text             = q{};
    my $earley_set_count = @{$earley_set_list};
    LIST: for my $ix ( 0 .. $earley_set_count - 1 ) {
        my $set = $earley_set_list->[$ix];
        next LIST if not defined $set;
        $text .= "Earley Set $ix\n" . Marpa::show_earley_set($set);
        my $leo_set = $leo_set_list->[$ix];
        next LIST if not defined $leo_set;
        $text .= Marpa::show_earley_set($leo_set);
    } ## end for my $ix ( 0 .. $earley_set_count - 1 )
    return $text;
} ## end sub Marpa::show_earley_set_list

sub Marpa::Recognizer::show_earley_sets {
    my ($recce) = @_;
    my $last_completed_earleme = $recce->[LAST_COMPLETED_EARLEME]
        // 'stripped';
    my $furthest_earleme = $recce->[FURTHEST_EARLEME];
    my $earley_set_list  = $recce->[EARLEY_SETS];
    my $leo_set_list     = $recce->[LEO_SETS];

    return
          "Last Completed: $last_completed_earleme; "
        . "Furthest: $furthest_earleme\n"
        . Marpa::show_earley_set_list( $earley_set_list, $leo_set_list );

} ## end sub Marpa::Recognizer::show_earley_sets

use Marpa::Offset qw(

    :package=Marpa::Internal::Progress_Report

    RULE_ID
    POSITION
    ORIGIN
    CURRENT
    IS_LEO

);

sub Marpa::Recognizer::show_progress {
    my ( $recce, $start_ix, $end_ix ) = @_;
    my $earley_set_list = $recce->[Marpa::Internal::Recognizer::EARLEY_SETS];
    my $grammar         = $recce->[Marpa::Internal::Recognizer::GRAMMAR];
    my $rules           = $grammar->[Marpa::Internal::Grammar::RULES];
    my $last_ix         = $#{$earley_set_list};
    $start_ix //=
        $recce->[Marpa::Internal::Recognizer::LAST_COMPLETED_EARLEME];
    if ( $start_ix < 0 or $start_ix > $last_ix ) {
        return
            "Marpa::Recognizer::show_progress start index is $start_ix, must be in range 0-$last_ix";
    }
    $end_ix //= $start_ix;
    if ( $end_ix < 0 ) {
        $end_ix += $last_ix + 1;
    }
    if ( $end_ix < 0 or $end_ix > $last_ix ) {
        return
            "Marpa::Recognizer::show_progress end index is $end_ix, must be in range 0-$last_ix";
    }
    my $text = q{};
    for my $ix ( $start_ix .. $end_ix ) {
        my $earley_set = $earley_set_list->[$ix];
        my $reports    = report_progress($earley_set);
        my @sort_data;
        for my $report ( @{$reports} ) {
            my $rule_id =
                $report->[Marpa::Internal::Progress_Report::RULE_ID];
            my $rule = $rules->[$rule_id];
            my $position =
                $report->[Marpa::Internal::Progress_Report::POSITION];
            my $origin = $report->[Marpa::Internal::Progress_Report::ORIGIN];
            my $current =
                $report->[Marpa::Internal::Progress_Report::CURRENT];
            my $is_leo = $report->[Marpa::Internal::Progress_Report::IS_LEO];
            my $rhs_length = scalar @{ $rule->[Marpa::Internal::Rule::RHS] };
            my $item_text;
            my $type;

            # flag indicating whether we need to show the dot in the rule
            my $show_dotted;
            if ( $position >= $rhs_length ) {
                $item_text .= 'COMPLETED';
                $type = 3;
                $item_text .= q{ @} . $origin . q{-} . $current . q{ };
                $is_leo and $item_text .= '(Leo) ';
            } ## end if ( $position >= $rhs_length )
            elsif ($position) {
                $item_text .= 'BUILDING';
                $type = 2;
                $item_text .= q{ @} . $origin . q{-} . $current . q{ };
                $show_dotted++;
            } ## end elsif ($position)
            else {
                $item_text .= 'PREDICTING';
                $type = 1;
                $item_text .= q{ @} . $origin . q{ };
            }
            if ($show_dotted) {
                $item_text .= Marpa::show_dotted_rule( $rule, $position );
            }
            else {
                $item_text .= Marpa::brief_rule($rule);
            }
            push @sort_data,
                [
                ( pack 'N*', $type, $rule_id, $position, $origin ), $item_text
                ];
        } ## end for my $report ( @{$reports} )
        $text .= (
            join "\n", map { $_->[1] } sort { $a->[0] cmp $b->[0] } @sort_data
        ) . "\n";
    } ## end for my $ix ( $start_ix .. $end_ix )
    return $text;
} ## end sub Marpa::Recognizer::show_progress

sub report_progress {
    my ($earley_set) = @_;

    # Reports must be unique by a key
    # composted of original rule, rule position, and
    # location in the parse.  This hash is to
    # quarantee that.
    my %progress_report_hash = ();

    for my $earley_item ( @{$earley_set} ) {
        my $AHFA_state = $earley_item->[Marpa::Internal::Earley_Item::STATE];
        my $origin     = $earley_item->[Marpa::Internal::Earley_Item::PARENT];
        my $current    = $earley_item->[Marpa::Internal::Earley_Item::SET];
        my $leo_links =
            $earley_item->[Marpa::Internal::Earley_Item::LEO_LINKS];
        my $is_leo = $leo_links && scalar @{$leo_links};
        my $NFA_states = $AHFA_state->[Marpa::Internal::AHFA::NFA_STATES];
        if ( not $NFA_states ) {
            Marpa::exception(
                'Cannot progress of Marpa::Recognizer: it is stripped');
        }
        NFA_STATE: for my $NFA_state ( @{$NFA_states} ) {
            my $LR0_item   = $NFA_state->[Marpa::Internal::NFA::ITEM];
            my $marpa_rule = $LR0_item->[Marpa::Internal::LR0_item::RULE];
            my $marpa_position =
                $LR0_item->[Marpa::Internal::LR0_item::POSITION];
            my $original_rule =
                $marpa_rule->[Marpa::Internal::Rule::ORIGINAL_RULE]
                // $marpa_rule;
            my $original_rhs = $original_rule->[Marpa::Internal::Rule::RHS];

            # position in original rule, to be calculated
            my $original_position;
            if ( my $chaf_start =
                $marpa_rule->[Marpa::Internal::Rule::VIRTUAL_START] )
            {
                my $chaf_rhs = $marpa_rule->[Marpa::Internal::Rule::RHS];
                $original_position =
                    $marpa_position >= scalar @{$chaf_rhs}
                    ? scalar @{$original_rhs}
                    : ( $chaf_start + $marpa_position );
            } ## end if ( my $chaf_start = $marpa_rule->[...])
            $original_position //= $marpa_position;
            my $rule_id         = $original_rule->[Marpa::Internal::Rule::ID];
            my @data            = ( $rule_id, $original_position, $origin );
            my $key             = join q{;}, @data;
            my $progress_report = $progress_report_hash{$key};

            # If an entry already exists, just update the Leo flag
            if ( defined $progress_report ) {
                if ($is_leo) {
                    $progress_report
                        ->[Marpa::Internal::Progress_Report::IS_LEO] = 1;
                }
                next NFA_STATE;
            } ## end if ( defined $progress_report )
            $progress_report_hash{$key} = [ @data, $current, $is_leo ];
        } ## end for my $NFA_state ( @{$NFA_states} )
    } ## end for my $earley_item ( @{$earley_set} )
    return [ values %progress_report_hash ];
} ## end sub report_progress

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
        my $ref_type = ref $token_ix_ref;
        if ( ref $token_ix_ref ne 'SCALAR' ) {
            my $description = $ref_type ? "ref to $ref_type" : 'not a ref';
            Marpa::exception(
                "Token index arg for Marpa::Recognizer::tokens is $description, must be ref to SCALAR"
            );
        } ## end if ( ref $token_ix_ref ne 'SCALAR' )
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

    Marpa::exception('Attempt to scan tokens after parsing is finished')
        if $recce->[Marpa::Internal::Recognizer::FINISHED]
            and scalar @{$tokens};

    Marpa::exception('Attempt to scan tokens when parsing is exhausted')
        if $recce->[Marpa::Internal::Recognizer::EXHAUSTED]
            and scalar @{$tokens};

    # TOKEN PROCESSING PHASE

    my $terminal_names = $grammar->[Marpa::Internal::Grammar::TERMINAL_NAMES];

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

        # It's not 100% clear whether it's best to leave
        # the token_ix_ref pointing at the start of the
        # earleme, or at the actual problem token.
        # Right now, we set it at the actual problem
        # token, which is probably what will turn out
        # to be easiest.
        # my $first_ix_of_this_earleme = ${$token_ix_ref};

        TOKEN: while ( $current_token_earleme == $next_token_earleme ) {

            last TOKEN if not my $token_args = $tokens->[ ${$token_ix_ref} ];
            Marpa::exception(
                'Tokens must be array refs: token #',
                ${$token_ix_ref}, " is $token_args\n",
            ) if ref $token_args ne 'ARRAY';
            ${$token_ix_ref}++;
            my ( $symbol_name, $value, $length, $offset ) = @{$token_args};

            my $postdot      = $recce->[Marpa::Internal::Recognizer::POSTDOT];
            my $postdot_here = $postdot->{$current_token_earleme};

            Marpa::exception(
                "Attempt to add token '$symbol_name' at location where processing is complete:\n",
                "  Add attempted at $current_token_earleme\n",
                "  Processing complete to $last_completed_earleme\n"
            ) if $current_token_earleme < $last_completed_earleme;

            if (   not defined $symbol_name
                or not $terminal_names->{$symbol_name} )
            {
                local $Data::Dumper::Terse    = 1;
                local $Data::Dumper::Maxdepth = 1;
                my $problem =
                    defined $symbol_name
                    ? qq{Token name "$symbol_name" is not the name of a terminal symbol}
                    : q{The name of a terminal symbol was an undef};
                Marpa::exception( q{Fatal Problem with token: },
                    Data::Dumper::Dumper($token_args), $problem );
            } ## end if ( not defined $symbol_name or not $terminal_names...)

            # Make sure it's an allowed terminal symbol.
            my $postdot_data = $postdot_here->{$symbol_name};
            if ( not $postdot_data ) {
                if ( not $interactive ) {
                    Marpa::exception(
                        qq{Terminal "$symbol_name" received when not expected}
                    );
                }
                if ($trace_terminals) {
                    say {$trace_fh}
                        qq{Rejected "$symbol_name" at $last_completed_earleme}
                        or Marpa::exception("Cannot print: $ERRNO");
                }

                # Current token didn't actually work, so back out
                # the increment
                ${$token_ix_ref}--;

                return (
                    $last_completed_earleme,
                    [   grep { $terminal_names->{$_} }
                            keys %{ $postdot->{$last_completed_earleme} }
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

                my ( $earley_item, $to_states, $parent, $leo_item ) =
                    @{$postdot_data_entry};

                next EARLEY_ITEM if not $to_states;

                if ($trace_terminals) {
                    $accepted{$token_name} = $length;
                }

                # Create the kernel item and its link.
                my $target_ix = $last_completed_earleme + $length;
                if ( $target_ix > $furthest_earleme ) {
                    $furthest_earleme = $target_ix;
                }

                my $target_set = ( $earley_set_list->[$target_ix] //= [] );
                TO_STATE: for my $to_state ( grep {ref} @{$to_states} ) {
                    my $reset =
                        $to_state->[Marpa::Internal::AHFA::RESET_ORIGIN];
                    my $origin      = $reset ? $target_ix : $parent;
                    my $to_state_id = $to_state->[Marpa::Internal::AHFA::ID];
                    my $name        = sprintf
                        'S%d@%d-%d',
                        $to_state_id, $origin, $target_ix;

                    my $target_item = $earley_hash->{$name};
                    if ( not defined $target_item ) {
                        $target_item = [];
                        $target_item->[Marpa::Internal::Earley_Item::NAME] =
                            $name;
                        $target_item->[Marpa::Internal::Earley_Item::STATE] =
                            $to_state;
                        $target_item->[Marpa::Internal::Earley_Item::PARENT] =
                            $origin;
                        $target_item
                            ->[Marpa::Internal::Earley_Item::LEO_LINKS] = [];
                        $target_item->[Marpa::Internal::Earley_Item::LINKS] =
                            [];
                        $target_item->[Marpa::Internal::Earley_Item::SET] =
                            $target_ix;
                        $earley_hash->{$name} = $target_item;
                        push @{$target_set}, $target_item;

                    } ## end if ( not defined $target_item )

                    next TO_STATE if $reset;

                    if ($leo_item) {
                        push @{ $target_item
                                ->[Marpa::Internal::Earley_Item::LEO_LINKS] },
                            [ $leo_item, undef, $token_name, $value_ref ];
                    }
                    else {
                        push @{ $target_item
                                ->[Marpa::Internal::Earley_Item::LINKS] },
                            [ $earley_item, undef, $token_name, $value_ref ];
                    }
                }    # for my $to_state

            }    # ALTERNATIVE

        }    # EARLEY_ITEM

        if ($trace_terminals) {
            TOKEN: while ( my ( $token_name, $length ) = each %accepted ) {

                # The logic assumes that
                # length is non-zero only for accepted tokens
                if ( $length <= 0 ) {
                    say {$trace_fh}
                        qq{Rejected "$token_name" at $last_completed_earleme}
                        or Marpa::exception("Cannot print: $ERRNO");
                    next TOKEN;
                } ## end if ( $length <= 0 )

                say {$trace_fh}
                    qq{Accepted "$token_name" at $last_completed_earleme-}
                    . ( $length + $last_completed_earleme )
                    or Marpa::exception("Cannot print: $ERRNO")

            } ## end while ( my ( $token_name, $length ) = each %accepted )
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

    my $earley_hash    = $recce->[Marpa::Internal::Recognizer::EARLEY_HASH];
    my $terminal_names = $grammar->[Marpa::Internal::Grammar::TERMINAL_NAMES];
    my $postdot        = $recce->[Marpa::Internal::Recognizer::POSTDOT];
    my $leo_sets       = $recce->[Marpa::Internal::Recognizer::LEO_SETS];
    my $too_many_earley_items =
        $recce->[Marpa::Internal::Recognizer::TOO_MANY_EARLEY_ITEMS];
    my $trace_earley_sets =
        $recce->[Marpa::Internal::Recognizer::TRACE_EARLEY_SETS];
    my $trace_terminals =
        $recce->[Marpa::Internal::Recognizer::TRACE_TERMINALS] // 0;

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
            my ( $parent_item, $states, $grandparent, $leo_item ) =
                @{$parent_data};
            my $parent_state =
                $parent_item->[Marpa::Internal::Earley_Item::STATE];

            TRANSITION_STATE:
            for my $transition_state ( grep {ref} @{$states} ) {
                my $reset =
                    $transition_state->[Marpa::Internal::AHFA::RESET_ORIGIN];
                my $origin =
                      $reset
                    ? $earleme_to_complete
                    : $grandparent;
                my $transition_state_id =
                    $transition_state->[Marpa::Internal::AHFA::ID];
                my $name = sprintf
                    'S%d@%d-%d',
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
                    $target_item->[Marpa::Internal::Earley_Item::LEO_LINKS] =
                        [];
                    $target_item->[Marpa::Internal::Earley_Item::LINKS] = [];
                    $target_item->[Marpa::Internal::Earley_Item::SET] =
                        $earleme_to_complete;
                    $earley_hash->{$name} = $target_item;
                    push @{$earley_set}, $target_item;
                }    # unless defined $target_item
                next TRANSITION_STATE if $reset;
                if ($leo_item) {
                    push @{ $target_item
                            ->[Marpa::Internal::Earley_Item::LEO_LINKS] },
                        [ $leo_item, $earley_item ];
                }
                else {
                    push
                        @{ $target_item->[Marpa::Internal::Earley_Item::LINKS]
                        },
                        [ $parent_item, $earley_item ];
                } ## end else [ if ($leo_item) ]
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

    # Each possible cause
    # link is only visited once.
    # It may be paired with several different predecessors.
    # The cause may complete several different LHS symbols
    # and Marpa will seek predecessors for each at
    # the parent location.
    # Different completed LHS symbols might be postdot
    # symbols for the same predecessor Earley item.
    # For this reason,
    # predecessor-cause pairs might not be unique
    # within an Earley item.
    #
    # This is not an issue for unambiguous parsing.
    # It *IS* an issue for iterating ambiguous parses.

    if ($trace_earley_sets) {
        print {$Marpa::Internal::TRACE_FH}
            "=== Earley set $earleme_to_complete\n"
            or Marpa::exception("Cannot print: $ERRNO");
        print {$Marpa::Internal::TRACE_FH} Marpa::show_earley_set($earley_set)
            or Marpa::exception("Cannot print: $ERRNO");
    } ## end if ($trace_earley_sets)

    my $postdot_here = $postdot->{$earleme_to_complete} = {};
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

    my $leo_set = $leo_sets->[$earleme_to_complete] = [];
    my @leo_worklist =
        $recce->[Marpa::Internal::Recognizer::USE_LEO]
        ? ( keys %{$postdot_here}, ['prediction'] )
        : ();
    my %leo_triggers = ();
    my $leo_phase    = 'initial';
    SYMBOL:
    for ( ;; ) {
        my $postdot_symbol = shift @leo_worklist;
        if ( not defined $postdot_symbol ) {
            last SYMBOL if $leo_phase eq 'final';
            $leo_phase    = 'final';
            @leo_worklist = values %leo_triggers;
            next SYMBOL;
        } ## end if ( not defined $postdot_symbol )
        if ( ref $postdot_symbol ) {
            $leo_phase = $postdot_symbol->[0];
            next SYMBOL;
        }
        my $postdot_data = $postdot_here->{$postdot_symbol};
        next SYMBOL if scalar @{$postdot_data} != 1;
        my $postdot_0 = $postdot_data->[0];
        my ( $base_earley_item, $next_states, $leo_parent, $already_done ) =
            @{$postdot_0};

        # $already_done is actually the leo item.
        # It is true iff we've already added a leo item for this postdot symbol
        next SYMBOL if $already_done;

        my ( $leo_lhs, $leo_state ) = @{$next_states};

        # Only one transition in the Earley set on this symbol,
        # but it is not to a Leo completion.
        next SYMBOL if ref $leo_lhs;

        my $leo_actual_state = List::Util::first {ref} (
            @{  $base_earley_item->[Marpa::Internal::Earley_Item::STATE]
                    ->[Marpa::Internal::AHFA::TRANSITION]->{$postdot_symbol}
                }
        );

        # A flag that indicates if we working on a prediction
        # Leo item.  Set here because $leo_parent is changed
        # below.
        my $prediction = $leo_parent == $earleme_to_complete;

        my $predecessor_leo_item;
        FIND_LEO_ITEM_DATA: {

            my $predecessor_postdot = $postdot->{$leo_parent}->{$leo_lhs};
            (   undef,
                ( my $predecessor_to_states ),
                ( my $predecessor_parent ),
                $predecessor_leo_item
            ) = @{ $predecessor_postdot->[0] };

            if ($predecessor_leo_item) {
                $leo_parent = $predecessor_parent;
                $leo_state =
                    List::Util::first {ref} @{$predecessor_to_states};
                last FIND_LEO_ITEM_DATA;
            } ## end if ($predecessor_leo_item)

            # If here, we didn't find a predecessor Leo item.
            # That's ok in the final phase,
            # or if we are not working on a Leo prediction item.
            last FIND_LEO_ITEM_DATA
                if not $prediction
                    or $leo_phase eq 'final';

            # Set up so that when we do find the $leo_lhs,
            # we will 'trigger' this postdot symbol again.
            $leo_triggers{$leo_lhs} = $postdot_symbol;

            next SYMBOL;

        } ## end FIND_LEO_ITEM_DATA:

        my $name = sprintf
            'L%d@%d-%d',
            $leo_state->[Marpa::Internal::AHFA::ID], $leo_parent,
            $earleme_to_complete;
        my $leo_item = [];
        $leo_item->[Marpa::Internal::Earley_Item::NAME]   = $name;
        $leo_item->[Marpa::Internal::Earley_Item::STATE]  = $leo_state;
        $leo_item->[Marpa::Internal::Earley_Item::PARENT] = $leo_parent;
        $leo_item->[Marpa::Internal::Earley_Item::SET] = $earleme_to_complete;
        $leo_item->[Marpa::Internal::Earley_Item::LEO_SYMBOL] =
            $postdot_symbol;
        $leo_item->[Marpa::Internal::Earley_Item::LEO_ACTUAL_STATE] =
            $leo_actual_state;
        $leo_item->[Marpa::Internal::Earley_Item::LINKS] =
            [ [ $predecessor_leo_item, $base_earley_item ] ];

        push @{$leo_set},   $leo_item;
        push @{$postdot_0}, $leo_item;
        $postdot_here->{$postdot_symbol} =
            [ [ $base_earley_item, [$leo_state], $leo_parent, $leo_item ] ];

        # The rest of the processing is to deal with Leo prediction items.
        next SYMBOL if not $prediction;

        next SYMBOL
            if not my $triggered_symbol = $leo_triggers{$postdot_symbol};

        push @leo_worklist, $triggered_symbol;
        delete $leo_triggers{$postdot_symbol};

    } ## end for ( ;; )

    if ( $trace_terminals > 1 ) {
        for my $terminal (
            grep { $terminal_names->{$_} }
            keys %{ $postdot->{$earleme_to_complete} }
            )
        {
            say {$Marpa::Internal::TRACE_FH}
                qq{Expecting "$terminal" at $earleme_to_complete}
                or Marpa::exception("Cannot print: $ERRNO");
        } ## end for my $terminal ( grep { $terminal_names->{$_} } keys...)
    } ## end if ( $trace_terminals > 1 )

    return $earleme_to_complete;

} ## end sub complete

1;
