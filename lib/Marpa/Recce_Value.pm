package Marpa::Internal::Recce_Value;

use 5.010;
use warnings;
use strict;
use integer;

# our $DEBUG = 0;

use Scalar::Util;
use List::Util;
use English qw( -no_match_vars );
use Data::Dumper;
use Marpa::Internal;

# This perlcritic check is broken as of 9 Aug 2010
## no critic (TestingAndDebugging::ProhibitNoWarnings)
no warnings qw(qw);
## use critic

use Marpa::Offset qw(

    :package=Marpa::Internal::Or_Node

    ID
    TAG
    ITEMS
    RULE_ID
    POSITION
    AND_NODE_IDS

    SOURCE_OR_NODE { The name of the first grandparent or-node,
    for keeping track while populating the ITEMS element }

    CYCLE { Can this Or node be part of a cycle? }

    INITIAL_RANK_REF

    =LAST_FIELD
);

use Marpa::Offset qw(

    :package=Marpa::Internal::And_Node

    ID
    TAG
    RULE_ID
    TOKEN_NAME
    VALUE_REF
    VALUE_OPS

    { Fields before this (except ID)
    are used in evaluate() }

    PREDECESSOR_ID
    CAUSE_ID

    CAUSE_EARLEME

    INITIAL_RANK_REF
    CONSTANT_RANK_REF
    TOKEN_RANK_REF

    { These earleme positions will be needed for the callbacks: }

    START_EARLEME
    END_EARLEME

    POSITION { This is only used for diagnostics, but
    diagnostics are important. }

    =LAST_FIELD

);

use Marpa::Offset qw(

    :package=Marpa::Internal::Iteration_Node

    OR_NODE { The or-node }

    CHOICES {
    A list of remaining choices of and-node.
    The current choice is first in the list.
    }

    PARENT { Offset of the parent in the iterations stack }
    CAUSE_IX { Offset of the cause child, if any }
    PREDECESSOR_IX { Offset of the predecessor child, if any }

    CHILD_TYPE { Cause or Predecessor }

    RANK { Current rank }

);

use Marpa::Offset qw(

    :package=Marpa::Internal::Task

    CREATE_SUBTREE
    GRAFT_SUBTREE
    POPULATE_OR_NODE
    POPULATE_DEPTH
    INITIALIZE

    RANK_ALL

    ITERATE
    REDO_PREDECESSORS
    READJUST_STACK
    RE_RANK

);

use Marpa::Offset qw(

    :package=Marpa::Internal::Op

    :{ These are the valuation-time ops }
    ARGC
    CALL
    CONSTANT_RESULT
    VIRTUAL_HEAD
    VIRTUAL_HEAD_NO_SEP
    VIRTUAL_KERNEL
    VIRTUAL_TAIL

);

use Marpa::Offset qw(

    :package=Marpa::Internal::Choice

    { These are the valuation-time ops }

    AND_NODE
    RANK { *NOT* a rank ref }
    ITERATION_SUBTREE

);

use constant SKIP => -1;

use warnings;

sub Marpa::Recognizer::show_recce_and_node {
    my ( $recce, $and_node, $verbose ) = @_;
    $verbose //= 0;

    my $text = "show_recce_and_node:\n";

    my $grammar = $recce->[Marpa::Internal::Recognizer::GRAMMAR];
    my $rules   = $grammar->[Marpa::Internal::Grammar::RULES];

    my $name = $and_node->[Marpa::Internal::And_Node::TAG];
    my $id   = $and_node->[Marpa::Internal::And_Node::ID];
    my $predecessor_id =
        $and_node->[Marpa::Internal::And_Node::PREDECESSOR_ID];
    my $cause_id  = $and_node->[Marpa::Internal::And_Node::CAUSE_ID];
    my $value_ref = $and_node->[Marpa::Internal::And_Node::VALUE_REF];
    my $rule_id   = $and_node->[Marpa::Internal::And_Node::RULE_ID];
    my $position  = $and_node->[Marpa::Internal::And_Node::POSITION];

    my @rhs = ();

    my $rule          = $rules->[$rule_id];
    my $original_rule = $rule->[Marpa::Internal::Rule::ORIGINAL_RULE]
        // $rule;
    my $is_virtual_rule = $rule != $original_rule;

    my $or_nodes = $recce->[Marpa::Internal::Recognizer::OR_NODES];

    my $predecessor;
    if ($predecessor_id) {
        $predecessor = $or_nodes->[$predecessor_id];

        push @rhs, $predecessor->[Marpa::Internal::Or_Node::TAG]
            . "o$predecessor_id";
    }    # predecessor

    my $cause;
    if ($cause_id) {
        $cause = $or_nodes->[$cause_id];
        push @rhs, $cause->[Marpa::Internal::Or_Node::TAG] . "o$cause_id";
    }    # cause

    if ( defined $value_ref ) {
        my $value_as_string =
            Data::Dumper->new( [$value_ref] )->Terse(1)->Dump;
        chomp $value_as_string;
        push @rhs, $value_as_string;
    }    # value

    $text .= "R$rule_id:$position" . "a$id -> " . join( q{ }, @rhs ) . "\n";

    SHOW_RULE: {
        if ( $is_virtual_rule and $verbose >= 2 ) {
            $text
                .= '    rule '
                . $rule->[Marpa::Internal::Rule::ID] . ': '
                . Marpa::show_dotted_rule( $rule, $position + 1 )
                . "\n    "
                . Marpa::brief_virtual_rule( $rule, $position + 1 ) . "\n";
            last SHOW_RULE;
        } ## end if ( $is_virtual_rule and $verbose >= 2 )

        last SHOW_RULE if not $verbose;
        $text
            .= '    rule '
            . $rule->[Marpa::Internal::Rule::ID] . ': '
            . Marpa::brief_virtual_rule( $rule, $position + 1 ) . "\n";

    } ## end SHOW_RULE:

    if ( $verbose >= 2 ) {
        my @comment = ();
        if ( $and_node->[Marpa::Internal::And_Node::VALUE_OPS] ) {
            push @comment, 'value_ops';
        }
        if ( scalar @comment ) {
            $text .= q{    } . ( join q{, }, @comment ) . "\n";
        }
    } ## end if ( $verbose >= 2 )

    return $text;

} ## end sub Marpa::Recognizer::show_recce_and_node

sub Marpa::Recognizer::show_recce_or_node {
    my ( $recce, $or_node, $verbose, ) = @_;
    $verbose //= 0;

    my $grammar     = $recce->[Marpa::Internal::Recognizer::GRAMMAR];
    my $rules       = $grammar->[Marpa::Internal::Grammar::RULES];
    my $rule_id     = $or_node->[Marpa::Internal::Or_Node::RULE_ID];
    my $position    = $or_node->[Marpa::Internal::Or_Node::POSITION];
    my $or_node_id  = $or_node->[Marpa::Internal::Or_Node::ID];
    my $or_node_tag = $or_node->[Marpa::Internal::Or_Node::TAG];

    my $text = "show_recce_or_node o$or_node_id:\n";

    my $rule          = $rules->[$rule_id];
    my $original_rule = $rule->[Marpa::Internal::Rule::ORIGINAL_RULE]
        // $rule;
    my $is_virtual_rule = $rule != $original_rule;

    my $and_nodes = $recce->[Marpa::Internal::Recognizer::AND_NODES];

    LIST_AND_NODES: {
        my $and_node_ids = $or_node->[Marpa::Internal::Or_Node::AND_NODE_IDS];
        if ( not defined $and_node_ids ) {
            $text .= $or_node_tag . "o$or_node_id: UNPOPULATED\n";
            last LIST_AND_NODES;
        }
        if ( not scalar @{$and_node_ids} ) {
            $text .= $or_node_tag . "o$or_node_id: Empty and-node list!\n";
            last LIST_AND_NODES;
        }
        for my $index ( 0 .. $#{$and_node_ids} ) {
            my $and_node_id  = $and_node_ids->[$index];
            my $and_node     = $and_nodes->[$and_node_id];
            my $and_node_tag = $or_node_tag . "a$and_node_id";
            if ( $verbose >= 2 ) {
                $text .= $or_node_tag
                    . "o$or_node_id: $or_node_tag -> $and_node_tag\n";
            }
            $text .= $recce->show_recce_and_node( $and_node, $verbose );
        } ## end for my $index ( 0 .. $#{$and_node_ids} )
    } ## end LIST_AND_NODES:

    SHOW_RULE: {
        if ( $is_virtual_rule and $verbose >= 2 ) {
            $text
                .= '    rule '
                . $rule->[Marpa::Internal::Rule::ID] . ': '
                . Marpa::show_dotted_rule( $rule, $position + 1 )
                . "\n    "
                . Marpa::brief_virtual_rule( $rule, $position + 1 ) . "\n";
            last SHOW_RULE;
        } ## end if ( $is_virtual_rule and $verbose >= 2 )

        last SHOW_RULE if not $verbose;
        $text
            .= '    rule '
            . $rule->[Marpa::Internal::Rule::ID] . ': '
            . Marpa::brief_virtual_rule( $rule, $position + 1 ) . "\n";

    } ## end SHOW_RULE:

    return $text;

} ## end sub Marpa::Recognizer::show_recce_or_node

sub Marpa::brief_iteration_node {
    my ($iteration_node) = @_;

    my $or_node = $iteration_node->[Marpa::Internal::Iteration_Node::OR_NODE];
    my $or_node_id   = $or_node->[Marpa::Internal::Or_Node::ID];
    my $and_node_ids = $or_node->[Marpa::Internal::Or_Node::AND_NODE_IDS];
    my $text         = "o$or_node_id";
    DESCRIBE_CHOICES: {
        if ( not defined $and_node_ids ) {
            $text .= ' UNPOPULATED';
            last DESCRIBE_CHOICES;
        }
        my $choices =
            $iteration_node->[Marpa::Internal::Iteration_Node::CHOICES];
        if ( not defined $choices ) {
            $text .= ' Choices not initialized';
            last DESCRIBE_CHOICES;
        }
        my $choice = $choices->[0];
        if ( defined $choice ) {
            $text
                .= " [$choice] == a"
                . $choice->[Marpa::Internal::Choice::AND_NODE]
                ->[Marpa::Internal::And_Node::ID];
            last DESCRIBE_CHOICES;
        } ## end if ( defined $choice )
        $text .= "o$or_node_id has no choices left";
    } ## end DESCRIBE_CHOICES:
    my $parent_ix = $iteration_node->[Marpa::Internal::Iteration_Node::PARENT]
        // q{-};
    return "$text; p=$parent_ix";
} ## end sub Marpa::brief_iteration_node

sub Marpa::show_rank_ref {
    my ($rank_ref) = @_;
    return 'undef' if not defined $rank_ref;
    return 'SKIP'  if $rank_ref == Marpa::Internal::Recce_Value::SKIP;
    return ${$rank_ref};
} ## end sub Marpa::show_rank_ref

sub Marpa::Recognizer::show_iteration_node {
    my ( $recce, $iteration_node, $verbose ) = @_;

    my $or_node = $iteration_node->[Marpa::Internal::Iteration_Node::OR_NODE];
    my $or_node_id  = $or_node->[Marpa::Internal::Or_Node::ID];
    my $or_node_tag = $or_node->[Marpa::Internal::Or_Node::TAG];
    my $text        = "o$or_node_id $or_node_tag; ";
    given ( $iteration_node->[Marpa::Internal::Iteration_Node::CHILD_TYPE] ) {
        when (Marpa::Internal::And_Node::CAUSE_ID) {
            $text .= 'cause '
        }
        when (Marpa::Internal::And_Node::PREDECESSOR_ID) {
            $text .= 'predecessor '
        }
        default {
            $text .= '- '
        }
    } ## end given

    $text
        .= 'pr='
        . ( $iteration_node->[Marpa::Internal::Iteration_Node::PREDECESSOR_IX]
            // q{-} )
        . q{;c=}
        . ( $iteration_node->[Marpa::Internal::Iteration_Node::CAUSE_IX]
            // q{-} )
        . q{;p=}
        . ( $iteration_node->[Marpa::Internal::Iteration_Node::PARENT]
            // q{-} )
        . q{; rank=}
        . $iteration_node->[Marpa::Internal::Iteration_Node::RANK] . "\n";

    DESCRIBE_CHOICES: {
        my $and_node_ids = $or_node->[Marpa::Internal::Or_Node::AND_NODE_IDS];
        if ( not defined $and_node_ids ) {
            $text .= " UNPOPULATED\n";
            last DESCRIBE_CHOICES;
        }
        my $choices =
            $iteration_node->[Marpa::Internal::Iteration_Node::CHOICES];
        if ( not defined $choices ) {
            $text .= " Choices not initialized\n";
            last DESCRIBE_CHOICES;
        }
        if ( not scalar @{$choices} ) {
            $text .= " has no choices left\n";
            last DESCRIBE_CHOICES;
        }
        for my $choice_ix ( 0 .. $#{$choices} ) {
            my $choice = $choices->[$choice_ix];
            $text .= " o$or_node_id" . '[' . $choice_ix . '] ';
            my $and_node     = $choice->[Marpa::Internal::Choice::AND_NODE];
            my $and_node_tag = $and_node->[Marpa::Internal::And_Node::TAG];
            my $and_node_id  = $and_node->[Marpa::Internal::And_Node::ID];
            $text .= " ::= a$and_node_id $and_node_tag";
            no integer;
            $verbose
                and $text
                .= q{; rank=} . $choice->[Marpa::Internal::Choice::RANK];
            $text .= "\n";
            last CHOICE if not $verbose;
        } ## end for my $choice_ix ( 0 .. $#{$choices} )
    } ## end DESCRIBE_CHOICES:
    return $text;
} ## end sub Marpa::Recognizer::show_iteration_node

sub Marpa::Recognizer::show_iteration_stack {
    my ( $recce, $verbose ) = @_;
    my $iteration_stack =
        $recce->[Marpa::Internal::Recognizer::ITERATION_STACK];
    my $text = q{};
    for my $ix ( 0 .. $#{$iteration_stack} ) {
        my $iteration_node = $iteration_stack->[$ix];
        $text .= "$ix: "
            . $recce->show_iteration_node( $iteration_node, $verbose );
    }
    return $text;
} ## end sub Marpa::Recognizer::show_iteration_stack

package Marpa::Internal::Recognizer;
our $DEFAULT_ACTION_VALUE = \undef;

package Marpa::Internal::Recce_Value;

sub Marpa::Internal::Recognizer::set_null_values {
    my ($recce)      = @_;
    my $grammar      = $recce->[Marpa::Internal::Recognizer::GRAMMAR];
    my $trace_values = $recce->[Marpa::Internal::Recognizer::TRACE_VALUES];

    my $rules   = $grammar->[Marpa::Internal::Grammar::RULES];
    my $symbols = $grammar->[Marpa::Internal::Grammar::SYMBOLS];
    my $default_null_value =
        $grammar->[Marpa::Internal::Grammar::DEFAULT_NULL_VALUE];

    my $null_values;
    $#{$null_values} = $#{$symbols};

    SYMBOL: for my $symbol ( @{$symbols} ) {
        next SYMBOL if not $symbol->[Marpa::Internal::Symbol::NULLING];

        my $null_value = undef;
        if ( $symbol->[Marpa::Internal::Symbol::NULL_VALUE] ) {
            $null_value = ${ $symbol->[Marpa::Internal::Symbol::NULL_VALUE] };
        }
        else {
            $null_value = $default_null_value;
        }
        next SYMBOL if not defined $null_value;

        my $symbol_id = $symbol->[Marpa::Internal::Symbol::ID];
        $null_values->[$symbol_id] = $null_value;

        if ($trace_values) {
            print {$Marpa::Internal::TRACE_FH}
                'Setting null value for symbol ',
                $symbol->[Marpa::Internal::Symbol::NAME],
                ' to ', Data::Dumper->new( [ \$null_value ] )->Terse(1)->Dump
                or Marpa::exception('Could not print to trace file');
        } ## end if ($trace_values)

    } ## end for my $symbol ( @{$symbols} )

    return $null_values;

}    # set_null_values

# Given the grammar and an action name, resolve it to a closure,
# or return undef
sub Marpa::Internal::Recognizer::resolve_semantics {
    my ( $recce, $closure_name ) = @_;
    my $grammar       = $recce->[Marpa::Internal::Recognizer::GRAMMAR];
    my $closures      = $recce->[Marpa::Internal::Recognizer::CLOSURES];
    my $trace_actions = $recce->[Marpa::Internal::Recognizer::TRACE_ACTIONS];

    Marpa::exception(q{Trying to resolve 'undef' as closure name})
        if not defined $closure_name;

    if ( my $closure = $closures->{$closure_name} ) {
        if ($trace_actions) {
            print {$Marpa::Internal::TRACE_FH}
                qq{Resolved "$closure_name" to explicit closure\n}
                or Marpa::exception('Could not print to trace file');
        }

        return $closure;
    } ## end if ( my $closure = $closures->{$closure_name} )

    my $fully_qualified_name;
    DETERMINE_FULLY_QUALIFIED_NAME: {
        if ( $closure_name =~ /([:][:])|[']/xms ) {
            $fully_qualified_name = $closure_name;
            last DETERMINE_FULLY_QUALIFIED_NAME;
        }
        if (defined(
                my $actions_package =
                    $grammar->[Marpa::Internal::Grammar::ACTIONS]
            )
            )
        {
            $fully_qualified_name = $actions_package . q{::} . $closure_name;
            last DETERMINE_FULLY_QUALIFIED_NAME;
        } ## end if ( defined( my $actions_package = $grammar->[...]))

        if (defined(
                my $action_object_class =
                    $grammar->[Marpa::Internal::Grammar::ACTION_OBJECT]
            )
            )
        {
            $fully_qualified_name =
                $action_object_class . q{::} . $closure_name;
        } ## end if ( defined( my $action_object_class = $grammar->[...]))
    } ## end DETERMINE_FULLY_QUALIFIED_NAME:

    return if not defined $fully_qualified_name;

    no strict 'refs';
    my $closure = *{$fully_qualified_name}{'CODE'};
    use strict 'refs';

    if ($trace_actions) {
        print {$Marpa::Internal::TRACE_FH}
            ( $closure ? 'Successful' : 'Failed' )
            . qq{ resolution of "$closure_name" },
            'to ', $fully_qualified_name, "\n"
            or Marpa::exception('Could not print to trace file');
    } ## end if ($trace_actions)

    return $closure;

} ## end sub Marpa::Internal::Recognizer::resolve_semantics

sub Marpa::Internal::Recognizer::set_actions {
    my ($recce) = @_;
    my $grammar = $recce->[Marpa::Internal::Recognizer::GRAMMAR];

    my ( $rules, $default_action, ) = @{$grammar}[
        Marpa::Internal::Grammar::RULES,
        Marpa::Internal::Grammar::DEFAULT_ACTION,
    ];

    my $evaluator_rules = [];

    my $default_action_closure;
    if ( defined $default_action ) {
        $default_action_closure =
            Marpa::Internal::Recognizer::resolve_semantics( $recce,
            $default_action );
        Marpa::exception(
            "Could not resolve default action named '$default_action'")
            if not $default_action_closure;
    } ## end if ( defined $default_action )

    RULE: for my $rule ( @{$rules} ) {

        next RULE if not $rule->[Marpa::Internal::Rule::USED];

        my $rule_id = $rule->[Marpa::Internal::Rule::ID];
        my $ops = $evaluator_rules->[$rule_id] = [];

        my $virtual_rhs = $rule->[Marpa::Internal::Rule::VIRTUAL_RHS];
        my $virtual_lhs = $rule->[Marpa::Internal::Rule::VIRTUAL_LHS];

        if ($virtual_lhs) {
            push @{$ops},
                (
                $virtual_rhs
                ? Marpa::Internal::Op::VIRTUAL_KERNEL
                : Marpa::Internal::Op::VIRTUAL_TAIL
                ),
                $rule->[Marpa::Internal::Rule::REAL_SYMBOL_COUNT];
            next RULE;
        } ## end if ($virtual_lhs)

        # If we are here the LHS is real, not virtual

        if ($virtual_rhs) {
            push @{$ops},
                (
                $rule->[Marpa::Internal::Rule::DISCARD_SEPARATION]
                ? Marpa::Internal::Op::VIRTUAL_HEAD_NO_SEP
                : Marpa::Internal::Op::VIRTUAL_HEAD
                ),
                $rule->[Marpa::Internal::Rule::REAL_SYMBOL_COUNT];
        } ## end if ($virtual_rhs)
            # assignment instead of comparison is deliberate
        elsif ( my $argc = scalar @{ $rule->[Marpa::Internal::Rule::RHS] } ) {
            push @{$ops}, Marpa::Internal::Op::ARGC, $argc;
        }

        if ( my $action = $rule->[Marpa::Internal::Rule::ACTION] ) {
            my $closure =
                Marpa::Internal::Recognizer::resolve_semantics( $recce,
                $action );

            Marpa::exception(qq{Could not resolve action name: "$action"})
                if not defined $closure;
            push @{$ops}, Marpa::Internal::Op::CALL, $closure;
            next RULE;
        } ## end if ( my $action = $rule->[Marpa::Internal::Rule::ACTION...])

        # Try to resolve the LHS as a closure name,
        # if it is not internal.
        # If we can't resolve
        # the LHS as a closure name, it's not
        # a fatal error.
        if ( my $action =
            $rule->[Marpa::Internal::Rule::LHS]
            ->[Marpa::Internal::Symbol::NAME] )
        {
            if ($action !~ /[\]] \z/xms
                and defined(
                    my $closure =
                        Marpa::Internal::Recognizer::resolve_semantics(
                        $recce, $action
                        )
                )
                )
            {
                push @{$ops}, Marpa::Internal::Op::CALL, $closure;
                next RULE;
            } ## end if ( $action !~ /[\]] \z/xms and defined( my $closure...)[)
        } ## end if ( my $action = $rule->[Marpa::Internal::Rule::LHS...])

        if ( defined $default_action_closure ) {
            push @{$ops}, Marpa::Internal::Op::CALL, $default_action_closure;
            next RULE;
        }

        # If there is no default action specified, the fallback
        # is to return an undef
        push @{$ops}, Marpa::Internal::Op::CONSTANT_RESULT,
            $Marpa::Internal::Recognizer::DEFAULT_ACTION_VALUE;

    } ## end for my $rule ( @{$rules} )

    return $evaluator_rules;

}    # set_actions

# Returns false if no parse
sub do_rank_all {
    my ( $recce, $depth_by_id ) = @_;
    my $grammar = $recce->[Marpa::Internal::Recognizer::GRAMMAR];
    my $symbols = $grammar->[Marpa::Internal::Grammar::SYMBOLS];
    my $rules   = $grammar->[Marpa::Internal::Grammar::RULES];

    my $cycle_ranking_action =
        $grammar->[Marpa::Internal::Grammar::CYCLE_RANKING_ACTION];
    my $cycle_closure;
    if ( defined $cycle_ranking_action ) {
        $cycle_closure =
            Marpa::Internal::Recognizer::resolve_semantics( $recce,
            $cycle_ranking_action );
        Marpa::exception(
            "Could not resolve cycle ranking action named '$cycle_ranking_action'"
        ) if not $cycle_closure;
    } ## end if ( defined $cycle_ranking_action )

    # Set up rank closures by symbol
    my %ranking_closures_by_symbol = ();
    SYMBOL: for my $symbol ( @{$symbols} ) {
        my $ranking_action =
            $symbol->[Marpa::Internal::Symbol::RANKING_ACTION];
        next SYMBOL if not defined $ranking_action;
        my $ranking_closure =
            Marpa::Internal::Recognizer::resolve_semantics( $recce,
            $ranking_action );
        my $symbol_name = $symbol->[Marpa::Internal::Symbol::NAME];
        Marpa::exception(
            "Could not resolve ranking action for symbol.\n",
            qq{    Symbol was "$symbol_name".},
            qq{    Ranking action was "$ranking_action".}
        ) if not defined $ranking_closure;
        $ranking_closures_by_symbol{$symbol_name} = $ranking_closure;
    }    # end for my $symbol ( @{$symbols} )

    # Get closure used in ranking, by rule
    my @ranking_closures_by_rule = ();
    RULE: for my $rule ( @{$rules} ) {

        my $ranking_action = $rule->[Marpa::Internal::Rule::RANKING_ACTION];
        my $ranking_closure;
        my $cycle_rule = $rule->[Marpa::Internal::Rule::CYCLE];

        Marpa::exception(
            "Rule which cycles has an explicit ranking action\n",
            qq{   The ranking action is "$ranking_action"\n},
            qq{   To solve this problem,\n},
            qq{   Rewrite the grammar so that this rule does not cycle\n},
            qq{   Or eliminate its ranking action.\n}
        ) if $ranking_action and $cycle_rule;

        if ($ranking_action) {
            $ranking_closure =
                Marpa::Internal::Recognizer::resolve_semantics( $recce,
                $ranking_action );
            Marpa::exception("Ranking closure '$ranking_action' not found")
                if not defined $ranking_closure;
        } ## end if ($ranking_action)

        if ($cycle_rule) {
            $ranking_closure = $cycle_closure;
        }

        next RULE if not $ranking_closure;

        # If the RHS is empty ...
        # Empty rules are never in cycles -- they are either
        # unused (because of the CHAF rewrite) or the special
        # null start rule.
        if ( not scalar @{ $rule->[Marpa::Internal::Rule::RHS] } ) {
            Marpa::exception("Ranking closure '$ranking_action' not found")
                if not defined $ranking_closure;

            $ranking_closures_by_symbol{ $rule->[Marpa::Internal::Rule::LHS]
                    ->[Marpa::Internal::Symbol::NULL_ALIAS]
                    ->[Marpa::Internal::Symbol::NAME] } = $ranking_closure;
        } ## end if ( not scalar @{ $rule->[Marpa::Internal::Rule::RHS...]})

        next RULE if not $rule->[Marpa::Internal::Rule::USED];

        $ranking_closures_by_rule[ $rule->[Marpa::Internal::Rule::ID] ] =
            $ranking_closure;

    } ## end for my $rule ( @{$rules} )

    my $and_nodes = $recce->[Marpa::Internal::Recognizer::AND_NODES];
    my $or_nodes  = $recce->[Marpa::Internal::Recognizer::OR_NODES];

    my @and_node_worklist = ();
    AND_NODE: for my $and_node_id ( 0 .. $#{$and_nodes} ) {

        my $and_node     = $and_nodes->[$and_node_id];
        my $rule_id      = $and_node->[Marpa::Internal::And_Node::RULE_ID];
        my $rule_closure = $ranking_closures_by_rule[$rule_id];
        my $token_name   = $and_node->[Marpa::Internal::And_Node::TOKEN_NAME];
        my $token_closure;
        if ($token_name) {
            $token_closure = $ranking_closures_by_symbol{$token_name};
        }

        my $token_rank_ref;
        my $rule_rank_ref;

        # It is a feature of the ranking closures that they are always
        # called once per instance, even if the result is never used.
        # This sometimes makes for unnecessary calls,
        # but it makes these closures predictable enough
        # to allow their use for side effects.
        EVALUATION:
        for my $evaluation_data (
            [ \$token_rank_ref, $token_closure ],
            [ \$rule_rank_ref,  $rule_closure ]
            )
        {
            my ( $rank_ref_ref, $closure ) = @{$evaluation_data};
            next EVALUATION if not defined $closure;

            my @warnings;
            my $eval_ok;
            my $rank_ref;
            DO_EVAL: {
                local $Marpa::Internal::CONTEXT = [ 'and-node', $and_node ];
                local $SIG{__WARN__} =
                    sub { push @warnings, [ $_[0], ( caller 0 ) ]; };
                $eval_ok = eval { $rank_ref = $closure->(); 1; };
            } ## end DO_EVAL:

            my $fatal_error;
            CHECK_FOR_ERROR: {
                if ( not $eval_ok or scalar @warnings ) {
                    $fatal_error = $EVAL_ERROR // 'Fatal Error';
                    last CHECK_FOR_ERROR;
                }
                if ( defined $rank_ref and not ref $rank_ref ) {
                    $fatal_error =
                        "Invalid return value from ranking closure: $rank_ref";
                }
            } ## end CHECK_FOR_ERROR:

            if ( defined $fatal_error ) {

                Marpa::Internal::code_problems(
                    {   fatal_error => $fatal_error,
                        grammar     => $grammar,
                        eval_ok     => $eval_ok,
                        warnings    => \@warnings,
                        where       => 'ranking and-node '
                            . $and_node->[Marpa::Internal::And_Node::TAG],
                    }
                );
            } ## end if ( defined $fatal_error )

            ${$rank_ref_ref} = $rank_ref
                // Marpa::Internal::Recce_Value::SKIP;

        } ## end for my $evaluation_data ( [ \$token_rank_ref, $token_closure...])

# say STDERR __LINE__, " DEBUG a$and_node_id token name: ", ( $token_name // "undef" );

        # Set the token rank if there is a token.
        # It is zero if there is no token, or
        # if there is one with no closure.
        # Note: token can never cause a cycle, but they
        # can cause an and-node to be skipped.
        if ($token_name) {
            $and_node->[Marpa::Internal::And_Node::TOKEN_RANK_REF] =
                $token_rank_ref // \0;
        }

        # See if we can set the rank for this node to a constant.
        my $constant_rank_ref;
        SET_CONSTANT_RANK: {

            if ( defined $token_rank_ref && !ref $token_rank_ref ) {
                $constant_rank_ref = Marpa::Internal::Recce_Value::SKIP;
                last SET_CONSTANT_RANK;
            }

            # If we have ranking closure for this rule, the rank
            # is constant:
            # 0 for a non-final node,
            # the result of the closure for a final one
            if ( defined $rule_rank_ref ) {
                $constant_rank_ref =
                      $and_node->[Marpa::Internal::And_Node::VALUE_OPS]
                    ? $rule_rank_ref
                    : \0;
                last SET_CONSTANT_RANK;
            } ## end if ( defined $rule_rank_ref )

            # It there is a token and no predecessor, the rank
            # of this rule is a constant:
            # 0 is there was not token symbol closure
            # the result of that closure if there was one
            if ( $token_name
                and not
                defined $and_node->[Marpa::Internal::And_Node::PREDECESSOR_ID]
                )
            {
                $constant_rank_ref = $token_rank_ref // \0;
            } ## end if ( $token_name and not defined $and_node->[...])

        } ## end SET_CONSTANT_RANK:

        if ( defined $constant_rank_ref ) {
            $and_node->[Marpa::Internal::And_Node::INITIAL_RANK_REF] =
                $and_node->[Marpa::Internal::And_Node::CONSTANT_RANK_REF] =
                $constant_rank_ref;

# say STDERR "DEBUG line ", __LINE__, # DEBUG
            # ": Setting INITIAL_RANK of a$and_node_id to ", # DEBUG
            # $and_node->[Marpa::Internal::And_Node::ID], " ", # DEBUG
            # Marpa::show_rank_ref($constant_rank_ref); # DEBUG

            next AND_NODE;
        } ## end if ( defined $constant_rank_ref )

        # If we are here there is (so far) no constant rank
        # so we stack this and-node for depth-sensitive evaluation
        push @and_node_worklist, $and_node_id;

    } ## end for my $and_node_id ( 0 .. $#{$and_nodes} )

    # Now go through the and-nodes that require context to be ranked
    # This loop assumes that all cycles has been taken care of
    # with constant ranks
    AND_NODE: while ( defined( my $and_node_id = pop @and_node_worklist ) ) {

        no integer;

        # say STDERR "DEBUG: Ranking a$and_node_id";

        my $and_node = $and_nodes->[$and_node_id];

        # Go to next if we have already ranked this and-node
# say STDERR "rank for a$and_node_id already defined" if defined $and_node->[Marpa::Internal::And_Node::INITIAL_RANK_REF];
        next AND_NODE
            if
            defined $and_node->[Marpa::Internal::And_Node::INITIAL_RANK_REF];

        # The rank calculated so far from the
        # children
        my $calculated_rank = 0;

        my $is_cycle = 0;
        my $is_skip  = 0;
        OR_NODE:
        for my $field (
            Marpa::Internal::And_Node::PREDECESSOR_ID,
            Marpa::Internal::And_Node::CAUSE_ID,
            )
        {
            my $or_node_id = $and_node->[$field];
            next OR_NODE if not defined $or_node_id;

# say STDERR "DEBUG Ranking o$or_node_id";

            my $or_node = $or_nodes->[$or_node_id];
            if (defined(
                    my $or_node_initial_rank_ref =
                        $or_node->[Marpa::Internal::Or_Node::INITIAL_RANK_REF]
                )
                )
            {
                if ( ref $or_node_initial_rank_ref ) {
                    $calculated_rank += ${$or_node_initial_rank_ref};
                    next OR_NODE;
                }

                # At this point only possible value is skip
                $and_node->[Marpa::Internal::And_Node::INITIAL_RANK_REF] =
                    $and_node->[Marpa::Internal::And_Node::CONSTANT_RANK_REF]
                    = Marpa::Internal::Recce_Value::SKIP;

                # say STDERR "DEBUG line ", __LINE__,
                # ": Setting INITIAL_RANK of a$and_node_id to SKIP";

                next AND_NODE;
            } ## end if ( defined( my $or_node_initial_rank_ref = $or_node...))
            my @ranks              = ();
            my @unranked_and_nodes = ();
            CHILD_AND_NODE:
            for my $child_and_node_id (
                @{ $or_node->[Marpa::Internal::Or_Node::AND_NODE_IDS] } )
            {
                my $rank_ref =
                    $and_nodes->[$child_and_node_id]
                    ->[Marpa::Internal::And_Node::INITIAL_RANK_REF];
                if ( not defined $rank_ref ) {
                    push @unranked_and_nodes, $child_and_node_id;

# say STDERR "rank for a$child_and_node_id not defined -- re-adding to worklist";

                    next CHILD_AND_NODE;
                } ## end if ( not defined $rank_ref )

                # Right now the only defined scalar value for a rank is
                # Marpa::Internal::Recce_Value::SKIP
                next CHILD_AND_NODE if not ref $rank_ref;

                push @ranks, ${$rank_ref};

            } ## end for my $child_and_node_id ( @{ $or_node->[...]})

            # If we have unranked child and nodes, those have to be
            # ranked first.  Schedule the work and move on.
            if ( scalar @unranked_and_nodes ) {

# say STDERR "DEBUG Adding and-nodes back to worklist: ", join ", ", $and_node_id, @unranked_and_nodes;
                push @and_node_worklist, $and_node_id, @unranked_and_nodes;
                next AND_NODE;
            } ## end if ( scalar @unranked_and_nodes )

            # If there were no non-skipped and-nodes, the
            # parent and-node must also be skipped
            if ( not scalar @ranks ) {
                $or_node->[Marpa::Internal::Or_Node::INITIAL_RANK_REF] =
                    $and_node->[Marpa::Internal::And_Node::INITIAL_RANK_REF] =
                    $and_node->[Marpa::Internal::And_Node::CONSTANT_RANK_REF]
                    = Marpa::Internal::Recce_Value::SKIP;

                # say STDERR "DEBUG line ", __LINE__,
                # ": Setting INITIAL_RANK of a$and_node_id to SKIP";

                next AND_NODE;
            } ## end if ( not scalar @ranks )

            my $or_calculated_rank = List::Util::max @ranks;
            $or_node->[Marpa::Internal::Or_Node::INITIAL_RANK_REF] =
                \$or_calculated_rank;
            $calculated_rank += $or_calculated_rank;

            # say STDERR
            # "DEBUG Setting INITIAL_RANK of o$or_node_id to $calculated_rank, line ",
            #    __LINE__;

        } ## end for my $field ( Marpa::Internal::And_Node::PREDECESSOR_ID...)

        my $token_rank_ref =
            $and_node->[Marpa::Internal::And_Node::TOKEN_RANK_REF];
        $calculated_rank += defined $token_rank_ref ? ${$token_rank_ref} : 0;
        $and_node->[Marpa::Internal::And_Node::INITIAL_RANK_REF] =
            \$calculated_rank;

        # say STDERR
        # "DEBUG Setting INITIAL_RANK of a$and_node_id to $calculated_rank, line ",
        # __LINE__;

    } ## end while ( defined( my $and_node_id = pop @and_node_worklist...))

    return;

} ## end sub do_rank_all

# Does not modify stack
sub Marpa::Internal::Recognizer::evaluate {
    my ( $recce, $stack ) = @_;
    my $grammar      = $recce->[Marpa::Internal::Recognizer::GRAMMAR];
    my $trace_values = $recce->[Marpa::Internal::Recognizer::TRACE_VALUES]
        // 0;

    my $rules = $grammar->[Marpa::Internal::Grammar::RULES];
    my $action_object_class =
        $grammar->[Marpa::Internal::Grammar::ACTION_OBJECT];

    my $action_object_constructor;
    if ( defined $action_object_class ) {
        my $constructor_name = $action_object_class . q{::new};
        my $closure = Marpa::Internal::Recognizer::resolve_semantics( $recce,
            $constructor_name );
        Marpa::exception(qq{Could not find constructor "$constructor_name"})
            if not defined $closure;
        $action_object_constructor = $closure;
    } ## end if ( defined $action_object_class )

    my $action_object;
    if ($action_object_constructor) {
        my @warnings;
        my $eval_ok;
        my $fatal_error;
        DO_EVAL: {
            local $EVAL_ERROR = undef;
            local $SIG{__WARN__} = sub {
                push @warnings, [ $_[0], ( caller 0 ) ];
            };

            $eval_ok = eval {
                $action_object =
                    $action_object_constructor->($action_object_class);
                1;
            };
            $fatal_error = $EVAL_ERROR;
        } ## end DO_EVAL:

        if ( not $eval_ok or @warnings ) {
            Marpa::Internal::code_problems(
                {   fatal_error => $fatal_error,
                    grammar     => $grammar,
                    eval_ok     => $eval_ok,
                    warnings    => \@warnings,
                    where       => 'constructing action object',
                }
            );
        } ## end if ( not $eval_ok or @warnings )
    } ## end if ($action_object_constructor)

    $action_object //= {};

    my @evaluation_stack   = ();
    my @virtual_rule_stack = ();
    TREE_NODE: for my $and_node ( reverse @{$stack} ) {

        if ( $trace_values >= 3 ) {
            for my $i ( reverse 0 .. $#evaluation_stack ) {
                printf {$Marpa::Internal::TRACE_FH} 'Stack position %3d:', $i
                    or Marpa::exception('print to trace handle failed');
                print {$Marpa::Internal::TRACE_FH} q{ },
                    Data::Dumper->new( [ $evaluation_stack[$i] ] )->Terse(1)
                    ->Dump
                    or Marpa::exception('print to trace handle failed');
            } ## end for my $i ( reverse 0 .. $#evaluation_stack )
        } ## end if ( $trace_values >= 3 )

        my $value_ref = $and_node->[Marpa::Internal::And_Node::VALUE_REF];

        if ( defined $value_ref ) {

            push @evaluation_stack, $value_ref;

            if ($trace_values) {
                my $token_name =
                    $and_node->[Marpa::Internal::And_Node::TOKEN_NAME];

                print {$Marpa::Internal::TRACE_FH}
                    'Pushed value from a',
                    $and_node->[Marpa::Internal::And_Node::ID],
                    q{ },
                    $and_node->[Marpa::Internal::And_Node::TAG], ': ',
                    ( $token_name ? qq{$token_name = } : q{} ),
                    Data::Dumper->new( [$value_ref] )->Terse(1)->Dump
                    or Marpa::exception('print to trace handle failed');
            } ## end if ($trace_values)

        }    # defined $value_ref

        my $ops = $and_node->[Marpa::Internal::And_Node::VALUE_OPS];

        next TREE_NODE if not defined $ops;

        my $current_data = [];
        my $op_ix        = 0;
        while ( $op_ix < scalar @{$ops} ) {
            given ( $ops->[ $op_ix++ ] ) {

                when (Marpa::Internal::Op::ARGC) {

                    my $argc = $ops->[ $op_ix++ ];

                    if ($trace_values) {
                        my $rule_id =
                            $and_node->[Marpa::Internal::And_Node::RULE_ID];
                        my $rule = $rules->[$rule_id];
                        say {$Marpa::Internal::TRACE_FH}
                            'Popping ',
                            $argc,
                            ' values to evaluate a',
                            $and_node->[Marpa::Internal::And_Node::ID],
                            q{ },
                            $and_node->[Marpa::Internal::And_Node::TAG],
                            ', rule: ', Marpa::brief_rule($rule)
                            or
                            Marpa::exception('Could not print to trace file');
                    } ## end if ($trace_values)

                    $current_data =
                        [ map { ${$_} }
                            ( splice @evaluation_stack, -$argc ) ];

                } ## end when (Marpa::Internal::Op::ARGC)

                when (Marpa::Internal::Op::VIRTUAL_HEAD) {
                    my $real_symbol_count = $ops->[ $op_ix++ ];

                    if ($trace_values) {
                        my $rule_id =
                            $and_node->[Marpa::Internal::And_Node::RULE_ID];
                        my $rule = $rules->[$rule_id];
                        say {$Marpa::Internal::TRACE_FH}
                            'Head of Virtual Rule: a',
                            $and_node->[Marpa::Internal::And_Node::ID],
                            q{ },
                            $and_node->[Marpa::Internal::And_Node::TAG],
                            ', rule: ', Marpa::brief_rule($rule),
                            "\n",
                            "Incrementing virtual rule by $real_symbol_count symbols\n",
                            'Currently ',
                            ( scalar @virtual_rule_stack ),
                            ' rules; ', $virtual_rule_stack[-1], ' symbols;',
                            or
                            Marpa::exception('Could not print to trace file');
                    } ## end if ($trace_values)

                    $real_symbol_count += pop @virtual_rule_stack;
                    $current_data =
                        [ map { ${$_} }
                            ( splice @evaluation_stack, -$real_symbol_count )
                        ];

                } ## end when (Marpa::Internal::Op::VIRTUAL_HEAD)

                when (Marpa::Internal::Op::VIRTUAL_HEAD_NO_SEP) {
                    my $real_symbol_count = $ops->[ $op_ix++ ];

                    if ($trace_values) {
                        my $rule_id =
                            $and_node->[Marpa::Internal::And_Node::RULE_ID];
                        my $rule = $rules->[$rule_id];
                        say {$Marpa::Internal::TRACE_FH}
                            'Head of Virtual Rule (discards separation): a',
                            $and_node->[Marpa::Internal::And_Node::ID],
                            q{ },
                            $and_node->[Marpa::Internal::And_Node::TAG],
                            ', rule: ', Marpa::brief_rule($rule),
                            "\nAdding $real_symbol_count symbols; currently ",
                            ( scalar @virtual_rule_stack ),
                            ' rules; ', $virtual_rule_stack[-1], ' symbols'
                            or
                            Marpa::exception('Could not print to trace file');
                    } ## end if ($trace_values)

                    $real_symbol_count += pop @virtual_rule_stack;
                    my $base =
                        ( scalar @evaluation_stack ) - $real_symbol_count;
                    $current_data = [
                        map { ${$_} } @evaluation_stack[
                            map { $base + 2 * $_ }
                            ( 0 .. ( $real_symbol_count + 1 ) / 2 - 1 )
                        ]
                    ];

                    # truncate the evaluation stack
                    $#evaluation_stack = $base - 1;

                } ## end when (Marpa::Internal::Op::VIRTUAL_HEAD_NO_SEP)

                when (Marpa::Internal::Op::VIRTUAL_KERNEL) {
                    my $real_symbol_count = $ops->[ $op_ix++ ];
                    $virtual_rule_stack[-1] += $real_symbol_count;

                    if ($trace_values) {
                        my $rule_id =
                            $and_node->[Marpa::Internal::And_Node::RULE_ID];
                        my $rule = $rules->[$rule_id];
                        say {$Marpa::Internal::TRACE_FH}
                            'Virtual Rule: a',
                            $and_node->[Marpa::Internal::And_Node::ID],
                            q{ },
                            $and_node->[Marpa::Internal::And_Node::TAG],
                            ', rule: ', Marpa::brief_rule($rule),
                            "\nAdding $real_symbol_count, now ",
                            ( scalar @virtual_rule_stack ),
                            ' rules; ', $virtual_rule_stack[-1], ' symbols'
                            or
                            Marpa::exception('Could not print to trace file');
                    } ## end if ($trace_values)

                } ## end when (Marpa::Internal::Op::VIRTUAL_KERNEL)

                when (Marpa::Internal::Op::VIRTUAL_TAIL) {
                    my $real_symbol_count = $ops->[ $op_ix++ ];

                    if ($trace_values) {
                        my $rule_id =
                            $and_node->[Marpa::Internal::And_Node::RULE_ID];
                        my $rule = $rules->[$rule_id];
                        say {$Marpa::Internal::TRACE_FH}
                            'New Virtual Rule: a',
                            $and_node->[Marpa::Internal::And_Node::ID],
                            q{ },
                            $and_node->[Marpa::Internal::And_Node::TAG],
                            ', rule: ', Marpa::brief_rule($rule),
                            "\nSymbol count is $real_symbol_count, now ",
                            ( scalar @virtual_rule_stack + 1 ), ' rules',
                            or
                            Marpa::exception('Could not print to trace file');
                    } ## end if ($trace_values)

                    push @virtual_rule_stack, $real_symbol_count;

                } ## end when (Marpa::Internal::Op::VIRTUAL_TAIL)

                when (Marpa::Internal::Op::CONSTANT_RESULT) {
                    my $result = $ops->[ $op_ix++ ];
                    if ($trace_values) {
                        print {$Marpa::Internal::TRACE_FH}
                            'Constant result: ',
                            'Pushing 1 value on stack: ',
                            Data::Dumper->new( [$result] )->Terse(1)->Dump
                            or
                            Marpa::exception('Could not print to trace file');
                    } ## end if ($trace_values)
                    push @evaluation_stack, $result;
                } ## end when (Marpa::Internal::Op::CONSTANT_RESULT)

                when (Marpa::Internal::Op::CALL) {
                    my $closure = $ops->[ $op_ix++ ];
                    my $result;

                    my @warnings;
                    my $eval_ok;
                    DO_EVAL: {
                        local $SIG{__WARN__} = sub {
                            push @warnings, [ $_[0], ( caller 0 ) ];
                        };

                        $eval_ok = eval {
                            $result =
                                $closure->( $action_object,
                                @{$current_data} );
                            1;
                        };

                    } ## end DO_EVAL:

                    if ( not $eval_ok or @warnings ) {
                        my $rule_id =
                            $and_node->[Marpa::Internal::And_Node::RULE_ID];
                        my $rule        = $rules->[$rule_id];
                        my $fatal_error = $EVAL_ERROR;
                        Marpa::Internal::code_problems(
                            {   fatal_error => $fatal_error,
                                grammar     => $grammar,
                                eval_ok     => $eval_ok,
                                warnings    => \@warnings,
                                where       => 'computing value',
                                long_where  => 'Computing value for rule: '
                                    . Marpa::brief_rule($rule),
                            }
                        );
                    } ## end if ( not $eval_ok or @warnings )

                    if ($trace_values) {
                        print {$Marpa::Internal::TRACE_FH}
                            'Calculated and pushed value: ',
                            Data::Dumper->new( [$result] )->Terse(1)->Dump
                            or
                            Marpa::exception('print to trace handle failed');
                    } ## end if ($trace_values)

                    push @evaluation_stack, \$result;

                } ## end when (Marpa::Internal::Op::CALL)

                default {
                    Marpa::Exception("Unknown evaluator Op: $_");
                }

            } ## end given
        } ## end while ( $op_ix < scalar @{$ops} )

    }    # TREE_NODE

    return pop @evaluation_stack;
} ## end sub Marpa::Internal::Recognizer::evaluate

# null parse is special case
sub Marpa::Internal::Recognizer::do_null_parse {
    my ( $recce, $start_rule ) = @_;

    my $start_symbol = $start_rule->[Marpa::Internal::Rule::LHS];

    # Cannot increment the null parse
    return if $recce->[Marpa::Internal::Recognizer::PARSE_COUNT]++;
    my $null_values = $recce->[Marpa::Internal::Recognizer::NULL_VALUES];
    my $evaluator_rules =
        $recce->[Marpa::Internal::Recognizer::EVALUATOR_RULES];

    my $start_symbol_id = $start_symbol->[Marpa::Internal::Symbol::ID];
    my $start_rule_id   = $start_rule->[Marpa::Internal::Rule::ID];

    my $and_node = [];
    $#{$and_node} = Marpa::Internal::And_Node::LAST_FIELD;
    $and_node->[Marpa::Internal::And_Node::VALUE_REF] =
        \( $null_values->[$start_symbol_id] );
    $and_node->[Marpa::Internal::And_Node::RULE_ID] =
        $start_rule->[Marpa::Internal::Rule::ID];
    $and_node->[Marpa::Internal::And_Node::VALUE_OPS] =
        $evaluator_rules->[$start_rule_id];

    $and_node->[Marpa::Internal::And_Node::POSITION]      = 0;
    $and_node->[Marpa::Internal::And_Node::START_EARLEME] = 0;
    $and_node->[Marpa::Internal::And_Node::CAUSE_EARLEME] = 0;
    $and_node->[Marpa::Internal::And_Node::END_EARLEME]   = 0;
    $and_node->[Marpa::Internal::And_Node::ID]            = 0;

    $recce->[Marpa::Internal::Recognizer::AND_NODES]->[0] = $and_node;

    my $symbol_name = $start_symbol->[Marpa::Internal::Symbol::NAME];
    $and_node->[Marpa::Internal::And_Node::TAG] = q{T@0-0_} . $symbol_name;

    return Marpa::Internal::Recognizer::evaluate( $recce, [$and_node] );

} ## end sub Marpa::Internal::Recognizer::do_null_parse

# Returns false if no parse
sub Marpa::Recognizer::value {
    my ( $recce, @arg_hashes ) = @_;

    my $parse_set_arg = $recce->[Marpa::Internal::Recognizer::END];

    my $trace_tasks = $recce->[Marpa::Internal::Recognizer::TRACE_TASKS];
    local $Marpa::Internal::TRACE_FH =
        $recce->[Marpa::Internal::Recognizer::TRACE_FILE_HANDLE];

    my $and_nodes  = $recce->[Marpa::Internal::Recognizer::AND_NODES];
    my $or_nodes   = $recce->[Marpa::Internal::Recognizer::OR_NODES];
    my $cycle_hash = $recce->[Marpa::Internal::Recognizer::CYCLE_HASH];
    my $ranking_method =
        $recce->[Marpa::Internal::Recognizer::RANKING_METHOD];

    if ( $recce->[Marpa::Internal::Recognizer::SINGLE_PARSE_MODE] ) {
        Marpa::exception(
            qq{Arguments were passed directly to value() in a previous call\n},
            qq{Only one call to value() is allowed per recognizer when arguments are passed directly\n},
            qq{This is the second call to value()\n}
        );
    } ## end if ( $recce->[Marpa::Internal::Recognizer::SINGLE_PARSE_MODE...])

    my $parse_count = $recce->[Marpa::Internal::Recognizer::PARSE_COUNT];
    my $max_parses  = $recce->[Marpa::Internal::Recognizer::MAX_PARSES];
    if ( $max_parses and $parse_count > $max_parses ) {
        Marpa::exception("Maximum parse count ($max_parses) exceeded");
    }

    for my $arg_hash (@arg_hashes) {

        if ( exists $arg_hash->{end} ) {
            if ($parse_count) {
                Marpa::exception(
                    q{Cannot change "end" after first parse result});
            }
            $recce->[Marpa::Internal::Recognizer::SINGLE_PARSE_MODE] = 1;
            $parse_set_arg = $arg_hash->{end};
            delete $arg_hash->{end};
        } ## end if ( exists $arg_hash->{end} )

        if ( exists $arg_hash->{closures} ) {
            if ($parse_count) {
                Marpa::exception(
                    q{Cannot change "closures" after first parse result});
            }
            $recce->[Marpa::Internal::Recognizer::SINGLE_PARSE_MODE] = 1;
            my $closures = $arg_hash->{closures};
            while ( my ( $action, $closure ) = each %{$closures} ) {
                Marpa::exception(qq{Bad closure for action "$action"})
                    if ref $closure ne 'CODE';
            }
            $recce->[Marpa::Internal::Recognizer::CLOSURES] = $closures;
            delete $arg_hash->{closures};
        } ## end if ( exists $arg_hash->{closures} )

        if ( exists $arg_hash->{trace_actions} ) {
            $recce->[Marpa::Internal::Recognizer::SINGLE_PARSE_MODE] = 1;
            $recce->[Marpa::Internal::Recognizer::TRACE_ACTIONS] =
                $arg_hash->{trace_actions};
            delete $arg_hash->{trace_actions};
        } ## end if ( exists $arg_hash->{trace_actions} )

        if ( exists $arg_hash->{trace_values} ) {
            $recce->[Marpa::Internal::Recognizer::SINGLE_PARSE_MODE] = 1;
            $recce->[Marpa::Internal::Recognizer::TRACE_VALUES] =
                $arg_hash->{trace_values};
            delete $arg_hash->{trace_values};
        } ## end if ( exists $arg_hash->{trace_values} )

        # A typo made its way into the documentation, so now it's a
        # synonym.
        for my $trace_fh_alias (qw(trace_fh trace_file_handle)) {
            if ( exists $arg_hash->{$trace_fh_alias} ) {
                $recce->[Marpa::Internal::Recognizer::TRACE_FILE_HANDLE] =
                    $Marpa::Internal::TRACE_FH = $arg_hash->{$trace_fh_alias};
                delete $arg_hash->{$trace_fh_alias};
            }
        } ## end for my $trace_fh_alias (qw(trace_fh trace_file_handle))

        my @unknown_arg_names = keys %{$arg_hash};
        Marpa::exception(
            'Unknown named argument(s) to Marpa::Recognizer::value: ',
            ( join q{ }, @unknown_arg_names ) )
            if @unknown_arg_names;

    } ## end for my $arg_hash (@arg_hashes)

    my $grammar     = $recce->[Marpa::Internal::Recognizer::GRAMMAR];
    my $earley_sets = $recce->[Marpa::Internal::Recognizer::EARLEY_SETS];
    my $earley_hash = $recce->[Marpa::Internal::Recognizer::EARLEY_HASH];

    my $furthest_earleme =
        $recce->[Marpa::Internal::Recognizer::FURTHEST_EARLEME];
    my $last_completed_earleme =
        $recce->[Marpa::Internal::Recognizer::LAST_COMPLETED_EARLEME];
    Marpa::exception(
        "Attempt to evaluate incompletely recognized parse:\n",
        "  Last token ends at location $furthest_earleme\n",
        "  Recognition done only as far as location $last_completed_earleme\n"
    ) if $furthest_earleme > $last_completed_earleme;

    my $rules   = $grammar->[Marpa::Internal::Grammar::RULES];
    my $symbols = $grammar->[Marpa::Internal::Grammar::SYMBOLS];

    my $current_parse_set = $parse_set_arg
        // $recce->[Marpa::Internal::Recognizer::FURTHEST_EARLEME];

    # Look for the start item and start rule
    my $earley_set = $earley_sets->[$current_parse_set];

    # The null values are currently a function of the grammar,
    # and should be constant for the life of a recognizer.
    my $null_values = $recce->[Marpa::Internal::Recognizer::NULL_VALUES] //=
        Marpa::Internal::Recognizer::set_null_values($recce);

    my @task_list;
    my $start_item;
    my $start_rule;
    if ($parse_count) {
        @task_list = ( [Marpa::Internal::Task::ITERATE] );
    }
    else {
        my $start_state;

        EARLEY_ITEM: for my $item ( @{$earley_set} ) {
            $start_state = $item->[Marpa::Internal::Earley_Item::STATE];
            $start_rule  = $start_state->[Marpa::Internal::AHFA::START_RULE];
            next EARLEY_ITEM if not $start_rule;
            $start_item = $item;
            last EARLEY_ITEM;
        } ## end for my $item ( @{$earley_set} )

        return if not $start_rule;

        $recce->[Marpa::Internal::Recognizer::EVALUATOR_RULES] =
            Marpa::Internal::Recognizer::set_actions($recce);

        return Marpa::Internal::Recognizer::do_null_parse( $recce,
            $start_rule )
            if $start_rule->[Marpa::Internal::Rule::LHS]
                ->[Marpa::Internal::Symbol::NULLING];

        @task_list = ();
        push @task_list, [Marpa::Internal::Task::INITIALIZE];
    } ## end else [ if ($parse_count) ]

    $recce->[Marpa::Internal::Recognizer::PARSE_COUNT]++;

    my $evaluator_rules =
        $recce->[Marpa::Internal::Recognizer::EVALUATOR_RULES];
    my $iteration_stack =
        $recce->[Marpa::Internal::Recognizer::ITERATION_STACK];

    my @rank_dirty  = ();
    my @order_dirty = ();

    TASK: while ( my $task = pop @task_list ) {

        my ( $task_type, @task_data ) = @{$task};

        # Create the unpopulated top or-node
        if ( $task_type == Marpa::Internal::Task::INITIALIZE ) {

            if ($trace_tasks) {
                print {$Marpa::Internal::TRACE_FH}
                    'Task: INITIALIZE; ',
                    ( scalar @task_list ), " tasks pending\n"
                    or Marpa::exception('print to trace handle failed');
            } ## end if ($trace_tasks)

            my $start_rule_id = $start_rule->[Marpa::Internal::Rule::ID];

            my $start_or_node = [];
            {
                my $start_or_node_tag =
                    $start_or_node->[Marpa::Internal::Or_Node::TAG] =
                    "F$start_rule_id"
                    .
                    ## no critic (ValuesAndExpressions::RequireInterpolationOfMetachars)
                    '@0-' .
                    ## use critic
                    $current_parse_set;
                $recce->[Marpa::Internal::Recognizer::OR_NODE_HASH]
                    ->{$start_or_node_tag} = $start_or_node;
            }
            $start_or_node->[Marpa::Internal::Or_Node::ID] = 0;
            $start_or_node->[Marpa::Internal::Or_Node::ITEMS] =
                { $start_item->[Marpa::Internal::Earley_Item::NAME] =>
                    $start_item };
            $start_or_node->[Marpa::Internal::Or_Node::RULE_ID] =
                $start_rule_id;

            # Start or-node cannot cycle
            $start_or_node->[Marpa::Internal::Or_Node::CYCLE] = 0;
            $start_or_node->[Marpa::Internal::Or_Node::POSITION] =
                scalar @{ $start_rule->[Marpa::Internal::Rule::RHS] };

            # No source or-node for the start or-node
            $start_or_node->[Marpa::Internal::Or_Node::SOURCE_OR_NODE] =
                '[TOP]';

            # Zero out the evaluation
            $#{$and_nodes}       = -1;
            $#{$or_nodes}        = -1;
            $#{$iteration_stack} = -1;

            # Populate the start or-node
            $or_nodes->[0] = $start_or_node;

            my $start_iteration_node = [];
            $start_iteration_node->[Marpa::Internal::Iteration_Node::OR_NODE]
                = $start_or_node;

            @task_list = ();
            push @task_list,
                [
                Marpa::Internal::Task::CREATE_SUBTREE,
                $start_iteration_node
                ];

            if ( $ranking_method eq 'constant' ) {
                push @task_list, [Marpa::Internal::Task::RANK_ALL],
                    [
                    Marpa::Internal::Task::POPULATE_DEPTH, 0,
                    [$start_or_node]
                    ],
                    [
                    Marpa::Internal::Task::POPULATE_OR_NODE,
                    $start_or_node
                    ];
            } ## end if ( $ranking_method eq 'constant' )

            next TASK;

        } ## end if ( $task_type == Marpa::Internal::Task::INITIALIZE)

        # Special processing for the top iteration node
        if ( $task_type == Marpa::Internal::Task::ITERATE ) {

            if ($trace_tasks) {
                print {$Marpa::Internal::TRACE_FH}
                    'Task: ITERATE; ',
                    ( scalar @task_list ), " tasks pending\n"
                    or Marpa::exception('print to trace handle failed');
            } ## end if ($trace_tasks)

            @rank_dirty  = ();
            @order_dirty = ();

            # In this pass, we go up the iteration stack,
            # looking a node which we can iterate.
            my $iteration_node;
            my $iteration_ix;
            ITERATION_NODE:
            while ( $iteration_node = pop @{$iteration_stack} ) {
                $iteration_ix = scalar @{$iteration_stack};

                # This or-node is already populated,
                # or it would not have been put
                # onto the iteration stack
                my $choices = $iteration_node
                    ->[Marpa::Internal::Iteration_Node::CHOICES];

                next ITERATION_NODE if scalar @{$choices} <= 1;

                shift @{$choices};

                last ITERATION_NODE;

            } ## end while ( $iteration_node = pop @{$iteration_stack} )

# say STDERR "DEBUG Hit top of stack without finding node to iterate" if not defined $iteration_node;
            # If we hit the top of the stack without finding any node
            # to iterate, that is it for parsing.
            return if not defined $iteration_node;

            push @task_list,
                [ Marpa::Internal::Task::READJUST_STACK, $iteration_node ];

            next TASK;
        } ## end if ( $task_type == Marpa::Internal::Task::ITERATE )

        if ( $task_type == Marpa::Internal::Task::READJUST_STACK ) {

            my $iteration_node = $task_data[0];

            # Prune the cycle hash to eliminate everything we have
            # popped off the stack
            my $top_of_stack = $#{$iteration_stack};
            while ( my ( $node, $index ) = each %{$cycle_hash} ) {
                $index > $top_of_stack and delete $cycle_hash->{$node};
            }

            if ($trace_tasks) {
                print {$Marpa::Internal::TRACE_FH}
                    q{Task: READJUST_STACK },
                    ( scalar @task_list ), " tasks pending\n"
                    or Marpa::exception('print to trace handle failed');
            } ## end if ($trace_tasks)

            if ($ranking_method eq 'constant'
                and defined(
                    my $parent_ix =
                        $iteration_node
                        ->[Marpa::Internal::Iteration_Node::PARENT]
                )
                )
            {
                push @rank_dirty, $parent_ix;
                push @task_list, [Marpa::Internal::Task::RE_RANK];
            } ## end if ( $ranking_method eq 'constant' and defined( my ...))

            my $choices =
                $iteration_node->[Marpa::Internal::Iteration_Node::CHOICES];
            push @task_list,
                [ Marpa::Internal::Task::REDO_PREDECESSORS, $iteration_node ],
                [
                (   $choices->[0]
                        ->[Marpa::Internal::Choice::ITERATION_SUBTREE]
                    ? Marpa::Internal::Task::GRAFT_SUBTREE
                    : Marpa::Internal::Task::CREATE_SUBTREE
                ),
                $iteration_node
                ];

            next TASK;
        } ## end if ( $task_type == Marpa::Internal::Task::READJUST_STACK)

        if ( $task_type == Marpa::Internal::Task::REDO_PREDECESSORS ) {

            my $iteration_node = $task_data[0];

            if ($trace_tasks) {
                print {$Marpa::Internal::TRACE_FH}
                    q{Task: REDO_PREDECESSORS },
                    ( scalar @task_list ), " tasks pending\n"
                    or Marpa::exception('print to trace handle failed');
            } ## end if ($trace_tasks)

            my @new_tasks = ();

            # We go up the iteration stack
            # looking for the predecessor subtrees eliminated,
            # so we can reinitialize them.
            #
            # start iteration node is only one with child type
            # not defined
            #
            ITERATION_NODE:
            while ( my $child_type =
                $iteration_node->[Marpa::Internal::Iteration_Node::CHILD_TYPE]
                )
            {

                # "Decrement" the iteration node back up to its parent.
                my $parent_ix = $iteration_node
                    ->[Marpa::Internal::Iteration_Node::PARENT];
                $iteration_node = $iteration_stack->[$parent_ix];

                my $and_node =
                    $iteration_node
                    ->[Marpa::Internal::Iteration_Node::CHOICES]->[0]
                    ->[Marpa::Internal::Choice::AND_NODE];
                my $predecessor_id =
                    $and_node->[Marpa::Internal::And_Node::PREDECESSOR_ID];
                my $predecessor_or_node =
                    defined $predecessor_id
                    ? $or_nodes->[$predecessor_id]
                    : undef;

                # If we have just ascended a predecessor link,
                # we just continue ascending.
                # Only predecessors need to be reinitialized,
                # and those only when when their nodes
                # have been interated on the cause side.
                next ITERATION_NODE
                    if $child_type
                        == Marpa::Internal::And_Node::PREDECESSOR_ID;

                # We do not necessarily have a
                # predecessor or-node
                next ITERATION_NODE if not defined $predecessor_or_node;

                # If we are reinitializing the predecessor, that
                # "dirties" the rank of the parent
                push @rank_dirty, $parent_ix;

                my $new_iteration_node = [];
                $new_iteration_node
                    ->[Marpa::Internal::Iteration_Node::OR_NODE] =
                    $predecessor_or_node;
                $new_iteration_node->[Marpa::Internal::Iteration_Node::PARENT]
                    = $parent_ix;
                $new_iteration_node
                    ->[Marpa::Internal::Iteration_Node::CHILD_TYPE] =
                    Marpa::Internal::And_Node::PREDECESSOR_ID;

                push @new_tasks,
                    [
                    Marpa::Internal::Task::CREATE_SUBTREE,
                    $new_iteration_node
                    ]

            } ## end while ( my $child_type = $iteration_node->[...])

            push @task_list, reverse @new_tasks;

            next TASK;
        } ## end if ( $task_type == Marpa::Internal::Task::REDO_PREDECESSORS)

        if ( $task_type == Marpa::Internal::Task::RE_RANK ) {

            if ($trace_tasks) {
                print {$Marpa::Internal::TRACE_FH}
                    q{Task: RE_RANK; },
                    ( scalar @task_list ), " tasks pending\n"
                    or Marpa::exception('print to trace handle failed');
            } ## end if ($trace_tasks)

            @rank_dirty = sort { $a <=> $b } @rank_dirty;

            # Loop to ascend parent links
            RE_RANK_NODE:
            while ( defined( my $current_ix = pop @rank_dirty ) ) {

# say STDERR "DEBUG: RE_RANK ranking i$current_ix" if $DEBUG;

                my $rerank_node = $iteration_stack->[$current_ix];
                my $choices =
                    $rerank_node->[Marpa::Internal::Iteration_Node::CHOICES];
                my $current_choice = $choices->[0];
                my $and_node =
                    $current_choice->[Marpa::Internal::Choice::AND_NODE];

# { no integer;
# say STDERR "DEBUG Rank for current choice: i$current_ix: ", $current_choice->[Marpa::Internal::Choice::RANK] if $DEBUG;
# say STDERR "DEBUG Current Rank: i$current_ix: ", $rerank_node->[Marpa::Internal::Iteration_Node::RANK] if $DEBUG;
# say STDERR "DEBUG Test for constant rank: i$current_ix" if $DEBUG;
# }

                # Constant rank nodes are easy -- they keep the same rank until
                # exhausted and never lose their place to another choice.
                next RE_RANK_NODE
                    if defined $and_node
                        ->[Marpa::Internal::And_Node::CONSTANT_RANK_REF];

# say STDERR "DEBUG Calculating new rank: i$current_ix" if $DEBUG;

                no integer;

                # Sum up the new rank, if it is not constant:
                my $token_rank_ref =
                    $and_node->[Marpa::Internal::And_Node::TOKEN_RANK_REF];
                my $new_rank =
                    defined $token_rank_ref ? ${$token_rank_ref} : 0;

# say STDERR __LINE__, " DEBUG Calculating new rank: i$current_ix, $new_rank" if $DEBUG;

                my $predecessor_ix = $rerank_node
                    ->[Marpa::Internal::Iteration_Node::PREDECESSOR_IX];

# say STDERR __LINE__, " DEBUG Calculating new rank: i$current_ix, predecessor ix = ",  $predecessor_ix if defined $predecessor_ix and $DEBUG;

                $new_rank +=
                    defined $predecessor_ix
                    ? $iteration_stack->[$predecessor_ix]
                    ->[Marpa::Internal::Iteration_Node::RANK]
                    : 0;

# say STDERR __LINE__, " DEBUG Calculating new rank: i$current_ix, predecessor rank = ",  $iteration_stack->[$predecessor_ix]->[Marpa::Internal::Iteration_Node::RANK] if defined $predecessor_ix and $DEBUG;
# say STDERR __LINE__, " DEBUG Calculating new rank: i$current_ix, $new_rank" if $DEBUG;

                my $cause_ix =
                    $rerank_node->[Marpa::Internal::Iteration_Node::CAUSE_IX];
                $new_rank +=
                    defined $cause_ix
                    ? $iteration_stack->[$cause_ix]
                    ->[Marpa::Internal::Iteration_Node::RANK]
                    : 0;

# say STDERR __LINE__, " DEBUG Calculating new rank: i$current_ix, cause rank = ",  $iteration_stack->[$cause_ix]->[Marpa::Internal::Iteration_Node::RANK] if $DEBUG;

                # Nothing to do if the old rank and new rank are the same
                my $old_rank =
                    $current_choice->[Marpa::Internal::Choice::RANK];

# say STDERR "DEBUG Comparing new vs. old rank at i$current_ix: $new_rank vs. $old_rank" if $DEBUG;

                next RE_RANK_NODE if $old_rank == $new_rank;

# say STDERR "DEBUG New rank for i$current_ix is $new_rank" if $DEBUG;

                # New rank differs from old, so
                # replace it
                $rerank_node->[Marpa::Internal::Iteration_Node::RANK] =
                    $current_choice->[Marpa::Internal::Choice::RANK] =
                    $new_rank;

                # Put the current node on the "dirty" order list
                push @order_dirty, $current_ix;

                my $parent_ix =
                    $rerank_node->[Marpa::Internal::Iteration_Node::PARENT];
                next RE_RANK_NODE if not defined $parent_ix;

                push @rank_dirty, $parent_ix;

            } ## end while ( defined( my $current_ix = pop @rank_dirty ) )

            @order_dirty = sort { $a <=> $b } @order_dirty;
            REORDER_NODE:
            while ( defined( my $reorder_node_ix = pop @order_dirty ) ) {

# say STDERR "DEBUG: RE_RANK re-ordering i$reorder_node_ix" if $DEBUG;

                my $reorder_node = $iteration_stack->[$reorder_node_ix];

                my $choices =
                    $reorder_node->[Marpa::Internal::Iteration_Node::CHOICES];

                # Now to determine if the new rank puts this choice out
                # of proper order.
                # First off, unless there are 2 or more choices, the
                # current choice is clearly the right one.
                next REORDER_NODE if scalar @{$choices} < 2;

                no integer;

                # Secondly, if the current choice is still greater
                # than or equal to the next highest, it is the right
                # one
                my $current_choice = $choices->[0];
                my $new_rank =
                    $current_choice->[Marpa::Internal::Choice::RANK];
                next REORDER_NODE
                    if $new_rank
                        >= $choices->[1]->[Marpa::Internal::Choice::RANK];

# say STDERR "DEBUG New rank for i$reorder_node_ix is out of order -- must swap" if $DEBUG;
# say STDERR "DEBUG New vs. old: $new_rank vs. ", $choices->[1]->[Marpa::Internal::Choice::RANK] if $DEBUG;

                # Now we know we have to swap choices.  But
                # with which other choice?  We look for the
                # first one not greater than (less than or
                # equal to) the current choice.
                my $first_le_choice = 1;
                FIND_LE: while ( ++$first_le_choice <= $#{$choices} ) {
                    last FIND_LE
                        if $new_rank >= $choices->[$first_le_choice]
                            ->[Marpa::Internal::Choice::RANK];
                }

                my $last_descendant_ix = $reorder_node_ix;
                LOOK_FOR_DESCENDANT: while (1) {
                    my $inode    = $iteration_stack->[$last_descendant_ix];
                    my $child_ix = $inode
                        ->[Marpa::Internal::Iteration_Node::PREDECESSOR_IX];
                    if ( defined $child_ix ) {
                        $last_descendant_ix = $child_ix;
                        next LOOK_FOR_DESCENDANT;
                    }
                    $child_ix =
                        $inode->[Marpa::Internal::Iteration_Node::CAUSE_IX];
                    last LOOK_FOR_DESCENDANT if not defined $child_ix;
                    $last_descendant_ix = $child_ix;
                } ## end while (1)

                # We need to save the part of iteration stack
                # below the node being reordered
                $current_choice->[Marpa::Internal::Choice::ITERATION_SUBTREE]
                    = [ @{$iteration_stack}
                        [ $reorder_node_ix + 1 .. $last_descendant_ix ] ];

                # Prune the iteration stack
                $#{$iteration_stack} = $reorder_node_ix;

                my $swap_choice = $first_le_choice - 1;

# say STDERR "DEBUG i$reorder_node_ix: Swapping with choice $swap_choice" if $DEBUG;

                ( $choices->[0], $choices->[$swap_choice] ) =
                    ( $choices->[$swap_choice], $choices->[0] );

                my $new_iteration_node = pop @{$iteration_stack};
                push @task_list,
                    [
                    Marpa::Internal::Task::READJUST_STACK,
                    $new_iteration_node
                    ];

                next TASK;

            } ## end while ( defined( my $reorder_node_ix = pop @order_dirty...))

            next TASK;
        } ## end if ( $task_type == Marpa::Internal::Task::RE_RANK )

        if ( $task_type == Marpa::Internal::Task::POPULATE_OR_NODE ) {

            my $work_or_node = $task_data[0];

            if ($trace_tasks) {
                print {$Marpa::Internal::TRACE_FH}
                    'Task: POPULATE_OR_NODE o',
                    $work_or_node->[Marpa::Internal::Or_Node::ID],
                    q{; }, ( scalar @task_list ), " tasks pending\n"
                    or Marpa::exception('print to trace handle failed');
            } ## end if ($trace_tasks)

            my $work_node_name =
                $work_or_node->[Marpa::Internal::Or_Node::TAG];

            # SET Should be the same for all items
            my $or_node_items = [
                values %{ $work_or_node->[Marpa::Internal::Or_Node::ITEMS] }
            ];
            my $work_set;
            my $work_node_origin;
            {
                my $first_item = $or_node_items->[0];
                $work_set = $first_item->[Marpa::Internal::Earley_Item::SET];
                $work_node_origin =
                    $first_item->[Marpa::Internal::Earley_Item::PARENT];
            }

            my $work_rule_id =
                $work_or_node->[Marpa::Internal::Or_Node::RULE_ID];
            my $work_rule = $rules->[$work_rule_id];
            my $work_position =
                $work_or_node->[Marpa::Internal::Or_Node::POSITION] - 1;
            my $work_symbol =
                $work_rule->[Marpa::Internal::Rule::RHS]->[$work_position];

            for my $item ( @{$or_node_items} ) {

                my $or_sapling_set = $work_set;

# Marpa::Display
# name: Leo Expansion
# inline: 1

                my $leo_links =
                    $item->[Marpa::Internal::Earley_Item::LEO_LINKS] // [];

                # If this is a Leo completion, translate the Leo links
                for my $leo_link ( @{$leo_links} ) {

                    my ( $leo_item, $cause, $token_name, $token_value ) =
                        @{$leo_link};

                    my ( $next_leo_item, $leo_base_item ) =
                        @{ $leo_item->[Marpa::Internal::Earley_Item::LINKS]
                            ->[0] };

                    my $next_links = [];
                    if ($token_name) {
                        push @{$next_links},
                            [
                            $leo_base_item, undef,
                            $token_name,    $token_value
                            ];
                    } ## end if ($token_name)
                    if ($cause) {
                        push @{$next_links}, [ $leo_base_item, $cause ];
                    }

                    LEO_ITEM: for ( ;; ) {

                        if ( not $next_leo_item ) {

                            push @{ $item
                                    ->[Marpa::Internal::Earley_Item::LINKS] },
                                @{$next_links};

                            # Now that the Leo links are translated, remove them
                            $item->[Marpa::Internal::Earley_Item::LEO_LINKS] =
                                undef;
                            last LEO_ITEM;

                        } ## end if ( not $next_leo_item )

                        my $state =
                            $leo_item
                            ->[ Marpa::Internal::Earley_Item::LEO_ACTUAL_STATE
                            ];
                        my $origin = $next_leo_item
                            ->[Marpa::Internal::Earley_Item::SET];
                        my $name = sprintf
                            'S%d@%d-%d',
                            $state->[Marpa::Internal::AHFA::ID],
                            $origin,
                            $or_sapling_set;
                        my $target_item = $earley_hash->{$name};
                        if ( not defined $target_item ) {
                            $target_item = [];
                            $target_item->[Marpa::Internal::Earley_Item::NAME]
                                = $name;
                            $target_item
                                ->[Marpa::Internal::Earley_Item::PARENT] =
                                $origin;
                            $target_item
                                ->[Marpa::Internal::Earley_Item::STATE] =
                                $state;
                            $target_item
                                ->[Marpa::Internal::Earley_Item::LINKS] = [];
                            $target_item->[Marpa::Internal::Earley_Item::SET]
                                = $or_sapling_set;
                            $earley_hash->{$name} = $target_item;
                            push @{ $earley_sets->[$or_sapling_set] },
                                $target_item;
                        } ## end if ( not defined $target_item )

                        push @{ $target_item
                                ->[Marpa::Internal::Earley_Item::LINKS] },
                            @{$next_links};

                        $leo_item = $next_leo_item;

                        ( $next_leo_item, $leo_base_item ) =
                            @{ $leo_item
                                ->[Marpa::Internal::Earley_Item::LINKS]->[0]
                            };

                        $next_links = [ [ $leo_base_item, $target_item ] ];

                    } ## end for ( ;; )
                } ## end for my $leo_link ( @{$leo_links} )

# Marpa::Display::End

            } ## end for my $item ( @{$or_node_items} )

            my @link_worklist;

            CREATE_LINK_WORKLIST: {

                # link worklist item is $predecessor, $cause, $token_name, $value_ref
                my ( $predecessor, $cause, $token_name, $value_ref );

                # All predecessors apply to a
                # nulling work symbol.

                if ( $work_symbol->[Marpa::Internal::Symbol::NULLING] ) {
                    my $nulling_symbol_id =
                        $work_symbol->[Marpa::Internal::Symbol::ID];
                    $value_ref = \$null_values->[$nulling_symbol_id];
                    $token_name =
                        $work_symbol->[Marpa::Internal::Symbol::NAME];
                    @link_worklist =
                        map { [ $_, undef, $token_name, $value_ref ] }
                        @{$or_node_items};
                    last CREATE_LINK_WORKLIST;
                } ## end if ( $work_symbol->[Marpa::Internal::Symbol::NULLING...])

                # Maps token links ($predecessor, $token_name, $value_ref)
                # to link work items
                @link_worklist =
                    map { @{ $_->[Marpa::Internal::Earley_Item::LINKS] } }
                    @{$or_node_items};

            } ## end CREATE_LINK_WORKLIST:

            # The and node data is put into the hash, only to be taken out immediately,
            # but in the process the very important step of eliminating duplicates
            # is accomplished.
            my %and_node_data = ();

            LINK_WORK_ITEM: for my $link_work_item (@link_worklist) {

                # CHOICE POINT
                my ( $predecessor, $cause, $token_name, $value_ref ) =
                    @{$link_work_item};

                my $cause_earleme = $work_node_origin;
                my $predecessor_id;

                if ( $work_position > 0 ) {

                    $cause_earleme =
                        $predecessor->[Marpa::Internal::Earley_Item::SET];

                    my $predecessor_name =
                          "R$work_rule_id:$work_position" . q{@}
                        . $predecessor->[Marpa::Internal::Earley_Item::ORIGIN]
                        . q{-}
                        . $cause_earleme;

                    FIND_PREDECESSOR: {
                        my $predecessor_or_node =
                            $recce
                            ->[Marpa::Internal::Recognizer::OR_NODE_HASH]
                            ->{$predecessor_name};
                        if ($predecessor_or_node) {
                            $predecessor_id = $predecessor_or_node
                                ->[Marpa::Internal::Or_Node::ID];
                            last FIND_PREDECESSOR
                                if $predecessor_or_node->[
                                    Marpa::Internal::Or_Node::SOURCE_OR_NODE
                                ] ne $work_node_name;

                            # If the working or node is the grandparent of this new or-node,
                            # we are building it, and need to populate the list of Earley items
                            $predecessor_or_node
                                ->[Marpa::Internal::Or_Node::ITEMS]
                                ->{ $predecessor
                                    ->[Marpa::Internal::Earley_Item::NAME] } =
                                $predecessor;

                            last FIND_PREDECESSOR;

                        } ## end if ($predecessor_or_node)

                        $predecessor_or_node = [];
                        $predecessor_or_node->[Marpa::Internal::Or_Node::TAG]
                            = $predecessor_name;
                        $recce->[Marpa::Internal::Recognizer::OR_NODE_HASH]
                            ->{$predecessor_name} = $predecessor_or_node;
                        $predecessor_or_node
                            ->[Marpa::Internal::Or_Node::RULE_ID] =
                            $work_rule_id;

                        # nulling nodes are never part of cycles
                        # thanks to the CHAF rewrite
                        $predecessor_or_node
                            ->[Marpa::Internal::Or_Node::CYCLE] =
                            $work_rule->[Marpa::Internal::Rule::VIRTUAL_CYCLE]
                            && $cause_earleme != $work_node_origin;
                        $predecessor_or_node
                            ->[Marpa::Internal::Or_Node::POSITION] =
                            $work_position;
                        $predecessor_or_node
                            ->[Marpa::Internal::Or_Node::ITEMS] =
                            { $predecessor
                                ->[Marpa::Internal::Earley_Item::NAME] =>
                                $predecessor };
                        $predecessor_or_node
                            ->[Marpa::Internal::Or_Node::SOURCE_OR_NODE] =
                            $work_node_name;
                        $predecessor_id =
                            ( push @{$or_nodes}, $predecessor_or_node ) - 1;

                        Marpa::exception(
                            "Too many or-nodes for evaluator: $predecessor_id"
                            )
                            if $predecessor_id
                                & ~(Marpa::Internal::N_FORMAT_MAX);
                        $predecessor_or_node->[Marpa::Internal::Or_Node::ID] =
                            $predecessor_id;

                    } ## end FIND_PREDECESSOR:

                } ## end if ( $work_position > 0 )

                my $cause_id;

                if ( defined $cause ) {

                    my $cause_symbol_id =
                        $work_symbol->[Marpa::Internal::Symbol::ID];

                    my $state = $cause->[Marpa::Internal::Earley_Item::STATE];

                    for my $cause_rule (
                        @{  $state->[Marpa::Internal::AHFA::COMPLETE_RULES]
                                ->[$cause_symbol_id]
                        }
                        )
                    {

                        my $cause_rule_id =
                            $cause_rule->[Marpa::Internal::Rule::ID];

                        my $cause_name =
                              "F$cause_rule_id" . q{@}
                            . $cause->[Marpa::Internal::Earley_Item::ORIGIN]
                            . q{-}
                            . $cause->[Marpa::Internal::Earley_Item::SET];

                        FIND_CAUSE: {
                            my $cause_or_node =
                                $recce
                                ->[Marpa::Internal::Recognizer::OR_NODE_HASH]
                                ->{$cause_name};
                            if ($cause_or_node) {
                                $cause_id = $cause_or_node
                                    ->[Marpa::Internal::Or_Node::ID];
                                last FIND_CAUSE
                                    if $cause_or_node->[
                                        Marpa::Internal::Or_Node::SOURCE_OR_NODE
                                    ] ne $work_node_name;

                                # If the working or node is the grandparent of this new or-node,
                                # we are building it, and need to populate the list of Earley items
                                $cause_or_node
                                    ->[Marpa::Internal::Or_Node::ITEMS]
                                    ->{ $cause
                                        ->[Marpa::Internal::Earley_Item::NAME]
                                    } = $cause;
                                last FIND_CAUSE if $cause_or_node;
                            } ## end if ($cause_or_node)

                            $cause_or_node = [];
                            $cause_or_node->[Marpa::Internal::Or_Node::TAG] =
                                $cause_name;
                            $recce
                                ->[Marpa::Internal::Recognizer::OR_NODE_HASH]
                                ->{$cause_name} = $cause_or_node;
                            $cause_or_node
                                ->[Marpa::Internal::Or_Node::RULE_ID] =
                                $cause_rule_id;

                            # nulling nodes are never part of cycles
                            # thanks to the CHAF rewrite
                            $cause_or_node->[Marpa::Internal::Or_Node::CYCLE]
                                = $cause_rule
                                ->[Marpa::Internal::Rule::VIRTUAL_CYCLE]
                                && $cause_earleme != $work_set;
                            $cause_or_node
                                ->[Marpa::Internal::Or_Node::POSITION] =
                                scalar
                                @{ $cause_rule->[Marpa::Internal::Rule::RHS]
                                };
                            $cause_or_node->[Marpa::Internal::Or_Node::ITEMS]
                                = {
                                $cause->[Marpa::Internal::Earley_Item::NAME]
                                    => $cause };
                            $cause_or_node
                                ->[Marpa::Internal::Or_Node::SOURCE_OR_NODE] =
                                $work_node_name;
                            $cause_id =
                                ( push @{$or_nodes}, $cause_or_node ) - 1;

                            Marpa::exception(
                                "Too many or-nodes for evaluator: $cause_id")
                                if $cause_id
                                    & ~(Marpa::Internal::N_FORMAT_MAX);
                            $cause_or_node->[Marpa::Internal::Or_Node::ID] =
                                $cause_id;

                        } ## end FIND_CAUSE:

                        my $and_node = [];
                        #<<< cycles in perltidy as of 5 Jul 2010
                        $and_node
                            ->[Marpa::Internal::And_Node::PREDECESSOR_ID
                            ] = $predecessor_id;
                        #>>>
                        $and_node->[Marpa::Internal::And_Node::CAUSE_EARLEME]
                            = $cause_earleme;
                        $and_node->[Marpa::Internal::And_Node::CAUSE_ID] =
                            $cause_id;

                        $and_node_data{
                            join q{:},
                            ( $predecessor_id // q{} ),
                            $cause_id
                            }
                            = $and_node;

                    } ## end for my $cause_rule ( @{ $state->[...]})

                    next LINK_WORK_ITEM;

                }    # if cause

                my $and_node = [];
                $and_node->[Marpa::Internal::And_Node::PREDECESSOR_ID] =
                    $predecessor_id;
                $and_node->[Marpa::Internal::And_Node::CAUSE_EARLEME] =
                    $cause_earleme;
                $and_node->[Marpa::Internal::And_Node::TOKEN_NAME] =
                    $token_name;
                $and_node->[Marpa::Internal::And_Node::VALUE_REF] =
                    $value_ref;

                $and_node_data{
                    join q{:}, ( $predecessor_id // q{} ),
                    q{}, $token_name
                    }
                    = $and_node;

            } ## end for my $link_work_item (@link_worklist)

            my @child_and_nodes = values %and_node_data;

            for my $and_node (@child_and_nodes) {

                $and_node->[Marpa::Internal::And_Node::RULE_ID] =
                    $work_rule_id;

                $and_node->[Marpa::Internal::And_Node::VALUE_OPS] =
                    $work_position
                    == $#{ $work_rule->[Marpa::Internal::Rule::RHS] }
                    ? $evaluator_rules
                    ->[ $work_rule->[Marpa::Internal::Rule::ID] ]
                    : undef;

                $and_node->[Marpa::Internal::And_Node::POSITION] =
                    $work_position;
                $and_node->[Marpa::Internal::And_Node::START_EARLEME] =
                    $work_node_origin;
                $and_node->[Marpa::Internal::And_Node::END_EARLEME] =
                    $work_set;
                my $id = ( push @{$and_nodes}, $and_node ) - 1;
                Marpa::exception("Too many and-nodes for evaluator: $id")
                    if $id & ~(Marpa::Internal::N_FORMAT_MAX);
                $and_node->[Marpa::Internal::And_Node::ID] = $id;

                {
                    my $token_name =
                        $and_node->[Marpa::Internal::And_Node::TOKEN_NAME];
                    my $cause_earleme =
                        $and_node->[Marpa::Internal::And_Node::CAUSE_EARLEME];
                    my $tag            = q{};
                    my $predecessor_id = $and_node
                        ->[Marpa::Internal::And_Node::PREDECESSOR_ID];
                    my $predecessor_or_node =
                          $predecessor_id
                        ? $or_nodes->[$predecessor_id]
                        : undef;
                    $predecessor_or_node
                        and $tag
                        .= $predecessor_or_node
                        ->[Marpa::Internal::Or_Node::TAG];
                    my $cause_id =
                        $and_node->[Marpa::Internal::And_Node::CAUSE_ID];
                    my $cause_or_node =
                        $cause_id ? $or_nodes->[$cause_id] : undef;
                    $cause_or_node
                        and $tag
                        .= $cause_or_node->[Marpa::Internal::Or_Node::TAG];
                    $token_name
                        and $tag
                        .= q{T@}
                        . $cause_earleme . q{-}
                        . $work_set . q{_}
                        . $token_name;
                    $and_node->[Marpa::Internal::And_Node::TAG] = $tag;

                }
            } ## end for my $and_node (@child_and_nodes)

            # Populate the or-node, now that we have ID's for all the and-nodes
            $work_or_node->[Marpa::Internal::Or_Node::AND_NODE_IDS] =
                [ map { $_->[Marpa::Internal::And_Node::ID] }
                    @child_and_nodes ];

            next TASK;
        } ## end if ( $task_type == Marpa::Internal::Task::POPULATE_OR_NODE)

        if ( $task_type == Marpa::Internal::Task::CREATE_SUBTREE ) {

            my $work_iteration_node = $task_data[0];
            my $or_node             = $work_iteration_node
                ->[Marpa::Internal::Iteration_Node::OR_NODE];

            if ($trace_tasks) {
                print {$Marpa::Internal::TRACE_FH}
                    'Task: CREATE_SUBTREE o',
                    $or_node->[Marpa::Internal::Or_Node::ID],
                    q{; }, ( scalar @task_list ), " tasks pending\n"
                    or Marpa::exception('print to trace handle failed');
            } ## end if ($trace_tasks)

            my $and_node_ids =
                $or_node->[Marpa::Internal::Or_Node::AND_NODE_IDS];

            # If the or-node is not populated,
            # restack this task, and stack a task to populate the
            # or-node on top of it.
            if ( not defined $and_node_ids ) {
                push @task_list, $task,
                    [ Marpa::Internal::Task::POPULATE_OR_NODE, $or_node ];
                next TASK;
            }

            my $choices = $work_iteration_node
                ->[Marpa::Internal::Iteration_Node::CHOICES];

            # At this point we know the iteration node is populated, so if we don't
            # have the choices list initialized, we can do so now.
            if ( not defined $choices ) {

# say STDERR "Initializing choices" if $DEBUG;

                if ( $ranking_method eq 'constant' ) {
                    no integer;
                    my @choices = ();
                    AND_NODE: for my $and_node_id ( @{$and_node_ids} ) {
                        my $and_node   = $and_nodes->[$and_node_id];
                        my $new_choice = [];
                        $new_choice->[Marpa::Internal::Choice::AND_NODE] =
                            $and_node;
                        my $rank_ref = $and_node
                            ->[Marpa::Internal::And_Node::INITIAL_RANK_REF];
                        die "Undefined rank for a$and_node_id"
                            if not defined $rank_ref;
                        next AND_NODE if not ref $rank_ref;
                        $new_choice->[Marpa::Internal::Choice::RANK] =
                            ${$rank_ref};
                        push @choices, $new_choice;
                    } ## end for my $and_node_id ( @{$and_node_ids} )
                    ## no critic (BuiltinFunctions::ProhibitReverseSortBlock)
                    $choices = [
                        sort {
                            $b->[Marpa::Internal::Choice::RANK]
                                <=> $a->[Marpa::Internal::Choice::RANK]
                            } @choices
                    ];
                } ## end if ( $ranking_method eq 'constant' )
                else {
                    $choices =
                        [ map { [ $and_nodes->[$_], 0 ] } @{$and_node_ids} ];
                }
                $work_iteration_node
                    ->[Marpa::Internal::Iteration_Node::CHOICES] = $choices;

            } ## end if ( not defined $choices )

            # Due to skipping, even an initialized set of choices
            # may be empty.  If it is, throw away the stack and iterate.
            if ( not scalar @{$choices} ) {

# say STDERR "Initialized iteration-node has no choices";
                @task_list = ( [Marpa::Internal::Task::ITERATE] );
                next TASK;
            } ## end if ( not scalar @{$choices} )

            # Make our choice and set RANK
            my $choice = $choices->[0];
            {
                no integer;
                $work_iteration_node->[Marpa::Internal::Iteration_Node::RANK]
                    = $choice->[Marpa::Internal::Choice::RANK];
            }

            # Grafting frozen subtrees is handled in
            # another task, so we assume there's none of that
            # here.

            my $and_node = $choice->[Marpa::Internal::Choice::AND_NODE];
            my $next_iteration_stack_ix = scalar @{$iteration_stack};

            my $and_node_tag = $and_node->[Marpa::Internal::And_Node::TAG];

            # Check if we are about to cycle.
            if ( $or_node->[Marpa::Internal::Or_Node::CYCLE]
                and exists $cycle_hash->{$and_node_tag} )
            {

                # If there is another choice, increment choice and restack
                # this task ...
                if ( scalar @{$choices} > 1 ) {
                    shift @{$choices};
                    push @task_list, $task;
                    next TASK;
                }

                # Otherwise, throw away all pending tasks and
                # iterate
                @task_list = ( [Marpa::Internal::Task::ITERATE] );
                next TASK;
            } ## end if ( $or_node->[Marpa::Internal::Or_Node::CYCLE] and...)
            $cycle_hash->{$and_node_tag} = $next_iteration_stack_ix;

            OR_NODE_FIELD:
            for my $or_node_field ( Marpa::Internal::And_Node::PREDECESSOR_ID,
                Marpa::Internal::And_Node::CAUSE_ID
                )
            {
                #<<< Cycles in perltidy as of 9 Aug 2010
                next OR_NODE_FIELD
                    if not defined(
                            my $or_node_id = $and_node->[$or_node_field]
                    );
                #>>>
                my $iteration_node = [];
                my $child_or_node  = $or_nodes->[$or_node_id];
                $iteration_node->[Marpa::Internal::Iteration_Node::OR_NODE] =
                    $child_or_node;
                $iteration_node->[Marpa::Internal::Iteration_Node::PARENT] =
                    $next_iteration_stack_ix;
                $iteration_node->[Marpa::Internal::Iteration_Node::CHILD_TYPE]
                    = $or_node_field;
                push @task_list,
                    [ Marpa::Internal::Task::CREATE_SUBTREE,
                    $iteration_node ];
            } ## end for my $or_node_field ( ...)

            # Tell the parent that the new iteration node is its child.
            if (defined(
                    my $child_type =
                        $work_iteration_node
                        ->[Marpa::Internal::Iteration_Node::CHILD_TYPE]
                )
                )
            {
                my $parent_ix = $work_iteration_node
                    ->[Marpa::Internal::Iteration_Node::PARENT];
                push @rank_dirty, $parent_ix;
                $iteration_stack->[$parent_ix]->[
                    $child_type == Marpa::Internal::And_Node::PREDECESSOR_ID
                    ? Marpa::Internal::Iteration_Node::PREDECESSOR_IX
                    : Marpa::Internal::Iteration_Node::CAUSE_IX
                    ]
                    = scalar @{$iteration_stack};
            } ## end if ( defined( my $child_type = $work_iteration_node->...))

            push @{$iteration_stack}, $work_iteration_node;
            next TASK;
        } ## end if ( $task_type == Marpa::Internal::Task::CREATE_SUBTREE)

        if ( $task_type == Marpa::Internal::Task::GRAFT_SUBTREE ) {

            my $work_iteration_node = $task_data[0];
            my $or_node             = $work_iteration_node
                ->[Marpa::Internal::Iteration_Node::OR_NODE];

            if ($trace_tasks) {
                print {$Marpa::Internal::TRACE_FH}
                    'Task: GRAFT_SUBTREE o',
                    $or_node->[Marpa::Internal::Or_Node::ID],
                    q{; }, ( scalar @task_list ), " tasks pending\n"
                    or Marpa::exception('print to trace handle failed');
            } ## end if ($trace_tasks)

            my $and_node_ids =
                $or_node->[Marpa::Internal::Or_Node::AND_NODE_IDS];

            my $choices = $work_iteration_node
                ->[Marpa::Internal::Iteration_Node::CHOICES];

            # We assume the node is populated and that choices
            # are initialized.
            # If we are grafting subtrees, let's hope so.

            # set RANK
            my $choice = $choices->[0];
            {
                no integer;
                $work_iteration_node->[Marpa::Internal::Iteration_Node::RANK]
                    = $choice->[Marpa::Internal::Choice::RANK];

# say STDERR "DEBUG Swapping in choice with rank ", $choice->[Marpa::Internal::Choice::RANK] if $DEBUG;

            }

            my $subtree =
                $choice->[Marpa::Internal::Choice::ITERATION_SUBTREE];

            # Undef the old "frozen" values,
            # now that we are putting them back into play.
            $choice->[Marpa::Internal::Choice::ITERATION_SUBTREE] = undef;

            my $previous_stack_ix = scalar @{$iteration_stack};

            # In grafting, we know that this subtree already was put
            # on the stack before and was checked for cycles then.
            # We do not need to check for them now.
            {
                my $and_node_tag =
                    $choice->[Marpa::Internal::Choice::AND_NODE]
                    ->[Marpa::Internal::And_Node::TAG];
                $cycle_hash->{$and_node_tag} = $previous_stack_ix;
            }

            push @{$iteration_stack}, $work_iteration_node, @{$subtree};
            my $current_stack_ix = scalar @{$iteration_stack};

            # Add the pieces of the cycle hash back
            # into the hash and reset the parent's cause and predecessor
            IX:
            for (
                my $ix = $previous_stack_ix;
                $ix < $current_stack_ix;
                $ix++
                )
            {
                my $iteration_node = $iteration_stack->[$ix];
                if ($iteration_node->[Marpa::Internal::Iteration_Node::PARENT]
                    == $previous_stack_ix )
                {
                    my $child_type = $iteration_node
                        ->[Marpa::Internal::Iteration_Node::CHILD_TYPE];
                    $iteration_stack->[$previous_stack_ix]->[
                        $child_type
                        == Marpa::Internal::And_Node::PREDECESSOR_ID
                        ? Marpa::Internal::Iteration_Node::PREDECESSOR_IX
                        : Marpa::Internal::Iteration_Node::CAUSE_IX
                        ]
                        = $ix;
                } ## end if ( $iteration_node->[...])
                my $regrafted_or_node = $iteration_node
                    ->[Marpa::Internal::Iteration_Node::OR_NODE];
                next IX
                    if
                    not $regrafted_or_node->[Marpa::Internal::Or_Node::CYCLE];
                my $and_node_tag =
                    $iteration_node
                    ->[Marpa::Internal::Iteration_Node::CHOICES]->[0]
                    ->[Marpa::Internal::Choice::AND_NODE]
                    ->[Marpa::Internal::And_Node::TAG];
                $cycle_hash->{$and_node_tag} = $ix;
            } ## end for ( my $ix = $previous_stack_ix; $ix < ...)

            # We are done.
            next TASK;

        } ## end if ( $task_type == Marpa::Internal::Task::GRAFT_SUBTREE)

        if ( $task_type == Marpa::Internal::Task::RANK_ALL ) {

            if ($trace_tasks) {
                print {$Marpa::Internal::TRACE_FH} 'Task: RANK_ALL; ',
                    ( scalar @task_list ), " tasks pending\n"
                    or Marpa::exception('print to trace handle failed');
            }

            do_rank_all($recce);

            next TASK;
        } ## end if ( $task_type == Marpa::Internal::Task::RANK_ALL )

        # This task is for pre-populating the entire and-node and or-node
        # space one "depth level" at a time.  It is used when ranking is
        # being done, because to rank you need to make a pre-pass through
        # the entire and-node and or-node space.
        #
        # As a side effect, depths are calculated for all the and-nodes.
        if ( $task_type == Marpa::Internal::Task::POPULATE_DEPTH ) {
            my ( $depth, $or_node_list ) = @task_data;

            if ($trace_tasks) {
                print {$Marpa::Internal::TRACE_FH} 'Task: POPULATE_DEPTH; ',
                    ( scalar @task_list ), " tasks pending\n"
                    or Marpa::exception('print to trace handle failed');
            }

            # We can assume all or-nodes in the list are populated

            my %or_nodes_at_next_depth = ();

            # Assign a depth to all the and-node children which
            # do not already have one assigned.
            for my $and_node_id (
                map { @{ $_->[Marpa::Internal::Or_Node::AND_NODE_IDS] } }
                @{$or_node_list} )
            {
                my $and_node = $and_nodes->[$and_node_id];
                FIELD:
                for my $field (
                    Marpa::Internal::And_Node::PREDECESSOR_ID,
                    Marpa::Internal::And_Node::CAUSE_ID
                    )
                {
                    my $child_or_node_id = $and_node->[$field];
                    next FIELD if not defined $child_or_node_id;

                    my $next_depth_or_node = $or_nodes->[$child_or_node_id];

                    # Push onto list only if child or-node
                    # is not already populated
                    $next_depth_or_node
                        ->[Marpa::Internal::Or_Node::AND_NODE_IDS]
                        or $or_nodes_at_next_depth{$next_depth_or_node} =
                        $next_depth_or_node;

                } ## end for my $field ( ...)

            } ## end for my $and_node_id ( map { @{ $_->[...]}})

            # No or-nodes at next depth?
            # Great, we are done!
            my @or_nodes_at_next_depth = values %or_nodes_at_next_depth;
            next TASK if not scalar @or_nodes_at_next_depth;

            push @task_list,
                [
                Marpa::Internal::Task::POPULATE_DEPTH, $depth + 1,
                \@or_nodes_at_next_depth
                ],
                map { [ Marpa::Internal::Task::POPULATE_OR_NODE, $_ ] }
                @or_nodes_at_next_depth;

            next TASK;

        } ## end if ( $task_type == Marpa::Internal::Task::POPULATE_DEPTH)

        Marpa::internal_error(
            "Internal error: Unknown task type: $task_type");

    } ## end while ( my $task = pop @task_list )

    my @stack = map {
        $_->[Marpa::Internal::Iteration_Node::CHOICES]->[0]
            ->[Marpa::Internal::Choice::AND_NODE]
    } @{$iteration_stack};

# say STDERR "DEBUG Iteration Stack:\n", $recce->show_iteration_stack(99) if $DEBUG;

    return Marpa::Internal::Recognizer::evaluate( $recce, \@stack );

} ## end sub Marpa::Recognizer::value

1;
