package Marpa::Evaluator;

use 5.010;
use warnings;
no warnings qw(recursion qw);
use strict;
use integer;

use Marpa::Internal;

# The bocage is Marpa's structure for keeping multiple parses.
# A parse bocage is a list of or-nodes, whose child
# and-nodes must be (at most) binary.

# "Parse forests" are the structures used to keep multiple
# parses in many parsers, but Marpa
# can't use them because
# Marpa allows cyclical parses, and
# it breaks the RHS of productions into
# and-nodes of a most two symbols.
# And-nodes start in binary form
# in the Aycock-Horspool Earley items, and because
# binary and-nodes store the parses
# compactly, and allow easier tree
# traversals, I keep them that way.

# Bocage is a special type of forest,
# consisting of hedgerows deliberately cultivated
# as obstacles to cattle and armies.

# Saplings which become or-nodes when they grow up.

use Marpa::Offset qw(

    :package=Marpa::Internal::Or_Sapling

    NAME ITEM RULE
    POSITION CHILD_LHS_SYMBOL

);

use Marpa::Offset qw(

    :package=Marpa::Internal::And_Node

    TAG ID
    PREDECESSOR_ID
    CAUSE_ID
    TOKEN VALUE_REF
    TREE_OPS
    VALUE_OPS
    START_EARLEME
    END_EARLEME
    CAUSE_EARLEME
    RULE_ID

    POSITION { Position in an and-node is not the same as
    position in a rule.  Rule positions are locations BETWEEN
    symbols, and start from 0 (before the first symbol).
    And-node positions are zero-based locations OF symbols.
    An and-node position of -1 means the and-node is for a
    rule with an empty RHS. }

    PARENT_ID
    PARENT_CHOICE
    DELETED

    =LAST_GENERAL_EVALUATOR_FIELD

    FIXED_RANKING_DATA { Rank for this and-node itself,
    but not including any of the children.
    It takes into account the token, if any,
    but not the rank of any of the children.
    Once calculated, it's a constant
    for the life of the and-node.
    }

    RANKING_CLOSURE

    =LAST_PER_METHOD_EVALUATOR_FIELD
    =LAST_FIELD

);

use Marpa::Offset qw)

    :package=Marpa::Internal::Original_Sort_Data
    SORT_KEY
    TRAILING_NULLS
);

use Marpa::Offset qw(

    :package=Marpa::Internal::And_Iteration

    RANKING_DATA
    CURRENT_CHILD_FIELD

    =LAST_FIELD

);

use Marpa::Offset qw(

    :package=Marpa::Internal::Or_Node

    TAG
    ID
    CHILD_IDS
    START_EARLEME
    END_EARLEME
    PARENT_IDS
    DELETED

    =LAST_GENERAL_EVALUATOR_FIELD
    =LAST_FIELD
);

use Marpa::Offset qw(

    :package=Marpa::Internal::Or_Iteration

    AND_CHOICE0
    AND_CHOICE1
    { And so on ... }

);

use Marpa::Offset qw(
    :package=Marpa::Internal::And_Choice
    ID
    RANKING_DATA
    FROZEN_ITERATION
    =LAST_FIELD
);

use Marpa::Offset qw(

    :package=Marpa::Internal::Evaluator

    GRAMMAR
    SEMANTICS_SETTLED
    PARSE_COUNT :{ number of parses in an ambiguous parse :}
    AND_NODES
    OR_NODES
    RULE_TREE_OPS
    RULE_VALUE_OPS
    AND_ITERATIONS
    OR_ITERATIONS
    ACTION_OBJECT_CONSTRUCTOR
    RANKING_CLOSURES_BY_RULE :{ array, by rule id }
    RANKING_CLOSURES_BY_SYMBOL :{ array, by symbol id }

    CYCLE_NODES
    CYCLE_REWRITE
    CYCLE_SCALE
    EXPERIMENTAL
    MAX_PARSES
    PARSE_ORDER
    TRACING
    TRACE_ACTIONS
    TRACE_EVALUATION
    TRACE_FILE_HANDLE
    TRACE_TASKS
    TRACE_VALUES

);

use Marpa::Offset qw(

    :package=Marpa::Internal::Evaluator_Op

    :{ These are the valuation-time ops }
    ARGC
    CALL
    CONSTANT_RESULT
    VIRTUAL_HEAD
    VIRTUAL_HEAD_NO_SEP
    VIRTUAL_KERNEL
    VIRTUAL_TAIL

    :{ These are the tree-time ops }
    CYCLE
    COUNTED_RULE

);

package Marpa::Internal::Evaluator;

# use Smart::Comments '-ENV';

### Using smart comments <where>...

use Scalar::Util;
use List::Util;
use English qw( -no_match_vars );
use Data::Dumper;
use Storable;
use Marpa::Callback;
use Marpa::Internal;

our $DEFAULT_ACTION_VALUE = \undef;

sub set_null_values {
    my ($grammar) = @_;

    my $rules   = $grammar->[Marpa::Internal::Grammar::RULES];
    my $symbols = $grammar->[Marpa::Internal::Grammar::SYMBOLS];
    my $default_null_value =
        $grammar->[Marpa::Internal::Grammar::DEFAULT_NULL_VALUE];
    my $actions_package = $grammar->[Marpa::Internal::Grammar::ACTIONS];

    my $null_values;
    $#{$null_values} = $#{$symbols};

    SYMBOL: for my $symbol ( @{$symbols} ) {
        my $id = $symbol->[Marpa::Internal::Symbol::ID];
        $null_values->[$id] = $default_null_value;
    }

    # Set null values specified in
    # empty rules.
    RULE: for my $rule ( @{$rules} ) {

        my $action = $rule->[Marpa::Internal::Rule::ACTION];

        # Set the null value of symbols from the action for their
        # empty rules
        my $rhs = $rule->[Marpa::Internal::Rule::RHS];

        # Empty rule with action?
        if ( defined $action and @{$rhs} <= 0 ) {

            my $closure =
                Marpa::Internal::Evaluator::resolve_semantics( $grammar,
                $action );
            Marpa::exception("Action closure '$action' not found")
                if not defined $closure;

            my $lhs            = $rule->[Marpa::Internal::Rule::LHS];
            my $nulling_symbol = $lhs->[Marpa::Internal::Symbol::NULL_ALIAS]
                // $lhs;

            my $null_value;

            DO_EVAL: {
                my @warnings;
                my $eval_ret;
                my $eval_error;
                {
                    local $EVAL_ERROR = undef;
                    local $SIG{__WARN__} =
                        sub { push @warnings, [ $_[0], ( caller 0 ) ]; };
                    $eval_ret = eval { $null_value = $closure->(); 1; };
                    $eval_error = $EVAL_ERROR;
                }

                last DO_EVAL if $eval_ret and not scalar @warnings;
                Marpa::Internal::code_problems(
                    {   eval_ok     => $eval_ret,
                        fatal_error => $eval_error,
                        grammar     => $grammar,
                        warnings    => \@warnings,
                        where       => 'evaluating null value',
                        long_where  => 'evaluating null value for '
                            . $nulling_symbol
                            ->[Marpa::Internal::Symbol::NAME],
                    }
                );
            } ## end DO_EVAL:
            my $nulling_symbol_id =
                $nulling_symbol->[Marpa::Internal::Symbol::ID];
            $null_values->[$nulling_symbol_id] = $null_value;

            if ($Marpa::Internal::TRACE_ACTIONS) {
                print {$Marpa::Internal::TRACE_FH}
                    'Setting null value for symbol ',
                    $nulling_symbol->[Marpa::Internal::Symbol::NAME],
                    ' to ',
                    Data::Dumper->new( [ \$null_value ] )->Terse(1)->Dump,
                    "\n"
                    or Marpa::exception('Could not print to trace file');
            } ## end if ($Marpa::Internal::TRACE_ACTIONS)

            next RULE;

        } ## end if ( defined $action and @{$rhs} <= 0 )

    }    # RULE

    if ($Marpa::Internal::TRACE_ACTIONS) {
        SYMBOL: for my $symbol ( @{$symbols} ) {
            my ( $name, $id ) = @{$symbol}[
                Marpa::Internal::Symbol::NAME, Marpa::Internal::Symbol::ID,
            ];
            print {$Marpa::Internal::TRACE_FH}
                'Setting null value for CHAF symbol ',
                $name, ' to ',
                Data::Dumper->new( [ $null_values->[$id] ] )->Terse(1)->Dump,
                or Marpa::exception('Could not print to trace file');
        } ## end for my $symbol ( @{$symbols} )
    } ## end if ($Marpa::Internal::TRACE_ACTIONS)

    return $null_values;

}    # set_null_values

# Given the grammar and an action name, resolve it to a closure,
# or return undef
sub resolve_semantics {
    my ( $grammar, $closure_name ) = @_;

    Marpa::exception(q{Trying to resolve 'undef' as closure name})
        if not defined $closure_name;

    if ( my $closure = $Marpa::Internal::EXPLICIT_CLOSURES->{$closure_name} )
    {
        if ($Marpa::Internal::TRACE_ACTIONS) {
            print {$Marpa::Internal::TRACE_FH}
                qq{Resolved "$closure_name" to explicit closure\n}
                or Marpa::exception('Could not print to trace file');
        }

        return $closure;
    } ## end if ( my $closure = $Marpa::Internal::EXPLICIT_CLOSURES...)

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
                my $action_object =
                    $grammar->[Marpa::Internal::Grammar::ACTION_OBJECT]
            )
            )
        {
            $fully_qualified_name = $action_object . q{::} . $closure_name;
        } ## end if ( defined( my $action_object = $grammar->[...]))
    } ## end DETERMINE_FULLY_QUALIFIED_NAME:

    return if not defined $fully_qualified_name;

    no strict 'refs';
    my $closure = *{$fully_qualified_name}{'CODE'};
    use strict 'refs';

    if ($Marpa::Internal::TRACE_ACTIONS) {
        print {$Marpa::Internal::TRACE_FH}
            ( $closure ? 'Successful' : 'Failed' )
            . qq{ resolution of "$closure_name" },
            'to ', $fully_qualified_name, "\n"
            or Marpa::exception('Could not print to trace file');
    } ## end if ($Marpa::Internal::TRACE_ACTIONS)

    return $closure;

} ## end sub resolve_semantics

sub set_actions {
    my ($grammar) = @_;

    my ( $rules, $default_action, ) = @{$grammar}[
        Marpa::Internal::Grammar::RULES,
        Marpa::Internal::Grammar::DEFAULT_ACTION,
    ];

    my $evaluator_rules = [];

    my $default_action_closure;
    if ( defined $default_action ) {
        $default_action_closure =
            Marpa::Internal::Evaluator::resolve_semantics( $grammar,
            $default_action );
        Marpa::exception(
            "Could not resolve default action named '$default_action'")
            if not $default_action_closure;
    } ## end if ( defined $default_action )

    RULE: for my $rule ( @{$rules} ) {

        next RULE if not $rule->[Marpa::Internal::Rule::USEFUL];

        my $rule_id = $rule->[Marpa::Internal::Rule::ID];
        my $ops = $evaluator_rules->[$rule_id] = [];

        my $virtual_rhs = $rule->[Marpa::Internal::Rule::VIRTUAL_RHS];
        my $virtual_lhs = $rule->[Marpa::Internal::Rule::VIRTUAL_LHS];

        if ($virtual_lhs) {
            push @{$ops},
                (
                $virtual_rhs
                ? Marpa::Internal::Evaluator_Op::VIRTUAL_KERNEL
                : Marpa::Internal::Evaluator_Op::VIRTUAL_TAIL
                ),
                $rule->[Marpa::Internal::Rule::REAL_SYMBOL_COUNT];
            next RULE;
        } ## end if ($virtual_lhs)

        # If we are here the LHS is real, not virtual

        if ($virtual_rhs) {
            push @{$ops},
                (
                $rule->[Marpa::Internal::Rule::DISCARD_SEPARATION]
                ? Marpa::Internal::Evaluator_Op::VIRTUAL_HEAD_NO_SEP
                : Marpa::Internal::Evaluator_Op::VIRTUAL_HEAD
                ),
                $rule->[Marpa::Internal::Rule::REAL_SYMBOL_COUNT];
        } ## end if ($virtual_rhs)
            # assignment instead of comparison is deliberate
        elsif ( my $argc = scalar @{ $rule->[Marpa::Internal::Rule::RHS] } ) {
            push @{$ops}, Marpa::Internal::Evaluator_Op::ARGC, $argc;
        }

        if ( my $action = $rule->[Marpa::Internal::Rule::ACTION] ) {
            my $closure =
                Marpa::Internal::Evaluator::resolve_semantics( $grammar,
                $action );

            Marpa::exception(qq{Could not resolve action name: "$action"})
                if not defined $closure;
            push @{$ops}, Marpa::Internal::Evaluator_Op::CALL, $closure;
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
                        Marpa::Internal::Evaluator::resolve_semantics(
                        $grammar, $action
                        )
                )
                )
            {
                push @{$ops}, Marpa::Internal::Evaluator_Op::CALL, $closure;
                next RULE;
            } ## end if ( $action !~ /[\]] \z/xms and defined( my $closure...)[)
        } ## end if ( my $action = $rule->[Marpa::Internal::Rule::LHS...])

        if ( defined $default_action_closure ) {
            push @{$ops}, Marpa::Internal::Evaluator_Op::CALL,
                $default_action_closure;
            next RULE;
        }

        # If there is no default action specified, the fallback
        # is to return an undef
        push @{$ops}, Marpa::Internal::Evaluator_Op::CONSTANT_RESULT,
            $Marpa::Internal::Evaluator::DEFAULT_ACTION_VALUE;

    } ## end for my $rule ( @{$rules} )

    return $evaluator_rules;

}    # set_actions

sub audit_or_node {
    my ( $evaler, $or_node ) = @_;
    my $or_nodes  = $evaler->[Marpa::Internal::Evaluator::OR_NODES];
    my $and_nodes = $evaler->[Marpa::Internal::Evaluator::AND_NODES];

    my $or_node_id = $or_node->[Marpa::Internal::Or_Node::ID];

    if ( not defined $or_node_id ) {
        Marpa::exception('ID not defined in or-node');
    }
    my $or_nodes_entry = $or_nodes->[$or_node_id];
    if ( $or_node != $or_nodes_entry ) {
        Marpa::exception(
            "or_node #$or_node_id does not match its or-nodes entry");
    }
    if ( $#{$or_node} != Marpa::Internal::Or_Node::LAST_FIELD ) {
        Marpa::exception(
            "Bad field count in or-node #$or_node_id: want ",
            Marpa::Internal::Or_Node::LAST_FIELD,
            ', got ', $#{$or_node}
        );
    } ## end if ( $#{$or_node} != Marpa::Internal::Or_Node::LAST_FIELD)

    my $deleted = $or_node->[Marpa::Internal::Or_Node::DELETED];

    my $parent_ids = $or_node->[Marpa::Internal::Or_Node::PARENT_IDS];

    # No parents for top or-node, or-node 0
    if ( $or_node_id != 0 ) {
        my $has_parents = ( defined $parent_ids and scalar @{$parent_ids} );
        if ( not $deleted and not $has_parents ) {
            Marpa::exception("or-node #$or_node_id has no parents");
        }
        if ( $deleted and $has_parents ) {
            Marpa::exception("Deleted or-node #$or_node_id has parents");
        }
    } ## end if ( $or_node_id != 0 )

    {
        my %parent_id_seen;
        PARENT_ID: for my $parent_id ( @{$parent_ids} ) {
            next PARENT_ID if not $parent_id_seen{$parent_id}++;
            Marpa::exception(
                "or-node #$or_node_id has duplicate parent, #$parent_id");
        }
    }

    PARENT_ID: for my $parent_id ( @{$parent_ids} ) {
        my $parent   = $and_nodes->[$parent_id];
        my $cause_id = $parent->[Marpa::Internal::And_Node::CAUSE_ID];
        next PARENT_ID if defined $cause_id and $or_node_id == $cause_id;

        my $predecessor_id =
            $parent->[Marpa::Internal::And_Node::PREDECESSOR_ID];
        next PARENT_ID
            if defined $predecessor_id and $or_node_id == $predecessor_id;

        Marpa::exception(
            "or_node #$or_node_id is not the cause or predecessor of parent and-node #$parent_id"
        );

    } ## end for my $parent_id ( @{$parent_ids} )

    my $child_ids = $or_node->[Marpa::Internal::Or_Node::CHILD_IDS];
    my $has_children = ( defined $child_ids and scalar @{$child_ids} );
    if ( not $deleted and not $has_children ) {
        Marpa::exception("or-node #$or_node_id has no children");
    }
    if ( $deleted and $has_children ) {
        Marpa::exception("Deleted or-node #$or_node_id has children");
    }

    {
        my %child_id_seen;
        CHILD_ID: for my $child_id ( @{$child_ids} ) {
            next CHILD_ID if not $child_id_seen{$child_id}++;
            Marpa::exception(
                "or-node #$or_node_id has duplicate child, #$child_id");
        }
    }

    for my $child_id ( @{$child_ids} ) {
        my $child        = $and_nodes->[$child_id];
        my $child_parent = $child->[Marpa::Internal::And_Node::PARENT_ID];
        if ( not defined $child_parent or $or_node_id != $child_parent ) {
            Marpa::exception(
                "or_node #$or_node_id is not the parent of child and-node #$child_id"
            );
        }
    } ## end for my $child_id ( @{$child_ids} )

    return;
} ## end sub audit_or_node

sub audit_and_node {
    my ( $evaler, $audit_and_node ) = @_;
    my $or_nodes  = $evaler->[Marpa::Internal::Evaluator::OR_NODES];
    my $and_nodes = $evaler->[Marpa::Internal::Evaluator::AND_NODES];

    my $audit_and_node_id = $audit_and_node->[Marpa::Internal::And_Node::ID];

    if ( not defined $audit_and_node_id ) {
        Marpa::exception('ID not defined in and-node');
    }
    my $and_nodes_entry = $and_nodes->[$audit_and_node_id];
    if ( $audit_and_node != $and_nodes_entry ) {
        Marpa::exception(
            "and_node #$audit_and_node_id does not match its and-nodes entry"
        );
    }
    if ( $#{$audit_and_node} != Marpa::Internal::And_Node::LAST_FIELD ) {
        Marpa::exception(
            "Bad field count in and-node #$audit_and_node_id: want ",
            Marpa::Internal::And_Node::LAST_FIELD,
            ', got ', $#{$audit_and_node}
        );
    } ## end if ( $#{$audit_and_node} != ...)

    my $deleted = $audit_and_node->[Marpa::Internal::And_Node::DELETED];

    my $parent_id = $audit_and_node->[Marpa::Internal::And_Node::PARENT_ID];
    my $parent_choice =
        $audit_and_node->[Marpa::Internal::And_Node::PARENT_CHOICE];
    if ( not $deleted ) {
        my $parent_or_node = $or_nodes->[$parent_id];
        my $parent_idea_of_child_id =
            $parent_or_node->[Marpa::Internal::Or_Node::CHILD_IDS]
            ->[$parent_choice];
        if ( $audit_and_node_id != $parent_idea_of_child_id ) {
            Marpa::exception(
                "and_node #$audit_and_node_id does not match its CHILD_IDS entry in its parent"
            );
        }
    } ## end if ( not $deleted )
    else {
        if ( defined $parent_id ) {
            Marpa::exception(
                "deleted and_node $audit_and_node_id has defined PARENT_ID: #$parent_id"
            );
        }
        if ( defined $parent_choice ) {
            Marpa::exception(
                "deleted and_node $audit_and_node_id has defined PARENT_CHOICE: #$parent_choice"
            );
        }
    } ## end else [ if ( not $deleted ) ]

    FIELD:
    for my $field (
        Marpa::Internal::And_Node::PREDECESSOR_ID,
        Marpa::Internal::And_Node::CAUSE_ID,
        )
    {
        my $child_or_node_id = $audit_and_node->[$field];
        next FIELD if not defined $child_or_node_id;
        my $child_or_node = $or_nodes->[$child_or_node_id];
        if ( $deleted and defined $child_or_node_id ) {
            Marpa::exception(
                "deleted and-node $audit_and_node_id has defined child: #$parent_id"
            );
        }
        my $child_idea_of_parent_ids =
            $child_or_node->[Marpa::Internal::Or_Node::PARENT_IDS];
        if ( $deleted and scalar @{$child_idea_of_parent_ids} ) {
            Marpa::exception(
                "deleted and-node $audit_and_node_id has parents: ",
                ( join q{, }, @{$child_idea_of_parent_ids} )
            );
        } ## end if ( $deleted and scalar @{$child_idea_of_parent_ids...})
        next FIELD if $deleted;
        my $audit_and_node_index = List::Util::first {
            $child_idea_of_parent_ids->[$_] == $audit_and_node_id;
        }
        ( 0 .. $#{$child_idea_of_parent_ids} );
        if ( not defined $audit_and_node_index ) {
            Marpa::exception(
                "child of and-node (or-node $child_or_node_id) does not have and-node $audit_and_node_id as parent"
            );
        }

    } ## end for my $field ( Marpa::Internal::And_Node::PREDECESSOR_ID...)

    return;
} ## end sub audit_and_node

sub Marpa::Evaluator::audit {
    my ($evaler) = @_;
    my $or_nodes = $evaler->[Marpa::Internal::Evaluator::OR_NODES];
    for my $or_node ( @{$or_nodes} ) {
        audit_or_node( $evaler, $or_node );
    }
    my $and_nodes = $evaler->[Marpa::Internal::Evaluator::AND_NODES];
    for my $and_node ( @{$and_nodes} ) {
        audit_and_node( $evaler, $and_node );
    }

    ### Bocage passed audit ...

    return;
} ## end sub Marpa::Evaluator::audit

# Internal routine to clone an and-node
sub clone_and_node {
    my ( $evaler, $and_node, $new_parent_or_node_id,
        $child_or_node_id_translation )
        = @_;

    my $and_nodes = $evaler->[Marpa::Internal::Evaluator::AND_NODES];
    my $or_nodes  = $evaler->[Marpa::Internal::Evaluator::OR_NODES];

    my $new_and_node;
    $#{$new_and_node} = Marpa::Internal::And_Node::LAST_FIELD;
    my $new_and_node_id = $new_and_node->[Marpa::Internal::And_Node::ID] =
        scalar @{$and_nodes};

    push @{$and_nodes}, $new_and_node;

    for my $field (
        Marpa::Internal::And_Node::VALUE_REF,
        Marpa::Internal::And_Node::TOKEN,
        Marpa::Internal::And_Node::TREE_OPS,
        Marpa::Internal::And_Node::VALUE_OPS,
        Marpa::Internal::And_Node::START_EARLEME,
        Marpa::Internal::And_Node::END_EARLEME,
        Marpa::Internal::And_Node::CAUSE_EARLEME,
        Marpa::Internal::And_Node::RULE_ID,
        Marpa::Internal::And_Node::POSITION,
        Marpa::Internal::And_Node::FIXED_RANKING_DATA,
        Marpa::Internal::And_Node::RANKING_CLOSURE,
        )
    {
        $new_and_node->[$field] = $and_node->[$field];
    } ## end for my $field ( Marpa::Internal::And_Node::VALUE_REF,...)

    # link the newly cloned and-node to
    # its or-node parent
    $new_parent_or_node_id //=
        $and_node->[Marpa::Internal::And_Node::PARENT_ID];

    my $new_parent_or_node = $or_nodes->[$new_parent_or_node_id];
    my $siblings = $new_parent_or_node->[Marpa::Internal::Or_Node::CHILD_IDS];

    $new_and_node->[Marpa::Internal::And_Node::PARENT_CHOICE] = @{$siblings};
    $new_and_node->[Marpa::Internal::And_Node::PARENT_ID] =
        $new_parent_or_node_id;
    push @{$siblings}, $new_and_node_id;

    my $tag = $and_node->[Marpa::Internal::And_Node::TAG];
    $tag =~ s{ [o] \d+ [a] \d+ \z }{}xms;
    $tag .= 'o' . $new_parent_or_node_id . 'a' . $new_and_node_id;
    $new_and_node->[Marpa::Internal::And_Node::TAG] = $tag;

    # link the newly cloned and-node
    # to its or-node children
    $child_or_node_id_translation //= {};
    FIELD:
    for my $field (
        Marpa::Internal::And_Node::CAUSE_ID,
        Marpa::Internal::And_Node::PREDECESSOR_ID
        )
    {
        my $old_child_or_node_id = $and_node->[$field];
        next FIELD if not defined $old_child_or_node_id;
        my $new_child_or_node_id =
            $child_or_node_id_translation->{$old_child_or_node_id};
        $new_child_or_node_id //= $old_child_or_node_id;

        my $new_or_child = $or_nodes->[$new_child_or_node_id];

        $new_and_node->[$field] = $new_child_or_node_id;
        push @{ $new_or_child->[Marpa::Internal::Or_Node::PARENT_IDS] },
            $new_and_node_id;
    } ## end for my $field ( Marpa::Internal::And_Node::CAUSE_ID, ...)

    return $new_and_node;
} ## end sub clone_and_node

# Returns the number of nodes actually deleted
sub delete_nodes {
    my ( $evaler, $delete_work_list ) = @_;

    # Should be deletion-consistent at this point
    #### assert: Marpa'Evaluator'audit($evaler) or 1

    my $deleted_count = 0;

    my $and_nodes = $evaler->[Marpa::Internal::Evaluator::AND_NODES];
    my $or_nodes  = $evaler->[Marpa::Internal::Evaluator::OR_NODES];
    DELETE_WORK_ITEM:
    while ( my $delete_work_item = pop @{$delete_work_list} ) {
        my ( $node_type, $delete_node_id ) = @{$delete_work_item};

        if ( $node_type eq 'a' ) {

            my $delete_and_node = $and_nodes->[$delete_node_id];

            next DELETE_WORK_ITEM
                if $delete_and_node->[Marpa::Internal::And_Node::DELETED];

            my $parent_id =
                $delete_and_node->[Marpa::Internal::And_Node::PARENT_ID];
            my $parent_or_node = $or_nodes->[$parent_id];

            if ( not $parent_or_node->[Marpa::Internal::Or_Node::DELETED] ) {
                push @{$delete_work_list}, [ 'o', $parent_id ];
                my $parent_choice = $delete_and_node
                    ->[Marpa::Internal::And_Node::PARENT_CHOICE];

                my $parent_child_ids =
                    $parent_or_node->[Marpa::Internal::Or_Node::CHILD_IDS];

                splice @{$parent_child_ids}, $parent_choice, 1;

                # Eliminating one of the choices means all subsequent ones
                # are renumbered -- adjust accordingly.
                for my $choice ( $parent_choice .. $#{$parent_child_ids} ) {
                    my $sibling_and_node_id = $parent_child_ids->[$choice];
                    my $sibling_and_node = $and_nodes->[$sibling_and_node_id];
                    $sibling_and_node
                        ->[Marpa::Internal::And_Node::PARENT_CHOICE] =
                        $choice;

                } ## end for my $choice ( $parent_choice .. $#{...})

            } ## end if ( not $parent_or_node->[...])

            FIELD:
            for my $field (
                Marpa::Internal::And_Node::PREDECESSOR_ID,
                Marpa::Internal::And_Node::CAUSE_ID,
                )
            {
                my $child_or_node_id = $delete_and_node->[$field];
                next FIELD if not defined $child_or_node_id;
                my $child_or_node = $or_nodes->[$child_or_node_id];
                next FIELD
                    if $child_or_node->[Marpa::Internal::Or_Node::DELETED];

                push @{$delete_work_list}, [ 'o', $child_or_node_id ];

                # Splice out the reference to this or-node in the PARENT_IDS
                # field of the or-node child
                my $parent_ids =
                    $child_or_node->[Marpa::Internal::Or_Node::PARENT_IDS];

                my $delete_node_index =
                    List::Util::first { $parent_ids->[$_] == $delete_node_id }
                ( 0 .. $#{$parent_ids} );

                splice @{$parent_ids}, $delete_node_index, 1;
            }    # FIELD

            FIELD:
            for my $field (
                Marpa::Internal::And_Node::PARENT_ID,
                Marpa::Internal::And_Node::PARENT_CHOICE,
                Marpa::Internal::And_Node::CAUSE_ID,
                Marpa::Internal::And_Node::PREDECESSOR_ID,
                Marpa::Internal::And_Node::VALUE_REF,
                Marpa::Internal::And_Node::TOKEN,
                )
            {
                $delete_and_node->[$field] = undef;
            } ## end for my $field ( Marpa::Internal::And_Node::PARENT_ID,...)

            $delete_and_node->[Marpa::Internal::And_Node::DELETED] = 1;
            $deleted_count++;

            next DELETE_WORK_ITEM;
        } ## end if ( $node_type eq 'a' )

        if ( $node_type eq 'o' ) {

            my $or_node = $or_nodes->[$delete_node_id];
            next DELETE_WORK_ITEM
                if $or_node->[Marpa::Internal::Or_Node::DELETED];
            my $parent_ids = $or_node->[Marpa::Internal::Or_Node::PARENT_IDS];
            my $child_ids  = $or_node->[Marpa::Internal::Or_Node::CHILD_IDS];

            # Do not delete unless no children, or no parents and not the
            # start or-node.
            # Start or-node is always ID 0.

            next DELETE_WORK_ITEM
                if ( scalar @{$parent_ids} or $delete_node_id == 0 )
                and scalar @{$child_ids};

            $or_node->[Marpa::Internal::Or_Node::DELETED] = 1;
            $deleted_count++;

            push @{$delete_work_list},
                map { [ 'a', $_ ] } @{$parent_ids}, @{$child_ids};
            for my $field (
                Marpa::Internal::Or_Node::PARENT_IDS,
                Marpa::Internal::Or_Node::CHILD_IDS,
                )
            {
                $or_node->[$field] = [];
            } ## end for my $field ( Marpa::Internal::Or_Node::PARENT_IDS,...)

            next DELETE_WORK_ITEM;
        } ## end if ( $node_type eq 'o' )

        Marpa::exception("Unknown delete-work-list node-type: $node_type");
    } ## end while ( my $delete_work_item = pop @{$delete_work_list})
    return $deleted_count;
} ## end sub delete_nodes

# Rewrite to eliminate cycles.
sub rewrite_cycles {
    my ( $evaler, $cycle_rule_ids ) = @_;

    my $or_nodes  = $evaler->[Marpa::Internal::Evaluator::OR_NODES];
    my $and_nodes = $evaler->[Marpa::Internal::Evaluator::AND_NODES];

    my $trace_evaluation;

    my $grammar = $evaler->[Marpa::Internal::Evaluator::GRAMMAR];
    my $warn_on_cycle =
        $grammar->[Marpa::Internal::Grammar::CYCLE_ACTION] ne 'quiet';
    $trace_evaluation =
        $evaler->[Marpa::Internal::Evaluator::TRACE_EVALUATION];

    my $initial_and_nodes = @{$and_nodes};
    my $maximum_and_nodes = List::Util::max(
        $initial_and_nodes
            + $evaler->[Marpa::Internal::Evaluator::CYCLE_NODES],
        $initial_and_nodes
            * $evaler->[Marpa::Internal::Evaluator::CYCLE_SCALE]
    );

    my @cycle_rules;
    @cycle_rules[ @{$cycle_rule_ids} ] = (1) x scalar @{$cycle_rule_ids};
    my @cycle_or_nodes =
        grep { not $_->[Marpa::Internal::Or_Node::DELETED] }
        map  { $or_nodes->[ $_->[Marpa::Internal::And_Node::PARENT_ID] ] }
        grep {
        not $_->[Marpa::Internal::And_Node::DELETED]
            and $cycle_rules[ $_->[Marpa::Internal::And_Node::RULE_ID] ]
        } @{$and_nodes};

    # Group or-nodes by span.  Only or-nodes with the same
    # span can be in a cycle.
    my %or_nodes_by_span;
    for my $or_node (@cycle_or_nodes) {
        push @{
            $or_nodes_by_span{
                join q{,},
                @{$or_node}[
                    Marpa::Internal::Or_Node::START_EARLEME,
                Marpa::Internal::Or_Node::END_EARLEME
                ]
                }
            },
            $or_node;
    } ## end for my $or_node (@cycle_or_nodes)

    # Initialize the span sets
    my @span_sets = values %or_nodes_by_span;

    SPAN_SET: while ( my $span_set = pop @span_sets ) {
        @{$span_set} =
            grep { not $_->[Marpa::Internal::Or_Node::DELETED] } @{$span_set};
        next SPAN_SET if not @{$span_set};

        my %in_span_set = ();
        for my $or_node_ix ( 0 .. $#{$span_set} ) {
            my $or_node_id =
                $span_set->[$or_node_ix]->[Marpa::Internal::Or_Node::ID];

            $in_span_set{$or_node_id} = $or_node_ix;
        } ## end for my $or_node_ix ( 0 .. $#{$span_set} )

        # Set up matrix of or-node to or-node transitions.
        my @transition;
        my @work_list;
        for my $or_parent_ix ( 0 .. $#{$span_set} ) {
            my @or_child_ixes =
                grep { defined $_ }
                map  { $in_span_set{$_} }
                grep { defined $_ }
                map {
                @{$_}[
                    Marpa::Internal::And_Node::CAUSE_ID,
                    Marpa::Internal::And_Node::PREDECESSOR_ID
                    ]
                } @{$and_nodes}[
                @{ $span_set->[$or_parent_ix]
                        ->[Marpa::Internal::Or_Node::CHILD_IDS] }
                ];
            for my $or_child_ix (@or_child_ixes) {
                $transition[$or_parent_ix][$or_child_ix]++;
                push @work_list, [ $or_parent_ix, $or_child_ix ];
            }
        } ## end for my $or_parent_ix ( 0 .. $#{$span_set} )

        # Compute transitive closure of matrix of or-node transitions.
        while ( my $work_item = pop @work_list ) {
            my ( $from_ix, $to_ix ) = @{$work_item};
            GRAND_CHILD:
            for my $new_to_ix ( grep { $transition[$to_ix][$_] }
                ( 0 .. $#{$span_set} ) )
            {
                my $transition_row = $transition[$from_ix];
                next GRAND_CHILD if $transition_row->[$new_to_ix];
                $transition_row->[$new_to_ix]++;
                push @work_list, [ $from_ix, $new_to_ix ];
            } ## end for my $new_to_ix ( grep { $transition[$to_ix][$_] } ...)
        } ## end while ( my $work_item = pop @work_list )

        # Use the transitions to find the cycles in the span set
        my @cycle;
        {
            my $span_set_index =
                List::Util::first { $transition[$_][$_] }
            ( 0 .. $#{$span_set} );
            next SPAN_SET if not defined $span_set_index;
            @cycle = map { $span_set->[$_] } (
                $span_set_index,
                grep {
                            $transition[$span_set_index][$_]
                        and $transition[$_][$span_set_index]
                    } ( $span_set_index + 1 .. $#{$span_set} )
            );
        }

        if ($trace_evaluation) {
            say {$Marpa::Internal::TRACE_FH} 'Found cycle of length ',
                ( scalar @cycle )
                or Marpa::exception("Cannot print: $ERRNO");
            for my $ix ( 0 .. $#cycle ) {
                my $or_node = $cycle[$ix];
                print {$Marpa::Internal::TRACE_FH} "Node $ix in cycle: ",
                    Marpa::Evaluator::show_or_node( $evaler, $or_node,
                    $trace_evaluation )
                    or Marpa::exception('print to trace handle failed');
            } ## end for my $ix ( 0 .. $#cycle )
        } ## end if ($trace_evaluation)

        # If we found any cycles in the span set, put the
        # whole span set back
        # on the work list for another pass
        push @span_sets, $span_set;

        # Find the internal and-nodes in the cycle
        my %internal_and_nodes = ();
        for my $or_node (@cycle) {
            for my $and_node_id (
                @{ $or_node->[Marpa::Internal::Or_Node::CHILD_IDS] } )
            {
                $internal_and_nodes{$and_node_id} = 1;
            }
        } ## end for my $or_node (@cycle)

        # Find the root or-nodes in the cycle
        # They are the or-nodes, at least
        # one of whose parent and-nodes
        # are external.
        my @root_or_nodes = grep {
            grep { not( $_ ~~ \%internal_and_nodes ) }
                @{ $_->[Marpa::Internal::Or_Node::PARENT_IDS] }
        } @cycle;

        ## deletion-consistent at this point
        #### assert: Marpa'Evaluator'audit($evaler) or 1

        my @delete_work_list = ();

        # now make the copies
        for my $copy ( 1 .. $#root_or_nodes ) {

            my $original_root_or_node = $root_or_nodes[$copy];
            my $original_root_or_node_id =
                $original_root_or_node->[Marpa::Internal::Or_Node::ID];

            # Copy non-link dependent fields
            # Make translation tables
            # Create interior and-node to or-node links
            my %translate_or_node_id;
            my %translate_and_node_id;

            # store our new cycle set here, so we can add it
            # to the span set work list
            my @copied_cycle;

            # Copy the or- and and-nodes and build the translation
            # tables.
            for my $or_node (@cycle) {
                my $or_node_id = $or_node->[Marpa::Internal::Or_Node::ID];

                my $new_or_node;
                $#{$new_or_node} = Marpa::Internal::Or_Node::LAST_FIELD;
                for my $field (
                    Marpa::Internal::Or_Node::START_EARLEME,
                    Marpa::Internal::Or_Node::END_EARLEME,
                    Marpa::Internal::Or_Node::TAG,
                    )
                {
                    $new_or_node->[$field] = $or_node->[$field];
                } ## end for my $field ( ...)

                my $new_or_node_id = @{$or_nodes};
                $new_or_node->[Marpa::Internal::Or_Node::ID] =
                    $new_or_node_id;
                $new_or_node->[Marpa::Internal::Or_Node::TAG] =~ s{
                        [o] \d* \z
                    }{o$new_or_node_id}xms;
                $new_or_node->[Marpa::Internal::Or_Node::CHILD_IDS] = [];

                push @{$or_nodes}, $new_or_node;
                push @copied_cycle, $new_or_node;
                $translate_or_node_id{$or_node_id} = $new_or_node_id;
            } ## end for my $or_node (@cycle)

            for my $old_or_node (@cycle) {
                my $old_or_node_id =
                    $old_or_node->[Marpa::Internal::Or_Node::ID];
                my $new_or_node_id = $translate_or_node_id{$old_or_node_id};
                for my $old_child_and_node_id (
                    @{ $old_or_node->[Marpa::Internal::Or_Node::CHILD_IDS] } )
                {
                    my $old_child_and_node =
                        $and_nodes->[$old_child_and_node_id];

                    my $new_child_and_node = clone_and_node(
                        $evaler,         $old_child_and_node,
                        $new_or_node_id, \%translate_or_node_id
                    );
                    my $new_child_and_node_id =
                        $new_child_and_node->[Marpa::Internal::And_Node::ID];
                    if ( $new_child_and_node_id > $maximum_and_nodes ) {
                        Marpa::exception(
                            "Cycle produced too many nodes: $maximum_and_nodes\n",
                            "Rewrite grammar or increase cycle_scale\n"
                        );
                    } ## end if ( $new_child_and_node_id > $maximum_and_nodes )
                    $translate_and_node_id{$old_child_and_node_id} =
                        $new_child_and_node_id;

                } ## end for my $old_child_and_node_id ( @{ $old_or_node->[...]})

            } ## end for my $old_or_node (@cycle)

            # Translate the cycle-internal links
            # and duplicate the outgoing external links (which
            # will be from the and-nodes)

            for my $original_or_node (@cycle) {

                my $original_or_node_id =
                    $original_or_node->[Marpa::Internal::Or_Node::ID];
                my $new_or_node_id =
                    $translate_or_node_id{$original_or_node_id};
                my $new_or_node = $or_nodes->[$new_or_node_id];

                # This throws away all external links to the or-nodes,
                # for the moment.  Below, I'll re-add the ones for the
                # root node.
                $new_or_node->[Marpa::Internal::Or_Node::PARENT_IDS] = [
                    grep    { defined $_ }
                        map { $translate_and_node_id{$_} } @{
                        $original_or_node
                            ->[Marpa::Internal::Or_Node::PARENT_IDS]
                        }
                ];

            } ## end for my $original_or_node (@cycle)

            # It remains now to duplicate the external links to the cycle
            # and to mark internal links to the root node for deletion.
            # External links are allowed only to the root node of the cycle.

            my $new_root_or_node_id =
                $translate_or_node_id{ $original_root_or_node
                    ->[Marpa::Internal::Or_Node::ID] };

            my $new_root_or_node = $or_nodes->[$new_root_or_node_id];

            PARENT_AND_NODE:
            for my $original_parent_and_node_id (
                @{  $original_root_or_node
                        ->[Marpa::Internal::Or_Node::PARENT_IDS]
                }
                )
            {

                # Internal nodes need to be put on the list to be deleted
                if (defined(
                        my $new_parent_and_node_id =
                            $translate_and_node_id{
                            $original_parent_and_node_id}
                    )
                    )
                {
                    push @delete_work_list, [ 'a', $new_parent_and_node_id ];
                    next PARENT_AND_NODE;
                } ## end if ( defined( my $new_parent_and_node_id = ...))

                # If we are here, the parent node is cycle-external.

                # Clone the external parent node
                my $old_parent_and_node =
                    $and_nodes->[$original_parent_and_node_id];
                my $new_parent_and_node =
                    clone_and_node( $evaler, $old_parent_and_node, undef,
                    { $original_root_or_node_id => $new_root_or_node_id } );

                Marpa::exception( 'Rewrite of intertwined nulling cycles',
                    ' not yet implemented' )
                    if grep { defined and defined $translate_or_node_id{$_} }
                        @{$new_parent_and_node}[
                        Marpa::Internal::And_Node::CAUSE_ID,
                    Marpa::Internal::And_Node::PREDECESSOR_ID
                        ];

            } ## end for my $original_parent_and_node_id ( @{ ...})

            push @span_sets, \@copied_cycle;

            # Should be deletion-consistent at this point
            #### assert: Marpa'Evaluator'audit($evaler) or 1

        } ## end for my $copy ( 1 .. $#root_or_nodes )

        ## DELETE non-root external link on original
        ## DELETE root internal links on original
        my $original_root_or_node = $root_or_nodes[0];
        for my $original_or_node (@cycle) {
            my $is_root = $original_or_node == $original_root_or_node;
            PARENT_AND_NODE:
            for my $original_parent_and_node_id (
                @{ $original_or_node->[Marpa::Internal::Or_Node::PARENT_IDS] }
                )
            {

                next PARENT_AND_NODE
                    if $is_root
                        xor $internal_and_nodes{$original_parent_and_node_id};

                push @delete_work_list, [ 'a', $original_parent_and_node_id ];
            } ## end for my $original_parent_and_node_id ( @{ ...})
        } ## end for my $original_or_node (@cycle)

        # we should be deletion-consistent at this point

        # Now actually do the deletions
        delete_nodes( $evaler, \@delete_work_list );

        # Should be deletion-consistent at this point
        #### assert: Marpa'Evaluator'audit($evaler) or 1

        # Have we deleted the top or-node?
        # If so, there will be no parses.
        if ( $or_nodes->[0]->[Marpa::Internal::Or_Node::DELETED] ) {
            if ($warn_on_cycle) {
                print {$Marpa::Internal::TRACE_FH}
                    "Cycles found, but no parses\n"
                    or Marpa::exception('print to trace handle failed');
            }
            return;
        } ## end if ( $or_nodes->[0]->[Marpa::Internal::Or_Node::DELETED...])

    } ## end while ( my $span_set = pop @span_sets )

    ### assert: Marpa'Evaluator'audit($evaler) or 1

    return;
} ## end sub rewrite_cycles

=begin Implementation:

Deleting nodes can change the equivalence classes (EC), so we need
multiple passes.  In practice two passes should suffice in almost
all cases.

Deleting nodes combines ECs; never splits them.  You can prove this
by induction on the node levels, where a level 0 node has no children,
and a level n+1 node has children of level n or less.

Level 0 nodes (always terminal and-nodes) will always have the same
signature regardless of node deletions.  So if two level 0 nodes are in
the same EC before a set of deletions, they will be after.

Induction hypothesis: any two nodes of level n in a common EC before a
set of deletions, will be in a common EC after the set of deletions.

Two level n+1 or-nodes in the same EC: The EC's of their children must
have been the same.  Since deletions are based on the EC of the children
on a per or-node basis, the same deletions will be made in both level n+1
or-nodes.  And by the induction hypothesis, any node in an EC with one of
the children before the set of deletions, also shares and EC afterwards.
So the signature of the two level n+1 or-nodes will remain identical.

Two level n+1 and-nodes: If either child is deleted, the level n+1
and-node is also deleted and becomes irrelevant.  By the induction
hypothesis, and following the same argument as for level n+1 or-node
children, the signatures of the two level n+1 and-nodes will remain the
same, and they will remain together in an EC.

=end Implementation:

=cut

# Negative so they cannot be the same as the ID of any
# actual child and-node or or-node.
use constant CHILD_IS_PRESENT => -2;
use constant CHILD_IS_ABSENT  => -1;

# Make sure and-nodes are unique.
sub delete_duplicate_nodes {

    my ($evaler) = @_;

    my $trace_evaluation =
        $evaler->[Marpa::Internal::Evaluator::TRACE_EVALUATION];

    my $or_nodes  = $evaler->[Marpa::Internal::Evaluator::OR_NODES];
    my $and_nodes = $evaler->[Marpa::Internal::Evaluator::AND_NODES];

    # Should the CAUSE_EARLEME be added to the base signature?

    # The base signatures
    # never change except when an and-node is deleted.
    # In that case the base signature is never examined.
    # It becomes irrelevant, and the obsolete
    # entry is harmless.
    my @and_base_signatures;
    for my $and_node ( @{$and_nodes} ) {
        my $and_node_id = $and_node->[Marpa::Internal::And_Node::ID];
        my $token       = $and_node->[Marpa::Internal::And_Node::TOKEN];
        $and_base_signatures[$and_node_id] =
            join q{,},
            $and_node->[Marpa::Internal::And_Node::RULE_ID],
            $and_node->[Marpa::Internal::And_Node::POSITION],
            $and_node->[Marpa::Internal::And_Node::START_EARLEME],
            $and_node->[Marpa::Internal::And_Node::END_EARLEME],
            ( defined $token ? $token->[Marpa::Internal::Symbol::ID] : -1 );
    } ## end for my $and_node ( @{$and_nodes} )

    # As long as duplicates are found, we continue to loop
    DELETE_DUPLICATE_PASS: while (1) {

        # We start with a first cut at the equivalence classes,
        # and refine.  When we can't refine any more, we have
        # our equivalence classes

        # Initially, lump everything into one huge proto-equivalence
        # class.
        my $and_class_by_signature =
            { INITIAL => Marpa::Internal::Evaluator::CHILD_IS_PRESENT };
        my $or_class_by_signature =
            { INITIAL => Marpa::Internal::Evaluator::CHILD_IS_PRESENT };
        my $and_node_ids_by_signature = {
            INITIAL => [
                grep {
                    not $and_nodes->[$_]->[Marpa::Internal::And_Node::DELETED]
                    } ( 0 .. $#{$and_nodes} )
            ]
        };
        my $or_node_ids_by_signature = {
            INITIAL => [
                grep {
                    not $or_nodes->[$_]->[Marpa::Internal::Or_Node::DELETED]
                    } ( 0 .. $#{$or_nodes} )
            ]
        };
        my $or_class_by_id =
            [ (Marpa::Internal::Evaluator::CHILD_IS_PRESENT) x
                scalar @{$or_nodes} ];

        REFINE_CLASSES_PASS: while (1) {

            my $changed = 0;

            my $new_and_class_by_signature    = {};
            my $new_or_class_by_signature     = {};
            my $new_and_node_ids_by_signature = {};
            my $new_or_node_ids_by_signature  = {};

            my $and_class_by_id = [];
            $#{$and_class_by_id} = $#{$and_nodes};
            my $new_or_class_by_id = [];
            $#{$new_or_class_by_id} = $#{$or_nodes};

            AND_CLASS:
            while ( my ( $signature, $and_node_ids ) =
                each %{$and_node_ids_by_signature} )
            {

                for my $and_node_id ( @{$and_node_ids} ) {

                    # Deleted nodes should never make it in here
                    my $new_signature =
                        $and_base_signatures[$and_node_id] . q{;}
                        . (
                        join q{,},
                        map { defined $_ ? $or_class_by_id->[$_] : -1 }
                            @{ $and_nodes->[$and_node_id] }[
                            Marpa::Internal::And_Node::PREDECESSOR_ID,
                        Marpa::Internal::And_Node::CAUSE_ID
                            ]
                        );
                    $changed ||= $new_signature ne $signature;

                    my $new_class =
                        $new_and_class_by_signature->{$new_signature};
                    if ( not defined $new_class ) {
                        $new_class =
                            $new_and_class_by_signature->{$new_signature} =
                            $and_node_id;
                    }
                    $and_class_by_id->[$and_node_id] = $new_class;
                    push
                        @{ $new_and_node_ids_by_signature->{$new_signature} },
                        $and_node_id;

                } ## end for my $and_node_id ( @{$and_node_ids} )
            } ## end while ( my ( $signature, $and_node_ids ) = each %{...})

            OR_CLASS:
            while ( my ( $signature, $or_node_ids ) =
                each %{$or_node_ids_by_signature} )
            {

                for my $or_node_id ( @{$or_node_ids} ) {

                    # Deleted nodes should never make it in here
                    my $new_signature =
                        join q{,},
                        sort map { $and_class_by_id->[$_] }
                        @{ $or_nodes->[$or_node_id]
                            ->[Marpa::Internal::Or_Node::CHILD_IDS] };
                    $changed ||= $new_signature ne $signature;

                    my $new_class =
                        $new_or_class_by_signature->{$new_signature};
                    if ( not defined $new_class ) {
                        $new_class =
                            $new_or_class_by_signature->{$new_signature} =
                            $or_node_id;
                    }
                    $new_or_class_by_id->[$or_node_id] = $new_class;
                    push
                        @{ $new_or_node_ids_by_signature->{$new_signature} },
                        $or_node_id;

                } ## end for my $or_node_id ( @{$or_node_ids} )
            } ## end while ( my ( $signature, $or_node_ids ) = each %{...})

            last REFINE_CLASSES_PASS if not $changed;

            $and_class_by_signature    = $new_and_class_by_signature;
            $or_class_by_signature     = $new_or_class_by_signature;
            $and_node_ids_by_signature = $new_and_node_ids_by_signature;
            $or_node_ids_by_signature  = $new_or_node_ids_by_signature;
            $or_class_by_id            = $new_or_class_by_id;

        } ## end while (1)

        my @delete_work_list = ();
        AND_CLASS:
        while ( my ( $signature, $and_node_ids ) =
            each %{$and_node_ids_by_signature} )
        {
            next AND_CLASS if scalar @{$and_node_ids} <= 1;

            # We delete and-nodes in the same equivalence class
            # if they have the same parent
            my %parent;
            AND_NODE: for my $and_node_id ( @{$and_node_ids} ) {
                next AND_NODE
                    if not $parent{
                            $and_nodes->[$and_node_id]
                                ->[Marpa::Internal::And_Node::PARENT_ID]
                        }++;

                push @delete_work_list, [ 'a', $and_node_id ];

                next AND_NODE if not $trace_evaluation;

                print {$Marpa::Internal::TRACE_FH}
                    "Deleting duplicate and-node:\n",
                    $and_nodes->[$and_node_id]
                    ->[Marpa::Internal::And_Node::TAG], "\n"
                    or Marpa::exception('print to trace handle failed');

            } ## end for my $and_node_id ( @{$and_node_ids} )
        } ## end while ( my ( $signature, $and_node_ids ) = each %{...})

        # If no nodes are deleted, we are finished
        last DELETE_DUPLICATE_PASS
            if not scalar @delete_work_list
                or delete_nodes( $evaler, \@delete_work_list ) <= 0;

    } ## end while (1)

    return;

} ## end sub delete_duplicate_nodes

# Returns false if no parse
sub Marpa::Evaluator::new {
    my ( $class, @arg_hashes ) = @_;

    ### Constructing new evaluator
    my $self = bless [], $class;

    my $recce;
    my $parse_set_arg;

    local $Marpa::Internal::EXPLICIT_CLOSURES = {};

    for my $arg_hash (@arg_hashes) {

        my @recce_arg_values =
            grep {defined} @{$arg_hash}{qw(recognizer recce)};
        if ( not defined $recce ) {
            Marpa::exception('recognizer specified more than once')
                if scalar @recce_arg_values > 1;
            $recce = shift @recce_arg_values;
        }
        else {
            Marpa::exception('recognizer specified more than once')
                if scalar @recce_arg_values;
        }
        delete @{$arg_hash}{qw(recognizer recce)};

        if ( defined $arg_hash->{end} ) {
            $parse_set_arg = $arg_hash->{end};
            delete $arg_hash->{end};
        }

        if ( defined $arg_hash->{closures} ) {
            $Marpa::Internal::EXPLICIT_CLOSURES = $arg_hash->{closures};
            delete $arg_hash->{closures};
        }

    } ## end for my $arg_hash (@arg_hashes)

    Marpa::exception('No recognizer specified') if not defined $recce;
    my $recce_class = ref $recce;
    Marpa::exception(
        "${class}::new() recognizer arg has wrong class: $recce_class")
        if $recce_class ne 'Marpa::Recognizer';

    my $grammar = $recce->[Marpa::Internal::Recognizer::GRAMMAR];
    $self->[Marpa::Internal::Evaluator::GRAMMAR] = $grammar;

    local $Marpa::Internal::TRACE_FH =
        $self->[Marpa::Internal::Evaluator::TRACE_FILE_HANDLE] =
        $recce->[Marpa::Internal::Recognizer::TRACE_FILE_HANDLE];

    my $earley_sets = $recce->[Marpa::Internal::Recognizer::EARLEY_SETS];

    my $furthest_earleme =
        $recce->[Marpa::Internal::Recognizer::FURTHEST_EARLEME];
    my $last_completed_earleme =
        $recce->[Marpa::Internal::Recognizer::LAST_COMPLETED_EARLEME];
    Marpa::exception(
        "Attempt to evaluate incompletely recognized parse:\n",
        "  Last token ends at location $furthest_earleme\n",
        "  Recognition done only as far as location $last_completed_earleme\n"
    ) if $furthest_earleme > $last_completed_earleme;

    # default settings
    $self->[Marpa::Internal::Evaluator::CYCLE_NODES]   = 1000;
    $self->[Marpa::Internal::Evaluator::CYCLE_SCALE]   = 2;
    $self->[Marpa::Internal::Evaluator::CYCLE_REWRITE] = 1;
    $self->[Marpa::Internal::Evaluator::MAX_PARSES]    = -1;
    $self->[Marpa::Internal::Evaluator::PARSE_ORDER]   = 'numeric';
    $self->[Marpa::Internal::Evaluator::TRACE_VALUES]  = 0;

    $self->set(@arg_hashes);

    my $rules   = $grammar->[Marpa::Internal::Grammar::RULES];
    my $symbols = $grammar->[Marpa::Internal::Grammar::SYMBOLS];

    my $parse_order = $self->[Marpa::Internal::Evaluator::PARSE_ORDER];

    my $trace_tasks = $self->[Marpa::Internal::Evaluator::TRACE_TASKS];

    $self->[Marpa::Internal::Evaluator::PARSE_COUNT] = 0;
    my $or_nodes  = $self->[Marpa::Internal::Evaluator::OR_NODES]  = [];
    my $and_nodes = $self->[Marpa::Internal::Evaluator::AND_NODES] = [];

    my $current_parse_set = $parse_set_arg
        // $recce->[Marpa::Internal::Recognizer::FURTHEST_EARLEME];

    # Look for the start item and start rule
    my $earley_set = $earley_sets->[$current_parse_set];

    my $start_item;
    my $start_rule;
    my $start_state;

    EARLEY_ITEM: for my $item ( @{$earley_set} ) {
        $start_state = $item->[Marpa::Internal::Earley_Item::STATE];
        $start_rule  = $start_state->[Marpa::Internal::QDFA::START_RULE];
        next EARLEY_ITEM if not $start_rule;
        $start_item = $item;
        last EARLEY_ITEM;
    } ## end for my $item ( @{$earley_set} )

    return if not $start_rule;

    my $start_rule_id = $start_rule->[Marpa::Internal::Rule::ID];

    local $Marpa::Internal::TRACE_ACTIONS =
        $self->[Marpa::Internal::Evaluator::TRACE_ACTIONS];

    my $null_values;
    $null_values = set_null_values($grammar);

    # Set up rank closures by symbol
    my $ranking_closures_by_symbol =
        $self->[Marpa::Internal::Evaluator::RANKING_CLOSURES_BY_SYMBOL] = [];
    $#{$ranking_closures_by_symbol} = $#{$symbols};
    SYMBOL: for my $symbol ( @{$symbols} ) {
        my $ranking_action =
            $symbol->[Marpa::Internal::Symbol::RANKING_ACTION];
        next SYMBOL if not defined $ranking_action;
        my $ranking_closure =
            Marpa::Internal::Evaluator::resolve_semantics( $grammar,
            $ranking_action );
        Marpa::exception("Ranking closure '$ranking_action' not found")
            if not defined $ranking_closure;
        $ranking_closures_by_symbol->[ $symbol->[Marpa::Internal::Symbol::ID]
        ] = $ranking_closure;
    } ## end for my $symbol ( @{$symbols} )

    my $evaluator_rules =
        $self->[Marpa::Internal::Evaluator::RULE_VALUE_OPS] =
        set_actions($grammar);

    # Get closure used in ranking, by rule
    my $ranking_closures_by_rule =
        $self->[Marpa::Internal::Evaluator::RANKING_CLOSURES_BY_RULE] = [];
    $#{$ranking_closures_by_rule} = $#{$rules};
    RULE: for my $rule ( @{$rules} ) {
        next RULE
            if not my $ranking_action =
                $rule->[Marpa::Internal::Rule::RANKING_ACTION];

        # If the RHS is empty ...
        if ( not scalar @{ $rule->[Marpa::Internal::Rule::RHS] } ) {
            my $ranking_closure =
                Marpa::Internal::Evaluator::resolve_semantics( $grammar,
                $ranking_action );
            Marpa::exception("Ranking closure '$ranking_action' not found")
                if not defined $ranking_closure;

            $ranking_closures_by_symbol->[ $rule->[Marpa::Internal::Rule::LHS]
                ->[Marpa::Internal::Symbol::NULL_ALIAS]
                ->[Marpa::Internal::Symbol::ID] ] = $ranking_closure;
        } ## end if ( not scalar @{ $rule->[Marpa::Internal::Rule::RHS...]})

        next RULE if not $rule->[Marpa::Internal::Rule::USEFUL];
        my $ranking_closure =
            Marpa::Internal::Evaluator::resolve_semantics( $grammar,
            $ranking_action );
        Marpa::exception("Ranking closure '$ranking_action' not found")
            if not defined $ranking_closure;
        $ranking_closures_by_rule->[ $rule->[Marpa::Internal::Rule::ID] ] =
            $ranking_closure;
    } ## end for my $rule ( @{$rules} )

    if (defined(
            my $action_object =
                $grammar->[Marpa::Internal::Grammar::ACTION_OBJECT]
        )
        )
    {
        my $constructor_name = $action_object . q{::new};
        my $closure = resolve_semantics( $grammar, $constructor_name );
        Marpa::exception(qq{Could not find constructor "$constructor_name"})
            if not defined $closure;
        $self->[Marpa::Internal::Evaluator::ACTION_OBJECT_CONSTRUCTOR] =
            $closure;
    } ## end if ( defined( my $action_object = $grammar->[...]))

    $self->[Marpa::Internal::Evaluator::SEMANTICS_SETTLED] = 1;

    my @tree_rules;
    $#tree_rules = $#{$rules};
    my @cycle_rule_ids =
        map { $_->[Marpa::Internal::Rule::ID] }
        @{ Marpa::Internal::Grammar::cycle_rules($grammar) };
    @tree_rules[@cycle_rule_ids] =
        ( [Marpa::Internal::Evaluator_Op::CYCLE] ) x scalar @cycle_rule_ids;

    my $start_symbol = $start_rule->[Marpa::Internal::Rule::LHS];
    my ( $nulling, $symbol_id ) =
        @{$start_symbol}[ Marpa::Internal::Symbol::NULLING,
        Marpa::Internal::Symbol::ID, ];
    my $start_null_value = $null_values->[$symbol_id];

    # deal with a null parse as a special case
    if ($nulling) {

        my $or_node = [];
        $#{$or_node} = Marpa::Internal::Or_Node::LAST_FIELD;

        my $and_node = [];
        $#{$and_node} = Marpa::Internal::And_Node::LAST_FIELD;

        $or_node->[Marpa::Internal::Or_Node::CHILD_IDS]     = [0];
        $or_node->[Marpa::Internal::Or_Node::START_EARLEME] = 0;
        $or_node->[Marpa::Internal::Or_Node::END_EARLEME]   = 0;
        my $or_node_id = $or_node->[Marpa::Internal::Or_Node::ID] = 0;
        my $or_node_tag = $or_node->[Marpa::Internal::Or_Node::TAG] =
            $start_item->[Marpa::Internal::Earley_Item::NAME]
            . "o$or_node_id";

        $and_node->[Marpa::Internal::And_Node::VALUE_REF] =
            \$start_null_value;
        $and_node->[Marpa::Internal::And_Node::TREE_OPS] =
            $tree_rules[$start_rule_id];
        $and_node->[Marpa::Internal::And_Node::VALUE_OPS] =
            $evaluator_rules->[$start_rule_id];
        $and_node->[Marpa::Internal::And_Node::RULE_ID]  = $start_rule_id;
        $and_node->[Marpa::Internal::And_Node::POSITION] = -1;
        $and_node->[Marpa::Internal::And_Node::START_EARLEME] = 0;
        $and_node->[Marpa::Internal::And_Node::END_EARLEME]   = 0;
        $and_node->[Marpa::Internal::And_Node::CAUSE_EARLEME] = 0;
        $and_node->[Marpa::Internal::And_Node::PARENT_ID]     = 0;
        $and_node->[Marpa::Internal::And_Node::PARENT_CHOICE] = 0;
        given ($parse_order) {
            when ('numeric') {
                $and_node->[Marpa::Internal::And_Node::FIXED_RANKING_DATA] =
                    0;
                $and_node->[Marpa::Internal::And_Node::RANKING_CLOSURE] =
                    $ranking_closures_by_rule->[$start_rule_id];
            } ## end when ('numeric')
        } ## end given
        my $and_node_id = $and_node->[Marpa::Internal::And_Node::ID] = 0;
        $and_node->[Marpa::Internal::And_Node::TAG] =
            $or_node_tag . "a$and_node_id";

        push @{$or_nodes},  $or_node;
        push @{$and_nodes}, $and_node;

        return $self;

    }    # if $nulling

    my @or_saplings;
    my %or_node_by_name;
    my $start_sapling = [];
    {
        my $start_name = $start_item->[Marpa::Internal::Earley_Item::NAME];
        my $start_symbol_id = $start_symbol->[Marpa::Internal::Symbol::ID];
        $start_name .= 'L' . $start_symbol_id;
        $start_sapling->[Marpa::Internal::Or_Sapling::NAME] = $start_name;
    }
    $start_sapling->[Marpa::Internal::Or_Sapling::ITEM] = $start_item;
    $start_sapling->[Marpa::Internal::Or_Sapling::CHILD_LHS_SYMBOL] =
        $start_symbol;
    push @or_saplings, $start_sapling;

    OR_SAPLING: while ( my $or_sapling = pop @or_saplings ) {

        my $sapling_name = $or_sapling->[Marpa::Internal::Or_Sapling::NAME];
        my $item         = $or_sapling->[Marpa::Internal::Or_Sapling::ITEM];
        my $child_lhs_symbol =
            $or_sapling->[Marpa::Internal::Or_Sapling::CHILD_LHS_SYMBOL];
        my $rule     = $or_sapling->[Marpa::Internal::Or_Sapling::RULE];
        my $position = $or_sapling->[Marpa::Internal::Or_Sapling::POSITION];

        # If we don't have a current rule, we need to get one or
        # more rules, and deduce the position and a new symbol from
        # them.
        my @and_saplings;

        my $is_kernel_or_node = defined $position;

        if ($is_kernel_or_node) {

            # Kernel or-node: We have a rule and a position.
            # get the current symbol

            $position--;
            my $symbol = $rule->[Marpa::Internal::Rule::RHS]->[$position];
            push @and_saplings, [ $rule, $position, $symbol ];

        } ## end if ($is_kernel_or_node)
        else {

            # Closure or-node.

            my $child_lhs_id =
                $child_lhs_symbol->[Marpa::Internal::Symbol::ID];
            my $state = $item->[Marpa::Internal::Earley_Item::STATE];
            for my $rule (
                @{  $state->[Marpa::Internal::QDFA::COMPLETE_RULES]
                        ->[$child_lhs_id];
                }
                )
            {

                my $rhs = $rule->[Marpa::Internal::Rule::RHS];

                my $last_position = @{$rhs} - 1;
                push @and_saplings,
                    [
                    $rule,
                    $last_position,
                    $rhs->[$last_position],
                    $evaluator_rules->[ $rule->[Marpa::Internal::Rule::ID] ]
                    ];

            }    # for my $rule

        }    # closure or-node

        my $start_earleme = $item->[Marpa::Internal::Earley_Item::PARENT];
        my $end_earleme   = $item->[Marpa::Internal::Earley_Item::SET];

        my @child_and_nodes;

        my $item_name = $item->[Marpa::Internal::Earley_Item::NAME];

        for my $and_sapling (@and_saplings) {

            my ($sapling_rule, $sapling_position,
                $symbol,       $value_processing
            ) = @{$and_sapling};

            my $rule_id     = $sapling_rule->[Marpa::Internal::Rule::ID];
            my $rhs         = $sapling_rule->[Marpa::Internal::Rule::RHS];
            my $rule_length = @{$rhs};

            my @or_bud_list;
            if ( $symbol->[Marpa::Internal::Symbol::NULLING] ) {
                my $nulling_symbol_id =
                    $symbol->[Marpa::Internal::Symbol::ID];
                my $null_value = $null_values->[$nulling_symbol_id];
                @or_bud_list = ( [ $item, undef, $symbol, \$null_value, ] );
            } ## end if ( $symbol->[Marpa::Internal::Symbol::NULLING] )
            else {
                @or_bud_list = (
                    (   map { [ $_->[0], undef, @{$_}[ 1, 2 ] ] }
                            @{ $item->[Marpa::Internal::Earley_Item::TOKENS] }
                    ),
                    (   map { [ $_->[0], $_->[1] ] }
                            @{ $item->[Marpa::Internal::Earley_Item::LINKS] }
                    )
                );
            } ## end else [ if ( $symbol->[Marpa::Internal::Symbol::NULLING] ) ]

            for my $or_bud (@or_bud_list) {

                my ( $predecessor, $cause, $token, $value_ref ) = @{$or_bud};

                my $predecessor_name;

                if ( $sapling_position > 0 ) {

                    $predecessor_name =
                        $predecessor->[Marpa::Internal::Earley_Item::NAME]
                        . "R$rule_id:$sapling_position";

                    # We check that the predecessor has not already been
                    # processed so that cycles don't put us into a loop
                    if ( not $predecessor_name ~~ %or_node_by_name ) {

                        $or_node_by_name{$predecessor_name} = [];

                        my $sapling = [];
                        @{$sapling}[
                            Marpa::Internal::Or_Sapling::NAME,
                            Marpa::Internal::Or_Sapling::RULE,
                            Marpa::Internal::Or_Sapling::POSITION,
                            Marpa::Internal::Or_Sapling::ITEM,
                            ]
                            = (
                            $predecessor_name, $sapling_rule,
                            $sapling_position, $predecessor,
                            );

                        push @or_saplings, $sapling;

                    }    # $predecessor_name ~~ %or_node_by_name

                }    # if sapling_position > 0

                my $cause_name;

                if ( defined $cause ) {

                    my $cause_symbol_id =
                        $symbol->[Marpa::Internal::Symbol::ID];

                    $cause_name =
                          $cause->[Marpa::Internal::Earley_Item::NAME] . 'L'
                        . $cause_symbol_id;

                    # We check that the cause has not already been
                    # processed so that cycles don't put us into a loop
                    if ( not $cause_name ~~ %or_node_by_name ) {

                        $or_node_by_name{$cause_name} = [];

                        my $sapling = [];
                        @{$sapling}[
                            Marpa::Internal::Or_Sapling::NAME,
                            Marpa::Internal::Or_Sapling::CHILD_LHS_SYMBOL,
                            Marpa::Internal::Or_Sapling::ITEM,
                            ]
                            = ( $cause_name, $symbol, $cause, );

                        push @or_saplings, $sapling;

                    }    # $cause_name ~~ %or_node_by_name

                }    # if cause

                my $and_node = [];
                $#{$and_node} = Marpa::Internal::And_Node::LAST_FIELD;

                # At this point names stand in for the or-node ids,
                # which will eventually replace them in these fields
                $and_node->[Marpa::Internal::And_Node::PREDECESSOR_ID] =
                    $predecessor_name;
                $and_node->[Marpa::Internal::And_Node::CAUSE_ID] =
                    $cause_name;

                $and_node->[Marpa::Internal::And_Node::TOKEN] = $token;
                $and_node->[Marpa::Internal::And_Node::VALUE_REF] =
                    $value_ref;
                $and_node->[Marpa::Internal::And_Node::RULE_ID] = $rule_id;

                # Right now tree processing is only done on
                # closure and-nodes.
                if ( $sapling_position
                    == $#{ $sapling_rule->[Marpa::Internal::Rule::RHS] } )
                {
                    $and_node->[Marpa::Internal::And_Node::TREE_OPS] =
                        $tree_rules[$rule_id];
                } ## end if ( $sapling_position == $#{ $sapling_rule->[...]})
                $and_node->[Marpa::Internal::And_Node::VALUE_OPS] =
                    $value_processing;
                given ($parse_order) {
                    when ('numeric') {
                        $and_node
                            ->[Marpa::Internal::And_Node::RANKING_CLOSURE] =
                            $ranking_closures_by_rule->[$rule_id];
                        $and_node
                            ->[Marpa::Internal::And_Node::FIXED_RANKING_DATA]
                            = 0
                    } ## end when ('numeric')
                } ## end given

                $and_node->[Marpa::Internal::And_Node::POSITION] =
                    $sapling_position;
                $and_node->[Marpa::Internal::And_Node::START_EARLEME] =
                    $start_earleme;
                $and_node->[Marpa::Internal::And_Node::CAUSE_EARLEME] =
                      $predecessor
                    ? $item->[Marpa::Internal::Earley_Item::SET]
                    : $start_earleme;
                $and_node->[Marpa::Internal::And_Node::END_EARLEME] =
                    $end_earleme;
                my $id = $and_node->[Marpa::Internal::And_Node::ID] =
                    @{$and_nodes};
                Marpa::exception("Too many and-nodes for evaluator: $id")
                    if $id & ~(Marpa::Internal::N_FORMAT_MAX);
                push @{$and_nodes}, $and_node;

                push @child_and_nodes, $and_node;

            }    # for my $or_bud

        }    # for my $and_sapling

        my $or_node = [];
        $#{$or_node} = Marpa::Internal::Or_Node::LAST_FIELD;
        my $or_node_id = $or_node->[Marpa::Internal::Or_Node::ID] =
            @{$or_nodes};
        my $or_node_tag = $or_node->[Marpa::Internal::Or_Node::TAG] =
            $sapling_name . "o$or_node_id";
        $or_node->[Marpa::Internal::Or_Node::CHILD_IDS] =
            [ map { $_->[Marpa::Internal::And_Node::ID] } @child_and_nodes ];
        for my $and_node_choice ( 0 .. $#child_and_nodes ) {
            my $and_node    = $child_and_nodes[$and_node_choice];
            my $and_node_id = $and_node->[Marpa::Internal::And_Node::ID];
            $and_node->[Marpa::Internal::And_Node::TAG] =
                $or_node_tag . "a$and_node_id";
            $and_node->[Marpa::Internal::And_Node::PARENT_ID] = $or_node_id;
            $and_node->[Marpa::Internal::And_Node::PARENT_CHOICE] =
                $and_node_choice;
        } ## end for my $and_node_choice ( 0 .. $#child_and_nodes )
        $or_node->[Marpa::Internal::Or_Node::START_EARLEME] = $start_earleme;
        $or_node->[Marpa::Internal::Or_Node::END_EARLEME]   = $end_earleme;
        $or_node->[Marpa::Internal::Or_Node::PARENT_IDS]    = [];
        push @{$or_nodes}, $or_node;
        $or_node_by_name{$sapling_name} = $or_node;

    }    # OR_SAPLING

    # resolve links in the bocage
    for my $and_node ( @{$and_nodes} ) {
        my $and_node_id = $and_node->[Marpa::Internal::And_Node::ID];

        FIELD:
        for my $field (
            Marpa::Internal::And_Node::PREDECESSOR_ID,
            Marpa::Internal::And_Node::CAUSE_ID,
            )
        {
            my $name = $and_node->[$field];
            next FIELD if not defined $name;
            my $child_or_node = $or_node_by_name{$name};
            $and_node->[$field] =
                $child_or_node->[Marpa::Internal::Or_Node::ID];
            my $parent_ids =
                $child_or_node->[Marpa::Internal::Or_Node::PARENT_IDS];
            push @{$parent_ids}, $and_node_id;
        } ## end for my $field ( Marpa::Internal::And_Node::PREDECESSOR_ID...)

    } ## end for my $and_node ( @{$and_nodes} )

    my $first_ambiguous_or_node = List::Util::first {
        @{ $_->[Marpa::Internal::Or_Node::CHILD_IDS] } > 1;
    }
    @{$or_nodes};

    ### assert: Marpa'Evaluator'audit($self) or 1

    if ( defined $first_ambiguous_or_node ) {
        delete_duplicate_nodes($self);
    }

=begin Implementation:

We don't allow zero-length or-nodes to have more than one and-node parent.
We do that to prevent two and-nodes in a parse from overlapping.  For
non-zero-length or-nodes preventing overlap is easy -- if no and-nodes
have overlapping spans as determined by start and end earleme, they
won't have overlapping non-zero-length or-nodes.  But with zero-length
or-nodes, an or-node can be a trailing or-node and a lead or-node at
the same earleme location.  That means that two adjacent and-nodes can
share the same child or-node -- one which has it as a trailing or-node,
the other which has it as a leading or-node.

So in the below, we make sure every zero-length or-node has only one
parent.

I can assume no cycles.  Reason: Marpa does not allow zero-length rules,
and cycles in the bocage can only occur when rules derive rules.  Breaking up
rules into and-nodes with at most two children will not create cycles.
It is impossible by breaking a rule up into pieces to make it cycle.
Any predecessor chain of null symbols must lead back to the beginning
of the rule, where it will end.

=end Implementation:

=cut

    my @zero_width_work_list = grep {
        not $_->[Marpa::Internal::Or_Node::DELETED]
            and $_->[Marpa::Internal::Or_Node::START_EARLEME]
            == $_->[Marpa::Internal::Or_Node::END_EARLEME]
    } @{$or_nodes};

    OR_NODE: while ( my $or_node = pop @zero_width_work_list ) {

        my $or_node_id = $or_node->[Marpa::Internal::Or_Node::ID];

        my $parent_and_node_ids =
            $or_node->[Marpa::Internal::Or_Node::PARENT_IDS];
        next OR_NODE if scalar @{$parent_and_node_ids} <= 1;

        # Remove the other parents from the original (uncloned)
        # or-node.
        $or_node->[Marpa::Internal::Or_Node::PARENT_IDS] =
            [ $parent_and_node_ids->[0] ];

        my @child_and_nodes =
            map { $and_nodes->[$_] }
            @{ $or_node->[Marpa::Internal::Or_Node::CHILD_IDS] };

        push @zero_width_work_list, map { $or_nodes->[$_] }
            grep {defined}
            map {
            @{$_}[
                Marpa::Internal::And_Node::PREDECESSOR_ID,
                Marpa::Internal::And_Node::CAUSE_ID
                ]
            } @child_and_nodes;

        # This or-node needs to be cloned, so that it will be
        # unique to its parent and-node
        for my $parent_and_node_id (
            @{$parent_and_node_ids}[ 1 .. $#{$parent_and_node_ids} ] )
        {

            my $cloned_or_node = [];
            $#{$cloned_or_node} = Marpa::Internal::Or_Node::LAST_FIELD;
            my $cloned_or_node_id =
                $cloned_or_node->[Marpa::Internal::Or_Node::ID] =
                @{$or_nodes};
            for my $field (
                Marpa::Internal::Or_Node::START_EARLEME,
                Marpa::Internal::Or_Node::END_EARLEME,
                Marpa::Internal::Or_Node::TAG
                )
            {
                $cloned_or_node->[$field] = $or_node->[$field];
            } ## end for my $field ( Marpa::Internal::Or_Node::START_EARLEME...)
            $cloned_or_node->[Marpa::Internal::Or_Node::TAG]
                =~ s/ (o\d+) \z /o$cloned_or_node_id/xms;
            push @{$or_nodes}, $cloned_or_node;
            $cloned_or_node->[Marpa::Internal::Or_Node::PARENT_IDS] =
                [$parent_and_node_id];
            $cloned_or_node->[Marpa::Internal::Or_Node::CHILD_IDS] = [];

            for my $child_and_node (@child_and_nodes) {
                clone_and_node( $self, $child_and_node, $cloned_or_node_id );
            }

            my $parent_and_node = $and_nodes->[$parent_and_node_id];
            FIELD:
            for my $field (
                Marpa::Internal::And_Node::CAUSE_ID,
                Marpa::Internal::And_Node::PREDECESSOR_ID
                )
            {
                my $sibling_id = $parent_and_node->[$field];
                next FIELD if not defined $sibling_id;
                next FIELD if $sibling_id != $or_node_id;
                $parent_and_node->[$field] = $cloned_or_node_id;
            } ## end for my $field ( Marpa::Internal::And_Node::CAUSE_ID, ...)

        } ## end for my $parent_and_node_id ( @{$parent_and_node_ids}[...])

    } ## end while ( my $or_node = pop @zero_width_work_list )

    ### assert: Marpa'Evaluator'audit($self) or 1

    if (    $grammar->[Marpa::Internal::Grammar::HAS_CYCLE]
        and $self->[Marpa::Internal::Evaluator::CYCLE_REWRITE] )
    {
        rewrite_cycles( $self, \@cycle_rule_ids );
    }

    ### assert: Marpa'Evaluator'audit($self) or 1

    return $self;

}    # sub new

sub Marpa::dump_sort_key {
    my ($sort_key) = @_;
    my @element_dumps = ();
    for my $sort_element (
        map { [ unpack 'N*', $_ ] }
        sort map { pack 'N*', @{$_} } @{$sort_key}
        )
    {
        push @element_dumps, join q{ }, map {
                  ( $_ & Marpa::Internal::N_FORMAT_HIGH_BIT )
                ? ( q{~} . ~$_ )
                : "$_"
        } @{$sort_element};
    } ## end for my $sort_element ( map { [ unpack 'N*', $_ ] } sort...)
    return join q{ }, map { '<' . $_ . '>' } @element_dumps;
} ## end sub Marpa::dump_sort_key

sub Marpa::Evaluator::show_sort_keys {
    my ($evaler) = @_;
    my $parse_order = $evaler->[Marpa::Internal::Evaluator::PARSE_ORDER];
    Marpa::exception(
        "show_sort_keys called when parse order is not original\n",
        "parse order is $parse_order" )
        if $parse_order ne 'original';

    my $or_iterations = $evaler->[Marpa::Internal::Evaluator::OR_ITERATIONS];
    my $top_or_iteration = $or_iterations->[0];
    Marpa::exception('show_sort_keys called on exhausted parse')
        if not $top_or_iteration;

    my $text = q{};
    for my $and_choice ( reverse @{$top_or_iteration} ) {
        my $sort_data =
            $and_choice->[Marpa::Internal::And_Choice::RANKING_DATA];
        my $sort_key =
            $sort_data->[Marpa::Internal::Original_Sort_Data::SORT_KEY];
        $text .= Marpa::dump_sort_key($sort_key) . "\n";
    } ## end for my $and_choice ( reverse @{$top_or_iteration} )
    return $text;
} ## end sub Marpa::Evaluator::show_sort_keys

sub Marpa::Evaluator::show_and_node {
    my ( $evaler, $and_node, $verbose ) = @_;
    $verbose //= 0;

    return q{} if $and_node->[Marpa::Internal::And_Node::DELETED];

    my $return_value = q{};

    my $grammar = $evaler->[Marpa::Internal::Evaluator::GRAMMAR];
    my $rules   = $grammar->[Marpa::Internal::Grammar::RULES];

    my $name = $and_node->[Marpa::Internal::And_Node::TAG];
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

    my $or_nodes = $evaler->[Marpa::Internal::Evaluator::OR_NODES];

    my $predecessor;
    if ($predecessor_id) {
        $predecessor = $or_nodes->[$predecessor_id];
        push @rhs, $predecessor->[Marpa::Internal::Or_Node::TAG];
    }    # predecessor

    my $cause;
    if ($cause_id) {
        $cause = $or_nodes->[$cause_id];
        push @rhs, $cause->[Marpa::Internal::Or_Node::TAG];
    }    # cause

    if ( defined $value_ref ) {
        my $value_as_string =
            Data::Dumper->new( [ ${$value_ref} ] )->Terse(1)->Dump;
        chomp $value_as_string;
        push @rhs, $value_as_string;
    }    # value

    $return_value .= "$name -> " . join( q{ }, @rhs ) . "\n";

    SHOW_RULE: {
        if ( $is_virtual_rule and $verbose >= 2 ) {
            $return_value
                .= '    rule '
                . $rule->[Marpa::Internal::Rule::ID] . ': '
                . Marpa::show_dotted_rule( $rule, $position )
                . "\n    "
                . Marpa::brief_virtual_rule( $rule, $position ) . "\n";
            last SHOW_RULE;
        } ## end if ( $is_virtual_rule and $verbose >= 2 )

        last SHOW_RULE if not $verbose;
        $return_value
            .= '    rule '
            . $rule->[Marpa::Internal::Rule::ID] . ': '
            . Marpa::brief_virtual_rule( $rule, $position ) . "\n";

    } ## end SHOW_RULE:

    if ( $verbose >= 2 ) {
        my @comment = ();
        if ( $and_node->[Marpa::Internal::And_Node::TREE_OPS] ) {
            push @comment, 'tree_ops';
        }
        if ( $and_node->[Marpa::Internal::And_Node::VALUE_OPS] ) {
            push @comment, 'value_ops';
        }
        if ( scalar @comment ) {
            $return_value .= q{    } . ( join q{, }, @comment ) . "\n";
        }
    } ## end if ( $verbose >= 2 )

    return $return_value;

} ## end sub Marpa::Evaluator::show_and_node

sub Marpa::Evaluator::show_or_node {
    my ( $evaler, $or_node, $verbose, ) = @_;
    $verbose //= 0;

    return q{} if $or_node->[Marpa::Internal::Or_Node::DELETED];

    my $and_nodes = $evaler->[Marpa::Internal::Evaluator::AND_NODES];

    my $text = q{};

    my $or_node_tag  = $or_node->[Marpa::Internal::Or_Node::TAG];
    my $and_node_ids = $or_node->[Marpa::Internal::Or_Node::CHILD_IDS];

    for my $index ( 0 .. $#{$and_node_ids} ) {
        my $and_node_id = $and_node_ids->[$index];
        my $and_node    = $and_nodes->[$and_node_id];

        my $and_node_tag = $or_node_tag . "a$and_node_id";
        if ( $verbose >= 2 ) {
            $text .= "$or_node_tag -> $and_node_tag\n";
        }

        $text .= $evaler->show_and_node( $and_node, $verbose );

    } ## end for my $index ( 0 .. $#{$and_node_ids} )

    return $text;

} ## end sub Marpa::Evaluator::show_or_node

sub Marpa::Evaluator::show_bocage {
    my ( $evaler, $verbose, ) = @_;
    $verbose //= 0;

    my $parse_count = $evaler->[Marpa::Internal::Evaluator::PARSE_COUNT];
    my $or_nodes    = $evaler->[Marpa::Internal::Evaluator::OR_NODES];

    my $text = 'parse count: ' . $parse_count . "\n";

    for my $or_node ( @{$or_nodes} ) {

        $text
            .= Marpa::Evaluator::show_or_node( $evaler, $or_node, $verbose );

    } ## end for my $or_node ( @{$or_nodes} )

    return $text;
} ## end sub Marpa::Evaluator::show_bocage

sub Marpa::Evaluator::show_ambiguity {
    my ( $evaler, $verbose, ) = @_;
    my $and_nodes = $evaler->[Marpa::Internal::Evaluator::AND_NODES];
    my $or_nodes  = $evaler->[Marpa::Internal::Evaluator::OR_NODES];
    my $grammar   = $evaler->[Marpa::Internal::Evaluator::GRAMMAR];
    my $QDFA      = $grammar->[Marpa::Internal::Grammar::QDFA];
    $verbose //= 0;
    my $text = q{};

    OR_NODE:
    for my $or_node ( @{$or_nodes} ) {
        my $child_ids   = $or_node->[Marpa::Internal::Or_Node::CHILD_IDS];
        my $child_count = scalar @{$child_ids};
        next OR_NODE if $child_count <= 1;
        my $or_tag = $or_node->[Marpa::Internal::Or_Node::TAG];
        $text .= "$or_tag is Ambiguous: $child_count children\n";
        for my $child_ix ( 0 .. $#{$child_ids} ) {
            my $child_and_node_id = $child_ids->[$child_ix];
            my $and_node          = $and_nodes->[$child_and_node_id];
            my $and_tag = $and_node->[Marpa::Internal::And_Node::TAG];
            $text .= "  choice #$child_ix: $and_tag ::=";
            my $detail_text = q{};
            if (defined(
                    my $predecessor_id =
                        $and_node->[Marpa::Internal::And_Node::PREDECESSOR_ID]
                )
                )
            {
                my $or_grandchild = $or_nodes->[$predecessor_id];
                my $grandchild_tag =
                    $or_grandchild->[Marpa::Internal::Or_Node::TAG];
                my ($state) = ( $grandchild_tag =~ /\A S (\d+) [@]/xms );
                $text .= " $grandchild_tag";
                $detail_text
                    .= Marpa::show_QDFA_state( $QDFA->[ $state + 0 ], 0 );
            } ## end if ( defined( my $predecessor_id = $and_node->[...]))
            if (defined(
                    my $cause_id =
                        $and_node->[Marpa::Internal::And_Node::CAUSE_ID]
                )
                )
            {
                my $or_grandchild = $or_nodes->[$cause_id];
                my $grandchild_tag =
                    $or_grandchild->[Marpa::Internal::Or_Node::TAG];
                my ($state) = ( $grandchild_tag =~ /\A S (\d+) [@]/xms );
                $text .= " $grandchild_tag";
                $detail_text
                    .= Marpa::show_QDFA_state( $QDFA->[ $state + 0 ], 0 );
            } ## end if ( defined( my $cause_id = $and_node->[...]))
            if (defined(
                    my $value_ref =
                        $and_node->[Marpa::Internal::And_Node::VALUE_REF]
                )
                )
            {
                $text .= ' Token';
                $detail_text
                    .= Data::Dumper->new($value_ref)->Terse(1)->Dump();
            } ## end if ( defined( my $value_ref = $and_node->[...]))
            $detail_text =~ s/^/    /gxms;
            $text .= "\n$detail_text";
        } ## end for my $child_ix ( 0 .. $#{$child_ids} )
    } ## end for my $or_node ( @{$or_nodes} )

    return $text;
} ## end sub Marpa::Evaluator::show_ambiguity

use constant EVALUATOR_OPTIONS => [
    qw{
        cycle_nodes
        cycle_rewrite
        cycle_scale
        experimental
        max_parses
        parse_order
        trace_actions
        trace_evaluation
        trace_file_handle
        trace_tasks
        trace_values
        }
];

sub Marpa::Evaluator::set {
    my ( $evaler, @arg_hashes ) = @_;
    local $Marpa::Internal::TRACE_FH =
        $evaler->[Marpa::Internal::Evaluator::TRACE_FILE_HANDLE];

    for my $args (@arg_hashes) {

        my $ref_type = ref $args;
        if ( not $ref_type or $ref_type ne 'HASH' ) {
            Marpa::exception(
                'Marpa expects args as ref to HASH, got ',
                ( "ref to $ref_type" || 'non-reference' ),
                ' instead'
            );
        } ## end if ( not $ref_type or $ref_type ne 'HASH' )
        if (my @bad_options =
            grep { not $_ ~~ Marpa::Internal::Evaluator::EVALUATOR_OPTIONS }
            keys %{$args}
            )
        {
            Marpa::exception( 'Unknown option(s) for Marpa Evaluator: ',
                join q{ }, @bad_options );
        } ## end if ( my @bad_options = grep { not $_ ~~ ...})

        if ( defined( my $value = $args->{'trace_file_handle'} ) ) {
            $evaler->[Marpa::Internal::Evaluator::TRACE_FILE_HANDLE] = $value;
        }

        if ( defined( my $value = $args->{'trace_actions'} ) ) {
            $evaler->[Marpa::Internal::Evaluator::TRACE_ACTIONS] = $value;
            if ($value) {
                say {$Marpa::Internal::TRACE_FH}
                    'Setting trace_actions option'
                    or Marpa::exception("Cannot print: $ERRNO");
                if ($evaler->[Marpa::Internal::Evaluator::SEMANTICS_SETTLED] )
                {
                    say {$Marpa::Internal::TRACE_FH}
                        'Warning: setting trace_actions option after semantics were finalized'
                        or Marpa::exception("Cannot print: $ERRNO");
                } ## end if ( $evaler->[...])
                $evaler->[Marpa::Internal::Evaluator::TRACING] = 1;
            } ## end if ($value)
        } ## end if ( defined( my $value = $args->{'trace_actions'} ))

        # TO HERE

        if ( defined( my $value = $args->{'trace_values'} ) ) {
            Marpa::exception('trace_values must be set to a number >= 0')
                if not $value =~ /\A\d+\z/xms;
            $evaler->[Marpa::Internal::Evaluator::TRACE_VALUES] = $value + 0;
            if ($value) {
                say {$Marpa::Internal::TRACE_FH}
                    "Setting trace_values option to $value"
                    or Marpa::exception("Cannot print: $ERRNO");
                $evaler->[Marpa::Internal::Evaluator::TRACING] = 1;
            } ## end if ($value)
        } ## end if ( defined( my $value = $args->{'trace_values'} ) )

        if ( defined( my $value = $args->{'trace_tasks'} ) ) {
            Marpa::exception('trace_tasks must be set to a number >= 0')
                if $value !~ /\A\d+\z/xms;
            $evaler->[Marpa::Internal::Evaluator::TRACE_TASKS] = $value + 0;
            if ($value) {
                say {$Marpa::Internal::TRACE_FH}
                    "Setting trace_tasks option to $value"
                    or Marpa::exception("Cannot print: $ERRNO");
                $evaler->[Marpa::Internal::Evaluator::TRACING] = 1;
            } ## end if ($value)
        } ## end if ( defined( my $value = $args->{'trace_tasks'} ) )

        if ( defined( my $value = $args->{'trace_evaluation'} ) ) {
            Marpa::exception('trace_evaluation must be set to a number >= 0')
                if $value !~ /\A\d+\z/xms;
            $evaler->[Marpa::Internal::Evaluator::TRACE_EVALUATION] =
                $value + 0;
            if ($value) {
                say {$Marpa::Internal::TRACE_FH}
                    "Setting trace_evaluation option to $value"
                    or Marpa::exception("Cannot print: $ERRNO");
                $evaler->[Marpa::Internal::Evaluator::TRACING] = 1;
            } ## end if ($value)
        } ## end if ( defined( my $value = $args->{'trace_evaluation'...}))

        if ( defined( my $value = $args->{'cycle_scale'} ) ) {
            Marpa::exception(
                'cycle_scale option only allowed in experimental mode')
                if not $evaler->[Marpa::Internal::Evaluator::EXPERIMENTAL];
            Marpa::exception(q{cycle_scale must be >1})
                if $value <= 1;
            no integer;
            $evaler->[Marpa::Internal::Evaluator::CYCLE_SCALE] =
                POSIX::ceil($value);
            use integer;
        } ## end if ( defined( my $value = $args->{'cycle_scale'} ) )

        if ( defined( my $value = $args->{'cycle_nodes'} ) ) {
            Marpa::exception(
                'cycle_nodes option only allowed in experimental mode')
                if $evaler->[Marpa::Internal::Evaluator::EXPERIMENTAL] <= 0;
            Marpa::exception(q{cycle_nodes must be >0})
                if $value <= 0;
            $evaler->[Marpa::Internal::Evaluator::CYCLE_NODES] = $value;
        } ## end if ( defined( my $value = $args->{'cycle_nodes'} ) )

        if ( defined( my $value = $args->{'cycle_rewrite'} ) ) {
            $evaler->[Marpa::Internal::Evaluator::CYCLE_REWRITE] = $value;
        }

        if ( defined( my $value = $args->{'max_parses'} ) ) {
            $evaler->[Marpa::Internal::Evaluator::MAX_PARSES] = $value;
        }

        if ( defined( my $value = $args->{'experimental'} ) ) {
            given ($value) {
                when (undef) { $value = 0 }
                when ('no warning') {
                    $value = 1
                }
                default {
                    say {
                        $Marpa::Internal::TRACE_FH
                    }
                    'Experimental (in other words, buggy) features enabled'
                        or Marpa::exception("Cannot print: $ERRNO");
                    $value = 1;
                } ## end default
            } ## end given
            $evaler->[Marpa::Internal::Evaluator::EXPERIMENTAL] = $value;
        } ## end if ( defined( my $value = $args->{'experimental'} ) )

        if ( defined( my $value = $args->{'parse_order'} ) ) {
            Marpa::exception(q{parse_order must be 'original' or 'none'})
                if not $value ~~ [qw(original numeric none)];
            $evaler->[Marpa::Internal::Evaluator::PARSE_ORDER] = $value;
        }

    } ## end for my $args (@arg_hashes)

    return 1;
} ## end sub Marpa::Evaluator::set

use Marpa::Offset qw(
    { tasks for use in Marpa::Evaluator::value }
    :package=Marpa::Internal::Task
    RESET_AND_NODE
    SETUP_AND_NODE
    NEXT_AND_TREE
    ITERATE_AND_TREE
    ITERATE_AND_TREE_2
    ITERATE_AND_TREE_3
    RESET_AND_TREE
    RESET_OR_NODE
    RESET_OR_TREE
    ITERATE_OR_NODE
    ITERATE_OR_TREE
    FREEZE_TREE
    THAW_TREE
    EVALUATE
);

sub evaluate {
    my ( $grammar, $action_object, $stack, $trace_values ) = @_;

    $trace_values //= 0;
    my $rules = $grammar->[Marpa::Internal::Grammar::RULES];

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
                my $token = $and_node->[Marpa::Internal::And_Node::TOKEN];
                print {$Marpa::Internal::TRACE_FH}
                    'Pushed value from ',
                    $and_node->[Marpa::Internal::And_Node::TAG], ': ',
                    (
                    $token
                    ? ( $token->[Marpa::Internal::Symbol::NAME] . q{ = } )
                    : q{}
                    ),
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

                when (Marpa::Internal::Evaluator_Op::ARGC) {

                    my $argc = $ops->[ $op_ix++ ];

                    if ($trace_values) {
                        my $rule_id =
                            $and_node->[Marpa::Internal::And_Node::RULE_ID];
                        my $rule = $rules->[$rule_id];
                        say {$Marpa::Internal::TRACE_FH}
                            'Popping ',
                            $argc,
                            ' values to evaluate ',
                            $and_node->[Marpa::Internal::And_Node::TAG],
                            ', rule: ', Marpa::brief_rule($rule)
                            or
                            Marpa::exception('Could not print to trace file');
                    } ## end if ($trace_values)

                    $current_data =
                        [ map { ${$_} }
                            ( splice @evaluation_stack, -$argc ) ];

                } ## end when (Marpa::Internal::Evaluator_Op::ARGC)

                when (Marpa::Internal::Evaluator_Op::VIRTUAL_HEAD) {
                    my $real_symbol_count = $ops->[ $op_ix++ ];

                    if ($trace_values) {
                        my $rule_id =
                            $and_node->[Marpa::Internal::And_Node::RULE_ID];
                        my $rule = $rules->[$rule_id];
                        say {$Marpa::Internal::TRACE_FH}
                            'Head of Virtual Rule: ',
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

                } ## end when (Marpa::Internal::Evaluator_Op::VIRTUAL_HEAD)

                when (Marpa::Internal::Evaluator_Op::VIRTUAL_HEAD_NO_SEP) {
                    my $real_symbol_count = $ops->[ $op_ix++ ];

                    if ($trace_values) {
                        my $rule_id =
                            $and_node->[Marpa::Internal::And_Node::RULE_ID];
                        my $rule = $rules->[$rule_id];
                        say {$Marpa::Internal::TRACE_FH}
                            'Head of Virtual Rule (discards separation): ',
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

                } ## end when (...)

                when (Marpa::Internal::Evaluator_Op::VIRTUAL_KERNEL) {
                    my $real_symbol_count = $ops->[ $op_ix++ ];
                    $virtual_rule_stack[-1] += $real_symbol_count;

                    if ($trace_values) {
                        my $rule_id =
                            $and_node->[Marpa::Internal::And_Node::RULE_ID];
                        my $rule = $rules->[$rule_id];
                        say {$Marpa::Internal::TRACE_FH}
                            'Virtual Rule: ',
                            $and_node->[Marpa::Internal::And_Node::TAG],
                            ', rule: ', Marpa::brief_rule($rule),
                            "\nAdding $real_symbol_count, now ",
                            ( scalar @virtual_rule_stack ),
                            ' rules; ', $virtual_rule_stack[-1], ' symbols'
                            or
                            Marpa::exception('Could not print to trace file');
                    } ## end if ($trace_values)

                } ## end when (Marpa::Internal::Evaluator_Op::VIRTUAL_KERNEL)

                when (Marpa::Internal::Evaluator_Op::VIRTUAL_TAIL) {
                    my $real_symbol_count = $ops->[ $op_ix++ ];

                    if ($trace_values) {
                        my $rule_id =
                            $and_node->[Marpa::Internal::And_Node::RULE_ID];
                        my $rule = $rules->[$rule_id];
                        say {$Marpa::Internal::TRACE_FH}
                            'New Virtual Rule: ',
                            $and_node->[Marpa::Internal::And_Node::TAG],
                            ', rule: ', Marpa::brief_rule($rule),
                            "\nSymbol count is $real_symbol_count, now ",
                            ( scalar @virtual_rule_stack + 1 ), ' rules',
                            or
                            Marpa::exception('Could not print to trace file');
                    } ## end if ($trace_values)

                    push @virtual_rule_stack, $real_symbol_count;

                } ## end when (Marpa::Internal::Evaluator_Op::VIRTUAL_TAIL)

                when (Marpa::Internal::Evaluator_Op::CONSTANT_RESULT) {
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
                } ## end when (Marpa::Internal::Evaluator_Op::CONSTANT_RESULT)

                when (Marpa::Internal::Evaluator_Op::CALL) {
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
                                long_where  => 'computing value for rule: '
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

                } ## end when (Marpa::Internal::Evaluator_Op::CALL)

                default {
                    Marpa::Exception("Unknown evaluator Op: $_");
                }

            } ## end given
        } ## end while ( $op_ix < scalar @{$ops} )

    }    # TREE_NODE

    return pop @evaluation_stack;
} ## end sub evaluate

# This will replace the old value method
sub Marpa::Evaluator::value {
    my ($evaler) = @_;

    Marpa::exception('No parse supplied') if not defined $evaler;
    my $evaler_class = ref $evaler;
    my $right_class  = 'Marpa::Evaluator';
    Marpa::exception(
        "Don't parse argument is class: $evaler_class; should be: $right_class"
    ) if $evaler_class ne $right_class;

    local $Marpa::Internal::TRACE_FH =
        $evaler->[Marpa::Internal::Evaluator::TRACE_FILE_HANDLE];

    my $grammar = $evaler->[Marpa::Internal::Evaluator::GRAMMAR];
    my $rules   = $grammar->[Marpa::Internal::Grammar::RULES];

    my $parse_order = $evaler->[Marpa::Internal::Evaluator::PARSE_ORDER];

    my $parse_count = $evaler->[Marpa::Internal::Evaluator::PARSE_COUNT]++;

    my $evaluator_rules =
        $evaler->[Marpa::Internal::Evaluator::RULE_VALUE_OPS];
    my $and_nodes = $evaler->[Marpa::Internal::Evaluator::AND_NODES];
    my $or_nodes  = $evaler->[Marpa::Internal::Evaluator::OR_NODES];
    my $ranking_closures_by_symbol =
        $evaler->[Marpa::Internal::Evaluator::RANKING_CLOSURES_BY_SYMBOL];

    # If the arrays of iteration data
    # for the and-nodes and or-nodes are undefined,
    # this is the first pass through, and there is some
    # initialization that needs to be done.
    my $and_iterations =
        $evaler->[Marpa::Internal::Evaluator::AND_ITERATIONS];
    my $or_iterations = $evaler->[Marpa::Internal::Evaluator::OR_ITERATIONS];
    SET_UP_ITERATIONS: {
        last SET_UP_ITERATIONS if defined $and_iterations;

        $#{$and_iterations} = $#{$and_nodes};
        $#{$or_iterations}  = $#{$or_nodes};
        $evaler->[Marpa::Internal::Evaluator::AND_ITERATIONS] =
            $and_iterations;
        $evaler->[Marpa::Internal::Evaluator::OR_ITERATIONS] = $or_iterations;

        if ( $parse_order eq 'numeric' ) {
            AND_NODE: for my $and_node ( @{$and_nodes} ) {
                next AND_NODE
                    if not my $token =
                        $and_node->[Marpa::Internal::And_Node::TOKEN];

                next AND_NODE
                    if not my $ranking_closure = $ranking_closures_by_symbol
                        ->[ $token->[Marpa::Internal::Symbol::ID] ];

                my $rank;
                my @warnings;
                my $eval_ok;
                DO_EVAL: {
                    local $Marpa::Internal::CONTEXT =
                        [ 'setup and-node', $and_node ];
                    local $SIG{__WARN__} =
                        sub { push @warnings, [ $_[0], ( caller 0 ) ]; };
                    $eval_ok = eval { $rank = $ranking_closure->(); 1; };
                } ## end DO_EVAL:

                if ( not $eval_ok or @warnings ) {
                    my $fatal_error = $EVAL_ERROR;
                    my $symbol_name = $token->[Marpa::Internal::Symbol::NAME];
                    Marpa::Internal::code_problems(
                        {   fatal_error => $fatal_error,
                            grammar     => $grammar,
                            eval_ok     => $eval_ok,
                            warnings    => \@warnings,
                            where       => "ranking symbol $symbol_name",
                        }
                    );
                } ## end if ( not $eval_ok or @warnings )
                $and_node->[Marpa::Internal::And_Node::FIXED_RANKING_DATA] =
                    $rank;

            } ## end for my $and_node ( @{$and_nodes} )
            last SET_UP_ITERATIONS;
        } ## end if ( $parse_order eq 'numeric' )

        last SET_UP_ITERATIONS if $parse_order ne 'original';

        # This could be done in the ::new constructor, but intuitively
        # I feel it does not belong -- that someday it would get
        # factored out to here.
        AND_NODE: for my $and_node ( @{$and_nodes} ) {

            # the absence of evaluator data means this is not a closure and-node
            # and does not count in the sort order
            next AND_NODE
                if not $and_node->[Marpa::Internal::And_Node::VALUE_OPS];

            my $rule_id  = $and_node->[Marpa::Internal::And_Node::RULE_ID];
            my $rule     = $rules->[$rule_id];
            my $greed    = $rule->[Marpa::Internal::Rule::GREED];
            my $priority = $rule->[Marpa::Internal::Rule::PRIORITY];

            next AND_NODE if not $greed and not $priority;

            my $and_node_start_earleme =
                $and_node->[Marpa::Internal::And_Node::START_EARLEME];
            my $and_node_end_earleme =
                $and_node->[Marpa::Internal::And_Node::END_EARLEME];

            # compute this and-nodes sort key element
            # insert it into the predecessor sort key elements
            my $location = $and_node_start_earleme;
            my $length;
            given ($greed) {
                when (undef) { $length = 0 }
                when (0)     { $length = 0 }
                when ( $_ > 0 ) {
                    $length =
                        ~( ( $and_node_end_earleme - $and_node_start_earleme )
                        & Marpa::Internal::N_FORMAT_MASK )
                }
                default {
                    $length =
                        ( $and_node_end_earleme - $and_node_start_earleme );
                }
            } ## end given
            $and_node->[Marpa::Internal::And_Node::FIXED_RANKING_DATA] = [
                $location,                                       0,
                ~( $priority & Marpa::Internal::N_FORMAT_MASK ), $length
            ];

        } ## end for my $and_node ( @{$and_nodes} )

    } ## end SET_UP_ITERATIONS:

    my $max_parses = $evaler->[Marpa::Internal::Evaluator::MAX_PARSES];
    if ( $max_parses > 0 && $parse_count >= $max_parses ) {
        Marpa::exception("Maximum parse count ($max_parses) exceeded");
    }

    my @tasks = (
        [Marpa::Internal::Task::EVALUATE],
        [   (   $parse_count
                ? Marpa::Internal::Task::ITERATE_OR_TREE
                : Marpa::Internal::Task::RESET_OR_TREE
            ),
            0,
            {}
        ]
    );

    my $trace_tasks = $evaler->[Marpa::Internal::Evaluator::TRACE_TASKS];

    while (1) {

        my $task_entry = pop @tasks;
        my $task       = shift @{$task_entry};

        given ($task) {
            when (Marpa::Internal::Task::RESET_OR_NODE) {
                my ($or_node_id) = @{$task_entry};
                my $or_node = $or_nodes->[$or_node_id];

                if ($trace_tasks) {
                    print {$Marpa::Internal::TRACE_FH}
                        "Task: RESET_OR_NODE #o$or_node_id; ",
                        ( scalar @tasks ), " tasks pending\n"
                        or Marpa::exception('print to trace handle failed');
                } ## end if ($trace_tasks)

                # Set up the and-choices from the children
                my @and_choices = ();
                AND_CHOICE:
                for my $child_and_node_id (
                    @{ $or_node->[Marpa::Internal::Or_Node::CHILD_IDS] } )
                {
                    my $and_iteration = $and_iterations->[$child_and_node_id];
                    next AND_CHOICE if not defined $and_iteration;
                    my $and_choice;
                    $#{$and_choice} = Marpa::Internal::And_Choice::LAST_FIELD;
                    $and_choice->[Marpa::Internal::And_Choice::ID] =
                        $child_and_node_id;
                    $and_choice->[Marpa::Internal::And_Choice::RANKING_DATA] =
                        $and_iteration
                        ->[Marpa::Internal::And_Iteration::RANKING_DATA];

                    push @and_choices, $and_choice;

                } ## end for my $child_and_node_id ( @{ $or_node->[...]})

                # If there are no and_choices, this or-node is
                # exhausted.
                # With no choices, there is no need to
                # sort the choices or to freeze any
                # of them.
                if ( not scalar @and_choices ) {
                    $or_iterations->[$or_node_id] = undef;
                    break;    # next TASK
                }

                # Sort and-choices
                my $or_iteration;
                given ($parse_order) {
                    when ('numeric') {
                        no integer;
                        $or_iteration = [
                            map      { $_->[1] }
                                sort { $a->[0] <=> $b->[0] }
                                map {
                                [   $_->[
                                        Marpa::Internal::And_Choice::RANKING_DATA
                                    ],
                                    $_
                                ]
                                } @and_choices
                        ];
                    } ## end when ('numeric')
                    when ('original') {

                        $or_iteration = [
                            map      { $_->[1] }
                                sort { $a->[0] cmp $b->[0] }
                                map {
                                [   ~(  join q{},
                                        sort map { pack 'N*', @{$_} } @{
                                            $_->[
                                                Marpa::Internal::And_Choice::RANKING_DATA
                                                ]->[
                                                Marpa::Internal::Original_Sort_Data::SORT_KEY
                                                ]
                                            }
                                    ),
                                    $_
                                ]
                                } @and_choices
                        ];
                    } ## end when ('original')
                    default {
                        $or_iteration = \@and_choices;
                    }
                } ## end given

                $or_iterations->[$or_node_id] = $or_iteration;

                push @tasks,
                    map { [ Marpa::Internal::Task::FREEZE_TREE, $_ ] }
                    @{$or_iteration}[ 0 .. $#{$or_iteration} - 1 ];

            } ## end when (Marpa::Internal::Task::RESET_OR_NODE)

            when (Marpa::Internal::Task::RESET_AND_NODE) {

                my ($and_node_id) = @{$task_entry};

                if ($trace_tasks) {
                    print {$Marpa::Internal::TRACE_FH}
                        "Task: RESET_AND_NODE #a$and_node_id; ",
                        ( scalar @tasks ), " tasks pending\n"
                        or Marpa::exception('print to trace handle failed');
                } ## end if ($trace_tasks)

                my $and_node = $and_nodes->[$and_node_id];

                my $and_node_iteration = $and_iterations->[$and_node_id] = [];

                $and_node_iteration
                    ->[Marpa::Internal::And_Iteration::CURRENT_CHILD_FIELD] =
                    defined $and_node->[Marpa::Internal::And_Node::CAUSE_ID]
                    ? Marpa::Internal::And_Node::CAUSE_ID
                    : defined
                    $and_node->[Marpa::Internal::And_Node::PREDECESSOR_ID]
                    ? Marpa::Internal::And_Node::PREDECESSOR_ID
                    : undef;

                push @tasks,
                    [ Marpa::Internal::Task::SETUP_AND_NODE, $and_node_id ];

            } ## end when (Marpa::Internal::Task::RESET_AND_NODE)

            # Set up task for followup on both initialization and iteration
            # This is safe to call on exhausted nodes
            when (Marpa::Internal::Task::SETUP_AND_NODE) {

                my ($and_node_id) = @{$task_entry};

                if ($trace_tasks) {
                    print {$Marpa::Internal::TRACE_FH}
                        "Task: SETUP_AND_NODE #a$and_node_id; ",
                        ( scalar @tasks ), " tasks pending\n"
                        or Marpa::exception('print to trace handle failed');
                } ## end if ($trace_tasks)

                my $and_node = $and_nodes->[$and_node_id];

                my $and_node_iteration = $and_iterations->[$and_node_id];
                break if not $and_node_iteration;

                my $cause;
                my $cause_id;
                my $cause_or_node_iteration;
                my $cause_and_node_choice;

                # assignment instead of comparison intentional
                if ( $cause_id =
                    $and_node->[Marpa::Internal::And_Node::CAUSE_ID] )
                {
                    $cause                   = $or_nodes->[$cause_id];
                    $cause_or_node_iteration = $or_iterations->[$cause_id];

                    # If there is a predecessor, but it is
                    # exhausted, this and-node is exhausted.
                    if ( not $cause_or_node_iteration ) {
                        $and_iterations->[$and_node_id] = undef;
                        break;
                    }

                    $cause_and_node_choice = $cause_or_node_iteration->[-1];
                } ## end if ( $cause_id = $and_node->[...])

                my $predecessor;
                my $predecessor_id;
                my $predecessor_or_node_iteration;
                my $predecessor_and_node_choice;

                # assignment instead of comparison intentional
                if ( $predecessor_id =
                    $and_node->[Marpa::Internal::And_Node::PREDECESSOR_ID] )
                {
                    $predecessor = $or_nodes->[$predecessor_id];
                    $predecessor_or_node_iteration =
                        $or_iterations->[$predecessor_id];

                    # If there is a predecessor, but it is
                    # exhausted, this and-node is exhausted.
                    if ( not $predecessor_or_node_iteration ) {
                        $and_iterations->[$and_node_id] = undef;
                        break;    # next TASK
                    }

                    $predecessor_and_node_choice =
                        $predecessor_or_node_iteration->[-1];

                } ## end if ( $predecessor_id = $and_node->[...])

                # The rest of the processing is for ranking parses
                break if $parse_order eq 'none';    # next TASK

                my $cause_ranking_data;
                my $cause_and_node_iteration;
                if ( defined $cause_and_node_choice ) {
                    my $cause_and_node_id = $cause_and_node_choice
                        ->[Marpa::Internal::And_Choice::ID];
                    $cause_and_node_iteration =
                        $and_iterations->[$cause_and_node_id];
                    $cause_ranking_data = $cause_and_node_iteration
                        ->[Marpa::Internal::And_Iteration::RANKING_DATA];
                } ## end if ( defined $cause_and_node_choice )

                my $predecessor_ranking_data;
                my $predecessor_and_node_iteration;
                if ( defined $predecessor_and_node_choice ) {
                    my $predecessor_and_node_id = $predecessor_and_node_choice
                        ->[Marpa::Internal::And_Choice::ID];
                    $predecessor_and_node_iteration =
                        $and_iterations->[$predecessor_and_node_id];
                    $predecessor_ranking_data =
                        $predecessor_and_node_iteration
                        ->[Marpa::Internal::And_Iteration::RANKING_DATA];
                } ## end if ( defined $predecessor_and_node_choice )

                my $token = $and_node->[Marpa::Internal::And_Node::TOKEN];

                if ( $parse_order eq 'numeric' ) {
                    my $ranking_closure = $and_node
                        ->[Marpa::Internal::And_Node::RANKING_CLOSURE];
                    if ( not $ranking_closure ) {

                        no integer;

                        # Initialize with the rank of this node
                        my $rank = $and_node
                            ->[Marpa::Internal::And_Node::FIXED_RANKING_DATA];

                        ### assert: defined $rank

                        #Then add cause and predecessor
                        # if they exist
                        if ($cause_and_node_choice) {
                            $rank
                                += $cause_and_node_iteration->[
                                Marpa::Internal::And_Iteration::RANKING_DATA
                                ];

                            ### and node: Marpa'Evaluator'show_and_node($evaler, $and_nodes->[$cause_and_node_choice->[Marpa'Internal'And_Choice'ID]], 99)

                            ### assert: defined $cause_and_node_iteration->[ Marpa'Internal'And_Iteration'RANKING_DATA ]

                        } ## end if ($cause_and_node_choice)
                        if ($predecessor_and_node_choice) {
                            $rank
                                += $predecessor_and_node_iteration->[
                                Marpa::Internal::And_Iteration::RANKING_DATA
                                ];
                        } ## end if ($predecessor_and_node_choice)

                        $and_node_iteration
                            ->[Marpa::Internal::And_Iteration::RANKING_DATA] =
                            $rank;

                        # With the rank processing finished, the
                        # SETUP_AND_NODE task is finished
                        break;    # next TASK

                    } ## end if ( not $ranking_closure )
                    my $rank;
                    my @warnings;
                    my $eval_ok;
                    DO_EVAL: {
                        local $Marpa::Internal::CONTEXT =
                            [ 'rank and-node', $and_node ];
                        local $SIG{__WARN__} =
                            sub { push @warnings, [ $_[0], ( caller 0 ) ]; };
                        $eval_ok = eval { $rank = $ranking_closure->(); 1; };
                    } ## end DO_EVAL:

                    if ( not $eval_ok or @warnings ) {
                        my $fatal_error = $EVAL_ERROR;
                        my $rule_id =
                            $and_node->[Marpa::Internal::And_Node::RULE_ID];
                        my $rule = $rules->[$rule_id];
                        Marpa::Internal::code_problems(
                            {   fatal_error => $fatal_error,
                                grammar     => $grammar,
                                eval_ok     => $eval_ok,
                                warnings    => \@warnings,
                                where       => 'ranking rule',
                                long_where  => 'ranking rule: '
                                    . Marpa::brief_rule($rule),
                            }
                        );
                    } ## end if ( not $eval_ok or @warnings )

                    if ( not defined $rank ) {
                        my $rule_id =
                            $and_node->[Marpa::Internal::And_Node::RULE_ID];
                        my $rule = $rules->[$rule_id];
                        Marpa::exception(
                            'numeric ranking action returned undef, rule: ',
                            Marpa::brief_rule($rule),
                        );
                    } ## end if ( not defined $rank )

                    $and_node_iteration
                        ->[Marpa::Internal::And_Iteration::RANKING_DATA] =
                        $rank;

                    ### assert: defined $rank

                    # With the rank processing finished, the
                    # SETUP_AND_NODE task is finished
                    break;    # next TASK

                } ## end if ( $parse_order eq 'numeric' )

                # The rest of the processing is for the original parse
                # ranking
                #
                break if $parse_order ne 'original';    # next TASK

                my $and_node_end_earleme =
                    $and_node->[Marpa::Internal::And_Node::END_EARLEME];

                my $sort_element = $and_node
                    ->[Marpa::Internal::And_Node::FIXED_RANKING_DATA];
                my @current_sort_elements =
                    $sort_element ? ($sort_element) : ();
                my $trailing_nulls = 0;

                my $cause_sort_elements = [];

                if ( defined $cause_ranking_data ) {
                    $cause_sort_elements = $cause_ranking_data
                        ->[Marpa::Internal::Original_Sort_Data::SORT_KEY];

                    #<<< As of 2 Nov 2009 perltidy cycles on this
                    $trailing_nulls += $cause_ranking_data->[
                        Marpa::Internal::Original_Sort_Data::TRAILING_NULLS ];
                    #>>>
                } ## end if ( defined $cause_ranking_data )

                my $predecessor_sort_elements = [];
                my $predecessor_end_earleme;
                my $internal_nulls = 0;

                if ( defined $predecessor_ranking_data ) {
                    $predecessor_end_earleme =
                        $predecessor->[Marpa::Internal::Or_Node::END_EARLEME];

                    $predecessor_sort_elements = $predecessor_ranking_data
                        ->[Marpa::Internal::Original_Sort_Data::SORT_KEY];
                    #<<< As of 2 Nov 2009 perltidy cycles on this
                    $internal_nulls = $predecessor_ranking_data->[
                        Marpa::Internal::Original_Sort_Data::TRAILING_NULLS ];
                    #>>>
                    if ( $predecessor_end_earleme == $and_node_end_earleme ) {
                        $trailing_nulls += $internal_nulls;
                    }
                } ## end if ( defined $predecessor_ranking_data )

                PROCESS_TOKEN: {
                    last PROCESS_TOKEN if not defined $token;

                    if ( $token->[Marpa::Internal::Symbol::NULLABLE] ) {
                        $trailing_nulls += 1;
                    }

                    my $greed = $token->[Marpa::Internal::Symbol::GREED];
                    last PROCESS_TOKEN if not $greed;

                    my $token_start_earleme = $predecessor_end_earleme
                        // $and_node
                        ->[Marpa::Internal::And_Node::START_EARLEME];
                    my $length =
                        $token->[Marpa::Internal::Symbol::GREED] > 0
                        ? ~( ( $and_node_end_earleme - $token_start_earleme )
                        & Marpa::Internal::N_FORMAT_MASK )
                        : ( $and_node_end_earleme - $token_start_earleme );

                    push @current_sort_elements,
                        [
                        $token_start_earleme, $internal_nulls,
                        ~0,                   $length,
                        ];

                } ## end PROCESS_TOKEN:

                if ($internal_nulls) {
                    my @new_cause_sort_elements = ();
                    SORT_ELEMENT:
                    for my $cause_sort_element ( @{$cause_sort_elements} ) {
                        my ($location, $preceding_nulls,
                            $priority, $length
                        ) = @{$cause_sort_element};

                        # If it will be unchanged, just push the reference to save memory
                        if ( $location != $predecessor_end_earleme ) {
                            push @new_cause_sort_elements,
                                $cause_sort_element;
                            next SORT_ELEMENT;
                        }
                        push @new_cause_sort_elements,
                            [
                            $location, $preceding_nulls + $internal_nulls,
                            $priority, $length
                            ];
                    } ## end for my $cause_sort_element ( @{$cause_sort_elements})
                    $cause_sort_elements = \@new_cause_sort_elements;
                } ## end if ($internal_nulls)

                my $and_node_sort_data = $and_node_iteration
                    ->[Marpa::Internal::And_Iteration::RANKING_DATA] = [];

                $and_node_sort_data
                    ->[Marpa::Internal::Original_Sort_Data::SORT_KEY] = [
                    @current_sort_elements, @{$predecessor_sort_elements},
                    @{$cause_sort_elements}
                    ];

                $and_node_sort_data
                    ->[Marpa::Internal::Original_Sort_Data::TRAILING_NULLS] =
                    $trailing_nulls;

                if (    defined $cause
                    and defined $predecessor )
                {
                    my ( $cause_sort_string, $predecessor_sort_string ) =
                        map {
                        ~( join q{}, sort map { pack 'N*', @{$_} } @{$_} )
                        } ( $cause_sort_elements,
                        $predecessor_sort_elements );
                    my $current_child_field =
                        $cause_sort_string ge $predecessor_sort_string
                        ? Marpa::Internal::And_Node::CAUSE_ID
                        : Marpa::Internal::And_Node::PREDECESSOR_ID;

                    #<<< current (2009 Oct 20) version of perltidy cycles on this

                    $and_node_iteration->[
                        Marpa::Internal::And_Iteration::CURRENT_CHILD_FIELD ]
                        = $current_child_field;

                    #>>>
                } ## end if ( defined $cause and defined $predecessor )

            } ## end when (Marpa::Internal::Task::SETUP_AND_NODE)

=begin Implementation:

The visited arguments is needed for RESET_OR_TREE and RESET_AND_TREE
because otherwise every node will be reset once for every possible
derivation involving it.  Resets are idempotent, so in one sense this
is harmless.  But in some cases the number of derivations is O(n!)
in the size of the input and the CPU time consumed can be staggering.

Preventing re-visits to reset items is NOT the same as cycle prevention.
Reset nodes are tracked over the entire tree.  Cycles only occur is a
node appears more than once on the path back to the root node.

=end Implementation:

=cut

            when (Marpa::Internal::Task::RESET_OR_TREE) {

                my ( $or_node_id, $path, $visited ) = @{$task_entry};

                if ($trace_tasks) {
                    print {$Marpa::Internal::TRACE_FH}
                        "Task: RESET_OR_TREE from #o$or_node_id; ",
                        ( scalar @tasks ), " tasks pending\n"
                        or Marpa::exception('print to trace handle failed');
                } ## end if ($trace_tasks)

                my $or_node = $or_nodes->[$or_node_id];
                $visited //= {};
                my @unvisited_children =
                    grep { !( $visited->{$_}++ ) }
                    @{ $or_node->[Marpa::Internal::Or_Node::CHILD_IDS] };
                push @tasks,
                    [ Marpa::Internal::Task::RESET_OR_NODE, $or_node_id ],
                    map {
                    [   Marpa::Internal::Task::NEXT_AND_TREE,
                        $_, $path, $visited
                    ]
                    } @unvisited_children;
            } ## end when (Marpa::Internal::Task::RESET_OR_TREE)

            # This is a bit hack-ish.  It's becomes a reset or
            # an interate depending on the presence of absence
            # of the 3rd "visited" argument.
            when (Marpa::Internal::Task::NEXT_AND_TREE) {
                my ( $and_node_id, $path, $visited ) = @{$task_entry};

                if ($trace_tasks) {
                    print {$Marpa::Internal::TRACE_FH}
                        "Task: NEXT_AND_TREE from #a$and_node_id; ",
                        ( scalar @tasks ), " tasks pending\n"
                        or Marpa::exception('print to trace handle failed');
                } ## end if ($trace_tasks)

                my $and_node = $and_nodes->[$and_node_id];

                if ( my $tree_ops =
                    $and_node->[Marpa::Internal::And_Node::TREE_OPS] )
                {

                    my $use_this_and_node = 1;
                    my @add_to_path       = ();
                    my $op_ix             = 0;
                    TREE_OP: while ( $op_ix <= $#{$tree_ops} ) {

                        my $tree_op = $tree_ops->[ $op_ix++ ];
                        my $rule_id;
                        my $max_count;
                        given ($tree_op) {
                            when (Marpa::Internal::Evaluator_Op::CYCLE) {
                                my @keys =
                                    map  { 'o' . $_ }
                                    grep {defined} @{$and_node}[
                                    Marpa::Internal::And_Node::CAUSE_ID,
                                    Marpa::Internal::And_Node::PREDECESSOR_ID
                                    ];

                                if ( grep { $path->{$_} } @keys ) {
                                    $use_this_and_node = 0;
                                }
                                else {
                                    push @add_to_path,
                                        map { [ $_, 1 ] } @keys;
                                }
                            } ## end when (Marpa::Internal::Evaluator_Op::CYCLE)
                            when ( Marpa::Internal::Evaluator_Op::COUNTED_RULE
                                )
                            {

                                # counted rule logic is not fully tested
                                $rule_id   = $tree_ops->[ $op_ix++ ];
                                $max_count = $tree_ops->[ $op_ix++ ];
                                my $key = "r$rule_id";
                                my $count = $path->{$key} // 0;
                                if ( ++$count >= $max_count ) {
                                    $use_this_and_node = 0;
                                }
                                else {
                                    push @add_to_path, [ $key, $count ];
                                }
                            } ## end when ( Marpa::Internal::Evaluator_Op::COUNTED_RULE )
                            default {
                                Marpa::exception("Unknown tree op: $_");
                            }
                        } ## end given
                    } ## end while ( $op_ix <= $#{$tree_ops} )

                    # This would be a cycle.  Mark the and-node
                    # exhausted and move on.
                    # Note we take some care not to modify
                    # $path until we have to.
                    if ( not $use_this_and_node ) {
                        $and_iterations->[$and_node_id] = undef;

                        break;    # next TASK
                    }

                    # The path must be
                    # re-copied.  If it is shared
                    # among branches, it will become
                    # incorrect.
                    # For efficiency, we use copy-on-write.
                    if ( scalar @add_to_path ) {
                        my %new_path = %{$path};
                        for my $add_to_path (@add_to_path) {
                            my ( $key, $value ) = @{$add_to_path};

                            $new_path{$key} = $value;
                        }
                        $path = \%new_path;
                    } ## end if ( scalar @add_to_path )

                } ## end if ( my $tree_ops = $and_node->[...])

                # If there is no $visited argument,
                # this is an iteration, not a reset
                push @tasks,
                    [
                    (   $visited
                        ? Marpa::Internal::Task::RESET_AND_TREE
                        : Marpa::Internal::Task::ITERATE_AND_TREE
                    ),
                    $and_node_id,
                    $path, $visited
                    ];

            } ## end when (Marpa::Internal::Task::NEXT_AND_TREE)

            when (Marpa::Internal::Task::RESET_AND_TREE) {
                my ( $and_node_id, $path, $visited ) = @{$task_entry};

                if ($trace_tasks) {
                    print {$Marpa::Internal::TRACE_FH}
                        "Task: RESET_AND_TREE from #a$and_node_id; ",
                        ( scalar @tasks ), " tasks pending\n"
                        or Marpa::exception('print to trace handle failed');
                } ## end if ($trace_tasks)

                my $and_node = $and_nodes->[$and_node_id];

                push @tasks,
                    [ Marpa::Internal::Task::RESET_AND_NODE, $and_node_id ],
                    map {
                    [   Marpa::Internal::Task::RESET_OR_TREE,
                        $_, $path, $visited
                    ]
                    }
                    grep { defined $_ } @{$and_node}[
                    Marpa::Internal::And_Node::CAUSE_ID,
                    Marpa::Internal::And_Node::PREDECESSOR_ID
                    ];

            } ## end when (Marpa::Internal::Task::RESET_AND_TREE)

            when (Marpa::Internal::Task::ITERATE_AND_TREE) {
                my ( $and_node_id, $path ) = @{$task_entry};

                if ($trace_tasks) {
                    print {$Marpa::Internal::TRACE_FH}
                        "Task: ITERATE_AND_TREE from #a$and_node_id; ",
                        ( scalar @tasks ), " tasks pending\n"
                        or Marpa::exception('print to trace handle failed');
                } ## end if ($trace_tasks)

                push @tasks,
                    [ Marpa::Internal::Task::SETUP_AND_NODE, $and_node_id ];

                # Iteration of and-node without child always results in
                # exhausted and-node
                my $current_child_field =
                    $and_iterations->[$and_node_id]
                    ->[Marpa::Internal::And_Iteration::CURRENT_CHILD_FIELD];
                if ( not defined $current_child_field ) {
                    $and_iterations->[$and_node_id] = undef;
                    break;    # next TASK
                }

                my $and_node = $and_nodes->[$and_node_id];

                my $cause_id =
                    $and_node->[Marpa::Internal::And_Node::CAUSE_ID];
                my $predecessor_id =
                    $and_node->[Marpa::Internal::And_Node::PREDECESSOR_ID];
                if ( defined $cause_id and defined $predecessor_id ) {
                    push @tasks,
                        [
                        Marpa::Internal::Task::ITERATE_AND_TREE_2,
                        $and_node_id, $path
                        ];
                } ## end if ( defined $cause_id and defined $predecessor_id )

                push @tasks,
                    [
                    Marpa::Internal::Task::ITERATE_OR_TREE,
                    $and_node->[$current_child_field],
                    $path
                    ];

            } ## end when (Marpa::Internal::Task::ITERATE_AND_TREE)

            when (Marpa::Internal::Task::ITERATE_AND_TREE_2) {

                # We always have both a cause and a predecessor if we are
                # in this task.

                my ( $and_node_id, $path ) = @{$task_entry};

                if ($trace_tasks) {
                    print {$Marpa::Internal::TRACE_FH}
                        "Task: ITERATE_AND_TREE_2 from #a$and_node_id; ",
                        ( scalar @tasks ), " tasks pending\n"
                        or Marpa::exception('print to trace handle failed');
                } ## end if ($trace_tasks)

                my $and_node = $and_nodes->[$and_node_id];

                my $current_child_field =
                    $and_iterations->[$and_node_id]
                    ->[Marpa::Internal::And_Iteration::CURRENT_CHILD_FIELD];

                # if the current child is not exhausted, the last task
                # successfully iterated it.  So SETUP_AND_NODE
                # (which is already on the tasks stack) is all
                # that is needed.
                break
                    if defined
                        $or_iterations->[ $and_node->[$current_child_field] ];

                my $other_child_id = $and_node->[
                    $current_child_field
                    == Marpa::Internal::And_Node::CAUSE_ID
                    ? Marpa::Internal::And_Node::PREDECESSOR_ID
                    : Marpa::Internal::And_Node::CAUSE_ID
                ];

                push @tasks,
                    [
                    Marpa::Internal::Task::ITERATE_AND_TREE_3, $and_node_id,
                    $path
                    ],
                    [
                    Marpa::Internal::Task::ITERATE_OR_TREE, $other_child_id,
                    $path
                    ];

            } ## end when (Marpa::Internal::Task::ITERATE_AND_TREE_2)

            when (Marpa::Internal::Task::ITERATE_AND_TREE_3) {

                # We always have both a cause and a predecessor if we are
                # in this task.
                my ( $and_node_id, $path ) = @{$task_entry};

                if ($trace_tasks) {
                    print {$Marpa::Internal::TRACE_FH}
                        "Task: ITERATE_AND_TREE_3 from #a$and_node_id; ",
                        ( scalar @tasks ), " tasks pending\n"
                        or Marpa::exception('print to trace handle failed');
                } ## end if ($trace_tasks)

                my $and_node = $and_nodes->[$and_node_id];

                my @exhausted_children =
                    grep { not defined $or_iterations->[$_] } @{$and_node}[
                    Marpa::Internal::And_Node::CAUSE_ID,
                    Marpa::Internal::And_Node::PREDECESSOR_ID
                    ];

                # If both children exhausted, this and node is exhausted
                # Let SETUP_AND_NODE (which is already on the tasks stack)
                # deal with that.
                break if @exhausted_children >= 2;

                # The RESET_OR_TREE either will find a valid iteration,
                # or leave the one exhausted child still exhausted.
                # Either way SETUP_AND_NODE
                # (which is already on the tasks stack)
                # can deal with that.
                push @tasks,
                    [
                    Marpa::Internal::Task::RESET_OR_TREE,
                    $exhausted_children[0],
                    $path
                    ];

            } ## end when (Marpa::Internal::Task::ITERATE_AND_TREE_3)

            when (Marpa::Internal::Task::ITERATE_OR_NODE) {
                my ($or_node_id) = @{$task_entry};

                if ($trace_tasks) {
                    print {$Marpa::Internal::TRACE_FH}
                        "Task: ITERATE_OR_NODE #o$or_node_id; ",
                        ( scalar @tasks ), " tasks pending\n"
                        or Marpa::exception('print to trace handle failed');
                } ## end if ($trace_tasks)

                my $and_choices = $or_iterations->[$or_node_id];

                my $current_and_choice = $and_choices->[-1];
                my $current_and_node_id =
                    $current_and_choice->[Marpa::Internal::And_Choice::ID];
                my $current_and_iteration =
                    $and_iterations->[$current_and_node_id];

                # If the current and-choice is exhausted ...
                if ( not defined $current_and_iteration ) {
                    pop @{$and_choices};

                    if ($trace_tasks) {
                        print {$Marpa::Internal::TRACE_FH}
                            "...ITERATE_OR_NODE #a$current_and_node_id exhausted\n",
                            or
                            Marpa::exception('print to trace handle failed');
                    } ## end if ($trace_tasks)

                    # If there are no more choices, the or-node is exhausted ...
                    if ( scalar @{$and_choices} == 0 ) {
                        if ($trace_tasks) {
                            print {$Marpa::Internal::TRACE_FH}
                                "...ITERATE_OR_NODE #o$or_node_id exhausted\n",
                                or Marpa::exception(
                                'print to trace handle failed');
                        } ## end if ($trace_tasks)
                        $or_iterations->[$or_node_id] = undef;
                        break;
                    } ## end if ( scalar @{$and_choices} == 0 )

                    # Thaw out the current and-choice,
                    push @tasks,
                        [
                        Marpa::Internal::Task::THAW_TREE,
                        $and_choices->[-1]
                        ];

                    break;    # next TASK

                } ## end if ( not defined $current_and_iteration )

                # If we are here the current and-choice is not exhausted,
                # but it may have been iterated to the point where it is
                # no longer the first in sort order.

                # Refresh and-choice's fields
                $current_and_choice
                    ->[Marpa::Internal::And_Choice::RANKING_DATA] =
                    $current_and_iteration
                    ->[Marpa::Internal::And_Iteration::RANKING_DATA];

                # The rest of the logic is for keeping the order correct
                # for the "original" parse ordering

                break    # next TASK
                    if $parse_order eq 'none';

                # If only one choice still active,
                # clearly no need to
                # worry about sorting alternatives.
                break if @{$and_choices} <= 1;

                my $insert_point;
                given ($parse_order) {
                    when ('numeric') {
                        my $current_sort_key = $current_and_choice
                            ->[Marpa::Internal::And_Choice::RANKING_DATA];
                        no integer;
                        AND_CHOICE:
                        for (
                            my $and_choice_ix = $#{$and_choices} - 1;
                            $and_choice_ix >= 0;
                            $and_choice_ix--
                            )
                        {
                            if ( $and_choices->[$and_choice_ix]
                                ->[Marpa::Internal::And_Choice::RANKING_DATA]
                                <= $current_sort_key )
                            {
                                $insert_point = $and_choice_ix + 1;
                                last AND_CHOICE;
                            } ## end if ( $and_choices->[$and_choice_ix]->[...])
                        } ## end for ( my $and_choice_ix = $#{$and_choices} - 1; ...)
                    } ## end when ('numeric')
                    when ('original') {
                        my $current_sort_key = ~(
                            join q{},
                            sort map { pack 'N*', @{$_} } @{
                                $current_and_choice->[
                                    Marpa::Internal::And_Choice::RANKING_DATA]
                                    ->[
                                    Marpa::Internal::Original_Sort_Data::SORT_KEY
                                    ]
                                }
                        );

                        AND_CHOICE:
                        for (
                            my $and_choice_ix = $#{$and_choices} - 1;
                            $and_choice_ix >= 0;
                            $and_choice_ix--
                            )
                        {
                            if (~(  join q{},
                                    sort map { pack 'N*', @{$_} } @{
                                        $and_choices->[$and_choice_ix]->[
                                            Marpa::Internal::And_Choice::RANKING_DATA
                                            ]->[
                                            Marpa::Internal::Original_Sort_Data::SORT_KEY
                                            ]
                                        }
                                ) le $current_sort_key
                                )
                            {
                                $insert_point = $and_choice_ix + 1;
                                last AND_CHOICE;
                            } ## end if ( ~( join q{}, sort map { pack 'N*', @{$_} } @{ ...}))
                        } ## end for ( my $and_choice_ix = $#{$and_choices} - 1; ...)

                    } ## end when ('original')
                } ## end given

                $insert_point //= 0;

                # If current choice would be inserted where it already
                # is now, we're done
                break if $insert_point == $#{$and_choices};

                my $former_current_choice = pop @{$and_choices};
                splice @{$and_choices}, $insert_point, 0,
                    $former_current_choice;

                if ($trace_tasks) {
                    printf {$Marpa::Internal::TRACE_FH} (
                        "...ITERATE_OR_NODE Sorting and-choices\n",
                        "...ITERATE_OR_NODE Replacing #a%d with #a%d\n",
                        $former_current_choice
                            ->[Marpa::Internal::And_Choice::ID],
                        $and_choices->[-1]->[Marpa::Internal::And_Choice::ID],
                    ) or Marpa::exception('print to trace handle failed');
                } ## end if ($trace_tasks)

                push @tasks,
                    [ Marpa::Internal::Task::THAW_TREE, $and_choices->[-1] ],
                    [
                    Marpa::Internal::Task::FREEZE_TREE,
                    $former_current_choice
                    ];

            } ## end when (Marpa::Internal::Task::ITERATE_OR_NODE)

            when (Marpa::Internal::Task::ITERATE_OR_TREE) {
                my ( $or_node_id, $path ) = @{$task_entry};

                if ($trace_tasks) {
                    print {$Marpa::Internal::TRACE_FH}
                        "Task: ITERATE_OR_TREE #o$or_node_id; ",
                        ( scalar @tasks ), " tasks pending\n"
                        or Marpa::exception('print to trace handle failed');
                } ## end if ($trace_tasks)

                my $or_node = $or_nodes->[$or_node_id];

                my $current_and_node_id =
                    $or_iterations->[$or_node_id]->[-1]
                    ->[Marpa::Internal::And_Choice::ID];
                push @tasks,
                    [ Marpa::Internal::Task::ITERATE_OR_NODE, $or_node_id ],
                    [
                    Marpa::Internal::Task::NEXT_AND_TREE,
                    $current_and_node_id, $path
                    ];
            } ## end when (Marpa::Internal::Task::ITERATE_OR_TREE)

            when (Marpa::Internal::Task::FREEZE_TREE) {
                my ($and_choice) = @{$task_entry};

                my $and_node_id =
                    $and_choice->[Marpa::Internal::And_Choice::ID];

                if ($trace_tasks) {
                    printf {$Marpa::Internal::TRACE_FH}
                        "Task: FREEZE_TREE; #a%d; %d tasks pending\n",
                        $and_node_id, ( scalar @tasks )
                        or Marpa::exception('print to trace handle failed');
                } ## end if ($trace_tasks)

                my @work_list = ($and_node_id);
                my @and_slice = ();
                my @or_slice  = ();

                AND_NODE: while ( scalar @work_list ) {
                    my $descendant_and_node_id = pop @work_list;
                    push @and_slice, $descendant_and_node_id;
                    my @descendant_or_node_ids = grep { defined $_ }
                        map { $and_nodes->[$descendant_and_node_id]->[$_] }
                        ( Marpa::Internal::And_Node::PREDECESSOR_ID,
                        Marpa::Internal::And_Node::CAUSE_ID
                        );
                    push @or_slice,  @descendant_or_node_ids;
                    push @work_list, map {
                        $or_iterations->[$_]->[-1]
                            ->[Marpa::Internal::And_Choice::ID]
                    } @descendant_or_node_ids;
                } ## end while ( scalar @work_list )

                my @or_values  = @{$or_iterations}[@or_slice];
                my @and_values = @{$and_iterations}[@and_slice];

                $and_choice->[Marpa::Internal::And_Choice::FROZEN_ITERATION] =
                    Storable::freeze(
                    [ \@and_slice, \@and_values, \@or_slice, \@or_values ] );

            } ## end when (Marpa::Internal::Task::FREEZE_TREE)

            when (Marpa::Internal::Task::THAW_TREE) {
                my ($and_choice) = @{$task_entry};

                my $and_node_id =
                    $and_choice->[Marpa::Internal::And_Choice::ID];

                if ($trace_tasks) {
                    printf {$Marpa::Internal::TRACE_FH}
                        "Task: THAW_TREE; and-node #a%d; %d tasks pending\n",
                        $and_node_id, ( scalar @tasks )
                        or Marpa::exception('print to trace handle failed');
                } ## end if ($trace_tasks)

                # If we are here, the current choice is new
                # It must be thawed and its frozen iteration thrown away
                my ( $and_slice, $and_values, $or_slice, $or_values ) = @{
                    Storable::thaw(
                        $and_choice
                            ->[Marpa::Internal::And_Choice::FROZEN_ITERATION]
                    )
                    };

                @{$and_iterations}[ @{$and_slice} ] = @{$and_values};
                @{$or_iterations}[ @{$or_slice} ]   = @{$or_values};

                # Refresh and-choice's fields
                my $current_and_iteration = $and_iterations->[$and_node_id];
                $and_choice->[Marpa::Internal::And_Choice::RANKING_DATA] =
                    $current_and_iteration
                    ->[Marpa::Internal::And_Iteration::RANKING_DATA];

                # Once it's unfrozen, it's subject to change, so the
                # the frozen version will become invalid.
                # We undef it.
                $and_choice->[Marpa::Internal::And_Choice::FROZEN_ITERATION] =
                    undef;

            } ## end when (Marpa::Internal::Task::THAW_TREE)

            when (Marpa::Internal::Task::EVALUATE) {

                if ($trace_tasks) {
                    print {
                        $Marpa::Internal::TRACE_FH
                    }
                    'Task: EVALUATE; ', ( scalar @tasks ), " tasks pending\n"
                        or Marpa::exception('print to trace handle failed');
                } ## end if ($trace_tasks)

                # If the top or node is exhausted, we are done
                my $top_or_iteration = $or_iterations->[0];
                return if not $top_or_iteration;

                # Write the and-nodes out in preorder
                my @preorder = ();

                # Initialize the work list to the top and-node
                my @work_list = (
                    $and_nodes->[
                        $top_or_iteration->[-1]
                        ->[Marpa::Internal::And_Choice::ID]
                    ]
                );

                AND_NODE: while ( scalar @work_list ) {
                    my $and_node = pop @work_list;
                    push @work_list, map {
                        $and_nodes->[ $or_iterations->[$_]->[-1]
                            ->[Marpa::Internal::And_Choice::ID] ]
                        }
                        grep { defined $_ }
                        map  { $and_node->[$_] }
                        ( Marpa::Internal::And_Node::PREDECESSOR_ID,
                        Marpa::Internal::And_Node::CAUSE_ID
                        );
                    push @preorder, $and_node;
                } ## end while ( scalar @work_list )

                my $action_object_class =
                    $grammar->[Marpa::Internal::Grammar::ACTION_OBJECT];
                my $action_object_constructor = $evaler
                    ->[Marpa::Internal::Evaluator::ACTION_OBJECT_CONSTRUCTOR];

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
                                $action_object_constructor->(
                                $action_object_class);
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

                return Marpa::Internal::Evaluator::evaluate( $grammar,
                    $action_object, \@preorder,
                    $evaler->[Marpa::Internal::Evaluator::TRACE_VALUES] );

            } ## end when (Marpa::Internal::Task::EVALUATE)
            ## End EVALUATE

            default {
                Carp::confess("Internal error: Unknown task, number $task");
            }
        } ## end given

    } ## end while (1)

    Carp::confess('Internal error: Should not reach here');

} ## end sub Marpa::Evaluator::value

1;
