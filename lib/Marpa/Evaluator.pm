package Marpa::Evaluator;

use 5.010;
use warnings;
no warnings qw(recursion qw);
use strict;
use integer;

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
    PREDECESSOR CAUSE
    VALUE_REF PERL_CLOSURE END_EARLEME
    ARGC RULE POSITION
    PARENT_ID
    PARENT_CHOICE
    DELETED

    { delete this } RANK

    =LAST_FIELD

);

use Marpa::Offset qw(

    :package=Marpa::Internal::Or_Node

    TAG ID CHILD_IDS

    { Delete this ... }
    AND_NODES
    { ... and this }
    IS_COMPLETED { is this a completed or-node? }

    START_EARLEME END_EARLEME
    PARENT_IDS
    DELETED

    =LAST_FIELD
);

use Marpa::Offset qw(

    { Delete this whole thing }
    :{ Will these be needed? :}
    :package=Marpa::Internal::Tree_Node

    OR_NODE CHOICE PREDECESSOR
    CAUSE DEPTH
    PERL_CLOSURE ARGC VALUE_REF
    RULE POSITION PARENT
);

use Marpa::Offset qw(

    :package=Marpa::Internal::Evaluator

    RECOGNIZER
    PARSE_COUNT :{ number of parses in an ambiguous parse :}
    OR_NODES
    AND_NODES
    { Delete this } TREE { current evaluation tree }
    RULE_DATA
    PACKAGE
    NULL_VALUES
    { Delete this } CYCLES { Will this be needed? }
    JOURNAL
    INSTANCES
    DECISIONS

);

# Tags for the Journal Entries
use Marpa::Offset qw(

    :package=Marpa::Internal::Journal_Tag

    NODE { Records the acceptance or rejection an and-node. }

    FORK { An interable decision to follow one branch of a fork
    up the parse bocage. }

    INSTANCE { An iterable decision to accept one of an instance's
    and-nodes , or to reject them all.  }

    RATCHET_INSTANCE_ACCEPTANCES {
    Ratchet instance acceptances below "here".  That is,
    the only iteration allowed for an accepted instance
    is to reject it -- other possible ways of accepting
    the instance are not considered.
    }

);

use Marpa::Offset qw(

    :package=Marpa::Internal::Evaluator_Rule
    CODE PERL_CLOSURE

);

use Marpa::Offset qw(

    { Delete all these }
    :package=Marpa::Internal::Ranked_Node
    DECISION

);

package Marpa::Internal::Evaluator;

# use Smart::Comments '###';

### Using smart comments <where>...

use Scalar::Util qw(weaken);
use List::Util qw(min);
use English qw( -no_match_vars );
use Data::Dumper;
use Marpa::Internal;
our @CARP_NOT = @Marpa::Internal::CARP_NOT;

sub run_preamble {
    my $grammar = shift;
    my $package = shift;

    my $preamble = $grammar->[Marpa::Internal::Grammar::PREAMBLE];
    return if not defined $preamble;

    my $code = 'package ' . $package . ";\n" . $preamble;
    my $eval_ok;
    my @warnings;
    DO_EVAL: {
        local $SIG{__WARN__} =
            sub { push @warnings, [ $_[0], ( caller 0 ) ]; };

        ## no critic (BuiltinFunctions::ProhibitStringyEval)
        $eval_ok = eval $code;
        ## use critic
    } ## end DO_EVAL:

    if ( not $eval_ok or @warnings ) {
        my $fatal_error = $EVAL_ERROR;
        Marpa::Internal::code_problems(
            {   grammar     => $grammar,
                eval_ok     => $eval_ok,
                fatal_error => $fatal_error,
                warnings    => \@warnings,
                where       => 'evaluating preamble',
                code        => \$code,
            }
        );
    } ## end if ( not $eval_ok or @warnings )

    return;

}    # run_preamble

# Given symbol, returns null value, calculating it
# if necessary.
#
# Assumes all but CHAF values have already been set
sub set_null_symbol_value {
    my $null_values = shift;
    my $symbol      = shift;

    # if it's not a CHAF nulling symbol,
    # or the value is already set, use what we have
    my $chaf_nulling = $symbol->[Marpa::Internal::Symbol::IS_CHAF_NULLING];
    my $symbol_id    = $symbol->[Marpa::Internal::Symbol::ID];
    my $null_value   = $null_values->[$symbol_id];
    if ( not $chaf_nulling or defined $null_value ) {
        return $null_value;
    }

    # it is a CHAF nulling symbol, but needs its null value calculated.
    my @chaf_null_values = ();
    for my $rhs_symbol ( @{$chaf_nulling} ) {
        my $nulling_symbol =
            $rhs_symbol->[Marpa::Internal::Symbol::NULL_ALIAS] // $rhs_symbol;
        my $value = set_null_symbol_value( $null_values, $nulling_symbol );
        push @chaf_null_values, $value;
    } ## end for my $rhs_symbol ( @{$chaf_nulling} )
    push @chaf_null_values, [];

    return ( $null_values->[$symbol_id] = \@chaf_null_values );

}    # null symbol value

sub set_null_values {
    my $grammar = shift;
    my $package = shift;

    my ( $rules, $symbols, $tracing, $default_null_value ) = @{$grammar}[
        Marpa::Internal::Grammar::RULES,
        Marpa::Internal::Grammar::SYMBOLS,
        Marpa::Internal::Grammar::TRACING,
        Marpa::Internal::Grammar::DEFAULT_NULL_VALUE,
    ];

    my $null_values;
    $#{$null_values} = $#{$symbols};

    my $trace_fh;
    my $trace_actions;
    if ($tracing) {
        $trace_fh = $grammar->[Marpa::Internal::Grammar::TRACE_FILE_HANDLE];
        $trace_actions = $grammar->[Marpa::Internal::Grammar::TRACE_ACTIONS];
    }

    SYMBOL: for my $symbol ( @{$symbols} ) {
        next SYMBOL
            if $symbol->[Marpa::Internal::Symbol::IS_CHAF_NULLING];
        my $id = $symbol->[Marpa::Internal::Symbol::ID];
        $null_values->[$id] = $default_null_value;
    } ## end for my $symbol ( @{$symbols} )

    # Before tackling the CHAF symbols, set null values specified in
    # empty rules.
    RULE: for my $rule ( @{$rules} ) {

        my $action = $rule->[Marpa::Internal::Rule::ACTION];

        # Set the null value of symbols from the action for their
        # empty rules
        my $rhs = $rule->[Marpa::Internal::Rule::RHS];

        # Empty rule with action?
        if ( defined $action and @{$rhs} <= 0 ) {

            my $lhs            = $rule->[Marpa::Internal::Rule::LHS];
            my $nulling_symbol = $lhs->[Marpa::Internal::Symbol::NULL_ALIAS]
                // $lhs;

            my $null_value;
            my $code =
                ## no critic (ValuesAndExpressions::RequireInterpolationOfMetachars)
                '$null_value = do {' . "\n"
                ## use critic
                . "    package $package;\n" . $action . "};\n" . "1;\n";

            my @warnings;
            my $eval_ok;
            DO_EVAL: {
                local $SIG{__WARN__} =
                    sub { push @warnings, [ $_[0], ( caller 0 ) ]; };

                ## no critic (BuiltinFunctions::ProhibitStringyEval)
                $eval_ok = eval $code;
                ## use critic
            } ## end DO_EVAL:

            if ( not $eval_ok or @warnings ) {
                my $fatal_error = $EVAL_ERROR;
                Marpa::Internal::code_problems(
                    {   eval_ok     => $eval_ok,
                        fatal_error => $fatal_error,
                        grammar     => $grammar,
                        warnings    => \@warnings,
                        where       => 'evaluating null value',
                        long_where  => 'evaluating null value for '
                            . $nulling_symbol
                            ->[Marpa::Internal::Symbol::NAME],
                        code => \$code,
                    }
                );
            } ## end if ( not $eval_ok or @warnings )
            my $nulling_symbol_id =
                $nulling_symbol->[Marpa::Internal::Symbol::ID];
            $null_values->[$nulling_symbol_id] = $null_value;

            if ($trace_actions) {
                print {$trace_fh} 'Setting null value for symbol ',
                    $nulling_symbol->[Marpa::Internal::Symbol::NAME],
                    " from\n", $code, "\n",
                    ' to ',
                    Data::Dumper->new( [ \$null_value ] )->Terse(1)->Dump,
                    "\n"
                    or Marpa::exception('Could not print to trace file');
            } ## end if ($trace_actions)

        } ## end if ( defined $action and @{$rhs} <= 0 )

    }    # RULE

    SYMBOL: for my $symbol ( @{$symbols} ) {
        next SYMBOL
            if not $symbol->[Marpa::Internal::Symbol::IS_CHAF_NULLING];
        set_null_symbol_value( $null_values, $symbol );
    }

    if ($trace_actions) {
        SYMBOL: for my $symbol ( @{$symbols} ) {
            next SYMBOL
                if not $symbol->[Marpa::Internal::Symbol::IS_CHAF_NULLING];

            my ( $name, $id ) = @{$symbol}[
                Marpa::Internal::Symbol::NAME, Marpa::Internal::Symbol::ID,
            ];
            print {$trace_fh}
                'Setting null value for CHAF symbol ',
                $name, ' to ',
                Data::Dumper->new( [ $null_values->[$id] ] )->Terse(1)->Dump,
                or Marpa::exception('Could not print to trace file');
        } ## end for my $symbol ( @{$symbols} )
    } ## end if ($trace_actions)

    return $null_values;

}    # set_null_values

sub set_actions {
    my $grammar = shift;
    my $package = shift;

    my ( $rules, $tracing, $default_action, ) = @{$grammar}[
        Marpa::Internal::Grammar::RULES,
        Marpa::Internal::Grammar::TRACING,
        Marpa::Internal::Grammar::DEFAULT_ACTION,
    ];

    # need trace_fh for code problems here, even if not tracing
    my $trace_fh = $grammar->[Marpa::Internal::Grammar::TRACE_FILE_HANDLE];
    my $trace_actions;
    if ($tracing) {
        $trace_actions = $grammar->[Marpa::Internal::Grammar::TRACE_ACTIONS];
    }

    my $rule_data = [];

    RULE: for my $rule ( @{$rules} ) {

        next RULE if not $rule->[Marpa::Internal::Rule::USEFUL];

        my $action = $rule->[Marpa::Internal::Rule::ACTION];

        ACTION: {

            $action //= $default_action;
            last ACTION if not defined $action;

            # HAS_CHAF_RHS and HAS_CHAF_LHS would work well as a bit
            # mask in a C implementation
            my $has_chaf_lhs = $rule->[Marpa::Internal::Rule::HAS_CHAF_LHS];
            my $has_chaf_rhs = $rule->[Marpa::Internal::Rule::HAS_CHAF_RHS];

            last ACTION if not $has_chaf_lhs and not $has_chaf_rhs;

            if ( $has_chaf_rhs and $has_chaf_lhs ) {
                ## no critic (ValuesAndExpressions::RequireInterpolationOfMetachars)
                $action = q{ \@_; };
                ## use critic
                last ACTION;
            } ## end if ( $has_chaf_rhs and $has_chaf_lhs )

            # At this point has chaf rhs or lhs but not both
            if ($has_chaf_lhs) {

                ## no critic (ValuesAndExpressions::RequireInterpolationOfMetachars)
                $action = q{push @_, [];} . "\n" . q{\@_} . "\n";
                ## use critic
                last ACTION;

            } ## end if ($has_chaf_lhs)

            # at this point must have chaf rhs and not a chaf lhs

            #<<< no perltidy
            $action =
                  "    TAIL: for (;;) {\n"
                ## no critic (ValuesAndExpressions::RequireInterpolationOfMetachars)
                . q<        my $tail = pop @_;> . "\n"
                . q<        last TAIL if not scalar @{$tail};> . "\n"
                . q<        push @_, @{$tail};> . "\n"
                ## use critic
                . "    } # TAIL\n"
                . $action;
            #>>>
        }    # ACTION

        my $rule_id = $rule->[Marpa::Internal::Rule::ID];

        if ( not defined $action ) {

            if ($trace_actions) {
                print {$trace_fh} 'Setting action for rule ',
                    Marpa::brief_rule($rule), " to undef by default\n"
                    or Marpa::exception('Could not print to trace file');
            }

            my $rule_datum;
            $rule_datum->[Marpa::Internal::Evaluator_Rule::CODE] =
                'default to undef';
            $rule_datum->[Marpa::Internal::Evaluator_Rule::PERL_CLOSURE] =
                \undef;
            $rule_data->[$rule_id] = $rule_datum;
            next RULE;
        } ## end if ( not defined $action )

        my $code =
            "sub {\n" . '    package ' . $package . ";\n" . $action . '}';

        if ($trace_actions) {
            print {$trace_fh} 'Setting action for rule ',
                Marpa::brief_rule($rule), " to\n", $code, "\n"
                or Marpa::exception('Could not print to trace file');
        }

        my $closure;
        my @warnings;
        DO_EVAL: {
            local $SIG{__WARN__} =
                sub { push @warnings, [ $_[0], ( caller 0 ) ]; };

            ## no critic (BuiltinFunctions::ProhibitStringyEval)
            $closure = eval $code;
            ## use critic
        } ## end DO_EVAL:

        if ( not $closure or @warnings ) {
            my $fatal_error = $EVAL_ERROR;
            Marpa::Internal::code_problems(
                {   fatal_error => $fatal_error,
                    grammar     => $grammar,
                    warnings    => \@warnings,
                    where       => 'compiling action',
                    long_where  => 'compiling action for '
                        . Marpa::brief_rule($rule),
                    code => \$code,
                }
            );
        } ## end if ( not $closure or @warnings )

        my $rule_datum;
        $rule_datum->[Marpa::Internal::Evaluator_Rule::CODE] = $code;
        $rule_datum->[Marpa::Internal::Evaluator_Rule::PERL_CLOSURE] =
            $closure;

        $rule_data->[$rule_id] = $rule_datum;

    }    # RULE

    return $rule_data;

}    # set_actions

sub audit_or_node {
    my ( $evaler, $or_node ) = @_;
    my $or_nodes  = $evaler->[Marpa::Internal::Evaluator::OR_NODES];
    my $and_nodes = $evaler->[Marpa::Internal::Evaluator::AND_NODES];

    my $id = $or_node->[Marpa::Internal::Or_Node::ID];

    if ( not defined $id ) {
        Marpa::exception('ID not defined in or-node');
    }
    my $or_nodes_entry = $or_nodes->[$id];
    if ( $or_node != $or_nodes_entry ) {
        Marpa::exception("or_node #$id does not match its or-nodes entry");
    }
    if ( $#{$or_node} != Marpa::Internal::Or_Node::LAST_FIELD ) {
        Marpa::exception(
            "Bad field count in or-node #$id: want ",
            Marpa::Internal::Or_Node::LAST_FIELD,
            ', got ', $#{$or_node}
        );
    } ## end if ( $#{$or_node} != Marpa::Internal::Or_Node::LAST_FIELD)

    my $deleted = $or_node->[Marpa::Internal::Or_Node::DELETED];

    my $parent_ids = $or_node->[Marpa::Internal::Or_Node::PARENT_IDS];

    # No parents for top or-node, or-node 0
    if ( $id != 0 ) {
        my $has_parents = ( defined $parent_ids and scalar @{$parent_ids} );
        if ( not $deleted and not $has_parents ) {
            Marpa::exception("or-node #$id has no parents");
        }
        if ( $deleted and $has_parents ) {

            ### Or node, id, parent_ids: $id, $parent_ids

            Marpa::exception("Deleted or-node #$id has parents");
        } ## end if ( $deleted and $has_parents )
    } ## end if ( $id != 0 )

    {
        my %parent_id_seen;
        PARENT_ID: for my $parent_id ( @{$parent_ids} ) {
            next PARENT_ID if not $parent_id_seen{$parent_id}++;
            Marpa::exception(
                "or-node #$id has duplicate parent, #$parent_id");
        }
    }

    PARENT_ID: for my $parent_id ( @{$parent_ids} ) {
        my $parent = $and_nodes->[$parent_id];
        my $cause  = $parent->[Marpa::Internal::And_Node::CAUSE];
        next PARENT_ID if defined $cause and $or_node == $cause;

        my $predecessor = $parent->[Marpa::Internal::And_Node::PREDECESSOR];
        next PARENT_ID if defined $predecessor and $or_node == $predecessor;

        ### or_node: $or_node->[Marpa'Internal'Or_Node'ID]
        ### cause: $cause->[Marpa'Internal'Or_Node'ID]
        ### predecessor: $predecessor->[Marpa'Internal'Or_Node'ID]

        Marpa::exception(
            "or_node #$id is not the cause or predecessor of parent and-node #$parent_id"
        );

    } ## end for my $parent_id ( @{$parent_ids} )

    my $child_ids = $or_node->[Marpa::Internal::Or_Node::CHILD_IDS];
    my $has_children = ( defined $child_ids and scalar @{$child_ids} );
    if ( not $deleted and not $has_children ) {
        Marpa::exception("or-node #$id has no children");
    }
    if ( $deleted and $has_children ) {
        Marpa::exception("Deleted or-node #$id has children");
    }

    {
        my %child_id_seen;
        CHILD_ID: for my $child_id ( @{$child_ids} ) {
            next CHILD_ID if not $child_id_seen{$child_id}++;
            Marpa::exception("or-node #$id has duplicate child, #$child_id");
        }
    }

    for my $child_id ( @{$child_ids} ) {
        my $child        = $and_nodes->[$child_id];
        my $child_parent = $child->[Marpa::Internal::And_Node::PARENT_ID];
        if ( not defined $child_parent or $id != $child_parent ) {
            Marpa::exception(
                "or_node #$id is not the parent of child and-node #$child_id"
            );
        }
    } ## end for my $child_id ( @{$child_ids} )

    my $child_and_nodes = $or_node->[Marpa::Internal::Or_Node::AND_NODES];
    for my $child_ix ( 0 .. $#{$child_and_nodes} ) {
        my $child_and_node = $child_and_nodes->[$child_ix];
        my $child_and_node_id =
            $child_and_node->[Marpa::Internal::And_Node::ID];
        if ( $child_and_node_id != $child_ids->[$child_ix] ) {
            Marpa::exception(
                "or_node #$id, child $child_ix: AND_NODES child does not match CHILD_IDS"
            );
        }
    } ## end for my $child_ix ( 0 .. $#{$child_and_nodes} )

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
        my $parent_idea_of_child =
            $parent_or_node->[Marpa::Internal::Or_Node::AND_NODES]
            ->[$parent_choice];
        if ( $audit_and_node != $parent_idea_of_child ) {
            Marpa::exception(
                "and_node #$audit_and_node_id does not match its AND_NODES entry in its parent"
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
        Marpa::Internal::And_Node::PREDECESSOR,
        Marpa::Internal::And_Node::CAUSE,
        )
    {
        my $child_or_node = $audit_and_node->[$field];
        next FIELD if not defined $child_or_node;
        my $child_or_node_id = $child_or_node->[Marpa::Internal::Or_Node::ID];
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

    } ## end for my $field ( Marpa::Internal::And_Node::PREDECESSOR...)

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
    my ( $evaler, $and_node ) = @_;
    my $and_nodes = $evaler->[Marpa::Internal::Evaluator::AND_NODES];
    my $new_and_node;
    $#{$new_and_node} = Marpa::Internal::And_Node::LAST_FIELD;
    my $new_and_node_id = $new_and_node->[Marpa::Internal::And_Node::ID] =
        scalar @{$and_nodes};

    ### Creating new and node, id: $new_and_node_id

    ### Cloning from and-node id: $and_node->[Marpa'Internal'And_Node'ID]

    push @{$and_nodes}, $new_and_node;

    for my $field (
        Marpa::Internal::And_Node::TAG,
        Marpa::Internal::And_Node::VALUE_REF,
        Marpa::Internal::And_Node::PERL_CLOSURE,
        Marpa::Internal::And_Node::END_EARLEME,
        Marpa::Internal::And_Node::ARGC,
        Marpa::Internal::And_Node::RULE,
        Marpa::Internal::And_Node::POSITION,
        Marpa::Internal::And_Node::RANK,
        )
    {
        $new_and_node->[$field] = $and_node->[$field];
    } ## end for my $field ( Marpa::Internal::And_Node::TAG, ...)
    $new_and_node->[Marpa::Internal::And_Node::TAG] =~ s{
        [#] \d* \z
    }{#$new_and_node_id}xms;

    return $new_and_node;
} ## end sub clone_and_node

use Marpa::Offset qw(DELETE_AND_NODE PRUNE_OR_NODE);

sub delete_nodes {
    my ( $evaler, $delete_work_list ) = @_;

    ### Executing delete_nodes ...

    # Should be deletion-consistent at this point
    ### assert: Marpa'Evaluator'audit($evaler) or 1

    my $and_nodes = $evaler->[Marpa::Internal::Evaluator::AND_NODES];
    my $or_nodes  = $evaler->[Marpa::Internal::Evaluator::OR_NODES];
    DELETE_WORK_ITEM:
    while ( my $delete_work_item = pop @{$delete_work_list} ) {
        my ( $action, $delete_node_id ) = @{$delete_work_item};

        if ( $action == DELETE_AND_NODE ) {

            ### Deleting and-node: $delete_node_id

            my $delete_and_node = $and_nodes->[$delete_node_id];

            next DELETE_WORK_ITEM
                if $delete_and_node->[Marpa::Internal::And_Node::DELETED];

            if ( not $delete_and_node->[Marpa::Internal::And_Node::DELETED] )
            {
                my $parent_id =
                    $delete_and_node->[Marpa::Internal::And_Node::PARENT_ID];
                my $parent_or_node = $or_nodes->[$parent_id];

                ### Adding or-node to delete work list: $parent_id
                push @{$delete_work_list}, [ PRUNE_OR_NODE, $parent_id ];
                my $parent_choice = $delete_and_node
                    ->[Marpa::Internal::And_Node::PARENT_CHOICE];

                ### Splicing out parent's child, id, choice: $parent_id, $parent_choice

                ### Before splice: $parent_or_node->[Marpa'Internal'Or_Node'CHILD_IDS]

                splice
                    @{ $parent_or_node->[Marpa::Internal::Or_Node::AND_NODES]
                    },
                    $parent_choice,
                    1;

                ### After splice: $parent_or_node->[Marpa'Internal'Or_Node'CHILD_IDS]

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

                    ### Renumbering choice in and-node, id, choice: $sibling_and_node_id, $choice
                } ## end for my $choice ( $parent_choice .. $#{...})

            } ## end if ( not $delete_and_node->[...])

            FIELD:
            for my $field (
                Marpa::Internal::And_Node::PREDECESSOR,
                Marpa::Internal::And_Node::CAUSE,
                )
            {
                my $child_or_node = $delete_and_node->[$field];
                next FIELD if not defined $child_or_node;
                next FIELD
                    if $child_or_node->[Marpa::Internal::Or_Node::DELETED];
                my $id = $child_or_node->[Marpa::Internal::Or_Node::ID];

                ### <where> child or-node id: $id

                ### Adding or-node to delete work list: $id
                push @{$delete_work_list}, [ PRUNE_OR_NODE, $id ];

                # Splice out the reference to this or-node in the PARENT_IDS
                # field of the or-node child
                my $parent_ids =
                    $child_or_node->[Marpa::Internal::Or_Node::PARENT_IDS];

                ### <where> parent ids: $parent_ids

                my $delete_node_index =
                    List::Util::first { $parent_ids->[$_] == $delete_node_id }
                ( 0 .. $#{$parent_ids} );

                ### delete_node_index: $delete_node_index;

                ### assert: defined $delete_node_index;

                splice @{$parent_ids}, $delete_node_index, 1;
            }    # FIELD

            FIELD:
            for my $field (
                Marpa::Internal::And_Node::PARENT_ID,
                Marpa::Internal::And_Node::PARENT_CHOICE,
                Marpa::Internal::And_Node::CAUSE,
                Marpa::Internal::And_Node::PREDECESSOR,
                Marpa::Internal::And_Node::VALUE_REF,
                )
            {
                $delete_and_node->[$field] = undef;
            } ## end for my $field ( Marpa::Internal::And_Node::PARENT_ID,...)

            $delete_and_node->[Marpa::Internal::And_Node::DELETED] = 1;

            next DELETE_WORK_ITEM;
        } ## end if ( $action == DELETE_AND_NODE )

        if ( $action == PRUNE_OR_NODE ) {

            ### Pruning or node: $delete_node_id

            my $or_node = $or_nodes->[$delete_node_id];
            next DELETE_WORK_ITEM
                if $or_node->[Marpa::Internal::Or_Node::DELETED];
            my $parent_ids = $or_node->[Marpa::Internal::Or_Node::PARENT_IDS];
            my $child_ids  = $or_node->[Marpa::Internal::Or_Node::CHILD_IDS];

            # Do not delete unless no children, or no parents and not the
            # start or-node.
            # Start or-node is always ID 0.

            ### Pruning or node, parent_ids: $parent_ids
            ### Pruning or node, child_ids: $child_ids

            next DELETE_WORK_ITEM
                if ( scalar @{$parent_ids} or $delete_node_id == 0 )
                and scalar @{$child_ids};

            ### Deleting or node: $delete_node_id

            $or_node->[Marpa::Internal::Or_Node::DELETED] = 1;

            ### Adding parent ids (and-nodes) to delete work list: $parent_ids
            ### Adding child ids (and-nodes) to delete work list: $child_ids

            push @{$delete_work_list},
                map { [ DELETE_AND_NODE, $_ ] } @{$parent_ids}, @{$child_ids};
            for my $field (
                Marpa::Internal::Or_Node::PARENT_IDS,
                Marpa::Internal::Or_Node::CHILD_IDS,
                Marpa::Internal::Or_Node::AND_NODES,
                )
            {
                $or_node->[$field] = [];
            } ## end for my $field ( Marpa::Internal::Or_Node::PARENT_IDS,...)

            ### Deleting or node, id, parent_ids: $delete_node_id, $or_node->[Marpa'Internal'Or_Node'PARENT_IDS]

            next DELETE_WORK_ITEM;
        } ## end if ( $action == PRUNE_OR_NODE )

        Marpa::exception("Unknown delete-work-list action: $action");
    } ## end while ( my $delete_work_item = pop @{$delete_work_list})
    return;
} ## end sub delete_nodes

## no critic (ControlStructures::ProhibitDeepNests)

# Rewrite to eliminate cycles.
sub rewrite_cycles {
    my ($evaler) = @_;

    my $or_nodes  = $evaler->[Marpa::Internal::Evaluator::OR_NODES];
    my $and_nodes = $evaler->[Marpa::Internal::Evaluator::AND_NODES];

    my $trace_fh;
    my $trace_evaluation;

    my $recce   = $evaler->[Marpa::Internal::Evaluator::RECOGNIZER];
    my $grammar = $recce->[Marpa::Internal::Recognizer::GRAMMAR];
    my $warn_on_cycle =
        $grammar->[Marpa::Internal::Grammar::CYCLE_ACTION] ne 'quiet';
    my $tracing = $warn_on_cycle
        || $grammar->[Marpa::Internal::Grammar::TRACING];
    if ($tracing) {
        $trace_fh = $grammar->[Marpa::Internal::Grammar::TRACE_FILE_HANDLE];
        $trace_evaluation =
            $grammar->[Marpa::Internal::Grammar::TRACE_EVALUATION];
    }

    # Grour or-nodes by span.  Only or-nodes with the same
    # span can be in a cycle.
    my %or_nodes_by_span;
    for my $or_node ( @{$or_nodes} ) {
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
    } ## end for my $or_node ( @{$or_nodes} )

    # Initialize the span sets
    my @span_sets = values %or_nodes_by_span;

    SPAN_SET: while ( my $span_set = pop @span_sets ) {
        @{$span_set} =
            grep { not $_->[Marpa::Internal::Or_Node::DELETED] } @{$span_set};
        next SPAN_SET if not @{$span_set};

        ### Processing Span Set, set left: scalar @span_sets

        my %in_span_set = ();
        for my $or_node_ix ( 0 .. $#{$span_set} ) {
            my $or_node_id =
                $span_set->[$or_node_ix]->[Marpa::Internal::Or_Node::ID];

            ### Span set or-node ix, id: $or_node_ix, $or_node_id

            $in_span_set{$or_node_id} = $or_node_ix;
        } ## end for my $or_node_ix ( 0 .. $#{$span_set} )

        # Set up matrix of or-node to or-node transitions.
        my @transition;
        my @work_list;
        for my $or_parent_ix ( 0 .. $#{$span_set} ) {
            my @or_child_ixes =
                grep { defined $_ }
                map  { $in_span_set{ $_->[Marpa::Internal::Or_Node::ID] } }
                grep { defined $_ }
                map {
                @{$_}[
                    Marpa::Internal::And_Node::CAUSE,
                    Marpa::Internal::And_Node::PREDECESSOR
                    ]
                } @{$and_nodes}[
                @{ $span_set->[$or_parent_ix]
                        ->[Marpa::Internal::Or_Node::CHILD_IDS] }
                ];
            for my $or_child_ix (@or_child_ixes) {
                ### initial transition: $or_parent_ix, $or_child_ix
                $transition[$or_parent_ix][$or_child_ix]++;
                push @work_list, [ $or_parent_ix, $or_child_ix ];
            }
        } ## end for my $or_parent_ix ( 0 .. $#{$span_set} )

        # Compute transitive closure of matrix of or-node transitions.
        while ( my $work_item = pop @work_list ) {
            my ( $parent_ix, $child_ix ) = @{$work_item};
            ### work item: $parent_ix, $child_ix
            GRAND_CHILD:
            for my $grandchild_ix ( grep { $transition[$child_ix][$_] }
                ( 0 .. $#{$span_set} ) )
            {
                my $transition_row = $transition[$parent_ix];
                next GRAND_CHILD if $transition_row->[$grandchild_ix];
                $transition_row->[$grandchild_ix]++;
                ### transition: $parent_ix, $grandchild_ix
                push @work_list, [ $parent_ix, $grandchild_ix ];
            } ## end for my $grandchild_ix ( grep { $transition[$child_ix]...})
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
            say {$trace_fh} 'Found cycle of length ', ( scalar @cycle );
            for my $ix ( 0 .. $#cycle ) {
                my $or_node = $cycle[$ix];
                print {$trace_fh} "Node $ix in cycle: ",
                    Marpa::Evaluator::show_or_node( $evaler, $or_node,
                    $trace_evaluation )
                    or Marpa::exception('print to trace handle failed');
            } ## end for my $ix ( 0 .. $#cycle )
        } ## end if ($trace_evaluation)

        # If we found any cycles in the span set, put the
        # whole span set back
        # on the work list for another pass
        push @span_sets, $span_set;

        # determine which in the original cycle set are
        # internal and-nodes
        my %internal_and_nodes = ();
        for my $or_node (@cycle) {
            for my $and_node_id (
                @{ $or_node->[Marpa::Internal::Or_Node::CHILD_IDS] } )
            {
                $internal_and_nodes{$and_node_id} = 1;
            }
        } ## end for my $or_node (@cycle)

        # determine which in the original span set are the
        # root or-nodes
        my @root_or_nodes = grep {
            defined List::Util::first { not defined $internal_and_nodes{$_} }
            @{ $_->[Marpa::Internal::Or_Node::PARENT_IDS] }
        } @cycle;

        ### cycle set size: scalar @cycle

        ### cycle set ids: join(';', map { $_->[Marpa'Internal'Or_Node'ID] } @cycle )

        ### Or-node 1 PARENT_IDS: join(';', @{ $or_nodes->[1]->[Marpa'Internal'Or_Node'PARENT_IDS] })

        ### internal and-node ids: join('; ', keys %internal_and_nodes )

        ### number of root or-nodes: scalar @root_or_nodes

        ### assert: scalar @root_or_nodes

        ## deletion-consistent at this point
        ### assert: Marpa'Evaluator'audit($evaler) or 1

        my @delete_work_list = ();

        ## now make the copies
        for my $copy ( 1 .. $#root_or_nodes ) {

            ### Making copy, copy number: $copy

            my $original_root_or_node = $root_or_nodes[$copy];
            my $original_root_or_node_id =
                $original_root_or_node->[Marpa::Internal::Or_Node::ID];

            ### Root or node id: $original_root_or_node_id

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
                my $new_or_node;
                $#{$new_or_node} = Marpa::Internal::Or_Node::LAST_FIELD;
                for my $field (
                    Marpa::Internal::Or_Node::IS_COMPLETED,
                    Marpa::Internal::Or_Node::START_EARLEME,
                    Marpa::Internal::Or_Node::END_EARLEME,
                    Marpa::Internal::Or_Node::TAG,
                    )
                {
                    $new_or_node->[$field] = $or_node->[$field];
                } ## end for my $field ( ...)

                my $new_or_node_id = @{$or_nodes};

                ### Creating new or-node: $new_or_node_id

                $new_or_node->[Marpa::Internal::Or_Node::TAG] =~ s{
                        [#] \d* \z
                    }{#$new_or_node_id}xms;

                $new_or_node->[Marpa::Internal::Or_Node::ID] =
                    $new_or_node_id;
                push @{$or_nodes}, $new_or_node;
                push @copied_cycle, $new_or_node;
                $translate_or_node_id{ $or_node
                        ->[Marpa::Internal::Or_Node::ID] } = $new_or_node_id;

                ### Or Node translation: $or_node->[Marpa'Internal'Or_Node'ID], $new_or_node_id

                my $child_ids =
                    $or_node->[Marpa::Internal::Or_Node::CHILD_IDS];
                for my $choice ( 0 .. $#{$child_ids} ) {
                    my $and_node_id  = $child_ids->[$choice];
                    my $and_node     = $and_nodes->[$and_node_id];
                    my $new_and_node = clone_and_node( $evaler, $and_node );
                    my $new_and_node_id =
                        $new_and_node->[Marpa::Internal::And_Node::ID];
                    push @{$and_nodes}, $new_and_node;
                    $translate_and_node_id{$and_node_id} = $new_and_node_id;

                    ### And Node translation: $and_node_id, $new_and_node_id

                    $new_or_node->[Marpa::Internal::Or_Node::AND_NODES]
                        ->[$choice] = $new_and_node;
                    $new_or_node->[Marpa::Internal::Or_Node::CHILD_IDS]
                        ->[$choice] = $new_and_node_id;
                    $new_and_node->[Marpa::Internal::And_Node::PARENT_ID] =
                        $new_or_node_id;
                    $new_and_node->[Marpa::Internal::And_Node::PARENT_CHOICE]
                        = $choice;
                } ## end for my $choice ( 0 .. $#{$child_ids} )

            } ## end for my $or_node (@cycle)

            # Translate the cycle-internal links
            # and duplicate the outgoing external links (which
            # will be from the and-nodes)

            for my $original_or_node (@cycle) {

                my $original_or_node_id =
                    $original_or_node->[Marpa::Internal::Or_Node::ID];
                my $new_or_node_id =
                    $translate_or_node_id{$original_or_node_id};
                my $new_or_node = $or_nodes->[$new_or_node_id];

                ### Translating links for or-node, original: $original_or_node_id
                ### Translating links for or-node, new: $new_or_node_id

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

                ### PARENT_IDS for original or-node: $original_or_node_id, $original_or_node->[Marpa'Internal'Or_Node'PARENT_IDS]
                ### PARENT_IDS for new or-node: $new_or_node_id, $new_or_node->[Marpa'Internal'Or_Node'PARENT_IDS]

                for my $original_and_node_id (
                    @{  $original_or_node
                            ->[Marpa::Internal::Or_Node::CHILD_IDS]
                    }
                    )
                {
                    my $original_and_node =
                        $and_nodes->[$original_and_node_id];
                    my $new_and_node_id =
                        $translate_and_node_id{$original_and_node_id};
                    my $new_and_node = $and_nodes->[$new_and_node_id];

                    ### Translating links for and-node, original: $original_and_node_id
                    ### Translating links for and-node, new: $new_and_node_id

                    FIELD:
                    for my $field (
                        Marpa::Internal::And_Node::CAUSE,
                        Marpa::Internal::And_Node::PREDECESSOR
                        )
                    {
                        my $original_or_child = $original_and_node->[$field];
                        next FIELD if not defined $original_or_child;
                        my $original_or_child_id = $original_or_child
                            ->[Marpa::Internal::Or_Node::ID];
                        my $new_or_child_id =
                            $translate_or_node_id{$original_or_child_id};

                        my $new_or_child;
                        if ( defined $new_or_child_id ) {

                            $new_or_child = $or_nodes->[$new_or_child_id];
                            $new_and_node->[$field] = $new_or_child;

                            ### Changing field, and-node, field: $original_and_node_id, $field
                            ### From or-node, to or-node: $original_or_child_id, $new_or_child_id

                            next FIELD;

                        } ## end if ( defined $new_or_child_id )

                        # If here, the or-child is external.

                        $new_or_child = $new_and_node->[$field] =
                            $original_or_child;

                        ### Pushing additional parent id, or-node: $new_and_node_id, $new_or_child_id

                        ### assert: not grep { $_ == $new_and_node_id } @{ $new_or_child ->[Marpa'Internal'Or_Node'PARENT_IDS] }

                        # Since the or-child is external,
                        # we need to duplicate the link.
                        push @{ $new_or_child
                                ->[Marpa::Internal::Or_Node::PARENT_IDS] },
                            $new_and_node_id;

                    } ## end for my $field ( Marpa::Internal::And_Node::CAUSE, ...)
                } ## end for my $original_and_node_id ( @{ $original_or_node...})

            } ## end for my $original_or_node (@cycle)

            # It remains now to duplicate the external links to the cycle
            # and to mark internal links to the root node for deletion.
            # External links are allowed only to the root node of the cycle.

            my $new_root_or_node_id =
                $translate_or_node_id{ $original_root_or_node
                    ->[Marpa::Internal::Or_Node::ID] };

            ### Old root or node id: $original_root_or_node->[Marpa'Internal'Or_Node'ID]
            ### New root or node id: $new_root_or_node_id

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

                    ### Adding and-node to delete work list: $new_parent_and_node_id
                    push @delete_work_list,
                        [ DELETE_AND_NODE, $new_parent_and_node_id ];
                    next PARENT_AND_NODE;

                } ## end if ( defined( my $new_parent_and_node_id = ...))

                # If we are here, the parent node is cycle-external.

                # Clone the external parent node
                my $original_parent_and_node =
                    $and_nodes->[$original_parent_and_node_id];
                my $new_parent_and_node =
                    clone_and_node( $evaler, $original_parent_and_node );
                my $new_parent_and_node_id =
                    $new_parent_and_node->[Marpa::Internal::And_Node::ID];

                ### Cloned root parent and-node, old: $original_parent_and_node_id
                ### Cloned root parent and-node, new: $new_parent_and_node_id

                # Now tell the cloned and-node about its children, one
                # of which is the new root or-node
                FIELD:
                for my $field (
                    Marpa::Internal::And_Node::CAUSE,
                    Marpa::Internal::And_Node::PREDECESSOR
                    )
                {
                    my $original_root_or_node_sibling =
                        $original_parent_and_node->[$field];
                    next FIELD if not defined $original_root_or_node_sibling;

                    # If this field was the root or node in the old
                    # parent and-node, make it the case that the
                    # new root or-node is this same field in the
                    # new parent and-node.
                    # Uses a referent address comparison.
                    my $new_root_or_node_sibling;
                    if ( $original_root_or_node_sibling
                        == $original_root_or_node )
                    {
                        $new_root_or_node_sibling =
                            $new_parent_and_node->[$field] =
                            $new_root_or_node;

                        ### Field from or-node: $original_root_or_node_sibling->[Marpa'Internal'Or_Node'ID]
                        ### Field to or-node: $new_root_or_node->[Marpa'Internal'Or_Node'ID]

                    } ## end if ( $original_root_or_node_sibling == ...)
                    else {
                        $new_root_or_node_sibling =
                            $new_parent_and_node->[$field] =
                            $original_root_or_node_sibling;
                    }

                    ### Pushing additional parent id, or-node: $new_parent_and_node_id, $new_root_or_node_sibling->[Marpa'Internal'Or_Node'ID]

                    ### assert: not grep { $_ == $new_parent_and_node_id } @{ $new_root_or_node_sibling->[Marpa'Internal'Or_Node'PARENT_IDS] }

                    push @{ $new_root_or_node_sibling
                            ->[Marpa::Internal::Or_Node::PARENT_IDS] },
                        $new_parent_and_node_id;

                    # We assume that a field is either
                    # a clone of the root, or cycle-external.
                    # We can do this because:
                    #
                    #   1. All or-nodes in a cycle must have the same
                    #      span.
                    #   2. For both children of an and-node to have
                    #      the same span, both must have a zero-width
                    #      span.
                    #   3. If more than one zero-width span occurred
                    #      in an and-node,
                    #      the parent or-node and and-node would have
                    #      zero-width as well.
                    #   4. Zero-width and-nodes do not have children,
                    #      because of Marpa's assignment of constant
                    #      "null values" to null symbols.
                } ## end for my $field ( Marpa::Internal::And_Node::CAUSE, ...)

                # Tell the parent of the newly cloned and-node
                # about its new child
                my $grandparent_or_node_id = $original_parent_and_node
                    ->[Marpa::Internal::And_Node::PARENT_ID];
                my $grandparent_or_node =
                    $or_nodes->[$grandparent_or_node_id];
                my $child_ids_of_grandparent = $grandparent_or_node
                    ->[Marpa::Internal::Or_Node::CHILD_IDS];
                my $choice = @{$child_ids_of_grandparent};
                push @{$child_ids_of_grandparent}, $new_parent_and_node_id;
                push @{ $grandparent_or_node
                        ->[Marpa::Internal::Or_Node::AND_NODES] },
                    $new_parent_and_node;

                # Tell the new cloned and-node about its parent
                $new_parent_and_node->[Marpa::Internal::And_Node::PARENT_ID] =
                    $grandparent_or_node_id;
                $new_parent_and_node
                    ->[Marpa::Internal::And_Node::PARENT_CHOICE] = $choice;

            } ## end for my $original_parent_and_node_id ( @{ ...})

            push @span_sets, \@copied_cycle;

            # Should be deletion-consistent at this point
            ### assert: Marpa'Evaluator'audit($evaler) or 1

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
                ### Is root node?: $is_root
                ### And-node to delete: $original_parent_and_node_id

                next PARENT_AND_NODE
                    if $is_root
                        xor $internal_and_nodes{$original_parent_and_node_id};

                ### Adding and-node to delete work list: $original_parent_and_node_id

                push @delete_work_list,
                    [ DELETE_AND_NODE, $original_parent_and_node_id ];
            } ## end for my $original_parent_and_node_id ( @{ ...})
        } ## end for my $original_or_node (@cycle)

        # we should be deletion-consistent at this point

        # Now actually do the deletions
        delete_nodes( $evaler, \@delete_work_list );

        ### <where> After call to delete nodes ...

        # Should be deletion-consistent at this point
        ### assert: Marpa'Evaluator'audit($evaler) or 1

        # Have we deleted the top or-node?
        # If so, there will be no parses.
        if ( $or_nodes->[0]->[Marpa::Internal::Or_Node::DELETED] ) {
            if ($warn_on_cycle) {
                print {$trace_fh} "Cycles found, but no parses\n"
                    or Marpa::exception('print to trace handle failed');
            }
            return;
        } ## end if ( $or_nodes->[0]->[Marpa::Internal::Or_Node::DELETED...])

    } ## end while ( my $span_set = pop @span_sets )

    ### Bocage: &Marpa'Evaluator'show_bocage($evaler, 3) or 1

    ### assert: Marpa'Evaluator'audit($evaler) or 1

    return;
} ## end sub rewrite_cycles

# Returns false if no parse
sub Marpa::Evaluator::new {
    my $class = shift;
    my $args  = shift;

    my $self = bless [], $class;

    ### Constructing new evaluator

    my $recce;
    RECCE_ARG_NAME: for my $recce_arg_name (qw(recognizer recce)) {
        my $arg_value = $args->{$recce_arg_name};
        delete $args->{$recce_arg_name};
        next RECCE_ARG_NAME if not defined $arg_value;
        Marpa::exception('recognizer specified twice') if defined $recce;
        $recce = $arg_value;
    } ## end for my $recce_arg_name (qw(recognizer recce))
    Marpa::exception('No recognizer specified') if not defined $recce;

    my $recce_class = ref $recce;
    Marpa::exception(
        "${class}::new() recognizer arg has wrong class: $recce_class")
        if $recce_class ne 'Marpa::Recognizer';

    my $parse_set_arg = $args->{end};
    delete $args->{end};

    my $clone_arg = $args->{clone};
    delete $args->{clone};
    my $clone = $clone_arg // 1;

    if ($clone) {
        $recce = $recce->clone();
    }

    my ( $grammar, $earley_sets, ) = @{$recce}[
        Marpa::Internal::Recognizer::GRAMMAR,
        Marpa::Internal::Recognizer::EARLEY_SETS,
    ];

    my $phase = $grammar->[Marpa::Internal::Grammar::PHASE];

    # Marpa::exception('Recognizer already in use by Evaluator')
    # if $phase == Marpa::Internal::Phase::EVALUATING;
    Marpa::exception(
        'Attempt to evaluate grammar in wrong phase: ',
        Marpa::Internal::Phase::description($phase)
    ) if $phase < Marpa::Internal::Phase::RECOGNIZED;

    $self->[Marpa::Internal::Evaluator::RECOGNIZER] = $recce;

    $self->set($args);

    $grammar->[Marpa::Internal::Grammar::PHASE] =
        Marpa::Internal::Phase::EVALUATING;

    my $tracing = $grammar->[Marpa::Internal::Grammar::TRACING];

    my $trace_fh;
    my $trace_iterations;
    my $trace_evaluation;

    if ($tracing) {
        $trace_fh = $grammar->[Marpa::Internal::Grammar::TRACE_FILE_HANDLE];
        $trace_evaluation =
            $grammar->[Marpa::Internal::Grammar::TRACE_EVALUATION];
        $trace_iterations =
            $grammar->[Marpa::Internal::Grammar::TRACE_ITERATIONS];
    } ## end if ($tracing)

    $self->[Marpa::Internal::Evaluator::PARSE_COUNT] = 0;
    my $or_nodes  = $self->[Marpa::Internal::Evaluator::OR_NODES]  = [];
    my $and_nodes = $self->[Marpa::Internal::Evaluator::AND_NODES] = [];
    $self->[Marpa::Internal::Evaluator::CYCLES] = {};

    my $current_parse_set = $parse_set_arg
        // $recce->[Marpa::Internal::Recognizer::CURRENT_SET];

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

    state $parse_number = 0;
    my $package = $self->[Marpa::Internal::Evaluator::PACKAGE] =
        sprintf 'Marpa::E_%x', $parse_number++;
    run_preamble( $grammar, $package );
    my $null_values = $self->[Marpa::Internal::Evaluator::NULL_VALUES] =
        set_null_values( $grammar, $package );
    my $rule_data = $self->[Marpa::Internal::Evaluator::RULE_DATA] =
        set_actions( $grammar, $package );

    my $start_symbol = $start_rule->[Marpa::Internal::Rule::LHS];
    my ( $nulling, $symbol_id ) =
        @{$start_symbol}[ Marpa::Internal::Symbol::NULLING,
        Marpa::Internal::Symbol::ID, ];
    my $start_null_value = $null_values->[$symbol_id];

    # deal with a null parse as a special case
    if ($nulling) {

        my $closure =
            $rule_data->[ $start_rule->[Marpa::Internal::Rule::ID] ]
            ->[Marpa::Internal::Evaluator_Rule::PERL_CLOSURE];

        my $or_node = [];
        $#{$or_node} = Marpa::Internal::Or_Node::LAST_FIELD;

        my $and_node = [];
        $#{$and_node} = Marpa::Internal::And_Node::LAST_FIELD;

        $or_node->[Marpa::Internal::Or_Node::AND_NODES]     = [$and_node];
        $or_node->[Marpa::Internal::Or_Node::CHILD_IDS]     = [0];
        $or_node->[Marpa::Internal::Or_Node::START_EARLEME] = 0;
        $or_node->[Marpa::Internal::Or_Node::END_EARLEME]   = 0;
        $or_node->[Marpa::Internal::Or_Node::IS_COMPLETED]  = 1;

        $and_node->[Marpa::Internal::And_Node::VALUE_REF] =
            \$start_null_value;
        $and_node->[Marpa::Internal::And_Node::PERL_CLOSURE] = $closure;
        $and_node->[Marpa::Internal::And_Node::ARGC] =
            scalar @{ $start_rule->[Marpa::Internal::Rule::RHS] };
        $and_node->[Marpa::Internal::And_Node::RULE]        = $start_rule;
        $and_node->[Marpa::Internal::And_Node::POSITION]    = 0;
        $and_node->[Marpa::Internal::And_Node::END_EARLEME] = 0;
        my $id = $and_node->[Marpa::Internal::And_Node::ID] = 0;
        my $or_node_tag = $or_node->[Marpa::Internal::Or_Node::TAG] =
            $start_item->[Marpa::Internal::Earley_Item::NAME] . q{#} . $id;
        $and_node->[Marpa::Internal::And_Node::TAG] = $or_node_tag . '[0]#0';

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

    my $i = 0;
    OR_SAPLING: while (1) {

        my ( $sapling_name, $item, $child_lhs_symbol, $rule, $position ) =
            @{ $or_saplings[ $i++ ] }[
            Marpa::Internal::Or_Sapling::NAME,
            Marpa::Internal::Or_Sapling::ITEM,
            Marpa::Internal::Or_Sapling::CHILD_LHS_SYMBOL,
            Marpa::Internal::Or_Sapling::RULE,
            Marpa::Internal::Or_Sapling::POSITION,
            ];

        last OR_SAPLING if not defined $item;

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
                my $closure =
                    $rule_data->[ $rule->[Marpa::Internal::Rule::ID] ]
                    ->[Marpa::Internal::Evaluator_Rule::PERL_CLOSURE];

                my $last_position = @{$rhs} - 1;
                push @and_saplings,
                    [
                    $rule,                  $last_position,
                    $rhs->[$last_position], $closure
                    ];

            }    # for my $rule

        }    # closure or-node

        my $start_earleme = $item->[Marpa::Internal::Earley_Item::PARENT];
        my $end_earleme   = $item->[Marpa::Internal::Earley_Item::SET];

        my @child_and_nodes;

        my $item_name = $item->[Marpa::Internal::Earley_Item::NAME];

        for my $and_sapling (@and_saplings) {

            my ( $sapling_rule, $sapling_position, $symbol, $closure ) =
                @{$and_sapling};

            my ( $rule_id, $rhs ) =
                @{$sapling_rule}[ Marpa::Internal::Rule::ID,
                Marpa::Internal::Rule::RHS ];
            my $rule_length = @{$rhs};

            my @or_bud_list;
            if ( $symbol->[Marpa::Internal::Symbol::NULLING] ) {
                my $nulling_symbol_id =
                    $symbol->[Marpa::Internal::Symbol::ID];
                my $null_value = $null_values->[$nulling_symbol_id];
                @or_bud_list = ( [ $item, undef, \$null_value, ] );
            } ## end if ( $symbol->[Marpa::Internal::Symbol::NULLING] )
            else {
                @or_bud_list = (
                    (   map { [ $_->[0], undef, \( $_->[1] ) ] }
                            @{ $item->[Marpa::Internal::Earley_Item::TOKENS] }
                    ),
                    (   map { [ $_->[0], $_->[1] ] }
                            @{ $item->[Marpa::Internal::Earley_Item::LINKS] }
                    )
                );
            } ## end else [ if ( $symbol->[Marpa::Internal::Symbol::NULLING] ) ]

            for my $or_bud (@or_bud_list) {

                my ( $predecessor, $cause, $value_ref ) = @{$or_bud};

                my $predecessor_name;

                if ( $sapling_position > 0 ) {

                    $predecessor_name =
                        $predecessor->[Marpa::Internal::Earley_Item::NAME]
                        . "R$rule_id:$sapling_position";

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

                $and_node->[Marpa::Internal::And_Node::PREDECESSOR] =
                    $predecessor_name;
                $and_node->[Marpa::Internal::And_Node::CAUSE] = $cause_name;
                $and_node->[Marpa::Internal::And_Node::VALUE_REF] =
                    $value_ref;
                $and_node->[Marpa::Internal::And_Node::PERL_CLOSURE] =
                    $closure;
                $and_node->[Marpa::Internal::And_Node::ARGC] = $rule_length;
                $and_node->[Marpa::Internal::And_Node::RULE] = $sapling_rule;
                $and_node->[Marpa::Internal::And_Node::POSITION] =
                    $sapling_position;
                $and_node->[Marpa::Internal::And_Node::END_EARLEME] =
                    $end_earleme;
                my $id = $and_node->[Marpa::Internal::And_Node::ID] =
                    @{$and_nodes};
                push @{$and_nodes}, $and_node;

                push @child_and_nodes, $and_node;

            }    # for my $or_bud

        }    # for my $and_sapling

        my $or_node = [];
        $#{$or_node} = Marpa::Internal::Or_Node::LAST_FIELD;
        my $or_node_id = $or_node->[Marpa::Internal::Or_Node::ID] =
            @{$or_nodes};
        my $or_node_tag = $or_node->[Marpa::Internal::Or_Node::TAG] =
            $sapling_name . q{#} . $or_node_id;
        $or_node->[Marpa::Internal::Or_Node::AND_NODES] = \@child_and_nodes;
        $or_node->[Marpa::Internal::Or_Node::CHILD_IDS] =
            [ map { $_->[Marpa::Internal::And_Node::ID] } @child_and_nodes ];
        for my $and_node_choice ( 0 .. $#child_and_nodes ) {
            my $and_node    = $child_and_nodes[$and_node_choice];
            my $and_node_id = $and_node->[Marpa::Internal::And_Node::ID];
            $and_node->[Marpa::Internal::And_Node::TAG] =
                  $or_node_tag . '['
                . $and_node_choice . ']' . q{#}
                . $and_node_id;
            $and_node->[Marpa::Internal::And_Node::PARENT_ID] = $or_node_id;
            $and_node->[Marpa::Internal::And_Node::PARENT_CHOICE] =
                $and_node_choice;
        } ## end for my $and_node_choice ( 0 .. $#child_and_nodes )
        $or_node->[Marpa::Internal::Or_Node::IS_COMPLETED] =
            not $is_kernel_or_node;
        $or_node->[Marpa::Internal::Or_Node::START_EARLEME] = $start_earleme;
        $or_node->[Marpa::Internal::Or_Node::END_EARLEME]   = $end_earleme;
        $or_node->[Marpa::Internal::Or_Node::PARENT_IDS]    = [];
        push @{$or_nodes}, $or_node;
        $or_node_by_name{$sapling_name} = $or_node;

    }    # OR_SAPLING

    my $and_node_counter = 0;

    # resolve links in the bocage
    for my $parent_or_node ( @{$or_nodes} ) {
        my $parent_or_node_id =
            $parent_or_node->[Marpa::Internal::Or_Node::ID];

        ## no critic (BuiltinFunctions::ProhibitReverseSortBlock)
        my @child_and_node_ids =
            map  { $_->[0] }
            sort { $b->[1] <=> $a->[1] }
            map {
            [   $_,
                $and_nodes->[$_]->[Marpa::Internal::And_Node::RULE]
                    ->[Marpa::Internal::Rule::INTERNAL_PRIORITY] // 0
            ]
            } @{ $parent_or_node->[Marpa::Internal::Or_Node::CHILD_IDS] };
        ## use critic

        for my $choice ( 0 .. $#child_and_node_ids ) {
            my $and_node_id = $child_and_node_ids[$choice];
            my $and_node    = $and_nodes->[$and_node_id];

            FIELD:
            for my $field (
                Marpa::Internal::And_Node::PREDECESSOR,
                Marpa::Internal::And_Node::CAUSE,
                )
            {
                my $name = $and_node->[$field];
                next FIELD if not defined $name;
                my $child_or_node = $or_node_by_name{$name};
                $and_node->[$field] = $child_or_node;
                push @{ $child_or_node->[Marpa::Internal::Or_Node::PARENT_IDS]
                    },
                    $and_node_id;
            } ## end for my $field ( Marpa::Internal::And_Node::PREDECESSOR...)

        } ## end for my $choice ( 0 .. $#child_and_node_ids )
    } ## end for my $parent_or_node ( @{$or_nodes} )

    ### Bocage: &Marpa'Evaluator'show_bocage($self, 3)

    # TODO: Add code to only attempt rewrite if grammar is cyclical
    rewrite_cycles($self);

    # There can be duplicate and-nodes.  Prune these.
    my @short_and_signatures;
    $#short_and_signatures = @{$and_nodes};
    my @or_node_id_work_list         = ();
    my @and_node_id_work_list        = ();
    my @prune_candidate_and_node_ids = ();

    OR_NODE: for my $parent_or_node ( @{$or_nodes} ) {
        my $parent_or_node_id =
            $parent_or_node->[Marpa::Internal::Or_Node::ID];
        my $child_and_node_ids =
            $parent_or_node->[Marpa::Internal::Or_Node::CHILD_IDS];
        next OR_NODE if scalar @{$child_and_node_ids} < 2;
        my %short_signature;
        for my $child_and_node_id ( @{$child_and_node_ids} ) {
            my $and_node            = $and_nodes->[$child_and_node_id];
            my $short_and_signature = join q{,},
                $and_node->[Marpa::Internal::And_Node::RULE] + 0,
                $and_node->[Marpa::Internal::And_Node::POSITION] + 0,
                $and_node->[Marpa::Internal::And_Node::END_EARLEME] + 0,
                (
                defined $and_node->[Marpa::Internal::And_Node::PREDECESSOR]
                ? 1
                : 0
                ),
                (
                defined $and_node->[Marpa::Internal::And_Node::CAUSE] ? 1
                : 0
                ),
                ( $and_node->[Marpa::Internal::And_Node::VALUE_REF] // 0 )
                + 0,
                ;
            $short_and_signatures[$child_and_node_id] = $short_and_signature;
            push @{ $short_signature{$short_and_signature} },
                $child_and_node_id;
        } ## end for my $child_and_node_id ( @{$child_and_node_ids} )

        SHORT_SIGNATURE:
        while ( my ( $short_signature, $child_ids ) = each %short_signature )
        {
            next SHORT_SIGNATURE if scalar @{$child_ids} < 2;
            push @prune_candidate_and_node_ids, $child_ids;
            if ($trace_iterations) {
                say {$trace_fh} 'Possible duplicate and nodes: ', join q{ },
                    map { $and_nodes->[$_]->[Marpa::Internal::And_Node::TAG] }
                    @{$child_ids};
            }

            # Look for the ids which we will want will want to test
            # for identity, so we can set up equivalence classes.
            FIELD:
            for my $field (
                Marpa::Internal::And_Node::PREDECESSOR,
                Marpa::Internal::And_Node::CAUSE,
                )
            {
                my @or_nodes = grep { defined $_ }
                    map { $and_nodes->[$_]->[$field] } @{$child_ids};
                next FIELD if scalar @or_nodes < 2;
                my @or_node_ids =
                    map { $_->[Marpa::Internal::Or_Node::ID] } @or_nodes;
                my $first = $or_node_ids[0];
                my $first_different =
                    List::Util::first { $_ != $first }
                @or_node_ids[ 1 .. $#or_node_ids ];
                next FIELD if not defined $first_different;
                push @or_node_id_work_list, @or_node_ids;
            } ## end for my $field ( Marpa::Internal::And_Node::PREDECESSOR...)

        } ## end while ( my ( $short_signature, $child_ids ) = each ...)

    } ## end for my $parent_or_node ( @{$or_nodes} )

    # Determine the or-nodes and and-nodes which are of interest for
    # finding duplicate and-nodes.
    my @or_nodes_of_interest;
    $#or_nodes_of_interest = $#{$or_nodes};
    my @and_nodes_of_interest;
    $#and_nodes_of_interest = $#{$and_nodes};

    while (@or_node_id_work_list) {
        OR_NODE: while ( my $or_node_id = pop @or_node_id_work_list ) {
            next OR_NODE if defined $or_nodes_of_interest[$or_node_id];
            $or_nodes_of_interest[$or_node_id] = 1;
            push @and_node_id_work_list,
                @{ $or_nodes->[$or_node_id]
                    ->[Marpa::Internal::Or_Node::CHILD_IDS] };
        } ## end while ( my $or_node_id = pop @or_node_id_work_list )
        AND_NODE: while ( my $and_node_id = pop @and_node_id_work_list ) {
            next AND_NODE if defined $and_nodes_of_interest[$and_node_id];
            $and_nodes_of_interest[$and_node_id] = 1;
            push @or_node_id_work_list,
                map  { $_->[Marpa::Internal::Or_Node::ID] }
                grep { defined $_ } @{ $and_nodes->[$and_node_id] }[
                Marpa::Internal::And_Node::PREDECESSOR,
                Marpa::Internal::And_Node::CAUSE
                ];
        } ## end while ( my $and_node_id = pop @and_node_id_work_list )
    } ## end while (@or_node_id_work_list)

    my @equivalence_work_list = ();

    # Initialize the equivalence work list with terminal and nodes
    AND_NODE:
    for my $and_node_id ( grep { $and_nodes_of_interest[$_] }
        ( 0 .. $#and_nodes_of_interest ) )
    {
        my @or_children =
            grep { defined $_ } @{ $and_nodes->[$and_node_id] }[
            Marpa::Internal::And_Node::PREDECESSOR,
            Marpa::Internal::And_Node::CAUSE
            ];
        next AND_NODE if @or_children;
        push @equivalence_work_list, [ 1, $and_node_id ];
    } ## end for my $and_node_id ( grep { $and_nodes_of_interest[$_...]})

    my @and_node_equivalence_class;
    my %and_node_equivalence_class;
    my @or_node_equivalence_class;
    my %or_node_equivalence_class;
    EQUIVALENCE_WORK_ITEM:
    while ( my $equivalence_work_item = pop @equivalence_work_list ) {
        my $is_and_node = shift @{$equivalence_work_item};
        if ($is_and_node) {
            my $and_node_id    = shift @{$equivalence_work_item};
            my $and_node       = $and_nodes->[$and_node_id];
            my @child_or_nodes = @{$and_node}[
                Marpa::Internal::And_Node::PREDECESSOR,
                Marpa::Internal::And_Node::CAUSE
            ];
            my @child_or_node_ids =
                map {
                defined $_
                    ? $_->[Marpa::Internal::Or_Node::ID]
                    : undef
                } @child_or_nodes;
            my @child_equivalence_classes =
                map { defined $_ ? $or_node_equivalence_class[$_] : 'undef' }
                @child_or_node_ids;
            for my $class (@child_equivalence_classes) {
                next EQUIVALENCE_WORK_ITEM if not defined $class;
            }
            my $signature = join q{,},
                $and_node->[Marpa::Internal::And_Node::RULE] + 0,
                $and_node->[Marpa::Internal::And_Node::POSITION] + 0,
                $and_node->[Marpa::Internal::And_Node::END_EARLEME] + 0,
                @child_equivalence_classes,
                ( $and_node->[Marpa::Internal::And_Node::VALUE_REF] // 0 )
                + 0,
                ;
            my $equivalence_class = $and_node_equivalence_class{$signature};

            if ( not defined $equivalence_class ) {
                $equivalence_class = $and_node_equivalence_class{$signature} =
                    $and_node->[Marpa::Internal::And_Node::TAG];
            }
            $and_node_equivalence_class[$and_node_id] = $equivalence_class;
            if ( $or_nodes_of_interest[Marpa::Internal::And_Node::PARENT_ID]
                and not defined $or_node_equivalence_class
                [Marpa::Internal::And_Node::PARENT_ID] )
            {
                push @equivalence_work_list,
                    [ 0, Marpa::Internal::And_Node::PARENT_ID ];
            } ## end if ( $or_nodes_of_interest[...])
        } ## end if ($is_and_node)
        else {
            my $or_node_id = shift @{$equivalence_work_item};
            my $or_node    = $or_nodes->[$or_node_id];
            my @child_and_node_ids =
                @{ $or_node->[Marpa::Internal::Or_Node::CHILD_IDS] };
            my @child_equivalence_classes =
                map { defined $_ ? $and_node_equivalence_class[$_] : 'undef' }
                @child_and_node_ids;
            for my $class (@child_equivalence_classes) {
                next EQUIVALENCE_WORK_ITEM if not defined $class;
            }
            my $signature = join q{,}, @child_equivalence_classes;
            my $equivalence_class = $or_node_equivalence_class{$signature};

            if ( not defined $equivalence_class ) {
                $equivalence_class = $or_node_equivalence_class{$signature} =
                    $or_node->[Marpa::Internal::And_Node::TAG];
            }
            $or_node_equivalence_class[$or_node_id] = $equivalence_class;
            push @equivalence_work_list, map { [ 1, $_ ] }
                grep {
                $and_nodes_of_interest[$_]
                    and not defined $and_node_equivalence_class[$_]
                } @{ $or_node->[Marpa::Internal::Or_Node::PARENT_IDS] };
        } ## end else [ if ($is_and_node) ]
    } ## end while ( my $equivalence_work_item = pop @equivalence_work_list)

    for my $prune_candidate_set (@prune_candidate_and_node_ids) {
        my %seen;
        AND_NODE_ID: for my $and_node_id ( @{$prune_candidate_set} ) {
            my $equivalence_class = $and_node_equivalence_class[$and_node_id];
            next AND_NODE_ID if not defined $equivalence_class;
            if ( $seen{$equivalence_class} ) {
                $and_nodes->[$and_node_id]
                    ->[Marpa::Internal::And_Node::DELETED] = 1;
                if ($trace_iterations) {
                    say {$trace_fh} 'Pruning duplicate and node: ',
                        $and_nodes->[$_]->[Marpa::Internal::And_Node::TAG];
                }
            } ## end if ( $seen{$equivalence_class} )
            $seen{$equivalence_class}++;
        } ## end for my $and_node_id ( @{$prune_candidate_set} )
    } ## end for my $prune_candidate_set (@prune_candidate_and_node_ids)

    return $self;

}    # sub new

## use critic

sub Marpa::show_and_node {
    my ( $and_node, $verbose ) = @_;
    $verbose //= 0;

    return q{} if $and_node->[Marpa::Internal::And_Node::DELETED];

    my $return_value = q{};

    my ( $name, $predecessor, $cause, $value_ref, $closure, $argc, $rule,
        $position, )
        = @{$and_node}[
        Marpa::Internal::And_Node::TAG,
        Marpa::Internal::And_Node::PREDECESSOR,
        Marpa::Internal::And_Node::CAUSE,
        Marpa::Internal::And_Node::VALUE_REF,
        Marpa::Internal::And_Node::PERL_CLOSURE,
        Marpa::Internal::And_Node::ARGC,
        Marpa::Internal::And_Node::RULE,
        Marpa::Internal::And_Node::POSITION,
        ];

    my @rhs = ();

    if ($predecessor) {
        push @rhs, $predecessor->[Marpa::Internal::Or_Node::TAG];
    }    # predecessor

    if ($cause) {
        push @rhs, $cause->[Marpa::Internal::Or_Node::TAG];
    }    # cause

    if ( defined $value_ref ) {
        my $value_as_string =
            Data::Dumper->new( [ ${$value_ref} ] )->Terse(1)->Dump;
        chomp $value_as_string;
        push @rhs, $value_as_string;
    }    # value

    $return_value .= "$name ::= " . join( q{ }, @rhs ) . "\n";

    if ($verbose) {
        $return_value
            .= '    rule '
            . $rule->[Marpa::Internal::Rule::ID] . ': '
            . Marpa::brief_virtual_rule( $rule, $position + 1 ) . "\n";
    } ## end if ($verbose)

    if ( $verbose >= 2 ) {
        $return_value .= "    rhs length = $argc";
        if ( defined $closure ) {
            $return_value .= '; closure';
        }
        $return_value .= "\n";
    } ## end if ( $verbose >= 2 )

    return $return_value;

} ## end sub Marpa::show_and_node

sub Marpa::Evaluator::show_decisions {
    my ( $evaler, $verbose ) = @_;
    $verbose //= 0;
    my $return_value = q{};

    my $instances = $evaler->[Marpa::Internal::Evaluator::INSTANCES];
    my $decisions = $evaler->[Marpa::Internal::Evaluator::DECISIONS];
    my $and_nodes = $evaler->[Marpa::Internal::Evaluator::AND_NODES];

    for my $rank ( 0 .. $#{$instances} ) {
        my $and_node_ids = $instances->[$rank];
        for my $choice ( 0 .. $#{$and_node_ids} ) {
            my $and_node_id = $and_node_ids->[$choice];
            my $decision    = $decisions->[$and_node_id];
            my $and_node    = $and_nodes->[$and_node_id];
            $return_value
                .= "$rank.$choice: "
                . Marpa::show_decision($decision) . q{ }
                . Marpa::show_and_node( $and_node, $verbose );
        } ## end for my $choice ( 0 .. $#{$and_node_ids} )
    } ## end for my $rank ( 0 .. $#{$instances} )

    return $return_value;

} ## end sub Marpa::Evaluator::show_decisions

sub Marpa::Evaluator::show_or_node {
    my ( $evaler, $or_node, $verbose ) = @_;
    $verbose //= 0;

    return q{} if $or_node->[Marpa::Internal::Or_Node::DELETED];

    my $and_nodes = $evaler->[Marpa::Internal::Evaluator::AND_NODES];

    my $text = q{};

    my $or_node_tag  = $or_node->[Marpa::Internal::Or_Node::TAG];
    my $and_node_ids = $or_node->[Marpa::Internal::Or_Node::CHILD_IDS];

    for my $index ( 0 .. $#{$and_node_ids} ) {
        my $and_node_id = $and_node_ids->[$index];
        my $and_node    = $and_nodes->[$and_node_id];

        my $and_node_tag = $or_node_tag . '[' . $index . ']#' . $and_node_id;
        if ( $verbose >= 2 ) {
            $text .= "$or_node_tag ::= $and_node_tag\n";
        }

        $text .= Marpa::show_and_node( $and_node, $verbose );

    } ## end for my $index ( 0 .. $#{$and_node_ids} )

    return $text;

} ## end sub Marpa::Evaluator::show_or_node

sub Marpa::Evaluator::show_bocage {
    my ( $evaler, $verbose ) = @_;
    $verbose //= 0;

    my $parse_count = $evaler->[Marpa::Internal::Evaluator::PARSE_COUNT];
    my $or_nodes    = $evaler->[Marpa::Internal::Evaluator::OR_NODES];
    my $package     = $evaler->[Marpa::Internal::Evaluator::PACKAGE];

    my $text =
        'package: ' . $package . '; parse count: ' . $parse_count . "\n";

    for my $or_node ( @{$or_nodes} ) {

        $text
            .= Marpa::Evaluator::show_or_node( $evaler, $or_node, $verbose );

    } ## end for my $or_node ( @{$or_nodes} )

    return $text;
} ## end sub Marpa::Evaluator::show_bocage

sub Marpa::Evaluator::show_tree {
    my ( $evaler, $verbose ) = @_;

    my $tree = $evaler->[Marpa::Internal::Evaluator::TREE];

    my $text = q{};

    my $tree_position = 0;
    for my $tree_node ( @{$tree} ) {

        my ($or_node, $choice,   $predecessor, $cause,
            $depth,   $closure,  $argc,        $value_ref,
            $rule,    $position, $parent
            )
            = @{$tree_node}[
            Marpa::Internal::Tree_Node::OR_NODE,
            Marpa::Internal::Tree_Node::CHOICE,
            Marpa::Internal::Tree_Node::PREDECESSOR,
            Marpa::Internal::Tree_Node::CAUSE,
            Marpa::Internal::Tree_Node::DEPTH,
            Marpa::Internal::Tree_Node::PERL_CLOSURE,
            Marpa::Internal::Tree_Node::ARGC,
            Marpa::Internal::Tree_Node::VALUE_REF,
            Marpa::Internal::Tree_Node::RULE,
            Marpa::Internal::Tree_Node::POSITION,
            Marpa::Internal::Tree_Node::PARENT,
            ];

        $text
            .= "Tree Node #$tree_position: "
            . $or_node->[Marpa::Internal::Or_Node::TAG]
            . "[$choice]";
        if ( defined $parent ) {
            $text .= "; Parent = $parent";
        }
        $text .= "; Depth = $depth; Rhs Length = $argc\n";

        $text .= '    Rule: '
            . Marpa::show_dotted_rule( $rule, $position + 1 ) . "\n";
        if ( defined $predecessor ) {
            $text
                .= '    Kernel: '
                . $predecessor->[Marpa::Internal::Tree_Node::OR_NODE]
                ->[Marpa::Internal::Or_Node::TAG] . "\n";
        } ## end if ( defined $predecessor )
        if ( defined $cause ) {
            $text
                .= '    Closure: '
                . $cause->[Marpa::Internal::Tree_Node::OR_NODE]
                ->[Marpa::Internal::Or_Node::TAG] . "\n";
        } ## end if ( defined $cause )
        if ($verbose) {
            $text .= '    Perl Closure: ' . ( defined $closure ? 'Y' : 'N' );
            if ( defined $value_ref ) {
                $text .= '; Token: '
                    . Data::Dumper->new( [ ${$value_ref} ] )->Terse(1)->Dump;
            }
            else {
                $text .= "\n";
            }
        } ## end if ($verbose)

        $tree_position++;

    }    # $tree_node

    return $text;

} ## end sub Marpa::Evaluator::show_tree

sub Marpa::Evaluator::set {
    my $evaler  = shift;
    my $args    = shift;
    my $recce   = $evaler->[Marpa::Internal::Evaluator::RECOGNIZER];
    my $grammar = $recce->[Marpa::Internal::Recognizer::GRAMMAR];
    Marpa::Grammar::set( $grammar, $args );
    return 1;
} ## end sub Marpa::Evaluator::set

# A constant to indicate in the decision vector,
# that a node is rejected.  It must not be a possible
# index to that vector, and therefore must be some negative
# number
use constant REJECTED     => -1;
use constant NOT_REJECTED => ( REJECTED - 1 );

use Marpa::Offset qw(
    { tasks for use in Marpa::Evaluator::value }
    :package=Marpa::Internal::Task
    ITERATE_INSTANCE
    ACCEPT_NODE
    REJECT_NODE
    BACKTRACK_TO_FORK
    BACKTRACK_TO_INSTANCE
    EVALUATE
    FORK_MARKER
);

use constant NEGATIVE_N_WIDTH => -4;

sub Marpa::show_decision {
    my ($decision) = @_;
    return 'none' if not defined $decision;
    return 'REJECTED' if $decision == REJECTED;
    return "ACCEPTED($decision)";
} ## end sub Marpa::show_decision

# This will replace the old value method
sub Marpa::Evaluator::new_value {
    my ($evaler) = @_;

    Marpa::exception('No parse supplied') if not defined $evaler;
    my $evaler_class = ref $evaler;
    my $right_class  = 'Marpa::Evaluator';
    Marpa::exception(
        "Don't parse argument is class: $evaler_class; should be: $right_class"
    ) if $evaler_class ne $right_class;

    my $journal   = $evaler->[Marpa::Internal::Evaluator::JOURNAL];
    my $and_nodes = $evaler->[Marpa::Internal::Evaluator::AND_NODES];
    my $decisions = $evaler->[Marpa::Internal::Evaluator::DECISIONS];
    my $instances = $evaler->[Marpa::Internal::Evaluator::INSTANCES];

    # If the journal is defined, but empty, that means we've
    # exhausted all parses.  Patiently keep returning failure
    # whenever called.
    return if defined $journal and @{$journal} == 0;

    my $recognizer = $evaler->[Marpa::Internal::Evaluator::RECOGNIZER];
    my $grammar    = $recognizer->[Marpa::Internal::Recognizer::GRAMMAR];

    my $tracing  = $grammar->[Marpa::Internal::Grammar::TRACING];
    my $trace_fh = $grammar->[Marpa::Internal::Grammar::TRACE_FILE_HANDLE];
    my $trace_values     = 0;
    my $trace_journal    = 0;
    my $trace_iterations = 0;
    my $trace_tasks      = 0;
    if ($tracing) {
        $trace_journal = $grammar->[Marpa::Internal::Grammar::TRACE_JOURNAL];
        $trace_values  = $grammar->[Marpa::Internal::Grammar::TRACE_VALUES];
        $trace_iterations =
            $grammar->[Marpa::Internal::Grammar::TRACE_ITERATIONS];
        $trace_tasks = $trace_iterations >= 2;
    } ## end if ($tracing)

    my $or_nodes    = $evaler->[Marpa::Internal::Evaluator::OR_NODES];
    my $rule_data   = $evaler->[Marpa::Internal::Evaluator::RULE_DATA];
    my $null_values = $evaler->[Marpa::Internal::Evaluator::NULL_VALUES];
    my $parse_count = $evaler->[Marpa::Internal::Evaluator::PARSE_COUNT]++;

    my $max_parses = $grammar->[Marpa::Internal::Grammar::MAX_PARSES];
    if ( $max_parses > 0 && $parse_count >= $max_parses ) {
        Marpa::exception("Maximum parse count ($max_parses) exceeded");
    }

    my $earleme_beyond_last =
        @{ $recognizer->[Marpa::Internal::Recognizer::EARLEY_SETS] };

    # Default is to backtrack, but if this is a new parse (no journal)
    # it will be overriden
    my @tasks = ( [Marpa::Internal::Task::BACKTRACK_TO_INSTANCE] );

    if ( not defined $journal ) {

        # This is a Guttman-Rossler Transform, which you can look up on Wikipedia.
        # Note the use of Unicode for packing, which I've not seen anyone else do.
        my %instances_by_sortkey = ();
        OR_NODE: for my $or_node ( @{$or_nodes} ) {
            my $start_earleme =
                $or_node->[Marpa::Internal::Or_Node::START_EARLEME];
            my $end_earleme =
                $or_node->[Marpa::Internal::Or_Node::END_EARLEME];
            my $or_node_children =
                $or_node->[Marpa::Internal::Or_Node::CHILD_IDS];
            for my $and_node_id ( @{$or_node_children} ) {
                my $and_node = $and_nodes->[$and_node_id];
                my $rule     = $and_node->[Marpa::Internal::And_Node::RULE];
                my $user_priority =
                    $rule->[Marpa::Internal::Rule::USER_PRIORITY];
                $user_priority //= 0;
                ## a non-zero user priority makes an and-node interesting

                my $internal_priority =
                    $rule->[Marpa::Internal::Rule::INTERNAL_PRIORITY];
                $internal_priority //= 0;

                # is this and-node interesting for priority purposes?
                ## any non-zero priority makes an and-node interesting
                my $interesting = $user_priority || $internal_priority;

                my $is_hasty = $rule->[Marpa::Internal::Rule::MINIMAL];
                my $laziness = 0;
                if ( defined $is_hasty ) {

                    # a hasty or lazy and-node is interesting
                    $interesting = 1;

                    $laziness = $end_earleme - $start_earleme;
                    if ($is_hasty) {
                        $laziness = $earleme_beyond_last - $laziness;
                    }
                } ## end if ( defined $is_hasty )

                # Only sort by location if the and-node is "interesting"
                # and has the same span as it original rule.
                my $location = 0;
                my $virtual_span =
                    $rule->[Marpa::Internal::Rule::VIRTUAL_SPAN];
                if ( $interesting and not $virtual_span ) {
                    $location = $earleme_beyond_last - $start_earleme;
                }

                # The sort order must
                # 1.) Preserve the chaf order that is in the internal priority.
                #     The below does that because all chaf pieces share the same
                #     rule and therefor the same user-priority, and because
                #     only the higher-sorted CHAF rules can have a non-zero
                #     location, and they will all have the same location.
                # 2.) Group all and-nodes with the signature together.
                # Otherwise, things break.

                # N-format is limited to 32-bits.
                # Limits on all these values are enforced to prevent overflow,
                # with the current exception of the RULE_ID.
                # TODO: Put a limit on RULE_ID.  Right now memory
                # overflow will occur long before it is reached,
                # but someday technology may allow there to be 2**31
                # rules.  Something to live for. :-)

                my $rule_id = $rule->[Marpa::Internal::Rule::ID];
                my $rule_position =
                    $and_node->[Marpa::Internal::And_Node::POSITION];
                my $sort_key = pack 'N*',
                    $location, $user_priority, $internal_priority, $laziness,
                    $rule_id, $rule_position, $start_earleme, $end_earleme;
                push @{ $instances_by_sortkey{$sort_key} }, $and_node_id;

            } ## end for my $and_node_id ( @{$or_node_children} )
        } ## end for my $or_node ( @{$or_nodes} )

        $instances = $evaler->[Marpa::Internal::Evaluator::INSTANCES] = [
            reverse map { $instances_by_sortkey{$_} }
                sort keys %instances_by_sortkey
        ];
        $journal   = $evaler->[Marpa::Internal::Evaluator::JOURNAL]   = [];
        $decisions = $evaler->[Marpa::Internal::Evaluator::DECISIONS] = [];
        @tasks = ( [ Marpa::Internal::Task::ITERATE_INSTANCE, 0, 0 ] );

    } ## end if ( not defined $journal )
    ## End not defined $journal

    ## This code is inherently complex, and like a case statement
    ## in that one case is laid out after another.  The performance
    ## hit from Perl subroutines would buy only a lot of page-flipping
    ## by the reader of the code.
    ## no critic (ControlStructures::ProhibitDeepNests)

    TASK: while ( my $task_data = pop @tasks ) {

        my $task = shift @{$task_data};

        if ( $task == Marpa::Internal::Task::ITERATE_INSTANCE ) {

            my ( $rank, $current_choice ) = @{$task_data};

            if ($trace_tasks) {
                print {$trace_fh} "Task: ITERATE_INSTANCE; rank $rank; ",
                    "current choice $current_choice; ",
                    ( scalar @tasks ), " tasks pending\n"
                    or Marpa::exception('print to trace handle failed');
            } ## end if ($trace_tasks)

            if ( $rank > $#{$instances} ) {
                @tasks = ( [ Marpa::Internal::Task::EVALUATE, 0 ] );
                next TASK;
            }

            my $choices = $instances->[$rank];

            if ( $current_choice > $#{$choices} or $current_choice < 0 ) {
                $current_choice = -1;
            }

            # Create the @tasks stack.
            # Note that we # overwrite the @tasks stack,
            # clearing it of any previous tasks.
            # We start by stacking the next INSTANCE iteration.
            # After that, we stack the ACCEPT_NODE task, if there is one.
            # It's important that the ACCEPT_NODE task
            # be performed after all the REJECT_NODE tasks.

            # If it's an iterable choice, then journal it.
            if ( $current_choice >= 0 ) {

                # Journal this choice.
                push @{$journal},
                    [
                    Marpa::Internal::Journal_Tag::INSTANCE, $rank,
                    $current_choice
                    ];

                if ($trace_journal) {
                    print {$trace_fh}
                        "Journal: Accepted instance, rank $rank, choice $current_choice\n"
                        or Marpa::exception('print to trace handle failed');
                }

                if ($trace_iterations) {
                    my $and_node_id = $choices->[$current_choice];
                    my $and_node    = $and_nodes->[$and_node_id];
                    my $and_node_tag =
                        $and_node->[Marpa::Internal::And_Node::TAG];
                    print {$trace_fh}
                        "Iteration: Accepted instance; rank $rank; ",
                        "choice $current_choice; and-node $and_node_tag\n"
                        or Marpa::exception('print to trace handle failed');
                } ## end if ($trace_iterations)

                @tasks = (
                    [ Marpa::Internal::Task::ITERATE_INSTANCE, $rank + 1, 0 ],
                    [   Marpa::Internal::Task::ACCEPT_NODE, $rank,
                        $choices->[$current_choice]
                    ]
                );

            } ## end if ( $current_choice >= 0 )
            else {

                if ($trace_iterations) {
                    print {$trace_fh}
                        "Iteration: Rejected instance, rank $rank\n"
                        or Marpa::exception('print to trace handle failed');
                }

                @tasks = (
                    [ Marpa::Internal::Task::ITERATE_INSTANCE, $rank + 1, 0 ],
                );

            } ## end else [ if ( $current_choice >= 0 ) ]

            REJECT_TASK_CHOICE:
            for my $rejection_choice ( 0 .. $#{$choices} ) {

                # Don't reject the current acceptance choice.
                next REJECT_TASK_CHOICE
                    if $rejection_choice == $current_choice;

                push @tasks,
                    [
                    Marpa::Internal::Task::REJECT_NODE, $rank,
                    $choices->[$rejection_choice]
                    ];

            } ## end for my $rejection_choice ( 0 .. $#{$choices} )

            next TASK;

        } ## end if ( $task == Marpa::Internal::Task::ITERATE_INSTANCE)

        if ( $task == Marpa::Internal::Task::ACCEPT_NODE ) {

            my ( $rank, $and_node_id ) = @{$task_data};

            if ($trace_tasks) {
                print {$trace_fh}
                    "Task: ACCEPT_NODE, rank $rank; and-node $and_node_id; ",
                    ( scalar @tasks ), " tasks pending\n"
                    or Marpa::exception('print to trace handle failed');
            } ## end if ($trace_tasks)

            my $decision = $decisions->[$and_node_id];

            # If this node is not decided, accept it
            if ( not defined $decision ) {

                push @{$journal},
                    [ Marpa::Internal::Journal_Tag::NODE, $and_node_id, ];

                my $and_node = $and_nodes->[$and_node_id];
                if ($trace_journal) {
                    my $and_node_tag =
                        $and_node->[Marpa::Internal::And_Node::TAG];
                    print {$trace_fh}
                        "Journal: Accepted $and_node_tag, rank $rank\n"
                        or Marpa::exception('print to trace handle failed');
                } ## end if ($trace_journal)

                $decisions->[$and_node_id] = $rank;

                my $or_node_id =
                    $and_node->[Marpa::Internal::And_Node::PARENT_ID];
                my $or_node = $or_nodes->[$or_node_id];
                my $and_node_choice =
                    $and_node->[Marpa::Internal::And_Node::PARENT_CHOICE];

                # reject all alternatives to this choice at this or-node
                my $siblings =
                    $or_node->[Marpa::Internal::Or_Node::CHILD_IDS];

                my @sibling_reject_tasks = ();
                SIBLING: for my $sibling_choice ( 0 .. $#{$siblings} ) {

                    # Not a proper sibling -- this is our original accepted node.
                    next SIBLING if $sibling_choice == $and_node_choice;

                    my $sibling_id = $siblings->[$sibling_choice];

                    push @sibling_reject_tasks,
                        [
                        Marpa::Internal::Task::REJECT_NODE, $rank,
                        $sibling_id
                        ];

                    next SIBLING;

                } ## end for my $sibling_choice ( 0 .. $#{$siblings} )

                # Now deal with the parent and-node.  This may involve
                # having to choose which fork to take on the path
                # upward.

                my $forks = $or_node->[Marpa::Internal::Or_Node::PARENT_IDS];

                # No forks means we are are the top or-node.
                # If we are at the top or-node,
                # we are done following the upward path.

                if ( not scalar @{$forks} ) {
                    push @tasks, @sibling_reject_tasks;
                    next TASK;
                }

                # Pick the first fork.  Do not journal the
                # decision unless it is iterable.  In other
                # words, do not journal if this fork choice
                # is already the last possible one.

                my $fork_choice = 0;
                my $parent_id   = $forks->[$fork_choice];
                my $parent_node = $and_nodes->[$parent_id];

                if ($trace_iterations) {
                    my $parent_node_tag =
                        $parent_node->[Marpa::Internal::And_Node::TAG];
                    print {$trace_fh}
                        "Iteration: Choosing upward fork $fork_choice, ",
                        "to $parent_node_tag, rank $rank\n"
                        or Marpa::exception('print to trace handle failed');
                } ## end if ($trace_iterations)

                # Are there other iterations possible after this one?
                if ( $fork_choice < $#{$forks} ) {

                    push @{$journal},
                        [
                        Marpa::Internal::Journal_Tag::FORK,
                        $rank, $forks, $fork_choice
                        ];

                    if ($trace_journal) {
                        my $parent_node_tag =
                            $parent_node->[Marpa::Internal::And_Node::TAG];
                        print {$trace_fh}
                            "Journal: Choosing upward fork $fork_choice, ",
                            "to and-node $parent_node_tag, rank $rank\n"
                            or
                            Marpa::exception('print to trace handle failed');
                    } ## end if ($trace_journal)

                    # put a marker on the @tasks stack, so we can unwind to here.
                    push @tasks,
                        [ Marpa::Internal::Task::FORK_MARKER, $fork_choice ];

                } ## end if ( $fork_choice < $#{$forks} )

                push @tasks,
                    [ Marpa::Internal::Task::ACCEPT_NODE, $rank, $parent_id ],
                    @sibling_reject_tasks;
                next TASK;

            } ## end if ( not defined $decision )

            # If the and-node has been rejected, or if we
            # are in a cycle, then backtrack.
            if (   $decision == REJECTED
                or $decision == $rank )
            {

                if ($trace_iterations) {
                    my $problem =
                        $decision == REJECTED
                        ? 'attempt to accept rejected node'
                        : 'cycle';
                    my $and_node = $and_nodes->[$and_node_id];
                    my $and_node_tag =
                        $and_node->[Marpa::Internal::And_Node::TAG];
                    print {$trace_fh}
                        "Iteration: Backtracking due to $problem at ",
                        "$and_node_tag\n"
                        or Marpa::exception('print to trace handle failed');
                } ## end if ($trace_iterations)

                push @tasks, [Marpa::Internal::Task::BACKTRACK_TO_FORK];
                next TASK;

            } ## end if ( $decision == REJECTED or $decision == $rank )

            # At this point we know that
            # the node was accepted
            # by a previous instance.  Update the decision, journal that,
            # and resume scanning instances.

            my $and_node = $and_nodes->[$and_node_id];

            push @{$journal},
                [
                Marpa::Internal::Journal_Tag::NODE, $and_node_id,
                $decision
                ];

            if ($trace_journal) {
                my $and_node_tag =
                    $and_node->[Marpa::Internal::And_Node::TAG];
                print {$trace_fh}
                    "Journal: Re-accepted $and_node_tag; rank $rank\n"
                    or Marpa::exception('print to trace handle failed');
            } ## end if ($trace_journal)

            $decisions->[$and_node_id] = $rank;

            push @tasks,
                [ Marpa::Internal::Task::ITERATE_INSTANCE, $rank + 1, 0 ];
            next TASK;

        } ## end if ( $task == Marpa::Internal::Task::ACCEPT_NODE )

        # FORK_MARKER tasks are no-ops, inserted to help the backtracking
        # rearrange the stack
        if ( $task == Marpa::Internal::Task::FORK_MARKER ) {
            next TASK;
        }

=begin Implementation:

In theory, we need to journal changes to the @tasks stack, so we can
reverse them on backtracking.  In practice, we avoid this with a few
tricks.

Case 1, INSTANCE iterations.  At every instance iteration, the @tasks
stack is empty.  So we can 'restore' the @tasks stack by clearing it.

Case 2, FORK iterations.  Here we need to be trickier.  FORK iterations
and pushing of BACKTRACK_TO_FORK tasks only happens during an ACCEPT_NODE
task.  We set up so that ACCEPT_NODE tasks happen last, after all
REJECT_NODE tasks are popped.  This means that at a FORK iteration, and
when a BACKTRACK_TO_FORK task is pushed, the @tasks stack will always
contain only a ITERATE_INSTANCE task.  So 'restoring' the @tasks stack is
a no-op.  If we backtrack all the way to an instance, case 1 applies.

=end Implementation:

=cut

        if (   $task == Marpa::Internal::Task::BACKTRACK_TO_FORK
            or $task == Marpa::Internal::Task::BACKTRACK_TO_INSTANCE )
        {

            if ($trace_tasks) {
                my $task_name =
                    $task == Marpa::Internal::Task::BACKTRACK_TO_INSTANCE
                    ? 'BACKTRACK_TO_INSTANCE'
                    : 'BACKTRACK_TO_FORK';
                print {$trace_fh} "Task: $task_name; ",
                    ( scalar @tasks ), " tasks pending\n"
                    or Marpa::exception('print to trace handle failed');
            } ## end if ($trace_tasks)

            # Start with ratcheting off
            my $instance_acceptances_ratcheted = 0;

            ENTRY: while ( my $journal_entry = pop @{$journal} ) {

                my $entry_type = shift @{$journal_entry};

                # A simple, non-iterable record of a decision about an
                # and-node.  Restore the previous value,
                # and move on.
                if ( $entry_type == Marpa::Internal::Journal_Tag::NODE ) {
                    my ( $and_node_id, $previous_value ) = @{$journal_entry};

                    if ($trace_journal) {
                        my $decision = $decisions->[$and_node_id];
                        my $and_node = $and_nodes->[$and_node_id];
                        my $and_node_tag =
                            $and_node->[Marpa::Internal::And_Node::TAG];
                        print {$trace_fh}
                            "Journal: Changing $and_node_tag decision from ",
                            Marpa::show_decision($decision),
                            ' back to ',
                            Marpa::show_decision($previous_value), "\n"
                            or
                            Marpa::exception('print to trace handle failed');
                    } ## end if ($trace_journal)

                    $decisions->[$and_node_id] = $previous_value;
                    next ENTRY;
                } ## end if ( $entry_type == ...)

                # Iterate the choice at a fork in the upward path
                # while following up on a decision to accept
                if ( $entry_type == Marpa::Internal::Journal_Tag::FORK ) {

                    # If we're backtracking to iterate an instance, just
                    # throw this away.
                    next ENTRY
                        if $task != Marpa::Internal::Task::BACKTRACK_TO_FORK;

                    my ( $rank, $forks, $fork_choice ) = @{$journal_entry};
                    $fork_choice++;

                    # Because we don't journal non-iterable FORK
                    # events, we are sure that there is a
                    # $forks->[$fork_choice]
                    my $parent_id   = $forks->[$fork_choice];
                    my $parent_node = $and_nodes->[$parent_id];

                    # Unwind the tasks stack to the point where
                    # this fork was taken
                    TASK: while ( my $unwound_task = pop @tasks ) {
                        my $task_name = shift @{$unwound_task};
                        next TASK
                            if $task_name
                                != Marpa::Internal::Task::FORK_MARKER;
                        my $forks_value_in_marker = shift @{$unwound_task};
                        last TASK if $forks_value_in_marker == $forks;
                    } ## end while ( my $unwound_task = pop @tasks )

                    # Don't add a journal entry unless there is at
                    # least one more iteration left
                    if ( $fork_choice < $#{$forks} ) {

                        push @{$journal},
                            [
                            Marpa::Internal::Journal_Tag::FORK,
                            $rank, $forks, $fork_choice
                            ];

                        if ($trace_journal) {
                            my $parent_node_tag = $parent_node
                                ->[Marpa::Internal::And_Node::TAG];
                            print {$trace_fh}
                                "Journal: Choosing upward fork $fork_choice, to and-node $parent_node_tag,",
                                " rank $rank\n"
                                or Marpa::exception(
                                'print to trace handle failed');
                        } ## end if ($trace_journal)

                        push @tasks,
                            [
                            Marpa::Internal::Task::FORK_MARKER, $fork_choice
                            ];

                    } ## end if ( $fork_choice < $#{$forks} )

                    if ($trace_iterations) {
                        my $parent_node_tag =
                            $parent_node->[Marpa::Internal::And_Node::TAG];
                        print {$trace_fh}
                            "Iteration: Choosing upward fork $fork_choice, to and-node $parent_node_tag,",
                            " rank $rank\n"
                            or
                            Marpa::exception('print to trace handle failed');
                    } ## end if ($trace_iterations)

                    push @tasks,
                        [
                        Marpa::Internal::Task::ACCEPT_NODE, $rank,
                        $parent_id,
                        ];
                    next TASK;
                } ## end if ( $entry_type == ...)

                # "Ratchet" instance acceptances, that is, only iterate an
                # accepted instance by rejecting it.
                if ( $entry_type
                    == Marpa::Internal::Journal_Tag::RATCHET_INSTANCE_ACCEPTANCES
                    )
                {

                    # Ratchets encountered while the ratchet flag is already set will be no-ops.
                    # There are left over from previous parses, and do no harm.
                    $instance_acceptances_ratcheted = 1;
                    next ENTRY;
                } ## end if ( $entry_type == ...)

                # This entry records a decision to accept an instance.
                # Iterate this instance.
                if ( $entry_type == Marpa::Internal::Journal_Tag::INSTANCE ) {
                    my ( $rank, $choice ) = @{$journal_entry};

                    if ($instance_acceptances_ratcheted) {

                        if ($trace_journal) {
                            print {$trace_fh}
                                "Journal: Instance acceptance ratchet moved to rank $rank\n"
                                or Marpa::exception(
                                'print to trace handle failed');
                        } ## end if ($trace_journal)

                        push @{$journal},
                            [
                            Marpa::Internal::Journal_Tag::RATCHET_INSTANCE_ACCEPTANCES
                            ];

                        if ($trace_iterations) {
                            print {$trace_fh}
                                "Iteration: Backtracking, rejecting ratcheted instance, rank $rank\n"
                                or Marpa::exception(
                                'print to trace handle failed');
                        } ## end if ($trace_iterations)

                        @tasks = (
                            [   Marpa::Internal::Task::ITERATE_INSTANCE,
                                $rank, -1
                            ]
                        );
                        next TASK;
                    } ## end if ($instance_acceptances_ratcheted)

                    $choice++;

                    if ($trace_journal) {
                        print {$trace_fh}
                            "Journal: Attempting to iterate rank $rank to ",
                            " choice $choice\n"
                            or
                            Marpa::exception('print to trace handle failed');
                    } ## end if ($trace_journal)

                    @tasks = (
                        [   Marpa::Internal::Task::ITERATE_INSTANCE, $rank,
                            $choice
                        ]
                    );

                    next TASK;

                } ## end if ( $entry_type == ...)

                Marpa::exception(
                    'Internal error: unknown evaluator journal entry');

            } ## end while ( my $journal_entry = pop @{$journal} )
            ## End ENTRY

            if ($trace_iterations) {
                print {$trace_fh}
                    'Iteration: Have backtracked over the entire journal: ',
                    "will return no parses\n"
                    or Marpa::exception('print to trace handle failed');
            } ## end if ($trace_iterations)

            # If we are here, we have backed out the entire journal.
            # There are no (or no more) parses.
            return;

        } ## end if ( $task == Marpa::Internal::Task::BACKTRACK_TO_FORK...)

        if ( $task == Marpa::Internal::Task::REJECT_NODE ) {

            my ( $rank, $and_node_id ) = @{$task_data};

            my $and_node = $and_nodes->[$and_node_id];

            if ($trace_tasks) {
                my $and_node_tag =
                    $and_node->[Marpa::Internal::And_Node::TAG];
                print {$trace_fh}
                    "Task: REJECT_NODE, rank $rank; node $and_node_tag; ",
                    ( scalar @tasks ), " tasks pending\n"
                    or Marpa::exception('print to trace handle failed');
            } ## end if ($trace_tasks)

            my $decision = $decisions->[$and_node_id];

            # Has the decision already been made?
            if ( defined $decision ) {

                # If it's already rejected, we're done.
                # Next task.
                if ( $decision == REJECTED ) {

                    if ( $trace_iterations >= 3 ) {
                        my $and_node_tag =
                            $and_node->[Marpa::Internal::And_Node::TAG];
                        print {$trace_fh}
                            "Iteration: Already rejected and-node $and_node_tag, rank $rank\n"
                            or
                            Marpa::exception('print to trace handle failed');
                    } ## end if ( $trace_iterations >= 3 )

                    next TASK;
                } ## end if ( $decision == REJECTED )

                # If we're here, a decision to accept has already
                # been made.  This can't happen and we need to
                # backtrack.
                if ( $trace_iterations >= 3 ) {
                    my $and_node_tag =
                        $and_node->[Marpa::Internal::And_Node::TAG];
                    print {$trace_fh}
                        "Iteration: Attempted to reject an accepted node: $and_node_tag; ",
                        "need to backtrack\n"
                        or Marpa::exception('print to trace handle failed');
                } ## end if ( $trace_iterations >= 3 )

                @tasks = ( [Marpa::Internal::Task::BACKTRACK_TO_INSTANCE] );
                next TASK;
            } ## end if ( defined $decision )

            # If we are at this point, this and-node is undecided.
            # Reject it.
            push @{$journal},
                [ Marpa::Internal::Journal_Tag::NODE, $and_node_id ];

            if ($trace_journal) {
                my $and_node_tag =
                    $and_node->[Marpa::Internal::And_Node::TAG];
                print {$trace_fh} "Journal: Rejecting $and_node_tag, ",
                    "rank $rank\n"
                    or Marpa::exception('print to trace handle failed');
            } ## end if ($trace_journal)

            $decisions->[$and_node_id] = REJECTED;

            PARENT_OR_NODE: {
                my $parent_or_node_id =
                    $and_node->[Marpa::Internal::And_Node::PARENT_ID];
                my $parent_or_node = $or_nodes->[$parent_or_node_id];

                # Is there an unrejected sibling?
                my $child_ids =
                    $parent_or_node->[Marpa::Internal::Or_Node::CHILD_IDS];
                my $first_unrejected_sibling = List::Util::first {
                    ( $decisions->[ $child_ids->[$_] ] // NOT_REJECTED )
                        != REJECTED;
                }
                ( 0 .. $#{$child_ids} );

                # If there is an unrejected sibling, stop here.
                last PARENT_OR_NODE if defined $first_unrejected_sibling;

                # If all the siblings were rejected, we reject the parent
                # and-nodes as well.

                my $parent_and_node_ids =
                    $parent_or_node->[Marpa::Internal::Or_Node::PARENT_IDS];

                # We're at the top or node, and have just rejected it
                # and all its children.  This can't be allowed to
                # happen, and we need to backtrack.
                if ( not scalar @{$parent_and_node_ids} ) {
                    if ($trace_iterations) {
                        print {$trace_fh}
                            "Iteration: Just rejected the top node -- need to backtrack\n"
                            or
                            Marpa::exception('print to trace handle failed');
                    } ## end if ($trace_iterations)
                    @tasks =
                        ( [Marpa::Internal::Task::BACKTRACK_TO_INSTANCE] );
                    next TASK;
                } ## end if ( not scalar @{$parent_and_node_ids} )

                # We tested for the case of no parents (the top
                # node) above, so this will add at least one task
                for my $parent_and_node_id ( @{$parent_and_node_ids} ) {

                    push @tasks,
                        [
                        Marpa::Internal::Task::REJECT_NODE, $rank,
                        $parent_and_node_id
                        ];

                } ## end for my $parent_and_node_id ( @{$parent_and_node_ids})
            } ## end PARENT_OR_NODE:

            CHILD_OR_NODE:
            for my $child_field ( Marpa::Internal::And_Node::CAUSE,
                Marpa::Internal::And_Node::PREDECESSOR
                )
            {
                my $child_or_node = $and_node->[$child_field];

                # Is there an unrejected co-parent?
                my $parent_ids =
                    $child_or_node->[Marpa::Internal::Or_Node::PARENT_IDS];
                my $first_unrejected_coparent = List::Util::first {
                    ( $decisions->[ $parent_ids->[$_] ] // NOT_REJECTED )
                        != REJECTED;
                }
                ( 0 .. $#{$parent_ids} );

                # If there is an unrejected co-parent, stop here.
                next CHILD_OR_NODE if defined $first_unrejected_coparent;

                # If all the co-parents of the or-node
                # were rejected, we reject the child
                # and-nodes as well.
                my $child_and_nodes =
                    $child_or_node->[Marpa::Internal::Or_Node::CHILD_IDS];

                for my $child_and_node_id ( @{$child_and_nodes} ) {

                    push @tasks,
                        [
                        Marpa::Internal::Task::REJECT_NODE, $rank,
                        $child_and_node_id
                        ];

                } ## end for my $child_and_node_id ( @{$child_and_nodes} )
            } ## end for my $child_field ( Marpa::Internal::And_Node::CAUSE...)

            next TASK;

        } ## end if ( $task == Marpa::Internal::Task::REJECT_NODE )

        if ( $task == Marpa::Internal::Task::EVALUATE ) {

            if ($trace_tasks) {
                print {$trace_fh} 'Task: EVALUATE; ',
                    ( scalar @tasks ), " tasks pending\n"
                    or Marpa::exception('print to trace handle failed');
            }

            my @work_list;

            if ($trace_journal) {
                print {$trace_fh}
                    "Journal: Instance acceptance ratchet placed at end of journal\n"
                    or Marpa::exception('print to trace handle failed');
            }

            push @{$journal},
                [Marpa::Internal::Journal_Tag::RATCHET_INSTANCE_ACCEPTANCES];

            my @or_node_choices;
            $#or_node_choices = $#{$or_nodes};
            AND_NODE: for my $and_node_id ( 0 .. $#{$decisions} ) {
                my $decision = $decisions->[$and_node_id];
                next AND_NODE
                    if not defined $decision
                        or $decision == REJECTED;
                my $and_node = $and_nodes->[$and_node_id];
                my $parent_choice =
                    $and_node->[Marpa::Internal::And_Node::PARENT_CHOICE];
                my $parent_id =
                    $and_node->[Marpa::Internal::And_Node::PARENT_ID];
                $or_node_choices[$parent_id] = $parent_choice;
            } ## end for my $and_node_id ( 0 .. $#{$decisions} )

            # Write the and-nodes out in preorder
            my @preorder = ();

            @work_list = (
                do {
                    my $or_node    = $or_nodes->[0];
                    my $or_node_id = $or_node->[Marpa::Internal::Or_Node::ID];
                    my $choice     = $or_node_choices[$or_node_id];
                    $and_nodes
                        ->[ $or_node->[Marpa::Internal::Or_Node::CHILD_IDS]
                        ->[$choice] ];
                    } ## end do
            );
            OR_NODE: while ( my $and_node = pop @work_list ) {
                my $left_or_node =
                    $and_node->[Marpa::Internal::And_Node::PREDECESSOR];
                my $right_or_node =
                    $and_node->[Marpa::Internal::And_Node::CAUSE];
                if ( not defined $left_or_node and defined $right_or_node ) {
                    $left_or_node  = $right_or_node;
                    $right_or_node = undef;
                }
                if ( defined $left_or_node ) {
                    my $or_node_id =
                        $left_or_node->[Marpa::Internal::Or_Node::ID];
                    my $choice = $or_node_choices[$or_node_id];
                    push @work_list,
                        $and_nodes->[ $left_or_node
                        ->[Marpa::Internal::Or_Node::CHILD_IDS]->[$choice] ];
                } ## end if ( defined $left_or_node )
                if ( defined $right_or_node ) {
                    my $or_node_id =
                        $right_or_node->[Marpa::Internal::Or_Node::ID];
                    my $choice = $or_node_choices[$or_node_id];
                    push @work_list,
                        $and_nodes->[ $right_or_node
                        ->[Marpa::Internal::Or_Node::CHILD_IDS]->[$choice] ];
                } ## end if ( defined $right_or_node )
                push @preorder, $and_node;
            } ## end while ( my $and_node = pop @work_list )
            ## End OR_NODE:

            my @evaluation_stack = ();

            TREE_NODE: for my $and_node ( reverse @preorder ) {

                if ( $trace_values >= 3 ) {
                    for my $i ( reverse 0 .. $#evaluation_stack ) {
                        printf {$trace_fh} 'Stack position %3d:', $i
                            or
                            Marpa::exception('print to trace handle failed');
                        print {$trace_fh} q{ },
                            Data::Dumper->new( [ $evaluation_stack[$i] ] )
                            ->Terse(1)->Dump
                            or
                            Marpa::exception('print to trace handle failed');
                    } ## end for my $i ( reverse 0 .. $#evaluation_stack )
                } ## end if ( $trace_values >= 3 )

                my ( $closure, $value_ref, $argc ) = @{$and_node}[
                    Marpa::Internal::And_Node::PERL_CLOSURE,
                    Marpa::Internal::And_Node::VALUE_REF,
                    Marpa::Internal::And_Node::ARGC,
                ];

                if ( defined $value_ref ) {

                    push @evaluation_stack, $value_ref;

                    if ($trace_values) {
                        print {$trace_fh}
                            'Pushed value from ',
                            $and_node->[Marpa::Internal::And_Node::TAG],
                            ': ',
                            Data::Dumper->new( [ ${$value_ref} ] )->Terse(1)
                            ->Dump
                            or
                            Marpa::exception('print to trace handle failed');
                    } ## end if ($trace_values)

                }    # defined $value_ref

                next TREE_NODE if not defined $closure;

                if ($trace_values) {
                    my $rule = $and_node->[Marpa::Internal::And_Node::RULE];
                    say {$trace_fh}
                        'Popping ',
                        $argc,
                        ' values to evaluate ',
                        $and_node->[Marpa::Internal::And_Node::TAG],
                        ', rule: ', Marpa::brief_rule($rule)
                        or Marpa::exception('Could not print to trace file');
                } ## end if ($trace_values)

                my $args =
                    [ map { ${$_} } ( splice @evaluation_stack, -$argc ) ];

                my $result;

                my $closure_type = ref $closure;

                if ( $closure_type eq 'CODE' ) {

                    my @warnings;
                    my $eval_ok;
                    DO_EVAL: {
                        local $SIG{__WARN__} =
                            sub { push @warnings, [ $_[0], ( caller 0 ) ]; };

                        $eval_ok =
                            eval { $result = $closure->( @{$args} ); 1 };
                    } ## end DO_EVAL:

                    if ( not $eval_ok or @warnings ) {
                        my $fatal_error = $EVAL_ERROR;
                        my $rule =
                            $and_node->[Marpa::Internal::And_Node::RULE];
                        my $code =
                            $rule_data->[ $rule->[Marpa::Internal::Rule::ID] ]
                            ->[Marpa::Internal::Evaluator_Rule::CODE];
                        Marpa::Internal::code_problems(
                            {   fatal_error => $fatal_error,
                                grammar     => $grammar,
                                eval_ok     => $eval_ok,
                                warnings    => \@warnings,
                                where       => 'computing value',
                                long_where  => 'computing value for rule: '
                                    . Marpa::brief_rule($rule),
                                code => \$code,
                            }
                        );
                    } ## end if ( not $eval_ok or @warnings )

                } ## end if ( $closure_type eq 'CODE' )

                # don't document this behavior -- I'll probably want to
                # use non-reference "closure" values for special hacks
                # in the future.
                elsif ( $closure_type eq q{} ) {    # when not reference
                    $result = $closure;
                }    # when not reference

                else {    # when non-code reference
                    $result = ${$closure};
                }    # when non-code reference

                if ($trace_values) {
                    print {$trace_fh} 'Calculated and pushed value: ',
                        Data::Dumper->new( [$result] )->Terse(1)->Dump
                        or Marpa::exception('print to trace handle failed');
                }

                push @evaluation_stack, \$result;

            }    # TREE_NODE

            return pop @evaluation_stack;

        } ## end if ( $task == Marpa::Internal::Task::EVALUATE )
        ## End EVALUATE

    } ## end while ( my $task_data = pop @tasks )
    ## End TASK
    ## use critic

    Carp::confess('Internal error: Should not reach here');

} ## end sub Marpa::Evaluator::new_value

# Apparently perlcritic has a bug and doesn't see the final return
sub Marpa::Evaluator::value {

    my $evaler     = shift;
    my $recognizer = $evaler->[Marpa::Internal::Evaluator::RECOGNIZER];

    Marpa::exception('No parse supplied') if not defined $evaler;
    my $evaler_class = ref $evaler;
    my $right_class  = 'Marpa::Evaluator';
    Marpa::exception(
        "Don't parse argument is class: $evaler_class; should be: $right_class"
    ) if not $evaler_class eq $right_class;

    my ( $grammar, ) =
        @{$recognizer}[ Marpa::Internal::Recognizer::GRAMMAR, ];

    my $tracing  = $grammar->[Marpa::Internal::Grammar::TRACING];
    my $trace_fh = $grammar->[Marpa::Internal::Grammar::TRACE_FILE_HANDLE];
    my $trace_values     = 0;
    my $trace_iterations = 0;
    if ($tracing) {
        $trace_values = $grammar->[Marpa::Internal::Grammar::TRACE_VALUES];
        $trace_iterations =
            $grammar->[Marpa::Internal::Grammar::TRACE_ITERATIONS];
    }

    my ( $bocage, $tree, $rule_data, $null_values, ) = @{$evaler}[
        Marpa::Internal::Evaluator::OR_NODES,
        Marpa::Internal::Evaluator::TREE,
        Marpa::Internal::Evaluator::RULE_DATA,
        Marpa::Internal::Evaluator::NULL_VALUES,
    ];

    my $max_parses = $grammar->[Marpa::Internal::Grammar::MAX_PARSES];

    my $parse_count = $evaler->[Marpa::Internal::Evaluator::PARSE_COUNT]++;
    if ( $max_parses > 0 && $parse_count >= $max_parses ) {
        Marpa::exception("Maximum parse count ($max_parses) exceeded");
    }

    my @traversal_stack;

    $tree = $evaler->[Marpa::Internal::Evaluator::TREE];
    if ( not defined $tree ) {

        my $new_tree_node;
        @{$new_tree_node}[
            Marpa::Internal::Tree_Node::OR_NODE,
            Marpa::Internal::Tree_Node::DEPTH,
            ]
            = ( $bocage->[0], 0, );
        @traversal_stack = ($new_tree_node);

        $evaler->[Marpa::Internal::Evaluator::TREE] = $tree = [];

    } ## end if ( not defined $tree )
    else {

        # If we are called with empty tree after the first parse,
        # we've already returned all the parses.  Patiently keep
        # returning failure.
        return if @{$tree} == 0;

    } ## end else [ if ( not defined $tree ) ]

    my @old_tree = @{$tree};
    my @last_position_by_depth;
    my $build_node;

    TREE_NODE: while (1) {

        my $node = pop @{$tree};

        # if no more nodes to pop and none on the traversal stack
        # we've exhausted the parse possibilities
        return
            if not defined $node and not scalar @traversal_stack;

        my $tree_position = @{$tree};

        if ( defined $node ) {

            my ( $choice, $or_node, $depth, $parent ) = @{$node}[
                Marpa::Internal::Tree_Node::CHOICE,
                Marpa::Internal::Tree_Node::OR_NODE,
                Marpa::Internal::Tree_Node::DEPTH,
                Marpa::Internal::Tree_Node::PARENT,
            ];

            if ( defined $build_node ) {

                @traversal_stack =
                    grep { $_->[Marpa::Internal::Tree_Node::DEPTH] < $depth }
                    @traversal_stack;

                if ( $build_node <= $tree_position ) { undef $build_node }

            } ## end if ( defined $build_node )

            my $and_nodes = $or_node->[Marpa::Internal::Or_Node::AND_NODES];

            $choice++;

            if ( $choice >= @{$and_nodes} ) {
                if ( not defined $build_node ) {
                    $last_position_by_depth[$depth] = $tree_position;
                }
                next TREE_NODE;
            } ## end if ( $choice >= @{$and_nodes} )

            if ($trace_iterations) {
                say {$trace_fh}
                    'Iteration ',
                    $choice,
                    ' tree node #',
                    $tree_position, q{ },
                    $or_node->[Marpa::Internal::Or_Node::TAG],
                    or Marpa::exception('print to trace handle failed');
            } ## end if ($trace_iterations)

            my $new_tree_node;
            @{$new_tree_node}[
                Marpa::Internal::Tree_Node::CHOICE,
                Marpa::Internal::Tree_Node::OR_NODE,
                Marpa::Internal::Tree_Node::DEPTH,
                Marpa::Internal::Tree_Node::PARENT,
                ]
                = ( $choice, $or_node, $depth, $parent );
            push @traversal_stack, $new_tree_node;

        }    # defined $node

        # A preorder traversal, to build the tree
        # Start with the first or-node of the bocage.
        # The code below assumes the or-node is the first field of the tree node.
        OR_NODE: while (@traversal_stack) {

            $build_node //= $tree_position;

            my $new_tree_node = pop @traversal_stack;

            my ( $or_node, $choice, $depth ) = @{$new_tree_node}[
                Marpa::Internal::Tree_Node::OR_NODE,
                Marpa::Internal::Tree_Node::CHOICE,
                Marpa::Internal::Tree_Node::DEPTH,
            ];
            $choice //= 0;

            my ( $predecessor_or_node, $cause_or_node, $closure, $argc,
                $value_ref, $rule, $rule_position, );

            my $and_nodes = $or_node->[Marpa::Internal::Or_Node::AND_NODES];

            my $or_node_is_closure =
                $or_node->[Marpa::Internal::Or_Node::IS_COMPLETED];

            my $or_node_id = $or_node->[Marpa::Internal::Or_Node::ID];

            AND_NODE: while (1) {

                my $and_node = $and_nodes->[$choice];

                # if none of the and nodes are useable, this or node is discarded
                # and we go to the outer loop and pop tree nodes until
                # we find one which can be iterated.
                next TREE_NODE if not defined $and_node;

                my $and_node_id = $and_node->[Marpa::Internal::And_Node::ID];
                (   $predecessor_or_node, $cause_or_node, $closure, $argc,
                    $value_ref, $rule, $rule_position,
                    )
                    = @{$and_node}[
                    Marpa::Internal::And_Node::PREDECESSOR,
                    Marpa::Internal::And_Node::CAUSE,
                    Marpa::Internal::And_Node::PERL_CLOSURE,
                    Marpa::Internal::And_Node::ARGC,
                    Marpa::Internal::And_Node::VALUE_REF,
                    Marpa::Internal::And_Node::RULE,
                    Marpa::Internal::And_Node::POSITION,
                    ];

                # if this or node is not a closure or-node or
                # this rule is not part of a cycle, we can use this and-node
                last AND_NODE if not $or_node_is_closure;
                last AND_NODE if not $rule->[Marpa::Internal::Rule::CYCLE];

                # if this rule is part of a cycle,
                # and this is a closure or-node
                # check to see if we have cycled

                my $cycles = $evaler->[Marpa::Internal::Evaluator::CYCLES];

                # if by an initial highball estimate
                # we have yet to cycle more than a limit (now hard coded
                # to 1), then we can use this and node
                last AND_NODE if $cycles->{$and_node_id}++ < 1;

                # compute actual cycles count
                my $parent =
                    $new_tree_node->[Marpa::Internal::Tree_Node::PARENT];
                my $cycles_count = 0;

                while ( defined $parent ) {
                    my $parent_node = $tree->[$parent];
                    my ( $tree_or_node, $parent_choice );
                    ( $tree_or_node, $parent, $parent_choice ) =
                        @{$parent_node}[
                        Marpa::Internal::Tree_Node::OR_NODE,
                        Marpa::Internal::Tree_Node::PARENT,
                        Marpa::Internal::Tree_Node::CHOICE,
                        ];
                    my $parent_or_node_id =
                        $tree_or_node->[Marpa::Internal::Or_Node::ID];
                    if (    $or_node_id eq $parent_or_node_id
                        and $choice == $parent_choice )
                    {
                        $cycles_count++;
                    }
                } ## end while ( defined $parent )

                # replace highball estimate with actual count
                $cycles->{$and_node_id} = $cycles_count;

                # repeat the test
                last AND_NODE if $cycles->{$and_node_id}++ < 1;

                # this and-node was rejected -- try the next
                $choice++;

            }    # AND_NODE

            my $predecessor_tree_node;
            if ( defined $predecessor_or_node ) {
                @{$predecessor_tree_node}[
                    Marpa::Internal::Tree_Node::OR_NODE,
                    Marpa::Internal::Tree_Node::DEPTH,
                    Marpa::Internal::Tree_Node::PARENT
                    ]
                    = ( $predecessor_or_node, $depth + 1, scalar @{$tree} );
            } ## end if ( defined $predecessor_or_node )

            my $cause_tree_node;
            if ( defined $cause_or_node ) {
                @{$cause_tree_node}[
                    Marpa::Internal::Tree_Node::OR_NODE,
                    Marpa::Internal::Tree_Node::DEPTH,
                    Marpa::Internal::Tree_Node::PARENT
                    ]
                    = ( $cause_or_node, $depth + 1, scalar @{$tree} );
            } ## end if ( defined $cause_or_node )

            @{$new_tree_node}[
                Marpa::Internal::Tree_Node::CHOICE,
                Marpa::Internal::Tree_Node::PREDECESSOR,
                Marpa::Internal::Tree_Node::CAUSE,
                Marpa::Internal::Tree_Node::PERL_CLOSURE,
                Marpa::Internal::Tree_Node::ARGC,
                Marpa::Internal::Tree_Node::RULE,
                Marpa::Internal::Tree_Node::VALUE_REF,
                Marpa::Internal::Tree_Node::POSITION,
                ]
                = (
                $choice, $predecessor_tree_node, $cause_tree_node, $closure,
                $argc, $rule, $value_ref, $rule_position,
                );

            if ($trace_iterations) {
                my $value_description = "\n";
                if ( defined $value_ref ) {
                    $value_description = '; value='
                        . Data::Dumper->new( ${$value_ref} )->Terse(1)->Dump;
                }
                print {$trace_fh}
                    'Pushing tree node #',
                    ( scalar @{$tree} ), q{ },
                    $or_node->[Marpa::Internal::Or_Node::TAG],
                    "[$choice]: ",
                    Marpa::show_dotted_rule( $rule, $rule_position + 1 ),
                    $value_description
                    or Marpa::exception('print to trace handle failed');
            } ## end if ($trace_iterations)

            push @{$tree}, $new_tree_node;

            push @traversal_stack,
                grep { defined $_ }
                ( $predecessor_tree_node, $cause_tree_node );

        }    # OR_NODE

        last TREE_NODE;

    }    # TREE_NODE

    # The scheme for finding which leaf side nodes to take
    # from the old tree is based on this fact:  Any child
    # of a later-in-preorder node at or above the depth of
    # the root of the build tree, cannot be in that build
    # tree, and vice versa.

    my $build_depth =
        $tree->[$build_node]->[Marpa::Internal::Tree_Node::DEPTH];
    my $leaf_side_start_position =
        min( grep { defined $_ }
            @last_position_by_depth[ 0 .. $build_depth ] );
    my $nodes_built = @{$tree} - $build_node;

    if ($trace_iterations) {
        say {$trace_fh} 'Nodes built: ', $nodes_built,
            '; kept on root side: ', $build_node, '; kept on leaf side: ',
            (
            defined $leaf_side_start_position
            ? @old_tree - $leaf_side_start_position
            : 0
            ) or Marpa::exception('print to trace handle failed');
    } ## end if ($trace_iterations)

    # Put the uniterated leaf side of the tree back on the stack.
    if ( defined $leaf_side_start_position ) {
        push @{$tree}, @old_tree[ $leaf_side_start_position .. $#old_tree ];
    }

    my @evaluation_stack = ();

    TREE_NODE: for my $node ( reverse @{$tree} ) {

        if ( $trace_values >= 3 ) {
            for my $i ( reverse 0 .. $#evaluation_stack ) {
                printf {$trace_fh} 'Stack position %3d:', $i;
                print {$trace_fh} q{ },
                    Data::Dumper->new( [ $evaluation_stack[$i] ] )->Terse(1)
                    ->Dump
                    or Marpa::exception('print to trace handle failed');
            } ## end for my $i ( reverse 0 .. $#evaluation_stack )
        } ## end if ( $trace_values >= 3 )

        my ( $closure, $value_ref, $argc ) = @{$node}[
            Marpa::Internal::Tree_Node::PERL_CLOSURE,
            Marpa::Internal::Tree_Node::VALUE_REF,
            Marpa::Internal::Tree_Node::ARGC,
        ];

        if ( defined $value_ref ) {

            push @evaluation_stack, $value_ref;

            if ($trace_values) {
                my ( $or_node, ) =
                    @{$node}[ Marpa::Internal::Tree_Node::OR_NODE, ];
                print {$trace_fh}
                    'Pushed value from ',
                    $or_node->[Marpa::Internal::Or_Node::TAG],
                    ': ',
                    Data::Dumper->new( [ ${$value_ref} ] )->Terse(1)->Dump
                    or Marpa::exception('print to trace handle failed');
            } ## end if ($trace_values)

        }    # defined $value_ref

        next TREE_NODE if not defined $closure;

        if ($trace_values) {
            my ( $or_node, $rule ) = @{$node}[
                Marpa::Internal::Tree_Node::OR_NODE,
                Marpa::Internal::Tree_Node::RULE,
            ];
            say {$trace_fh}
                'Popping ',
                $argc,
                ' values to evaluate ',
                $or_node->[Marpa::Internal::Or_Node::TAG],
                ', rule: ',
                Marpa::brief_rule($rule);
        } ## end if ($trace_values)

        my $args = [ map { ${$_} } ( splice @evaluation_stack, -$argc ) ];

        my $result;

        my $closure_type = ref $closure;

        if ( $closure_type eq 'CODE' ) {

            {
                my @warnings;
                my $eval_ok;
                DO_EVAL: {
                    local $SIG{__WARN__} =
                        sub { push @warnings, [ $_[0], ( caller 0 ) ]; };

                    $eval_ok = eval { $result = $closure->( @{$args} ); 1 };

                } ## end DO_EVAL:

                if ( not $eval_ok or @warnings ) {
                    my $fatal_error = $EVAL_ERROR;
                    my $rule = $node->[ Marpa::Internal::Tree_Node::RULE, ];
                    my $code =
                        $rule_data->[ $rule->[Marpa::Internal::Rule::ID] ]
                        ->[Marpa::Internal::Evaluator_Rule::CODE];
                    Marpa::Internal::code_problems(
                        {   fatal_error => $fatal_error,
                            grammar     => $grammar,
                            eval_ok     => $eval_ok,
                            warnings    => \@warnings,
                            where       => 'computing value',
                            long_where  => 'computing value for rule: '
                                . Marpa::brief_rule($rule),
                            code => \$code,
                        }
                    );
                } ## end if ( not $eval_ok or @warnings )
            }

        }    # when CODE

        # don't document this behavior -- I'll probably want to
        # use non-reference "closure" values for special hacks
        # in the future.
        elsif ( $closure_type eq q{} ) {    # when not reference
            $result = $closure;
        }    # when not reference

        else {    # when non-code reference
            $result = ${$closure};
        }    # when non-code reference

        if ($trace_values) {
            print {$trace_fh} 'Calculated and pushed value: ',
                Data::Dumper->new( [$result] )->Terse(1)->Dump
                or Marpa::exception('print to trace handle failed');
        }

        push @evaluation_stack, \$result;

    }    # TREE_NODE

    return pop @evaluation_stack;

} ## end sub Marpa::Evaluator::value

*Marpa::Evaluator::old_value = \&Marpa::Evaluator::value;

1;

__END__

=pod

=head1 NAME

Marpa::Evaluator - Marpa Evaluator Objects

=head1 SYNOPSIS

=begin Marpa::Test::Display:

## next 3 displays
in_file($_, 't/equation_s.t')

=end Marpa::Test::Display:

    my $fail_offset = $recce->text('2-0*3+1');
    if ( $fail_offset >= 0 ) {
        Marpa::exception("Parse failed at offset $fail_offset");
    }

    my $evaler = Marpa::Evaluator->new( { recognizer => $recce } );
    Marpa::exception('Parse failed') if not $evaler;

    my $i = -1;
    while ( defined( my $value = $evaler->value() ) ) {
        $i++;
        if ( $i > $#expected ) {
            Test::More::fail( 'Ambiguous equation has extra value: ' . ${$value} . "\n" );
        }
        else {
            Marpa::Test::is( ${$value}, $expected[$i],
                "Ambiguous Equation Value $i" );
        }
    } ## end while ( defined( my $value = $evaler->value() ) )

=head1 DESCRIPTION

Parses are found and evaluated by Marpa's evaluator objects.
Evaluators are created with the C<new> constructor,
which requires a Marpa recognizer object
as an argument.

Marpa allows ambiguous parses, so evaluator objects are iterators.
Iteration is performed with the C<value> method,
which returns a reference to the value of the next parse.
Often only the first parse is needed,
in which case the C<value> method can be called just once.

By default, the C<new> constructor clones the recognizer, so that
multiple evaluators do not interfere with each other.

=head2 Null Values

A "null value" is the value used for a symbol when it is nulled in a parse.
By default, the null value is a Perl undefined.
The default null value is a Marpa option (C<default_null_value>) and can be reset.

Each symbol can have its own null symbol value.
The null symbol value for any symbol is calculated using the null symbol action.
The B<null symbol action> for a symbol is the action
specified for the empty rule with that symbol on its left hand side.
The null symbol action is B<not> a rule action.
It's a property of the symbol, and applies whenever the symbol is nulled,
even when the symbol's empty rule is not involved.

For example, in MDL,
the following says that whenever the symbol C<A> is nulled,
its value should be a string that says it is missing.

=begin Marpa::Test::Display:

## next display
in_file($_, 'example/null_value.marpa');

=end Marpa::Test::Display:

    A: . q{'A is missing'}.

Null symbol actions are evaluated differently from rule actions.
Null symbol actions are run at evaluator creation time and the value of the result
at that point
becomes fixed as the null symbol value.
This is not the case with rule actions.
During the creation of the evaluator object,
rule actions are B<compiled into closures>.
During parse evaluation,
whenever a node for that rule needs its value recalculated,
the compiled rule closure is run.
A compiled rule closure
can produce a different value every time it runs.

I treat null symbol actions differently for efficiency.
They have no child values,
and a fixed value is usually what is wanted.
If you want to calculate a symbol's null value with a closure run at parse evaluation time,
the null symbol action can return a reference to a closure.
Rules with that nullable symbol in their right hand side
can then be set up to run that closure.

=head3 Evaluating Null Derivations

A null derivation may consist of many steps and may contain many symbols.
Marpa's rule is that the value of a null derivation is
the null symbol value of the B<highest null symbol> in that
derivation.
This section describes in detail how a parse is evaluated,
focusing on what happens when nulled symbols are involved.

The first step in evaluating a parse is to determine which nodes
B<count> for the purpose of evaluation, and which do not.
Marpa follows these principles:

=over 4

=item 1

The start node always counts.

=item 2

Nodes count if they derive a non-empty sentence.

=item 3

All other nodes do not count.

=item 4

In evaluating a parse, Marpa uses only nodes that count.

=back

These are all consequences of the principles above:

=over 4

=item 1

The value of null derivation is the value of the highest null symbol in it.

=item 2

A nulled node counts only if it is the start node.

=item 3

The value of a null parse is the null value of the start symbol.

=back

If you think some of the rules or symbols represented by nodes that don't count
are important in your grammar,
Marpa can probably accommodate your ideas.
First,
for every nullable symbol,
determine how to calculate the value which your semantics produces
when that nullable symbol is a "highest null symbol".
If it's a constant, write a null action for that symbol which returns that constant.
If your semantics do not produce a constant value by evaluator creation time,
write a null action which returns a reference to a closure
and arrange to have that closure run by the parent node.

=head3 Example

Suppose a grammar has these rules

=begin Marpa::Test::Display:

## start display
## next display
in_file($_, 'example/null_value.marpa');

=end Marpa::Test::Display:

    S: A, Y. q{ $_[0] . ", but " . $_[1] }. # Call me the start rule
    note: you can also call me Rule 0.

    A: . q{'A is missing'}. # Call me Rule 1

    A: B, C. q{"I'm sometimes null and sometimes not"}. # Call me Rule 2

    B: . q{'B is missing'}. # Call me Rule 3

    C: . q{'C is missing'}. # Call me Rule 4

    C: Y.  q{'C matches Y'}. # Call me Rule 5

    Y: /Z/. q{'Zorro was here'}. # Call me Rule 6


=begin Marpa::Test::Display:

## end display

=end Marpa::Test::Display:

In the above MDL, the Perl 5 regex "C</Z/>" occurs on the rhs of Rule 6.
Where a regex is on the rhs of a rule, MDL internally creates a terminal symbol
to match that regex in the input text.
In this example, the MDL internal terminal symbol that
matches input text using the regex
C</Z/> will be called C<Z>.

If the input text is the Perl 5 string "C<Z>",
the derivation is as follows:

=begin Marpa::Test::Display:

## skip 2 displays

=end Marpa::Test::Display:

    S -> A Y      (Rule 0)
      -> A "Z"    (Y produces "Z", by Rule 6)
      -> B C "Z"  (A produces B C, by Rule 2)
      -> B "Z"    (C produces the empty string, by Rule 4)
      -> "Z"      (B produces the empty string, by Rule 3)

The parse tree can be described as follows:

    Node 0 (root): S (2 children, nodes 1 and 4)
        Node 1: A (2 children, nodes 2 and 3)
	    Node 2: B (matches empty string)
	    Node 3: C (matches empty string)
	Node 4: Y (1 child, node 5)
	    Node 5: "Z" (terminal node)

Here's a table showing, for each node, its lhs symbol,
the sentence it derives, and
its value.

=begin Marpa::Test::Display:

## skip 2 displays

=end Marpa::Test::Display:

                        Symbol      Sentence     Value
                                    Derived

    Node 0:                S         "Z"         "A is missing, but Zorro is here"
        Node 1:            A         empty       "A is missing"
	    Node 2:        B         empty       No value
	    Node 3:        C         empty       No value
	Node 4:            Y         "Z"         "Zorro was here"
	    Node 5:        -         "Z"         "Z"

In this derivation,
nodes 1, 2 and 3 derive the empty sentence.
None of them are the start node so that none of them count.

Nodes 0, 4 and 5 all derive the same non-empty sentence, C<Z>,
so they all count.
Node 0 is the start node, so it would have counted in any case.

Since node 5 is a terminal node, it's value comes from the lexer.
Where the lexing is done with a Perl 5 regex,
the value will be the Perl 5 string that the regex matched.
In this case it's the string "C<Z>".

Node 4 is not nulled,
so it is evaluated normally, using the rule it represents.
That is rule 6.
The action for rule 6 returns "C<Zorro was here>", so that
is the value of node 4.
Node 4 has a child node, node 5, but rule 6's action pays no
attention to child values.
The action for each rule is free to use or not use child values.

Nodes 1, 2 and 3 don't count and will all remain unevaluated.
The only rule left to be evaluated
is node 0, the start node.
It is not nulled, so
its value is calculated using the action for the rule it
represents (rule 0).

Rule 0's action uses the values of its child nodes.
There are two child nodes and their values are
elements 0 and 1 in the C<@_> array of the action.
The child value represented by the symbol C<Y>,
C<< $_[1] >>, comes from node 4.
From the table above, we can see that that value was
"C<Zorro was here>".

The first child value is represented by the symbol C<A>,
which is nulled.
For nulled symbols, we must use the null symbol value.
Null symbol values for each symbol can be explicitly set
by specifying an rule action for an empty rule with that symbol
as its lhs.
For symbol C<A>,
this was done in Rule 1.
Rule 1's action evaluates to the Perl 5 string
"C<A is missing>".

Even though rule 1's action plays a role in calculating the value of this parse,
rule 1 is not actually used in the derivation.
No node in the derivation represents rule 1.
Rule 1's action is used because it is the null symbol action for
the symbol C<A>.

Now that we have both child values, we can use rule 0's action
to calculate the value of node 0.
That value is "C<A is missing, but Zorro was here>",
This becomes the value of C<S>, rule 0's left hand side symbol and
the start symbol of the grammar.
A parse has the value of its start symbol,
so "C<A is missing, but Zorro was here>" is also
the value of the parse.

=head2 Cloning

The C<new> constructor requires a recognizer object to be one of its arguments.
By default, the C<new> constructor clones the recognizer object.
This is done so that evaluators do not interfere with each other by
modifying the same data.
Cloning is the default behavior, and is always safe.

While safe, cloning does impose an overhead in memory and time.
This can be avoided by using the C<clone> option with the C<new>
constructor.
Not cloning is safe if you know that the recognizer object will not be shared by another evaluator.
You must also be sure that the
underlying grammar object is not being shared by multiple recognizers.

It is very common for a Marpa program to have a simple
structure, where no more than one recognizer is created from any grammar,
and no more than one evaluator is created from any recognizer.
When this is the case, cloning is unnecessary.

=head1 METHODS

=head2 new

=begin Marpa::Test::Display:

## next display
in_file($_, 't/equation_s.t');

=end Marpa::Test::Display:

    my $evaler = Marpa::Evaluator->new(
      { recognizer => $recce }
    );

Z<>

=begin Marpa::Test::Display:

## next display
in_file($_, 'author.t/misc.t');

=end Marpa::Test::Display:

    my $evaler = Marpa::Evaluator->new( {
        recce => $recce,
        end => $location,
        clone => 0,
    } );

The C<new> method's one, required, argument is a hash reference of named
arguments.
The C<new> method either returns a new evaluator object or throws an exception.
The C<recognizer> option is required,
Its value must be a recognizer object which has finished recognizing a text.
The C<recce> option is a synonym for the the C<recognizer> option.

By default,
parsing ends at the default end of parsing,
which was set in the recognizer.
If an C<end> option is specified, 
it will be used as the number of the earleme at which to end parsing.

If the C<clone> argument is set to 1,
C<new> clones the recognizer object, so that multiple
evaluators do not interfere with each other's data.
This is the default and is always safe.
If C<clone> is set to 0, the evaluator will work directly with
the recognizer object which was its argument.
See L<above|/"Cloning"> for more detail.

Marpa options can also
be named arguments to C<new>.
For these, see L<Marpa::Doc::Options>.

=head2 set

=begin Marpa::Test::Display:

## next display
is_file($_, 'author.t/misc.t', 'evaler set snippet')

=end Marpa::Test::Display:

    $evaler->set( { trace_values => 1 } );

The C<set> method takes as its one, required, argument a reference to a hash of named arguments.
It allows Marpa options
to be specified for an evaluator object.
Relatively few Marpa options are not available at
evaluation time.
The options which are available
are mainly those which control evaluation time tracing.
C<set> either returns true or throws an exception.

=head2 value

=begin Marpa::Test::Display:

## next display
in_file($_, 't/ah2.t');

=end Marpa::Test::Display:

    my $result = $evaler->value();

Iterates the evaluator object, returning a reference to the value of the next parse.
If there are no more parses, returns undefined.
Successful parses may evaluate to a Perl 5 undefined,
which the C<value> method will return as a reference to an undefined.
Failures are thrown as exceptions.

When the order of parses is important,
it may be manipulated by assigning priorities to the rules and
terminals.
If a symbol can both match a token and derive a rule,
the token match always takes priority.
Otherwise the parse order is implementation dependent.

A failed parse does not always show up as an exhausted parse in the recognizer.
Just because the recognizer was active when it was used to create
the evaluator, does not mean that the input matches the grammar.
If it does not match, there will be no parses and the C<value> method will
return undefined the first time it is called.

=head1 SUPPORT

See the L<support section|Marpa/SUPPORT> in the main module.

=head1 AUTHOR

Jeffrey Kegler

=head1 LICENSE AND COPYRIGHT

Copyright 2007 - 2009 Jeffrey Kegler

This program is free software; you can redistribute
it and/or modify it under the same terms as Perl 5.10.0.

=cut
