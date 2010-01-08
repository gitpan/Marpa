package Marpa::Internal::Recce_Value;

use 5.010;
use warnings;
no warnings qw(recursion qw);
use strict;
use integer;

# use Smart::Comments '-ENV';

### Using smart comments <where>...

use Scalar::Util;
use List::Util;
use English qw( -no_match_vars );
use Data::Dumper;
use Marpa::Internal;

# Returns false if no parse
sub Marpa::Recognizer::value {
    my ( $self, @arg_hashes ) = @_;

    my $parse_set_arg;
    my $trace_values = 0;

    # default settings
    local $Marpa::Internal::EXPLICIT_CLOSURES = {};
    local $Marpa::Internal::TRACE_ACTIONS     = 0;

    for my $arg_hash (@arg_hashes) {

        if ( defined $arg_hash->{end} ) {
            $parse_set_arg = $arg_hash->{end};
        }

        if ( defined $arg_hash->{closures} ) {
            $Marpa::Internal::EXPLICIT_CLOSURES = $arg_hash->{closures};
        }

        if ( defined $arg_hash->{trace_actions} ) {
            $Marpa::Internal::TRACE_ACTIONS = $arg_hash->{trace_actions};
        }

        if ( defined $arg_hash->{trace_values} ) {
            $trace_values = $arg_hash->{trace_actions};
        }

    } ## end for my $arg_hash (@arg_hashes)

    local $Marpa::Internal::TRACE_FH =
        $self->[Marpa::Internal::Recognizer::TRACE_FILE_HANDLE];

    my $grammar = $self->[Marpa::Internal::Recognizer::GRAMMAR];
    my $action_object_class =
        $grammar->[Marpa::Internal::Grammar::ACTION_OBJECT];
    my $earley_sets = $self->[Marpa::Internal::Recognizer::EARLEY_SETS];
    Marpa::exception(
        "Attempt to use quick evaluator on an infinitely ambiguous grammar\n",
        "  Rewrite to remove cycles, or\n",
        "  Use the power evaluator\n"
    ) if $grammar->[Marpa::Internal::Grammar::IS_INFINITE];

    my $furthest_earleme =
        $self->[Marpa::Internal::Recognizer::FURTHEST_EARLEME];
    my $last_completed_earleme =
        $self->[Marpa::Internal::Recognizer::LAST_COMPLETED_EARLEME];
    Marpa::exception(
        "Attempt to evaluate incompletely recognized parse:\n",
        "  Last token ends at location $furthest_earleme\n",
        "  Recognition done only as far as location $last_completed_earleme\n"
    ) if $furthest_earleme > $last_completed_earleme;

    my $rules   = $grammar->[Marpa::Internal::Grammar::RULES];
    my $symbols = $grammar->[Marpa::Internal::Grammar::SYMBOLS];

    my $and_nodes = [];

    my $current_parse_set = $parse_set_arg
        // $self->[Marpa::Internal::Recognizer::FURTHEST_EARLEME];

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

    my $null_values;
    $null_values = Marpa::Internal::Evaluator::set_null_values($grammar);

    my $evaluator_rules = Marpa::Internal::Evaluator::set_actions($grammar);

    my $action_object_constructor;

    if (defined(
            my $action_object =
                $grammar->[Marpa::Internal::Grammar::ACTION_OBJECT]
        )
        )
    {
        my $constructor_name = $action_object . q{::new};
        my $closure = Marpa::Internal::Evaluator::resolve_semantics( $grammar,
            $constructor_name );
        Marpa::exception(qq{Could not find constructor "$constructor_name"})
            if not defined $closure;
        $action_object_constructor = $closure;
    } ## end if ( defined( my $action_object = $grammar->[...]))

    my $start_symbol = $start_rule->[Marpa::Internal::Rule::LHS];
    my ( $nulling, $symbol_id ) =
        @{$start_symbol}[ Marpa::Internal::Symbol::NULLING,
        Marpa::Internal::Symbol::ID, ];
    my $start_null_value = $null_values->[$symbol_id];

    # null parse as special case?

    my $start_sapling = [];
    $start_sapling->[Marpa::Internal::Or_Sapling::ITEM] = $start_item;
    $start_sapling->[Marpa::Internal::Or_Sapling::CHILD_LHS_SYMBOL] =
        $start_symbol;

    my @or_saplings = ($start_sapling);
    my @stack       = ();

    OR_SAPLING: while ( my $or_sapling = pop @or_saplings ) {

        my $item = $or_sapling->[Marpa::Internal::Or_Sapling::ITEM];
        my $child_lhs_symbol =
            $or_sapling->[Marpa::Internal::Or_Sapling::CHILD_LHS_SYMBOL];
        my $rule     = $or_sapling->[Marpa::Internal::Or_Sapling::RULE];
        my $position = $or_sapling->[Marpa::Internal::Or_Sapling::POSITION];

        # If we don't have a current rule, we need to get one or
        # more rules, and deduce the position and a new symbol from
        # them.
        my $is_kernel_or_node = defined $position;
        my $sapling_rule;
        my $sapling_position;
        my $symbol;
        my $value_processing;

        if ( defined $position ) {

            # Kernel or-node: We have a rule and a position.
            # get the current symbol

            $position--;
            $symbol = $rule->[Marpa::Internal::Rule::RHS]->[$position];

            $sapling_rule     = $rule;
            $sapling_position = $position;

        } ## end if ( defined $position )
        else {    # Closure or-node.

            my $child_lhs_id =
                $child_lhs_symbol->[Marpa::Internal::Symbol::ID];
            my $state = $item->[Marpa::Internal::Earley_Item::STATE];

            # ================
            # CHOICE POINT HERE
            # ================
            #
            # Arbitarily picks the first complete rule for
            # the QDFA state.

            $sapling_rule =
                $state->[Marpa::Internal::QDFA::COMPLETE_RULES]
                ->[$child_lhs_id]->[0];

            my $rhs = $sapling_rule->[Marpa::Internal::Rule::RHS];

            $sapling_position = @{$rhs} - 1;
            $symbol           = $rhs->[$sapling_position];
            $value_processing =
                $evaluator_rules->[ $sapling_rule->[Marpa::Internal::Rule::ID]
                ];

        }    # closure or-node

        my $start_earleme = $item->[Marpa::Internal::Earley_Item::PARENT];
        my $end_earleme   = $item->[Marpa::Internal::Earley_Item::SET];

        my $rule_id     = $sapling_rule->[Marpa::Internal::Rule::ID];
        my $rhs         = $sapling_rule->[Marpa::Internal::Rule::RHS];
        my $rule_length = @{$rhs};

        my ( $predecessor, $cause, $token, $value_ref );

        FIND_OR_BUDS: {
            if ( $symbol->[Marpa::Internal::Symbol::NULLING] ) {
                my $nulling_symbol_id =
                    $symbol->[Marpa::Internal::Symbol::ID];
                $value_ref   = \$null_values->[$nulling_symbol_id];
                $token       = $symbol;
                $predecessor = $item;
                last FIND_OR_BUDS;
            } ## end if ( $symbol->[Marpa::Internal::Symbol::NULLING] )

            # CHOICE POINT
            # Arbitrarily pick the first token,
            # if more than one

            my $token_choice =
                $item->[Marpa::Internal::Earley_Item::TOKENS]->[0];
            if ( defined $token_choice ) {
                ( $predecessor, $token, $value_ref ) = @{$token_choice};
                last FIND_OR_BUDS;
            }

            # CHOICE POINT

            # If here, no tokens and not nulling,
            # so there must be at least one link choice.
            # Arbitrarily pick the first.
            ( $predecessor, $cause, ) =
                @{ $item->[Marpa::Internal::Earley_Item::LINKS]->[0] };
        } ## end FIND_OR_BUDS:

        if ( $sapling_position > 0 ) {

            my $sapling = [];
            @{$sapling}[
                Marpa::Internal::Or_Sapling::RULE,
                Marpa::Internal::Or_Sapling::POSITION,
                Marpa::Internal::Or_Sapling::ITEM,
                ]
                = ( $sapling_rule, $sapling_position, $predecessor, );

            push @or_saplings, $sapling;

        }    # if sapling_position > 0

        if ( defined $cause ) {

            my $sapling = [];
            @{$sapling}[
                Marpa::Internal::Or_Sapling::CHILD_LHS_SYMBOL,
                Marpa::Internal::Or_Sapling::ITEM,
                ]
                = ( $symbol, $cause, );

            push @or_saplings, $sapling;

        }    # if cause

        my $and_node = [];
        $#{$and_node} = Marpa::Internal::And_Node::LAST_FIELD;

        $and_node->[Marpa::Internal::And_Node::TOKEN]     = $token;
        $and_node->[Marpa::Internal::And_Node::VALUE_REF] = $value_ref;
        $and_node->[Marpa::Internal::And_Node::RULE_ID]   = $rule_id;

        $and_node->[Marpa::Internal::And_Node::VALUE_OPS] = $value_processing;

        $and_node->[Marpa::Internal::And_Node::POSITION] = $sapling_position;
        $and_node->[Marpa::Internal::And_Node::START_EARLEME] =
            $start_earleme;
        $and_node->[Marpa::Internal::And_Node::CAUSE_EARLEME] =
              $predecessor
            ? $item->[Marpa::Internal::Earley_Item::SET]
            : $start_earleme;
        $and_node->[Marpa::Internal::And_Node::END_EARLEME] = $end_earleme;
        my $id = $and_node->[Marpa::Internal::And_Node::ID] = scalar @stack;
        Marpa::exception("Too many and-nodes for evaluator: $id")
            if $id & ~(Marpa::Internal::N_FORMAT_MAX);
        push @stack, $and_node;

    }    # OR_SAPLING

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

    return Marpa::Internal::Evaluator::evaluate( $grammar, $action_object,
        \@stack, $trace_values );

} ## end sub Marpa::Recognizer::value

1;
