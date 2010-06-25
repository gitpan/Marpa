package Marpa::Internal::Recce_Value;

use 5.010;
use warnings;
no warnings qw(recursion qw);
use strict;
use integer;

use Scalar::Util;
use List::Util;
use English qw( -no_match_vars );
use Data::Dumper;
use Marpa::Internal;

# Returns false if no parse
sub Marpa::Recognizer::value {
    my ( $self, @arg_hashes ) = @_;

    my $parse_set_arg = $self->[Marpa::Internal::Recognizer::END];
    my $trace_values  = $self->[Marpa::Internal::Recognizer::TRACE_VALUES]
        // 0;

    # default settings
    local $Marpa::Internal::EXPLICIT_CLOSURES =
        $self->[Marpa::Internal::Recognizer::CLOSURES] // {};
    local $Marpa::Internal::TRACE_ACTIONS =
        $self->[Marpa::Internal::Recognizer::TRACE_ACTIONS] // 0;

    local $Marpa::Internal::TRACE_FH =
        $self->[Marpa::Internal::Recognizer::TRACE_FILE_HANDLE];

    if ( $self->[Marpa::Internal::Recognizer::SINGLE_PARSE_MODE] ) {
        Marpa::exception(
            qq{Arguments were passed directly to value() in a previous call\n},
            qq{Only one call to value() is allowed per recognizer when arguments are passed directly\n},
            qq{This is the second call to value()\n}
        );
    } ## end if ( $self->[Marpa::Internal::Recognizer::SINGLE_PARSE_MODE...])

    for my $arg_hash (@arg_hashes) {

        if ( exists $arg_hash->{end} ) {
            $self->[Marpa::Internal::Recognizer::SINGLE_PARSE_MODE] = 1;
            $parse_set_arg = $arg_hash->{end};
            delete $arg_hash->{end};
        }

        if ( exists $arg_hash->{closures} ) {
            $self->[Marpa::Internal::Recognizer::SINGLE_PARSE_MODE] = 1;
            $Marpa::Internal::EXPLICIT_CLOSURES = $arg_hash->{closures};
            while ( my ( $action, $closure ) =
                each %{$Marpa::Internal::EXPLICIT_CLOSURES} )
            {
                Marpa::exception(qq{Bad closure for action "$action"})
                    if ref $closure ne 'CODE';
            } ## end while ( my ( $action, $closure ) = each %{...})
            delete $arg_hash->{closures};
        } ## end if ( exists $arg_hash->{closures} )

        if ( exists $arg_hash->{trace_actions} ) {
            $self->[Marpa::Internal::Recognizer::SINGLE_PARSE_MODE] = 1;
            $Marpa::Internal::TRACE_ACTIONS = $arg_hash->{trace_actions};
            delete $arg_hash->{trace_actions};
        }

        if ( exists $arg_hash->{trace_values} ) {
            $self->[Marpa::Internal::Recognizer::SINGLE_PARSE_MODE] = 1;
            $trace_values = $arg_hash->{trace_values};
            delete $arg_hash->{trace_values};
        }

        if ( exists $arg_hash->{trace_fh} ) {
            $Marpa::Internal::TRACE_FH = $arg_hash->{trace_fh};
            delete $arg_hash->{trace_fh};
        }

        my @unknown_arg_names = keys %{$arg_hash};
        Marpa::exception(
            'Unknown named argument(s) to Marpa::Recognizer::value: ',
            ( join q{ }, @unknown_arg_names ) )
            if @unknown_arg_names;

    } ## end for my $arg_hash (@arg_hashes)

    my $grammar = $self->[Marpa::Internal::Recognizer::GRAMMAR];
    my $action_object_class =
        $grammar->[Marpa::Internal::Grammar::ACTION_OBJECT];
    my $earley_sets = $self->[Marpa::Internal::Recognizer::EARLEY_SETS];
    my $earley_hash = $self->[Marpa::Internal::Recognizer::EARLEY_HASH];
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
        $start_rule  = $start_state->[Marpa::Internal::AHFA::START_RULE];
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

    my $start_symbol = $start_rule->[Marpa::Internal::Rule::LHS];
    my ( $nulling, $symbol_id ) =
        @{$start_symbol}[ Marpa::Internal::Symbol::NULLING,
        Marpa::Internal::Symbol::ID, ];

    # null parse is special case
    if ($nulling) {

        my $and_node = [];
        $#{$and_node} = Marpa::Internal::And_Node::LAST_FIELD;

        $and_node->[Marpa::Internal::And_Node::VALUE_REF] =
            \( $null_values->[$symbol_id] );
        $and_node->[Marpa::Internal::And_Node::RULE_ID] = $start_rule_id;
        $and_node->[Marpa::Internal::And_Node::VALUE_OPS] =
            $evaluator_rules->[$start_rule_id];

        $and_node->[Marpa::Internal::And_Node::POSITION]      = 0;
        $and_node->[Marpa::Internal::And_Node::START_EARLEME] = 0;
        $and_node->[Marpa::Internal::And_Node::CAUSE_EARLEME] = 0;
        $and_node->[Marpa::Internal::And_Node::END_EARLEME]   = 0;
        $and_node->[Marpa::Internal::And_Node::ID]            = 0;
        $and_node->[Marpa::Internal::And_Node::TAG] =
            $start_item->[Marpa::Internal::Earley_Item::NAME] . 'o0a0';

        return Marpa::Internal::Evaluator::evaluate( $grammar, $action_object,
            [$and_node], $trace_values );

    } ## end if ($nulling)

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

    my @or_saplings = ($start_sapling);
    my @stack       = ();

    OR_SAPLING: while ( my $or_sapling = pop @or_saplings ) {

        my $sapling_name   = $or_sapling->[Marpa::Internal::Or_Sapling::NAME];
        my $item           = $or_sapling->[Marpa::Internal::Or_Sapling::ITEM];
        my $or_sapling_set = $item->[Marpa::Internal::Earley_Item::SET];
        my $leo_links      = $item->[Marpa::Internal::Earley_Item::LEO_LINKS]
            // [];

        # If this is a Leo completion, translate the Leo links
        for my $leo_link ( @{$leo_links} ) {

            my ( $leo_item, $cause, $token_name, $token_value ) =
                @{$leo_link};

            my ( $next_leo_item, $leo_base_item ) =
                @{ $leo_item->[Marpa::Internal::Earley_Item::LINKS]->[0] };

            my $next_tokens = [];
            if ($token_name) {
                push @{$next_tokens},
                    [ $leo_base_item, $token_name, $token_value ];
            }
            my $next_links = [];
            if ($cause) {
                push @{$next_links}, [ $leo_base_item, $cause ];
            }

            LEO_ITEM: for ( ;; ) {

                if ( not $next_leo_item ) {

                    push @{ $item->[Marpa::Internal::Earley_Item::LINKS] },
                        @{$next_links};
                    push @{ $item->[Marpa::Internal::Earley_Item::TOKENS] },
                        @{$next_tokens};

                    # Now that the Leo links are translated, remove them
                    $item->[Marpa::Internal::Earley_Item::LEO_LINKS] = undef;
                    last LEO_ITEM;

                } ## end if ( not $next_leo_item )

                my $state = $leo_item
                    ->[Marpa::Internal::Earley_Item::LEO_ACTUAL_STATE];
                my $origin =
                    $next_leo_item->[Marpa::Internal::Earley_Item::SET];
                my $name = sprintf
                    ## no critic (ValuesAndExpressions::RequireInterpolationOfMetachars)
                    'S%d@%d-%d',
                    ## use critic
                    $state->[Marpa::Internal::AHFA::ID],
                    $origin,
                    $or_sapling_set;
                my $target_item = $earley_hash->{$name};
                if ( not defined $target_item ) {
                    $target_item = [];
                    $target_item->[Marpa::Internal::Earley_Item::NAME] =
                        $name;
                    $target_item->[Marpa::Internal::Earley_Item::PARENT] =
                        $origin;
                    $target_item->[Marpa::Internal::Earley_Item::STATE] =
                        $state;
                    $target_item->[Marpa::Internal::Earley_Item::TOKENS] = [];
                    $target_item->[Marpa::Internal::Earley_Item::LINKS]  = [];
                    $target_item->[Marpa::Internal::Earley_Item::SET] =
                        $or_sapling_set;
                    $earley_hash->{$name} = $target_item;
                    push @{ $earley_sets->[$or_sapling_set] }, $target_item;
                } ## end if ( not defined $target_item )

                push @{ $target_item->[Marpa::Internal::Earley_Item::LINKS] },
                    @{$next_links};
                push @{ $target_item->[Marpa::Internal::Earley_Item::TOKENS]
                    }, @{$next_tokens};

                $leo_item = $next_leo_item;

                ( $next_leo_item, $leo_base_item ) =
                    @{ $leo_item->[Marpa::Internal::Earley_Item::LINKS]->[0]
                    };

                $next_tokens = [];
                $next_links = [ [ $leo_base_item, $target_item ] ];

            } ## end for ( ;; )
        } ## end for my $leo_link ( @{$leo_links} )

        my $child_lhs_symbol =
            $or_sapling->[Marpa::Internal::Or_Sapling::CHILD_LHS_SYMBOL];
        my $rule = $or_sapling->[Marpa::Internal::Or_Sapling::RULE];
        my $sapling_position =
            $or_sapling->[Marpa::Internal::Or_Sapling::POSITION];

        # If we don't have a current rule, we need to get one or
        # more rules, and deduce the position and a new symbol from
        # them.
        my $sapling_rule;
        my $symbol;
        my $value_processing;

        if ( defined $sapling_position ) {

            # Kernel or-node: We have a rule and a position.
            # get the current symbol

            $sapling_position--;
            $symbol =
                $rule->[Marpa::Internal::Rule::RHS]->[$sapling_position];
            $sapling_rule = $rule;

        } ## end if ( defined $sapling_position )
        else {    # Closure or-node.

            my $child_lhs_id =
                $child_lhs_symbol->[Marpa::Internal::Symbol::ID];
            my $state = $item->[Marpa::Internal::Earley_Item::STATE];

            # ================
            # CHOICE POINT HERE
            # ================
            #
            # Arbitarily picks the first complete rule for
            # the AHFA state.

            $sapling_rule =
                $state->[Marpa::Internal::AHFA::COMPLETE_RULES]
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

        my $rule_id = $sapling_rule->[Marpa::Internal::Rule::ID];

        my ( $predecessor, $cause, $token_name, $value_ref );

        FIND_OR_BUDS: {
            if ( $symbol->[Marpa::Internal::Symbol::NULLING] ) {
                my $nulling_symbol_id =
                    $symbol->[Marpa::Internal::Symbol::ID];
                $value_ref   = \$null_values->[$nulling_symbol_id];
                $token_name  = $symbol->[Marpa::Internal::Symbol::NAME];
                $predecessor = $item;
                last FIND_OR_BUDS;
            } ## end if ( $symbol->[Marpa::Internal::Symbol::NULLING] )

            # CHOICE POINT
            # Arbitrarily pick the first token,
            # if more than one

            my $token_choice =
                $item->[Marpa::Internal::Earley_Item::TOKENS]->[0];
            if ( defined $token_choice ) {
                ( $predecessor, $token_name, $value_ref ) = @{$token_choice};
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

            my $predecessor_name =
                $predecessor->[Marpa::Internal::Earley_Item::NAME]
                . "R$rule_id:$sapling_position";

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

        }    # if sapling_position > 0

        if ( defined $cause ) {

            my $cause_symbol_id = $symbol->[Marpa::Internal::Symbol::ID];

            my $cause_name =
                  $cause->[Marpa::Internal::Earley_Item::NAME] . 'L'
                . $cause_symbol_id;

            my $sapling = [];
            @{$sapling}[
                Marpa::Internal::Or_Sapling::NAME,
                Marpa::Internal::Or_Sapling::CHILD_LHS_SYMBOL,
                Marpa::Internal::Or_Sapling::ITEM,
                ]
                = ( $cause_name, $symbol, $cause, );

            push @or_saplings, $sapling;

        }    # if cause

        my $and_node = [];
        $#{$and_node} = Marpa::Internal::And_Node::LAST_FIELD;

        $and_node->[Marpa::Internal::And_Node::TOKEN_NAME] = $token_name;
        $and_node->[Marpa::Internal::And_Node::VALUE_REF]  = $value_ref;
        $and_node->[Marpa::Internal::And_Node::RULE_ID]    = $rule_id;

        $and_node->[Marpa::Internal::And_Node::VALUE_OPS] = $value_processing;

        $and_node->[Marpa::Internal::And_Node::POSITION] = $sapling_position;
        $and_node->[Marpa::Internal::And_Node::START_EARLEME] =
            $start_earleme;
        $and_node->[Marpa::Internal::And_Node::CAUSE_EARLEME] =
              $predecessor
            ? $predecessor->[Marpa::Internal::Earley_Item::SET]
            : $start_earleme;
        $and_node->[Marpa::Internal::And_Node::END_EARLEME] = $end_earleme;
        my $id = $and_node->[Marpa::Internal::And_Node::ID] = scalar @stack;
        Marpa::exception("Too many and-nodes for evaluator: $id")
            if $id & ~(Marpa::Internal::N_FORMAT_MAX);
        $and_node->[Marpa::Internal::And_Node::TAG] =
            $sapling_name . 'o' . $id . 'a' . $id;

        push @stack, $and_node;

    }    # OR_SAPLING

    return Marpa::Internal::Evaluator::evaluate( $grammar, $action_object,
        \@stack, $trace_values );

} ## end sub Marpa::Recognizer::value

1;
