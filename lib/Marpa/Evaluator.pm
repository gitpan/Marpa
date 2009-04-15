package Marpa::Internal::Evaluator;
use 5.010;

use warnings;
no warnings 'recursion';
use strict;
use integer;
use List::Util qw(min);
use English qw( -no_match_vars );

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

package Marpa::Internal;

use Marpa::Offset Or_Sapling => qw(NAME ITEM RULE POSITION CHILD_LHS_SYMBOL);

use Marpa::Offset And_Node =>
    qw(NAME ID PARENT_OR_NODE PREDECESSOR CAUSE VALUE_REF PERL_CLOSURE END_EARLEME ARGC RULE POSITION);

use Marpa::Offset Or_Node =>
    qw(NAME ID PARENT_OR_NODE AND_NODES IS_COMPLETED START_EARLEME AND_CHOICE CHOICE_MAP MAP_IX PARENT_OR_CHOICES);

# IS_COMPLETED - is this a completed or-node?

use Marpa::Offset Tree_Node => qw(OR_NODE CHOICE PREDECESSOR CAUSE DEPTH
    PERL_CLOSURE ARGC VALUE_REF RULE POSITION PARENT);

use Marpa::Offset Evaluator =>
    qw(RECOGNIZER PARSE_COUNT OR_NODES TREE RULE_DATA PACKAGE NULL_VALUES CYCLES
    OR_NODES_BY_EARLEME COMPLETIONS_BY_EARLEME CHOICE_POINTS
);

# PARSE_COUNT  number of parses in an ambiguous parse
# TREE         current evaluation tree

package Marpa::Internal::Evaluator;

use Marpa::Offset Rule => qw(CODE PERL_CLOSURE);

use Scalar::Util qw(weaken);
use Data::Dumper;
use Marpa::Internal;
our @CARP_NOT = @Marpa::Internal::CARP_NOT;

sub run_preamble {
    my $grammar = shift;
    my $package = shift;

    my $preamble = $grammar->[Marpa::Internal::Grammar::PREAMBLE];
    return unless defined $preamble;

    my $code = 'package ' . $package . ";\n" . $preamble;
    my $eval_ok;
    my @warnings;
    my $old_warn_handler = $SIG{__WARN__};
    $SIG{__WARN__} = sub { push @warnings, [ $_[0], ( caller 0 ) ]; };

    ## no critic (BuiltinFunctions::ProhibitStringyEval)
    $eval_ok = eval $code;
    ## use critic

    $SIG{__WARN__} = $old_warn_handler;

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
                  '$null_value = do {' . "\n"
                . "    package $package;\n"
                . $action . "};\n" . "1;\n";
            my @warnings;
            my $old_warn_handler = $SIG{__WARN__};
            $SIG{__WARN__} = sub { push @warnings, [ $_[0], ( caller 0 ) ]; };

            ## no critic (BuiltinFunctions::ProhibitStringyEval)
            my $eval_ok = eval $code;
            ## use critic

            $SIG{__WARN__} = $old_warn_handler;

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
            unless $symbol->[Marpa::Internal::Symbol::IS_CHAF_NULLING];
        set_null_symbol_value( $null_values, $symbol );
    }

    if ($trace_actions) {
        SYMBOL: for my $symbol ( @{$symbols} ) {
            next SYMBOL
                unless $symbol->[Marpa::Internal::Symbol::IS_CHAF_NULLING];

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

        next RULE unless $rule->[Marpa::Internal::Rule::USEFUL];

        my $action = $rule->[Marpa::Internal::Rule::ACTION];

        ACTION: {

            $action //= $default_action;
            last ACTION unless defined $action;

            # HAS_CHAF_RHS and HAS_CHAF_LHS would work well as a bit
            # mask in a C implementation
            my $has_chaf_lhs = $rule->[Marpa::Internal::Rule::HAS_CHAF_LHS];
            my $has_chaf_rhs = $rule->[Marpa::Internal::Rule::HAS_CHAF_RHS];

            last ACTION unless $has_chaf_lhs or $has_chaf_rhs;

            if ( $has_chaf_rhs and $has_chaf_lhs ) {
                $action = q{ \@_; };
                last ACTION;
            }

            # At this point has chaf rhs or lhs but not both
            if ($has_chaf_lhs) {

                $action = q{push @_, [];} . "\n" . q{\@_} . "\n";
                last ACTION;

            } ## end if ($has_chaf_lhs)

            # at this point must have chaf rhs and not a chaf lhs

            $action =
                  "    TAIL: for (;;) {\n"
                . q<        my $tail = pop @_;> . "\n"
                . q<        last TAIL unless scalar @{$tail};> . "\n"
                . q<        push @_, @{$tail};> . "\n"
                . "    } # TAIL\n"
                . $action;

        }    # ACTION

        my $rule_id = $rule->[Marpa::Internal::Rule::ID];

        if ( not defined $action ) {

            if ($trace_actions) {
                print {$trace_fh} 'Setting action for rule ',
                    Marpa::brief_rule($rule), " to undef by default\n"
                    or Marpa::exception('Could not print to trace file');
            }

            my $rule_datum;
            $rule_datum->[Marpa::Internal::Evaluator::Rule::CODE] =
                'default to undef';
            $rule_datum->[Marpa::Internal::Evaluator::Rule::PERL_CLOSURE] =
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
        {
            my $old_warn_handler = $SIG{__WARN__};
            my @warnings;
            $SIG{__WARN__} = sub { push @warnings, [ $_[0], ( caller 0 ) ]; };

            ## no critic (BuiltinFunctions::ProhibitStringyEval)
            $closure = eval $code;
            ## use critic

            $SIG{__WARN__} = $old_warn_handler;

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
        }

        my $rule_datum;
        $rule_datum->[Marpa::Internal::Evaluator::Rule::CODE] = $code;
        $rule_datum->[Marpa::Internal::Evaluator::Rule::PERL_CLOSURE] =
            $closure;

        $rule_data->[$rule_id] = $rule_datum;

    }    # RULE

    return $rule_data;

}    # set_actions

# Returns false if no parse
sub Marpa::Evaluator::new {
    my $class = shift;
    my $args  = shift;

    my $self = bless [], $class;

    my $recce;
    RECCE_ARG_NAME: for my $recce_arg_name (qw(recognizer recce)) {
        my $arg_value = $args->{$recce_arg_name};
        delete $args->{$recce_arg_name};
        next RECCE_ARG_NAME unless defined $arg_value;
        Marpa::exception('recognizer specified twice') if defined $recce;
        $recce = $arg_value;
    } ## end for my $recce_arg_name (qw(recognizer recce))
    Marpa::exception('No recognizer specified') unless defined $recce;

    my $recce_class = ref $recce;
    Marpa::exception(
        "${class}::new() recognizer arg has wrong class: $recce_class")
        unless $recce_class eq 'Marpa::Recognizer';

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

    if ($tracing) {
        $trace_fh = $grammar->[Marpa::Internal::Grammar::TRACE_FILE_HANDLE];
        $trace_iterations =
            $grammar->[Marpa::Internal::Grammar::TRACE_ITERATIONS];
    }

    $self->[Marpa::Internal::Evaluator::PARSE_COUNT] = 0;
    $self->[Marpa::Internal::Evaluator::OR_NODES]    = [];
    $self->[Marpa::Internal::Evaluator::CYCLES]      = {};

    my $current_parse_set = $parse_set_arg
        // $recce->[Marpa::Internal::Recognizer::CURRENT_SET];

    # Look for the start item and start rule
    my $earley_set = $earley_sets->[$current_parse_set];

    my $start_item;
    my $start_rule;
    my $start_state;

    EARLEY_ITEM: for my $item ( @{$earley_set} ) {
        $start_state = $item->[Marpa::Internal::Earley_item::STATE];
        $start_rule  = $start_state->[Marpa::Internal::QDFA::START_RULE];
        next EARLEY_ITEM unless $start_rule;
        $start_item = $item;
        last EARLEY_ITEM;
    } ## end for my $item ( @{$earley_set} )

    return unless $start_rule;

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
        my $and_node = [];

        my $closure =
            $rule_data->[ $start_rule->[Marpa::Internal::Rule::ID] ]
            ->[Marpa::Internal::Evaluator::Rule::PERL_CLOSURE];

        @{$and_node}[
            Marpa::Internal::And_Node::VALUE_REF,
            Marpa::Internal::And_Node::PERL_CLOSURE,
            Marpa::Internal::And_Node::ARGC,
            Marpa::Internal::And_Node::RULE,
            Marpa::Internal::And_Node::POSITION,
            ]
            = (
            \$start_null_value, $closure,
            ( scalar @{ $start_rule->[Marpa::Internal::Rule::RHS] } ),
            $start_rule, 0,
            );

        my $or_node      = [];
        my $or_node_name = $or_node->[Marpa::Internal::Or_Node::NAME] =
            $start_item->[Marpa::Internal::Earley_item::NAME];
        $or_node->[Marpa::Internal::Or_Node::AND_NODES] = [$and_node];
        $and_node->[Marpa::Internal::And_Node::NAME] = $or_node_name . '[0]';

        $self->[OR_NODES] = [$or_node];

        return $self;

    }    # if $nulling

    my @or_saplings;
    my %or_node_by_name;
    my $start_sapling = [];
    {
        my $start_name = $start_item->[Marpa::Internal::Earley_item::NAME];
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

        last OR_SAPLING unless defined $item;

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
            my $state = $item->[Marpa::Internal::Earley_item::STATE];
            for my $rule (
                @{  $state->[Marpa::Internal::QDFA::COMPLETE_RULES]
                        ->[$child_lhs_id];
                }
                )
            {

                my $rhs = $rule->[Marpa::Internal::Rule::RHS];
                my $closure =
                    $rule_data->[ $rule->[Marpa::Internal::Rule::ID] ]
                    ->[Marpa::Internal::Evaluator::Rule::PERL_CLOSURE];

                my $last_position = @{$rhs} - 1;
                push @and_saplings,
                    [
                    $rule,                  $last_position,
                    $rhs->[$last_position], $closure
                    ];

            }    # for my $rule

        }    # closure or-node

        my $start_earleme = $item->[Marpa::Internal::Earley_item::PARENT];
        my $end_earleme   = $item->[Marpa::Internal::Earley_item::SET];

        my @and_nodes;

        my $item_name = $item->[Marpa::Internal::Earley_item::NAME];

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
                            @{ $item->[Marpa::Internal::Earley_item::TOKENS] }
                    ),
                    (   map { [ $_->[0], $_->[1] ] }
                            @{ $item->[Marpa::Internal::Earley_item::LINKS] }
                    )
                );
            } ## end else [ if ( $symbol->[Marpa::Internal::Symbol::NULLING] )

            for my $or_bud (@or_bud_list) {

                my ( $predecessor, $cause, $value_ref ) = @{$or_bud};

                my $predecessor_name;

                if ( $sapling_position > 0 ) {

                    $predecessor_name =
                        $predecessor->[Marpa::Internal::Earley_item::NAME]
                        . "R$rule_id:$sapling_position";

                    unless ( $predecessor_name ~~ %or_node_by_name ) {

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
                          $cause->[Marpa::Internal::Earley_item::NAME] . 'L'
                        . $cause_symbol_id;

                    unless ( $cause_name ~~ %or_node_by_name ) {

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
                $and_node->[Marpa::Internal::And_Node::NAME] =
                    ( $sapling_name . '[' . ( scalar @and_nodes ) . ']' );

                push @and_nodes, $and_node;

            }    # for my $or_bud

        }    # for my $and_sapling

        my $or_node = [];
        $or_node->[Marpa::Internal::Or_Node::NAME]      = $sapling_name;
        $or_node->[Marpa::Internal::Or_Node::AND_NODES] = \@and_nodes;
        weaken( $_->[Marpa::Internal::And_Node::PARENT_OR_NODE] = $or_node )
            for @and_nodes;
        $or_node->[Marpa::Internal::Or_Node::IS_COMPLETED] =
            not $is_kernel_or_node;
        $or_node->[Marpa::Internal::Or_Node::START_EARLEME] = $start_earleme;
        push @{ $self->[Marpa::Internal::Evaluator::OR_NODES] }, $or_node;
        $or_node_by_name{$sapling_name} = $or_node;

    }    # OR_SAPLING

    # resolve links in the bocage
    for my $and_node ( map { @{ $_->[Marpa::Internal::Or_Node::AND_NODES] } }
        @{ $self->[OR_NODES] } )
    {
        FIELD:
        for my $field (
            Marpa::Internal::And_Node::PREDECESSOR,
            Marpa::Internal::And_Node::CAUSE,
            )
        {
            my $name = $and_node->[$field];
            next FIELD unless defined $name;
            my $or_node = $or_node_by_name{$name};
            $and_node->[$field] = $or_node;
        } ## end for my $field ( Marpa::Internal::And_Node::PREDECESSOR...

    } ## end for my $and_node ( map { @{ $_->[...

    my $choice_or_nodes =
        $self->[Marpa::Internal::Evaluator::OR_NODES_BY_EARLEME] = [];
    my $choice_and_nodes =
        $self->[Marpa::Internal::Evaluator::COMPLETIONS_BY_EARLEME] = [];

    # Find the choice points
    OR_NODE: for my $or_node ( @{ $self->[OR_NODES] } ) {
        my $start_earleme =
            $or_node->[Marpa::Internal::Or_Node::START_EARLEME];
        if ( $or_node->[Marpa::Internal::Or_Node::AND_NODES] >= 2 ) {
            $choice_and_nodes->[$start_earleme] = [];
        }
        my $or_nodes_here = $choice_or_nodes->[$start_earleme];
        if ( defined $or_nodes_here ) {
            push @{$or_nodes_here}, $or_node;
            $or_node->[Marpa::Internal::Or_Node::ID] = $#{$or_nodes_here};
        }
        else {
            $choice_or_nodes->[$start_earleme] = [$or_node];
            $or_node->[Marpa::Internal::Or_Node::ID] = 0;
        }
    } ## end for my $or_node ( @{ $self->[OR_NODES] } )
    ## End OR_NODE:

    # Compute the lists of completed and_nodes at choice points
    OR_NODE: for my $or_node ( @{ $self->[OR_NODES] } ) {

        # If this is a completed or node, the child and nodes will be completed
        # and nodes.  Push them on a list.
        next OR_NODE
            unless $or_node->[Marpa::Internal::Or_Node::IS_COMPLETED];
        my $start_earleme =
            $or_node->[Marpa::Internal::Or_Node::START_EARLEME];
        my $and_nodes_here = $choice_and_nodes->[$start_earleme];
        next OR_NODE unless defined $and_nodes_here;
        push @{$and_nodes_here},
            @{ $or_node->[Marpa::Internal::Or_Node::AND_NODES] };

    } ## end for my $or_node ( @{ $self->[OR_NODES] } )
    ## End OR_NODE:

    # Sort the lists of completed and_nodes
    OR_NODE: for my $start_earleme ( 0 .. $#{$choice_and_nodes} ) {
        my $and_nodes           = $choice_and_nodes->[$start_earleme];
        my @decorated_and_nodes = ();
        for my $and_node ( @{ $choice_and_nodes->[$start_earleme] } ) {
            my $end_earleme =
                $and_node->[Marpa::Internal::And_Node::END_EARLEME];
            my $rule = $and_node->[Marpa::Internal::And_Node::RULE];
            my ( $external_priority, $internal_priority ) = unpack 'NN',
                $rule->[Marpa::Internal::Rule::PRIORITY];
            $external_priority //= 0;
            $internal_priority //= 0;
            my $is_hasty = $rule->[Marpa::Internal::Rule::MINIMAL];
            my $laziness = $end_earleme;
            $laziness = -$laziness if $is_hasty;
            push @decorated_and_nodes,
                [
                $and_node,          $external_priority,
                $internal_priority, $laziness
                ];
        } ## end for my $and_node ( @{ $choice_and_nodes->[$start_earleme...
        $and_nodes = [];
        for my $decorated_and_node (
            ## no critic (BuiltinFunctions::ProhibitReverseSortBlock)
            sort {
                       $b->[1] <=> $a->[1]
                    || $b->[2] <=> $a->[2]
                    || $b->[3] <=> $a->[3]
            }
            ## use critic
            @decorated_and_nodes
            )
        {
            my $and_node = $decorated_and_node->[0];
            $and_node->[Marpa::Internal::And_Node::ID] = @{$and_nodes};
            push @{$and_nodes}, $and_node;
        } ## end for my $decorated_and_node ( sort { $b->[1] <=> $a->[...
        $choice_and_nodes->[$start_earleme] = $and_nodes;

    } ## end for my $start_earleme ( 0 .. $#{$choice_and_nodes} )
    ## End OR_NODE:

    return $self;

}    # sub new

sub Marpa::show_and_node {
    my ( $and_node, $verbose ) = @_;
    $verbose //= 0;
    my $return_value = q{};

    my ( $name, $predecessor, $cause, $value_ref, $closure, $argc, $rule,
        $position, )
        = @{$and_node}[
        Marpa::Internal::And_Node::NAME,
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
        push @rhs, $predecessor->[Marpa::Internal::Or_Node::NAME];
    }    # predecessor

    if ($cause) {
        push @rhs, $cause->[Marpa::Internal::Or_Node::NAME];
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

sub Marpa::Evaluator::show_choices {
    my ($evaler)               = @_;
    my $text                   = q{};
    my $completions_by_earleme = $evaler->[COMPLETIONS_BY_EARLEME];
    CHOICE_EARLEME:
    for my $choice_earleme ( 0 .. $#{$completions_by_earleme} ) {
        my $completions_here = $completions_by_earleme->[$choice_earleme];
        next CHOICE_EARLEME unless defined $completions_here;
        $text .= "Completions at earleme $choice_earleme\n";
        for my $rank ( 0 .. $#{$completions_here} ) {
            $text .= ( sprintf '  %3d: ', $rank )
                . Marpa::show_and_node( $completions_here->[$rank], 99 );
        }
    } ## end for my $choice_earleme ( 0 .. $#{$completions_by_earleme...
    ## End CHOICE_EARLEME
    OR_NODE:
    for my $or_node ( @{ $evaler->[Marpa::Internal::Evaluator::OR_NODES] } ) {
        my $map = $or_node->[Marpa::Internal::Or_Node::CHOICE_MAP];
        next OR_NODE unless $map;
        $text .= $evaler->show_choice_point($or_node);
    }
    ## End OR_NODE
    return $text;
} ## end sub Marpa::Evaluator::show_choices

sub Marpa::Evaluator::show_choice_point {
    my ( $evaler, $choice_point ) = @_;
    my $map = $choice_point->[Marpa::Internal::Or_Node::CHOICE_MAP];
    my $choice_point_name = $choice_point->[Marpa::Internal::Or_Node::NAME];
    if ( not defined $map ) {
        return "No choice map for $choice_point_name\n";
    }
    my $text      = q{};
    my $choice_ix = $choice_point->[Marpa::Internal::Or_Node::MAP_IX];
    if ( not defined $choice_ix ) {
        $text .= "Choice not defined for $choice_point_name\n";
        $choice_ix = -1;
    }
    my $start_earleme =
        $choice_point->[Marpa::Internal::Or_Node::START_EARLEME];
    my $choice_or_nodes =
        $evaler->[Marpa::Internal::Evaluator::OR_NODES_BY_EARLEME]
        ->[$start_earleme];
    my $choice_and_nodes =
        $evaler->[Marpa::Internal::Evaluator::COMPLETIONS_BY_EARLEME]
        ->[$start_earleme];
    for my $map_ix ( 0 .. $#{$map} ) {
        $text .= 'CHOSEN: ' if $map_ix == $choice_ix;
        $text .= "Alternative $map_ix for $choice_point_name:\n";
        my ( $and_vec, $or_choices ) = @{ $map->[$map_ix] };
        AND_IX: for my $and_ix ( 0 .. $#{$choice_and_nodes} ) {
            next AND_IX unless ( substr $and_vec, $and_ix, 1 ) eq '1';
            my $and_node = $choice_and_nodes->[$and_ix];
            my $or_parent =
                $and_node->[Marpa::Internal::And_Node::PARENT_OR_NODE];
            my $and_nodes = $or_parent->[Marpa::Internal::Or_Node::AND_NODES];
            my $or_ix     = $or_parent->[Marpa::Internal::Or_Node::ID];
            my $and_choice   = $or_choices->[$or_ix];
            my $choice_count = scalar @{$and_nodes};
            my $choice_label = q{};
            $choice_label = ", choice $and_choice of $choice_count"
                if $choice_count > 1;
            $text .= "Completion$choice_label: "
                . Marpa::show_and_node( $and_node, 1 );
        } ## end for my $and_ix ( 0 .. $#{$choice_and_nodes} )
        OR_IX: for my $or_ix ( 0 .. $#{$choice_or_nodes} ) {
            my $and_choice = $or_choices->[$or_ix];
            next OR_IX if not defined $and_choice or $and_choice < 0;
            my $or_node = $choice_or_nodes->[$or_ix];

            # completed and nodes were already shown above
            next OR_IX if $or_node->[Marpa::Internal::Or_Node::IS_COMPLETED];
            my $and_nodes = $or_node->[Marpa::Internal::Or_Node::AND_NODES];
            my $choice_count = scalar @{$and_nodes};
            my $choice_label = 'Trivial';
            $choice_label = "Choice $and_choice of $choice_count"
                if $choice_count > 1;
            my $and_node = $and_nodes->[$and_choice];
            $text .= "$choice_label: " . Marpa::show_and_node( $and_node, 1 );
        } ## end for my $or_ix ( 0 .. $#{$choice_or_nodes} )
    } ## end for my $map_ix ( 0 .. $#{$map} )
    return $text;
} ## end sub Marpa::Evaluator::show_choice_point

sub Marpa::Evaluator::show_bocage {
    my ( $evaler, $verbose ) = @_;
    $verbose //= 0;

    my ( $parse_count, $or_nodes, $package, ) = @{$evaler}[
        Marpa::Internal::Evaluator::PARSE_COUNT,
        Marpa::Internal::Evaluator::OR_NODES,
        Marpa::Internal::Evaluator::PACKAGE,
    ];

    my $text =
        'package: ' . $package . '; parse count: ' . $parse_count . "\n";

    for my $or_node ( @{ $evaler->[OR_NODES] } ) {

        my $or_node_name = $or_node->[Marpa::Internal::Or_Node::NAME];
        my $choice       = $or_node->[Marpa::Internal::Or_Node::AND_CHOICE];
        my $and_nodes    = $or_node->[Marpa::Internal::Or_Node::AND_NODES];

        for my $index ( 0 .. $#{$and_nodes} ) {
            my $and_node  = $and_nodes->[$index];
            my $is_choice = q{};
            $is_choice = q{*} if defined $choice and $choice == $index;

            my $and_node_name = $or_node_name . '[' . $index . ']';
            if ( $verbose >= 2 ) {
                $text .= "$is_choice$or_node_name ::= $and_node_name\n";
            }

            $text .= q{*} if $is_choice;
            $text .= Marpa::show_and_node( $and_node, $verbose );

        } ## end for my $index ( 0 .. $#{$and_nodes} )

    } ## end for my $or_node ( @{ $evaler->[OR_NODES] } )

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
            . $or_node->[Marpa::Internal::Or_Node::NAME]
            . "[$choice]";
        $text .= "; Parent = $parent" if defined $parent;
        $text .= "; Depth = $depth; Rhs Length = $argc\n";

        $text .= '    Rule: '
            . Marpa::show_dotted_rule( $rule, $position + 1 ) . "\n";
        $text
            .= '    Kernel: '
            . $predecessor->[Marpa::Internal::Tree_Node::OR_NODE]
            ->[Marpa::Internal::Or_Node::NAME] . "\n"
            if defined $predecessor;
        $text
            .= '    Closure: '
            . $cause->[Marpa::Internal::Tree_Node::OR_NODE]
            ->[Marpa::Internal::Or_Node::NAME] . "\n"
            if defined $cause;
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
    my $evaler       = shift;
    my $args         = shift;
    my $recce        = $evaler->[Marpa::Internal::Evaluator::RECOGNIZER];
    my ( $grammar, ) = @{$recce}[ Marpa::Internal::Recognizer::GRAMMAR, ];
    Marpa::Grammar::set( $grammar, $args );
    return 1;
} ## end sub Marpa::Evaluator::set

# map the choices at the choice point or node
sub map_choice_point {
    my ( $evaler, $choice_point ) = @_;

    # array in which to build map
    my @map;

    my $start_earleme =
        $choice_point->[Marpa::Internal::Or_Node::START_EARLEME];

    # create the vector of parent or nodes
    my $parent_or_choices = [];
    CHOICE_OR_NODE:
    for my $choice_or_node (
        @{  $evaler->[Marpa::Internal::Evaluator::OR_NODES_BY_EARLEME]
                ->[$start_earleme]
        }
        )
    {
        if ( defined $choice_or_node->[Marpa::Internal::Or_Node::AND_CHOICE] )
        {
            $parent_or_choices
                ->[ $choice_or_node->[Marpa::Internal::Or_Node::ID] ] = -1;
        }
    } ## end for my $choice_or_node ( @{ $evaler->[...
    ## End CHOICE_OR_NODE:
    $choice_point->[Marpa::Internal::Or_Node::PARENT_OR_CHOICES] =
        $parent_or_choices;

    # build the choice map for this choice point or node
    my @ur_map = (
        [   $choice_point,
            '0' x scalar @{
                $evaler->[Marpa::Internal::Evaluator::COMPLETIONS_BY_EARLEME]
                    ->[$start_earleme]
                },
            [ @{$parent_or_choices} ]
        ]
    );
    MAP_ENTRY: while ( my $ur_map_entry = pop @ur_map ) {
        my ( $map_or_node, $and_vec, $or_choices, ) = @{$ur_map_entry};

        if (defined
            $or_choices->[ $map_or_node->[Marpa::Internal::Or_Node::ID] ] )
        {
            Marpa::exception( 'Cycle at '
                    . $map_or_node->[Marpa::Internal::Or_Node::NAME] );
            ## next MAP_ENTRY;
        } ## end if ( defined $or_choices->[ $map_or_node->[...

        my $is_completed =
            $map_or_node->[Marpa::Internal::Or_Node::IS_COMPLETED];

        CHOICE:
        for my $choice (
            0 .. $#{ $map_or_node->[Marpa::Internal::Or_Node::AND_NODES] } )
        {
            my $new_and_vec = $and_vec;

            my $map_and_node =
                $map_or_node->[Marpa::Internal::Or_Node::AND_NODES]
                ->[$choice];
            if ($is_completed) {
                substr $new_and_vec,
                    $map_and_node->[Marpa::Internal::And_Node::ID],
                    1, '1';
            }

            my $cause = $map_and_node->[Marpa::Internal::And_Node::CAUSE];
            my $predecessor =
                $map_and_node->[Marpa::Internal::And_Node::PREDECESSOR];

            my $new_or_choices = [ @{$or_choices} ];
            $new_or_choices->[ $map_or_node->[Marpa::Internal::Or_Node::ID] ]
                = $choice;

            if ( not defined $cause and not defined $predecessor ) {
                push @map, [ $new_and_vec, $new_or_choices ];
            }

            if ( defined $cause
                and $cause->[Marpa::Internal::Or_Node::START_EARLEME]
                <= $start_earleme )
            {
                push @ur_map,
                    [ $cause, $new_and_vec, [ @{$new_or_choices} ], ];
            } ## end if ( defined $cause and $cause->[...

            if ( defined $predecessor
                and $predecessor->[Marpa::Internal::Or_Node::START_EARLEME]
                <= $start_earleme )
            {
                push @ur_map,
                    [ $predecessor, $new_and_vec, [ @{$new_or_choices} ] ];
            } ## end if ( defined $predecessor and $predecessor->[...
        } ## end for my $choice ( 0 .. $#{ $map_or_node->[...
    } ## end while ( my $ur_map_entry = pop @ur_map )
    ## End MAP_ENTRY

    ## no critic (BuiltinFunctions::ProhibitReverseSortBlock)
    $choice_point->[Marpa::Internal::Or_Node::CHOICE_MAP] =
        [ sort { $b->[0] cmp $a->[0] } @map ];
    ## use critic

    return;
} ## end sub map_choice_point

# This will replace the old value method
sub Marpa::Evaluator::value {
    my $evaler     = shift;
    my $recognizer = $evaler->[Marpa::Internal::Evaluator::RECOGNIZER];

    Marpa::exception('No parse supplied') unless defined $evaler;
    my $evaler_class = ref $evaler;
    my $right_class  = 'Marpa::Evaluator';
    Marpa::exception(
        "Don't parse argument is class: $evaler_class; should be: $right_class"
    ) unless $evaler_class eq $right_class;

    my $grammar = $recognizer->[Marpa::Internal::Recognizer::GRAMMAR];

    my $tracing  = $grammar->[Marpa::Internal::Grammar::TRACING];
    my $trace_fh = $grammar->[Marpa::Internal::Grammar::TRACE_FILE_HANDLE];
    my $trace_values     = 0;
    my $trace_choices    = 0;
    my $trace_iterations = 0;
    if ($tracing) {
        $trace_choices = $grammar->[Marpa::Internal::Grammar::TRACE_CHOICES];
        $trace_values  = $grammar->[Marpa::Internal::Grammar::TRACE_VALUES];
        $trace_iterations =
            $grammar->[Marpa::Internal::Grammar::TRACE_ITERATIONS];
    } ## end if ($tracing)

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

    # Initialize the work list for the disambiguation with the top or-node
    my @work_list = ( $bocage->[0] );

    # This loop does disambiguation -- that is picks one parse from an
    # ambiguous bocage.
    #
    my $chosen_to_here_earleme = -1;
    OR_NODE: while ( my $or_node = pop @work_list ) {
        my $start_earleme =
            $or_node->[Marpa::Internal::Or_Node::START_EARLEME];
        my $and_nodes = $or_node->[Marpa::Internal::Or_Node::AND_NODES];

        if ( $trace_choices >= 2 ) {
            say {$trace_fh} 'Making choice for ',
                $or_node->[Marpa::Internal::Or_Node::NAME];
        }
        MAKE_CHOICE: {

            # The choice is already made ...
            if ( defined $or_node->[Marpa::Internal::Or_Node::AND_CHOICE] ) {
                if ( $trace_choices >= 2 ) {
                    say {$trace_fh} 'Choice already made for ',
                        $or_node->[Marpa::Internal::Or_Node::NAME];
                }

                # ... which can be a bad thing
                if ( $start_earleme > $chosen_to_here_earleme ) {
                    Marpa::exception( 'Cycle at '
                            . $or_node->[Marpa::Internal::Or_Node::NAME] );
                }
                last MAKE_CHOICE;
            } ## end if ( defined $or_node->[...

            # The choice is trivial
            if ( @{$and_nodes} <= 1 ) {

                if ( $trace_choices >= 2 ) {
                    say {$trace_fh} 'Choice trivial for ',
                        $or_node->[Marpa::Internal::Or_Node::NAME];
                }

                $or_node->[Marpa::Internal::Or_Node::AND_CHOICE] = 0;
                last MAKE_CHOICE;
            } ## end if ( @{$and_nodes} <= 1 )

            if ($trace_choices) {
                say {$trace_fh} 'Choice non-trivial for ',
                    $or_node->[Marpa::Internal::Or_Node::NAME];
            }

            # The choice is non-trivial
            my $choices = $or_node->[Marpa::Internal::Or_Node::CHOICE_MAP];
            if ( not defined $choices ) {
                if ($trace_choices) {
                    say {$trace_fh} 'Mapping choice point: ',
                        $or_node->[Marpa::Internal::Or_Node::NAME];
                }
                map_choice_point( $evaler, $or_node );
                $choices = $or_node->[Marpa::Internal::Or_Node::CHOICE_MAP];
            } ## end if ( not defined $choices )
            if ( @{$choices} <= 0 ) {
                Marpa::exception( 'No valid choices for '
                        . $or_node->[Marpa::Internal::Or_Node::NAME] );
            }
            $or_node->[Marpa::Internal::Or_Node::MAP_IX] = my $map_ix = 0;
            my $or_choices = $choices->[$map_ix]->[1];
            my $or_nodes_here =
                $evaler->[Marpa::Internal::Evaluator::OR_NODES_BY_EARLEME]
                ->[$start_earleme];

            OR_IX: for my $or_ix ( 0 .. $#{$or_choices} ) {
                my $choice_or_node = $or_nodes_here->[$or_ix];
                my $and_choice     = $or_choices->[$or_ix];
                next OR_IX if not defined $and_choice or $and_choice < 0;
                if ($trace_choices) {
                    say {$trace_fh} 'setting choice for ',
                        $choice_or_node->[Marpa::Internal::Or_Node::NAME],
                        " to $and_choice";
                }
                $choice_or_node->[Marpa::Internal::Or_Node::AND_CHOICE] =
                    $and_choice;
            } ## end for my $or_ix ( 0 .. $#{$or_choices} )

            $chosen_to_here_earleme = $start_earleme;

        } ## end MAKE_CHOICE:
        ## End MAKE_CHOICE

        my $and_choice = $or_node->[Marpa::Internal::Or_Node::AND_CHOICE];
        my $and_node   = $and_nodes->[$and_choice];

        my @new_work_nodes = grep { defined $_ } @{$and_node}[
            Marpa::Internal::And_Node::CAUSE,
            Marpa::Internal::And_Node::PREDECESSOR,
        ];
        weaken( $_->[Marpa::Internal::Or_Node::PARENT_OR_NODE] = $or_node )
            for @new_work_nodes;
        push @work_list, @new_work_nodes;

    } ## end while ( my $or_node = pop @work_list )
    ## End OR_NODE:

    # Write the and-nodes out in preorder
    my @preorder = ();
    @work_list = (
        do {
            my $or_node = $bocage->[0];
            my $choice  = $or_node->[Marpa::Internal::Or_Node::AND_CHOICE];
            $or_node->[Marpa::Internal::Or_Node::AND_NODES]->[$choice];
            } ## end do
    );
    OR_NODE: while ( my $and_node = pop @work_list ) {
        my $left_or_node =
            $and_node->[Marpa::Internal::And_Node::PREDECESSOR];
        my $right_or_node = $and_node->[Marpa::Internal::And_Node::CAUSE];
        if ( not defined $left_or_node and defined $right_or_node ) {
            $left_or_node  = $right_or_node;
            $right_or_node = undef;
        }
        if ( defined $left_or_node ) {
            my $choice =
                $left_or_node->[Marpa::Internal::Or_Node::AND_CHOICE];
            push @work_list,
                $left_or_node->[Marpa::Internal::Or_Node::AND_NODES]
                ->[$choice];
        } ## end if ( defined $left_or_node )
        if ( defined $right_or_node ) {
            my $choice =
                $right_or_node->[Marpa::Internal::Or_Node::AND_CHOICE];
            push @work_list,
                $right_or_node->[Marpa::Internal::Or_Node::AND_NODES]
                ->[$choice];
        } ## end if ( defined $right_or_node )
        push @preorder, $and_node;
    } ## end while ( my $and_node = pop @work_list )
    ## End OR_NODE:

    my @evaluation_stack = ();

    TREE_NODE: for my $and_node ( reverse @preorder ) {

        if ( $trace_values >= 3 ) {
            for my $i ( reverse 0 .. $#evaluation_stack ) {
                printf {$trace_fh} 'Stack position %3d:', $i;
                print {$trace_fh} q{ },
                    Data::Dumper->new( [ $evaluation_stack[$i] ] )->Terse(1)
                    ->Dump
                    or Marpa::exception('print to trace handle failed');
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
                    $and_node->[Marpa::Internal::And_Node::NAME],
                    ': ',
                    Data::Dumper->new( [ ${$value_ref} ] )->Terse(1)->Dump
                    or Marpa::exception('print to trace handle failed');
            } ## end if ($trace_values)

        }    # defined $value_ref

        next TREE_NODE unless defined $closure;

        if ($trace_values) {
            my $rule = $and_node->[Marpa::Internal::And_Node::RULE];
            say {$trace_fh}
                'Popping ',
                $argc,
                ' values to evaluate ',
                $and_node->[Marpa::Internal::And_Node::NAME],
                ', rule: ',
                Marpa::brief_rule($rule);
        } ## end if ($trace_values)

        my $args = [ map { ${$_} } ( splice @evaluation_stack, -$argc ) ];

        my $result;

        my $closure_type = ref $closure;

        if ( $closure_type eq 'CODE' ) {

            {
                my $old_warn_handler = $SIG{__WARN__};
                my @warnings;
                $SIG{__WARN__} =
                    sub { push @warnings, [ $_[0], ( caller 0 ) ]; };

                my $eval_ok = eval { $result = $closure->( @{$args} ); 1 };

                $SIG{__WARN__} = $old_warn_handler;

                if ( not $eval_ok or @warnings ) {
                    my $fatal_error = $EVAL_ERROR;
                    my $rule = $and_node->[Marpa::Internal::And_Node::RULE];
                    my $code =
                        $rule_data->[ $rule->[Marpa::Internal::Rule::ID] ]
                        ->[Marpa::Internal::Evaluator::Rule::CODE];
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

# Apparently perlcritic has a bug and doesn't see the final return
## no critic (Subroutines::RequireFinalReturn)
sub Marpa::Evaluator::old_value {
## use critic

    my $evaler     = shift;
    my $recognizer = $evaler->[Marpa::Internal::Evaluator::RECOGNIZER];

    Marpa::exception('No parse supplied') unless defined $evaler;
    my $evaler_class = ref $evaler;
    my $right_class  = 'Marpa::Evaluator';
    Marpa::exception(
        "Don't parse argument is class: $evaler_class; should be: $right_class"
    ) unless $evaler_class eq $right_class;

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

    } ## end else [ if ( not defined $tree )

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
                $last_position_by_depth[$depth] = $tree_position
                    unless defined $build_node;
                next TREE_NODE;
            }

            if ($trace_iterations) {
                say {$trace_fh}
                    'Iteration ',
                    $choice,
                    ' tree node #',
                    $tree_position, q{ },
                    $or_node->[Marpa::Internal::Or_Node::NAME],
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

            $build_node = $tree_position unless defined $build_node;

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

            AND_NODE: while (1) {

                my $and_node = $and_nodes->[$choice];

                # if none of the and nodes are useable, this or node is discarded
                # and we go to the outer loop and pop tree nodes until
                # we find one which can be iterated.
                next TREE_NODE unless defined $and_node;

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
                last AND_NODE unless $or_node_is_closure;
                last AND_NODE unless $rule->[Marpa::Internal::Rule::CYCLE];

                # if this rule is part of a cycle,
                # and this is a closure or-node
                # check to see if we have cycled

                my $or_node_name = $or_node->[Marpa::Internal::Or_Node::NAME];
                my $and_node_name = $or_node_name . "[$choice]";

                my $cycles = $evaler->[Marpa::Internal::Evaluator::CYCLES];

                # if by an initial highball estimate
                # we have yet to cycle more than a limit (now hard coded
                # to 1), then we can use this and node
                last AND_NODE if $cycles->{$and_node_name}++ < 1;

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
                    my $parent_or_node_name =
                        $tree_or_node->[Marpa::Internal::Or_Node::NAME];
                    $cycles_count++
                        if $or_node_name eq $parent_or_node_name
                            and $choice == $parent_choice;
                } ## end while ( defined $parent )

                # replace highball estimate with actual count
                $cycles->{$and_node_name} = $cycles_count;

                # repeat the test
                last AND_NODE if $cycles->{$and_node_name}++ < 1;

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
                $value_description =
                    '; value='
                    . Data::Dumper->new( ${$value_ref} )->Terse(1)->Dump
                    if defined $value_ref;
                print {$trace_fh}
                    'Pushing tree node #',
                    ( scalar @{$tree} ), q{ },
                    $or_node->[Marpa::Internal::Or_Node::NAME],
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
    push @{$tree}, @old_tree[ $leaf_side_start_position .. $#old_tree ]
        if defined $leaf_side_start_position;

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
                    $or_node->[Marpa::Internal::Or_Node::NAME],
                    ': ',
                    Data::Dumper->new( [ ${$value_ref} ] )->Terse(1)->Dump
                    or Marpa::exception('print to trace handle failed');
            } ## end if ($trace_values)

        }    # defined $value_ref

        next TREE_NODE unless defined $closure;

        if ($trace_values) {
            my ( $or_node, $rule ) = @{$node}[
                Marpa::Internal::Tree_Node::OR_NODE,
                Marpa::Internal::Tree_Node::RULE,
            ];
            say {$trace_fh}
                'Popping ',
                $argc,
                ' values to evaluate ',
                $or_node->[Marpa::Internal::Or_Node::NAME],
                ', rule: ',
                Marpa::brief_rule($rule);
        } ## end if ($trace_values)

        my $args = [ map { ${$_} } ( splice @evaluation_stack, -$argc ) ];

        my $result;

        my $closure_type = ref $closure;

        if ( $closure_type eq 'CODE' ) {

            {
                my $old_warn_handler = $SIG{__WARN__};
                my @warnings;
                $SIG{__WARN__} =
                    sub { push @warnings, [ $_[0], ( caller 0 ) ]; };

                my $eval_ok = eval { $result = $closure->( @{$args} ); 1 };

                $SIG{__WARN__} = $old_warn_handler;

                if ( not $eval_ok or @warnings ) {
                    my $fatal_error = $EVAL_ERROR;
                    my $rule = $node->[ Marpa::Internal::Tree_Node::RULE, ];
                    my $code =
                        $rule_data->[ $rule->[Marpa::Internal::Rule::ID] ]
                        ->[Marpa::Internal::Evaluator::Rule::CODE];
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

} ## end sub Marpa::Evaluator::old_value

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
    Marpa::exception('Parse failed') unless $evaler;

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
to be specified for an evaler object.
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
