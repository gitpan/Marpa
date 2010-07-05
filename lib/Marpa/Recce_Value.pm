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

use Marpa::Offset qw(

    :package=Marpa::Internal::Recce_Or_Node

    ID
    TAG
    ITEMS
    RULE_ID
    POSITION
    AND_NODE_IDS

    SOURCE_OR_NODE { The name of the first grandparent or-node,
    for keeping track while populating the ITEMS element }

    =LAST_FIELD
);

use Marpa::Offset qw(

    :package=Marpa::Internal::Recce_And_Node

    ID
    TAG
    RULE_ID
    TOKEN_NAME
    VALUE_REF
    VALUE_OPS

    { Fields before this (except ID)
    are used in evaluate()
    and must be in the same location
    for both Recce_And_Node and And_Node.
    ID is included for orthogonality. }

    PREDECESSOR_ID
    CAUSE_ID

    { Fields after this are not yet used in the
    2010 evaluator.  As they are, they will be
    moved above this comment.  Any left after
    the 2010 evaluator is complete should be deleted. }

    START_EARLEME
    END_EARLEME
    CAUSE_EARLEME

    POSITION {
    What are the semantics for the 2010 evaluator?
    2009 evaluator semantices were as follows:
    Position in an and-node is not the same as
    position in a rule.  Rule positions are locations BETWEEN
    symbols, and start from 0 (before the first symbol).
    And-node positions are zero-based locations OF symbols.
    An and-node position of -1 means the and-node is for a
    rule with an empty RHS.  }

    FIXED_RANKING_DATA { Rank for this and-node itself,
    but not including any of the children.
    It takes into account the token, if any,
    but not the rank of any of the children.
    Once calculated, it's a constant
    for the life of the and-node.
    }

    RANKING_CLOSURE

    TREE_OPS
    PARENT_ID
    PARENT_CHOICE
    DELETED

    =LAST_FIELD

);

sub Marpa::Recognizer::show_recce_and_node {
    my ( $recce, $and_node, $verbose ) = @_;
    $verbose //= 0;

    my $text = "show_recce_and_node:\n";

    my $grammar = $recce->[Marpa::Internal::Recognizer::GRAMMAR];
    my $rules   = $grammar->[Marpa::Internal::Grammar::RULES];

    my $name = $and_node->[Marpa::Internal::Recce_And_Node::TAG];
    my $id   = $and_node->[Marpa::Internal::Recce_And_Node::ID];
    my $predecessor_id =
        $and_node->[Marpa::Internal::Recce_And_Node::PREDECESSOR_ID];
    my $cause_id  = $and_node->[Marpa::Internal::Recce_And_Node::CAUSE_ID];
    my $value_ref = $and_node->[Marpa::Internal::Recce_And_Node::VALUE_REF];
    my $rule_id   = $and_node->[Marpa::Internal::Recce_And_Node::RULE_ID];
    my $position  = $and_node->[Marpa::Internal::Recce_And_Node::POSITION];

    my @rhs = ();

    my $rule          = $rules->[$rule_id];
    my $original_rule = $rule->[Marpa::Internal::Rule::ORIGINAL_RULE]
        // $rule;
    my $is_virtual_rule = $rule != $original_rule;

    my $or_nodes = $recce->[Marpa::Internal::Recognizer::OR_NODES];

    my $predecessor;
    if ($predecessor_id) {
        $predecessor = $or_nodes->[$predecessor_id];

        push @rhs, $predecessor->[Marpa::Internal::Recce_Or_Node::TAG]
            . "o$predecessor_id";
    }    # predecessor

    my $cause;
    if ($cause_id) {
        $cause = $or_nodes->[$cause_id];
        push @rhs,
            $cause->[Marpa::Internal::Recce_Or_Node::TAG] . "o$cause_id";
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
        if ( $and_node->[Marpa::Internal::Recce_And_Node::TREE_OPS] ) {
            push @comment, 'tree_ops';
        }
        if ( $and_node->[Marpa::Internal::Recce_And_Node::VALUE_OPS] ) {
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

    my $text = "show_recce_or_node:\n";

    my $grammar     = $recce->[Marpa::Internal::Recognizer::GRAMMAR];
    my $rules       = $grammar->[Marpa::Internal::Grammar::RULES];
    my $rule_id     = $or_node->[Marpa::Internal::Recce_Or_Node::RULE_ID];
    my $position    = $or_node->[Marpa::Internal::Recce_Or_Node::POSITION];
    my $or_node_id  = $or_node->[Marpa::Internal::Recce_Or_Node::ID];
    my $or_node_tag = $or_node->[Marpa::Internal::Recce_Or_Node::TAG];

    my $rule          = $rules->[$rule_id];
    my $original_rule = $rule->[Marpa::Internal::Rule::ORIGINAL_RULE]
        // $rule;
    my $is_virtual_rule = $rule != $original_rule;

    my $and_nodes = $recce->[Marpa::Internal::Recognizer::AND_NODES];

    if ( my $and_node_ids =
        $or_node->[Marpa::Internal::Recce_Or_Node::AND_NODE_IDS] )
    {
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
    } ## end if ( my $and_node_ids = $or_node->[...])
    else {
        $text .= $or_node_tag . "o$or_node_id: UNPOPULATED\n";
    }

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

# Returns false if no parse
sub Marpa::Recognizer::value {
    my ( $recce, @arg_hashes ) = @_;

    my $parse_set_arg = $recce->[Marpa::Internal::Recognizer::END];
    my $trace_values  = $recce->[Marpa::Internal::Recognizer::TRACE_VALUES]
        // 0;

    # default settings
    local $Marpa::Internal::EXPLICIT_CLOSURES =
        $recce->[Marpa::Internal::Recognizer::CLOSURES] // {};
    local $Marpa::Internal::TRACE_ACTIONS =
        $recce->[Marpa::Internal::Recognizer::TRACE_ACTIONS] // 0;

    local $Marpa::Internal::TRACE_FH =
        $recce->[Marpa::Internal::Recognizer::TRACE_FILE_HANDLE];

    my $and_nodes = $recce->[Marpa::Internal::Recognizer::AND_NODES];
    my $or_nodes  = $recce->[Marpa::Internal::Recognizer::OR_NODES];

    if ( $recce->[Marpa::Internal::Recognizer::SINGLE_PARSE_MODE] ) {
        Marpa::exception(
            qq{Arguments were passed directly to value() in a previous call\n},
            qq{Only one call to value() is allowed per recognizer when arguments are passed directly\n},
            qq{This is the second call to value()\n}
        );
    } ## end if ( $recce->[Marpa::Internal::Recognizer::SINGLE_PARSE_MODE...])

    for my $arg_hash (@arg_hashes) {

        if ( exists $arg_hash->{end} ) {
            $recce->[Marpa::Internal::Recognizer::SINGLE_PARSE_MODE] = 1;
            $parse_set_arg = $arg_hash->{end};
            delete $arg_hash->{end};
        }

        if ( exists $arg_hash->{closures} ) {
            $recce->[Marpa::Internal::Recognizer::SINGLE_PARSE_MODE] = 1;
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
            $recce->[Marpa::Internal::Recognizer::SINGLE_PARSE_MODE] = 1;
            $Marpa::Internal::TRACE_ACTIONS = $arg_hash->{trace_actions};
            delete $arg_hash->{trace_actions};
        }

        if ( exists $arg_hash->{trace_values} ) {
            $recce->[Marpa::Internal::Recognizer::SINGLE_PARSE_MODE] = 1;
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

    my $grammar = $recce->[Marpa::Internal::Recognizer::GRAMMAR];
    my $action_object_class =
        $grammar->[Marpa::Internal::Grammar::ACTION_OBJECT];
    my $earley_sets = $recce->[Marpa::Internal::Recognizer::EARLEY_SETS];
    my $earley_hash = $recce->[Marpa::Internal::Recognizer::EARLEY_HASH];
    Marpa::exception(
        "Attempt to use quick evaluator on an infinitely ambiguous grammar\n",
        "  Rewrite to remove cycles, or\n",
        "  Use the power evaluator\n"
    ) if $grammar->[Marpa::Internal::Grammar::IS_INFINITE];

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
    my $nulling      = $start_symbol->[Marpa::Internal::Symbol::NULLING];
    my $symbol_id    = $start_symbol->[Marpa::Internal::Symbol::ID];

    # null parse is special case
    if ($nulling) {

        my $and_node = [];
        $#{$and_node} = Marpa::Internal::Recce_And_Node::LAST_FIELD;

        $and_node->[Marpa::Internal::Recce_And_Node::VALUE_REF] =
            \( $null_values->[$symbol_id] );
        $and_node->[Marpa::Internal::Recce_And_Node::RULE_ID] =
            $start_rule_id;
        $and_node->[Marpa::Internal::Recce_And_Node::VALUE_OPS] =
            $evaluator_rules->[$start_rule_id];

        $and_node->[Marpa::Internal::Recce_And_Node::POSITION]      = 0;
        $and_node->[Marpa::Internal::Recce_And_Node::START_EARLEME] = 0;
        $and_node->[Marpa::Internal::Recce_And_Node::CAUSE_EARLEME] = 0;
        $and_node->[Marpa::Internal::Recce_And_Node::END_EARLEME]   = 0;
        $and_node->[Marpa::Internal::Recce_And_Node::ID]            = 0;
        $and_nodes->[0] = $and_node;

        my $symbol_name = $start_symbol->[Marpa::Internal::Symbol::NAME];
        ## no critic (ValuesAndExpressions::RequireInterpolationOfMetachars)
        $and_node->[Marpa::Internal::Recce_And_Node::TAG] =
            q{T@0-0_} . $symbol_name;
        ## use critic

        return Marpa::Internal::Evaluator::evaluate( $grammar, $action_object,
            [$and_node], $trace_values );

    } ## end if ($nulling)

    my $start_or_node = [];
    {
        my $start_or_node_tag =
            $start_or_node->[Marpa::Internal::Recce_Or_Node::TAG] =
            "F$start_rule_id"
            .
            ## no critic (ValuesAndExpressions::RequireInterpolationOfMetachars)
            '@0-' .
            ## use critic
            $current_parse_set;
        $recce->[Marpa::Internal::Recognizer::OR_NODE_HASH]
            ->{$start_or_node_tag} = $start_or_node;
    }
    $start_or_node->[Marpa::Internal::Recce_Or_Node::ID]    = 0;
    $start_or_node->[Marpa::Internal::Recce_Or_Node::ITEMS] = [$start_item];
    $start_or_node->[Marpa::Internal::Recce_Or_Node::RULE_ID] =
        $start_rule_id;
    $start_or_node->[Marpa::Internal::Recce_Or_Node::POSITION] =
        scalar @{ $start_rule->[Marpa::Internal::Rule::RHS] };

    # No source or-node for the start or-node
    $start_or_node->[Marpa::Internal::Recce_Or_Node::SOURCE_OR_NODE] =
        '[TOP]';

    my $start_and_node = [];
    $start_and_node->[Marpa::Internal::Recce_And_Node::RULE_ID] =
        $start_rule_id;
    $start_and_node->[Marpa::Internal::Recce_And_Node::VALUE_OPS] =
        $evaluator_rules->[$start_rule_id];
    $start_and_node->[Marpa::Internal::Recce_And_Node::POSITION] =
        scalar @{ $start_rule->[Marpa::Internal::Rule::RHS] };
    $start_and_node->[Marpa::Internal::Recce_And_Node::START_EARLEME] = 0;
    $start_and_node->[Marpa::Internal::Recce_And_Node::END_EARLEME] =
        $current_parse_set;
    $start_and_node->[Marpa::Internal::Recce_And_Node::ID] = 0;
    $start_and_node->[Marpa::Internal::Recce_And_Node::TAG] =
        $start_or_node->[Marpa::Internal::Recce_Or_Node::TAG];

    # Populate the start or-node
    $and_nodes->[0] = $start_and_node;
    $or_nodes->[0]  = $start_or_node;
    $start_or_node->[Marpa::Internal::Recce_Or_Node::AND_NODE_IDS] = [0];

    # Initalize work list and stack
    my @or_worklist = ( [ $start_or_node, 0 ] );
    my @stack = ($start_and_node);

    OR_WORKITEM: while ( my $or_workitem = pop @or_worklist ) {

        my ( $work_or_node, $and_choice ) = @{$or_workitem};

        my $work_node_name =
            $work_or_node->[Marpa::Internal::Recce_Or_Node::TAG];

        # SET Should be the same for all items
        my $or_node_items =
            $work_or_node->[Marpa::Internal::Recce_Or_Node::ITEMS];
        my $work_set;
        my $work_node_origin;
        {
            my $first_item = $or_node_items->[0];
            $work_set = $first_item->[Marpa::Internal::Earley_Item::SET];
            $work_node_origin =
                $first_item->[Marpa::Internal::Earley_Item::PARENT];
        }

        my $work_rule_id =
            $work_or_node->[Marpa::Internal::Recce_Or_Node::RULE_ID];
        my $work_rule = $rules->[$work_rule_id];
        my $work_position =
            $work_or_node->[Marpa::Internal::Recce_Or_Node::POSITION] - 1;
        my $work_symbol =
            $work_rule->[Marpa::Internal::Rule::RHS]->[$work_position];

        for my $item ( @{$or_node_items} ) {

            my $or_sapling_set = $work_set;

# Marpa::Display
# name: Leo Expansion
# inline: 1

            my $leo_links = $item->[Marpa::Internal::Earley_Item::LEO_LINKS]
                // [];

            # If this is a Leo completion, translate the Leo links
            for my $leo_link ( @{$leo_links} ) {

                my ( $leo_item, $cause, $token_name, $token_value ) =
                    @{$leo_link};

                my ( $next_leo_item, $leo_base_item ) =
                    @{ $leo_item->[Marpa::Internal::Earley_Item::LINKS]->[0]
                    };

                my $next_links = [];
                if ($token_name) {
                    push @{$next_links},
                        [ $leo_base_item, undef, $token_name, $token_value ];
                }
                if ($cause) {
                    push @{$next_links}, [ $leo_base_item, $cause ];
                }

                LEO_ITEM: for ( ;; ) {

                    if ( not $next_leo_item ) {

                        push @{ $item->[Marpa::Internal::Earley_Item::LINKS]
                            },
                            @{$next_links};

                        # Now that the Leo links are translated, remove them
                        $item->[Marpa::Internal::Earley_Item::LEO_LINKS] =
                            undef;
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
                        $target_item->[Marpa::Internal::Earley_Item::LINKS] =
                            [];
                        $target_item->[Marpa::Internal::Earley_Item::SET] =
                            $or_sapling_set;
                        $earley_hash->{$name} = $target_item;
                        push @{ $earley_sets->[$or_sapling_set] },
                            $target_item;
                    } ## end if ( not defined $target_item )

                    push
                        @{ $target_item->[Marpa::Internal::Earley_Item::LINKS]
                        },
                        @{$next_links};

                    $leo_item = $next_leo_item;

                    ( $next_leo_item, $leo_base_item ) =
                        @{ $leo_item->[Marpa::Internal::Earley_Item::LINKS]
                            ->[0] };

                    $next_links = [ [ $leo_base_item, $target_item ] ];

                } ## end for ( ;; )
            } ## end for my $leo_link ( @{$leo_links} )

# Marpa::Display::End

        } ## end for my $item ( @{$or_node_items} )

        my @link_worklist;

        CREATE_LINK_WORKLIST: {

            # link worklist item is $predecessor, $cause, $token_name, $value_ref
            my ( $predecessor, $cause, $token_name, $value_ref );

            # All items need to made predecessors to a
            # nulling work symbol.

            if ( $work_symbol->[Marpa::Internal::Symbol::NULLING] ) {
                my $nulling_symbol_id =
                    $work_symbol->[Marpa::Internal::Symbol::ID];
                $value_ref  = \$null_values->[$nulling_symbol_id];
                $token_name = $work_symbol->[Marpa::Internal::Symbol::NAME];
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

# say STDERR "DEBUG: link worklist has ", (scalar @link_worklist), " items";

        # The and node data is put into the hash, only to be taken out immediately,
        # but in the process the very important step of eliminating duplicates
        # is accomplished.
        my %and_node_data = ();

        LINK_WORK_ITEM: for my $link_work_item (@link_worklist) {

# say STDERR "DEBUG: Starting link work item";

            # CHOICE POINT
            my ( $predecessor, $cause, $token_name, $value_ref ) =
                @{$link_work_item};

            my $cause_earleme = $work_node_origin;
            my $predecessor_id;

# say STDERR "DEBUG: work_position=", $work_position;

            if ( $work_position > 0 ) {

                $cause_earleme =
                    $predecessor->[Marpa::Internal::Earley_Item::SET];

                my $predecessor_name =
                      "R$work_rule_id:$work_position" . q{@}
                    . $predecessor->[Marpa::Internal::Earley_Item::ORIGIN]
                    . q{-}
                    . $cause_earleme;

# say STDERR "DEBUG: Processing predecessor $predecessor_name";

                FIND_PREDECESSOR: {
                    my $predecessor_or_node =
                        $recce->[Marpa::Internal::Recognizer::OR_NODE_HASH]
                        ->{$predecessor_name};
                    if ($predecessor_or_node) {
                        $predecessor_id = $predecessor_or_node
                            ->[Marpa::Internal::Recce_Or_Node::ID];
                        last FIND_PREDECESSOR
                            if $predecessor_or_node->[
                                Marpa::Internal::Recce_Or_Node::SOURCE_OR_NODE
                            ] ne $work_node_name;

                        # If the working or node is the grandparent of this new or-node,
                        # we are building it, and need to populate the list of Earley items
                        push @{ $predecessor_or_node
                                ->[Marpa::Internal::Recce_Or_Node::ITEMS] },
                            $predecessor;

                        last FIND_PREDECESSOR;

                    } ## end if ($predecessor_or_node)

                    $predecessor_or_node = [];
                    $predecessor_or_node
                        ->[Marpa::Internal::Recce_Or_Node::TAG] =
                        $predecessor_name;
                    $recce->[Marpa::Internal::Recognizer::OR_NODE_HASH]
                        ->{$predecessor_name} = $predecessor_or_node;
                    $predecessor_or_node
                        ->[Marpa::Internal::Recce_Or_Node::RULE_ID] =
                        $work_rule_id;
                    $predecessor_or_node
                        ->[Marpa::Internal::Recce_Or_Node::POSITION] =
                        $work_position;
                    $predecessor_or_node
                        ->[Marpa::Internal::Recce_Or_Node::ITEMS] =
                        [$predecessor];
                    $predecessor_or_node
                        ->[Marpa::Internal::Recce_Or_Node::SOURCE_OR_NODE] =
                        $work_node_name;
                    $predecessor_id =
                        ( push @{$or_nodes}, $predecessor_or_node ) - 1;

# say STDERR "DEBUG: Created o$predecessor_id: R$work_rule_id:$work_position";

                    Marpa::exception(
                        "Too many or-nodes for evaluator: $predecessor_id")
                        if $predecessor_id & ~(Marpa::Internal::N_FORMAT_MAX);
                    $predecessor_or_node->[Marpa::Internal::Recce_Or_Node::ID]
                        = $predecessor_id;
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

# say STDERR "DEBUG: Processing cause $cause_name";

                    FIND_CAUSE: {
                        my $cause_or_node =
                            $recce
                            ->[Marpa::Internal::Recognizer::OR_NODE_HASH]
                            ->{$cause_name};
                        if ($cause_or_node) {
                            $cause_id = $cause_or_node
                                ->[Marpa::Internal::Recce_Or_Node::ID];
                            last FIND_CAUSE
                                if $cause_or_node->[
                                    Marpa::Internal::Recce_Or_Node::SOURCE_OR_NODE
                                ] ne $work_node_name;

                            # If the working or node is the grandparent of this new or-node,
                            # we are building it, and need to populate the list of Earley items
                            push @{ $cause_or_node
                                    ->[Marpa::Internal::Recce_Or_Node::ITEMS]
                                },
                                $cause;
                            last FIND_CAUSE if $cause_or_node;
                        } ## end if ($cause_or_node)

                        $cause_or_node = [];
                        $cause_or_node->[Marpa::Internal::Recce_Or_Node::TAG]
                            = $cause_name;
                        $recce->[Marpa::Internal::Recognizer::OR_NODE_HASH]
                            ->{$cause_name} = $cause_or_node;
                        $cause_or_node
                            ->[Marpa::Internal::Recce_Or_Node::RULE_ID] =
                            $cause_rule_id;
                        $cause_or_node
                            ->[Marpa::Internal::Recce_Or_Node::POSITION] =
                            scalar
                            @{ $cause_rule->[Marpa::Internal::Rule::RHS] };
                        $cause_or_node
                            ->[Marpa::Internal::Recce_Or_Node::ITEMS] =
                            [$cause];
                        $cause_or_node
                            ->[Marpa::Internal::Recce_Or_Node::SOURCE_OR_NODE]
                            = $work_node_name;
                        $cause_id = ( push @{$or_nodes}, $cause_or_node ) - 1;

# say STDERR "DEBUG: Created o$cause_id: ", $cause_or_node->[Marpa::Internal::Recce_Or_Node::TAG];

                        Marpa::exception(
                            "Too many or-nodes for evaluator: $cause_id")
                            if $cause_id & ~(Marpa::Internal::N_FORMAT_MAX);
                        $cause_or_node->[Marpa::Internal::Recce_Or_Node::ID] =
                            $cause_id;
                    } ## end FIND_CAUSE:

# say STDERR "DEBUG: predecessor: ",
                    # $recce->show_recce_or_node( $or_nodes->[$predecessor_id], 99 )
                    # if defined $predecessor_id;
# say STDERR "DEBUG: cause: ",
                    # $recce->show_recce_or_node( $or_nodes->[$cause_id], 99 )
                    # if defined $cause_id;

                    my $and_node = [];
                    $and_node
                        ->[Marpa::Internal::Recce_And_Node::PREDECESSOR_ID] =
                        $predecessor_id;
                    $and_node
                        ->[Marpa::Internal::Recce_And_Node::CAUSE_EARLEME] =
                        $cause_earleme;
                    $and_node->[Marpa::Internal::Recce_And_Node::CAUSE_ID] =
                        $cause_id;

                    $and_node_data{ join q{:}, ( $predecessor_id // q{} ),
                        $cause_id } = $and_node;

                } ## end for my $cause_rule ( @{ $state->[...]})

                next LINK_WORK_ITEM;

            }    # if cause

            my $and_node = [];
            $and_node->[Marpa::Internal::Recce_And_Node::PREDECESSOR_ID] =
                $predecessor_id;
            $and_node->[Marpa::Internal::Recce_And_Node::CAUSE_EARLEME] =
                $cause_earleme;
            $and_node->[Marpa::Internal::Recce_And_Node::TOKEN_NAME] =
                $token_name;
            $and_node->[Marpa::Internal::Recce_And_Node::VALUE_REF] =
                $value_ref;

            $and_node_data{ join q{:}, ( $predecessor_id // q{} ), q{},
                $token_name } = $and_node;

        } ## end for my $link_work_item (@link_worklist)

        my @child_and_nodes = values %and_node_data;
        for my $and_node (@child_and_nodes) {

            $and_node->[Marpa::Internal::Recce_And_Node::RULE_ID] =
                $work_rule_id;

            $and_node->[Marpa::Internal::Recce_And_Node::VALUE_OPS] =
                $work_position
                == $#{ $work_rule->[Marpa::Internal::Rule::RHS] }
                ? $evaluator_rules->[ $work_rule->[Marpa::Internal::Rule::ID]
                ]
                : undef;

            $and_node->[Marpa::Internal::Recce_And_Node::POSITION] =
                $work_position;
            $and_node->[Marpa::Internal::Recce_And_Node::START_EARLEME] =
                $work_node_origin;
            $and_node->[Marpa::Internal::Recce_And_Node::END_EARLEME] =
                $work_set;
            my $id = ( push @{$and_nodes}, $and_node ) - 1;
            Marpa::exception("Too many and-nodes for evaluator: $id")
                if $id & ~(Marpa::Internal::N_FORMAT_MAX);
            $and_node->[Marpa::Internal::Recce_And_Node::ID] = $id;

            {
                my $token_name =
                    $and_node->[Marpa::Internal::Recce_And_Node::TOKEN_NAME];
                my $cause_earleme = $and_node
                    ->[Marpa::Internal::Recce_And_Node::CAUSE_EARLEME];
                my $tag            = q{};
                my $predecessor_id = $and_node
                    ->[Marpa::Internal::Recce_And_Node::PREDECESSOR_ID];
                my $predecessor_or_node =
                    $predecessor_id ? $or_nodes->[$predecessor_id] : undef;
                $predecessor_or_node
                    and $tag
                    .= $predecessor_or_node
                    ->[Marpa::Internal::Recce_Or_Node::TAG];
                my $cause_id =
                    $and_node->[Marpa::Internal::Recce_And_Node::CAUSE_ID];
                my $cause_or_node =
                    $cause_id ? $or_nodes->[$cause_id] : undef;
                $cause_or_node
                    and $tag
                    .= $cause_or_node->[Marpa::Internal::Recce_Or_Node::TAG];
                $token_name
                    and $tag
                    .= q{T@}
                    . $cause_earleme . q{-}
                    . $work_set . q{_}
                    . $token_name;
                $and_node->[Marpa::Internal::Recce_And_Node::TAG] = $tag;
            }
        } ## end for my $and_node (@child_and_nodes)

        # Populate the or-node, now that we have ID's for all the and-nodes
        $work_or_node->[Marpa::Internal::Recce_Or_Node::AND_NODE_IDS] =
            [ map { $_->[Marpa::Internal::Recce_And_Node::ID] }
                @child_and_nodes ];

        my $and_node = $child_and_nodes[0];

        my $predecessor_id =
            $and_node->[Marpa::Internal::Recce_And_Node::PREDECESSOR_ID];

        my $predecessor_or_node =
            $predecessor_id ? $or_nodes->[$predecessor_id] : undef;
        my $cause_id = $and_node->[Marpa::Internal::Recce_And_Node::CAUSE_ID];

        my $cause_or_node = $cause_id ? $or_nodes->[$cause_id] : undef;
        push @or_worklist,
            map { [ $_, 0 ] } grep {defined} $predecessor_or_node,
            $cause_or_node;

        # For now, just push the first and-node on the evaluation stack
        push @stack, $and_node;

    } ## end while ( my $or_workitem = pop @or_worklist )

    return Marpa::Internal::Evaluator::evaluate( $grammar, $action_object,
        \@stack, $trace_values );

} ## end sub Marpa::Recognizer::value

1;
