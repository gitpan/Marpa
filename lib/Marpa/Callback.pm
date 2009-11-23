package Marpa::Internal::Callback;

use 5.010;
use warnings;
use strict;
use integer;

# use Smart::Comments '-ENV';

### Using smart comments <where>...

use English qw( -no_match_vars );

use Marpa::Internal;
use Marpa::Evaluator;

sub Marpa::location {
    Marpa::exception('No context for location tie')
        if not my $context = $Marpa::Internal::CONTEXT;
    my ( $context_type, $and_node ) = @{$context};
    Marpa::exception('LOCATION called outside and-node context')
        if not $context_type ~~ [ 'setup and-node', 'rank and-node' ];
    return $and_node->[Marpa::Internal::And_Node::START_EARLEME];
} ## end sub Marpa::location

sub Marpa::token_location {
    Marpa::exception('No context for TOKEN_LOCATION tie')
        if not my $context = $Marpa::Internal::CONTEXT;
    my ( $context_type, $and_node ) = @{$context};
    Marpa::exception('TOKEN_LOCATION called outside and-node context')
        if not $context_type ~~ [ 'setup and-node', 'rank and-node' ];
    my $predecessor_id =
        $and_node->[Marpa::Internal::And_Node::PREDECESSOR_ID];

    return $and_node->[Marpa::Internal::And_Node::START_EARLEME]
        if not defined $predecessor_id;
    my $eval_instance = $Marpa::Internal::EVAL_INSTANCE;
    my $or_nodes = $eval_instance->[Marpa::Internal::Evaluator::OR_NODES];

    return $or_nodes->[$predecessor_id]
        ->[Marpa::Internal::Or_Node::END_EARLEME];
} ## end sub Marpa::token_location

sub Marpa::length {
    Marpa::exception('No context for LENGTH tie')
        if not my $context = $Marpa::Internal::CONTEXT;
    my ( $context_type, $and_node ) = @{$context};
    Marpa::exception('LENGTH called outside and-node context')
        if not $context_type ~~ [ 'setup and-node', 'rank and-node' ];
    return $and_node->[Marpa::Internal::And_Node::END_EARLEME]
        - $and_node->[Marpa::Internal::And_Node::START_EARLEME];
} ## end sub Marpa::length

1;
