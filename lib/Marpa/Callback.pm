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

sub Marpa::cause_location {
    Marpa::exception('No context for cause_location')
        if not my $context = $Marpa::Internal::CONTEXT;
    my ( $context_type, $and_node ) = @{$context};
    Marpa::exception('cause_location() called outside and-node context')
        if not $context_type ~~ [ 'setup and-node', 'rank and-node' ];
    return $and_node->[Marpa::Internal::And_Node::CAUSE_EARLEME];
} ## end sub Marpa::cause_location

no strict 'refs';
*{'Marpa::token_location'} = \&Marpa::cause_location;
use strict;

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
