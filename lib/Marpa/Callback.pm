package Marpa::Internal::Callback;

use 5.010;
use warnings;
use strict;
use integer;

use English qw( -no_match_vars );

use Marpa::Internal::Carp_Not;

sub Marpa::location {
    Marpa::exception('No context for location callback')
        if not my $context = $Marpa::Internal::CONTEXT;
    my ( $context_type, $and_node ) = @{$context};
    if ( $context_type ~~ [ 'setup eval and-node', 'rank eval and-node' ] ) {
        return $and_node->[Marpa::Internal::Eval_And_Node::START_EARLEME];
    }
    if ( $context_type eq 'and-node' ) {
        return $and_node->[Marpa::Internal::And_Node::START_EARLEME];
    }
    Marpa::exception('LOCATION called outside and-node context');
} ## end sub Marpa::location

sub Marpa::cause_location {
    Marpa::exception('No context for cause_location callback')
        if not my $context = $Marpa::Internal::CONTEXT;
    my ( $context_type, $and_node ) = @{$context};
    if ( $context_type ~~ [ 'setup eval and-node', 'rank eval and-node' ] ) {
        return $and_node->[Marpa::Internal::Eval_And_Node::CAUSE_EARLEME];
    }
    if ( $context_type eq 'and-node' ) {
        return $and_node->[Marpa::Internal::And_Node::CAUSE_EARLEME];
    }
    Marpa::exception('cause_location() called outside and-node context');
} ## end sub Marpa::cause_location

no strict 'refs';
*{'Marpa::token_location'} = \&Marpa::cause_location;
use strict;

sub Marpa::length {
    Marpa::exception('No context for LENGTH tie')
        if not my $context = $Marpa::Internal::CONTEXT;
    my ( $context_type, $and_node ) = @{$context};
    if ( $context_type ~~ [ 'setup eval and-node', 'rank eval and-node' ] ) {
        return $and_node->[Marpa::Internal::Eval_And_Node::END_EARLEME]
            - $and_node->[Marpa::Internal::Eval_And_Node::START_EARLEME];
    }
    if ( $context_type eq 'and-node' ) {
        return $and_node->[Marpa::Internal::And_Node::END_EARLEME]
            - $and_node->[Marpa::Internal::And_Node::START_EARLEME];
    }
    Marpa::exception('LENGTH called outside and-node context');
} ## end sub Marpa::length

1;
