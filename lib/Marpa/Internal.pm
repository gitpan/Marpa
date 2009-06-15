package Marpa::Internal;

use 5.010;
use strict;
use warnings;
use integer;
use Carp;

*Marpa::exception = \&Carp::croak;

our @CARP_NOT = qw(
    Marpa
    Marpa::Evaluator
    Marpa::Grammar
    Marpa::Internal
    Marpa::Internal::And_Node
    Marpa::Internal::Earley_Item
    Marpa::Internal::Evaluator
    Marpa::Internal::Evaluator::Rule
    Marpa::Internal::Grammar
    Marpa::Internal::Interface
    Marpa::Internal::Lex
    Marpa::Internal::LR0_item
    Marpa::Internal::NFA
    Marpa::Internal::Or_Node
    Marpa::Internal::Or_Sapling
    Marpa::Internal::Phase
    Marpa::Internal::QDFA
    Marpa::Internal::Recognizer
    Marpa::Internal::Rule
    Marpa::Internal::Source_Eval
    Marpa::Internal::Source_Raw
    Marpa::Internal::Symbol
    Marpa::Internal::Tree_Node
    Marpa::Lex
    Marpa::MDL
    Marpa::Recognizer
);

1;
