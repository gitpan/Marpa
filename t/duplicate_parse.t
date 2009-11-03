#!/usr/bin/perl

# Test of deletion of duplicate parses.

use 5.010;
use strict;
use warnings;

use Test::More tests => 4;

use lib 'lib';
use t::lib::Marpa::Test;

BEGIN {
    Test::More::use_ok('Marpa');
}

## no critic (Subroutines::RequireArgUnpacking)

sub default_action {
    shift;
    my $v_count = scalar @_;
    return q{} if $v_count <= 0;
    my @vals = map { $_ // q{-} } @_;
    return $vals[0] if $v_count == 1;
    return '(' . join( q{;}, @vals ) . ')';
} ## end sub default_action

## use critic

my $grammar = Marpa::Grammar->new(
    {   start => 'S',
        strip => 0,

        # Set max at 10 just in case there's an infinite loop.
        # This is for debugging, after all
        max_parses => 10,

        rules => [
            [ 'S', [qw/p p p n/], ],
            [ 'p', ['a'], ],
            [ 'p', [], ],
            [ 'n', ['a'], ],
        ],
        terminals      => ['a'],
        default_action => 'main::default_action',

    }
);

$grammar->precompute();

Marpa::Test::is( $grammar->show_rules,
    <<'END_OF_STRING', 'final nonnulling Rules' );
0: S -> p p p n /* !useful */
1: p -> a
2: p -> /* empty !useful nullable */
3: n -> a
4: S -> p p S[R0:2][x5] /* vrhs real=2 */
5: S -> p p[] S[R0:2][x5] /* vrhs real=2 */
6: S -> p[] p S[R0:2][x5] /* vrhs real=2 */
7: S -> p[] p[] S[R0:2][x5] /* vrhs real=2 */
8: S[R0:2][x5] -> p n /* vlhs real=2 */
9: S[R0:2][x5] -> p[] n /* vlhs real=2 */
10: S['] -> S /* vlhs real=1 */
END_OF_STRING

Marpa::Test::is( $grammar->show_QDFA,
    <<'END_OF_STRING', 'final nonnulling QDFA' );
Start States: S0; S1
S0: 27
S['] -> . S
 <S> => S2
S1: predict; 1,3,5,9,14,19,21,25
p -> . a
n -> . a
S -> . p p S[R0:2][x5]
S -> . p p[] S[R0:2][x5]
S -> p[] . p S[R0:2][x5]
S -> p[] p[] . S[R0:2][x5]
S[R0:2][x5] -> . p n
S[R0:2][x5] -> p[] . n
 <S[R0:2][x5]> => S3
 <a> => S4
 <n> => S5
 <p> => S6; S7
S2: 28
S['] -> S .
S3: 20
S -> p[] p[] S[R0:2][x5] .
S4: 2,4
p -> a .
n -> a .
S5: 26
S[R0:2][x5] -> p[] n .
S6: 6,11,15,22
S -> p . p S[R0:2][x5]
S -> p p[] . S[R0:2][x5]
S -> p[] p . S[R0:2][x5]
S[R0:2][x5] -> p . n
 <S[R0:2][x5]> => S8
 <n> => S9
 <p> => S10; S7
S7: predict; 1,3,21,25
p -> . a
n -> . a
S[R0:2][x5] -> . p n
S[R0:2][x5] -> p[] . n
 <a> => S4
 <n> => S5
 <p> => S11; S12
S8: 12,16
S -> p p[] S[R0:2][x5] .
S -> p[] p S[R0:2][x5] .
S9: 23
S[R0:2][x5] -> p n .
S10: 7
S -> p p . S[R0:2][x5]
 <S[R0:2][x5]> => S13
S11: 22
S[R0:2][x5] -> p . n
 <n> => S9
S12: predict; 3
n -> . a
 <a> => S14
S13: 8
S -> p p S[R0:2][x5] .
S14: 4
n -> a .
END_OF_STRING

use constant SPACE => 0x60;

my $input_length = 3;
my $recce = Marpa::Recognizer->new( { grammar => $grammar } );
$recce->tokens(
    [ map { [ 'a', chr( SPACE + $_ ), 1 ] } ( 1 .. $input_length ) ] );
my $evaler = Marpa::Evaluator->new( { recce => $recce, clone => 0 } );

my $bocage = $evaler->show_bocage(3);

Marpa::Test::is( $bocage, <<'END_OF_STRING', 'Bocage' );
parse count: 0
S2@0-3L6o0 -> S2@0-3L6o0a0
S2@0-3L6o0a0 -> S13@0-3L1o1
    rule 10: S['] -> . S
    value_ops
S2@0-3L6o0 -> S2@0-3L6o0a1
S2@0-3L6o0a1 -> S8@0-3L1o2
    rule 10: S['] -> . S
    value_ops
S13@0-3L1o1 -> S13@0-3L1o1a2
S13@0-3L1o1a2 -> S10@0-2R4:2o3 S5@2-3L5o4
    rule 4: S -> p p . S[R0:2][x5]
    (part of 0) S -> < p > < p > . < p n >
    value_ops
S8@0-3L1o2 -> S8@0-3L1o2a3
S8@0-3L1o2a3 -> S6@0-1R5:2o5 S9@1-3L5o6
    rule 5: S -> p p[] . S[R0:2][x5]
    (part of 0) S -> < p > < p > . < p n >
    value_ops
S8@0-3L1o2 -> S8@0-3L1o2a4
S8@0-3L1o2a4 -> S6@0-1R6:2o7 S9@1-3L5o6
    rule 6: S -> p[] p . S[R0:2][x5]
    (part of 0) S -> < p > < p > . < p n >
    value_ops
S10@0-2R4:2o3 -> S10@0-2R4:2o3a5
S10@0-2R4:2o3a5 -> S6@0-1R4:1o8 S4@1-2L2o9
    rule 4: S -> p . p S[R0:2][x5]
    (part of 0) S -> < p > . < p > < p n >
S5@2-3L5o4 -> S5@2-3L5o4a6
S5@2-3L5o4a6 -> S7@2-2R9:1o10 S4@2-3L3o11
    rule 9: S[R0:2][x5] -> p[] . n
    (part of 0) S -> p p < p > . < n >
    value_ops
S6@0-1R5:2o5 -> S6@0-1R5:2o5a8
S6@0-1R5:2o5a8 -> S6@0-1R5:1o13 undef
    rule 5: S -> p . p[] S[R0:2][x5]
    (part of 0) S -> < p > . < p > < p n >
S9@1-3L5o6 -> S9@1-3L5o6a9
S9@1-3L5o6a9 -> S11@1-2R8:1o14 S4@2-3L3o11
    rule 8: S[R0:2][x5] -> p . n
    (part of 0) S -> p p < p > . < n >
    value_ops
S6@0-1R6:2o7 -> S6@0-1R6:2o7a11
S6@0-1R6:2o7a11 -> S1@0-0R6:1o15 S4@0-1L2o16
    rule 6: S -> p[] . p S[R0:2][x5]
    (part of 0) S -> < p > . < p > < p n >
S6@0-1R4:1o8 -> S6@0-1R4:1o8a12
S6@0-1R4:1o8a12 -> S4@0-1L2o16
    rule 4: S -> . p p S[R0:2][x5]
    (part of 0) S -> . < p > < p > < p n >
S4@1-2L2o9 -> S4@1-2L2o9a13
S4@1-2L2o9a13 -> 'b'
    rule 1: p -> . a
    value_ops
S7@2-2R9:1o10 -> S7@2-2R9:1o10a14
S7@2-2R9:1o10a14 -> undef
    rule 9: S[R0:2][x5] -> . p[] n
    (part of 0) S -> p p . < p > < n >
S4@2-3L3o11 -> S4@2-3L3o11a15
S4@2-3L3o11a15 -> 'c'
    rule 3: n -> . a
    value_ops
S6@0-1R5:1o13 -> S6@0-1R5:1o13a17
S6@0-1R5:1o13a17 -> S4@0-1L2o16
    rule 5: S -> . p p[] S[R0:2][x5]
    (part of 0) S -> . < p > < p > < p n >
S11@1-2R8:1o14 -> S11@1-2R8:1o14a18
S11@1-2R8:1o14a18 -> S4@1-2L2o9
    rule 8: S[R0:2][x5] -> . p n
    (part of 0) S -> p p . < p > < n >
S1@0-0R6:1o15 -> S1@0-0R6:1o15a19
S1@0-0R6:1o15a19 -> undef
    rule 6: S -> . p[] p S[R0:2][x5]
    (part of 0) S -> . < p > < p > < p n >
S4@0-1L2o16 -> S4@0-1L2o16a20
S4@0-1L2o16a20 -> 'a'
    rule 1: p -> . a
    value_ops
END_OF_STRING

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
