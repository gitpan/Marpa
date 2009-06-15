#!perl

# the example grammar in Aycock/Horspool "Practical Earley Parsing",
# _The Computer Journal_, Vol. 45, No. 6, pp. 620-630,
# in its "NNF" form

use 5.010;
use strict;
use warnings;

use Test::More tests => 20;

use lib 'lib';
use lib 't/lib';
use Marpa::Test;

BEGIN {
    Test::More::use_ok('Marpa');
}

my $grammar = Marpa::Grammar->new(
    {   precompute => 0,
        start      => 'S',
        strip      => 0,
        rules      => [
            [ 'S', [qw/A A A A/] ],
            [ 'A', [qw/a/] ],
            [ 'A', [qw/E/] ],
            ['E'],
        ],
        default_null_value => q{},
        default_action     => <<'EOCODE'
     my $v_count = scalar @_;
     return q{} if $v_count <= 0;
     return $_[0] if $v_count == 1;
     '(' . join(';', @_) . ')';
EOCODE
    }
);

$grammar->set( { terminals => ['a'], } );

$grammar->precompute();

Marpa::Test::is( $grammar->show_rules, <<'EOS', 'Aycock/Horspool Rules' );
0: S -> A A A A /* !useful nullable */
1: A -> a
2: A -> E /* !useful nullable nulling */
3: E -> /* !useful empty nullable nulling */
4: S -> A S[R0:1][x6] /* priority=0.44 */
5: S -> A[] S[R0:1][x6] /* priority=0.42 */
6: S -> A S[R0:1][x6][] /* priority=0.43 */
7: S[R0:1][x6] -> A S[R0:2][x8] /* priority=0.34 */
8: S[R0:1][x6] -> A[] S[R0:2][x8] /* priority=0.32 */
9: S[R0:1][x6] -> A S[R0:2][x8][] /* priority=0.33 */
10: S[R0:2][x8] -> A A /* priority=0.24 */
11: S[R0:2][x8] -> A[] A /* priority=0.22 */
12: S[R0:2][x8] -> A A[] /* priority=0.23 */
13: S['] -> S
14: S['][] -> /* empty nullable nulling */
EOS

Marpa::Test::is( $grammar->show_symbols, <<'EOS', 'Aycock/Horspool Symbols' );
0: S, lhs=[0 4 5 6] rhs=[13]
1: A, lhs=[1 2] rhs=[0 4 6 7 9 10 11 12]
2: a, lhs=[] rhs=[1] terminal
3: E, lhs=[3] rhs=[2] nullable nulling
4: S[], lhs=[] rhs=[] nullable nulling
5: A[], lhs=[] rhs=[5 8 11 12] nullable nulling
6: S[R0:1][x6], lhs=[7 8 9] rhs=[4 5]
7: S[R0:1][x6][], lhs=[] rhs=[6] nullable nulling
8: S[R0:2][x8], lhs=[10 11 12] rhs=[7 8]
9: S[R0:2][x8][], lhs=[] rhs=[9] nullable nulling
10: S['], lhs=[13] rhs=[]
11: S['][], lhs=[14] rhs=[] nullable nulling
EOS

Marpa::Test::is(
    $grammar->show_nullable_symbols,
    q{A[] E S['][] S[R0:1][x6][] S[R0:2][x8][] S[]},
    'Aycock/Horspool Nullable Symbols'
);
Marpa::Test::is(
    $grammar->show_nulling_symbols,
    q{A[] E S['][] S[R0:1][x6][] S[R0:2][x8][] S[]},
    'Aycock/Horspool Nulling Symbols'
);
Marpa::Test::is(
    $grammar->show_productive_symbols,
    q{A A[] E S S['] S['][] S[R0:1][x6] S[R0:1][x6][] S[R0:2][x8] S[R0:2][x8][] S[] a},
    'Aycock/Horspool Productive Symbols'
);
Marpa::Test::is(
    $grammar->show_accessible_symbols,
    q{A A[] E S S['] S['][] S[R0:1][x6] S[R0:1][x6][] S[R0:2][x8] S[R0:2][x8][] S[] a},
    'Aycock/Horspool Accessible Symbols'
);

Marpa::Test::is( $grammar->show_NFA, <<'EOS', 'Aycock/Horspool NFA' );
S0: /* empty */
 empty => S30 S32
S1: A ::= . a
 <a> => S2
S2: A ::= a .
S3: S ::= . A S[R0:1][x6]
 empty => S1
 <A> => S4
S4: S ::= A . S[R0:1][x6]
 empty => S12 S15 S18
 <S[R0:1][x6]> => S5
S5: S ::= A S[R0:1][x6] .
S6: S ::= . A[] S[R0:1][x6]
at_nulling
 <A[]> => S7
S7: S ::= A[] . S[R0:1][x6]
 empty => S12 S15 S18
 <S[R0:1][x6]> => S8
S8: S ::= A[] S[R0:1][x6] .
S9: S ::= . A S[R0:1][x6][]
 empty => S1
 <A> => S10
S10: S ::= A . S[R0:1][x6][]
at_nulling
 <S[R0:1][x6][]> => S11
S11: S ::= A S[R0:1][x6][] .
S12: S[R0:1][x6] ::= . A S[R0:2][x8]
 empty => S1
 <A> => S13
S13: S[R0:1][x6] ::= A . S[R0:2][x8]
 empty => S21 S24 S27
 <S[R0:2][x8]> => S14
S14: S[R0:1][x6] ::= A S[R0:2][x8] .
S15: S[R0:1][x6] ::= . A[] S[R0:2][x8]
at_nulling
 <A[]> => S16
S16: S[R0:1][x6] ::= A[] . S[R0:2][x8]
 empty => S21 S24 S27
 <S[R0:2][x8]> => S17
S17: S[R0:1][x6] ::= A[] S[R0:2][x8] .
S18: S[R0:1][x6] ::= . A S[R0:2][x8][]
 empty => S1
 <A> => S19
S19: S[R0:1][x6] ::= A . S[R0:2][x8][]
at_nulling
 <S[R0:2][x8][]> => S20
S20: S[R0:1][x6] ::= A S[R0:2][x8][] .
S21: S[R0:2][x8] ::= . A A
 empty => S1
 <A> => S22
S22: S[R0:2][x8] ::= A . A
 empty => S1
 <A> => S23
S23: S[R0:2][x8] ::= A A .
S24: S[R0:2][x8] ::= . A[] A
at_nulling
 <A[]> => S25
S25: S[R0:2][x8] ::= A[] . A
 empty => S1
 <A> => S26
S26: S[R0:2][x8] ::= A[] A .
S27: S[R0:2][x8] ::= . A A[]
 empty => S1
 <A> => S28
S28: S[R0:2][x8] ::= A . A[]
at_nulling
 <A[]> => S29
S29: S[R0:2][x8] ::= A A[] .
S30: S['] ::= . S
 empty => S3 S6 S9
 <S> => S31
S31: S['] ::= S .
S32: S['][] ::= .
EOS

Marpa::Test::is( $grammar->show_QDFA, <<'EOS', 'Aycock/Horspool QDFA' );
Start States: S0; S1
S0: 30,32
S['] ::= . S
S['][] ::= .
 <S> => S2
S1: predict; 1,3,7,9,12,16,18,21,25,27
A ::= . a
S ::= . A S[R0:1][x6]
S ::= A[] . S[R0:1][x6]
S ::= . A S[R0:1][x6][]
S[R0:1][x6] ::= . A S[R0:2][x8]
S[R0:1][x6] ::= A[] . S[R0:2][x8]
S[R0:1][x6] ::= . A S[R0:2][x8][]
S[R0:2][x8] ::= . A A
S[R0:2][x8] ::= A[] . A
S[R0:2][x8] ::= . A A[]
 <A> => S3; S4
 <S[R0:1][x6]> => S5
 <S[R0:2][x8]> => S6
 <a> => S7
S2: 31
S['] ::= S .
S3: 4,11,13,20,22,26,29
S ::= A . S[R0:1][x6]
S ::= A S[R0:1][x6][] .
S[R0:1][x6] ::= A . S[R0:2][x8]
S[R0:1][x6] ::= A S[R0:2][x8][] .
S[R0:2][x8] ::= A . A
S[R0:2][x8] ::= A[] A .
S[R0:2][x8] ::= A A[] .
 <A> => S8
 <S[R0:1][x6]> => S9
 <S[R0:2][x8]> => S10
S4: predict; 1,12,16,18,21,25,27
A ::= . a
S[R0:1][x6] ::= . A S[R0:2][x8]
S[R0:1][x6] ::= A[] . S[R0:2][x8]
S[R0:1][x6] ::= . A S[R0:2][x8][]
S[R0:2][x8] ::= . A A
S[R0:2][x8] ::= A[] . A
S[R0:2][x8] ::= . A A[]
 <A> => S11; S12
 <S[R0:2][x8]> => S6
 <a> => S7
S5: 8
S ::= A[] S[R0:1][x6] .
S6: 17
S[R0:1][x6] ::= A[] S[R0:2][x8] .
S7: 2
A ::= a .
S8: 23
S[R0:2][x8] ::= A A .
S9: 5
S ::= A S[R0:1][x6] .
S10: 14
S[R0:1][x6] ::= A S[R0:2][x8] .
S11: 13,20,22,26,29
S[R0:1][x6] ::= A . S[R0:2][x8]
S[R0:1][x6] ::= A S[R0:2][x8][] .
S[R0:2][x8] ::= A . A
S[R0:2][x8] ::= A[] A .
S[R0:2][x8] ::= A A[] .
 <A> => S8
 <S[R0:2][x8]> => S10
S12: predict; 1,21,25,27
A ::= . a
S[R0:2][x8] ::= . A A
S[R0:2][x8] ::= A[] . A
S[R0:2][x8] ::= . A A[]
 <A> => S13; S14
 <a> => S7
S13: 22,26,29
S[R0:2][x8] ::= A . A
S[R0:2][x8] ::= A[] A .
S[R0:2][x8] ::= A A[] .
 <A> => S8
S14: predict; 1
A ::= . a
 <a> => S7
EOS

my $recce = Marpa::Recognizer->new( { grammar => $grammar, clone => 0 } );

my $set0_new = <<'EOS';
Earley Set 0
S0@0-0
S1@0-0
EOS

my $set1_at_0 = <<'EOS';
Earley Set 1
S7@0-1 [p=S1@0-0; t='a']
EOS

my $set1_at_1 = <<'EOS';
S3@0-1 [p=S1@0-0; c=S7@0-1]
S4@1-1
S2@0-1 [p=S0@0-0; c=S3@0-1] [p=S0@0-0; c=S5@0-1]
S5@0-1 [p=S1@0-0; c=S3@0-1] [p=S1@0-0; c=S6@0-1]
S6@0-1 [p=S1@0-0; c=S3@0-1]
EOS

my $set2_at_1 = <<'EOS';
Earley Set 2
S7@1-2 [p=S4@1-1; t='a']
EOS

my $set2_at_2 = <<'EOS';
S8@0-2 [p=S3@0-1; c=S7@1-2]
S11@1-2 [p=S4@1-1; c=S7@1-2]
S12@2-2
S6@0-2 [p=S1@0-0; c=S8@0-2]
S9@0-2 [p=S3@0-1; c=S11@1-2] [p=S3@0-1; c=S6@1-2]
S10@0-2 [p=S3@0-1; c=S11@1-2]
S6@1-2 [p=S4@1-1; c=S11@1-2]
S5@0-2 [p=S1@0-0; c=S6@0-2] [p=S1@0-0; c=S10@0-2]
S2@0-2 [p=S0@0-0; c=S9@0-2] [p=S0@0-0; c=S5@0-2]
EOS

my $set3_at_2 = <<'EOS';
Earley Set 3
S7@2-3 [p=S12@2-2; t='a']
EOS

my $set3_at_3 = <<'EOS';
S8@1-3 [p=S11@1-2; c=S7@2-3]
S13@2-3 [p=S12@2-2; c=S7@2-3]
S14@3-3
S10@0-3 [p=S3@0-1; c=S8@1-3]
S6@1-3 [p=S4@1-1; c=S8@1-3]
S10@1-3 [p=S11@1-2; c=S13@2-3]
S5@0-3 [p=S1@0-0; c=S10@0-3]
S9@0-3 [p=S3@0-1; c=S6@1-3] [p=S3@0-1; c=S10@1-3]
S2@0-3 [p=S0@0-0; c=S5@0-3] [p=S0@0-0; c=S9@0-3]
EOS

my $set4_at_3 = <<'EOS';
Earley Set 4
S7@3-4 [p=S14@3-3; t='a']
EOS

my $set4_at_4 = <<'EOS';
S8@2-4 [p=S13@2-3; c=S7@3-4]
S10@1-4 [p=S11@1-2; c=S8@2-4]
S9@0-4 [p=S3@0-1; c=S10@1-4]
S2@0-4 [p=S0@0-0; c=S9@0-4]
EOS

my $sets_new  = $set0_new;
my $sets_at_0 = $sets_new . $set1_at_0;
my $sets_at_1 = $sets_at_0 . $set1_at_1 . $set2_at_1;
my $sets_at_2 = $sets_at_1 . $set2_at_2 . $set3_at_2;
my $sets_at_3 = $sets_at_2 . $set3_at_3 . $set4_at_3;
my $sets_at_4 = $sets_at_3 . $set4_at_4;

Marpa::Test::is(
    $recce->show_earley_sets(1),
    "Current Earley Set: 0; Furthest: 0\n" . $sets_new,
    'Aycock/Horspool Parse Status before parse'
);

my $a = $grammar->get_symbol('a');
$recce->earleme( [ $a, 'a', 1 ] ) or Marpa::exception('Parsing exhausted');

Marpa::Test::is(
    $recce->show_earley_sets(1),
    "Current Earley Set: 1; Furthest: 1\n" . $sets_at_0,
    'Aycock/Horspool Parse Status at 0'
);

$recce->earleme( [ $a, 'a', 1 ] ) or Marpa::exception('Parsing exhausted');

Marpa::Test::is(
    $recce->show_earley_sets(1),
    "Current Earley Set: 2; Furthest: 2\n" . $sets_at_1,
    'Aycock/Horspool Parse Status at 1'
);

$recce->earleme( [ $a, 'a', 1 ] ) or Marpa::exception('Parsing exhausted');

Marpa::Test::is(
    $recce->show_earley_sets(1),
    "Current Earley Set: 3; Furthest: 3\n" . $sets_at_2,
    'Aycock/Horspool Parse Status at 2'
);

$recce->earleme( [ $a, 'a', 1 ] ) or Marpa::exception('Parsing exhausted');

Marpa::Test::is(
    $recce->show_earley_sets(1),
    "Current Earley Set: 4; Furthest: 4\n" . $sets_at_3,
    'Aycock/Horspool Parse Status at 3'
);

$recce->end_input();

Marpa::Test::is(
    $recce->show_earley_sets(1),
    "Current Earley Set: 4; Furthest: 4\n" . $sets_at_4,
    'Aycock/Horspool Parse Status at 4'
);

my @answer = ( q{}, qw[(a;;;) (a;a;;) (a;a;a;) (a;a;a;a)] );

for my $i ( 0 .. 4 ) {
    my $evaler = Marpa::Evaluator->new(
        {   recce => $recce,
            end   => $i,
            clone => 0,
        }
    );
    my $result = $evaler->value();
    TODO: {
        ## no critic (ControlStructures::ProhibitPostfixControls)
        local $TODO = 'new evaluator not yet written' if $i == 3;
        ## use critic
        Test::More::is( ${$result}, $answer[$i], "parse permutation $i" );
    } ## end TODO:
} ## end for my $i ( 0 .. 4 )

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
