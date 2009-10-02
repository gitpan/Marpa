#!perl

# the example grammar in Aycock/Horspool "Practical Earley Parsing",
# _The Computer Journal_, Vol. 45, No. 6, pp. 620-630,
# in its "NNF" form

use 5.010;
use strict;
use warnings;

use lib 'lib';
use Test::More tests => 20;
use t::lib::Marpa::Test;

BEGIN {
    Test::More::use_ok('Marpa');
}

my $grammar = Marpa::Grammar->new(
    {   precompute => 0,
        start      => 'S',
        strip      => 0,
        maximal    => 1,
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
0: S -> A A A A /* !useful nullable maximal */
1: A -> a /* maximal */
2: A -> E /* !useful nullable maximal */
3: E -> /* !useful empty nullable maximal */
4: S -> A S[R0:1][x6] /* maximal vrhs real=1 */
5: S -> A A[] A[] A[] /* maximal */
6: S -> A[] S[R0:1][x6] /* maximal vrhs real=1 */
7: S[R0:1][x6] -> A S[R0:2][x7] /* maximal vlhs vrhs real=1 */
8: S[R0:1][x6] -> A A[] A[] /* maximal vlhs real=3 */
9: S[R0:1][x6] -> A[] S[R0:2][x7] /* maximal vlhs vrhs real=1 */
10: S[R0:2][x7] -> A A /* maximal vlhs real=2 */
11: S[R0:2][x7] -> A A[] /* maximal vlhs real=2 */
12: S[R0:2][x7] -> A[] A /* maximal vlhs real=2 */
13: S['] -> S /* maximal */
14: S['][] -> /* empty nullable maximal */
EOS

Marpa::Test::is( $grammar->show_symbols, <<'EOS', 'Aycock/Horspool Symbols' );
0: S, lhs=[0 4 5 6] rhs=[13]
1: A, lhs=[1 2] rhs=[0 4 5 7 8 10 11 12]
2: a, lhs=[] rhs=[1] terminal maximal
3: E, lhs=[3] rhs=[2] nullable=1 nulling
4: S[], lhs=[] rhs=[] nullable=4 nulling
5: A[], lhs=[] rhs=[5 6 8 9 11 12] nullable=1 nulling
6: S[R0:1][x6], lhs=[7 8 9] rhs=[4 6]
7: S[R0:2][x7], lhs=[10 11 12] rhs=[7 9]
8: S['], lhs=[13] rhs=[]
9: S['][], lhs=[14] rhs=[] nullable=1 nulling
EOS

Marpa::Test::is(
    $grammar->show_nullable_symbols,
    q{A[] E S['][] S[]},
    'Aycock/Horspool Nullable Symbols'
);
Marpa::Test::is(
    $grammar->show_nulling_symbols,
    q{A[] E S['][] S[]},
    'Aycock/Horspool Nulling Symbols'
);
Marpa::Test::is(
    $grammar->show_productive_symbols,
    q{A A[] E S S['] S['][] S[R0:1][x6] S[R0:2][x7] S[] a},
    'Aycock/Horspool Productive Symbols'
);
Marpa::Test::is(
    $grammar->show_accessible_symbols,
    q{A A[] E S S['] S['][] S[R0:1][x6] S[R0:2][x7] S[] a},
    'Aycock/Horspool Accessible Symbols'
);

Marpa::Test::is( $grammar->show_NFA, <<'EOS', 'Aycock/Horspool NFA' );
S0: /* empty */
 empty => S33 S35
S1: A -> . a
 <a> => S2
S2: A -> a .
S3: S -> . A S[R0:1][x6]
 empty => S1
 <A> => S4
S4: S -> A . S[R0:1][x6]
 empty => S14 S17 S21
 <S[R0:1][x6]> => S5
S5: S -> A S[R0:1][x6] .
S6: S -> . A A[] A[] A[]
 empty => S1
 <A> => S7
S7: S -> A . A[] A[] A[]
at_nulling
 <A[]> => S8
S8: S -> A A[] . A[] A[]
at_nulling
 <A[]> => S9
S9: S -> A A[] A[] . A[]
at_nulling
 <A[]> => S10
S10: S -> A A[] A[] A[] .
S11: S -> . A[] S[R0:1][x6]
at_nulling
 <A[]> => S12
S12: S -> A[] . S[R0:1][x6]
 empty => S14 S17 S21
 <S[R0:1][x6]> => S13
S13: S -> A[] S[R0:1][x6] .
S14: S[R0:1][x6] -> . A S[R0:2][x7]
 empty => S1
 <A> => S15
S15: S[R0:1][x6] -> A . S[R0:2][x7]
 empty => S24 S27 S30
 <S[R0:2][x7]> => S16
S16: S[R0:1][x6] -> A S[R0:2][x7] .
S17: S[R0:1][x6] -> . A A[] A[]
 empty => S1
 <A> => S18
S18: S[R0:1][x6] -> A . A[] A[]
at_nulling
 <A[]> => S19
S19: S[R0:1][x6] -> A A[] . A[]
at_nulling
 <A[]> => S20
S20: S[R0:1][x6] -> A A[] A[] .
S21: S[R0:1][x6] -> . A[] S[R0:2][x7]
at_nulling
 <A[]> => S22
S22: S[R0:1][x6] -> A[] . S[R0:2][x7]
 empty => S24 S27 S30
 <S[R0:2][x7]> => S23
S23: S[R0:1][x6] -> A[] S[R0:2][x7] .
S24: S[R0:2][x7] -> . A A
 empty => S1
 <A> => S25
S25: S[R0:2][x7] -> A . A
 empty => S1
 <A> => S26
S26: S[R0:2][x7] -> A A .
S27: S[R0:2][x7] -> . A A[]
 empty => S1
 <A> => S28
S28: S[R0:2][x7] -> A . A[]
at_nulling
 <A[]> => S29
S29: S[R0:2][x7] -> A A[] .
S30: S[R0:2][x7] -> . A[] A
at_nulling
 <A[]> => S31
S31: S[R0:2][x7] -> A[] . A
 empty => S1
 <A> => S32
S32: S[R0:2][x7] -> A[] A .
S33: S['] -> . S
 empty => S3 S6 S11
 <S> => S34
S34: S['] -> S .
S35: S['][] -> .
EOS

Marpa::Test::is( $grammar->show_QDFA, <<'EOS', 'Aycock/Horspool QDFA' );
Start States: S0; S1
S0: 33,35
S['] -> . S
S['][] -> .
 <S> => S2
S1: predict; 1,3,6,12,14,17,22,24,27,31
A -> . a
S -> . A S[R0:1][x6]
S -> . A A[] A[] A[]
S -> A[] . S[R0:1][x6]
S[R0:1][x6] -> . A S[R0:2][x7]
S[R0:1][x6] -> . A A[] A[]
S[R0:1][x6] -> A[] . S[R0:2][x7]
S[R0:2][x7] -> . A A
S[R0:2][x7] -> . A A[]
S[R0:2][x7] -> A[] . A
 <A> => S3; S4
 <S[R0:1][x6]> => S5
 <S[R0:2][x7]> => S6
 <a> => S7
S2: 34
S['] -> S .
S3: 4,10,15,20,25,29,32
S -> A . S[R0:1][x6]
S -> A A[] A[] A[] .
S[R0:1][x6] -> A . S[R0:2][x7]
S[R0:1][x6] -> A A[] A[] .
S[R0:2][x7] -> A . A
S[R0:2][x7] -> A A[] .
S[R0:2][x7] -> A[] A .
 <A> => S8
 <S[R0:1][x6]> => S9
 <S[R0:2][x7]> => S10
S4: predict; 1,14,17,22,24,27,31
A -> . a
S[R0:1][x6] -> . A S[R0:2][x7]
S[R0:1][x6] -> . A A[] A[]
S[R0:1][x6] -> A[] . S[R0:2][x7]
S[R0:2][x7] -> . A A
S[R0:2][x7] -> . A A[]
S[R0:2][x7] -> A[] . A
 <A> => S11; S12
 <S[R0:2][x7]> => S6
 <a> => S7
S5: 13
S -> A[] S[R0:1][x6] .
S6: 23
S[R0:1][x6] -> A[] S[R0:2][x7] .
S7: 2
A -> a .
S8: 26
S[R0:2][x7] -> A A .
S9: 5
S -> A S[R0:1][x6] .
S10: 16
S[R0:1][x6] -> A S[R0:2][x7] .
S11: 15,20,25,29,32
S[R0:1][x6] -> A . S[R0:2][x7]
S[R0:1][x6] -> A A[] A[] .
S[R0:2][x7] -> A . A
S[R0:2][x7] -> A A[] .
S[R0:2][x7] -> A[] A .
 <A> => S8
 <S[R0:2][x7]> => S10
S12: predict; 1,24,27,31
A -> . a
S[R0:2][x7] -> . A A
S[R0:2][x7] -> . A A[]
S[R0:2][x7] -> A[] . A
 <A> => S13; S14
 <a> => S7
S13: 25,29,32
S[R0:2][x7] -> A . A
S[R0:2][x7] -> A A[] .
S[R0:2][x7] -> A[] A .
 <A> => S8
S14: predict; 1
A -> . a
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
S7@0-1 [p=S1@0-0; s=a; t=\'a']
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
S7@1-2 [p=S4@1-1; s=a; t=\'a']
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
S7@2-3 [p=S12@2-2; s=a; t=\'a']
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
S7@3-4 [p=S14@3-3; s=a; t=\'a']
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

my @expected = ( q{}, qw[(a;;;) (a;a;;) (a;a;a;) (a;a;a;a)] );

for my $i ( 0 .. 4 ) {
    my $evaler = Marpa::Evaluator->new(
        {   recce => $recce,
            end   => $i,
            clone => 0,
        }
    );
    my $result = $evaler->value();
    Test::More::is( ${$result}, $expected[$i], "parse permutation $i" );

} ## end for my $i ( 0 .. 4 )

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
