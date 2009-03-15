#!perl

# the example grammar in Aycock/Horspool "Practical Earley Parsing",
# _The Computer Journal_, Vol. 45, No. 6, pp. 620-630,
# in its "NNF" form

use 5.010;
use strict;
use warnings;

use Test::More tests => 16;

use lib 'lib';
use lib 't/lib';
use Carp;
use Marpa::Test;

BEGIN {
    use_ok('Marpa');
}

my $grammar = new Marpa::Grammar(
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

Marpa::Test::is( $grammar->show_rules(), <<'EOS', 'Aycock/Horspool Rules' );
0: S -> A A A A /* !useful nullable */
1: A -> a
2: A -> E /* !useful nullable nulling */
3: E -> /* !useful empty nullable nulling */
4: S -> A S[R0:1][x6] /* priority=0.3 */
5: S -> A[] S[R0:1][x6] /* priority=0.1 */
6: S -> A S[R0:1][x6][] /* priority=0.2 */
7: S[R0:1][x6] -> A S[R0:2][x8] /* priority=0.3 */
8: S[R0:1][x6] -> A[] S[R0:2][x8] /* priority=0.1 */
9: S[R0:1][x6] -> A S[R0:2][x8][] /* priority=0.2 */
10: S[R0:2][x8] -> A A /* priority=0.3 */
11: S[R0:2][x8] -> A[] A /* priority=0.1 */
12: S[R0:2][x8] -> A A[] /* priority=0.2 */
13: S['] -> S
14: S['][] -> /* empty nullable nulling */
EOS

Marpa::Test::is( $grammar->show_symbols(),
    <<'EOS', 'Aycock/Horspool Symbols' );
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
    $grammar->show_nullable_symbols(),
    q{A[] E S['][] S[R0:1][x6][] S[R0:2][x8][] S[]},
    'Aycock/Horspool Nullable Symbols'
);
Marpa::Test::is(
    $grammar->show_nulling_symbols(),
    q{A[] E S['][] S[R0:1][x6][] S[R0:2][x8][] S[]},
    'Aycock/Horspool Nulling Symbols'
);
Marpa::Test::is(
    $grammar->show_productive_symbols(),
    q{A A[] E S S['] S['][] S[R0:1][x6] S[R0:1][x6][] S[R0:2][x8] S[R0:2][x8][] S[] a},
    'Aycock/Horspool Productive Symbols'
);
Marpa::Test::is(
    $grammar->show_accessible_symbols(),
    q{A A[] E S S['] S['][] S[R0:1][x6] S[R0:1][x6][] S[R0:2][x8] S[R0:2][x8][] S[] a},
    'Aycock/Horspool Accessible Symbols'
);

Marpa::Test::is( $grammar->show_NFA(), <<'EOS', 'Aycock/Horspool NFA' );
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
priority=0.3
S6: S ::= . A[] S[R0:1][x6]
at_nulling
 <A[]> => S7
S7: S ::= A[] . S[R0:1][x6]
 empty => S12 S15 S18
 <S[R0:1][x6]> => S8
S8: S ::= A[] S[R0:1][x6] .
priority=0.1
S9: S ::= . A S[R0:1][x6][]
 empty => S1
 <A> => S10
S10: S ::= A . S[R0:1][x6][]
at_nulling
 <S[R0:1][x6][]> => S11
S11: S ::= A S[R0:1][x6][] .
priority=0.2
S12: S[R0:1][x6] ::= . A S[R0:2][x8]
 empty => S1
 <A> => S13
S13: S[R0:1][x6] ::= A . S[R0:2][x8]
 empty => S21 S24 S27
 <S[R0:2][x8]> => S14
S14: S[R0:1][x6] ::= A S[R0:2][x8] .
priority=0.3
S15: S[R0:1][x6] ::= . A[] S[R0:2][x8]
at_nulling
 <A[]> => S16
S16: S[R0:1][x6] ::= A[] . S[R0:2][x8]
 empty => S21 S24 S27
 <S[R0:2][x8]> => S17
S17: S[R0:1][x6] ::= A[] S[R0:2][x8] .
priority=0.1
S18: S[R0:1][x6] ::= . A S[R0:2][x8][]
 empty => S1
 <A> => S19
S19: S[R0:1][x6] ::= A . S[R0:2][x8][]
at_nulling
 <S[R0:2][x8][]> => S20
S20: S[R0:1][x6] ::= A S[R0:2][x8][] .
priority=0.2
S21: S[R0:2][x8] ::= . A A
 empty => S1
 <A> => S22
S22: S[R0:2][x8] ::= A . A
 empty => S1
 <A> => S23
S23: S[R0:2][x8] ::= A A .
priority=0.3
S24: S[R0:2][x8] ::= . A[] A
at_nulling
 <A[]> => S25
S25: S[R0:2][x8] ::= A[] . A
 empty => S1
 <A> => S26
S26: S[R0:2][x8] ::= A[] A .
priority=0.1
S27: S[R0:2][x8] ::= . A A[]
 empty => S1
 <A> => S28
S28: S[R0:2][x8] ::= A . A[]
at_nulling
 <A[]> => S29
S29: S[R0:2][x8] ::= A A[] .
priority=0.2
S30: S['] ::= . S
 empty => S3 S6 S9
 <S> => S31
S31: S['] ::= S .
S32: S['][] ::= .
EOS

Marpa::Test::is( $grammar->show_ii_QDFA(), <<'EOS', 'Aycock/Horspool QDFA' );
Start States: St11; St3
St0: predict; 1
A ::= . a
 <a> => St7
St1: predict; 1,12,16,18,21,25,27
A ::= . a
S[R0:1][x6] ::= . A S[R0:2][x8]
S[R0:1][x6] ::= A[] . S[R0:2][x8]
S[R0:1][x6] ::= . A S[R0:2][x8][]
S[R0:2][x8] ::= . A A
S[R0:2][x8] ::= A[] . A
S[R0:2][x8] ::= . A A[]
 <A> => St10; St2; St4
 <S[R0:2][x8]> => St6
 <a> => St7
St2: predict; 1,21,25,27
A ::= . a
S[R0:2][x8] ::= . A A
S[R0:2][x8] ::= A[] . A
S[R0:2][x8] ::= . A A[]
 <A> => St0; St10; St8
 <a> => St7
St3: predict; 1,3,7,9,12,16,18,21,25,27
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
 <A> => St1; St10; St13
 <S[R0:1][x6]> => St15
 <S[R0:2][x8]> => St6
 <a> => St7
St4: pri=0.2; 13,20,22,29
S[R0:1][x6] ::= A . S[R0:2][x8]
S[R0:1][x6] ::= A S[R0:2][x8][] .
S[R0:2][x8] ::= A . A
S[R0:2][x8] ::= A A[] .
 <A> => St9
 <S[R0:2][x8]> => St5
St5: pri=0.3; 14
S[R0:1][x6] ::= A S[R0:2][x8] .
St6: pri=0.1; 17
S[R0:1][x6] ::= A[] S[R0:2][x8] .
St7: 2
A ::= a .
St8: pri=0.2; 22,29
S[R0:2][x8] ::= A . A
S[R0:2][x8] ::= A A[] .
 <A> => St9
St9: pri=0.3; 23
S[R0:2][x8] ::= A A .
St10: pri=0.1; 26
S[R0:2][x8] ::= A[] A .
St11: 30,32
S['] ::= . S
S['][] ::= .
 <S> => St12
St12: 31
S['] ::= S .
St13: pri=0.2; 4,11,13,20,22,29
S ::= A . S[R0:1][x6]
S ::= A S[R0:1][x6][] .
S[R0:1][x6] ::= A . S[R0:2][x8]
S[R0:1][x6] ::= A S[R0:2][x8][] .
S[R0:2][x8] ::= A . A
S[R0:2][x8] ::= A A[] .
 <A> => St9
 <S[R0:1][x6]> => St14
 <S[R0:2][x8]> => St5
St14: pri=0.3; 5
S ::= A S[R0:1][x6] .
St15: pri=0.1; 8
S ::= A[] S[R0:1][x6] .
EOS

my $recce = new Marpa::Recognizer( { grammar => $grammar, clone => 0 } );

my $set0_new = <<'EOS';
Earley Set 0
St11@0-0
St3@0-0
EOS

my $set1_at_0 = <<'EOS';
Earley Set 1
St7@0-1 [p=St3@0-0; t=a]
EOS

my $set1_at_1 = <<'EOS';
St10@0-1 [p=St3@0-0; c=St7@0-1]
St13@0-1 [p=St3@0-0; c=St7@0-1]
St1@1-1
St6@0-1 [p=St3@0-0; c=St13@0-1] [p=St3@0-0; c=St10@0-1]
St12@0-1 [p=St11@0-0; c=St13@0-1] [p=St11@0-0; c=St15@0-1]
St15@0-1 [p=St3@0-0; c=St13@0-1] [p=St3@0-0; c=St6@0-1]
EOS

my $set2_at_1 = <<'EOS';
Earley Set 2
St7@1-2 [p=St1@1-1; t=a]
EOS

my $set2_at_2 = <<'EOS';
St9@0-2 [p=St13@0-1; c=St7@1-2]
St10@1-2 [p=St1@1-1; c=St7@1-2]
St4@1-2 [p=St1@1-1; c=St7@1-2]
St2@2-2
St6@0-2 [p=St3@0-0; c=St9@0-2]
St5@0-2 [p=St13@0-1; c=St4@1-2] [p=St13@0-1; c=St10@1-2]
St6@1-2 [p=St1@1-1; c=St4@1-2] [p=St1@1-1; c=St10@1-2]
St14@0-2 [p=St13@0-1; c=St4@1-2] [p=St13@0-1; c=St6@1-2]
St15@0-2 [p=St3@0-0; c=St5@0-2] [p=St3@0-0; c=St6@0-2]
St12@0-2 [p=St11@0-0; c=St14@0-2] [p=St11@0-0; c=St15@0-2]
EOS

my $set3_at_2 = <<'EOS';
Earley Set 3
St7@2-3 [p=St2@2-2; t=a]
EOS

my $set3_at_3 = <<'EOS';
St9@1-3 [p=St4@1-2; c=St7@2-3]
St10@2-3 [p=St2@2-2; c=St7@2-3]
St8@2-3 [p=St2@2-2; c=St7@2-3]
St0@3-3
St5@0-3 [p=St13@0-1; c=St9@1-3]
St6@1-3 [p=St1@1-1; c=St9@1-3]
St5@1-3 [p=St4@1-2; c=St8@2-3] [p=St4@1-2; c=St10@2-3]
St15@0-3 [p=St3@0-0; c=St5@0-3]
St14@0-3 [p=St13@0-1; c=St5@1-3] [p=St13@0-1; c=St6@1-3]
St12@0-3 [p=St11@0-0; c=St14@0-3] [p=St11@0-0; c=St15@0-3]
EOS

my $set4_at_3 = <<'EOS';
Earley Set 4
St7@3-4 [p=St0@3-3; t=a]
EOS

my $set4_at_4 = <<'EOS';
St9@2-4 [p=St8@2-3; c=St7@3-4]
St5@1-4 [p=St4@1-2; c=St9@2-4]
St14@0-4 [p=St13@0-1; c=St5@1-4]
St12@0-4 [p=St11@0-0; c=St14@0-4]
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
$recce->earleme( [ $a, 'a', 1 ] ) or croak('Parsing exhausted');

Marpa::Test::is(
    $recce->show_earley_sets(1),
    "Current Earley Set: 1; Furthest: 1\n" . $sets_at_0,
    'Aycock/Horspool Parse Status at 0'
);

$recce->earleme( [ $a, 'a', 1 ] ) or croak('Parsing exhausted');

Marpa::Test::is(
    $recce->show_earley_sets(1),
    "Current Earley Set: 2; Furthest: 2\n" . $sets_at_1,
    'Aycock/Horspool Parse Status at 1'
);

$recce->earleme( [ $a, 'a', 1 ] ) or croak('Parsing exhausted');

Marpa::Test::is(
    $recce->show_earley_sets(1),
    "Current Earley Set: 3; Furthest: 3\n" . $sets_at_2,
    'Aycock/Horspool Parse Status at 2'
);

$recce->earleme( [ $a, 'a', 1 ] ) or croak('Parsing exhausted');

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

my $failure_count = 0;
my $total_count   = 0;
my @answer        = ( q{}, qw[(a;;;) (a;a;;) (a;a;a;) (a;a;a;a)] );

for my $i ( 0 .. 4 ) {
    my $evaler = new Marpa::Evaluator(
        {   recce => $recce,
            end   => $i,
            clone => 0,
        }
    );
    my $result = $evaler->value();
    $total_count++;
    if ( $answer[$i] ne ${$result} ) {
        diag( 'got ' . ${$result} . ', expected ' . $answer[$i] . "\n" );
        $failure_count++;
    }
} ## end for my $i ( 0 .. 4 )

ok( !$failure_count,
    ( $total_count - $failure_count )
        . " of $total_count parse permutations succeeded" );

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
