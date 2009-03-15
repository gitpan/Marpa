#!perl
#
use 5.010;
use strict;
use warnings;

use Test::More tests => 9;

use lib 'lib';
use lib 't/lib';
use Carp;
use Marpa::Test;

BEGIN {
    use_ok('Marpa');
}

# A grammar from Hopcroft & Ullman,
# _Introduction to Automata Theory, Languages and Computation_,
# (Addison-Wesley, Reading, Massachusetts: 1979),
# pp. 248, 250.

my $g = new Marpa::Grammar(
    {   precompute => 0,
        start      => q{S'},
        strip      => 0,
        rules      => [
            [ q{S'}, [qw/S c/] ],
            [ 'S',   [qw/S A/] ],
            [ 'S',   [qw/A/] ],
            [ 'A',   [qw/a S b/] ],
            [ 'A',   [qw/a b/] ],
        ],
        academic => 1,
    }
);

$g->set(
    {   terminals => [
            [ 'a' => { regex => qr/a/xms } ],
            [ 'b' => { regex => qr/b/xms } ],
            [ 'c' => { regex => qr/c/xms } ],
        ],
    }
);

$g->precompute();

Marpa::Test::is( $g->show_rules(), <<'EOS', 'Hopcroft/Ullman Rules' );
0: S' -> S c
1: S -> S A
2: S -> A
3: A -> a S b
4: A -> a b
EOS

Marpa::Test::is( $g->show_symbols(), <<'EOS', 'Hopcroft/Ullman Symbols' );
0: S', lhs=[0] rhs=[]
1: S, lhs=[1 2] rhs=[0 1 3]
2: c, lhs=[] rhs=[0] terminal
3: A, lhs=[3 4] rhs=[1 2]
4: a, lhs=[] rhs=[3 4] terminal
5: b, lhs=[] rhs=[3 4] terminal
EOS

Marpa::Test::is( $g->show_nullable_symbols(),
    q{}, 'Hopcroft/Ullman Nullable Symbols' );
Marpa::Test::is( $g->show_nulling_symbols(),
    q{}, 'Hopcroft/Ullman Nulling Symbols' );
Marpa::Test::is(
    $g->show_productive_symbols(),
    'A S S\' a b c',
    'Hopcroft/Ullman Productive Symbols'
);
Marpa::Test::is(
    $g->show_accessible_symbols(),
    'A S S\' a b c',
    'Hopcroft/Ullman Accessible Symbols'
);
Marpa::Test::is( $g->show_NFA(), <<'EOS', 'Hopcroft/Ullman NFA' );
S0: /* empty */
 empty => S1
S1: S' ::= . S c
 empty => S4 S7
 <S> => S2
S2: S' ::= S . c
 <c> => S3
S3: S' ::= S c .
S4: S ::= . S A
 empty => S4 S7
 <S> => S5
S5: S ::= S . A
 empty => S9 S13
 <A> => S6
S6: S ::= S A .
S7: S ::= . A
 empty => S9 S13
 <A> => S8
S8: S ::= A .
S9: A ::= . a S b
 <a> => S10
S10: A ::= a . S b
 empty => S4 S7
 <S> => S11
S11: A ::= a S . b
 <b> => S12
S12: A ::= a S b .
S13: A ::= . a b
 <a> => S14
S14: A ::= a . b
 <b> => S15
S15: A ::= a b .
EOS

Marpa::Test::is( $g->show_ii_QDFA(), <<'EOS', 'Hopcroft/Ullman QDFA' );
Start States: St0; St7
St0: 1
S' ::= . S c
 <S> => St5
St1: 10,14
A ::= a . S b
A ::= a . b
 <S> => St2
 <b> => St4
St2: 11
A ::= a S . b
 <b> => St3
St3: 12
A ::= a S b .
St4: 15
A ::= a b .
St5: 2
S' ::= S . c
 <c> => St6
St6: 3
S' ::= S c .
St7: predict; 4,7,9,13
S ::= . S A
S ::= . A
A ::= . a S b
A ::= . a b
 <A> => St10
 <S> => St11; St8
 <a> => St1; St7
St8: 5
S ::= S . A
 <A> => St9
St9: 6
S ::= S A .
St10: 8
S ::= A .
St11: predict; 9,13
A ::= . a S b
A ::= . a b
 <a> => St1; St7
EOS

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
