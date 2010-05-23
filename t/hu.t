#!perl
#
use 5.010;
use strict;
use warnings;

use Test::More tests => 9;

use lib 'lib';
use lib 't/lib';
use Marpa::Test;

BEGIN {
    Test::More::use_ok('Marpa');
}

# A grammar from Hopcroft & Ullman,
# _Introduction to Automata Theory, Languages and Computation_,
# (Addison-Wesley, Reading, Massachusetts: 1979),
# pp. 248, 250.

my $g = Marpa::Grammar->new(
    {   start => q{S'},
        strip => 0,
        rules => [
            [ q{S'}, [qw/S c/] ],
            [ 'S',   [qw/S A/] ],
            [ 'S',   [qw/A/] ],
            [ 'A',   [qw/a S b/] ],
            [ 'A',   [qw/a b/] ],
        ],
        academic => 1,
    }
);

$g->set( { terminals => [qw(a b c)] } );

$g->precompute();

Marpa::Test::is( $g->show_rules, <<'EOS', 'Hopcroft/Ullman Rules' );
0: S' -> S c
1: S -> S A
2: S -> A
3: A -> a S b
4: A -> a b
EOS

Marpa::Test::is( $g->show_symbols, <<'EOS', 'Hopcroft/Ullman Symbols' );
0: S', lhs=[0] rhs=[]
1: S, lhs=[1 2] rhs=[0 1 3]
2: c, lhs=[] rhs=[0] terminal
3: A, lhs=[3 4] rhs=[1 2]
4: a, lhs=[] rhs=[3 4] terminal
5: b, lhs=[] rhs=[3 4] terminal
EOS

Marpa::Test::is( $g->show_nullable_symbols, q{},
    'Hopcroft/Ullman Nullable Symbols' );
Marpa::Test::is( $g->show_nulling_symbols, q{},
    'Hopcroft/Ullman Nulling Symbols' );
Marpa::Test::is(
    $g->show_productive_symbols,
    'A S S\' a b c',
    'Hopcroft/Ullman Productive Symbols'
);
Marpa::Test::is(
    $g->show_accessible_symbols,
    'A S S\' a b c',
    'Hopcroft/Ullman Accessible Symbols'
);
Marpa::Test::is( $g->show_NFA, <<'EOS', 'Hopcroft/Ullman NFA' );
S0: /* empty */
 empty => S1
S1: S' -> . S c
 empty => S4 S7
 <S> => S2
S2: S' -> S . c
 <c> => S3
S3: S' -> S c .
S4: S -> . S A
 empty => S4 S7
 <S> => S5
S5: S -> S . A
 empty => S9 S13
 <A> => S6
S6: S -> S A .
S7: S -> . A
 empty => S9 S13
 <A> => S8
S8: S -> A .
S9: A -> . a S b
 <a> => S10
S10: A -> a . S b
 empty => S4 S7
 <S> => S11
S11: A -> a S . b
 <b> => S12
S12: A -> a S b .
S13: A -> . a b
 <a> => S14
S14: A -> a . b
 <b> => S15
S15: A -> a b .
EOS

Marpa::Test::is( $g->show_AHFA, <<'EOS', 'Hopcroft/Ullman AHFA' );
Start States: S0; S1
S0: 1
S' -> . S c
 <S> => S2
S1: predict; 4,7,9,13
S -> . S A
S -> . A
A -> . a S b
A -> . a b
 <A> => S3; leo(S)
 <S> => S4; S5
 <a> => S1; S6
S2: 2
S' -> S . c
 <c> => S7
S3: leo-c; 8
S -> A .
S4: 5
S -> S . A
 <A> => S8; leo(S)
S5: predict; 9,13
A -> . a S b
A -> . a b
 <a> => S1; S6
S6: 10,14
A -> a . S b
A -> a . b
 <S> => S9
 <b> => S10
S7: 3
S' -> S c .
S8: leo-c; 6
S -> S A .
S9: 11
A -> a S . b
 <b> => S11
S10: 15
A -> a b .
S11: 12
A -> a S b .
EOS

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
