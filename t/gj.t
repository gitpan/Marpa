#!perl

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

# A grammar from Grune & Jacobs, Parsing Techniques: A Practical Guide, pp 206-207.
# The book is available on the web.

my $g = Marpa::Grammar->new(
    {   start => q{S'},
        strip => 0,
        rules => [
            [ q{S'}, [qw/S $/] ],
            [ 'S',   [qw/E/] ],
            [ 'E',   [qw/E - T/] ],
            [ 'E',   [qw/T/] ],
            [ 'T',   [qw/n/] ],
            [ 'T',   [qw/( E )/] ],
        ],
        academic => 1,
    }
);

$g->set( { terminals => [ 'n', q{$}, ')', '(', q{-}, ] } );

$g->precompute();

Marpa::Test::is( $g->show_rules, <<'EOS', 'Grune/Jacobs Rules' );
0: S' -> S $
1: S -> E
2: E -> E - T
3: E -> T
4: T -> n
5: T -> ( E )
EOS

Marpa::Test::is( $g->show_symbols, <<'EOS', 'Grune/Jacobs Symbols' );
0: S', lhs=[0] rhs=[]
1: S, lhs=[1] rhs=[0]
2: $, lhs=[] rhs=[0] terminal
3: E, lhs=[2 3] rhs=[1 2 5]
4: -, lhs=[] rhs=[2] terminal
5: T, lhs=[4 5] rhs=[2 3]
6: n, lhs=[] rhs=[4] terminal
7: (, lhs=[] rhs=[5] terminal
8: ), lhs=[] rhs=[5] terminal
EOS

Marpa::Test::is( $g->show_nullable_symbols, q{},
    'Grune/Jacobs Nullable Symbols' );
Marpa::Test::is( $g->show_nulling_symbols, q{},
    'Grune/Jacobs Nulling Symbols' );
Marpa::Test::is(
    $g->show_productive_symbols,
    '$ ( ) - E S S\' T n',
    'Grune/Jacobs Productive Symbols'
);
Marpa::Test::is(
    $g->show_accessible_symbols,
    '$ ( ) - E S S\' T n',
    'Grune/Jacobs Accessible Symbols'
);
Marpa::Test::is( $g->show_NFA, <<'EOS', 'Grune/Jacobs NFA' );
S0: /* empty */
 empty => S1
S1: S' -> . S $
 empty => S4
 <S> => S2
S2: S' -> S . $
 <$> => S3
S3: S' -> S $ .
S4: S -> . E
 empty => S6 S10
 <E> => S5
S5: S -> E .
S6: E -> . E - T
 empty => S6 S10
 <E> => S7
S7: E -> E . - T
 <-> => S8
S8: E -> E - . T
 empty => S12 S14
 <T> => S9
S9: E -> E - T .
S10: E -> . T
 empty => S12 S14
 <T> => S11
S11: E -> T .
S12: T -> . n
 <n> => S13
S13: T -> n .
S14: T -> . ( E )
 <(> => S15
S15: T -> ( . E )
 empty => S6 S10
 <E> => S16
S16: T -> ( E . )
 <)> => S17
S17: T -> ( E ) .
EOS

Marpa::Test::is( $g->show_QDFA, <<'EOS', 'Grune/Jacobs QDFA' );
Start States: S0; S1
S0: 1
S' -> . S $
 <S> => S2
S1: predict; 4,6,10,12,14
S -> . E
E -> . E - T
E -> . T
T -> . n
T -> . ( E )
 <(> => S3; S4
 <E> => S5
 <T> => S6
 <n> => S7
S2: 2
S' -> S . $
 <$> => S8
S3: 15
T -> ( . E )
 <E> => S9
S4: predict; 6,10,12,14
E -> . E - T
E -> . T
T -> . n
T -> . ( E )
 <(> => S3; S4
 <E> => S10
 <T> => S6
 <n> => S7
S5: 5,7
S -> E .
E -> E . - T
 <-> => S11; S12
S6: 11
E -> T .
S7: 13
T -> n .
S8: 3
S' -> S $ .
S9: 16
T -> ( E . )
 <)> => S13
S10: 7
E -> E . - T
 <-> => S11; S12
S11: 8
E -> E - . T
 <T> => S14
S12: predict; 12,14
T -> . n
T -> . ( E )
 <(> => S3; S4
 <n> => S7
S13: 17
T -> ( E ) .
S14: 9
E -> E - T .
EOS

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
