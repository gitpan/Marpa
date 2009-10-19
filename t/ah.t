#!perl
#
# The example grammar in Aycock/Horspool "Practical Earley Parsing",
# _The Computer Journal_, Vol. 45, No. 6, pp. 620-630

use 5.010;
use strict;
use warnings;
use lib 'lib';
use Test::More tests => 8;
use t::lib::Marpa::Test;

BEGIN {
    Test::More::use_ok('Marpa');
}

my $g = Marpa::Grammar->new(
    {   start => q{S'},
        rules => [
            [ q{S'}, [qw/S/] ],
            [ 'S',   [qw/A A A A/] ],
            [ 'A',   [qw/a/] ],
            [ 'A',   [qw/E/] ],
            ['E'],
        ],
        academic => 1,
        strip    => 0.
    }
);

$g->set( { terminals => [ [ 'a' => { regex => 'a' } ], ], } );

$g->precompute();

Marpa::Test::is( $g->show_rules, <<'EOS', 'Aycock/Horspool Rules' );
0: S' -> S /* nullable */
1: S -> A A A A /* nullable */
2: A -> a
3: A -> E /* nullable */
4: E -> /* empty nullable */
EOS

Marpa::Test::is( $g->show_symbols, <<'EOS', 'Aycock/Horspool Symbols' );
0: S', lhs=[0] rhs=[] nullable=4
1: S, lhs=[1] rhs=[0] nullable=4
2: A, lhs=[2 3] rhs=[1] nullable=1
3: a, lhs=[] rhs=[2] terminal
4: E, lhs=[4] rhs=[3] nullable=1 nulling
EOS

Marpa::Test::is( $g->show_nullable_symbols, q{A E S S'},
    'Aycock/Horspool Nullable Symbols' );
Marpa::Test::is( $g->show_nulling_symbols, 'E',
    'Aycock/Horspool Nulling Symbols' );
Marpa::Test::is( $g->show_productive_symbols, q{A E S S' a},
    'Aycock/Horspool Productive Symbols' );
Marpa::Test::is( $g->show_accessible_symbols, q{A E S S' a},
    'Aycock/Horspool Accessible Symbols' );

Marpa::Test::is( $g->show_NFA, <<'EOS', 'Aycock/Horspool NFA' );
S0: /* empty */
 empty => S1
S1: S' -> . S
 empty => S3
 <S> => S2
S2: S' -> S .
S3: S -> . A A A A
 empty => S8 S10
 <A> => S4
S4: S -> A . A A A
 empty => S8 S10
 <A> => S5
S5: S -> A A . A A
 empty => S8 S10
 <A> => S6
S6: S -> A A A . A
 empty => S8 S10
 <A> => S7
S7: S -> A A A A .
S8: A -> . a
 <a> => S9
S9: A -> a .
S10: A -> . E
at_nulling
 empty => S12
 <E> => S11
S11: A -> E .
S12: E -> .
EOS

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
