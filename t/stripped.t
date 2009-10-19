#!perl

use 5.010;
use strict;
use warnings;
use lib 'lib';
use lib 't/lib';
use Test::More tests => 8;
use Marpa::Test;

BEGIN {
    Test::More::use_ok('Marpa');
}

# The example grammar in Aycock/Horspool "Practical Earley Parsing",
# _The Computer Journal_, Vol. 45, No. 6, pp. 620-630
# This time testing the stripped output

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
    }
);

$g->set( { terminals => [ [ 'a' => { regex => 'a' } ], ], } );

$g->precompute();

Marpa::Test::is( $g->show_rules, <<'EOS', 'Aycock/Horspool Rules' );
0: S' -> S /* stripped */
1: S -> A A A A /* stripped */
2: A -> a /* stripped */
3: A -> E /* stripped */
4: E -> /* empty stripped */
EOS

Marpa::Test::is( $g->show_symbols, <<'EOS', 'Aycock/Horspool Symbols' );
0: S', stripped nullable=4
1: S, stripped nullable=4
2: A, stripped nullable=1
3: a, stripped terminal
4: E, stripped nullable=1 nulling
EOS

Marpa::Test::is( $g->show_nullable_symbols, 'stripped_',
    'Aycock/Horspool Nullable Symbols' );
Marpa::Test::is( $g->show_nulling_symbols, 'stripped_',
    'Aycock/Horspool Nulling Symbols' );
Marpa::Test::is( $g->show_productive_symbols, 'stripped_',
    'Aycock/Horspool Productive Symbols' );
Marpa::Test::is( $g->show_accessible_symbols, 'stripped_',
    'Aycock/Horspool Accessible Symbols' );

Marpa::Test::is( $g->show_NFA, <<'EOS', 'Aycock/Horspool NFA' );
stripped
EOS

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
