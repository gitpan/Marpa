#!perl

use 5.010;

# variations on
# the example grammar in Aycock/Horspool "Practical Earley Parsing",
# _The Computer Journal_, Vol. 45, No. 6, pp. 620-630,

use strict;
use warnings;
use lib 'lib';

use Test::More tests => 8;
use Marpa::Test;

BEGIN {
    Test::More::use_ok('Marpa', 'alpha');
}

sub ah_extended {
    my $n = shift;

    my $g = Marpa::Grammar->new(
        {   start => 'S',

            rules => [
                [ 'S', [ ('A') x $n ] ],
                [ 'A', [qw/a/] ],
                [ 'A', [qw/E/] ],
                ['E'],
            ],
            terminals => ['a'],

            # no warnings for $n equals zero
            warnings => ( $n ? 1 : 0 ),
        }
    );
    $g->precompute();

    my $recce = Marpa::Recognizer->new( { grammar => $g } );

    $recce->tokens( [ ( [ 'a', 'a', 1 ] ) x ($n) ] );

    my @parse_counts;
    for my $loc ( 0 .. $n ) {
        my $parse_number = 0;
        my $evaler       = Marpa::Evaluator->new(
            {   recce       => $recce,
                end         => $loc,
                parse_order => 'none',

                # An arbitrary maximum is put on the number of parses -- this is for
                # debugging, and infinite loops happen.
                max_parses => 999,
            }
        );
        Marpa::exception("Cannot initialize parse at location $loc")
            if not $evaler;
        while ( $evaler->value() ) { $parse_counts[$loc]++ }
    } ## end for my $loc ( 0 .. $n )
    return join q{ }, @parse_counts;
} ## end sub ah_extended

my @answers = (
    '1',
    '1 1',
    '1 2 1',
    '1 3 3 1',
    '1 4 6 4 1',
    '1 5 10 10 5 1',
    '1 6 15 20 15 6 1',
    '1 7 21 35 35 21 7 1',
    '1 8 28 56 70 56 28 8 1',
    '1 9 36 84 126 126 84 36 9 1',
    '1 10 45 120 210 252 210 120 45 10 1',
);

## no critic (ValuesAndExpressions::ProhibitMagicNumbers)
for my $a ( ( 0 .. 5 ), 10 ) {
## use critic

    Marpa::Test::is( ah_extended($a), $answers[$a],
        "Row $a of Pascal's triangle matches parse counts" );

} ## end for my $a ( ( 0 .. 5 ), 10 )

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
