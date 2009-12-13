#!perl
#
use 5.010;
use strict;
use warnings;

# A test of priorities.
# Since it's a basic functionality,
# I bypass MDL.

use lib 'lib';

use Test::More tests => 6;
use Marpa::Test;

BEGIN {
    Test::More::use_ok('Marpa');
    Test::More::use_ok('Marpa::MDLex');
}

sub str100 { return 100 }
sub str200 { return 200 }
sub str300 { return 300 }
sub str400 { return 400 }

my $g = Marpa::Grammar->new(
    {   start => 'S',
        rules => [
            [ 'S', ['P300'], 'main::str300', 300 ],
            [ 'S', ['P200'], 'main::str200', 200 ],
            [ 'S', ['P400'], 'main::str400', 400 ],
            [ 'S', ['P100'], 'main::str100', 100 ],
        ],
        terminals => [qw(P100 P200 P300 P400)],
    }
);

$g->precompute();

my $recce = Marpa::Recognizer->new( { grammar => $g, } );

my $lexer = Marpa::MDLex->new(
    {   recce     => $recce,
        terminals => [
            [ 'P200', 'a' ],
            [ 'P400', 'a' ],
            [ 'P100', 'a' ],
            [ 'P300', 'a' ],
        ]
    }
);

my @expected = qw(400 300 200 100);

my $fail_offset = $lexer->text('a');
if ( $fail_offset >= 0 ) {
    Marpa::exception("Parse failed at offset $fail_offset");
}

$recce->end_input();

my $evaler = Marpa::Evaluator->new(
    {   recce => $recce,

        # Set max_parses to 20 in case there's an infinite loop.
        # This is for debugging, after all
        ## no critic (ValuesAndExpressions::ProhibitMagicNumbers)
        max_parses => 20,
    }
);
Marpa::exception('Could not initialize parse') if not $evaler;

my $i = -1;
while ( defined( my $value = $evaler->value() ) ) {
    $i++;
    if ( $i > $#expected ) {
        Test::More::fail( 'Parse has extra value: ' . ${$value} . "\n" );
    }
    else {
        Marpa::Test::is( ${$value}, $expected[$i], "Priority Value $i" );
    }
} ## end while ( defined( my $value = $evaler->value() ) )

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
