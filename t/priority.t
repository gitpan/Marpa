#!perl
#
use 5.010;
use strict;
use warnings;

# A test of priorities.
# Since it's a basic functionality,
# I bypass MDL.

use lib 'lib';
use lib 't/lib';

use Test::More tests => 5;
use Marpa::Test;

BEGIN {
    Test::More::use_ok('Marpa');
}

my $g = Marpa::Grammar->new(
    {   start => 'S',

        # Set max_parses to 20 in case there's an infinite loop.
        # This is for debugging, after all
        ## no critic (ValuesAndExpressions::ProhibitMagicNumbers)
        max_parses => 20,
        rules      => [
            [ 'S', ['P300'], '300', 300 ],
            [ 'S', ['P200'], '200', 200 ],
            [ 'S', ['P400'], '400', 400 ],
            [ 'S', ['P100'], '100', 100 ],
        ],
        ## use critic
        terminals => [
            [ 'P200' => { regex => qr/a/xms } ],
            [ 'P400' => { regex => qr/a/xms } ],
            [ 'P100' => { regex => qr/a/xms } ],
            [ 'P300' => { regex => qr/a/xms } ],
        ],
    }
);

my $recce = Marpa::Recognizer->new( { grammar => $g, } );

my @expected = qw(400 300 200 100);

my $fail_offset = $recce->text( \('a') );
if ( $fail_offset >= 0 ) {
    Marpa::exception("Parse failed at offset $fail_offset");
}

$recce->end_input();

my $evaler = Marpa::Evaluator->new( { recce => $recce } );
Marpa::exception('Could not initialize parse') if not $evaler;

my $i = -1;
TODO: {
    local $TODO = 'priorities broken';
    while ( defined( my $value = $evaler->value() ) ) {
        $i++;
        if ( $i > $#expected ) {
            Test::More::fail(
                'Minuses equation has extra value: ' . ${$value} . "\n" );
        }
        else {
            Marpa::Test::is( ${$value}, $expected[$i], "Priority Value $i" );
        }
    } ## end while ( defined( my $value = $evaler->value() ) )
} ## end TODO:

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
