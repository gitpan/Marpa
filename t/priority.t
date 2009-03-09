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
use Carp;

use Test::More tests => 5;
use Marpa::Test;

BEGIN {
	use_ok( 'Marpa' );
}

# The inefficiency (at least some of it) is deliberate.
# Passing up a duples of [ string, value ] and then
# assembling a final string at the top would be better
# than assembling the string then taking it
# apart at each step.  But I wanted to test having
# a start symbol that appears repeatedly on the RHS.

my $g = new Marpa::Grammar({
    start => 'S',

    # Set max_parses to 20 in case there's an infinite loop.
    # This is for debugging, after all
    max_parses => 20,
    rules => [
	[ 'S', ['P300'], '300', 300 ],
	[ 'S', ['P200'], '200', 200 ],
	[ 'S', ['P400'], '400', 400 ],
	[ 'S', ['P100'], '100', 100 ],
    ],
    terminals => [
	[ 'P200' => { regex => qr/a/xms } ],
	[ 'P400' => { regex => qr/a/xms } ],
	[ 'P100' => { regex => qr/a/xms } ],
	[ 'P300' => { regex => qr/a/xms } ],
    ],
});

my $recce = new Marpa::Recognizer({
    grammar => $g,
});

my @expected = qw(400 300 200 100);

my $fail_offset = $recce->text(\('a'));
if ($fail_offset >= 0) {
   croak("Parse failed at offset $fail_offset");
}

$recce->end_input();

my $evaler = new Marpa::Evaluator( { recce => $recce } );
croak('Could not initialize parse') unless $evaler;

my $i = -1;
while ( defined(my $value = $evaler->value()) ) {
    $i++;
    if ($i > $#expected) {
       fail('Minuses equation has extra value: ' . ${$value} . "\n");
    } else {
       Marpa::Test::is(${$value}, $expected[$i], "Priority Value $i");
    }
}

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
