#!perl
# An ambiguous equation

use 5.010;
use strict;
use warnings;
use lib 'lib';
use lib 't/lib';
use English qw( -no_match_vars );
use Fatal qw(open close chdir);

use Test::More tests => 6;
use Marpa::Test;

BEGIN {
    Test::More::use_ok('Marpa');
}

# The inefficiency (at least some of it) is deliberate.
# Passing up a duples of [ string, value ] and then
# assembling a final string at the top would be better
# than assembling the string then taking it
# apart at each step.  But I wanted to test having
# a start symbol that appears repeatedly on the RHS.

my $example_dir = 'example';
chdir $example_dir;

open my $grammar_fh, q{<}, 'equation.marpa';
my $source;
{ local ($RS) = undef; $source = <$grammar_fh> };
close $grammar_fh;

# Set max_parses to 10 in case there's an infinite loop.
# This is for debugging, after all

my $grammar =
    Marpa::Grammar->new( { max_parses => 10, mdl_source => \$source, } );

my $recce = Marpa::Recognizer->new( { grammar => $grammar } );

my $fail_offset = $recce->text('2-0*3+1');
if ( $fail_offset >= 0 ) {
    Marpa::exception("Parse failed at offset $fail_offset");
}

$recce->end_input();

my @expected = (
    '(((2-0)*3)+1)==7', '((2-(0*3))+1)==3',
    '((2-0)*(3+1))==8', '(2-((0*3)+1))==1',
    '(2-(0*(3+1)))==2',
);

my $evaler = Marpa::Evaluator->new( { recognizer => $recce } );
Marpa::exception('Parse failed') if not $evaler;

my $i = -1;
while ( defined( my $value = $evaler->value() ) ) {
    $i++;
    if ( $i > $#expected ) {
        Test::More::fail(
            'Ambiguous equation has extra value: ' . ${$value} . "\n" );
    }
    else {
        Marpa::Test::is( ${$value}, $expected[$i],
            "Ambiguous Equation Value $i" );
    }
} ## end while ( defined( my $value = $evaler->value() ) )

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
