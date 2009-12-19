#!perl
# An ambiguous equation

use 5.010;
use strict;
use warnings;
use lib 'lib';
use English qw( -no_match_vars );
use Fatal qw(open close chdir);
use Carp;

use Test::More tests => 8;
use Marpa::Test;

BEGIN {
    Test::More::use_ok('Marpa', 'alpha');
    Test::More::use_ok('Marpa::MDL');
    Test::More::use_ok('Marpa::MDL::example::equation');
}

# The inefficiency (at least some of it) is deliberate.
# Passing up a duples of [ string, value ] and then
# assembling a final string at the top would be better
# than assembling the string then taking it
# apart at each step.  But I wanted to test having
# a start symbol that appears repeatedly on the RHS.

my $source;
{
    local $RS = undef;
    open my $fh, q{<}, 'lib/Marpa/MDL/example/equation.marpa';
    $source = <$fh>;
    close $fh;
}

my ( $marpa_options, $mdlex_options ) = Marpa::MDL::to_raw($source);

my $grammar = Marpa::Grammar->new(
    { action_object => 'Marpa::MDL::Example::Equation', },
    @{$marpa_options} );

Carp::croak('Failed to create grammar') if not defined $grammar;

$grammar->precompute();

my $recce = Marpa::Recognizer->new( { grammar => $grammar } );
my $lexer = Marpa::MDLex->new( { recce => $recce }, @{$mdlex_options} );

my $fail_offset = $lexer->text('2-0*3+1');
if ( $fail_offset >= 0 ) {
    Marpa::exception("Parse failed at offset $fail_offset");
}

$recce->end_input();

my %expected_value = (
    '(2-(0*(3+1)))==2' => 1,
    '(((2-0)*3)+1)==7' => 1,
    '((2-(0*3))+1)==3' => 1,
    '((2-0)*(3+1))==8' => 1,
    '(2-((0*3)+1))==1' => 1,
);

# Set max_parses to 10 in case there's an infinite loop.
# This is for debugging, after all
# Note: code below used in display
my $evaler =
    Marpa::Evaluator->new( { recognizer => $recce, max_parses => 10 } );
Marpa::exception('Parse failed') if not $evaler;

my $i = 0;
VALUE: while ( my $value_ref = $evaler->value() ) {
    my $value     = ${$value_ref};
    my $test_name = "Value $i (unspecified order)";
    $i++;
    if ( defined $expected_value{$value} ) {
        Test::More::pass($test_name);
        delete $expected_value{$value};
        next VALUE;
    }
    Test::More::fail($test_name);
} ## end while ( my $value_ref = $evaler->value() )

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
