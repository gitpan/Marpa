#!perl

# the example grammar in Aycock/Horspool "Practical Earley Parsing",
# _The Computer Journal_, Vol. 45, No. 6, pp. 620-630,
# in source form

use 5.010;
use strict;
use warnings;
use lib 'lib';
use English qw( -no_match_vars );

use Test::More tests => 6;

BEGIN {
    Test::More::use_ok('Marpa::MDL');
}

my $source = do { local $RS = undef; <main::DATA> };
my ($marpa_options) = Marpa::MDL::to_raw($source);

my $grammar = Marpa::Grammar->new( { maximal => 1, }, @{$marpa_options} );

$grammar->precompute();

my $recce = Marpa::Recognizer->new( { grammar => $grammar } );

my $lc_a = Marpa::MDL::get_terminal( $grammar, 'lowercase a' );
$recce->earleme( [ $lc_a, 'lowercase a', 1 ] );
$recce->earleme( [ $lc_a, 'lowercase a', 1 ] );
$recce->earleme( [ $lc_a, 'lowercase a', 1 ] );
$recce->earleme( [ $lc_a, 'lowercase a', 1 ] );
$recce->end_input();

my @answer = (
    q{},
    '(lowercase a;;;)',
    '(lowercase a;lowercase a;;)',
    '(lowercase a;lowercase a;lowercase a;)',
    '(lowercase a;lowercase a;lowercase a;lowercase a)',
);

for my $i ( 0 .. 4 ) {
    my $evaler = Marpa::Evaluator->new(
        {   recce => $recce,
            end   => $i
        }
    );
    my $result = $evaler->value();
    Test::More::is( ${$result}, $answer[$i], "parse permutation $i" );
} ## end for my $i ( 0 .. 4 )

## no critic (Subroutines::RequireArgUnpacking)

sub default_action {
    shift;
    my $v_count = scalar @_;
    return q{}   if $v_count <= 0;
    return $_[0] if $v_count == 1;
    return '(' . join( q{;}, @_ ) . ')';
} ## end sub default_action

## use critic

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:

__DATA__
semantics are perl5.  version is 0.001_019.  the start symbol is
S.  the default null value is q{}.
the default action is 'main::default_action'.

S: A, A, A, A.

A: lowercase a.

lowercase a matches /a/.

E: . # empty

A: E.
