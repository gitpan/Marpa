#!perl

# the example grammar in Aycock/Horspool "Practical Earley Parsing",
# _The Computer Journal_, Vol. 45, No. 6, pp. 620-630,
# in source form

use 5.010;
use strict;
use warnings;
use lib 'lib';
use English qw( -no_match_vars );
use Carp;

use Test::More tests => 2;

BEGIN {
    use_ok('Marpa');
}

my $source;
{ local ($RS) = undef; $source = <DATA> };

my $grammar = new Marpa::Grammar(
    {   warnings   => 1,
        code_lines => -1,
    }
);

$grammar->set( { mdl_source => \$source } );

$grammar->precompute();

my $recce = new Marpa::Recognizer( { grammar => $grammar } );

my $lc_a = Marpa::MDL::get_symbol( $grammar, 'lowercase a' );
$recce->earleme( [ $lc_a, 'lowercase a', 1 ] );
$recce->earleme( [ $lc_a, 'lowercase a', 1 ] );
$recce->earleme( [ $lc_a, 'lowercase a', 1 ] );
$recce->earleme( [ $lc_a, 'lowercase a', 1 ] );
$recce->end_input();

my $failure_count = 0;
my $total_count   = 0;
my @answer        = (
    q{},
    '(lowercase a;;;)',
    '(lowercase a;lowercase a;;)',
    '(lowercase a;lowercase a;lowercase a;)',
    '(lowercase a;lowercase a;lowercase a;lowercase a)',
);

for my $i ( 0 .. 4 ) {
    my $evaler = new Marpa::Evaluator(
        {   recce => $recce,
            end   => $i
        }
    );
    my $result = $evaler->value();
    $total_count++;
    if ( $answer[$i] ne ${$result} ) {
        diag( 'got ' . ${$result} . ', expected ' . $answer[$i] . "\n" );
        $failure_count++;
    }
} ## end for my $i ( 0 .. 4 )

ok( !$failure_count,
    ( $total_count - $failure_count )
        . " of $total_count parse permutations succeeded" );

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:

__DATA__
semantics are perl5.  version is 0.001_004.  the start symbol is
S.  the default null value is q{}.  the default action is q{
     my $v_count = scalar @_;
     return q{} if $v_count <= 0;
     return $_[0] if $v_count == 1;
     '(' . join(';', @_) . ')';
}.

S: A, A, A, A.

A: lowercase a.

lowercase a matches /a/.

E: . # empty

A: E.
