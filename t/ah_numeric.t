#!perl

# the example grammar in Aycock/Horspool "Practical Earley Parsing",
# _The Computer Journal_, Vol. 45, No. 6, pp. 620-630,
# in its "NNF" form

use 5.010;
use strict;
use warnings;

use lib 'lib';
use Test::More tests => 11;
use Marpa::Test;

BEGIN {
    Test::More::use_ok('Marpa');
}

## no critic (Subroutines::RequireArgUnpacking)

sub null_string { return q{} }

sub null_a {
    return ( $MyTest::MAXIMAL ? -1 : 1 )
        * 10**( 3 - Marpa::token_location() );
}

sub default_action {
    shift;
    my $v_count = scalar @_;
    return q{}   if $v_count <= 0;
    return $_[0] if $v_count == 1;
    return '(' . join( q{;}, @_ ) . ')';
} ## end sub default_action

## use critic

my $grammar = Marpa::Grammar->new(
    {   start => 'S',
        rules => [
            [ 'S', [qw/A A A A/] ],
            [ 'A', [qw/a/] ],
            [ 'A', [qw/E/] ],
            { lhs => 'A', rhs => [], ranking_action => 'main::null_a' },
            ['E'],
        ],
        default_null_action => 'main::null_string',
        default_action      => 'main::default_action',
    }
);

$grammar->set( { terminals => ['a'], } );

$grammar->precompute();

my $recce = Marpa::Recognizer->new( { grammar => $grammar, } );

my $input_length = 4;
$recce->tokens( [ ( [ 'a', 'a', 1 ] ) x $input_length ] );

my @maximal = ( q{}, qw[(a;;;) (a;a;;) (a;a;a;) (a;a;a;a)] );
my @minimal = ( q{}, qw[(;;;a) (;;a;a) (;a;a;a) (a;a;a;a)] );

for my $i ( 0 .. $input_length ) {
    for my $maximal ( 0, 1 ) {
        local $MyTest::MAXIMAL = $maximal;
        my $expected = $maximal ? \@maximal : \@minimal;
        my $name     = $maximal ? 'maximal' : 'minimal';
        my $evaler   = Marpa::Evaluator->new(
            {   recce => $recce,
                end   => $i,
            }
        );
        my $result = $evaler->value();
        Test::More::is( ${$result}, $expected->[$i],
            "$name parse permutation $i" );

    } ## end for my $maximal ( 0, 1 )
} ## end for my $i ( 0 .. $input_length )

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
