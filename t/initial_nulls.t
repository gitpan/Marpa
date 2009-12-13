#!perl
# Two rules which start with nullables, and cycle.

use 5.010;
use strict;
use warnings;

use Test::More tests => 25;

use lib 'lib';
use Marpa::Test;

BEGIN {
    Test::More::use_ok('Marpa');
}

## no critic (Subroutines::RequireArgUnpacking)

sub default_action {
    shift;
    my $v_count = scalar @_;
    return q{} if $v_count <= 0;
    my @vals = map { $_ // q{-} } @_;
    return $_[0] if scalar @vals == 1;
    return '(' . join( q{;}, @vals ) . ')';
} ## end sub default_action

## use critic

my $grammar = Marpa::Grammar->new(
    {   start   => 'S',
        strip   => 0,
        maximal => 1,

        rules => [
            [ 'S',  [qw/p p p n/], ],
            [ 'p',  ['t'], ],
            [ 'p',  [], ],
            [ 'n',  ['t'], ],
            [ 'n',  ['r2'], ],
            [ 'r2', [qw/a b c d e x/], ],
            [ 'a',  [] ],
            [ 'b',  [] ],
            [ 'c',  [] ],
            [ 'd',  [] ],
            [ 'e',  [] ],
            [ 'a',  ['t'] ],
            [ 'b',  ['t'] ],
            [ 'c',  ['t'] ],
            [ 'd',  ['t'] ],
            [ 'e',  ['t'] ],
            [ 'x',  ['t'], ],
        ],
        terminals      => ['t'],
        maximal        => 1,
        default_action => 'main::default_action',
    }
);

$grammar->precompute();

my @results;
$results[1][0] = '(-;-;-;(-;-;-;-;-;t))';
$results[1][1] = '(-;-;-;t)';
$results[2][0] = '(t;-;-;(-;-;-;-;-;t))';
$results[2][1] = '(t;-;-;t)';
$results[2][2] = '(-;t;-;(-;-;-;-;-;t))';
$results[3][0] = '(t;t;-;(-;-;-;-;-;t))';
$results[3][1] = '(t;t;-;t)';
$results[3][2] = '(t;-;t;(-;-;-;-;-;t))';
$results[4][0] = '(t;t;t;(-;-;-;-;-;t))';
$results[4][1] = '(t;t;t;t)';
$results[4][2] = '(t;t;-;(t;-;-;-;-;t))';
$results[5][0] = '(t;t;t;(t;-;-;-;-;t))';
$results[5][1] = '(t;t;t;(-;t;-;-;-;t))';
$results[5][2] = '(t;t;t;(-;-;t;-;-;t))';
$results[6][0] = '(t;t;t;(t;t;-;-;-;t))';
$results[6][1] = '(t;t;t;(t;-;t;-;-;t))';
$results[6][2] = '(t;t;t;(t;-;-;t;-;t))';
$results[7][0] = '(t;t;t;(t;t;t;-;-;t))';
$results[7][1] = '(t;t;t;(t;t;-;t;-;t))';
$results[7][2] = '(t;t;t;(t;t;-;-;t;t))';
$results[8][0] = '(t;t;t;(t;t;t;t;-;t))';
$results[8][1] = '(t;t;t;(t;t;t;-;t;t))';
$results[8][2] = '(t;t;t;(t;t;-;t;t;t))';
$results[9][0] = '(t;t;t;(t;t;t;t;t;t))';

for my $input_length ( 1 .. 9 ) {
    my $recce = Marpa::Recognizer->new( { grammar => $grammar } );
    $recce->tokens( [ ( [ 't', 't', 1 ] ) x $input_length ] );
    my $evaler = Marpa::Evaluator->new(
        { recce => $recce, parse_order => 'original', } );
    my $i = 0;
    while ( $i < 3 and my $value = $evaler->value() ) {
        my $expected = $results[$input_length][$i] // q{[unexpected result]};
        Marpa::Test::is( ${$value}, $expected,
            "cycle with initial nullables, input length=$input_length, value #$i"
        );
        $i++;
    } ## end while ( $i < 3 and my $value = $evaler->value() )
} ## end for my $input_length ( 1 .. 9 )

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
