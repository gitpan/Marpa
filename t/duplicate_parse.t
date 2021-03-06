#!/usr/bin/perl

# Test of deletion of duplicate parses.

use 5.010;
use strict;
use warnings;

use Test::More tests => 6;

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
    return $vals[0] if $v_count == 1;
    return '(' . join( q{;}, @vals ) . ')';
} ## end sub default_action

## use critic

my $grammar = Marpa::Grammar->new(
    {   start => 'S',
        strip => 0,

        rules => [
            [ 'S', [qw/p p p n/], ],
            [ 'p', ['a'], ],
            [ 'p', [], ],
            [ 'n', ['a'], ],
        ],
        terminals      => ['a'],
        default_action => 'main::default_action',

    }
);

$grammar->precompute();

Marpa::Test::is( $grammar->show_rules,
    <<'END_OF_STRING', 'final nonnulling Rules' );
0: S -> p p p n /* !used */
1: p -> a
2: p -> /* empty !used nullable */
3: n -> a
4: S -> p p S[R0:2] /* vrhs real=2 */
5: S -> p p[] S[R0:2] /* vrhs real=2 */
6: S -> p[] p S[R0:2] /* vrhs real=2 */
7: S -> p[] p[] S[R0:2] /* vrhs real=2 */
8: S[R0:2] -> p n /* vlhs real=2 */
9: S[R0:2] -> p[] n /* vlhs real=2 */
10: S['] -> S /* vlhs real=1 */
END_OF_STRING

Marpa::Test::is( $grammar->show_AHFA,
    <<'END_OF_STRING', 'final nonnulling AHFA' );
Start States: S0; S1
S0: 27
S['] -> . S
 <S> => S2; leo(S['])
S1: predict; 1,3,5,9,14,19,21,25
p -> . a
n -> . a
S -> . p p S[R0:2]
S -> . p p[] S[R0:2]
S -> p[] . p S[R0:2]
S -> p[] p[] . S[R0:2]
S[R0:2] -> . p n
S[R0:2] -> p[] . n
 <S[R0:2]> => S3; leo(S)
 <a> => S4
 <n> => S5; leo(S[R0:2])
 <p> => S6; S7
S2: leo-c; 28
S['] -> S .
S3: leo-c; 20
S -> p[] p[] S[R0:2] .
S4: 2,4
p -> a .
n -> a .
S5: leo-c; 26
S[R0:2] -> p[] n .
S6: 6,11,15,22
S -> p . p S[R0:2]
S -> p p[] . S[R0:2]
S -> p[] p . S[R0:2]
S[R0:2] -> p . n
 <S[R0:2]> => S8
 <n> => S9; leo(S[R0:2])
 <p> => S10; S7
S7: predict; 1,3,21,25
p -> . a
n -> . a
S[R0:2] -> . p n
S[R0:2] -> p[] . n
 <a> => S4
 <n> => S5; leo(S[R0:2])
 <p> => S11; S12
S8: 12,16
S -> p p[] S[R0:2] .
S -> p[] p S[R0:2] .
S9: leo-c; 23
S[R0:2] -> p n .
S10: 7
S -> p p . S[R0:2]
 <S[R0:2]> => S13; leo(S)
S11: 22
S[R0:2] -> p . n
 <n> => S9; leo(S[R0:2])
S12: predict; 3
n -> . a
 <a> => S14
S13: leo-c; 8
S -> p p S[R0:2] .
S14: 4
n -> a .
END_OF_STRING

use constant SPACE => 0x60;

my $input_length = 3;
my $recce = Marpa::Recognizer->new( { grammar => $grammar } );
$recce->tokens(
    [ map { [ 'a', chr( SPACE + $_ ), 1 ] } ( 1 .. $input_length ) ] );

# Set max at 10 just in case there's an infinite loop.
# This is for debugging, after all
$recce->set( { max_parses => 10 } );

my %expected = map { ( $_ => 1 ) } qw( (-;a;b;c) (a;-;b;c) (a;b;-;c) );

while ( my $value_ref = $recce->value() ) {
    my $value = $value_ref ? ${$value_ref} : 'No parse';
    if ( defined $expected{$value} ) {
        delete $expected{$value};
        Test::More::pass("Expected value: $value");
    }
    else {
        Test::More::fail("Unexpected value: $value");
    }
} ## end while ( my $value_ref = $recce->value() )

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
