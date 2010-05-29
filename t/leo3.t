#!perl
# The example from p. 166 of Leo's paper,
# augmented to test Leo prediction items.
#

use 5.010;
use strict;
use warnings;

use Test::More tests => 8;

use lib 'lib';
use Marpa::Test;

BEGIN {
    Test::More::use_ok('Marpa');
}

## no critic (Subroutines::RequireArgUnpacking)

sub main::default_action {
    shift;
    return ( join q{}, grep {defined} @_ );
}

## use critic

my $grammar = Marpa::Grammar->new(
    {   start => 'S',
        strip => 0,
        rules => [
            [ 'S', [qw/a A/] ],
            [ 'A', [qw/B/] ],
            [ 'B', [qw/C/] ],
            [ 'C', [qw/S/] ],
            [ 'S', [], ],
        ],
        terminals      => [qw(a)],
        default_action => 'main::default_action',
    }
);

$grammar->precompute();

Marpa::Test::is( $grammar->show_symbols(),
    <<'END_OF_STRING', 'Leo166 Symbols' );
0: a, lhs=[] rhs=[0 5 6] terminal
1: S, lhs=[0 4 5 6] rhs=[3 9 10]
2: A, lhs=[1 7] rhs=[0 5]
3: B, lhs=[2 8] rhs=[1 7]
4: C, lhs=[3 9] rhs=[2 8]
5: S[], lhs=[] rhs=[] nullable=1 nulling
6: A[], lhs=[] rhs=[6] nullable=1 nulling
7: B[], lhs=[] rhs=[] nullable=1 nulling
8: C[], lhs=[] rhs=[] nullable=1 nulling
9: S['], lhs=[10] rhs=[]
10: S['][], lhs=[11] rhs=[] nullable=1 nulling
END_OF_STRING

Marpa::Test::is( $grammar->show_rules, <<'END_OF_STRING', 'Leo166 Rules' );
0: S -> a A /* !used */
1: A -> B /* !used nullable */
2: B -> C /* !used nullable */
3: C -> S /* !used nullable */
4: S -> /* empty !used nullable */
5: S -> a A
6: S -> a A[]
7: A -> B
8: B -> C
9: C -> S
10: S['] -> S /* vlhs real=1 */
11: S['][] -> /* empty nullable vlhs real=1 */
END_OF_STRING

Marpa::Test::is( $grammar->show_AHFA, <<'END_OF_STRING', 'Leo166 AHFA' );
Start States: S0; S1
S0: 13,15
S['] -> . S
S['][] -> .
 <S> => S2; leo(S['])
S1: predict; 1,4
S -> . a A
S -> . a A[]
 <a> => S3; S4
S2: leo-c; 14
S['] -> S .
S3: 2,6
S -> a . A
S -> a A[] .
 <A> => S5; leo(S)
S4: predict; 1,4,7,9,11
S -> . a A
S -> . a A[]
A -> . B
B -> . C
C -> . S
 <B> => S6; leo(A)
 <C> => S7; leo(B)
 <S> => S8; leo(C)
 <a> => S3; S4
S5: leo-c; 3
S -> a A .
S6: leo-c; 8
A -> B .
S7: leo-c; 10
B -> C .
S8: leo-c; 12
C -> S .
END_OF_STRING

my $a_token = [ 'a', 'a' ];
my $length = 20;

LEO_FLAG: for my $leo_flag ( 0, 1 ) {
    my $recce = Marpa::Recognizer->new(
        { grammar => $grammar, mode => 'stream', leo => $leo_flag } );

    my $i        = 0;
    my $max_size = $recce->earley_set_size();
    TOKEN: while ( $i++ < $length ) {
        $recce->tokens( [$a_token] );
        my $size = $recce->earley_set_size();

        $max_size = $size > $max_size ? $size : $max_size;
    } ## end while ( $i++ < $length )

    # Note that the length formula only works
    # beginning with Earley set c, for some small
    # constant c
    my $expected_size = $leo_flag ? 4 : ( $length - 1 ) * 4 + 3;
    Marpa::Test::is( $max_size, $expected_size,
        "Leo flag $leo_flag, size $max_size" );

    my $value_ref = $recce->value( {} );
    my $value = $value_ref ? ${$value_ref} : 'No parse';
    Marpa::Test::is( $value, 'a' x $length, 'Leo p166 parse' );

} ## end for my $leo_flag ( 0, 1 )

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
