#!perl
# The example from p. 168-169 of Leo's paper.
#
# Make sure I have a CHAF example!
#

use 5.010;
use strict;
use warnings;

use Test::More tests => 18;

use lib 'lib';
use Marpa::Test;

BEGIN {
    Test::More::use_ok('Marpa');
}

## no critic (Subroutines::RequireArgUnpacking)

sub generate_action {
    my ($lhs) = @_;
    return sub {
        shift;
        my $v_count = scalar @_;
        return q{} if $v_count <= 0;
        my @vals = map { $_ // q{-} } @_;
        return $lhs . '(' . ( join q{;}, @vals ) . ')';
        }
} ## end sub generate_action

my $C_action       = generate_action('C');
my $S_action       = generate_action('S');
my $default_action = generate_action(q{?});

## use critic

my $grammar = Marpa::Grammar->new(
    {   start => 'S',
        strip => 0,
        rules => [
            [ 'S', [qw/a S/],   'S_action', ],
            [ 'S', [qw/C/],     'S_action', ],
            [ 'C', [qw(a C b)], 'C_action', ],
            [ 'C', [], ],
        ],
        terminals      => [qw(a b)],
        default_action => 'default_action',
    }
);

$grammar->precompute();

Marpa::Test::is( $grammar->show_symbols(),
    <<'END_OF_STRING', 'Leo168 Symbols' );
0: a, lhs=[] rhs=[0 2 4 5 7 8] terminal
1: b, lhs=[] rhs=[2 7 8] terminal
2: S, lhs=[0 1 4 5 6] rhs=[0 4 9]
3: C, lhs=[2 3 7 8] rhs=[1 2 6 7]
4: S[], lhs=[] rhs=[5] nullable=1 nulling
5: C[], lhs=[] rhs=[8] nullable=1 nulling
6: S['], lhs=[9] rhs=[]
7: S['][], lhs=[10] rhs=[] nullable=1 nulling
END_OF_STRING

Marpa::Test::is( $grammar->show_rules, <<'END_OF_STRING', 'Leo168 Rules' );
0: S -> a S /* !used */
1: S -> C /* !used nullable */
2: C -> a C b /* !used */
3: C -> /* empty !used nullable */
4: S -> a S
5: S -> a S[]
6: S -> C
7: C -> a C b
8: C -> a C[] b
9: S['] -> S /* vlhs real=1 */
10: S['][] -> /* empty nullable vlhs real=1 */
END_OF_STRING

Marpa::Test::is( $grammar->show_AHFA, <<'END_OF_STRING', 'Leo168 AHFA' );
Start States: S0; S1
S0: 17,19
S['] -> . S
S['][] -> .
 <S> => S2; leo(S['])
S1: predict; 1,4,7,9,13
S -> . a S
S -> . a S[]
S -> . C
C -> . a C b
C -> . a C[] b
 <C> => S3; leo(S)
 <a> => S1; S4
S2: leo-c; 18
S['] -> S .
S3: leo-c; 8
S -> C .
S4: 2,6,10,15
S -> a . S
S -> a S[] .
C -> a . C b
C -> a C[] . b
 <C> => S5
 <S> => S6; leo(S)
 <b> => S7
S5: 11
C -> a C . b
 <b> => S8
S6: leo-c; 3
S -> a S .
S7: 16
C -> a C[] b .
S8: 12
C -> a C b .
END_OF_STRING

my $a_token = [ 'a', 'a' ];
my $b_token = [ 'b', 'b' ];
my %expected = (
    'a'        => q{S(a;-)},
    'ab'       => q{S(C(a;-;b))},
    'aa'       => q{S(a;S(a;-))},
    'aab'      => q{S(a;S(C(a;-;b)))},
    'aabb'     => q{S(C(a;C(a;-;b);b))},
    'aaa'      => q{S(a;S(a;S(a;-)))},
    'aaab'     => q{S(a;S(a;S(C(a;-;b))))},
    'aaabb'    => q{S(a;S(C(a;C(a;-;b);b)))},
    'aaabbb'   => q{S(C(a;C(a;C(a;-;b);b);b))},
    'aaaa'     => q{S(a;S(a;S(a;S(a;-))))},
    'aaaab'    => q{S(a;S(a;S(a;S(C(a;-;b)))))},
    'aaaabb'   => q{S(a;S(a;S(C(a;C(a;-;b);b))))},
    'aaaabbb'  => q{S(a;S(C(a;C(a;C(a;-;b);b);b)))},
    'aaaabbbb' => q{S(C(a;C(a;C(a;C(a;-;b);b);b);b))},
);

for my $a_length ( 1 .. 4 ) {
    for my $b_length ( 0 .. $a_length ) {

        my $string = ( 'a' x $a_length ) . ( 'b' x $b_length );
        my $recce = Marpa::Recognizer->new( { grammar => $grammar } );
        $recce->tokens(
            [ ( ($a_token) x $a_length ), ( ($b_token) x $b_length ), ] );

        my $evaler = Marpa::Evaluator->new(
            {   recce    => $recce,
                closures => {
                    'C_action'       => $C_action,
                    'S_action'       => $S_action,
                    'default_action' => $default_action,
                }
            }
        );
        my $value_ref = $evaler->value();
        my $value = $value_ref ? ${$value_ref} : 'No parse';
        Marpa::Test::is( $value, $expected{$string}, "Parse of $string" );

    } ## end for my $b_length ( 0 .. $a_length )
} ## end for my $a_length ( 1 .. 4 )

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
