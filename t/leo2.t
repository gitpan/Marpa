#!perl
# The example from p. 166 of Leo's paper.

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
    {   start          => 'S',
        strip          => 0,
        rules          => [ [ 'S', [qw/a S/] ], [ 'S', [], ], ],
        terminals      => [qw(a)],
        default_action => 'main::default_action',
    }
);

$grammar->precompute();

Marpa::Test::is( $grammar->show_symbols(),
    <<'END_OF_STRING', 'Leo166 Symbols' );
0: a, lhs=[] rhs=[0 2 3] terminal
1: S, lhs=[0 1 2 3] rhs=[0 2 4]
2: S[], lhs=[] rhs=[3] nullable=1 nulling
3: S['], lhs=[4] rhs=[]
4: S['][], lhs=[5] rhs=[] nullable=1 nulling
END_OF_STRING

Marpa::Test::is( $grammar->show_rules, <<'END_OF_STRING', 'Leo166 Rules' );
0: S -> a S /* !used */
1: S -> /* empty !used nullable */
2: S -> a S
3: S -> a S[]
4: S['] -> S /* vlhs real=1 */
5: S['][] -> /* empty nullable vlhs real=1 */
END_OF_STRING

Marpa::Test::is( $grammar->show_AHFA, <<'END_OF_STRING', 'Leo166 AHFA' );
Start States: S0; S1
S0: 7,9
S['] -> . S
S['][] -> .
 <S> => S2; leo(S['])
S1: predict; 1,4
S -> . a S
S -> . a S[]
 <a> => S1; S3
S2: leo-c; 8
S['] -> S .
S3: 2,6
S -> a . S
S -> a S[] .
 <S> => S4; leo(S)
S4: leo-c; 3
S -> a S .
END_OF_STRING

my $a_token = [ 'a', 'a' ];
my $length = 50;

LEO_FLAG: for my $leo_flag ( 0, 1 ) {
    my $recce = Marpa::Recognizer->new(
        { grammar => $grammar, mode => 'stream', leo => $leo_flag } );

    my $i        = 0;
    my $max_size = $recce->earley_set_size();
    TOKEN: while ( $i++ < $length ) {
        $recce->tokens( [$a_token] );
        my $size = $recce->earley_set_size();
        $max_size = $size > $max_size ? $size : $max_size;
    }

    my $expected_size = $leo_flag ? 4 : $length + 2;
    Marpa::Test::is( $max_size, $expected_size, "Leo flag $leo_flag, size" );

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
