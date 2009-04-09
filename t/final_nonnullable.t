#!perl
# Catch the case of a final non-nulling symbol at the end of a rule
# which has more than 2 proper nullables
# This is to test an untested branch of the CHAF logic.

use 5.010;
use strict;
use warnings;

use Test::More tests => 7;

use lib 'lib';
use lib 't/lib';
use Marpa::Test;
use Carp;

BEGIN {
    use_ok('Marpa');
}

my $grammar = new Marpa::Grammar(
    {   start => 'S',
        strip => 0,

        # Set max at 10 just in case there's an infinite loop.
        # This is for debugging, after all
        max_parses => 10,

        rules => [
            [ 'S', [qw/p p p n/], ],
            [ 'p', ['a'], ],
            [ 'p', [], ],
            [ 'n', ['a'], ],
        ],
        terminals => ['a'],

        default_action => <<'EO_CODE',
     my $v_count = scalar @_;
     return q{} if $v_count <= 0;
     my @vals = map { $_ // '-' } @_;
     return $vals[0] if $v_count == 1;
     '(' . join(q{;}, @vals) . ')';
EO_CODE

    }
);

$grammar->precompute();

Marpa::Test::is( $grammar->show_rules,
    <<'END_OF_STRING', 'final nonnulling Rules' );
0: S -> p p p n /* !useful */
1: p -> a
2: p -> /* !useful empty nullable nulling */
3: n -> a
4: S -> p p S[R0:2][x5] /* priority=0.3 */
5: S -> p[] p S[R0:2][x5] /* priority=0.1 */
6: S -> p p[] S[R0:2][x5] /* priority=0.2 */
7: S -> p[] p[] S[R0:2][x5]
8: S[R0:2][x5] -> p n /* priority=0.3 */
9: S[R0:2][x5] -> p[] n /* priority=0.1 */
10: S['] -> S
END_OF_STRING

Marpa::Test::is( $grammar->show_ii_QDFA,
    <<'END_OF_STRING', 'final nonnulling QDFA' );
Start States: St1; St9
St0: predict; 1,3,21,25
p ::= . a
n ::= . a
S[R0:2][x5] ::= . p n
S[R0:2][x5] ::= p[] . n
 <a> => St4
 <n> => St8
 <p> => St11; St6
St1: predict; 1,3,5,10,13,19,21,25
p ::= . a
n ::= . a
S ::= . p p S[R0:2][x5]
S ::= p[] . p S[R0:2][x5]
S ::= . p p[] S[R0:2][x5]
S ::= p[] p[] . S[R0:2][x5]
S[R0:2][x5] ::= . p n
S[R0:2][x5] ::= p[] . n
 <S[R0:2][x5]> => St5
 <a> => St4
 <n> => St8
 <p> => St0; St13
St2: pri=0.1; 12
S ::= p[] p S[R0:2][x5] .
St3: pri=0.2; 16
S ::= p p[] S[R0:2][x5] .
St4: 2,4
p ::= a .
n ::= a .
St5: 20
S ::= p[] p[] S[R0:2][x5] .
St6: 22
S[R0:2][x5] ::= p . n
 <n> => St7
St7: pri=0.3; 23
S[R0:2][x5] ::= p n .
St8: pri=0.1; 26
S[R0:2][x5] ::= p[] n .
St9: 27
S['] ::= . S
 <S> => St10
St10: 28
S['] ::= S .
St11: predict; 3
n ::= . a
 <a> => St12
St12: 4
n ::= a .
St13: 6,11,15,22
S ::= p . p S[R0:2][x5]
S ::= p[] p . S[R0:2][x5]
S ::= p p[] . S[R0:2][x5]
S[R0:2][x5] ::= p . n
 <S[R0:2][x5]> => St2; St3
 <n> => St7
 <p> => St0; St14
St14: 7
S ::= p p . S[R0:2][x5]
 <S[R0:2][x5]> => St15
St15: pri=0.3; 8
S ::= p p S[R0:2][x5] .
END_OF_STRING

my $a = $grammar->get_symbol('a');

my @results = qw{NA (-;-;-;a) (a;-;-;a) (a;a;-;a) (a;a;a;a)};

for my $input_length ( 1 .. 4 ) {
    my $recce = new Marpa::Recognizer( { grammar => $grammar } );
    TOKEN: for my $token ( 1 .. $input_length ) {
        next TOKEN if $recce->earleme( [ $a, 'a', 1 ] );
        croak( 'Parsing exhausted at character: ', $token );
    }
    $recce->end_input();
    my $evaler = new Marpa::Evaluator( { recce => $recce, clone => 0 } );
    my $value = $evaler->old_value();
    Marpa::Test::is( ${$value}, $results[$input_length],
        "final nonnulling, input length=$input_length" );
} ## end for my $input_length ( 1 .. 4 )

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
