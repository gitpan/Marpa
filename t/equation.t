#!perl
# An ambiguous equation

use 5.010;
use strict;
use warnings;

use Test::More tests => 8;

use lib 'lib';
use lib 't/lib';
use Marpa::Test;

BEGIN {
    Test::More::use_ok('Marpa');
}

# The inefficiency (at least some of it) is deliberate.
# Passing up a duples of [ string, value ] and then
# assembling a final string at the top would be better
# than assembling the string then taking it
# apart at each step.  But I wanted to test having
# a start symbol that appears repeatedly on the RHS.

my $grammar = Marpa::Grammar->new(
    {   start => 'E',
        strip => 0,

        # Set max at 10 just in case there's an infinite loop.
        # This is for debugging, after all
        max_parses => 10,

        rules => [
            [   'E', [qw/E Op E/],
                <<'EOCODE'
            my ($right_string, $right_value)
                = ($_[2] =~ /^(.*)==(.*)$/);
            my ($left_string, $left_value)
                = ($_[0] =~ /^(.*)==(.*)$/);
            my $op = $_[1];
            my $value;
            if ($op eq '+') {
               $value = $left_value + $right_value;
            } elsif ($op eq '*') {
               $value = $left_value * $right_value;
            } elsif ($op eq '-') {
               $value = $left_value - $right_value;
            } else {
               Marpa::exception("Unknown op: $op");
            }
            '(' . $left_string . $op . $right_string . ')==' . $value;
EOCODE
            ],
            [   'E', [qw/Number/],
                <<'EOCODE'
           my $v0 = pop @_;
           $v0 . q{==} . $v0;
EOCODE
            ],
        ],

        default_action => <<'EO_CODE',
     my $v_count = scalar @_;
     return q{} if $v_count <= 0;
     return $_[0] if $v_count == 1;
     '(' . join(q{;}, @_) . ')';
EO_CODE

    }
);

$grammar->precompute();

Marpa::Test::is( $grammar->show_rules,
    <<'END_RULES', 'Ambiguous Equation Rules' );
0: E -> E Op E
1: E -> Number
2: E['] -> E
END_RULES

Marpa::Test::is( $grammar->show_QDFA,
    <<'END_QDFA', 'Ambiguous Equation QDFA' );
Start States: S0; S1
S0: 7
E['] ::= . E
 <E> => S2
S1: predict; 1,5
E ::= . E Op E
E ::= . Number
 <E> => S3
 <Number> => S4
S2: 8
E['] ::= E .
S3: 2
E ::= E . Op E
 <Op> => S1; S5
S4: 6
E ::= Number .
S5: 3
E ::= E Op . E
 <E> => S6
S6: 4
E ::= E Op E .
END_QDFA

my $recce = Marpa::Recognizer->new( { grammar => $grammar } );

my $op     = $grammar->get_symbol('Op');
my $number = $grammar->get_symbol('Number');

my @tokens = (
    [ $number, 2,    1 ],
    [ $op,     q{-}, 1 ],
    [ $number, 0,    1 ],
    [ $op,     q{*}, 1 ],
    [ $number, 3,    1 ],
    [ $op,     q{+}, 1 ],
    [ $number, 1,    1 ],
);

TOKEN: for my $token (@tokens) {
    next TOKEN if $recce->earleme($token);
    Marpa::exception( 'Parsing exhausted at character: ', $token->[1] );
}

$recce->end_input();

my @expected = (
    #<<< no perltidy
    '(((2-0)*3)+1)==7',
    '((2-(0*3))+1)==3',
    '((2-0)*(3+1))==8',
    '(2-((0*3)+1))==1',
    '(2-(0*(3+1)))==2',
    #>>>
);
my $evaler = Marpa::Evaluator->new( { recce => $recce, clone => 0 } );

my $i = -1;
while ( defined( my $value = $evaler->value() ) ) {
    $i++;
    if ( $i > $#expected ) {
        Test::More::fail(
            'Ambiguous equation has extra value: ' . ${$value} . "\n" );
    }
    else {
        Marpa::Test::is( ${$value}, $expected[$i],
            "Ambiguous Equation Value $i" );
    }
} ## end while ( defined( my $value = $evaler->value() ) )

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
