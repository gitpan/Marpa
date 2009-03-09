#!perl
# An ambiguous equation

use 5.010;
use strict;
use warnings;

use Test::More tests => 8;

use lib 'lib';
use lib 't/lib';
use Marpa::Test;
use Carp;

BEGIN {
	use_ok( 'Marpa' );
}

# The inefficiency (at least some of it) is deliberate.
# Passing up a duples of [ string, value ] and then
# assembling a final string at the top would be better
# than assembling the string then taking it
# apart at each step.  But I wanted to test having
# a start symbol that appears repeatedly on the RHS.

my $grammar = new Marpa::Grammar({
    start => 'E',
    strip => 0,

    # Set max at 10 just in case there's an infinite loop.
    # This is for debugging, after all
    max_parses => 10,

    rules => [
	[ 'E', [qw/E Op E/],
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
               croak("Unknown op: $op");
            }
            '(' . $left_string . $op . $right_string . ')==' . $value;
EOCODE
        ],
	[ 'E', [qw/Number/],
<<'EOCODE'
           my $v0 = pop @_;
           $v0 . q{==} . $v0;
EOCODE
        ],
    ],

    default_action =>
<<'EO_CODE',
     my $v_count = scalar @_;
     return q{} if $v_count <= 0;
     return $_[0] if $v_count == 1;
     '(' . join(q{;}, @_) . ')';
EO_CODE

});

$grammar->precompute();

Marpa::Test::is( $grammar->show_rules(), <<'END_RULES', 'Ambiguous Equation Rules' );
0: E -> E Op E
1: E -> Number
2: E['] -> E
END_RULES

Marpa::Test::is( $grammar->show_ii_QDFA(), <<'END_QDFA', 'Ambiguous Equation QDFA' );
Start States: St0; St5
St0: predict; 1,5
E ::= . E Op E
E ::= . Number
 <E> => St1
 <Number> => St4
St1: 2
E ::= E . Op E
 <Op> => St0; St2
St2: 3
E ::= E Op . E
 <E> => St3
St3: 4
E ::= E Op E .
St4: 6
E ::= Number .
St5: 7
E['] ::= . E
 <E> => St6
St6: 8
E['] ::= E .
END_QDFA

my $recce = new Marpa::Recognizer({grammar => $grammar});

my $op = $grammar->get_symbol('Op');
my $number = $grammar->get_symbol('Number');

my @tokens = (
    [$number, 2, 1],
    [$op, q{-}, 1],
    [$number, 0, 1],
    [$op, q{*}, 1],
    [$number, 3, 1],
    [$op, q{+}, 1],
    [$number, 1, 1],
);

TOKEN: for my $token (@tokens) {
    next TOKEN if $recce->earleme($token);
    croak('Parsing exhausted at character: ', $token->[1]);
}

$recce->end_input();

my @expected = (
    '(((2-0)*3)+1)==7',
    '((2-(0*3))+1)==3',
    '((2-0)*(3+1))==8',
    '(2-((0*3)+1))==1',
    '(2-(0*(3+1)))==2',
);
my $evaler = new Marpa::Evaluator( { recce => $recce, clone => 0 } );

my $i = -1;
while (defined(my $value = $evaler->value()))
{
    $i++;
    if ($i > $#expected) {
       fail('Ambiguous equation has extra value: ' . ${$value} . "\n");
    } else {
       Marpa::Test::is(${$value}, $expected[$i], "Ambiguous Equation Value $i");
    }
}

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
