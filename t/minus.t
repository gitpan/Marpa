#!perl

use 5.010;

# An ambiguous equation,
# this time using the lexer

use strict;
use warnings;

use Test::More tests => 11;

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

my $g = Marpa::Grammar->new(
    {   start => 'E',
        strip => 0,

        # Set max_parses to 20 in case there's an infinite loop.
        # This is for debugging, after all
        max_parses => 20,

        rules => [
            [   'E', [qw/E Minus E/],
                <<'EOCODE'
    my ($right_string, $right_value)
        = ($_[2] =~ /^(.*)==(.*)$/);
    my ($left_string, $left_value)
        = ($_[0] =~ /^(.*)==(.*)$/);
    my $value = $left_value - $right_value;
    "(" . $left_string . "-" . $right_string . ")==" . $value;
EOCODE
            ],
            [   'E', [qw/E MinusMinus/],
                <<'EOCODE'
    my ($string, $value)
        = ($_[0] =~ /^(.*)==(.*)$/);
    "(" . $string . "--" . ")==" . $value--;
EOCODE
            ],
            [   'E', [qw/MinusMinus E/],
                <<'EOCODE'
    my ($string, $value)
        = ($_[1] =~ /^(.*)==(.*)$/);
    "(" . "--" . $string . ")==" . --$value;
EOCODE
            ],
            [   'E', [qw/Minus E/],
                <<'EOCODE'
    my ($string, $value)
        = ($_[1] =~ /^(.*)==(.*)$/);
    "(" . "-" . $string . ")==" . -$value;
EOCODE
            ],
            [   'E', [qw/Number/],
                <<'EOCODE'
    my $value = $_[0];
    "$value==$value";
EOCODE
            ],
        ],
        terminals => [
            [ 'Number'     => { regex => qr/\d+/xms } ],
            [ 'Minus'      => { regex => qr/[-]/xms } ],
            [ 'MinusMinus' => { regex => qr/[-][-]/xms } ],
        ],
        default_action => <<'EOCODE',
     my $v_count = scalar @_;
     return "" if $v_count <= 0;
     return $_[0] if $v_count == 1;
     "(" . join(";", @_) . ")";
EOCODE
    }
);

my $recce = Marpa::Recognizer->new( { grammar => $g, } );

Marpa::Test::is( $g->show_rules, <<'END_RULES', 'Minuses Equation Rules' );
0: E -> E Minus E
1: E -> E MinusMinus
2: E -> MinusMinus E
3: E -> Minus E
4: E -> Number
5: E['] -> E
END_RULES

Marpa::Test::is( $g->show_QDFA, <<'END_QDFA', 'Minuses Equation QDFA' );
Start States: S0; S1
S0: 16
E['] ::= . E
 <E> => S2
S1: predict; 1,5,8,11,14
E ::= . E Minus E
E ::= . E MinusMinus
E ::= . MinusMinus E
E ::= . Minus E
E ::= . Number
 <E> => S3
 <Minus> => S1; S4
 <MinusMinus> => S1; S5
 <Number> => S6
S2: 17
E['] ::= E .
S3: 2,6
E ::= E . Minus E
E ::= E . MinusMinus
 <Minus> => S1; S7
 <MinusMinus> => S8
S4: 12
E ::= Minus . E
 <E> => S9
S5: 9
E ::= MinusMinus . E
 <E> => S10
S6: 15
E ::= Number .
S7: 3
E ::= E Minus . E
 <E> => S11
S8: 7
E ::= E MinusMinus .
S9: 13
E ::= Minus E .
S10: 10
E ::= MinusMinus E .
S11: 4
E ::= E Minus E .
END_QDFA

my @expected = (
    #<<< no perltidy
    '(((6--)--)-1)==5',
    '((6--)-(--1))==6',
    '(6-(--(--1)))==7',
    '(6-(--(-(-1))))==6',
    '((6--)-(-(-1)))==5',
    '(6-(-(-(--1))))==6',
    '(6-(-(-(-(-1)))))==5',
    '(6-(-(--(-1))))==4',
    #>>>
);

# test multiple text calls
for my $string_piece ( '6', '-----', '1' ) {
    my $fail_offset = $recce->text($string_piece);
    if ( $fail_offset >= 0 ) {
        Marpa::exception("Parse failed at offset $fail_offset");
    }
} ## end for my $string_piece ( '6', '-----', '1' )

$recce->end_input();

my $evaler = Marpa::Evaluator->new( { recce => $recce, clone => 0 } );
Marpa::exception('Could not initialize parse') if not $evaler;

my $i = -1;
while ( defined( my $value = $evaler->value() ) ) {
    $i++;
    if ( $i > $#expected ) {
        Test::More::fail(
            'Minuses equation has extra value: ' . ${$value} . "\n" );
    }
    else {
        Marpa::Test::is( ${$value}, $expected[$i],
            "Minuses Equation Value $i" );
    }
} ## end while ( defined( my $value = $evaler->value() ) )

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
