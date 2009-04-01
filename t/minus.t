#!perl

use 5.010;

# An ambiguous equation,
# this time using the lexer

use strict;
use warnings;

use Test::More tests => 11;

use lib 'lib';
use lib 't/lib';
use Carp;
use Marpa::Test;

BEGIN {
    use_ok('Marpa');
}

# The inefficiency (at least some of it) is deliberate.
# Passing up a duples of [ string, value ] and then
# assembling a final string at the top would be better
# than assembling the string then taking it
# apart at each step.  But I wanted to test having
# a start symbol that appears repeatedly on the RHS.

my $g = new Marpa::Grammar(
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

my $recce = new Marpa::Recognizer( { grammar => $g, } );

Marpa::Test::is( $g->show_rules(), <<'END_RULES', 'Minuses Equation Rules' );
0: E -> E Minus E
1: E -> E MinusMinus
2: E -> MinusMinus E
3: E -> Minus E
4: E -> Number
5: E['] -> E
END_RULES

Marpa::Test::is( $g->show_ii_QDFA(), <<'END_QDFA', 'Minuses Equation QDFA' );
Start States: St0; St5
St0: predict; 1,5,8,11,14
E ::= . E Minus E
E ::= . E MinusMinus
E ::= . MinusMinus E
E ::= . Minus E
E ::= . Number
 <E> => St7
 <Minus> => St0; St2
 <MinusMinus> => St0; St11
 <Number> => St4
St1: 10
E ::= MinusMinus E .
St2: 12
E ::= Minus . E
 <E> => St3
St3: 13
E ::= Minus E .
St4: 15
E ::= Number .
St5: 16
E['] ::= . E
 <E> => St6
St6: 17
E['] ::= E .
St7: 2,6
E ::= E . Minus E
E ::= E . MinusMinus
 <Minus> => St0; St8
 <MinusMinus> => St10
St8: 3
E ::= E Minus . E
 <E> => St9
St9: 4
E ::= E Minus E .
St10: 7
E ::= E MinusMinus .
St11: 9
E ::= MinusMinus . E
 <E> => St1
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
        croak("Parse failed at offset $fail_offset");
    }
} ## end for my $string_piece ( '6', '-----', '1' )

$recce->end_input();

my $evaler = new Marpa::Evaluator( { recce => $recce, clone => 0 } );
croak('Could not initialize parse') unless $evaler;

my $i = -1;
while ( defined( my $value = $evaler->old_value() ) ) {
    $i++;
    if ( $i > $#expected ) {
        fail( 'Minuses equation has extra value: ' . ${$value} . "\n" );
    }
    else {
        Marpa::Test::is( ${$value}, $expected[$i],
            "Minuses Equation Value $i" );
    }
} ## end while ( defined( my $value = $evaler->old_value() ) )

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
