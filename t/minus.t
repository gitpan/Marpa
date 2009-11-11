#!perl

use 5.010;

# An ambiguous equation,
# this time using the lexer

use strict;
use warnings;

use Test::More tests => 13;

use lib 'lib';
use t::lib::Marpa::Test;

BEGIN {
    Test::More::use_ok('Marpa');
    Test::More::use_ok('Marpa::MDLex');
}

# The inefficiency (at least some of it) is deliberate.
# Passing up a duples of [ string, value ] and then
# assembling a final string at the top would be better
# than assembling the string then taking it
# apart at each step.  But I wanted to test having
# a start symbol that appears repeatedly on the RHS.

## no critic (Subroutines::RequireArgUnpacking)

sub subtraction {
    shift;
    my ( $right_string, $right_value ) = ( $_[2] =~ /^(.*)==(.*)$/xms );
    my ( $left_string,  $left_value )  = ( $_[0] =~ /^(.*)==(.*)$/xms );
    my $value = $left_value - $right_value;
    return '(' . $left_string . q{-} . $right_string . ')==' . $value;
} ## end sub subtraction

sub postfix_decr {
    shift;
    my ( $string, $value ) = ( $_[0] =~ /^(.*)==(.*)$/xms );
    return '(' . $string . q{--} . ')==' . $value--;
}

sub prefix_decr {
    shift;
    my ( $string, $value ) = ( $_[1] =~ /^(.*)==(.*)$/xms );
    return '(' . q{--} . $string . ')==' . --$value;
}

sub negation {
    shift;
    my ( $string, $value ) = ( $_[1] =~ /^(.*)==(.*)$/xms );
    return '(' . q{-} . $string . ')==' . -$value;
}

sub number {
    shift;
    my $value = $_[0];
    return "$value==$value";
}

sub default_action {
    shift;
    given ( scalar @_ ) {
        when ( $_ <= 0 ) { return q{} }
        when (1)         { return $_[0] }
    };
    return '(' . join( q{;}, @_ ) . ')';
} ## end sub default_action

## use critic

my $grammar = Marpa::Grammar->new(
    {   start => 'E',
        strip => 0,

        # Set max_parses to 20 in case there's an infinite loop.
        # This is for debugging, after all
        max_parses => 20,

        actions => 'main',
        rules   => [
            {   lhs      => 'E',
                rhs      => [qw/E Minus E/],
                priority => 50,
                action   => 'subtraction',
            },
            {   lhs      => 'E',
                rhs      => [qw/E MinusMinus/],
                priority => 40,
                action   => 'postfix_decr',
            },
            {   lhs      => 'E',
                rhs      => [qw/MinusMinus E/],
                priority => 30,
                action   => 'prefix_decr',
            },
            {   lhs      => 'E',
                rhs      => [qw/Minus E/],
                priority => 20,
                action   => 'negation'
            },
            {   lhs    => 'E',
                rhs    => [qw/Number/],
                action => 'number'
            },
        ],
        terminals      => [qw( Number Minus MinusMinus )],
        default_action => 'default_action',
        parse_order    => 'original',
    }
);
$grammar->precompute();

my $recce = Marpa::Recognizer->new( { grammar => $grammar } );

my $lexer = Marpa::MDLex->new(
    {   recognizer => $recce,
        terminals  => [
            [ 'Number',     '\d+' ],
            [ 'Minus',      '[-]' ],
            [ 'MinusMinus', '[-][-]' ],
        ]
    }
);

Marpa::Test::is( $grammar->show_rules,
    <<'END_RULES', 'Minuses Equation Rules' );
0: E -> E Minus E /* priority=50 */
1: E -> E MinusMinus /* priority=40 */
2: E -> MinusMinus E /* priority=30 */
3: E -> Minus E /* priority=20 */
4: E -> Number
5: E['] -> E /* vlhs real=1 */
END_RULES

Marpa::Test::is( $grammar->show_QDFA, <<'END_QDFA', 'Minuses Equation QDFA' );
Start States: S0; S1
S0: 16
E['] -> . E
 <E> => S2
S1: predict; 1,5,8,11,14
E -> . E Minus E
E -> . E MinusMinus
E -> . MinusMinus E
E -> . Minus E
E -> . Number
 <E> => S3
 <Minus> => S1; S4
 <MinusMinus> => S1; S5
 <Number> => S6
S2: 17
E['] -> E .
S3: 2,6
E -> E . Minus E
E -> E . MinusMinus
 <Minus> => S1; S7
 <MinusMinus> => S8
S4: 12
E -> Minus . E
 <E> => S9
S5: 9
E -> MinusMinus . E
 <E> => S10
S6: 15
E -> Number .
S7: 3
E -> E Minus . E
 <E> => S11
S8: 7
E -> E MinusMinus .
S9: 13
E -> Minus E .
S10: 10
E -> MinusMinus E .
S11: 4
E -> E Minus E .
END_QDFA

my @expected_values = (
    #<<< no perltidy
    '(((6--)--)-1)==5',
    '((6--)-(--1))==6',
    '((6--)-(-(-1)))==5',
    '(6-(--(--1)))==7',
    '(6-(--(-(-1))))==6',
    '(6-(-(--(-1))))==4',
    '(6-(-(-(--1))))==6',
    '(6-(-(-(-(-1)))))==5',
    #>>>
);

# test multiple text calls
for my $string_piece ( '6', '-----', '1' ) {
    my $fail_offset = $lexer->text($string_piece);
    if ( $fail_offset >= 0 ) {
        Marpa::exception("Parse failed at offset $fail_offset");
    }
} ## end for my $string_piece ( '6', '-----', '1' )

$recce->tokens();

my $evaler = Marpa::Evaluator->new( { recce => $recce, clone => 0 } );
Marpa::exception('Could not initialize parse') if not $evaler;

for my $i ( 0 .. $#expected_values ) {
    my $value_ref = $evaler->value();
    my $test_name = "Minus Equation Value $i";
    if ( not defined $value_ref ) {
        Test::More::fail("No value for $test_name");
    }
    else {
        Marpa::Test::is( ${$value_ref}, $expected_values[$i], $test_name );
    }
} ## end for my $i ( 0 .. $#expected_values )

my @extra_values = ();
while ( my $value = $evaler->value() ) {
    push @extra_values, ${$value};
}

my $extra_values_count = scalar @extra_values;
for my $extra_value (@extra_values) {
    Test::More::diag("Extra Value: $extra_value");
}
Marpa::Test::is( $extra_values_count, 0, 'Minuses Equation Value Count' );

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
