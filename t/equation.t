#!perl
# An ambiguous equation

use 5.010;
use strict;
use warnings;

use Test::More tests => 8;

use lib 'lib';
use t::lib::Marpa::Test;

BEGIN {
    Test::More::use_ok('Marpa');
}

## no critic (Subroutines::RequireArgUnpacking)

sub do_op {
    shift;
    my ( $right_string, $right_value ) = ( $_[2] =~ /^(.*)==(.*)$/xms );
    my ( $left_string,  $left_value )  = ( $_[0] =~ /^(.*)==(.*)$/xms );
    my $op = $_[1];
    my $value;
    if ( $op eq q{+} ) {
        $value = $left_value + $right_value;
    }
    elsif ( $op eq q{*} ) {
        $value = $left_value * $right_value;
    }
    elsif ( $op eq q{-} ) {
        $value = $left_value - $right_value;
    }
    else {
        Marpa::exception("Unknown op: $op");
    }
    return '(' . $left_string . $op . $right_string . ')==' . $value;
} ## end sub do_op

sub number {
    shift;
    my $v0 = pop @_;
    return $v0 . q{==} . $v0;
}

sub default_action {
    shift;
    my $v_count = scalar @_;
    return q{}   if $v_count <= 0;
    return $_[0] if $v_count == 1;
    return '(' . join( q{;}, @_ ) . ')';
} ## end sub default_action

## use critic

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
        actions    => 'main',
        rules      => [
            [ 'E', [qw/E Op E/], 'do_op' ],
            [ 'E', [qw/Number/], 'number' ],
        ],
        default_action => 'default_action',
    }
);

$grammar->precompute();

Marpa::Test::is( $grammar->show_rules,
    <<'END_RULES', 'Ambiguous Equation Rules' );
0: E -> E Op E
1: E -> Number
2: E['] -> E /* vlhs real=1 */
END_RULES

Marpa::Test::is( $grammar->show_QDFA,
    <<'END_QDFA', 'Ambiguous Equation QDFA' );
Start States: S0; S1
S0: 7
E['] -> . E
 <E> => S2
S1: predict; 1,5
E -> . E Op E
E -> . Number
 <E> => S3
 <Number> => S4
S2: 8
E['] -> E .
S3: 2
E -> E . Op E
 <Op> => S1; S5
S4: 6
E -> Number .
S5: 3
E -> E Op . E
 <E> => S6
S6: 4
E -> E Op E .
END_QDFA

my $recce = Marpa::Recognizer->new( { grammar => $grammar } );

$recce->tokens(
    [   [ 'Number', 2,    1 ],
        [ 'Op',     q{-}, 1 ],
        [ 'Number', 0,    1 ],
        [ 'Op',     q{*}, 1 ],
        [ 'Number', 3,    1 ],
        [ 'Op',     q{+}, 1 ],
        [ 'Number', 1,    1 ],
    ]
);

my %expected_value = (
    '(2-(0*(3+1)))==2' => 1,
    '(((2-0)*3)+1)==7' => 1,
    '((2-(0*3))+1)==3' => 1,
    '((2-0)*(3+1))==8' => 1,
    '(2-((0*3)+1))==1' => 1,
);
my $evaler = Marpa::Evaluator->new( { recce => $recce, clone => 0 } );
Marpa::exception('Parse failed') if not $evaler;

my $i = 0;
while ( defined( my $value = $evaler->value() ) ) {
    my $value = ${$value};
    Test::More::ok( $expected_value{$value}, "Value $i (unspecified order)" );
    delete $expected_value{$value};
    $i++;
} ## end while ( defined( my $value = $evaler->value() ) )

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
