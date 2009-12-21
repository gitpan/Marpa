#!perl
#
use 5.010;

# An ambiguous equation,
# this time using the lexer

use strict;
use warnings;
use lib 'lib';
use English qw( -no_match_vars );

use Test::More tests => 11;
use Marpa::Test;

BEGIN {
    Test::More::use_ok( 'Marpa', 'alpha' );
    Test::More::use_ok('Marpa::MDL');
}

my $source = do { local $RS = undef; <main::DATA> };

my $text = '6-----1';

my ( $marpa_options, $mdlex_options ) = Marpa::MDL::to_raw($source);

my $g = Marpa::Grammar->new(
    {   maximal => 1,
        actions => 'main',
    },
    @{$marpa_options}
);

$g->precompute();

my $recce = Marpa::Recognizer->new( { grammar => $g } );
my $lexer = Marpa::MDLex->new( { recce => $recce }, @{$mdlex_options} );
$lexer->text( \$text );
$recce->end_input();

my $evaler = Marpa::Evaluator->new(
    { recce => $recce, max_parses => 30, parse_order => 'original' } );
my @values = ();
while ( defined( my $value = $evaler->value() ) ) {
    push @values, $value;
}

my @expected = (
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

my $expected_count = @expected;
my $values_count   = @values;

Marpa::Test::is( $expected_count, $values_count,
    'Expected number of values' );

for my $i ( 0 .. $#expected ) {
    Marpa::Test::is( ${ $values[$i] },
        $expected[$i], "Minuses Equation Value $i" );
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
    return '(' . $string . '--)==' . $value--;
}

sub prefix_decr {
    shift;
    my ( $string, $value ) = ( $_[1] =~ /^(.*)==(.*)$/xms );
    return '(--' . $string . ')==' . --$value;
}

sub negation {
    shift;
    my ( $string, $value ) = ( $_[1] =~ /^(.*)==(.*)$/xms );
    return '(-' . $string . ')==' . -$value;
}

sub number {
    shift;
    my $value = $_[0];
    return "$value==$value";
}

sub default_action {
    shift;
    my $v_count = scalar @_;
    return q{}   if $v_count <= 0;
    return $_[0] if $v_count == 1;
    return '(' . join( q{;}, @_ ) . ')';
} ## end sub default_action

## use critic

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:

__DATA__
semantics are perl5.

the start symbol is E.

E: E, Minus, E.  priority 50.  'subtraction'.
 
E: E, Minus Minus.  priority 40.  'postfix_decr'.

E: Minus Minus, E.  priority 30.  'prefix_decr'.

E: Minus, E.  priority 20.  'negation'.

E: Number.  'number'.  

Number matches qr/\d+/.

Minus matches qr/[-]/.

Minus Minus matches qr/[-][-]/.

the default action is 'default_action'.
