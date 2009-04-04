#!perl
#
use 5.010;
use strict;
use warnings;
use lib 'lib';
use lib 't/lib';
use English qw( -no_match_vars );

use Test::More tests => 5;
use Marpa::Test;
use Carp;

BEGIN {
    use_ok('Marpa');
}

my @tests = split /\n/xms, <<'EO_TESTS';
time  / 25 ; # / ; die "this dies!";
sin  / 25 ; # / ; die "this dies!";
EO_TESTS

# my @tests = split /\n/xms, <<'EO_TESTS';
# time  / 25 ; # / ; die "this dies!";
# sin  / 25 ; # / ; die "this dies!";
# caller  / 25 ; # / ; die "this dies!";
# eof  / 25 ; # / ; die "this dies!";
# localtime  / 25 ; # / ; die "this dies!";
# EO_TESTS

my $source;
{ local ($RS) = undef; $source = <DATA> };

my $g = new Marpa::Grammar(
    {   warnings   => 1,
        code_lines => -1,
    }
);

$g->set( { mdl_source => \$source } );

$g->precompute();

TEST: while ( my $test = pop @tests ) {
    my $recce = new Marpa::Recognizer( { grammar => $g } );
    $recce->text( \$test );
    $recce->end_input();
    my $evaler = new Marpa::Evaluator( { recce => $recce } );
    my @parses;
    while ( defined( my $value = $evaler->old_value ) ) {
        push @parses, $value;
    }
    my @expected_parses;
    my ($test_name) = ( $test =~ /\A([a-z]+) /xms );
    given ($test_name) {
        when ('time') {
            @expected_parses = ('division, comment');
        }
        when ('sin') {
            @expected_parses =
                ( 'division, comment', 'sin function call, die statement', );
        }
        default {
            croak("unexpected test: $test_name");
        }
    } ## end given
    my $expected_parse_count = scalar @expected_parses;
    my $parse_count          = scalar @parses;
    Marpa::Test::is( $parse_count, $expected_parse_count,
        "Parse count for $test_name is $parse_count" );
    my $mismatch_count = 0;
    my $parses_to_check =
          $parse_count < $expected_parse_count
        ? $expected_parse_count
        : $parse_count;
    for my $i ( 0 .. ( $parses_to_check - 1 ) ) {
        if ( ${ $parses[$i] } ne $expected_parses[$i] ) {
            diag(     "Mismatch on parse $i for test $test_name: "
                    . ${ $parses[$i] } . ' vs. '
                    . $expected_parses[$i] );
            $mismatch_count++;
        } ## end if ( ${ $parses[$i] } ne $expected_parses[$i] )
    } ## end for my $i ( 0 .. ( $parses_to_check - 1 ) )
    ok( !$mismatch_count,
        ( $expected_parse_count - $mismatch_count )
            . " of the $expected_parse_count parses expected succeeded" );
} ## end while ( my $test = pop @tests )

__DATA__
semantics are perl5.  version is 0.001_006.  the start symbol is perl line.
the default lex prefix is qr/\s*/.

perl line: perl statements, optional comment.
q{
    my $result = $_[0];
    $result .= ", comment"
	if defined $_[1];
    $result
}.

perl statements: semicolon separated perl statement sequence.
q{ join(", ", @_) }.

perl statement: division. q{ "division" }.

perl statement: function call.
q{ $_[0] }.

perl statement: empty statement.  q{ "empty statement" }.

perl statement: /die/, string literal.  q{ "die statement" }.

division: expr, division sign, expr.

expr: function call.

expr: number.

function call: unary function name, argument.
q{ $_[0] . " function call" }.

function call: nullary function name.
q{ $_[0] . " function call" }.

argument: pattern match.

empty statement: horizontal whitespace.

horizontal whitespace matches qr/ \t/.

unary function name matches /(caller|eof|sin|localtime)/.

nullary function name matches /(caller|eof|sin|time|localtime)/.

number matches /\d+/.

semicolon matches /;/.

division sign matches qr{/}.

pattern match matches qr{/[^/]*/}.

comment matches /#.*/.

string literal matches qr{"[^"]*"}.
