#!/usr/bin/perl

use 5.010;
use strict;
use warnings;
use lib 'lib';
use English qw( -no_match_vars );

use Test::More tests => 5;
use t::lib::Marpa::Test;

BEGIN {
    Test::More::use_ok('Marpa');
}

my @tests = split /\n/xms, <<'EO_TESTS';
sin  / 25 ; # / ; die "this dies!";
time  / 25 ; # / ; die "this dies!";
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

my $g = Marpa::Grammar->new(
    {   warnings   => 1,
        code_lines => -1,
    }
);

$g->set( { mdl_source => \$source } );

$g->precompute();

TEST: while ( my $test = pop @tests ) {
    my $recce = Marpa::Recognizer->new( { grammar => $g } );
    $recce->text( \$test );
    $recce->end_input();
    my $evaler = Marpa::Evaluator->new( { recce => $recce } );
    my @parses;
    while ( defined( my $value = $evaler->value ) ) {
        push @parses, ${$value};
    }
    my @expected_parses;
    my ($test_name) = ( $test =~ /\A([a-z]+) /xms );
    given ($test_name) {
        when ('time') {
            @expected_parses = ('division, comment');
        }
        when ('sin') {
            @expected_parses =
                ( 'sin function call, die statement', 'division, comment' );
        }
        default {
            Marpa::exception("unexpected test: $test_name");
        }
    } ## end given
    my $expected_parse_count = scalar @expected_parses;
    my $parse_count          = scalar @parses;
    Marpa::Test::is( $parse_count, $expected_parse_count,
        "Parse count for $test_name is $parse_count" );

    my $expected = join "\n", @expected_parses;
    my $actual   = join "\n", @parses;
    Marpa::Test::is( $actual, $expected, 'Parses match' );
} ## end while ( my $test = pop @tests )

__DATA__
semantics are perl5.  version is 0.001_017.  the start symbol is perl line.
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
