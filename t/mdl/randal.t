#!/usr/bin/perl

use 5.010;
use strict;
use warnings;
use lib 'lib';
use English qw( -no_match_vars );

use Test::More tests => 5;
use t::lib::Marpa::Test;

BEGIN {
    Test::More::use_ok('Marpa::MDL');
}

my @tests = split /\n/xms, <<'EO_TESTS';
sin  / 25 ; # / ; die "this dies!";
time  / 25 ; # / ; die "this dies!";
EO_TESTS

my $source;
{ local ($RS) = undef; $source = <DATA> };

my ( $marpa_options, $mdlex_options ) = Marpa::MDL::to_raw($source);

my $g = Marpa::Grammar->new(
    {   warnings   => 1,
        code_lines => -1,
        actions    => 'main',
    },
    @{$marpa_options}
);

$g->precompute();

TEST: while ( my $test = pop @tests ) {

    my $recce = Marpa::Recognizer->new( { grammar => $g } );
    my $lexer = Marpa::MDLex->new( { recce => $recce }, @{$mdlex_options} );
    $lexer->text( \$test );
    $recce->tokens();

    my $evaler = Marpa::Evaluator->new( { recce => $recce } );
    my @parses;
    while ( defined( my $value = $evaler->value ) ) {
        push @parses, ${$value};
    }
    my ($test_name) = ( $test =~ /\A([a-z]+) /xms );
    my @expected_parses;
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

## no critic (Subroutines::RequireArgUnpacking)

sub show_perl_line {
    my $result = $_[1];
    defined $_[2]
        and $result .= ', comment';
    return $result;
} ## end sub show_perl_line

sub show_statement_sequence { shift; return join q{, }, @_ }
sub show_division           { return 'division' }
sub show_function_call      { return $_[1] }
sub show_die                { return 'die statement' }
sub show_unary   { return $_[1] . ' function call' }
sub show_nullary { return $_[1] . ' function call' }

## use critic

__DATA__
semantics are perl5.  the start symbol is perl line.
the default lex prefix is qr/\s*/.

perl line: perl statements, optional comment. 'show_perl_line'.

perl statements: semicolon separated perl statement sequence.
'show_statement_sequence'.

perl statement: division. 'show_division'.

perl statement: function call.  'show_function_call'.

perl statement: /die/, string literal.  'show_die'.

division: expr, division sign, expr.

expr: function call.

expr: number.

function call: unary function name, argument. 'show_unary'.

function call: nullary function name. 'show_nullary'.

argument: pattern match.

unary function name matches /(caller|eof|sin|localtime)/.

nullary function name matches /(caller|eof|sin|time|localtime)/.

number matches /\d+/.

semicolon matches /;/.

division sign matches qr{[/]}.

pattern match matches qr{[/][^/]*/}.

comment matches /[#].*/.

string literal matches qr{"[^"]*"}.
