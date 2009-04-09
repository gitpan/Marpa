#!perl

use 5.010;
use strict;
use warnings;
use Test::More tests => 2;
use lib 'lib';
use Marpa;

# This is code to test examples, in order to prove that they do actually
# compile and execute.  No checking other than for compilation errors
# or fatal exceptions is done.  This code DOES NOT do anything sensible.

pass('misc.pl compiled');

my $mdl_source = <<'END';
semantics are perl5.
version is 0.001_007.
start symbol is S.

S: Document.

Document: /.+/ .

Document: .

END

my $trace_fh;
my $location;
my $first_result;
my @all_results;

my $grammar = new Marpa::Grammar();

$grammar->set( { mdl_source => \$mdl_source } );

my $cloned_grammar = $grammar->clone();

my $stringified_grammar = $grammar->stringify();

## use Marpa::Test::Display unstringify snippet

$grammar = Marpa::Grammar::unstringify( $stringified_grammar, $trace_fh );

$grammar = Marpa::Grammar::unstringify($stringified_grammar);

## no Marpa::Test::Display

my $new_lex_preamble = q{1};

## use Marpa::Test::Display new Recognizer snippet

my $recce = new Marpa::Recognizer(
    {   grammar      => $grammar,
        lex_preamble => $new_lex_preamble,
    }
);

## no Marpa::Test::Display

$recce->end_input();

my $stringified_recce = $recce->stringify();

## use Marpa::Test::Display unstringify Recognizer snippet

$recce = Marpa::Recognizer::unstringify( $stringified_recce, $trace_fh );

$recce = Marpa::Recognizer::unstringify($stringified_recce);

## no Marpa::Test::Display

my $cloned_recce = $recce->clone();

my $evaler = new Marpa::Evaluator(
    {   recce => $recce,
        end   => $location,
        clone => 0,
    }
);

my $depth = 1;

## use Marpa::Test::Display evaler set snippet

$evaler->set( { trace_values => 1 } );

## no Marpa::Test::Display

my $input_string = q{};
my $lexeme_start = 0;

{
## use Marpa::Test::Display lex_regex snippet

    my ( $regex, $token_length ) =
        Marpa::Lex::lex_regex( \$input_string, $lexeme_start );

## no Marpa::Test::Display
}

{
## use Marpa::Test::Display lex_q_quote snippet

    my ( $string, $token_length ) =
        Marpa::Lex::lex_q_quote( \$input_string, $lexeme_start );

## no Marpa::Test::Display
}

my $g = new Marpa::Grammar();

$g->set( { start => Marpa::MDL::canonical_symbol_name('Document') } );

## use Marpa::Test::Display get_symbol snippet

my $op = Marpa::MDL::get_symbol( $grammar, 'Op' );

## no Marpa::Test::Display

my $grammar_description = $mdl_source;
my $string_to_parse     = q{};

## use Marpa::Test::Display mdl scalar snippet

$first_result = Marpa::mdl( \$grammar_description, \$string_to_parse );

## no Marpa::Test::Display

## use Marpa::Test::Display mdl array snippet

@all_results = Marpa::mdl( \$grammar_description, \$string_to_parse );

## no Marpa::Test::Display

## use Marpa::Test::Display mdl scalar hash args snippet

$first_result =
    Marpa::mdl( \$grammar_description, \$string_to_parse, { warnings => 0 } );

## no Marpa::Test::Display

pass('misc.pl ran to end');
