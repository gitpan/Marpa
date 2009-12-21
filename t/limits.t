#!perl

# the example grammar in Aycock/Horspool "Practical Earley Parsing",
# _The Computer Journal_, Vol. 45, No. 6, pp. 620-630,
# in its "NNF" form

use 5.010;
use strict;
use warnings;
use English qw( -no_match_vars );
use Test::More tests => 6;

use lib 'lib';
use Marpa::Test;

BEGIN {
    Test::More::use_ok( 'Marpa', 'alpha' );
}

## no critic (Subroutines::RequireArgUnpacking)
sub default_action {
    shift;
    my $v_count = scalar @_;
    return q{}   if $v_count <= 0;
    return $_[0] if $v_count == 1;
    return '(' . join( q{;}, @_ ) . ')';
} ## end sub default_action
## use critic

sub test_grammar {
    my ( $grammar_args, $earleme_length ) = @_;
    $earleme_length //= 1;

    my $grammar;
    my $eval_ok = eval { $grammar = Marpa::Grammar->new($grammar_args); 1; };
    Marpa::exception("Exception while creating Grammar:\n$EVAL_ERROR")
        if not $eval_ok;
    Marpa::exception("Grammar not created\n") if not $grammar;
    $grammar->precompute();

    my $recce;
    $eval_ok = eval {
        $recce = Marpa::Recognizer->new(
            { grammar => $grammar, mode => 'stream' } );
        1;
    };
    Marpa::exception("Exception while creating Recognizer:\n$EVAL_ERROR")
        if not $eval_ok;
    Marpa::exception("Recognizer not created\n") if not $recce;

    my $earleme_result;
    $eval_ok = eval {
        $earleme_result = $recce->tokens( [ [ 'a', 'a', 1 ] ] );
        1;
    };
    Marpa::exception("Exception while recognizing earleme:\n$EVAL_ERROR")
        if not $eval_ok;
    Marpa::exception("Parsing exhausted\n") if not defined $earleme_result;

    $eval_ok = eval {
        $earleme_result = $recce->tokens( [ [ 'a', 'a', $earleme_length ] ] );
        1;
    };
    Marpa::exception("Exception while recognizing earleme:\n$EVAL_ERROR")
        if not $eval_ok;
    Marpa::exception("Parsing exhausted\n") if not defined $earleme_result;

    $eval_ok = eval { $recce->end_input(); 1; };
    Marpa::exception("Exception while recognizing end of input:\n$EVAL_ERROR")
        if not $eval_ok;

    my $evaler;
    $eval_ok = eval {
        $evaler = Marpa::Evaluator->new( { recce => $recce, } );
        1;
    };
    Marpa::exception("Exception while creating Evaluator:\n$EVAL_ERROR")
        if not $eval_ok;
    Marpa::exception("Evaluator not created\n") if not $evaler;
    my $value = $evaler->value;
    Marpa::exception("No parse\n") if not $value;
    return ${$value};
} ## end sub test_grammar

# RHS too long is not testable
# Perl runs out of memory first

# test a grammar with no limit problems
my $result_on_success = '(a;a)';

my $placebo = {
    start => 'S',
    strip => 0,
    rules => [
        #<<< no perltidy
        [ 'S', [ qw(A A) ] ],
        [ 'A', [qw/a/] ]
        #>>>
    ],
    default_null_value => q{},
    default_action     => 'main::default_action',
};

sub test_rule_priority {
    my ($priority) = @_;
    return {
        start => 'S',
        rules => [
        #<<< no perltidy
        [ 'S', [ qw(A A) ] ],
        [ 'A', [qw/a/], undef, $priority ]
        #>>>
        ],
        default_null_value => q{},
        default_action     => 'main::default_action',
    };
} ## end sub test_rule_priority

my $value;
my $eval_ok = eval { $value = test_grammar($placebo); 1; };
if ( not defined $eval_ok ) {
    Test::More::diag($EVAL_ERROR);
    Test::More::fail('Placebo grammar');
}
else { Test::More::is( $value, $result_on_success, 'Placebo grammar' ) }

## lots of test values in the following, some of them pretty
## arbitrary

$eval_ok =
    eval { $value = test_grammar( test_rule_priority(1_000_000) ); 1; };
if ( not defined $eval_ok ) { Test::More::fail('Priority very high') }
else {
    Test::More::is( $value, $result_on_success,
        'Priority very high, but still OK' );
}

$eval_ok = eval { $value = test_grammar( test_rule_priority( 2**31 ) ); 1; };
REPORT_RESULT: {
    if ( defined $eval_ok ) {
        Test::More::fail('Did not catch over-high rule priority');
        last REPORT_RESULT;
    }
    if ( $EVAL_ERROR =~ / \A Exception \s while \s creating \s Grammar /xms )
    {
        Test::More::pass('Caught over-high rule priority');
        last REPORT_RESULT;
    }
    Test::More::is(
        $EVAL_ERROR,
        "Exception while creating Grammar\n",
        'Priority too high'
    );
} ## end REPORT_RESULT:

$eval_ok = eval { $value = test_grammar( $placebo, 20_031 ); 1; };
if ( not defined $eval_ok ) { Test::More::fail('Earleme very long') }
else {
    Test::More::is( $value, $result_on_success,
        'Earleme very long, but still OK' );
}

$eval_ok = eval { $value = test_grammar( $placebo, 2**31 ); 1; };
REPORT_RESULT: {
    if ( defined $eval_ok ) {
        Test::More::diag("Earleme too long test returned value: $value");
        Test::More::fail('Did not catch problem with earleme too long');
        last REPORT_RESULT;
    }
    if ( $EVAL_ERROR
        =~ / \A Exception \s while \s recognizing \s earleme /xms )
    {
        Test::More::pass('Caught over-long earleme');
        last REPORT_RESULT;
    } ## end if ( $EVAL_ERROR =~ ...)
    Test::More::is( $EVAL_ERROR, q{}, 'Grammar with earleme too long' );
} ## end REPORT_RESULT:

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
