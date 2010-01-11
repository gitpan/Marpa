#!perl

# Basic tests of sequences.
# The matrix is separation (none/perl5/proper);
# and minimium count (0, 1, 3);
# keep vs. no-keep.

use 5.010;
use strict;
use warnings;

use lib 'lib';
use Test::More tests => 71;
use Marpa::Test;

BEGIN {
    Test::More::use_ok('Marpa');
}

## no critic (Subroutines::RequireArgUnpacking)

sub null_string { return q{} }

sub default_action {
    shift;
    my $v_count = scalar @_;
    return q{}   if $v_count <= 0;
    return $_[0] if $v_count == 1;
    return '(' . join( q{;}, @_ ) . ')';
} ## end sub default_action

## use critic

sub run_sequence_test {
    my ( $minimum, $separation, $keep ) = @_;

    my @terminals       = ('A');
    my @separation_args = ();
    if ( $separation ne 'none' ) {
        push @separation_args, separator => 'sep';
        push @terminals, 'sep';
    }
    if ( $separation eq 'proper' ) {
        push @separation_args, proper => 1;
    }

    my $grammar = Marpa::Grammar->new(
        {   start => 'TOP',
            strip => 0,
            rules => [
                {   lhs  => 'TOP',
                    rhs  => [qw/A/],
                    min  => $minimum,
                    keep => $keep,
                    @separation_args
                },
            ],
            default_action      => 'main::default_action',
            default_null_action => 'main::null_string',
        }
    );

    $grammar->set( { terminals => \@terminals } );

    $grammar->precompute();

    # Number of symbols to test at the higher numbers is
    # more or less arbitrary.  You really need to test 0 .. 3.
    # And you ought to test a couple of higher values,
    # say 5 and 10.
    SYMBOL_COUNT: for my $symbol_count ( 0, 1, 2, 3, 5, 10 ) {

        next SYMBOL_COUNT if $symbol_count < $minimum;
        my $test_name =
              "min=$minimum;"
            . ( $keep ? 'keep;' : q{} )
            . ( $separation ne 'none' ? "$separation;" : q{} )
            . ";count=$symbol_count";
        my $recce = Marpa::Recognizer->new(
            { grammar => $grammar, mode => 'stream' } );

        my @expected       = ();
        my $last_symbol_ix = $symbol_count - 1;
        SYMBOL_IX: for my $symbol_ix ( 0 .. $last_symbol_ix ) {
            push @expected, 'a';
            defined $recce->tokens( [ [ 'A', 'a', 1 ] ] )
                or Marpa::exception('Parsing exhausted');
            next SYMBOL_IX if $separation eq 'none';
            next SYMBOL_IX
                if $symbol_ix >= $last_symbol_ix
                    and $separation ne 'perl5';
            if ($keep) {
                push @expected, q{!};
            }
            defined $recce->tokens( [ [ 'sep', q{!}, 1 ] ] )
                or Marpa::exception('Parsing exhausted');
        } ## end for my $symbol_ix ( 0 .. $last_symbol_ix )

        $recce->end_input();

        my $evaler = Marpa::Evaluator->new( { recce => $recce } );
        if ( not $evaler ) {
            Test::More::fail("$test_name: Parse failed");
            next SYMBOL_COUNT;
        }
        my $result = $evaler->value();

        my $expected = join q{;}, @expected;
        if ( @expected > 1 ) {
            $expected = "($expected)";
        }
        Test::More::is( ${$result}, $expected, $test_name );

    } ## end for my $symbol_count ( 0, 1, 2, 3, 5, 10 )

    return;
} ## end sub run_sequence_test

for my $minimum ( 0, 1, 3 ) {
    run_sequence_test( $minimum, 'none', 0 );
    for my $separation (qw(proper perl5)) {
        for my $keep ( 0, 1 ) {
            run_sequence_test( $minimum, $separation, $keep );
        }
    }
} ## end for my $minimum ( 0, 1, 3 )

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
