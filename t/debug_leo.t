#!/usr/bin/perl

# Debug Leo Example

use 5.010;
use strict;
use warnings;

use Test::More tests => 3;

use lib 'lib';
use English qw( -no_match_vars );
use Fatal qw( open close );
use Marpa::Test;

BEGIN {
    Test::More::use_ok('Marpa');
}

my $progress_report = q{};

# Marpa::Display
# name: Debug Leo Example

my $grammar = Marpa::Grammar->new(
    {   start         => 'S',
        strip         => 0,
        lhs_terminals => 0,
        rules         => [
            { lhs => 'S',            rhs => [qw/Top_sequence/] },
            { lhs => 'Top_sequence', rhs => [qw/Top Top_sequence/] },
            { lhs => 'Top_sequence', rhs => [qw/Top/] },
            { lhs => 'Top',          rhs => [qw/Upper_Middle/] },
            { lhs => 'Upper_Middle', rhs => [qw/Lower_Middle/] },
            { lhs => 'Lower_Middle', rhs => [qw/Bottom/] },
            { lhs => 'Bottom',       rhs => [qw/T/] },
        ],
    }
);

# Marpa::Display::End

$grammar->precompute();

my @tokens = ( ['T'] ) x 20;

my $recce = Marpa::Recognizer->new( { grammar => $grammar } );

my $current_earleme = $recce->tokens( \@tokens );

$progress_report = $recce->show_progress();

my $value_ref = $recce->value;
Test::More::ok( $value_ref, 'Parse ok?' );

# Marpa::Display
# name: Debug Leo Example Progress Report
# start-after-line: END_PROGRESS_REPORT
# end-before-line: '^END_PROGRESS_REPORT$'

Marpa::Test::is( $progress_report,
    <<'END_PROGRESS_REPORT', 'sorted progress report' );
PREDICTING @20 1: Top_sequence -> Top Top_sequence
PREDICTING @20 2: Top_sequence -> Top
PREDICTING @20 3: Top -> Upper_Middle
PREDICTING @20 4: Upper_Middle -> Lower_Middle
PREDICTING @20 5: Lower_Middle -> Bottom
PREDICTING @20 6: Bottom -> T
BUILDING @19-20 Top_sequence -> Top . Top_sequence
COMPLETED @0-20 0: S -> Top_sequence
COMPLETED @0-20 (Leo) 1: Top_sequence -> Top Top_sequence
COMPLETED @19-20 2: Top_sequence -> Top
COMPLETED @19-20 (Leo) 3: Top -> Upper_Middle
COMPLETED @19-20 (Leo) 4: Upper_Middle -> Lower_Middle
COMPLETED @19-20 6: Bottom -> T
COMPLETED @0-20 7: S['] -> S
END_PROGRESS_REPORT

# Marpa::Display::End

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
