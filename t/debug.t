#!/usr/bin/perl

# Engine Synopsis

use 5.010;
use strict;
use warnings;

use Test::More tests => 4;

use lib 'lib';
use English qw( -no_match_vars );
use Fatal qw( open close );
use Marpa::Test;

BEGIN {
    Test::More::use_ok('Marpa');
}

my $progress_report = q{};

# Marpa::Display
# name: Debug Example Part 1

my $grammar = Marpa::Grammar->new(
    {   start          => 'Expression',
        actions        => 'My_Actions',
        default_action => 'first_arg',
        strip          => 0,
        rules          => [
            ## This is a deliberate error in the grammar
            ## The next line should be:
            ## { lhs => 'Expression', rhs => [qw/Term/] },
            ## I have changed the Term to 'Factor' which
            ## will cause problems.
            { lhs => 'Expression', rhs => [qw/Factor/] },
            { lhs => 'Term',       rhs => [qw/Factor/] },
            { lhs => 'Factor',     rhs => [qw/Number/] },
            {   lhs    => 'Term',
                rhs    => [qw/Term Add Term/],
                action => 'do_add'
            },
            {   lhs    => 'Factor',
                rhs    => [qw/Factor Multiply Factor/],
                action => 'do_multiply'
            },
        ],
    }
);

# Marpa::Display::End

## no critic (InputOutput::RequireBriefOpen)
open my $trace_fh, q{>}, \( my $trace_output = q{} );
## use critic

$grammar->set( { trace_file_handle => $trace_fh } );

# Marpa::Display
# name: Debug Example Part 2

$grammar->precompute();

my @tokens = (
    [ 'Number',   42 ],
    [ 'Multiply', q{*} ],
    [ 'Number',   1 ],
    [ 'Add',      q{+} ],
    [ 'Number',   7 ],
);

sub My_Actions::do_add {
    my ( undef, $t1, undef, $t2 ) = @_;
    return $t1 + $t2;
}

sub My_Actions::do_multiply {
    my ( undef, $t1, undef, $t2 ) = @_;
    return $t1 * $t2;
}

sub My_Actions::first_arg { shift; return shift; }

my $recce = Marpa::Recognizer->new(
    { grammar => $grammar, trace_terminals => 2, mode => 'stream' } );

my $token_ix = 0;

my ( $current_earleme, $expected_tokens ) =
    $recce->tokens( \@tokens, \$token_ix );

if ( $token_ix <= $#tokens ) {
    $progress_report = $recce->show_progress( 0, $current_earleme );
}

# Marpa::Display::End

my $value_ref = $recce->value;
my $value = $value_ref ? ${$value_ref} : 'No Parse';

Test::More::is( $value, 42, 'value' );

# Marpa::Display
# name: Debug Example Progress Report
# start-after-line: END_PROGRESS_REPORT
# end-before-line: '^END_PROGRESS_REPORT$'

Test::More::is( $progress_report,
    <<'END_PROGRESS_REPORT', 'progress report' );
PREDICTING @0 0: Expression -> Factor
PREDICTING @0 2: Factor -> Number
PREDICTING @0 4: Factor -> Factor Multiply Factor
PREDICTING @0 5: Expression['] -> Expression
BUILDING @0-1 Factor -> Factor . Multiply Factor
COMPLETED @0-1 0: Expression -> Factor
COMPLETED @0-1 2: Factor -> Number
COMPLETED @0-1 5: Expression['] -> Expression
PREDICTING @2 2: Factor -> Number
PREDICTING @2 4: Factor -> Factor Multiply Factor
BUILDING @0-2 Factor -> Factor Multiply . Factor
BUILDING @0-3 Factor -> Factor . Multiply Factor
BUILDING @2-3 Factor -> Factor . Multiply Factor
COMPLETED @0-3 0: Expression -> Factor
COMPLETED @2-3 2: Factor -> Number
COMPLETED @0-3 4: Factor -> Factor Multiply Factor
COMPLETED @0-3 5: Expression['] -> Expression
END_PROGRESS_REPORT

# Marpa::Display::End

# Marpa::Display
# name: Debug Example Trace Output
# start-after-line: END_TRACE_OUTPUT
# end-before-line: '^END_TRACE_OUTPUT$'

Test::More::is( $trace_output, <<'END_TRACE_OUTPUT', 'trace output' );
Inaccessible symbol: Add
Inaccessible symbol: Term
Setting trace_terminals option
Expecting "Number" at earleme 0
Expecting "Expression" at earleme 0
Expecting "Factor" at earleme 0
Accepted "Number" at 0-1
Expecting "Multiply" at 1
Accepted "Multiply" at 1-2
Expecting "Number" at 2
Expecting "Factor" at 2
Accepted "Number" at 2-3
Expecting "Multiply" at 3
Rejected "Add" at 3
END_TRACE_OUTPUT

# Marpa::Display::End

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
