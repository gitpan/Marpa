#!perl
# An ambiguous equation

use 5.010;
use strict;
use warnings;

use Test::More tests => 2;

use lib 'lib';
use Marpa::Test;
use English qw( -no_match_vars );
use Fatal qw(open close);

BEGIN {
    Test::More::use_ok('Marpa');
}

## no critic (InputOutput::RequireBriefOpen)
open my $original_stdout, q{>&STDOUT};
## use critic

sub save_stdout {
    my $save;
    my $save_ref = \$save;
    close STDOUT;
    open STDOUT, q{>}, $save_ref;
    return $save_ref;
} ## end sub save_stdout

sub restore_stdout {
    close STDOUT;
    open STDOUT, q{>&}, $original_stdout;
    return 1;
}

## no critic (Subroutines::RequireArgUnpacking)

# Marpa::Display
# name: Null Value Example

sub L {
    shift;
    return 'L(' . ( join q{;}, @_ ) . ')';
}

sub R {
    shift;
    return 'R(' . ( join q{;}, @_ ) . ')';
}

sub S {
    shift;
    return 'S(' . ( join q{;}, @_ ) . ')';
}

my $grammar = Marpa::Grammar->new(
    {   start   => 'S',
        actions => 'main',
        rules   => [
            [ 'S', [qw/L R/] ],
            [ 'L', [qw/A B X/] ],
            [ 'L', [] ],
            [ 'R', [qw/A B Y/] ],
            [ 'R', [] ],
            [ 'A', [] ],
            [ 'B', [] ],
            [ 'X', [] ],
            [ 'Y', [] ],
        ],
        symbols => {
            L => { null_value => 'null L' },
            R => { null_value => 'null R' },
            A => { null_value => 'null A' },
            B => { null_value => 'null B' },
            X => { null_value => 'null X', terminal => 1 },
            Y => { null_value => 'null Y', terminal => 1 },
        },
    }
);

$grammar->precompute();

my $recce = Marpa::Recognizer->new( { grammar => $grammar } );

$recce->tokens( [ [ 'X', 'x' ], ] );

# Marpa::Display::End

## use critic

# Marpa::Display
# name: Null Value Example Output
# start-after-line: END_OF_OUTPUT
# end-before-line: '^END_OF_OUTPUT$'

chomp( my $expected = <<'END_OF_OUTPUT');
S(L(null A;null B;x);null R)
END_OF_OUTPUT

# Marpa::Display::End

my $value = $recce->value();
Marpa::Test::is( ${$value}, $expected, 'Null example' );

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
