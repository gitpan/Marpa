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

sub default_action {
    shift;
    my $v_count = scalar @_;
    return q{}   if $v_count <= 0;
    return $_[0] if $v_count == 1;
    return '(' . ( join q{;}, ( map { $_ // 'undef' } @_ ) ) . ')';
} ## end sub default_action

sub null_A { return 'null A' }
sub null_B { return 'null B' }
sub null_R { return 'null R' }
sub null_L { return 'null L' }
sub null_X { return 'null X' }
sub null_Y { return 'null Y' }

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
        default_action => 'default_action',
        symbols        => {
            L => { null_action => 'null_L' },
            R => { null_action => 'null_R' },
            A => { null_action => 'null_A' },
            B => { null_action => 'null_B' },
            X => { null_action => 'null_X' },
            Y => { null_action => 'null_Y' },
        },
        terminals => [qw(X Y)],
    }
);

# Marpa::Display::End

## use critic
$grammar->precompute();

my $recce = Marpa::Recognizer->new( { grammar => $grammar } );

$recce->tokens( [ [ 'X', 'x' ], ] );

my $value = $recce->value();
Marpa::Test::is( ${$value}, q{((null A;null B;x);null R)}, 'Null example' );

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
