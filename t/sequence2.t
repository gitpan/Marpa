#!/usr/bin/perl

# Tests of the sequence in the Marpa::Grammar doc

use 5.010;
use strict;
use warnings;

use Fatal qw(open close);
use Test::More tests => 4;

use lib 'lib';
use Marpa::Test;

BEGIN {
    Test::More::use_ok('Marpa');
}

## no critic (Subroutines::RequireArgUnpacking)
sub sequence      { shift; return 'seq(' .  ( join q{;}, @_ ) . ')' }
sub sequence_item { shift; return 'item(' . ( join q{;}, @_ ) . ')' }
## use critic

my $grammar;
my $recce;
my $value_ref;
my $value;

my $min0 =
#<<< no perltidy
# Marpa::Display
# name: Marpa::Grammar min 0 sequence example

    { lhs => 'sequence', rhs => ['sequence_item'], min => 0 }

# Marpa::Display::End
; # semicolon to terminate rule

$grammar = Marpa::Grammar->new(
    {   start     => 'sequence',
        terminals => [qw(sequence_item)],
        rules     => [$min0],
        actions => 'main'
    }
);

$grammar->precompute();

$recce = Marpa::Recognizer->new( { grammar => $grammar } );

$recce->tokens( [ [ 'sequence_item', '0' ], ['sequence_item', '1'] ]);

$value_ref = $recce->value();
$value = $value_ref ? ${$value_ref} : 'No Parse';
$value //= 'undef returned';

Marpa::Test::is( $value, 'seq(0;1)', 'min 0 value' );

my $min1 =
#<<< no perltidy
# Marpa::Display
# name: Marpa::Grammar min 1 sequence example

    { lhs => 'sequence', rhs => ['sequence_item'], min => 1 }

# Marpa::Display::End
; # semicolon to terminate rule

$grammar = Marpa::Grammar->new({
     start => 'sequence',
     rules => [ $min1 ],
        actions => 'main'
});

$grammar->precompute();

$recce = Marpa::Recognizer->new( { grammar => $grammar } );

$recce->tokens( [ [ 'sequence_item', '0' ], [ 'sequence_item', '1' ] ] );

$value_ref = $recce->value();
$value = $value_ref ? ${$value_ref} : 'No Parse';
$value //= 'undef returned';

Marpa::Test::is( $value, 'seq(0;1)', 'min 1 value' );

my $multipart = [
#<<< no perltidy
# Marpa::Display
# name: Marpa::Grammar multipart rhs sequence example

    { lhs => 'sequence', rhs => [qw(sequence_item)], min => 0 },
    { lhs => 'sequence_item', rhs => [qw(part1 part2)], },

# Marpa::Display::End
]; # semicolon to terminate rule

$grammar = Marpa::Grammar->new(
    {   start => 'sequence',
        terminals => [qw(part1 part2)],
        rules => $multipart,
        actions => 'main'
    }
);


$grammar->precompute();

$recce = Marpa::Recognizer->new( { grammar => $grammar } );

$recce->tokens( [ [ 'part1', '0' ], [ 'part2', '1' ] ] );

$value_ref = $recce->value();
$value = $value_ref ? ${$value_ref} : 'No Parse';
$value //= 'undef returned';

Marpa::Test::is( $value, 'seq(item(0;1))', 'multipart rhs value' );

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
