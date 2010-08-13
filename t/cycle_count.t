#!perl

use 5.010;
use strict;
use warnings;

use lib 'lib';
use Test::More tests => 9;
use English qw( -no_match_vars );
use Marpa::Test;
use Carp;

BEGIN {
    Test::More::use_ok('Marpa');
}

## no critic (Subroutines::RequireArgUnpacking)

# Ranks are less than zero
# to make sure
# that the ranks are floating point --
# that integer rounding
# is not happening anywhere.

our $CYCLE_RANK = 1;

# If we are counting up, the lowest number
# has to have the highest numerical rank.
# sub rank_cycle { return \($main::CYCLE_RANK*(Marpa::location()+1)) }
sub rank_cycle { return \( $main::CYCLE_RANK * ( 9 - Marpa::location() ) ) }

sub rule_action  { return 'direct' }
sub cycle_action { return 'cycle' }

sub default_rule_action {
    shift;
    return join q{;}, @_;
}

## use critic

my $grammar = Marpa::Grammar->new(
    {   start                => 'S',
        strip                => 0,
        infinite_action      => 'quiet',
        cycle_ranking_action => 'main::rank_cycle',
        rules                => [
            {   lhs    => 'S',
                rhs    => [qw/item item/],
                action => 'main::default_rule_action'
            },
            {   lhs    => 'item',
                rhs    => ['direct'],
                action => 'main::rule_action',

                # ranking_action => 'main::rank_rule'
            },
            {   lhs    => 'item',
                rhs    => ['cycle'],
                action => 'main::cycle_action'
            },
            {   lhs => 'cycle',
                rhs => ['cycle2'],
            },
            {   lhs => 'cycle2',
                rhs => ['cycle'],
            },
            {   lhs => 'direct',
                rhs => ['t'],
            },
            {   lhs => 'cycle2',
                rhs => ['t'],
            },
        ],
        terminals => [qw(t)],
    }
);

$grammar->precompute();

my $recce = Marpa::Recognizer->new(
    { grammar => $grammar, ranking_method => 'constant' } );

my $input_length = 2;
$recce->tokens( [ ( ['t'] ) x $input_length ] );

my @expected1 = qw(
    direct;direct
    direct;cycle
    cycle;direct
    cycle;cycle
);
my @expected = ( @expected1, ( reverse @expected1 ) );

my $i = 0;
for my $cycle_rank ( -1, 1 ) {
    $main::CYCLE_RANK = $cycle_rank;
    $recce->reset_evaluation();
    while ( my $result = $recce->value() ) {
        Test::More::is( ${$result}, $expected[$i], "cycle_rank=$cycle_rank" );
        $i++;
    }
} ## end for my $cycle_rank ( -1, 1 )

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
