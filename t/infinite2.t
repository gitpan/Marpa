#!perl

# A grammars with cycles
use 5.010;
use strict;
use warnings;
use lib 'lib';
use English qw( -no_match_vars );
use Fatal qw(open close chdir);
use Test::More tests => 4;
use Marpa::Test;

BEGIN {
    Test::More::use_ok('Marpa');
}

my %expected_original = map { ( $_ => 1 ) } qw( A(B(a)) a );

## no critic (Subroutines::RequireArgUnpacking)
sub show_a         { return 'A(' . $_[1] . ')' }
sub show_b         { return 'B(' . $_[1] . ')' }
sub default_action { shift; return join q{ }, @_ }
## use critic

package Test_Grammar;

$Test_Grammar::MARPA_OPTIONS = [
    {   'default_action' => 'main::default_action',
        'rules'          => [
            {   'lhs' => 's',
                'rhs' => ['a']
            },
            {   'action' => 'main::show_a',
                'lhs'    => 'a',
                'rhs'    => ['b']
            },
            {   'lhs' => 'a',
                'rhs' => ['a:k0']
            },
            {   'action' => 'main::show_b',
                'lhs'    => 'b',
                'rhs'    => ['a']
            }
        ],
        'start'           => 's',
        'terminals'       => ['a:k0'],
        'infinite_action' => 'warn'
    }
];

my $trace;
open my $MEMORY, '>', \$trace;
my $grammar = Marpa::Grammar->new(
    { trace_file_handle => $MEMORY, infinite_action => 'warn' },
    @{$Test_Grammar::MARPA_OPTIONS} );
$grammar->precompute();
close $MEMORY;

Marpa::Test::is( $trace, <<'EOS', 'cycle detection' );
Cycle found involving rule: 3: b -> a
Cycle found involving rule: 1: a -> b
EOS

my $recce = Marpa::Recognizer->new(
    {   grammar           => $grammar,
        trace_file_handle => *STDERR,
    }
);

$recce->tokens( [ [ 'a:k0', 'a' ] ] );

my %expected = %expected_original;
while ( my $value_ref = $recce->value() ) {
    my $value = ${$value_ref};
    if ( defined $expected{$value} ) {
        Test::More::pass(qq{Expected value: "$value"});
        delete $expected{$value};
    }
} ## end while ( my $value_ref = $recce->value() )

for my $missing_value ( keys %expected ) {
    Test::More::fail(qq{Missing value: "$missing_value"});
}

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
