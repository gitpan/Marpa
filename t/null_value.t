#!perl

use 5.010;
use strict;
use warnings;
use lib 'lib';

use Test::More tests => 2;
use Marpa::Test;

BEGIN {
    Test::More::use_ok('Marpa');
}

package Test;

# The start rule

sub new { my $class = shift; return bless {}, $class }

## no critic (Subroutines::RequireArgUnpacking)
sub rule0 {
    return $_[1] . ', but ' . $_[2];
}
## use critic

sub rule1 { return 'A is missing' }
sub rule2 { return q{I'm sometimes null and sometimes not} }
sub rule3 { return 'B is missing' }
sub rule4 { return 'C is missing' }
sub rule5 { return 'C matches Y' }
sub rule6 { return 'Zorro was here' }

package Test_Grammar;

$Test_Grammar::MARPA_OPTIONS = [
    {   'rules' => [
            {   'action' => 'rule0',
                'lhs'    => 's',
                'rhs'    => [ 'a', 'y' ]
            },
            {   'lhs' => 'a',
                'rhs' => []
            },
            {   'action' => 'rule2',
                'lhs'    => 'a',
                'rhs'    => [ 'b', 'c' ]
            },
            {   'lhs' => 'b',
                'rhs' => []
            },
            {   'lhs' => 'c',
                'rhs' => []
            },
            {   'action' => 'rule5',
                'lhs'    => 'c',
                'rhs'    => ['y']
            },
            {   'action' => 'rule6',
                'lhs'    => 'y',
                'rhs'    => ['Z']
            }
        ],
        'start' => 's',
        symbols => {
            a => { null_value => 'A is missing' },
            b => { null_value => 'B is missing' },
            c => { null_value => 'C is missing' },
        },
        'terminals'     => ['Z'],
        'action_object' => 'Test'
    }
];

package main;

my $g = Marpa::Grammar->new( @{$Test_Grammar::MARPA_OPTIONS} );
$g->precompute();
my $recce = Marpa::Recognizer->new( { grammar => $g } );
$recce->tokens( [ [ 'Z', 'Z' ] ] );
my $ref_value = $recce->value();
my $value = $ref_value ? ${$ref_value} : 'No parse';
Marpa::Test::is(
    $value,
    'A is missing, but Zorro was here',
    'null value example'
);

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
