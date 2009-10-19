#!perl

use 5.010;
use strict;
use warnings;
use lib 'lib';

use Test::More tests => 2;
use t::lib::Marpa::Test;

BEGIN { Test::More::use_ok('Marpa::MDLex'); }

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
            {   'action' => 'rule1',
                'lhs'    => 'a',
                'rhs'    => []
            },
            {   'action' => 'rule2',
                'lhs'    => 'a',
                'rhs'    => [ 'b', 'c' ]
            },
            {   'action' => 'rule3',
                'lhs'    => 'b',
                'rhs'    => []
            },
            {   'action' => 'rule4',
                'lhs'    => 'c',
                'rhs'    => []
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
        'start'         => 's',
        'terminals'     => ['Z'],
        'action_object' => 'Test'
    }
];

$Test_Grammar::MDLEX_OPTIONS = [
    {   'terminals' => [
            {   'name'  => 'Z',
                'regex' => 'Z'
            }
        ]
    }
];

package main;

my $g     = Marpa::Grammar->new( @{$Test_Grammar::MARPA_OPTIONS} );
my $cap_z = $g->get_terminal('Z');
$g->precompute();
my $recce = Marpa::Recognizer->new( { grammar => $g } );
my $lexer =
    Marpa::MDLex->new( { recce => $recce }, @{$Test_Grammar::MDLEX_OPTIONS} );

my $fail_offset = $lexer->text('Z');
if ( $fail_offset >= 0 ) {
    Carp::croak("Parse failed at offset $fail_offset");
}

$recce->end_input();
my $evaler = Marpa::Evaluator->new( { recce => $recce } );
Marpa::exception('No parse found') if not $evaler;
my $value = $evaler->value();
Marpa::exception('No evaluation found') if not $value;
Marpa::Test::is(
    ${$value},
    'A is missing, but Zorro was here',
    'null value example'
);

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
