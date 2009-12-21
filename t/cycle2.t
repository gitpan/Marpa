#!perl

# A grammars with cycles
use 5.010;
use strict;
use warnings;
use lib 'lib';
use English qw( -no_match_vars );
use Fatal qw(open close chdir);
use Test::More tests => 7;
use Marpa::Test;

BEGIN {
    Test::More::use_ok( 'Marpa', 'alpha' );
    Test::More::use_ok('Marpa::MDLex');
}

my @expected_values = split /\n/xms, <<'EOS';
A(B(a))
a
EOS

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
        'start'        => 's',
        'terminals'    => ['a:k0'],
        'cycle_action' => 'warn'
    }
];

$Test_Grammar::MDLEX_OPTIONS = [
    {   'terminals' => [
            {   'name'  => 'a:k0',
                'regex' => 'a'
            }
        ]
    }
];

my $trace;
open my $MEMORY, '>', \$trace;
my $grammar = Marpa::Grammar->new(
    { trace_file_handle => $MEMORY, cycle_action => 'warn' },
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

my $lexer =
    Marpa::MDLex->new( { recce => $recce }, @{$Test_Grammar::MDLEX_OPTIONS} );
my $text          = 'a';
my $fail_location = $lexer->text( \$text );
if ( $fail_location >= 0 ) {
    Marpa::exception(
        Marpa::show_location( 'Parsing failed', \$text, $fail_location ) );
}
$recce->end_input();

my $evaler = Marpa::Evaluator->new( { recce => $recce, cycle_rewrite => 0 } );
my $parse_count = 0;
while ( my $value = $evaler->value() ) {
    Marpa::Test::is(
        ${$value},
        $expected_values[$parse_count],
        "cycle depth test $parse_count"
    );
    $parse_count++;
} ## end while ( my $value = $evaler->value() )

$evaler = Marpa::Evaluator->new( { recce => $recce, cycle_rewrite => 1 } );
$parse_count = 0;
while ( my $value = $evaler->value() ) {
    Marpa::Test::is(
        ${$value},
        $expected_values[$parse_count],
        "cycle depth test $parse_count"
    );
    $parse_count++;
} ## end while ( my $value = $evaler->value() )

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
