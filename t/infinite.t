#!perl
# A grammar with cycles

use 5.010;
use strict;
use warnings;
use English qw( -no_match_vars );
use Fatal qw(open close chdir);

use Test::More tests => 13;
use lib 'lib';
use Marpa::Test;

BEGIN {
    Test::More::use_ok('Marpa::MDLex');
}

## no critic (Subroutines::RequireArgUnpacking)
sub default_action {
    shift;
    return join q{ }, grep { defined $_ } @_;
}
## use critic

package Test_Grammar;

# Formatted by Data::Dumper, which disagrees with
# perltidy and perlcritic about things
#<<< no perltidy
##no critic (ValuesAndExpressions::ProhibitNoisyQuotes)

$Test_Grammar::MARPA_OPTIONS_1 = [
    {   'default_action' => 'main::default_action',
        'rules'          => [
            {   'lhs' => 's',
                'rhs' => [ 's' ]
            }
        ],
        'start'     => 's',
        'terminals' => [ 's' ],
    }
];

$Test_Grammar::MDLEX_OPTIONS_1 = [
    {   'terminals' => [
            {   'name'  => 's',
                'regex' => '.'
            }
        ]
    }
];

$Test_Grammar::MARPA_OPTIONS_2 = [
    {   'default_action' => 'main::default_action',
        'rules'          => [
            {   'lhs' => 's',
                'rhs' => [ 'a' ]
            },
            {   'lhs' => 'a',
                'rhs' => [ 's' ]
            }
        ],
        'start'     => 's',
        'terminals' => [ 'a' ],
    }
];

$Test_Grammar::MDLEX_OPTIONS_2 = [
    {   'terminals' => [
            {   'name'  => 'a',
                'regex' => '.'
            }
        ]
    }
];

$Test_Grammar::MARPA_OPTIONS_8 = [
    {   'default_action' => 'main::default_action',
        'rules'          => [
            {   'lhs' => 's',
                'rhs' => [ 'a' ]
            },
            {   'lhs' => 'a',
                'rhs' => [ 'b', 't', 'u' ]
            },
            {   'lhs' => 'b',
                'rhs' => [ 'v', 'c' ]
            },
            {   'lhs' => 'c',
                'rhs' => [ 'w', 'd', 'x' ]
            },
            {   'lhs' => 'd',
                'rhs' => [ 'e' ]
            },
            {   'lhs' => 'e',
                'rhs' => [ 's' ]
            },
            {   'lhs' => 't',
                'rhs' => []
            },
            {   'lhs' => 'u',
                'rhs' => []
            },
            {   'lhs' => 'v',
                'rhs' => []
            },
            {   'lhs' => 'w',
                'rhs' => []
            },
            {   'lhs' => 'x',
                'rhs' => []
            }
        ],
        'start'     => 's',
        'terminals' => [ 'e', 't', 'u', 'v', 'w', 'x' ],
    }
];

$Test_Grammar::MDLEX_OPTIONS_8 = [
    {   'terminals' => [
            {   'name'  => 'e',
                'regex' => '.'
            },
            {   'name'  => 't',
                'regex' => '.'
            },
            {   'name'  => 'u',
                'regex' => '.'
            },
            {   'name'  => 'v',
                'regex' => '.'
            },
            {   'name'  => 'w',
                'regex' => '.'
            },
            {   'name'  => 'x',
                'regex' => '.'
            }
        ]
    }
];

#>>>
## use critic

package main;

my $cycle1_test = [
    'cycle1',
    $Test_Grammar::MARPA_OPTIONS_1,
    $Test_Grammar::MDLEX_OPTIONS_1,
    \('1'),
    '1',
    <<'EOS'
Cycle found involving rule: 0: s -> s
EOS
];

my $cycle2_test = [
    'cycle2',
    $Test_Grammar::MARPA_OPTIONS_2,
    $Test_Grammar::MDLEX_OPTIONS_2,
    \('1'),
    '1',
    <<'EOS'
Cycle found involving rule: 1: a -> s
Cycle found involving rule: 0: s -> a
EOS
];

my $cycle8_test = [
    'cycle8',
    $Test_Grammar::MARPA_OPTIONS_8,
    $Test_Grammar::MDLEX_OPTIONS_8,
    \('123456'),
    '1 2 3 4 5 6',
    <<'EOS'
Cycle found involving rule: 3: c -> w d x
Cycle found involving rule: 2: b -> v c
Cycle found involving rule: 1: a -> b t u
Cycle found involving rule: 5: e -> s
Cycle found involving rule: 4: d -> e
Cycle found involving rule: 0: s -> a
EOS
];

my @test_data;
for my $base_test ( $cycle1_test, $cycle2_test, $cycle8_test ) {
    my $test_name = $base_test->[0];
    push @test_data,
        [
        "$test_name infinite_rewrite",
        @{$base_test}[ 1 .. $#{$base_test} ],
        { infinite_rewrite => 0 }
        ];
    push @test_data,
        [
        "$test_name no infinite_rewrite",
        @{$base_test}[ 1 .. $#{$base_test} ],
        { infinite_rewrite => 1 }
        ];
} ## end for my $base_test ( $cycle1_test, $cycle2_test, $cycle8_test)

for my $test_data (@test_data) {
    my ( $test_name, $marpa_options, $mdlex_options, $input, $expected,
        $expected_trace, $evaler_options )
        = @{$test_data};
    my $trace = q{};
    open my $MEMORY, '>', \$trace;
    my $grammar = Marpa::Grammar->new(
        {   infinite_action   => 'warn',
            trace_file_handle => $MEMORY,
        },
        @{$marpa_options},
    );
    $grammar->precompute();

    my $recce = Marpa::Recognizer->new( { grammar => $grammar } );
    my $lexer = Marpa::MDLex->new( { recce => $recce }, @{$mdlex_options} );
    my $fail_offset = $lexer->text($input);
    my $result;
    given ($fail_offset) {
        when ( $_ < 0 ) {
            $recce->end_input();
            my $evaler =
                Marpa::Evaluator->new( { recce => $recce, },
                $evaler_options );
            $result = $evaler->value();
        } ## end when ( $_ < 0 )
        default {
            $result = \"Parse failed at offset $fail_offset";
        }
    };

    close $MEMORY;

    Marpa::Test::is( ${$result}, $expected,       "$test_name result" );
    Marpa::Test::is( $trace,     $expected_trace, "$test_name trace" );

} ## end for my $test_data (@test_data)

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
