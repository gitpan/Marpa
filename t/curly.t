#!perl

use 5.010;
use warnings;
use strict;

use English qw( -no_match_vars );

use Getopt::Long ();
use Test::More ( import => [] );

BEGIN {
    my $PPI_problem;
    CHECK_PPI: {
        if ( not eval { require PPI } ) {
            $PPI_problem = 'PPI not installed';
            last CHECK_PPI;
        }
        if ( not PPI->VERSION(1.206) ) {
            $PPI_problem = 'PPI 1.206 not installed';
        }
    } ## end CHECK_PPI:
    if ($PPI_problem) {
        Test::More::plan skip_all => $PPI_problem;
    }
    else {
        Test::More::plan tests => 5;
    }
    Test::More::use_ok('Marpa');
} ## end BEGIN

use Marpa::Perl ();
use Marpa::Test ();

# Run in utility mode?
my $utility = 0;
die if not Getopt::Long::GetOptions( utility => \$utility );

my %closure = (
    'anon_hash' => \&do_hashblock,
    'block'     => \&do_codeblock,
    'mblock'    => \&do_codeblock,
);

sub gen_closure {
    my ( $lhs, $rhs, $action ) = @_;
    my $closure = $closure{$action};
    return ( undef, $closure );
}

my %hash;
my %codeblock;

sub do_hashblock {
    shift;
    my $location = 'line '
        . Marpa::Perl::token()->logical_line_number()
        . q{, column }
        . Marpa::Perl::token()->column_number;
    $hash{$location}++;
    return;
} ## end sub do_hashblock

sub do_codeblock {
    shift;
    my $location = 'line '
        . Marpa::Perl::token()->logical_line_number()
        . q{, column }
        . Marpa::Perl::token()->column_number;
    $codeblock{$location}++;
    return;
} ## end sub do_codeblock

my $parser = Marpa::Perl->new( \&gen_closure );

my @tests;
if ($utility) {
    push @tests, do { local $RS = undef; <STDIN> };
}
else {
    @tests = (
        [   '{42;{1,2,3;4}}', << 'END_OF_RESULT'
Number of values: 0
Code block at line 1, column 1
Code block at line 1, column 5
END_OF_RESULT
        ],
        [   '{42;{1,2,3,4}}', << 'END_OF_RESULT'
Number of values: 0
Hash at line 1, column 5
Code block at line 1, column 1
Code block at line 1, column 5
END_OF_RESULT
        ],
        [   '{42;{;1,2,3;4}}', << 'END_OF_RESULT'
Number of values: 0
Code block at line 1, column 1
Code block at line 1, column 5
END_OF_RESULT
        ],
        [   '{42;+{1,2,3,4}}', << 'END_OF_RESULT'
Number of values: 0
Hash at line 1, column 6
Code block at line 1, column 1
END_OF_RESULT
        ],
    );
} ## end else [ if ($utility) ]

TEST: for my $test (@tests) {
    my ( $string, $expected ) = @{$test};
    my @values = $parser->parse( \$string );
    my $result = 'Number of values: ' . scalar @values . "\n";
    for my $location ( sort keys %hash ) {
        $result .= "Hash at $location\n";
    }
    for my $location ( sort keys %codeblock ) {
        $result .= "Code block at $location\n";
    }
    Marpa::Test::is( $result, $expected, qq{Test of "$string"} );
    %hash      = ();
    %codeblock = ();
} ## end for my $test (@tests)

