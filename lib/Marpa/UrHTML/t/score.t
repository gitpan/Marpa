#!/usr/bin/perl

use 5.010;
use strict;
use warnings;

use English qw( -no_match_vars );
use Fatal qw(open close);
use File::Spec;

use lib 'lib';
use Test::More;
use Marpa::Test;

BEGIN {
    if ( eval { require HTML::PullParser } ) {
        Test::More::plan tests => 5;
    }
    else {
        Test::More::plan skip_all => 'HTML::PullParser not available';
    }
    Test::More::use_ok('Marpa::Test::Util');
} ## end BEGIN

my @eg_dir   = qw( lib Marpa UrHTML eg );
my @data_dir = qw( lib Marpa UrHTML t fmt_t_data );

for my $test (qw(1 2)) {
    my $expected;
    my $output = Marpa::Test::Util::run_command(
        File::Spec->catfile( @eg_dir, 'urhtml_score' ),
        File::Spec->catfile( @data_dir, ( 'input' . $test . '.html' ) ) );
    local $RS = undef;
    open my $fh, q{<},
        File::Spec->catfile( @data_dir,
        ( 'score_expected' . $test . '.html' ) );
    $expected = <$fh>;
    close $fh;
    Marpa::Test::is( $output, $expected, 'urhtml_score test' );
} ## end for my $test (qw(1 2))


