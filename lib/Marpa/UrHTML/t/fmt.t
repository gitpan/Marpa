#!/usr/bin/perl

use 5.010;
use strict;
use warnings;

use English qw( -no_match_vars );
use Fatal qw(open close);
use File::Spec;

use lib 'lib';
use Test::More tests => 7;
Test::More::use_ok('Marpa::Test');
Test::More::use_ok('Marpa::Test::Util');
Test::More::use_ok('HTML::PullParser');

my @script_dir = qw( lib Marpa UrHTML script );
my @data_dir   = qw( lib Marpa UrHTML t fmt_t_data );

for my $test (qw(1 2)) {
    my $expected;
    my $output = Marpa::Test::Util::run_command(
        File::Spec->catfile( @script_dir, 'urhtml_fmt' ),
        File::Spec->catfile( @data_dir, ( 'input' . $test . '.html' ) ) );
    local $RS = undef;
    open my $fh, q{<},
        File::Spec->catfile( @data_dir, ( 'expected' . $test . '.html' ) );
    $expected = <$fh>;
    close $fh;
    Marpa::Test::is( $output, $expected, 'urhtml_fmt test' );
} ## end for my $test (qw(1 2))

