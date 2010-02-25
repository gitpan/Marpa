#!/usr/bin/perl

use 5.010;
use strict;
use warnings;

use lib 'lib';
use Test::More tests => 4;

Test::More::use_ok('HTML::PullParser');
Test::More::use_ok('Marpa');
Test::More::use_ok('Marpa::UrHTML');

use Carp;
use Data::Dumper;
use English qw( -no_match_vars );
use Fatal qw(open close);

my $document;
{
    local $RS = undef;
    open my $fh, q{<:utf8}, 'lib/Marpa/UrHTML/t/test.html';
    $document = <$fh>;
    close $fh
};

my $value = Marpa::UrHTML::urhtml( \$document );

Test::More::is( ${$value}, $document, 'Straight copy using defaults' );
