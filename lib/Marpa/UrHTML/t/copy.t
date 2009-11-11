#!/usr/bin/perl

use 5.010;
use strict;
use warnings;

use Test::More tests => 2;
use lib 'lib';

BEGIN {
    Test::More::use_ok('Marpa');
}

use Carp;
use Data::Dumper;
use English qw( -no_match_vars );
use Fatal qw(open close);

use Marpa::UrHTML;

my $document;
{
    local $RS = undef;
    open my $fh, q{<:utf8}, 'lib/Marpa/UrHTML/t/test.html';
    $document = <$fh>;
    close $fh
};

my $p     = Marpa::UrHTML->new();
my $value = $p->parse( \$document );

Test::More::is( ${ ${$value} }, $document, 'Straight copy using defaults' );
