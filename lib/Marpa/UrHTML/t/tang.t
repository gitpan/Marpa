#!/usr/bin/perl

use 5.010;
use strict;
use warnings;

use lib 'lib';
use Test::More;
use Marpa::Test;

BEGIN {
    if ( eval { require HTML::PullParser } ) {
        Test::More::plan tests => 3;
    }
    else {
        Test::More::plan skip_all => 'HTML::PullParser not available';
    }
    Test::More::use_ok('Marpa');
    Test::More::use_ok('Marpa::UrHTML');
} ## end BEGIN

use Carp;
use Data::Dumper;
use English qw( -no_match_vars );
use Fatal qw(open close);

my $document;
{
    local $RS = undef;
    open my $fh, q{<:utf8}, 'lib/Marpa/UrHTML/t/test.html';
    $document = <$fh>;
    close $fh;
};

my $no_tang_document;
{
    local $RS = undef;
    open my $fh, q{<:utf8}, 'lib/Marpa/UrHTML/t/no_tang.html';
    $no_tang_document = <$fh>;
    close $fh;
};

my $p = Marpa::UrHTML->new(
    { handlers => [ [ '.ktang' => sub { return q{}; } ] ] } );
my $value = $p->parse( \$document );

Marpa::Test::is( ${$value}, $no_tang_document, 'remove kTang class' );
