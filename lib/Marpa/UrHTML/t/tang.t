#!/usr/bin/perl

use 5.010;
use strict;
use warnings;

use Test::More tests => 2;
use lib 'lib';
use t::lib::Marpa::Test;
use Fatal qw(open close);

BEGIN {
    Test::More::use_ok('Marpa');
}

use Carp;
use Data::Dumper;
use English qw( -no_match_vars );
use Fatal qw(open);

use Marpa::UrHTML;

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
    { handlers => [ [ '.kTang' => sub { return q{} } ] ] } );
my $value = $p->parse( \$document );

Marpa::Test::is( ${ ${$value} }, $no_tang_document, 'remove kTang class' );
