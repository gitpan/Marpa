#!perl

use 5.010;
use warnings;
use strict;

use Test::More tests => 5;
use lib 'lib';

BEGIN {
    Test::More::use_ok('Devel::SawAmpersand');
    Test::More::use_ok('Marpa');
    Test::More::use_ok('Marpa::Perl');
    Test::More::use_ok('Marpa::Test');
} ## end BEGIN

Test::More::ok( !Devel::SawAmpersand::sawampersand(), 'PL_sawampersand set' );
