#!perl

use 5.010;
use warnings;
use strict;

use Test::More tests => 2;
use lib 'lib';

BEGIN {
    Test::More::use_ok('Marpa::MDL');
}

Test::More::ok("Testing Marpa $Marpa::VERSION, Perl $], $^X");