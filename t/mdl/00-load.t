#!perl

use 5.010;
use warnings;
use strict;

use Test::More tests => 1;
use lib 'lib';

BEGIN {
    Test::More::use_ok('Marpa::MDL');
}
