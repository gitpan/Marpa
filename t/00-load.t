#!perl

use 5.010;
use warnings;
use strict;

use Test::More tests => 1;
use lib 'lib';

BEGIN {
    Test::More::use_ok('Marpa');
}

defined $INC{'Marpa.pm'} or Test::More::BAIL_OUT('Could not load Marpa');
