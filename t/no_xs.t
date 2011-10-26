#!perl

use 5.010;
use warnings;
use strict;

use Test::More tests => 1;
use English qw( -no_match_vars );
use lib 'lib';

my $loaded_marpa_xs = eval { require Marpa::XS; 1 };
my $loaded_marpa;
SKIP: {
    skip 'No Marpa::XS, which is OK', 1 unless $loaded_marpa_xs;
    my $loaded_marpa = eval { require Marpa; 1 };
    Test::More::ok(!$loaded_marpa, 'Marpa::XS incompatible');
}

