package Marpa::Example::MDL_Displays;

use 5.010;
use strict;
use warnings;

## no critic (Subroutines::RequireArgUnpacking)

sub concatenate_lines {
    return ( scalar @_ ) ? ( join "\n", ( grep {$_} @_ ) ) : undef;
}

## use critic

sub do_whatever { return 1; }

1;
