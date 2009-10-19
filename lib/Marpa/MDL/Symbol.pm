package Marpa::MDL::Symbol;

# These two routines are here, so they can be loaded without
# the overname and namespace impact from loading all of MDL.

use 5.010;
use strict;
use warnings;
use Marpa;

sub Marpa::MDL::canonical_symbol_name {
    my $symbol = lc shift;
    $symbol =~ s/[-_\s]+/-/gxms;
    return $symbol;
}

sub Marpa::MDL::get_terminal {
    my ( $grammar, $symbol_name ) = @_;
    return Marpa::Grammar::get_terminal( $grammar,
        Marpa::MDL::canonical_symbol_name($symbol_name) );
}

1;
