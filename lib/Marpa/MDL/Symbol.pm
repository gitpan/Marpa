package Marpa::MDL::Internal::Symbol;

# These two routines are here.
# This allows them to be loaded, while avoiding
# loading all of MDL, which has a measurable
# overhead and substantial namespace impact.

use 5.010;
use strict;
use warnings;

sub Marpa::MDL::canonical_symbol_name {
    my $symbol = lc shift;
    $symbol =~ s/[-_\s]+/-/gxms;
    return $symbol;
}

sub Marpa::MDL::get_terminal {
    my ( $grammar, $symbol_name ) = @_;
    return Marpa::MDL::canonical_symbol_name($symbol_name);
}

1;
