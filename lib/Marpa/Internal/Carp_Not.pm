package Marpa::Internal::Carp_Not;

use 5.010;
use strict;
use warnings;
use integer;

our @CARP_NOT = (__PACKAGE__);

sub import {
    my $calling_package = ( caller 0 );
    push @CARP_NOT, $calling_package;
    no strict 'refs';
    *{ $calling_package . q{::CARP_NOT} } =
        \@Marpa::Internal::Carp_Not::CARP_NOT;
    return 1;
} ## end sub import

1;
