package Marpa::Internal;

use 5.010;
use strict;
use warnings;
use integer;
use Carp;

our @CARP_NOT = (__PACKAGE__);

sub import {
    my $calling_package = ( caller 0 );
    push @CARP_NOT, $calling_package;
    no strict 'refs';
    *{ $calling_package . q{::CARP_NOT} } = \@Marpa::Internal::CARP_NOT;
    return 1;
} ## end sub import

*Marpa::exception = \&Carp::croak;

# Perl critic at present is not smart about underscores
# in hex numbers
## no critic (ValuesAndExpressions::RequireNumberSeparators)
use constant N_FORMAT_MASK     => 0xffff_ffff;
use constant N_FORMAT_HIGH_BIT => 0x8000_0000;
## use critic

# Also used as mask, so must be 2**n-1
# Perl critic at present is not smart about underscores
# in hex numbers
use constant N_FORMAT_MAX => 0x7fff_ffff;

1;
