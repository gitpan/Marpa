package Marpa::Internal;

use 5.010;
use strict;
use warnings;
use integer;

use Marpa::Internal::Carp_Not;

*Marpa::exception = \&Carp::croak;

## no critic (Subroutines::RequireArgUnpacking)
sub Marpa::internal_error {
    Carp::confess( "Internal Marpa Error: This could be a bug in Marpa\n",
        @_ );
}
## use critic

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
