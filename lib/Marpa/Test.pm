package Marpa::Test;
use 5.010;
use strict;
use warnings;
use Marpa::Internal;

Marpa::exception('Test::More not loaded')
    if not defined &Test::More::is;

BEGIN {
    ## no critic (BuiltinFunctions::ProhibitStringyEval)
    ## no critic (ErrorHandling::RequireCheckingReturnValueOfEval)
    eval 'use Test::Differences';
    ## use critic
} ## end BEGIN
use Data::Dumper;

## no critic (Subroutines::RequireArgUnpacking)
sub is {
## use critic
    goto &Test::Differences::eq_or_diff
        if defined &Test::Differences::eq_or_diff && @_ > 1;
    @_ = map { ref $_ ? Data::Dumper::Dumper(@_) : $_ } @_;
    goto &Test::More::is;
} ## end sub is

1;

