package Marpa::Offset;

use 5.010;
use strict;
use warnings;
use integer;

sub import {
    my ($class, $struct_name, @fields) = @_;
    my $pkg = caller;
    my $prefix = $pkg . q{::} . $struct_name . q{::};
    my $offset = 0;

    ## no critic (TestingAndDebugging::ProhibitNoStrict)
    no strict 'refs';
    ## use critic
    for my $field (@fields) {
       # offset must be copy, because contents of sub must be constant
       my $field_name = $prefix . $field;
       *{$field_name} = sub () { $offset };
       $offset++;
    }
    return 1;
}

1;
