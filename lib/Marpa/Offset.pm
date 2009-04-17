package Marpa::Offset;

use 5.010;
use strict;
use warnings;
use integer;

sub import {
    my ( $class, $struct_name, @fields ) = @_;
    my $pkg    = caller;
    my $prefix = $pkg . q{::} . $struct_name . q{::};
    my $offset = -1;

    ## no critic (TestingAndDebugging::ProhibitNoStrict)
    no strict 'refs';
    ## use critic
    for my $field (@fields) {

        $offset++ unless $field =~ s/\A=//xms;
        Marpa::exception("Unacceptable field name: $field")
            if $field =~ /[^A-Z0-9_]/xms;
        my $field_name = $prefix . $field;
        *{$field_name} = sub () {$offset};
    } ## end for my $field (@fields)
    return 1;
} ## end sub import

1;
