package Marpa::MDL;

use 5.010;
use strict;
use warnings;

sub gen_symbol_from_regex {
    my $regex = shift;
    my $data  = shift;
    if ( scalar @{$data} == 0 ) {
        my $number = 0;
        push @{$data}, {}, \$number;
    }
    my ( $regex_hash, $uniq_number ) = @{$data};
    given ($regex) {
        when (/^qr/xms) { $regex = substr $regex, 3, -1; }
        default      { $regex = substr $regex, 1, -1; };
    }
    my $symbol = $regex_hash->{$regex};
    return $symbol if defined $symbol;
    $symbol = substr $regex, 0, 20;
    $symbol =~ s/%/%%/gxms;
    $symbol =~ s/([^[:alnum:]_-])/sprintf("%%%.2x", ord($1))/gexms;
    $symbol .= sprintf ':k%x', (${$uniq_number})++;
    $regex_hash->{$regex} = $symbol;
    return ( $symbol, 1 );
}

sub canonical_symbol_name {
    my $symbol = lc shift;
    $symbol =~ s/[-_\s]+/-/gxms;
    return $symbol;
}

sub get_symbol {
    my $grammar     = shift;
    my $symbol_name = shift;
    return Marpa::Grammar::get_symbol( $grammar,
        canonical_symbol_name($symbol_name) );
}

1;

__END__

=head1 NAME

Marpa::MDL -- Utility Methods for MDL

=head1 DESCRIPTION

If you are looking for the document that describes the Marpa Demonstration
Language, this is not it -- you want L<Marpa::Doc::MDL>.
This document describes some utility routines that come with
C<Marpa::MDL>.

These routines handle the conversion of MDL symbol names
to plumbing symbol names.
MDL symbol names behave differently from the plumbing names.
MDL symbol names are allowed to vary in capitalization and separation while,
in the plumbing,
every slight variation of capitalization or separation produces a new,
unique name.

MDL symbol names have a canonical form.
MDL uses the canonical form of its symbol names as
their plumbing names.
B<Canonical MDL names> are all lowercase,
with hyphens for separation.
For example,
the MDL symbol whose acceptable variants include
C<My symbol> and C<MY_SYMBOL> is, in canonical form, C<my-symbol>.
Users should always use 
the C<Marpa::MDL::canonical_name> method to convert from the
MDL symbol name to its canonical form.

=head1 METHODS

=head2 canonical_name

=begin Marpa::Test::Display:

## next display
in_file($_, 'author.t/misc.t');

=end Marpa::Test::Display:

    $g->set( {
        start => Marpa::MDL::canonical_symbol_name('Document')
    } );

This static method takes as its one argument an MDL symbol
name.
It returns the canonical MDL name, which is also
the symbol's plumbing name.

=head2 get_symbol

=begin Marpa::Test::Display:

## next display
is_file($_, 'author.t/misc.t', 'get_symbol snippet');

=end Marpa::Test::Display:

    my $op = Marpa::MDL::get_symbol( $grammar, 'Op' );

This static method takes a Marpa grammar object as its first argument and an MDL symbol name as its second.
It returns the symbol's "cookie".
Symbol cookies are needed to use the C<Marpa::Recognizer::earleme> method.

=head1 SUPPORT

See the L<support section|Marpa/SUPPORT> in the main module.

=head1 AUTHOR

Jeffrey Kegler

=head1 LICENSE AND COPYRIGHT

Copyright 2007 - 2009 Jeffrey Kegler

This program is free software; you can redistribute
it and/or modify it under the same terms as Perl 5.10.0.

=cut

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
