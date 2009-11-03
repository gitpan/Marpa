package Marpa::MDL;

use 5.010;
use strict;
use warnings;
use Marpa;
use Marpa::MDLex;
use Marpa::MDL::Symbol;

## no critic (Variables::ProhibitPackageVars)
BEGIN {
    $Marpa::MDL::Self_Raw::raw_mdl_file = 'Marpa/MDL/self.mdl.raw';

    package Marpa::MDL::Self_Raw;
    require $Marpa::MDL::Self_Raw::raw_mdl_file;
} ## end BEGIN
## use critic

use Marpa::MDL::Internal::Actions;

sub to_raw {
    my ($mdl_source) = @_;

    ## no critic (Variables::ProhibitPackageVars)
    my $marpa_options = $Marpa::MDL::Self_Raw::data->{marpa_options};
    Carp::croak("No marpa_options in $Marpa::MDL::Self_Raw::raw_mdl_file")
        if not $marpa_options;

    my $mdlex_options = $Marpa::MDL::Self_Raw::data->{mdlex_options};
    Carp::croak("No marpa_options in $Marpa::MDL::Self_Raw::raw_mdl_file")
        if not $mdlex_options;
    ## use critic

    my $data = Marpa::MDLex::mdlex(
        [   { action_object => 'Marpa::MDL::Internal::Actions' },
            @{$marpa_options}
        ],
        $mdlex_options,
        $mdl_source
    );

    Carp::croak('mdlex returned undef') if not defined $data;

    return ${$data}->{marpa_options}, ${$data}->{mdlex_options}
        if wantarray;

    my $d = Data::Dumper->new( [ ${$data} ], [qw(data)] );
    $d->Sortkeys(1);
    $d->Purity(1);
    $d->Deepcopy(1);
    $d->Indent(1);
    return $d->Dump();
} ## end sub to_raw

1;

__END__

=head1 NAME

Marpa::MDL -- Marpa Demo Language

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

    $g->set( { start => Marpa::MDL::canonical_symbol_name('Document') } );

This static method takes as its one argument an MDL symbol
name.
It returns the canonical MDL name, which is also
the symbol's plumbing name.

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
