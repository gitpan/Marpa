package Marpa::MDL;

use 5.010;
use strict;
use warnings;

Carp::croak( "Marpa not loaded\n", "Marpa::MDL requires it\n" )
    if not defined $Marpa::VERSION;

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
        [   { action_object => 'Marpa::MDL::Internal::Actions', },
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
