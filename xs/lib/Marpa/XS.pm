package PPI::XS;

# Modeled on PPI/XS.pm by Adam Kennedy

use 5.010;
use strict;
use warnings;

use XSLoader;

# In the unlikely case they someone tries to manually load
# Marpa::XS without Marpa itself loaded, they probably MEAN for us
# to load in Marpa as well. Pretty useless otherwise, because we
# need to _overwrite_ the Marpa methods, we can't have it loading
# after we do.
use Marpa 0.013_003 ();

# Define compatibility information
BEGIN {
    our $VERSION = '0.013_003';
    our %EXCLUDE = ();
}

# Does the main package define the minimum set of variables?
return 1 unless defined $Marpa::VERSION;
return 1 if $Marpa::VERSION =~ /_/xms;

# Are we compatible with the main package
return 1 unless $VERSION == $Marpa::VERSION;

# Provide an option to manually disable this package
return 1 if $Marpa::XS_DISABLE;

# Load the XS functions
XSLoader::load( 'Marpa::XS' => $VERSION );

# Find all the functions in Marpa::XS
no strict 'refs';
XS_FUNCTION:
for my $xs_function (
    sort grep { /^_Marpa/xms and defined &{"Marpa::XS::$_"} }
    keys %{'Marpa::XS::'}
    )
{

    # Prepare
    next XS_FUNCTION
        if
        not( my ( $class, $function ) =
                ( $xs_function =~ / \A _(\w+?)__(\w+) \z /xms ) );
    $class =~ s/_/::/gxms;

    if ( $EXCLUDE{$xs_function} ) {

        # Remove the un-needed function.
        # The primary purpose of this is to recover the memory
        # occupied by the useless functions, but has the
        # additional benefit of allowing us to detect which
        # functions were actually mapped in by examining the
        # names of the functions remaining in the Marpa::XS symbol
        # table.
        delete ${'Marpa::XS::'}{$xs_function};
    } ## end if ( $EXCLUDE{$xs_function} )
    else {

        # Map in the function
        *{"${class}::${function}"} = *{"Marpa::XS::$xs_function"};
    }
} ## end for my $xs_function ( sort grep { /^_Marpa/xms and defined...})

1;

__END__
