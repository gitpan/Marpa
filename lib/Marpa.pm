package Marpa;

use 5.010;
use warnings;
use strict;

BEGIN {
    our $VERSION = '0.208000';
}

BEGIN {
    if ( defined $Marpa::XS::VERSION ) {
	Carp::croak( "You can only load one of the Marpa modules at a time\n",
	    'Version ', $Marpa::XS::VERSION, " of Marpa::XS is already loaded\n" );
    }
    if ( defined $Marpa::PP::VERSION ) {
	Carp::croak( "You can only load one of the Marpa modules at a time\n",
	    'Version ', $Marpa::PP::VERSION, " of Marpa::PP is already loaded\n" );
    }
}

use Scalar::Util ();
use List::Util   ();
use Carp         ();
use Data::Dumper ();
use Storable     ();

use Marpa::Internal::Carp_Not;
use Marpa::Internal;
use Marpa::Grammar;
use Marpa::Recognizer;
use Marpa::Evaluator;
use Marpa::Recce_Value;
use Marpa::Callback;

sub Marpa::compatible { return 1; }

1;
