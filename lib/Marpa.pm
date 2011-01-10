package Marpa;

use 5.010;
use warnings;
use strict;

BEGIN {
    our $VERSION = '0.202000';
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
