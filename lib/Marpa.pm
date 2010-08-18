package Marpa;

use 5.010;
use warnings;
use strict;

BEGIN {
    our $VERSION = '0.105_008';
}

use Marpa::Internal;
use Marpa::Grammar;
use Marpa::Recognizer;
use Marpa::Evaluator;
use Marpa::Recce_Value;

sub Marpa::compatible { return 1; }

1;
