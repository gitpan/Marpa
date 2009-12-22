package Marpa;

use 5.010;
use warnings;
use strict;

BEGIN {
    our $VERSION = '0.001_038';
}

use Marpa::Internal;
use Marpa::Grammar;
use Marpa::Recognizer;
use Marpa::Evaluator;
use Marpa::Recce_Value;

sub import {
    goto &Marpa::VERSION;
}

package Marpa::Internal;

sub version_error {
    my $message = <<'END_OF_MESSAGE';
====================================
Marpa's "use" statement semantics is non-standard,
  at least while it remains alpha.
The "use Marpa" statement must have an argument.
You have two choices:

  use Marpa <<VERSION>>;

means match this version and THIS VERSION ONLY.

  use Marpa 'alpha';

means you are WILLING TO DEAL WITH INTERFACE CHANGES
  and will accept whatever version is installed.
I apologize for the non-standard behavior,
but I hope it follows the principle of least surprise,
because while Marpa remains alpha,
THE INTERFACE CAN CHANGE even between minor versions.
====================================
END_OF_MESSAGE
    $message =~ s/<<VERSION>>/$Marpa::VERSION/xms;
    Marpa::exception($message);
} ## end sub version_error

sub Marpa::VERSION {
    my ( $class, $require ) = @_;
    given ($require) {
        when (undef) { version_error() }
        when ( lc $_ eq 'alpha' ) { return $Marpa::VERSION }
        when (/^[0-9]/xms) {
            return $Marpa::VERSION if $Marpa::VERSION eq $_;
            Marpa::exception(
                "Marpa is still alpha\n",
                "  Versions must match *EXACTLY*\n",
                "  You asked for $_\n",
                "  Actual version is $Marpa::VERSION\n"
                )
        } ## end when (/^[0-9]/xms)
    } ## end given
    return version_error();    # return is to fool perlcritic
} ## end sub Marpa::VERSION

1;
