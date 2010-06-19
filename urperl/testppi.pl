#!perl

## no critic (ErrorHandling::RequireCarping);

use 5.010;
use strict;
use warnings;
use Data::Dumper;
use PPI;
use English qw( -no_match_vars );    # Avoids regex performance penalty

local $RS = undef;
my $i = <ARGV>;
my $t = PPI::Tokenizer->new( \$i );
say Data::Dumper::Dumper( $t->all_tokens() ) or die "Cannot print: $ERRNO";
