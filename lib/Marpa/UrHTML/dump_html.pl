#!/usr/bin/perl

use 5.010;
use strict;
use warnings;

use Carp;
use Data::Dumper;
use English qw( -no_match_vars ) ;

use Marpa::UrHTML;

my $document = do { local $RS = undef; <STDIN> };

my $p = Marpa::UrHTML->new(\$document);

my $marpa_tokens = $p->evaluate();

# say Data::Dumper::Dumper($marpa_tokens);

# say join "\n", map { $_->[0] } @{$marpa_tokens};
