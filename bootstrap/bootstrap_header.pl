#!perl
# This is the beginning of bootstrap_header.pl

## no critic (ValuesAndExpressions::ProhibitImplicitNewlines)
## no critic (RegularExpressions::RequireExtendedFormatting)
## no critic (RegularExpressions::RequireLineBoundaryMatching)
## no critic (RegularExpressions::RequireDotMatchAnything)

use 5.010;
use strict;
use warnings;
use Marpa;
use Marpa::MDL;
use Carp;
use Fatal qw(open close);
use English qw( -no_match_vars ) ;

my $new_terminals = [];
my $new_rules = [];
my $new_preamble;
my $new_lex_preamble;
my $new_start_symbol;
my $new_semantics;
my $new_version;
my $new_default_action;
my $new_default_null_value;
my $new_default_lex_prefix;
my %strings;

sub usage {
   croak("usage: $0 grammar-file\n");
}

my $argc = @ARGV;
usage() if $argc < 1 or $argc > 3;

my $grammar_file_name = shift @ARGV;
my $header_file_name = shift @ARGV;
my $trailer_file_name = shift @ARGV;

# This is the end of bootstrap_header.pl
