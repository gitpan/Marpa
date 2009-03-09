require 5.010;

use warnings;
use strict;

# It's all integers, except for the version number
use integer;

package Marpa::Internal::Source_Raw;

my $new_terminals = [];
my $new_rules = [];
my $new_preamble = "";
my $new_start_symbol;
my $new_semantics;
my $new_version;
my $new_default_action;
my $new_default_null_value;
my $new_default_lex_prefix;
my %strings;

