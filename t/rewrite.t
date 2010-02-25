#!/usr/bin/perl

# Rewriting tests, to check the accuracy of the
# tracing documentation.

use 5.010;
use strict;
use warnings;

use Fatal qw(open close);
use Test::More tests => 3;

use lib 'lib';
use Marpa::Test;

BEGIN {
    Test::More::use_ok('Marpa');
}

my $chaf_rule =
#<<< no perltidy
# Marpa::Display
# name: CHAF Rule

{   lhs => 'statement',
    rhs => [
        qw/optional_whitespace expression
            optional_whitespace optional_modifier
            optional_whitespace/
    ]
}

# Marpa::Display::End
; # semicolon to terminate rule

#>>> no perltidy

my $separated_sequence_rule =
#<<< no perltidy
# Marpa::Display
# name: Separated Sequence Rule

{
    lhs       => 'statements',
    rhs       => [qw/statement/],
    separator => 'comma',
    min       => 1
}

# Marpa::Display::End
; # semicolon to terminate rule

#>>> no perltidy

my $sequence_rule =
#<<< no perltidy
# Marpa::Display
# name: Sequence Rule

    { lhs => 'block', rhs => [qw/statements/], min => 0 },

# Marpa::Display::End
; # semicolon to terminate rule

#>>> no perltidy

my $grammar = Marpa::Grammar->new(
    {   start   => 'block',
        strip   => 0,
        symbols => {
            'block' => {
                terminal   => 1,
                null_value => 'Null parse'
            }
        },
        terminals => [qw(whitespace modifier expression comma)],
        rules     => [
            $chaf_rule,
            $separated_sequence_rule,
            $sequence_rule,
            { lhs => 'optional_whitespace', rhs => [qw(whitespace)] },
            { lhs => 'optional_whitespace', },
            { lhs => 'optional_modifier',   rhs => [qw(modifier)] },
            { lhs => 'optional_modifier', },
        ],
    }
);

$grammar->precompute();

my $recce = Marpa::Recognizer->new( { grammar => $grammar } );

# While we are at it, test the handling of null parses in
# the Single Parse Evaluator
my @tokens = ();

$recce->tokens( \@tokens );

# Marpa::Display::End

my $show_rules_output = $grammar->show_rules();

# Marpa::Display
# name: Rewrite show_rules Output
# start-after-line: END_RULES
# end-before-line: '^END_RULES$'

Marpa::Test::is( $show_rules_output, <<'END_RULES', 'Rewritten Rules' );
0: statement -> optional_whitespace expression optional_whitespace optional_modifier optional_whitespace /* !used */
1: statements -> statement[Seq:1-*][Sep:comma][x9] /* vrhs discard_sep real=0 */
2: statements -> statement[Seq:1-*][Sep:comma][x9] comma /* vrhs discard_sep real=1 */
3: statement[Seq:1-*][Sep:comma][x9] -> statement /* vlhs real=1 */
4: statement[Seq:1-*][Sep:comma][x9] -> statement[Seq:1-*][Sep:comma][x9] comma statement /* vlhs vrhs real=2 */
5: block -> /* empty !used nullable */
6: block -> statements[Seq:1-*][xa] /* vrhs real=0 */
7: statements[Seq:1-*][xa] -> statements /* vlhs real=1 */
8: statements[Seq:1-*][xa] -> statements[Seq:1-*][xa] statements /* vlhs vrhs real=1 */
9: optional_whitespace -> whitespace
10: optional_whitespace -> /* empty !used nullable */
11: optional_modifier -> modifier
12: optional_modifier -> /* empty !used nullable */
13: statement -> optional_whitespace expression statement[R0:2][xe] /* vrhs real=2 */
14: statement -> optional_whitespace expression optional_whitespace[] optional_modifier[] optional_whitespace[]
15: statement -> optional_whitespace[] expression statement[R0:2][xe] /* vrhs real=2 */
16: statement -> optional_whitespace[] expression optional_whitespace[] optional_modifier[] optional_whitespace[]
17: statement[R0:2][xe] -> optional_whitespace statement[R0:3][xf] /* vlhs vrhs real=1 */
18: statement[R0:2][xe] -> optional_whitespace optional_modifier[] optional_whitespace[] /* vlhs real=3 */
19: statement[R0:2][xe] -> optional_whitespace[] statement[R0:3][xf] /* vlhs vrhs real=1 */
20: statement[R0:3][xf] -> optional_modifier optional_whitespace /* vlhs real=2 */
21: statement[R0:3][xf] -> optional_modifier optional_whitespace[] /* vlhs real=2 */
22: statement[R0:3][xf] -> optional_modifier[] optional_whitespace /* vlhs real=2 */
23: block['] -> block /* vlhs real=1 */
24: block['][] -> /* empty nullable vlhs real=1 */
END_RULES

# Marpa::Display::End

my $value_ref = $recce->value();
my $value = $value_ref ? ${$value_ref} : 'No Parse';

Marpa::Test::is( $value, 'Null parse', 'Null parse value' );

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
