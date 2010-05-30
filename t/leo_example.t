#!/usr/bin/perl

# Engine Synopsis

use 5.010;
use strict;
use warnings;

use Fatal qw(open close);
use Test::More tests => 8;

use lib 'lib';
use Marpa::Test;

BEGIN {
    Test::More::use_ok('Marpa');
}

## no critic (Subroutines::RequireArgUnpacking)

# Marpa::Display
# name: Leo Example

my $grammar = Marpa::Grammar->new(
    {   start          => 'Statement',
        actions        => 'My_Actions',
        default_action => 'first_arg',
        strip          => 0,
        rules          => [
            {   lhs    => 'Statement',
                rhs    => [qw/Expression/],
                action => 'do_Statement'
            },
            {   lhs    => 'Expression',
                rhs    => [qw/Lvalue AssignOp Expression/],
                action => 'do_Expression'
            },
            {   lhs    => 'Expression',
                rhs    => [qw/Lvalue AddAssignOp Expression/],
                action => 'do_Expression'
            },
            {   lhs    => 'Expression',
                rhs    => [qw/Lvalue MinusAssignOp Expression/],
                action => 'do_Expression'
            },
            {   lhs    => 'Expression',
                rhs    => [qw/Lvalue MultiplyAssignOp Expression/],
                action => 'do_Expression'
            },
            {   lhs    => 'Expression',
                rhs    => [qw/Variable/],
                action => 'do_Expression'
            },
            { lhs => 'Lvalue', rhs => [qw/Variable/] },
        ],
    }
);

$grammar->precompute();

my $recce = Marpa::Recognizer->new( { grammar => $grammar } );

my @tokens = (
    [ 'Variable',         'a' ],
    [ 'AssignOp',         q{=} ],
    [ 'Variable',         'b' ],
    [ 'AddAssignOp',      q{+=} ],
    [ 'Variable',         'c' ],
    [ 'MinusAssignOp',    q{-=} ],
    [ 'Variable',         'd' ],
    [ 'MultiplyAssignOp', q{*=} ],
    [ 'Variable',         'e' ],
);

$recce->tokens( \@tokens );

%My_Actions::VALUES = ( a => 711, b => 47, c => 1, d => 2, e => 3 );

sub My_Actions::do_Statement {
    return join q{ }, map { $_ . q{=} . $My_Actions::VALUES{$_} }
        sort keys %My_Actions::VALUES;
}

sub My_Actions::do_Expression {
    my ( undef, $lvariable, $op, $rvalue ) = @_;
    my $original_value = $My_Actions::VALUES{$lvariable};
    return $original_value if not defined $rvalue;
    return
        $My_Actions::VALUES{$lvariable} =
          $op eq q{*=} ? ( $original_value * $rvalue )
        : $op eq q{+=} ? ( $original_value + $rvalue )
        : $op eq q{-=} ? ( $original_value - $rvalue )
        : $rvalue

} ## end sub My_Actions::do_Expression

sub My_Actions::first_arg { return $_[1] }

# Marpa::Display::End

## use critic

my $show_symbols_output = $grammar->show_symbols();

# Marpa::Display
# name: Leo Example show_symbols Output
# start-after-line: END_SYMBOLS
# end-before-line: '^END_SYMBOLS$'

Marpa::Test::is( $show_symbols_output,
    <<'END_SYMBOLS', 'Leo Example Symbols' );
0: Statement, lhs=[0] rhs=[7] terminal
1: Expression, lhs=[1 2 3 4 5] rhs=[0 1 2 3 4] terminal
2: Lvalue, lhs=[6] rhs=[1 2 3 4] terminal
3: AssignOp, lhs=[] rhs=[1] terminal
4: AddAssignOp, lhs=[] rhs=[2] terminal
5: MinusAssignOp, lhs=[] rhs=[3] terminal
6: MultiplyAssignOp, lhs=[] rhs=[4] terminal
7: Variable, lhs=[] rhs=[5 6] terminal
8: Statement['], lhs=[7] rhs=[]
END_SYMBOLS

# Marpa::Display::End

my $show_rules_output = $grammar->show_rules();

# Marpa::Display
# name: Leo Example show_rules Output
# start-after-line: END_RULES
# end-before-line: '^END_RULES$'

Marpa::Test::is( $show_rules_output, <<'END_RULES', 'Leo Example Rules' );
0: Statement -> Expression
1: Expression -> Lvalue AssignOp Expression
2: Expression -> Lvalue AddAssignOp Expression
3: Expression -> Lvalue MinusAssignOp Expression
4: Expression -> Lvalue MultiplyAssignOp Expression
5: Expression -> Variable
6: Lvalue -> Variable
7: Statement['] -> Statement /* vlhs real=1 */
END_RULES

# Marpa::Display::End

my $show_AHFA_output = $grammar->show_AHFA();

# Marpa::Display
# name: Leo Example show_AHFA Output
# start-after-line: END_AHFA
# end-before-line: '^END_AHFA$'

Marpa::Test::is( $show_AHFA_output, <<'END_AHFA', 'Leo Example AHFA' );
Start States: S0; S1
S0: 23
Statement['] -> . Statement
 <Statement> => S2; leo(Statement['])
S1: predict; 1,3,7,11,15,19,21
Statement -> . Expression
Expression -> . Lvalue AssignOp Expression
Expression -> . Lvalue AddAssignOp Expression
Expression -> . Lvalue MinusAssignOp Expression
Expression -> . Lvalue MultiplyAssignOp Expression
Expression -> . Variable
Lvalue -> . Variable
 <Expression> => S3; leo(Statement)
 <Lvalue> => S4
 <Variable> => S5
S2: leo-c; 24
Statement['] -> Statement .
S3: leo-c; 2
Statement -> Expression .
S4: 4,8,12,16
Expression -> Lvalue . AssignOp Expression
Expression -> Lvalue . AddAssignOp Expression
Expression -> Lvalue . MinusAssignOp Expression
Expression -> Lvalue . MultiplyAssignOp Expression
 <AddAssignOp> => S6; S7
 <AssignOp> => S7; S8
 <MinusAssignOp> => S7; S9
 <MultiplyAssignOp> => S10; S7
S5: 20,22
Expression -> Variable .
Lvalue -> Variable .
S6: 9
Expression -> Lvalue AddAssignOp . Expression
 <Expression> => S11; leo(Expression)
S7: predict; 3,7,11,15,19,21
Expression -> . Lvalue AssignOp Expression
Expression -> . Lvalue AddAssignOp Expression
Expression -> . Lvalue MinusAssignOp Expression
Expression -> . Lvalue MultiplyAssignOp Expression
Expression -> . Variable
Lvalue -> . Variable
 <Lvalue> => S4
 <Variable> => S5
S8: 5
Expression -> Lvalue AssignOp . Expression
 <Expression> => S12; leo(Expression)
S9: 13
Expression -> Lvalue MinusAssignOp . Expression
 <Expression> => S13; leo(Expression)
S10: 17
Expression -> Lvalue MultiplyAssignOp . Expression
 <Expression> => S14; leo(Expression)
S11: leo-c; 10
Expression -> Lvalue AddAssignOp Expression .
S12: leo-c; 6
Expression -> Lvalue AssignOp Expression .
S13: leo-c; 14
Expression -> Lvalue MinusAssignOp Expression .
S14: leo-c; 18
Expression -> Lvalue MultiplyAssignOp Expression .
END_AHFA

# Marpa::Display::End

my $show_earley_sets_output_before = $recce->show_earley_sets();

# Marpa::Display
# name: Leo Example show_earley_sets "Before" Output
# start-after-line: END_EARLEY_SETS
# end-before-line: '^END_EARLEY_SETS$'

Marpa::Test::is( $show_earley_sets_output_before,
    <<'END_EARLEY_SETS', 'Leo Example Earley Sets "Before"' );
Last Completed: 9; Furthest: 9
Earley Set 0
S0@0-0
S1@0-0
Earley Set 1
S5@0-1 [p=S1@0-0; s=Variable; t=\'a']
S3@0-1 [p=S1@0-0; c=S5@0-1]
S4@0-1 [p=S1@0-0; c=S5@0-1]
S2@0-1 [p=S0@0-0; c=S3@0-1]
Earley Set 2
S8@0-2 [p=S4@0-1; s=AssignOp; t=\'=']
S7@2-2
L12@0-2; actual="Expression"->12; [c=S8@0-2]
Earley Set 3
S5@2-3 [p=S7@2-2; s=Variable; t=\'b']
S12@0-3 [l=L12@0-2; c=S5@2-3]
S4@2-3 [p=S7@2-2; c=S5@2-3]
S3@0-3 [p=S1@0-0; c=S12@0-3]
S2@0-3 [p=S0@0-0; c=S3@0-3]
Earley Set 4
S6@2-4 [p=S4@2-3; s=AddAssignOp; t=\'+=']
S7@4-4
L12@0-4; actual="Expression"->11; [p=L12@0-2; c=S6@2-4]
Earley Set 5
S5@4-5 [p=S7@4-4; s=Variable; t=\'c']
S12@0-5 [l=L12@0-4; c=S5@4-5]
S4@4-5 [p=S7@4-4; c=S5@4-5]
S3@0-5 [p=S1@0-0; c=S12@0-5]
S2@0-5 [p=S0@0-0; c=S3@0-5]
Earley Set 6
S9@4-6 [p=S4@4-5; s=MinusAssignOp; t=\'-=']
S7@6-6
L12@0-6; actual="Expression"->13; [p=L12@0-4; c=S9@4-6]
Earley Set 7
S5@6-7 [p=S7@6-6; s=Variable; t=\'d']
S12@0-7 [l=L12@0-6; c=S5@6-7]
S4@6-7 [p=S7@6-6; c=S5@6-7]
S3@0-7 [p=S1@0-0; c=S12@0-7]
S2@0-7 [p=S0@0-0; c=S3@0-7]
Earley Set 8
S10@6-8 [p=S4@6-7; s=MultiplyAssignOp; t=\'*=']
S7@8-8
L12@0-8; actual="Expression"->14; [p=L12@0-6; c=S10@6-8]
Earley Set 9
S5@8-9 [p=S7@8-8; s=Variable; t=\'e']
S12@0-9 [l=L12@0-8; c=S5@8-9]
S4@8-9 [p=S7@8-8; c=S5@8-9]
S3@0-9 [p=S1@0-0; c=S12@0-9]
S2@0-9 [p=S0@0-0; c=S3@0-9]
END_EARLEY_SETS

# Marpa::Display::End

my $trace_output;
open my $trace_fh, q{>}, \$trace_output;
my $value_ref = $recce->value( { trace_fh => $trace_fh, trace_values => 1 } );
close $trace_fh;

my $value = ref $value_ref ? ${$value_ref} : 'No Parse';
Marpa::Test::is( $value, 'a=42 b=42 c=-5 d=6 e=3', 'Leo Example Value' );

my $show_earley_sets_output_after = $recce->show_earley_sets();

# Marpa::Display
# name: Leo Example show_earley_sets "After" Output
# start-after-line: END_EARLEY_SETS
# end-before-line: '^END_EARLEY_SETS$'

Marpa::Test::is( $show_earley_sets_output_after,
    <<'END_EARLEY_SETS', 'Leo Example Earley Sets "After"' );
Last Completed: 9; Furthest: 9
Earley Set 0
S0@0-0
S1@0-0
Earley Set 1
S5@0-1 [p=S1@0-0; s=Variable; t=\'a']
S3@0-1 [p=S1@0-0; c=S5@0-1]
S4@0-1 [p=S1@0-0; c=S5@0-1]
S2@0-1 [p=S0@0-0; c=S3@0-1]
Earley Set 2
S8@0-2 [p=S4@0-1; s=AssignOp; t=\'=']
S7@2-2
L12@0-2; actual="Expression"->12; [c=S8@0-2]
Earley Set 3
S5@2-3 [p=S7@2-2; s=Variable; t=\'b']
S12@0-3 [l=L12@0-2; c=S5@2-3]
S4@2-3 [p=S7@2-2; c=S5@2-3]
S3@0-3 [p=S1@0-0; c=S12@0-3]
S2@0-3 [p=S0@0-0; c=S3@0-3]
Earley Set 4
S6@2-4 [p=S4@2-3; s=AddAssignOp; t=\'+=']
S7@4-4
L12@0-4; actual="Expression"->11; [p=L12@0-2; c=S6@2-4]
Earley Set 5
S5@4-5 [p=S7@4-4; s=Variable; t=\'c']
S12@0-5 [l=L12@0-4; c=S5@4-5]
S4@4-5 [p=S7@4-4; c=S5@4-5]
S3@0-5 [p=S1@0-0; c=S12@0-5]
S2@0-5 [p=S0@0-0; c=S3@0-5]
Earley Set 6
S9@4-6 [p=S4@4-5; s=MinusAssignOp; t=\'-=']
S7@6-6
L12@0-6; actual="Expression"->13; [p=L12@0-4; c=S9@4-6]
Earley Set 7
S5@6-7 [p=S7@6-6; s=Variable; t=\'d']
S12@0-7 [l=L12@0-6; c=S5@6-7]
S4@6-7 [p=S7@6-6; c=S5@6-7]
S3@0-7 [p=S1@0-0; c=S12@0-7]
S2@0-7 [p=S0@0-0; c=S3@0-7]
Earley Set 8
S10@6-8 [p=S4@6-7; s=MultiplyAssignOp; t=\'*=']
S7@8-8
L12@0-8; actual="Expression"->14; [p=L12@0-6; c=S10@6-8]
Earley Set 9
S5@8-9 [p=S7@8-8; s=Variable; t=\'e']
S12@0-9 [p=S8@0-2; c=S11@2-9]
S4@8-9 [p=S7@8-8; c=S5@8-9]
S3@0-9 [p=S1@0-0; c=S12@0-9]
S2@0-9 [p=S0@0-0; c=S3@0-9]
S14@6-9 [p=S10@6-8; c=S5@8-9]
S13@4-9 [p=S9@4-6; c=S14@6-9]
S11@2-9 [p=S6@2-4; c=S13@4-9]
END_EARLEY_SETS

# Marpa::Display::End

# Marpa::Display
# name: Leo Example trace_values Output
# start-after-line: END_TRACE_OUTPUT
# end-before-line: '^END_TRACE_OUTPUT$'

Marpa::Test::is( $trace_output,
    <<'END_TRACE_OUTPUT', 'Leo Example Trace Output' );
Pushed value from S5@0-1L2o18a18: Variable = \'a'
Popping 1 values to evaluate S5@0-1L2o18a18, rule: 6: Lvalue -> Variable
Calculated and pushed value: 'a'
Pushed value from S8@0-2R1:2o16a16: AssignOp = \'='
Pushed value from S5@2-3L2o15a15: Variable = \'b'
Popping 1 values to evaluate S5@2-3L2o15a15, rule: 6: Lvalue -> Variable
Calculated and pushed value: 'b'
Pushed value from S6@2-4R2:2o13a13: AddAssignOp = \'+='
Pushed value from S5@4-5L2o12a12: Variable = \'c'
Popping 1 values to evaluate S5@4-5L2o12a12, rule: 6: Lvalue -> Variable
Calculated and pushed value: 'c'
Pushed value from S9@4-6R3:2o10a10: MinusAssignOp = \'-='
Pushed value from S5@6-7L2o9a9: Variable = \'d'
Popping 1 values to evaluate S5@6-7L2o9a9, rule: 6: Lvalue -> Variable
Calculated and pushed value: 'd'
Pushed value from S10@6-8R4:2o7a7: MultiplyAssignOp = \'*='
Pushed value from S5@8-9L1o6a6: Variable = \'e'
Popping 1 values to evaluate S5@8-9L1o6a6, rule: 5: Expression -> Variable
Calculated and pushed value: 3
Popping 3 values to evaluate S14@6-9L1o5a5, rule: 4: Expression -> Lvalue MultiplyAssignOp Expression
Calculated and pushed value: 6
Popping 3 values to evaluate S13@4-9L1o4a4, rule: 3: Expression -> Lvalue MinusAssignOp Expression
Calculated and pushed value: -5
Popping 3 values to evaluate S11@2-9L1o3a3, rule: 2: Expression -> Lvalue AddAssignOp Expression
Calculated and pushed value: 42
Popping 3 values to evaluate S12@0-9L1o2a2, rule: 1: Expression -> Lvalue AssignOp Expression
Calculated and pushed value: 42
Popping 1 values to evaluate S3@0-9L0o1a1, rule: 0: Statement -> Expression
Calculated and pushed value: 'a=42 b=42 c=-5 d=6 e=3'
New Virtual Rule: S2@0-9L8o0a0, rule: 7: Statement['] -> Statement
Symbol count is 1, now 1 rules
END_TRACE_OUTPUT

# Marpa::Display::End

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
