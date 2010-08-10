#!/usr/bin/perl

# Engine Synopsis

use 5.010;
use strict;
use warnings;

use Fatal qw(open close);
use Test::More tests => 7;

use lib 'lib';
use Marpa::Test;

BEGIN {
    Test::More::use_ok('Marpa');
}

# Marpa::Display
# name: Implementation Example

my $grammar = Marpa::Grammar->new(
    {   start          => 'Expression',
        actions        => 'My_Actions',
        default_action => 'first_arg',
        strip          => 0,
        rules          => [
            { lhs => 'Expression', rhs => [qw/Term/] },
            { lhs => 'Term',       rhs => [qw/Factor/] },
            { lhs => 'Factor',     rhs => [qw/Number/] },
            { lhs => 'Term', rhs => [qw/Term Add Term/], action => 'do_add' },
            {   lhs    => 'Factor',
                rhs    => [qw/Factor Multiply Factor/],
                action => 'do_multiply'
            },
        ],
    }
);

$grammar->precompute();

my $recce = Marpa::Recognizer->new( { grammar => $grammar } );

my @tokens = (
    [ 'Number',   42 ],
    [ 'Multiply', q{*} ],
    [ 'Number',   1 ],
    [ 'Add',      q{+} ],
    [ 'Number',   7 ],
);

$recce->tokens( \@tokens );

sub My_Actions::do_add {
    my ( undef, $t1, undef, $t2 ) = @_;
    return $t1 + $t2;
}

sub My_Actions::do_multiply {
    my ( undef, $t1, undef, $t2 ) = @_;
    return $t1 * $t2;
}

sub My_Actions::first_arg { shift; return shift; }

# Marpa::Display::End

my $show_symbols_output = $grammar->show_symbols();

# Marpa::Display
# name: Implementation Example show_symbols Output
# start-after-line: END_SYMBOLS
# end-before-line: '^END_SYMBOLS$'

Marpa::Test::is( $show_symbols_output,
    <<'END_SYMBOLS', 'Implementation Example Symbols' );
0: Expression, lhs=[0] rhs=[5] terminal
1: Term, lhs=[1 3] rhs=[0 3] terminal
2: Factor, lhs=[2 4] rhs=[1 4] terminal
3: Number, lhs=[] rhs=[2] terminal
4: Add, lhs=[] rhs=[3] terminal
5: Multiply, lhs=[] rhs=[4] terminal
6: Expression['], lhs=[5] rhs=[]
END_SYMBOLS

# Marpa::Display::End

my $show_rules_output = $grammar->show_rules();

# Marpa::Display
# name: Implementation Example show_rules Output
# start-after-line: END_RULES
# end-before-line: '^END_RULES$'

Marpa::Test::is( $show_rules_output,
    <<'END_RULES', 'Implementation Example Rules' );
0: Expression -> Term
1: Term -> Factor
2: Factor -> Number
3: Term -> Term Add Term
4: Factor -> Factor Multiply Factor
5: Expression['] -> Expression /* vlhs real=1 */
END_RULES

# Marpa::Display::End

my $show_AHFA_output = $grammar->show_AHFA();

# Marpa::Display
# name: Implementation Example show_AHFA Output
# start-after-line: END_AHFA
# end-before-line: '^END_AHFA$'

Marpa::Test::is( $show_AHFA_output,
    <<'END_AHFA', 'Implementation Example AHFA' );
Start States: S0; S1
S0: 15
Expression['] -> . Expression
 <Expression> => S2; leo(Expression['])
S1: predict; 1,3,5,7,11
Expression -> . Term
Term -> . Factor
Factor -> . Number
Term -> . Term Add Term
Factor -> . Factor Multiply Factor
 <Factor> => S3
 <Number> => S4
 <Term> => S5
S2: leo-c; 16
Expression['] -> Expression .
S3: 4,12
Term -> Factor .
Factor -> Factor . Multiply Factor
 <Multiply> => S6; S7
S4: 6
Factor -> Number .
S5: 2,8
Expression -> Term .
Term -> Term . Add Term
 <Add> => S8; S9
S6: 13
Factor -> Factor Multiply . Factor
 <Factor> => S10; leo(Factor)
S7: predict; 5,11
Factor -> . Number
Factor -> . Factor Multiply Factor
 <Factor> => S11
 <Number> => S4
S8: 9
Term -> Term Add . Term
 <Term> => S12; leo(Term)
S9: predict; 3,5,7,11
Term -> . Factor
Factor -> . Number
Term -> . Term Add Term
Factor -> . Factor Multiply Factor
 <Factor> => S3
 <Number> => S4
 <Term> => S13
S10: leo-c; 14
Factor -> Factor Multiply Factor .
S11: 12
Factor -> Factor . Multiply Factor
 <Multiply> => S6; S7
S12: leo-c; 10
Term -> Term Add Term .
S13: 8
Term -> Term . Add Term
 <Add> => S8; S9
END_AHFA

# Marpa::Display::End

my $show_earley_sets_output = $recce->show_earley_sets();

# Marpa::Display
# name: Implementation Example show_earley_sets Output
# start-after-line: END_EARLEY_SETS
# end-before-line: '^END_EARLEY_SETS$'

Marpa::Test::is( $show_earley_sets_output,
    <<'END_EARLEY_SETS', 'Implementation Example Earley Sets' );
Last Completed: 5; Furthest: 5
Earley Set 0
S0@0-0
S1@0-0
Earley Set 1
S4@0-1 [p=S1@0-0; s=Number; t=\42]
S3@0-1 [p=S1@0-0; c=S4@0-1]
S5@0-1 [p=S1@0-0; c=S3@0-1]
S2@0-1 [p=S0@0-0; c=S5@0-1]
Earley Set 2
S6@0-2 [p=S3@0-1; s=Multiply; t=\'*']
S7@2-2
Earley Set 3
S4@2-3 [p=S7@2-2; s=Number; t=\1]
S10@0-3 [p=S6@0-2; c=S4@2-3]
S11@2-3 [p=S7@2-2; c=S4@2-3]
S3@0-3 [p=S1@0-0; c=S10@0-3]
S5@0-3 [p=S1@0-0; c=S3@0-3]
S2@0-3 [p=S0@0-0; c=S5@0-3]
Earley Set 4
S8@0-4 [p=S5@0-3; s=Add; t=\'+']
S9@4-4
Earley Set 5
S4@4-5 [p=S9@4-4; s=Number; t=\7]
S3@4-5 [p=S9@4-4; c=S4@4-5]
S12@0-5 [p=S8@0-4; c=S3@4-5]
S13@4-5 [p=S9@4-4; c=S3@4-5]
S5@0-5 [p=S1@0-0; c=S12@0-5]
S2@0-5 [p=S0@0-0; c=S5@0-5]
END_EARLEY_SETS

# Marpa::Display::End

# Marpa::Display
# name: Implementation Example Multi-parse Evaluator Call

my $evaler = Marpa::Evaluator->new( { recce => $recce } );

# Marpa::Display::End

my $value_ref = $evaler->value();
my $value = $value_ref ? ${$value_ref} : 'No Parse';
Marpa::Test::is( 49, $value, 'Implementation Example G2 Value' );

# Marpa::Display
# name: Implementation Example show_bocage Call

my $show_bocage_output = $evaler->show_bocage(2);

# Marpa::Display::End

# Marpa::Display
# name: Implementation Example show_bocage Output
# start-after-line: END_BOCAGE
# end-before-line: '^END_BOCAGE$'

Marpa::Test::is( $show_bocage_output,
    <<'END_BOCAGE' , 'Implementation Example Bocage' );
parse count: 1
S2@0-5L6o0 -> S2@0-5L6o0a0
S2@0-5L6o0a0 -> S5@0-5L0o1
    rule 5: Expression['] -> Expression .
    value_ops
S5@0-5L0o1 -> S5@0-5L0o1a1
S5@0-5L0o1a1 -> S12@0-5L1o2
    rule 0: Expression -> Term .
    value_ops
S12@0-5L1o2 -> S12@0-5L1o2a2
S12@0-5L1o2a2 -> S8@0-4R3:2o5 S3@4-5L1o3
    rule 3: Term -> Term Add Term .
    value_ops
S3@4-5L1o3 -> S3@4-5L1o3a3
S3@4-5L1o3a3 -> S4@4-5L2o4
    rule 1: Term -> Factor .
    value_ops
S4@4-5L2o4 -> S4@4-5L2o4a4
S4@4-5L2o4a4 -> \7
    rule 2: Factor -> Number .
    value_ops
S8@0-4R3:2o5 -> S8@0-4R3:2o5a5
S8@0-4R3:2o5a5 -> S5@0-3R3:1o6 \'+'
    rule 3: Term -> Term Add . Term
S5@0-3R3:1o6 -> S5@0-3R3:1o6a6
S5@0-3R3:1o6a6 -> S3@0-3L1o7
    rule 3: Term -> Term . Add Term
S3@0-3L1o7 -> S3@0-3L1o7a7
S3@0-3L1o7a7 -> S10@0-3L2o8
    rule 1: Term -> Factor .
    value_ops
S10@0-3L2o8 -> S10@0-3L2o8a8
S10@0-3L2o8a8 -> S6@0-2R4:2o10 S4@2-3L2o9
    rule 4: Factor -> Factor Multiply Factor .
    value_ops
S4@2-3L2o9 -> S4@2-3L2o9a9
S4@2-3L2o9a9 -> \1
    rule 2: Factor -> Number .
    value_ops
S6@0-2R4:2o10 -> S6@0-2R4:2o10a10
S6@0-2R4:2o10a10 -> S3@0-1R4:1o11 \'*'
    rule 4: Factor -> Factor Multiply . Factor
S3@0-1R4:1o11 -> S3@0-1R4:1o11a11
S3@0-1R4:1o11a11 -> S4@0-1L2o12
    rule 4: Factor -> Factor . Multiply Factor
S4@0-1L2o12 -> S4@0-1L2o12a12
S4@0-1L2o12a12 -> \42
    rule 2: Factor -> Number .
    value_ops
END_BOCAGE

# Marpa::Display::End

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
