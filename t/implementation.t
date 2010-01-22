#!/usr/bin/perl

# Engine Synopsis

use 5.010;
use strict;
use warnings;

use Test::More tests => 5;

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
    [ 'Number', 42 ],
    [ 'Multiply', ],
    [ 'Number', 1 ],
    [ 'Add', ],
    [ 'Number', 7 ],
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

my $value_ref = $recce->value;
my $value = $value_ref ? ${$value_ref} : 'No Parse';

# Marpa::Display::End

my $show_symbols_output = $grammar->show_symbols();

# Marpa::Display
# name: Synopsis show_symbols Output
# start-after-line: END_SYMBOLS
# end-before-line: '^END_SYMBOLS$'

Marpa::Test::is( $show_symbols_output, <<'END_SYMBOLS', 'Synopsis Symbols' );
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
# name: Synopsis show_rules Output
# start-after-line: END_RULES
# end-before-line: '^END_RULES$'

Marpa::Test::is( $show_rules_output, <<'END_RULES', 'Synopsis Rules' );
0: Expression -> Term
1: Term -> Factor
2: Factor -> Number
3: Term -> Term Add Term
4: Factor -> Factor Multiply Factor
5: Expression['] -> Expression /* vlhs real=1 */
END_RULES

# Marpa::Display::End

my $show_QDFA_output = $grammar->show_QDFA();

# Marpa::Display
# name: Synopsis show_QDFA Output
# start-after-line: END_QDFA
# end-before-line: '^END_QDFA$'

Marpa::Test::is( $show_QDFA_output, <<'END_QDFA', 'Synopsis QDFA' );
Start States: S0; S1
S0: 15
Expression['] -> . Expression
 <Expression> => S2
S1: predict; 1,3,5,7,11
Expression -> . Term
Term -> . Factor
Factor -> . Number
Term -> . Term Add Term
Factor -> . Factor Multiply Factor
 <Factor> => S3
 <Number> => S4
 <Term> => S5
S2: 16
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
 <Factor> => S10
S7: predict; 5,11
Factor -> . Number
Factor -> . Factor Multiply Factor
 <Factor> => S11
 <Number> => S4
S8: 9
Term -> Term Add . Term
 <Term> => S12
S9: predict; 3,5,7,11
Term -> . Factor
Factor -> . Number
Term -> . Term Add Term
Factor -> . Factor Multiply Factor
 <Factor> => S3
 <Number> => S4
 <Term> => S13
S10: 14
Factor -> Factor Multiply Factor .
S11: 12
Factor -> Factor . Multiply Factor
 <Multiply> => S6; S7
S12: 10
Term -> Term Add Term .
S13: 8
Term -> Term . Add Term
 <Add> => S8; S9
END_QDFA

# Marpa::Display::End

my $show_earley_sets_output = $recce->show_earley_sets();

# Marpa::Display
# name: Synopsis show_earley_sets Output
# start-after-line: END_EARLEY_SETS
# end-before-line: '^END_EARLEY_SETS$'

Marpa::Test::is( $show_earley_sets_output,
    <<'END_EARLEY_SETS', 'Synopsis Earley Sets' );
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
S6@0-2 [p=S3@0-1; s=Multiply; t=\undef]
S7@2-2
Earley Set 3
S4@2-3 [p=S7@2-2; s=Number; t=\1]
S10@0-3 [p=S6@0-2; c=S4@2-3]
S11@2-3 [p=S7@2-2; c=S4@2-3]
S3@0-3 [p=S1@0-0; c=S10@0-3]
S5@0-3 [p=S1@0-0; c=S3@0-3]
S2@0-3 [p=S0@0-0; c=S5@0-3]
Earley Set 4
S8@0-4 [p=S5@0-3; s=Add; t=\undef]
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

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
