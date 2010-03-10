#!perl

# the example grammar in Aycock/Horspool "Practical Earley Parsing",
# _The Computer Journal_, Vol. 45, No. 6, pp. 620-630,
# in its "NNF" form

use 5.010;
use strict;
use warnings;

use lib 'lib';
use Test::More tests => 20;
use Marpa::Test;

BEGIN {
    Test::More::use_ok('Marpa');
}

## no critic (Subroutines::RequireArgUnpacking)

sub default_action {
    shift;
    my $v_count = scalar @_;
    return q{}   if $v_count <= 0;
    return $_[0] if $v_count == 1;
    return '(' . join( q{;}, @_ ) . ')';
} ## end sub default_action

## use critic

my $grammar = Marpa::Grammar->new(
    {   start   => 'S',
        strip   => 0,
        maximal => 1,
        rules   => [
            [ 'S', [qw/A A A A/] ],
            [ 'A', [qw/a/] ],
            [ 'A', [qw/E/] ],
            ['E'],
        ],
        default_null_value => q{},
        default_action     => 'main::default_action',
    }
);

$grammar->set( { terminals => ['a'], } );

$grammar->precompute();

Marpa::Test::is( $grammar->show_rules, <<'EOS', 'Aycock/Horspool Rules' );
0: S -> A A A A /* !used nullable maximal */
1: A -> a /* maximal */
2: A -> E /* !used nullable maximal */
3: E -> /* empty !used nullable maximal */
4: S -> A S[R0:1] /* vrhs maximal real=1 */
5: S -> A A[] A[] A[] /* maximal */
6: S -> A[] S[R0:1] /* vrhs maximal real=1 */
7: S[R0:1] -> A S[R0:2] /* vlhs vrhs maximal real=1 */
8: S[R0:1] -> A A[] A[] /* vlhs maximal real=3 */
9: S[R0:1] -> A[] S[R0:2] /* vlhs vrhs maximal real=1 */
10: S[R0:2] -> A A /* vlhs maximal real=2 */
11: S[R0:2] -> A A[] /* vlhs maximal real=2 */
12: S[R0:2] -> A[] A /* vlhs maximal real=2 */
13: S['] -> S /* vlhs maximal real=1 */
14: S['][] -> /* empty nullable vlhs maximal real=1 */
EOS

Marpa::Test::is( $grammar->show_symbols, <<'EOS', 'Aycock/Horspool Symbols' );
0: S, lhs=[0 4 5 6] rhs=[13] maximal
1: A, lhs=[1 2] rhs=[0 4 5 7 8 10 11 12] maximal
2: a, lhs=[] rhs=[1] terminal maximal
3: E, lhs=[3] rhs=[2] nullable=1 nulling maximal
4: S[], lhs=[] rhs=[] nullable=4 nulling maximal
5: A[], lhs=[] rhs=[5 6 8 9 11 12] nullable=1 nulling maximal
6: S[R0:1], lhs=[7 8 9] rhs=[4 6] maximal
7: S[R0:2], lhs=[10 11 12] rhs=[7 9] maximal
8: S['], lhs=[13] rhs=[] maximal
9: S['][], lhs=[14] rhs=[] nullable=1 nulling maximal
EOS

Marpa::Test::is(
    $grammar->show_nullable_symbols,
    q{A[] E S['][] S[]},
    'Aycock/Horspool Nullable Symbols'
);
Marpa::Test::is(
    $grammar->show_nulling_symbols,
    q{A[] E S['][] S[]},
    'Aycock/Horspool Nulling Symbols'
);
Marpa::Test::is(
    $grammar->show_productive_symbols,
    q{A A[] E S S['] S['][] S[R0:1] S[R0:2] S[] a},
    'Aycock/Horspool Productive Symbols'
);
Marpa::Test::is(
    $grammar->show_accessible_symbols,
    q{A A[] E S S['] S['][] S[R0:1] S[R0:2] S[] a},
    'Aycock/Horspool Accessible Symbols'
);

Marpa::Test::is( $grammar->show_NFA, <<'EOS', 'Aycock/Horspool NFA' );
S0: /* empty */
 empty => S33 S35
S1: A -> . a
 <a> => S2
S2: A -> a .
S3: S -> . A S[R0:1]
 empty => S1
 <A> => S4
S4: S -> A . S[R0:1]
 empty => S14 S17 S21
 <S[R0:1]> => S5
S5: S -> A S[R0:1] .
S6: S -> . A A[] A[] A[]
 empty => S1
 <A> => S7
S7: S -> A . A[] A[] A[]
at_nulling
 <A[]> => S8
S8: S -> A A[] . A[] A[]
at_nulling
 <A[]> => S9
S9: S -> A A[] A[] . A[]
at_nulling
 <A[]> => S10
S10: S -> A A[] A[] A[] .
S11: S -> . A[] S[R0:1]
at_nulling
 <A[]> => S12
S12: S -> A[] . S[R0:1]
 empty => S14 S17 S21
 <S[R0:1]> => S13
S13: S -> A[] S[R0:1] .
S14: S[R0:1] -> . A S[R0:2]
 empty => S1
 <A> => S15
S15: S[R0:1] -> A . S[R0:2]
 empty => S24 S27 S30
 <S[R0:2]> => S16
S16: S[R0:1] -> A S[R0:2] .
S17: S[R0:1] -> . A A[] A[]
 empty => S1
 <A> => S18
S18: S[R0:1] -> A . A[] A[]
at_nulling
 <A[]> => S19
S19: S[R0:1] -> A A[] . A[]
at_nulling
 <A[]> => S20
S20: S[R0:1] -> A A[] A[] .
S21: S[R0:1] -> . A[] S[R0:2]
at_nulling
 <A[]> => S22
S22: S[R0:1] -> A[] . S[R0:2]
 empty => S24 S27 S30
 <S[R0:2]> => S23
S23: S[R0:1] -> A[] S[R0:2] .
S24: S[R0:2] -> . A A
 empty => S1
 <A> => S25
S25: S[R0:2] -> A . A
 empty => S1
 <A> => S26
S26: S[R0:2] -> A A .
S27: S[R0:2] -> . A A[]
 empty => S1
 <A> => S28
S28: S[R0:2] -> A . A[]
at_nulling
 <A[]> => S29
S29: S[R0:2] -> A A[] .
S30: S[R0:2] -> . A[] A
at_nulling
 <A[]> => S31
S31: S[R0:2] -> A[] . A
 empty => S1
 <A> => S32
S32: S[R0:2] -> A[] A .
S33: S['] -> . S
 empty => S3 S6 S11
 <S> => S34
S34: S['] -> S .
S35: S['][] -> .
EOS

Marpa::Test::is( $grammar->show_QDFA, <<'EOS', 'Aycock/Horspool QDFA' );
Start States: S0; S1
S0: 33,35
S['] -> . S
S['][] -> .
 <S> => S2
S1: predict; 1,3,6,12,14,17,22,24,27,31
A -> . a
S -> . A S[R0:1]
S -> . A A[] A[] A[]
S -> A[] . S[R0:1]
S[R0:1] -> . A S[R0:2]
S[R0:1] -> . A A[] A[]
S[R0:1] -> A[] . S[R0:2]
S[R0:2] -> . A A
S[R0:2] -> . A A[]
S[R0:2] -> A[] . A
 <A> => S3; S4
 <S[R0:1]> => S5
 <S[R0:2]> => S6
 <a> => S7
S2: 34
S['] -> S .
S3: 4,10,15,20,25,29,32
S -> A . S[R0:1]
S -> A A[] A[] A[] .
S[R0:1] -> A . S[R0:2]
S[R0:1] -> A A[] A[] .
S[R0:2] -> A . A
S[R0:2] -> A A[] .
S[R0:2] -> A[] A .
 <A> => S8
 <S[R0:1]> => S9
 <S[R0:2]> => S10
S4: predict; 1,14,17,22,24,27,31
A -> . a
S[R0:1] -> . A S[R0:2]
S[R0:1] -> . A A[] A[]
S[R0:1] -> A[] . S[R0:2]
S[R0:2] -> . A A
S[R0:2] -> . A A[]
S[R0:2] -> A[] . A
 <A> => S11; S12
 <S[R0:2]> => S6
 <a> => S7
S5: 13
S -> A[] S[R0:1] .
S6: 23
S[R0:1] -> A[] S[R0:2] .
S7: 2
A -> a .
S8: 26
S[R0:2] -> A A .
S9: 5
S -> A S[R0:1] .
S10: 16
S[R0:1] -> A S[R0:2] .
S11: 15,20,25,29,32
S[R0:1] -> A . S[R0:2]
S[R0:1] -> A A[] A[] .
S[R0:2] -> A . A
S[R0:2] -> A A[] .
S[R0:2] -> A[] A .
 <A> => S8
 <S[R0:2]> => S10
S12: predict; 1,24,27,31
A -> . a
S[R0:2] -> . A A
S[R0:2] -> . A A[]
S[R0:2] -> A[] . A
 <A> => S13; S14
 <a> => S7
S13: 25,29,32
S[R0:2] -> A . A
S[R0:2] -> A A[] .
S[R0:2] -> A[] A .
 <A> => S8
S14: predict; 1
A -> . a
 <a> => S7
EOS

my $recce =
    Marpa::Recognizer->new( { grammar => $grammar, mode => 'stream' } );

my @set = (
    <<'END_OF_SET0', <<'END_OF_SET1', <<'END_OF_SET2', <<'END_OF_SET3', <<'END_OF_SET4', );
Earley Set 0
S0@0-0
S1@0-0
END_OF_SET0
Earley Set 1
S7@0-1 [p=S1@0-0; s=a; t=\'a']
S3@0-1 [p=S1@0-0; c=S7@0-1]
S4@1-1
S2@0-1 [p=S0@0-0; c=S3@0-1] [p=S0@0-0; c=S5@0-1]
S5@0-1 [p=S1@0-0; c=S3@0-1] [p=S1@0-0; c=S6@0-1]
S6@0-1 [p=S1@0-0; c=S3@0-1]
END_OF_SET1
Earley Set 2
S7@1-2 [p=S4@1-1; s=a; t=\'a']
S8@0-2 [p=S3@0-1; c=S7@1-2]
S11@1-2 [p=S4@1-1; c=S7@1-2]
S12@2-2
S6@0-2 [p=S1@0-0; c=S8@0-2]
S9@0-2 [p=S3@0-1; c=S11@1-2] [p=S3@0-1; c=S6@1-2]
S10@0-2 [p=S3@0-1; c=S11@1-2]
S6@1-2 [p=S4@1-1; c=S11@1-2]
S5@0-2 [p=S1@0-0; c=S6@0-2] [p=S1@0-0; c=S10@0-2]
S2@0-2 [p=S0@0-0; c=S9@0-2] [p=S0@0-0; c=S5@0-2]
END_OF_SET2
Earley Set 3
S7@2-3 [p=S12@2-2; s=a; t=\'a']
S8@1-3 [p=S11@1-2; c=S7@2-3]
S13@2-3 [p=S12@2-2; c=S7@2-3]
S14@3-3
S10@0-3 [p=S3@0-1; c=S8@1-3]
S6@1-3 [p=S4@1-1; c=S8@1-3]
S10@1-3 [p=S11@1-2; c=S13@2-3]
S5@0-3 [p=S1@0-0; c=S10@0-3]
S9@0-3 [p=S3@0-1; c=S6@1-3] [p=S3@0-1; c=S10@1-3]
S2@0-3 [p=S0@0-0; c=S5@0-3] [p=S0@0-0; c=S9@0-3]
END_OF_SET3
Earley Set 4
S7@3-4 [p=S14@3-3; s=a; t=\'a']
S8@2-4 [p=S13@2-3; c=S7@3-4]
S10@1-4 [p=S11@1-2; c=S8@2-4]
S9@0-4 [p=S3@0-1; c=S10@1-4]
S2@0-4 [p=S0@0-0; c=S9@0-4]
END_OF_SET4

my $input_length = 4;
EARLEME: for my $earleme ( 0 .. $input_length + 1 ) {
    my $current_earleme = my $furthest = my $last_completed =
        List::Util::min( $earleme, $input_length );
    Marpa::Test::is(
        $recce->show_earley_sets(1),
        "Last Completed: $last_completed; Furthest: $furthest\n"
            . join( q{}, @set[ 0 .. $furthest ] ),
        "Aycock/Horspool Parse Status at earleme $earleme"
    );
    given ($earleme) {
        when ($input_length) {
            $recce->end_input();
        }
        when ( $input_length + 1 ) {break}
        default {
            defined $recce->tokens( [ [ 'a', 'a', 1 ] ], )
                or Marpa::exception('Parsing exhausted');
        }
    } ## end given
} ## end for my $earleme ( 0 .. $input_length + 1 )

my @expected = ( q{}, qw[(a;;;) (a;a;;) (a;a;a;) (a;a;a;a)] );

for my $i ( 0 .. $input_length ) {
    my $evaler = Marpa::Evaluator->new(
        {   recce       => $recce,
            end         => $i,
            parse_order => 'original',
        }
    );
    my $result = $evaler->value();
    Test::More::is( ${$result}, $expected[$i], "parse permutation $i" );

} ## end for my $i ( 0 .. $input_length )

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
