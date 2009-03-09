#!perl

use 5.010;
use strict;
use warnings;

use Test::More tests => 9;

use lib 'lib';
use lib 't/lib';
use Marpa::Test;
use Carp;

BEGIN {
	use_ok( 'Marpa' );
}

# A grammar from Grune & Jacobs, Parsing Techniques: A Practical Guide, pp 206-207.
# The book is available on the web.

my $g = new Marpa::Grammar({
    precompute => 0,
    start => q{S'},
    strip => 0,
    rules => [
        [ q{S'}, [qw/S $/] ],
        [ 'S',  [qw/E/] ],
        [ 'E',  [qw/E - T/] ],
        [ 'E',  [qw/T/] ],
        [ 'T',  [qw/n/] ],
        [ 'T',  [qw/( E )/] ],
    ],
    academic => 1,
});

$g->set({
    terminals => [
        [ 'n' => { regex => qr/n/xms } ],
        [ q{$} => { regex => qr/\$/xms } ],
        [ ')' => { regex => qr/\)/xms } ],
        [ '(' => { regex => qr/\(/xms } ],
        [ q{-} => { regex => qr/\-/xms } ],
    ],
});

$g->precompute();

Marpa::Test::is($g->show_rules(), <<'EOS', 'Grune/Jacobs Rules');
0: S' -> S $
1: S -> E
2: E -> E - T
3: E -> T
4: T -> n
5: T -> ( E )
EOS

Marpa::Test::is($g->show_symbols(), <<'EOS', 'Grune/Jacobs Symbols');
0: S', lhs=[0] rhs=[]
1: S, lhs=[1] rhs=[0]
2: $, lhs=[] rhs=[0] terminal
3: E, lhs=[2 3] rhs=[1 2 5]
4: -, lhs=[] rhs=[2] terminal
5: T, lhs=[4 5] rhs=[2 3]
6: n, lhs=[] rhs=[4] terminal
7: (, lhs=[] rhs=[5] terminal
8: ), lhs=[] rhs=[5] terminal
EOS

Marpa::Test::is($g->show_nullable_symbols(), q{}, 'Grune/Jacobs Nullable Symbols');
Marpa::Test::is($g->show_nulling_symbols(), q{}, 'Grune/Jacobs Nulling Symbols');
Marpa::Test::is($g->show_productive_symbols(), '$ ( ) - E S S\' T n', 'Grune/Jacobs Productive Symbols');
Marpa::Test::is($g->show_accessible_symbols(), '$ ( ) - E S S\' T n', 'Grune/Jacobs Accessible Symbols');
Marpa::Test::is($g->show_NFA(), <<'EOS', 'Grune/Jacobs NFA');
S0: /* empty */
 empty => S1
S1: S' ::= . S $
 empty => S4
 <S> => S2
S2: S' ::= S . $
 <$> => S3
S3: S' ::= S $ .
S4: S ::= . E
 empty => S6 S10
 <E> => S5
S5: S ::= E .
S6: E ::= . E - T
 empty => S6 S10
 <E> => S7
S7: E ::= E . - T
 <-> => S8
S8: E ::= E - . T
 empty => S12 S14
 <T> => S9
S9: E ::= E - T .
S10: E ::= . T
 empty => S12 S14
 <T> => S11
S11: E ::= T .
S12: T ::= . n
 <n> => S13
S13: T ::= n .
S14: T ::= . ( E )
 <(> => S15
S15: T ::= ( . E )
 empty => S6 S10
 <E> => S16
S16: T ::= ( E . )
 <)> => S17
S17: T ::= ( E ) .
EOS

Marpa::Test::is( $g->show_ii_QDFA(), <<'EOS', 'Grune/Jacobs QDFA');
Start States: St0; St9
St0: 1
S' ::= . S $
 <S> => St7
St1: 11
E ::= T .
St2: predict; 12,14
T ::= . n
T ::= . ( E )
 <(> => St11; St4
 <n> => St3
St3: 13
T ::= n .
St4: 15
T ::= ( . E )
 <E> => St5
St5: 16
T ::= ( E . )
 <)> => St6
St6: 17
T ::= ( E ) .
St7: 2
S' ::= S . $
 <$> => St8
St8: 3
S' ::= S $ .
St9: predict; 4,6,10,12,14
S ::= . E
E ::= . E - T
E ::= . T
T ::= . n
T ::= . ( E )
 <(> => St11; St4
 <E> => St10
 <T> => St1
 <n> => St3
St10: 5,7
S ::= E .
E ::= E . - T
 <-> => St13; St2
St11: predict; 6,10,12,14
E ::= . E - T
E ::= . T
T ::= . n
T ::= . ( E )
 <(> => St11; St4
 <E> => St12
 <T> => St1
 <n> => St3
St12: 7
E ::= E . - T
 <-> => St13; St2
St13: 8
E ::= E - . T
 <T> => St14
St14: 9
E ::= E - T .
EOS

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
