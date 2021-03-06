#!perl
# An ambiguous equation

use 5.010;
use strict;
use warnings;

use Test::More tests => 13;

use lib 'lib';
use Marpa::Test;
use English qw( -no_match_vars );
use Fatal qw( close open );

BEGIN {
    Test::More::use_ok('Marpa');
}

## no critic (InputOutput::RequireBriefOpen)
open my $original_stdout, q{>&STDOUT};
## use critic

sub save_stdout {
    my $save;
    my $save_ref = \$save;
    close STDOUT;
    open STDOUT, q{>}, $save_ref;
    return $save_ref;
} ## end sub save_stdout

sub restore_stdout {
    close STDOUT;
    open STDOUT, q{>&}, $original_stdout;
    return 1;
}

## no critic (Subroutines::RequireArgUnpacking)

sub do_op {
    shift;
    my ( $right_string, $right_value ) = ( $_[2] =~ /^(.*)==(.*)$/xms );
    my ( $left_string,  $left_value )  = ( $_[0] =~ /^(.*)==(.*)$/xms );
    my $op = $_[1];
    my $value;
    if ( $op eq q{+} ) {
        $value = $left_value + $right_value;
    }
    elsif ( $op eq q{*} ) {
        $value = $left_value * $right_value;
    }
    elsif ( $op eq q{-} ) {
        $value = $left_value - $right_value;
    }
    else {
        Marpa::exception("Unknown op: $op");
    }
    return '(' . $left_string . $op . $right_string . ')==' . $value;
} ## end sub do_op

sub number {
    shift;
    my $v0 = pop @_;
    return $v0 . q{==} . $v0;
}

sub default_action {
    shift;
    my $v_count = scalar @_;
    return q{}   if $v_count <= 0;
    return $_[0] if $v_count == 1;
    return '(' . join( q{;}, @_ ) . ')';
} ## end sub default_action

my $grammar = Marpa::Grammar->new(
    {   start   => 'E',
        strip   => 0,
        actions => 'main',
        rules   => [
            [ 'E', [qw/E Op E/], 'do_op' ],
            [ 'E', [qw/Number/], 'number' ],
        ],
        default_action => 'default_action',
    }
);

$grammar->precompute();

my $actual_ref;
$actual_ref = save_stdout();

# Marpa::Display
# name: show_symbols Synopsis

print $grammar->show_symbols()
    or die "print failed: $ERRNO";

# Marpa::Display::End

restore_stdout();

Marpa::Test::is( ${$actual_ref},
    <<'END_SYMBOLS', 'Ambiguous Equation Symbols' );
0: E, lhs=[0 1] rhs=[0 2] terminal
1: Op, lhs=[] rhs=[0] terminal
2: Number, lhs=[] rhs=[1] terminal
3: E['], lhs=[2] rhs=[]
END_SYMBOLS

$actual_ref = save_stdout();

# Marpa::Display
# name: show_rules Synopsis

print $grammar->show_rules()
    or die "print failed: $ERRNO";

# Marpa::Display::End

Marpa::Test::is( ${$actual_ref}, <<'END_RULES', 'Ambiguous Equation Rules' );
0: E -> E Op E
1: E -> Number
2: E['] -> E /* vlhs real=1 */
END_RULES

$actual_ref = save_stdout();

print $grammar->show_NFA()
    or die "print failed: $ERRNO";

Marpa::Test::is( ${$actual_ref}, <<'END_NFA', 'Ambiguous Equation NFA' );
S0: /* empty */
 empty => S7
S1: E -> . E Op E
 empty => S1 S5
 <E> => S2
S2: E -> E . Op E
 <Op> => S3
S3: E -> E Op . E
 empty => S1 S5
 <E> => S4
S4: E -> E Op E .
S5: E -> . Number
 <Number> => S6
S6: E -> Number .
S7: E['] -> . E
 empty => S1 S5
 <E> => S8
S8: E['] -> E .
END_NFA

$actual_ref = save_stdout();

# Marpa::Display
# name: show_AHFA Synopsis

print $grammar->show_AHFA()
    or die "print failed: $ERRNO";

# Marpa::Display::End

Marpa::Test::is( ${$actual_ref}, <<'END_AHFA', 'Ambiguous Equation AHFA' );
Start States: S0; S1
S0: 7
E['] -> . E
 <E> => S2; leo(E['])
S1: predict; 1,5
E -> . E Op E
E -> . Number
 <E> => S3
 <Number> => S4
S2: leo-c; 8
E['] -> E .
S3: 2
E -> E . Op E
 <Op> => S1; S5
S4: 6
E -> Number .
S5: 3
E -> E Op . E
 <E> => S6; leo(E)
S6: leo-c; 4
E -> E Op E .
END_AHFA

$actual_ref = save_stdout();

# Marpa::Display
# name: show_problems Synopsis

print $grammar->show_problems()
    or die "print failed: $ERRNO";

# Marpa::Display::End

Marpa::Test::is(
    ${$actual_ref},
    "Grammar has no problems\n",
    'Ambiguous Equation Problems'
);

restore_stdout();

my $recce = Marpa::Recognizer->new( { grammar => $grammar } );

$recce->tokens(
    [   [ 'Number', 2,    1 ],
        [ 'Op',     q{-}, 1 ],
        [ 'Number', 0,    1 ],
        [ 'Op',     q{*}, 1 ],
        [ 'Number', 3,    1 ],
        [ 'Op',     q{+}, 1 ],
        [ 'Number', 1,    1 ],
    ]
);

$actual_ref = save_stdout();

# Marpa::Display
# name: show_earley_sets Synopsis

print $recce->show_earley_sets()
    or die "print failed: $ERRNO";

# Marpa::Display::End

Marpa::Test::is( ${$actual_ref},
    <<'END_OF_EARLEY_SETS', 'Ambiguous Equation Earley Sets' );
Last Completed: 7; Furthest: 7
Earley Set 0
S0@0-0
S1@0-0
Earley Set 1
S4@0-1 [p=S1@0-0; s=Number; t=\2]
S2@0-1 [p=S0@0-0; c=S4@0-1]
S3@0-1 [p=S1@0-0; c=S4@0-1]
Earley Set 2
S5@0-2 [p=S3@0-1; s=Op; t=\'-']
S1@2-2
Earley Set 3
S4@2-3 [p=S1@2-2; s=Number; t=\0]
S6@0-3 [p=S5@0-2; c=S4@2-3]
S3@2-3 [p=S1@2-2; c=S4@2-3]
S2@0-3 [p=S0@0-0; c=S6@0-3]
S3@0-3 [p=S1@0-0; c=S6@0-3]
Earley Set 4
S5@2-4 [p=S3@2-3; s=Op; t=\'*']
S1@4-4
S5@0-4 [p=S3@0-3; s=Op; t=\'*']
Earley Set 5
S4@4-5 [p=S1@4-4; s=Number; t=\3]
S6@2-5 [p=S5@2-4; c=S4@4-5]
S3@4-5 [p=S1@4-4; c=S4@4-5]
S6@0-5 [p=S5@0-4; c=S4@4-5] [p=S5@0-2; c=S6@2-5]
S3@2-5 [p=S1@2-2; c=S6@2-5]
S2@0-5 [p=S0@0-0; c=S6@0-5]
S3@0-5 [p=S1@0-0; c=S6@0-5]
Earley Set 6
S5@4-6 [p=S3@4-5; s=Op; t=\'+']
S1@6-6
S5@2-6 [p=S3@2-5; s=Op; t=\'+']
S5@0-6 [p=S3@0-5; s=Op; t=\'+']
Earley Set 7
S4@6-7 [p=S1@6-6; s=Number; t=\1]
S6@4-7 [p=S5@4-6; c=S4@6-7]
S3@6-7 [p=S1@6-6; c=S4@6-7]
S6@2-7 [p=S5@2-6; c=S4@6-7] [p=S5@2-4; c=S6@4-7]
S6@0-7 [p=S5@0-6; c=S4@6-7] [p=S5@0-4; c=S6@4-7] [p=S5@0-2; c=S6@2-7]
S3@4-7 [p=S1@4-4; c=S6@4-7]
S3@2-7 [p=S1@2-2; c=S6@2-7]
S2@0-7 [p=S0@0-0; c=S6@0-7]
S3@0-7 [p=S1@0-0; c=S6@0-7]
END_OF_EARLEY_SETS

restore_stdout();

$actual_ref = save_stdout();

# Marpa::Display
# name: show_progress Synopsis

print $recce->show_progress()
    or die "print failed: $ERRNO";

# Marpa::Display::End

Marpa::Test::is( ${$actual_ref},
    <<'END_OF_PROGRESS_REPORT', 'Ambiguous Equation Progress Report' );
BUILDING @0-7 E -> E . Op E
BUILDING @2-7 E -> E . Op E
BUILDING @4-7 E -> E . Op E
BUILDING @6-7 E -> E . Op E
COMPLETED @0-7 0: E -> E Op E
COMPLETED @2-7 0: E -> E Op E
COMPLETED @4-7 0: E -> E Op E
COMPLETED @6-7 1: E -> Number
COMPLETED @0-7 2: E['] -> E
END_OF_PROGRESS_REPORT

restore_stdout();

my %expected_value = (
    '(2-(0*(3+1)))==2' => 1,
    '(((2-0)*3)+1)==7' => 1,
    '((2-(0*3))+1)==3' => 1,
    '((2-0)*(3+1))==8' => 1,
    '(2-((0*3)+1))==1' => 1,
);
my $evaler = Marpa::Evaluator->new(
    {   recce => $recce,

        # Set max at 10 just in case there's an infinite loop.
        # This is for debugging, after all
        max_parses => 10,
    }
);
Marpa::exception('Parse failed') if not $evaler;

my $i = 0;
while ( defined( my $value = $evaler->value() ) ) {
    my $value = ${$value};
    Test::More::ok( $expected_value{$value}, "Value $i (unspecified order)" );
    delete $expected_value{$value};
    $i++;
} ## end while ( defined( my $value = $evaler->value() ) )

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
