#!perl
# Ensure various coding errors are caught

use 5.010;
use strict;
use warnings;

use Test::More tests => 42;

use lib 'lib';
use t::lib::Marpa::Test;
use English qw( -no_match_vars );

BEGIN {
    Test::More::use_ok('Marpa');
}

my @features = qw(
    preamble lex_preamble
    e_op_action default_action
    lexer
    null_action
    unstringify_grammar
    unstringify_recce
);

my @tests = (
    'compile phase warning',
    'compile phase fatal',
    'run phase warning',
    'run phase error',
    'run phase die',
);

my %good_code = (
    ## no critic (ValuesAndExpressions::RequireInterpolationOfMetachars)
    'e op action'     => 'my $error =',
    'e number action' => 'my $error =',
    'default action'  => 'my $error =',
    ## use critic
);

my %test_code;
my %expected;
for my $test (@tests) {
    $test_code{$test} = '1;';
    for my $feature (@features) {
        $expected{$test}{$feature} = q{};
    }
} ## end for my $test (@tests)

my $getting_headers = 1;
my @headers;
my $data = q{};

LINE: while ( my $line = <DATA> ) {

    if ($getting_headers) {
        next LINE if $line =~ m/ \A \s* \Z/xms;
        if ( $line =~ s/ \A [|] \s+ //xms ) {
            chomp $line;
            push @headers, $line;
            next LINE;
        }
        else {
            $getting_headers = 0;
            $data            = q{};
        }
    } ## end if ($getting_headers)

    # getting data

    if ( $line =~ /\A__END__\Z/xms ) {
        HEADER: while ( my $header = pop @headers ) {
            if ( $header =~ s/\A expected \s //xms ) {
                my ( $feature, $test ) =
                    ( $header =~ m/\A ([^\s]*) \s+ (.*) \Z/xms );
                Marpa::exception(
                    "expected result given for unknown test, feature: $test, $feature"
                ) if not defined $expected{$test}{$feature};
                $expected{$test}{$feature} = $data;
                next HEADER;
            } ## end if ( $header =~ s/\A expected \s //xms )
            if ( $header =~ s/\A good \s code \s //xms ) {
                chomp $header;
                $good_code{$header} = $data;
                next HEADER;
            }
            if ( $header =~ s/\A bad \s code \s //xms ) {
                chomp $header;
                Marpa::exception("test code given for unknown test: $header")
                    if not defined $test_code{$header};
                $test_code{$header} = $data;
                next HEADER;
            } ## end if ( $header =~ s/\A bad \s code \s //xms )
            Marpa::exception("Bad header: $header");
        }    # HEADER
        $getting_headers = 1;
        $data            = q{};
    }    # if $line

    $data .= $line;
} ## end while ( my $line = <DATA> )

sub canonical {
    my $template   = shift;
    my $where      = shift;
    my $long_where = shift;
    $long_where //= $where;
    $template =~ s{ \b package \s Marpa [:][:] [EP] _ [0-9a-fA-F]+ [;] $
        }{package Marpa::<PACKAGE>;}xms;
    $template =~ s/ \s* at \s [^\s]* code_diag[.]t \s line  \s \d+\Z//xms;
    $template =~ s/[<]WHERE[>]/$where/xmsg;
    $template =~ s/[<]LONG_WHERE[>]/$long_where/xmsg;
    $template =~ s{ \s [<]DATA[>] \s line \s \d+
            }{ <DATA> line <LINE_NO>}xmsg;
    $template =~ s{
            \s at \s [(] eval \s \d+ [)] \s line \s
            }{ at (eval <LINE_NO>) line }xmsg;
    return $template;
} ## end sub canonical

sub run_test {
    my $args = shift;

    my $e_op_action        = $good_code{e_op_action};
    my $e_number_action    = $good_code{e_number_action};
    my $preamble           = q{1};
    my $lex_preamble       = q{1};
    my $default_action     = $good_code{default_action};
    my $text_lexer         = 'lex_q_quote';
    my $null_action        = q{ '[null]' };
    my $default_null_value = q{[default null]};

    while ( my ( $arg, $value ) = each %{$args} ) {
        given ( lc $arg ) {
            when ('e_op_action')     { $e_op_action     = $value }
            when ('e_number_action') { $e_number_action = $value }
            when ('default_action')  { $default_action  = $value }
            when ('lex_preamble')    { $lex_preamble    = $value }
            when ('preamble')        { $preamble        = $value }
            when ('lexer')           { $text_lexer      = $value }
            when ('null_action')     { $null_action     = $value }
            when ('unstringify_grammar') {
                return Marpa::Grammar::unstringify( \$value );
            }
            when ('unstringify_recce') {
                return Marpa::Recognizer::unstringify( \$value );
            }
            default {
                Marpa::exception("unknown argument to run_test: $arg");
            };
        } ## end given
    } ## end while ( my ( $arg, $value ) = each %{$args} )

    my $grammar = Marpa::Grammar->new(
        {   start => 'S',
            rules => [
                [ 'S', [qw/E trailer optional_trailer1 optional_trailer2/], ],
                [ 'E', [qw/E Op E/], $e_op_action, ],
                [ 'E', [qw/Number/], $e_number_action, ],
                [ 'optional_trailer1', [qw/trailer/], ],
                [ 'optional_trailer1', [], ],
                [ 'optional_trailer2', [], $null_action ],
                [ 'trailer',           [qw/Text/], ],
            ],
            terminals => [
                [ 'Number' => { regex  => qr/\d+/xms } ],
                [ 'Op'     => { regex  => qr/[-+*]/xms } ],
                [ 'Text'   => { action => $text_lexer } ],
            ],
            default_action     => $default_action,
            preamble           => $preamble,
            lex_preamble       => $lex_preamble,
            default_lex_prefix => '\s*',
            default_null_value => $default_null_value,
        }
    );

    my $recce = Marpa::Recognizer->new( { grammar => $grammar } );

    my $fail_offset = $recce->text('2 - 0 * 3 + 1 q{trailer}');
    if ( $fail_offset >= 0 ) {
        Marpa::exception("Parse failed at offset $fail_offset");
    }

    $recce->end_input();

    my $expected = '((2-(0*(3+1)))==2; q{trailer};[default null];[null])';
    my $evaler   = Marpa::Evaluator->new( { recce => $recce } );
    my $value    = $evaler->value();
    Marpa::Test::is( ${$value}, $expected, 'Ambiguous Equation Value' );

    return 1;

}    # sub run_test

run_test( {} );

my %where = (
    preamble            => 'evaluating preamble',
    lex_preamble        => 'evaluating lex preamble',
    e_op_action         => 'compiling action',
    default_action      => 'compiling action',
    null_action         => 'evaluating null value',
    lexer               => 'compiling lexer',
    unstringify_grammar => 'unstringifying grammar',
    unstringify_recce   => 'unstringifying recognizer',
);

my %long_where = (
    preamble       => 'evaluating preamble',
    lex_preamble   => 'evaluating lex preamble',
    e_op_action    => 'compiling action for 1: E -> E Op E',
    default_action => 'compiling action for 3: optional_trailer1 -> trailer',
    null_action    => 'evaluating null value for optional_trailer2',
    lexer          => 'compiling lexer for Text',
    unstringify_grammar => 'unstringifying grammar',
    unstringify_recce   => 'unstringifying recognizer',
);

for my $test (@tests) {
    for my $feature (@features) {
        my $test_name = "$test in $feature";
        if ( eval { run_test( { $feature => $test_code{$test}, } ); } ) {
            Test::More::fail(
                "$test_name did not fail -- that shouldn't happen");
        }
        else {
            my $eval_error = $EVAL_ERROR;
            my $where      = $where{$feature};
            my $long_where = $long_where{$feature};
            Marpa::Test::is(
                canonical( $eval_error,                $where, $long_where ),
                canonical( $expected{$test}{$feature}, $where, $long_where ),
                $test_name
            );
        } ## end else [ if ( eval { run_test( { $feature => $test_code{$test...}})})]
    } ## end for my $feature (@features)
} ## end for my $test (@tests)

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:

__DATA__
| bad code compile phase warning
# this should be a compile phase warning
my $x = 0;
my $x = 1;
my $x = 2;
$x++;
1;
__END__

| expected preamble compile phase warning
| expected lex_preamble compile phase warning
Fatal problem(s) in <LONG_WHERE>
2 Warning(s)
Warning(s) treated as fatal problem
7 lines in problem code, last warning occurred here:
2: # this should be a compile phase warning
3: my $x = 0;
*4: my $x = 1;
*5: my $x = 2;
6: $x++;
7: 1;
======
Warning #0 in <WHERE>:
"my" variable $x masks earlier declaration in same scope at (eval <LINE_NO>) line 4, <DATA> line 1.
======
Warning #1 in <WHERE>:
"my" variable $x masks earlier declaration in same scope at (eval <LINE_NO>) line 5, <DATA> line 1.
======
__END__

| expected unstringify_grammar compile phase warning
| expected unstringify_recce compile phase warning
Fatal problem(s) in <LONG_WHERE>
2 Warning(s)
Warning(s) treated as fatal problem
6 lines in problem code, last warning occurred here:
1: # this should be a compile phase warning
2: my $x = 0;
*3: my $x = 1;
*4: my $x = 2;
5: $x++;
6: 1;
======
Warning #0 in <WHERE>:
"my" variable $x masks earlier declaration in same scope at (eval <LINE_NO>) line 3, <DATA> line 1.
======
Warning #1 in <WHERE>:
"my" variable $x masks earlier declaration in same scope at (eval <LINE_NO>) line 4, <DATA> line 1.
======
__END__

| expected e_op_action compile phase warning
| expected default_action compile phase warning
Fatal problem(s) in <LONG_WHERE>
2 Warning(s)
Warning(s) treated as fatal problem
9 lines in problem code, last warning occurred here:
3: # this should be a compile phase warning
4: my $x = 0;
*5: my $x = 1;
*6: my $x = 2;
7: $x++;
8: 1;
9: }
======
Warning #0 in <WHERE>:
"my" variable $x masks earlier declaration in same scope at (eval <LINE_NO>) line 5, <DATA> line 1.
======
Warning #1 in <WHERE>:
"my" variable $x masks earlier declaration in same scope at (eval <LINE_NO>) line 6, <DATA> line 1.
======
__END__

| expected null_action compile phase warning
Fatal problem(s) in <LONG_WHERE>
2 Warning(s)
Warning(s) treated as fatal problem
10 lines in problem code, last warning occurred here:
3: # this should be a compile phase warning
4: my $x = 0;
*5: my $x = 1;
*6: my $x = 2;
7: $x++;
8: 1;
9: };
======
Warning #0 in <WHERE>:
"my" variable $x masks earlier declaration in same scope at (eval <LINE_NO>) line 5, <DATA> line 1.
======
Warning #1 in <WHERE>:
"my" variable $x masks earlier declaration in same scope at (eval <LINE_NO>) line 6, <DATA> line 1.
======
__END__

| expected lexer compile phase warning
Fatal problem(s) in <LONG_WHERE>
2 Warning(s)
Warning(s) treated as fatal problem
13 lines in problem code, last warning occurred here:
5:     # this should be a compile phase warning
6: my $x = 0;
*7: my $x = 1;
*8: my $x = 2;
9: $x++;
10: 1;
11: ;
======
Warning #0 in <WHERE>:
"my" variable $x masks earlier declaration in same scope at (eval <LINE_NO>) line 7, <DATA> line 1.
======
Warning #1 in <WHERE>:
"my" variable $x masks earlier declaration in same scope at (eval <LINE_NO>) line 8, <DATA> line 1.
======
__END__

| bad code compile phase fatal
# this should be a compile phase error
my $x = 0;
$x=;
$x++;
1;
__END__

| expected preamble compile phase fatal
| expected lex_preamble compile phase fatal
Fatal problem(s) in <LONG_WHERE>
Fatal Error
6 lines in problem code, beginning:
1: package Marpa::<PACKAGE>;
2: # this should be a compile phase error
3: my $x = 0;
4: $x=;
5: $x++;
6: 1;
======
Error in <WHERE>:
syntax error at (eval <LINE_NO>) line 4, at EOF
======
__END__

| expected unstringify_grammar compile phase fatal
| expected unstringify_recce compile phase fatal
Fatal problem(s) in <LONG_WHERE>
Fatal Error
5 lines in problem code, beginning:
1: # this should be a compile phase error
2: my $x = 0;
3: $x=;
4: $x++;
5: 1;
======
Error in <WHERE>:
syntax error at (eval <LINE_NO>) line 3, at EOF
======
__END__

| expected e_op_action compile phase fatal
| expected default_action compile phase fatal
Fatal problem(s) in <LONG_WHERE>
Fatal Error
8 lines in problem code, beginning:
1: sub {
2:     package Marpa::<PACKAGE>;
3: # this should be a compile phase error
4: my $x = 0;
5: $x=;
6: $x++;
7: 1;
======
Error in <WHERE>:
syntax error at (eval <LINE_NO>) line 5, at EOF
======
__END__

| expected null_action compile phase fatal
Fatal problem(s) in <LONG_WHERE>
Fatal Error
9 lines in problem code, beginning:
1: $null_value = do {
2:     package Marpa::<PACKAGE>;
3: # this should be a compile phase error
4: my $x = 0;
5: $x=;
6: $x++;
7: 1;
======
Error in <WHERE>:
syntax error at (eval <LINE_NO>) line 5, at EOF
======
__END__

| expected lexer compile phase fatal
Fatal problem(s) in <LONG_WHERE>
Fatal Error
12 lines in problem code, beginning:
1: sub {
2:     my $STRING = shift;
3:     my $START = shift;
4:     package Marpa::<PACKAGE>;
5:     # this should be a compile phase error
6: my $x = 0;
7: $x=;
======
Error in <WHERE>:
syntax error at (eval <LINE_NO>) line 7, at EOF
======
__END__

| bad code run phase warning
# this should be a run phase warning
my $x = 0;
warn "Test Warning 1";
warn "Test Warning 2";
$x++;
1;
__END__

| expected preamble run phase warning
| expected lex_preamble run phase warning
Fatal problem(s) in <LONG_WHERE>
2 Warning(s)
Warning(s) treated as fatal problem
7 lines in problem code, last warning occurred here:
2: # this should be a run phase warning
3: my $x = 0;
*4: warn "Test Warning 1";
*5: warn "Test Warning 2";
6: $x++;
7: 1;
======
Warning #0 in <WHERE>:
Test Warning 1 at (eval <LINE_NO>) line 4, <DATA> line <LINE_NO>.
======
Warning #1 in <WHERE>:
Test Warning 2 at (eval <LINE_NO>) line 5, <DATA> line <LINE_NO>.
======
__END__

| expected unstringify_grammar run phase warning
| expected unstringify_recce run phase warning
Fatal problem(s) in <LONG_WHERE>
2 Warning(s)
Warning(s) treated as fatal problem
6 lines in problem code, last warning occurred here:
1: # this should be a run phase warning
2: my $x = 0;
*3: warn "Test Warning 1";
*4: warn "Test Warning 2";
5: $x++;
6: 1;
======
Warning #0 in <WHERE>:
Test Warning 1 at (eval <LINE_NO>) line 3, <DATA> line <LINE_NO>.
======
Warning #1 in <WHERE>:
Test Warning 2 at (eval <LINE_NO>) line 4, <DATA> line <LINE_NO>.
======
__END__

| expected e_op_action run phase warning
Fatal problem(s) in computing value for rule: 1: E -> E Op E
2 Warning(s)
Warning(s) treated as fatal problem
9 lines in problem code, last warning occurred here:
3: # this should be a run phase warning
4: my $x = 0;
*5: warn "Test Warning 1";
*6: warn "Test Warning 2";
7: $x++;
8: 1;
9: }
======
Warning #0 in computing value:
Test Warning 1 at (eval <LINE_NO>) line 5, <DATA> line <LINE_NO>.
======
Warning #1 in computing value:
Test Warning 2 at (eval <LINE_NO>) line 6, <DATA> line <LINE_NO>.
======
__END__

| expected default_action run phase warning
Fatal problem(s) in computing value for rule: 6: trailer -> Text
2 Warning(s)
Warning(s) treated as fatal problem
9 lines in problem code, last warning occurred here:
3: # this should be a run phase warning
4: my $x = 0;
*5: warn "Test Warning 1";
*6: warn "Test Warning 2";
7: $x++;
8: 1;
9: }
======
Warning #0 in computing value:
Test Warning 1 at (eval <LINE_NO>) line 5, <DATA> line <LINE_NO>.
======
Warning #1 in computing value:
Test Warning 2 at (eval <LINE_NO>) line 6, <DATA> line <LINE_NO>.
======
__END__

| expected null_action run phase warning
Fatal problem(s) in <LONG_WHERE>
2 Warning(s)
Warning(s) treated as fatal problem
10 lines in problem code, last warning occurred here:
3: # this should be a run phase warning
4: my $x = 0;
*5: warn "Test Warning 1";
*6: warn "Test Warning 2";
7: $x++;
8: 1;
9: };
======
Warning #0 in <WHERE>:
Test Warning 1 at (eval <LINE_NO>) line 5, <DATA> line <LINE_NO>.
======
Warning #1 in <WHERE>:
Test Warning 2 at (eval <LINE_NO>) line 6, <DATA> line <LINE_NO>.
======
__END__

| expected lexer run phase warning
Fatal problem(s) in user supplied lexer for Text at 1
2 Warning(s)
Warning(s) treated as fatal problem
13 lines in problem code, last warning occurred here:
5:     # this should be a run phase warning
6: my $x = 0;
*7: warn "Test Warning 1";
*8: warn "Test Warning 2";
9: $x++;
10: 1;
11: ;
======
Warning #0 in user supplied lexer:
Test Warning 1 at (eval <LINE_NO>) line 7, <DATA> line <LINE_NO>.
======
Warning #1 in user supplied lexer:
Test Warning 2 at (eval <LINE_NO>) line 8, <DATA> line <LINE_NO>.
======
__END__

| bad code run phase error
# this should be a run phase error
my $x = 0;
$x = 711/0;
$x++;
1;
__END__

| expected preamble run phase error
| expected lex_preamble run phase error
Fatal problem(s) in <LONG_WHERE>
Fatal Error
6 lines in problem code, beginning:
1: package Marpa::<PACKAGE>;
2: # this should be a run phase error
3: my $x = 0;
4: $x = 711/0;
5: $x++;
6: 1;
======
Error in <WHERE>:
Illegal division by zero at (eval <LINE_NO>) line 4, <DATA> line <LINE_NO>.
======
__END__

| expected unstringify_grammar run phase error
| expected unstringify_recce run phase error
Fatal problem(s) in <LONG_WHERE>
Fatal Error
5 lines in problem code, beginning:
1: # this should be a run phase error
2: my $x = 0;
3: $x = 711/0;
4: $x++;
5: 1;
======
Error in <WHERE>:
Illegal division by zero at (eval <LINE_NO>) line 3, <DATA> line <LINE_NO>.
======
__END__

| expected e_op_action run phase error
Fatal problem(s) in computing value for rule: 1: E -> E Op E
Fatal Error
8 lines in problem code, beginning:
1: sub {
2:     package Marpa::<PACKAGE>;
3: # this should be a run phase error
4: my $x = 0;
5: $x = 711/0;
6: $x++;
7: 1;
======
Error in computing value:
Illegal division by zero at (eval <LINE_NO>) line 5, <DATA> line <LINE_NO>.
======
__END__

| expected default_action run phase error
Fatal problem(s) in computing value for rule: 6: trailer -> Text
Fatal Error
8 lines in problem code, beginning:
1: sub {
2:     package Marpa::<PACKAGE>;
3: # this should be a run phase error
4: my $x = 0;
5: $x = 711/0;
6: $x++;
7: 1;
======
Error in computing value:
Illegal division by zero at (eval <LINE_NO>) line 5, <DATA> line <LINE_NO>.
======
__END__

| expected null_action run phase error
Fatal problem(s) in <LONG_WHERE>
Fatal Error
9 lines in problem code, beginning:
1: $null_value = do {
2:     package Marpa::<PACKAGE>;
3: # this should be a run phase error
4: my $x = 0;
5: $x = 711/0;
6: $x++;
7: 1;
======
Error in <WHERE>:
Illegal division by zero at (eval <LINE_NO>) line 5, <DATA> line <LINE_NO>.
======
__END__

| expected lexer run phase error
Fatal problem(s) in user supplied lexer for Text at 1
Fatal Error
12 lines in problem code, beginning:
1: sub {
2:     my $STRING = shift;
3:     my $START = shift;
4:     package Marpa::<PACKAGE>;
5:     # this should be a run phase error
6: my $x = 0;
7: $x = 711/0;
======
Error in user supplied lexer:
Illegal division by zero at (eval <LINE_NO>) line 7, <DATA> line <LINE_NO>.
======
__END__

| bad code run phase die
# this is a call to die()
my $x = 0;
die('test call to die');
$x++;
1;
__END__

| expected preamble run phase die
| expected lex_preamble run phase die
Fatal problem(s) in <LONG_WHERE>
Fatal Error
6 lines in problem code, beginning:
1: package Marpa::<PACKAGE>;
2: # this is a call to die()
3: my $x = 0;
4: die('test call to die');
5: $x++;
6: 1;
======
Error in <WHERE>:
test call to die at (eval <LINE_NO>) line 4, <DATA> line <LINE_NO>.
======
__END__

| expected unstringify_grammar run phase die
| expected unstringify_recce run phase die
Fatal problem(s) in <LONG_WHERE>
Fatal Error
5 lines in problem code, beginning:
1: # this is a call to die()
2: my $x = 0;
3: die('test call to die');
4: $x++;
5: 1;
======
Error in <WHERE>:
test call to die at (eval <LINE_NO>) line 3, <DATA> line <LINE_NO>.
======
__END__

| expected e_op_action run phase die
Fatal problem(s) in computing value for rule: 1: E -> E Op E
Fatal Error
8 lines in problem code, beginning:
1: sub {
2:     package Marpa::<PACKAGE>;
3: # this is a call to die()
4: my $x = 0;
5: die('test call to die');
6: $x++;
7: 1;
======
Error in computing value:
test call to die at (eval <LINE_NO>) line 5, <DATA> line <LINE_NO>.
======
__END__

| expected default_action run phase die
Fatal problem(s) in computing value for rule: 6: trailer -> Text
Fatal Error
8 lines in problem code, beginning:
1: sub {
2:     package Marpa::<PACKAGE>;
3: # this is a call to die()
4: my $x = 0;
5: die('test call to die');
6: $x++;
7: 1;
======
Error in computing value:
test call to die at (eval <LINE_NO>) line 5, <DATA> line <LINE_NO>.
======
__END__

| expected null_action run phase die
Fatal problem(s) in <LONG_WHERE>
Fatal Error
9 lines in problem code, beginning:
1: $null_value = do {
2:     package Marpa::<PACKAGE>;
3: # this is a call to die()
4: my $x = 0;
5: die('test call to die');
6: $x++;
7: 1;
======
Error in <WHERE>:
test call to die at (eval <LINE_NO>) line 5, <DATA> line <LINE_NO>.
======
__END__

| expected lexer run phase die
Fatal problem(s) in user supplied lexer for Text at 1
Fatal Error
12 lines in problem code, beginning:
1: sub {
2:     my $STRING = shift;
3:     my $START = shift;
4:     package Marpa::<PACKAGE>;
5:     # this is a call to die()
6: my $x = 0;
7: die('test call to die');
======
Error in user supplied lexer:
test call to die at (eval <LINE_NO>) line 7, <DATA> line <LINE_NO>.
======
__END__


| good code e_op_action
my ($right_string, $right_value)
    = ($_[2] =~ /^(.*)==(.*)$/);
my ($left_string, $left_value)
    = ($_[0] =~ /^(.*)==(.*)$/);
my $op = $_[1];
my $value;
if ($op eq '+') {
   $value = $left_value + $right_value;
} elsif ($op eq '*') {
   $value = $left_value * $right_value;
} elsif ($op eq '-') {
   $value = $left_value - $right_value;
} else {
   Marpa::exception("Unknown op: $op");
}
'(' . $left_string . $op . $right_string . ')==' . $value;
__END__

| good code e_number_action
my $v0 = pop @_;
$v0 . q{==} . $v0;
__END__

| good code default_action
my $v_count = scalar @_;
return q{} if $v_count <= 0;
return $_[0] if $v_count == 1;
'(' . join(q{;}, (map { $_ // 'undef' } @_)) . ')';
__END__

