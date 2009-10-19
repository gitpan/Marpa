#!perl
# Ensure various coding errors are caught

use 5.010;
use strict;
use warnings;

use Test::More tests => 22;

use lib 'lib';
use t::lib::Marpa::Test;
use English qw( -no_match_vars );

# use Smart::Comments;

BEGIN {
    Test::More::use_ok('Marpa');
    Test::More::use_ok('Marpa::MDLex');
}

my @uncompiled_features = qw(
    e_op_action default_action
    null_action
);

my @compiled_features = qw(
    unstringify_grammar
    unstringify_recce
);

my @features = ( @compiled_features, @uncompiled_features );

my @compile_tests = ( 'compile phase warning', 'compile phase fatal', );
my @runtime_tests =
    ( 'run phase warning', 'run phase error', 'run phase die', );

my @tests = ( @compile_tests, @runtime_tests );

my %good_code = (
    'e_op_action'     => 'main::e_op_action',
    'e_number_action' => 'main::e_number_action',
    'default_action'  => 'main::default_action',
);

# Code to produce a run phase warning
sub run_phase_warning {
    my $x;
    ## no critic (ErrorHandling::RequireCarping)
    warn 'Test Warning 1';
    warn 'Test Warning 2';
    $x++;
    return 1;
} ## end sub run_phase_warning

# Code to produce a run phase error
sub run_phase_error {
    my $x = 0;
    $x = 1 / 0;
    return $x++;
}

# Code to produce a run phase die()
sub run_phase_die {
    my $x = 0;
    ## no critic (ErrorHandling::RequireCarping)
    die 'test call to die';
}

my %test_arg;
my %expected;
for my $test (@tests) {
    for my $compiled_feature (@compiled_features) {
        $test_arg{$test}{$compiled_feature} = '1;';
        $expected{$test}{$compiled_feature} = q{};
    }
} ## end for my $test (@tests)
for my $runtime_test (@runtime_tests) {
    for my $uncompiled_feature (@uncompiled_features) {
        $test_arg{$runtime_test}{$uncompiled_feature} = '1;';
        $expected{$runtime_test}{$uncompiled_feature} = q{};
    }
} ## end for my $runtime_test (@runtime_tests)

for my $uncompiled_feature (@uncompiled_features) {
    $test_arg{'run phase warning'}{$uncompiled_feature} =
        'main::run_phase_warning';
    $test_arg{'run phase error'}{$uncompiled_feature} =
        'main::run_phase_error';
    $test_arg{'run phase die'}{$uncompiled_feature} = 'main::run_phase_die';
} ## end for my $uncompiled_feature (@uncompiled_features)

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
                Marpa::exception(
                    'Good code should no longer be in data section');
            }
            if ( $header =~ s/\A bad \s code \s //xms ) {
                chomp $header;
                Marpa::exception("test code given for unknown test: $header")
                    if not defined $test_arg{$header};
                for my $compiled_feature (@compiled_features) {
                    $test_arg{$header}{$compiled_feature} = $data;
                }
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
    $template =~ s{
            \b package \s
            Marpa [:][:] Internal [:][:] Recognizer [:][:]
            [EP] _ [0-9a-fA-F]+ [;] $
        }{package Marpa::<PACKAGE>;}xms;
    $template =~ s{ \s* at \s (\S*[/]|)code_diag[.]t \s line \s \d+}{}gxms;
    $template =~ s/[<]WHERE[>]/$where/xmsg;
    $template =~ s/[<]LONG_WHERE[>]/$long_where/xmsg;
    $template =~ s{ \s [<]DATA[>] \s line \s \d+
            }{ <DATA> line <LINE_NO>}xmsg;
    $template =~ s{
            \s at \s [(] eval \s \d+ [)] \s line \s
            }{ at (eval <LINE_NO>) line }xmsg;
    return $template;
} ## end sub canonical

sub null_action { return '[null]' }

sub run_test {
    my $args = shift;

    my $e_op_action        = $good_code{e_op_action};
    my $e_number_action    = $good_code{e_number_action};
    my $default_action     = $good_code{default_action};
    my $null_action        = 'main::null_action';
    my $default_null_value = q{[default null]};

    ### e_op_action default: $e_op_action
    ### e_number_action default: $e_number_action

    while ( my ( $arg, $value ) = each %{$args} ) {
        given ( lc $arg ) {
            when ('e_op_action')     { $e_op_action     = $value }
            when ('e_number_action') { $e_number_action = $value }
            when ('default_action')  { $default_action  = $value }
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

    ### e_op_action: $e_op_action
    ### e_number_action: $e_number_action

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
            default_action     => $default_action,
            default_null_value => $default_null_value,
            terminals          => [qw(Number Op Text)],
        }
    );
    $grammar->precompute();

    my $recce = Marpa::Recognizer->new( { grammar => $grammar } );

    my $lexer = Marpa::MDLex->new(
        {   recce          => $recce,
            default_prefix => '\s*',
            terminals      => [
                [ 'Number', '\d+' ],
                [ 'Op',     '[-+*]' ],
                { name => 'Text', builtin => 'q_quote' },
            ],
        }
    );

    my $fail_offset = $lexer->text('2 - 0 * 3 + 1 q{trailer}');
    if ( $fail_offset >= 0 ) {
        Marpa::exception("Parse failed at offset $fail_offset");
    }

    $recce->end_input();

    my $expected = '((2-(0*(3+1)))==2; q{trailer};[default null];[null])';
    my $evaler = Marpa::Evaluator->new( { recce => $recce } );
    Carp::croak('Parse failed') if not defined $evaler;
    my $value = $evaler->value();
    Marpa::Test::is( ${$value}, $expected, 'Ambiguous Equation Value' );

    return 1;

}    # sub run_test

run_test( {} );

my %where = (
    e_op_action         => 'compiling action',
    default_action      => 'compiling action',
    null_action         => 'evaluating null value',
    unstringify_grammar => 'unstringifying grammar',
    unstringify_recce   => 'unstringifying recognizer',
);

my %long_where = (
    e_op_action    => 'compiling action for 1: E -> E Op E',
    default_action => 'compiling action for 3: optional_trailer1 -> trailer',
    null_action    => 'evaluating null value for optional_trailer2',
    unstringify_grammar => 'unstringifying grammar',
    unstringify_recce   => 'unstringifying recognizer',
);

for my $test (@tests) {
    FEATURE: for my $feature (@features) {
        next FEATURE if not defined $expected{$test}{$feature};
        my $test_name = "$test in $feature";
        if ( eval { run_test( { $feature => $test_arg{$test}{$feature}, } ); }
            )
        {
            Test::More::fail(
                "$test_name did not fail -- that shouldn't happen");
        } ## end if ( eval { run_test( { $feature => $test_arg{$test}...})})
        else {
            my $eval_error = $EVAL_ERROR;
            my $where      = $where{$feature};
            my $long_where = $long_where{$feature};
            Marpa::Test::is(
                canonical( $eval_error,                $where, $long_where ),
                canonical( $expected{$test}{$feature}, $where, $long_where ),
                $test_name
            );
        } ## end else [ if ( eval { run_test( { $feature => $test_arg{$test}...})})]
    } ## end for my $feature (@features)
} ## end for my $test (@tests)

## no critic (Subroutines::RequireArgUnpacking)

sub e_op_action {
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
} ## end sub e_op_action

sub e_number_action {
    shift;
    my $v0 = pop @_;
    return $v0 . q{==} . $v0;
}

sub default_action {
    shift;
    my $v_count = scalar @_;
    return q{}   if $v_count <= 0;
    return $_[0] if $v_count == 1;
    return '(' . join( q{;}, ( map { $_ // 'undef' } @_ ) ) . ')';
} ## end sub default_action

## use critic

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

| bad code compile phase fatal
# this should be a compile phase error
my $x = 0;
$x=;
$x++;
1;
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

| bad code run phase warning
# this should be a run phase warning
my $x = 0;
warn "Test Warning 1";
warn "Test Warning 2";
$x++;
1;
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
Warning #0 in computing value:
Test Warning 1, <DATA> line <LINE_NO>.
======
Warning #1 in computing value:
Test Warning 2, <DATA> line <LINE_NO>.
======
__END__

| expected default_action run phase warning
Fatal problem(s) in computing value for rule: 6: trailer -> Text
2 Warning(s)
Warning(s) treated as fatal problem
Warning #0 in computing value:
Test Warning 1, <DATA> line <LINE_NO>.
======
Warning #1 in computing value:
Test Warning 2, <DATA> line <LINE_NO>.
======
__END__

| expected null_action run phase warning
Fatal problem(s) in <LONG_WHERE>
2 Warning(s)
Warning(s) treated as fatal problem
Warning #0 in <WHERE>:
Test Warning 1, <DATA> line <LINE_NO>.
======
Warning #1 in <WHERE>:
Test Warning 2, <DATA> line <LINE_NO>.
======
__END__

| bad code run phase error
# this should be a run phase error
my $x = 0;
$x = 711/0;
$x++;
1;
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
Error in computing value:
Illegal division by zero, <DATA> line <LINE_NO>.
======
__END__

| expected default_action run phase error
Fatal problem(s) in computing value for rule: 6: trailer -> Text
Fatal Error
Error in computing value:
Illegal division by zero, <DATA> line <LINE_NO>.
======
__END__

| expected null_action run phase error
Fatal problem(s) in <LONG_WHERE>
Fatal Error
Error in <WHERE>:
Illegal division by zero, <DATA> line <LINE_NO>.
======
__END__

| bad code run phase die
# this is a call to die()
my $x = 0;
die('test call to die');
$x++;
1;
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
Error in computing value:
test call to die, <DATA> line <LINE_NO>.
======
__END__

| expected default_action run phase die
Fatal problem(s) in computing value for rule: 6: trailer -> Text
Fatal Error
Error in computing value:
test call to die, <DATA> line <LINE_NO>.
======
__END__

| expected null_action run phase die
Fatal problem(s) in <LONG_WHERE>
Fatal Error
Error in <WHERE>:
test call to die, <DATA> line <LINE_NO>.
======
__END__

