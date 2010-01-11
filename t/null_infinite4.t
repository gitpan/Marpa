#!perl
# Two rules which start with nullables, and cycle.

use 5.010;
use strict;
use warnings;

use Test::More tests => 13;

use lib 'lib';
use Marpa::Test;

BEGIN {
    Test::More::use_ok('Marpa');
}

## no critic (Subroutines::RequireArgUnpacking)

sub default_action {
    shift;
    my $v_count = scalar @_;
    return q{} if $v_count <= 0;
    my @vals = map { $_ // q{-} } @_;
    return '(' . join( q{;}, @vals ) . ')';
} ## end sub default_action

sub null_a { return 'a' }
sub null_b { return 'b' }
sub null_c { return 'c' }
sub null_d { return 'd' }
sub null_e { return 'e' }
sub null_p { return 'p' }

sub rule_a {
    shift;
    return 'a(' . ( join q{;}, map { $_ // q{-} } @_ ) . ')';
}

sub rule_b {
    shift;
    return 'b(' . ( join q{;}, map { $_ // q{-} } @_ ) . ')';
}

sub rule_c {
    shift;
    return 'c(' . ( join q{;}, map { $_ // q{-} } @_ ) . ')';
}

sub rule_d {
    shift;
    return 'd(' . ( join q{;}, map { $_ // q{-} } @_ ) . ')';
}

sub rule_e {
    shift;
    return 'e(' . ( join q{;}, map { $_ // q{-} } @_ ) . ')';
}

sub rule_p {
    shift;
    return 'p(' . ( join q{;}, map { $_ // q{-} } @_ ) . ')';
}

sub rule_z {
    shift;
    return 'z(' . ( join q{;}, map { $_ // q{-} } @_ ) . ')';
}

sub start_rule {
    shift;
    return 'S(' . ( join q{;}, ( map { $_ // q{-} } @_ ) ) . ')';
}

sub rule_n1 {
    shift;
    return 'n1(' . ( join q{;}, ( map { $_ // q{-} } @_ ) ) . ')';
}

sub rule_n2 {
    shift;
    return 'n2(' . ( join q{;}, ( map { $_ // q{-} } @_ ) ) . ')';
}

sub rule_r2 {
    shift;
    return 'r2(' . ( join q{;}, ( map { $_ // q{-} } @_ ) ) . ')';
}

## use critic

my $grammar = Marpa::Grammar->new(
    {   start           => 'S',
        strip           => 0,
        maximal         => 1,
        infinite_action => 'quiet',

        rules => [
            {   lhs    => 'S',
                rhs    => [qw/p p p n/],
                action => 'main::start_rule'
            },
            { lhs => 'p', rhs => ['a'],  action => 'main::rule_p' },
            { lhs => 'p', rhs => [] },
            { lhs => 'n', rhs => ['a'],  action => 'main::rule_n1' },
            { lhs => 'n', rhs => ['r2'], action => 'main::rule_n2' },
            {   lhs    => 'r2',
                rhs    => [qw/a b c d e z/],
                action => 'main::rule_r2'
            },
            { lhs => 'a', rhs => [] },
            { lhs => 'b', rhs => [] },
            { lhs => 'c', rhs => [] },
            { lhs => 'd', rhs => [] },
            { lhs => 'e', rhs => [] },
            { lhs => 'a', rhs => ['a'], action => 'main::rule_a' },
            { lhs => 'b', rhs => ['a'], action => 'main::rule_b' },
            { lhs => 'c', rhs => ['a'], action => 'main::rule_c' },
            { lhs => 'd', rhs => ['a'], action => 'main::rule_d' },
            { lhs => 'e', rhs => ['a'], action => 'main::rule_e' },
            { lhs => 'z', rhs => ['S'], action => 'main::rule_z' },
        ],
        symbols => {
            p => { null_action => 'main::null_p' },
            a => { null_action => 'main::null_a', terminal => 1 },
            b => { null_action => 'main::null_b' },
            c => { null_action => 'main::null_c' },
            d => { null_action => 'main::null_d' },
            e => { null_action => 'main::null_e' },
        },
        maximal        => 1,
        default_action => 'main::default_action',
    }
);

$grammar->precompute();

my @results;
$results[1] = [
    qw{
        S(p;p;p;n2(r2(a;b;c;d;e;z(S(p(a(A));p;p;-)))))
        S(p;p;p;n2(r2(a;b;c;d;e;z(S(p(A);p;p;-)))))
        S(p;p;p;n2(r2(a;b;c;d;e(a(A));-)))
        S(p;p;p;n2(r2(a;b;c;d;e(A);-)))
        }
];

$results[2] = [
    qw{
        S(p;p;p;n2(r2(a;b;c;d;e;z(S(p(a(A));p;p;n2(r2(a;b;c;d;e;z(S(p;p(a(A));p;-)))))))))
        S(p;p;p;n2(r2(a;b;c;d;e;z(S(p(a(A));p;p;n2(r2(a;b;c;d;e;z(S(p;p(A);p;-)))))))))
        S(p;p;p;n2(r2(a;b;c;d;e;z(S(p(a(A));p;p;n2(r2(a;b;c;d;e;z(S(p(a(A));p;p;-)))))))))
        S(p;p;p;n2(r2(a;b;c;d;e;z(S(p(a(A));p;p;n2(r2(a;b;c;d;e;z(S(p(A);p;p;-)))))))))
        }
];

$results[3] = [
    qw{
        S(p;p;p;n2(r2(a;b;c;d;e;z(S(p(a(A));p;p;n2(r2(a;b;c;d;e;z(S(p;p(a(A));p;n2(r2(a;b;c;d;e;z(S(p;p;p(a(A));-)))))))))))))
        S(p;p;p;n2(r2(a;b;c;d;e;z(S(p(a(A));p;p;n2(r2(a;b;c;d;e;z(S(p;p(a(A));p;n2(r2(a;b;c;d;e;z(S(p;p;p(A);-)))))))))))))
        S(p;p;p;n2(r2(a;b;c;d;e;z(S(p(a(A));p;p;n2(r2(a;b;c;d;e;z(S(p;p(a(A));p;n2(r2(a;b;c;d;e;z(S(p;p(a(A));p;-)))))))))))))
        S(p;p;p;n2(r2(a;b;c;d;e;z(S(p(a(A));p;p;n2(r2(a;b;c;d;e;z(S(p;p(a(A));p;n2(r2(a;b;c;d;e;z(S(p;p(A);p;-)))))))))))))
        }
];

for my $input_length ( 1 .. 3 ) {
    my $recce = Marpa::Recognizer->new( { grammar => $grammar } );
    defined $recce->tokens( [ ( [ 'a', 'A' ] ) x $input_length ] )
        or Marpa::exception('Parsing exhausted');
    my $evaler = Marpa::Evaluator->new(
        { experimental => 'no warning' },
        {   recce            => $recce,
            infinite_rewrite => 0,
            parse_order      => 'none',
        }
    );
    my $i = 0;
    while ( my $value = $evaler->value() and $i < 4 ) {
        my $result = ${$value};
        Marpa::Test::is( ${$value}, $results[$input_length][$i],
            "cycle with initial nullables, input length=$input_length, pass $i"
        );
        $i++;
    } ## end while ( my $value = $evaler->value() and $i < 4 )
} ## end for my $input_length ( 1 .. 3 )

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
