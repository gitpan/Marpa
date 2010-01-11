#!perl
# Two rules which start with nullables, and cycle.

use 5.010;
use strict;
use warnings;

use Test::More tests => 29;

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
sub null_n { return 'n' }
sub null_p { return 'p' }

sub rule_a {
    shift;
    return 'a(' . ( join q{;}, map { $_ // q{-} } @_ ) . ')';
}

sub rule_n {
    shift;
    return 'n(' . ( join q{;}, map { $_ // q{-} } @_ ) . ')';
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

sub rule_na {
    shift;
    return 'na(' . ( join q{;}, ( map { $_ // q{-} } @_ ) ) . ')';
}

sub rule_nr2 {
    shift;
    return 'nr2(' . ( join q{;}, ( map { $_ // q{-} } @_ ) ) . ')';
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
            { lhs => 'S', rhs => [qw/p n/], action => 'main::start_rule' },
            { lhs => 'p', rhs => ['a'],     action => 'main::rule_p' },
            { lhs => 'p', rhs => [] },
            { lhs => 'n', rhs => ['a'],     action => 'main::rule_na' },
            { lhs => 'n', rhs => [] },
            { lhs => 'n', rhs => ['r2'],    action => 'main::rule_nr2' },
            {   lhs    => 'r2',
                rhs    => [qw/a z/],
                action => 'main::rule_r2'
            },
            { lhs => 'a', rhs => [] },
            { lhs => 'a', rhs => ['a'], action => 'main::rule_a' },
            { lhs => 'z', rhs => ['S'], action => 'main::rule_z' },
        ],
        symbols => {
            a => { null_action => 'main::null_a', terminal => 1 },
            n => { null_action => 'main::null_n' },
            p => { null_action => 'main::null_p' },
        },
        maximal        => 1,
        default_action => 'main::default_action',
    }
);

$grammar->precompute();

my @results;
$results[1] = [
    qw{
        S(p;nr2(r2(a;z(S(p(a(A));n)))))
        S(p;nr2(r2(a;z(S(p(A);n)))))
        S(p;nr2(r2(a(A);-)))
        S(p;nr2(r2(A;-)))
        S(p;na(a(A)))
        S(p;na(A))
        S(p(a(A));n)
        S(p(A);n)
        }
];
$results[2] = [
    qw{
        S(p;nr2(r2(a;z(S(p(a(A));nr2(r2(a;z(S(p;na(a(A)))))))))))
        S(p;nr2(r2(a;z(S(p(a(A));nr2(r2(a;z(S(p;na(A))))))))))
        S(p;nr2(r2(a;z(S(p(a(A));nr2(r2(a;z(S(p(a(A));n)))))))))
        S(p;nr2(r2(a;z(S(p(a(A));nr2(r2(a;z(S(p(A);n)))))))))
        S(p;nr2(r2(a;z(S(p(a(A));nr2(r2(a(A);-)))))))
        S(p;nr2(r2(a;z(S(p(a(A));nr2(r2(A;-)))))))
        S(p;nr2(r2(a;z(S(p(A);nr2(r2(a;z(S(p;na(a(A)))))))))))
        S(p;nr2(r2(a;z(S(p(A);nr2(r2(a;z(S(p;na(A))))))))))
        S(p;nr2(r2(a;z(S(p(A);nr2(r2(a;z(S(p(a(A));n)))))))))
        S(p;nr2(r2(a;z(S(p(A);nr2(r2(a;z(S(p(A);n)))))))))
        }
];
$results[3] = [
    qw{
        S(p;nr2(r2(a;z(S(p(a(A));nr2(r2(a;z(S(p(a(A));nr2(r2(a;z(S(p;na(a(A)))))))))))))))
        S(p;nr2(r2(a;z(S(p(a(A));nr2(r2(a;z(S(p(a(A));nr2(r2(a;z(S(p;na(A))))))))))))))
        S(p;nr2(r2(a;z(S(p(a(A));nr2(r2(a;z(S(p(a(A));nr2(r2(a;z(S(p(a(A));n)))))))))))))
        S(p;nr2(r2(a;z(S(p(a(A));nr2(r2(a;z(S(p(a(A));nr2(r2(a;z(S(p(A);n)))))))))))))
        S(p;nr2(r2(a;z(S(p(a(A));nr2(r2(a;z(S(p(a(A));nr2(r2(a(A);-)))))))))))
        S(p;nr2(r2(a;z(S(p(a(A));nr2(r2(a;z(S(p(a(A));nr2(r2(A;-)))))))))))
        S(p;nr2(r2(a;z(S(p(a(A));nr2(r2(a;z(S(p(A);nr2(r2(a;z(S(p;na(a(A)))))))))))))))
        S(p;nr2(r2(a;z(S(p(a(A));nr2(r2(a;z(S(p(A);nr2(r2(a;z(S(p;na(A))))))))))))))
        S(p;nr2(r2(a;z(S(p(a(A));nr2(r2(a;z(S(p(A);nr2(r2(a;z(S(p(a(A));n)))))))))))))
        S(p;nr2(r2(a;z(S(p(a(A));nr2(r2(a;z(S(p(A);nr2(r2(a;z(S(p(A);n)))))))))))))
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
    while ( my $value = $evaler->value() and $i < 10 ) {
        Marpa::Test::is( ${$value}, $results[$input_length][$i],
            "cycle with initial nullables, input length=$input_length, pass $i"
        );
        $i++;
    } ## end while ( my $value = $evaler->value() and $i < 10 )
} ## end for my $input_length ( 1 .. 3 )

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
