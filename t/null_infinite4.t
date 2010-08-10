#!perl
# A CHAF rule which starts with nullables, and cycle.

use 5.010;
use strict;
use warnings;

use Test::More tests => 2;

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

sub rule_n {
    shift;
    return 'n(' . ( join q{;}, map { $_ // q{-} } @_ ) . ')';
}

sub start_rule {
    shift;
    return 'S(' . ( join q{;}, ( map { $_ // q{-} } @_ ) ) . ')';
}

sub rule_f {
    shift;
    return 'f(' . ( join q{;}, ( map { $_ // q{-} } @_ ) ) . ')';
}

## use critic

my $grammar = Marpa::Grammar->new(
    {   start           => 'S',
        strip           => 0,
        infinite_action => 'quiet',

        rules => [
            {   lhs    => 'S',
                rhs    => [qw/n n n f/],
                action => 'main::start_rule'
            },
            { lhs => 'n', rhs => ['a'], action => 'main::rule_n' },
            { lhs => 'n', rhs => [] },
            { lhs => 'f', rhs => ['a'], action => 'main::rule_f' },
            { lhs => 'f', rhs => [] },
            { lhs => 'f', rhs => ['S'], action => 'main::rule_f' },
        ],
        symbols        => { a => { terminal => 1 }, },
        default_action => 'main::default_action',
    }
);

$grammar->precompute();

my @expected = qw{
    S(-;-;-;f(A))
    S(-;-;-;f(S(n(A);-;-;-)))
    S(-;-;n(A);-)
    S(-;n(A);-;-)
    S(n(A);-;-;-)
};

my $input_length = 1;
my $recce =
    Marpa::Recognizer->new( { grammar => $grammar, max_parses => 99 } );
$recce->tokens( [ ( [ 'a', 'A' ] ) x $input_length ] );
my $expected = $expected[$input_length];
my @values   = ();
while ( my $value_ref = $recce->value() ) {
    push @values, ${$value_ref};
}
Marpa::Test::is(
    ( join "\n", sort @values ),
    ( join "\n", @expected ),
    "value for input length $input_length"
);

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
