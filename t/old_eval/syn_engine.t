#!/usr/bin/perl

# Engine Synopsis

use 5.010;
use strict;
use warnings;

use Test::More tests => 4;

use lib 'lib';
use Marpa::Test;

BEGIN {
    Test::More::use_ok('Marpa');
}

# Marpa::Display
# name: Engine Synopsis Unambiguous Parse

my $grammar = Marpa::Grammar->new(
    {   start          => 'Expression',
        actions        => 'My_Actions',
        default_action => 'first_arg',
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

# Ambiguous, Array Form Rules

# Marpa::Display
# name: Engine Synopsis Ambiguous Parse

my $ambiguous_grammar = Marpa::Grammar->new(
    {   start   => 'E',
        actions => 'My_Actions',
        rules   => [
            [ 'E', [qw/E Add E/],      'do_add' ],
            [ 'E', [qw/E Multiply E/], 'do_multiply' ],
            [ 'E', [qw/Number/], ],
        ],
        default_action => 'first_arg',
    }
);

$ambiguous_grammar->precompute();

my $ambiguous_recce =
    Marpa::Recognizer->new( { grammar => $ambiguous_grammar } );

$ambiguous_recce->tokens( \@tokens );

my $evaler = Marpa::Evaluator->new( { recce => $ambiguous_recce } );

my @values = ();
if ($evaler) {
    while ( defined( my $ambiguous_value_ref = $evaler->value() ) ) {
        push @values, ${$ambiguous_value_ref};
    }
}

# Marpa::Display::End

Test::More::is( $value, 49, 'Unambiguous Value' );
Test::More::is_deeply( [ sort @values ], [ 336, 49 ], 'Ambiguous Values' );

# An example of interactive lexing, using
# the unambiguous grammar.

sub fix_things {
    die q{Don't know how to fix things};
}

$recce = Marpa::Recognizer->new( { grammar => $grammar, mode => 'stream' } );

# Marpa::Display
# name: Engine Synopsis Interactive Parse

RECCE_RESPONSE: for ( my $token_ix = 0;; ) {

    my ( $current_earleme, $expected_tokens ) =
        $recce->tokens( \@tokens, \$token_ix );

    last RECCE_RESPONSE if $token_ix > $#tokens;

    fix_things( \@tokens, $expected_tokens )
        or die q{Don't know how to fix things};

} ## end for ( my $token_ix = 0;; )

# Marpa::Display::End

$value_ref = $recce->value;
$value = $value_ref ? ${$value_ref} : 'No Parse';
Test::More::is( $value, 49, 'Interactive Value' );

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
