#!/usr/bin/perl

# A grammar with cycles

use 5.010;
use strict;
use warnings;
use lib 'lib';

use English qw( -no_match_vars );
use Fatal qw(open close chdir);

use Test::More tests => 10;
use Marpa::Test;

use constant A_LOT_OF_VALUES => 25;

BEGIN {
    Test::More::use_ok('Marpa');
}

sub make_rule {
    my ( $lhs_symbol_name, $rhs_symbol_name ) = @_;
    my $action_name = "main::action_$lhs_symbol_name$rhs_symbol_name";

    no strict 'refs';
    my $closure = *{$action_name}{'CODE'};
    use strict;

    if ( not defined $closure ) {
        my $action =
            sub { $lhs_symbol_name . $rhs_symbol_name . '(' . $_[1] . ')' };

        no strict 'refs';
        *{$action_name} = $action;
        use strict;
    } ## end if ( not defined $closure )

    return [ $lhs_symbol_name, [$rhs_symbol_name], $action_name ];
} ## end sub make_rule

sub make_plex_rules {
    my ($size) = @_;
    my @symbol_names = map { chr +( $_ + ord 'A' ) } ( 0 .. $size - 1 );
    my @rules;
    for my $infinite_symbol (@symbol_names) {
        for my $rhs_symbol (@symbol_names) {
            push @rules, make_rule( $infinite_symbol, $rhs_symbol );
        }
        push @rules, make_rule( $infinite_symbol, 't' );
        push @rules, make_rule( 's',           $infinite_symbol );
    } ## end for my $infinite_symbol (@symbol_names)
    return \@rules;
} ## end sub make_plex_rules

my $plex1_test = [
    '1-plex test',
    [ start => 's', rules => make_plex_rules(1) ],
    <<'EOS',
sA(AA(At(t)))
sA(At(t))
EOS
    <<'EOS',
sA(AA(At(t)))
sA(At(t))
EOS
    <<'EOS',
Cycle found involving rule: 0: A -> A
EOS
];

my $plex2_test = [
    '2-plex test',
    [ start => 's', rules => make_plex_rules(2) ],
    <<'EOS',
sA(AA(AB(BA(At(t)))))
sA(AA(AB(BB(BA(At(t))))))
sA(AA(AB(BB(Bt(t)))))
sA(AA(AB(Bt(t))))
sA(AA(At(t)))
sA(AB(BA(AA(At(t)))))
sA(AB(BA(At(t))))
sA(AB(BB(BA(AA(At(t))))))
sA(AB(BB(BA(At(t)))))
sA(AB(BB(Bt(t))))
sA(AB(Bt(t)))
sA(At(t))
sB(BA(AA(AB(BB(Bt(t))))))
sB(BA(AA(AB(Bt(t)))))
sB(BA(AA(At(t))))
sB(BA(AB(BB(Bt(t)))))
sB(BA(AB(Bt(t))))
sB(BA(At(t)))
sB(BB(BA(AA(AB(Bt(t))))))
sB(BB(BA(AA(At(t)))))
sB(BB(BA(AB(Bt(t)))))
sB(BB(BA(At(t))))
sB(BB(Bt(t)))
sB(Bt(t))
EOS
    <<'EOS',
sA(AA(AB(Bt(t))))
sA(AA(At(t)))
sA(AB(Bt(t)))
sA(At(t))
sB(BA(AA(AB(Bt(t)))))
sB(BA(AA(At(t))))
sB(BA(AB(Bt(t))))
sB(BA(At(t)))
sB(BB(BA(AA(AB(Bt(t))))))
sB(BB(BA(AA(At(t)))))
sB(BB(BA(AB(Bt(t)))))
sB(BB(BA(At(t))))
sB(BB(Bt(t)))
sB(Bt(t))
EOS
    <<'EOS',
Cycle found involving rule: 5: B -> B
Cycle found involving rule: 4: B -> A
Cycle found involving rule: 1: A -> B
Cycle found involving rule: 0: A -> A
EOS
];

my $plex3_test = [
    '3-plex test',
    [ start => 's', rules => make_plex_rules(3) ],
    <<'EOS',
1884 values
EOS
    <<'EOS',
119 values
EOS
    <<'EOS',
Cycle found involving rule: 12: C -> C
Cycle found involving rule: 11: C -> B
Cycle found involving rule: 10: C -> A
Cycle found involving rule: 7: B -> C
Cycle found involving rule: 6: B -> B
Cycle found involving rule: 5: B -> A
Cycle found involving rule: 2: A -> C
Cycle found involving rule: 1: A -> B
Cycle found involving rule: 0: A -> A
EOS
];

for my $test_data ( $plex1_test, $plex2_test, $plex3_test ) {
    my ( $test_name, $rules, $expected_values_rw, $expected_values_norw,
        $expected_trace )
        = @{$test_data};

    my $trace = q{};
    open my $MEMORY, '>', \$trace;
    my %args = (
        @{$rules},
        infinite_action => 'warn',
        strip        => 0,

        # Let the cycles make the parse absurdly large
        # That's the point of the test
        trace_file_handle => $MEMORY,
    );
    my $grammar = Marpa::Grammar->new( \%args );
    $grammar->precompute();

    close $MEMORY;
    Marpa::Test::is( $trace, $expected_trace, "$test_name trace" );

    my $recce = Marpa::Recognizer->new(
        { grammar => $grammar, trace_file_handle => \*STDERR } );
    $recce->tokens( [ [ 't', 't', 1 ] ] );

    for my $infinite_rewrite ( 0, 1 ) {
        my $evaler = Marpa::Evaluator->new(
            { experimental => 'no warning' },
            {   recce         => $recce,
                infinite_scale   => 200,
                infinite_rewrite => $infinite_rewrite,
            }
        );
        if ( not defined $evaler ) {
            Marpa::exception('Input not recognized');
        }

        my @values = ();
        while ( my $value = $evaler->value() ) {
            push @values, ${$value};
        }

        my $values = q{};
        if ( @values > A_LOT_OF_VALUES ) {
            $values = @values . ' values';
        }
        else {
            $values = join "\n", sort @values;
        }
        Marpa::Test::is(
            "$values\n",
            (   $infinite_rewrite ? $expected_values_rw : $expected_values_norw,
                "$test_name " . ( $infinite_rewrite ? 'rewrite' : 'no rewrite' )
            )
        );
    } ## end for my $infinite_rewrite ( 0, 1 )

} ## end for my $test_data ( $plex1_test, $plex2_test, $plex3_test)

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
