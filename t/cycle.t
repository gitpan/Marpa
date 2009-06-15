#!perl
# A grammar with cycles

use 5.010;
use strict;
use warnings;
use English qw( -no_match_vars );
use Fatal qw(open close chdir);

use Test::More tests => 7;
use lib 'lib';
use lib 't/lib';
use Marpa::Test;

BEGIN {
    Test::More::use_ok('Marpa');
}

my $example_dir = 'example';
chdir $example_dir;

my $mdl_header = <<'EOF';
semantics are perl5.  version is 0.001_013.
start symbol is S.
default action is q{join(q{ }, @_)}.

EOF

my $cycle1_mdl = $mdl_header . <<'EOF';
S: S.

S matches /./.

EOF

my $cycle2_mdl = $mdl_header . <<'EOF';
S: A.

A: S.

A matches /./.

EOF

my $cycle8_mdl = $mdl_header . <<'EOF';
S: A.

A: B, T, U.

B: V, C.

C: W, D, X.

D: E.

E: S.

E matches /./.

T matches /./.

T: .

U matches /./.

U: .

V matches /./.

V: .

W matches /./.

W: .

X matches /./.

X: .

EOF

for my $test_data (
    [   \$cycle1_mdl,
        \('1'),
        '1',
        <<'EOS'
Cycle found involving rule: 0: s -> s
EOS
    ],
    [   \$cycle2_mdl,
        \('1'),
        '1',
        <<'EOS'
Cycle found involving rule: 1: a -> s
Cycle found involving rule: 0: s -> a
EOS
    ],
    [   \$cycle8_mdl,
        \('123456'),
        '1 2 3 4 5 6',
        <<'EOS'
Cycle found involving rule: 3: c -> w d x
Cycle found involving rule: 2: b -> v c
Cycle found involving rule: 1: a -> b t u
Cycle found involving rule: 5: e -> s
Cycle found involving rule: 4: d -> e
Cycle found involving rule: 0: s -> a
EOS
    ],
    )
{
    my ( $grammar, $input, $expected, $expected_trace ) = @{$test_data};
    my $trace = q{};
    open my $MEMORY, '>', \$trace;
    my $value = Marpa::mdl(
        $grammar, $input,
        {   cycle_action      => 'warn',
            trace_file_handle => $MEMORY,
        }
    );
    close $MEMORY;
    Marpa::Test::is( ${$value}, $expected );
    Marpa::Test::is( $trace,    $expected_trace );
} ## end for my $test_data ( [ \$cycle1_mdl, \('1'), '1', <<'EOS'...

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
