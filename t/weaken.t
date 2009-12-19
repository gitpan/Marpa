#!perl

use 5.010;
use strict;
use warnings;
use lib 'inc';
use lib 'lib';

use Test::More;

BEGIN {
    if ( eval { require Task::Weaken } ) {
        Test::More::plan tests => 3;
    }
    else {
        Test::More::plan skip_all => 'Scalar::Util::weaken() not implemented';
    }
    Test::More::use_ok('Marpa', 'alpha');
    Test::More::use_ok('Test::Weaken');
} ## end BEGIN

my $test = sub {
    my $g = Marpa::Grammar->new(
        {   start => 'S',
            rules => [
                [ 'S', [qw/A A A A/] ],
                [ 'A', [qw/a/] ],
                [ 'A', [qw/E/] ],
                ['E'],
            ],
            terminals => ['a'],
        }
    );
    $g->precompute();
    my $recce = Marpa::Recognizer->new( { grammar => $g } );
    $recce->tokens( [ ( [ 'a', 'a', 1 ] ) x 4 ] );
    my $evaler = Marpa::Evaluator->new( { recce => $recce } );
    Marpa::exception('No parse found') if not $evaler;
    $evaler->value();
    [ $g, $recce, $evaler ];
};

my $tester            = Test::Weaken->new($test);
my $unfreed_count     = $tester->test();
my $unfreed_proberefs = $tester->unfreed_proberefs();
my $total             = $tester->probe_count();
my $freed_count       = $total - $unfreed_count;

# The evaluator (for And_Node::PERL_CLOSURE) assigns a \undef, and this creates
# an undef "global".  No harm done if there's only one.

my $ignored_count = 0;
DELETE_UNDEF_CONSTANT: for my $ix ( 0 .. $#{$unfreed_proberefs} ) {
    if ( ref $unfreed_proberefs->[$ix] eq 'SCALAR'
        and not defined ${ $unfreed_proberefs->[$ix] } )
    {
        delete $unfreed_proberefs->[$ix];
        $ignored_count++;
        last DELETE_UNDEF_CONSTANT;
    } ## end if ( ref $unfreed_proberefs->[$ix] eq 'SCALAR' and not...)
} ## end for my $ix ( 0 .. $#{$unfreed_proberefs} )
$unfreed_count = @{$unfreed_proberefs};

# "Freed=$freed_count, ignored=$ignored_count, unfreed=$unfreed_count, total=$total"

Test::More::cmp_ok( $unfreed_count, q{==}, 0, 'All refs freed' )
    or Test::More::diag("Unfreed refs: $unfreed_count");

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
