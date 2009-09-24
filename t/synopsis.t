#!perl
#
use 5.010;
use strict;
use warnings;
use lib 'lib';
use English qw( -no_match_vars );
use Config;
use Fatal qw( chdir close waitpid );
use IPC::Open2;

use Test::More;
use t::lib::Marpa::Test;

if ( $Config{'d_fork'} ) {
    Test::More::plan tests => 2;
}
else {
    Test::More::plan skip_all => 'Fork required to test examples';
    exit 0;
}

my $example_dir = 'example';
chdir $example_dir;

my $this_perl = $EXECUTABLE_NAME;
local ($RS) = undef;

my $pipe;
open $pipe, q{-|}, $this_perl, '-I../lib', 'synopsis.pl'
    or Marpa::exception("Problem opening pipe to perl: $ERRNO");
Marpa::Test::is( <$pipe>, "12\n", 'synopsis example' );
close $pipe;

RUN_MDL: {
    local $ENV{PERL5LIB} = '../lib:' . $ENV{PERL5LIB};
    my $text;
    my $pid =
        IPC::Open2::open2( $pipe, $text, $this_perl, '../bin/mdl', 'parse',
        '-grammar', '../example/null_value.marpa' );
    say {$text} 'Z';
    close $text;
    Marpa::Test::is(
        <$pipe>,
        "A is missing, but Zorro was here\n",
        'null value example'
    );
    close $pipe;
    waitpid $pid, 0;
} ## end RUN_MDL:
