#!perl
use 5.010;
use strict;
use warnings;
use lib 'lib';
use lib 't/lib';
use Test::More;
use Fatal qw(close open waitpid);
use English qw( -no_match_vars );
use Config;
use IPC::Open3;

use Marpa::Test;

if ( $Config{'d_fork'} ) {
    Test::More::plan tests => 2;
}
else {
    Test::More::plan skip_all => 'Fork required to test examples';
    exit 0;
}

my $this_perl = $EXECUTABLE_NAME;

my $script = <<"END_OF_SCRIPT";
cd bootstrap
echo Bootstrap 1
$this_perl -I../lib bootstrap.pl self.marpa bootstrap_header.pl bootstrap_trailer.pl >t_bootcopy0.pl
echo Bootstrap 2
$this_perl -I../lib t_bootcopy0.pl self.marpa bootstrap_header.pl bootstrap_trailer.pl >t_bootcopy1.pl
echo Diff
diff t_bootcopy0.pl t_bootcopy1.pl && echo Test OK
echo Cleanup
rm -f t_bootcopy0.pl t_bootcopy1.pl
exit 0
END_OF_SCRIPT

my ( $wtr, $rdr, $err );
my $pid = IPC::Open3::open3( $wtr, $rdr, $err, 'sh' );
print {$wtr} $script or Marpa::exception("write to open3 failed: $ERRNO");
close $wtr;
waitpid $pid, 0;

my $script_err = do { local ($RS) = undef; defined $err ? <$err> : q{} };
Marpa::Test::is( $script_err, q{}, 'script stderr empty' );

my $script_output = do { local ($RS) = undef; <$rdr> };
Marpa::Test::is(
    $script_output,
    "Bootstrap 1\n" . "Bootstrap 2\n" . "Diff\n" . "Test OK\n" . "Cleanup\n",
    'bootstrapped copies identical'
);

