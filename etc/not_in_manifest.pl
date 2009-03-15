#!perl

use strict;
use warnings;

use Carp;
use Fatal qw( chdir waitpid close );
use English qw( -no_match_vars );

my %manifest;

open my $manifest, '<', '../MANIFEST'
    or croak("open of MANIFEST failed: $ERRNO");
FILE: while ( my $line = <$manifest> ) {
    chomp $line;
    $manifest{$line} = 1;
}
close $manifest;

chdir q{..};
my $pid = open my $rdr, q{-|}, 'svn', 'list', '-R'
    or croak("open of svn list pipe failed: $ERRNO");

FILE: while ( my $line = <$rdr> ) {
    chomp $line;
    next FILE if -d $line;
    next FILE if $manifest{$line};
    print "$line\n"
        or croak("Cannot print: $ERRNO");
} ## end while ( my $line = <$rdr> )

close $rdr;
waitpid $pid, 0;
