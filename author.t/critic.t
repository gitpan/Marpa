#!perl

use strict;
use warnings;

use Test::More;
use Fatal qw( open close waitpid );
use English qw( -no_match_vars );
use IPC::Open2;
use POSIX qw(WIFEXITED);
use lib 'lib';
use Marpa;

my %exclude = map { ( $_, 1 ) } qw(
    Makefile.PL
    bootstrap/bootstrap.pl
    bootstrap/bootstrap_header.pl
    bootstrap/bootstrap_trailer.pl
    lib/Marpa/Raw_Source.pm
    lib/Marpa/header_Raw_Source.pm
    lib/Marpa/trailer_Raw_Source.pm
    inc/Test/Weaken.pm
);

# usually workarounds for perlcritic bugs
my %per_file_options =
    ( 'bootstrap/bootstrap.pl' => [qw(--exclude CodeLayout::RequireTidyCode)],
    );

sub run_critic {
    my $file = shift;

    my $per_file_options = $per_file_options{$file};
    my @cmd              = ('perlcritic');
    if ( defined $per_file_options ) {
        push @cmd, @{$per_file_options};
    }
    push @cmd, qw(--profile author.t/perlcriticrc);
    push @cmd, $file;

    my ( $child_out, $child_in );

    my $pid = IPC::Open2::open2( $child_out, $child_in, @cmd )
        or Marpa::Exception("IPC::Open2 of perlcritic pipe failed: $ERRNO");
    close $child_in;
    my $critic_output = do {
        local ($RS) = undef;
        <$child_out>;
    };
    close $child_out;
    waitpid $pid, 0;
    if ( my $child_error = $CHILD_ERROR ) {
        my $error_message;
        if (WIFEXITED(
                ## critic apparently can't find ${^CHILD_ERROR_NATIVE}
                ## no critic (Subroutines::ProhibitCallsToUndeclaredSubs)
                ${^CHILD_ERROR_NATIVE}
                    ## use critic (Subroutines::ProhibitCallsToUndeclaredSubs)
            ) != 1
            )
        {
            $error_message = "perlcritic returned $child_error";
        } ## end if ( WIFEXITED( ${^CHILD_ERROR_NATIVE} ) != 1 )
        if ( defined $error_message ) {
            print {*STDERR} $error_message, "\n"
                or Marpa::Exception("Cannot print to STDERR: $ERRNO");
            $critic_output .= "$error_message\n";
        }
        return \$critic_output;
    } ## end if ( my $child_error = $CHILD_ERROR )
    return q{};
} ## end sub run_critic

open my $manifest, '<', 'MANIFEST'
    or Marpa::Exception("open of MANIFEST failed: $ERRNO");

my @test_files = ();
FILE: while ( my $file = <$manifest> ) {
    chomp $file;
    $file =~ s/\s*[#].*\z//xms;
    next FILE if -d $file;
    next FILE if $exclude{$file};
    my ($ext) = $file =~ / [.] ([^.]+) \z /xms;
    next FILE if not defined $ext;
    $ext = lc $ext;
    next FILE
        if $ext ne 'pl'
            and $ext ne 'pm'
            and $ext ne 't';

    push @test_files, $file;
}    # FILE
close $manifest;

Test::More::plan tests => scalar @test_files;

open my $error_file, '>', 'author.t/perlcritic.errs';
FILE: for my $file (@test_files) {
    if ( not -f $file ) {
        Test::More::fail("perlcritic of non-file: $file");
        next FILE;
    }
    my $warnings = run_critic($file);
    my $clean    = 1;
    my $message  = "perlcritic clean for $file";
    if ($warnings) {
        $clean = 0;
        my @newlines = ( ${$warnings} =~ m/\n/xmsg );
        $message =
              "perlcritic for $file: "
            . ( scalar @newlines )
            . ' lines of warnings';
    } ## end if ($warnings)
    Test::More::ok( $clean, $message );
    next FILE if $clean;
    print {$error_file} "=== $file ===\n" . ${$warnings}
        or Marpa::Exception("print failed: $ERRNO");
} ## end for my $file (@test_files)
close $error_file;
