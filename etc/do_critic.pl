#!perl

use 5.010;
use strict;
use warnings;

use Fatal qw( close waitpid );
use English qw( -no_match_vars );
use IPC::Open2;
use POSIX qw(WIFEXITED);

my %exclude = map { ($_, 1) } qw(
Changes
MANIFEST
META.yml
Makefile.PL
README
bootstrap/bootstrap_header.pl
bootstrap/bootstrap_trailer.pl
etc/perlcriticrc
lib/Marpa/Raw_Source.pm
lib/Marpa/header_Raw_Source.pm
lib/Marpa/trailer_Raw_Source.pm
t/lib/Test/Weaken.pm
);

sub run_critic {
    my $file = shift;
    my @cmd = qw(perlcritic -profile perlcriticrc);
    push @cmd, $file;
    my ($child_out, $child_in);
    # say STDERR join(" ", @cmd);
    my $pid = open2($child_out, $child_in, @cmd)
        or croak("IPC::Open2 of perlcritic pipe failed: $ERRNO");
    close $child_in;
    my $critic_output = do {
        local($RS) = undef;
         <$child_out>;
    };
    close $child_out;
    waitpid $pid, 0;
    if (my $child_error = $CHILD_ERROR)
    {
        my $error_message;
        if (WIFEXITED(${^CHILD_ERROR_NATIVE}) != 1) {
            $error_message = "perlcritic returned $child_error";
        }
        if (defined $error_message)
        {
            say STDERR $error_message;
            $critic_output .= "$error_message\n";
        }
        my @newlines = ($critic_output =~ m/\n/xmsg);
        say STDERR "$file: ", scalar @newlines, ' lines of complaints';
        return \$critic_output;
    }
    say STDERR "$file: clean";
    return q{};
}

open my $manifest, '<', '../MANIFEST'
    or croak("open of MANIFEST failed: $ERRNO");

FILE: while (my $file = <$manifest>) {
    chomp $file;
    $file =~ s/\s*[#].*\z//xms;
    next FILE if $file =~ /.pod\z/xms;
    next FILE if $file =~ /.marpa\z/xms;
    next FILE if $file =~ /\/Makefile\z/xms;
    next FILE if $exclude{$file};
    $file = '../' . $file;
    next FILE if -d $file;
    croak("No such file: $file") unless -f $file;
    if (my $result = run_critic( $file ))
    {
        say "=== $file ===";
        print ${$result}
            or croak("print failed: $ERRNO");
    }
}
close $manifest;
