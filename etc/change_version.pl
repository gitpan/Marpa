#!perl

use 5.010;
use strict;
use warnings;
use Fatal qw(:void open close unlink select rename);
use English qw( -no_match_vars );

my @test_files = qw(
    ./bootstrap/self.marpa
    ./example/null_value.marpa
    ./example/equation.marpa
    ./example/synopsis.pl
    ./lib/Marpa/Doc/Internals.pod
    ./lib/Marpa/Doc/MDL.pod
    ./t/ah_s.t
    ./t/cycle.t
    ./t/cycle2.t
    ./t/minus_s.t
    ./t/randal.t
    ./author.t/MDL_displays.marpa
    ./author.t/minimal.marpa
    ./author.t/misc.t
);

our $FH;

croak("usage: $0: old_version new_version") unless scalar @ARGV == 2;

my ( $old, $new ) = @ARGV;

say STDERR "$old $new";

sub check_version {
    my $version = shift;
    my ( $major, $minor1, $underscore, $minor2 ) =
        ( $version =~ m/^ ([0-9]+) [.] ([0-9.]{3}) ([_]?) ([0-9.]{3}) $/xms );
    if ( not defined $minor2 ) {
        croak("Bad format in version number: $version");
    }
    if ( $minor1 % 2 and $underscore ne '_' ) {
        croak("No underscore in developer's version number: $version");
    }
    if ( $minor1 % 2 == 0 and $underscore eq '_' ) {
        croak("Underscore in official release version number: $version");
    }
} ## end sub check_version

check_version($old);
check_version($new);

## no critic (BuiltinFunctions::ProhibitStringyEval)
croak("$old >= $new") if eval $old >= eval $new;
## use critic

sub change {
    my ( $fix, @files ) = @_;
    for my $file (@files) {
        open my $fh, '<', $file;
        my $text = do { local ($RS) = undef; <$fh> };
        close $fh;
        my $backup = "save/$file";
        rename $file, $backup;
        open my $argvout, '>', $file;
        print {$argvout} ${ $fix->( \$text, $file ) }
            or croak("Could not print to argvout: $ERRNO");
        close $argvout;
    } ## end for my $file (@files)
    return 1;
} ## end sub change

sub fix_META_yml {
    my $text_ref  = shift;
    my $file_name = shift;

    unless ( ${$text_ref} =~ s/(version:\s*)$old/$1$new/gxms ) {
        say {*STDERR}
            "failed to change version from $old to $new in $file_name"
            or croak("Could not print to STDERR: $ERRNO");
    }
    return $text_ref;
} ## end sub fix_META_yml

sub fix_Marpa_pm {
    my $text_ref  = shift;
    my $file_name = shift;

    unless ( ${$text_ref} =~ s/(our\s+\$VERSION\s*=\s*')$old';/$1$new';/xms ) {
        say {*STDERR}
            "failed to change VERSION from $old to $new in $file_name"
            or croak("Could not print to STDERR: $ERRNO");
    }
    unless ( ${$text_ref} =~ s/(version\s+is\s+)$old/$1$new/xms ) {
        say {*STDERR}
            "failed to change version from $old to $new in $file_name"
            or croak("Could not print to STDERR: $ERRNO");
    }
    return $text_ref;
} ## end sub fix_Marpa_pm

sub fix_bootstrap_pl {
    my $text_ref  = shift;
    my $file_name = shift;

    unless ( ${$text_ref} =~ s/(\$new_version\s*=\s*')$old';/$1$new';/xms ) {
        say {*STDERR}
            "failed to change version from $old to $new in $file_name"
            or croak("Could not print to STDERR: $ERRNO");
    }
    return $text_ref;
} ## end sub fix_bootstrap_pl

sub fix_test_files {
    my $text_ref  = shift;
    my $file_name = shift;

    unless ( ${$text_ref} =~ s/(version\s+is\s+)$old/$1$new/gxms ) {
        say {*STDERR}
            "failed to change version from $old to $new in $file_name"
            or croak("Could not print to STDERR: $ERRNO");
    }
    return $text_ref;
} ## end sub fix_test_files

sub update_changes {
    my $text_ref  = shift;
    my $file_name = shift;

    my $date_stamp = localtime;
    unless ( ${$text_ref}
        =~ s/(\ARevision\s+history\s+[^\n]*\n\n)/$1$new $date_stamp\n/xms )
    {
        say {*STDERR} "failed to add $new to $file_name"
            or croak("Could not print to STDERR: $ERRNO");
    } ## end unless ( ${$text_ref} =~ ...
    return $text_ref;
} ## end sub update_changes

change( \&fix_META_yml,     'META.yml' );
change( \&fix_Marpa_pm,     'lib/Marpa.pm' );
change( \&fix_bootstrap_pl, 'bootstrap/bootstrap.pl' );
change( \&fix_test_files,   @test_files );
change( \&update_changes,   'Changes' );

say {*STDERR} 'REMEMBER TO UPDATE Changes file'
    or croak("Could not print to STDERR: $ERRNO");
