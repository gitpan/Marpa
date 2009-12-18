#!perl

use 5.010;
use strict;
use warnings;

use English qw( -no_match_vars );
use Fatal qw(open close);
use Text::Diff;
use Getopt::Long qw(GetOptions);
use Test::More tests => 1;
use Carp;

use lib 'lib';
use Marpa::Display;

my $warnings = 0;
my $options_result = GetOptions( 'warnings' => \$warnings );

Marpa::exception("$PROGRAM_NAME options parsing failed")
    if not $options_result;

sub normalize_whitespace {
    my $ref  = shift;
    my $text = ${$ref};
    $text =~ s/\A\s*//xms;
    $text =~ s/\s*\z//xms;
    $text =~ s/\s+/ /gxms;
    return \$text;
} ## end sub normalize_whitespace

my %exclude = map { ( $_, 1 ) } qw(
    Makefile.PL
    inc/Test/Weaken.pm
    sandbox/TODO.pod
);

my @test_files = @ARGV;
my $debug_mode = scalar @test_files;
if ( not $debug_mode ) {
    open my $manifest, '<', 'MANIFEST'
        or Marpa::exception("Cannot open MANIFEST: $ERRNO");
    FILE: while ( my $file = <$manifest> ) {
        chomp $file;
        $file =~ s/\s*[#].*\z//xms;
        next FILE if $exclude{$file};
        next FILE if -d $file;
        my ($ext) = $file =~ / [.] ([^.]+) \z /xms;
        next FILE if not defined $ext;
        $ext = lc $ext;
        next FILE
            if $ext ne 'pod'
                and $ext ne 'pl'
                and $ext ne 'pm'
                and $ext ne 't';

        push @test_files, $file;
    }    # FILE
    close $manifest;
} ## end if ( not $debug_mode )

my $error_file;
## no critic (InputOutput::RequireBriefOpen)
if ($debug_mode) {
    open $error_file, '>&STDOUT'
        or Marpa::exception("Cannot dup STDOUT: $ERRNO");
}
else {
    open $error_file, '>', 'author.t/display.errs'
        or Marpa::exception("Cannot open display.errs: $ERRNO");
}
## use critic

my $display_data = Marpa::Display->new();

FILE: for my $file (@test_files) {
    if ( not -f $file ) {
        Test::More::fail("attempt to test displays in non-file: $file");
        next FILE;
    }
    $display_data->read($file);

} ## end for my $file (@test_files)

my @formatting_instructions = qw(normalize-whitespace);

sub format_display {
    my ( $text, $instructions ) = @_;
    my $result = ${$text};

    if ( $instructions->{'normalize-whitespace'} ) {
        $result =~ s/^\s+//gxms;
        $result =~ s/\s+$//gxms;
        $result =~ s/\s+/ /gxms;
        $result =~ s/\n+/\n/gxms;
    } ## end if ( $instructions->{'normalize-whitespace'} )
    return \$result;
} ## end sub format_display

# reformat two display according to the instructions in the
# second, and compare.
sub compare {
    my ( $original, $copy ) = @_;
    my $formatted_original = format_display( \$original->{content}, $copy );
    my $formatted_copy     = format_display( \$copy->{content},     $copy );
    return 1 if ${$formatted_original} eq ${$formatted_copy};
    say STDERR Text::Diff::diff $formatted_original, $formatted_copy,
        { STYLE => 'Table' }
        or Carp::croak("Cannot print: $ERRNO");
    return 0;
} ## end sub compare

my $displays_by_name = $display_data->{displays};
DISPLAY_NAME: for my $display_name ( keys %{$displays_by_name} ) {

    my $displays = $displays_by_name->{$display_name};
    if ( scalar @{$displays} <= 1 ) {
        Test::More::fail("Display $display_name has only one instance");
    }

    # find the "original"
    my $original_ix;
    DISPLAY: for my $display_ix ( 0 .. $#{$displays} ) {
        if (not grep { $_ ~~ \@formatting_instructions }
            keys %{ $displays->[$display_ix] }
            )
        {
            $original_ix = $display_ix;
        } ## end if ( not grep { $_ ~~ \@formatting_instructions } keys...)
    } ## end for my $display_ix ( 0 .. $#{$displays} )

    # Warn if there wasn't a clear original?
    $original_ix //= 0;    # default to the first

    DISPLAY: for my $copy_ix ( 0 .. $#{$displays} ) {
        next DISPLAY if $copy_ix == $original_ix;
        Test::More::ok compare( $displays->[$original_ix],
            $displays->[$copy_ix] ), "$display_name, copy $copy_ix";
    }

} ## end for my $display_name ( keys %{$displays_by_name} )

__END__
