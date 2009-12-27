#!perl

use 5.010;
use strict;
use warnings;

use English qw( -no_match_vars );
use Fatal qw(open close);
use Text::Diff;
use Getopt::Long qw(GetOptions);
use Test::More 0.94;
use Carp;
use Perl::Tidy;

use lib 'lib';
use Marpa::Display;

my $warnings = 0;
my $options_result = GetOptions( 'warnings' => \$warnings );

Marpa::exception("$PROGRAM_NAME options parsing failed")
    if not $options_result;

my %exclude = map { ( $_, 1 ) } qw(
    Makefile.PL
    inc/Test/Weaken.pm
    sandbox/TODO.pod
);

my @test_files = @ARGV;
my $debug_mode = scalar @test_files;
if ( not $debug_mode ) {

    for my $additional_file (
        'lib/Marpa/UrHTML/Doc/UrHTML.pod',
        'lib/Marpa/UrHTML/Doc/Parsing_HTML.pod'
        )
    {
        Test::More::diag("Adding $additional_file");
        push @test_files, $additional_file;
    } ## end for my $additional_file ( 'lib/Marpa/UrHTML/Doc/UrHTML.pod'...)

    open my $manifest, '<', 'MANIFEST'
        or Marpa::exception("Cannot open MANIFEST: $ERRNO");
    FILE: while ( my $file = <$manifest> ) {
        chomp $file;
        $file =~ s/\s*[#].*\z//xms;
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

    my %file_seen = ();
    FILE: for my $test_file (@test_files) {
        next FILE if $exclude{$test_file};
        next FILE if -d $test_file;
        if ( $file_seen{$test_file}++ ) {
            Test::More::diag("Duplicate file: $test_file");
        }
    } ## end for my $test_file (@test_files)
    @test_files = keys %file_seen;

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

my @formatting_instructions = qw(perltidy remove-display-indent normalize-whitespace);

sub format_display {
    my ( $text, $instructions, $is_copy ) = @_;
    my $result = ${$text};

    if ( $instructions->{'remove-display-indent'} and $is_copy) {
        my ($first_line_spaces) = ($result =~ /^ (\s+) \S/xms);
        $first_line_spaces = quotemeta $first_line_spaces;
        $result =~ s/^$first_line_spaces//gxms;
    }
    if ( $instructions->{'normalize-whitespace'} ) {
        $result =~ s/^\s+//gxms;
        $result =~ s/\s+$//gxms;
        $result =~ s/[ \f\t]+/ /gxms;
        $result =~ s/\n+/\n/gxms;
    } ## end if ( $instructions->{'normalize-whitespace'} )
    if ( defined( my $tidy_options = $instructions->{'perltidy'} ) ) {
        my $tidied;
        Perl::Tidy::perltidy(
            source      => \$result,
            destination => \$tidied,
            perltidyrc  => \$tidy_options
        );
        $result = $tidied;
    } ## end if ( defined( my $tidy_options = $instructions->{'perltidy'...}))
    return \$result;
} ## end sub format_display

# reformat two display according to the instructions in the
# second, and compare.
sub compare {
    my ( $original, $copy ) = @_;
    my $formatted_original = format_display( \$original->{content}, $copy, 0 );
    my $formatted_copy     = format_display( \$copy->{content},     $copy, 1 );
    return 1 if ${$formatted_original} eq ${$formatted_copy};
    Test::More::diag(
        'Differences: ', $original->{filename},
        ' vs. ',         $copy->{filename},
        "\n",            Text::Diff::diff $formatted_original,
        $formatted_copy, { STYLE => 'Table' }
    );
    return 0;
} ## end sub compare

my $tests_run        = 0;
my $displays_by_name = $display_data->{displays};
DISPLAY_NAME: for my $display_name ( keys %{$displays_by_name} ) {

    my $displays = $displays_by_name->{$display_name};
    if ( scalar @{$displays} <= 1 ) {
        Test::More::fail(
            qq{Display "$display_name" has only one instance, in file }
                . $displays->[0]->{filename} );
        $tests_run++;
    } ## end if ( scalar @{$displays} <= 1 )

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
        $tests_run++;
    } ## end for my $copy_ix ( 0 .. $#{$displays} )

} ## end for my $display_name ( keys %{$displays_by_name} )

Test::More::done_testing($tests_run);

__END__
