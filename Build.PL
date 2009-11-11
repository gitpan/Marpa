#!/usr/bin/perl

use 5.010;
use strict;
use warnings;

use Module::Build;

my $marpa_version = '0.001_022';

my $class = Module::Build->subclass(
    class => 'Module::Build::Custom',
    code  => <<'SUBCLASS' );

    sub ACTION_dist {
        my $self = shift;
        open( my $manifest_fh, q{<}, 'MANIFEST' )
            or die("Cannot open MANIFEST: $!");

        my @problems;
        FILE: while ( my $file = <$manifest_fh> ) {
            chomp $file;
            next FILE if $file =~ /^\s*[#]/xms;
            next FILE if $file !~ /[.](pl|pm|t)$/xmsi;
            my $file_text = do {
                local $/ = undef;
                open( my $fh, q{<}, $file ) or die("Cannot open $file: $!");
                <$fh>;
            };
            if ( $file_text =~ /^ \s+ use \s+ Smart[:][:]Comments /xms ) {
                push @problems, "Smart::Comments enabled in $file";
            }
        } ## end while ( my $file = <$manifest_fh> )

        die(join "\n", @problems, "Fatal Error\n") if @problems;
        $self->SUPER::ACTION_dist;
    } ## end sub ACTION_dist
SUBCLASS

my $build = $class->new(
    dist_name            => 'Marpa',
    dist_author          => 'Jeffrey Kegler',
    dist_version         => $marpa_version,
    dist_abstract        => 'Generate Parsers from any BNF grammar',
    requires             => { 'Scalar::Util' => 0, },
    recursive_test_files => 1,
    build_requires       => {

        # Scalar::Util::weaken no longer required
        'Test::More' => 0,
    },
    meta_merge => {
        no_index => {
            directory => [qw(inc t author.t save)],
            namespace => [
                'Marpa::Internal', 'Marpa::MDL::Internal',
                'Marpa::MDLex::Internal'
            ],
            package => [qw(Marpa::Internal Marpa::Offset)],
        },
    },
    meta_add => {
        distribution_type => 'module',
        provides          => {
            'Marpa' => {
                file    => 'lib/Marpa.pm',
                version => $marpa_version,
            },
            'Marpa::Evaluator' => {
                file    => 'lib/Marpa/Evaluator.pm',
                version => $marpa_version,
            },
            'Marpa::Recognizer' => {
                file    => 'lib/Marpa/Recognizer.pm',
                version => $marpa_version,
            },
            'Marpa::Grammar' => {
                file    => 'lib/Marpa/Grammar.pm',
                version => $marpa_version,
            },
            'Marpa::MDL' => {
                file    => 'lib/Marpa/MDL.pm',
                version => $marpa_version,
            },
            'Marpa::MDLex' => {
                file    => 'lib/Marpa/MDLex.pm',
                version => $marpa_version,
            },
        },
    },
    license            => 'perl',
    create_readme      => 0,
    create_makefile_pl => 'small',
    test_files =>
        [ map { glob $_ } qw(t/*.t t/*/*.t lib/Marpa/UrHTML/t/*.t) ],
);

$build->create_build_script;
