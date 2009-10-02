#!/usr/bin/perl

use 5.010;
use strict;
use warnings;

use Module::Build;

my $marpa_version = '0.001_018';

my $class = Module::Build->subclass(
    class => 'Module::Build::Custom',
    code  => <<'SUBCLASS' );

    sub ACTION_dist {
        my $self = shift;
        open( my $manifest_fh, q{<}, 'MANIFEST' )
            or die("Cannot open MANIFEST: $!");
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
                say STDERR "Fatal Error: Smart::Comments enabled in $file";
                die("dist action abended\n");
            }
        } ## end while ( my $file = <$manifest_fh> )
        $self->SUPER::ACTION_dist;
    } ## end sub ACTION_dist
SUBCLASS

my $build = $class->new(
    dist_name     => 'Marpa',
    dist_author   => 'Jeffrey Kegler',
    dist_version  => $marpa_version,
    dist_abstract => 'Generate Parsers from any BNF grammar',
    requires      => { 'Test::More' => 0 },
    meta_merge    => {
        no_index => {
            directory => [qw(inc t example bootstrap author.t save)],
            namespace => ['Marpa::Internal'],
            package   => [qw(Marpa::Internal Marpa::Offset)],
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
            'Marpa::Lex' => {
                file    => 'lib/Marpa/Lex.pm',
                version => $marpa_version,
            },
        },
    },
    PL_files  => { 'lib/Marpa/Source.pm.PL' => 'lib/Marpa/Source.pm' },
    pod_files => {
        'lib/Marpa/Doc/Algorithm.pod' =>
            "$(INST_LIB)/Marpa/Doc/Algorithm.pod",
        'lib/Marpa/Doc/Bibliography.pod' =>
            "$(INST_LIB)/Marpa/Doc/Bibliography.pod",
        'lib/Marpa/Doc/Debugging.pod' =>
            "$(INST_LIB)/Marpa/Doc/Debugging.pod",
        'lib/Marpa/Doc/Internals.pod' =>
            "$(INST_LIB)/Marpa/Doc/Internals.pod",
        'lib/Marpa/Doc/MDL.pod'     => "$(INST_LIB)/Marpa/Doc/MDL.pod",
        'lib/Marpa/Doc/Options.pod' => "$(INST_LIB)/Marpa/Doc/Options.pod",
        'lib/Marpa/Doc/Parse_Terms.pod' =>
            "$(INST_LIB)/Marpa/Doc/Parse_Terms.pod",
        'lib/Marpa/Doc/Plumbing.pod' => "$(INST_LIB)/Marpa/Doc/Plumbing.pod",
        'lib/Marpa/Doc/To_Do.pod'    => "$(INST_LIB)/Marpa/Doc/To_Do.pod",
    },
    pm_files => {
        'lib/Marpa.pm'            => "$(INST_LIB)/Marpa.pm",
        'lib/Marpa/Evaluator.pm'  => "$(INST_LIB)/Marpa/Evaluator.pm",
        'lib/Marpa/Grammar.pm'    => "$(INST_LIB)/Marpa/Grammar.pm",
        'lib/Marpa/Internal.pm'   => "$(INST_LIB)/Marpa/Internal.pm",
        'lib/Marpa/Lex.pm'        => "$(INST_LIB)/Marpa/Lex.pm",
        'lib/Marpa/MDL.pm'        => "$(INST_LIB)/Marpa/MDL.pm",
        'lib/Marpa/Offset.pm'     => "$(INST_LIB)/Marpa/Offset.pm",
        'lib/Marpa/Raw_Source.pm' => "$(INST_LIB)/Marpa/Raw_Source.pm",
        'lib/Marpa/Recognizer.pm' => "$(INST_LIB)/Marpa/Recognizer.pm",
        'lib/Marpa/Source.pm'     => "$(INST_LIB)/Marpa/Source.pm",
        'lib/Marpa/header_Raw_Source.pm' =>
            "$(INST_LIB)/Marpa/header_Raw_Source.pm",
        'lib/Marpa/trailer_Raw_Source.pm' =>
            "$(INST_LIB)/Marpa/trailer_Raw_Source.pm"
    },
    license            => 'perl',
    create_readme      => 0,
    create_makefile_pl => 'small',
);

$build->create_build_script;
