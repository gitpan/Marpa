#!/usr/bin/perl

use 5.010;
use strict;
use warnings;
use Carp;
use Fatal qw(open close);

use Module::Build;

my $marpa_version = '0.103_000';

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
                if ( not -r $file ) {
                    Carp::carp("Cannot open $file: $!");
                    next FILE;
                }
                open( my $fh, q{<}, $file );
                <$fh>;
            };
            if ( $file_text =~ /^ \s+ use \s+ Smart[:][:]Comments /xms ) {
                push @problems, "Smart::Comments enabled in $file";
            }
        } ## end while ( my $file = <$manifest_fh> )

        Carp::carp(join "\n", @problems, "Fatal Error\n") if @problems;
        $self->SUPER::ACTION_dist;
    } ## end sub ACTION_dist
SUBCLASS

my $build = $class->new(
    dist_name     => 'Marpa',
    dist_author   => 'Jeffrey Kegler',
    dist_version  => $marpa_version,
    dist_abstract => 'Generate Parsers from any BNF grammar',
    requires      => {
        'Scalar::Util'     => 0,
        'HTML::Parser'     => '3.64',
        'HTML::PullParser' => '3.57',
    },
    build_requires => { 'Test::More' => 0.94 },
    PL_files       => {},            # workaround Module::Build bug
    pod_files      => {
        'lib/Marpa/Doc/Algorithm.pod'      => 'lib/Marpa/Algorithm.pod',
        'lib/Marpa/Doc/Bibliography.pod'   => 'lib/Marpa/Bibliography.pod',
        'lib/Marpa/Doc/Bocage.pod'         => 'lib/Marpa/Bocage.pod',
        'lib/Marpa/Doc/Evaluator.pod'      => 'lib/Marpa/Evaluator.pod',
        'lib/Marpa/Doc/Grammar.pod'        => 'lib/Marpa/Grammar.pod',
        'lib/Marpa/Doc/Implementation.pod' => 'lib/Marpa/Implementation.pod',
        'lib/Marpa/Doc/Marpa.pod'          => 'lib/Marpa.pod',
        'lib/Marpa/Doc/Models.pod'         => 'lib/Marpa/Models.pod',
        'lib/Marpa/Doc/Parse_Terms.pod'    => 'lib/Marpa/Parse_Terms.pod',
        'lib/Marpa/Doc/Recognizer.pod'     => 'lib/Marpa/Recognizer.pod',
        'lib/Marpa/Doc/Semantics.pod'      => 'lib/Marpa/Semantics.pod',
        'lib/Marpa/Doc/Support.pod'        => 'lib/Marpa/Support.pod',
        'lib/Marpa/Doc/Tracing.pod'        => 'lib/Marpa/Tracing.pod',
    },
    meta_merge => {
        resources =>
            { repository => 'git://github.com/jeffreykegler/Marpa.git', },
        no_index => {
            directory =>
                [qw( inc xs libmarpa lib/Marpa/UrHTML t author.t save )],
            namespace => [
                'Marpa::Internal',          'Marpa::MDL::Internal',
                'Marpa::MDLex::Internal',   'Marpa::UrHTML::Internal',
                'Marpa::Display::Internal', 'Marpa::Test',
                'Marpa::Recognizer',        'Marpa::Evaluator',
                'Marpa::Grammar',
            ],
            package => [

                # not public, at least for now
                qw(Marpa::Offset Marpa::Test Marpa::Display ),

                # permanently on the noindex list, as they are EOL'd
                # and will not be documented
                qw( Marpa::MDL Marpa::MDLex ),

                # moved elsewhere
                qw( Marpa::UrHTML ),

                # never intended to see the light of day
                qw(Marpa::Internal),

            ],
        },
    },
    meta_add => {
        distribution_type => 'module',
        provides          => {
            'Marpa' => {
                file    => 'lib/Marpa.pm',
                version => $marpa_version,
            },
        },
    },
    license            => 'perl',
    create_license     => 1,
    create_readme      => 0,
    create_makefile_pl => 'small',
    test_files         => [ map { glob $_ } qw(t/*.t t/*/*.t) ],
);

$build->create_build_script;
