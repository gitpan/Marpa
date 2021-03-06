#!/usr/bin/perl

use 5.010;
use strict;
use warnings;
use Carp;
use Fatal qw(open close);

use Module::Build;

my $marpa_version = '0.208000';

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

my %pod_files = map { ( "lib/Marpa/Doc/$_" => "lib/Marpa/$_" ) } qw(
    Advanced/Algorithm.pod
    Advanced/Bibliography.pod
    Advanced/Implementation.pod
    Advanced/Models.pod
    Deprecated/Bocage.pod
    Deprecated/Evaluator.pod
    Deprecated/Implementation.pod
    Grammar.pod
    Marpa.pod
    Parse_Terms.pod
    Recognizer.pod
    Semantics.pod
    Support.pod
    Tracing.pod
);

my @no_index_namespace_inclusive = (

    # not for now, at least
    qw(Marpa::Perl Marpa::Offset Marpa::Test Marpa::Display ),

    # permanently on the noindex list, as they are EOL'd
    # and will not be documented
    qw( Marpa::MDL Marpa::MDLex Marpa::Evaluator ),

    # moved elsewhere
    qw( Marpa::UrHTML ),

    # never intended to see the light of day
    qw( Marpa::Internal),

);

my $build = $class->new(
    dist_name     => 'Marpa',
    dist_author   => 'Jeffrey Kegler',
    dist_version  => $marpa_version,
    dist_abstract => 'Generate Parsers from any BNF grammar',
    recommends    => {
        'PPI'        => '1.206',
        'Test::More' => '0.94',
    },
    configure_requires => { 'Module::Build' => 0.38 },
    requires => {
        'Scalar::Util'     => 0,
        'HTML::Parser'     => '3.64',
        'HTML::PullParser' => '3.57',
    },
    build_requires => { 'Test::More' => 0.94 },
    PL_files       => {},            # workaround Module::Build bug
    meta_merge     => {
        resources =>
            { repository => 'git://github.com/jeffreykegler/Marpa.git', },
        no_index => {
            directory =>
                [qw( inc xs lib/Marpa/UrHTML t author.t save )],
            namespace => [
                'Marpa::Recognizer', 'Marpa::Grammar',
                @no_index_namespace_inclusive
            ],
            package => [@no_index_namespace_inclusive],
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
    pod_files          => \%pod_files,
    license            => 'perl',
    create_readme      => 0,
    create_makefile_pl => 'small',
    test_files         => [ map { glob $_ } qw(t/*.t t/*/*.t) ],
);

$build->create_build_script;
