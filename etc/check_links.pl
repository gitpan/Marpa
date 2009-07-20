#!/usr/bin/perl

use 5.010;
use strict;
use warnings;

use LWP::UserAgent;
use URI::URL;
use HTML::LinkExtor;
use English qw( -no_match_vars );
use Fatal qw(open close);

use constant OK => 200;

my $fh;
open $fh, q{<}, 'lib/Marpa.pm';
LINE: while ( my $line = <$fh> ) {
    if ($line =~ m{
            ([\$*])
            (
                ([\w\:\']*)
                \b
                VERSION
            ) \b .* \=
            }xms
        )
    {
        {

            package Marpa;
            ## no critic (BuiltinFunctions::ProhibitStringyEval)
            my $retval = eval $line;
            ## use critic
            if ( not defined $retval ) {
                Carp::croak("eval of $line failed");
            }
            last LINE;
        }
    } ## end if ( $line =~ m{ ) (})
} ## end while ( my $line = <$fh> )
close $fh;

my $cpan_base = 'http://search.cpan.org';
my $marpa_doc_base =
    $cpan_base . '/~jkegl/Marpa-' . $Marpa::VERSION . '/lib/Test/';

print "Starting at $marpa_doc_base\n"
    or Carp::croak("Cannot print: $ERRNO");

my @url = qw(
    Marpa.pm
    Marpa/Doc/Algorithm.pod
    Marpa/Doc/Bibliography.pod
    Marpa/Doc/Debugging.pod
    Marpa/Doc/Internals.pod
    Marpa/Doc/MDL.pod
    Marpa/Doc/Options.pod
    Marpa/Doc/Parse_Terms.pod
    Marpa/Doc/Plumbing.pod
    Marpa/Doc/To_Do.pod
    Marpa/Evaluator.pm
    Marpa/Grammar.pm
    Marpa/Lex.pm
    Marpa/MDL.pm
    Marpa/Recognizer.pm
);

my %link;

sub cb {
    my ( $tag, %links ) = @_;
    return if $tag ne 'a';
    my $href = $links{href};
    return if $href =~ /\A#/xms;
    return $link{$href} = 1;
} ## end sub cb

my %link_ok;

$OUTPUT_AUTOFLUSH = 1;

PAGE: for my $url (@url) {
    $url = $marpa_doc_base . $url;

    my $p  = HTML::LinkExtor->new( \&cb );
    my $ua = LWP::UserAgent->new;

    %link = ();

    # Request document and parse it as it arrives
    my $response = $ua->request( HTTP::Request->new( GET => $url ),
        sub { $p->parse( $_[0] ) } );

    my $page_response_status_line = $response->status_line;
    if ( $response->code != OK ) {
        say 'PAGE: ', $page_response_status_line, q{ }, $url;
        next PAGE;
    }

    LINK: for my $link ( keys %link ) {

        if ( $link =~ m{\A/}xms ) {
            $link = 'http://search.cpan.org' . $link;
        }
        next LINK if $link_ok{$link};

        my $link_response =
            $ua->request( HTTP::Request->new( GET => $link ) );

        if ( $link_response->code == OK ) {
            $link_ok{$link} = 1;
            print q{.}
                or Carp::croak('Cannot print to STDOUT');
            next LINK;
        } ## end if ( $link_response->code == OK )

        say 'LINK: ', $link_response->status_line, q{ }, $link;

    } ## end for my $link ( keys %link )

    say " PAGE: $page_response_status_line: $url";

} ## end for my $url (@url)
