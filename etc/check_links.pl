#!/usr/bin/perl

use 5.010;
use strict;
use warnings;

use LWP::UserAgent;
use URI::URL;
use HTML::LinkExtor;
use English qw( -no_match_vars );
use Fatal qw(open close);
use CPAN;
use Getopt::Long;

my $verbose = 0;
Carp::croak("usage: $PROGRAM_NAME [--verbose=[0|1|2]")
    if not Getopt::Long::GetOptions( 'verbose=i' => \$verbose );

use constant OK => 200;

my @distributions =
    sort map { $_->[2] }
    CPAN::Shell->expand( 'Author', 'JKEGL' )->ls( 'Marpa-*', 2 );
my $most_recent_distribution = pop @distributions;
$most_recent_distribution =~ s/\.tar\.gz$//xms;

my $cpan_base      = 'http://search.cpan.org';
my $marpa_doc_base = $cpan_base . '/~jkegl/' . "$most_recent_distribution/";

if ($verbose) {
    print "Starting at $marpa_doc_base\n"
        or Carp::croak("Cannot print: $ERRNO");
}

$OUTPUT_AUTOFLUSH = 1;

my @doc_urls = ();

{
    my $p  = HTML::LinkExtor->new();
    my $ua = LWP::UserAgent->new;

    # Request document and parse it as it arrives
    my $response = $ua->request( HTTP::Request->new( GET => $marpa_doc_base ),
        sub { $p->parse( $_[0] ) } );

    my $page_response_status_line = $response->status_line;
    if ( $response->code != OK ) {
        say 'PAGE: ', $page_response_status_line, q{ }, $marpa_doc_base
            or Carp::croak("Cannot print: $ERRNO");
        next PAGE;
    }

    my @links =
        map { $_->[2] }
        grep { $_->[0] eq 'a' and $_->[1] eq 'href' and $_->[2] !~ /^[#]/xms }
        $p->links();
    @doc_urls = grep {/^lib\//xms} @links;
}

my %url_seen = ();

PAGE: for my $url (@doc_urls) {
    $url = $marpa_doc_base . $url;
    say "Examining document $url" or Carp::croak("Cannot print: $ERRNO");

    my $p  = HTML::LinkExtor->new();
    my $ua = LWP::UserAgent->new;

    # Request document and parse it as it arrives
    my $response = $ua->request( HTTP::Request->new( GET => $url ),
        sub { $p->parse( $_[0] ) } );

    my $page_response_status_line = $response->status_line;
    if ( $response->code != OK ) {
        say 'PAGE: ', $page_response_status_line, q{ }, $url
            or Carp::croak("Cannot print: $ERRNO");
        next PAGE;
    }

    my @links =
        map { $_->[2] }
        grep { $_->[0] eq 'a' and $_->[1] eq 'href' } $p->links();

    LINK: for my $link (@links) {

        given ($link) {
            when (/\A\//xms) {
                $link = 'http://search.cpan.org' . $link;
            }
            when (/\A[#]/xms) {
                $link = $url . $link;
            }
        } ## end given

        if ( $url_seen{$link}++ ) {
            $verbose < 2
                or say STDERR "Already tried $link"
                or Carp::croak("Cannot print: $ERRNO");
            next LINK;
        } ## end if ( $url_seen{$link}++ )
        $verbose < 1
            or say STDERR "Trying $link"
            or Carp::croak("Cannot print: $ERRNO");

        my $link_response =
            $ua->request( HTTP::Request->new( GET => $link ) );

        if ( $link_response->code == OK ) {
            $verbose
                or print {STDERR} q{.}
                or Carp::croak("Cannot print: $ERRNO");
            next LINK;
        } ## end if ( $link_response->code == OK )

        say 'LINK: ', $link_response->status_line, q{ }, $link
            or Carp::croak("Cannot print: $ERRNO");

    } ## end for my $link (@links)

    say " PAGE: $page_response_status_line: $url"
        or Carp::croak("Cannot print: $ERRNO");

} ## end for my $url (@doc_urls)
