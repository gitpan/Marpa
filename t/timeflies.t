#!/usr/bin/perl

# This example is from Ralf Muschall, who clearly knows English
# grammar better than most native speakers.  I've reworked the
# terminology to follow _A Comprehensive Grammar of the English
# Language_, by Quirk, Greenbaum, Leech and Svartvik.  My edition
# was the "Seventh (corrected) impression 1989".
#
# When it is not a verb, I treat "like"
# as a preposition in an adjunct of manner,
# as per 8.79, p. 557; 9.4, pp. 661; and 9.48, pp. 698-699.
#
# The saying "time flies like an arrow; fruit flies like a banana"
# is attributed to Groucho Marx, but there is no reason to believe
# he ever said it.  Apparently, the saying
# first appeared on the Usenet on net.jokes in 1982.
# I've documented this whole thing on Wikipedia:
# http://en.wikipedia.org/wiki/Time_flies_like_an_arrow
#
# The permalink is:
# http://en.wikipedia.org/w/index.php?title=Time_flies_like_an_arrow&oldid=311163283

use 5.010;
use strict;
use warnings;
use lib 'lib';
use English qw( -no_match_vars );

use Test::More tests => 2;
use t::lib::Marpa::Test;

BEGIN {
    Test::More::use_ok('Marpa');
}

my $grammar = Marpa::Grammar->new(
    {   start              => 'sentence',
        strip              => 0,
        default_lex_prefix => '\s+|\A',
        ## no critic (ValuesAndExpressions::RequireInterpolationOfMetachars)
        rules => [
            [   'sentence', [qw(subject verb adjunct)],
                q{ "sva($_[0];$_[1];$_[2])" }
            ],
            [   'sentence', [qw(subject verb object)],
                q{ "svo($_[0];$_[1];$_[2])" }
            ],
            [ 'adjunct', [qw(preposition object)], q{ "adju($_[0];$_[1])" } ],
            [ 'adjective', [qw(adjective_noun_lex)], q{ "adje($_[0])" } ],
            [ 'subject',   [qw(adjective noun)],     q{ "s($_[0];$_[1])" } ],
            [ 'subject',   [qw(noun)],               q{ "s($_[0])" } ],
            [ 'noun',      [qw(adjective_noun_lex)], q{ "n($_[0])" } ],
            [ 'verb',      [qw(verb_lex)],           q{ "v($_[0])" } ],
            [ 'object',    [qw(article noun)],       q{ "o($_[0];$_[1])" } ],
            [ 'article',   [qw(article_lex)],        q{ "art($_[0])" } ],
            [ 'preposition', [qw(preposition_lex)], q{ "pr($_[0])" } ],
        ],
        ## use critic
        terminals => [
            [ preposition_lex => { regex => qr/like/xms } ],
            [ verb_lex        => { regex => qr/like|flies/xms } ],
            [   adjective_noun_lex =>
                    { regex => qr/fruit|banana|time|arrow|flies/xms }
            ],
            [ article_lex => { regex => qr/a\b|an/xms } ],
        ]
    }
);

my $expected = <<'EOS';
sva(s(n(time));v(flies);adju(pr(like);o(art(an);n(arrow))))
svo(s(adje(time);n(flies));v(like);o(art(an);n(arrow)))
sva(s(n(fruit));v(flies);adju(pr(like);o(art(a);n(banana))))
svo(s(adje(fruit);n(flies));v(like);o(art(a);n(banana)))
EOS
my $actual = q{};

for my $data ( 'time flies like an arrow.', 'fruit flies like a banana.' ) {

    my $recce = Marpa::Recognizer->new( { grammar => $grammar } );
    my $fail_offset = $recce->text($data);
    if ( $fail_offset >= 0 ) {
        Carp::croak("Parse failed at offset $fail_offset");
    }
    $recce->end_input();

    my $evaler =
        Marpa::Evaluator->new( { recognizer => $recce, clone => 0 } );
    Carp::croak('Parse failed') if not $evaler;

    while ( defined( my $value = $evaler->value() ) ) {
        $actual .= ${$value} . "\n";
    }
} ## end for my $data ( 'time flies like an arrow.', ...)

Marpa::Test::is( $actual, $expected, 'Ambiguous English sentences' );
