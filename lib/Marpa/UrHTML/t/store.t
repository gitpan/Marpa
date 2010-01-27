#!/usr/bin/perl

use 5.010;
use strict;
use warnings;

use lib 'lib';
use Test::More;

# This test uses the literal_ref() call for historic reasons --
# that's how it's test file was created.
# Anyway, it's good that that form is tested somewhere.

BEGIN {
    if ( not eval { require JSON } ) {
        Test::More::plan skip_all => 'JSON not available';
    }
    my $json_version = $JSON::VERSION;
    if ( $json_version < 2.0 ) {
        Test::More::plan skip_all =>
            "JSON is $json_version: at least 2.0 required";
    }
    if ( not eval { require HTML::PullParser } ) {
        Test::More::plan skip_all => 'HTML::PullParser not available';
    }
    Test::More::plan tests => 2;
    Test::More::use_ok('Marpa::UrHTML');
} ## end BEGIN

use Carp;
use Data::Dumper;
use English qw( -no_match_vars );
use Fatal qw(open close binmode);

my $document;
{
    local $RS = undef;
    open my $fh, q{<:utf8}, 'lib/Marpa/UrHTML/t/test.html';
    $document = <$fh>;
    close $fh
};

my %handlers = (
    ':TOP' => sub {
        return $Marpa::UrHTML::INSTANCE;
    },
    '.codepoint' => sub {
        CHILD: for my $value ( @{ ( Marpa::UrHTML::values() ) } ) {
            next CHILD if not $value;
            my ( $class, $literal, $data ) = @{$value};
            if ( $class eq 'occurrences' ) {
                $Marpa::UrHTML::INSTANCE->{ Marpa::UrHTML::title() }
                    ->{occurrence_count} = $data;
            }
            $Marpa::UrHTML::INSTANCE->{ Marpa::UrHTML::title() }->{$class} =
                $literal;
        } ## end for my $value ( @{ ( Marpa::UrHTML::values() ) } )
        return;
    },
    '.occurrences' => sub {
        my $literal = Marpa::UrHTML::literal();
        my ($occurrence_count) =
            ( $literal =~ / Occurrences \s+ [(] (\d+) [)] [:] /xms );
        return [ 'occurrences', $literal, $occurrence_count ];
    },
);

my @text_fields = qw( cedict_definition glyph kfrequency kgradelevel
    kiicore kmandarin kmatthews krskangxi
    krsunicode ktang ktotalstrokes shrift_notes
    shrift_occurrences unicode_value unihan_definition );

for my $text_field (@text_fields) {
    $handlers{".$text_field"} =
        sub { return [ $text_field, Marpa::UrHTML::literal() ] };
}

my $value = Marpa::UrHTML::urhtml( \$document, \%handlers );

my $expected_json = do {
    local $RS = undef;
    <main::DATA>;
};
my $expected = JSON::decode_json $expected_json;

Test::More::is_deeply( $value, $expected, 'comparison with stored JSON' );

__DATA__
{
   "u+4e00" : {
      "kmatthews" : "<div class=\"kMatthews\">\n<span class=\"codepoint_datum_label\">Mathews</span>:\n3016\n</div>",
      "krskangxi" : "<div class=\"kRSKangXi\">\n<span class=\"codepoint_datum_label\">Kang Xi RS</span>:\n1.0\n</div>",
      "occurrences" : "<div class=\"occurrences\">\n<span class=\"codepoint_datum_label\">Occurrences</span>:\nOccurrences (7): 32.5x:5, 38.6:8, 41.3:5, 41.3:7, 41.3x:0, 45.1:10, 56.5:2\n</div>",
      "ktang" : "<div class=\"kTang\">\n<span class=\"codepoint_datum_label\">Tang Pronunciation</span>:\n*qit qit\n</div>",
      "kfrequency" : "<div class=\"kFrequency\">\n<span class=\"codepoint_datum_label\">Frequency</span>:\n1\n</div>",
      "unicode_value" : "<div class=\"unicode_value\">\nU+4E00\n</div>",
      "shrift_occurrences" : "<div class=\"shrift_occurrences\">\n<span class=\"codepoint_datum_label\">Shrift Occurrences</span>:\nShrift Occurrences (4): 41.3: one; 41.3: one; 41.3x: one; 45.1: one\n</div>",
      "occurrence_count" : "7",
      "cedict_definition" : "<div class=\"cedict_definition\">\n<span class=\"codepoint_datum_label\">CEDICT Definition</span>:\n/one/1/single/a (article)/as soon as/entire/whole/all/throughout/\r\n</div>",
      "kiicore" : "<div class=\"kIICore\">\n<span class=\"codepoint_datum_label\">IICore</span>:\n2.1\n</div>",
      "shrift_notes" : "<div class=\"shrift_notes\">\n<span class=\"codepoint_datum_label\">Notes</span>:\none\n</div>",
      "krsunicode" : "<div class=\"kRSUnicode\">\n<span class=\"codepoint_datum_label\">Unicode RS</span>:\n1.0\n</div>",
      "ktotalstrokes" : "<div class=\"kTotalStrokes\">\n<span class=\"codepoint_datum_label\">Total Strokes</span>:\n1\n</div>",
      "unihan_definition" : "<div class=\"unihan_definition\">\n<span class=\"codepoint_datum_label\">Unihan Definition</span>:\none; a, an; alone\n</div>",
      "kmandarin" : "<div class=\"kMandarin\">\n<span class=\"codepoint_datum_label\">Mandarin</span>:\nyi1\n</div>",
      "kgradelevel" : "<div class=\"kGradeLevel\">\n<span class=\"codepoint_datum_label\">Grade Level</span>:\n1\n</div>",
      "glyph" : "<div class=\"glyph\">一</div>"
   },
   "u+4e09" : {
      "kmatthews" : "<div class=\"kMatthews\">\n<span class=\"codepoint_datum_label\">Mathews</span>:\n5415\n</div>",
      "krskangxi" : "<div class=\"kRSKangXi\">\n<span class=\"codepoint_datum_label\">Kang Xi RS</span>:\n1.2\n</div>",
      "occurrences" : "<div class=\"occurrences\">\n<span class=\"codepoint_datum_label\">Occurrences</span>:\nOccurrences (30): 04.0:15, 05.6:8, 06.2:9, 06.6:7, 07.2:7, 07.2x:8, 08.5:4, 13.3:8, 13.3x:6, 18.0:13, 18.0:9, 29.6:8, 29.6x:5, 35.0:10, 36.1:11, 40.2:2, 41.3:0, 41.3x:3, 47.1:9, 49.3:6, 49.3x:2, 53.5:5, 55.6:13, 57.4:4, 57.4x:2, 57.5:13, 57.5:17, 63.3:5, 63.3x:0, 64.4:9\n</div>",
      "ktang" : "<div class=\"kTang\">\n<span class=\"codepoint_datum_label\">Tang Pronunciation</span>:\n*sɑm sɑm\n</div>",
      "kfrequency" : "<div class=\"kFrequency\">\n<span class=\"codepoint_datum_label\">Frequency</span>:\n2\n</div>",
      "unicode_value" : "<div class=\"unicode_value\">\nU+4E09\n</div>",
      "shrift_occurrences" : "<div class=\"shrift_occurrences\">\n<span class=\"codepoint_datum_label\">Shrift Occurrences</span>:\nShrift Occurrences (3): 05.6: three; 41.3: three; 41.3x: three\n</div>",
      "occurrence_count" : "30",
      "cedict_definition" : "<div class=\"cedict_definition\">\n<span class=\"codepoint_datum_label\">CEDICT Definition</span>:\n/three/3/\r\n</div>",
      "kiicore" : "<div class=\"kIICore\">\n<span class=\"codepoint_datum_label\">IICore</span>:\n2.1\n</div>",
      "shrift_notes" : "<div class=\"shrift_notes\">\n<span class=\"codepoint_datum_label\">Notes</span>:\nthree\n</div>",
      "krsunicode" : "<div class=\"kRSUnicode\">\n<span class=\"codepoint_datum_label\">Unicode RS</span>:\n1.2\n</div>",
      "ktotalstrokes" : "<div class=\"kTotalStrokes\">\n<span class=\"codepoint_datum_label\">Total Strokes</span>:\n3\n</div>",
      "unihan_definition" : "<div class=\"unihan_definition\">\n<span class=\"codepoint_datum_label\">Unihan Definition</span>:\nthree\n</div>",
      "kmandarin" : "<div class=\"kMandarin\">\n<span class=\"codepoint_datum_label\">Mandarin</span>:\nsan1 san4\n</div>",
      "kgradelevel" : "<div class=\"kGradeLevel\">\n<span class=\"codepoint_datum_label\">Grade Level</span>:\n1\n</div>",
      "glyph" : "<div class=\"glyph\">三</div>"
   },
   "u+4e03" : {
      "kmatthews" : "<div class=\"kMatthews\">\n<span class=\"codepoint_datum_label\">Mathews</span>:\n579\n</div>",
      "krskangxi" : "<div class=\"kRSKangXi\">\n<span class=\"codepoint_datum_label\">Kang Xi RS</span>:\n1.1\n</div>",
      "occurrences" : "<div class=\"occurrences\">\n<span class=\"codepoint_datum_label\">Occurrences</span>:\nOccurrences (4): 24.0:14, 51.2:12, 63.2:6, 63.2x:0\n</div>",
      "ktang" : "<div class=\"kTang\">\n<span class=\"codepoint_datum_label\">Tang Pronunciation</span>:\n*tsit tsit\n</div>",
      "kfrequency" : "<div class=\"kFrequency\">\n<span class=\"codepoint_datum_label\">Frequency</span>:\n3\n</div>",
      "unicode_value" : "<div class=\"unicode_value\">\nU+4E03\n</div>",
      "occurrence_count" : "4",
      "cedict_definition" : "<div class=\"cedict_definition\">\n<span class=\"codepoint_datum_label\">CEDICT Definition</span>:\n/seven/7/\r\n</div>",
      "kiicore" : "<div class=\"kIICore\">\n<span class=\"codepoint_datum_label\">IICore</span>:\n2.1\n</div>",
      "krsunicode" : "<div class=\"kRSUnicode\">\n<span class=\"codepoint_datum_label\">Unicode RS</span>:\n1.1\n</div>",
      "ktotalstrokes" : "<div class=\"kTotalStrokes\">\n<span class=\"codepoint_datum_label\">Total Strokes</span>:\n2\n</div>",
      "unihan_definition" : "<div class=\"unihan_definition\">\n<span class=\"codepoint_datum_label\">Unihan Definition</span>:\nseven\n</div>",
      "kmandarin" : "<div class=\"kMandarin\">\n<span class=\"codepoint_datum_label\">Mandarin</span>:\nqi1\n</div>",
      "glyph" : "<div class=\"glyph\">七</div>",
      "kgradelevel" : "<div class=\"kGradeLevel\">\n<span class=\"codepoint_datum_label\">Grade Level</span>:\n1\n</div>"
   },
   "u+4e08" : {
      "kmatthews" : "<div class=\"kMatthews\">\n<span class=\"codepoint_datum_label\">Mathews</span>:\n200\n</div>",
      "krskangxi" : "<div class=\"kRSKangXi\">\n<span class=\"codepoint_datum_label\">Kang Xi RS</span>:\n1.2\n</div>",
      "occurrences" : "<div class=\"occurrences\">\n<span class=\"codepoint_datum_label\">Occurrences</span>:\nOccurrences (4): 07.0:2, 17.2:4, 17.3:1, 17.3x:1\n</div>",
      "ktang" : "<div class=\"kTang\">\n<span class=\"codepoint_datum_label\">Tang Pronunciation</span>:\n*djhiɑ̌ng djhiɑ̌ng\n</div>",
      "kfrequency" : "<div class=\"kFrequency\">\n<span class=\"codepoint_datum_label\">Frequency</span>:\n4\n</div>",
      "unicode_value" : "<div class=\"unicode_value\">\nU+4E08\n</div>",
      "shrift_occurrences" : "<div class=\"shrift_occurrences\">\n<span class=\"codepoint_datum_label\">Shrift Occurrences</span>:\nShrift Occurrences (2): 17.3: experienced; 17.3x: experienced\n</div>",
      "occurrence_count" : "4",
      "cedict_definition" : "<div class=\"cedict_definition\">\n<span class=\"codepoint_datum_label\">CEDICT Definition</span>:\n/ten feet/\r\n</div>",
      "kiicore" : "<div class=\"kIICore\">\n<span class=\"codepoint_datum_label\">IICore</span>:\n2.1\n</div>",
      "shrift_notes" : "<div class=\"shrift_notes\">\n<span class=\"codepoint_datum_label\">Notes</span>:\nexperienced\n</div>",
      "krsunicode" : "<div class=\"kRSUnicode\">\n<span class=\"codepoint_datum_label\">Unicode RS</span>:\n1.2\n</div>",
      "ktotalstrokes" : "<div class=\"kTotalStrokes\">\n<span class=\"codepoint_datum_label\">Total Strokes</span>:\n3\n</div>",
      "unihan_definition" : "<div class=\"unihan_definition\">\n<span class=\"codepoint_datum_label\">Unihan Definition</span>:\nunit of length equal 3.3 meters; gentleman, man, husband\n</div>",
      "kmandarin" : "<div class=\"kMandarin\">\n<span class=\"codepoint_datum_label\">Mandarin</span>:\nzhang4\n</div>",
      "kgradelevel" : "<div class=\"kGradeLevel\">\n<span class=\"codepoint_datum_label\">Grade Level</span>:\n2\n</div>",
      "glyph" : "<div class=\"glyph\">丈</div>"
   }
}
