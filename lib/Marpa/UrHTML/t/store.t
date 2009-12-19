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
    if ( eval { require HTML::PullParser } ) {
        Test::More::plan tests => 3;
    }
    else {
        Test::More::plan skip_all => 'HTML::PullParser not available';
    }
    Test::More::use_ok('Marpa', 'alpha');
    Test::More::use_ok('Marpa::UrHTML', 'alpha');
} ## end BEGIN

use Carp;
use Data::Dumper;
use English qw( -no_match_vars );
use Fatal qw(open close);
use Storable;
use File::Compare;

my $document;
{
    local $RS = undef;
    open my $fh, q{<:utf8}, 'lib/Marpa/UrHTML/t/test.html';
    $document = <$fh>;
    close $fh
};

my @handlers = (
    [   ':TOP' => sub {
            return $Marpa::UrHTML::INSTANCE;
            }
    ],
    [   '.codepoint' => sub {
            CHILD: for my $value ( @{ ( Marpa::UrHTML::child_values() ) } ) {
                next CHILD if not $value;
                my ( $class, $literal, $data ) = @{$value};
                if ( $class eq 'occurrences' ) {
                    $Marpa::UrHTML::INSTANCE->{ Marpa::UrHTML::title() }
                        ->{occurrence_count} = $data;
                }
                $Marpa::UrHTML::INSTANCE->{ Marpa::UrHTML::title() }
                    ->{$class} = $literal;
            } ## end for my $value ( @{ ( Marpa::UrHTML::child_values() ) ...})
            return;
            }
    ],
    [   '.occurrences' => sub {
            my $literal = Marpa::UrHTML::literal_ref();
            my ($occurrence_count) =
                ( ${$literal} =~ / Occurrences \s+ [(] (\d+) [)] [:] /xms );
            return [ 'occurrences', $literal, $occurrence_count ];
            }
    ],
);

my @text_fields = qw( cedict_definition glyph kfrequency kgradelevel
    kiicore kmandarin kmatthews krskangxi
    krsunicode ktang ktotalstrokes shrift_notes
    shrift_occurrences unicode_value unihan_definition );

for my $text_field (@text_fields) {
    push @handlers,
        [ ".$text_field" =>
            sub { return [ $text_field, Marpa::UrHTML::literal_ref() ] } ];
}

my $p = Marpa::UrHTML->new( { handlers => \@handlers, } );
my $value = $p->parse( \$document );

my $old = 'lib/Marpa/UrHTML/t/test.storable.old';
my $new = 'lib/Marpa/UrHTML/t/test.storable.new';
Storable::nstore $value, $new;

Test::More::ok( ( File::Compare::compare( $old, $new ) == 0 ),
    'conversion to stored form' );

__END__
