#!perl

use 5.010;
use strict;
use warnings;
use English qw( -no_match_vars );
use List::Util;
use Test::More;
use Marpa::Test;

# Non-synopsis example in UrHTML.pod

BEGIN {
    if ( eval { require HTML::PullParser } ) {
        Test::More::plan tests => 4;
    }
    else {
        Test::More::plan skip_all => 'HTML::PullParser not available';
    }
    Test::More::use_ok('Marpa');
    Test::More::use_ok('Marpa::UrHTML');
} ## end BEGIN

# Marpa::Display
# name: 'UrHTML Pod: Handler Precedence'

my $html = <<'END_OF_HTML';
<span class="high">High Span</span>
<span class="low">Low Span</span>
<div class="high">High Div</div>
<div class="low">Low Div</div>
<div class="oddball">Oddball Div</div>
END_OF_HTML

my $result = Marpa::UrHTML::urhtml(
    \$html,
    {   q{*} => sub {
            return 'wildcard handler: ' . Marpa::UrHTML::contents();
        },
        'head' => sub { return Marpa::UrHTML::literal() },
        'html' => sub { return Marpa::UrHTML::literal() },
        'body' => sub { return Marpa::UrHTML::literal() },
        'div'  => sub {
            return '"div" handler: ' . Marpa::UrHTML::contents();
        },
        '.high' => sub {
            return '".high" handler: ' . Marpa::UrHTML::contents();
        },
        'div.high' => sub {
            return '"div.high" handler: ' . Marpa::UrHTML::contents();
        },
        '.oddball' => sub {
            return '".oddball" handler: ' . Marpa::UrHTML::contents();
        },
    }
);

# Marpa::Display::End

# Marpa::Display
# name: 'UrHTML Pod: Handler Precedence Result'
# start-after-line: EXPECTED_RESULT
# end-before-line: '^EXPECTED_RESULT$'

my $expected_result = <<'EXPECTED_RESULT';
".high" handler: High Span
wildcard handler: Low Span
"div.high" handler: High Div
"div" handler: Low Div
".oddball" handler: Oddball Div
EXPECTED_RESULT

# Marpa::Display::End

Marpa::Test::is( ${$result}, $expected_result, 'handler precedence example' );

# Marpa::Display
# name: 'UrHTML Pod: Structure vs. Element Example'
# start-after-line: END_OF_EXAMPLE
# end-before-line: '^END_OF_EXAMPLE$'

my $tagged_html_example = <<'END_OF_EXAMPLE';
    <title>Short</title><p>Text</head><head>
END_OF_EXAMPLE

# Marpa::Display::End

my $expected_structured_result = <<'END_OF_EXPECTED';
    <html>
<head>
<title>Short</title></head>
<body>
<p>Text</head><head>
</p>
</body>
</html>
END_OF_EXPECTED

sub supply_missing_tags {
    my $tagname = Marpa::UrHTML::tagname();
    return
          ( Marpa::UrHTML::start_tag() // "<$tagname>\n" )
        . Marpa::UrHTML::contents()
        . ( Marpa::UrHTML::end_tag() // "</$tagname>\n" );
} ## end sub supply_missing_tags
my $structured_html_ref =
    Marpa::UrHTML::urhtml( \$tagged_html_example,
    { q{*} => \&supply_missing_tags } );

Marpa::Test::is( ${$structured_html_ref}, $expected_structured_result,
    'structure vs. tags example' );

