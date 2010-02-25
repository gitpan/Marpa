#!perl

use 5.010;
use strict;
use warnings;
use English qw( -no_match_vars );
use List::Util;
use Test::More tests => 12;
Test::More::use_ok('HTML::PullParser');
Test::More::use_ok('Marpa');
Test::More::use_ok('Marpa::Test');

# This is just a dummy value for the synopsis
my %empty_elements = ();

# Marpa::Display
# name: 'UrHTML Synopsis: Delete Tables'

use Marpa::UrHTML qw(urhtml);

my $with_table = 'Text<table><tr><td>I am a cell</table> More Text';
my $no_table   = urhtml(
    \$with_table,
    {   table => sub { return q{} }
    }
);

# Marpa::Display::End

# Marpa::Display
# name: 'UrHTML Synopsis: Delete Everything But Tables'

my %handlers_to_keep_only_tables = (
    table  => sub { return Marpa::UrHTML::original() },
    ':TOP' => sub { return \( join q{}, @{ Marpa::UrHTML::values() } ) }
);
my $only_table = urhtml( \$with_table, \%handlers_to_keep_only_tables );

# Marpa::Display::End

# Marpa::Display
# name: 'UrHTML Synopsis: Defective Tables'

my $with_bad_table = 'Text<tr>I am a cell</table> More Text';
my $only_bad_table =
    urhtml( \$with_bad_table, \%handlers_to_keep_only_tables );

# Marpa::Display::End

# Marpa::Display
# name: 'UrHTML Synopsis: Delete Comments'

my $with_comment = 'Text <!-- I am a comment --> I am not a comment';
my $no_comment   = urhtml(
    \$with_comment,
    {   ':COMMENT' => sub { return q{} }
    }
);

# Marpa::Display::End

# Marpa::Display
# name: 'UrHTML Synopsis: Change Title'

my $old_title = '<title>Old Title</title>A little html text';
my $new_title = urhtml(
    \$old_title,
    {   'title' => sub { return '<title>New Title</title>' }
    }
);

# Marpa::Display::End

# Marpa::Display
# name: 'UrHTML Synopsis: Delete by Class'

my $stuff_to_be_edited = '<p>A<p class="delete_me">B<p>C';
my $edited_stuff       = urhtml(
    \$stuff_to_be_edited,
    {   '.delete_me' => sub { return q{} }
    }
);

# Marpa::Display::End

# Marpa::Display
# name: 'UrHTML Synopsis: Supply Missing Tags'

sub supply_missing_tags {
    my $tagname = Marpa::UrHTML::tagname();
    return if $empty_elements{$tagname};
    return
          ( Marpa::UrHTML::start_tag() // "<$tagname>\n" )
        . Marpa::UrHTML::contents()
        . ( Marpa::UrHTML::end_tag() // "</$tagname>\n" );
} ## end sub supply_missing_tags
my $html_with_just_a_title = '<title>I am a title and That is IT!';
my $valid_html_with_all_tags =
    urhtml( \$html_with_just_a_title, { q{*} => \&supply_missing_tags } );

# Marpa::Display::End

# Marpa::Display
# name: 'UrHTML Synopsis: Maximum Element Depth'

sub depth_below_me {
    return List::Util::max( 0, @{ Marpa::UrHTML::values() } );
}
my %handlers_to_calculate_maximum_element_depth = (
    q{*}   => sub { return 1 + depth_below_me() },
    ':TOP' => sub { return depth_below_me() },
);
my $maximum_depth_with_just_a_title = urhtml( \$html_with_just_a_title,
    \%handlers_to_calculate_maximum_element_depth );

# Marpa::Display::End

my $maximum_depth_with_all_tags_supplied = urhtml( $valid_html_with_all_tags,
    \%handlers_to_calculate_maximum_element_depth );
Marpa::Test::is( $maximum_depth_with_just_a_title,
    3, 'compute maximum depth' );
Marpa::Test::is(
    $maximum_depth_with_just_a_title,
    $maximum_depth_with_all_tags_supplied,
    'compare maximum depths'
);

my $expected_valid_html_with_all_tags = <<'END_OF_EXPECTED';
<html>
<head>
<title>I am a title and That is IT!</title>
</head>
<body>
</body>
</html>
END_OF_EXPECTED

Marpa::Test::is( ${$no_table}, 'Text More Text', 'delete tables' );
Marpa::Test::is(
    ${$only_table},
    '<table><tr><td>I am a cell</table>',
    'keep only tables'
);
Marpa::Test::is(
    ${$only_bad_table},
    '<tr>I am a cell</table>',
    'keep only tables -- bad table'
);
Marpa::Test::is(
    ${$no_comment},
    'Text  I am not a comment',
    'delete comments'
);
Marpa::Test::is(
    ${$new_title},
    '<title>New Title</title>A little html text',
    'replace title'
);
Marpa::Test::is( ${$edited_stuff}, '<p>A<p>C', 'delete by class name' );
Marpa::Test::is(
    ${$valid_html_with_all_tags},
    $expected_valid_html_with_all_tags,
    'supply tags'
);
