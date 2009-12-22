#!perl

use 5.010;
use strict;
use warnings;
use English qw( -no_match_vars );
use List::Util;
use Test::More tests => 10;
use Marpa::Test;
use Test::More;

BEGIN {
    Test::More::use_ok( 'Marpa::UrHTML', 'alpha' );
}

# Delete comments
# From notes: # 'delete_me' class

my $example1 = '<?pi><table><?pi><tr><td><table><?pi>x</table></table><?pi>';
my $example_y =
    q{<p>I say<q>hello</q>I don't know why you say<q>goodbye</q></p>};

{
    my $expected = $example1;
    my $result   = Marpa::UrHTML->new()->parse( \$example1 );
    Marpa::Test::is( ${$result}, $expected, 'no args is a straight copy' );
}

my $example2 = <<'END_OF_EXAMPLE';
I am text
<table>
<tr>Table Cell
</table>
I am text between tables
<tr>Cell in table with missing start tag
</table>
Text at the end
END_OF_EXAMPLE

my $expected2 = <<'END_OF_EXPECTED';
I am text

I am text between tables

Text at the end
END_OF_EXPECTED

{
    my $result = Marpa::UrHTML->new(
        { handlers => [ [ table => sub { return q{} } ] ] } )
        ->parse( \$example2 );
    Marpa::Test::is( ${$result}, $expected2, 'delete tables' );
}

my $comment_example = <<'END_OF_EXAMPLE';
I am text.
<!-- I am a comment -->
I am more text.
END_OF_EXAMPLE

my $comment_example_expected = <<'END_OF_EXPECTED';
I am text.

I am more text.
END_OF_EXPECTED

{
    my $result = Marpa::UrHTML->new(
        { handlers => [ [ ':COMMENT' => sub { return q{} } ] ] } )
        ->parse( \$comment_example );
    Marpa::Test::is( ${$result}, $comment_example_expected,
        'delete comment' );
}

my $delete_class_example = <<'END_OF_EXAMPLE';
I am text.
<span class="delete_me">I am text in a span</span>
I am more text.
END_OF_EXAMPLE

my $delete_class_expected = <<'END_OF_EXPECTED';
I am text.

I am more text.
END_OF_EXPECTED

{
    my $result = Marpa::UrHTML->new(
        { handlers => [ [ '.delete_me' => sub { return q{} } ] ] } )
        ->parse( \$delete_class_example );
    Marpa::Test::is( ${$result}, $delete_class_expected, 'delete by class' );
}

chomp( my $expected3 = <<'END_OF_EXPECTED');
<table>
<tr>Table Cell
</table><tr>Cell in table with missing start tag
</table>
END_OF_EXPECTED

{
    my $result = Marpa::UrHTML->new(
        {   handlers => [
                [ table => sub { return; } ],
                [ q{*}  => sub { return Marpa::UrHTML::contents(); } ],
                map {
                    [ $_ => sub { return q{} }, ]
                    } qw(:PI :DECL :COMMENT :WHITESPACE :CDATA :PCDATA :CRUFT),
            ],
        },
    )->parse( \$example2 );
    Marpa::Test::is( ${$result}, $expected3,
        'delete everything but tables: natural' );
}

{
    my $result = Marpa::UrHTML->new(
        {   handlers => [
                [ table => sub { return Marpa::UrHTML::original() } ],
                [   ':TOP' => sub {
                        return join q{}, @{ Marpa::UrHTML::child_values() };
                    },
                ],
            ]
        }
    )->parse( \$example2 );
    Marpa::Test::is( $result, $expected3,
        'delete everything but tables: fast' );
}

{

    sub compute_depth {
        return List::Util::max( 0,
            map { ${$_} } grep { ref $_ }
            map { $_->[0] } @{ Marpa::UrHTML::child_data('value') } );
    }
    my $result = Marpa::UrHTML->new(
        {   handlers => [
                [ q{*} => sub { return \( 1 + compute_depth() ) }, ],
                [   ':TOP' =>
                        sub { return 'Maximum depth = ' . compute_depth() }
                ],
            ]
        }
    )->parse( \$example1 );
    Marpa::Test::is( $result, 'Maximum depth = 10', 'maximum depth' );
}

{
    chomp( my $expected = <<'END_OF_EXPECTED');
I am body text
<!-- 24 characters of cruft on the next line -->
<head attr="I am cruft">I am more body text
END_OF_EXPECTED

    sub mark_cruft {
        my $literal = Marpa::UrHTML::literal();
        my ( $dummy, $line ) = Marpa::UrHTML::offset();
        return
              "\n<!-- "
            . ( length $literal )
            . " characters of cruft on the next line -->\n"
            . $literal;
    } ## end sub mark_cruft
    my $cruft_example =
        'I am body text<head attr="I am cruft">I am more body text';
    my $result =
        Marpa::UrHTML->new( { handlers => [ [ ':CRUFT' => \&mark_cruft ] ] } )
        ->parse( \$cruft_example );
    Marpa::Test::is( ${$result}, $expected, 'cruft marking' );
}

SKIP: {
    Test::More::skip 'test requires HTML::Tagset', 1
        if not eval { require HTML::Tagset };

    my $expected = <<'END_OF_EXPECTED';
<?pi>
<!-- Missing start tag for html element -->

<!-- Missing start tag for head element -->

<!-- Missing end tag for head element -->

<!-- Missing start tag for body element -->
<table><?pi>
<!-- Missing start tag for tbody element -->
<tr><td><table><?pi>
<!-- Missing start tag for tbody element -->

<!-- Missing start tag for tr element -->

<!-- Missing start tag for td element -->
x
<!-- Missing end tag for td element -->

<!-- Missing end tag for tr element -->

<!-- Missing end tag for tbody element -->
</table>
<!-- Missing end tag for td element -->

<!-- Missing end tag for tr element -->

<!-- Missing end tag for tbody element -->
</table><?pi>
<!-- Missing end tag for body element -->

<!-- Missing end tag for html element -->
END_OF_EXPECTED

    sub mark_missing_tags {
        my $tagname = Marpa::UrHTML::tagname();
        ## no critic (Variables::ProhibitPackageVars)
        return if $HTML::Tagset::emptyElement{$tagname};
        ## use critic
        my $literal = (
            Marpa::UrHTML::start_tag()
            ? q{}
            : qq{\n<!-- Missing start tag for $tagname element -->\n}
        ) . Marpa::UrHTML::literal();
        !Marpa::UrHTML::end_tag()
            and $literal
            .= qq{\n<!-- Missing end tag for $tagname element -->\n};
        return $literal;
    } ## end sub mark_missing_tags
    my $result = Marpa::UrHTML->new(
        {   handlers => [
                [ q{*} => \&mark_missing_tags ],
                [   ':TOP' => sub {
                        my $r = Marpa::UrHTML::literal();
                        $r =~ s/^$//xmsg;
                        return $r;
                        }
                ],
            ]
        }
    )->parse( \$example1 );
    Marpa::Test::is( $result, $expected, 'missing tag marking' );

} ## end SKIP:

