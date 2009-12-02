#!perl

use strict;
use warnings;

# These tests are based closely on those in the HTML-Tree module,
# the authors of which I gratefully acknowledge.

use Test::More tests => 40;
my $DEBUG = 2;

BEGIN {
    ## no critic (BuiltinFunctions::ProhibitStringyEval)
    ## no critic (ErrorHandling::RequireCheckingReturnValueOfEval)
    eval 'require HTML::Entities';
    ## use critic
} ## end BEGIN

use Marpa::UrHTML;

my $urhtml_args = {
    handlers => [
        [   ':CRUFT' => sub {
                my $literal = Marpa::UrHTML::literal();
                say STDERR 'Cruft: ', $literal;
                return qq{<CRUFT literal="$literal">};
                }
        ],
        [   ':PCDATA' => sub {
                my $literal = Marpa::UrHTML::literal();
                if ( defined &HTML::Entities::decode_entities ) {
                    $literal =
                        HTML::Entities::encode_entities(
                        HTML::Entities::decode_entities($literal) );
                }
                return $literal;
                }
        ],
        [   ':PROLOG' => sub {
                my $literal = Marpa::UrHTML::literal();
                $literal =~ s/\A [\x{20}\t\f\x{200B}]+ //xms;
                $literal =~ s/ [\x{20}\t\f\x{200B}]+ \z//xms;
                return $literal;
                }
        ],
        [ ':COMMENT' => sub { return q{} } ],
        [   q{*} => (
                sub {
                    my $tagname = Marpa::UrHTML::tagname();

                    # say STDERR "In handler for $tagname element";

                    Carp::croak('Not in an element') if not $tagname;
                    my $attributes = Marpa::UrHTML::attributes();

                    # Note this logic suffices to get through
                    # the test set but it does not handle
                    # the necessary escaping for a production
                    # version
                    my $start_tag = "<$tagname";
                    for my $attribute ( sort keys %{$attributes} ) {
                        $start_tag .= qq{ $attribute="}
                            . $attributes->{$attribute} . q{"};
                    }
                    $start_tag .= '>';
                    my $end_tag = "</$tagname>";

                    my $child_data = Marpa::UrHTML::child_data(
                        'token_type,literal,element');

                    # For UL element, eliminate all but the LI element children
                    if ( $tagname eq 'ul' ) {
                        $child_data =
                            [ grep { defined $_->[2] and $_->[2] eq 'li' }
                                @{$child_data} ];
                    }

                    my $contents = join q{}, map { $_->[1] }
                        grep {
                               not defined $_->[0]
                            or not $_->[0] ~~
                            [qw(S E)]
                        } @{$child_data};
                    $contents =~ s/\A [\x{20}\t\f\x{200B}]+ //xms;
                    $contents =~ s/ [\x{20}\t\f\x{200B}]+ \z//xms;
                    return join q{}, $start_tag, $contents, $end_tag;
                }
            )
        ]
    ]
};

Test::More::ok 1;

Test::More::ok same(
    '<title>foo</title><p>I like pie',
    '<html><head><title>foo</title></head><body><p>I like pie</p></body></html>'
);

Test::More::ok !same( 'x'   => 'y', 1 );
Test::More::ok !same( '<p>' => 'y', 1 );

Test::More::ok same( q{} => q{} );
Test::More::ok same( q{} => q{ } );
Test::More::ok same( q{} => q{  } );

Test::More::ok same( q{} => '<!-- tra la la -->' );
Test::More::ok same( q{} => '<!-- tra la la --><!-- foo -->' );

Test::More::ok same( q{} => \'<head></head><body></body>' );

Test::More::ok same( '<head>' => q{} );

Test::More::ok same( '<head></head><body>' => \'<head></head><body></body>' );

Test::More::ok same(
    '<img alt="456" src="123">' => '<img src="123" alt="456">' );
Test::More::ok same(
    '<img alt="456" src="123">' => '<img src="123"    alt="456">' );
Test::More::ok same(
    '<img alt="456" src="123">' => '<img src="123"    alt="456"   >' );

Test::More::ok !same(
    '<img alt="456" >' => '<img src="123"    alt="456"   >',
    1
);

SKIP: {
    ## no critic (ValuesAndExpressions::ProhibitMagicNumbers)
    defined &HTML::Entities::decode_entities
        or Test::More::skip 'HTML::Entities not installed', 6;
    ## use critic

    Test::More::ok same( 'abc&#32;xyz'  => 'abc xyz' );
    Test::More::ok same( 'abc&#x20;xyz' => 'abc xyz' );

    Test::More::ok same( 'abc&#43;xyz'  => 'abc+xyz' );
    Test::More::ok same( 'abc&#x2b;xyz' => 'abc+xyz' );

    Test::More::ok same( '&#97;bc+xyz'  => 'abc+xyz' );
    Test::More::ok same( '&#x61;bc+xyz' => 'abc+xyz' );

} ## end SKIP:

# Now some list tests.

Test::More::ok same( '<ul><li>x</ul>after' => '<ul><li>x</li></ul>after' );
Test::More::ok same(
    '<ul><li>x<li>y</ul>after' => '<ul><li>x</li><li>y</li></ul>after' );

Test::More::ok same( '<ul> <li>x</li> <li>y</li> </ul>after' =>
        '<ul><li>x</li><li>y</li></ul>after' );

Test::More::ok same( '<ul><li>x<li>y</ul>after' => \
        '<head></head><body><ul><li>x</li><li>y</li></ul>after</body>' );

# Now some table tests.

Test::More::ok same( '<table>x<td>y<td>z' =>
        '<table><tr><td>x</td><td>y</td><td>z</td></table>' );

Test::More::ok same( '<table>x<td>y<tr>z' =>
        '<table><tr><td>x</td><td>y</td></tr><tr><td>z</td></tr></table>' );

Test::More::ok same(
    '<table><tr><td>x</td><td>y</td></tr><tr><td>z</td></tr></table>' =>
        '<table><tr><td>x</td><td>y</td></tr><tr><td>z</td></tr></table>' );
Test::More::ok same(
    '<table><tr><td>x</td><td>y</td></tr><tr><td>z</td></tr></table>' => \
        '<head></head><body><table><tr><td>x</td><td>y</td></tr><tr><td>z</td></tr></table>'
);

Test::More::ok same( '<table>x' => '<td>x' );
Test::More::ok same( '<table>x' => '<table><td>x' );
Test::More::ok same( '<table>x' => '<tr>x' );
Test::More::ok same( '<table>x' => '<tr><td>x' );
Test::More::ok same( '<table>x' => '<table><tr>x' );
Test::More::ok same( '<table>x' => '<table><tr><td>x' );

# Now some p tests.

Test::More::ok same( '<p>x<p>y<p>z' => '<p>x</p><p>y</p><p>z' );
Test::More::ok same( '<p>x<p>y<p>z' => '<p>x</p><p>y<p>z</p>' );
Test::More::ok same( '<p>x<p>y<p>z' => '<p>x</p><p>y</p><p>z</p>' );
Test::More::ok same(
    '<p>x<p>y<p>z' => \'<head></head><body><p>x</p><p>y</p><p>z</p>' );

sub same {
    my ( $code1, $code2, $flip ) = @_;
    my $p1 = Marpa::UrHTML->new($urhtml_args);
    my $p2 = Marpa::UrHTML->new($urhtml_args);

    if ( ref $code1 ) { $code1 = ${$code1} }
    if ( ref $code2 ) { $code2 = ${$code2} }

    my $value1 = $p1->parse( \$code1 );
    my $value2 = $p2->parse( \$code2 );

    if ( not defined $value1 ) {
        print "No parse for $code1"
            or Carp::croak("Cannot print: $!");
        return $flip;
    }
    if ( not defined $value2 ) {
        print "No parse for $code2"
            or Carp::croak("Cannot print: $!");
        return $flip;
    }

    my $out1 = ${$value1};
    my $out2 = ${$value2};

    my $rv = ( $out1 eq $out2 );

    #print $rv? "RV TRUE\n" : "RV FALSE\n";
    #print $flip? "FLIP TRUE\n" : "FLIP FALSE\n";

    if ( $flip ? ( !$rv ) : $rv ) {
        if ( $DEBUG > 2 ) {
            print
                "In1 $code1\n",
                "In2 $code2\n", "Out1 $out1\n", "Out2 $out2\n", "\n\n"
                or Carp::croak("Cannot print: $!");
        } ## end if ( $DEBUG > 2 )
    } ## end if ( $flip ? ( !$rv ) : $rv )
    else {
        foreach my $line (
            q{},
            'The following failure is at ' . join( ' : ', caller ),
            'Explanation of failure: '
            . ( $flip ? 'same' : 'different' )
            . ' parse trees!',
            'Input code 1:',
            $code1,
            'Input code 2:',
            $code2,
            'Output tree (as XML) 1:',
            $out1,
            'Output tree (as XML) 2:',
            $out2,
            )
        {
            $line =~ s/\n/\n# /gxms;
            print qq{# $line\n}
                or Carp::croak("Cannot print: $!");
        } ## end foreach my $line ( q{}, 'The following failure is at ' . ...)
    } ## end else [ if ( $flip ? ( !$rv ) : $rv ) ]

    return $rv;
} ## end sub same

