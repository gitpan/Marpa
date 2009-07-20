package Marpa::Lex;

use 5.010;

use warnings;
use strict;

# It's all integers, except for the version number
use integer;

# \x{5c} is backslash
sub gen_bracket_regex {
    my ( $left_bracket, $right_bracket ) = @_;
    return qr{
        \G
        [^\Q$left_bracket$right_bracket\E\x{5c}]*
        (
              \Q$left_bracket\E
            | \Q$right_bracket\E
            | \x{5c}\Q$left_bracket\E
            | \x{5c}\Q$right_bracket\E
            | \x{5c}
        )
    }xms;
} ## end sub gen_bracket_regex

my %regex_data = (
    '{' => [ '}', gen_bracket_regex( '{', '}' ) ],
    '<' => [ '>', gen_bracket_regex( '<', '>' ) ],
    '[' => [ ']', gen_bracket_regex( '[', ']' ) ],
    '(' => [ ')', gen_bracket_regex( '(', ')' ) ],
);

# This is POSIX "punct" character class, except for backslash,
# and the right side bracketing symbols.
# hex 27 is single quote, hex 5b is the left square bracket.
## no critic (RegularExpressions::ProhibitUnusualDelimiters)
my $punct = qr'[!"#$%&\x{27}(*+,-./:;<=?\x{5b}^_`{|~@]'xms;
## use critic

sub lex_q_quote {
    my $string = shift;
    my $start  = shift;
    my ($left_bracket) = ( ${$string} =~ m/\Gqq?($punct)/xmsogc );
    return if not defined $left_bracket;

    my $regex_data = $regex_data{$left_bracket};
    if ( not defined $regex_data ) {

        # \x{5c} is backslash
        my $regex = qr{
                \G
                [^\Q$left_bracket\E\x{5c}]*
                (
                     \Q$left_bracket\E
                    |\x{5c}\Q$left_bracket\E
                    |\x{5c}
                )
            }xms;
        $regex_data{$left_bracket} = $regex_data = [ undef, $regex ];
    } ## end if ( not defined $regex_data )
    my ( $right_bracket, $regex ) = @{$regex_data};

    # unbracketed quote
    if ( not defined $right_bracket ) {
        MATCH: while ( ${$string} =~ /$regex/gcxms ) {
            next MATCH if not defined $1;
            if ( $1 eq $left_bracket ) {
                my $length = ( pos ${$string} ) - $start;
                return ( substr( ${$string}, $start, $length ), $length );
            }
        } ## end while ( ${$string} =~ /$regex/gcxms )
        return;
    } ## end if ( not defined $right_bracket )

    # bracketed quote
    my $depth = 1;
    MATCH: while ( ${$string} =~ /$regex/gxms ) {
        return if not defined $1;
        if ( $left_bracket  eq $1 ) { $depth++; }
        if ( $right_bracket eq $1 ) { $depth--; }
        if ( $depth <= 0 ) {
            my $length = ( pos ${$string} ) - $start;
            return ( substr( ${$string}, $start, $length ), $length );
        }
    } ## end while ( ${$string} =~ /$regex/gxms )
    return;
} ## end sub lex_q_quote

sub lex_regex {
    my $string       = shift;
    my $lexeme_start = shift;
    my $value_start  = pos ${$string};
    my ($left_side) = ( ${$string} =~ m{\G(qr$punct|/)}xmsogc );
    return if not defined $left_side;
    my $left_bracket = substr $left_side, -1;
    my $prefix = ( $left_side =~ /^qr/xms ) ? q{} : 'qr';

    my $regex_data = $regex_data{$left_bracket};
    if ( not defined $regex_data ) {

        # \x{5c} is backslash
        my $regex = qr{
                \G
                [^\Q$left_bracket\E\x{5c}]*
                (
                     \Q$left_bracket\E
                    |\x{5c}\Q$left_bracket\E
                    |\x{5c}
                )
            }xms;
        $regex_data{$left_bracket} = $regex_data = [ undef, $regex ];
    } ## end if ( not defined $regex_data )
    my ( $right_bracket, $regex ) = @{$regex_data};

    # unbracketed quote
    if ( not defined $right_bracket ) {
        MATCH: while ( ${$string} =~ /$regex/xmsgc ) {
            next MATCH if not defined $1;
            if ( $1 eq $left_bracket ) {

                # also take in trailing options
                ${$string} =~ /\G[msixpo]*/gxms;
                my $pos = pos ${$string};
                my $value =
                    $prefix
                    . ( substr ${$string}, $value_start,
                    $pos - $value_start );
                return ( $value, $pos - $lexeme_start );
            } ## end if ( $1 eq $left_bracket )
        } ## end while ( ${$string} =~ /$regex/xmsgc )
        return;
    } ## end if ( not defined $right_bracket )

    # bracketed quote
    my $depth = 1;
    MATCH: while ( ${$string} =~ /$regex/gxms ) {
        return if not defined $1;
        if ( $left_bracket  eq $1 ) { $depth++; }
        if ( $right_bracket eq $1 ) { $depth--; }
        if ( $depth <= 0 ) {

            # also take in trailing options
            ${$string} =~ /\G[msixpo]*/gxms;
            my $pos   = pos ${$string};
            my $value = $prefix
                . ( substr ${$string}, $value_start, $pos - $value_start );
            return ( $value, $pos - $lexeme_start );

        } ## end if ( $depth <= 0 )
    } ## end while ( ${$string} =~ /$regex/gxms )
    return;
} ## end sub lex_regex

1;    # End of Marpa

__END__

=head1 NAME

Marpa::Lex -- Utility Methods for Lexing

=head1 DESCRIPTION

These routines are used internally by MDL to implement lexing of regexes
and of C<q-> and C<qq->quoted strings.
They are documented here to make them available for general use within
Marpa.

=head1 METHODS

=head2 lex_regex

=begin Marpa::Test::Display:

## next display
is_file($_, 'author.t/misc.t', 'lex_regex snippet');

=end Marpa::Test::Display:

    my ( $regex, $token_length ) =
        Marpa::Lex::lex_regex( \$input_string, $lexeme_start );

Takes two required arguments.
C<$string>
must be a reference to a string that might contain a regex.
The regex will be expected to start at the position pointed to by C<pos $$string>.

C<$lexeme_start> must be the start earleme of the regex for lexing purposes.
In many cases (such as the removal of leading whitespace), it's useful to discard
prefixes.
If a prefix was removed
prior to the call to C<lex_regex>,
C<$lexeme_start>
should be the location where the prefix started.
If no prefix was removed, C<$lexeme_start> will be the same as C<pos ${$string}>.

How C<lex_regex> delimits a regex is described in L<the MDL document|Marpa::Doc::MDL>.
C<lex_regex> returns the null array if no regex was found.
If a regex was found,
C<lex_regex> returns an array of two elements.
The first element is a string containing the regex,
its delimiters,
any postfix modifiers it had,
and its C<qr-> "operator" if there was one.
The second is the regex's length for lexing purposes,
which will include the length of any discarded prefix.

=head2 lex_q_quote

=begin Marpa::Test::Display:

## next display
is_file( $_, 'author.t/misc.t', 'lex_q_quote snippet' );

=end Marpa::Test::Display:

    my ( $string, $token_length ) =
        Marpa::Lex::lex_q_quote( \$input_string, $lexeme_start );

Takes two required arguments, a I<string reference> and a I<start earleme>.
The I<string reference> must be to a string that might contain a C<q-> or C<qq->quoted string.
The C<q-> or C<qq->quoted string will be expected
to start at the position pointed to by C<pos ${$string}>.

C<$lexeme_start> must contain the start earleme of the quoted string for lexing purposes.
In many cases (such as the removal of leading whitespace), it's useful to discard
prefixes.
If a prefix was removed
prior to the call to C<lex_regex>,
C<$lexeme_start>
should be the location where the prefix started.
If no prefix was removed, C<$lexeme_start> should be the same as C<pos $$string>.

How C<lex_q_quote> delimits a C<q-> or C<qq->quoted string is described in L<the MDL document|Marpa::Doc::MDL>.
C<lex_q_quote> returns the null array if no string was found.
If a string was found,
C<lex_q_quote> returns an array of two elements.
The first element is a string containing the C<q-> or C<qq->quoted string,
including the C<q-> or C<qq-> "operator" and the delimiters.
The second is the quoted string's length for lexing purposes,
which will include the length of any discarded prefix.

=head1 SUPPORT

See the L<support section|Marpa/SUPPORT> in the main module.

=head1 AUTHOR

Jeffrey Kegler

=head1 LICENSE AND COPYRIGHT

Copyright 2007 - 2009 Jeffrey Kegler

This program is free software; you can redistribute
it and/or modify it under the same terms as Perl 5.10.0.

=cut

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
