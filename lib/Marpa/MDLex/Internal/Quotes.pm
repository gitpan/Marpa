package Marpa::MDLex::Internal::Quotes;

use 5.010;

use warnings;
use strict;

# use Smart::Comments '-ENV';

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

    my ($left_side) = ( ${$string} =~ m{\G(qr$punct|/)}xmsogc );
    my $value_start = pos ${$string};
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

                my $before_options = pos ${$string};

                # also take in trailing options
                ${$string} =~ /\G[msixpo]*/gxms;
                my $after_options = pos ${$string};

                my $value = q{"}
                    . (
                    quotemeta substr ${$string},
                    $value_start, ( $before_options - $value_start ) - 1
                    ) . q{"};

                return ( $value, $after_options - $lexeme_start );
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

            my $before_options = pos ${$string};

            # also take in trailing options
            ${$string} =~ /\G[msixpo]*/gxms;
            my $after_options = pos ${$string};

            my $value = q{"}
                . (
                quotemeta substr ${$string},
                $value_start, ( $before_options - $value_start ) - 1
                ) . q{"};

            return ( $value, $after_options - $lexeme_start );

        } ## end if ( $depth <= 0 )
    } ## end while ( ${$string} =~ /$regex/gxms )
    return;
} ## end sub lex_regex

sub lex_single_quote {
    my $string       = shift;
    my $lexeme_start = shift;
    my $match_start  = pos ${$string};
    state $prefix_regex = qr/\G'/oxms;
    return if ${$string} !~ /$prefix_regex/gxms;
    state $regex = qr/\G[^'\0134]*('|\0134')/xms;
    MATCH: while ( ${$string} =~ /$regex/gcxms ) {
        next MATCH if not defined $1;
        if ( $1 eq q{'} ) {
            my $end_pos      = pos ${$string};
            my $match_length = $end_pos - $match_start;
            my $lex_length   = $end_pos - $lexeme_start;
            return ( substr( ${$string}, $match_start, $match_length ),
                $lex_length );
        } ## end if ( $1 eq q{'} )
    } ## end while ( ${$string} =~ /$regex/gcxms )
    return;
} ## end sub lex_single_quote

sub lex_double_quote {
    my $string       = shift;
    my $lexeme_start = shift;
    my $match_start  = pos ${$string};
    state $prefix_regex = qr/\G"/oxms;
    return if ${$string} !~ /$prefix_regex/gxms;
    state $regex = qr/\G[^"\0134]*("|\0134")/xms;
    MATCH: while ( ${$string} =~ /$regex/gxmsc ) {
        next MATCH if not defined $1;
        if ( $1 eq q{"} ) {
            my $end_pos      = pos ${$string};
            my $match_length = $end_pos - $match_start;
            my $lex_length   = $end_pos - $lexeme_start;
            return ( substr( ${$string}, $match_start, $match_length ),
                $lex_length );
        } ## end if ( $1 eq q{"} )
    } ## end while ( ${$string} =~ /$regex/gxmsc )
    return;
} ## end sub lex_double_quote

1;    # End of Marpa
