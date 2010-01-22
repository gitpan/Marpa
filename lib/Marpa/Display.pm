package Marpa::Display;

use 5.010;
use strict;
use warnings;
use integer;
use Fatal qw(open close);
use YAML::XS;
use Data::Dumper;    # for debugging
use Carp;

package Marpa::Display::Internal;

use English qw( -no_match_vars );

sub Marpa::Display::new {
    my ($class) = @_;
    return bless {}, $class;
}

@Marpa::Display::Internal::DISPLAY_SPECS = qw(
    start-after-line end-before-line perltidy normalize-whitespace name
    remove-display-indent
    remove-blank-last-line
    partial flatten
);

sub Marpa::Display::read {
    my ( $self, $data_arg, $name ) = @_;
    my @lines;
    GET_LINES: {
        if ( not ref $data_arg ) {
            $name //= $data_arg;
            open my $fh, q{<}, $data_arg;
            @lines = <$fh>;
            close $fh;
            last GET_LINES;
        } ## end if ( not ref $data_arg )
        $name //= q{?};
        @lines = split /\n/xms, ${$data_arg};
    } ## end GET_LINES:
    chomp @lines;
    LINE: for my $zero_based_line ( 0 .. $#lines ) {
        my $line = $lines[$zero_based_line];

        my $display_spec;
        my $display_spec_line_number = $zero_based_line + 1;
        if ( $line =~ /^[#] \s+ Marpa[:][:]Display/xms ) {

            my $yaml = q{};
            while ( ( my $yaml_line = $lines[ ++$zero_based_line ] )
                =~ /^[#]/xms )
            {
                $yaml .= "$yaml_line\n";
            }
            if ( $yaml =~ / \S /xms ) {
                $yaml =~ s/^ [#] \s? //xmsg;
                local $main::EVAL_ERROR = undef;
                my $eval_ok =
                    eval { $display_spec = YAML::XS::Load($yaml); 1 };
                if ( not $eval_ok ) {
                    say STDERR $main::EVAL_ERROR
                        or Carp::croak("Cannot print: $ERRNO");
                    say STDERR
                        "Fatal error in YAML Display spec at $name, line "
                        . ( $display_spec_line_number + 1 )
                        or Carp::croak("Cannot print: $ERRNO");
                } ## end if ( not $eval_ok )
            } ## end if ( $yaml =~ / \S /xms )
        } ## end if ( $line =~ /^[#] \s+ Marpa[:][:]Display/xms )

        if ( $line =~ /^[=]for \s+ Marpa[:][:]Display/xms ) {

            my $yaml = q{};
            while (
                ( my $yaml_line = $lines[ ++$zero_based_line ] ) =~ /\S/xms )
            {
                $yaml .= "$yaml_line\n";
            }
            if ( $yaml =~ / \S /xms ) {
                local $main::EVAL_ERROR = undef;
                my $eval_ok =
                    eval { $display_spec = YAML::XS::Load($yaml); 1 };
                if ( not $eval_ok ) {
                    say STDERR $main::EVAL_ERROR
                        or Carp::croak("Cannot print: $ERRNO");
                    say STDERR
                        "Fatal error in YAML Display spec at $name, line "
                        . ( $display_spec_line_number + 1 )
                        or Carp::croak("Cannot print: $ERRNO");
                } ## end if ( not $eval_ok )
            } ## end if ( $yaml =~ / \S /xms )
        } ## end if ( $line =~ /^[=]for \s+ Marpa[:][:]Display/xms )

        next LINE if not defined $display_spec;

        SPEC: for my $spec ( keys %{$display_spec} ) {
            next SPEC if $spec ~~ \@Marpa::Display::Internal::DISPLAY_SPECS;
            say STDERR
                qq{Warning: Unknown display spec "$spec" in $name, line $display_spec_line_number}
                or Carp::croak("Cannot print: $ERRNO");
        } ## end for my $spec ( keys %{$display_spec} )

        my $content;
        my $content_start_line;
        if ( defined( my $end_pattern = $display_spec->{'end-before-line'} ) )
        {
            my $end_pat = qr/$end_pattern/xms;
            if (defined(
                    my $start_pattern = $display_spec->{'start-after-line'}
                )
                )
            {
                my $start_pat = qr/$start_pattern/xms;
                PRE_CONTENT_LINE: while (1) {
                    my $pre_content_line = $lines[ ++$zero_based_line ];
                    if ( not defined $pre_content_line ) {
                        say STDERR
                            qq{Warning: Pattern "$start_pattern" never found, },
                            qq{started looking at $name, line $display_spec_line_number}
                            or Carp::croak("Cannot print: $ERRNO");
                        return $self;
                    } ## end if ( not defined $pre_content_line )
                    last PRE_CONTENT_LINE
                        if $pre_content_line =~ /$start_pat/xms;
                } ## end while (1)
            } ## end if ( defined( my $start_pattern = $display_spec->{...}))

            CONTENT_LINE: while (1) {
                my $content_line = $lines[ ++$zero_based_line ];
                if ( not defined $content_line ) {
                    say STDERR
                        qq{Warning: Pattern "$end_pattern" never found, },
                        qq{started looking at $name, line $display_spec_line_number}
                        or Carp::croak("Cannot print: $ERRNO");
                } ## end if ( not defined $content_line )
                last CONTENT_LINE if $content_line =~ /$end_pat/xms;
                $content .= "$content_line\n";
                $content_start_line //= $zero_based_line + 1;
            } ## end while (1)
        } ## end if ( defined( my $end_pattern = $display_spec->{...}))

        if ( not defined $content ) {
            CONTENT_LINE: while (1) {
                my $content_line = $lines[ ++$zero_based_line ];
                if ( not defined $content_line ) {
                    say STDERR
                        q{Warning: Pattern "Marpa::Display::End" never found,}
                        . qq{started looking at $name, line $display_spec_line_number}
                        or Carp::croak("Cannot print: $ERRNO");
                    return $self;
                } ## end if ( not defined $content_line )
                last CONTENT_LINE
                    if $content_line
                        =~ /^[=]for \s+ Marpa[:][:]Display[:][:]End\b/xms;
                last CONTENT_LINE
                    if $content_line
                        =~ /^[#] \s* Marpa[:][:]Display[:][:]End\b/xms;
                $content .= "$content_line\n";
                $content_start_line //= $zero_based_line + 1;
            } ## end while (1)
        } ## end if ( not defined $content )

        $content //= '!?! No Content Found !?!';

        my $display_spec_name = $display_spec->{name};
        if ( not $display_spec_name ) {
            say STDERR q{Warning: Unnamed display }
                . qq{at $name, line $display_spec_line_number}
                or Carp::croak("Cannot print: $ERRNO");
            next LINE;
        } ## end if ( not $display_spec_name )

        $display_spec->{filename}           = $name;
        $display_spec->{display_spec_line}  = $display_spec_line_number;
        $display_spec->{content}            = $content;
        $display_spec->{content_start_line} = $content_start_line;
        $display_spec->{line}               = $content_start_line
            // $display_spec_line_number;

        push @{ $self->{displays}->{$display_spec_name} }, $display_spec;

    } ## end for my $zero_based_line ( 0 .. $#lines )

    return $self;

} ## end sub Marpa::Display::read

1;
