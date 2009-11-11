package Marpa::MDL::Internal::Actions;

use 5.010;
use strict;
use warnings;

use Marpa::MDL::Symbol;

## no critic (Subroutines::RequireArgUnpacking)
## no critic (ValuesAndExpressions::ProhibitMagicNumbers)

sub new {
    my $class = shift;
    my $self  = {
        regex_data  => [],
        rule_hash   => {},
        options     => [],
        lex_options => [],
        strings     => {},
    };
    return bless $self, $class;
} ## end sub new

sub first_arg {
    return $_[1];
}

sub concatenate_lines {
    shift;
    return [@_];
}

sub grammar {
    my ( $self, $text ) = @_;

    TERMINAL:
    for my $terminal ( @{ $self->{lex_options}->[0]->{terminals} } ) {
        if ( $terminal->{regex} =~ /^["]/xms ) {
            ## no critic (BuiltinFunctions::ProhibitStringyEval)
            $terminal->{regex} = eval $terminal->{regex};
            next TERMINAL;
        }
        $terminal->{builtin} = $terminal->{regex};
        delete $terminal->{regex};
    } ## end for my $terminal ( @{ $self->{lex_options}->[0]->{terminals...}})
    $self->{options}->[0]->{terminals} =
        [ map { $_->{name} } @{ $self->{lex_options}->[0]->{terminals} } ];
    $self->{options}->[0]->{parse_order} = 'original';
    return {
        marpa_options => $self->{options},
        mdlex_options => $self->{lex_options}
    };
} ## end sub grammar

# production paragraph:
# non structural production sentences,
# production sentence,
# non structural production sentences,
# optional action sentence,
# non structural production sentences.
sub production_paragraph {
    my $self = shift;
    push @{ $self->{options}->[0]->{rules} },
        {
        map      { @{$_} }
            grep { defined $_ }
            ( @{ $_[0] }, $_[1], @{ $_[2] }, $_[3], @{ $_[4] } )
        };
    return q{};
} ## end sub production_paragraph

# non structural production sentence: /priority/, integer, period.
sub non_structural_production_sentence { return [ priority => $_[2] ] }

# action sentence:
# optional /the/, /action/, /is/, action specifier, period.
sub long_action_sentence {
    return [ action => $_[4] ];
}

# action sentence: action specifier, period.
sub short_action_sentence {
    return [ action => $_[1] ];
}

# definition: predefined setting, period.  q{ $_[0] }.  priority 1000.
sub definition_of_predefined { return $_[1] }

# semantics setting:  optional /the/, /semantics/, copula, /perl5/.
sub semantics_predicate {
    my $self = shift;
    $self->{options}->[0]->{semantics} = $_[3];
    return q{};
}

# semantics setting: /perl5/, copula, optional /the/, /semantics/.
sub semantics_subject {
    my $self = shift;
    $self->{options}->[0]->{semantics} = $_[0];
    return q{};
}

# version setting: optional /the/, /version/, copula, version number.
sub version_predicate {
    my $self = shift;
    $self->{options}->[0]->{version} = $_[3];
    return q{};
}

# version setting: /version number/, copula, optional /the/, /version/.
sub version_subject {
    my $self = shift;
    $self->{options}->[0]->{version} = $_[1];
    return q{};
}

# start symbol setting: optional /the/, /start/, /symbol/, copula,
# symbol phrase.
sub start_symbol_predicate {
    my $self = shift;
    $self->{options}->[0]->{start} = $_[4];
    return q{};
}

# start symbol setting: symbol phrase, copula, optional /the/, /start/,
# /symbol/, .
sub start_symbol_subject {
    my $self = shift;
    $self->{options}->[0]->{start} = $_[1];
    return q{};
}

# default lex prefix setting: regex, copula, optional /the/, /default/,
# /lex/, /prefix/, .
sub default_lex_prefix_subject {
    my $self = shift;

    # The eval is very hack-ish, but I'm throwing this interface away
    ## no critic (BuiltinFunctions::ProhibitStringyEval)

    $self->{lex_options}->[0]->{default_prefix} = eval $_[0];
    return q{};
} ## end sub default_lex_prefix_subject

# default lex prefix setting: optional /the/, /default/, /lex/,
# /prefix/, copula, regex, .
sub default_lex_prefix_predicate {

    my $self = shift;

    # The eval is very hack-ish, but I'm throwing this interface away
    ## no critic (BuiltinFunctions::ProhibitStringyEval)

    $self->{lex_options}->[0]->{default_prefix} = eval $_[5];
    return q{};
} ## end sub default_lex_prefix_predicate

# default null value setting: string specifier, copula, optional /the/, /default/,
# /null/, /value/, .
sub default_null_value_subject {
    my $self = shift;
    $self->{options}->[0]->{default_null_value} = $_[0];
    return q{};
}

# default null value setting: optional /the/, /default/, /null/,
# /value/, copula, string specifier, .
sub default_null_value_predicate {
    my $self = shift;
    $self->{options}->[0]->{default_null_value} = $_[5];
    return q{};
}

# string definition:
# symbol phrase, /is/, string specifier, period.
sub string_definition {
    my $self = shift;
    $self->{strings}->{ $_[0] } = $_[2];
    return q{};
}

# default action setting:
# action specifier, /is/, optional /the/, /default/, /action/.
sub default_action_subject {
    my $self = shift;
    $self->{options}->[0]->{default_action} = $_[0];
    return q{};
}

# default action setting:
# optional /the/, /default/, /action/, /is/, action specifier.
sub default_action_predicate {
    my $self = shift;
    $self->{options}->[0]->{default_action} = $_[4];
    return q{};
}

# literal string: q string.  q{ $_[1] }.
sub q_string {
## no critic (ErrorHandling::RequireCheckingReturnValueOfEval)
## no critic (BuiltinFunctions::ProhibitStringyEval)
    return eval $_[1];
}

sub literal_string {
## no critic (ErrorHandling::RequireCheckingReturnValueOfEval)
## no critic (BuiltinFunctions::ProhibitStringyEval)
    return eval $_[1];
}

# production sentence: lhs, production copula, rhs, period.
sub production_sentence {
    return [ lhs => $_[1], @{ $_[3] } ];
}

# symbol phrase: symbol word sequence.
sub symbol_phrase {
    shift;
    return Marpa::MDL::canonical_symbol_name( join q{-}, @_ );
}

# lhs: symbol phrase.
sub lhs { return $_[1] }

# rhs: .
sub empty_rhs { return [ rhs => [] ] }

# rhs: comma separated rhs element sequence.
sub comma_separated_rhs {
    shift;
    return [ rhs => [ map { @{$_} } @_ ] ];
}

# rhs: symbol phrase, /sequence/.
sub sequence_rhs {
    return [ rhs => [ $_[1] ], min => 1 ];
}

# rhs: /optional/, symbol phrase, /sequence/.
sub optional_sequence_rhs {
    return [ rhs => [ $_[2] ], min => 0 ];
}

# rhs: symbol phrase, /separated/, symbol phrase, /sequence/.
sub separated_sequence_rhs {
    return [
        rhs       => [ $_[3] ],
        separator => $_[1],
        min       => 1
    ];
} ## end sub separated_sequence_rhs

# rhs: /optional/, symbol phrase, /separated/, symbol phrase, /sequence/.
sub optional_separated_sequence_rhs {
    return [
        rhs       => [ $_[4] ],
        separator => $_[2],
        min       => 1
    ];
} ## end sub optional_separated_sequence_rhs

# mandatory rhs element: rhs symbol specifier.
sub mandatory_rhs_element { return $_[1] }

# optional rhs element: /optional/, rhs symbol specifier.
sub optional_rhs_element {
    my ( $self, $dummy, $symbol_phrase ) = @_;
    my $optional_symbol_phrase = $symbol_phrase . ':optional';
    my $rule_hash              = $self->{rule_hash};
    if ( not defined $rule_hash->{$optional_symbol_phrase} ) {
        $rule_hash->{$optional_symbol_phrase} = 1;
        push
            @{ $self->{options}->[0]->{rules} },
            {
            lhs    => $optional_symbol_phrase,
            rhs    => [$symbol_phrase],
            action => __PACKAGE__ . q{::first_arg}
            },
            { lhs => $optional_symbol_phrase, rhs => [], };
    } ## end if ( not defined $rule_hash->{$optional_symbol_phrase...})
    return $optional_symbol_phrase;
} ## end sub optional_rhs_element

# rhs_symbol specifier: symbol phrase.
sub rhs_symbol_phrase_specifier { return $_[1] }

# rhs symbol specifier: regex.
sub rhs_regex_specifier {
    my ( $self, $regex ) = @_;
    my ( $symbol, $new ) =
        Marpa::MDL::Internal::Actions::gen_symbol_from_regex( $regex,
        $self->{regex_data} );
    if ($new) {
        push @{ $self->{lex_options}->[0]->{terminals} },
            { name => $symbol, regex => $regex };
    }
    return $symbol;
} ## end sub rhs_regex_specifier

# terminal sentence:
# symbol phrase, /matches/, regex, period.
sub regex_terminal_sentence {
    my $self = shift;
    push @{ $self->{lex_options}->[0]->{terminals} },
        {
        name  => $_[0],
        regex => $_[2]
        };
    return q{};
} ## end sub regex_terminal_sentence

# terminal sentence:
# /match/, symbol phrase, /using/, string specifier, period.
sub string_terminal_sentence {
    my $self = shift;
    push @{ $self->{lex_options}->[0]->{terminals} },
        {
        name  => $_[1],
        regex => $_[3]
        };
    return q{};
} ## end sub string_terminal_sentence

# string specifier: symbol phrase.
sub string_name_specifier {
    my $self = shift;
    return $self->{strings}->{ $_[0] };
}
## use critic

sub gen_symbol_from_regex {
    my $regex = shift;
    my $data  = shift;
    if ( scalar @{$data} == 0 ) {
        my $number = 0;
        push @{$data}, {}, \$number;
    }
    my ( $regex_hash, $uniq_number ) = @{$data};
    given ($regex) {
        when (/^qr/xms) { $regex = substr $regex, 3, -1; }
        default         { $regex = substr $regex, 1, -1; };
    }
    my $symbol = $regex_hash->{$regex};
    return $symbol if defined $symbol;

    ## no critic (ValuesAndExpressions::ProhibitMagicNumbers)
    $symbol = substr $regex, 0, 20;
    ## use critic
    $symbol =~ s/%/%%/gxms;
    $symbol =~ s/([^[:alnum:]_-])/sprintf("%%%.2x", ord($1))/gexms;
    $symbol .= sprintf ':k%x', ( ${$uniq_number} )++;
    $regex_hash->{$regex} = $symbol;
    return ( $symbol, 1 );
} ## end sub gen_symbol_from_regex

1;

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
