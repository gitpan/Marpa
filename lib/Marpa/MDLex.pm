package Marpa::MDLex;

use 5.010;
use warnings;
use strict;

# It's all integers, except for the version number
use integer;

Carp::croak('Marpa not loaded') if not defined $Marpa::VERSION;

use Marpa::Offset qw(

    :package=Marpa::MDLex::Internal::Terminal

    NAME
    PRIORITY
    PREFIX
    LEXER

    =LAST_FIELD

);

use Marpa::Offset qw(

    :package=Marpa::MDLex::Internal::Lexer

    RECOGNIZER { The Marpa recognizer used }

    CURRENT_EARLEME
    CURRENT_LEXABLES

    TERMINAL_HASH { hash of terminals by name }

    DEFAULT_PREFIX { default prefix for lexing }

    AMBIGUOUS { lex ambiguously? }

    TRACE_FILE_HANDLE
    TRACING { Master flag.  Set if any tracing is being done.
    Limits overhead for non-tracing processes. }

    TRACE_MATCHES
    TRACE_TRIES

    INITIALIZED { All the phase tracking we need.
    At least for now }

);

package Marpa::Internal::MDLex;

# use Smart::Comments '-ENV';

### Using smart comments <where>...

use Data::Dumper;
use English qw( -no_match_vars );
use List::Util;
use Carp;
use Marpa::MDLex::Internal::Quotes;

# Static "all-in-one" call, for convenience
sub Marpa::MDLex::mdlex {
    my ( $grammar_args, $lexer_args, $text ) = @_;
    my $grammar_arg_hashes =
        ( ref $grammar_args eq 'ARRAY' ) ? $grammar_args : [$grammar_args];
    my $lexer_arg_hashes =
        ( ref $lexer_args eq 'ARRAY' ) ? $lexer_args : [$lexer_args];

    my $grammar = Marpa::Grammar->new( @{$grammar_arg_hashes} );
    $grammar->precompute();
    my $recce =
        Marpa::Recognizer->new( { grammar => $grammar, mode => 'stream' } );
    my $lexer =
        Marpa::MDLex->new( { recce => $recce, }, @{$lexer_arg_hashes} );
    $lexer->text($text);
    $recce->end_input();    # complete parsing
    my $evaler = Marpa::Evaluator->new(
        { recce => $recce, parse_order => 'original' } );
    return $evaler->value();
} ## end sub Marpa::MDLex::mdlex

sub Marpa::MDLex::new {
    my ( $class, @arg_hashes ) = @_;

    my $lexer = [];
    bless $lexer, $class;

    # set defaults
    $lexer->[Marpa::MDLex::Internal::Lexer::AMBIGUOUS]         = 1;
    $lexer->[Marpa::MDLex::Internal::Lexer::INITIALIZED]       = 0;
    $lexer->[Marpa::MDLex::Internal::Lexer::TERMINAL_HASH]     = {};
    $lexer->[Marpa::MDLex::Internal::Lexer::TRACE_FILE_HANDLE] = *STDERR;
    $lexer->[Marpa::MDLex::Internal::Lexer::TRACE_MATCHES]     = 0;
    $lexer->[Marpa::MDLex::Internal::Lexer::TRACE_TRIES]       = 0;
    $lexer->[Marpa::MDLex::Internal::Lexer::TRACING]           = 0;

    for my $args (@arg_hashes) {
        set( $lexer, $args );
    }
    my $recce = $lexer->[Marpa::MDLex::Internal::Lexer::RECOGNIZER];
    Carp::croak( 'No Recognizer for ' . __PACKAGE__ . ' constructor' )
        if not $recce;
    $recce->set( { mode => 'stream' } );
    $lexer->[Marpa::MDLex::Internal::Lexer::INITIALIZED] = 1;
    return $lexer;

} ## end sub Marpa::MDLex::new

# Keep this internal, at least for now
sub set {
    my ( $lexer, $args ) = @_;
    $args //= {};

    my $initialized = $lexer->[Marpa::MDLex::Internal::Lexer::INITIALIZED];
    my $tracing     = $lexer->[Marpa::MDLex::Internal::Lexer::TRACING];

    # set trace_fh even if no tracing, because we may turn it on in this method
    my $trace_fh = $lexer->[Marpa::MDLex::Internal::Lexer::TRACE_FILE_HANDLE];

    my @options = qw(
        trace_file_handle
        trace
        trace_tries
        trace_matches
        ambiguous
        default_prefix
        recce
        recognizer
        terminals
    );

    if ( my @bad_options = grep { not $_ ~~ @options } keys %{$args} ) {
        Carp::croak( 'Unknown option(s) to Marpa::MDLex::new: ',
            join q{ }, @bad_options );
    }

    if ( defined $args->{recce} ) {
        Carp::croak(
            'recce and recognizer args both passed to Marpa::MDLex::new: use one or the other'
        ) if defined $args->{recognizer};
        $args->{recognizer} = $args->{recce};
    } ## end if ( defined $args->{recce} )

    if ( my $value = $args->{trace_file_handle} ) {
        $lexer->[Marpa::MDLex::Internal::Lexer::TRACE_FILE_HANDLE] = $value;
    }
    if ( my $value = $args->{trace} ) {
        $lexer->[Marpa::MDLex::Internal::Lexer::TRACE_TRIES] =
            $lexer->[Marpa::MDLex::Internal::Lexer::TRACE_MATCHES] = $value;
        if ($value) {
            say {$trace_fh} 'Setting trace option'
                or Marpa::exception("Cannot print: $ERRNO");
            $lexer->[Marpa::MDLex::Internal::Lexer::TRACING] = 1;
        }
    } ## end if ( my $value = $args->{trace} )
    if ( my $value = $args->{trace_tries} ) {
        $lexer->[Marpa::MDLex::Internal::Lexer::TRACE_TRIES] = $value;
        if ($value) {
            say {$trace_fh} 'Setting trace_tries option'
                or Marpa::exception("Cannot print: $ERRNO");
            $lexer->[Marpa::MDLex::Internal::Lexer::TRACING] = 1;
        }
    } ## end if ( my $value = $args->{trace_tries} )
    if ( my $value = $args->{trace_matches} ) {
        $lexer->[Marpa::MDLex::Internal::Lexer::TRACE_MATCHES] = $value;
        if ($value) {
            say {$trace_fh} 'Setting trace_matches option'
                or Marpa::exception("Cannot print: $ERRNO");
            $lexer->[Marpa::MDLex::Internal::Lexer::TRACING] = 1;
        }
    } ## end if ( my $value = $args->{trace_matches} )

    if ( my $value = $args->{default_prefix} ) {
        Marpa::exception(
            'default_prefix option not allowed after lexer is initialized')
            if $initialized;
        $lexer->[Marpa::MDLex::Internal::Lexer::DEFAULT_PREFIX] = $value;
    } ## end if ( my $value = $args->{default_prefix} )
    if ( my $value = $args->{ambiguous} ) {
        Marpa::exception(
            'ambiguous option not allowed after lexer is initialized')
            if $initialized;
        $lexer->[Marpa::MDLex::Internal::Lexer::AMBIGUOUS] = $value;
    } ## end if ( my $value = $args->{ambiguous} )

    if ( my $value = $args->{recognizer} ) {
        Marpa::exception(
            'recognizer option not allowed after lexer is initialized')
            if $initialized;
        my $ref = ref $value;
        Marpa::exception('Not a valid Marpa recognizer')
            if not defined $ref
                or $ref ne 'Marpa::Recognizer';
        $lexer->[Marpa::MDLex::Internal::Lexer::RECOGNIZER] = $value;
    } ## end if ( my $value = $args->{recognizer} )

    if ( my $value = $args->{terminals} ) {
        Marpa::exception(
            'terminals option not allowed after lexer is initialized')
            if $initialized;
        add_user_terminals( $lexer, $value );
    } ## end if ( my $value = $args->{terminals} )

    return 1;
} ## end sub set

sub add_user_terminals {
    my $lexer     = shift;
    my $terminals = shift;

    my $recce = $lexer->[Marpa::MDLex::Internal::Lexer::RECOGNIZER];
    my $terminal_hash =
        $lexer->[Marpa::MDLex::Internal::Lexer::TERMINAL_HASH];

    TERMINAL: for my $options ( @{$terminals} ) {

        my $terminal = [];
        $#{$terminal} = Marpa::MDLex::Internal::Terminal::LAST_FIELD;

        my $regex;
        my $prefix;
        my $builtin;
        my $name;
        my $priority = 0;

        given ( ref $options ) {
            when ('HASH') {
                while ( my ( $key, $value ) = each %{$options} ) {
                    given ($key) {
                        when ('priority') { $priority = $value; }
                        when ('prefix')   { $prefix   = $value; }
                        when ('regex')    { $regex    = $value; }
                        when ('builtin')  { $builtin  = $value; }
                        when ('name')     { $name     = $value; }
                        default {
                            Marpa::exception(
                                "Attempt to add terminal named $name with unknown option $key"
                            );
                        }
                    } ## end given
                } ## end while ( my ( $key, $value ) = each %{$options} )
            } ## end when ('HASH')
            when ('ARRAY') {
                ( $name, $regex, $priority ) = @{$options};
            }
            default {
                Carp::croak(
                    'Terminal description must be ref to array or hash')
            }
        } ## end given

        $priority ||= 0;

        Carp::croak('Terminal must have name') if not defined $name;
        if ( not $recce->check_terminal($name) ) {
            Carp::croak("Terminal '$name' not known to Marpa");
        }
        $terminal->[Marpa::MDLex::Internal::Terminal::NAME] = $name;

        if ( $terminal_hash->{$name} ) {
            Carp::croak("Terminal $name already defined");
        }
        $terminal_hash->{$name} = $terminal;

        $terminal->[Marpa::MDLex::Internal::Terminal::PRIORITY] = $priority;

        if ( defined $regex and defined $builtin ) {
            Carp::croak(
                "Terminal '$name' wants to use both regex and builtin -- can't do that"
            );
        }

        $prefix //= $lexer->[Marpa::MDLex::Internal::Lexer::DEFAULT_PREFIX];
        my $terminal_lexer;

        given ($builtin) {
            when (undef) {;}    # do nothing
            when ('single_quote') {
                $terminal_lexer =
                    \&Marpa::MDLex::Internal::Quotes::lex_single_quote;
            }
            when ('double_quote') {
                $terminal_lexer =
                    \&Marpa::MDLex::Internal::Quotes::lex_double_quote;
            }
            when ('q_quote') {
                $terminal_lexer =
                    \&Marpa::MDLex::Internal::Quotes::lex_q_quote;
            }
            when ('regex') {
                $terminal_lexer = \&Marpa::MDLex::Internal::Quotes::lex_regex;
            }
            default {
                Marpa::exception("Unknown builtin lexer: $builtin");
            }
        };    ## end given

        if ( defined $terminal_lexer ) {
            $terminal->[Marpa::MDLex::Internal::Terminal::LEXER] =
                $terminal_lexer;
            if ( defined $prefix ) {
                $terminal->[Marpa::MDLex::Internal::Terminal::PREFIX] =
                    qr/$prefix/xms;
            }
            next TERMINAL;
        } ## end if ( defined $terminal_lexer )

        if ( q{} =~ $regex ) {
            Carp::croak( 'Attempt to add nullable terminal: ',
                $terminal->[Marpa::MDLex::Internal::Terminal::NAME] );
        }

        $prefix ||= q{};
        $terminal->[Marpa::MDLex::Internal::Terminal::LEXER] = qr{
            \G
            (?<mArPa_prefix>$prefix)
            (?<mArPa_match>$regex)
        }xms;

    } ## end for my $options ( @{$terminals} )

    return 1;

} ## end sub add_user_terminals

# return values for text method
use Marpa::Offset qw(
    :package=Marpa::MDLex::Internal
    PARSING_STILL_ACTIVE=-2
    PARSING_EXHAUSTED=-1
);

sub Marpa::MDLex::text {
    my $lexer        = shift;
    my $input        = shift;
    my $input_length = shift;

    Marpa::exception(
        __PACKAGE__ . '::text() third argument not yet implemented' )
        if defined $input_length;

    my $recce = $lexer->[Marpa::MDLex::Internal::Lexer::RECOGNIZER];
    my ( $current_earleme, $lexables ) = $recce->status();
    return 0 if not defined $current_earleme;

    my $input_ref;
    given ( ref $input ) {
        when (q{})      { $input_ref = \$input; }
        when ('SCALAR') { $input_ref = $input; }
        default {
            Marpa::exception( 'text argument to Marpa::Recognizer::text() ',
                'must be string or string ref' );
        }
    }    # given ref $input

    my $tracing = $lexer->[Marpa::MDLex::Internal::Lexer::TRACING];
    my $trace_fh;
    my $trace_tries;
    my $trace_matches;
    if ($tracing) {
        $trace_fh =
            $lexer->[Marpa::MDLex::Internal::Lexer::TRACE_FILE_HANDLE];
        $trace_tries = $lexer->[Marpa::MDLex::Internal::Lexer::TRACE_TRIES];
        $trace_matches =
            $lexer->[Marpa::MDLex::Internal::Lexer::TRACE_MATCHES];
    } ## end if ($tracing)

    my $ambiguous = $lexer->[Marpa::MDLex::Internal::Lexer::AMBIGUOUS];
    my $terminal_hash =
        $lexer->[Marpa::MDLex::Internal::Lexer::TERMINAL_HASH];

    $input_length //= length ${$input_ref};
    my $pos = 0;

    ### input length: $input_length

    pos ${$input_ref} = 0;
    POS: while ( $pos < $input_length ) {
        my @alternatives;

        ### pos: $pos

        # NOTE: Often the number of the earley set, and the idea of
        # lexical position will correspond.  Be careful that Marpa
        # imposes no such requirement, however.

        if ( $trace_tries and scalar @{$lexables} ) {
            my $string_to_match = substr ${$input_ref}, $pos, 20;
            $string_to_match
                =~ s/([\x00-\x1F\x7F-\xFF])/sprintf('{%#.2x}', ord($1))/gexms;
            say $trace_fh "Match target at $pos: ", $string_to_match
                or Marpa::exception("Cannot print: $ERRNO");
        } ## end if ( $trace_tries and scalar @{$lexables} )

        LEXABLE: for my $lexable ( @{$lexables} ) {
            my $terminal = $terminal_hash->{$lexable};

            # It is not a error for a lexer to not define one of the parser's
            # terminals
            next LEXABLE if not defined $terminal;

            if ($trace_tries) {
                print {$trace_fh} 'Trying to match ',
                    $terminal->[Marpa::MDLex::Internal::Terminal::NAME],
                    " at $pos\n"
                    or Marpa::exception('Could not print to trace file');
            } ## end if ($trace_tries)

            my $terminal_lexer =
                $terminal->[Marpa::MDLex::Internal::Terminal::LEXER];
            my $lexer_type = ref $terminal_lexer;
            Marpa::exception('Illegal type for lexer: undefined')
                if not defined $lexer_type;

            pos ${$input_ref} = $pos;

            if ( $lexer_type eq 'Regexp' ) {

                if ( ${$input_ref} =~ /$terminal_lexer/xmsg ) {

                    ## no critic (Variables::ProhibitPunctuationVars)
                    my $match = $+{mArPa_match};
                    ## use critic

                    my $length = ( pos ${$input_ref} ) - $pos;
                    Carp::croak(
                        'Internal error, zero length token -- this is a Marpa bug'
                    ) if not $length;
                    push @alternatives, [ $lexable, $match, $length, 0 ];
                    if ($trace_matches) {
                        print {$trace_fh}
                            'Matched regex for ',
                            $terminal
                            ->[Marpa::MDLex::Internal::Terminal::NAME],
                            " at $pos: ", $match, "\n"
                            or
                            Marpa::exception('Could not print to trace file');
                    } ## end if ($trace_matches)
                    last LEXABLE if not $ambiguous;
                }    # if match

                next LEXABLE;

            }    # if defined regex

            # If it's a lexable and a regex was not defined, there must be a
            # closure
            Marpa::exception( q{Terminal '}
                    . $lexable->[Marpa::MDLex::Internal::Terminal::NAME]
                    . "': Illegal type for lexer, type is '$lexer_type'" )
                if not $lexer_type eq 'CODE';

            my $prefix =
                $terminal->[Marpa::MDLex::Internal::Terminal::PREFIX];
            if ( defined $prefix ) {
                ${$input_ref} =~ /\G$prefix/xmsg;
            }

            my ( $match, $length ) = $terminal_lexer->( $input_ref, $pos );

            next LEXABLE if not defined $match;

            $length //= length $match;

            push @alternatives, [ $lexable, $match, $length, 0 ];
            if ($trace_matches) {
                print {$trace_fh}
                    'Matched Closure for ',
                    $terminal->[Marpa::MDLex::Internal::Terminal::NAME],
                    " at $pos: ", $match, "\n"
                    or Marpa::exception('Could not print to trace file');
            } ## end if ($trace_matches)

            last LEXABLE if not $ambiguous;

        }    # LEXABLE

        $pos++;

        ( $current_earleme, $lexables ) =
            Marpa::Recognizer::tokens( $recce, [@alternatives] );
        return Marpa::MDLex::Internal::PARSING_EXHAUSTED
            if not defined $current_earleme;

    }    # POS

    return $pos < $input_length
        ? $pos
        : Marpa::MDLex::Internal::PARSING_STILL_ACTIVE;

}    # sub text

1;
