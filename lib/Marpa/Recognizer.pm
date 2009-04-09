package Marpa::Internal;
use 5.010;

use warnings;
no warnings 'recursion';
use strict;
use integer;
use English qw( -no_match_vars );

package Marpa::Internal;

# Elements of the EARLEY ITEM structure
# Note that these are Earley items as modified by Aycock & Horspool, with QDFA states instead of
# LR(0) items.

use Marpa::Offset Earley_item =>

    # evaluator data
    qw(NAME STATE TOKENS LINKS),

    # temporary data
    qw(PARENT SET);

# We don't prune the Earley items because we want PARENT and SET
# around for debugging

# NAME   - unique string describing Earley item
# STATE  - the QDFA state
# PARENT - the number of the Earley set with the parent item(s)
# TOKENS - a list of the links from token scanning
# LINKS  - a list of the links from the completer step
# SET    - the set this item is in, for debugging

# Elements of the RECOGNIZER structure
use Marpa::Offset Recognizer => (

    # evaluator data
    qw( GRAMMAR EARLEY_SETS CURRENT_SET ),

    # temporary data
    qw(
        EARLEY_HASH FURTHEST_EARLEME EXHAUSTED
        PACKAGE LEXERS LEXABLES_BY_STATE LAST_COMPLETED_SET
        )
);

package Marpa::Internal::Recognizer;
use constant LAST_EVALUATOR_FIELD => Marpa::Internal::Recognizer::CURRENT_SET;

package Marpa::Internal;

# GRAMMAR            - the grammar used
# CURRENT_SET        - For an active parse, the set scanned items will be added
#                      to.  For an exhausted parse, the set at which the parse
#                      was exhausted.
# EARLEY_SETS        - the array of the Earley sets
# EARLEY_HASH        - hash of the Earley items
#                      to build the Earley sets
# FURTHEST_EARLEME   - last earley set with a token
# EXHAUSTED          - parse can't continue?
# EVALUATOR          - the current evaluator for this recognizer
# PACKAGE            - special "safe" namespace
# LEXERS             - an array, indexed by symbol id,
#                      of the lexer for each symbol
# LEXABLES_BY_STATE  - an array, indexed by QDFA state id,
#                      of the lexables belonging in it
# LAST_COMPLETED_SET - last earley set completed

package Marpa::Internal::Recognizer;

use Scalar::Util qw(weaken);
use Data::Dumper;
use English qw( -no_match_vars );

use Carp;
our @CARP_NOT = qw(
    Marpa
    Marpa::Evaluator
    Marpa::Grammar
    Marpa::Internal
    Marpa::Internal::And_Node
    Marpa::Internal::Earley_item
    Marpa::Internal::Evaluator
    Marpa::Internal::Evaluator::Rule
    Marpa::Internal::Grammar
    Marpa::Internal::Interface
    Marpa::Internal::LR0_item
    Marpa::Internal::Lex
    Marpa::Internal::NFA
    Marpa::Internal::Or_Node
    Marpa::Internal::Or_Sapling
    Marpa::Internal::Phase
    Marpa::Internal::QDFA
    Marpa::Internal::Recognizer
    Marpa::Internal::Rule
    Marpa::Internal::Source_Eval
    Marpa::Internal::Source_Raw
    Marpa::Internal::Symbol
    Marpa::Internal::Tree_Node
    Marpa::Lex
    Marpa::MDL
    Marpa::Recognizer
);

my $parse_number = 0;

sub set_lexers {

    my $grammar = shift;
    my $package = shift;

    my ( $symbols, $symbol_hash, $QDFA, $tracing, $default_prefix,
        $default_suffix )
        = @{$grammar}[
        Marpa::Internal::Grammar::SYMBOLS,
        Marpa::Internal::Grammar::SYMBOL_HASH,
        Marpa::Internal::Grammar::QDFA,
        Marpa::Internal::Grammar::TRACING,
        Marpa::Internal::Grammar::DEFAULT_LEX_PREFIX,
        Marpa::Internal::Grammar::DEFAULT_LEX_SUFFIX,
        ];

    my $trace_fh;
    my $trace_actions;
    if ($tracing) {
        $trace_fh = $grammar->[Marpa::Internal::Grammar::TRACE_FILE_HANDLE];
        $trace_actions = $grammar->[Marpa::Internal::Grammar::TRACE_ACTIONS];
    }

    my @lexers;
    $#lexers = $#{$symbols};

    SYMBOL: for my $ix ( 0 .. $#lexers ) {

        my $symbol = $symbols->[$ix];
        my ( $name, $regex, $action, $symbol_prefix, $symbol_suffix ) =
            @{$symbol}[
            Marpa::Internal::Symbol::NAME,
            Marpa::Internal::Symbol::REGEX,
            Marpa::Internal::Symbol::ACTION,
            Marpa::Internal::Symbol::PREFIX,
            Marpa::Internal::Symbol::SUFFIX,
            ];

        if ( defined $regex ) {
            $lexers[$ix] = $regex;
            next SYMBOL;
        }

        my $prefix = $symbol_prefix // $default_prefix;
        $prefix = qr/$prefix/xms if defined $prefix;
        my $suffix = $symbol_suffix // $default_suffix;
        $suffix = qr/$suffix/xms if defined $suffix;

        given ($action) {
            when (undef) {;}    # do nothing
                                # Right now do nothing but find lex_q_quote
            when ('lex_q_quote') {
                $lexers[$ix] =
                    [ \&Marpa::Lex::lex_q_quote, $prefix, $suffix ];
            }
            when ('lex_regex') {
                $lexers[$ix] = [ \&Marpa::Lex::lex_regex, $prefix, $suffix ];
            }
            default {
                my $code =
                      "sub {\n"
                    . '    my $STRING = shift;' . "\n"
                    . '    my $START = shift;' . "\n"
                    . '    package '
                    . $package . ";\n" . q{    }
                    . $action . ";\n"
                    . "    return\n" . "}\n";

                if ($trace_actions) {
                    print {$trace_fh} 'Setting action for terminal ', $name,
                        " to\n", $code, "\n"
                        or croak('Could not print to trace file');
                }

                my $closure;
                {
                    my $old_warn_handler = $SIG{__WARN__};
                    my @warnings;
                    $SIG{__WARN__} =
                        sub { push @warnings, [ $_[0], ( caller 0 ) ]; };

                    ## no critic (BuiltinFunctions::ProhibitStringyEval)
                    $closure = eval $code;
                    ## use critic

                    $SIG{__WARN__} = $old_warn_handler;

                    if ( not $closure or @warnings ) {
                        my $fatal_error = $EVAL_ERROR;
                        Marpa::Internal::code_problems(
                            {   eval_ok     => $closure,
                                fatal_error => $fatal_error,
                                grammar     => $grammar,
                                warnings    => \@warnings,
                                where       => 'compiling lexer',
                                long_where  => "compiling lexer for $name",
                                code        => \$code,
                            }
                        );
                    } ## end if ( not $closure or @warnings )
                }

                $symbol->[Marpa::Internal::Symbol::ACTION] = $code;
                $lexers[$ix] = [ $closure, $prefix, $suffix ];

            } ## end default
        } ## end given

    }    # SYMBOL

    my @lexables_by_state;
    $#lexables_by_state = $#{$QDFA};

    for my $state ( @{$QDFA} ) {
        my ( $id, $transition ) =
            @{$state}[ Marpa::Internal::QDFA::ID,
            Marpa::Internal::QDFA::TRANSITION, ];
        $lexables_by_state[$id] = [
            grep    { $lexers[$_] }
                map { $symbol_hash->{$_}->[Marpa::Internal::Symbol::ID] }
                keys %{$transition}
        ];
    } ## end for my $state ( @{$QDFA} )

    return ( \@lexers, \@lexables_by_state, );

}    # sub set_lexers

sub compile_regexes {
    my $grammar = shift;
    my ( $symbols, $default_lex_prefix, $default_lex_suffix, ) = @{$grammar}[
        Marpa::Internal::Grammar::SYMBOLS,
        Marpa::Internal::Grammar::DEFAULT_LEX_PREFIX,
        Marpa::Internal::Grammar::DEFAULT_LEX_SUFFIX,
    ];

    SYMBOL: for my $symbol ( @{$symbols} ) {
        my $regex = $symbol->[Marpa::Internal::Symbol::REGEX];
        next SYMBOL unless defined $regex;
        if ( q{} =~ $regex ) {
            my $name = $symbol->[Marpa::Internal::Symbol::NAME];
            croak( 'Attempt to add nullable terminal: ', $name );
        }
        my $prefix = $symbol->[Marpa::Internal::Symbol::PREFIX]
            // $default_lex_prefix;
        my $suffix = $symbol->[Marpa::Internal::Symbol::SUFFIX]
            // $default_lex_suffix;
        my $compiled_regex = qr{
            \G
            (?<mArPa_prefix>$prefix)
            (?<mArPa_match>$regex)
            (?<mArPa_suffix>$suffix)
        }xms;
        $symbol->[Marpa::Internal::Symbol::REGEX] = $compiled_regex;
    }    # SYMBOL

    return;

} ## end sub compile_regexes

sub prepare_grammar_for_recognizer {
    my $parse   = shift;
    my $grammar = shift;

    local ($Data::Dumper::Terse) = 1;
    my $package = $parse->[Marpa::Internal::Recognizer::PACKAGE] =
        sprintf 'Marpa::P_%x', $parse_number++;

    my $lex_preamble   = $grammar->[Marpa::Internal::Grammar::LEX_PREAMBLE];
    my $default_action = $grammar->[Marpa::Internal::Grammar::DEFAULT_ACTION];
    my $default_null_value =
        $grammar->[Marpa::Internal::Grammar::DEFAULT_NULL_VALUE];

    if ( defined $lex_preamble ) {
        my $old_warn_handler = $SIG{__WARN__};
        my @warnings;
        $SIG{__WARN__} = sub { push @warnings, [ $_[0], ( caller 0 ) ]; };

        my $code = 'package ' . $package . ";\n" . $lex_preamble;
        ## no critic (BuiltinFunctions::ProhibitStringyEval)
        my $eval_ok = eval $code;
        ## use critic

        $SIG{__WARN__} = $old_warn_handler;

        if ( not $eval_ok or @warnings ) {
            my $fatal_error = $EVAL_ERROR;
            Marpa::Internal::code_problems(
                {   eval_ok     => $eval_ok,
                    fatal_error => $fatal_error,
                    grammar     => $grammar,
                    warnings    => \@warnings,
                    where       => 'evaluating lex preamble',
                    code        => \$code,
                }
            );
        } ## end if ( not $eval_ok or @warnings )
    } ## end if ( defined $lex_preamble )

    compile_regexes($grammar);
    @{$parse}[ LEXERS, LEXABLES_BY_STATE ] = set_lexers( $grammar, $package );

    return;

} ## end sub prepare_grammar_for_recognizer

# Returns the new parse object or throws an exception
sub Marpa::Recognizer::new {
    my $class = shift;
    my $args  = shift;

    my $arg_trace_fh = $args->{trace_file_handle};

    my $parse = [];
    my $ambiguous_lex;
    my $lex_preamble;

    my $clone_arg = $args->{clone};
    delete $args->{clone};
    my $clone = $clone_arg // 1;

    my $grammar = $args->{grammar};
    if ( not defined $grammar ) {
        my $stringified_grammar = $args->{stringified_grammar};
        croak('No grammar specified') unless defined $stringified_grammar;
        delete $args->{stringified_grammar};
        my $trace_fh = $arg_trace_fh // (*STDERR);
        $grammar =
            Marpa::Grammar::unstringify( $stringified_grammar, $trace_fh );
        $clone = 0;
    } ## end if ( not defined $grammar )
    else {
        delete $args->{grammar};
    }

    my $grammar_class = ref $grammar;
    croak("${class}::new() grammar arg has wrong class: $grammar_class")
        unless $grammar_class eq 'Marpa::Grammar';

    my $tracing = $grammar->[Marpa::Internal::Grammar::TRACING];

    my $problems = $grammar->[Marpa::Internal::Grammar::PROBLEMS];
    if ($problems) {
        croak(
            Marpa::show_problems($grammar),
            "Attempt to parse grammar with fatal problems\n",
            'Marpa cannot proceed',
        );
    } ## end if ($problems)

    if ( $grammar->[Marpa::Internal::Grammar::ACADEMIC] ) {
        croak( "Attempt to parse grammar marked academic\n",
            'Marpa cannot proceed' );
    }

    my $phase = $grammar->[Marpa::Internal::Grammar::PHASE];
    if ( $phase != Marpa::Internal::Phase::PRECOMPUTED ) {
        croak(
            'Attempt to parse grammar in inappropriate phase ',
            Marpa::Internal::Phase::description($phase)
        );
    } ## end if ( $phase != Marpa::Internal::Phase::PRECOMPUTED )

    if ($clone) {
        $grammar = $grammar->clone($arg_trace_fh);
        delete $args->{trace_file_handle};
    }

    # options are not set until *AFTER* the grammar is cloned
    Marpa::Grammar::set( $grammar, $args );

    prepare_grammar_for_recognizer( $parse, $grammar );

    $grammar->[Marpa::Internal::Grammar::PHASE] =
        Marpa::Internal::Phase::RECOGNIZING;

    my $earley_hash;
    my $earley_set;

    my $start_states = $grammar->[Marpa::Internal::Grammar::START_STATES];

    for my $state ( @{$start_states} ) {
        my $state_id = $state->[Marpa::Internal::QDFA::ID];
        my $name = sprintf 'S%d@%d-%d', $state_id, 0, 0;
        my $item;
        @{$item}[
            Marpa::Internal::Earley_item::NAME,
            Marpa::Internal::Earley_item::STATE,
            Marpa::Internal::Earley_item::PARENT,
            Marpa::Internal::Earley_item::TOKENS,
            Marpa::Internal::Earley_item::LINKS,
            Marpa::Internal::Earley_item::SET
            ]
            = ( $name, $state, 0, [], [], 0 );
        push @{$earley_set}, $item;
        $earley_hash->{$name} = $item;
    } ## end for my $state ( @{$start_states} )

    @{$parse}[
        CURRENT_SET, FURTHEST_EARLEME, EARLEY_HASH,
        GRAMMAR,     EARLEY_SETS,      LAST_COMPLETED_SET,
        ]
        = ( 0, 0, $earley_hash, $grammar, [$earley_set], -1, );

    bless $parse, $class;
} ## end sub Marpa::Recognizer::new

# Convert Recognizer into string form
#
sub Marpa::Recognizer::stringify {
    my $recce   = shift;
    my $grammar = $recce->[Marpa::Internal::Recognizer::GRAMMAR];

    my $tracing = $grammar->[Marpa::Internal::Grammar::TRACING];
    my $trace_fh;
    if ($tracing) {
        $trace_fh = $grammar->[Marpa::Internal::Grammar::TRACE_FILE_HANDLE];
    }

    my $phase = $grammar->[Marpa::Internal::Grammar::PHASE];
    if ( $phase != Marpa::Internal::Phase::RECOGNIZED ) {
        croak(
            "Attempt to stringify recognizer in inappropriate state\nAttempt to stringify ",
            Marpa::Internal::Phase::description($phase)
        );
    } ## end if ( $phase != Marpa::Internal::Phase::RECOGNIZED )

    my $d = Data::Dumper->new( [$recce], ['recce'] );
    $d->Purity(1);
    $d->Indent(0);

    # returns a ref -- dumps can be long
    return \( $d->Dump() );
} ## end sub Marpa::Recognizer::stringify

# First arg is stringified recognizer
# Second arg (optional) is trace file handle, either saved and restored
# If not trace file handle supplied, it reverts to the default, STDERR
#
# Returns the unstringified recognizer
sub Marpa::Recognizer::unstringify {
    my $stringified_recce = shift;
    my $trace_fh          = shift;
    $trace_fh //= *STDERR;

    croak('Attempt to unstringify undefined recognizer')
        unless defined $stringified_recce;
    croak('Arg to unstringify must be ref to SCALAR')
        if ref $stringified_recce ne 'SCALAR';

    my $recce;
    {
        my $old_warn_handler = $SIG{__WARN__};
        my @warnings;
        $SIG{__WARN__} = sub { push @warnings, [ $_[0], ( caller 0 ) ]; };

        ## no critic (BuiltinFunctions::ProhibitStringyEval,TestingAndDebugging::ProhibitNoStrict)
        no strict 'refs';
        my $eval_ok = eval ${$stringified_recce};
        use strict 'refs';
        ## use critic

        $SIG{__WARN__} = $old_warn_handler;

        if ( not $eval_ok or @warnings ) {
            my $fatal_error = $EVAL_ERROR;
            Marpa::Internal::code_problems(
                {   eval_ok     => $eval_ok,
                    fatal_error => $fatal_error,
                    warnings    => \@warnings,
                    where       => 'unstringifying recognizer',
                    code        => $stringified_recce,
                }
            );
        } ## end if ( not $eval_ok or @warnings )
    }

    my $grammar = $recce->[Marpa::Internal::Recognizer::GRAMMAR];
    $grammar->[Marpa::Internal::Grammar::TRACE_FILE_HANDLE] = $trace_fh;

    return $recce;

} ## end sub Marpa::Recognizer::unstringify

sub Marpa::Recognizer::clone {
    my $recce = shift;

    my $grammar  = $recce->[Marpa::Internal::Recognizer::GRAMMAR];
    my $trace_fh = $grammar->[Marpa::Internal::Grammar::TRACE_FILE_HANDLE];

    if ( $#{$recce} > Marpa::Internal::Recognizer::LAST_EVALUATOR_FIELD ) {
        croak( "Cloning of unstripped recognizers not yet implemented\n",
            "Strip the recognizer or turn off cloning\n" );
    }
    my $stringified_recce = Marpa::Recognizer::stringify($recce);

    # say $$stringified_recce;
    # exit 0;
    return Marpa::Recognizer::unstringify( $stringified_recce, $trace_fh );

} ## end sub Marpa::Recognizer::clone

# Viewing methods, for debugging
sub Marpa::brief_earley_item {
    my $item = shift;
    my $ii   = shift;
    return $item->[Marpa::Internal::Earley_item::NAME] unless $ii;
    my ( $state, $parent, $set ) = @{$item}[
        Marpa::Internal::Earley_item::STATE,
        Marpa::Internal::Earley_item::PARENT,
        Marpa::Internal::Earley_item::SET
    ];
    my ( $id, $tag ) =
        @{$state}[ Marpa::Internal::QDFA::ID, Marpa::Internal::QDFA::TAG ];
    my $text = defined $tag ? ( 'St' . $tag ) : ( 'S' . $id );
    $text .= q{@} . $parent . q{-} . $set;
    return $text;
} ## end sub Marpa::brief_earley_item

sub Marpa::show_token_choice {
    my ( $token, $ii ) = @_;
    return
          '[p='
        . Marpa::brief_earley_item( $token->[0], $ii ) . '; t='
        . $token->[1] . ']';
} ## end sub Marpa::show_token_choice

sub Marpa::show_link_choice {
    my ( $link, $ii ) = @_;
    return
          '[p='
        . Marpa::brief_earley_item( $link->[0], $ii ) . '; c='
        . Marpa::brief_earley_item( $link->[1], $ii ) . ']';
} ## end sub Marpa::show_link_choice

sub Marpa::show_earley_item {
    my ( $item,   $ii )    = @_;
    my ( $tokens, $links ) = @{$item}[
        Marpa::Internal::Earley_item::TOKENS,
        Marpa::Internal::Earley_item::LINKS,
    ];

    my $text = Marpa::brief_earley_item( $item, $ii );

    if ( defined $tokens and @{$tokens} ) {
        for my $token ( @{$tokens} ) {
            $text .= q{ } . Marpa::show_token_choice( $token, $ii );
        }
    }
    if ( defined $links and @{$links} ) {
        for my $link ( @{$links} ) {
            $text .= q{ } . Marpa::show_link_choice( $link, $ii );
        }
    }
    return $text;
} ## end sub Marpa::show_earley_item

sub Marpa::show_earley_set {
    my ( $earley_set, $ii ) = @_;
    my $text = q{};
    for my $earley_item ( @{$earley_set} ) {
        $text .= Marpa::show_earley_item( $earley_item, $ii ) . "\n";
    }
    return $text;
} ## end sub Marpa::show_earley_set

sub Marpa::show_earley_set_list {
    my ( $earley_set_list, $ii ) = @_;
    my $text             = q{};
    my $earley_set_count = @{$earley_set_list};
    LIST: for my $ix ( 0 .. $earley_set_count - 1 ) {
        my $set = $earley_set_list->[$ix];
        next LIST unless defined $set;
        $text .= "Earley Set $ix\n" . Marpa::show_earley_set( $set, $ii );
    }
    return $text;
} ## end sub Marpa::show_earley_set_list

sub Marpa::Recognizer::show_earley_sets {
    my ( $recce, $ii ) = @_;
    my $current_set      = $recce->[CURRENT_SET];
    my $furthest_earleme = $recce->[FURTHEST_EARLEME];
    my $earley_set_list  = $recce->[EARLEY_SETS];

    my $text =
        defined $furthest_earleme
        ? "Current Earley Set: $current_set; Furthest: $furthest_earleme\n"
        : "At End of Input\n";

    $text .= Marpa::show_earley_set_list( $earley_set_list, $ii );
    return $text;
} ## end sub Marpa::Recognizer::show_earley_sets

# check class of parse?

## no critic (Subroutines::RequireArgUnpacking)
sub Marpa::Recognizer::earleme {
## use critic

    my $parse = shift;

    my $grammar = $parse->[Marpa::Internal::Recognizer::GRAMMAR];
    my $phase   = $grammar->[Marpa::Internal::Grammar::PHASE];
    if ( $phase >= Marpa::Internal::Phase::RECOGNIZED ) {
        croak('New earlemes not allowed after end of input');
    }

    # lexables not checked -- don't use prediction here
    # maybe add this as an option?
    my $lexables = Marpa::Internal::Recognizer::complete_set($parse);
    return Marpa::Internal::Recognizer::scan_set( $parse, @_ );
} ## end sub Marpa::Recognizer::earleme

sub Marpa::Recognizer::text {
    my $parse        = shift;
    my $input        = shift;
    my $input_length = shift;

    return 0 if $parse->[Marpa::Internal::Recognizer::EXHAUSTED];

    croak('Marpa::Recognizer::text() third argument not yet implemented')
        if defined $input_length;

    my $input_ref;
    given ( ref $input ) {
        when (q{})      { $input_ref = \$input; }
        when ('SCALAR') { $input_ref = $input; }
        default {
            croak( 'text argument to Marpa::Recognizer::text() ',
                'must be string or string ref' );
        }
    }    # given ref $input

    my ( $grammar, $earley_sets, $current_set, $lexers ) = @{$parse}[
        Marpa::Internal::Recognizer::GRAMMAR,
        Marpa::Internal::Recognizer::EARLEY_SETS,
        Marpa::Internal::Recognizer::CURRENT_SET,
        Marpa::Internal::Recognizer::LEXERS,
    ];

    my $phase = $grammar->[Marpa::Internal::Grammar::PHASE];
    if ( $phase >= Marpa::Internal::Phase::RECOGNIZED ) {
        croak('More text not allowed after end of input');
    }

    my $tracing = $grammar->[Marpa::Internal::Grammar::TRACING];
    my $trace_fh;
    my $trace_lex_tries;
    my $trace_lex_matches;
    if ($tracing) {
        $trace_fh = $grammar->[Marpa::Internal::Grammar::TRACE_FILE_HANDLE];
        $trace_lex_tries =
            $grammar->[Marpa::Internal::Grammar::TRACE_LEX_TRIES];
        $trace_lex_matches =
            $grammar->[Marpa::Internal::Grammar::TRACE_LEX_MATCHES];
    } ## end if ($tracing)

    my ( $symbols, $ambiguous_lex ) = @{$grammar}[
        Marpa::Internal::Grammar::SYMBOLS,
        Marpa::Internal::Grammar::AMBIGUOUS_LEX,
    ];

    $input_length = length ${$input_ref} unless defined $input_length;

    my $active = 1;
    my $pos    = 0;

    pos ${$input_ref} = 0;
    POS: while ( $pos < $input_length ) {
        my @alternatives;

        # NOTE: Often the number of the earley set, and the idea of
        # lexical position will correspond.  Be careful that Marpa
        # imposes no such requirement, however.

        my $lexables = complete_set($parse);

        if ( $trace_lex_tries and scalar @{$lexables} ) {
            my $string_to_match = substr ${$input_ref}, $pos, 20;
            $string_to_match
                =~ s/([\x00-\x1F\x7F-\xFF])/sprintf('{%#.2x}', ord($1))/gexm;
            say $trace_fh "Match target at $pos: ", $string_to_match;
        } ## end if ( $trace_lex_tries and scalar @{$lexables} )

        LEXABLE: for my $lexable ( @{$lexables} ) {
            my ($symbol_id) = @{$lexable}[Marpa::Internal::Symbol::ID];
            if ($trace_lex_tries) {
                print {$trace_fh} 'Trying to match ',
                    $lexable->[Marpa::Internal::Symbol::NAME], " at $pos\n"
                    or croak('Could not print to trace file');
            }

            my $lexer      = $lexers->[$symbol_id];
            my $lexer_type = ref $lexer;
            croak('Illegal type for lexer: undefined')
                unless defined $lexer_type;

            pos ${$input_ref} = $pos;

            if ( $lexer_type eq 'Regexp' ) {

                ## no critic (RegularExpressions::RequireLineBoundaryMatching)
                ## no critic (RegularExpressions::RequireExtendedFormatting)
                if ( ${$input_ref} =~ /$lexer/g ) {
                    ## use critic

                    ## no critic (Variables::ProhibitPunctuationVars)
                    my $match = $+{mArPa_match};
                    ## use critic

                    # my $prefix = $+{mArPa_prefix};
                    # my $suffix = $+{mArPa_suffix};
                    # my $length = length(${^MATCH});

                    my $length = ( pos ${$input_ref} ) - $pos;
                    croak(
                        'Internal error, zero length token -- this is a Marpa bug'
                    ) unless $length;
                    push @alternatives, [ $lexable, $match, $length ];
                    if ($trace_lex_matches) {
                        print {$trace_fh}
                            'Matched regex for ',
                            $lexable->[Marpa::Internal::Symbol::NAME],
                            " at $pos: ", $match, "\n"
                            or croak('Could not print to trace file');
                    } ## end if ($trace_lex_matches)
                    last LEXABLE unless $ambiguous_lex;
                }    # if match

                next LEXABLE;

            }    # if defined regex

            # If it's a lexable and a regex was not defined, there must be a
            # closure
            croak("Illegal type for lexer: $lexer_type")
                unless $lexer_type eq 'ARRAY';

            my ( $lex_closure, $prefix, $suffix ) = @{$lexer};
            if ( defined $prefix ) {

                ## no critic (RegularExpressions::RequireLineBoundaryMatching)
                ## no critic (RegularExpressions::RequireExtendedFormatting)
                ${$input_ref} =~ /\G$prefix/g;
                ## use critic

            } ## end if ( defined $prefix )

            my ( $match, $length );
            {
                my $old_warn_handler = $SIG{__WARN__};
                my @warnings;
                $SIG{__WARN__} =
                    sub { push @warnings, [ $_[0], ( caller 0 ) ]; };

                my $eval_ok = eval {
                    ( $match, $length ) = $lex_closure->( $input_ref, $pos );
                    1;
                };

                $SIG{__WARN__} = $old_warn_handler;

                if ( not $eval_ok or @warnings ) {
                    my $fatal_error = $EVAL_ERROR;
                    Marpa::Internal::code_problems(
                        {   eval_ok     => $eval_ok,
                            fatal_error => $fatal_error,
                            grammar     => $grammar,
                            warnings    => \@warnings,
                            where       => 'user supplied lexer',
                            long_where  => 'user supplied lexer for '
                                . $lexable->[Marpa::Internal::Symbol::NAME]
                                . " at $pos",
                            code => \(
                                $lexable->[Marpa::Internal::Symbol::ACTION]
                            ),
                        }
                    );
                } ## end if ( not $eval_ok or @warnings )
            }

            next LEXABLE if not defined $match;

            $length //= length $match;

            push @alternatives, [ $lexable, $match, $length ];
            if ($trace_lex_matches) {
                print {$trace_fh}
                    'Matched Closure for ',
                    $lexable->[Marpa::Internal::Symbol::NAME],
                    " at $pos: ", $match, "\n"
                    or croak('Could not print to trace file');
            } ## end if ($trace_lex_matches)

            last LEXABLE unless $ambiguous_lex;

        }    # LEXABLE

        $active = scan_set( $parse, @alternatives );

        $pos++;

        last POS if not $active;

    }    # POS

    return
          $active              ? -2
        : $pos < $input_length ? $pos
        :                        -1;

}    # sub text

# Always returns success
sub Marpa::Recognizer::end_input {
    my $self = shift;

    my $grammar = $self->[Marpa::Internal::Recognizer::GRAMMAR];
    my $phase   = $grammar->[Marpa::Internal::Grammar::PHASE];

    # If called repeatedly, just return success,
    # without complaint.  In other words, be idempotent.
    return 1 if $phase >= Marpa::Internal::Phase::RECOGNIZED;

    my $last_completed_set =
        $self->[Marpa::Internal::Recognizer::LAST_COMPLETED_SET];
    my $furthest_earleme =
        $self->[Marpa::Internal::Recognizer::FURTHEST_EARLEME];
    while ( $last_completed_set++ < $furthest_earleme ) {
        Marpa::Internal::Recognizer::complete_set($self);
    }
    $self->[Marpa::Internal::Recognizer::CURRENT_SET] = $furthest_earleme;

    if ( $grammar->[Marpa::Internal::Grammar::STRIP] ) {

        $#{$self} = Marpa::Internal::Recognizer::LAST_EVALUATOR_FIELD;

        $#{$grammar} = Marpa::Internal::Grammar::LAST_EVALUATOR_FIELD;
        for my $symbol ( @{ $grammar->[Marpa::Internal::Grammar::SYMBOLS] } )
        {
            $#{$symbol} = Marpa::Internal::Symbol::LAST_EVALUATOR_FIELD;
        }
        for my $rule ( @{ $grammar->[Marpa::Internal::Grammar::RULES] } ) {
            $#{$rule} = Marpa::Internal::Rule::LAST_EVALUATOR_FIELD;
        }
        for my $QDFA ( @{ $grammar->[Marpa::Internal::Grammar::QDFA] } ) {
            $#{$QDFA} = Marpa::Internal::QDFA::LAST_EVALUATOR_FIELD;
        }

    } ## end if ( $grammar->[Marpa::Internal::Grammar::STRIP] )

    $grammar->[Marpa::Internal::Grammar::PHASE] =
        Marpa::Internal::Phase::RECOGNIZED;

    return 1;
} ## end sub Marpa::Recognizer::end_input

# It's bad style, but this routine is in a tight loop -- it may be called
# as often as once per character of input in.  For efficiency
# I pull the token alternatives out of @_ one by one as I go in the code,
# rather than at the beginning of the method.

# The remaining arguments should be a list of token alternatives, as
# array references.  The array for each alternative is (token, value,
# length), where token is a symbol reference, value can anything
# meaningful to the user, and length is the length of this token in
# earlemes.

# Given a parse object and a list of alternative tokens starting at
# the current earleme, add Earley items to recognize those tokens.

## no critic (Subroutines::RequireArgUnpacking)
sub scan_set {
## use critic

    my $parse = shift;

    my ($earley_set_list, $earley_hash,      $grammar,
        $current_set,     $furthest_earleme, $exhausted,
        )
        = @{$parse}[
        EARLEY_SETS,      EARLEY_HASH, GRAMMAR, CURRENT_SET,
        FURTHEST_EARLEME, EXHAUSTED
        ];
    croak('Attempt to scan tokens after parsing was exhausted') if $exhausted;
    my $QDFA = $grammar->[Marpa::Internal::Grammar::QDFA];

    my $earley_set = $earley_set_list->[$current_set];

    if ( not defined $earley_set ) {
        $earley_set_list->[$current_set] = [];
        $parse->[Marpa::Internal::Recognizer::CURRENT_SET]++;
        return 1;
    }

    # Important: more earley sets can be added in the loop
    my $earley_set_ix = -1;
    EARLEY_ITEM: while (1) {

        my $earley_item = $earley_set->[ ++$earley_set_ix ];
        last EARLEY_ITEM unless defined $earley_item;

        my ( $state, $parent ) = @{$earley_item}[
            Marpa::Internal::Earley_item::STATE,
            Marpa::Internal::Earley_item::PARENT
        ];

        # I allow ambigious tokenization.
        # Loop through the alternative tokens.
        ALTERNATIVE: for my $alternative (@_) {
            my ( $token, $value, $length ) = @{$alternative};

            if ( $length <= 0 ) {
                croak(    'Token '
                        . $token->[Marpa::Internal::Symbol::NAME]
                        . ' with bad length '
                        . $length );
            } ## end if ( $length <= 0 )

            # Make sure it's an allowed terminal symbol.
            unless ( $token->[Marpa::Internal::Symbol::TERMINAL] ) {
                my $name = $token->[Marpa::Internal::Symbol::NAME];
                croak(    'Non-terminal '
                        . ( defined $name ? "$name " : q{} )
                        . 'supplied as token' );
            } ## end unless ( $token->[Marpa::Internal::Symbol::TERMINAL] )

            # compute goto(state, token_name)
            my $states =
                $QDFA->[ $state->[Marpa::Internal::QDFA::ID] ]
                ->[Marpa::Internal::QDFA::TRANSITION]
                ->{ $token->[Marpa::Internal::Symbol::NAME] };
            next ALTERNATIVE unless $states;

            # Create the kernel item and its link.
            my $target_ix = $current_set + $length;
            my $target_set = ( $earley_set_list->[$target_ix] //= [] );
            if ( $target_ix > $furthest_earleme ) {
                $parse->[Marpa::Internal::Recognizer::FURTHEST_EARLEME] =
                    $furthest_earleme = $target_ix;
            }
            STATE: for my $state ( @{$states} ) {
                my $reset    = $state->[Marpa::Internal::QDFA::RESET_ORIGIN];
                my $origin   = $reset ? $target_ix : $parent;
                my $state_id = $state->[Marpa::Internal::QDFA::ID];
                my $name     = sprintf 'S%d@%d-%d', $state_id, $origin,
                    $target_ix;
                my $target_item = $earley_hash->{$name};
                unless ( defined $target_item ) {
                    $target_item = [];
                    @{$target_item}[
                        Marpa::Internal::Earley_item::NAME,
                        Marpa::Internal::Earley_item::STATE,
                        Marpa::Internal::Earley_item::PARENT,
                        Marpa::Internal::Earley_item::LINKS,
                        Marpa::Internal::Earley_item::TOKENS,
                        Marpa::Internal::Earley_item::SET
                        ]
                        = ( $name, $state, $origin, [], [], $target_ix );
                    $earley_hash->{$name} = $target_item;
                    push @{$target_set}, $target_item;
                } ## end unless ( defined $target_item )
                next STATE if $reset;
                push @{ $target_item->[Marpa::Internal::Earley_item::TOKENS]
                    },
                    [ $earley_item, $value ];
            }    # for my $state

        }    # ALTERNATIVE

    }    # EARLEY_ITEM

    if ( $current_set >= $furthest_earleme ) {
        $parse->[Marpa::Internal::Recognizer::EXHAUSTED] = $exhausted = 1;
        return 0;
    }

    $parse->[Marpa::Internal::Recognizer::CURRENT_SET]++;
    return 1;

}    # sub scan_set

sub complete_set {
    my $parse = shift;

    my ($earley_set_list,  $earley_hash, $grammar, $current_set,
        $furthest_earleme, $exhausted,   $lexables_by_state
        )
        = @{$parse}[
        EARLEY_SETS,      EARLEY_HASH, GRAMMAR, CURRENT_SET,
        FURTHEST_EARLEME, EXHAUSTED,   LEXABLES_BY_STATE,
        ];
    croak(
        'Attempt to complete another earley set after parsing was exhausted')
        if $exhausted;

    my $earley_set = $earley_set_list->[$current_set];

    $earley_set ||= [];

    my ( $QDFA, $symbols, $tracing ) = @{$grammar}[
        Marpa::Internal::Grammar::QDFA,
        Marpa::Internal::Grammar::SYMBOLS,
        Marpa::Internal::Grammar::TRACING,
    ];

    my ( $trace_fh, $trace_completions );
    if ($tracing) {
        $trace_fh = $grammar->[Marpa::Internal::Grammar::TRACE_FILE_HANDLE];
        $trace_completions =
            $grammar->[Marpa::Internal::Grammar::TRACE_COMPLETIONS];
    }

    my $lexable_seen = [];
    $#{$lexable_seen} = $#{$symbols};

    # Important: more earley sets can be added in the loop
    my $earley_set_ix = -1;
    EARLEY_ITEM: while (1) {

        my $earley_item = $earley_set->[ ++$earley_set_ix ];
        last EARLEY_ITEM unless defined $earley_item;

        my ( $state, $parent ) = @{$earley_item}[
            Marpa::Internal::Earley_item::STATE,
            Marpa::Internal::Earley_item::PARENT
        ];
        my $state_id = $state->[Marpa::Internal::QDFA::ID];

        for my $lexable ( @{ $lexables_by_state->[$state_id] } ) {
            $lexable_seen->[$lexable] = 1;
        }

        next EARLEY_ITEM if $current_set == $parent;

        COMPLETE_RULE:
        for my $complete_symbol_name (
            @{ $state->[Marpa::Internal::QDFA::COMPLETE_LHS] } )
        {
            PARENT_ITEM:
            for my $parent_item ( @{ $earley_set_list->[$parent] } ) {
                my ( $parent_state, $grandparent ) = @{$parent_item}[
                    Marpa::Internal::Earley_item::STATE,
                    Marpa::Internal::Earley_item::PARENT
                ];
                my $states =
                    $QDFA->[ $parent_state->[Marpa::Internal::QDFA::ID] ]
                    ->[Marpa::Internal::QDFA::TRANSITION]
                    ->{$complete_symbol_name};
                next PARENT_ITEM unless defined $states;

                TRANSITION_STATE: for my $transition_state ( @{$states} ) {
                    my $reset = $transition_state
                        ->[Marpa::Internal::QDFA::RESET_ORIGIN];
                    my $origin = $reset ? $current_set : $grandparent;
                    my $transition_state_id =
                        $transition_state->[Marpa::Internal::QDFA::ID];
                    my $name = sprintf 'S%d@%d-%d', $transition_state_id,
                        $origin, $current_set;
                    my $target_item = $earley_hash->{$name};
                    unless ( defined $target_item ) {
                        $target_item = [];
                        @{$target_item}[
                            Marpa::Internal::Earley_item::NAME,
                            Marpa::Internal::Earley_item::STATE,
                            Marpa::Internal::Earley_item::PARENT,
                            Marpa::Internal::Earley_item::LINKS,
                            Marpa::Internal::Earley_item::TOKENS,
                            Marpa::Internal::Earley_item::SET,
                            ]
                            = (
                            $name, $transition_state, $origin, [], [],
                            $current_set,
                            );
                        $earley_hash->{$name} = $target_item;
                        push @{$earley_set}, $target_item;
                    }    # unless defined $target_item
                    next TRANSITION_STATE if $reset;
                    push
                        @{ $target_item->[Marpa::Internal::Earley_item::LINKS]
                        },
                        [ $parent_item, $earley_item ];
                }    # TRANSITION_STATE

            }    # PARENT_ITEM

        }    # COMPLETE_RULE

    }    # EARLEY_ITEM

    EARLEY_ITEM: for my $earley_item ( @{$earley_set} ) {
        my $links = $earley_item->[Marpa::Internal::Earley_item::LINKS];
        my @sorted_links =
            map { $_->[0] }
            ## no critic (BuiltinFunctions::ProhibitReverseSortBlock)
            sort { $b->[1] cmp $a->[1] }
            ## use critic
            map {
            [   $_,
                $_->[1]->[Marpa::Internal::Earley_item::STATE]
                    ->[Marpa::Internal::QDFA::PRIORITY]
            ]
            } @{$links};
        $earley_item->[Marpa::Internal::Earley_item::LINKS] = \@sorted_links;
    } ## end for my $earley_item ( @{$earley_set} )

    # TODO: Prove that the completion links are UNIQUE

    $parse->[Marpa::Internal::Recognizer::LAST_COMPLETED_SET] = $current_set;

    if ($trace_completions) {
        print {$trace_fh} Marpa::show_earley_set($earley_set)
            or croak('Cannot print to trace file');
    }

    my $lexables = [
        sort {
            $a->[Marpa::Internal::Symbol::PRIORITY]
                cmp $b->[Marpa::Internal::Symbol::PRIORITY]
            }
            map { $symbols->[$_] }
            grep { $lexable_seen->[$_] } ( 0 .. $#{$symbols} )
    ];
    return $lexables;

}    # sub complete_set

1;

__END__

=pod

=head1 NAME

Marpa::Recognizer - Marpa Recognizer Objects

=head1 SYNOPSIS

=begin Marpa::Test::Display:

## next 2 displays
in_file($_, 't/equation_s.t');

=end Marpa::Test::Display:

    my $recce = new Marpa::Recognizer( { grammar => $grammar } );

    my $fail_offset = $recce->text('2-0*3+1');
    if ( $fail_offset >= 0 ) {
        croak("Parse failed at offset $fail_offset");
    }

Z<>

=begin Marpa::Test::Display:

## next 5 displays
in_file($_, 't/equation.t');

=end Marpa::Test::Display:

    my $recce = new Marpa::Recognizer( { grammar => $grammar } );

    my $op     = $grammar->get_symbol('Op');
    my $number = $grammar->get_symbol('Number');

    my @tokens = (
        [ $number, 2,    1 ],
        [ $op,     q{-}, 1 ],
        [ $number, 0,    1 ],
        [ $op,     q{*}, 1 ],
        [ $number, 3,    1 ],
        [ $op,     q{+}, 1 ],
        [ $number, 1,    1 ],
    );

    TOKEN: for my $token (@tokens) {
        next TOKEN if $recce->earleme($token);
        croak( 'Parsing exhausted at character: ', $token->[1] );
    }

    $recce->end_input();

=head1 DESCRIPTION

Marpa parsing takes place in three major phases: grammar creation, input recognition
and parse evaluation.
Once a grammar object has rules,
a recognizer object can be created from it.
The recognizer accepts input and
can be used to create a Marpa evaluator object.

=head2 Tokens and Earlemes

Marpa allows ambiguous tokens.
Several Marpa tokens can start at a single parsing location.
Marpa tokens can be of various lengths.
Marpa tokens can even overlap.

For most parsers, position is location in a token stream.
To deal with variable-length and overlapping tokens,
Marpa needs a more flexible idea of location.
Marpa's idea of position is location in an B<earleme> stream.
B<Earlemes> are named after Jay Earley, the inventor of the first algorithm
in Marpa's lineage.

While scanning, Marpa keeps track of the B<current earleme>.
Earlemes in an earleme start at earleme 0 and increase numerically.
The earleme immediately following earleme 0 is earleme 1,
the earleme immediately following earleme 1 is earleme 2,
and so on.
The earleme immediately following earleme I<N> is always earleme I<N+1>.

B<Distance> in the earleme stream are what you'd expect.
The distance between earleme I<X> and earleme I<Y> is
the absolute value of the difference between I<X> and I<Y>,
I<|X-Y|>.
The distance from earleme 3 to earleme 6,
for example, is 3 earlemes.

Whenever a token is given to Marpa to be scanned,
it starts at the current earleme.
In addition to the type and value of the token,
Marpa must be told token's B<length> in earlemes.
The length of a Marpa token must be greater than zero.
This earleme length will become
the distance from the start of the
token to the end of the token.

The start of the token is put at the current earleme.
If the length of the token is I<L>,
and the number of the current earleme is I<C>,
the end of the token will be at the earleme number I<C+L>.

=head3 The One-Character-Per-Earleme Model

Many different models of the relationship between tokens and earlemes
are possible, but two are particularly important.
One is the one-token-per-earleme model.
The other is the one-character-per-earleme model.
If you do your lexing with the C<text> method,
you will use a
one-character-per-earleme model.

Using the C<text> method, Marpa receives the input as the series
of strings and string reference.
provided in the one or more calls to the C<text> method.
The B<raw input> can be thought of the concatenation of these
strings,
even though the strings are not physically concatenated.
When the C<text> method is used,
character position in this raw input will 
correspond exactly one-to-one with the earleme position.

Every character will be treated as being exactly one
earleme in length.
Any tokens which are more than one several character in length,
will span earlemes.

It is common, when a one-character-per-earleme model of input is used,
for there to be many earlemes at which no tokens start.
For example,
in a standard implementation
of a grammar for a language which allows
comments,
no tokens will start at
any earlemes which corresponds to character locations inside
a comment.

=head3 Other Models

Marpa is not restricted to the one-character-per-earleme model.
Most parser generators treat location as position in a token stream.
In Marpa, this correspoind to a
one-token-per-earleme model.

If you use the C<earleme> method, you can structure your input in almost any way you like.
There are only four restrictions:

=over 4

=item 1

Scanning always starts at earleme 0.

=item 2

Earleme I<N> is always scanned immediately before earleme I<N+1>.
In other words, the earlemes are scanned one by one in increasing numerical order.

=item 3

When an earleme is scanned, all tokens starting at that earleme must be
added.
It is perfectly acceptable for there to be no tokens
starting at a given earleme.
However, once earleme I<N> is scanned,
it is no longer possible to add a token starting at any of the earlemes
from 0 to I<N>.

=item 4

With every token, a length in earlemes must be given,
and this length cannot be zero or negative.

=back

=head2 Exhaustion

At the start of parsing,
the B<furthest earleme> is earleme 0.
When a token is recognized, its end earleme is determined by
adding the token length to the current earleme.
If the new token's end earleme is after the furthest earleme,
the furthest earleme is set at the new token's end earleme.

If, after scanning all the tokens at an earleme,
the current earleme
has reached the furthest earleme,
no more successful parses are possible.
At this point, the recognizer is said to
be B<exhausted>.
A recognizer is B<active>
if and only if it is not exhausted.

Parsing is said to be exhausted,
when the recognizer is exhausted.
Parsing is said to be active,
when the recognizer is active.

Exhausted parsing does not mean failed parsing.
In particular,
parsing is often exhausted at the point of a successful parse.
An exhausted recognizer
may also contain successful parses
both prior to the current earleme.

Conversely, active parsing does not mean successful parsing.
A recognizer remains active as long as some potential input
I<might> produce a successful parse.
This does not mean that it ever will.

Marpa parsing can remain active even if
no token is found at the current earleme.
In the one-character-per-earleme model,
the current earleme might fall in the middle of a
previously recognized token
and parsing will remain active at least until the end of that
token is reached.
In the one-character-per-earleme model,
stretches where no token either starts or ends
can be many earlemes in length.

=head2 Cloning

The C<new> constructor requires a grammar to be specified in
one of its arguments.
By default, the C<new> constructor clones the grammar object.
This is done so that recognizers do not interfere with each other by
modifying the same data.
Cloning is the default behavior, and is always safe.

While safe, cloning does impose an overhead in memory and time.
This can be avoided by using the C<clone> option with the C<new>
constructor.
Not cloning is safe if you know that the grammar object will not be shared by another recognizer
or used by more than one evaluator.

It is very common for a Marpa program to have simple
flows of data, where no more than one recognizer is created from any grammar,
and no more than one evaluator is created from any recognizer.
When this is the case, cloning is unnecessary.

=head1 METHODS

=head2 new

=begin Marpa::Test::Display:

## start display
## next display
is_file($_, 'author.t/misc.t', 'new Recognizer snippet');

=end Marpa::Test::Display:

    my $recce = new Marpa::Recognizer(
        {    grammar      => $grammar,
             lex_preamble => $new_lex_preamble,
        }
    );

=begin Marpa::Test::Display:

## end display

=end Marpa::Test::Display:

The C<new> method's one, required, argument is a hash reference of named
arguments.
The C<new> method either returns a new parse object or throws an exception.
Either the C<stringified_grammar> or the C<grammar> named argument must be specified, but not both.
A recognizer is created with
the current earleme
set at earleme 0.

If the C<grammar> option is specified, 
its value must be a grammar object with rules defined.
By default, the grammar is cloned for use in the recognizer.

If the C<stringified_grammar> option is specified, 
its value must be a Perl 5 string containing a stringified Marpa grammar,
as produced by L<C<Marpa::Grammar::stringify>|Marpa::Grammar/"stringify">.
It will be unstringified for use in the recognizer.
When the C<stringified_grammar> option is specified, 
the resulting grammar is never cloned,
regardless of the setting of the C<clone> argument.

If the C<clone> argument is set to 1,
and the grammar argument is not in stringified form,
C<new> clones the grammar object.
This prevents
evaluators from interfering with each other's data.
This is the default and is always safe.
If C<clone> is set to 0,
the evaluator will work directly with
the grammar object which was its argument.
See L<above|/"Cloning"> for more detail.

Marpa options can also
be named arguments to C<new>.
For details of the Marpa options,
see L<Marpa::Doc::Options>.

=head2 text

=begin Marpa::Test::Display:

## next display
in_file($_, 't/equation_s.t');

=end Marpa::Test::Display:

    my $fail_offset = $recce->text('2-0*3+1');
    if ( $fail_offset >= 0 ) {
        croak("Parse failed at offset $fail_offset");
    }

Extends the parse using the one-character-per-earleme model.
The one, required, argument must be
a string or a
reference to a string which contains text to be parsed.
If all the input was successfully consumed, the C<text> method returns
a negative number.
The return value is -1 if parsing was exhausted after consuming the
entire input.
The return value is -2 if parsing was still active after consuming the
entire input.

If parsing was exhausted before all the input was consumed,
the C<text> method returns the number of characters that were
consumed before parsing was exhausted.
If C<text> is called on an exhausted recognizer,
so that none of the input can be consumed,
the return value is 0.
Failures, other than exhausted recognizers, are thrown as exceptions.

Terminals are recognized in the text
using the lexers that were specified in the porcelain
or the plumbing.
The earleme length of each token is
set to the length of the token in characters.
(If a token has a "lex prefix",
the length of the lex prefix counts as part of the token length.)

Subsequent
calls to C<text> on the same recognizer always advance the earleme numbering
monotonically.
The I<c>th character,
where the count I<c> includes
all characters from any previous calls to the C<text> method
for this recognizer,
will start at earleme I<c-1>
and will end at earleme I<c>.

How a string is divided up among calls to the C<text> method
makes no difference in the earleme location of individual characters,
but it can affect the recognition of terminals by the lexers.
If the characters from a single terminal
are split between two C<text> calls,
the lexers will fail to recognize that terminal.
Terminals cannot span calls to the C<text> method.

=head2 earleme

=begin Marpa::Test::Display:

## next display
in_file($_, 't/ah2.t');

=end Marpa::Test::Display:

    my $a = $grammar->get_symbol('a');
    $recce->earleme( [ $a, 'a', 1 ] ) or croak('Parsing exhausted');

The C<earleme> method takes zero or more arguments.
Each argument represents a token which starts at the B<current earleme>.
Because ambiguous lexing is allowed.
more than one token may start at each earleme,
in which case, there will be one argument per token.
Because tokens can span earlemes,
no tokens may start at an earleme
in which case the call to C<earleme> will have zero arguments.

After adding the tokens to the recognizer,
the C<earleme> method determines whether the recognizer is active or exhausted.
If the recognizer is still active,
the C<earleme> method moves the current earleme forward by one,
and the C<earleme> method returns 1.
If the recognizer is exhausted, the current earleme stays where it is,
and the C<earleme> method returns 0.
The C<earleme> method throws an exception on failure.
Any attempt to add more input to an exhausted recognizer will fail.

Each token argument is a reference to a three element array.
The first element is a "cookie" for the token's symbol,
as returned by the C<Marpa::Grammar::get_symbol> method
or the C<get_symbol> method of a porcelain interface.
The second element is the token's value in the parse,
and may be any value legal in Perl 5, including undefined.
The third is the token's length in earlemes.

While the recognizer is active, 
an earleme remains the current earleme during only one call of the C<earleme> method.
All tokens starting at that earleme must be added in that call.
The first time that the C<earleme> method is called in a recognizer,
the current earleme is at earleme 0.

Once a recognizer is exhausted, the current earleme never moves
and no more input can be added.
It is possible for a call to B<earleme>
with no arguments
to exhaust the recognizer.
This happens if
C<earleme> is called
with zero arguments when the current earleme reaches 
the furthest earleme.

C<earleme> is the low-level token input method.
Unlike C<text>, the C<earleme> method assumes no particular model of the input.
It is up to the user to define the relationship between
tokens and earlemes.

=head2 end_input

=begin Marpa::Test::Display:

## next display
in_file($_, 't/equation.t');

=end Marpa::Test::Display:

    $recce->end_input();

Used to indicate the end of input.
Tells the recognizer that
no new tokens will be added,
or, in other words,
that no tokens will start at
or after the current earleme.
The C<end_input> method takes no arguments.

The C<end_input> method
does not change the location of the furthest earleme.
After a successful call to 
the C<end_input> method,
the current earleme will be positioned at the furthest earleme.
Since positioning the current earleme at the furthest
earleme leaves the recognizer exhausted,
any further calls to C<text> will return 0,
and any further calls to C<earleme> will throw an
exception.

The C<end_input> method returns a Perl true value on success.
On failure, it throws an exception.
The C<end_input> method can only usefully be called once
per recognizer, but the method is idempotent.
Subsequent calls to the C<end_input> method
will have no effect and will return a Perl true.

=head2 stringify

=begin Marpa::Test::Display:

## next display
in_file($_, 'author.t/misc.t');

=end Marpa::Test::Display:

    my $stringified_recce = $recce->stringify();

The C<stringify> method takes as its single argument a recognizer object
and converts it into a string.
It returns a reference to the string.
The string is created 
using L<Data::Dumper>.
On failure, C<stringify> throws an exception.

=head2 unstringify

=begin Marpa::Test::Display:

## start display
## next display
is_file($_, 'author.t/misc.t', 'unstringify Recognizer snippet');

=end Marpa::Test::Display:

    $recce = Marpa::Recognizer::unstringify( $stringified_recce, $trace_fh );

    $recce = Marpa::Recognizer::unstringify($stringified_recce);

=begin Marpa::Test::Display:

## end display

=end Marpa::Test::Display:

The C<unstringify> static method takes a reference to a stringified recognizer as its first
argument.
Its second, optional, argument is a file handle.
The file handle argument will be used both as the unstringified recognizer's trace file handle,
and for any trace messages produced by C<unstringify> itself.
C<unstringify> returns the unstringified recognizer object unless it throws an
exception.

If the trace file handle argument is omitted,
it defaults to C<STDERR>
and the unstringified recognizer's trace file handle reverts to the default for a new
recognizer, which is also C<STDERR>.
The trace file handle argument is necessary because in the course of stringifying,
the recognizer's original trace file handle may have been lost.

=head2 clone

=begin Marpa::Test::Display:

## next 2 displays
in_file($_, 'author.t/misc.t');

=end Marpa::Test::Display:

    my $cloned_recce = $recce->clone();

The C<clone> method creates a useable copy of a recognizer object.
It returns a successfully cloned recognizer object,
or throws an exception.

=head1 SUPPORT

See the L<support section|Marpa/SUPPORT> in the main module.

=head1 AUTHOR

Jeffrey Kegler

=head1 LICENSE AND COPYRIGHT

Copyright 2007 - 2009 Jeffrey Kegler

This program is free software; you can redistribute
it and/or modify it under the same terms as Perl 5.10.0.

=cut
