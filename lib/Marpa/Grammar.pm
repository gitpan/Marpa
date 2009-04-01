package Marpa::Internal;

use 5.010;

use warnings;
no warnings 'recursion';
use strict;

=begin Implementation:

Structures and Objects: The design is to present an object-oriented
interface, but internally to avoid overheads.  So internally, where
objects might be used, I use array with constant indices to imitate
what in C would be structures.

=end Implementation:

=cut

# It's all integers, except for the version number
use integer;

package Marpa::Internal;

use Marpa::Offset Symbol =>

    # basic data
    qw(ID NAME),

    # evaluator data
    qw(IS_CHAF_NULLING NULL_ALIAS NULLING),

    # recognizer data
    qw(
    ACTION PREFIX SUFFIX
    REGEX PRIORITY TERMINAL
    ),

    # temporary data
    qw(
    LHS RHS ACCESSIBLE PRODUCTIVE START
    NULLABLE NULL_VALUE
    CLOSURE
    COUNTED
);

package Marpa::Internal::Symbol;
use constant LAST_EVALUATOR_FIELD  => Marpa::Internal::Symbol::NULLING;
use constant LAST_RECOGNIZER_FIELD => Marpa::Internal::Symbol::TERMINAL;
use constant LAST_FIELD            => Marpa::Internal::Symbol::COUNTED;

package Marpa::Internal;

# LHS             - rules with this as the lhs,
#                   as a ref to an array of rule refs
# RHS             - rules with this in the rhs,
#                   as a ref to an array of rule refs
# ACCESSIBLE      - reachable from start symbol?
# PRODUCTIVE      - reachable from input symbol?
# START           - is one of the start symbols?
# REGEX           - regex, for terminals; undef otherwise
# NULLING         - always is null?
# NULLABLE        - can match null?
# NULL_VALUE      - value when null
# NULL_ALIAS      - for a non-nullable symbol,
#                   ref of a its nulling alias,
#                   if there is one
#                   otherwise undef
# TERMINAL        - terminal?
# CLOSURE         - closure to do lexing
# PRIORITY        - order, for lexing
# COUNTED         - used on rhs of counted rule?
# ACTION          - lexing action specified by user
# PREFIX          - lexing prefix specified by user
# SUFFIX          - lexing suffix specified by user
# IS_CHAF_NULLING - if CHAF nulling lhs, ref to array
#                   of rhs symbols

use Marpa::Offset Rule =>

    # basic data
    qw(ID NAME LHS RHS
    ),

    # evaluator data
    qw(
    USEFUL ACTION
    CODE CYCLE
    PRIORITY HASTY
    HAS_CHAF_LHS HAS_CHAF_RHS
    ),

    # temporary data
    qw(
    ORIGINAL_RULE
    NULLABLE ACCESSIBLE PRODUCTIVE NULLING
);

package Marpa::Internal::Rule;
use constant LAST_EVALUATOR_FIELD  => Marpa::Internal::Rule::HAS_CHAF_RHS;
use constant LAST_RECOGNIZER_FIELD => Marpa::Internal::Rule::HAS_CHAF_RHS;
use constant LAST_FIELD            => Marpa::Internal::Rule::NULLING;

package Marpa::Internal;

=begin Implementation:

LHS - ref of the left hand symbol
RHS - array of symbol refs
NULLABLE - can match null?
ACCESSIBLE - reachable from start symbol?
PRODUCTIVE - reachable from input symbol?
NULLING - always matches null?
USEFUL - use this rule in NFA?
ACTION - action for this rule as specified by user
CLOSURE - closure for evaluating this rule
ORIGINAL_RULE - for a rewritten rule, the original
HAS_CHAF_LHS  - has CHAF internal symbol as lhs?
HAS_CHAF_RHS - has CHAF internal symbol on rhs?
PRIORITY - rule priority
CODE - code used to create closure
CYCLE - is this rule part of a cycle?

=end Implementation:

=cut

use Marpa::Offset NFA =>
    qw(ID NAME ITEM TRANSITION AT_NULLING COMPLETE PRIORITY);

=begin Implementation:

ITEM - an LR(0) item
TRANSITION - the transitions, as a hash from symbol name to NFA states
AT_NULLING - dot just before a nullable symbol?
COMPLETE - rule is complete?
PRIORITY - rule priority

=end Implementation:

=cut

use Marpa::Offset QDFA =>

    # basic data
    qw(ID NAME TAG),

    # evaluator data
    qw(COMPLETE_RULES START_RULE),

    # recognizer data
    qw(TRANSITION COMPLETE_LHS
    RESET_ORIGIN PRIORITY
    ),

    # temporary data
    qw(NFA_STATES
);

package Marpa::Internal::QDFA;
use constant LAST_EVALUATOR_FIELD  => Marpa::Internal::QDFA::START_RULE;
use constant LAST_RECOGNIZER_FIELD => Marpa::Internal::QDFA::PRIORITY;
use constant LAST_FIELD            => Marpa::Internal::QDFA::NFA_STATES;

package Marpa::Internal;

=begin Implementation:

NFA_STATES     - in an QDFA: an array of NFA states
TRANSITION     - the transitions, as a hash
               - from symbol name to references to arrays
               - of QDFA states
COMPLETE_LHS   - an array of the lhs's of complete rules
COMPLETE_RULES - an array of lists of the complete rules,
               - indexed by lhs
START_RULE     - the start rule
TAG            - implementation-independant tag
RESET_ORIGIN   - reset origin for this state?
PRIORITY       - priority of this state

=end Implementation:

=cut

use Marpa::Offset LR0_item => qw(RULE POSITION);

use Marpa::Offset Grammar =>

    # basic data
    qw(
    ID NAME VERSION
    RULES SYMBOLS QDFA
    PHASE DEFAULT_ACTION
    TRACE_FILE_HANDLE TRACING
    STRIP CODE_LINES
    ),

    # evaluator data
    qw(
    DEFAULT_NULL_VALUE
    CYCLE_ACTION
    TRACE_ITERATIONS
    TRACE_ACTIONS TRACE_VALUES
    MAX_PARSES
    PREAMBLE
    ),

    # recognizer data
    qw(
    PROBLEMS
    ACADEMIC
    DEFAULT_LEX_PREFIX DEFAULT_LEX_SUFFIX AMBIGUOUS_LEX
    TRACE_LEX_MATCHES TRACE_COMPLETIONS TRACE_LEX_TRIES
    SYMBOL_HASH
    START_STATES
    LEX_PREAMBLE
    ),

    # temporary data
    qw(
    RULE_HASH
    START START_NAME
    NFA QDFA_BY_NAME
    NULLABLE_SYMBOL
    LOCATION_CALLBACK
    WARNINGS
    INACCESSIBLE_OK
    UNPRODUCTIVE_OK
    SEMANTICS
    TRACE_RULES TRACE_STRINGS TRACE_PREDEFINEDS TRACE_PRIORITIES
    ALLOW_RAW_SOURCE INTERFACE
);

package Marpa::Internal::Grammar;

use constant LAST_EVALUATOR_FIELD  => Marpa::Internal::Grammar::PREAMBLE;
use constant LAST_RECOGNIZER_FIELD => Marpa::Internal::Grammar::LEX_PREAMBLE;
use constant LAST_FIELD            => Marpa::Internal::Grammar::INTERFACE;

=begin Implementation:

ID                 - number of this grammar
NAME               - namespace special to this grammar
                     it should only be used BEFORE compilation, because it's not
                     guaranteed unique after decompilation
RULES              - array of rule refs
SYMBOLS            - array of symbol refs
RULE_HASH          - hash by name of rule refs
SYMBOL_HASH        - hash by name of symbol refs
START              - ref to start symbol
START_NAME         - name of original symbol
NFA                - array of states
QDFA               - array of states
QDFA_BY_NAME       - hash from QDFA name to QDFA reference
NULLABLE_SYMBOL    - array of refs of the nullable symbols
ACADEMIC           - true if this is a textbook grammar,
                   - for checking the NFA and QDFA, and NOT
                   - for actual Earley parsing
DEFAULT_NULL_VALUE - default value for nulling symbols
DEFAULT_ACTION     - action for rules without one
DEFAULT_LEX_PREFIX - default prefix for lexing
DEFAULT_LEX_SUFFIX - default suffix for lexing
AMBIGUOUS_LEX      - lex ambiguously?
LOCATION_CALLBACK  - default callback for showing location
PROBLEMS - fatal problems
PREAMBLE - evaluation preamble
LEX_PREAMBLE - lex preamble
WARNINGS - print warnings about grammar?
VERSION - Marpa version this grammar was stringified from
CODE_LINES - max lines to display on failure
SEMANTICS - semantics (currently perl5 only)
STRIP - Boolean.  If true, strip unused data to save space.
TRACING - master flag, set if any tracing is being done
    (to control overhead for non-tracing processes)
TRACE_STRINGS - trace strings defined in marpa grammar
TRACE_PREDEFINEDS - trace predefineds in marpa grammar
PHASE - the grammar's phase
INTERFACE - the grammar's interface
START_STATES - ref to array of the start states
CYCLE_ACTION - ref to array of the start states

=end Implementation:

=cut

package Marpa::Internal;

# values for grammar interfaces
use Marpa::Offset Interface => qw(RAW MDL);

# Apparently Perl::Critic has a bug and cannot find the final return here
## no critic (Subroutines::RequireFinalReturn)
sub Marpa::Internal::Interface::description {
## use critic
    my $interface = shift;
    given ($interface) {
        when (Marpa::Internal::Interface::RAW) { return 'raw interface' }
        when (Marpa::Internal::Interface::MDL) {
            return 'Marpa Description Language interface';
        }
    } ## end given
    return 'unknown interface';
} ## end sub Marpa::Internal::Interface::description

# values for grammar phases
use Marpa::Offset Phase =>
    qw(NEW RULES PRECOMPUTED RECOGNIZING RECOGNIZED EVALUATING);

sub Marpa::Internal::Phase::description {
    my $phase       = shift;
    my $description = 'unknown phase';
    given ($phase) {
        when (Marpa::Internal::Phase::NEW) {
            $description = 'grammar without rules';
        }
        when (Marpa::Internal::Phase::RULES) {
            $description = 'grammar with rules entered';
        }
        when (Marpa::Internal::Phase::PRECOMPUTED) {
            $description = 'precomputed grammar';
        }
        when (Marpa::Internal::Phase::RECOGNIZING) {
            $description = 'grammar being recognized';
        }
        when (Marpa::Internal::Phase::RECOGNIZED) {
            $description = 'recognized grammar';
        }
        when (Marpa::Internal::Phase::EVALUATING) {
            $description = 'grammar being evaluated';
        }
    } ## end given
    return $description;
} ## end sub Marpa::Internal::Phase::description

package Marpa::Internal::Grammar;

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

sub Marpa::Internal::code_problems {
    my $args = shift;

    my $grammar;
    my $fatal_error;
    my $warnings = [];
    my $where    = '?where?';
    my $long_where;
    my $code;
    my @msg = ();
    my $eval_value;
    my $eval_given = 0;

    while ( my ( $arg, $value ) = each %{$args} ) {
        given ($arg) {
            when ('fatal_error') { $fatal_error = $value }
            when ('grammar')     { $grammar     = $value }
            when ('where')       { $where       = $value }
            when ('long_where')  { $long_where  = $value }
            when ('warnings')    { $warnings    = $value }
            when ('code')        { $code        = $value }
            when ('eval_ok') {
                $eval_value = $value;
                $eval_given = 1;
            }
            default { push @msg, "Unknown argument to code_problems: $arg" };
        } ## end given
    } ## end while ( my ( $arg, $value ) = each %{$args} )

    my @problem_line     = ();
    my $max_problem_line = -1;
    for my $warning_data ( @{$warnings} ) {
        my ( $warning, $package, $filename, $problem_line ) =
            @{$warning_data};
        $problem_line[$problem_line] = 1;
        $max_problem_line = $problem_line
            if $problem_line > $max_problem_line;
    } ## end for my $warning_data ( @{$warnings} )

    $long_where //= $where;
    my $code_lines;
    if ( defined $grammar ) {
        $code_lines = $grammar->[Marpa::Internal::Grammar::CODE_LINES];
    }
    $code_lines //= 3;

    # if we have code
    my $code_to_print;
    my $code_length;

    # block to look for the code to print
    CODE_TO_PRINT: {

        last CODE_TO_PRINT unless defined $code;
        last CODE_TO_PRINT unless defined ${$code};

        my @lines = split /\n/xms, ${$code};
        $code_length = scalar @lines;

        last CODE_TO_PRINT if $code_lines == 0;

        # which lines to print?
        my $first_line;
        my $max_line;

        # if code_lines < 0, print all lines
        if ( $code_lines < 0 ) {
            $code_lines = $code_length;
        }

        # if we know the problem line, print code_lines
        # worth of context
        if ( $max_problem_line >= 0 ) {
            $first_line = $max_problem_line - $code_lines;
            $first_line = 1 if $first_line < 1;
            $max_line   = $max_problem_line + $code_lines;

            # else print the first 2*code_lines+1 lines
        } ## end if ( $max_problem_line >= 0 )
        else {
            $first_line = 1;
            $max_line   = $code_lines * 2 + 1;
            $max_line   = $code_length if $code_lines > $code_length;
        }

        # now create an array of the lines to print
        my @lines_to_print = do {
            my $start_splice = $first_line - 1;
            my $end_splice   = $max_line - 1;
            $end_splice = $#lines if $end_splice > $#lines;
            @lines[ $start_splice .. $end_splice ];
        };

        my @labeled_lines = ();
        LINE: for my $i ( 0 .. $#lines_to_print ) {
            my $line_number = $first_line + $i;
            my $marker      = q{};
            $marker = q{*}
                if $problem_line[$line_number];
            push @labeled_lines,
                "$marker$line_number: " . $lines_to_print[$i];
        } ## end for my $i ( 0 .. $#lines_to_print )
        $code_to_print = \( ( join "\n", @labeled_lines ) . "\n" );
    } ## end CODE_TO_PRINT:

    push @msg, 'Fatal problem(s) in ' . $long_where . "\n";
    my $warnings_count = scalar @{$warnings};
    {
        my $msg_line = 'Problems: ';
        my @problems;
        my $false_eval = $eval_given && !$eval_value && !$fatal_error;
        if ($false_eval) {
            push @problems, 'Code returned False';
        }
        push @problems, 'Fatal Error' if $fatal_error;
        push @problems, "$warnings_count Warning(s)" if $warnings_count;
        push @msg, ( join q{; }, @problems ) . "\n";
        if ( $warnings_count and not $false_eval and not $fatal_error ) {
            push @msg, "Warning(s) treated as fatal problem\n";
        }
    }

    # If we have a section of code to print
    if ( defined $code_to_print ) {
        my $header = "$code_length lines in problem code, beginning:";
        if ( $max_problem_line >= 0 ) {
            $header =
                "$code_length lines in problem code, last warning occurred here:";
        }
        push @msg, "$header\n" . ${$code_to_print} . q{======} . "\n";
    } ## end if ( defined $code_to_print )
    elsif ( defined $code_length ) {
        push @msg,
            "$code_length lines in problem code, code printing disabled\n";
    }

    for my $warning_ix ( 0 .. ( $warnings_count - 1 ) ) {
        push @msg, "Warning #$warning_ix in $where:\n";
        my $warning_message = $warnings->[$warning_ix]->[0];
        $warning_message =~ s/\n*\z/\n/xms;
        push @msg, $warning_message;
        push @msg, q{======} . "\n";
    } ## end for my $warning_ix ( 0 .. ( $warnings_count - 1 ) )

    if ($fatal_error) {
        push @msg, "Error in $where:\n";
        my $fatal_error_message = $fatal_error;
        $fatal_error_message =~ s/\n*\z/\n/xms;
        push @msg, $fatal_error_message;
        push @msg, q{======} . "\n";
    } ## end if ($fatal_error)

    croak(@msg);
} ## end sub Marpa::Internal::code_problems

package Marpa::Internal::Source_Eval;

use English qw( -no_match_vars );

sub Marpa::Internal::Grammar::raw_grammar_eval {
    my $grammar     = shift;
    my $raw_grammar = shift;

    my ( $trace_fh, $trace_strings, $trace_predefineds );
    if ( $grammar->[Marpa::Internal::Grammar::TRACING] ) {
        $trace_fh = $grammar->[Marpa::Internal::Grammar::TRACE_FILE_HANDLE];
        $trace_strings = $grammar->[Marpa::Internal::Grammar::TRACE_STRINGS];
        $trace_predefineds =
            $grammar->[Marpa::Internal::Grammar::TRACE_PREDEFINEDS];
    } ## end if ( $grammar->[Marpa::Internal::Grammar::TRACING] )

    my $new_start_symbol;
    my $new_semantics;
    my $new_version;
    my $new_preamble;
    my $new_lex_preamble;
    my $new_default_lex_prefix;
    my $new_default_action;
    my $new_default_null_value;
    my $new_rules;
    my $new_terminals;
    my %strings;

    {
        my $old_warn_handler = $SIG{__WARN__};
        my @warnings;
        $SIG{__WARN__} = sub { push @warnings, [ $_[0], ( caller 0 ) ]; };

        ## no critic (BuiltinFunctions::ProhibitStringyEval)
        my $eval_ok = eval ${$raw_grammar};
        ## use critic

        $SIG{__WARN__} = $old_warn_handler;

        if ( not $eval_ok or @warnings ) {
            my $fatal_error = $EVAL_ERROR;
            Marpa::Internal::code_problems(
                {   grammar     => $grammar,
                    eval_ok     => $eval_ok,
                    fatal_error => $fatal_error,
                    warnings    => \@warnings,
                    where       => 'evaluating gramar',
                    code        => $raw_grammar,
                }
            );
        } ## end if ( not $eval_ok or @warnings )
    }

    if ($trace_strings) {
        for my $string ( keys %strings ) {
            say {$trace_fh} qq{String "$string" set to '}, $strings{$string},
                q{'};
        }
    } ## end if ($trace_strings)

    if ( defined $new_start_symbol ) {
        $grammar->[Marpa::Internal::Grammar::START_NAME] = $new_start_symbol;
        say {$trace_fh} 'Start symbol set to ', $new_start_symbol
            if $trace_predefineds;
    }

    Carp::croak('Semantics must be set to perl5 in marpa grammar')
        if not defined $new_semantics
            or $new_semantics ne 'perl5';
    $grammar->[Marpa::Internal::Grammar::SEMANTICS] = $new_semantics;
    say {$trace_fh} 'Semantics set to ', $new_semantics
        if $trace_predefineds;

    Carp::croak('Version must be set in marpa grammar')
        if not defined $new_version;

    no integer;
    Carp::croak(
        "Version in marpa grammar ($new_version) does not match Marpa (",
        $Marpa::VERSION, ')' )
        if $new_version ne $Marpa::VERSION;
    use integer;

    $grammar->[Marpa::Internal::Grammar::VERSION] = $new_version;
    say {$trace_fh} 'Version set to ', $new_version
        if $trace_predefineds;

    if ( defined $new_lex_preamble ) {
        $grammar->[Marpa::Internal::Grammar::LEX_PREAMBLE] =
            $new_lex_preamble;
        say {$trace_fh} q{Lex preamble set to '}, $new_lex_preamble, q{'}
            if defined $trace_predefineds;
    } ## end if ( defined $new_lex_preamble )

    if ( defined $new_preamble ) {
        $grammar->[Marpa::Internal::Grammar::PREAMBLE] = $new_preamble;
        say {$trace_fh} q{Preamble set to '}, $new_preamble, q{'}
            if defined $trace_predefineds;
    }

    if ( defined $new_default_lex_prefix ) {
        $grammar->[Marpa::Internal::Grammar::DEFAULT_LEX_PREFIX] =
            $new_default_lex_prefix;
        say {$trace_fh} q{Default lex prefix set to '},
            $new_default_lex_prefix, q{'}
            if defined $trace_predefineds;
    } ## end if ( defined $new_default_lex_prefix )

    if ( defined $new_default_action ) {
        $grammar->[Marpa::Internal::Grammar::DEFAULT_ACTION] =
            $new_default_action;
        say {$trace_fh} q{Default action set to '}, $new_default_action, q{'}
            if $trace_predefineds;
    } ## end if ( defined $new_default_action )

    if ( defined $new_default_null_value ) {
        $grammar->[Marpa::Internal::Grammar::DEFAULT_NULL_VALUE] =
            $new_default_null_value;
        say {$trace_fh} q{Default null_value set to '},
            $new_default_null_value, q{'}
            if $trace_predefineds;
    } ## end if ( defined $new_default_null_value )

    Marpa::Internal::Grammar::add_user_rules( $grammar, $new_rules );
    Marpa::Internal::Grammar::add_user_terminals( $grammar, $new_terminals );

    $grammar->[Marpa::Internal::Grammar::PHASE] =
        Marpa::Internal::Phase::RULES;
    $grammar->[Marpa::Internal::Grammar::INTERFACE] =
        Marpa::Internal::Interface::RAW;

    return;
} ## end sub Marpa::Internal::Grammar::raw_grammar_eval

package Marpa::Internal::Grammar;

sub Marpa::Grammar::new {
    my ( $class, $args ) = @_;
    $args //= {};

    my $grammar = [];
    bless $grammar, $class;

    # set the defaults and the default defaults
    $grammar->[Marpa::Internal::Grammar::TRACE_FILE_HANDLE] = *STDERR;
    state $grammar_number = 0;
    $grammar->[Marpa::Internal::Grammar::ID] = $grammar_number++;

    # Note: this limits the number of grammar to the number of integers --
    # not likely to be a big problem.
    $grammar->[Marpa::Internal::Grammar::NAME] = sprintf 'Marpa::G_%x',
        $grammar_number;

    $grammar->[Marpa::Internal::Grammar::ACADEMIC]           = 0;
    $grammar->[Marpa::Internal::Grammar::DEFAULT_LEX_PREFIX] = q{};
    $grammar->[Marpa::Internal::Grammar::DEFAULT_LEX_SUFFIX] = q{};
    $grammar->[Marpa::Internal::Grammar::AMBIGUOUS_LEX]      = 1;
    $grammar->[Marpa::Internal::Grammar::TRACE_RULES]        = 0;
    $grammar->[Marpa::Internal::Grammar::TRACE_VALUES]       = 0;
    $grammar->[Marpa::Internal::Grammar::TRACE_ITERATIONS]   = 0;
    $grammar->[Marpa::Internal::Grammar::TRACING]            = 0;
    $grammar->[Marpa::Internal::Grammar::STRIP]              = 1;
    $grammar->[Marpa::Internal::Grammar::LOCATION_CALLBACK] =
        q{ 'Earleme ' . $earleme };
    $grammar->[Marpa::Internal::Grammar::WARNINGS]        = 1;
    $grammar->[Marpa::Internal::Grammar::INACCESSIBLE_OK] = {};
    $grammar->[Marpa::Internal::Grammar::UNPRODUCTIVE_OK] = {};
    $grammar->[Marpa::Internal::Grammar::CYCLE_ACTION]    = 'warn';
    $grammar->[Marpa::Internal::Grammar::CODE_LINES]      = undef;
    $grammar->[Marpa::Internal::Grammar::PHASE] = Marpa::Internal::Phase::NEW;
    $grammar->[Marpa::Internal::Grammar::SYMBOLS]      = [];
    $grammar->[Marpa::Internal::Grammar::SYMBOL_HASH]  = {};
    $grammar->[Marpa::Internal::Grammar::RULES]        = [];
    $grammar->[Marpa::Internal::Grammar::RULE_HASH]    = {};
    $grammar->[Marpa::Internal::Grammar::QDFA_BY_NAME] = {};
    $grammar->[Marpa::Internal::Grammar::MAX_PARSES]   = -1;

    $grammar->set($args);
    return $grammar;
} ## end sub Marpa::Grammar::new

sub Marpa::show_source_grammar_status {
    my $status =
        $Marpa::Internal::STRINGIFIED_SOURCE_GRAMMAR ? 'Stringified' : 'Raw';
    if ($Marpa::Internal::STRINGIFIED_EVAL_ERROR) {
        $status .= "\nStringified source had error:\n"
            . $Marpa::Internal::STRINGIFIED_EVAL_ERROR;
    }
    return $status;
} ## end sub Marpa::show_source_grammar_status

# For use some day to make locator() more efficient on repeated calls
sub binary_search {
    my ( $target, $data ) = @_;
    my ( $lower, $upper ) = ( 0, $#{$data} );
    while ( $lower <= $upper ) {
        my $i = int +( ( $lower + $upper ) / 2 );
        given ( $data->[$i] ) {
            when ( $_ < $target ) { $lower = $i; }
            when ( $_ > $target ) { $upper = $i; }
            default               { return $i };
        }
    } ## end while ( $lower <= $upper )
    return $lower;
} ## end sub binary_search

sub locator {
    my $earleme = shift;
    my $string  = shift;

    my $lines;
    $lines //= [0];
    my $pos = pos ${$string} = 0;
    NL: while ( ${$string} =~ /\n/gxms ) {
        $pos = pos ${$string};
        push @{$lines}, $pos;
        last NL if $pos > $earleme;
    }
    my $line = ( @{$lines} ) - ( $pos > $earleme ? 2 : 1 );
    my $line_start = $lines->[$line];
    return ( $line, $line_start );
} ## end sub locator

sub Marpa::show_location {
    my $msg     = shift;
    my $source  = shift;
    my $earleme = shift;
    my $result  = q{};

    my ( $line, $line_start ) = locator( $earleme, $source );
    $result .= $msg . ' at line ' . ( $line + 1 ) . ", earleme $earleme\n";
    given ( index ${$source}, "\n", $line_start ) {
        when (undef) {
            $result .= ( substr ${$source}, $line_start ) . "\n";
        }
        default {
            $result .= ( substr ${$source}, $line_start, $_ - $line_start )
                . "\n";
        }
    } ## end given
    $result .= ( q{ } x ( $earleme - $line_start ) ) . "^\n";
    return $result;
} ## end sub Marpa::show_location

sub die_with_parse_failure {
    my $source  = shift;
    my $earleme = shift;

    croak( Marpa::show_location( 'Parse failed', $source, $earleme ) );
} ## end sub die_with_parse_failure

# The following method fails if "use Marpa::Raw_Source" is not
# specified by the user.  This is an undocumented bootstrapping routine,
# not having the "use" in this code saves a few cycles in the normal case.
# Also, forcing the user to be specific about the fact he's doing bootstrapping,
# seems like a good idea in itself.

sub Marpa::stringify_source_grammar {

    # Overwrite the existing stringified source grammar, if we already have one
    # This allows us to bootstrap in a new version

    my $raw_source_grammar = Marpa::Internal::raw_source_grammar();
    my $raw_source_version =
        $raw_source_grammar->[Marpa::Internal::Grammar::VERSION];
    $raw_source_version //= 'not defined';
    if ( $raw_source_version ne $Marpa::VERSION ) {
        croak(
            "raw source grammar version ($raw_source_version) does not match Marpa version (",
            $Marpa::VERSION, ')'
        );
    } ## end if ( $raw_source_version ne $Marpa::VERSION )
    $raw_source_grammar->precompute();
    return $raw_source_grammar->stringify();
} ## end sub Marpa::stringify_source_grammar

# Build a grammar from an MDL description.
# First arg is the grammar being built.
# Second arg is ref to string containing the MDL description.
sub parse_source_grammar {
    my $grammar        = shift;
    my $source         = shift;
    my $source_options = shift;

    my $trace_fh = $grammar->[Marpa::Internal::Grammar::TRACE_FILE_HANDLE];
    my $allow_raw_source =
        $grammar->[Marpa::Internal::Grammar::ALLOW_RAW_SOURCE];
    if ( not defined $Marpa::Internal::STRINGIFIED_SOURCE_GRAMMAR ) {
        if ($allow_raw_source) {
            $Marpa::Internal::STRINGIFIED_SOURCE_GRAMMAR =
                Marpa::stringify_source_grammar();
        }
        else {
            my $eval_error = $Marpa::Internal::STRINGIFIED_EVAL_ERROR
                // 'no eval error';
            croak( "No stringified source grammar:\n", $eval_error );
        }
    } ## end if ( not defined $Marpa::Internal::STRINGIFIED_SOURCE_GRAMMAR)

    $source_options //= {};

    my $recce = new Marpa::Recognizer(
        {   stringified_grammar =>
                $Marpa::Internal::STRINGIFIED_SOURCE_GRAMMAR,
            trace_file_handle => $trace_fh,
            %{$source_options}
        }
    );

    my $failed_at_earleme = $recce->text($source);
    if ( $failed_at_earleme >= 0 ) {
        die_with_parse_failure( $source, $failed_at_earleme );
    }
    $recce->end_input();
    my $evaler = new Marpa::Evaluator( { recce => $recce } );
    croak('Marpa Internal error: failed to create evaluator for MDL')
        unless defined $evaler;
    my $value = $evaler->old_value();
    raw_grammar_eval( $grammar, $value );
    return;
} ## end sub parse_source_grammar

sub Marpa::Grammar::set {
    my ( $grammar, $args ) = @_;
    $args //= {};

    my $tracing = $grammar->[Marpa::Internal::Grammar::TRACING];

    # set trace_fh even if no tracing, because we may turn it on in this method
    my $trace_fh = $grammar->[Marpa::Internal::Grammar::TRACE_FILE_HANDLE];

    my $phase     = $grammar->[Marpa::Internal::Grammar::PHASE];
    my $interface = $grammar->[Marpa::Internal::Grammar::INTERFACE];

    my $precompute = 1;
    if ( exists $args->{precompute} ) {
        $precompute = $args->{precompute};
        delete $args->{precompute};
    }

    # value of source needs to be a *REF* to a string
    my $source = $args->{'mdl_source'};
    if ( defined $source ) {
        croak('Cannot source grammar with some rules already defined')
            if $phase != Marpa::Internal::Phase::NEW;
        croak('Source for grammar must be specified as string ref')
            unless ref $source eq 'SCALAR';
        croak('Source for grammar undefined')
            if not defined ${$source};
        parse_source_grammar( $grammar, $source, $args->{'source_options'} );
        delete $args->{'mdl_source'};
        delete $args->{'source_options'};
        $phase = $grammar->[Marpa::Internal::Grammar::PHASE] =
            Marpa::Internal::Phase::RULES;
    } ## end if ( defined $source )

    while ( my ( $option, $value ) = each %{$args} ) {
        given ($option) {
            when ('rules') {
                $grammar->[Marpa::Internal::Grammar::INTERFACE] //=
                    Marpa::Internal::Interface::RAW;
                $interface = $grammar->[Marpa::Internal::Grammar::INTERFACE];
                croak( 'rules option not allowed with '
                        . interface_description($interface) )
                    if $interface ne Marpa::Internal::Interface::RAW;
                croak(
                    "$option option not allowed after grammar is precomputed")
                    if $phase >= Marpa::Internal::Phase::PRECOMPUTED;
                add_user_rules( $grammar, $value );
                $phase = $grammar->[Marpa::Internal::Grammar::PHASE] =
                    Marpa::Internal::Phase::RULES;
            } ## end when ('rules')
            when ('terminals') {
                $grammar->[Marpa::Internal::Grammar::INTERFACE] //=
                    Marpa::Internal::Interface::RAW;
                $interface = $grammar->[Marpa::Internal::Grammar::INTERFACE];
                croak( 'terminals option not allowed with '
                        . interface_description($interface) )
                    if $interface ne Marpa::Internal::Interface::RAW;
                croak(
                    "$option option not allowed after grammar is precomputed")
                    if $phase >= Marpa::Internal::Phase::PRECOMPUTED;
                add_user_terminals( $grammar, $value );
                $phase = $grammar->[Marpa::Internal::Grammar::PHASE] =
                    Marpa::Internal::Phase::RULES;
            } ## end when ('terminals')
            when ('start') {
                croak(
                    "$option option not allowed after grammar is precomputed")
                    if $phase >= Marpa::Internal::Phase::PRECOMPUTED;
                $grammar->[Marpa::Internal::Grammar::START_NAME] = $value;
            } ## end when ('start')
            when ('academic') {
                croak(
                    "$option option not allowed after grammar is precomputed")
                    if $phase >= Marpa::Internal::Phase::PRECOMPUTED;
                $grammar->[Marpa::Internal::Grammar::ACADEMIC] = $value;
            } ## end when ('academic')
            when ('default_null_value') {
                croak( "$option option not allowed in ",
                    Marpa::Internal::Phase::description($phase) )
                    if $phase >= Marpa::Internal::Phase::RECOGNIZING;
                $grammar->[Marpa::Internal::Grammar::DEFAULT_NULL_VALUE] =
                    $value;
            } ## end when ('default_null_value')
            when ('default_action') {
                croak( "$option option not allowed in ",
                    Marpa::Internal::Phase::description($phase) )
                    if $phase >= Marpa::Internal::Phase::RECOGNIZING;
                $grammar->[Marpa::Internal::Grammar::DEFAULT_ACTION] = $value;
            } ## end when ('default_action')
            when ('default_lex_prefix') {
                croak(
                    "$option option not allowed after grammar is precomputed")
                    if $phase >= Marpa::Internal::Phase::PRECOMPUTED;
                $grammar->[Marpa::Internal::Grammar::DEFAULT_LEX_PREFIX] =
                    $value;
            } ## end when ('default_lex_prefix')
            when ('default_lex_suffix') {
                croak(
                    "$option option not allowed after grammar is precomputed")
                    if $phase >= Marpa::Internal::Phase::PRECOMPUTED;
                $grammar->[Marpa::Internal::Grammar::DEFAULT_LEX_SUFFIX] =
                    $value;
            } ## end when ('default_lex_suffix')
            when ('ambiguous_lex') {
                croak(
                    "$option option not allowed after grammar is precomputed")
                    if $phase >= Marpa::Internal::Phase::PRECOMPUTED;
                $grammar->[Marpa::Internal::Grammar::AMBIGUOUS_LEX] = $value;
            } ## end when ('ambiguous_lex')
            when ('strip') {
                croak( "$option option not allowed in ",
                    Marpa::Internal::Phase::description($phase) )
                    if $phase >= Marpa::Internal::Phase::EVALUATING;
                $grammar->[Marpa::Internal::Grammar::STRIP] = $value;
            } ## end when ('strip')
            when ('trace_file_handle') {
                $grammar->[Marpa::Internal::Grammar::TRACE_FILE_HANDLE] =
                    $value;
            }
            when ('trace_actions') {
                $grammar->[Marpa::Internal::Grammar::TRACE_ACTIONS] = $value;
                if ($value) {
                    say {$trace_fh} "Setting $option option";
                    say {$trace_fh}
                        "Warning: setting $option option after semantics were finalized"
                        if $phase >= Marpa::Internal::Phase::RECOGNIZING;
                    $grammar->[Marpa::Internal::Grammar::TRACING] = 1;
                } ## end if ($value)
            } ## end when ('trace_actions')
            when ('trace_lex') {
                $grammar->[Marpa::Internal::Grammar::TRACE_LEX_TRIES] =
                    $grammar->[Marpa::Internal::Grammar::TRACE_LEX_MATCHES] =
                    $value;
                if ($value) {
                    say {$trace_fh} "Setting $option option";
                    $grammar->[Marpa::Internal::Grammar::TRACING] = 1;
                }
            } ## end when ('trace_lex')
            when ('trace_lex_tries') {
                $grammar->[Marpa::Internal::Grammar::TRACE_LEX_TRIES] =
                    $value;
                if ($value) {
                    say {$trace_fh} "Setting $option option";
                    $grammar->[Marpa::Internal::Grammar::TRACING] = 1;
                }
            } ## end when ('trace_lex_tries')
            when ('trace_lex_matches') {
                $grammar->[Marpa::Internal::Grammar::TRACE_LEX_MATCHES] =
                    $value;
                if ($value) {
                    say {$trace_fh} "Setting $option option";
                    $grammar->[Marpa::Internal::Grammar::TRACING] = 1;
                }
            } ## end when ('trace_lex_matches')
            when ('trace_values') {
                croak('trace_values must be set to a number >= 0')
                    unless $value =~ /\A\d+\z/xms;
                $grammar->[Marpa::Internal::Grammar::TRACE_VALUES] =
                    $value + 0;
                if ($value) {
                    say {$trace_fh} "Setting $option option to $value";
                    $grammar->[Marpa::Internal::Grammar::TRACING] = 1;
                }
            } ## end when ('trace_values')
            when ('trace_rules') {
                $grammar->[Marpa::Internal::Grammar::TRACE_RULES] = $value;
                if ($value) {
                    my $rules = $grammar->[Marpa::Internal::Grammar::RULES];
                    my $rule_count = @{$rules};
                    say {$trace_fh} "Setting $option";
                    say {$trace_fh}
                        "Warning: Setting $option when $rule_count rules already exist"
                        if $rule_count;
                    $grammar->[Marpa::Internal::Grammar::TRACING] = 1;
                } ## end if ($value)
            } ## end when ('trace_rules')
            when ('trace_strings') {
                $grammar->[Marpa::Internal::Grammar::TRACE_STRINGS] = $value;
                if ($value) {
                    my $rules = $grammar->[Marpa::Internal::Grammar::RULES];
                    my $rule_count = @{$rules};
                    say {$trace_fh} "Setting $option";
                    say {$trace_fh}
                        "Warning: Setting $option after $rule_count rules have been defined"
                        if $rule_count;
                    $grammar->[Marpa::Internal::Grammar::TRACING] = 1;
                } ## end if ($value)
            } ## end when ('trace_strings')
            when ('trace_predefineds') {
                $grammar->[Marpa::Internal::Grammar::TRACE_PREDEFINEDS] =
                    $value;
                if ($value) {
                    my $rules = $grammar->[Marpa::Internal::Grammar::RULES];
                    my $rule_count = @{$rules};
                    say {$trace_fh} "Setting $option";
                    say {$trace_fh}
                        "Warning: Setting $option after $rule_count rules have been defined"
                        if $rule_count;
                    $grammar->[Marpa::Internal::Grammar::TRACING] = 1;
                } ## end if ($value)
            } ## end when ('trace_predefineds')
            when ('trace_iterations') {
                croak('trace_iterations must be set to a number >= 0')
                    unless $value =~ /\A\d+\z/xms;
                $grammar->[Marpa::Internal::Grammar::TRACE_ITERATIONS] =
                    $value + 0;
                if ($value) {
                    say {$trace_fh} "Setting $option option to $value";
                    $grammar->[Marpa::Internal::Grammar::TRACING] = 1;
                }
            } ## end when ('trace_iterations')
            when ('trace_priorities') {
                $grammar->[Marpa::Internal::Grammar::TRACE_PRIORITIES] =
                    $value;
                if ($value) {
                    say {$trace_fh} "Setting $option";
                    say {$trace_fh}
                        "Warning: Setting $option after semantics were finalized"
                        if $phase >= Marpa::Internal::Phase::PRECOMPUTED;
                    $grammar->[Marpa::Internal::Grammar::TRACING] = 1;
                } ## end if ($value)
            } ## end when ('trace_priorities')
            when ('trace_completions') {
                $grammar->[Marpa::Internal::Grammar::TRACE_COMPLETIONS] =
                    $value;
                if ($value) {
                    say {$trace_fh} "Setting $option option";
                    $grammar->[Marpa::Internal::Grammar::TRACING] = 1;
                }
            } ## end when ('trace_completions')
            when ('location_callback') {
                croak('location callback not yet implemented');
            }
            when ('opaque') {
                croak('the opaque option has been removed');
            }
            when ('cycle_action') {
                #<<< perltidy gets confused
                if ( $value && $phase >= Marpa::Internal::Phase::PRECOMPUTED )
                {
                    say {$trace_fh}
                        '"cycle_action" option is useless after grammar is precomputed';
                }
                #>>>
                croak("$option must be 'warn', 'quiet' or 'fatal'")
                    unless $value eq 'warn'
                        || $value eq 'quiet'
                        || $value eq 'fatal';
                $grammar->[Marpa::Internal::Grammar::CYCLE_ACTION] = $value;
            } ## end when ('cycle_action')
            when ('cycle_depth') {
                croak('cycle_depth option no longer implemented');
            }
            when ('warnings') {
                #<<< perltidy gets confused
                if ( $value && $phase >= Marpa::Internal::Phase::PRECOMPUTED )
                {
                    say {
                        $trace_fh
                    }
                    q{"warnings" option is useless after grammar is precomputed};
                }
                #>>>
                $grammar->[Marpa::Internal::Grammar::WARNINGS] = $value;
            } ## end when ('warnings')
            when ('inaccessible_ok') {
                #<<< perltidy gets confused
                if ( $value && $phase >= Marpa::Internal::Phase::PRECOMPUTED )
                {
                    say {
                        $trace_fh
                    }
                    q{"inaccessible_ok" option is useless after grammar is precomputed};
                }
                #>>>
                croak('value of inaccessible_ok option must be an array ref')
                    unless ref $value eq 'ARRAY';
                $grammar->[Marpa::Internal::Grammar::INACCESSIBLE_OK] =
                    { map { ( $_, 1 ) } @{$value} };
            } ## end when ('inaccessible_ok')
            when ('unproductive_ok') {
                #<<< perltidy gets confused
                if ( $value && $phase >= Marpa::Internal::Phase::PRECOMPUTED )
                {
                    say {
                        $trace_fh
                    }
                    q{"unproductive_ok" option is useless after grammar is precomputed};
                }
                #>>>
                croak('value of unproductive_ok option must be an array ref')
                    unless ref $value eq 'ARRAY';
                $grammar->[Marpa::Internal::Grammar::UNPRODUCTIVE_OK] =
                    { map { ( $_, 1 ) } @{$value} };
            } ## end when ('unproductive_ok')
            when ('code_lines') {
                $grammar->[Marpa::Internal::Grammar::CODE_LINES] = $value;
            }
            when ('allow_raw_source') {
                croak( "$option option not allowed in ",
                    Marpa::Internal::Phase::description($phase) )
                    if $phase >= Marpa::Internal::Phase::RULES;
                $grammar->[Marpa::Internal::Grammar::ALLOW_RAW_SOURCE] =
                    $value;
            } ## end when ('allow_raw_source')
            when ('max_parses') {
                $grammar->[Marpa::Internal::Grammar::MAX_PARSES] = $value;
            }
            when ('version') {
                croak(
                    "$option option not allowed after grammar is precomputed")
                    if $phase >= Marpa::Internal::Phase::PRECOMPUTED;
                $grammar->[Marpa::Internal::Grammar::VERSION] = $value;
            } ## end when ('version')
            when ('semantics') {
                croak(
                    "$option option not allowed after grammar is precomputed")
                    if $phase >= Marpa::Internal::Phase::PRECOMPUTED;
                $grammar->[Marpa::Internal::Grammar::SEMANTICS] = $value;
            } ## end when ('semantics')
            when ('lex_preamble') {
                croak( "$option option not allowed in ",
                    Marpa::Internal::Phase::description($phase) )
                    if $phase >= Marpa::Internal::Phase::RECOGNIZING;
                $grammar->[Marpa::Internal::Grammar::LEX_PREAMBLE] = $value;
            } ## end when ('lex_preamble')
            when ('preamble') {
                croak( "$option option not allowed in ",
                    Marpa::Internal::Phase::description($phase) )
                    if $phase >= Marpa::Internal::Phase::RECOGNIZING;
                $grammar->[Marpa::Internal::Grammar::PREAMBLE] = $value;
            } ## end when ('preamble')
            default {
                croak("$_ is not an available Marpa option");
            }
        } ## end given
    } ## end while ( my ( $option, $value ) = each %{$args} )

    if ( $precompute and $phase == Marpa::Internal::Phase::RULES ) {
        $grammar->precompute();
    }

    return 1;
} ## end sub Marpa::Grammar::set

=begin Implementation:

In order to automatically ELIMINATE inaccessible and unproductive
productions from a grammar, you have to first eliminate the
unproductive productions, THEN the inaccessible ones.  I don't do
this in the below.

The reason is my purposes are primarily diagnostic.  The difference
shows in the case of an unproductive start symbol.  Following the
correct procedure for automatically cleaning the grammar, I would
have to regard the start symbol and its productions as eliminated
and therefore go on to report every other production and symbol as
inaccessible.  Almost certainly all these inaccessiblity reports,
while theoretically correct, are irrelevant, since the user will
probably respond by making the start symbol productive, and the
extra "information" would only get in the way.

The downside is that in a few uncommon cases, a user relying entirely
on the Marpa warnings to clean up his grammar will have to go through
more than a single pass of the diagnostics.  I think even those
users will prefer less cluttered diagnostics, and I'm sure most
users will.

=end Implementation:

=cut

sub Marpa::Grammar::precompute {
    my $grammar = shift;

    my $tracing = $grammar->[Marpa::Internal::Grammar::TRACING];
    my $trace_fh;
    if ($tracing) {
        $trace_fh = $grammar->[Marpa::Internal::Grammar::TRACE_FILE_HANDLE];
    }

    my $problems = $grammar->[Marpa::Internal::Grammar::PROBLEMS];
    if ($problems) {
        croak(
            Marpa::Grammar::show_problems($grammar),
            "Second attempt to precompute grammar with fatal problems\n",
            'Marpa cannot proceed'
        );
    } ## end if ($problems)

    my $phase = $grammar->[Marpa::Internal::Grammar::PHASE];

    # Be idempotent.  If the grammar is already precomputed, just
    # return success without doing anything.
    if ( $phase == Marpa::Internal::Phase::PRECOMPUTED ) {
        return $grammar;
    }

    if ( $phase != Marpa::Internal::Phase::RULES ) {
        croak(
            "Attempt to precompute grammar in inappropriate state\nAttempt to precompute ",
            Marpa::Internal::Phase::description($phase)
        );
    } ## end if ( $phase != Marpa::Internal::Phase::RULES )

    if ( not terminals_distinguished($grammar) ) {
        mark_all_symbols_terminal($grammar);
    }
    else {
        nulling($grammar);
        nullable($grammar) or return $grammar;
        productive($grammar);
    }

    check_start($grammar) or return $grammar;

    accessible($grammar);
    if ( $grammar->[Marpa::Internal::Grammar::ACADEMIC] ) {
        setup_academic_grammar($grammar);
    }
    else {
        rewrite_as_CHAF($grammar);
        detect_cycle($grammar);
    }
    create_NFA($grammar);
    create_QDFA($grammar);
    if ( $grammar->[Marpa::Internal::Grammar::WARNINGS] ) {
        $trace_fh //= $grammar->[Marpa::Internal::Grammar::TRACE_FILE_HANDLE];
        my $ok = $grammar->[Marpa::Internal::Grammar::INACCESSIBLE_OK];
        SYMBOL:
        for my $symbol ( @{ Marpa::Grammar::inaccessible_symbols($grammar) } )
        {
            next SYMBOL if $ok->{$symbol};
            say {$trace_fh} "Inaccessible symbol: $symbol";
        }
        $ok = $grammar->[Marpa::Internal::Grammar::UNPRODUCTIVE_OK];
        SYMBOL:
        for my $symbol ( @{ Marpa::Grammar::unproductive_symbols($grammar) } )
        {
            next SYMBOL if $ok->{$symbol};
            say {$trace_fh} "Unproductive symbol: $symbol";
        }
    } ## end if ( $grammar->[Marpa::Internal::Grammar::WARNINGS] )

    $grammar->[Marpa::Internal::Grammar::PHASE] =
        Marpa::Internal::Phase::PRECOMPUTED;

    if ( $grammar->[Marpa::Internal::Grammar::STRIP] ) {

        $#{$grammar} = Marpa::Internal::Grammar::LAST_RECOGNIZER_FIELD;

        for my $symbol ( @{ $grammar->[Marpa::Internal::Grammar::SYMBOLS] } )
        {
            $#{$symbol} = Marpa::Internal::Symbol::LAST_RECOGNIZER_FIELD;
        }

        for my $rule ( @{ $grammar->[Marpa::Internal::Grammar::RULES] } ) {
            $#{$rule} = Marpa::Internal::Rule::LAST_RECOGNIZER_FIELD;
        }

        for my $QDFA ( @{ $grammar->[Marpa::Internal::Grammar::QDFA] } ) {
            $#{$QDFA} = Marpa::Internal::QDFA::LAST_RECOGNIZER_FIELD;
        }

    } ## end if ( $grammar->[Marpa::Internal::Grammar::STRIP] )

    return $grammar;

} ## end sub Marpa::Grammar::precompute

sub Marpa::Grammar::show_problems {
    my $grammar = shift;

    my $problems = $grammar->[Marpa::Internal::Grammar::PROBLEMS];
    if ($problems) {
        my $problem_count = scalar @{$problems};
        return
            "Grammar has $problem_count problems:\n"
            . ( join "\n", @{$problems} ) . "\n";
    } ## end if ($problems)
    return "Grammar has no problems\n";
} ## end sub Marpa::Grammar::show_problems

# Convert Grammar into string form
#
# Note: copying strengthens weak refs
#
sub Marpa::Grammar::stringify {
    my $grammar = shift;

    my $tracing = $grammar->[Marpa::Internal::Grammar::TRACING];
    my $trace_fh;
    if ($tracing) {
        $trace_fh = $grammar->[Marpa::Internal::Grammar::TRACE_FILE_HANDLE];
    }

    my $phase = $grammar->[Marpa::Internal::Grammar::PHASE];
    if ( $phase != Marpa::Internal::Phase::PRECOMPUTED ) {
        croak(
            "Attempt to stringify grammar in inappropriate state\nAttempt to stringify ",
            Marpa::Internal::Phase::description($phase)
        );
    } ## end if ( $phase != Marpa::Internal::Phase::PRECOMPUTED )

    my $problems = $grammar->[Marpa::Internal::Grammar::PROBLEMS];
    if ($problems) {
        croak(
            Marpa::Grammar::show_problems($grammar),
            "Attempt to stringify grammar with fatal problems\n",
            'Marpa cannot proceed'
        );
    } ## end if ($problems)

    my $d = Data::Dumper->new( [$grammar], ['grammar'] );
    $d->Purity(1);
    $d->Indent(0);

    # returns a ref -- dumps can be long
    return \( $d->Dump() );
} ## end sub Marpa::Grammar::stringify

# First arg is stringified grammar
# Second arg (optional) is trace file handle, either saved and restored
# If not trace file handle supplied, it reverts to the default, STDERR
#
# Returns the unstringified grammar
sub Marpa::Grammar::unstringify {
    my $stringified_grammar = shift;
    my $trace_fh            = shift;
    $trace_fh //= *STDERR;

    croak('Attempt to unstringify undefined grammar')
        unless defined $stringified_grammar;
    croak('Arg to unstringify must be ref to SCALAR')
        if ref $stringified_grammar ne 'SCALAR';

    my $grammar;
    {
        my $old_warn_handler = $SIG{__WARN__};
        my @warnings;
        $SIG{__WARN__} = sub { push @warnings, [ $_[0], ( caller 0 ) ]; };

        ## no critic (BuiltinFunctions::ProhibitStringyEval,TestingAndDebugging::ProhibitNoStrict)
        no strict 'refs';
        my $eval_ok = eval ${$stringified_grammar};
        use strict 'refs';
        ## use critic

        $SIG{__WARN__} = $old_warn_handler;

        if ( not $eval_ok or @warnings ) {
            my $fatal_error = $EVAL_ERROR;
            Marpa::Internal::code_problems(
                {   eval_ok     => $eval_ok,
                    fatal_error => $fatal_error,
                    warnings    => \@warnings,
                    where       => 'unstringifying grammar',
                    code        => $stringified_grammar,
                }
            );
        } ## end if ( not $eval_ok or @warnings )
    }

    $grammar->[Marpa::Internal::Grammar::TRACE_FILE_HANDLE] = $trace_fh;

    # Eliminate or weaken all circular references
    my $symbol_hash = $grammar->[Marpa::Internal::Grammar::SYMBOL_HASH];
    while ( my ( $name, $ref ) = each %{$symbol_hash} ) {
        weaken( $symbol_hash->{$name} = $ref );
    }

    # these were weak references, but aren't used anyway, so
    # free up the memory
    for my $symbol ( @{ $grammar->[Marpa::Internal::Grammar::SYMBOLS] } ) {
        $symbol->[Marpa::Internal::Symbol::LHS] = undef;
        $symbol->[Marpa::Internal::Symbol::RHS] = undef;
    }

    return $grammar;

} ## end sub Marpa::Grammar::unstringify

sub Marpa::Grammar::clone {
    my $grammar  = shift;
    my $trace_fh = shift;

    my $stringified_grammar = Marpa::Grammar::stringify($grammar);
    $trace_fh //= $grammar->[Marpa::Internal::Grammar::TRACE_FILE_HANDLE];
    return Marpa::Grammar::unstringify( $stringified_grammar, $trace_fh );
} ## end sub Marpa::Grammar::clone

sub Marpa::show_symbol {
    my $symbol   = shift;
    my $text     = q{};
    my $stripped = $#{$symbol} < Marpa::Internal::Symbol::LAST_FIELD;

    $text .= sprintf '%d: %s,',
        $symbol->[Marpa::Internal::Symbol::ID],
        $symbol->[Marpa::Internal::Symbol::NAME];

    if ( exists $symbol->[Marpa::Internal::Symbol::LHS] ) {
        $text .= sprintf ' lhs=[%s]',
            (
            join q{ },
            map { $_->[Marpa::Internal::Rule::ID] }
                @{ $symbol->[Marpa::Internal::Symbol::LHS] }
            );
    } ## end if ( exists $symbol->[Marpa::Internal::Symbol::LHS] )

    if ( exists $symbol->[Marpa::Internal::Symbol::RHS] ) {
        $text .= sprintf ' rhs=[%s]',
            (
            join q{ },
            map { $_->[Marpa::Internal::Rule::ID] }
                @{ $symbol->[Marpa::Internal::Symbol::RHS] }
            );
    } ## end if ( exists $symbol->[Marpa::Internal::Symbol::RHS] )

    $text .= ' stripped' if $stripped;

    ELEMENT:
    for my $comment_element (
        (   [ 1, 'unproductive', Marpa::Internal::Symbol::PRODUCTIVE, ],
            [ 1, 'inaccessible', Marpa::Internal::Symbol::ACCESSIBLE, ],
            [ 0, 'nullable',     Marpa::Internal::Symbol::NULLABLE, ],
            [ 0, 'nulling',      Marpa::Internal::Symbol::NULLING, ],
            [ 0, 'terminal',     Marpa::Internal::Symbol::TERMINAL, ],
        )
        )
    {
        my ( $reverse, $comment, $offset ) = @{$comment_element};
        next ELEMENT unless exists $symbol->[$offset];
        my $value = $symbol->[$offset];
        $value = !$value if $reverse;
        $text .= " $comment" if $value;
    } ## end for my $comment_element ( ( [ 1, 'unproductive', ...

    return $text .= "\n";

} ## end sub Marpa::show_symbol

sub Marpa::Grammar::show_symbols {
    my $grammar = shift;
    my $symbols = $grammar->[Marpa::Internal::Grammar::SYMBOLS];
    my $text    = q{};
    for my $symbol_ref ( @{$symbols} ) {
        $text .= Marpa::show_symbol($symbol_ref);
    }
    return $text;
} ## end sub Marpa::Grammar::show_symbols

sub Marpa::Grammar::show_nulling_symbols {
    my $grammar = shift;
    my $symbols = $grammar->[Marpa::Internal::Grammar::SYMBOLS];
    return 'stripped_'
        if scalar grep { $#{$_} < Marpa::Internal::Symbol::LAST_FIELD }
            @{$symbols};
    return join q{ }, sort map { $_->[Marpa::Internal::Symbol::NAME] }
        grep { $_->[Marpa::Internal::Symbol::NULLING] } @{$symbols};
} ## end sub Marpa::Grammar::show_nulling_symbols

sub Marpa::Grammar::show_nullable_symbols {
    my $grammar = shift;
    return 'stripped_'
        unless exists $grammar->[Marpa::Internal::Grammar::NULLABLE_SYMBOL];
    my $symbols = $grammar->[Marpa::Internal::Grammar::NULLABLE_SYMBOL];
    return join q{ },
        sort map { $_->[Marpa::Internal::Symbol::NAME] } @{$symbols};
} ## end sub Marpa::Grammar::show_nullable_symbols

sub Marpa::Grammar::show_productive_symbols {
    my $grammar = shift;
    my $symbols = $grammar->[Marpa::Internal::Grammar::SYMBOLS];
    return 'stripped_'
        if scalar grep { $#{$_} < Marpa::Internal::Symbol::LAST_FIELD }
            @{$symbols};
    return join q{ }, sort map { $_->[Marpa::Internal::Symbol::NAME] }
        grep { $_->[Marpa::Internal::Symbol::PRODUCTIVE] } @{$symbols};
} ## end sub Marpa::Grammar::show_productive_symbols

sub Marpa::Grammar::show_accessible_symbols {
    my $grammar = shift;
    my $symbols = $grammar->[Marpa::Internal::Grammar::SYMBOLS];
    return 'stripped_'
        if scalar grep { $#{$_} < Marpa::Internal::Symbol::LAST_FIELD }
            @{$symbols};
    return join q{ }, sort map { $_->[Marpa::Internal::Symbol::NAME] }
        grep { $_->[Marpa::Internal::Symbol::ACCESSIBLE] } @{$symbols};
} ## end sub Marpa::Grammar::show_accessible_symbols

sub Marpa::Grammar::inaccessible_symbols {
    my $grammar = shift;
    my $symbols = $grammar->[Marpa::Internal::Grammar::SYMBOLS];
    return [
        sort map { $_->[Marpa::Internal::Symbol::NAME] }
            grep { !$_->[Marpa::Internal::Symbol::ACCESSIBLE] } @{$symbols}
    ];
} ## end sub Marpa::Grammar::inaccessible_symbols

sub Marpa::Grammar::unproductive_symbols {
    my $grammar = shift;
    my $symbols = $grammar->[Marpa::Internal::Grammar::SYMBOLS];
    return [
        sort map { $_->[Marpa::Internal::Symbol::NAME] }
            grep { !$_->[Marpa::Internal::Symbol::PRODUCTIVE] } @{$symbols}
    ];
} ## end sub Marpa::Grammar::unproductive_symbols

sub Marpa::brief_rule {
    my $rule = shift;
    my ( $lhs, $rhs, $rule_id ) = @{$rule}[
        Marpa::Internal::Rule::LHS, Marpa::Internal::Rule::RHS,
        Marpa::Internal::Rule::ID
    ];
    my $text
        .= $rule_id . ': ' . $lhs->[Marpa::Internal::Symbol::NAME] . ' ->';
    if ( @{$rhs} ) {
        $text
            .= q{ }
            . ( join q{ },
            map { $_->[Marpa::Internal::Symbol::NAME] } @{$rhs} );
    } ## end if ( @{$rhs} )
    return $text;
} ## end sub Marpa::brief_rule

sub Marpa::brief_original_rule {
    my $rule          = shift;
    my $original_rule = $rule->[Marpa::Internal::Rule::ORIGINAL_RULE]
        // $rule;
    return Marpa::brief_rule($original_rule);
} ## end sub Marpa::brief_original_rule

sub Marpa::show_rule {
    my $rule = shift;

    my $stripped = $#{$rule} < Marpa::Internal::Rule::LAST_FIELD;
    my ( $rhs, $useful ) =
        @{$rule}[ Marpa::Internal::Rule::RHS, Marpa::Internal::Rule::USEFUL,
        ];
    my @comment = ();

    if ( not $useful )    { push @comment, '!useful'; }
    if ( not( @{$rhs} ) ) { push @comment, 'empty'; }

    if ($stripped) { push @comment, 'stripped'; }

    ELEMENT:
    for my $comment_element (
        (   [ 1, 'unproductive', Marpa::Internal::Rule::PRODUCTIVE, ],
            [ 1, 'inaccessible', Marpa::Internal::Rule::ACCESSIBLE, ],
            [ 0, 'nullable',     Marpa::Internal::Rule::NULLABLE, ],
            [ 0, 'nulling',      Marpa::Internal::Rule::NULLING, ],
            [ 0, 'hasty',        Marpa::Internal::Rule::HASTY, ],
        )
        )
    {
        my ( $reverse, $comment, $offset ) = @{$comment_element};
        next ELEMENT unless exists $rule->[$offset];
        my $value = $rule->[$offset];
        $value = !$value if $reverse;
        next ELEMENT unless $value;
        push @comment, $comment;
    } ## end for my $comment_element ( ( [ 1, 'unproductive', ...

    if ( exists $rule->[Marpa::Internal::Rule::PRIORITY] ) {

        my $priority            = $rule->[Marpa::Internal::Rule::PRIORITY];
        my $priority_string_ref = Marpa::show_priority($priority);
        if ($priority_string_ref) {
            push @comment, 'priority=' . ${$priority_string_ref};
        }

    } ## end if ( exists $rule->[Marpa::Internal::Rule::PRIORITY])

    my $text = Marpa::brief_rule($rule);

    if (@comment) {
        $text .= q{ } . ( join q{ }, q{/*}, @comment, q{*/} );
    }

    return $text .= "\n";

}    # sub show_rule

# For displaying priorities.
# Returns undefined if priority undefined or zero.
# Returns ref to a string showing dotted priority otherwise.
# A true second arg, means create a string even if priority is zero.
sub Marpa::show_priority {
    my $priority = shift;
    return unless defined $priority;
    my $defined_if_zero = shift;
    my ( $pri1, $pri2 ) = unpack 'NN', $priority;
    return unless $defined_if_zero or $pri1 or $pri2;
    return \( $pri1 . q{.} . $pri2 );
} ## end sub Marpa::show_priority

sub Marpa::Grammar::show_rules {
    my $grammar = shift;
    my $rules   = $grammar->[Marpa::Internal::Grammar::RULES];
    my $text;

    for my $rule ( @{$rules} ) {
        $text .= Marpa::show_rule($rule);
    }
    return $text;
} ## end sub Marpa::Grammar::show_rules

sub Marpa::show_dotted_rule {
    my $rule     = shift;
    my $position = shift;

    my @names =
        map { $_->[Marpa::Internal::Symbol::NAME] }
        $rule->[Marpa::Internal::Rule::LHS],
        @{ $rule->[Marpa::Internal::Rule::RHS] };
    splice @names, $position + 1, 0, q{.};
    splice @names, 1, 0, '::=';
    return join q{ }, @names;
} ## end sub Marpa::show_dotted_rule

sub Marpa::show_item {
    my $item = shift;
    my $text = q{};
    if ( not defined $item ) {
        $text .= '/* empty */';
    }
    else {
        $text .= Marpa::show_dotted_rule(
            @{$item}[
                Marpa::Internal::LR0_item::RULE,
            Marpa::Internal::LR0_item::POSITION
            ]
        );
    } ## end else [ if ( not defined $item )
    return $text;
} ## end sub Marpa::show_item

sub Marpa::show_NFA_state {
    my $state = shift;
    my ( $name, $item, $transition, $at_nulling, $priority ) = @{$state}[
        Marpa::Internal::NFA::NAME,       Marpa::Internal::NFA::ITEM,
        Marpa::Internal::NFA::TRANSITION, Marpa::Internal::NFA::AT_NULLING,
        Marpa::Internal::NFA::PRIORITY,
    ];
    my $text = $name . ': ';
    $text .= Marpa::show_item($item) . "\n";
    my @properties = ();
    push @properties, 'at_nulling' if $at_nulling;
    my $priority_string_ref = Marpa::show_priority($priority);
    push @properties, 'priority=' . ${$priority_string_ref}
        if defined $priority_string_ref;
    $text .= join( q{ }, @properties ) . "\n" if @properties;

    for my $symbol_name ( sort keys %{$transition} ) {
        my $transition_states = $transition->{$symbol_name};
        $text
            .= q{ }
            . ( $symbol_name eq q{} ? 'empty' : '<' . $symbol_name . '>' )
            . ' => '
            . join( q{ },
            map { $_->[Marpa::Internal::NFA::NAME] } @{$transition_states} )
            . "\n";
    } ## end for my $symbol_name ( sort keys %{$transition} )
    return $text;
} ## end sub Marpa::show_NFA_state

sub Marpa::Grammar::show_NFA {
    my $grammar = shift;
    my $text    = q{};

    return "stripped\n"
        unless exists $grammar->[Marpa::Internal::Grammar::NFA];

    my $NFA = $grammar->[Marpa::Internal::Grammar::NFA];
    for my $state ( @{$NFA} ) {
        $text .= Marpa::show_NFA_state($state);
    }

    return $text;
} ## end sub Marpa::Grammar::show_NFA

sub Marpa::brief_QDFA_state {
    my $state = shift;
    my $tags  = shift;
    return 'St' . $state->[Marpa::Internal::QDFA::TAG]
        if defined $tags;
    return 'S' . $state->[Marpa::Internal::QDFA::ID];
} ## end sub Marpa::brief_QDFA_state

sub Marpa::show_QDFA_state {
    my $state = shift;
    my $tags  = shift;

    my $text     = q{};
    my $stripped = $#{$state} < Marpa::Internal::QDFA::LAST_FIELD;

    $text .= Marpa::brief_QDFA_state( $state, $tags ) . ': ';

    if ( exists $state->[Marpa::Internal::QDFA::RESET_ORIGIN] ) {
        $text .= 'predict; ' if $state->[Marpa::Internal::QDFA::RESET_ORIGIN];
    }

    if ( exists $state->[Marpa::Internal::QDFA::PRIORITY] ) {
        my $priority            = $state->[Marpa::Internal::QDFA::PRIORITY];
        my $priority_string_ref = Marpa::show_priority($priority);
        $text .= 'pri=' . ${$priority_string_ref} . '; '
            if defined $priority_string_ref;
    } ## end if ( exists $state->[Marpa::Internal::QDFA::PRIORITY...

    $text .= $state->[Marpa::Internal::QDFA::NAME] . "\n";

    if ( exists $state->[Marpa::Internal::QDFA::NFA_STATES] ) {
        my $NFA_states = $state->[Marpa::Internal::QDFA::NFA_STATES];
        for my $NFA_state ( @{$NFA_states} ) {
            my $item = $NFA_state->[Marpa::Internal::NFA::ITEM];
            $text .= Marpa::show_item($item) . "\n";
        }
    } ## end if ( exists $state->[Marpa::Internal::QDFA::NFA_STATES...

    $text .= "stripped\n" if $stripped;

    if ( exists $state->[Marpa::Internal::QDFA::TRANSITION] ) {
        my $transition = $state->[Marpa::Internal::QDFA::TRANSITION];
        for my $symbol_name ( sort keys %{$transition} ) {
            $text .= ' <' . $symbol_name . '> => ';
            my @qdfa_labels;
            for my $to_state ( @{ $transition->{$symbol_name} } ) {
                my $to_name = $to_state->[Marpa::Internal::QDFA::NAME];
                push @qdfa_labels,
                    Marpa::brief_QDFA_state( $to_state, $tags );
            }    # for my $to_state
            $text .= join '; ', sort @qdfa_labels;
            $text .= "\n";
        } ## end for my $symbol_name ( sort keys %{$transition} )
    } ## end if ( exists $state->[Marpa::Internal::QDFA::TRANSITION...

    return $text;
} ## end sub Marpa::show_QDFA_state

sub tag_QDFA {
    my $grammar = shift;
    my $QDFA    = $grammar->[Marpa::Internal::Grammar::QDFA];
    return if defined $QDFA->[0]->[Marpa::Internal::QDFA::TAG];
    my $tag = 0;
    for my $state (
        sort {
            $a->[Marpa::Internal::QDFA::NAME]
                cmp $b->[Marpa::Internal::QDFA::NAME]
        } @{$QDFA}
        )
    {
        $state->[Marpa::Internal::QDFA::TAG] = $tag++;
    } ## end for my $state ( sort { $a->[Marpa::Internal::QDFA::NAME...
    return;
} ## end sub tag_QDFA

sub Marpa::Grammar::show_QDFA {
    my $grammar = shift;
    my $tags    = shift;

    my $text         = q{};
    my $QDFA         = $grammar->[Marpa::Internal::Grammar::QDFA];
    my $start_states = $grammar->[Marpa::Internal::Grammar::START_STATES];
    $text .= 'Start States: ';
    $text .= join '; ',
        sort map { Marpa::brief_QDFA_state( $_, $tags ) } @{$start_states};
    $text .= "\n";

    for my $state ( @{$QDFA} ) {
        $text .= Marpa::show_QDFA_state( $state, $tags );
    }
    return $text;
} ## end sub Marpa::Grammar::show_QDFA

sub Marpa::Grammar::show_ii_QDFA {
    my $grammar = shift;
    my $text    = q{};
    my $QDFA    = $grammar->[Marpa::Internal::Grammar::QDFA];
    my $tags;
    tag_QDFA($grammar);

    for my $state ( @{$QDFA} ) {
        $tags->[ $state->[Marpa::Internal::QDFA::ID] ] =
            $state->[Marpa::Internal::QDFA::TAG];
    }
    my $start_states = $grammar->[Marpa::Internal::Grammar::START_STATES];
    $text .= 'Start States: ';
    $text .= join '; ',
        sort map { Marpa::brief_QDFA_state( $_, $tags ) } @{$start_states};
    $text .= "\n";
    for my $state (
        map  { $_->[0] }
        sort { $a->[1] <=> $b->[1] }
        map  { [ $_, $_->[Marpa::Internal::QDFA::TAG] ] } @{$QDFA}
        )
    {
        $text .= Marpa::show_QDFA_state( $state, $tags );
    } ## end for my $state ( map { $_->[0] } sort { $a->[1] <=> $b...
    return $text;
} ## end sub Marpa::Grammar::show_ii_QDFA

sub Marpa::Grammar::get_symbol {
    my $grammar     = shift;
    my $name        = shift;
    my $symbol_hash = $grammar->[Marpa::Internal::Grammar::SYMBOL_HASH];
    return defined $symbol_hash ? $symbol_hash->{$name} : undef;
} ## end sub Marpa::Grammar::get_symbol

sub add_terminal {
    my $grammar = shift;
    my $name    = shift;
    my $options = shift;
    my ( $regex, $prefix, $suffix );
    my $action;
    my $user_priority = 0;

    while ( my ( $key, $value ) = each %{$options} ) {
        given ($key) {
            when ('priority') { $user_priority = $value; }
            when ('action')   { $action        = $value; }
            when ('prefix')   { $prefix        = $value; }
            when ('suffix')   { $suffix        = $value; }
            when ('regex')    { $regex         = $value; }
            default {
                croak(
                    "Attempt to add terminal named $name with unknown option $key"
                );
            }
        } ## end given
    } ## end while ( my ( $key, $value ) = each %{$options} )

    my ( $symbol_hash, $symbols, $default_null_value ) = @{$grammar}[
        Marpa::Internal::Grammar::SYMBOL_HASH,
        Marpa::Internal::Grammar::SYMBOLS,
        Marpa::Internal::Grammar::DEFAULT_NULL_VALUE,
    ];

    # I allow redefinition of a LHS symbol as a terminal
    # I need to test that this works, or disallow it
    #
    # 11 August 2008 -- I'm pretty sure I have tested this,
    # but sometime should test it again to make sure
    # before removing this comment

    my $symbol = $symbol_hash->{$name};
    if ( defined $symbol ) {

        if ( $symbol->[Marpa::Internal::Symbol::TERMINAL] ) {
            croak("Attempt to add terminal twice: $name");
        }

        @{$symbol}[
            Marpa::Internal::Symbol::PRODUCTIVE,
            Marpa::Internal::Symbol::NULLING,
            Marpa::Internal::Symbol::REGEX,
            Marpa::Internal::Symbol::PREFIX,
            Marpa::Internal::Symbol::SUFFIX,
            Marpa::Internal::Symbol::ACTION,
            Marpa::Internal::Symbol::TERMINAL,
            Marpa::Internal::Symbol::PRIORITY,
            ]
            = (
            1, 0, $regex, $prefix, $suffix, $action, 1,
            ( pack 'NN', $user_priority, 0 ),
            );

        return;
    } ## end if ( defined $symbol )

    my $symbol_count = @{$symbols};
    my $new_symbol   = [];
    $#{$new_symbol} = Marpa::Internal::Symbol::LAST_FIELD;
    @{$new_symbol}[
        Marpa::Internal::Symbol::ID,
        Marpa::Internal::Symbol::NAME,
        Marpa::Internal::Symbol::LHS,
        Marpa::Internal::Symbol::RHS,
        Marpa::Internal::Symbol::NULLABLE,
        Marpa::Internal::Symbol::PRODUCTIVE,
        Marpa::Internal::Symbol::NULLING,
        Marpa::Internal::Symbol::REGEX,
        Marpa::Internal::Symbol::ACTION,
        Marpa::Internal::Symbol::TERMINAL,
        Marpa::Internal::Symbol::PRIORITY,
        ]
        = (
        $symbol_count, $name, [], [], 0, 1, 0, $regex, $action, 1,
        ( pack 'NN', $user_priority, 0 ),
        );

    push @{$symbols}, $new_symbol;
    return weaken( $symbol_hash->{$name} = $new_symbol );
} ## end sub add_terminal

sub assign_symbol {
    my $grammar = shift;
    my $name    = shift;
    my ( $symbol_hash, $symbols, $default_null_value, ) = @{$grammar}[
        Marpa::Internal::Grammar::SYMBOL_HASH,
        Marpa::Internal::Grammar::SYMBOLS,
        Marpa::Internal::Grammar::DEFAULT_NULL_VALUE,
    ];

    my $symbol_count = @{$symbols};
    my $symbol       = $symbol_hash->{$name};
    if ( not defined $symbol ) {
        $#{$symbol} = Marpa::Internal::Symbol::LAST_FIELD;
        @{$symbol}[
            Marpa::Internal::Symbol::ID,  Marpa::Internal::Symbol::NAME,
            Marpa::Internal::Symbol::LHS, Marpa::Internal::Symbol::RHS,
            ]
            = ( $symbol_count, $name, [], [] );
        push @{$symbols}, $symbol;
        weaken( $symbol_hash->{$name} = $symbol );
    } ## end if ( not defined $symbol )
    return $symbol;
} ## end sub assign_symbol

sub assign_user_symbol {
    my $self = shift;
    my $name = shift;
    if ( my $type = ref $name ) {
        croak("Symbol name was ref to $type; it must be a scalar string");
    }
    croak("Symbol name $name ends in '_': that's not allowed")
        if $name =~ /_\z/xms;
    return assign_symbol( $self, $name );
} ## end sub assign_user_symbol

sub add_user_rule {
    my $grammar       = shift;
    my $lhs_name      = shift;
    my $rhs_names     = shift;
    my $action        = shift;
    my $user_priority = shift;

    my ($rule_hash) = @{$grammar}[Marpa::Internal::Grammar::RULE_HASH];

    my $lhs_symbol = assign_user_symbol( $grammar, $lhs_name );
    $rhs_names //= [];
    my $rhs_symbols =
        [ map { assign_user_symbol( $grammar, $_ ); } @{$rhs_names} ];

    # Don't allow the user to duplicate a rule
    my $rule_key = join q{,},
        map { $_->[Marpa::Internal::Symbol::ID] }
        ( $lhs_symbol, @{$rhs_symbols} );
    croak( 'Duplicate rule: ',
        $lhs_name, ' -> ', ( join q{ }, @{$rhs_names} ) )
        if exists $rule_hash->{$rule_key};

    $rule_hash->{$rule_key} = 1;

    $user_priority //= 0;
    my $max_priority = 1_000_000;
    if ( $user_priority > $max_priority ) {
        croak(
            "Rule priority ($user_priority) greater than maximum ($max_priority)"
        );
    }
    my $min_priority = -1_000_000;
    if ( $user_priority < $min_priority ) {
        croak(
            "Rule priority ($user_priority) less than minimum ($min_priority)"
        );
    }

    return add_rule( $grammar, $lhs_symbol, $rhs_symbols, $action,
        ( pack 'NN', $user_priority, 0 ) );
} ## end sub add_user_rule

sub add_rule {
    my $grammar  = shift;
    my $lhs      = shift;
    my $rhs      = shift;
    my $action   = shift;
    my $priority = shift;

    my ( $rules, $package, $trace_rules, $trace_fh, ) = @{$grammar}[
        Marpa::Internal::Grammar::RULES,
        Marpa::Internal::Grammar::NAME,
        Marpa::Internal::Grammar::TRACE_RULES,
        Marpa::Internal::Grammar::TRACE_FILE_HANDLE,
    ];

    my $rule_count = @{$rules};
    my $new_rule   = [];
    my $nulling    = @{$rhs} ? undef : 1;
    $priority //= pack 'NN', 0, 0;

    @{$new_rule}[
        Marpa::Internal::Rule::ID,       Marpa::Internal::Rule::NAME,
        Marpa::Internal::Rule::LHS,      Marpa::Internal::Rule::RHS,
        Marpa::Internal::Rule::NULLABLE, Marpa::Internal::Rule::PRODUCTIVE,
        Marpa::Internal::Rule::NULLING,  Marpa::Internal::Rule::ACTION,
        Marpa::Internal::Rule::PRIORITY,
        ]
        = (
        $rule_count, "rule $rule_count", $lhs,
        $rhs,        $nulling,           $nulling,
        $nulling,    $action,            $priority,
        );
    $new_rule->[Marpa::Internal::Rule::HASTY] = 0;

    push @{$rules}, $new_rule;
    {
        my $lhs_rules = $lhs->[Marpa::Internal::Symbol::LHS];
        weaken( $lhs_rules->[ scalar @{$lhs_rules} ] = $new_rule );
    }
    if ($nulling) {
        @{$lhs}[
            Marpa::Internal::Symbol::NULLABLE,
            Marpa::Internal::Symbol::PRODUCTIVE
            ]
            = ( 1, 1 );
    } ## end if ($nulling)
    else {
        my $last_symbol = [];
        SYMBOL: for my $symbol ( sort @{$rhs} ) {
            next SYMBOL if $symbol == $last_symbol;
            my $rhs_rules = $symbol->[Marpa::Internal::Symbol::RHS];
            weaken( $rhs_rules->[ scalar @{$rhs_rules} ] = $new_rule );
            $last_symbol = $symbol;
        } ## end for my $symbol ( sort @{$rhs} )
    } ## end else [ if ($nulling)
    if ($trace_rules) {
        print {$trace_fh} 'Added rule #', $#{$rules}, ': ',
            $lhs->[Marpa::Internal::Symbol::NAME], ' -> ',
            join( q{ }, map { $_->[Marpa::Internal::Symbol::NAME] } @{$rhs} ),
            "\n"
            or croak('Could not print to trace file');
    } ## end if ($trace_rules)
    return $new_rule;
} ## end sub add_rule

# add one or more rules
sub add_user_rules {
    my $grammar = shift;
    my $rules   = shift;

    RULE: for my $rule ( @{$rules} ) {

        given ( ref $rule ) {
            when ('ARRAY') {
                my $arg_count = @{$rule};

                if ( $arg_count > 4 or $arg_count < 1 ) {
                    croak(
                        "Rule has $arg_count arguments: "
                            . join( ', ',
                            map { defined $_ ? $_ : 'undef' } @{$rule} )
                            . "\n"
                            . 'Rule must have from 1 to 3 arguments'
                    );
                } ## end if ( $arg_count > 4 or $arg_count < 1 )
                my ( $lhs, $rhs, $action, $user_priority ) = @{$rule};
                add_user_rule( $grammar, $lhs, $rhs, $action,
                    $user_priority );

            } ## end when ('ARRAY')
            when ('HASH') {
                add_rules_from_hash( $grammar, $rule );
            }
            default {
                croak( 'Invalid rule reftype ', ( $_ ? $_ : 'undefined' ) );
            }
        } ## end given

    }    # RULE

    return;

} ## end sub add_user_rules

sub add_rules_from_hash {
    my $grammar = shift;
    my $options = shift;

    my ( $lhs_name, $rhs_names, $action );
    my ( $min, $separator_name );
    my $proper_separation = 0;
    my $keep_separation   = 0;
    my $left_associative  = 1;
    my $user_priority     = 0;

    while ( my ( $option, $value ) = each %{$options} ) {
        given ($option) {
            when ('rhs')               { $rhs_names         = $value }
            when ('lhs')               { $lhs_name          = $value }
            when ('action')            { $action            = $value }
            when ('min')               { $min               = $value }
            when ('separator')         { $separator_name    = $value }
            when ('proper_separation') { $proper_separation = $value }
            when ('left_associative')  { $left_associative  = $value }
            when ('right_associative') { $left_associative  = !$value }
            when ('priority')          { $user_priority     = $value }
            default { croak("Unknown option in counted rule: $option") };
        } ## end given
    } ## end while ( my ( $option, $value ) = each %{$options} )

    croak('Only left associative sequences available')
        unless $left_associative;
    given ($min) {
        when (undef) {;}
        when ( [ 0, 1 ] ) {;}
        default {
            croak('If min is defined for a rule, it must be 0 or 1');
        }
    } ## end given

    if ( scalar @{$rhs_names} == 0 or not defined $min ) {

        if ( defined $separator_name ) {
            croak('separator defined for rule without repetitions');
        }

        # This is an ordinary, non-counted rule,
        # which we'll take care of first as a special case
        my $ordinary_rule =
            add_user_rule( $grammar, $lhs_name, $rhs_names, $action,
            $user_priority );

        return;

    }    # not defined $min

    # At this point we know that min must be 0 or 1
    # and that there is at least one symbol on the rhs

    # nulling rule is special case
    if ( $min == 0 ) {
        my $rule_action;
        given ($action) {
            when (undef) { $rule_action = undef }
            default {
                $rule_action = q{ @_ = (); } . $action;
            }
        } ## end given
        add_user_rule( $grammar, $lhs_name, [], $rule_action,
            $user_priority );
        $min = 1;
    } ## end if ( $min == 0 )

    croak('Only one rhs symbol allowed for counted rule')
        if scalar @{$rhs_names} != 1;

    # create the rhs symbol
    my $rhs_name = pop @{$rhs_names};
    my $rhs = assign_user_symbol( $grammar, $rhs_name );
    $rhs->[Marpa::Internal::Symbol::COUNTED] = 1;

    # create the separator symbol, if we're using one
    my $separator;
    if ( defined $separator_name ) {
        $separator = assign_user_symbol( $grammar, $separator_name );
        $separator->[Marpa::Internal::Symbol::COUNTED] = 1;
    }

    # create the sequence symbol
    my $sequence_name = $rhs_name . "[Seq:$min-*]";
    if ( defined $separator_name ) {
        my $punctuation_free_separator_name = $separator_name;
        $punctuation_free_separator_name =~ s/[^[:alnum:]]/_/gxms;
        $sequence_name .= '[Sep:' . $punctuation_free_separator_name . ']';
    }
    my $unique_name_piece = sprintf '[x%x]',
        scalar @{ $grammar->[Marpa::Internal::Grammar::SYMBOLS] };
    $sequence_name .= $unique_name_piece;
    my $sequence = assign_symbol( $grammar, $sequence_name );

    my $lhs = assign_user_symbol( $grammar, $lhs_name );

    # Don't allow the user to duplicate a rule
    # I'm pretty general here -- I consider a sequence rule a duplicate if rhs, lhs
    # and separator are the same.  I may want to get more fancy, but save that
    # for later.
    {
        my $rule_hash = $grammar->[Marpa::Internal::Grammar::RULE_HASH];
        my @key_rhs =
            defined $separator ? ( $rhs, $separator, $rhs ) : ($rhs);
        my $rule_key = join q{,},
            map { $_->[Marpa::Internal::Symbol::ID] } ( $lhs, @key_rhs );
        croak( 'Duplicate rule: ',
            $lhs_name, q{ -> }, ( join q{,}, @{$rhs_names} ) )
            if exists $rule_hash->{$rule_key};
        $rule_hash->{$rule_key} = 1;
    }

    my $rule_action;
    given ($action) {
        when (undef) { $rule_action = undef; }
        default {
            if ($left_associative) {

                # more efficient way to do this?
                $rule_action =

                    <<'EO_CODE'
    HEAD: while (1) {
        my $head = shift @_;
        last HEAD unless scalar @{$head};
        unshift(@_, @{$head});
    }
EO_CODE

            } ## end if ($left_associative)
            else {
                croak('Only left associative sequences available');
            }
            $rule_action .= $action;
        } ## end default
    } ## end given
    add_rule( $grammar, $lhs, [$sequence], $rule_action,
        ( pack 'NN', $user_priority, 0 ) );
    if ( defined $separator and not $proper_separation ) {
        unless ($keep_separation) {
            $rule_action = q{ pop @_; } . ( $rule_action // q{} );
        }
        add_rule( $grammar, $lhs, [ $sequence, $separator, ],
            $rule_action, ( pack 'NN', $user_priority, 0 ) );
    } ## end if ( defined $separator and not $proper_separation )

    my @separated_rhs = ($rhs);
    push @separated_rhs, $separator if defined $separator;

    # minimal sequence rule
    my $counted_rhs = [ (@separated_rhs) x ( $min - 1 ), $rhs ];

    if ($left_associative) {
        if ( defined $separator and not $keep_separation ) {
            $rule_action =

                <<'EO_CODE'
    [
        [],
        @_[
            grep { !($_ % 2) } (0 .. $#_)
        ]
    ]
EO_CODE

        } ## end if ( defined $separator and not $keep_separation )
        else {
            $rule_action = q{ unshift(@_, []); } . "\n" . q{ \@_ } . "\n";
        }
    } ## end if ($left_associative)
    else {
        croak('Only left associative sequences available');
    }

    add_rule( $grammar, $sequence, $counted_rhs, $rule_action,
        ( pack 'NN', $user_priority, 0 ) );

    # iterating sequence rule
    if ( defined $separator and not $keep_separation ) {
        $rule_action =

            <<'EO_CODE'
    [
        @_[
           grep { !($_ % 2) } (0 .. $#_)
        ],
    ]
EO_CODE

    } ## end if ( defined $separator and not $keep_separation )
    else {

        $rule_action = "\n" . '\@_' . "\n";

    }
    my @iterating_rhs = ( @separated_rhs, $sequence );
    if ($left_associative) {
        @iterating_rhs = reverse @iterating_rhs;
    }
    add_rule( $grammar, $sequence, ( \@iterating_rhs ),
        $rule_action, ( pack 'NN', $user_priority, 0 ) );

    return;

}    # sub add_rules_from_hash

sub add_user_terminals {
    my $grammar   = shift;
    my $terminals = shift;

    my $type = ref $terminals;
    TERMINAL: for my $terminal ( @{$terminals} ) {
        my $lhs_name;
        my $options = {};
        if ( ref $terminal eq 'ARRAY' ) {
            my $arg_count = @{$terminal};
            if ( $arg_count > 2 or $arg_count < 1 ) {
                croak('terminal must have 1 or 2 arguments');
            }
            ( $lhs_name, $options ) = @{$terminal};
        } ## end if ( ref $terminal eq 'ARRAY' )
        else {
            $lhs_name = $terminal;
        }
        add_user_terminal( $grammar, $lhs_name, $options );
    } ## end for my $terminal ( @{$terminals} )
    return 1;
} ## end sub add_user_terminals

sub add_user_terminal {
    my $grammar = shift;
    my $name    = shift;
    my $options = shift;

    if ( my $type = ref $name ) {
        croak("Terminal name was ref to $type; it must be a scalar string");
    }
    croak("Symbol name $name ends in '_': that's not allowed")
        if $name =~ /_\z/xms;
    add_terminal( $grammar, $name, $options );
    return;
} ## end sub add_user_terminal

sub check_start {
    my $grammar = shift;
    my $success = 1;

    my $start_name = $grammar->[Marpa::Internal::Grammar::START_NAME];
    croak('No start symbol specified') unless defined $start_name;
    if ( my $ref_type = ref $start_name ) {
        croak(
            "Start symbol name specified as a ref to $ref_type, it should be a string"
        );
    }

    my $symbol_hash = $grammar->[Marpa::Internal::Grammar::SYMBOL_HASH];
    my $start       = $symbol_hash->{$start_name};

    if ( not defined $start ) {
        croak( 'Start symbol: ' . $start_name . ' not defined' );
    }

    my ( $lhs, $rhs, $terminal, $productive ) = @{$start}[
        Marpa::Internal::Symbol::LHS,
        Marpa::Internal::Symbol::RHS,
        Marpa::Internal::Symbol::TERMINAL,
        Marpa::Internal::Symbol::PRODUCTIVE,
    ];

    if ( not scalar @{$lhs} and not $terminal ) {
        my $problem =
            'Start symbol ' . $start_name . ' not on LHS of any rule';
        push @{ $grammar->[Marpa::Internal::Grammar::PROBLEMS] }, $problem;
        $success = 0;
    } ## end if ( not scalar @{$lhs} and not $terminal )

    if ( not $productive ) {
        my $problem = 'Unproductive start symbol: ' . $start_name;
        push @{ $grammar->[Marpa::Internal::Grammar::PROBLEMS] }, $problem;
        $success = 0;
    }

    $grammar->[Marpa::Internal::Grammar::START] = $start;

    return $success;
} ## end sub check_start

# return list of rules reachable from the start symbol;
sub accessible {
    my $grammar = shift;
    my $start   = $grammar->[Marpa::Internal::Grammar::START];

    $start->[Marpa::Internal::Symbol::ACCESSIBLE] = 1;
    my $symbol_work_set = [$start];
    my $rule_work_set   = [];

    my $work_to_do = 1;

    while ($work_to_do) {
        $work_to_do = 0;

        SYMBOL_PASS: while ( my $work_symbol = shift @{$symbol_work_set} ) {
            my $rules_produced = $work_symbol->[Marpa::Internal::Symbol::LHS];
            PRODUCED_RULE: for my $rule ( @{$rules_produced} ) {

                next PRODUCED_RULE
                    if defined $rule->[Marpa::Internal::Rule::ACCESSIBLE];

                $rule->[Marpa::Internal::Rule::ACCESSIBLE] = 1;
                $work_to_do++;
                push @{$rule_work_set}, $rule;

            } ## end for my $rule ( @{$rules_produced} )
        }    # SYMBOL_PASS

        RULE: while ( my $work_rule = shift @{$rule_work_set} ) {
            my $rhs_symbol = $work_rule->[Marpa::Internal::Rule::RHS];

            RHS: for my $symbol ( @{$rhs_symbol} ) {

                next RHS
                    if defined $symbol->[Marpa::Internal::Symbol::ACCESSIBLE];
                $symbol->[Marpa::Internal::Symbol::ACCESSIBLE] = 1;
                $work_to_do++;

                push @{$symbol_work_set}, $symbol;
            } ## end for my $symbol ( @{$rhs_symbol} )

        }    # RULE

    }    # work_to_do loop

    return 1;

} ## end sub accessible

sub productive {
    my $grammar = shift;

    my ( $rules, $symbols ) =
        @{$grammar}[ Marpa::Internal::Grammar::RULES,
        Marpa::Internal::Grammar::SYMBOLS ];

    # If a symbol's nullability could not be determined, it was unproductive.
    # All nullable symbols are productive.
    for my $symbol ( @{$symbols} ) {
        if ( not defined $_->[Marpa::Internal::Symbol::NULLABLE] ) {
            $_->[Marpa::Internal::Symbol::PRODUCTIVE] = 0;
        }
        if ( $_->[Marpa::Internal::Symbol::NULLABLE] ) {
            $_->[Marpa::Internal::Symbol::PRODUCTIVE] = 1;
        }
    } ## end for my $symbol ( @{$symbols} )

    # If a rule's nullability could not be determined, it was unproductive.
    # All nullable rules are productive.
    for my $rule ( @{$rules} ) {
        if ( not defined $rule->[Marpa::Internal::Rule::NULLABLE] ) {
            $_->[Marpa::Internal::Symbol::PRODUCTIVE] = 0;
        }
        if ( $rule->[Marpa::Internal::Rule::NULLABLE] ) {
            $_->[Marpa::Internal::Symbol::PRODUCTIVE] = 1;
        }
    } ## end for my $rule ( @{$rules} )

    my $symbol_work_set = [];
    $#{$symbol_work_set} = $#{$symbols};
    my $rule_work_set = [];
    $#{$rule_work_set} = $#{$rules};

    for my $symbol_id (
        grep { defined $symbols->[$_]->[Marpa::Internal::Symbol::PRODUCTIVE] }
        ( 0 .. $#{$symbols} )
        )
    {
        $symbol_work_set->[$symbol_id] = 1;
    } ## end for my $symbol_id ( grep { defined $symbols->[$_]->[...
    for my $rule_id (
        grep { defined $rules->[$_]->[Marpa::Internal::Rule::PRODUCTIVE] }
        ( 0 .. $#{$rules} ) )
    {
        $rule_work_set->[$rule_id] = 1;
    } ## end for my $rule_id ( grep { defined $rules->[$_]->[...
    my $work_to_do = 1;

    while ($work_to_do) {
        $work_to_do = 0;

        SYMBOL_PASS:
        for my $symbol_id ( grep { $symbol_work_set->[$_] }
            ( 0 .. $#{$symbol_work_set} ) )
        {
            my $work_symbol = $symbols->[$symbol_id];
            $symbol_work_set->[$symbol_id] = 0;

            my $rules_producing =
                $work_symbol->[Marpa::Internal::Symbol::RHS];
            PRODUCING_RULE: for my $rule ( @{$rules_producing} ) {

                # no work to do -- this rule already has productive status marked
                next PRODUCING_RULE
                    if defined $rule->[Marpa::Internal::Rule::PRODUCTIVE];

                # assume productive until we hit an unmarked or unproductive symbol
                my $rule_productive = 1;

                # are all symbols on the RHS of this rule bottom marked?
                RHS_SYMBOL:
                for my $rhs_symbol (
                    @{ $rule->[Marpa::Internal::Rule::RHS] } )
                {
                    my $productive =
                        $rhs_symbol->[Marpa::Internal::Symbol::PRODUCTIVE];

                    # unmarked symbol, change the assumption for rule to undef,
                    # but keep scanning for unproductive
                    # symbol, which will override everything else
                    if ( not defined $productive ) {
                        $rule_productive = undef;
                        next RHS_SYMBOL;
                    }

                    # any unproductive RHS symbol means the rule is unproductive
                    if ( $productive == 0 ) {
                        $rule_productive = 0;
                        last RHS_SYMBOL;
                    }
                } ## end for my $rhs_symbol ( @{ $rule->[...

                # if this pass found the rule productive or unproductive, mark the rule
                if ( defined $rule_productive ) {
                    $rule->[Marpa::Internal::Rule::PRODUCTIVE] =
                        $rule_productive;
                    $work_to_do++;
                    $rule_work_set->[ $rule->[Marpa::Internal::Rule::ID] ] =
                        1;
                } ## end if ( defined $rule_productive )

            } ## end for my $rule ( @{$rules_producing} )
        }    # SYMBOL_PASS

        RULE:
        for my $rule_id ( grep { $rule_work_set->[$_] }
            ( 0 .. $#{$rule_work_set} ) )
        {
            my $work_rule = $rules->[$rule_id];
            $rule_work_set->[$rule_id] = 0;
            my $lhs_symbol = $work_rule->[Marpa::Internal::Rule::LHS];

            # no work to do -- this symbol already has productive status marked
            next RULE
                if defined $lhs_symbol->[Marpa::Internal::Symbol::PRODUCTIVE];

            # assume unproductive until we hit an unmarked or non-nullable symbol
            my $symbol_productive = 0;

            LHS_RULE:
            for my $rule ( @{ $lhs_symbol->[Marpa::Internal::Symbol::LHS] } )
            {

                my $productive = $rule->[Marpa::Internal::Rule::PRODUCTIVE];

                # unmarked symbol, change the assumption for rule to undef, but keep scanning for nullable
                # rule, which will override everything else
                if ( not defined $productive ) {
                    $symbol_productive = undef;
                    next LHS_RULE;
                }

                # any productive rule means the LHS is productive
                if ( $productive == 1 ) {
                    $symbol_productive = 1;
                    last LHS_RULE;
                }
            } ## end for my $rule ( @{ $lhs_symbol->[...

            # if this pass found the symbol productive or unproductive, mark the symbol
            if ( defined $symbol_productive ) {
                $lhs_symbol->[Marpa::Internal::Symbol::PRODUCTIVE] =
                    $symbol_productive;
                $work_to_do++;
                $symbol_work_set->[ $lhs_symbol->[Marpa::Internal::Symbol::ID]
                ] = 1;
            } ## end if ( defined $symbol_productive )

        }    # RULE

    }    # work_to_do loop

    return 1;

} ## end sub productive

sub terminals_distinguished {
    my ($grammar) = @_;
    my $symbols = $grammar->[Marpa::Internal::Grammar::SYMBOLS];
    for my $symbol ( @{$symbols} ) {
        return 1 if $symbol->[Marpa::Internal::Symbol::TERMINAL];
    }
    my $rules = $grammar->[Marpa::Internal::Grammar::RULES];
    RULE: for my $rule ( @{$rules} ) {
        next RULE if scalar @{ $rule->[Marpa::Internal::Rule::RHS] };
        croak('A grammar with empty rules must mark its terminals');
    }
    return 0;
} ## end sub terminals_distinguished

sub mark_all_symbols_terminal {
    my ($grammar) = @_;
    my $symbols = $grammar->[Marpa::Internal::Grammar::SYMBOLS];
    for my $symbol ( @{$symbols} ) {
        $symbol->[Marpa::Internal::Symbol::TERMINAL]   = 1;
        $symbol->[Marpa::Internal::Symbol::NULLING]    = 0;
        $symbol->[Marpa::Internal::Symbol::NULLABLE]   = 0;
        $symbol->[Marpa::Internal::Symbol::PRODUCTIVE] = 1;
    } ## end for my $symbol ( @{$symbols} )
    my $rules = $grammar->[Marpa::Internal::Grammar::RULES];
    for my $rule ( @{$rules} ) {
        $rule->[Marpa::Internal::Rule::NULLING]    = 0;
        $rule->[Marpa::Internal::Rule::NULLABLE]   = 0;
        $rule->[Marpa::Internal::Rule::PRODUCTIVE] = 1;
    }
    return 1;
} ## end sub mark_all_symbols_terminal

sub nulling {
    my $grammar = shift;

    my ( $rules, $symbols ) = @{$grammar}[
        Marpa::Internal::Grammar::RULES,
        Marpa::Internal::Grammar::SYMBOLS,
    ];

    my $symbol_work_set = [];
    $#{$symbol_work_set} = $#{$symbols};
    my $rule_work_set = [];
    $#{$rule_work_set} = $#{$rules};

    for my $rule_id (
        map  { $_->[Marpa::Internal::Rule::ID] }
        grep { $_->[Marpa::Internal::Rule::NULLING] } @{$rules}
        )
    {
        $rule_work_set->[$rule_id] = 1;
    } ## end for my $rule_id ( map { $_->[Marpa::Internal::Rule::ID...

    for my $symbol_id (
        map  { $_->[Marpa::Internal::Symbol::ID] }
        grep { $_->[Marpa::Internal::Symbol::NULLING] } @{$symbols}
        )
    {
        $symbol_work_set->[$symbol_id] = 1;
    } ## end for my $symbol_id ( map { $_->[Marpa::Internal::Symbol::ID...

    my $work_to_do = 1;

    while ($work_to_do) {
        $work_to_do = 0;

        RULE:
        for my $rule_id ( grep { $rule_work_set->[$_] }
            ( 0 .. $#{$rule_work_set} ) )
        {
            my $work_rule = $rules->[$rule_id];
            $rule_work_set->[$rule_id] = 0;
            my $lhs_symbol = $work_rule->[Marpa::Internal::Rule::LHS];

            # no work to do -- this symbol already is marked one way or the other
            next RULE
                if defined $lhs_symbol->[Marpa::Internal::Symbol::NULLING];

            # assume nulling until we hit an unmarked or non-nulling symbol
            my $symbol_nulling = 1;

            # make sure that all rules for this lhs are nulling
            LHS_RULE:
            for my $rule ( @{ $lhs_symbol->[Marpa::Internal::Symbol::LHS] } )
            {

                my $nulling = $rule->[Marpa::Internal::Rule::NULLING];

                # unmarked rule, change the assumption for the symbol to undef,
                # but keep scanning for rule marked non-nulling,
                # which will override everything else
                if ( not defined $nulling ) {
                    $symbol_nulling = undef;
                    next LHS_RULE;
                }

                # any non-nulling rule means the LHS is not nulling
                if ( $nulling == 0 ) {
                    $symbol_nulling = 0;
                    last LHS_RULE;
                }
            } ## end for my $rule ( @{ $lhs_symbol->[...

            # if this pass found the symbol nulling or non-nulling
            #  mark the symbol
            if ( defined $symbol_nulling ) {
                $lhs_symbol->[Marpa::Internal::Symbol::NULLING] =
                    $symbol_nulling;
                $work_to_do++;

                $symbol_work_set->[ $lhs_symbol->[Marpa::Internal::Symbol::ID]
                ] = 1;
            } ## end if ( defined $symbol_nulling )

        }    # RULE

        SYMBOL_PASS:
        for my $symbol_id ( grep { $symbol_work_set->[$_] }
            ( 0 .. $#{$symbol_work_set} ) )
        {
            my $work_symbol = $symbols->[$symbol_id];
            $symbol_work_set->[$symbol_id] = 0;

            my $rules_producing =
                $work_symbol->[Marpa::Internal::Symbol::RHS];
            PRODUCING_RULE: for my $rule ( @{$rules_producing} ) {

                # no work to do -- this rule already has nulling marked
                next PRODUCING_RULE
                    if defined $rule->[Marpa::Internal::Rule::NULLING];

                # assume nulling until we hit an unmarked or non-nulling symbol
                my $rule_nulling = 1;

                # are all symbols on the RHS of this rule marked?
                RHS_SYMBOL:
                for my $rhs_symbol (
                    @{ $rule->[Marpa::Internal::Rule::RHS] } )
                {
                    my $nulling =
                        $rhs_symbol->[Marpa::Internal::Symbol::NULLING];

                    # unmarked rule, change the assumption for rule to undef,
                    # but keep scanning for non-nulling
                    # rule, which will override everything else
                    if ( not defined $nulling ) {
                        $rule_nulling = undef;
                        next RHS_SYMBOL;
                    }

                    # any non-nulling RHS symbol means the rule is non-nulling
                    if ( $nulling == 0 ) {
                        $rule_nulling = 0;
                        last RHS_SYMBOL;
                    }
                } ## end for my $rhs_symbol ( @{ $rule->[...

                # if this pass found the rule nulling or non-nulling, mark the rule
                if ( defined $rule_nulling ) {
                    $rule->[Marpa::Internal::Rule::NULLING] = $rule_nulling;
                    $work_to_do++;
                    $rule_work_set->[ $rule->[Marpa::Internal::Rule::ID] ] =
                        1;
                } ## end if ( defined $rule_nulling )

            } ## end for my $rule ( @{$rules_producing} )
        }    # SYMBOL_PASS

    }    # work_to_do loop

    return 1;

} ## end sub nulling

# returns undef if there was a problem
sub nullable {
    my $grammar = shift;
    my ( $rules, $symbols, $tracing ) = @{$grammar}[
        Marpa::Internal::Grammar::RULES,
        Marpa::Internal::Grammar::SYMBOLS,
        Marpa::Internal::Grammar::TRACING,
    ];

    my $trace_fh;
    if ($tracing) {
        $trace_fh = $grammar->[Marpa::Internal::Grammar::TRACING];
    }

    # boolean to track if current pass has changed anything
    my $work_to_do = 1;

    my $symbol_work_set = [];
    $#{$symbol_work_set} = @{$symbols};
    my $rule_work_set = [];
    $#{$rule_work_set} = @{$rules};

    for my $symbol_id (
        map { $_->[Marpa::Internal::Symbol::ID] }
        grep {
                   $_->[Marpa::Internal::Symbol::NULLABLE]
                or $_->[Marpa::Internal::Symbol::NULLING]
        } @{$symbols}
        )
    {
        $symbol_work_set->[$symbol_id] = 1;
    } ## end for my $symbol_id ( map { $_->[Marpa::Internal::Symbol::ID...
    for my $rule_id (
        map  { $_->[Marpa::Internal::Rule::ID] }
        grep { defined $_->[Marpa::Internal::Rule::NULLABLE] } @{$rules}
        )
    {
        $rule_work_set->[$rule_id] = 1;
    } ## end for my $rule_id ( map { $_->[Marpa::Internal::Rule::ID...

    while ($work_to_do) {
        $work_to_do = 0;

        SYMBOL_PASS:
        for my $symbol_id ( grep { $symbol_work_set->[$_] }
            ( 0 .. $#{$symbol_work_set} ) )
        {
            my $work_symbol = $symbols->[$symbol_id];
            $symbol_work_set->[$symbol_id] = 0;
            my $rules_producing =
                $work_symbol->[Marpa::Internal::Symbol::RHS];

            PRODUCING_RULE: for my $rule ( @{$rules_producing} ) {

                # assume nullable until we hit an unmarked or non-nullable symbol
                my $rule_nullable = 1;

                # no work to do -- this rule already has nullability marked
                next PRODUCING_RULE
                    if defined $rule->[Marpa::Internal::Rule::NULLABLE];

                # are all symbols on the RHS of this rule bottom marked?
                RHS_SYMBOL:
                for my $rhs_symbol (
                    @{ $rule->[Marpa::Internal::Rule::RHS] } )
                {
                    my $nullable =
                        $rhs_symbol->[Marpa::Internal::Symbol::NULLABLE];

                    # unmarked symbol, change the assumption for rule to undef, but keep scanning for non-nullable
                    # symbol, which will override everything else
                    if ( not defined $nullable ) {
                        $rule_nullable = undef;
                        next RHS_SYMBOL;
                    }

                    # any non-nullable RHS symbol means the rule is not nullable
                    if ( $nullable == 0 ) {
                        $rule_nullable = 0;
                        last RHS_SYMBOL;
                    }
                } ## end for my $rhs_symbol ( @{ $rule->[...

                # if this pass found the rule nullable or not, so mark the rule
                if ( defined $rule_nullable ) {
                    $rule->[Marpa::Internal::Rule::NULLABLE] = $rule_nullable;
                    $work_to_do++;
                    $rule_work_set->[ $rule->[Marpa::Internal::Rule::ID] ] =
                        1;
                } ## end if ( defined $rule_nullable )

            } ## end for my $rule ( @{$rules_producing} )
        }    # SYMBOL_PASS

        RULE:
        for my $rule_id ( grep { $rule_work_set->[$_] }
            ( 0 .. $#{$rule_work_set} ) )
        {
            my $work_rule  = $rules->[$rule_id];
            my $lhs_symbol = $work_rule->[Marpa::Internal::Rule::LHS];

            # no work to do -- this symbol already has nullability marked
            next RULE
                if defined $lhs_symbol->[Marpa::Internal::Symbol::NULLABLE];

            # assume non-nullable until we hit an unmarked or non-nullable symbol
            my $symbol_nullable = 0;

            LHS_RULE:
            for my $rule ( @{ $lhs_symbol->[Marpa::Internal::Symbol::LHS] } )
            {

                my $nullable = $rule->[Marpa::Internal::Rule::NULLABLE];

                # unmarked symbol, change the assumption for rule to undef,
                # but keep scanning for nullable
                # rule, which will override everything else
                if ( not defined $nullable ) {
                    $symbol_nullable = undef;
                    next LHS_RULE;
                }

                # any nullable rule means the LHS is nullable
                if ( $nullable == 1 ) {
                    $symbol_nullable = 1;
                    last LHS_RULE;
                }
            } ## end for my $rule ( @{ $lhs_symbol->[...

            # if this pass found the symbol nullable or not, mark the symbol
            if ( defined $symbol_nullable ) {
                $lhs_symbol->[Marpa::Internal::Symbol::NULLABLE] =
                    $symbol_nullable;
                $work_to_do++;
                $symbol_work_set->[ $lhs_symbol->[Marpa::Internal::Symbol::ID]
                ] = 1;
            } ## end if ( defined $symbol_nullable )

        }    # RULE

    }    # work_to_do loop

    my $counted_nullable_count;
    for my $symbol ( @{$symbols} ) {
        my ( $name, $nullable, $counted, ) = @{$symbol}[
            Marpa::Internal::Symbol::NAME,
            Marpa::Internal::Symbol::NULLABLE,
            Marpa::Internal::Symbol::COUNTED,
        ];
        if ( $nullable and $counted ) {
            my $problem = "Nullable symbol $name is on rhs of counted rule";
            push @{ $grammar->[Marpa::Internal::Grammar::PROBLEMS] },
                $problem;
            $counted_nullable_count++;
        } ## end if ( $nullable and $counted )
    } ## end for my $symbol ( @{$symbols} )
    if ($counted_nullable_count) {
        my $problem =
            'Counted nullables confuse Marpa -- please rewrite the grammar';
        push @{ $grammar->[Marpa::Internal::Grammar::PROBLEMS] }, $problem;
        return;
    } ## end if ($counted_nullable_count)

    return 1;

} ## end sub nullable

# This assumes the grammar has been rewritten into CHAF form.
sub detect_cycle {
    my $grammar = shift;
    my ( $rules, $symbols, $cycle_action, $trace_fh ) = @{$grammar}[
        Marpa::Internal::Grammar::RULES,
        Marpa::Internal::Grammar::SYMBOLS,
        Marpa::Internal::Grammar::CYCLE_ACTION,
        Marpa::Internal::Grammar::TRACE_FILE_HANDLE,
    ];

    my $cycle_is_fatal = 1;
    my $warn_on_cycle  = 1;
    given ($cycle_action) {
        when ('warn') { $cycle_is_fatal = 0; }
        when ('quiet') {
            $cycle_is_fatal = 0;
            $warn_on_cycle  = 0;
        }
    } ## end given

    my @unit_derivation;         # for the unit derivation matrix
    my @new_unit_derivations;    # a list of new unit derivations
    my @unit_rules;              # a list of the unit rules

    # initialize the unit derivations from the rules
    RULE: for my $rule ( @{$rules} ) {
        next RULE if not $rule->[Marpa::Internal::Rule::USEFUL];
        my $rhs = $rule->[Marpa::Internal::Rule::RHS];
        my $non_nullable_symbol;

        # Only one empty rule is allowed in a CHAF grammar -- a nulling
        # start rule -- this takes care of that exception.
        next RULE unless scalar @{$rhs};

        for my $rhs_symbol ( @{$rhs} ) {
            if ( not $rhs_symbol->[Marpa::Internal::Symbol::NULLABLE] ) {

                # if we have two non-nullables on the RHS in this rule,
                # it can never amount to a unit rule and we can ignore it
                next RULE if defined $non_nullable_symbol;

                $non_nullable_symbol = $rhs_symbol;
            } ## end if ( not $rhs_symbol->[Marpa::Internal::Symbol::NULLABLE...
        }    # for $rhs_symbol

        # Above we've eliminated all rules with two or more non-nullables
        # on the RHS.  So here we have a rule with at most one non-nullable
        # on the RHS.

        next RULE unless defined $non_nullable_symbol;

        my $start_id =
            $rule->[Marpa::Internal::Rule::LHS]
            ->[Marpa::Internal::Symbol::ID];
        my $derived_id = $non_nullable_symbol->[Marpa::Internal::Symbol::ID];

        # Keep track of our unit rules
        push @unit_rules, [ $rule, $start_id, $derived_id ];

        $unit_derivation[$start_id][$derived_id] = 1;
        push @new_unit_derivations, [ $start_id, $derived_id ];

    } ## end for my $rule ( @{$rules} )

    # Now find the transitive closure of the unit derivation matrix
    CLOSURE_LOOP:
    while ( my $new_unit_derivation = shift @new_unit_derivations ) {

        my ( $start_id, $derived_id ) = @{$new_unit_derivation};
        ID: for my $id ( 0 .. $#{$symbols} ) {

            # does the derived symbol derive this id?
            # if not, no new derivation, and continue looping
            next ID if not $unit_derivation[$derived_id][$id];

            # also, if we've already recorded this unit derivation,
            # skip it
            next ID if $unit_derivation[$start_id][$id];

            $unit_derivation[$start_id][$id] = 1;
            push @new_unit_derivations, [ $start_id, $id ];
        } ## end for my $id ( 0 .. $#{$symbols} )

    } ## end while ( my $new_unit_derivation = shift @new_unit_derivations)

    my $cycle_count = 0;

    # produce a list of the rules which cycle
    RULE: while ( my $unit_rule_data = pop @unit_rules ) {

        my ( $rule, $start_symbol_id, $derived_symbol_id ) =
            @{$unit_rule_data};

        if (   $start_symbol_id == $derived_symbol_id
            || $unit_derivation[$derived_symbol_id][$start_symbol_id] )
        {
            $cycle_count++;
            $rule->[Marpa::Internal::Rule::CYCLE] = 1;

            my $warning_rule;

            my $original_rule = $rule->[Marpa::Internal::Rule::ORIGINAL_RULE];
            if ( defined $original_rule ) {
                if ( not $original_rule->[Marpa::Internal::Rule::CYCLE] ) {
                    $original_rule->[Marpa::Internal::Rule::CYCLE] = 1;
                    $warning_rule = $original_rule;
                }
            } ## end if ( defined $original_rule )
            else {

                # always warn if there's no original rule
                $warning_rule = $rule;
            }

            if ( $warn_on_cycle and defined $warning_rule ) {
                print {$trace_fh}
                    'Cycle found involving rule: ',
                    Marpa::brief_rule($warning_rule), "\n"
                    or croak('Could not print to trace file');
            } ## end if ( $warn_on_cycle and defined $warning_rule )
        } ## end if ( $start_symbol_id == $derived_symbol_id || ...
    } ## end while ( my $unit_rule_data = pop @unit_rules )

    croak('Cycle in grammar, fatal error')
        if $cycle_count and $cycle_is_fatal;

    return 1;

}    # sub detect_cycle

sub create_NFA {
    my $grammar = shift;
    my ( $rules, $symbols, $symbol_hash, $start, $academic ) = @{$grammar}[
        Marpa::Internal::Grammar::RULES,
        Marpa::Internal::Grammar::SYMBOLS,
        Marpa::Internal::Grammar::SYMBOL_HASH,
        Marpa::Internal::Grammar::START,
        Marpa::Internal::Grammar::ACADEMIC
    ];

    $grammar->[Marpa::Internal::Grammar::NULLABLE_SYMBOL] =
        [ grep { $_->[Marpa::Internal::Symbol::NULLABLE] } @{$symbols} ];

    my $NFA = [];
    $grammar->[Marpa::Internal::Grammar::NFA] = $NFA;

    my $state_id = 0;
    my @NFA_by_item;

    # create S0
    my $s0 = [];
    @{$s0}[
        Marpa::Internal::NFA::ID, Marpa::Internal::NFA::NAME,
        Marpa::Internal::NFA::TRANSITION
        ]
        = ( $state_id++, 'S0', {} );
    push @{$NFA}, $s0;

    # create the other states
    RULE: for my $rule ( @{$rules} ) {
        my ( $rule_id, $rhs, $useful ) = @{$rule}[
            Marpa::Internal::Rule::ID, Marpa::Internal::Rule::RHS,
            Marpa::Internal::Rule::USEFUL
        ];
        next RULE unless $academic or $useful;
        for my $position ( 0 .. scalar @{$rhs} ) {
            my $new_state = [];
            @{$new_state}[
                Marpa::Internal::NFA::ID,
                Marpa::Internal::NFA::NAME,
                Marpa::Internal::NFA::ITEM,
                Marpa::Internal::NFA::TRANSITION
                ]
                = ( $state_id, 'S' . $state_id, [ $rule, $position ], {} );
            $state_id++;
            push @{$NFA}, $new_state;
            $NFA_by_item[$rule_id][$position] = $new_state;
        }    # position
    }    # rule

    # now add the transitions
    STATE: for my $state ( @{$NFA} ) {
        my ( $id, $name, $item, $transition ) = @{$state};

        # First, deal with transitions from state 0.
        # S0 is the state with no LR(0) item
        if ( not defined $item ) {

            # start rules are rules with the start symbol
            # or with the start alias on the LHS.
            my @start_rules = @{ $start->[Marpa::Internal::Symbol::LHS] };
            my $start_alias = $start->[Marpa::Internal::Symbol::NULL_ALIAS];
            if ( defined $start_alias ) {
                push @start_rules,
                    @{ $start_alias->[Marpa::Internal::Symbol::LHS] };
            }

            # From S0, add an empty transition to the every NFA state
            # corresponding to a start rule with the dot at the beginning
            # of the RHS.
            RULE: for my $start_rule (@start_rules) {
                my ( $start_rule_id, $useful ) = @{$start_rule}[
                    Marpa::Internal::Rule::ID,
                    Marpa::Internal::Rule::USEFUL
                ];
                next RULE unless $useful;
                push @{ $transition->{q{}} }, $NFA_by_item[$start_rule_id][0];
            } ## end for my $start_rule (@start_rules)
            next STATE;
        } ## end if ( not defined $item )

        # transitions from states other than state 0:

        my ( $rule, $position ) = @{$item}[
            Marpa::Internal::LR0_item::RULE,
            Marpa::Internal::LR0_item::POSITION
        ];
        my $rule_id     = $rule->[Marpa::Internal::Rule::ID];
        my $next_symbol = $rule->[Marpa::Internal::Rule::RHS]->[$position];

        # no transitions if position is after the end of the RHS
        if ( not defined $next_symbol ) {
            $state->[Marpa::Internal::NFA::COMPLETE] = 1;
            $state->[Marpa::Internal::NFA::PRIORITY] =
                $rule->[Marpa::Internal::Rule::PRIORITY];
            next STATE;
        } ## end if ( not defined $next_symbol )

        $state->[Marpa::Internal::NFA::AT_NULLING] = 1
            if $next_symbol->[Marpa::Internal::Symbol::NULLING];

        # the scanning transition: the transition if the position is at symbol X
        # in the RHS, via symbol X, to the state corresponding to the same
        # rule with the position incremented by 1
        # should I use ID as the key for those hashes, or NAME?
        push @{ $transition->{ $next_symbol->[Marpa::Internal::Symbol::NAME]
                } },
            $NFA_by_item[$rule_id][ $position + 1 ];

        # the prediction transitions: transitions if the position is at symbol X
        # in the RHS, via the empty symbol, to all states with X on the LHS and
        # position 0
        RULE:
        for my $predicted_rule (
            @{ $next_symbol->[Marpa::Internal::Symbol::LHS] } )
        {
            my ( $predicted_rule_id, $useful ) =
                @{$predicted_rule}[ Marpa::Internal::Rule::ID,
                Marpa::Internal::Rule::USEFUL ];
            next RULE unless $useful;
            push @{ $transition->{q{}} }, $NFA_by_item[$predicted_rule_id][0];
        } ## end for my $predicted_rule ( @{ $next_symbol->[...
    } ## end for my $state ( @{$NFA} )

    return 1;
} ## end sub create_NFA

# take a list of kernel NFA states, possibly with duplicates, and return
# a reference to an array of the fully built quasi-DFA (QDFA) states.
# as necessary.  The build is complete, except for transitions, which are
# left to be set up later.
sub assign_QDFA_state_set {
    my $grammar       = shift;
    my $kernel_states = shift;

    my $tracing = $grammar->[Marpa::Internal::Grammar::TRACING];
    my ( $trace_fh, $trace_priorities );
    if ($tracing) {
        $trace_fh = $grammar->[Marpa::Internal::Grammar::TRACE_FILE_HANDLE];
        $trace_priorities =
            $grammar->[Marpa::Internal::Grammar::TRACE_PRIORITIES];
    }

    my ( $symbols, $NFA_states, $QDFA_by_name, $QDFA ) = @{$grammar}[
        Marpa::Internal::Grammar::SYMBOLS,
        Marpa::Internal::Grammar::NFA,
        Marpa::Internal::Grammar::QDFA_BY_NAME,
        Marpa::Internal::Grammar::QDFA
    ];

    my $highest_priority;

    # Track if a state has been seen.
    # Undefined if never seen.
    # Ref to an empty array if seen, but not a result
    # Ref to an array of reset origin flag, priority
    # and NFA ID, if seen and to go into result
    my @NFA_state_seen;

    # pre-allocate the array
    $#NFA_state_seen = @{$NFA_states};

    # The work list is an array of work items.  Each work item
    # is an NFA state, following by an optional prediction flag.
    my @work_list = map { [ $_, 0 ] } @{$kernel_states};

    # Use index because we extend this list while processing it.
    my $work_list_index = -1;
    WORK_ITEM: while (1) {

        my $work_list_entry = $work_list[ ++$work_list_index ];
        last WORK_ITEM unless defined $work_list_entry;

        my ( $NFA_state, $reset ) = @{$work_list_entry};

        my $NFA_id = $NFA_state->[Marpa::Internal::NFA::ID];
        next WORK_ITEM if defined $NFA_state_seen[$NFA_id];
        my $seen = $NFA_state_seen[$NFA_id] = [];

        my $transition = $NFA_state->[Marpa::Internal::NFA::TRANSITION];

        # if we are at a nulling symbol, this NFA states does NOT go into the
        # result, but all transitions go into the work list.  There should be
        # empty transition.
        if ( $NFA_state->[Marpa::Internal::NFA::AT_NULLING] ) {
            push @work_list, map { [ $_, $reset ] }
                map { @{$_} } values %{$transition};
            next WORK_ITEM;
        }

        # If we are here, were have an NFA state NOT at a nulling symbol.
        # This NFA state goes into the result, and the empty transitions
        # go into the worklist as reset items.
        my $empty_transitions = $transition->{q{}};
        if ($empty_transitions) {
            push @work_list, map { [ $_, 1 ] } @{$empty_transitions};
        }

        $reset //= 0;
        my $priority;
        $priority = $NFA_state->[Marpa::Internal::NFA::PRIORITY]
            if $NFA_state->[Marpa::Internal::NFA::COMPLETE];
        if ( defined $priority ) {
            $highest_priority = $priority
                if not defined $highest_priority
                    or $priority gt $highest_priority;
        }
        push @{$seen}, $reset, $priority, $NFA_id;

    }    # WORK_ITEM

    $highest_priority //= pack 'NN', 0, 0;

    my @result_data = map { $_->[0] }
        sort { $a->[1] cmp $b->[1] }
        ## no critic (BuiltinFunctions::ProhibitComplexMappings)
        map {
        $_->[1] //= $highest_priority;
        [ $_, ( ( pack 'N', $_->[0] ) . $_->[1] ) ]
        }
        ## use critic
        grep { defined $_ and scalar @{$_} } @NFA_state_seen;

    # this is a fake record with an
    # "impossible" value for the reset flag to force a
    # control break at the last record
    push @result_data, [-1];

    # this will hold the QDFA state set,
    # which is the result
    my @result_states = ();

    # Below is "control break logic", which was big in the days of tape sorts
    # (yes, I'm that old).  Anyway, it's fast in Perl, will be really fast in
    # C and I think actually easier to figure out than the alternative --
    # which is something like references to arrays of hash of references.

    my $old_reset = -2;    # -2 is an "impossible" value
    my $old_priority;
    my @NFA_ids = ();

    # result data is an array of the "records"
    DATUM: for my $result_data (@result_data) {

        my ( $reset, $priority, $NFA_id ) = @{$result_data};

        # if no "control break"
        if ( $old_reset == $reset and $old_priority eq $priority ) {
            push @NFA_ids, $NFA_id;
            next DATUM;
        }

        # here what's called the "control break", where the record key changes
        if (@NFA_ids) {
            my $name = join q{,}, @NFA_ids;
            my $QDFA_state = $QDFA_by_name->{$name};

            # this is a new QDFA state -- create it
            unless ($QDFA_state) {
                my $id = scalar @{$QDFA};

                my $start_rule;
                my $lhs_list       = [];
                my $complete_rules = [];
                my $QDFA_complete  = 0;
                my $NFA_state_list = [ @{$NFA_states}[@NFA_ids] ];
                NFA_STATE: for my $NFA_state ( @{$NFA_state_list} ) {
                    next NFA_STATE
                        unless $NFA_state->[Marpa::Internal::NFA::COMPLETE];
                    $QDFA_complete = 1;
                    my $item = $NFA_state->[Marpa::Internal::NFA::ITEM];
                    my $rule = $item->[Marpa::Internal::LR0_item::RULE];
                    my $lhs  = $rule->[Marpa::Internal::Rule::LHS];
                    my ( $lhs_id, $lhs_is_start ) = @{$lhs}[
                        Marpa::Internal::Symbol::ID,
                        Marpa::Internal::Symbol::START
                    ];
                    $lhs_list->[$lhs_id] = 1;
                    push @{ $complete_rules->[$lhs_id] }, $rule;
                    $start_rule = $rule if $lhs_is_start;
                } ## end for my $NFA_state ( @{$NFA_state_list} )
                my $new_priority =
                    $QDFA_complete ? $old_priority : ( pack 'NN', 0, 0 );
                @{$QDFA_state}[
                    Marpa::Internal::QDFA::ID,
                    Marpa::Internal::QDFA::NAME,
                    Marpa::Internal::QDFA::NFA_STATES,
                    Marpa::Internal::QDFA::RESET_ORIGIN,
                    Marpa::Internal::QDFA::PRIORITY,
                    Marpa::Internal::QDFA::START_RULE,
                    ]
                    = (
                    $id, $name, $NFA_state_list, $old_reset, $new_priority,
                    $start_rule,
                    );
                $QDFA_state->[Marpa::Internal::QDFA::COMPLETE_RULES] =
                    $complete_rules;
                $QDFA_state->[Marpa::Internal::QDFA::COMPLETE_LHS] =
                    [ map { $_->[Marpa::Internal::Symbol::NAME] }
                        @{$symbols}[ grep { $lhs_list->[$_] }
                        ( 0 .. $#{$lhs_list} ) ] ];
                if ($trace_priorities) {
                    my $string_ref = Marpa::show_priority($new_priority);
                    say {$trace_fh} "Priority for QDFA state $id: ",
                        $string_ref ? 'undef' : ${$string_ref};
                }
                push @{$QDFA}, $QDFA_state;
                $QDFA_by_name->{$name} = $QDFA_state;
            }    # unless $QDFA_state

            push @result_states, $QDFA_state;

        } ## end if (@NFA_ids)

        # reset everything for the next control break
        @NFA_ids      = ($NFA_id);
        $old_reset    = $reset;
        $old_priority = $priority;

    }    # DATUM

    return \@result_states;
} ## end sub assign_QDFA_state_set

sub create_QDFA {
    my $grammar = shift;
    my ( $symbols, $symbol_hash, $NFA, $tracing ) = @{$grammar}[
        Marpa::Internal::Grammar::SYMBOLS,
        Marpa::Internal::Grammar::SYMBOL_HASH,
        Marpa::Internal::Grammar::NFA,
        Marpa::Internal::Grammar::TRACING,
    ];

    my $trace_fh;
    if ($tracing) {
        $trace_fh = $grammar->[Marpa::Internal::Grammar::TRACE_FILE_HANDLE];
    }

    my $QDFA = $grammar->[Marpa::Internal::Grammar::QDFA] = [];
    my $NFA_s0 = $NFA->[0];

    # next QDFA state to compute transitions for
    my $next_state_id = 0;

    my $initial_NFA_states =
        $NFA_s0->[Marpa::Internal::NFA::TRANSITION]->{q{}};
    if ( not defined $initial_NFA_states ) {
        croak('Empty NFA, cannot create QDFA');
    }
    $grammar->[Marpa::Internal::Grammar::START_STATES] =
        assign_QDFA_state_set( $grammar, $initial_NFA_states );

    # assign_QDFA_state_set extends this array, which we are
    # simultaneously going through and adding transitions.
    # There is no problem with the process of adding transitions
    # overtaking assign_QDFA_state_set: if we reach a point where
    # all transitions have been added, and we are at the end of @$QDFA
    # we are finished.
    while ( $next_state_id < scalar @{$QDFA} ) {

        # compute the QDFA state transitions from the transitions
        # of the NFA states of which it is composed
        my $NFA_to_states_by_symbol = {};

        my $QDFA_state = $QDFA->[ $next_state_id++ ];

        # aggregrate the transitions, by symbol, for every NFA state in this QDFA
        # state
        for my $NFA_state (
            @{ $QDFA_state->[Marpa::Internal::QDFA::NFA_STATES] } )
        {
            my $transition = $NFA_state->[Marpa::Internal::NFA::TRANSITION];
            NFA_TRANSITION:
            while ( my ( $symbol, $to_states ) = each %{$transition} ) {
                next NFA_TRANSITION if $symbol eq q{};
                push @{ $NFA_to_states_by_symbol->{$symbol} }, @{$to_states};
            }
        }    # $NFA_state

        # for each transition symbol, create the transition to the QDFA kernel state
        while ( my ( $symbol, $to_states ) =
            each %{$NFA_to_states_by_symbol} )
        {
            $QDFA_state->[Marpa::Internal::QDFA::TRANSITION]->{$symbol} =
                assign_QDFA_state_set( $grammar, $to_states );
        } ## end while ( my ( $symbol, $to_states ) = each %{...
    } ## end while ( $next_state_id < scalar @{$QDFA} )

    return;

} ## end sub create_QDFA

sub setup_academic_grammar {
    my $grammar = shift;
    my $rules   = $grammar->[Marpa::Internal::Grammar::RULES];

    # in an academic grammar, consider all rules useful
    for my $rule ( @{$rules} ) {
        $rule->[Marpa::Internal::Rule::USEFUL] = 1;
    }

    return;
} ## end sub setup_academic_grammar

# given a nullable symbol, create a nulling alias and make the first symbol non-nullable
sub alias_symbol {
    my $grammar         = shift;
    my $nullable_symbol = shift;
    my ( $symbol, $symbols, ) = @{$grammar}[
        Marpa::Internal::Grammar::SYMBOL_HASH,
        Marpa::Internal::Grammar::SYMBOLS,
    ];
    my ( $accessible, $productive, $name, $null_value ) = @{$nullable_symbol}[
        Marpa::Internal::Symbol::ACCESSIBLE,
        Marpa::Internal::Symbol::PRODUCTIVE,
        Marpa::Internal::Symbol::NAME,
        Marpa::Internal::Symbol::NULL_VALUE,
    ];

    # create the new, nulling symbol
    my $symbol_count = @{$symbols};
    my $alias_name = $nullable_symbol->[Marpa::Internal::Symbol::NAME] . '[]';
    my $alias      = [];
    $#{$alias} = Marpa::Internal::Symbol::LAST_FIELD;
    @{$alias}[
        Marpa::Internal::Symbol::ID,
        Marpa::Internal::Symbol::NAME,
        Marpa::Internal::Symbol::LHS,
        Marpa::Internal::Symbol::RHS,
        Marpa::Internal::Symbol::ACCESSIBLE,
        Marpa::Internal::Symbol::PRODUCTIVE,
        Marpa::Internal::Symbol::NULLABLE,
        Marpa::Internal::Symbol::NULLING,
        Marpa::Internal::Symbol::NULL_VALUE,
        ]
        = (
        $symbol_count, $alias_name, [], [],
        $accessible, $productive, 1, 1,
        $null_value,
        );
    push @{$symbols}, $alias;
    weaken( $symbol->{$alias_name} = $alias );

    # turn the original symbol into a non-nullable with a reference to the new alias
    @{$nullable_symbol}[
        Marpa::Internal::Symbol::NULLABLE,
        Marpa::Internal::Symbol::NULLING,
        Marpa::Internal::Symbol::NULL_ALIAS,
        ]
        = ( 0, 0, $alias, );
    return $alias;
} ## end sub alias_symbol

# For efficiency, steps in the CHAF evaluation
# work on a last-is-rest principle -- productions
# with a CHAF head always return reference to an array
# of values, of which the last value is (in turn)
# a reference to an array with the "rest" of the values.
# An empty array signals that there are no more.

# rewrite as Chomsky-Horspool-Aycock Form
sub rewrite_as_CHAF {
    my $grammar = shift;
    my ( $rules, $symbols, $old_start_symbol ) = @{$grammar}[
        Marpa::Internal::Grammar::RULES, Marpa::Internal::Grammar::SYMBOLS,
        Marpa::Internal::Grammar::START,
    ];

    # add null aliases to symbols which need them
    my $symbol_count = @{$symbols};
    SYMBOL: for my $ix ( 0 .. ( $symbol_count - 1 ) ) {
        my $symbol = $symbols->[$ix];
        my ( $productive, $accessible, $nulling, $nullable, $null_alias ) =
            @{$symbol}[
            Marpa::Internal::Symbol::PRODUCTIVE,
            Marpa::Internal::Symbol::ACCESSIBLE,
            Marpa::Internal::Symbol::NULLING,
            Marpa::Internal::Symbol::NULLABLE,
            Marpa::Internal::Symbol::NULL_ALIAS
            ];

        # not necessary is the symbol already has a null
        # alias
        next SYMBOL if $null_alias;

        #  we don't bother with unreachable symbols
        next SYMBOL unless $productive;
        next SYMBOL unless $accessible;

        # look for proper nullable symbols
        next SYMBOL if $nulling;
        next SYMBOL unless $nullable;

        alias_symbol( $grammar, $symbol );
    } ## end for my $ix ( 0 .. ( $symbol_count - 1 ) )

    # mark, or create as needed, the useful rules

    # get the initial rule count -- new rules will be added and we don't iterate
    # over them
    my $rule_count = @{$rules};
    RULE: for my $rule_id ( 0 .. ( $rule_count - 1 ) ) {
        my $rule = $rules->[$rule_id];
        my ( $lhs, $rhs, $productive, $accessible, $nulling, $nullable,
            $priority )
            = @{$rule}[
            Marpa::Internal::Rule::LHS,
            Marpa::Internal::Rule::RHS,
            Marpa::Internal::Rule::PRODUCTIVE,
            Marpa::Internal::Rule::ACCESSIBLE,
            Marpa::Internal::Rule::NULLING,
            Marpa::Internal::Rule::NULLABLE,
            Marpa::Internal::Rule::PRIORITY,
            ];
        my ( $user_priority, $internal_priority ) = unpack 'NN', $priority;

        # unreachable and nulling rules are useless
        next RULE unless $productive;
        next RULE unless $accessible;
        next RULE if $nulling;

        # Keep track of whether the lhs side of any new rules we create should
        # be nullable.  If any symbol in a production is not nullable, the lhs
        # is not nullable.  If the original production is nullable, all symbols
        # are nullable, all subproductions will be, and all new lhs's should be.
        # But even if the original production is not nullable, some of the
        # subproductions may be.  These will always be in a series starting from
        # the far right.

        # Going from right to left,
        # once the first non-nullable symbol is encountered,
        # that subproduction is non-nullable,
        # that lhs will be non-nullable, and since that
        # new lhs is on the far rhs of subsequent (going left) subproductions,
        # all subsequent subproductions and their lhs's will be non-nullable.
        #
        # Finally, in one more complication, remember that the nullable flag
        # was unset if a nullable was aliased.  So we need to check both the
        # NULL_ALIAS (for proper nullables) and the NULLING flags to see if
        # the original rule was nullable.

        my $last_nonnullable = -1;
        my $proper_nullables = [];
        RHS_SYMBOL: for my $ix ( 0 .. $#{$rhs} ) {
            my $symbol = $rhs->[$ix];
            my ( $null_alias, $rhs_symbol_nulling, $null_value ) = @{$symbol}[
                Marpa::Internal::Symbol::NULL_ALIAS,
                Marpa::Internal::Symbol::NULLING,
                Marpa::Internal::Symbol::NULL_VALUE,
            ];
            next RHS_SYMBOL if $rhs_symbol_nulling;
            if ($null_alias) {
                push @{$proper_nullables}, $ix;
                next RHS_SYMBOL;
            }
            $last_nonnullable = $ix;
        } ## end for my $ix ( 0 .. $#{$rhs} )

        # we found no properly nullable symbols in the RHS, so this rule is useful without
        # any changes
        if ( @{$proper_nullables} == 0 ) {
            $rule->[Marpa::Internal::Rule::USEFUL] = 1;
            next RULE;
        }

        # The left hand side of the first subproduction is the lhs of the original rule
        my $subp_lhs   = $lhs;
        my $subp_start = 0;

        # break this production into subproductions with a fixed number of proper nullables,
        # then factor out the proper nullables into a set of productions
        # with only non-nullable and nulling symbols.
        SUBPRODUCTION: while (1) {

            my $subp_end;
            my $proper_nullable0      = $proper_nullables->[0];
            my $subp_proper_nullable0 = $proper_nullable0 - $subp_start;
            my $proper_nullable1;
            my $subp_proper_nullable1;
            my $subp_factor0_rhs;
            my $next_subp_lhs;

            SETUP_SUBPRODUCTION: {

                if ( @{$proper_nullables} == 1 ) {
                    $subp_end = $#{$rhs};
                    $subp_factor0_rhs =
                        [ @{$rhs}[ $subp_start .. $subp_end ] ];
                    $proper_nullables = [];
                    last SETUP_SUBPRODUCTION;
                } ## end if ( @{$proper_nullables} == 1 )

                $proper_nullable1      = $proper_nullables->[1];
                $subp_proper_nullable1 = $proper_nullable1 - $subp_start;

                if ( @{$proper_nullables} == 2 ) {
                    $subp_end = $#{$rhs};
                    $subp_factor0_rhs =
                        [ @{$rhs}[ $subp_start .. $subp_end ] ];
                    $proper_nullables = [];
                    last SETUP_SUBPRODUCTION;
                } ## end if ( @{$proper_nullables} == 2 )

                # The following subproduction is non-nullable.
                # TODO: Has this code been tried yet? ( 15 Jan 2008)
                if ( $proper_nullable1 < $last_nonnullable ) {
                    $subp_end = $proper_nullable1;
                    splice @{$proper_nullables}, 0, 2;

                    my $unique_name_piece = sprintf '[x%x]',
                        (
                        scalar
                            @{ $grammar->[Marpa::Internal::Grammar::SYMBOLS] }
                        );
                    $next_subp_lhs = assign_symbol( $grammar,
                              $lhs->[Marpa::Internal::Symbol::NAME] . '[R'
                            . $rule_id . q{:}
                            . ( $subp_end + 1 ) . ']'
                            . $unique_name_piece );
                    @{$next_subp_lhs}[
                        Marpa::Internal::Symbol::NULLABLE,
                        Marpa::Internal::Symbol::ACCESSIBLE,
                        Marpa::Internal::Symbol::PRODUCTIVE,
                        Marpa::Internal::Symbol::NULLING,
                        ]
                        = ( 0, 1, 1, 0 );
                    $subp_factor0_rhs = [
                        @{$rhs}[ $subp_start .. $subp_end ],
                        $next_subp_lhs
                    ];
                    last SETUP_SUBPRODUCTION;
                } ## end if ( $proper_nullable1 < $last_nonnullable )

                # if we got this far we have 3 or more proper nullables, and the next
                # subproduction is nullable
                $subp_end = $proper_nullable1 - 1;
                shift @{$proper_nullables};

                my $unique_name_piece = sprintf '[x%x]',
                    (
                    scalar @{ $grammar->[Marpa::Internal::Grammar::SYMBOLS] }
                    );

                #<<< perltidy gets confused here
                $next_subp_lhs = assign_symbol( $grammar,
                              $lhs->[Marpa::Internal::Symbol::NAME]
                            . '[R'
                                . $rule_id . q{:}
                                . ( $subp_end + 1 )
                            . ']'
                            . $unique_name_piece
                        );
                #>>>
                @{$next_subp_lhs}[
                    Marpa::Internal::Symbol::NULLABLE,
                    Marpa::Internal::Symbol::ACCESSIBLE,
                    Marpa::Internal::Symbol::PRODUCTIVE,
                    Marpa::Internal::Symbol::NULLING,
                    ]
                    = ( 1, 1, 1, 0, );
                my $nulling_subp_lhs =
                    alias_symbol( $grammar, $next_subp_lhs );
                $nulling_subp_lhs->[Marpa::Internal::Symbol::IS_CHAF_NULLING]
                    = [ @{$rhs}[ ( $subp_end + 1 ) .. $#{$rhs} ] ];
                $subp_factor0_rhs =
                    [ @{$rhs}[ $subp_start .. $subp_end ], $next_subp_lhs ];

            }    # SETUP_SUBPRODUCTION

            my $factored_rhs = [$subp_factor0_rhs];

            FACTOR: {

                # We have additional factored productions if
                # 1) there is more than one proper nullable;
                # 2) there's only one, but replacing it with a nulling symbol will
                #    not make the entire production nulling
                #
                # Here and below we use the nullable flag to establish whether a
                # factored subproduction rhs would be nulling, on this principle:
                #
                # If substituting nulling symbols for all proper nullables does not
                # make a production nulling, then it is not nullable, and vice versa.

                last FACTOR if $nullable and not defined $proper_nullable1;

                # The second factored production, with a nulling symbol substituted for
                # the first proper nullable.
                # and nulling it would make this factored subproduction nulling, don't
                # bother.
                $factored_rhs->[1] = [ @{$subp_factor0_rhs} ];
                $factored_rhs->[1]->[$subp_proper_nullable0] =
                    $subp_factor0_rhs->[$subp_proper_nullable0]
                    ->[Marpa::Internal::Symbol::NULL_ALIAS];

                # The third factored production, with a nulling symbol replacing the
                # second proper nullable.  Make sure there ARE two proper nullables.
                last FACTOR unless defined $proper_nullable1;
                $factored_rhs->[2] = [ @{$subp_factor0_rhs} ];
                $factored_rhs->[2]->[$subp_proper_nullable1] =
                    $subp_factor0_rhs->[$subp_proper_nullable1]
                    ->[Marpa::Internal::Symbol::NULL_ALIAS];

                # The fourth and last factored production, with a nulling symbol replacing
                # both proper nullables.  We don't include it if it results in a nulling
                # production.
                last FACTOR if $nullable;
                $factored_rhs->[3] = [ @{ $factored_rhs->[2] } ];
                $factored_rhs->[3]->[$subp_proper_nullable0] =
                    $subp_factor0_rhs->[$subp_proper_nullable0]
                    ->[Marpa::Internal::Symbol::NULL_ALIAS];

            }    # FACTOR

            for my $ix ( 0 .. $#{$factored_rhs} ) {
                my $factor_rhs = $factored_rhs->[$ix];

                # No need to bother putting together values
                # if the rule's closure is not defined
                # and the values would all be discarded

                # figure out which closure to use
                # if the LHS is the not LHS of the original rule, we have a
                # special CHAF header
                my $has_chaf_lhs = ( $subp_lhs != $lhs );

                # if a CHAF LHS was created for the next subproduction,
                # there is a CHAF continuation for this subproduction.
                # It applies to this factor if there is one of the first two
                # factors of more than two.
                my $has_chaf_rhs = $next_subp_lhs;

                # Add new rule.   In assigning internal priority:
                # The first factored production is
                # highest, last is lowest, but middle two are
                # reversed.
                my $new_rule =
                    add_rule( $grammar, $subp_lhs, $factor_rhs, undef,
                    ( pack 'NN', $user_priority, @{ [qw(3 1 2 0)] }[$ix] ) );

                @{$new_rule}[
                    Marpa::Internal::Rule::USEFUL,
                    Marpa::Internal::Rule::ACCESSIBLE,
                    Marpa::Internal::Rule::PRODUCTIVE,
                    Marpa::Internal::Rule::NULLABLE,
                    Marpa::Internal::Rule::NULLING,
                    Marpa::Internal::Rule::HAS_CHAF_LHS,
                    Marpa::Internal::Rule::HAS_CHAF_RHS,
                    ]
                    = ( 1, 1, 1, 0, 0, $has_chaf_lhs, $has_chaf_rhs, );

                $new_rule->[Marpa::Internal::Rule::ORIGINAL_RULE] = $rule;
                $new_rule->[Marpa::Internal::Rule::ACTION] =
                    $rule->[Marpa::Internal::Rule::ACTION];

            }    # for each factored rhs

            # no more
            last SUBPRODUCTION unless $next_subp_lhs;
            $subp_lhs   = $next_subp_lhs;
            $subp_start = $subp_end + 1;
            $nullable   = $subp_start > $last_nonnullable;

        }    # SUBPRODUCTION

    }    # RULE

    # Create a new start symbol
    my ( $productive, $null_value ) = @{$old_start_symbol}[
        Marpa::Internal::Symbol::PRODUCTIVE,
        Marpa::Internal::Symbol::NULL_VALUE,
    ];
    my $new_start_symbol =
        assign_symbol( $grammar,
        $old_start_symbol->[Marpa::Internal::Symbol::NAME] . q{[']} );
    @{$new_start_symbol}[
        Marpa::Internal::Symbol::PRODUCTIVE,
        Marpa::Internal::Symbol::ACCESSIBLE,
        Marpa::Internal::Symbol::START,
        Marpa::Internal::Symbol::NULL_VALUE,
        ]
        = ( $productive, 1, 1, $null_value );

    # Create a new start rule
    my $new_start_rule =
        add_rule( $grammar, $new_start_symbol, [$old_start_symbol], undef,
        0 );
    @{$new_start_rule}[
        Marpa::Internal::Rule::PRODUCTIVE,
        Marpa::Internal::Rule::ACCESSIBLE,
        Marpa::Internal::Rule::USEFUL,
        Marpa::Internal::Rule::ACTION,
        ]
        = ( $productive, 1, 1, q{ $_[0] } );

    # If we created a null alias for the original start symbol, we need
    # to create a nulling start rule
    my $old_start_alias =
        $old_start_symbol->[Marpa::Internal::Symbol::NULL_ALIAS];
    if ($old_start_alias) {
        my $new_start_alias = alias_symbol( $grammar, $new_start_symbol );
        @{$new_start_alias}[ Marpa::Internal::Symbol::START, ] = (1);
        my $new_start_alias_rule =
            add_rule( $grammar, $new_start_alias, [], undef, 0 );

        # Nulling rules are not considered useful, but the top-level one is an exception
        @{$new_start_alias_rule}[
            Marpa::Internal::Rule::PRODUCTIVE,
            Marpa::Internal::Rule::ACCESSIBLE,
            Marpa::Internal::Rule::USEFUL,
            ]
            = ( $productive, 1, 1, );
    } ## end if ($old_start_alias)
    $grammar->[Marpa::Internal::Grammar::START] = $new_start_symbol;
    return;
} ## end sub rewrite_as_CHAF

1;

__END__

=pod

=head1 NAME

Marpa::Grammar - Marpa Grammar Objects

=head1 DESCRIPTION

Grammar objects are created with the C<new> constructor.
Rules and options may be specified when the grammar is created, or later using
the C<set> method.
Rules are most conveniently added with the C<mdl_source> named argument, which
takes a reference to a string containing an MDL grammar description as its value.
MDL (the Marpa Description Language) is detailed in L<another document|Marpa::Doc::MDL>.

MDL indirectly uses another interface, the B<plumbing interface>.
The plumbing is described in L<a document of its own|Marpa::Doc::Plumbing>.
Users who want the last word in control can use the plumbing directly,
but they will lose a lot of convenience and maintainability.
Those who need the ultimate in efficiency can get the best of both worlds by
using MDL to create a grammar,
then stringifying it,
as L<described below|"Stringifying">.
The MDL parser itself uses a stringified MDL file.

=head2 Precomputation

Marpa needs to do extensive precompution on grammars
before they can be passed on to a recognizer or an evaluator.
The user usually does not need to perform this precomputation explicitly.
By default, in any method call that adds rules or terminal to a grammar,
precomputation is done automatically.

Once a grammar has been precomputed, it is frozen against many kinds of
changes.
For example, you cannot add rules to a precomputed grammar.
When the user wishes to build a grammar over several method calls,
the default behavior of automatic precomputation is undesirable.
Automatic precomputation can be turned off with the C<precompute> method option.
There is also a C<precompute> method, which
explicitly precomputes a grammar.
With the C<precompute> method option and the C<precompute> method, the user
can dictate exactly when precomputation takes place.

=head2 Cloning

By default,
Marpa recognizers make a clone of the the grammar used to create them.
This allows several recognizers to be created from the same grammar,
without risk of one recognizer modifying the data in a way that
interferes with another.
Cloning can be overriden by using the C<clone> option to the C<new> and C<set> methods.

=head2 Stringifying

Marpa can turn the grammar into a string with
Marpa's C<stringify> method.
A stringified grammar is a string in every sense and
can, for instance, be written to a file.

Marpa's C<unstringify> static method takes a stringified grammar,
C<eval>'s it,
then tweaks it a bit to create a properly set-up grammar object.
A subsequent Marpa process can read this file, C<unstringify> the string,
and continue the parse.
Using a stringified grammar eliminates the overhead both of parsing MDL and of precomputation.
As mentioned, where efficiency is a major consideration, this will
usually be better than using the
plumbing interface.

=head1 METHODS

=head2 new

=begin Marpa::Test::Display:

## next display
in_file($_, 'author.t/misc.t');

=end Marpa::Test::Display:

    my $grammar = new Marpa::Grammar();

Z<>

=begin Marpa::Test::Display:

## next display
in_file($_, 't/equation_s.t');

=end Marpa::Test::Display:

    my $grammar = new Marpa::Grammar(
	{ max_parses => 10, mdl_source => \$source, } );

C<Marpa::Recognizer::new> has one, optional, argument --
a reference to a hash of named arguments.
It returns a new grammar object or throws an exception.

Named arguments can be Marpa options.
For these see L<Marpa::Doc::Options>.
In addition to the Marpa options,
the C<mdl_source> named argument,
the C<precompute> named argument,
and the named arguments of the plumbing interface are also allowed.
For details of the plumbing and its named arguments, see L<Marpa::Doc::Plumbing>.

The value of the C<mdl_source> named argument should be
a B<reference> to a string containing a description of
the grammar in the L<Marpa Demonstration Language|Marpa::MDL>.
Either the C<mdl_source> named argument or the plumbing arguments may be used
to build a grammar,
but both cannot be used to build the same grammar object.

The value of the C<precompute> named argument is interpreted as a Boolean.
If true, and if any rules or terminals were added to the grammar in the method
call, then the grammar is precomputed.
This is the default behavior.
If, in a C<new> or C<set> method call, the C<precompute> named argument is set to a false value,
precomputation will not be done by that method call.

In the C<new> and C<set> methods,
a Marpa option can be specified both directly,
as a named argument to the method,
and indirectly,
in the MDL grammar description supplied as the value of an C<mdl_source> argument.
When that happens, the value in the MDL description is applied first,
and value supplied with the method's named argument is applied after the MDL is processed.
This fits the usual intent, which is for named arguments to override MDL settings.
However, this also
means that trace settings won't be in effect until after the grammar description is
processed, and that can be too late for some of the traces.
For a way around this, see L<the C<set> method|"set">.

=head2 set

=begin Marpa::Test::Display:

## next display
in_file($_, 't/ah_s.t');

=end Marpa::Test::Display:

    $grammar->set( { mdl_source => \$source } );

The C<set> method takes as its one, required, argument a reference to a hash of named arguments.
It allows Marpa options,
the C<precompute> named argument,
the C<mdl_source> named argument,
and the plumbing arguments
to be specified for an already existing grammar object.
The effect of these arguments is as described above
for L<the C<new> method call|"new">.
C<set> either returns true or throws an exception.

The C<set> method call can be used to control the order in which named arguments are applied.
In particular, some
tracing options need to be turned on prior to specifying the grammar.
To do this, a new grammar object can be created with the trace options set,
but without a grammar specification.
Once the constructor returns, tracing will be in effect,
and the C<set> method can be used to specify the grammar,
using either the C<mdl_source> named argument or the plumbing
arguments.

=head2 precompute

=begin Marpa::Test::Display:

## next display
in_file($_, 't/ah_s.t');

=end Marpa::Test::Display:

    $grammar->precompute();

The C<precompute> method performs Marpa's precomputations on a grammar.
It returns the grammar object or throws an exception.

It is usually not necessary for the user to call C<precompute> explicitly.
Precomputation is done automatically by the C<new> and C<set> methods
when any rule or terminal is added to a grammar.
The C<precompute> method, along with the C<precompute> option to
the C<new> and C<set> methods is used to override the default
behavior.
For more details, see L<above|"Precomputation">.

=head2 stringify

=begin Marpa::Test::Display:

## next display
in_file($_, 'bin/mdl');

=end Marpa::Test::Display:

    my $stringified_grammar = $grammar->stringify();

The C<stringify> method takes as its single argument a grammar object
and converts it into a string.
It returns a reference to the string.
The string is created 
using L<Data::Dumper>.
On failure, C<stringify> throws an exception.

=head2 unstringify

=begin Marpa::Test::Display:

## start display
## next display
is_file($_, 'author.t/misc.t', 'unstringify snippet');

=end Marpa::Test::Display:

    $grammar = Marpa::Grammar::unstringify( $stringified_grammar, $trace_fh );

    $grammar = Marpa::Grammar::unstringify($stringified_grammar);

=begin Marpa::Test::Display:

## end display

=end Marpa::Test::Display:

The C<unstringify> static method takes a reference to a stringified grammar as its first
argument.
Its second, optional, argument is a file handle.
The file handle argument will be used both as the unstringified grammar's trace file handle,
and for any trace messages produced by C<unstringify> itself.
C<unstringify> returns the unstringified grammar object unless it throws an
exception.

If the trace file handle argument is omitted,
it defaults to C<STDERR>
and the unstringified grammar's trace file handle reverts to the default for a new
grammar, which is also C<STDERR>.
The trace file handle argument is necessary because in the course of compilation,
the grammar's original trace file handle may have been lost.
For example, a stringified grammar can be written to a file and emailed.
Marpa cannot rely on finding the original trace file handle available and open
when a stringified grammar is unstringified.

When Marpa deep copies grammars internally, it uses the C<stringify> and C<unstringify> methods.
To preserve the trace file handle of the original grammar,
Marpa first copies the handle to a temporary,
then restores the handle using the C<trace_file_handle> argument of C<unstringify>.

=head2 clone

=begin Marpa::Test::Display:

## next 2 displays
in_file($_, 'author.t/misc.t');

=end Marpa::Test::Display:

    my $cloned_grammar = $grammar->clone();

The C<clone> method creates a useable copy of a grammar object.
It returns a successfully cloned grammar object,
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
