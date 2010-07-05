package Marpa::Perl;

use 5.010;
use strict;
use warnings;

BEGIN {
    our $VERSION = '0.105_000';
}

package Marpa::Internal::Perl;

use charnames ':full';
use Scalar::Util;
use Data::Dumper ();
use Carp 1.08 ();
use English qw( -no_match_vars );
use Test::More ();
use PPI 1.206 ();
use Marpa ();

# This code is about Perl GRAMMAR.
# If you're looking here
# for a Perl SEMANTICS here,
# you won't find one.

my $reference_grammar = <<'END_OF_GRAMMAR';

# This is taken from perly.y for Perl 5.12.1
prog: prog ::= lineseq ;

# /* An ordinary block */
block: block ::= '{' lineseq '}' ;

mblock ::= '{' lineseq '}' ;

# /* A collection of "lines" in the program */
lineseq_t: lineseq ::= ;
lineseq__decl: lineseq ::= lineseq decl ;
lineseq__line: lineseq ::= lineseq line ;

# /* A "line" in the program */
line ::= label cond ;
line ::= loop ; # /* loops add their own labels */
line ::= switch  ; # /* ... and so do switches */
line ::= label case ;
line ::= label ';' ;
line__sideff: line ::= label sideff ';' ;
line ::= label PLUGSTMT ;

/* An expression which may have a side-effect */
sideff ::= error ;
sideff: sideff ::= expr ;
sideff ::= expr IF expr ;
sideff ::= expr UNLESS expr ;
sideff ::= expr WHILE expr ;
sideff ::= expr UNTIL iexpr ;
sideff ::= expr FOR expr ;
sideff ::= expr WHEN expr ;

/* else and elsif blocks */
else ::= ; /* NULL */
else ::= ELSE mblock ;
else ::= ELSIF '(' mexpr ')' mblock else ;

/* Real conditional expressions */
cond ::= IF '(' mexpr ')' mblock else ;
cond ::= UNLESS '(' miexpr ')' mblock else ;

/* Cases for a switch statement */
case ::= WHEN '(' remember mexpr ')' mblock ;
case ::= DEFAULT block ;

/* Continue blocks */
cont ::= ; /* NULL */
cont ::= CONTINUE block ;

/* Loops: while, until, for, and a bare block */
loop ::= label WHILE '(' remember texpr ')' mintro mblock cont ;
loop ::= label UNTIL '(' remember iexpr ')' mintro mblock cont ;
loop ::= label FOR MY remember my_scalar '(' mexpr ')' mblock cont ;
loop ::= label FOR scalar '(' remember mexpr ')' mblock cont ;
loop ::= label FOR '(' remember mexpr ')' mblock cont ;
loop ::= label FOR '(' remember mnexpr ';' texpr ';' mintro mnexpr ')' mblock ;
/* basically fake up an initialize-while lineseq */
loop ::= label block cont  ; /* a block is a loop that happens once */

/* Switch blocks */
switch ::= label GIVEN '(' remember mydefsv mexpr ')' mblock ;

/* determine whether there are any new my declarations */
mintro ::= ; /* NULL */

/* Normal expression */
nexpr ::= ;
nexpr ::= sideff ;

/* Boolean expression */
texpr ::= ; /* NULL means true */
texpr ::= expr ;

/* Inverted boolean expression */
iexpr ::= expr ;

/* Expression with its own lexical scope */
mexpr ::= expr ;

mnexpr ::= nexpr ;

miexpr ::= iexpr ;

/* Optional "MAIN:"-style loop labels */
label ::= ; /* empty */
label ::= LABEL ;

/* Some kind of declaration - just hang on peg in the parse tree */
decl ::= format ;
decl ::= subrout ;
decl ::= mysubrout ;
decl ::= package ;
decl ::= use ;
decl ::= peg ;

peg ::= PEG ;

format ::= FORMAT startformsub formname block ;

formname ::= WORD ;
formname ::= ; /* NULL */

/* Unimplemented "my sub foo { }" */
mysubrout ::= MYSUB startsub subname proto subattrlist subbody ;

/* Subroutine definition */
subrout ::= SUB startsub subname proto subattrlist subbody ;

startsub ::= ; /* NULL */ /* start a regular subroutine scope */

startanonsub ::= ; /* NULL */ /* start an anonymous subroutine scope */

startformsub ::= ; /* NULL */ /* start a format subroutine scope */

/* Name of a subroutine - must be a bareword, could be special */
subname ::= WORD ;

/* Subroutine prototype */
proto ::= ; /* NULL */
proto ::= THING ;

/* Optional list of subroutine attributes */
subattrlist ::= ; /* NULL */
subattrlist ::= COLONATTR THING ;
subattrlist ::= COLONATTR ;

/* List of attributes for a "my" variable declaration */
myattrlist ::= COLONATTR  THING ;
myattrlist ::= COLONATTR ;

/* Subroutine body - either null or a block */
subbody ::= block ;
subbody ::= ';' ;

package ::= PACKAGE WORD WORD ';' ;

use ::= USE startsub WORD WORD listexpr ';' ;

/* Ordinary expressions; logical combinations */

expr: expr ::= or_expr;

# %left <i_tkval> OROP DOROP
or_expr: or_expr ::= or_expr OROP and_expr ;
or_expr__dor: or_expr ::= or_expr DOROP and_expr ;
or_expr__t : or_expr ::= and_expr ;

# %left <i_tkval> ANDOP
and_expr: and_expr ::= and_expr ANDOP argexpr ;
and_expr__t: and_expr ::= argexpr ;

/* Expressions are a list of terms joined by commas */
argexpr__comma: argexpr ::= argexpr ',' ;
argexpr: argexpr ::= argexpr ',' term ;
argexpr__t: argexpr ::= term ;

/* Names of methods. May use $object->$methodname */
method ::= METHOD ;
method ::= scalar ;


# %nonassoc <i_tkval> PREC_LOW
# %nonassoc LOOPEX
# %left <i_tkval> OROP DOROP
# %left <i_tkval> ANDOP
# %right <i_tkval> NOTOP
# %nonassoc LSTOP LSTOPSUB
# %left <i_tkval> ','
# %right <i_tkval> ASSIGNOP
# %right <i_tkval> '?' ':'
# %nonassoc DOTDOT YADAYADA
# %left <i_tkval> OROR DORDOR
# %left <i_tkval> ANDAND
# %left <i_tkval> BITOROP
# %left <i_tkval> BITANDOP
# %nonassoc EQOP
# %nonassoc RELOP
# %nonassoc UNIOP UNIOPSUB
# %nonassoc REQUIRE
# %left <i_tkval> SHIFTOP
# %left ADDOP
# %left MULOP
# %left <i_tkval> MATCHOP
# %right <i_tkval> '!' '~' UMINUS REFGEN
# %right <i_tkval> POWOP
# %nonassoc <i_tkval> PREINC PREDEC POSTINC POSTDEC
# %left <i_tkval> ARROW
# %nonassoc <i_tkval> ')'
# %left <i_tkval> '('
# %left '[' '{'

# %nonassoc <i_tkval> PREC_LOW
# no terms

# %nonassoc LOOPEX
term__t: term ::= term_notop ;
term ::= LOOPEX ;  /* loop exiting command (goto, last, dump, etc) */
term ::= LOOPEX term_notop ;

# %left <i_tkval> OROP DOROP
# %left <i_tkval> ANDOP
# no terms, just expr's

# %right <i_tkval> NOTOP
term_notop__t: term_notop ::= term_listop ;
term_notop ::= NOTOP argexpr   ;                    /* not $foo */

# %nonassoc LSTOP LSTOPSUB
/* List operators */
term_listop__t: term_listop ::= term_assign ;
term_listop ::= LSTOP indirob argexpr ; /* map {...} @args or print $fh @args */
term_lstop: term_listop ::= LSTOP listexpr ; /* print @args */
term_listop ::= LSTOPSUB startanonsub block listexpr ;
term_listop ::= METHOD indirob listexpr ;              /* new Class @args */
term_assign_lstop: term_listop ::= term_cond ASSIGNOP term_listop ; /* $x = bless $x, $y */

# /* sub f(&@);   f { foo } ... */ /* ... @bar */

# %left <i_tkval> ','
# no terms

# %right <i_tkval> ASSIGNOP
/* Binary operators between terms */
term_assign__t: term_assign ::= term_cond ;
# $x = $y
term_assign: term_assign ::= term_cond ASSIGNOP term_assign ;

# %right <i_tkval> '?' ':'
term_cond__t: term_cond ::= term_dotdot ;
term_cond: term_cond ::= term_dotdot '?' term_cond ':' term_cond ;

# %nonassoc DOTDOT YADAYADA
term_dotdot__t: term_dotdot ::= term_oror ;
# $x..$y, $x...$y */
term_dotdot: term_dotdot ::= term_oror DOTDOT term_oror ;
YADAYADA: term_dotdot ::= YADAYADA ;

# %left <i_tkval> OROR DORDOR
term_oror__t: term_oror ::= term_andand ;
term_oror ::= term_oror OROR term_andand     ;                   /* $x || $y */
term_oror ::= term_oror DORDOR term_andand   ;                   /* $x // $y */

# %left <i_tkval> ANDAND
term_andand__t: term_andand ::= term_bitorop ;
term_andand ::= term_andand ANDAND term_bitorop   ;                   /* $x && $y */

# %left <i_tkval> BITOROP
term_bitorop__t: term_bitorop ::= term_bitandop;
term_bitorop ::= term_bitorop BITOROP term_bitandop  ;                   /* $x | $y */

# %left <i_tkval> BITANDOP
term_bitandop__t: term_bitandop ::= term_eqop ;
term_bitandop ::= term_bitandop BITANDOP term_eqop ;                   /* $x & $y */

# %nonassoc EQOP
term_eqop__t: term_eqop ::= term_relop ;
term_eqop ::= term_relop EQOP term_relop ;                   /* $x == $y, $x eq $y */

# %nonassoc RELOP
term_relop__t: term_relop ::= term_uniop ;
term_relop ::= term_uniop RELOP term_uniop ;                   /* $x > $y, etc. */

# %nonassoc UNIOP UNIOPSUB
term_uniop__t: term_uniop ::= term_require ;
uniop: term_uniop ::= UNIOP           ; /* Unary op, $_ implied */
term_uniop ::= UNIOP block     ;                    /* eval { foo }* */
term_uniop ::= UNIOP term_require      ;                    /* Unary op */
term_uniop ::= UNIOPSUB        ;
term_uniop ::= UNIOPSUB term_require   ;                    /* Sub treated as unop */
/* Things called with "do" */
term_uniop ::=       DO term_require ;                   /* do $filename */
/* "my" declarations, with optional attributes */
# MY has no precedence
# so apparently %prec UNIOP for term ::= myattrterm does the job
term_myattr: term_uniop ::= MY myterm myattrlist ;
term_my: term_uniop ::= MY myterm ;
term_local: term_uniop ::= LOCAL term_require ;

# %nonassoc REQUIRE
term_require__t: term_require ::= term_shiftop ;
term_require ::= REQUIRE         ;                    /* require, $_ implied */
term_require ::= REQUIRE term_shiftop    ;                    /* require Foo */

# %left <i_tkval> SHIFTOP
term_shiftop__t: term_shiftop ::= term_addop ;
term_shiftop ::= term_shiftop SHIFTOP term_addop  ;                   /* $x >> $y, $x << $y */

# %left ADDOP
term_addop__t: term_addop ::= term_mulop ;
term_addop ::= term_addop ADDOP term_mulop    ;                   /* $x + $y */

# %left MULOP
term_mulop__t: term_mulop ::= term_matchop ;
term_mulop ::= term_mulop MULOP term_matchop    ;                   /* $x * $y, $x x $y */

# %left <i_tkval> MATCHOP
term_matchop__t: term_matchop ::= term_uminus ;
term_matchop ::= term_matchop MATCHOP term_uminus  ;                   /* $x =~ /$y/ */

# %right <i_tkval> '!' '~' UMINUS REFGEN
term_uminus__t: term_uminus ::= term_powop ;
term_uminus ::= '!' term_uminus                  ;            /* !$x */
term_uminus ::= '~' term_uminus                  ;            /* ~$x */
/* Unary operators and terms */
term_uminus ::= '-' term_uminus ;            /* -$x */
term_uminus ::= '+' term_uminus ;            /* +$x */
refgen: term_uminus ::= REFGEN term_uminus ; /* \$x, \@y, \%z */

# %right <i_tkval> POWOP
term_powop__t: term_powop ::= term_increment ;
term_powop ::= term_increment POWOP term_powop    ;                   /* $x ** $y */

# %nonassoc <i_tkval> PREINC PREDEC POSTINC POSTDEC
term_increment__t: term_increment ::= term_arrow ;
term_increment ::= term_arrow POSTINC              ;            /* $x++ */
term_increment ::= term_arrow POSTDEC              ;            /* $x-- */
term_increment ::= PREINC term_arrow               ;            /* ++$x */
term_increment ::= PREDEC term_arrow               ;            /* --$x */

# %left <i_tkval> ARROW
term_arrow__t: term_arrow ::= term_hi ;
term_arrow ::= term_arrow ARROW method '(' listexprcom ')' ; /* $foo->bar(list) */
term_arrow ::= term_arrow ARROW method  ;                   /* $foo->bar */

# Able to collapse the last few
# because no RHS terms
# %nonassoc <i_tkval> ')'
# %left <i_tkval> '('
# %left '[' '{' -- no terms at this precedence
term_hi__arrow_array: term_hi ::= term_hi ARROW '[' expr ']' ; /* somearef->[$element] */
term_hi__arrow_hash: term_hi ::= term_hi ARROW '{' expr ';' '}' ; /* somehref->{bar();} */
term_hi ::= term_hi ARROW '(' ')' ;        /* $subref->() */
term_hi ::= term_hi ARROW '(' expr ')' ;   /* $subref->(@args) */
term_hi ::= DO WORD '(' ')'           ;             /* do somesub() */
term_hi ::= DO WORD '(' expr ')'      ;             /* do somesub(@args) */
term_hi ::= DO scalar '(' ')'         ;            /* do $subref () */
term_hi ::= DO scalar '(' expr ')'    ;            /* do $subref (@args) */
term_hi__parens: term_hi ::= '(' expr ')' ;
term_hi ::= '(' ')' ;
term_hi ::= term_hi '(' expr ')' ; /* $foo->{bar}->(@args) */
term_hi ::= term_hi '(' ')' ;      /* $foo->{bar}->() */
term_hi ::= amper '(' ')' ;                      /* &foo() */
term_hi ::= amper '(' expr ')' ;                 /* &foo(@args) */
term_hi ::= FUNC0 '(' ')' ;
term_hi ::= FUNC1 '(' ')'         ;               /* not () */
term_hi ::= FUNC1 '(' expr ')'    ;               /* not($foo) */
term_hi ::= PMFUNC '(' argexpr ')' ; /* m//, s///, tr/// */
term_hi ::= FUNC '(' indirob expr ')'   ;    /* print ($fh @args */
term_hi ::= FUNCMETH indirob '(' listexprcom ')' ; /* method $object (@args) */
term_hi ::= FUNC '(' listexprcom ')' ;           /* print (@args) */
anon_hash: term_hi ::= HASHBRACK expr ';' '}' ; /* { foo => "Bar" } */
anon_empty_hash: term_hi ::= HASHBRACK ';' '}' ; /* { } (';' by tokener) */
term_hi ::= ANONSUB startanonsub proto subattrlist block ;
do_block: term_hi ::= DO block ; /* do { code */
term_hi__scalar: term_hi ::= scalar ;
term_hi__star: term_hi ::= star ;
term_hi__hsh: term_hi ::= hsh  ;
term_hi__ary: term_hi ::= ary  ;
# $#x, $#{ something }
term_hi__arylen: term_hi ::= arylen  ;
term_hi__THING: term_hi ::= THING ;
/* Constructors for anonymous data */
term_hi__anon_array: term_hi ::= '[' expr ']' ;
term_hi__anon_empty_array: term_hi ::= '[' ']' ;

# Some kind of subscripted expression
term_hi ::=    star '{' expr ';' '}' ; /* *main::{something} */
array_index: term_hi  ::= scalar '[' expr ']' ; /* $array[$element] */
array_index_r: term_hi  ::= term_hi '[' expr ']' ; /* $foo->[$bar]->[$baz] */
hash_index: term_hi  ::= scalar '{' expr ';' '}' ; /* $foo->{bar();} */
hash_index_r: term_hi  ::= term_hi '{' expr ';' '}' ; /* $foo->[bar]->{baz;} */
term_hi  ::= '(' expr ')' '[' expr ']' ;          /* list slice */
term_hi  ::= '(' ')' '[' expr ']' ;               /* empty list slice! */
term_hi  ::= ary '[' expr ']' ;                   /* array slice */
term_hi  ::= ary '{' expr ';' '}' ;               /* @hash{@keys} */

term_hi  ::= amper ;                              /* &foo; */
term_hi  ::= NOAMP WORD listexpr ;                /* foo(@args) */
term_hi  ::= FUNC0           ;                    /* Nullary operator */
term_hi  ::= FUNC0SUB              ;               /* Sub treated as nullop */
term_hi  ::= WORD ;
term_hi  ::= PLUGEXPR ;

# End of list of terms

/* Things that can be "my"'d */
myterm_scalar: myterm ::= scalar ;
myterm_hash: myterm ::= hsh  ;
myterm_array: myterm ::= ary  ;

/* Basic list expressions */
# Essentially, a listexpr is a nullable argexpr
listexpr_t: listexpr ::=  ; /* NULL */
listexpr: listexpr ::= argexpr    ;

# In perly.y listexprcom occurs only inside parentheses
listexprcom ::= ; /* NULL */
listexprcom ::= expr ;
listexprcom ::= expr ',' ;

/* A little bit of trickery to make "for my $foo (@bar)" actually be lexical */
my_scalar ::= scalar ;

amper ::= '&' indirob ;

scalar: scalar ::= '$' indirob ;

ary ::= '@' indirob ;

hsh ::= '%' indirob ;

arylen ::= DOLSHARP indirob ;

star ::= '*' indirob ;

/* Indirect objects */
indirob__WORD: indirob ::= WORD ;
indirob ::= scalar ;
indirob__block: indirob ::= block ;
indirob ::= PRIVATEREF ;
END_OF_GRAMMAR

## no critic (ValuesAndExpressions::RequireInterpolationOfMetachars)

my %symbol_name = (
    q{'~'} => 'TILDE',
    q{'-'} => 'MINUS',
    q{','} => 'COMMA',
    q{';'} => 'SEMI',
    q{':'} => 'COLON',
    q{'!'} => 'BANG',
    q{'?'} => 'QUESTION',
    q{'('} => 'LPAREN',
    q{')'} => 'RPAREN',
    q{'['} => 'LSQUARE',
    q{']'} => 'RSQUARE',
    q['{'] => 'LCURLY',
    q['}'] => 'RCURLY',
    q{'@'} => 'ATSIGN',
    q{'$'} => 'DOLLAR',
    q{'*'} => 'ASTERISK',
    q{'&'} => 'AMPERSAND',
    q{'%'} => 'PERCENT',
    q{'+'} => 'PLUS',
);

my %perl_type = (
    q{=}    => 'ASSIGNOP',
    q{~}    => 'TILDE',
    q{-}    => 'MINUS',
    q{,}    => 'COMMA',
    q{=>}   => 'COMMA',
    q{->}   => 'ARROW',
    q{;}    => 'SEMI',
    q{:}    => 'COLON',
    q{!}    => 'BANG',
    q{?}    => 'QUESTION',
    q{(}    => 'LPAREN',
    q{)}    => 'RPAREN',
    q{[}    => 'LSQUARE',
    q{]}    => 'RSQUARE',
    q[{]    => 'LCURLY',
    q[}]    => 'RCURLY',
    q{@}    => 'ATSIGN',
    q{$}    => 'DOLLAR',
    q{*}    => 'ASTERISK',
    q{&}    => 'AMPERSAND',
    q{%}    => 'PERCENT',
    q{+}    => 'PLUS',
    q{\\}   => 'REFGEN',
    'do'    => 'DO',
    'my'    => 'MY',
    'undef' => 'UNIOP',
    'bless' => 'LSTOP',
);

## use critic

sub Marpa::Perl::new {
    my ( $class, $gen_closure ) = @_;

    my %symbol = ();
    my @rules;
    my %closure;

    LINE:
    for my $line ( split /\n/xms, $reference_grammar ) {
        chomp $line;
        $line =~ s/ [#] .* \z //xms;
        $line =~ s/ [\/][*] .* \z //xms;
        $line =~ s/ \A \s+ //xms;
        next LINE if $line eq q{};
        Carp::croak("Misformed line: $line")
            if $line !~ / [:][:][=] .* [;] \s* \z /xms;
        my ($action_name) = ( $line =~ /\A (\w+) \s* [:] [^:] /gxms );
        my ( $lhs, $rhs_string ) =
            ( $line =~ / \s* (\w+) \s* [:][:][=] \s* (.*) [;] \s* \z/xms );
        my @rhs = map { $symbol_name{$_} // $_ } split q{ }, $rhs_string;

        for my $symbol ( $lhs, @rhs ) {
            $symbol{$symbol} //= 0;
            if ( $symbol =~ /\W/xms ) {
                Carp::croak("Misformed symbol: $symbol");
            }
        } ## end for my $symbol ( $lhs, @rhs )
        $symbol{$lhs}++;

        # only create action for non-empty rules
        my @action_arg = ();
        if ( scalar @rhs ) {
            $action_name ||= 'MyAction::rule_' . scalar @rules;
            my $action = $gen_closure->( $lhs, \@rhs, $action_name );
            $closure{"!$action_name"} = $action;
            @action_arg = ( action => "!$action_name" );
            use strict;
        } ## end if ( scalar @rhs )
        push @rules, { lhs => $lhs, rhs => \@rhs, @action_arg };
    } ## end for my $line ( split /\n/xms, $reference_grammar )

    my $grammar = Marpa::Grammar->new(
        {   start         => 'prog',
            rules         => \@rules,
            lhs_terminals => 0,
            strip         => 0
        }
    );

    $grammar->precompute();

    return bless { grammar => $grammar, closure => \%closure }, $class;

} ## end sub Marpa::Perl::new

sub Marpa::Perl::parse {

    my ( $parser, $input, $hash_arg ) = @_;

    $hash_arg //= {};

    my @recce_args = ();
    HASH_ARG: while ( my ( $arg, $value ) = each %{$hash_arg} ) {
        if ( $arg eq 'trace_terminals' ) {
            push @recce_args, $arg, $value;
            next HASH_ARG;
        }
        if ( $arg eq 'trace_values' ) {
            push @recce_args, $arg, $value;
            next HASH_ARG;
        }
        Carp::croak("Unknown hash arg: $arg");
    } ## end while ( my ( $arg, $value ) = each %{$hash_arg} )

    my $grammar = $parser->{grammar};

    my $recce = Marpa::Recognizer->new(
        {   grammar => $grammar,
            mode    => 'stream',
            @recce_args
        }
    );

    my @PPI_tokens = ();
    my $tokenizer  = PPI::Tokenizer->new($input);
    my $last_perl_type;
    my ( $current_earleme, $expected_tokens ) = $recce->status();

    TOKEN: while ( my $token = $tokenizer->get_token() ) {
        push @{ $PPI_tokens[$current_earleme] }, $token;
        my $PPI_type = ref $token;
        next TOKEN if $PPI_type eq 'PPI::Token::Whitespace';
        my @tokens = ();
        my $perl_type;
        FIND_TOKENS: {
            if ( $PPI_type eq 'PPI::Token::Symbol' ) {
                my ( $sigil, $word ) =
                    ( $token->{content} =~ / \A ([\$]) (\w*) \z /xms );
                if ( not defined $sigil or $sigil ne q{$} ) {
                    Carp::croak(
                        'Unknown symbol type: ',
                        Data::Dumper::Dumper($token)
                    );
                    next TOKEN;
                } ## end if ( not defined $sigil or $sigil ne q{$} )
                @tokens = ( [ 'DOLLAR', $sigil ], [ 'WORD', $word ] );
                last FIND_TOKENS;
            } ## end if ( $PPI_type eq 'PPI::Token::Symbol' )
            if ( $PPI_type eq 'PPI::Token::Cast' ) {
                my $content = $token->{content};
                for my $cast ( split //xms, $content ) {
                    $perl_type = $perl_type{$content};
                    push @tokens, [ $perl_type, $cast ];
                }
                last FIND_TOKENS;
            } ## end if ( $PPI_type eq 'PPI::Token::Cast' )
            if (   $PPI_type eq 'PPI::Token::Structure'
                or $PPI_type eq 'PPI::Token::Word'
                or $PPI_type eq 'PPI::Token::Operator' )
            {
                my $content = $token->{content};
                $perl_type = $perl_type{$content};
                if ( not defined $perl_type ) {
                    Carp::croak(qq{Unknown $PPI_type: "$content"});
                }
                if ( $perl_type eq 'RCURLY' ) {
                    if ((   not defined $last_perl_type
                            or $last_perl_type ne 'SEMI'
                        )
                        and 'SEMI' ~~ $expected_tokens
                        )
                    {
                        push @tokens, [ 'SEMI', q{;} ];
                    } ## end if ( ( not defined $last_perl_type or ...))
                    push @tokens, [ $perl_type, $content ];
                    last FIND_TOKENS;
                } ## end if ( $perl_type eq 'RCURLY' )
                if ( $perl_type eq 'LCURLY' ) {
                    my @potential_types = ();
                    push @potential_types, 'LCURLY';
                    if ( not defined $last_perl_type
                        or $last_perl_type ne 'DO' )
                    {
                        push @potential_types, 'HASHBRACK';
                    }
                    TYPE: for my $type (@potential_types) {
                        next TYPE if not $type ~~ $expected_tokens;
                        push @tokens, [ $type, $content, 1, 1 ];
                    }

                    # For all but the last token, set
                    # the token offset to zero.
                    for my $ix ( 0 .. $#tokens - 1 ) {
                        $tokens[$ix]->[3] = 0;
                    }

                    last FIND_TOKENS;
                } ## end if ( $perl_type eq 'LCURLY' )
                @tokens = ( [ $perl_type, $content ] );
                last FIND_TOKENS;
            } ## end if ( $PPI_type eq 'PPI::Token::Structure' or $PPI_type...)
            if ( $PPI_type eq 'PPI::Token::Number' ) {
                my $content = $token->{content};
                @tokens = ( [ 'THING', $content + 0 ] );
                last FIND_TOKENS;
            }
            if ( $PPI_type eq 'PPI::Token::Quote::Single' ) {
                my $content = $token->{content};
                ## no critic (BuiltinFunctions::ProhibitStringyEval)
                my $string = eval $content;
                ## use critic
                Carp::Croak("eval failed: $EVAL_ERROR")
                    if not defined $string;
                @tokens = ( [ 'THING', $string ] );
                last FIND_TOKENS;
            } ## end if ( $PPI_type eq 'PPI::Token::Quote::Single' )
        } ## end FIND_TOKENS:
        Carp::croak( 'Did not process token: ', Data::Dumper::Dumper($token) )
            if not scalar @tokens;
        TOKEN_SCAN: while (1) {
            my $ix = 0;
            ( $current_earleme, $expected_tokens ) =
                $recce->tokens( \@tokens, \$ix );
            last TOKEN_SCAN if $ix >= scalar @tokens;

            # say STDERR $recce->show_progress( 0, -1 );

            Carp::croak(
                'Recognizer did not accept token: ',
                Data::Dumper::Dumper( $tokens[$ix] ),
                "\n",
                'PPI token was: ',
                Data::Dumper::Dumper($token)
            );
        } ## end while (1)
        $last_perl_type = $perl_type;
    } ## end while ( my $token = $tokenizer->get_token() )
    $recce->end_input();
    if (wantarray) {
        my $evaler = Marpa::Evaluator->new(
            { recce => $recce, closures => $parser->{closure} } );
        my @values = ();
        while ( defined( my $value_ref = $evaler->value() ) ) {
            push @values, ${$value_ref};
        }
        return @values;
    } ## end if (wantarray)
    else {
        my $value_ref = $recce->value( { closures => $parser->{closure}, } );
        return $value_ref;
    }

} ## end sub Marpa::Perl::parse

1;
