package Marpa::Perl;

use 5.010;
use strict;
use warnings;

BEGIN {
    our $VERSION = '0.208000';
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

my %perl_type_by_cast = (
    q{\\} => 'REFGEN',
    q{$}  => 'DOLLAR',
    q{@}  => 'ATSIGN',
);

my %perl_type_by_structure = (
    q{(} => 'LPAREN',
    q{)} => 'RPAREN',
    q{[} => 'LSQUARE',
    q{]} => 'RSQUARE',
    q[{] => 'LCURLY',
    q[}] => 'RCURLY',
    q{;} => 'SEMI',
);

my %perl_type_by_op = (
    q{->}  => 'ARROW',       # 1
    q{--}  => 'PREDEC',      # 2
    q{++}  => 'PREINC',      # 2
    q{**}  => 'POWOP',       # 3
    q{~}   => 'TILDE',       # 4
    q{!}   => 'BANG',        # 4
    q{\\}  => 'REFGEN',      # 4
    q{=~}  => 'MATCHOP',     # 5
    q{!~}  => 'MATCHOP',     # 5
    q{/}   => 'MULOP',       # 6
    q{*}   => 'MULOP',       # 6
    q{%}   => 'MULOP',       # 6
    q{x}   => 'MULOP',       # 6
    q{-}   => 'MINUS',       # 7
    q{.}   => 'ADDOP',       # 7
    q{+}   => 'PLUS',        # 7
    q{<<}  => 'SHIFTOP',     # 8
    q{>>}  => 'SHIFTOP',     # 8
    q{-A}  => 'UNIOP',       # 9
    q{-b}  => 'UNIOP',       # 9
    q{-B}  => 'UNIOP',       # 9
    q{-c}  => 'UNIOP',       # 9
    q{-C}  => 'UNIOP',       # 9
    q{-d}  => 'UNIOP',       # 9
    q{-e}  => 'UNIOP',       # 9
    q{-f}  => 'UNIOP',       # 9
    q{-g}  => 'UNIOP',       # 9
    q{-k}  => 'UNIOP',       # 9
    q{-l}  => 'UNIOP',       # 9
    q{-M}  => 'UNIOP',       # 9
    q{-o}  => 'UNIOP',       # 9
    q{-O}  => 'UNIOP',       # 9
    q{-p}  => 'UNIOP',       # 9
    q{-r}  => 'UNIOP',       # 9
    q{-R}  => 'UNIOP',       # 9
    q{-s}  => 'UNIOP',       # 9
    q{-S}  => 'UNIOP',       # 9
    q{-t}  => 'UNIOP',       # 9
    q{-T}  => 'UNIOP',       # 9
    q{-u}  => 'UNIOP',       # 9
    q{-w}  => 'UNIOP',       # 9
    q{-W}  => 'UNIOP',       # 9
    q{-x}  => 'UNIOP',       # 9
    q{-X}  => 'UNIOP',       # 9
    q{-z}  => 'UNIOP',       # 9
    q{ge}  => 'RELOP',       # 10
    q{gt}  => 'RELOP',       # 10
    q{le}  => 'RELOP',       # 10
    q{lt}  => 'RELOP',       # 10
    q{<=}  => 'RELOP',       # 10
    q{<}   => 'RELOP',       # 10
    q{>=}  => 'RELOP',       # 10
    q{>}   => 'RELOP',       # 10
    q{cmp} => 'EQOP',        # 11
    q{eq}  => 'EQOP',        # 11
    q{ne}  => 'EQOP',        # 11
    q{~~}  => 'EQOP',        # 11
    q{<=>} => 'EQOP',        # 11
    q{==}  => 'EQOP',        # 11
    q{!=}  => 'EQOP',        # 11
    q{&}   => 'BITANDOP',    # 12
    q{^}   => 'BITOROP',     # 13
    q{|}   => 'BITOROP',     # 13
    q{&&}  => 'ANDAND',      # 14
    q{||}  => 'OROR',        # 15
    q{//}  => 'DORDOR',      # 15
    q{..}  => 'DOTDOT',      # 16
    q{...} => 'YADAYADA',    # 17
    q{:}   => 'COLON',       # 18
    q{?}   => 'QUESTION',    # 18
    q{^=}  => 'ASSIGNOP',    # 19
    q{<<=} => 'ASSIGNOP',    # 19
    q{=}   => 'ASSIGNOP',    # 19
    q{>>=} => 'ASSIGNOP',    # 19
    q{|=}  => 'ASSIGNOP',    # 19
    q{||=} => 'ASSIGNOP',    # 19
    q{-=}  => 'ASSIGNOP',    # 19
    q{/=}  => 'ASSIGNOP',    # 19
    q{.=}  => 'ASSIGNOP',    # 19
    q{*=}  => 'ASSIGNOP',    # 19
    q{**=} => 'ASSIGNOP',    # 19
    q{&=}  => 'ASSIGNOP',    # 19
    q{&&=} => 'ASSIGNOP',    # 19
    q{%=}  => 'ASSIGNOP',    # 19
    q{+=}  => 'ASSIGNOP',    # 19
    q{x=}  => 'ASSIGNOP',    # 19
    q{,}   => 'COMMA',       # 20
    q{=>}  => 'COMMA',       # 20
    q{not} => 'NOTOP',       # 22
    q{and} => 'ANDOP',       # 23
    q{or}  => 'OROP',        # 24
    q{xor} => 'DOROP',       # 24
);

my %perl_type_by_word = (
    'AUTOLOAD'         => 'PHASER -- TO BE DETERMINED',
    'BEGIN'            => 'PHASER -- TO BE DETERMINED',
    'CHECK'            => 'PHASER -- TO BE DETERMINED',
    'CORE'             => 'TO_BE_DETERMINED',
    'DESTROY'          => 'PHASER -- TO BE DETERMINED',
    'END'              => 'PHASER -- TO BE DETERMINED',
    'INIT'             => 'PHASER -- TO BE DETERMINED',
    'NULL'             => 'TO_BE_DETERMINED',
    'UNITCHECK'        => 'PHASER -- TO BE DETERMINED',
    '__DATA__'         => 'TO_BE_DETERMINED',
    '__END__'          => 'TO_BE_DETERMINED',
    '__FILE__'         => 'THING',
    '__LINE__'         => 'THING',
    '__PACKAGE__'      => 'THING',
    'abs'              => 'UNIOP',
    'accept'           => 'LSTOP',
    'alarm'            => 'UNIOP',
    'atan2'            => 'LSTOP',
    'bind'             => 'LSTOP',
    'binmode'          => 'LSTOP',
    'bless'            => 'LSTOP',
    'bless'            => 'LSTOP',
    'break'            => 'LOOPEX',
    'caller'           => 'UNIOP',
    'chdir'            => 'UNIOP',
    'chmod'            => 'LSTOP',
    'chomp'            => 'UNIOP',
    'chop'             => 'UNIOP',
    'chown'            => 'LSTOP',
    'chr'              => 'UNIOP',
    'chroot'           => 'UNIOP',
    'close'            => 'UNIOP',
    'closedir'         => 'UNIOP',
    'connect'          => 'LSTOP',
    'continue'         => 'CONTINUE',
    'cos'              => 'UNIOP',
    'crypt'            => 'LSTOP',
    'dbmclose'         => 'UNIOP',
    'dbmopen'          => 'LSTOP',
    'default'          => 'DEFAULT',
    'defined'          => 'UNIOP',
    'delete'           => 'UNIOP',
    'die'              => 'LSTOP',
    'do'               => 'DO',
    'dump'             => 'UNIOP',
    'each'             => 'UNIOP',
    'else'             => 'ELSE',
    'elsif'            => 'ELSIF',
    'endgrent'         => 'FUNC0',
    'endhostent'       => 'FUNC0',
    'endnetent'        => 'FUNC0',
    'endprotoent'      => 'FUNC0',
    'endpwent'         => 'FUNC0',
    'endservent'       => 'FUNC0',
    'eof'              => 'UNIOP',
    'eval'             => 'UNIOP',
    'exec'             => 'LSTOP',
    'exists'           => 'UNIOP',
    'exit'             => 'UNIOP',
    'exp'              => 'UNIOP',
    'fcntl'            => 'LSTOP',
    'fileno'           => 'UNIOP',
    'flock'            => 'LSTOP',
    'for'              => 'FOR',
    'foreach'          => 'FOR',
    'fork'             => 'FUNC0',
    'format'           => 'FUNC0',
    'formline'         => 'LSTOP',
    'getc'             => 'UNIOP',
    'getgrent'         => 'FUNC0',
    'getgrgid'         => 'UNIOP',
    'getgrnam'         => 'UNIOP',
    'gethostbyaddr'    => 'LSTOP',
    'gethostbyname'    => 'UNIOP',
    'gethostent'       => 'FUNC0',
    'getlogin'         => 'FUNC0',
    'getnetbyaddr'     => 'LSTOP',
    'getnetbyname'     => 'UNIOP',
    'getnetent'        => 'FUNC0',
    'getpeername'      => 'UNIOP',
    'getpgrp'          => 'UNIOP',
    'getppid'          => 'FUNC0',
    'getpriority'      => 'LSTOP',
    'getprotobyname'   => 'UNIOP',
    'getprotobynumber' => 'UNIOP',
    'getprotoent'      => 'FUNC0',
    'getpwent'         => 'FUNC0',
    'getpwnam'         => 'UNIOP',
    'getpwuid'         => 'UNIOP',
    'getservbyname'    => 'LSTOP',
    'getservbyport'    => 'LSTOP',
    'getservent'       => 'FUNC0',
    'getsockname'      => 'UNIOP',
    'getsockopt'       => 'LSTOP',
    'given'            => 'GIVEN',
    'glob'             => 'UNIOP',
    'gmtime'           => 'UNIOP',
    'goto'             => 'LOOPEX',
    'grep'             => 'LSTOP',
    'hex'              => 'UNIOP',
    'if'               => 'IF',
    'import'    => 'LSTOP',    # not really a keyword, but make it a LSTOP
    'index'     => 'LSTOP',
    'int'       => 'UNIOP',
    'ioctl'     => 'LSTOP',
    'join'      => 'LSTOP',
    'keys'      => 'UNIOP',
    'kill'      => 'LSTOP',
    'last'      => 'LOOPEX',
    'lc'        => 'UNIOP',
    'lcfirst'   => 'UNIOP',
    'length'    => 'UNIOP',
    'link'      => 'LSTOP',
    'listen'    => 'LSTOP',
    'local'     => 'LOCAL',
    'localtime' => 'UNIOP',
    'lock'      => 'UNIOP',
    'log'       => 'UNIOP',
    'lstat'     => 'UNIOP',
    'm'           => 'QUOTEABLE -- TO BE DETERMINED',
    'map'         => 'LSTOP',
    'mkdir'       => 'LSTOP',
    'msgctl'      => 'LSTOP',
    'msgget'      => 'LSTOP',
    'msgrcv'      => 'LSTOP',
    'msgsnd'      => 'LSTOP',
    'my'          => 'MY',
    'my'          => 'MY',
    'next'        => 'LOOPEX',
    'no'          => 'USE',
    'oct'         => 'UNIOP',
    'open'        => 'LSTOP',
    'opendir'     => 'LSTOP',
    'ord'         => 'UNIOP',
    'our'         => 'MY',
    'pack'        => 'LSTOP',
    'package'     => 'PACKAGE',
    'pipe'        => 'LSTOP',
    'pop'         => 'UNIOP',
    'pos'         => 'UNIOP',
    'print'       => 'LSTOP',
    'printf'      => 'LSTOP',
    'prototype'   => 'UNIOP',
    'push'        => 'LSTOP',
    'q'           => 'QUOTEABLE -- TO BE DETERMINED',
    'qq'          => 'QUOTEABLE -- TO BE DETERMINED',
    'qr'          => 'QUOTEABLE -- TO BE DETERMINED',
    'quotemeta'   => 'UNIOP',
    'qw'          => 'QUOTEABLE -- TO BE DETERMINED',
    'qx'          => 'QUOTEABLE -- TO BE DETERMINED',
    'rand'        => 'UNIOP',
    'read'        => 'LSTOP',
    'readdir'     => 'UNIOP',
    'readline'    => 'UNIOP',
    'readlink'    => 'UNIOP',
    'readpipe'    => 'UNIOP',
    'recv'        => 'LSTOP',
    'redo'        => 'LOOPEX',
    'ref'         => 'UNIOP',
    'rename'      => 'LSTOP',
    'require'     => 'REQUIRE',
    'reset'       => 'UNIOP',
    'return'      => 'LSTOP',
    'reverse'     => 'LSTOP',
    'rewinddir'   => 'UNIOP',
    'rindex'      => 'LSTOP',
    'rmdir'       => 'UNIOP',
    's'           => 'QUOTEABLE -- TO BE DETERMINED',
    'say'         => 'LSTOP',
    'scalar'      => 'UNIOP',
    'seek'        => 'LSTOP',
    'seekdir'     => 'LSTOP',
    'select'      => 'LSTOP',
    'semctl'      => 'LSTOP',
    'semget'      => 'LSTOP',
    'semop'       => 'LSTOP',
    'send'        => 'LSTOP',
    'setgrent'    => 'FUNC0',
    'sethostent'  => 'UNIOP',
    'setnetent'   => 'UNIOP',
    'setpgrp'     => 'LSTOP',
    'setpriority' => 'LSTOP',
    'setprotoent' => 'UNIOP',
    'setpwent'    => 'FUNC0',
    'setservent'  => 'UNIOP',
    'setsockopt'  => 'LSTOP',
    'shift'       => 'UNIOP',
    'shmctl'      => 'LSTOP',
    'shmget'      => 'LSTOP',
    'shmread'     => 'LSTOP',
    'shmwrite'    => 'LSTOP',
    'shutdown'    => 'LSTOP',
    'sin'         => 'UNIOP',
    'sleep'       => 'UNIOP',
    'socket'      => 'LSTOP',
    'socketpair'  => 'LSTOP',
    'sort'        => 'LSTOP',
    'splice'      => 'LSTOP',
    'split'       => 'LSTOP',
    'sprintf'     => 'LSTOP',
    'sqrt'        => 'UNIOP',
    'srand'       => 'UNIOP',
    'stat'        => 'UNIOP',
    'state'       => 'MY',
    'study'       => 'UNIOP',
    'sub'         => 'SUB',
    'substr'      => 'LSTOP',
    'symlink'     => 'LSTOP',
    'syscall'     => 'LSTOP',
    'sysopen'     => 'LSTOP',
    'sysread'     => 'LSTOP',
    'sysseek'     => 'LSTOP',
    'system'      => 'LSTOP',
    'syswrite'    => 'LSTOP',
    'tell'        => 'UNIOP',
    'telldir'     => 'UNIOP',
    'tie'         => 'LSTOP',
    'tied'        => 'UNIOP',
    'time'        => 'FUNC0',
    'times'       => 'FUNC0',
    'tr'          => 'QUOTEABLE -- TO BE DETERMINED',
    'truncate'    => 'LSTOP',
    'uc'          => 'UNIOP',
    'ucfirst'     => 'UNIOP',
    'umask'       => 'UNIOP',
    'undef'       => 'UNIOP',
    'undef'       => 'UNIOP',
    'unless'      => 'UNLESS',
    'unlink'      => 'LSTOP',
    'unpack'      => 'LSTOP',
    'unshift'     => 'LSTOP',
    'untie'       => 'UNIOP',
    'until'       => 'UNTIL',
    'use'         => 'USE',
    'utime'       => 'LSTOP',
    'values'      => 'UNIOP',
    'vec'         => 'LSTOP',
    'wait'        => 'FUNC0',
    'waitpid'     => 'LSTOP',
    'wantarray'   => 'FUNC0',
    'warn'        => 'LSTOP',
    'when'        => 'WHEN',
    'while'       => 'WHILE',
    'write'       => 'UNIOP',
    'y'           => 'QUOTEABLE -- TO BE DETERMINED',
);

## use critic

sub Marpa::Perl::new {
    my ( $class, $gen_closure ) = @_;

    my %symbol = ();
    my @rules;
    my %closure;
    my $has_ranking_action;

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
            my ( $action, $ranking_action ) =
                $gen_closure->( $lhs, \@rhs, $action_name );
            if ( defined $action ) {
                $closure{"!$action_name"} = $action;
                push @action_arg, action => "!$action_name";
            }
            if ( defined $ranking_action ) {
                $closure{"!r!$action_name"} = $ranking_action;
                push @action_arg, ranking_action => "!r!$action_name";
                $has_ranking_action++;
            }
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

    return bless {
        grammar            => $grammar,
        closure            => \%closure,
        has_ranking_action => $has_ranking_action
    }, $class;

} ## end sub Marpa::Perl::new

my @RECCE_NAMED_ARGUMENTS =
    qw(trace_tasks trace_terminals trace_values trace_actions);

sub Marpa::Perl::parse {

    my ( $parser, $input, $hash_arg ) = @_;

    $hash_arg //= {};

    my @recce_args = ();
    HASH_ARG: while ( my ( $arg, $value ) = each %{$hash_arg} ) {
        if ( $arg ~~ \@RECCE_NAMED_ARGUMENTS ) {
            push @recce_args, $arg, $value;
            next HASH_ARG;
        }
        Carp::croak("Unknown hash arg: $arg");
    } ## end while ( my ( $arg, $value ) = each %{$hash_arg} )

    my $grammar = $parser->{grammar};
    $parser->{has_ranking_action}
        and push @recce_args, ranking_method => 'constant';

    my $recce = Marpa::Recognizer->new(
        {   grammar  => $grammar,
            mode     => 'stream',
            closures => $parser->{closure},
            @recce_args
        }
    );

    my $document = PPI::Document->new($input);
    $document->index_locations();
    my @PPI_tokens = $document->tokens();
    my @earleme_to_PPI_token;
    my $last_perl_type;
    my ( $current_earleme, $expected_tokens ) = $recce->status();

    local $Marpa::Perl::Internal::CONTEXT =
        [ \@PPI_tokens, \@earleme_to_PPI_token ];
    TOKEN:
    for (
        my $PPI_token_ix = 0;
        $PPI_token_ix <= $#PPI_tokens;
        $PPI_token_ix++
        )
    {
        $earleme_to_PPI_token[$current_earleme] //= $PPI_token_ix;
        my $token    = $PPI_tokens[$PPI_token_ix];
        my $PPI_type = ref $token;
        next TOKEN if $PPI_type eq 'PPI::Token::Whitespace';
        next TOKEN if $PPI_type eq 'PPI::Token::Comment';
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
                    $perl_type = $perl_type_by_cast{$content};
                    if ( not defined $perl_type ) {
                        die qq{Unknown $PPI_type: "$content":},
                            Marpa::Perl::default_show_location($token),
                            "\n";
                    }
                    push @tokens, [ $perl_type, $cast ];
                } ## end for my $cast ( split //xms, $content )
                last FIND_TOKENS;
            } ## end if ( $PPI_type eq 'PPI::Token::Cast' )

            if ( $PPI_type eq 'PPI::Token::Word' ) {
                my $content = $token->{content};
                $perl_type = $perl_type_by_word{$content};
                if ( not defined $perl_type ) {
                    die qq{Unknown $PPI_type: "$content":},
                        Marpa::Perl::default_show_location($token),
                        "\n";
                }
                @tokens = ( [ $perl_type, $content ] );
                last FIND_TOKENS;
            } ## end if ( $PPI_type eq 'PPI::Token::Word' )

            if ( $PPI_type eq 'PPI::Token::Operator' ) {
                my $content = $token->{content};
                $perl_type = $perl_type_by_op{$content};
                if ( not defined $perl_type ) {
                    die qq{Unknown $PPI_type: "$content":},
                        Marpa::Perl::default_show_location($token),
                        "\n";
                }
                if ( $perl_type eq 'PLUS' ) {

                    # Apply the "ruby slippers"
                    # Make the plus sign be whatever the parser
                    # wishes it was
                    my @potential_types = qw(ADDOP PLUS);
                    TYPE: for my $type (@potential_types) {
                        next TYPE if not $type ~~ $expected_tokens;
                        push @tokens, [ $type, $content, 1, 0 ];
                    }

                    last FIND_TOKENS;
                } ## end if ( $perl_type eq 'PLUS' )
                if ( $perl_type eq 'MINUS' ) {

                    # Apply the "ruby slippers"
                    # Make the plus sign be whatever the parser
                    # wishes it was
                    my @potential_types = qw(ADDOP UMINUS);
                    TYPE: for my $type (@potential_types) {
                        next TYPE if not $type ~~ $expected_tokens;
                        push @tokens, [ $type, $content, 1, 0 ];
                    }

                    last FIND_TOKENS;
                } ## end if ( $perl_type eq 'MINUS' )
                @tokens = ( [ $perl_type, $content ] );
                last FIND_TOKENS;
            } ## end if ( $PPI_type eq 'PPI::Token::Operator' )

            if ( $PPI_type eq 'PPI::Token::Structure' ) {
                my $content = $token->{content};
                $perl_type = $perl_type_by_structure{$content};
                if ( not defined $perl_type ) {
                    die qq{Unknown $PPI_type: "$content":},
                        Marpa::Perl::default_show_location($token),
                        "\n";
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
                        push @tokens, [ $type, $content, 1, 0 ];
                    }

                    last FIND_TOKENS;
                } ## end if ( $perl_type eq 'LCURLY' )
                @tokens = ( [ $perl_type, $content ] );
                last FIND_TOKENS;
            } ## end if ( $PPI_type eq 'PPI::Token::Structure' )

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

        die 'Failed at Token: ', Data::Dumper::Dumper($token),
            'Marpa::Perl did not know how to process token',
            Marpa::Perl::default_show_location($token), "\n"
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
    } ## end for ( my $PPI_token_ix = 0; $PPI_token_ix <= $#PPI_tokens...)
    $recce->end_input();
    if (wantarray) {
        my @values = ();
        while ( defined( my $value_ref = $recce->value() ) ) {
            push @values, ${$value_ref};
        }
        return @values;
    } ## end if (wantarray)
    else {
        my $value_ref = $recce->value();
        return $value_ref;
    }

} ## end sub Marpa::Perl::parse

# Context-sensitive callback for
# application-provided closures to use.
sub Marpa::Perl::token {
    Marpa::exception('No Perl context for token callback')
        if not my $context = $Marpa::Perl::Internal::CONTEXT;
    my ( $PPI_tokens, $earleme_to_token ) = @{$context};
    my $earleme = Marpa::location();
    return $PPI_tokens->[ $earleme_to_token->[$earleme] ];
} ## end sub Marpa::Perl::token

sub Marpa::Perl::default_show_location {
    my ($token) = @_;
    my $file_name = $token->logical_filename();
    my $file_description = $file_name ? qq{ file "$file_name"} : q{};
    return
          "$file_description at line "
        . $token->logical_line_number()
        . q{, column }
        . $token->column_number();
} ## end sub Marpa::Perl::default_show_location

1;
