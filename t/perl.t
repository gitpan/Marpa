#!/usr/bin/perl -w

## no critic (ErrorHandling::RequireCarping);

use 5.010;
use strict;
use warnings;

use charnames ':full';
use Scalar::Util;
use Data::Dumper ();
use English qw( -no_match_vars );
use Carp 1.08 ();
use Test::More ();
Test::More::plan tests => 12;
use PPI 1.203 ();
use Marpa ();

our @OUTPUT = ();
our %SYMTAB = ( SCALAR => {} );

sub DEBUG_dump {
    say STDERR 'DEBUG: ', join "\n", @main::OUTPUT
        or die "Cannot print to STDERR: $ERRNO";
    say STDERR 'DEBUG: Symbol table: ', Data::Dumper::Dumper( \%SYMTAB )
        or die "Cannot print to STDERR: $ERRNO";
    return;
} ## end sub DEBUG_dump

# This code is about Perl GRAMMAR.
# If you're writing
# a Perl SEMANTICS, and looking for a place to start,
# you probably don't want to start here.
# The purpose of these semantics is to test the grammar -- no more.
# They are probably good for nothing else.
#
# Here are some of the defects:
#
# 1.  Not a 'safe' evaluator for code from untrusted sources.
#    'eval' is used to interpret the string constants.
#
# 2.  Most Perl semantics is not implementation and where
#     the implementation exists it often is at the toy level.
#     Basically, anything not needed to interpret
#     Data::Dumper output is ignored.
#
# 3.  No optimization.  It's fast enough for a test suite.
#
# 4.  Etc., etc., etc.  You get the idea.

sub coerce_to_R {
    my ($tagged) = @_;
    my ( $side, $v ) = @{$tagged};
    return $side eq 'R' ? $v : ${$v};
}

sub do_term_lstop {
    my ( undef, $lstop, $list_tagged ) = @_;
    die "Unimplemented lstop: $lstop" if $lstop ne 'bless';
    my $list_ref = coerce_to_R($list_tagged);
    return [ 'L', \\( bless $list_ref->[0], $list_ref->[1] ) ];
} ## end sub do_term_lstop

# term_hi : term_hi ARROW '{' expr ';' '}' ; term_hi__arrow_hash /* somehref->{bar();} */
sub do_term_hi__arrow_hash {
    my ( undef, $term, undef, undef, $element ) = @_;

    my $element_ref      = coerce_to_R($element);
    my $element_ref_type = Scalar::Util::reftype $element_ref;
    die "element in term->[element] is not an scalar: $element_ref_type"
        if $element_ref_type ne 'SCALAR';

    my ( $term_side, $term_ref ) = @{$term};
    if ( $term_side eq 'L' ) {
        $term_ref = ${$term_ref};
    }
    if (   ( my $ref_type = Scalar::Util::reftype $term_ref) ne 'REF'
        or ( my $ref_ref_type = Scalar::Util::reftype ${$term_ref} ) ne
        'HASH' )
    {
        my $type = $ref_type eq 'REF' ? "REF to $ref_ref_type" : $ref_type;
        die "term in term->[element] is not an array ref: it is $type";
    } ## end if ( ( my $ref_type = Scalar::Util::reftype $term_ref...))
    return [ 'L', \\( ${$term_ref}->{ ${$element_ref} } ) ];
} ## end sub do_term_hi__arrow_hash

# term_hi : term_hi ARROW '[' expr ']' ; term_hi__arrow_array /* somearef->[$element] */
sub do_term_hi__arrow_array {
    my ( undef, $term, undef, undef, $element ) = @_;

    my $element_ref      = coerce_to_R($element);
    my $element_ref_type = Scalar::Util::reftype $element_ref;
    die "element in term->[element] is not an scalar: $element_ref_type"
        if $element_ref_type ne 'SCALAR';

    my ( $term_side, $term_ref ) = @{$term};
    if ( $term_side eq 'L' ) {
        $term_ref = ${$term_ref};
    }
    if (   ( my $ref_type = Scalar::Util::reftype $term_ref) ne 'REF'
        or ( my $ref_ref_type = Scalar::Util::reftype ${$term_ref} ) ne
        'ARRAY' )
    {
        my $type = $ref_type eq 'REF' ? "REF to $ref_ref_type" : $ref_type;
        die "term in term->[element] is not an array ref: it is $type";
    } ## end if ( ( my $ref_type = Scalar::Util::reftype $term_ref...))
    return [ 'L', \\( ${$term_ref}->[ ${$element_ref} ] ) ];
} ## end sub do_term_hi__arrow_array

# term_hi  : scalar '{' expr ';' '}' ;  hash_index /* $foo->{bar();} */
# term_hi  : term_hi '{' expr ';' '}' ; hash_index_r /* $foo->[bar]->{baz;} */
sub do_hash_index {
    my ( undef, $term, undef, $element ) = @_;

    my $element_ref      = coerce_to_R($element);
    my $element_ref_type = Scalar::Util::reftype $element_ref;
    die "element in term->[element] is not an scalar: $element_ref_type"
        if $element_ref_type ne 'SCALAR';

    my ( $term_side, $term_ref ) = @{$term};
    if ( $term_side eq 'R' ) {
        die 'rvalue term in scalar[element] not implemented';
    }
    if (   ( my $ref_type = Scalar::Util::reftype ${$term_ref} ) ne 'REF'
        or ( my $ref_ref_type = Scalar::Util::reftype ${ ${$term_ref} } ) ne
        'HASH' )
    {
        my $type = $ref_type eq 'REF' ? "REF to $ref_ref_type" : $ref_type;
        die "scalar in scalar[element] is not an hash ref: it is $type";
    } ## end if ( ( my $ref_type = Scalar::Util::reftype ${$term_ref...}))
    return [ 'L', \\( ${ ${$term_ref} }->{ ${$element_ref} } ) ];
} ## end sub do_hash_index

sub do_array_index {
    my ( undef, $term, undef, $element ) = @_;

    my $element_ref      = coerce_to_R($element);
    my $element_ref_type = Scalar::Util::reftype $element_ref;
    die "element in term->[element] is not an scalar: $element_ref_type"
        if $element_ref_type ne 'SCALAR';

    my ( $term_side, $term_ref ) = @{$term};
    if ( $term_side eq 'R' ) {
        die 'rvalue term in scalar[element] not implemented';
    }
    if (   ( my $ref_type = Scalar::Util::reftype ${$term_ref} ) ne 'REF'
        or ( my $ref_ref_type = Scalar::Util::reftype ${ ${$term_ref} } ) ne
        'ARRAY' )
    {
        my $type = $ref_type eq 'REF' ? "REF to $ref_ref_type" : $ref_type;
        die "scalar in scalar[element] is not an hash ref: it is $type";
    } ## end if ( ( my $ref_type = Scalar::Util::reftype ${$term_ref...}))
    return [ 'L', \\( ${ ${$term_ref} }->[ ${$element_ref} ] ) ];
} ## end sub do_array_index

sub do_argexpr {
    my ( undef, $argexpr, undef, $term ) = @_;
    my $argexpr_ref = coerce_to_R($argexpr);
    my @result;
    given ( Scalar::Util::reftype $argexpr_ref) {
        when ('REF')    { push @result, ${$argexpr_ref} }
        when ('SCALAR') { push @result, ${$argexpr_ref} }
        when ('ARRAY')  { push @result, @{$argexpr_ref} }
        when ('HASH')   { push @result, %{$argexpr_ref} }
        default         { die "Unknown argexpr type: $_" }
    } ## end given
    my $term_ref = coerce_to_R($term);
    given ( Scalar::Util::reftype $term_ref) {
        when ('REF')    { push @result, ${$term_ref} }
        when ('SCALAR') { push @result, ${$term_ref} }
        when ('ARRAY')  { push @result, @{$term_ref} }
        when ('HASH')   { push @result, %{$term_ref} }
        default         { die "Unknown term type: $_" }
    } ## end given
    return [ 'L', \\@result ];
} ## end sub do_argexpr

# scalar assignment only
sub do_assign {
    my ( undef, $lhs, undef, $rhs ) = @_;
    my ( $side, $lhs_ref ) = @{$lhs};

    my $rhs_ref = coerce_to_R($rhs);

    # If the LHS is actually an rvalue,
    # it is the name of a variable
    # passed up from a 'scalar' rule.
    # In this 'toy' semantics, that's how
    # variables are "declared".
    if ( $side eq 'R' ) {
        my $name = ${$lhs_ref};
        if ( not defined $name or ref $name ) {
            die 'assignment to non-lvalue: ', Data::Dumper::Dumper($name);
        }
        my $v = ${$rhs_ref};
        $SYMTAB{SCALAR}->{$name} = \$v;
        $lhs_ref = \( $SYMTAB{SCALAR}->{$name} );
        return [ 'L', $lhs_ref ];
    } ## end if ( $side eq 'R' )

    if ( Scalar::Util::readonly ${ ${$lhs_ref} } ) {
        die 'lhs is read only!';
    }
    ${ ${$lhs_ref} } = ${$rhs_ref};
    return [ 'L', $lhs_ref ];
} ## end sub do_assign

sub do_THING {
    my ( undef, $value ) = @_;
    return [ 'R', \$value ];
}

sub do_anon_array {
    my ( undef, undef, $expr ) = @_;
    my $value_ref = coerce_to_R($expr);
    my @result    = ();
    given ( Scalar::Util::reftype $value_ref) {
        when ('SCALAR') { push @result, ${$value_ref} }
        when ('REF')    { push @result, ${$value_ref} }
        when ('ARRAY')  { push @result, @{$value_ref} }
        when ('HASH')   { push @result, %{$value_ref} }
        default         { die "Unknown expr type: $_" }
    } ## end given
    return [ 'L', \\[@result] ];
} ## end sub do_anon_array

sub do_anon_empty_array {
    return [ 'L', \\[] ];
}

sub do_anon_hash {
    my ( undef, undef, $expr ) = @_;
    my $value_ref = coerce_to_R($expr);
    my $result;
    given ( Scalar::Util::reftype $value_ref) {
        when ('REF') {
            die 'expr for anon hash cannot be REF'
        }
        when ('SCALAR') {
            die 'expr for anon hash cannot be SCALAR'
        }
        when ('ARRAY') {
            $result = {
                @{$value_ref}
            }
        }
        when ('HASH') { $result = \%{$value_ref} }
        default { die "Unknown expr type: $_" }
    } ## end given
    return [ 'R', \$result ];
} ## end sub do_anon_hash

sub do_anon_empty_hash {
    return [ 'R', \{} ];
}

# This assume that all 'my' variables
# are just ways to create
# undef lvalue's -- which is how
# Data::Dumper uses them
sub do_term_my {
    my $v = undef;
    return [ 'L', \\$v ];
}

# Very simplified here --
# References are dereferenced and passed up.
# All scalars not
# already defined are returned as strings.
# It is assumed that they will either be the only
# thing on the LHS of an assignment, or in
# a my declaration.  Data::Dumper uses my
# declarations to create undef's so the scalar
# names
# that go up to term_my's will be thrown away.
sub do_scalar {
    my ( undef, $dollar, $tagged_ob ) = @_;
    my ( $side, $ob_ref ) = @{$tagged_ob};
    if ( $side eq 'R' ) {
        my $name    = ${$ob_ref};
        my $scalars = $SYMTAB{SCALAR};
        if ( exists $scalars->{$name} ) {
            return [ 'L', \$scalars->{$name} ];
        }
        return [ 'R', \$name ];
    } ## end if ( $side eq 'R' )
    $ob_ref = ${$ob_ref};
    my $ob = ${$ob_ref};
    if ( ref $ob ) {
        return [ 'L', \$ob ];
    }
    return [ 'R', $ob ];
} ## end sub do_scalar

sub do_uniop {
    my ( undef, $op ) = @_;
    die "Unknown uniop: $op" if $op ne 'undef';
    return [ 'R', \undef ];
}

# refgen is always an rvalue
sub do_refgen {
    my ( undef, undef, $s1 ) = @_;
    return [ 'R', \coerce_to_R($s1) ];
}

# prog should always return an rvalue
sub do_prog {
    my ( undef, $s1 ) = @_;
    return [ 'R', coerce_to_R($s1) ];
}

sub symbol_1 {
    my ( undef, $s1 ) = @_;
    return $s1;
}

sub symbol_2 {
    my ( undef, undef, $s2 ) = @_;
    return $s2;
}

sub token_1 {
    my ( undef, $a ) = @_;
    return [ 'R', \$a ];
}

sub gen_debug_action {
    my ( $lhs, $rhs, $closure ) = @_;
    die "gen_debug_action, lhs=$lhs: $closure is not a closure"
        if defined $closure and ref $closure ne 'CODE';
    return sub {
        if ( not defined $closure ) {
            die 'No action defined for ',
                "$lhs ::= " . ( join q{ }, map { $_ // q{-} } @{$rhs} );
        }
        my $v = $closure->(@_);
        local $Data::Dumper::Terse  = 1;
        local $Data::Dumper::Indent = 0;

        # local $Data::Dumper::Maxdepth = 4;
        push @main::OUTPUT,
              "$lhs ::= "
            . ( join q{ }, map { $_ // q{-} } @{$rhs} ) . q{; }
            . Data::Dumper::Dumper( \$v );
        $v;
    };
} ## end sub gen_debug_action

my %unwrapped = (
    and_expr__t               => \&symbol_1,
    argexpr__comma            => \&symbol_1,
    argexpr                   => \&do_argexpr,
    argexpr__t                => \&symbol_1,
    array_index               => \&do_array_index,
    array_index_r             => \&do_array_index,
    block                     => \&symbol_2,
    do_block                  => \&symbol_2,
    expr                      => \&symbol_1,
    hash_index                => \&do_hash_index,
    hash_index_r              => \&do_hash_index,
    indirob__block            => \&symbol_1,
    indirob__WORD             => \&token_1,
    lineseq__line             => \&symbol_2,
    line__sideff              => \&symbol_2,
    listexpr                  => \&symbol_1,
    myterm_scalar             => \&symbol_1,
    or_expr__t                => \&symbol_1,
    prog                      => \&do_prog,
    refgen                    => \&do_refgen,
    scalar                    => \&do_scalar,
    sideff                    => \&symbol_1,
    term_addop__t             => \&symbol_1,
    term_andand__t            => \&symbol_1,
    term_arrow__t             => \&symbol_1,
    term_assign               => \&do_assign,
    term_assign_lstop         => \&do_assign,
    term_assign__t            => \&symbol_1,
    term_bitandop__t          => \&symbol_1,
    term_bitorop__t           => \&symbol_1,
    term_cond__t              => \&symbol_1,
    term_dotdot__t            => \&symbol_1,
    term_eqop__t              => \&symbol_1,
    term_hi__anon_array       => \&do_anon_array,
    term_hi__anon_empty_array => \&do_anon_empty_array,
    term_hi__anon_empty_hash  => \&do_anon_empty_hash,
    term_hi__anon_hash        => \&do_anon_hash,
    term_hi__arrow_array      => \&do_term_hi__arrow_array,
    term_hi__arrow_hash       => \&do_term_hi__arrow_hash,
    term_hi__parens           => \&symbol_2,
    term_hi__scalar           => \&symbol_1,
    term_hi__THING            => \&do_THING,
    term_increment__t         => \&symbol_1,
    term_listop__t            => \&symbol_1,
    term_lstop                => \&do_term_lstop,
    term_matchop__t           => \&symbol_1,
    term_mulop__t             => \&symbol_1,
    term_my                   => \&do_term_my,
    term_notop__t             => \&symbol_1,
    term_oror__t              => \&symbol_1,
    term_powop__t             => \&symbol_1,
    term_relop__t             => \&symbol_1,
    term_require__t           => \&symbol_1,
    term_shiftop__t           => \&symbol_1,
    term__t                   => \&symbol_1,
    term_uminus__t            => \&symbol_1,
    term_uniop__t             => \&symbol_1,
    uniop                     => \&do_uniop,
);

my @rules;

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
## no critic (ErrorHandling::RequireCarping);

my %symbol  = ();
my %closure = ();

LINE: while ( my $line = <DATA> ) {
    chomp $line;
    $line =~ s/ [#] .* \z //xms;
    $line =~ s/ [\/][*] .* \z //xms;
    $line =~ s/ \A \s+ //xms;
    next LINE if $line eq q{};
    die "Misformed line: $line" if $line !~ / [:] .* [;] /xms;
    my ( $lhs, $rhs_string, $action_name ) =
        ( $line =~ /\A (\w+) \s* [:] \s* (.*) [;] \s* (\w*) \s* \z/xms );
    my @rhs = map { $symbol_name{$_} // $_ } split q{ }, $rhs_string;

    for my $symbol ( $lhs, @rhs ) {
        $symbol{$symbol} //= 0;
        if ( $symbol =~ /\W/xms ) {
            say STDERR "Misformed symbol: $symbol"
                or die "Cannot print to STDERR: $ERRNO";
        }
    } ## end for my $symbol ( $lhs, @rhs )
    $symbol{$lhs}++;

    # only create action for non-empty rules
    my @action_arg = ();
    if ( scalar @rhs ) {
        $action_name ||= 'MyAction::rule_' . scalar @rules;
        my $action =
            gen_debug_action( $lhs, \@rhs, $unwrapped{$action_name} );
        $closure{$action_name} = $action;
        no strict 'refs';
        *{$action_name} = $action;
        @action_arg = ( action => $action_name );
        use strict;
    } ## end if ( scalar @rhs )
    push @rules, { lhs => $lhs, rhs => \@rhs, @action_arg };
} ## end while ( my $line = <DATA> )

our $PERL_GRAMMAR = Marpa::Grammar->new(
    {   start         => 'prog',
        rules         => \@rules,
        lhs_terminals => 0,
        strip         => 0
    }
);

$PERL_GRAMMAR->precompute();

## Tests from dumper.t

# Perlcritic cannot figure out that $a and $b are not magic variables
# for a sort comparison

# Trivial
if (1) {
    my $a = 1;
    test( [$a], [qw(a)] );
}

if (1) {
    my @c = ('c');
    my $c = \@c;
    my $b = {};
    my $a = [ 1, $b, $c ];
    $b->{a} = $a;
    $b->{b} = $a->[1];
    $b->{c} = $a->[2];

    test( [ $a, $b, $c ], [qw(a b c)] );
} ## end if (1)

if (1) {
    my $foo = {
        "abc\N{NULL}\'\efg" => "mno\N{NULL}",
        'reftest'           => \\1,
    };

    test( [$foo], [qw($foo)] );
} ## end if (1)

if (1) {
    my $foo = 5;
    my @foo = ( -10, \$foo );
    my %foo = ( a => 1, b => \$foo, c => \@foo );
    $foo{d} = \%foo;
    $foo[2] = \%foo;

    test( [ \%foo ], [qw($foo)] );
} ## end if (1)

if (1) {
    my @dogs   = qw( Fido Wags );
    my %kennel = (
        First  => \$dogs[0],
        Second => \$dogs[1],
    );
    $dogs[2] = \%kennel;
    my $mutts = \%kennel;
    eval {
        test( [ \@dogs, \%kennel, $mutts ], [qw($dogs $kennel $mutts)] );
        1;
    }
        or die "Eval failed: $EVAL_ERROR";
} ## end if (1)

if (1) {
    my $a = [];
    $a->[1] = \$a->[0];
    test( [$a], [qw($a)] );
}

if (1) {
    my $a = \\\\\'foo';
    my $b = ${ ${$a} };
    test( [ $a, $b ], [qw($a $b)] );
}

if (1) {
    ## no critic (Variables::RequireLocalizedPunctuationVars)
    my $b;
    my $a = [ { a => \$b }, { b => undef } ];
    $b = [ { c => \$b }, { d => \$a } ];
    test( [ $a, $b ], [qw($a $b)] );
} ## end if (1)

if (1) {
    my $a = [ [ [ [ \\\\\'foo' ] ] ] ];
    my $b = $a->[0][0];
    my $c = ${ ${ $b->[0][0] } };
    test( [ $a, $b, $c ], [qw($a $b $c)] );
} ## end if (1)

if (1) {
    my $f = 'pearl';
    my $e = [$f];
    my $d = { 'e' => $e };
    my $c = [$d];
    my $b = { 'c' => $c };
    my $a = { 'b' => $b };
    test( [ $a, $b, $c, $d, $e, $f ], [qw($a $b $c $d $e $f)] );
} ## end if (1)

if (1) {
    ## no critic (Variables::RequireLocalizedPunctuationVars)
    my $a;
    $a = \$a;
    my $b = [$a];
    test( [$b], [qw($b)] );
} ## end if (1)

## Test from Randal Schwartz

if (1) {
    my $x = bless { fred => 'flintstone' }, 'x';
    my $y = bless \$x, 'y';
    test( [ $x, $y ], [qw($x $y)] );
}

## no critic (Subroutines::RequireArgUnpacking)
sub test {

    my $input = Data::Dumper->new(@_)->Purity(1)->Sortkeys(1)->Dumpxs;

    # Table by type and name of data
    # All data is kept as refs.
    # For orthogonality, that includes scalars.
    %SYMTAB = ();
    @OUTPUT = ();

    my $recce = Marpa::Recognizer->new(
        {   grammar => $main::PERL_GRAMMAR,
            mode    => 'stream',
        }
    );
    my $tokenizer = PPI::Tokenizer->new( \$input );
    TOKEN: while ( my $token = $tokenizer->get_token() ) {
        my $PPI_type = ref $token;
        next TOKEN if $PPI_type eq 'PPI::Token::Whitespace';
        my @tokens = ();
        FIND_TOKENS: {
            if ( $PPI_type eq 'PPI::Token::Symbol' ) {
                my ( $sigil, $word ) =
                    ( $token->{content} =~ / \A ([\$]) (\w*) \z /xms );
                if ( not defined $sigil or $sigil ne q{$} ) {
                    say STDERR 'Unknown symbol type: ',
                        Data::Dumper::Dumper($token)
                        or die "Cannot print to STDERR: $ERRNO";
                    next TOKEN;
                } ## end if ( not defined $sigil or $sigil ne q{$} )
                @tokens = ( [ 'DOLLAR', $sigil ], [ 'WORD', $word ] );
                last FIND_TOKENS;
            } ## end if ( $PPI_type eq 'PPI::Token::Symbol' )
            if ( $PPI_type eq 'PPI::Token::Cast' ) {
                my $content = $token->{content};
                for my $cast ( split //xms, $content ) {
                    my $perl_type = $perl_type{$content};
                    push @tokens, [ $perl_type, $cast ];
                }
                last FIND_TOKENS;
            } ## end if ( $PPI_type eq 'PPI::Token::Cast' )
            if (   $PPI_type eq 'PPI::Token::Structure'
                or $PPI_type eq 'PPI::Token::Word'
                or $PPI_type eq 'PPI::Token::Operator' )
            {
                my $content   = $token->{content};
                my $perl_type = $perl_type{$content};
                if ( not defined $perl_type ) {
                    die qq{Unknown $PPI_type: "$content"};
                }
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
                die "eval failed: $EVAL_ERROR" if not defined $string;
                @tokens = ( [ 'THING', $string ] );
                last FIND_TOKENS;
            } ## end if ( $PPI_type eq 'PPI::Token::Quote::Single' )
        } ## end FIND_TOKENS:
        die 'Did not process token: ', Data::Dumper::Dumper($token)
            if not scalar @tokens;
        TOKEN_SCAN: while (1) {
            my $ix = 0;
            my ( $current_earleme, $expected_tokens ) =
                $recce->tokens( \@tokens, \$ix );
            last TOKEN_SCAN if $ix >= scalar @tokens;

            # If it wants a HASHBRACK, give it a HASHBRACK
            if ( $tokens[$ix]->[0] eq 'LCURLY'
                and 'HASHBRACK' ~~ $expected_tokens )
            {
                $tokens[$ix] = [ 'HASHBRACK', '{' ];
                next TOKEN_SCAN;
            } ## end if ( $tokens[$ix]->[0] eq 'LCURLY' and 'HASHBRACK' ~~...)
            if ( $tokens[$ix]->[0] eq 'RCURLY'
                and 'SEMI' ~~ $expected_tokens )
            {

                # Invent a semicolon if Perl wants one before
                # a RCURLY
                splice @tokens, 0, $ix, [ 'SEMI', q{;} ];
                next TOKEN_SCAN;
            } ## end if ( $tokens[$ix]->[0] eq 'RCURLY' and 'SEMI' ~~ ...)

            say STDERR $recce->show_progress( 0, -1 )
                or die "Cannot print to STDERR: $ERRNO";
            die
                'Recognizer did not accept token: ',
                Data::Dumper::Dumper( $tokens[$ix] ),
                "\n",
                'PPI token was: ',
                Data::Dumper::Dumper($token);
        } ## end while (1)
    } ## end while ( my $token = $tokenizer->get_token() )
    $recce->end_input();
    my $value_ref = $recce->value(
        {   closures => \%closure,

            # trace_values => 1
        }
    );
    if ( not defined $value_ref ) {
        die 'Valuation failed';
    }
    my @pointers = ();
    my @names    = ();
    for my $type ( sort keys %SYMTAB ) {
        my $sigil =
              $type eq 'SCALAR' ? q{$}
            : $type eq 'REF'    ? q{$}
            : $type eq 'ARRAY'  ? q{@}
            : $type eq 'HASH'   ? q{@}
            :                     q{!};
        my $symbols_by_name = $SYMTAB{$type};
        for my $name ( sort keys %{$symbols_by_name} ) {
            my $ref = $symbols_by_name->{$name};

            # The testing convention is to pass scalars directly
            $type eq 'SCALAR' and $ref = ${$ref};
            push @pointers, $ref;
            push @names,    "$sigil$name";
        } ## end for my $name ( sort keys %{$symbols_by_name} )
    } ## end for my $type ( sort keys %SYMTAB )
    my $output =
        Data::Dumper->new( \@pointers, \@names )->Purity(1)->Sortkeys(1)
        ->Dumpxs;
    Test::More::is( $output, $input );
    return;

} ## end sub test

## use critic

__END__

# This is taken from perly.y for Perl 5.12.1
prog : lineseq ; prog

# /* An ordinary block */
block : '{' lineseq '}' ; block

mblock : '{' lineseq '}' ;

# /* A collection of "lines" in the program */
lineseq : ; lineseq__t
lineseq : lineseq decl ; lineseq__decl
lineseq : lineseq line ; lineseq__line

# /* A "line" in the program */
line : label cond ;
line : loop ; # /* loops add their own labels */
line : switch  ; # /* ... and so do switches */
line : label case ;
line : label ';' ;
line : label sideff ';' ; line__sideff
line : label PLUGSTMT ;

/* An expression which may have a side-effect */
sideff : error ;
sideff : expr ; sideff
sideff : expr IF expr ;
sideff : expr UNLESS expr ;
sideff : expr WHILE expr ;
sideff : expr UNTIL iexpr ;
sideff : expr FOR expr ;
sideff : expr WHEN expr ;

/* else and elsif blocks */
else : ; /* NULL */
else : ELSE mblock ;
else : ELSIF '(' mexpr ')' mblock else ;

/* Real conditional expressions */
cond : IF '(' mexpr ')' mblock else ;
cond : UNLESS '(' miexpr ')' mblock else ;

/* Cases for a switch statement */
case : WHEN '(' remember mexpr ')' mblock ;
case : DEFAULT block ;

/* Continue blocks */
cont : ; /* NULL */
cont : CONTINUE block ;

/* Loops: while, until, for, and a bare block */
loop : label WHILE '(' remember texpr ')' mintro mblock cont ;
loop : label UNTIL '(' remember iexpr ')' mintro mblock cont ;
loop : label FOR MY remember my_scalar '(' mexpr ')' mblock cont ;
loop : label FOR scalar '(' remember mexpr ')' mblock cont ;
loop : label FOR '(' remember mexpr ')' mblock cont ;
loop : label FOR '(' remember mnexpr ';' texpr ';' mintro mnexpr ')' mblock ;
/* basically fake up an initialize-while lineseq */
loop : label block cont  ; /* a block is a loop that happens once */

/* Switch blocks */
switch : label GIVEN '(' remember mydefsv mexpr ')' mblock ;

/* determine whether there are any new my declarations */
mintro : ; /* NULL */

/* Normal expression */
nexpr : ;
nexpr : sideff ;

/* Boolean expression */
texpr : ; /* NULL means true */
texpr : expr ;

/* Inverted boolean expression */
iexpr : expr ;

/* Expression with its own lexical scope */
mexpr : expr ;

mnexpr : nexpr ;

miexpr : iexpr ;

/* Optional "MAIN:"-style loop labels */
label : ; /* empty */
label : LABEL ;

/* Some kind of declaration - just hang on peg in the parse tree */
decl : format ;
decl : subrout ;
decl : mysubrout ;
decl : package ;
decl : use ;
decl : peg ;

peg : PEG ;

format : FORMAT startformsub formname block ;

formname: WORD ;
formname : ; /* NULL */

/* Unimplemented "my sub foo { }" */
mysubrout: MYSUB startsub subname proto subattrlist subbody ;

/* Subroutine definition */
subrout : SUB startsub subname proto subattrlist subbody ;

startsub: ; /* NULL */ /* start a regular subroutine scope */

startanonsub: ; /* NULL */ /* start an anonymous subroutine scope */

startformsub: ; /* NULL */ /* start a format subroutine scope */

/* Name of a subroutine - must be a bareword, could be special */
subname : WORD ;

/* Subroutine prototype */
proto : ; /* NULL */
proto : THING ;

/* Optional list of subroutine attributes */
subattrlist: ; /* NULL */
subattrlist : COLONATTR THING ;
subattrlist : COLONATTR ;

/* List of attributes for a "my" variable declaration */
myattrlist: COLONATTR  THING ;
myattrlist : COLONATTR ;

/* Subroutine body - either null or a block */
subbody : block ;
subbody : ';' ;

package : PACKAGE WORD WORD ';' ;

use : USE startsub WORD WORD listexpr ';' ;

/* Ordinary expressions; logical combinations */

expr: or_expr; expr

# %left <i_tkval> OROP DOROP
or_expr : or_expr OROP and_expr ; or_expr
or_expr : or_expr DOROP and_expr ; or_expr__dor
or_expr : and_expr ; or_expr__t

# %left <i_tkval> ANDOP
and_expr : and_expr ANDOP argexpr ; and_expr
and_expr : argexpr ; and_expr__t

/* Expressions are a list of terms joined by commas */
argexpr : argexpr ',' ; argexpr__comma
argexpr : argexpr ',' term ; argexpr
argexpr : term ; argexpr__t

/* Names of methods. May use $object->$methodname */
method : METHOD ;
method : scalar ;


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
term : term_notop ; term__t
term : LOOPEX ;  /* loop exiting command (goto, last, dump, etc) */
term : LOOPEX term_notop ;

# %left <i_tkval> OROP DOROP
# %left <i_tkval> ANDOP
# no terms, just expr's

# %right <i_tkval> NOTOP
term_notop : term_listop ; term_notop__t
term_notop : NOTOP argexpr   ;                    /* not $foo */

# %nonassoc LSTOP LSTOPSUB
/* List operators */
term_listop : term_assign ; term_listop__t
term_listop : LSTOP indirob argexpr ; /* map {...} @args or print $fh @args */
term_listop : LSTOP listexpr ; term_lstop /* print @args */
term_listop : LSTOPSUB startanonsub block listexpr ;
term_listop  : METHOD indirob listexpr ;              /* new Class @args */
term_listop: term_cond ASSIGNOP term_listop ; term_assign_lstop /* $x = bless $x, $y */

# /* sub f(&@);   f { foo } ... */ /* ... @bar */

# %left <i_tkval> ','
# no terms

# %right <i_tkval> ASSIGNOP
/* Binary operators between terms */
term_assign : term_cond ; term_assign__t
# $x = $y
term_assign: term_cond ASSIGNOP term_assign ; term_assign

# %right <i_tkval> '?' ':'
term_cond: term_dotdot ; term_cond__t
term_cond : term_dotdot '?' term_cond ':' term_cond ; term_cond

# %nonassoc DOTDOT YADAYADA
term_dotdot : term_oror ; term_dotdot__t
# $x..$y, $x...$y */
term_dotdot : term_oror DOTDOT term_oror ; term_dotdot
term_dotdot : YADAYADA ; YADAYADA

# %left <i_tkval> OROR DORDOR
term_oror : term_andand ; term_oror__t
term_oror : term_oror OROR term_andand     ;                   /* $x || $y */
term_oror : term_oror DORDOR term_andand   ;                   /* $x // $y */

# %left <i_tkval> ANDAND
term_andand : term_bitorop ; term_andand__t
term_andand : term_andand ANDAND term_bitorop   ;                   /* $x && $y */

# %left <i_tkval> BITOROP
term_bitorop : term_bitandop; term_bitorop__t
term_bitorop : term_bitorop BITOROP term_bitandop  ;                   /* $x | $y */

# %left <i_tkval> BITANDOP
term_bitandop: term_eqop ; term_bitandop__t
term_bitandop : term_bitandop BITANDOP term_eqop ;                   /* $x & $y */

# %nonassoc EQOP
term_eqop : term_relop ; term_eqop__t
term_eqop : term_relop EQOP term_relop ;                   /* $x == $y, $x eq $y */

# %nonassoc RELOP
term_relop : term_uniop ; term_relop__t
term_relop : term_uniop RELOP term_uniop ;                   /* $x > $y, etc. */

# %nonassoc UNIOP UNIOPSUB
term_uniop : term_require ; term_uniop__t
term_uniop : UNIOP           ; uniop                   /* Unary op, $_ implied */
term_uniop : UNIOP block     ;                    /* eval { foo }* */
term_uniop : UNIOP term_require      ;                    /* Unary op */
term_uniop : UNIOPSUB        ;
term_uniop : UNIOPSUB term_require   ;                    /* Sub treated as unop */
/* Things called with "do" */
term_uniop :       DO term_require ;                   /* do $filename */
/* "my" declarations, with optional attributes */
# MY has no precedence
# so apparently %prec UNIOP for term ::= myattrterm does the job
term_uniop: MY myterm myattrlist ; term_myattr
term_uniop : MY myterm ; term_my
term_uniop : LOCAL term_require ; term_local

# %nonassoc REQUIRE
term_require : term_shiftop ; term_require__t
term_require : REQUIRE         ;                    /* require, $_ implied */
term_require : REQUIRE term_shiftop    ;                    /* require Foo */

# %left <i_tkval> SHIFTOP
term_shiftop : term_addop ; term_shiftop__t
term_shiftop : term_shiftop SHIFTOP term_addop  ;                   /* $x >> $y, $x << $y */

# %left ADDOP
term_addop: term_mulop ; term_addop__t
term_addop : term_addop ADDOP term_mulop    ;                   /* $x + $y */

# %left MULOP
term_mulop : term_matchop ; term_mulop__t
term_mulop : term_mulop MULOP term_matchop    ;                   /* $x * $y, $x x $y */

# %left <i_tkval> MATCHOP
term_matchop : term_uminus ; term_matchop__t
term_matchop : term_matchop MATCHOP term_uminus  ;                   /* $x =~ /$y/ */

# %right <i_tkval> '!' '~' UMINUS REFGEN
term_uminus : term_powop ; term_uminus__t
term_uminus : '!' term_uminus                  ;            /* !$x */
term_uminus : '~' term_uminus                  ;            /* ~$x */
/* Unary operators and terms */
term_uminus : '-' term_uminus ;            /* -$x */
term_uminus : '+' term_uminus ;            /* +$x */
term_uminus : REFGEN term_uminus ; refgen /* \$x, \@y, \%z */

# %right <i_tkval> POWOP
term_powop : term_increment ; term_powop__t
term_powop : term_increment POWOP term_powop    ;                   /* $x ** $y */

# %nonassoc <i_tkval> PREINC PREDEC POSTINC POSTDEC
term_increment : term_arrow ; term_increment__t
term_increment : term_arrow POSTINC              ;            /* $x++ */
term_increment : term_arrow POSTDEC              ;            /* $x-- */
term_increment : PREINC term_arrow               ;            /* ++$x */
term_increment : PREDEC term_arrow               ;            /* --$x */

# %left <i_tkval> ARROW
term_arrow : term_hi ; term_arrow__t
term_arrow : term_arrow ARROW method '(' listexprcom ')' ; /* $foo->bar(list) */
term_arrow : term_arrow ARROW method  ;                   /* $foo->bar */

# Able to collapse the last few
# because no RHS terms
# %nonassoc <i_tkval> ')'
# %left <i_tkval> '('
# %left '[' '{' -- no terms at this precedence
term_hi : term_hi ARROW '[' expr ']' ; term_hi__arrow_array /* somearef->[$element] */
term_hi : term_hi ARROW '{' expr ';' '}' ; term_hi__arrow_hash /* somehref->{bar();} */
term_hi : term_hi ARROW '(' ')' ;        /* $subref->() */
term_hi : term_hi ARROW '(' expr ')' ;   /* $subref->(@args) */
term_hi : DO WORD '(' ')'           ;             /* do somesub() */
term_hi : DO WORD '(' expr ')'      ;             /* do somesub(@args) */
term_hi : DO scalar '(' ')'         ;            /* do $subref () */
term_hi : DO scalar '(' expr ')'    ;            /* do $subref (@args) */
term_hi : '(' expr ')' ; term_hi__parens
term_hi : '(' ')' ;
term_hi : term_hi '(' expr ')' ; /* $foo->{bar}->(@args) */
term_hi : term_hi '(' ')' ;      /* $foo->{bar}->() */
term_hi : amper '(' ')' ;                      /* &foo() */
term_hi : amper '(' expr ')' ;                 /* &foo(@args) */
term_hi : FUNC0 '(' ')' ;
term_hi : FUNC1 '(' ')'         ;               /* not () */
term_hi : FUNC1 '(' expr ')'    ;               /* not($foo) */
term_hi : PMFUNC '(' argexpr ')' ; /* m//, s///, tr/// */
term_hi : FUNC '(' indirob expr ')'   ;    /* print ($fh @args */
term_hi : FUNCMETH indirob '(' listexprcom ')' ; /* method $object (@args) */
term_hi : FUNC '(' listexprcom ')' ;           /* print (@args) */
term_hi : HASHBRACK expr ';' '}' ; term_hi__anon_hash /* { foo => "Bar" } */
term_hi : HASHBRACK ';' '}' ; term_hi__anon_empty_hash /* { } (';' by tokener) */
term_hi : ANONSUB startanonsub proto subattrlist block ;
term_hi : DO block ; do_block /* do { code */
term_hi : scalar ; term_hi__scalar
term_hi : star ; term_hi__star
term_hi : hsh  ; term_hi__hsh
term_hi : ary  ; term_hi__ary
# $#x, $#{ something }
term_hi : arylen  ; term_hi__arylen
term_hi : THING ; term_hi__THING
/* Constructors for anonymous data */
term_hi: '[' expr ']' ; term_hi__anon_array
term_hi: '[' ']' ; term_hi__anon_empty_array

# Some kind of subscripted expression
term_hi :    star '{' expr ';' '}' ; /* *main::{something} */
term_hi  : scalar '[' expr ']' ; array_index /* $array[$element] */
term_hi  : term_hi '[' expr ']' ;  array_index_r /* $foo->[$bar]->[$baz] */
term_hi  : scalar '{' expr ';' '}' ;  hash_index /* $foo->{bar();} */
term_hi  : term_hi '{' expr ';' '}' ; hash_index_r /* $foo->[bar]->{baz;} */
term_hi  : '(' expr ')' '[' expr ']' ;          /* list slice */
term_hi  : '(' ')' '[' expr ']' ;               /* empty list slice! */
term_hi  : ary '[' expr ']' ;                   /* array slice */
term_hi  : ary '{' expr ';' '}' ;               /* @hash{@keys} */

term_hi  : amper ;                              /* &foo; */
term_hi  : NOAMP WORD listexpr ;                /* foo(@args) */
term_hi  : FUNC0           ;                    /* Nullary operator */
term_hi  : FUNC0SUB              ;               /* Sub treated as nullop */
term_hi  : WORD ;
term_hi  : PLUGEXPR ;

# End of list of terms

/* Things that can be "my"'d */
myterm : scalar ; myterm_scalar
myterm : hsh  ; myterm_hash
myterm : ary  ; myterm_array

/* Basic list expressions */
# Essentially, a listexpr is a nullable argexpr
listexpr:  ; listexpr_t /* NULL */
listexpr : argexpr    ; listexpr

# In perly.y listexprcom occurs only inside parentheses
listexprcom: ; /* NULL */
listexprcom : expr ;
listexprcom : expr ',' ;

/* A little bit of trickery to make "for my $foo (@bar)" actually be lexical */
my_scalar: scalar ;

amper : '&' indirob ;

scalar : '$' indirob ; scalar

ary : '@' indirob ;

hsh : '%' indirob ;

arylen : DOLSHARP indirob ;

star : '*' indirob ;

/* Indirect objects */
indirob : WORD ; indirob__WORD
indirob : scalar ;
indirob : block ; indirob__block
indirob : PRIVATEREF ;
