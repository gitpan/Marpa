package Marpa;

use 5.010;
use warnings;
no warnings 'recursion';
use strict;

BEGIN {
    our $VERSION = '0.001_023';
}

use integer;

use Marpa::Internal;
use Marpa::Grammar;
use Marpa::Recognizer;
use Marpa::Evaluator;

1;    # End of Marpa

__END__

=begin Apology:

The Coding Style of this Module,
Together with Some Thoughts about Coding Style in General

This is not my idea of a good, general Perl style.  But a single
coding style is not applicable in all cases.  The same coding style
quite adequate in a throw-away script would be terrible in code
intended for a critical production enviromment or maintainance
by a large, diversed staff.  The style of this module matches
its purpose, its likely future, and the likely resources for
maintaining it.

An important, but very non-standard, aim of this code is easy
translation into time-efficient C.  This is because parsers run
inside tight loops.  The rap against Earley's in particular has
always been speed.  Readability is not a good reason to write a
module that will never be used because it's too slow.

So I've written very C-ish Perl -- lots of references, avoidance
of hashes in the internals, no internal OO, etc., etc.  I don't
usually write Perl this way.  I don't think it's usually a good
idea to write Perl this way.  But as the lawyers say, circumstances
make cases.

C conversion is important because one of two things are going to
happen to Marpa: it turns out to be so slow it's difficult to use,
or it does not.  If Marpa is slow, the next thing to try is conversion
to C.  If it's fast, Marpa will be highly useful, and there will
almost certainly be demand for an even faster version -- in C++ or C.

Damian Conway's _Perl Best Practices_ is the best starting point
for thinking about Perl style, whether you agree with him or not.
I've made many exceptions due to necessity, as described above.
Many more I've no doubt made out of ignorance.  A few other exceptions
are because I can't agree with Damian.

An example of a deliberate exception I've made to Damian's guidelines:
I don't append "_ref" to the name references -- almost every variable
name in this code is a reference.  This may not be easy code to
read, but I can't believe having 90% of the variable names end in
"_ref" is going to make it any easier.  As Damian notes, his own
CPAN modules don't follow his guidelines all that closely.

=end Apology:

=head1 NAME

Marpa - General BNF Parsing (Experimental version)

=head1 SYNOPSIS

=begin Marpa::Test::Commented_Out_Display:

## start display
## next display
is_file($_, 'example/synopsis.pl');

=end Marpa::Test::Commented_Out_Display:

=begin Marpa::Test::Display:

## start display
## skip display

=end Marpa::Test::Display:

    #!perl
    
    use 5.010;
    use strict;
    use warnings;
    use English qw( -no_match_vars );
    use Marpa;

    # remember to use refs to strings
    my $value = Marpa::mdl(
        (   do { local ($RS) = undef; my $source = <DATA>; \$source; }
        ),
        \('2+2*3')
    );
    say ${$value};

    __DATA__
    semantics are perl5.  version is 0.001_020.  start symbol is Expression.

    Expression: Expression, /[*]/, Expression.  priority 200.  q{
        $_[0] * $_[2]
    }.

    Expression: Expression, /[+]/, Expression.  priority 100.  q{
        $_[0] + $_[2]
    }.

    Expression: /\d+/.  q{ $_[0] }.

=begin Marpa::Test::Display:

## end display

=end Marpa::Test::Display:

=head1 DESCRIPTION

This is alpha software.
B<This is an experimental fork from C<Parse::Marpa>.
At this point in development, the documentation is not being kept
up to date.>

If you can write a grammar in BNF, Marpa will generate a parser for it.
That means Marpa parses
left- and right-recursive grammars; all ambiguous grammars,
including infinitely ambiguous grammars;
grammars with empty rules;
and grammars with useless rules.

Here's all you need to get started:

=over 4

=item * This document.

=item * 
L<The MDL document|Marpa::Doc::MDL>.
It describes the format of Marpa's grammars.

=item * The L<Marpa::Doc::Options>
document.
This one you only need to skim, checking for
anything relevant to your application.

=back

The Marpa documents use a lot of parsing terminology.
For a quick refresher in the
standard parsing vocabulary,
there's a L<Marpa::Doc::Parse_Terms> document.
B<Defining uses> of terms are in boldface, for easy skimming.

=head2 What is in the Other Documents

If you want help debugging a grammar,
look at L<Marpa::Doc::Debugging>.
As you get into advanced applications of Marpa,
the first places to look will be the
documents for the various phases of Marpa parsing:
L<Marpa::Grammar>,
L<Marpa::Recognizer>,
and L<Marpa::Evaluator>.

A few documents describe details you may never need.
L<Marpa::Doc::Plumbing> documents Marpa's plumbing.
L<Marpa::MDL> documents utilities for converting MDL symbol
names to plumbing interface names.
L<Marpa::Lex> documents some lex actions which are used
by MDL, and which are available to users for their own lexing.

For reading Marpa's code,
it is necessary to understand Marpa's internals.
Internals knowledge can also be useful in advanced debugging.
Marpa's internals are described in 
L<Marpa::Doc::Internals>.

For those interesting in the theory behind Marpa and
the details of its programming,
L<Marpa::Doc::Algorithm> describes the algorithms,
explains how Marpa would not have been possible
without the work of others,
and details what is new with Marpa.
Details about sources (books, web pages and articles) referred to in these documents
or used in the writing of Marpa
are collected in
L<Marpa::Doc::Bibliography>.
L<Marpa::Doc::To_Do> is the list of things that might be done to
Marpa in the future.

=head2 The Easy Way

Most of Marpa's capabilities are available using a single static method:
L<C<Marpa::mdl>|/mdl>.
The C<mdl> method requires a grammar description in MDL (the Marpa Description Language) and a string.
C<mdl> parses the string according to the MDL description.
In scalar context, C<mdl> returns a reference to the value of the first parse.
In list context, it returns references to the values of all parses.
See L<below|/"mdl"> for more detail about the C<mdl> static method.

=head2 Parsing Terminology

The parsing terms in
these documents
are either explained in these documents
or are in standard use.
However, just because a parsing term is in "standard use"
doesn't mean it will be familiar.
Even if you've studied parsing,
you might not have run across that particular term,
or might not remember exactly what it meant.
I define all the terms I treat as "standard" in L<Marpa::Doc::Parse_Terms>.
The L<parse terms document|Marpa::Doc::Parse_Terms> is
designed for skimming:
the B<defining uses> of the terms are all in boldface.

If you want an
introduction to parsing concepts,
the chapter on parsing in
L<Mark Jason Dominus's
I<Higher Order Perl>|Marpa::Doc::Bibliography/"Dominus 2005">
is an excellent description in the Perl context.
It's available online.
L<Wikipedia|Marpa::Doc::Bibliography/"Wikipedia"> is also very useful.

=head2 Semantic Actions

Marpa's semantics
are specified with Perl 5 code strings, called B<actions>.
Marpa allows
rule actions, null symbol actions
and a preamble action.
It also allows lexing actions and a separate lex preamble action.

Rule and null symbol actions are calculated in
a special namespace set aside for that purpose.
The preamble action is always run first
and can be used to initialize that namespace.

Lex actions are run in a special namespace devoted to lex actions.
The special lex preamble action
can be used to initialize that namespace.

The result of an action is the result of running its Perl 5 code string.
From L<the synopsis|"SYNOPSIS">, here's a rule for an expression that does addition:

=begin Marpa::Test::Commented_Out_Display:

## next 2 displays
in_file($_, 'example/synopsis.pl');

=end Marpa::Test::Commented_Out_Display:

=begin Marpa::Test::Display:

## skip display

=end Marpa::Test::Display:

    Expression: Expression, /[+]/, Expression.

and here's its action:

=begin Marpa::Test::Display:

## skip display

=end Marpa::Test::Display:

    $_[0] + $_[2]

In rule actions, C<@_> is an array containing the values of the symbols
on the left hand side of the rule, as if they had been passed as arguments
to a subroutine.
Actions may not always be implemented as Perl subroutines, so
so please B<do not> C<return> out of an action.

Marpa is targeted to Perl 6.
When Perl 6 is ready, Perl 6 code will become its default semantics.

=head2 Null Symbol Values

Every symbol has a B<null symbol value>,
or more briefly, a B<null value>,
and this is used as the value of the symbol when it is nulled.
The default null value is a Marpa option (C<default_null_value>).
If not explicitly set, C<default_null_value> is a Perl 5 undefined.

Every symbol can have its own null symbol value.
In cases where a null symbol derives other null symbols,
only the value of the symbol highest in the null derivation is used.
For details and examples, see L<Marpa::Evaluator/"Null Symbol Values">.

=head2 Lexing

The easiest way to parse a Perl 5 string in Marpa is to use
MDL's default lexing.
MDL allows terminals to be defined either as Perl 5 regexes or,
for difficult cases, as lex actions,
which are Perl 5 code.
Unlike most parser generators,
Marpa does not require that
the regexes and lex actions result in a deterministic lexer.
It is OK with Marpa if more than one token is possible at a location,
or if possible tokens overlap.
Ambiguities encountered in lexing are passed up to Marpa's parse engine,
and dealt with there.

Marpa allows terminals to appear on the left hand side of rules.
Most parsers have a problem with this, but Marpa does not.

Marpa is not restricted to MDL's model of lexing.
Advanced users can invent new models of the input, customized to their applications.
For more detail see L<Marpa::Grammar/"Tokens and Earlemes">.

=head2 Lack of Backward Compatibility

Marpa versions may not be backward compatible.
MDL protects users by requiring the version to be specified,
and by insisting on an exact match with Marpa's version number.
This strict version regime is the same as that being considered for Perl 6.

=head2 Phases

The C<mdl> method
hides the details of creating Marpa objects
and using Marpa's object methods from the user.
But for advanced applications,
and for tracing and debugging,
it is useful to know in detail how Marpa works.

Marpa parsing take place in three phases:
B<grammar creation>,
B<input recognition>
and B<parse evaluation>.
For brevity, I'll often speak of the parse evaluation phase as
the B<evaluation> phase,
and the input recognition phase as
the B<recognition> phase.

Corresponding to the three phases,
Marpa has three kinds of object: grammars, recognizers and evaluators.
Recognizers are created from grammars and
evaluators are created from recognizers.

=head3 Grammars

Grammar objects (C<Marpa::Grammar>) are created first.
They may be created with rules or empty.
Rules may be added to grammar objects after they have been created.
After all the rules have been added, but before it is used to create a recognizer,
a grammar must be precomputed.
Precomputation is usually done automatically,
when rules are added, but this behavior can be fine-tuned.
Details on grammar objects and methods can be found at L<Marpa::Grammar>.

=head3 Recognizers

To create a Marpa recognizer object (C<Marpa::Recognizer>),
a Marpa grammar object is required.
Once a recognizer object has been created, it can accept input.
You can create multiple recognizers from a single grammar,
and can safely run them simultaneously.

Recognizing an input is answering the "yes" or "no" question:
Does the input match the grammar?
While recognizing its input, Marpa builds tables.
Marpa's evaluation phase works from these tables.
Before creation of an evaluation object, 
the input has not been parsed in the strict sense of the term,
that is, its structure according to the grammar has not been determined.
For more details on recognizer objects and methods,
see L<Marpa::Recognizer>.

=head3 Evaluators

Once the end of input is recognized,
an evaluator object (C<Marpa::Evaluator>) can be created.
For each recognizer, only one evaluator object can
be in use at any one time.

An evaluator object is an iterator.
If the grammar is ambiguous,
the evaluator object can be used to return the values of all the parses.
For details on evaluator objects and methods,
see L<Marpa::Evaluator>.

=head2 Plumbing and Porcelain

A grammar is specified to Marpa through a B<grammar interface>.
There are two kinds of grammar interfaces,
B<porcelain> and B<plumbing>.
There is only one B<plumbing interface>.
As of the moment there is also only one
B<porcelain interface>,
the B<Marpa Demonstration Language>.

The B<plumbing> is a set of named arguments to
the C<new> and C<set> methods of
Marpa grammar objects.
Porcelain interfaces use the plumbing indirectly.
The plumbing is efficient,
but MDL is easier to read, write and maintain.
Users seeking efficiency are usually better off
using stringified MDL.
The documentation for the plumbing
is L<Marpa::Doc::Plumbing>.

Users are encouraged to design their own porcelain.
In Marpa's eyes all porcelain will be equal.
I call the porcelain that I am delivering with 
Marpa the Marpa Demonstration Language instead
of the "Marpa Language" to emphasize its lack of special status.
The documentation for MDL can be found at L<Marpa::Doc::MDL>.

=head2 Namespaces

Actions run in
special namespaces unique to each recognizer object.
These special namespaces belong entirely to the user.

In the following namespaces,
users should use only documented methods:

=begin Marpa::Test::Display:

## skip display

=end Marpa::Test::Display:

    Marpa
    Marpa::Grammar
    Marpa::Lex
    Marpa::MDL
    Marpa::Recognizer
    Marpa::Evaluator

The C<$STRING> and C<$START> variables,
which are made available to the lex actions,
must be used on a read-only basis,
except as described in the documentation.
Marpa namespaces and variables not mentioned in this section,
should not be relied on or modified.

=head2 Returns and Exceptions

Most Marpa methods return only if successful.
On failure they throw an exception.
If you don't want the exception to be fatal, catch it using C<eval>.
A few failures are considered "non-exceptional" and returned.
Non-exceptional failures are described in the documentation for the method which returns them.

=head1 METHODS

=head2 mdl

=begin Marpa::Test::Display:

## next display
is_file($_, 'author.t/misc.t', 'mdl scalar snippet');

=end Marpa::Test::Display:

    $first_result =
        Marpa::mdl( \$grammar_description, \$string_to_parse );

=begin Marpa::Test::Display:

## next display
is_file($_, 'author.t/misc.t', 'mdl array snippet');

=end Marpa::Test::Display:

     @all_results
         = Marpa::mdl( \$grammar_description, \$string_to_parse );

=begin Marpa::Test::Display:

## next display
is_file($_, 'author.t/misc.t', 'mdl scalar hash args snippet');

=end Marpa::Test::Display:

     $first_result = Marpa::mdl(
         \$grammar_description,
         \$string_to_parse,
         { warnings => 0 }
     );

The C<mdl> static method takes three arguments:
a B<reference> to a string containing an MDL description of the grammar;
a B<reference> to a string with the text to be parsed;
and (optionally) a B<reference> to a hash with options.
The available options are described in L<Marpa::Doc::Options>.

In scalar context,  C<mdl> returns a B<reference> to the value of the first parse.
In list context, C<mdl> returns a list of B<references> to the values of the parses.
If there are no parses, C<mdl> returns undefined in scalar context and
the empty list in list context.

=head2 Debugging Methods

L<The separate document on debugging|Marpa::Doc::Debugging> deals
with methods for debugging grammars and parses.

=head1 IMPLEMENTATION NOTES

=head2 Exports and Object Orientation

Marpa exports nothing by default,
and allows no optional exports.
Use of object orientation in Marpa is superficial.
Only grammars, recognizers and evaluators are objects,
and they are not designed to be inherited.

=head2 Speed

Speed seems very good for an Earley's implementation.
Current performance limits are more often a function of the lexing
than of the Marpa parse engine.

=head3 Ambiguous Lexing

Ambiguous lexing has a cost, and grammars which can turn ambiguous lexing off
can expect to parse twice as fast.
Right now when Marpa lexes with multiple regexes at a single location, it uses
a series of Perl 5 regex matches, one for each terminal.

There may be
a more efficient way to find all the matches in
a set of alternatives.
A complication is that
Marpa does predictive lexing, so that the list of lexables is
not known until just before the match is attempted.
But I believe that
lazy evaluation and memoizing could have big payoffs in the cases of most
interest.

=head3 The Marpa Demonstration Language

The Marpa Demonstration Language was written
to demonstrate Marpa's capabilities,
including its lexing capabilities.
A porcelain interface which doesn't use ambiguous lexing could easily run
faster.
One with a customized lexer would be faster yet.

If MDL's parsing speed
becomes an issue for a particular grammar,
that grammar can be precomputed and stringified.
A recognizer can then be created
from the precomputed string grammar.
Using a grammar in the form of a precomputed string avoids 
the overhead of both MDL parsing and precomputation.
Marpa uses stringified grammars internally.
When you use MDL to specify a grammar to Marpa,
Marpa uses a stringified grammar to parse the MDL.

=head3 Comparison with other Parsers

In thinking about speed, it is helpful to
keep in mind Marpa's place in the parsing food chain.
Marpa parses grammars that B<bison>, B<yacc>, L<Parse::Yapp>,
and L<Parse::RecDescent>
cannot parse.
For these, Marpa is clearly faster.  When it comes to time efficiency,
never is not hard to beat.

Marpa allows grammars to be expressed in their most natural form.
It's ideal where programmer time is important relative to running time.
Right now, special-purpose needs are often addressed with regexes.
This works wonderfully if the grammar involved is regular, but
thousands of man-years have been spent trying to shoehorn non-regular
grammars into Perl 5 regexes.

Marpa is a good alternative to
parsers that backtrack.
Marpa finds every possible parse the first time through.
Backtracking is a gamble,
and one often made against the odds.

Some grammars have constructs to control backtracking.
This control comes at a high price.
Solutions with these constructs built into them are
as unreadable as anything in the world of programming,
and fragile in the face of change to boot.

If you know your grammar will be LALR or regular,
that is a good reason B<not> to use Marpa.
When a grammar is LALR or regular,
Marpa takes advantage of this and runs faster.
But such a grammar will run faster yet on a parser designed
for it:
B<bison>, B<yacc> and L<Parse::Yapp> for LALR; regexes
for regular grammars.

Finally, there are the many situations when we want to do some parsing as a one-shot
and don't want to have to care what subcategory our grammar falls in.
We want to write some quick BNF,
do the parsing,
and move on.
For this, there's Marpa.

=head1 DEPENDENCIES

Requires Perl 5.10.
Users who want or need the maturity and/or stability of Perl 5.8 or earlier
are probably also best off with more mature and stable alternatives to Marpa.
Marpa only uses modules that are part of its own distribution, or Perl's.

=head1 AUTHOR

Jeffrey Kegler

=head2 Why is it Called "Marpa"?

Marpa is the name of the greatest of the Tibetan "translators".
In his time (the 11th century AD) Indian Buddhism was
at its height.  A generation of scholars was devoting
itself to producing Tibetan versions of Buddhism's Sanskrit scriptures.
Marpa became the greatest of them,
and today is known as Marpa Lotsawa: "Marpa the Translator".

Translation in the 11th century was not a job for the indoors type.
A translator needed to study in India,
with the teachers who had the
texts and could explain them.
From Marpa's home in Tibet's
Lhotrak Valley,
the best way across the Himalayas to India was over
the Khala Chela Pass.
To reach the Khala Chela's
three-mile high summit,
Marpa had to cross two hundred lawless miles of Tibet.
Once a pilgrim crested the Himalayas,
the road to Nalanda University was all downhill.
Eager to reach their destination,
the first travelers from Tibet had descended the four hundred miles straight to the hot plains.

The last part of the journey had turned out to be by far
the most deadly.
Almost no germs live in the cold,
thin air of Tibet.
Pilgrims who didn't stop to acclimatize themselves
reached the great Buddhist center
with no immunity to India's diseases.
Several large expeditions reached Nalanda
only to have every single member die within weeks.

=head2 Blatant Plug

There's more about Marpa in my novel, B<The God Proof>, in which
his studies, travels and adventures are a subplot.  B<The God
Proof> centers around Kurt GE<ouml>del's proof of God's existence.
Yes, I<that> Kurt GE<ouml>del, and yes, he really did work out a
God Proof (it's in his I<Collected Works>, Vol. 3, pp. 403-404).
B<The God Proof> is available
as a free download (L<http://www.lulu.com/content/933192>)
and in print form at Amazon.com:
L<http://www.amazon.com/God-Proof-Jeffrey-Kegler/dp/1434807355>.

=head1 BUGS

Please report any bugs or feature requests to
C<bug-parse-marpa at rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Marpa>.
I will be notified, and then you'll automatically be notified of progress on
your bug as I make changes.

=head1 SUPPORT

You can find documentation for this module with the perldoc command.

=begin Marpa::Test::Display:

## skip display

=end Marpa::Test::Display:

    perldoc Marpa
    
You can also look for information at:

=over 4

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/Marpa>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/Marpa>

=item * RT: CPAN's request tracker

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=Marpa>

=item * Search CPAN

L<http://search.cpan.org/dist/Marpa>

=back

=head1 ACKNOWLEDGMENTS

Marpa is
derived from the parser described in
L<Aycock and Horspool 2002|Marpa::Doc::Bibliography/"Aycock and Horspool 2002">.
I've made significant changes to it,
which are documented separately (L<Marpa::Doc::Algorithm>).
Aycock and Horspool, for their part,
built on the
L<algorithm discovered by Jay Earley|Marpa::Doc::Bibliography/"Earley 1970">.

I'm grateful to Randal Schwartz for his encouragement over the years that
I've been working on Marpa.  My one conversation
with Larry Wall
about Marpa
was brief and long ago, but his openness to the idea was a major
encouragement,
and his insights into how humans do programming,
how they do languages,
and how those two endeavors interconnect,
a major influence.
More recently,
Allison Randal and Patrick Michaud have been generous with their
very valuable time.
They might have preferred that I volunteered as a Parrot cage-cleaner,
but if so, they were too polite to say.

Many at perlmonks.org answered questions for me.
I used answers from
chromatic, Corion, dragonchild,
jdporter, samtregar and Juerd,
among others,
in writing this module.
I'm just as grateful to those whose answers I didn't use.
My inquiries were made while I was thinking out the code and
it wasn't always 100% clear what I was after.
If the butt is moved after the round,
it shouldn't count against the archer.

In writing the Pure Perl version of Marpa, I benefited from studying
the work of Francois Desarmenien (C<Parse::Yapp>), 
Damian Conway (C<Parse::RecDescent>) and
Graham Barr (C<Scalar::Util>).
Adam Kennedy patiently instructed me
in module writing,
both on the finer points and
on issues about which I really should have know better.

=head1 LICENSE AND COPYRIGHT

Copyright 2007-2009 Jeffrey Kegler, all rights reserved.

This program is free software; you can redistribute
it and/or modify it under the same terms as Perl 5.10.0.

=cut

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
