package Test::Weaken;

use strict;
use warnings;

require Exporter;

use base qw(Exporter);
our @EXPORT_OK = qw(leaks poof);
our $VERSION   = '2.001_002';

## no critic (BuiltinFunctions::ProhibitStringyEval)
$VERSION = eval $VERSION;
## use critic

=begin Implementation:

The basic strategy: get a list of all the objects which allocate memory,
create probe references to them, weaken those probe references, attempt
to free the memory, and check the references.  If the memory is free,
the probe references will be undefined.

Probe references also serve a second purpose -- to avoid copying any
weak reference in the original object.  When you copy a weak reference,
the result is a strong reference.

There may be good reasons for Perl strengthen-on-copy policy, but that
behavior is a big problem for this module.  A lot of what might seem
like needless indirection in the code below is done to avoid working
with references directly in situations which could involve making a copy
of them, even implicitly.

=end Implementation:

=cut

package Test::Weaken::Internal;

use Scalar::Util qw(refaddr reftype isweak weaken);

sub follow {
    my $base_probe = shift;
    my $ignore     = shift;

    # Initialize the results with a reference to the dereferenced
    # base reference.

    # The initialization assumes the $base_probe is a reference,
    # not part of the test object, whose referent is also a reference
    # which IS part of the test object.
    my @follow_probes    = ($base_probe);
    my @tracking_probes  = ($base_probe);
    my %already_followed = ();
    my %already_tracked  = ();

    FOLLOW_OBJECT: while ( my $follow_probe = pop @follow_probes ) {

        # The follow probes are to objects which either will not be
        # tracked or which have already been added to @tracking_probes

        next FOLLOW_OBJECT if $already_followed{ $follow_probe + 0 }++;

        my $object_type  = reftype $follow_probe;

        if (defined $ignore) {
            my $safe_copy = $follow_probe;
            next FOLLOW_OBJECT if $ignore->($safe_copy);
        }

        if ($object_type eq 'ARRAY') {
            push @follow_probes, map { \$_ } grep { ref $_ } @{$follow_probe};
            next FOLLOW_OBJECT;
        }

        if ($object_type eq 'HASH') {
            push @follow_probes, map { \$_ } grep { ref $_ } values %{$follow_probe};
            next FOLLOW_OBJECT;
        }

        # ignore any IO, FORMAT, LVALUE object or object of a type not listed
        next FOLLOW_OBJECT if $object_type ne 'REF';

        # if we reach here, $object_type eq 'REF'

        # now figure what kind of object it points to, and put a new probe
        # in the follow probes, depending on the type

        my $ref_type = Scalar::Util::reftype ${$follow_probe};

        my $new_tracking_probe;
        my $new_follow_probe;

        if ($ref_type eq 'HASH') {
            $new_follow_probe = $new_tracking_probe = \%{ ${$follow_probe} };
        }

        if ($ref_type eq 'ARRAY') {
            $new_follow_probe = $new_tracking_probe = \@{ ${$follow_probe} };
        }

        if ( $ref_type eq 'REF') {
            $new_follow_probe = $new_tracking_probe = \${ ${$follow_probe} };
        }

        if (
            $ref_type eq 'SCALAR'
            or $ref_type eq 'VSTRING'
        ) {
            $new_tracking_probe = \${ ${$follow_probe} };
        }

        if ($ref_type eq 'CODE') {
            $new_tracking_probe = \&{ ${$follow_probe} };
        }

        push @follow_probes, $new_follow_probe if defined $new_follow_probe;

        next FOLLOW_OBJECT unless defined $new_tracking_probe;

        next FOLLOW_OBJECT if $already_tracked{ $new_tracking_probe + 0 }++;

        if (defined $ignore) {
            my $safe_copy = $new_tracking_probe;
            next FOLLOW_OBJECT if $ignore->($safe_copy);
        }

        push @tracking_probes, $new_tracking_probe;

    }    # FOLLOW_OBJECT

    return \@tracking_probes;

}    # sub follow

# See POD, below
sub Test::Weaken::new {
    my ( $class, $arg1, $arg2 ) = @_;
    my $constructor;
    my $destructor;
    my $self = {};
    bless $self, $class;

    UNPACK_ARGS: {
        if ( ref $arg1 eq 'CODE' ) {
            $self->{constructor} = $arg1;
            if ( defined $arg2 ) {
                $self->{destructor} = $arg2;
            }
            return $self;
        }

        if ( ref $arg1 ne 'HASH' ) {
            Marpa::Exception('arg to Test::Weaken::new is not HASH ref');
        }

        if ( defined $arg1->{constructor} ) {
            $self->{constructor} = $arg1->{constructor};
            delete $arg1->{constructor};
        }

        if ( defined $arg1->{destructor} ) {
            $self->{destructor} = $arg1->{destructor};
            delete $arg1->{destructor};
        }

        if ( defined $arg1->{ignore} ) {
            $self->{ignore} = $arg1->{ignore};
            delete $arg1->{ignore};
        }

        my @unknown_named_args = keys %{$arg1};

        if (@unknown_named_args) {
            my $message = q{};
            for my $unknown_named_arg (@unknown_named_args) {
                $message .= "Unknown named arg: '$unknown_named_arg'\n";
            }
            Marpa::exception( $message
                    . 'Test::Weaken failed due to unknown named arg(s)' );
        }

    }    # UNPACK_ARGS

    if ( my $ref_type = ref $self->{constructor} ) {
        Marpa::exception('Test::Weaken: constructor must be CODE ref')
            unless ref $self->{constructor} eq 'CODE';
    }

    if ( my $ref_type = ref $self->{destructor} ) {
        Marpa::exception('Test::Weaken: destructor must be CODE ref')
            unless ref $self->{destructor} eq 'CODE';
    }

    if ( my $ref_type = ref $self->{ignore} ) {
        Marpa::exception('Test::Weaken: ignore must be CODE ref')
            unless ref $self->{ignore} eq 'CODE';
    }

    return $self;

}    # sub new

sub Test::Weaken::test {

    my $self        = shift;
    my $constructor = $self->{constructor};
    my $destructor  = $self->{destructor};
    my $ignore      = $self->{ignore};

    my $test_object_probe = \( $constructor->() );
    if ( not ref ${$test_object_probe} ) {
        carp(
            'Test::Weaken test object constructor did not return a reference'
        );
    }
    my $probes =
        Test::Weaken::Internal::follow( $test_object_probe, $ignore );

    $self->{probe_count} = @{$probes};
    $self->{weak_probe_count} =
        grep { ref $_ eq 'REF' and isweak ${$_} } @{$probes};
    $self->{strong_probe_count} =
        $self->{probe_count} - $self->{weak_probe_count};

    for my $probe ( @{$probes} ) {
        weaken($probe);
    }

    # Now free everything.
    $destructor->( ${$test_object_probe} ) if defined $destructor;

    $test_object_probe = undef;

    my $unfreed_probes = [ grep { defined $_ } @{$probes} ];
    $self->{unfreed_probes} = $unfreed_probes;

    return scalar @{$unfreed_probes};

}    # sub test

sub poof_array_return {

    my $test    = shift;
    my $results = $test->{unfreed_probes};

    my @unfreed_strong = ();
    my @unfreed_weak   = ();
    for my $probe ( @{$results} ) {
        if ( ref $probe eq 'REF' and isweak ${$probe} ) {
            push @unfreed_weak, $probe;
        }
        else {
            push @unfreed_strong, $probe;
        }
    }

    # See the POD on the return values
    return ( @{$test}{qw(weak_probe_count strong_probe_count)},
        \@unfreed_weak, \@unfreed_strong );

} ## end sub poof_array_return;

sub Test::Weaken::poof {
    my @args   = @_;
    my $test   = new Test::Weaken(@args);
    my $result = $test->test();
    return Test::Weaken::Internal::poof_array_return($test) if wantarray;
    return $result;
}

sub Test::Weaken::leaks {
    my @args   = @_;
    my $test   = new Test::Weaken(@args);
    my $result = $test->test();
    return $test if $result;
    return;
}

sub Test::Weaken::unfreed_proberefs {
    my $test   = shift;
    my $result = $test->{unfreed_probes};
    if ( not defined $result ) {
        Marpa::exception('Results not available for this Test::Weaken object');
    }
    return $result;
}

sub Test::Weaken::unfreed_count {
    my $test   = shift;
    my $result = $test->{unfreed_probes};
    if ( not defined $result ) {
        Marpa::exception('Results not available for this Test::Weaken object');
    }
    return scalar @{$result};
}

sub Test::Weaken::probe_count {
    my $test  = shift;
    my $count = $test->{probe_count};
    if ( not defined $count ) {
        Marpa::exception('Results not available for this Test::Weaken object');
    }
    return $count;
}

sub Test::Weaken::weak_probe_count {
    my $test  = shift;
    my $count = $test->{weak_probe_count};
    if ( not defined $count ) {
        Marpa::exception('Results not available for this Test::Weaken object');
    }
    return $count;
}

sub Test::Weaken::strong_probe_count {
    my $test  = shift;
    my $count = $test->{strong_probe_count};
    if ( not defined $count ) {
        Marpa::exception('Results not available for this Test::Weaken object');
    }
    return $count;
}

sub Test::Weaken::check_ignore {
    my ( $ignore, $max_errors, $compare_depth, $reporting_depth ) = @_;

    my $error_count = 0;

    $max_errors = 1 if not defined $max_errors;
    if ( not Scalar::Util::looks_like_number($max_errors) ) {
        Marpa::exception('Test::Weaken::check_ignore max_errors must be a number');
    }
    $max_errors = 0 if $max_errors <= 0;

    $reporting_depth = -1 if not defined $reporting_depth;
    if ( not Scalar::Util::looks_like_number($reporting_depth) ) {
        Marpa::exception('Test::Weaken::check_ignore reporting_depth must be a number');
    }
    $reporting_depth = -1 if $reporting_depth < 0;

    $compare_depth = 0 if not defined $compare_depth;
    if ( not Scalar::Util::looks_like_number($compare_depth)
        or $compare_depth < 0 )
    {
        Marpa::exception(
            'Test::Weaken::check_ignore compare_depth must be a non-negative number'
        );
    }

    return sub {
        my ($probe_ref) = @_;

        my $before_weak =
            ( ref $probe_ref eq 'REF' and isweak( ${$probe_ref} ) );
        my $before_dump =
            Data::Dumper->new( [$probe_ref], [qw(proberef)] )
            ->Maxdepth($compare_depth)->Dump();
        my $before_reporting_dump;
        if ( $reporting_depth >= 0 ) {
            #<<< perltidy doesn't do this well
            $before_reporting_dump =
                Data::Dumper->new(
                    [$probe_ref],
                    [qw(proberef_before_callback)]
                )
                ->Maxdepth($reporting_depth)
                ->Dump();
            #>>>
        }

        my $return_value = $ignore->($probe_ref);

        my $after_weak =
            ( ref $probe_ref eq 'REF' and isweak( ${$probe_ref} ) );
        my $after_dump =
            Data::Dumper->new( [$probe_ref], [qw(proberef)] )
            ->Maxdepth($compare_depth)->Dump();
        my $after_reporting_dump;
        if ( $reporting_depth >= 0 ) {
            #<<< perltidy doesn't do this well
            $after_reporting_dump =
                Data::Dumper->new(
                    [$probe_ref],
                    [qw(proberef_after_callback)]
                )
                ->Maxdepth($reporting_depth)
                ->Dump();
            #<<<
        }

        my $problems       = q{};
        my $include_before = 0;
        my $include_after  = 0;

        if ( $before_weak != $after_weak ) {
            my $changed = $before_weak ? 'strengthened' : 'weakened';
            $problems .= "Probe referent $changed by ignore call\n";
            $include_before = defined $before_reporting_dump;
        }
        if ( $before_dump ne $after_dump ) {
            $problems .= "Probe referent changed by ignore call\n";
            $include_before = defined $before_reporting_dump;
            $include_after  = defined $after_reporting_dump;
        }

        return $return_value if not $problems;

        $error_count++;

        my $message .= q{};
        $message .= $before_reporting_dump
            if $include_before;
        $message .= $after_reporting_dump
            if $include_after;
        $message .= $problems;

        if ( $max_errors > 0 and $error_count >= $max_errors ) {
            $message
                .= "Terminating ignore callbacks after finding $error_count error(s)";
            Marpa::exception($message);
        }

        carp( $message . 'Above errors reported' );
        return $return_value;
    };
}

1;

__END__

=head1 NAME

Test::Weaken - Test that freed memory objects were, indeed, freed

=head1 SYNOPSIS

=begin Marpa::Test::Display:

## start display
## next display
is_file($_, 't/synopsis.t', 'synopsis')

=end Marpa::Test::Display:

    use Test::Weaken qw(leaks);
    use Data::Dumper;
    use Math::BigInt;
    use Math::BigFloat;
    use English qw( -no_match_vars );

    my $good_test = sub {
        my $obj1 = new Math::BigInt('42');
        my $obj2 = new Math::BigFloat('7.11');
        [ $obj1, $obj2 ];
    };

    if ( !leaks($good_test) ) {
        print "No leaks in test 1\n" or Marpa::exception("Cannot print to STDOUT: $ERRNO");
    }
    else {
        print "There were memory leaks from test 1!\n"
            or Marpa::exception("Cannot print to STDOUT: $ERRNO");
    }

    my $bad_test = sub {
        my $array = [ 42, 711 ];
        push @{$array}, $array;
        $array;
    };

    my $bad_destructor = sub {'I am useless'};

    my $test = Test::Weaken::leaks(
        {   constructor => $bad_test,
            destructor  => $bad_destructor,
        }
    );
    if ($test) {
        my $unfreed_proberefs = $test->unfreed_proberefs();
        my $unfreed_count     = @{$unfreed_proberefs};
        printf "Test 2: %d of %d original references were not freed\n",
            $test->unfreed_count(), $test->probe_count()
            or Marpa::exception("Cannot print to STDOUT: $ERRNO");
        print "These are the probe references to the unfreed objects:\n"
            or Marpa::exception("Cannot print to STDOUT: $ERRNO");
        for my $proberef ( @{$unfreed_proberefs} ) {
            print Data::Dumper->Dump( [$proberef], ['unfreed'] )
                or Marpa::exception("Cannot print to STDOUT: $ERRNO");
        }
    }

=begin Marpa::Test::Display:

## end display

=end Marpa::Test::Display:

=head1 DESCRIPTION

A memory leak occurs when an object is destroyed
but the memory that
the object uses is not completely deallocated.
Leaked memory is a useless overhead.
Leaks can significantly impact system performance.
They can also cause an application to abend due to lack of memory.

In Perl,
circular references
are
a common cause of memory leaks.
Circular references are allowed in Perl,
but objects containing circular references will leak memory
unless the programmer takes specific measures to prevent leaks.
Preventive measures include
weakening the references
and arranging to break the reference cycle just before
the object is destroyed,

It is easy to misdesign or misimplement a scheme for
preventing memory leaks.
Mistakes of this kind
have been hard to detect
in a test suite.

C<Test::Weaken> allows easy detection of unfreed memory objects.
C<Test::Weaken> allows you to examine the unfreed objects,
even objects which are usually inaccessible.
It performs this magic by creating a set of weakened B<probe references>, as explained L<below|/"IMPLEMENTATION">.

C<Test::Weaken> gets its test object from a closure.
The closure should return
a reference to the B<test object>.
This reference is called the B<test object reference>.

C<Test::Weaken> frees the test object,
then looks to see if any memory that can be accessed
from the test object reference was not actually deallocated.
To determine which memory can be accessed from the
test object reference,
C<Test::Weaken> follows
arrays, hashes, weak references and strong references.
It follows these recursively and to unlimited depth.

C<Test::Weaken> deals gracefully with circular references.
That's important,
because a major purpose of C<Test::Weaken> is to test schemes for
circular references.
To avoid infinite loops,
C<Test::Weaken> records all the memory objects it visits,
and will not visit the same memory object twice.

=head2 Independent Objects and Tracked Objects

An object is called a B<independent memory object>
if it has independently allocated memory.
For brevity, this document often refers to independent memory objects
as B<independent objects>.

Arrays, hashes, closures and variables are independent memory objects.
References and constants which are not elements of arrays or hashes are
also independent memory objects.
Elements of arrays and hashes are never independent memory objects, because their
memory is not independent --
it is always deallocated when the array or hash
to which the elements belong
is destroyed.

A independent object is called a B<tracked object>
if C<Test::Weaken> tracks it with a probe reference.
Tracked objects are always independent objects.

=head2 Followed Objects and External Objects

An object is called a B<followed object>
if C<Test::Weaken> examines it during its recursive search for
objects to track.
Followed objects are not always independent objects.
References are not independent objects when
they are elements of arrays and hashes,
but they are followed.

An object inside the test object is called an B<internal object>.
In the C<Test::Weaken> context, the relevant criterion for deciding
"inside" versus "outside" is the lifetime of an object.
If an object's lifetime is expected to be the same as that of the test
object, it is called an B<internal object>.
If an object's lifetime might be different from the lifetime of the test
object, then it is called an B<external object>.
Since the question is one of I<expected> lifetime,
this difference is ultimately subjective.

Objects found recursively
from the test object reference will usually be internal objects.
This may not always be the case, however.
Some objects found by C<Test::Weaken> might be external to the
test object.
If external objects are found and they are persistent,
they complicate matters.

An external object is called a B<persistent object>
if is expected that the lifetime of the external object might
extend beyond that of the test object.
Persistent objects are not memory leaks.
Persistent objects are objects not expected to be freed along
with the test object.
Leaked objects are objects that are expected to be freed
with the test object,
but that are not actually freed.

To determine which of the unfreed objects are memory leaks,
the user must separate out the persistent objects
from the other results.
Ways to do this are outlined
L<below|/"ADVANCED TECHNIQUES">.

=head2 Builtin Types

When it needs to classify object types precisely,
this document will
use the builtin type names as returned by L<Scalar::Util>'s
C<reftype> subroutine and Perl's C<ref> builtin.
Both C<reftype> and C<ref> take a reference as their argument.
They both return the type of the I<referent>.
For example, given a reference to
a number or a string,
C<reftype> and C<ref> return "C<SCALAR>".
If the argument to C<reftype> is a reference to a reference,
C<reftype> and C<ref> return "C<REF>".

There are differences between C<reftype> and C<ref>.
For an object blessed into a package, C<ref> returns the package name,
rather than the builtin type.
Also, C<ref> describes the builtin type of compiled regular expressions as "C<Regexp>",
while C<reftype> describes it as being of the same builtin type as a number or a string:
"C<SCALAR>".
In this document,
the list of builtin types is considered to be as follows:
SCALAR, ARRAY,
HASH, CODE, REF, GLOB, LVALUE, FORMAT, IO, VSTRING, and Regexp.

=head2 ARRAY and HASH Objects

Objects of builtin type ARRAY and HASH are always both tracked and followed.

=head2 REF Objects

Independent memory objects of builtin type REF are always both tracked and followed.
Objects of type REF which are elements of an array or a hash
are followed, but are not tracked.

=head2 CODE Objects

Objects of type CODE are tracked but are not followed.
This can be seen as a limitation, because
closures hold references to memory objects.
Future versions of C<Test::Weaken> may follow CODE objects.

=head2 SCALAR, VSTRING and Regexp Objects

Independent objects of builtin types SCALAR, VSTRING and Regexp are tracked.
Objects of type SCALAR, VSTRING and Regexp are independent if and only if they
are not array or hash elements.
SCALAR, VSTRING and Regexp objects are not followed because there is
nothing to follow
-- they do not hold references to other objects.

=head2 Array and Hash Elements

Elements of arrays and hashes are never tracked,
because they are not independent memory objects.
If they are REF objects, they are followed.

=head2 Objects That are Ignored

An object is said to be B<ignored> if it is neither
tracked or followed.
All objects of builtin types GLOB, IO, FORMAT and LVALUE are ignored.
All array and hash elements which are not of builtin type REF
are ignored.

Ignoring GLOB, IO and FORMAT objects
saves trouble.
These objects will almost always
be external.
GLOB objects refer to an
entry in the Perl symbol table,
which is external.
Objects of builtin type IO
are typically associated with GLOB objects.
FORMAT objects are always global.
Use of FORMAT objects is officially deprecated.

An LVALUE object could only be present in the test object
through a reference.
I have not seen LVALUE reference programming deprecated
anywhere.
Possibly nobody has found worth his breath to do so.
LVALUE references are rare.
Here's what one looks like:

=begin Marpa::Test::Display:

## skip display

=end Marpa::Test::Display:

    \pos($string)

There is
another reason that the user might be just as happy not to have
FORMAT, IO and LVALUE references reported in the results.
C<Data::Dumper> does not handle them
gracefully.
C<Data::Dumper>
issues a cryptic warning when it encounters a reference to
FORMAT, IO and LVALUE objects.

Future implementations of Perl may define builtin types
not known as of this writing.
Objects which do not fall into any of the types described above
will not be tracked or followed.

=head2 Why the Test Object is Passed via a Closure

C<Test::Weaken> gets its test object
indirectly,
as the return value from a
B<test object constructor>.
Why so roundabout?

Because the indirect way is the easiest.
When you
create the test object
in C<Test::Weaken>'s calling environment,
it takes a lot of craft to avoid
leaving
unintended references to the test object in that calling environment.
It is easy to get this wrong.

When the calling environment retains a reference to an object inside the test object,
the result usually appears as a memory leak.
In other words,
mistakes in setting up the test object
create memory leaks which are artifacts of the test environment.
These artifacts are very difficult to sort out from the real thing.

The easiest way to avoid leaving unintended references to memory inside
the test object is to work entirely within a closure.
Under this strategy,
the test object is created entirely in the closure, using
only objects local to that closure.
Memory objects local to a closure will be destroyed when the
closure returns, and any references they held will be released.
The closure-local strategy makes
it relatively easy to be sure that nothing is left behind
that will hold an unintended reference to memory inside the test
object.

To help the user to follow the closure-local strategy,
C<Test::Weaken> requires that its test object reference
be the return value of a closure.
The closure-local strategy is safe.
It is almost always right thing to do.
C<Test::Weaken> makes it the easy thing to do.

Nothing prevents a user from using a test object constructor
that refers to data in global or other scopes.
Nothing prevents a
test object constructor
from returning a reference to a test object
created from data in any scope the user desires.
Subverting the closure-local strategy takes little effort,
certainly by comparison to the great amount of trouble
that the user is exposing herself to.

=head2 Returns and Exceptions

The methods of C<Test::Weaken> do not return errors.
Errors are always thrown as exceptions.

=head1 PORCELAIN METHODS

=head2 leaks

=begin Marpa::Test::Display:

## start display
## next display
is_file($_, 't/snippet.t', 'leaks snippet')

=end Marpa::Test::Display:

    use Test::Weaken;
    use English qw( -no_match_vars );

    my $test = Test::Weaken::leaks(
        {   constructor => sub { new Buggy_Object },
            destructor  => \&destroy_buggy_object,
        }
    );
    if ($test) {
        print "There are leaks\n" or Marpa::exception("Cannot print to STDOUT: $ERRNO");
    }

=begin Marpa::Test::Display:

## end display

=end Marpa::Test::Display:

Arguments to the C<leaks> static method may be passed as a reference to
a hash of named arguments,
or directly as code references.
C<leaks> returns a C<Test::Weaken> object if it found unfreed objects,
and a Perl false value otherwise.
Users who only want to know if there were unfreed objects can
test the return value of C<leaks> for Perl true or false.

=over 4

=item constructor

The B<test object constructor> is a required argument.
It must be a code reference.
When the arguments are passed directly as code references,
the test object constructor must be the first argument to C<leaks>.
When named arguments are used,
the test object constructor must be the value of the C<constructor> named argument.

The test object constructor
should build the test object
and return a reference to it.
It is best to follow strictly the closure-local strategy,
as described above.

=item destructor

The B<test object destructor> is an optional argument.
If specified, it must be a code reference.
When the arguments are passed directly as code references,
the test object destructor is the second, optional, argument to C<leaks>.
When named arguments are used,
the test object destructor must be the value of the C<destructor> named argument.

If specified,
the test object destructor is called
just before the test object reference is undefined.
It will be passed one argument,
the test object reference.
The return value of the test object destructor is ignored.

Some test objects require
a destructor to be called when
they are freed.
The primary purpose for
the test object destructor is to enable
C<Test::Weaken> to work with these objects.

=item ignore

=begin Marpa::Test::Display:

## start display
## next 2 displays
is_file($_, 't/ignore.t', 'ignore snippet')

=end Marpa::Test::Display:

    sub ignore_my_global {
        my ($thing) = @_;
        return ( Scalar::Util::blessed($thing) && $thing->isa('MyGlobal') );
    }

    my $test = Test::Weaken::leaks(
        {   constructor => sub { MyObject->new },
            ignore      => \&ignore_my_global,
        }
    );

=begin Marpa::Test::Display:

## end display

=end Marpa::Test::Display:

The B<ignore> argument is optional.
It can be used to prevent C<Test::Weaken> from following
and tracking selected probe references, as chosen by
the user.
Use of the C<ignore> argument should be avoided
when possible.
Filtering the probe references returned by
L<unfreed_proberefs>
is easier, safer and
faster
after the fact.
The C<ignore> argument is provided for situations
where filtering after the fact
is not practical.
One such
situation is when
large or complicated sub-objects need to be filtered out of the results.

When specified, the value of the C<ignore> argument must be a
reference to a callback subroutine.
The subroutine will be called once for each probe reference,
with that probe reference as the only argument.
Everything that is referred to, directly or indirectly,
by this
probe reference
should be left unchanged by the C<ignore>
callback.
The result of modifying the probe referents might be
an exception, an abend, an infinite loop, or erroneous results.

The callback subroutine should return Perl true if the probe reference is
to an object that should be ignored --
that is, neither followed or tracked.
Otherwise the callback subroutine should return a Perl false.

For safety, C<Test::Weaken> does not pass the original probe reference
to the C<ignore> callback.
Instead, C<Test::Weaken> passes a copy of the probe reference.
This makes it less likely that
the probe reference will be altered by accident.
The object referred to by the probe reference is not a copy, however.
It is the original object.
If that is altered, all bets are off.

C<ignore> callbacks are best kept simple.
Defer as much of the analysis as you can
until after the test is completed.
C<ignore> callbacks 
can also be a significant overhead.
The C<ignore> callback is
invoked once per probe reference.

C<Test::Weaken> offers some help in debugging
C<ignore> callback subroutines.
See L<below|/"Debugging Ignore Subroutines">.

=back

=head2 unfreed_proberefs

=begin Marpa::Test::Display:

## start display
## next display
is_file($_, 't/snippet.t', 'unfreed_proberefs snippet')

=end Marpa::Test::Display:

    use Test::Weaken;
    use English qw( -no_match_vars );

    my $test = Test::Weaken::leaks( sub { new Buggy_Object } );
    if ($test) {
        my $unfreed_proberefs = $test->unfreed_proberefs();
        my $unfreed_count     = @{$unfreed_proberefs};
        printf "%d of %d references were not freed\n",
            $test->unfreed_count(), $test->probe_count()
            or Marpa::exception("Cannot print to STDOUT: $ERRNO");
        print "These are the probe references to the unfreed objects:\n"
            or Marpa::exception("Cannot print to STDOUT: $ERRNO");
        for my $proberef ( @{$unfreed_proberefs} ) {
            print Data::Dumper->Dump( [$proberef], ['unfreed'] )
                or Marpa::exception("Cannot print to STDOUT: $ERRNO");
        }
    }

=begin Marpa::Test::Display:

## end display

=end Marpa::Test::Display:

Returns a reference to an array of probe references to the unfreed objects.
Often, this data is examined
to pinpoint the source of a leak.
A user may also analyze this data to produce her own statistics about unfreed objects.

The array is returned as a reference because in some applications it can be quite long.
The array contains the probe references to the unfreed independent memory objects.

The array contains probe references rather than the objects themselves,
because it is not always possible to copy
the independent
objects directly into the array.
Arrays and hashes cannot be copied into individual
array elements --
references to them are the best that can be done.

Even when copying is possible, it destroys important information.
The original address of the copied object may be important for identifying it,
and the copy will have a different address.
And weak references are strengthened when they are copied.

=head2 unfreed_count

=begin Marpa::Test::Display:

## start display
## next display
is_file($_, 't/snippet.t', 'unfreed_count snippet')

=end Marpa::Test::Display:

    use Test::Weaken;
    use English qw( -no_match_vars );

    my $test = Test::Weaken::leaks( sub { new Buggy_Object } );
    next TEST if not $test;
    printf "%d objects were not freed\n", $test->unfreed_count(),
        or Marpa::exception("Cannot print to STDOUT: $ERRNO");

=begin Marpa::Test::Display:

## end display

=end Marpa::Test::Display:

Returns the count of unfreed objects.
This count will be exactly the length of the array referred to by
the return value of the C<unfreed_proberefs> method.

=head2 probe_count

=begin Marpa::Test::Display:

## start display
## next display
is_file($_, 't/snippet.t', 'probe_count snippet')

=end Marpa::Test::Display:

        use Test::Weaken;
        use English qw( -no_match_vars );

        my $test = Test::Weaken::leaks(
            {   constructor => sub { new Buggy_Object },
                destructor  => \&destroy_buggy_object,
            }
        );
        next TEST if not $test;
        printf "%d of %d objects were not freed\n",
            $test->unfreed_count(), $test->probe_count()
            or Marpa::exception("Cannot print to STDOUT: $ERRNO");

=begin Marpa::Test::Display:

## end display

=end Marpa::Test::Display:

Returns the total number of probe references in the test,
including references to freed objects.
This is the count of probe references
after C<Test::Weaken> was finished following the test object reference
recursively,
but before C<Test::Weaken> called the test object destructor and undefined the
test object reference.

=head2 weak_probe_count

=begin Marpa::Test::Display:

## start display
## next display
is_file($_, 't/snippet.t', 'weak_probe_count snippet')

=end Marpa::Test::Display:

        use Test::Weaken;
        use Scalar::Util qw(isweak);
        use English qw( -no_match_vars );

        my $test = Test::Weaken::leaks( sub { new Buggy_Object }, );
        next TEST if not $test;
        my $weak_unfreed_reference_count =
            scalar grep { ref $_ eq 'REF' and isweak( ${$_} ) }
            @{ $test->unfreed_proberefs() };
        printf "%d of %d weak references were not freed\n",
            $weak_unfreed_reference_count, $test->weak_probe_count(),
            or Marpa::exception("Cannot print to STDOUT: $ERRNO");

=begin Marpa::Test::Display:

## end display

=end Marpa::Test::Display:

Returns the number of probe references which were to weak references.
The count includes
probe references to freed weak references.
The count is made
after C<Test::Weaken> finishes following the test object reference
recursively,
but before C<Test::Weaken> calls the test object destructor and undefines the
test object reference.

=head2 strong_probe_count

=begin Marpa::Test::Display:

## start display
## next display
is_file($_, 't/snippet.t', 'strong_probe_count snippet')

=end Marpa::Test::Display:

    use Test::Weaken;
    use English qw( -no_match_vars );
    use Scalar::Util qw(isweak);

    my $test = Test::Weaken::leaks(
        {   constructor => sub { new Buggy_Object },
            destructor  => \&destroy_buggy_object,
        }
    );
    next TEST if not $test;
    my $proberefs = $test->unfreed_proberefs();
    my $strong_unfreed_object_count =
        grep { ref $_ ne 'REF' or not isweak( ${$_} ) } @{$proberefs};
    my $strong_unfreed_reference_count =
        grep { ref $_ eq 'REF' and not isweak( ${$_} ) } @{$proberefs};

    printf "%d of %d strong objects were not freed\n",
        $strong_unfreed_object_count, $test->strong_probe_count(),
        or Marpa::exception("Cannot print to STDOUT: $ERRNO");
    printf "%d of the unfreed strong objects were references\n",
        $strong_unfreed_reference_count
        or Marpa::exception("Cannot print to STDOUT: $ERRNO");

=begin Marpa::Test::Display:

## end display

=end Marpa::Test::Display:

Returns the number of probe references which were to strong objects.
Here "strong object" means any object which is not a weak reference.
This includes not just strong REF objects,
but all objects which are not of REF type.

The count includes probes to both freed and unfreed objects.
The count is taken
after C<Test::Weaken> finishes following the test object reference
recursively,
but before C<Test::Weaken> calls the test object destructor and undefines the
test object reference.

=head1 PLUMBING METHODS

Most users can skip this section.
The plumbing methods exist to satisfy object-oriented purists,
and to accommodate the rare user who wants to access the probe counts
even when the test did find any unfreed objects.

=head2 new

=begin Marpa::Test::Display:

## start display
## next display
is_file($_, 't/snippet.t', 'new snippet')

=end Marpa::Test::Display:

    use Test::Weaken;
    use English qw( -no_match_vars );

    my $test = new Test::Weaken( sub { new My_Object } );
    printf "There were %s leaks\n", $test->test()
        or Marpa::exception("Cannot print to STDOUT: $ERRNO");
    my $proberefs            = $test->unfreed_proberefs();
    my $unfreed_count        = 0;
    my $weak_unfreed_count   = 0;
    my $strong_unfreed_count = 0;
    PROBEREF: for my $proberef ( @{$proberefs} ) {
        $unfreed_count++;
        if ( ref $_ eq 'REF' and isweak( ${$_} ) ) {
            $weak_unfreed_count++;
        }
        else {
            $strong_unfreed_count++;
        }
    }
    printf "%d of %d objects freed\n", $unfreed_count, $test->probe_count()
        or Marpa::exception("Cannot print to STDOUT: $ERRNO");
    printf "%d of %d weak references freed\n", $weak_unfreed_count,
        $test->weak_probe_count()
        or Marpa::exception("Cannot print to STDOUT: $ERRNO");
    printf "%d of %d other objects freed\n", $strong_unfreed_count,
        $test->strong_probe_count()
        or Marpa::exception("Cannot print to STDOUT: $ERRNO");

=begin Marpa::Test::Display:

## end display

=end Marpa::Test::Display:

The C<new> method takes the same arguments as the C<leaks> method, described above.
Unlike the C<leaks> method, it always returns a test object.
Errors are thrown as exceptions.

Only one thing can be done with
the test object returned by C<new>.
That is to call the C<test> method on it.
Until the C<test> method is called,
no results will be available,
and calling any method which asks for a result will cause an
exception.

=head2 test

=begin Marpa::Test::Display:

## start display
## next display
is_file($_, 't/snippet.t', 'test snippet')

=end Marpa::Test::Display:

    use Test::Weaken;
    use English qw( -no_match_vars );

    my $test = new Test::Weaken(
        {   constructor => sub { new My_Object },
            destructor  => \&destroy_my_object,
        }
    );
    printf "There are %s\n", ( $test->test() ? 'leaks' : 'no leaks' )
        or Marpa::exception("Cannot print to STDOUT: $ERRNO");

The C<test> method should only be called on a C<Test::Weaken> object just
returned from the C<new> constructor.
It runs the test specified
by the arguments to the C<new> constructor
and obtains the results.
Calling any other method except C<test> on a C<Test::Weaken> object returned by
the C<new> constructor, but not yet evaluated with the C<test> method,
will produce an exception.

The C<test> method returns the count of unfreed objects.
This will be identical to the length of the array
returned by C<unfreed_proberefs> and
the count returned by C<unfreed_count>.
If there is an error, the C<test> method throws an exception.

=begin Marpa::Test::Display:

## end display

=end Marpa::Test::Display:

=head1 ADVANCED TECHNIQUES

=head2 Tracing Leaks

The C<unfreed_proberefs> method returns an array containing the unfreed
independent memory objects.
This can be used
to find the source of leaks.
If circumstances allow it,
you might find it useful to add "tag" elements to arrays and hashes
for tracking purposes.

You can quasi-uniquely identify memory objects using
the referent addresses of the probe references.
A referent address
can be determined by using the
C<refaddr> method of
L<Scalar::Util>.
You can also obtain the referent address of a reference by adding zero
to the reference.

Note that in other Perl documentation, the term "reference address" is often
used when a referent address is meant.
Any given reference has both a reference address and a referent address.
The reference address is the reference's own location in memory.
The referent address is the address of the memory object to which the reference refers.
It is the referent address that interests us here and,
happily, it is
the referent address that both zero addition and C<refaddr> return.

Sometimes, when you are interested in why an object is not being freed,
you want to seek out the reference
that keeps the object's refcount above zero.
Kevin Ryde reports that L<Devel::FindRef>
can be useful for this.

=head2 Quasi-unique addresses and Indiscernable Objects

I call referent addresses "quasi-unique", because they are only
unique at a
specific point in time.
Once an object is freed, its address can be reused.
Absent other evidence,
an object with a given referent address
is not 100% certain to be
the same object
as the object which had the same address earlier.
This can bite you
if you're not careful.
_
To be sure an earlier object and a later object with the same address
are actually the same object,
you need to know that the earlier object will be persistent,
or to compare the two objects.
If you want to be really pedantic,
even an exact match from a comparison doesn't settle the issue.
It is possible that two indiscernable
(that is, completely identical)
objects with the same referent address are different in the following
sense:
the first object might have been destroyed and a second, identical,
object created at the same address.
But for most practical programming purposes,
two indiscernable objects can be regarded as the same object.

=head2 Debugging Ignore Subroutines

It can be hard to determine if
C<ignore> callback subroutines
are inadvertently
modifying the test object.
The C<Test::Weaken::check_ignore> static method is
provided to make this debugging task easier.

=begin Marpa::Test::Display:

## start display
## next display
is_file($_, 't/ignore.t', 'check_ignore 1 arg snippet')

=end Marpa::Test::Display:

    $test = Test::Weaken::leaks(
        {   constructor => sub { MyObject->new },
            ignore => Test::Weaken::check_ignore( \&ignore_my_global ),
        }
    );

=begin Marpa::Test::Display:

## end display

=end Marpa::Test::Display:

=begin Marpa::Test::Display:

## start display
## next display
is_file($_, 't/ignore.t', 'check_ignore 4 arg snippet')

=end Marpa::Test::Display:

    $test = Test::Weaken::leaks(
        {   constructor => sub { DeepObject->new },
            ignore      => Test::Weaken::check_ignore(
                \&cause_deep_problem, 99, 0, $reporting_depth
            ),
        }
    );

=begin Marpa::Test::Display:

## end display

=end Marpa::Test::Display:

C<Test::Weaken::check_ignore> is a static method which constructs
a debugging wrapper from
four arguments, three of which are optional.
The first argument must be the ignore callback
which you are trying to debug.
This callback is the called test subject, or the
B<lab rat>.

The second, optional argument, is the maximum error count.
Below this count, errors are reported as warnings using C<carp>.
When the maximum error count is reached, an
exception is thrown using C<Marpa::exception>.
The maximum error count, if defined,
must be an number greater than or equal to 0.
By default the maximum error count is 1,
which means that the first error will be thrown
as an exception.

If the maximum error count is 0, all errors will be reported
as warnings and no exception will ever be thrown.
Infinite loops are a common behavior of
buggy lab rats,
and setting the maximum error
count to 0 will usually not be something you
want to do.

The third, optional, argument is the B<compare depth>.
It is the depth to which the probe referents will be checked,
as described below.
It must be a number greater than or equal to zero.
If the compare depth is zero, the probe referent is checked
to unlimited depth.
By default the compare depth is 0.

This fourth, optional, argument is the B<reporting depth>.
It is the depth to which the probe referents are dumped
in C<check_ignore>'s error messages.
It must be a number greater than or equal to -1.
If the reporting depth is zero, the object is dumped to unlimited depth.
If the reporting depth is -1, there is no dump in the error message.
By default, the reporting depth is -1.

C<Test::Weaken::check_ignore>
returns a reference to the wrapper callback.
If no problems are detected,
the wrapper callback behaves exactly like the lab rat callback,
except that the wrapper is slower.

To discover when and if the lab rat callback is
altering its arguments,
C<Test::Weaken::check_ignore>
compares the test object
before the call to the lab rat
with the test object after the lab rat returns.
C<Test::Weaken::check_ignore>
compares the before and after test object in two ways.
First, it dumps the test object's contents using
C<Data::Dumper>.
For comparison purposes,
the dump using C<Data::Dumper> is performed with C<Maxdepth>
set to the compare depth.
Second, if the immediate probe referent has builtin type REF,
C<Test::Weaken::check_ignore>
determines whether the immediate probe referent
is a weak reference or a strong one.

If either comparison shows a difference,
the wrapper treats it as a problem, and
produces an error message.
This error message is either a C<carp> warning or a
C<Marpa::exception> exception, depending on the number of error
messages already reported and the setting of the
maximum error count.
If the reporting depth is a non-negative number, the error
message include a dump from C<Data::Dumper> of the
test object.
C<Data::Dumper>'s C<Maxdepth>
for reporting purposes is the reporting depth.

A user who wants other features, such as deep checking
of the test object
for strengthened references,
can easily modify
C<Test::Weaken::check_ignore>.
C<Test::Weaken::check_ignore> is a static method
which does not use any C<Test::Weaken>
package resources.
It is easy to copy it from the C<Test::Weaken> source
and hack it up.
Because C<Test::Weaken::check_ignore> makes no use of
package resources, the hacked version does not need to
be part of the C<Test::Weaken> package.

=head1 EXPORTS

By default, C<Test::Weaken> exports nothing.  Optionally, C<leaks> may be exported.

=head1 IMPLEMENTATION

C<Test::Weaken> first recurses through the test object.
Starting from the test object reference,
it follows and tracks objects recursively,
as described above.
The test object is explored to unlimited depth,
looking for independent memory objects to track.
Independent objects visited during the recursion are recorded,
and no object is visited twice.
For each independent memory object, a
probe reference is created.

Once recursion through the test object is complete,
the probe references are weakened.
This prevents the probe references from interfering
with the normal deallocation of memory.
Next, the test object destructor is called,
if there is one.

Finally, the test object reference is undefined.
This should trigger the deallocation of all memory held by the test object.
To check that this happened, C<Test::Weaken> dereferences the probe references.
If the referent of a probe reference was deallocated,
the value of that probe reference will be C<undef>.
If a probe reference is still defined at this point,
it refers to an unfreed independent object.

=head1 AUTHOR

Jeffrey Kegler

=head1 BUGS

Please report any bugs or feature requests to C<bug-test-weaken at
rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Test-Weaken>.  I
will be notified, and then you'll automatically be notified of
progress on your bug as I make changes.

=head1 SUPPORT

You can find documentation for this module with the perldoc command.

=begin Marpa::Test::Display:

## skip display

=end Marpa::Test::Display:

    perldoc Test::Weaken

You can also look for information at:

=over 4

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/Test-Weaken>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/Test-Weaken>

=item * RT: CPAN's request tracker

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=Test-Weaken>

=item * Search CPAN

L<http://search.cpan.org/dist/Test-Weaken>

=back

=head1 SEE ALSO

Potential users will want to compare L<Test::Memory::Cycle> and
L<Devel::Cycle>, which examine existing structures non-destructively.
L<Devel::Leak> also covers similar ground, although it requires
Perl to be compiled with C<-DDEBUGGING> in order to work.  L<Devel::Cycle>
looks inside closures if PadWalker is present, a feature C<Test::Weaken>
does not have at present.

=head1 ACKNOWLEDGEMENTS

Thanks to jettero, Juerd and perrin of Perlmonks for their advice.
Thanks to Lincoln Stein (developer of L<Devel::Cycle>) for
test cases and other ideas.

After the first release of C<Test::Weaken>,
Kevin Ryde made several important suggestions
and provided test cases.
These provided the impetus for version 2.000000.

=head1 LICENSE AND COPYRIGHT

Copyright 2007-2009 Jeffrey Kegler, all rights reserved.

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl 5.10.

=cut

1;    # End of Test::Weaken

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
