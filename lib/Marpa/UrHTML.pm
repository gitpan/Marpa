package Marpa::UrHTML;

use 5.010;
use strict;
use warnings;

use Carp ();
use English qw( -no_match_vars );

use HTML::PullParser;
use HTML::Entities qw(decode_entities);
use HTML::Tagset ();
use Marpa;
use Marpa::Internal;
use Marpa::UrHTML::Tie;

# use Smart::Comments '-ENV';

package Marpa::UrHTML::Internal;

use Marpa::Internal;

sub per_element_handlers {
    my ( $element, $user_handlers ) = @_;
    return {} if not $element;
    return {} if not $user_handlers;
    my $wildcard_handlers    = $user_handlers->{ANY} // {};
    my %handlers             = %{$wildcard_handlers};
    my $per_element_handlers = $user_handlers->{$element} // {};
    @handlers{ keys %{$per_element_handlers} } =
        values %{$per_element_handlers};
    return \%handlers;
} ## end sub per_element_handlers

sub tdesc_list_to_text {
    my ( $self, $tdesc_list ) = @_;
    my $text     = q{};
    my $document = $self->{document};
    my $tokens   = $self->{tokens};
    TDESC: for my $tdesc ( @{$tdesc_list} ) {
        given ( $tdesc->[0] ) {
            when ('ELE') {
                my ( $first_token_id, $last_token_id, $value ) =
                    @{$tdesc}[ 1 .. $#{$tdesc} ];
                if ( defined $value ) {
                    $text .= $value;
                    break;    # next TDESC;
                }
                my $offset     = $tokens->[$first_token_id]->[1];
                my $end_offset = $tokens->[$last_token_id]->[2];
                $text .= substr ${$document}, $offset,
                    ( $end_offset - $offset );
            } ## end when ('ELE')
            when ('TOKEN_SPAN') {
                my ( $first_token_id, $last_token_id ) = @{$tdesc}[ 1, 2 ];
                my $offset     = $tokens->[$first_token_id]->[1];
                my $end_offset = $tokens->[$last_token_id]->[2];
                $text .= substr ${$document}, $offset, $end_offset - $offset;
            } ## end when ('TOKEN_SPAN')
            default {
                Marpa::exception(qq{Internal error: unknown tdesc type "$_"});
            }
        } ## end given
    } ## end for my $tdesc ( @{$tdesc_list} )
    return \$text;
} ## end sub tdesc_list_to_text

# Convert a list of text descriptions to text
sub default_top_handler {
    my ( $dummy, @tdesc_lists ) = @_;

    my $self = $Marpa::UrHTML::Internal::PARSE_INSTANCE;
    my @tdesc_list = map { @{$_} } @tdesc_lists;
    return tdesc_list_to_text( $self, \@tdesc_list );

} ## end sub default_top_handler

# Convert a list of text descriptions to a
# single, shortened text description
sub create_tdesc_handler {
    my ( $self, $element ) = @_;
    my $handlers_by_class =
        per_element_handlers( $element,
        ( $self ? $self->{user_handlers_by_class} : {} ) );
    my $handlers_by_id =
        per_element_handlers( $element,
        ( $self ? $self->{user_handlers_by_id} : {} ) );

    return sub {
        my ( $dummy, @tdesc_lists ) = @_;

        my @tdesc_list = map { @{$_} } @tdesc_lists;
        local $Marpa::UrHTML::Internal::TDESC_LIST = \@tdesc_list;
        my $self           = $Marpa::UrHTML::Internal::PARSE_INSTANCE;
        my $trace_fh       = $self->{trace_fh};
        my $trace_handlers = $self->{trace_handlers};

        my $tokens = $self->{tokens};

        my $user_handler;
        GET_USER_HANDLER: {
            if ( my $id = $Marpa::UrHTML::ID ) {
                if ( $user_handler = $handlers_by_id->{$id} ) {
                    if ($trace_handlers) {
                        say {$trace_fh}
                            "Resolved to user handler by element ($element) and id ($id)";
                    }
                    last GET_USER_HANDLER;
                } ## end if ( $user_handler = $handlers_by_id->{$id} )
            } ## end if ( my $id = $Marpa::UrHTML::ID )
            if ( my $class = $Marpa::UrHTML::CLASS ) {
                if ( $user_handler = $handlers_by_class->{$class} ) {
                    if ($trace_handlers) {
                        say {$trace_fh}
                            "Resolved to user handler by element ($element) and class ($class)";
                    }
                    last GET_USER_HANDLER;
                } ## end if ( $user_handler = $handlers_by_class->{$class} )
            } ## end if ( my $class = $Marpa::UrHTML::CLASS )
            $user_handler = $handlers_by_class->{ANY};
            if ( $trace_handlers and $user_handler ) {
                say {$trace_fh} +(
                    defined $element
                    ? "Resolved to user handler by element ($element)"
                    : 'Resolved to default user handler'
                );
            } ## end if ( $trace_handlers and $user_handler )
        } ## end GET_USER_HANDLER:
        if ( defined $user_handler ) {
            my $first_token = $tokens->[0]->[1];
            my $last_token  = $tokens->[-1]->[2];
            local $Marpa::UrHTML::Internal::NODE_SCRATCHPAD = {};
            return [
                [ ELE => $first_token, $last_token, $user_handler->() ] ];
        } ## end if ( defined $user_handler )

        my $doc          = $self->{doc};
        my @tdesc_result = ();

        my $last_token;
        my $first_token_id_in_current_span;
        my $last_token_id_in_current_span;

        TDESC: for my $tdesc ( @tdesc_list, ['FINAL'] ) {

            my $next_tdesc;
            my $first_token_id;
            my $last_token_id;
            PARSE_TDESC: {
                my $ref_type = ref $tdesc;
                if ( not $ref_type or $ref_type ne 'ARRAY' ) {
                    $next_tdesc = $tdesc;
                    last PARSE_TDESC;
                }
                given ( $tdesc->[0] ) {
                    when ('ELE') {
                        my $value = $tdesc->[3];
                        if ( not defined $value ) {
                            ( $first_token_id, $last_token_id ) =
                                @{$tdesc}[ 1, 2 ];
                            break;    # last PARSE_TDESC;
                        }
                        $next_tdesc = $tdesc;
                    } ## end when ('ELE')
                    when ('FINAL') {
                        $next_tdesc = $tdesc;
                    }
                    when ('TOKEN_SPAN') {
                        ( $first_token_id, $last_token_id ) =
                            @{$tdesc}[ 1, 2 ];
                    }
                    default {
                        Marpa::exception("Unknown text description type: $_");
                    }
                } ## end given
            } ## end PARSE_TDESC:

            if ( defined $first_token_id ) {
                if ( defined $first_token_id_in_current_span ) {
                    if ( $first_token_id
                        <= $last_token_id_in_current_span + 1 )
                    {
                        $last_token_id_in_current_span = $last_token_id;
                        next TDESC;
                    } ## end if ( $first_token_id <= ...)
                    push @tdesc_result,
                        [
                        'TOKEN_SPAN',
                        $first_token_id_in_current_span,
                        $last_token_id_in_current_span
                        ];
                } ## end if ( defined $first_token_id_in_current_span )
                $first_token_id_in_current_span = $first_token_id;
                $last_token_id_in_current_span  = $last_token_id;
                next TDESC;
            } ## end if ( defined $first_token_id )

            if ( defined $next_tdesc ) {
                if ( defined $first_token_id_in_current_span ) {
                    push @tdesc_result,
                        [
                        'TOKEN_SPAN',
                        $first_token_id_in_current_span,
                        $last_token_id_in_current_span
                        ];
                    $first_token_id_in_current_span =
                        $last_token_id_in_current_span = undef;
                } ## end if ( defined $first_token_id_in_current_span )
                my $ref_type = ref $next_tdesc;

                last TDESC
                    if $ref_type eq 'ARRAY' and $next_tdesc->[0] eq 'FINAL';
                push @tdesc_result, $next_tdesc;
            } ## end if ( defined $next_tdesc )

        } ## end for my $tdesc ( @tdesc_list, ['FINAL'] )

        return \@tdesc_result;
        }
} ## end sub create_tdesc_handler

my %ARGS = (
    start       => q{'S',offset,offset_end,tagname,attr},
    end         => q{'E',offset,offset_end,tagname},
    text        => q{'T',offset,offset_end,is_cdata},
    process     => q{'PI',offset,offset_end},
    comment     => q{'C',offset,offset_end},
    declaration => q{'D',offset,offset_end},

    # options that default on
    unbroken_text => 1,
);

sub add_handlers {
    my ( $self, $handler_spec_list ) = @_;
    HANDLER_SPEC: for my $handler_spec ( @{$handler_spec_list} ) {
        my $element;
        my $id;
        my $class;
        my $action;
        PARSE_HANDLER_SPEC: {
            my $ref_type = ref $handler_spec;
            if ( not defined $ref_type ) {
                Marpa::exception('undefined handler specification');
            }
            if ( $ref_type eq 'ARRAY' ) {
                my $specifier;
                ( $specifier, $action ) = @{$handler_spec};
                if ( $specifier eq 'TOP' ) {
                    $self->{user_top_handler} = $action;
                    next HANDLER_SPEC;
                }
                if ( $specifier eq 'DEFAULT' ) {
                    $self->{user_default_handler} = $action;
                    next HANDLER_SPEC;
                }
                if ( $specifier =~ /[A-Z]/xms ) {
                    Marpa::exception(
                        qq{Invalid CSS-style specifier: $specifier\n},
                        'Marpa wants CSS-style specifier to be all lower-case'
                    );
                } ## end if ( $specifier =~ /[A-Z]/xms )
                ( $element, $id ) =
                       ( $specifier =~ /\A ([^#]*) [#] (.*) \z/xms )
                    or ( $element, $class ) =
                    ( $specifier =~ /\A ([^.]*) [.] (.*) \z/xms )
                    or $element = $specifier;
                last PARSE_HANDLER_SPEC;
            } ## end if ( $ref_type eq 'ARRAY' )
            if ( $ref_type eq 'HASH' ) {
                $element = $handler_spec->{element};
                $id      = $handler_spec->{id};
                $class   = $handler_spec->{class};
                $action  = $handler_spec->{action};
                last PARSE_HANDLER_SPEC;
            } ## end if ( $ref_type eq 'HASH' )
            Marpa::exception(
                'handler specification must be ref to ARRAY or HASH');
        } ## end PARSE_HANDLER_SPEC:
        $element = $element ? lc $element : 'ANY';
        if ( defined $id ) {
            $self->{user_handlers_by_id}->{$element}->{ lc $id } = $action;
            next HANDLER_SPEC;
        }
        $class = defined $class ? lc $class : 'ANY';
        $self->{user_handlers_by_class}->{$element}->{$class} = $action;

    } ## end for my $handler_spec ( @{$handler_spec_list} )

    return 1;
} ## end sub add_handlers

sub Marpa::UrHTML::new {
    my ( $class, @hash_args ) = @_;
    my $self = bless {}, $class;
    $self->{trace_fh} = \*STDERR;
    for my $hash_arg (@hash_args) {
        for my $key ( keys %{$hash_arg} ) {
            given ($key) {
                when ( [qw(trace_fh trace_handlers)] ) {
                    $self->{$_} = $hash_arg->{$_}
                }
                when ('handlers') {
                    Marpa::UrHTML::Internal::add_handlers( $self,
                        $hash_arg->{$_} )
                }
                default {
                    Marpa::exception("Unknown option: $_");
                }
            } ## end given
        } ## end for my $key ( keys %{$hash_arg} )
    } ## end for my $hash_arg (@hash_args)
    return $self;
} ## end sub Marpa::UrHTML::new

%Marpa::UrHTML::Internal::BLOCK_ELEMENT = map { $_ => 1 } qw(
    h1 h2 h3 h4 h5 h6
    ul ol dir menu
    pre
    p dl div center
    noscript noframes
    blockquote form isindex hr
    table fieldset address
);

%Marpa::UrHTML::Internal::EMPTY_ELEMENT = map { $_ => 1 } qw(
    area base basefont br col frame hr
    img input isindex link meta param
);

%Marpa::UrHTML::Internal::OPTIONAL_TAGS =
    map { ( $_, 1 ) } qw( html head body tbody );
%Marpa::UrHTML::Internal::OPTIONAL_END_TAG = map { $_ => 1 } qw(
    colgroup dd dt li p td tfoot th thead tr
);

my @anywhere_rh_sides = qw(D C PI WHITESPACE);

# Start and end of optional-tag elements is simply
# ignored
push @anywhere_rh_sides, map { ( 'S_' . $_, 'E_' . $_ ) }
    keys %Marpa::UrHTML::Internal::OPTIONAL_TAGS;

@Marpa::UrHTML::Internal::CORE_TERMINALS =
    ( @anywhere_rh_sides, qw(CDATA PCDATA) );

# End tags for empty elements are ignored
push @anywhere_rh_sides,
    map { 'E_' . $_ } keys %Marpa::UrHTML::Internal::EMPTY_ELEMENTS;

my @anywhere_item_rules =
    map { { lhs => 'anywhere_item', rhs => [$_] } } @anywhere_rh_sides;

no strict 'refs';
*{'Marpa::UrHTML::Internal::default_action'} = create_tdesc_handler();
use strict;

@Marpa::UrHTML::Internal::CORE_RULES = (
    @anywhere_item_rules,

    {   lhs    => 'document',
        rhs    => ['flow'],
        action => '!top_handler',
    },
    { lhs => 'flow', rhs => ['terminated_flow_item'], min => 0 },

    # { lhs => 'flow',     rhs => ['U_ELE_p'], },
    # {   lhs => 'flow',
    # rhs => [qw(U_ELE_p block_terminating_flow)],
    # },
    # { lhs => 'block_terminating_flow', rhs => ['block_element'], },
    # { lhs => 'block_terminating_flow', rhs => [ 'block_element', 'flow' ], },
    # { lhs => 'inline_flow',            rhs => ['inline_flow_item'], },
    # { lhs => 'inline_flow',   rhs => [qw(inline_flow_item inline_flow)], },
    # { lhs => 'block_element', rhs => ['ELE_p'] },
    # { lhs => 'ELE_p',         rhs => ['T_ELE_p'] },
    # { lhs => 'ELE_p',         rhs => ['U_ELE_p'] },
    # { lhs => 'T_ELE_p',       rhs => [ 'S_p', 'inline_flow', 'E_p' ] },
    # { lhs => 'U_ELE_p', rhs => [ 'S_p', 'inline_flow' ] },
);

# push @Marpa::UrHTML::Internal::CORE_TERMINALS, qw(S_p E_p );

push @Marpa::UrHTML::Internal::CORE_RULES,
    map { { lhs => 'terminated_flow_item', rhs => [$_] } }
    qw(inline_flow_item block_element);

push @Marpa::UrHTML::Internal::CORE_RULES,
    map { { lhs => 'inline_flow_item', rhs => [$_] } }
    qw(CDATA PCDATA anywhere_item anywhere_element);

my %start_tags = ();
my %end_tags   = ();

sub Marpa::UrHTML::parse {
    my ( $self, $document_ref ) = @_;
    my $ref_type = ref $document_ref;
    Marpa::exception(
        'Arg to ' . __PACKAGE__ . '::parse must be ref to string' )
        if not $ref_type
            or $ref_type ne 'SCALAR';

    my %pull_parser_args;
    my $document = $pull_parser_args{doc} = $self->{document} = $document_ref;
    my $pull_parser = $self->{pull_parser} =
        HTML::PullParser->new( %pull_parser_args, %ARGS )
        || Carp::croak('Could not create pull parser');

    my @tokens = ();

    my %terminals = map { $_ => 1 } @Marpa::UrHTML::Internal::CORE_TERMINALS;
    my @html_parser_tokens = ();
    my @marpa_tokens       = ();
    while ( my $html_parser_token = $pull_parser->get_token ) {
        my $token_number = scalar @html_parser_tokens;
        push @html_parser_tokens, $html_parser_token;
        given ( $html_parser_token->[0] ) {
            when ('T') {
                my ( $offset, $offset_end, $is_cdata ) =
                    @{$html_parser_token}[ 1 .. $#{$html_parser_token} ];
                push @marpa_tokens,
                    [
                    (   substr(
                            ${$document}, $offset,
                            ( $offset_end - $offset )
                            ) =~ / \A \s* \z /xms ? 'WHITESPACE'
                        : $is_cdata ? 'CDATA'
                        : 'PCDATA'
                    ),
                    [ [ 'TOKEN_SPAN', $token_number, $token_number ] ],
                    ];
            } ## end when ('T')
            when ('S') {
                my ( $offset, $offset_end, $tag_name ) =
                    @{$html_parser_token}[ 1 .. $#{$html_parser_token} ];
                $start_tags{$tag_name}++;
                my $terminal = $_ . q{_} . $tag_name;
                $terminals{$terminal}++;
                push @marpa_tokens,
                    [
                    $terminal,
                    [ [ 'TOKEN_SPAN', $token_number, $token_number ] ],
                    ];
            } ## end when ('S')
            when ('E') {
                my ( $offset, $offset_end, $tag_name ) =
                    @{$html_parser_token}[ 1 .. $#{$html_parser_token} ];
                $end_tags{$tag_name}++;
                my $terminal = $_ . q{_} . $tag_name;
                $terminals{$terminal}++;
                push @marpa_tokens,
                    [
                    $terminal,
                    [ [ 'TOKEN_SPAN', $token_number, $token_number ] ],
                    ];
            } ## end when ('E')
            when ( [qw(C D)] ) {
                my ( $offset, $offset_end ) =
                    @{$html_parser_token}[ 1 .. $#{$html_parser_token} ];
                push @marpa_tokens,
                    [
                    $_, [ [ 'TOKEN_SPAN', $token_number, $token_number ] ],
                    ];
            } ## end when ( [qw(C D)] )
            when ( ['PI'] ) {
                my ( $offset, $offset_end ) =
                    @{$html_parser_token}[ 1 .. $#{$html_parser_token} ];
                push @marpa_tokens,
                    [
                    $_, [ [ 'TOKEN_SPAN', $token_number, $token_number ] ],
                    ];
            } ## end when ( ['PI'] )
            default { Carp::croak("Unprovided-for event: $_") }
        } ## end given
        $token_number++;
    } ## end while ( my $html_parser_token = $pull_parser->get_token)

    my @rules     = @Marpa::UrHTML::Internal::CORE_RULES;
    my @terminals = keys %terminals;

    my %element_actions = ();
    ELEMENT: for ( keys %start_tags ) {
        when ( defined $Marpa::UrHTML::Internal::OPTIONAL_TAGS{$_} ) {

            # All these tags are simply ignored
            # should be next ELEMENT, but perl bug 65114 causes warning if tag is given
            # next ELEMENT
            next;
        } ## end when ( defined $Marpa::UrHTML::Internal::OPTIONAL_TAGS...)
        when ( defined $Marpa::UrHTML::Internal::OPTIONAL_END_TAG{$_} ) {

            # These will need custom solutions
            # A dummy rule for now
            push @rules, {
                lhs        => "ELE_$_",
                    rhs    => [ "S_$_", "Contents_$_", "E_$_" ],
                    action => "!ELE_$_",
            }, {
                lhs        => "UELE_$_",
                    rhs    => [ "S_$_", "Contents_$_" ],
                    action => "!ELE_$_",
            },

                # a rule which will never be satisfied because
                # there are no unicorns
            {
                lhs     => "Contents_$_",
                    rhs => [q{!!!unicorn!!!}]
            };
            $element_actions{"!ELE_$_"} = $_;
        } ## end when ( defined $Marpa::UrHTML::Internal::OPTIONAL_END_TAG...)
        when ( defined $Marpa::UrHTML::Internal::EMPTY_ELEMENT{$_} ) {
            push @rules, {
                lhs        => "ELE_$_",
                    rhs    => ["S_$_"],
                    action => "!ELE_$_",
            };
            $element_actions{"!ELE_$_"} = $_;
        } ## end when ( defined $Marpa::UrHTML::Internal::EMPTY_ELEMENT...)
        default {
            my $this_element = "ELE_$_";
            push @rules,
                {
                lhs    => $this_element,
                rhs    => [ "S_$_", "Contents_$_", "E_$_" ],
                action => "!ELE_$_",
                },
                {
                lhs => "Contents_$_",
                rhs => ['flow']
                };
            my $element_type =
                $Marpa::UrHTML::Internal::BLOCK_ELEMENT{$_}
                ? 'block_element'
                : 'anywhere_element';
            push @rules,
                {
                lhs => $element_type,
                rhs => [$this_element],
                };
            $element_actions{"!ELE_$_"} = $_;
        } ## end default
    } ## end for ( keys %start_tags )

    my $grammar = Marpa::Grammar->new(
        {   rules          => \@rules,
            start          => 'document',
            terminals      => \@terminals,
            default_action => (
                $self->{user_default_handler}
                    // 'Marpa::UrHTML::Internal::default_action'
            ),
            strip => 0,
        }
    );
    $grammar->precompute();

    # say STDERR $grammar->show_rules();
    # say STDERR $grammar->show_QDFA();
    my $recce = Marpa::Recognizer->new( { grammar => $grammar } );
    $self->{tokens} = \@html_parser_tokens;
    $recce->tokens( \@marpa_tokens );

    my %closure = (
        '!top_handler' => (
            $self->{user_top_handler}
                // \&Marpa::UrHTML::Internal::default_top_handler
        )
    );

    ELEMENT:
    while ( my ( $element_action, $element ) = each %element_actions ) {
        $closure{$element_action} = create_tdesc_handler( $self, $element );
    }

    my $value = do {
        local $Marpa::UrHTML::Internal::PARSE_INSTANCE = $self;
        local $Marpa::UrHTML::INSTANCE                 = {};
        my $evaler = Marpa::Evaluator->new(
            { recce => $recce, closures => \%closure, } );
        $evaler->value;
    };
    Marpa::exception('undef returned') if not defined $value;
    return $value;

} ## end sub Marpa::UrHTML::parse

1;

__END__

=head1 NAME

Marpa::UrHTML - Element-level HTML Parser

=head1 SYNOPSIS

=head1 DESCRIPTION

=head1 AUTHOR

Jeffrey Kegler

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

The starting template for this code was
HTML::TokeParser, by Gisle Aas.

=head1 LICENSE AND COPYRIGHT

Copyright 2007-2009 Jeffrey Kegler, all rights reserved.

This program is free software; you can redistribute
it and/or modify it under the same terms as Perl 5.10.0.

=cut
