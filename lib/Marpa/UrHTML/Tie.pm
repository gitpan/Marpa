package Marpa::UrHTML::Internal::Tie;

use 5.010;
use warnings;
use strict;
use integer;

# use Smart::Comments '-ENV';

### Using smart comments <where>...

use English qw( -no_match_vars );

use Marpa::Internal;

## no critic (Miscellanea::ProhibitTies)

sub fetch_attribute {
    my ($attribute) = @_;
    return if not $Marpa::UrHTML::Internal::TDESC_LIST;
    return
        if not my $first_tdesc = $Marpa::UrHTML::Internal::TDESC_LIST->[0];
    return if not my $type = $first_tdesc->[0];
    return if $type ne 'TOKEN_SPAN';
    my $first_token_number = $first_tdesc->[1];
    my $parse_instance     = $Marpa::UrHTML::Internal::PARSE_INSTANCE;
    Marpa::exception(
        qq{Attempt to fetch attribute "$attribute" from undefined parse instance}
    ) if not defined $parse_instance;
    my $tokens           = $parse_instance->{tokens};
    my $first_token      = $tokens->[$first_token_number];
    my $first_token_type = $first_token->[0];
    return if $first_token_type ne 'S';
    my $attribute_value = $first_token->[4]->{$attribute};
    return defined $attribute_value ? lc $attribute_value : undef;
} ## end sub fetch_attribute

package Marpa::Internal::Tie::Id;

sub TIESCALAR {
    my ($class) = @_;
    my $instance;    # not used for anything
    return bless \$instance => $class;
}

sub FETCH { return Marpa::UrHTML::Internal::Tie::fetch_attribute('id'); }

sub STORE {
    Marpa::exception('ID is not writeable');
}

tie $Marpa::UrHTML::ID, __PACKAGE__;

package Marpa::Internal::Tie::Class;

sub TIESCALAR {
    my ($class) = @_;
    my $instance;    # not used for anything
    return bless \$instance => $class;
}

sub FETCH { return Marpa::UrHTML::Internal::Tie::fetch_attribute('class'); }

sub STORE {
    Marpa::exception('CLASS is not writeable');
}

tie $Marpa::UrHTML::CLASS, __PACKAGE__;

1;
