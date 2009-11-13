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

sub create_fetch_attribute_closure {
    my ($attribute) = @_;
    return sub {
        return if not $Marpa::UrHTML::Internal::TDESC_LIST;
        return
            if not my $first_tdesc =
                $Marpa::UrHTML::Internal::TDESC_LIST->[0];
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
        }
} ## end sub create_fetch_attribute_closure

sub tiescalar_stub {
    my ($class) = @_;
    my $instance;    # not used for anything
    return bless \$instance => $class;
}

sub create_not_writeable {
    my ($var_name) = @_;
    return sub { Marpa::exception( $var_name . ' is not writeable' ) }
}

sub tie_attribute {
    my ($attribute_name) = @_;
    my $package_name =
        'Marpa::UrHTML::Internal::Tie::' . ucfirst $attribute_name;
    my $tied_var_name = 'Marpa::UrHTML::Attribute::' . uc $attribute_name;

    no strict 'refs';
    *{ $package_name . '::TIESCALAR' } =
        *{'Marpa::UrHTML::Internal::Tie::tiescalar_stub'}{'CODE'};
    *{ $package_name . '::FETCH' } =
        Marpa::UrHTML::Internal::Tie::create_fetch_attribute_closure(
        lc $attribute_name );
    *{ $package_name . '::STORE' } =
        Marpa::UrHTML::Internal::Tie::create_not_writeable(
        uc $attribute_name );
    use strict;

    my $var;
    tie $var, $package_name;

    no strict 'refs';
    *{$tied_var_name} = \$var;
    use strict;

    return 1;
} ## end sub tie_attribute

tie_attribute('id');
tie_attribute('class');
tie_attribute('title');

# Allow Short form of the attribute names,
# when there are no conflicts
no strict 'refs';
*{'Marpa::UrHTML::ID'}    = *{'Marpa::UrHTML::Attribute::ID'}{'SCALAR'};
*{'Marpa::UrHTML::CLASS'} = *{'Marpa::UrHTML::Attribute::CLASS'}{'SCALAR'};
*{'Marpa::UrHTML::TITLE'} = *{'Marpa::UrHTML::Attribute::TITLE'}{'SCALAR'};
use strict;

package Marpa::UrHTML::Internal::Tie;

sub set_up_elements {
    my $elements = $Marpa::UrHTML::Internal::NODE_SCRATCHPAD->{elements} = [];

    # Assume the parent checked to see if this exists
    my $tdesc_list = $Marpa::UrHTML::Internal::TDESC_LIST;
    TDESC:
    for my $tdesc_ix ( 0 .. $#{$Marpa::UrHTML::Internal::TDESC_LIST} ) {
        next TDESC if $tdesc_list->[$tdesc_ix]->[0] ne 'ELE';
        push @{$elements}, $tdesc_ix;
    }
    return $elements;
} ## end sub set_up_elements

package Marpa::UrHTML::Internal::Tie::Element_Values;

no strict 'refs';
*{ __PACKAGE__ . '::TIESCALAR' } =
    *{'Marpa::UrHTML::Internal::Tie::tiescalar_stub'}{'CODE'};
*{ __PACKAGE__ . '::STORE' } =
    Marpa::UrHTML::Internal::Tie::create_not_writeable('ELEMENT_VALUES');
use strict;

sub FETCH {
    my $tdesc_list = $Marpa::UrHTML::Internal::TDESC_LIST;
    Marpa::exception('Attempt to get element values of non-existent node')
        if not defined $tdesc_list;
    my $elements = $Marpa::UrHTML::Internal::NODE_SCRATCHPAD->{elements}
        // Marpa::UrHTML::Internal::Tie::set_up_elements();
    return [ map { $_->[3] } @{$tdesc_list}[ @{$elements} ] ];
} ## end sub FETCH

tie $Marpa::UrHTML::ELEMENT_VALUES, __PACKAGE__;

package Marpa::UrHTML::Internal::Tie::Literal;

no strict 'refs';
*{ __PACKAGE__ . '::TIESCALAR' } =
    *{'Marpa::UrHTML::Internal::Tie::tiescalar_stub'}{'CODE'};
*{ __PACKAGE__ . '::STORE' } =
    Marpa::UrHTML::Internal::Tie::create_not_writeable('LITERAL');
use strict;

sub FETCH {
    my $tdesc_list = $Marpa::UrHTML::Internal::TDESC_LIST;
    Marpa::exception('Attempt to get element values of non-existent node')
        if not defined $tdesc_list;
    my $parse_instance = $Marpa::UrHTML::Internal::PARSE_INSTANCE;
    Marpa::exception(
        'Attempt to read element values in undefined parse instance')
        if not defined $parse_instance;
    return Marpa::UrHTML::Internal::tdesc_list_to_text( $parse_instance,
        $tdesc_list );
} ## end sub FETCH

tie $Marpa::UrHTML::LITERAL, __PACKAGE__;

1;
