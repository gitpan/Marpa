#!/usr/bin/perl

use 5.010;
use warnings;
use strict;
use English qw( -no_match_vars );
use Fatal qw( open close );
use Test::More tests => 5;
use lib 'lib';

BEGIN {
    Test::More::use_ok( 'Marpa', 'alpha' );
    Test::More::use_ok('Marpa::MDL');
}

use Marpa::Test;

my $self_mdl;
{
    local $RS = undef;
    open my $fh, q{<}, 'lib/Marpa/MDL/self.mdl';
    $self_mdl = <$fh>;
    close $fh;
};

my ( $marpa_options_1, $mdlex_options_1 ) = Marpa::MDL::to_raw($self_mdl);

my $action_object_option =
    { action_object => 'Marpa::MDL::Internal::Actions' };

my $data2 =
    Marpa::MDLex::mdlex( [ $action_object_option, @{$marpa_options_1} ],
    $mdlex_options_1, $self_mdl );
Test::More::ok( ( ref $data2 ), 'Second bootstrap' );
my ( $marpa_options_2, $mdlex_options_2 ) =
    @{ ${$data2} }{qw(marpa_options mdlex_options)};

my $data3 =
    Marpa::MDLex::mdlex( [ $action_object_option, @{$marpa_options_2} ],
    $mdlex_options_2, $self_mdl );
Test::More::ok( ( ref $data3 ), 'Third bootstrap' );

Test::More::is_deeply( $data2, $data3,
    'Compare Output of Bootstraps 2 and 3' );
