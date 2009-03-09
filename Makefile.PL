require 5.010;
use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
    NO_META             => 1,
    NAME                => 'Marpa',
    AUTHOR              => 'Jeffrey Kegler',
    VERSION_FROM        => 'lib/Marpa.pm',
    ABSTRACT_FROM       => 'lib/Marpa.pm',
    PL_FILES => {
        "lib/Marpa/Source.pm.PL" => "lib/Marpa/Source.pm"
    },
    PM => {
	'lib/Marpa/Raw_Source.pm'
	    => '$(INST_LIB)/Marpa/Raw_Source.pm',
	'lib/Marpa/Doc/Algorithm.pod'
	    => '$(INST_LIB)/Marpa/Doc/Algorithm.pod',
	'lib/Marpa/Doc/Bibliography.pod'
	    => '$(INST_LIB)/Marpa/Doc/Bibliography.pod',
	'lib/Marpa/Doc/Diagnostics.pod'
	    => '$(INST_LIB)/Marpa/Doc/Diagnostics.pod',
	'lib/Marpa/Doc/Internals.pod'
	    => '$(INST_LIB)/Marpa/Doc/Internals.pod',
	'lib/Marpa/Doc/Parse_Terms.pod'
	    => '$(INST_LIB)/Marpa/Doc/Parse_Terms.pod',
	'lib/Marpa/Doc/Plumbing.pod'
	    => '$(INST_LIB)/Marpa/Doc/Plumbing.pod',
	'lib/Marpa/Doc/Options.pod'
	    => '$(INST_LIB)/Marpa/Doc/Options.pod',
	'lib/Marpa/Doc/MDL.pod'
	    => '$(INST_LIB)/Marpa/Doc/MDL.pod',
	'lib/Marpa/Doc/To_Do.pod'
	    => '$(INST_LIB)/Marpa/Doc/To_Do.pod',
	'lib/Marpa/Lex.pm'
	    => '$(INST_LIB)/Marpa/Lex.pm',
	'lib/Marpa/MDL.pm'
	    => '$(INST_LIB)/Marpa/MDL.pm',
	'lib/Marpa/Grammar.pm'
	    => '$(INST_LIB)/Marpa/Grammar.pm',
	'lib/Marpa/Offset.pm'
	    => '$(INST_LIB)/Marpa/Offset.pm',
	'lib/Marpa/Recognizer.pm'
	    => '$(INST_LIB)/Marpa/Recognizer.pm',
	'lib/Marpa/Evaluator.pm'
	    => '$(INST_LIB)/Marpa/Evaluator.pm',
	'lib/Marpa.pm'
	    => '$(INST_LIB)/Marpa.pm',
	'lib/Marpa/trailer_Raw_Source.pm'
	    => '$(INST_LIB)/Marpa/trailer_Raw_Source.pm',
	'lib/Marpa/Source.pm'
	    => '$(INST_LIB)/Marpa/Source.pm',
	'lib/Marpa/Source.pm.PL'
	    => '$(INST_LIB)/Marpa/Source.pm.PL',
	'lib/Marpa/header_Raw_Source.pm'
	    => '$(INST_LIB)/Marpa/header_Raw_Source.pm',
    },
    EXE_FILES => [
                           'bin/mdl',
    ],
    PREREQ_PM => {
        'Test::More' => 0,
    },
    dist                => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
);
