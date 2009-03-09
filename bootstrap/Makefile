
.PHONY: all install install_raw install_stringified

all: test source_raw.pl

bootcopy1.pl: bootcopy0.pl self.marpa bootstrap_header.pl bootstrap_trailer.pl
	perl -I../lib bootcopy0.pl self.marpa bootstrap_header.pl bootstrap_trailer.pl > $@

bootcopy0.pl: bootstrap.pl self.marpa bootstrap_header.pl bootstrap_trailer.pl
	perl -I../lib bootstrap.pl self.marpa bootstrap_header.pl bootstrap_trailer.pl > $@

source_raw.pl: bootcopy1.pl self.marpa 
	perl -I../lib bootcopy1.pl self.marpa /dev/null /dev/null > $@

test: bootcopy0.pl bootcopy1.pl
	diff bootcopy0.pl bootcopy1.pl

install: test install_stringified install_raw

install_stringified: ../lib/Marpa/Source.pm

install_raw: ../lib/Marpa/Raw_Source.pm

../lib/Marpa/Source.pm: \
    ../lib/Marpa/Raw_Source.pm \
    ../lib/Marpa/Source.pm.PL \
    ../lib/Marpa/header_Raw_Source.pm
	(cd ..; perl -Ilib lib/Marpa/Source.pm.PL lib/Marpa/Source.pm )

../lib/Marpa/Raw_Source.pm: source_raw.pl ../lib/Marpa/header_Raw_Source.pm \
    ../lib/Marpa/trailer_Raw_Source.pm
	cat ../lib/Marpa/header_Raw_Source.pm source_raw.pl ../lib/Marpa/trailer_Raw_Source.pm \
	    > ../lib/Marpa/Raw_Source.pm
