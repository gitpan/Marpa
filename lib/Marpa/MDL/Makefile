
all: test

install:
	cp try3.mdl.raw self.mdl.raw

try1.mdl.raw: self.mdl bootstrap.pl
	perl -I../../ bootstrap.pl self.mdl > $@

try2.mdl.raw: try1.mdl.raw self.mdl
	PERL5LIB=$$PERL5LIB:../.. ../MDLex/bin/mdlex_dump -OMarpa::MDL::Internal::Actions \
		try1.mdl.raw self.mdl > $@

try3.mdl.raw: try2.mdl.raw self.mdl
	PERL5LIB=$$PERL5LIB:../.. ../MDLex/bin/mdlex_dump -OMarpa::MDL::Internal::Actions \
		try2.mdl.raw self.mdl > $@

test:  try2.mdl.raw try3.mdl.raw
	diff try2.mdl.raw try3.mdl.raw
