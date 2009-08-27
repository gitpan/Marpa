
build:
	(cd ..;perl Build.PL)
	(cd ..;./Build realclean)
	(cd ../bootstrap; make -W self.marpa install)
	(cd ..;perl Build.PL)
	(cd ..;./Build)
	(cd ..;./Build test)
	(cd ..;./Build disttest)
