
build:
	(cd ..;perl Build.PL)
	(cd ..;./Build realclean)
	(cd ../lib/Marpa/MDL; make -W self.mdl install)
	(cd ..;perl Build.PL)
	(cd ..;./Build)
	(cd ..;./Build distmeta)
	(cd ..;./Build test)
	(cd ..;./Build disttest)

update:
	(cd ..;perl Build.PL)
	(cd ..;./Build)
	(cd ..;./Build distmeta)
	(cd ..;./Build dist)
	(cd ..;perl Build.PL)

devlib:
	(cd ..;perl Build.PL)
	(cd ..;./Build install --install_base $$HOME/devlib)
