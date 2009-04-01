
.PHONY: all all_tests critic display

all: \
    equation_show_symbols.out \
    equation_show_rules.out \
    equation_show_QDFA.out \
    equation_show_earley_sets.out \
    equation_show_bocage.out \
    equation_show_tree.out \
    equation_trace_values.out \
    self_show_rules.out \
    MDL_displays.errs \
    test_pod.out \
    all_tests

all_tests:
	-(cd ..; prove author.t/*.t)

critic:
	-(cd ..; prove author.t/critic.t)

display:
	-(cd ..; prove author.t/display.t)

equation_show_symbols.out:
	echo 2+2 | perl -I../lib ../bin/mdl parse -grammar \
        ../example/equation.marpa --symbols | \
	sed -e '/(2+2)==4/d' > $@

equation_show_rules.out:
	echo 2+2 | perl -I../lib ../bin/mdl parse -grammar \
        ../example/equation.marpa --rules | \
	sed -e '/(2+2)==4/d' > $@

equation_show_QDFA.out:
	echo 2+2 | perl -I../lib ../bin/mdl parse -grammar \
        ../example/equation.marpa --QDFA | \
	sed -e '/(2+2)==4/d' > $@

equation_show_earley_sets.out:
	echo 2+2 | perl -I../lib ../bin/mdl parse -grammar \
        ../example/equation.marpa --earley_sets | \
	sed -e '/(2+2)==4/d' > $@

equation_show_bocage.out:
	echo 2+2 | perl -I../lib ../bin/mdl parse -grammar \
        ../example/equation.marpa --bocage=2 | \
	sed -e '/(2+2)==4/d' > $@

equation_show_tree.out:
	echo 2+2 | perl -I../lib ../bin/mdl parse -grammar \
        ../example/equation.marpa --tree=1 | \
	sed -e '/(2+2)==4/d' > $@

equation_trace_values.out:
	echo 2+2 | perl -I../lib ../bin/mdl parse -grammar \
        ../example/equation.marpa --stdout --trace_values > $@

self_show_rules.out:
	cat minimal.marpa | \
	perl -I../lib ../bin/mdl parse -grammar ../bootstrap/self.marpa -rules > $@

MDL_displays.errs:
	echo a | perl -I../lib ../bin/mdl parse \
	    -grammar MDL_displays.marpa 2>&1 | tee $@

