
.PHONY: all all_tests critic

all: all_tests

source_files: \
    equation_show_symbols.out \
    equation_show_rules.out \
    equation_show_QDFA.out \
    equation_show_earley_sets.out \
    equation_show_bocage.out \
    equation_trace_values.out \
    self_show_rules.out

all_tests:
	-(cd ..; prove author.t/*.t ) 2>&1 | tee all.errs

critic:
	-(cd ..; prove author.t/critic.t) 2>&1 | tee critic.errs

display:
	-(cd ..; prove author.t/display.t)

pod:
	-(cd ..; prove author.t/pod.t)

equation_show_symbols.out:
	echo 2+2 | \
	perl -I../lib -I../example -Mequation \
	../bin/mdl parse \
	-grammar ../example/equation.marpa \
	--actions Marpa::Example::Equation \
	--symbols | \
	sed -e '/(2+2)==4/d' > $@

equation_show_rules.out:
	echo 2+2 | \
	perl -I../lib -I../example -Mequation \
	../bin/mdl parse \
	-grammar ../example/equation.marpa \
	--actions Marpa::Example::Equation \
        --rules | \
	sed -e '/(2+2)==4/d' > $@

equation_show_QDFA.out:
	echo 2+2 | \
	perl -I../lib -I../example -Mequation \
	../bin/mdl parse \
	-grammar ../example/equation.marpa \
	--actions Marpa::Example::Equation \
	--QDFA | \
	sed -e '/(2+2)==4/d' > $@

equation_show_earley_sets.out:
	echo 2+2 | \
	perl -I../lib -I../example -Mequation \
	../bin/mdl parse \
	-grammar ../example/equation.marpa \
	--actions Marpa::Example::Equation \
        --earley_sets | \
	sed -e '/(2+2)==4/d' > $@

equation_show_bocage.out:
	echo 2+2 | \
	perl -I../lib -I../example -Mequation \
	../bin/mdl parse \
	-grammar ../example/equation.marpa \
	--actions Marpa::Example::Equation \
        --bocage=3 | \
	sed -e '/(2+2)==4/d' > $@

equation_trace_values.out:
	echo 2+2 | \
	perl -I../lib -I../example -Mequation \
	../bin/mdl parse \
	-grammar ../example/equation.marpa \
	--actions Marpa::Example::Equation \
	--stdout --trace_values > $@
