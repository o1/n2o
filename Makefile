all: compile run

clean:
	find . -name .cm | xargs rm -rf
	rm -f n2o.x86-darwin

compile:
	ml-build n2o.cm N2O.main

run: compile
	./run
