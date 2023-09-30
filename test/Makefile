
.PHONY : default clean golden all compare force-recompile

default : clean all compare

clean :
	@rm -rf build/*

cleanGolden :
	@rm -rf golden/*

all : Test/*.agda
	@for f in $^; do \
			echo == Compiling $${f} == ; \
			./agda2train $${f} -o build ; \
	done

golden :
	@cp -r build/* golden

compare : all
	@echo == Comparing output ==
	@diff -r build golden

force-recompile :
