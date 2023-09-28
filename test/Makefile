
.PHONY : default clean golden all compare force-recompile

default : clean all compare

clean :
	@rm -rf build/*

all : Test/*.agda
	for f in $^; do \
			echo == Compiling $${f} == ; \
			./agda2train $${f} -o build ; \
	done

compare :
	@echo == Comparing output ==
	@diff -r build golden

force-recompile :

golden :
	@cp -r build/* golden
