package := Chawk
program := $(package)/chawk
sources := $(wildcard $(package)/*.hs)
objects := $(patsubst %.hs,%.hi,$(sources)) \
           $(patsubst %.hs,%.o, $(sources))


$(program): $(sources)
	ghc --make -o $@ $^

.PHONY: clean
clean:
	$(RM) $(program) $(objects)
