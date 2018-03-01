# copyright (c) 2014, guillaume bury

J?=3
TAR=../source.tar.gz
INPUT=$(shell find input -name '*.in')
INPUT_RAW=$(notdir $(INPUT))
OUTPUT=$(addprefix output/, $(INPUT_RAW:.in=.out))

SOURCES=$(shell find . -name '*.ml')
BIN=_build/default/main.exe

all: $(BIN)

$(BIN): $(SOURCES)
	jbuilder build main.exe -j $J

clean:
	jbuilder clean

doc:
	jbuilder build @doc

test: $(OUTPUT)

output/%.out: input/%.in $(BIN)
	OCAMLRUNPARAM=b $(BIN) < $< > $@

analyze/%.analyze: input/%.in output/%.out $(BIN)
	OCAMLRUNPARAM=b $(BIN) input/$*.in output/$*.out > $@

nuke:
	rm -rf output/*.out

zip:
	git archive -o $(TAR) HEAD

.PHONY: clean doc all bin zip
