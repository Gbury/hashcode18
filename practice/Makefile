# copyright (c) 2014, guillaume bury

J?=3
TAR=../source.tar.gz
INPUT=$(shell find input -name '*.in')
INPUT_RAW=$(notdir $(INPUT))
OUTPUT=$(addprefix output/, $(INPUT_RAW:.in=.out))

all: bin

bin:
	jbuilder build main.exe -j $J

clean:
	jbuilder clean

doc:
	jbuilder build @doc

debug:
	@echo "INPUT: $(INPUT)"
	@echo "INPUT_RAW: $(INPUT_RAW)"
	@echo "OUTPUT: $(OUTPUT)"

test: $(OUTPUT)

output/%.out: input/%.in bin
	_build/default/main.exe < $< > $@

nuke:
	rm -rf output/*.out

zip:
	git archive -o $(TAR) HEAD

.PHONY: clean doc all bin zip
