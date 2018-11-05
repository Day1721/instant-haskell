all: compiler

STACK_IN_PATH := $(shell command -v stack 2> /dev/null)

compiler:
ifdef STACK_IN_PATH
	stack build
else
	../../PUBLIC/MRJP/Stack/stack-1.6.5 build
endif
	cp src/scripts/insc_jvm.sh insc_jvm
	cp src/scripts/insc_llvm.sh insc_llvm

clean:
	rm insc_jvm insc_llvm
