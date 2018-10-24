all: compiler

compiler:
	stack build
	cp src/scripts/insc_jvm.sh insc_jvm
	cp src/scripts/insc_llvm.sh insc_llvm

clean:
	rm insc_jvm insc_llvm
