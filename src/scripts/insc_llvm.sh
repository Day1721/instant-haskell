stack exec insc_llvm $1 &&
llvm-as -o "${1%.*}.bs" "${1%.*}.ll"