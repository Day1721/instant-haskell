stack exec insc_llvm $1 &&
llvm-as -o "${1%.*}.bc" "${1%.*}.ll"