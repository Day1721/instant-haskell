stack exec insc_jvm $1 &&
java -jar runtime/jasmin.jar "${1%.*}.j" -d $(dirname "$1")