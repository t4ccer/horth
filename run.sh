echo "$1:"
stack run -- compile -f elf64 -i examples/$1.horth -o $1 && ./$1 ${@:2}
echo ''
echo ''
