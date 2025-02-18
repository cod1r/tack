stubs.a: stubs.c
	cc -I/opt/homebrew/include stubs.c -c -o stubs.o -I`ocamlc -where` -fPIC
	ar -r libstubs.a stubs.o
