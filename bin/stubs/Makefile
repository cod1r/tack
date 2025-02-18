stubs.a: stubs.c
	cc -I/opt/homebrew/include -L/opt/homebrew/lib stubs.c -c -o stubs.o -I`ocamlc -where` -L`ocamlc -where` -fPIC -lSDl2
	ar -r libstubs.a stubs.o
