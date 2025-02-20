libstubs.a stubs.o: stubs.c
	cc -c `sdl2-config --cflags` stubs.c -o stubs.o -I`ocamlc -where` -fpic
	ar -r libstubs.a stubs.o
