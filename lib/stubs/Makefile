stubs.a: stubs.c
	cc `sdl2-config --cflags` stubs.c -c -o stubs.o -I`ocamlc -where` -fPIC -lSDl2 `sdl2-config --libs`
	ar -r libstubs.a stubs.o
