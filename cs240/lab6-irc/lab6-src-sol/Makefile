CXX = g++ -fPIC
NETLIBS= -lnsl -lsocket

all: HashTableVoidTest talk-server test-talk-server

HashTableVoidTest: HashTableVoidTest.cc HashTableVoid.cc
	g++ -g -o HashTableVoidTest HashTableVoidTest.cc HashTableVoid.cc

talk-server: talk-server.c
	gcc -g -o talk-server talk-server.c linked_list.c

test-talk-server: test-talk-server.c
	gcc -g -o test-talk-server test-talk-server.c linked_list.c

talk-client: talk-client.c
	gcc -g -o talk-client talk-client.c linked_list.c -lcurses

clean:
	rm -f *.o use-dlopen hello.so HashTableVoidTest
	rm -f *.o talk-server

