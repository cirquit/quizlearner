CC = ghc

helloworld: src/helloworld/helloworld.hs

	$(CC) src/helloworld/helloworld.hs -o bin/helloworld
	rm -f src/helloworld/*.o
	rm -f src/helloworld/*.a
	rm -f src/helloworld/*.hi


pages: src/pages/pages.hs

	$(CC) src/pages/pages.hs -o bin/pages
	rm -f src/pages/*.o
	rm -f src/pages/*.a
	rm -f src/pages/*.hi	