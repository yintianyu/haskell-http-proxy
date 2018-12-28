

all: proxy

proxy: proxy.hs cache.hs
	ghc -o $@ $^

.PHONY: clean

clean:
	rm proxy proxy.o cache.o proxy.hi cache.hi

