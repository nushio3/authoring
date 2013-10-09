all: build

.PHONY: init build doc test install ghc-head


init:
	cabal install --only-dependencies --enable-tests
	cabal configure --enable-tests

build:
	cabal build

test: build
	cabal test

install: init build test
	cabal install

doc:
	cabal haddock --hyperlink-source

ghc-7.6.3:
	wget --quiet -O ghc.tar.bz2 http://www.haskell.org/ghc/dist/7.6.3/ghc-7.6.3-x86_64-unknown-linux.tar.bz2
	tar xf ghc.tar.bz2
	sudo apt-get install libgmp3c2 libgmp3-dev  libghc-zlib-dev -y
	cd ghc-7.6.3/; ./configure;     sudo make install
	cabal update
	ghc --version
	cabal --version
