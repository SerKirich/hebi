HC    = ghc
ALEX  = alex
HAPPY = happy

BIN_PREFIX = /usr/bin
SHR_PREFIX = /usr/share

build: all clean

install:
	mkdir           $(SHR_PREFIX)/hebi
	cp arduino.hebi $(SHR_PREFIX)/hebi
	cp Buildfile    $(SHR_PREFIX)/hebi
	ln hebi         $(BIN_PREFIX)/hebi

uninstall:
	rm -r $(SHR_PREFIX)/hebi
	rm    $(BIN_PREFIX)/hebi

all: Native/Lexer.hs Native/Parser.hs
	$(HC) --make Main.hs -o hebi

dist: build
	rm -r Hebi
	mkdir Hebi
	cp -r Builder    Hebi
	cp -r Foreign    Hebi
	cp -r Native     Hebi
	cp -r Translator Hebi
	cp *.hs          Hebi
	cp arduino.hebi  Hebi
	cp Buildfile     Hebi
	cp Makefile      Hebi
	tar cf hebi.tar  Hebi
	gzip hebi.tar
	rm -r Hebi
	mkdir Hebi
	mv hebi.tar.gz   Hebi
	cp README.md     Hebi

again: clean all

Native/Lexer.hs:
	$(ALEX) Native/Lexer.x

Native/Parser.hs: Native/Lexer.hs
	$(HAPPY) Native/Parser.y

clean:
	rm ./*.hi ./*.o; \
	rm ./*/*.hi ./*/*.o; \
	rm ./Native/Lexer.hs; \
	rm ./Native/Parser.hs
