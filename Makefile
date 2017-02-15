
.PHONY: all test_consistency_hs_lhs

all: diff_ok PdInterpreter.pdf

PdInterpreter.pdf: PdInterpreter.tex
	pdflatex PdInterpreter.tex

PdInterpreter.tex: PdInterpreter.lhs
	~/.cabal/bin/lhs2TeX PdInterpreter.lhs > PdInterpreter.tex

PdInterpreter: PdInterpreter.lhs
	rm -f PdInterpreter.o PdInterpreter.hi
	ghc -o PdInterpreter PdInterpreter.lhs

lhs.wav: PdInterpreter
	./PdInterpreter > lhs.wav

hs.wav: PdInterpreter_hs
	./PdInterpreter_hs > hs.wav

diff_ok: hs.wav lhs.wav
	diff hs.wav lhs.wav > diff_ok
	[ `stat -c '%s' diff_ok` = 0 ]

PdInterpreter_hs: PdInterpreter_hs.hs
	rm -f PdInterpreter.o PdInterpreter.hi
	ghc -o PdInterpreter_hs PdInterpreter_hs.hs

warn:
	make clean
	ghc -o PdInterpreter -Wall PdInterpreter.lhs

clean:
	rm -f PdInterpreter PdInterpreter.o PdInterpreter.hi PdInterpreter.tex PdInterpreter.pdf


