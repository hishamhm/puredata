
.PHONY: all test_consistency_hs_lhs

all: diff_ok puredata.pdf

puredata.pdf: puredata.tex
	pdflatex puredata.tex

puredata.tex: puredata.lhs
	~/.cabal/bin/lhs2TeX puredata.lhs > puredata.tex

puredata: puredata.lhs
	rm -f puredata.o puredata.hi
	ghc -o puredata puredata.lhs

lhs.wav: puredata
	./puredata > lhs.wav

hs.wav: puredata_hs
	./puredata_hs > hs.wav

diff_ok: hs.wav lhs.wav
	diff hs.wav lhs.wav > diff_ok
	[ `stat -c '%s' diff_ok` = 0 ]

puredata_hs: puredata.hs
	rm -f puredata.o puredata.hi
	ghc -o puredata_hs puredata.hs

warn:
	make clean
	ghc -o puredata -Wall puredata.lhs

clean:
	rm -f puredata puredata.o puredata.hi puredata.tex puredata.pdf


