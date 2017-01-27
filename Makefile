
.PHONY: all test_consistency_hs_lhs

all: diff_ok puredata.pdf

puredata.pdf: puredata.tex
	pdflatex puredata.tex

puredata.tex: puredata.lhs
	~/.cabal/bin/lhs2TeX puredata.lhs > puredata.tex

puredata: puredata.lhs
	rm -f puredata.o puredata.hi
	ghc -o puredata puredata.lhs

lhs_lines: puredata
	./puredata | tr ',' '\n' | tr -d '[]' > lhs_lines

hs_lines: puredata_hs
	./puredata_hs | tr ',' '\n' | tr -d '[]' > hs_lines

diff_ok: hs_lines lhs_lines
	diff hs_lines lhs_lines > diff_ok
	[ `stat -c '%s' diff_ok` = 0 ]

puredata_hs: puredata.hs
	rm -f puredata.o puredata.hi
	ghc -o puredata_hs puredata.hs

warn:
	make clean
	ghc -o puredata -Wall puredata.lhs

clean:
	rm -f puredata puredata.o puredata.hi puredata.tex puredata.pdf


