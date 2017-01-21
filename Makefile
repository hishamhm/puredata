
.PHONY: all test_consistency_hs_lhs

all: puredata diff_ok puredata.pdf

puredata.pdf: puredata.tex
	pdflatex puredata.tex

puredata.tex: puredata.lhs
	~/.cabal/bin/lhs2TeX puredata.lhs > puredata.tex

diff_ok: puredata
	./puredata | tr ',' '\n' | tr -d '[]' > lhs_lines
	diff lines lhs_lines > diff_ok
	[ `stat -c '%s' diff_ok` = 0 ]

puredata: puredata.lhs
	ghc -o puredata puredata.lhs

warn:
	make clean
	ghc -o puredata -Wall puredata.lhs

clean:
	rm -f puredata puredata.hi puredata.tex puredata.pdf


