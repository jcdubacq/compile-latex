COMPILELATEX=../src/compile-latex
# start automatic section for //common
COMPILELATEX ?= compile-latex
all::
.PHONY: do depends distclean clean all
# stop automatic section for //common
# start automatic section for 00.tex
all:: 00.pdf
do::
	$(COMPILELATEX) 00
00.md5 00.pdf 00.log:\
00.tex
	$(COMPILELATEX) 00
distclean::
	rm -f 00.aux 00.log 00.md5 00.pdf
clean::
	rm -f 00.aux 00.log 00.md5
depends::
	$(COMPILELATEX) --gitignore --makefile 'Makefile' 00.tex
# stop automatic section for 00.tex
# start automatic section for 01.tex
all:: 01.pdf
do::
	$(COMPILELATEX) 01
01.pdf 01.md5 01.ilg 01.log:\
01.tex
	$(COMPILELATEX) 01
distclean::
	rm -f 01.aux 01.idx 01.ilg 01.ind 01.log 01.md5 01.pdf
clean::
	rm -f 01.aux 01.idx 01.ilg 01.ind 01.log 01.md5
depends::
	$(COMPILELATEX) --gitignore --makefile 'Makefile' 01.tex
# stop automatic section for 01.tex
# start automatic section for 02.tex
all:: 02.pdf
do::
	$(COMPILELATEX) --manual 02
02.log 02.pdf 02.md5 02.ilg:\
02.tex
	$(COMPILELATEX) --manual 02
distclean::
	rm -f 02.adx 02.and 02.aux 02.idx 02.ilg 02.ind 02.log 02.md5 02.pdf
clean::
	rm -f 02.adx 02.and 02.aux 02.idx 02.ilg 02.ind 02.log 02.md5
depends::
	$(COMPILELATEX) --gitignore --makefile 'Makefile' 02.tex
# stop automatic section for 02.tex
# start automatic section for 03.tex
all:: 03.pdf
do::
	$(COMPILELATEX) 03
03.blg 03.log 03.pdf 03.md5:\
03.tex\
03-annex.tex\
test.bib
	$(COMPILELATEX) 03
distclean::
	rm -f 03.aux 03.bbl 03.blg 03.log 03.md5 03.pdf
clean::
	rm -f 03.aux 03.bbl 03.blg 03.log 03.md5
depends::
	$(COMPILELATEX) --gitignore --makefile 'Makefile' 03.tex
# stop automatic section for 03.tex
# start automatic section for 04.tex
all:: 04.pdf
do::
	$(COMPILELATEX) 04
04.blg 04.md5 04.log 04.pdf xampl.blg:\
04.tex\
xampl.bib\
test.bib
	$(COMPILELATEX) 04
distclean::
	rm -f 04.aux 04.bbl 04.blg 04.log 04.md5 04.pdf xampl.aux xampl.bbl xampl.blg
clean::
	rm -f 04.aux 04.bbl 04.blg 04.log 04.md5 xampl.aux xampl.bbl xampl.blg
depends::
	$(COMPILELATEX) --gitignore --makefile 'Makefile' 04.tex
# stop automatic section for 04.tex
# start automatic section for 05.tex
all:: 05.pdf
do::
	$(COMPILELATEX) 05
05.log 05.pdf 05.md5:\
05.tex
	$(COMPILELATEX) 05
distclean::
	rm -f 05-annex.tex 05.aux 05.log 05.md5 05.pdf
clean::
	rm -f 05-annex.tex 05.aux 05.log 05.md5
depends::
	$(COMPILELATEX) --gitignore --makefile 'Makefile' 05.tex
# stop automatic section for 05.tex
