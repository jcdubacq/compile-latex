COMPILELATEX=../src/compile-latex
# start automatic section for //common
COMPILELATEX ?= compile-latex
all::
.PHONY: do depends-as-is depends distclean clean all
# stop automatic section for //common
# start automatic section for 00
all:: 00.pdf
do::
	$(COMPILELATEX) --load-none --jobname 00 --discover 00.tex
00.md5 00.pdf 00.log:\
00.tex
	$(COMPILELATEX) --load-none --jobname 00 --discover 00.tex
distclean::
	rm -f 00.aux 00.log 00.md5 00.pdf
clean::
	rm -f 00.aux 00.log 00.md5
depends::
	$(COMPILELATEX) --load-none --ignore --makefile Makefile --jobname 00 00.tex
depends-as-is::
	$(COMPILELATEX) --ignore --makefile Makefile --jobname 00 00.tex
# stop automatic section for 00
# start automatic section for 01
all:: 01.pdf
do::
	$(COMPILELATEX) --load-none --jobname 01 --discover 01.tex
01.pdf 01.md5 01.ilg 01.log:\
01.tex
	$(COMPILELATEX) --load-none --jobname 01 --discover 01.tex
distclean::
	rm -f 01.aux 01.idx 01.ilg 01.ind 01.log 01.md5 01.pdf
clean::
	rm -f 01.aux 01.idx 01.ilg 01.ind 01.log 01.md5
depends::
	$(COMPILELATEX) --load-none --ignore --makefile Makefile --jobname 01 01.tex
depends-as-is::
	$(COMPILELATEX) --ignore --makefile Makefile --jobname 01 01.tex
# stop automatic section for 01
# start automatic section for 02
all:: 02.pdf
do::
	$(COMPILELATEX) --load-none --jobname 02 --discover 02.tex
02.log 02.pdf 02.md5 02.ilg:\
02.tex
	$(COMPILELATEX) --load-none --jobname 02 --discover 02.tex
distclean::
	rm -f 02.adx 02.and 02.aux 02.idx 02.ilg 02.ind 02.log 02.md5 02.pdf
clean::
	rm -f 02.adx 02.and 02.aux 02.idx 02.ilg 02.ind 02.log 02.md5
depends::
	$(COMPILELATEX) --load-none --ignore --makefile Makefile --jobname 02 02.tex
depends-as-is::
	$(COMPILELATEX) --ignore --makefile Makefile --jobname 02 02.tex
# stop automatic section for 02
# start automatic section for 03
all:: 03.pdf
do::
	$(COMPILELATEX) --load-none --jobname 03 --discover 03.tex
03.blg 03.log 03.pdf 03.md5:\
03.tex\
03-annex.tex\
test.bib
	$(COMPILELATEX) --load-none --jobname 03 --discover 03.tex
distclean::
	rm -f 03.aux 03.bbl 03.blg 03.log 03.md5 03.pdf
clean::
	rm -f 03.aux 03.bbl 03.blg 03.log 03.md5
depends::
	$(COMPILELATEX) --load-none --ignore --makefile Makefile --jobname 03 03.tex
depends-as-is::
	$(COMPILELATEX) --ignore --makefile Makefile --jobname 03 03.tex
# stop automatic section for 03
# start automatic section for 04
all:: 04.pdf
do::
	$(COMPILELATEX) --load-none --jobname 04 --discover 04.tex
04.blg 04.md5 04.log 04.pdf xampl.blg:\
04.tex\
xampl.bib\
test.bib
	$(COMPILELATEX) --load-none --jobname 04 --discover 04.tex
distclean::
	rm -f 04.aux 04.bbl 04.blg 04.log 04.md5 04.pdf xampl.aux xampl.bbl xampl.blg
clean::
	rm -f 04.aux 04.bbl 04.blg 04.log 04.md5 xampl.aux xampl.bbl xampl.blg
depends::
	$(COMPILELATEX) --load-none --ignore --makefile Makefile --jobname 04 04.tex
depends-as-is::
	$(COMPILELATEX) --ignore --makefile Makefile --jobname 04 04.tex
# stop automatic section for 04
# start automatic section for 05
all:: 05.pdf
do::
	$(COMPILELATEX) --load-none --jobname 05 --discover 05.tex
05.log 05.pdf 05.md5:\
05.tex
	$(COMPILELATEX) --load-none --jobname 05 --discover 05.tex
distclean::
	rm -f 05-annex.tex 05.aux 05.log 05.md5 05.pdf
clean::
	rm -f 05-annex.tex 05.aux 05.log 05.md5
depends::
	$(COMPILELATEX) --load-none --ignore --makefile Makefile --jobname 05 05.tex
depends-as-is::
	$(COMPILELATEX) --ignore --makefile Makefile --jobname 05 05.tex
# stop automatic section for 05
# start automatic section for 06a
all:: 06a.pdf
do::
	$(COMPILELATEX) --load-none --jobname 06a --discover 06.tex
06a.md5 06a.log 06a.pdf:\
06.tex
	$(COMPILELATEX) --load-none --jobname 06a --discover 06.tex
distclean::
	rm -f 06a.aux 06a.log 06a.md5 06a.pdf
clean::
	rm -f 06a.aux 06a.log 06a.md5
depends::
	$(COMPILELATEX) --load-none --ignore --makefile Makefile --jobname 06a 06.tex
depends-as-is::
	$(COMPILELATEX) --ignore --makefile Makefile --jobname 06a 06.tex
# stop automatic section for 06a
# start automatic section for 06b
all:: 06b.pdf
do::
	$(COMPILELATEX) --load-none --jobname 06b --discover 06.tex
06b.md5 06b.pdf 06b.log:\
06.tex
	$(COMPILELATEX) --load-none --jobname 06b --discover 06.tex
distclean::
	rm -f 06b.aux 06b.log 06b.md5 06b.pdf
clean::
	rm -f 06b.aux 06b.log 06b.md5
depends::
	$(COMPILELATEX) --load-none --ignore --makefile Makefile --jobname 06b 06.tex
depends-as-is::
	$(COMPILELATEX) --ignore --makefile Makefile --jobname 06b 06.tex
# stop automatic section for 06b
