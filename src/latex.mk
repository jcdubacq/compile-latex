LATEX ?= compile-latex
DEPENDS ?= depends-latex

all:

define STEM_template
GENERATED += $$(GENERATED$(1))
TEXRESULTS += $(1).pdf

$(1).md5 $(1).pdf: $$(SOURCE$(1))
	$(LATEX) $$(LATEXARGS$(1)) $(1).tex
recompile::
	$(LATEX) $$(LATEXARGS$(1)) $(1).tex
endef

$(foreach dir,$(STEMS),$(eval $(call STEM_template,$(dir))))

RESULTS += $(TEXRESULTS)
GENERATED += $(GENERATEDother)

all: $(RESULTS)

clean:
	rm -f $(filter-out $(RESULTS),$(GENERATED))
distclean:
	rm -f $(GENERATED)

depends:
	$(DEPENDS) $(TEXRESULTS) -- $(GENERATEDother)

.PHONY: depends all clean distclean recompile

%.out:
%.a:
%.ln:
%.o:
%: %.o
%.c:
%: %.c
%.ln: %.c
%.o: %.c
%.cc:
%: %.cc
%.o: %.cc
%.C:
%: %.C
%.o: %.C
%.cpp:
%: %.cpp
%.o: %.cpp
%.p:
%: %.p
%.o: %.p
%.f:
%: %.f
%.o: %.f
%.F:
%: %.F
%.o: %.F
%.f: %.F
%.r:
%: %.r
%.o: %.r
%.f: %.r
%.y:
%.ln: %.y
%.c: %.y
%.l:
%.ln: %.l
%.c: %.l
%.r: %.l
%.s:
%: %.s
%.o: %.s
%.S:
%: %.S
%.o: %.S
%.s: %.S
%.mod:
%: %.mod
%.o: %.mod
%.sym:
%.def:
%.sym: %.def
%.h:
%.info:
%.dvi:
%.tex:
%.dvi: %.tex
%.texinfo:
%.info: %.texinfo
%.dvi: %.texinfo
%.texi:
%.info: %.texi
%.dvi: %.texi
%.txinfo:
%.info: %.txinfo
%.dvi: %.txinfo
%.w:
%.c: %.w
%.tex: %.w
%.ch:
%.web:
%.p: %.web
%.tex: %.web
%.sh:
%: %.sh
%.elc:
%.el:
%.out: %
%.c: %.w %.ch
%.tex: %.w %.ch
%:: %,v
%:: RCS/%,v
%:: RCS/%
%:: s.%
%:: SCCS/s.%

.NOTPARALLEL:
