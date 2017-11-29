ifndef PROJECT
  PROJECT = $(notdir $(abspath .))
endif
PDFLATEX = pdflatex
LHS2TEX = lhs2TeX

.PHONY : force

default : $(PROJECT).pdf

%-final.tex : %.lhs force
	$(LHS2TEX) -P../../includes: -s final -o $@ $<

%-handout.tex : %.lhs force
	$(LHS2TEX) -P../../includes: -s handout -o $@ $<

%.tex : %.lhs force
	$(LHS2TEX) -P../../includes: -o $@ $<

%.pdf : %.tex force
	sh -c ' \
	  TEXINPUTS="../../includes:" $(PDFLATEX) $<; \
	  while grep -s "Warning.*Rerun" $(<:.tex=.log); \
	    do TEXINPUTS="../../includes:" $(PDFLATEX) $<; done;'
