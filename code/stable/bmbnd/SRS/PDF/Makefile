srs: BmBnd_SRS.pdf

BmBnd_SRS.pdf: BmBnd_SRS.tex
	lualatex $(TEXFLAGS) BmBnd_SRS
	-bibtex $(BIBTEXFLAGS) BmBnd_SRS
	lualatex $(TEXFLAGS) BmBnd_SRS
	lualatex $(TEXFLAGS) BmBnd_SRS

.PHONY: srs
