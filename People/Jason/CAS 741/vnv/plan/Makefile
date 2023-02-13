.PHONY: help system_requirements build debug clean open

BUILD=plan

help:
	@echo "Build:"
	@echo "  - build: Build a fresh copy in the build/ folder."
	@echo "  - debug: Same as 'build', but pauses build on errors for easier debugging."
	@echo "  - clean: Clean working TeX build artifacts."
	@echo ""
	@echo "Supplementary Information:"
	@echo "  - help               : View this help guide."
	@echo "  - system_requirements: List system requirements."

system_requirements:
	@echo "System Requirements: LaTeX (latexmk + LuaLaTeX), Pygments, InkScape"

build: # standard build -- '-output-directory=build' is a special name and is referenced from the '\usepackage{minted}' region in the main tex.
	latexmk -use-make -output-directory=build -pdflatex=lualatex -pdf -interaction=nonstopmode "$(BUILD).tex" --shell-escape

debug: # for finding hard issues, this is an interactive version of 'build'
	latexmk -use-make -output-directory=build -pdflatex=lualatex -pdf "$(BUILD).tex" --shell-escape

clean:
	rm -rf "build/"

open:
	xdg-open "build/$(BUILD).pdf"
