ifeq "$(OS)" "Windows_NT"
    TARGET_EXTENSION=.exe
    CSC=csc
else
    UNAME_S := $(shell uname -s)
    ifeq ($(UNAME_S), Linux)
        TARGET_EXTENSION=
        CSC=mcs
    endif
    ifeq ($(UNAME_S), Darwin)
        TARGET_EXTENSION=
        CSC=mcs
    endif
endif

build: BmBnd$(TARGET_EXTENSION)

BmBnd$(TARGET_EXTENSION): Control.cs InputParameters.cs Constants.cs Calculations.cs OutputFormat.cs
	$(CSC) -out:BmBnd$(TARGET_EXTENSION) Control.cs InputParameters.cs Constants.cs Calculations.cs OutputFormat.cs

run: build
	./BmBnd$(TARGET_EXTENSION) $(RUNARGS)

doc: doxConfig Control.cs InputParameters.cs Constants.cs Calculations.cs OutputFormat.cs
	doxygen doxConfig

.PHONY: build run doc
