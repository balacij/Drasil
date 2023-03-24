ifeq "$(OS)" "Windows_NT"
    TARGET_EXTENSION=.exe
else
    UNAME_S := $(shell uname -s)
    ifeq ($(UNAME_S), Linux)
        TARGET_EXTENSION=
    endif
    ifeq ($(UNAME_S), Darwin)
        TARGET_EXTENSION=
    endif
endif

build: BmBnd$(TARGET_EXTENSION)

BmBnd$(TARGET_EXTENSION): InputParameters.hpp Constants.hpp Calculations.hpp OutputFormat.hpp Control.cpp InputParameters.cpp Constants.cpp Calculations.cpp OutputFormat.cpp
	"$(CXX)" Control.cpp InputParameters.cpp Constants.cpp Calculations.cpp OutputFormat.cpp --std=c++11 -o BmBnd$(TARGET_EXTENSION)

run: build
	./BmBnd$(TARGET_EXTENSION) $(RUNARGS)

doc: doxConfig InputParameters.hpp Constants.hpp Calculations.hpp OutputFormat.hpp Control.cpp
	doxygen doxConfig

.PHONY: build run doc
