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

BmBnd$(TARGET_EXTENSION): main.swift InputParameters.swift Constants.swift Calculations.swift OutputFormat.swift
	swiftc main.swift InputParameters.swift Constants.swift Calculations.swift OutputFormat.swift -o BmBnd$(TARGET_EXTENSION)

run: build
	./BmBnd$(TARGET_EXTENSION) $(RUNARGS)

doc: main.swift InputParameters.swift Constants.swift Calculations.swift OutputFormat.swift

.PHONY: build run doc
