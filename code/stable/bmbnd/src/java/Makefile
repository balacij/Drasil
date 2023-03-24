build: BmBnd/Control.class

BmBnd/Control.class: BmBnd/Control.java BmBnd/InputParameters.java BmBnd/Constants.java BmBnd/Calculations.java BmBnd/OutputFormat.java
	javac BmBnd/Control.java

run: build
	java BmBnd.Control $(RUNARGS)

doc: doxConfig BmBnd/Control.java BmBnd/InputParameters.java BmBnd/Constants.java BmBnd/Calculations.java BmBnd/OutputFormat.java
	doxygen doxConfig

.PHONY: build run doc
