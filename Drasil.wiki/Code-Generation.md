<small><i>Warning for Drasil Authors: This wiki should not be edited through the web GUI. Please use the wiki/ folder in the repository and follow the normal commit-PR workflow.</i></small>

This page will document interesting design notes about GOOL and Drasil's code generator. 

- GOOL's `int` type maps to 32-bit signed integers in most target languages, but in Python 3 the default integer type has unlimited precision, and this is used instead.