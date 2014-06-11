Morpheus
========

Implementation of a variability-aware refactoring engine for C.
Supports three standard refactorings (Rename Identifier, Extract Function, and Inlie Function).
In contrast to existing refactoring engines such as Eclipse/CDT, Morpheus can handle varibility in source code induced by #ifdefs.
That is all three refactorings are applied in all variants (with respect to #ifdef directives) in the source code.
