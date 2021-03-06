Morpheus
========

Implementation of a variability-aware refactoring engine for C.
Supports three standard refactorings (Rename Identifier, Extract Function, and Inlie Function).
In contrast to existing refactoring engines such as Eclipse/CDT, Morpheus can handle varibility in source code induced by #ifdefs.
That is all three refactorings are applied in all variants (with respect to #ifdef directives) in the source code.
The engine relies on the variability-aware parsing and analysis infrastructure [TypeChef](https://ckaestne.github.io/TypeChef/).


Installation and Usage
----------------------

Morpheus requires a modified version of TypeChef. To install it simply run:

    git clone https://github.com/aJanker/TypeChef.git
    cd TypeChef
    git checkout master
    ./publish.sh

To install the last version of Morpheus simply run:

    git clone https://github.com/joliebig/Morpheus.git
    cd Morpheus
    git checkout master
    ./mkrun.sh

The commands create a run-script (morpheus.sh) for the project. Morpheus comes with a simplified GUI (parameter: --showGui) and a command line interface for evaluation of the refactorings. The simplified GUI is intended for testing and presentation only. To apply Morpheus in a real setting, a proper project setup has to be passed to the engine. Since the project setup of existing software systems is difficult to integrate with Morpheus, we currently support only three systems. The systems, including the proper setup to run with Morpheus, are available on github, too: [Busybox](https://github.com/aJanker/Morpheus-BusyBoxEvaluation), [OpenSSL](https://github.com/aJanker/Morpheus-OpenSSLEvaluation), and [SQLite](https://github.com/aJanker/Morpheus-SQLiteEvaluation).

License
-------

Morpheus is published as open source under LGPL 3.0. See [LICENSE](LICENSE.md).
