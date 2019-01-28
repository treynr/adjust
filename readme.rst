
adjust
======

.. image:: https://img.shields.io/circleci/project/github/treynr/adjust/master.svg?style=flat-square
    :target: https://circleci.com/gh/treynr/adjust

In statistics, the `multiple testing problem`__ occurs when testing multiple hypotheses
simultaneously.
As the number of tests increases, so does the probability of encountering a type I error
(false positive).
With as little as 20 tests, the chance of finding a significant result is ~64% although
no tests may actually be significant [Goldman2008]_.
:code:`adjust` can be used to correct for multiple testing by controlling the family-wise
error rate (FWER__) or the false discovery rate (FDR__).
It is designed to be simple to use and relatively fast, e.g., controlling the FDR for a 
set of GWAS__ results--13,549,588 tests (1.4GB)--takes about a minute and a half 
(on a Xeon E5-2640 @ 2.50GHz, running on a single thread).

.. __: https://en.wikipedia.org/wiki/Multiple_comparisons_problem
.. __: https://en.wikipedia.org/wiki/Family-wise_error_rate
.. __: https://en.wikipedia.org/wiki/False_discovery_rate
.. __: https://en.wikipedia.org/wiki/Genome-wide_association_study


Usage
-----

.. code:: bash

    $ adjust [options] <input> <output>

:code:`adjust` operates over delimiter separated value (DSV) files.
If the file has a header, :code:`adjust` will attempt to infer which column contains the
p-value.
For example, the following command can be used to control the FDR at alpha < 0.05 using
the `Benjamini-Hochberg step-up procedure`__.
All rows that don't meet this criteria are removed:

.. __: https://en.wikipedia.org/wiki/False_discovery_rate#Benjamini%E2%80%93Hochberg_procedure

.. code:: bash

    $ adjust input-stats.tsv output-stats.tsv

Or, if you'd rather use the `Bonferroni correction`__ to control the FWER:

.. __: https://en.wikipedia.org/wiki/Bonferroni_correction

.. code:: bash

    $ adjust -b input-stats.tsv output-stats.tsv

It can also read from stdin and write to stdout if the command is just part of a larger
pipeline or processing step:

.. code:: bash

    $ cat input-stats.tsv | adjust -i -o > output-stats.tsv


Options
-------

- :code:`--adjust`: Convert and replace p-values with adjusted p-values

- :code:`-a, --alpha=NUM`: Set the alpha (default = 0.05)

- :code:`--fdr`: Control the FDR using Benjamini-Hochberg step-up procedure

- :code:`--fwer`: Control the FWER using the Bonferroni correction

- :code:`-d, --delim=CHAR`: Specify a delimiter to use when parsing the input and writing
  output.

- :code:`-c, --column=INT`: Zero-indexed column containing the p-value (*currently disabled*)

- :code:`-n, --no-header`: Specify that the input does not contain a header file
  (*currently disabled*)

- :code:`-r, --remove`: Remove rows above the given alpha threshold. This is only
  relevant when producing adjusted p-values using the :code:`--adjust` option.

- :code:`-i, --stdin`: Read from stdin instead of a file

- :code:`-o, --stdout`: Write to stdout instead of a file


Installation
------------

Compilation and installation is done with Stack. Setup GHC:

.. code:: bash

    $ stack setup

Build the application:

.. code:: bash

    $ stack build 

If you wish to install it to your :code:`$PATH`:

.. code:: bash

    $ stack build --copy-bins


Requirements
''''''''''''

- GHC >= 8.2.2
- Stack__

.. __: https://docs.haskellstack.org/en/stable/install_and_upgrade/


Refs
----

.. [Goldman2008] https://www.stat.berkeley.edu/~mgoldman/Section0402.pdf

