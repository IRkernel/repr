## Release summary

This package is new on CRAN. It contains a function to represent R objects in binary/string format. It is
a dependency for the IRkernel, which uses this package to serialize objects which should be displayed in the frontend.

## Test environments

* local Win7 64bit install, R 3.2.4
* ubuntu 12.04 (on travis-ci), R 3.2.2

## R CMD check results

There was 1 Warning:

* checking dependencies in R code ... WARNING: Unexported object imported by a ':::' call: 'utils:::.getHelpFile'
  See the note in ?`:::` about the use of this operator.
  Including base/recommended package(s):
  'utils'

  repr uses this to make help content available in the frontend, which needs a similar implementation like
  `utils:::print.help_files_with_topic`. The alternative is having `repr_*.help_files_with_topic` functions
  defined in `utils` or ask to export this function, which seems unreasonable for just this one case.

There was 2 NOTEs:

* checking CRAN incoming feasibility ... NOTE [...] New submission

  This is the first submission to CRAN, but it is already used via conda packages and on withins MS cloud offerings.

* checking DESCRIPTION meta-information ... NOTE: Packages listed in more than one of Depends, Imports, Suggests, Enhances:
  'data.table' 'dplyr' 'htmlwidgets'

  These are listed twice because some tests uses them and so `Enhances` is not enough.

## Downstream dependencies

As this is the first release to CRAN, there are no reverse dependencies.