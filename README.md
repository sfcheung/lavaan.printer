<!-- badges: start -->
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![CRAN status](https://www.r-pkg.org/badges/version/lavaan.printer?color=blue)](https://CRAN.R-project.org/package=lavaan.printer)
[![CRAN: Release Date](https://www.r-pkg.org/badges/last-release/lavaan.printer?color=blue)](https://cran.r-project.org/package=lavaan.printer)
[![Code size](https://img.shields.io/github/languages/code-size/sfcheung/lavaan.printer.svg)](https://github.com/sfcheung/lavaan.printer)
[![Last Commit at Main](https://img.shields.io/github/last-commit/sfcheung/lavaan.printer.svg)](https://github.com/sfcheung/lavaan.printer/commits/main)
[![R-CMD-check](https://github.com/sfcheung/lavaan.printer/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sfcheung/lavaan.printer/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

(Version 0.1.0.1, updated on 2024-09-17, [release history](https://sfcheung.github.io/lavaan.printer/news/index.html))

# `lavaan.printer`: Customize The Printout of `lavaan` Parameter Estimates

This package includes helper functions
for developers to customize the printout
of the parameter estimates in the output
of `lavaan`. These functions emphasize
flexibility, not user-friendliness. They
are not intended for end-users.

For more information on this package,
please visit its GitHub page:

https://sfcheung.github.io/lavaan.printer/

# Background

I wrote these two functions because I
want to customize how the parameter estimate
tables of a `lavaan` object are printed
in my packages. The style should be very
similar to that
used by the `summary()` method of `lavaan`,
such that users would find the tables
easy to read. However, it is not easy to
customize the output of `lavaan` because
it prints the formatted content directly
to the screen. Therefore, I wrote
`parameterEstimates_table_list()`
to mimic what `lavaan` does, but create
a list of tables (data frames) instead.

The [quick-start guide](https://sfcheung.github.io/lavaan.printer/articles/lavaan.printer.html)
illustrates how to use these functions.

# Installation

The stable CRAN version can be installed
by `install.packages()`:

```r
install.packages("lavaan.printer")
```

The latest developmental version of this
package can be installed by `remotes::install_github`:

```r
remotes::install_github("sfcheung/lavaan.printer")
```

# Issues

If you have any suggestions and found
any bugs, please feel feel to open a
GitHub issue. Thanks.

https://github.com/sfcheung/lavaan.printer/issues