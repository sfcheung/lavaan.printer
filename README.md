<!-- badges: start -->
[![Lifecycle: Experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Code size](https://img.shields.io/github/languages/code-size/sfcheung/lavaan.printer.svg)](https://github.com/sfcheung/lavaan.printer)
[![Last Commit at Main](https://img.shields.io/github/last-commit/sfcheung/lavaan.printer.svg)](https://github.com/sfcheung/lavaan.printer/commits/main)
[![R-CMD-check](https://github.com/sfcheung/lavaan.printer/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sfcheung/lavaan.printer/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

(Version 0.1.0, updated on 2024-09-12, [release history](https://sfcheung.github.io/lavaan.printer/news/index.html))

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

# Installation

The latest developmental version of this
package can be installed by `remotes::install_github`:

```r
remotes::install_github("sfcheung/lavaan.printer")
```

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

# Issues

If you have any suggestions and found
any bugs, please feel feel to open a
GitHub issue. Thanks.