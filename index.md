# `lavaan.printer`: Customize The Printout of `lavaan` Parameter Estimates

(Version 0.1.2, updated on 2026-07-19, [release
history](https://sfcheung.github.io/lavaan.printer/news/index.html))

This package includes helper functions for developers to customize the
printout of the parameter estimates in the output of `lavaan`. These
functions emphasize flexibility, not user-friendliness. They are not
intended for end-users.

For more information on this package, please visit its GitHub page:

<https://sfcheung.github.io/lavaan.printer/>

# Background

I wrote these two functions because I want to customize how the
parameter estimate tables of a `lavaan` object are printed in my
packages. The style should be very similar to that used by the
[`summary()`](https://rdrr.io/r/base/summary.html) method of `lavaan`,
such that users would find the tables easy to read. However, it is not
easy to customize the output of `lavaan` because it prints the formatted
content directly to the screen. Therefore, I wrote
[`parameterEstimates_table_list()`](https://sfcheung.github.io/lavaan.printer/reference/parameterEstimates_table_list.md)
to mimic what `lavaan` does, but create a list of tables (data frames)
instead.

The [quick-start
guide](https://sfcheung.github.io/lavaan.printer/articles/lavaan.printer.html)
illustrates how to use these functions.

# Installation

The stable CRAN version can be installed by
[`install.packages()`](https://rdrr.io/r/utils/install.packages.html):

``` r

install.packages("lavaan.printer")
```

The latest developmental version of this package can be installed by
`remotes::install_github`:

``` r

remotes::install_github("sfcheung/lavaan.printer")
```

# Issues

If you have any suggestions and found any bugs, please feel free to open
a GitHub issue. Thanks.

<https://github.com/sfcheung/lavaan.printer/issues>
