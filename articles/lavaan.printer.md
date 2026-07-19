# lavaan.printer

## Introduction

This article illustrates how to use the two main functions of
`lavaan.printer`:

- [`parameterEstimates_table_list()`](https://sfcheung.github.io/lavaan.printer/reference/parameterEstimates_table_list.md)

- [`print_parameterEstimates_table_list()`](https://sfcheung.github.io/lavaan.printer/reference/parameterEstimates_table_list.md)

These are the packages used in this article:

``` r

library(lavaan)
#> This is lavaan 0.7-2
#> lavaan is FREE software! Please report any bugs.
library(lavaan.printer)
```

## For What Scenarios?

These two functions are not for end-users. They are for package
developers, and are intended to be used internally by `print`-methods or
similar functions for the objects in other packages which generate
customized parameter tables.

- [`parameterEstimates_table_list()`](https://sfcheung.github.io/lavaan.printer/reference/parameterEstimates_table_list.md):
  creates a `parameterEstimates_table_list` object which is a list of
  data frames, organized based on the sections for parameter estimates
  in the [`summary()`](https://rdrr.io/r/base/summary.html) output of
  `lavaan`, such as factor loadings (`Latent Variables`) and covariances
  (`Covariances`). The data frames can be customized in a lot of ways,
  such as adding columns created by user-supplied functions, or adding
  header sections or footer sections.

- [`print_parameterEstimates_table_list()`](https://sfcheung.github.io/lavaan.printer/reference/parameterEstimates_table_list.md):
  It prints a `parameterEstimates_table_list` object, with some degree
  of customization. Most of the customization should be done when
  calling
  [`parameterEstimates_table_list()`](https://sfcheung.github.io/lavaan.printer/reference/parameterEstimates_table_list.md).
  The cells are formatted as strings across sections when being printed
  to ensure that the column widths are consistent.

## Scenarios

### Create a List of Data frames

In the simplest case,
[`parameterEstimates_table_list()`](https://sfcheung.github.io/lavaan.printer/reference/parameterEstimates_table_list.md)
can be used to generate the usual parameter estimates results of
`lavaan`, but as a list of data frames, each corresponding to a section
in the output (e.g., regression coefficients, factor loadings).
Arguments for to
[`lavaan::parameterEstimates()`](https://rdrr.io/pkg/lavaan/man/parameterEstimates.html)
can be used when calling
[`parameterEstimates_table_list()`](https://sfcheung.github.io/lavaan.printer/reference/parameterEstimates_table_list.md):

``` r

# Adapted from the example of cfa()
model_cfa <- "visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6"
fit <- cfa(model_cfa,
           data = HolzingerSwineford1939,
           group = "school")
est <- parameterEstimates_table_list(fit,
                                     rsquare = TRUE)
```

Because it is intended to be used inside a print function, it does not
have a print method itself. Call
[`print_parameterEstimates_table_list()`](https://sfcheung.github.io/lavaan.printer/reference/parameterEstimates_table_list.md)
instead:

``` r

print_parameterEstimates_table_list(est)
#> 
#> Parameter Estimates Settings:
#>                                              
#>  Standard errors:                  Standard  
#>  Information:                      Expected  
#>  Information saturated (h1) model: Structured
#> 
#> Group 1 [Pasteur]:
#> Latent Variables:
#>             Estimate  S.E.      Z P(>|z|)  CI.Lo CI.Up
#>  visual =~                                            
#>   x1           1.000                                  
#>   x2           0.373 0.121  3.081   0.002  0.136 0.610
#>   x3           0.541 0.141  3.843   0.000  0.265 0.817
#>  textual =~                                           
#>   x4           1.000                                  
#>   x5           1.187 0.102 11.640   0.000  0.987 1.387
#>   x6           0.870 0.076 11.372   0.000  0.720 1.020
#> 
#> Group 1 [Pasteur]:
#> Covariances:
#>             Estimate  S.E.      Z P(>|z|)  CI.Lo CI.Up
#>  visual ~~                                            
#>   textual      0.485 0.106  4.568   0.000  0.277 0.693
#> 
#> Group 1 [Pasteur]:
#> Intercepts:
#>             Estimate  S.E.      Z P(>|z|)  CI.Lo CI.Up
#>  .x1           4.941 0.095 52.249   0.000  4.756 5.127
#>  .x2           5.984 0.098 60.949   0.000  5.792 6.176
#>  .x3           2.487 0.093 26.778   0.000  2.305 2.669
#>  .x4           2.823 0.092 30.689   0.000  2.642 3.003
#>  .x5           3.995 0.105 38.183   0.000  3.790 4.200
#>  .x6           1.922 0.079 24.321   0.000  1.767 2.077
#>   visual       0.000                                  
#>   textual      0.000                                  
#> 
#> Group 1 [Pasteur]:
#> Variances:
#>             Estimate  S.E.      Z P(>|z|)  CI.Lo CI.Up
#>  .x1           0.244 0.258  0.947   0.344 -0.262 0.750
#>  .x2           1.344 0.158  8.498   0.000  1.034 1.653
#>  .x3           1.009 0.138  7.317   0.000  0.739 1.279
#>  .x4           0.424 0.069  6.105   0.000  0.288 0.560
#>  .x5           0.445 0.086  5.161   0.000  0.276 0.614
#>  .x6           0.297 0.051  5.864   0.000  0.197 0.396
#>   visual       1.151 0.300  3.835   0.000  0.563 1.739
#>   textual      0.896 0.150  5.967   0.000  0.602 1.190
#> 
#> Group 1 [Pasteur]:
#> R-Squares:
#>             Estimate  S.E.      Z P(>|z|)  CI.Lo CI.Up
#>   x1           0.825                                  
#>   x2           0.107                                  
#>   x3           0.250                                  
#>   x4           0.679                                  
#>   x5           0.740                                  
#>   x6           0.696                                  
#> 
#> Group 2 [Grant-White]:
#> Latent Variables:
#>             Estimate  S.E.      Z P(>|z|)  CI.Lo CI.Up
#>  visual =~                                            
#>   x1           1.000                                  
#>   x2           0.813 0.175  4.657   0.000  0.471 1.155
#>   x3           1.045 0.204  5.110   0.000  0.644 1.446
#>  textual =~                                           
#>   x4           1.000                                  
#>   x5           0.981 0.086 11.351   0.000  0.812 1.151
#>   x6           0.963 0.084 11.402   0.000  0.797 1.128
#> 
#> Group 2 [Grant-White]:
#> Covariances:
#>             Estimate  S.E.      Z P(>|z|)  CI.Lo CI.Up
#>  visual ~~                                            
#>   textual      0.377 0.096  3.941   0.000  0.189 0.564
#> 
#> Group 2 [Grant-White]:
#> Intercepts:
#>             Estimate  S.E.      Z P(>|z|)  CI.Lo CI.Up
#>  .x1           4.930 0.095 51.696   0.000  4.743 5.117
#>  .x2           6.200 0.092 67.416   0.000  6.020 6.380
#>  .x3           1.996 0.086 23.195   0.000  1.827 2.164
#>  .x4           3.317 0.093 35.625   0.000  3.135 3.500
#>  .x5           4.712 0.096 48.986   0.000  4.524 4.901
#>  .x6           2.469 0.094 26.277   0.000  2.285 2.653
#>   visual       0.000                                  
#>   textual      0.000                                  
#> 
#> Group 2 [Grant-White]:
#> Variances:
#>             Estimate  S.E.      Z P(>|z|)  CI.Lo CI.Up
#>  .x1           0.792 0.132  6.017   0.000  0.534 1.050
#>  .x2           0.878 0.124  7.104   0.000  0.636 1.120
#>  .x3           0.498 0.114  4.386   0.000  0.276 0.721
#>  .x4           0.310 0.065  4.776   0.000  0.183 0.437
#>  .x5           0.429 0.073  5.900   0.000  0.287 0.572
#>  .x6           0.402 0.069  5.818   0.000  0.267 0.537
#>   visual       0.527 0.155  3.399   0.001  0.223 0.830
#>   textual      0.947 0.153  6.196   0.000  0.648 1.247
#> 
#> Group 2 [Grant-White]:
#> R-Squares:
#>             Estimate  S.E.      Z P(>|z|)  CI.Lo CI.Up
#>   x1           0.399                                  
#>   x2           0.284                                  
#>   x3           0.536                                  
#>   x4           0.753                                  
#>   x5           0.680                                  
#>   x6           0.686
```

The output looks similar to the `lavaan` output, except for some minor
changes in column names (e.g., `CI.Lo` and `CI.Up` instead of `ci.lower`
and `ci.upper`) to shorten the width of the print, to make room for
additional columns developer may want to add. This is intentional:
mimicking the `lavaan` style to minimize the need for the users to learn
anything new in reading the output.

These options are available in
[`print_parameterEstimates_table_list()`](https://sfcheung.github.io/lavaan.printer/reference/parameterEstimates_table_list.md):

- `nd`: Control the number of digits after the decimal. Default is 3.

- `by_group`: For multiple-group models, whether the tables are grouped
  by groups first, as in `lavaan`. Default is `TRUE`. Setting this to
  `FALSE` facilitates comparing groups on estimates, especially when a
  model has a lot of parameters.

- `drop_cols`: A character vector of columns to be dropped. Any columns
  can be dropped. Can be used to drop columns that cannot be removed
  when calling [`summary()`](https://rdrr.io/r/base/summary.html) or
  [`lavaan::parameterEstimates()`](https://rdrr.io/pkg/lavaan/man/parameterEstimates.html),
  or columns to be dropped only when being printed.

- `na_str`: The string to be used for blank cells, such as the standard
  errors of fixed parameters. Default is `" "`.

This is an example of these arguments:

``` r

print_parameterEstimates_table_list(est,
                                    nd = 2,
                                    by_group = FALSE,
                                    drop_cols = "Z",
                                    na_str = "--")
#> 
#> Parameter Estimates Settings:
#>                                              
#>  Standard errors:                  Standard  
#>  Information:                      Expected  
#>  Information saturated (h1) model: Structured
#> 
#> Latent Variables:
#> Group 1 [Pasteur]:
#>             Estimate S.E. P(>|z|) CI.Lo CI.Up
#>  visual =~                                   
#>   x1            1.00   --      --    --    --
#>   x2            0.37 0.12    0.00  0.14  0.61
#>   x3            0.54 0.14    0.00  0.27  0.82
#>  textual =~                                  
#>   x4            1.00   --      --    --    --
#>   x5            1.19 0.10    0.00  0.99  1.39
#>   x6            0.87 0.08    0.00  0.72  1.02
#> 
#> Latent Variables:
#> Group 2 [Grant-White]:
#>             Estimate S.E. P(>|z|) CI.Lo CI.Up
#>  visual =~                                   
#>   x1            1.00   --      --    --    --
#>   x2            0.81 0.17    0.00  0.47  1.16
#>   x3            1.04 0.20    0.00  0.64  1.45
#>  textual =~                                  
#>   x4            1.00   --      --    --    --
#>   x5            0.98 0.09    0.00  0.81  1.15
#>   x6            0.96 0.08    0.00  0.80  1.13
#> 
#> Covariances:
#> Group 1 [Pasteur]:
#>             Estimate S.E. P(>|z|) CI.Lo CI.Up
#>  visual ~~                                   
#>   textual       0.49 0.11    0.00  0.28  0.69
#> 
#> Covariances:
#> Group 2 [Grant-White]:
#>             Estimate S.E. P(>|z|) CI.Lo CI.Up
#>  visual ~~                                   
#>   textual       0.38 0.10    0.00  0.19  0.56
#> 
#> Intercepts:
#> Group 1 [Pasteur]:
#>             Estimate S.E. P(>|z|) CI.Lo CI.Up
#>  .x1            4.94 0.09    0.00  4.76  5.13
#>  .x2            5.98 0.10    0.00  5.79  6.18
#>  .x3            2.49 0.09    0.00  2.31  2.67
#>  .x4            2.82 0.09    0.00  2.64  3.00
#>  .x5            4.00 0.10    0.00  3.79  4.20
#>  .x6            1.92 0.08    0.00  1.77  2.08
#>   visual        0.00   --      --    --    --
#>   textual       0.00   --      --    --    --
#> 
#> Intercepts:
#> Group 2 [Grant-White]:
#>             Estimate S.E. P(>|z|) CI.Lo CI.Up
#>  .x1            4.93 0.10    0.00  4.74  5.12
#>  .x2            6.20 0.09    0.00  6.02  6.38
#>  .x3            2.00 0.09    0.00  1.83  2.16
#>  .x4            3.32 0.09    0.00  3.13  3.50
#>  .x5            4.71 0.10    0.00  4.52  4.90
#>  .x6            2.47 0.09    0.00  2.28  2.65
#>   visual        0.00   --      --    --    --
#>   textual       0.00   --      --    --    --
#> 
#> Variances:
#> Group 1 [Pasteur]:
#>             Estimate S.E. P(>|z|) CI.Lo CI.Up
#>  .x1            0.24 0.26    0.34 -0.26  0.75
#>  .x2            1.34 0.16    0.00  1.03  1.65
#>  .x3            1.01 0.14    0.00  0.74  1.28
#>  .x4            0.42 0.07    0.00  0.29  0.56
#>  .x5            0.44 0.09    0.00  0.28  0.61
#>  .x6            0.30 0.05    0.00  0.20  0.40
#>   visual        1.15 0.30    0.00  0.56  1.74
#>   textual       0.90 0.15    0.00  0.60  1.19
#> 
#> Variances:
#> Group 2 [Grant-White]:
#>             Estimate S.E. P(>|z|) CI.Lo CI.Up
#>  .x1            0.79 0.13    0.00  0.53  1.05
#>  .x2            0.88 0.12    0.00  0.64  1.12
#>  .x3            0.50 0.11    0.00  0.28  0.72
#>  .x4            0.31 0.06    0.00  0.18  0.44
#>  .x5            0.43 0.07    0.00  0.29  0.57
#>  .x6            0.40 0.07    0.00  0.27  0.54
#>   visual        0.53 0.15    0.00  0.22  0.83
#>   textual       0.95 0.15    0.00  0.65  1.25
#> 
#> R-Squares:
#> Group 1 [Pasteur]:
#>             Estimate S.E. P(>|z|) CI.Lo CI.Up
#>  x1             0.82   --      --    --    --
#>  x2             0.11   --      --    --    --
#>  x3             0.25   --      --    --    --
#>  x4             0.68   --      --    --    --
#>  x5             0.74   --      --    --    --
#>  x6             0.70   --      --    --    --
#> 
#> R-Squares:
#> Group 2 [Grant-White]:
#>             Estimate S.E. P(>|z|) CI.Lo CI.Up
#>  x1             0.40   --      --    --    --
#>  x2             0.28   --      --    --    --
#>  x3             0.54   --      --    --    --
#>  x4             0.75   --      --    --    --
#>  x5             0.68   --      --    --    --
#>  x6             0.69   --      --    --    --
```

### Insert a Column Computed From Another Column

Suppose we would like to insert a column of symbols (e.g., `"*"` and
`"**"`) into the parameter estimates table based on the `pvalue` column,
to denote whether a parameter is significant,

This is the function:

``` r

add_sig <- function(object,
                    breaks = c(1, .05, .01, .001, -Inf),
                    labels = c("***", "** ", "*  ", "  ")) {
    tmp <- object[, "pvalue", drop = TRUE]
    if (!is.null(tmp)) {
        tmp[is.na(tmp)] <- 1
        tmp2 <- cut(tmp,
                    breaks = breaks,
                    labels = labels)
        i <- match("pvalue", colnames(object))
        out <- data.frame(object[, 1:i],
                          Sig = tmp2,
                          object[, seq(i + 1, ncol(object))])
      }
    out
  }
```

This can be done by the argument `est_funs`:

``` r

model_cfa <- "visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6"
fit <- cfa(model_cfa,
           data = HolzingerSwineford1939[1:100, ])
#> Warning: lavaan->lav_object_post_check():  
#>    some estimated ov variances are negative
est <- parameterEstimates_table_list(fit,
                                     est_funs = list(add_sig))
print_parameterEstimates_table_list(est)
#> 
#> Parameter Estimates Settings:
#>                                              
#>  Standard errors:                  Standard  
#>  Information:                      Expected  
#>  Information saturated (h1) model: Structured
#> 
#> Latent Variables:
#>             Estimate  S.E.      Z P(>|z|) Sig  CI.Lo CI.Up
#>  visual =~                                                
#>   x1           1.000                                      
#>   x2           0.163 0.118  1.379   0.168     -0.068 0.394
#>   x3           0.355 0.192  1.849   0.065     -0.021 0.732
#>  textual =~                                               
#>   x4           1.000                                      
#>   x5           1.202 0.129  9.322   0.000 ***  0.950 1.455
#>   x6           0.707 0.081  8.694   0.000 ***  0.548 0.867
#> 
#> Covariances:
#>             Estimate  S.E.      Z P(>|z|) Sig  CI.Lo CI.Up
#>  visual ~~                                                
#>   textual      0.430 0.123  3.485   0.000 ***  0.188 0.672
#> 
#> Variances:
#>             Estimate  S.E.      Z P(>|z|) Sig  CI.Lo CI.Up
#>  .x1          -0.257 0.793 -0.324   0.746     -1.810 1.297
#>  .x2           1.341 0.190  7.055   0.000 ***  0.969 1.714
#>  .x3           1.022 0.175  5.828   0.000 ***  0.678 1.366
#>  .x4           0.331 0.076  4.372   0.000 ***  0.183 0.479
#>  .x5           0.405 0.103  3.929   0.000 ***  0.203 0.607
#>  .x6           0.241 0.046  5.291   0.000 ***  0.152 0.331
#>   visual       1.622 0.814  1.992   0.046 *    0.026 3.219
#>   textual      0.812 0.166  4.899   0.000 ***  0.487 1.137
```

Note that `est_funs` must be a list, even if only one function is
supplied.

### Add a Header or Footer Section

Header or footer functions can be used to include a header or footer
section. The first argument will be one of the following object:

- If the input object of
  [`parameterEstimates_table_list()`](https://sfcheung.github.io/lavaan.printer/reference/parameterEstimates_table_list.md)
  is a `lavaan` object, then the function is called with the first
  argument being the *parameter* *estimates* table generated by
  [`lavaan::parameterEstimates()`](https://rdrr.io/pkg/lavaan/man/parameterEstimates.html))
  with `output = "text", header = TRUE`.

- If the input object of
  [`parameterEstimates_table_list()`](https://sfcheung.github.io/lavaan.printer/reference/parameterEstimates_table_list.md)
  is a data-frame-like object, such as a modified output of
  [`lavaan::parameterEstimates()`](https://rdrr.io/pkg/lavaan/man/parameterEstimates.html)
  generated by other functions, then the function is called with this
  object as the first argument. Header and footer sections can then be
  created using attributes stored in this object.

The following is a simple function to print the missing data handling
method, stored in the output of
[`lavaan::parameterEstimates()`](https://rdrr.io/pkg/lavaan/man/parameterEstimates.html)
when printed with `output = "text", header = "TRUE"`. The attribute
`section_title` is used to set the title for this section. By default,
[`print()`](https://rdrr.io/r/base/print.html) will be used to print a
section.

``` r

lavaan_missing <- function(x) {
    out0 <- attr(x, "missing")
    out1 <- data.frame(Option = "Missing",
                       Setting = out0)
    attr(out1, "section_title") <- "Additional Information:"
    out1
  }
```

This is a function to add some footnotes. The output is a character
vector, which should be printed by
[`cat()`](https://rdrr.io/r/base/cat.html). This can be done by setting
the attribute `print_fun` to `"cat"`. If the print function is
[`cat()`](https://rdrr.io/r/base/cat.html), the section is printed with
`sep = "\n"` by default.

``` r

footnotes <- function(x) {
    out0 <- c("- This is footnote 1.",
              paste("- This is footnote 2, a very very very very very",
                    "very very very very very very very very very very",
                    "very very long one. Wrapped by default"))
    attr(out0, "section_title") <- "Footnote:"
    attr(out0, "print_fun") <- "cat"
    out0
  }
```

These functions can then be used in `header_funs` and `footer_funs`:

``` r

model_cfa <- "visual  =~ x1 + x2 + x3"
fit <- cfa(model_cfa,
           data = HolzingerSwineford1939)
est <- parameterEstimates_table_list(fit,
                                     header_funs = list(lavaan_missing),
                                     footer_funs = list(footnotes))
print_parameterEstimates_table_list(est)
#> 
#> Additional Information:
#>  Option  Setting 
#>  Missing listwise
#> 
#> Parameter Estimates Settings:
#>                                              
#>  Standard errors:                  Standard  
#>  Information:                      Expected  
#>  Information saturated (h1) model: Structured
#> 
#> Latent Variables:
#>            Estimate  S.E.      Z P(>|z|) CI.Lo CI.Up
#>  visual =~                                          
#>   x1          1.000                                 
#>   x2          0.778 0.141  5.532   0.000 0.502 1.053
#>   x3          1.107 0.214  5.173   0.000 0.688 1.527
#> 
#> Variances:
#>            Estimate  S.E.      Z P(>|z|) CI.Lo CI.Up
#>  .x1          0.835 0.118  7.064   0.000 0.603 1.066
#>  .x2          1.065 0.105 10.177   0.000 0.860 1.270
#>  .x3          0.633 0.129  4.899   0.000 0.380 0.886
#>   visual      0.524 0.130  4.021   0.000 0.268 0.779
#> 
#> Footnote:
#> - This is footnote 1.
#> - This is footnote 2, a very very very very very very very very very
#> very very very very very very very very long one. Wrapped by default
```

Like `est_funs`, the value of `header_funs` (and `footer_funs`) must be
list.

## Further Information

There are other ways to customize the output when calling
[`parameterEstimates_table_list()`](https://sfcheung.github.io/lavaan.printer/reference/parameterEstimates_table_list.md).
For example:

- Drop one or more columns.

- Rename one or more columns.

- Set arguments when calling functions in `est_funs`, `header_funs`, and
  `footer_funs`.

Please refer to the help page of
[`parameterEstimates_table_list()`](https://sfcheung.github.io/lavaan.printer/reference/parameterEstimates_table_list.md)
for details.

## Issues

If you have any suggestions and found any bugs, please feel feel to open
a [GitHub issue](https://github.com/sfcheung/lavaan.printer/issues).
Thanks.
