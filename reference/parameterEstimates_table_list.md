# Convert an Estimates Table to a List of Data Frames

Create a list of data frames from the output of
[`lavaan::parameterEstimates()`](https://rdrr.io/pkg/lavaan/man/parameterEstimates.html),
formatted in nearly the same way the argument `output = "text"` does.

## Usage

``` r
parameterEstimates_table_list(
  object,
  ...,
  fit_object = NULL,
  se_also_to_na = character(0),
  se_not_to_na = character(0),
  drop_cols = "std.nox",
  rename_cols = character(0),
  est_funs = list(),
  header_funs = list(),
  footer_funs = list(),
  est_funs_args = NULL,
  header_funs_args = NULL,
  footer_funs_args = NULL
)

print_parameterEstimates_table_list(
  x,
  nd = 3,
  by_group = TRUE,
  drop_cols = character(0),
  na_str = " "
)
```

## Arguments

- object:

  It can a data frame similar in form to the output of
  [`lavaan::parameterEstimates()`](https://rdrr.io/pkg/lavaan/man/parameterEstimates.html),
  or a `lavaan` object (e.g., the output of
  [`lavaan::sem()`](https://rdrr.io/pkg/lavaan/man/sem.html)). If it is
  a `lavaan` object, then
  [`lavaan::parameterEstimates()`](https://rdrr.io/pkg/lavaan/man/parameterEstimates.html)
  will be called to generate the parameter estimates table.

- ...:

  If `object` is a `lavaan` object, then these are the optional
  arguments to be passed to
  [`lavaan::parameterEstimates()`](https://rdrr.io/pkg/lavaan/man/parameterEstimates.html)
  when it is called.

- fit_object:

  (Optional). The `lavaan` object for getting additional information, if
  they are not available in `object`, and added as attributes to
  `object`. It essentially does what
  [`lavaan::parameterEstimates()`](https://rdrr.io/pkg/lavaan/man/parameterEstimates.html)
  does when setting `output` to `"text"`.

- se_also_to_na:

  Columns for which cells will be set to `NA` if the standard error of a
  parameter is zero, which is assumed to mean that this parameter is
  fixed. By default, these columns are included and no need to specify
  them for this argument: `"z"`, `"pvalue"`, `"t"`, `"df"`,
  `"ci.lower"`, and `"ci.upper"`. To exclude one of these columns from
  `se_als_to_na`, add it to `se_not_to_na`.

- se_not_to_na:

  Columns for which cells will *not* be set to `NA` even if the standard
  error of a parameter is zero. Column names that appear here *override*
  `se_also_to_na`. Therefore, if `"z"`, `"pvalue"`, `"t"`, `"df"`,
  `"ci.lower"`, and `"ci.upper"` are included in this argument, they
  will also not be set to `NA`.

- drop_cols:

  The names of columns to be dropped from the printout. It can be the
  names after being renamed by `rename_cols`, or the original names
  before being renamed (i.e., the names in `object`), provided that the
  names in `object` are stored in the attribute `"original_name"` of
  each column, which is done by default by
  `parameterEstimates_table_list()`.

- rename_cols:

  If any columns are to be renamed, this is named character vector, with
  the names being the original names and the values being the new names.
  For example, `c("pvalue" = "P(|>z|)")` renames the column `"pvalue"`
  to `"P(|z|)"`. It is recommended to quote the names too because they
  may not be standard names.

- est_funs:

  If supplied, it should be a list of functions to be applied to each
  parameter estimates table, applied in the same order they appear in
  the list. It can be used create new columns or modify existing
  columns. Usually, this should be done *before* calling
  `parameterEstimates_table_list()` but provided as an option.

- header_funs:

  If supplied, it should be a list of functions to be applied to
  `object` to generate the header sections. See `Details` on the
  expected format of the output of these functions.

- footer_funs:

  If supplied, it should be a list of functions to be applied to
  `object` to generate the footer sections. See `Details` on the
  expected format of the output of these functions.

- est_funs_args:

  If supplied, it must be a "list of list(s)". The length of this list
  must be equal to the number of functions in `est_funs`. Each sub-list
  is the list of arguments to be used when calling a function in
  `est_funs`. It must be an empty
  [`list()`](https://rdrr.io/r/base/list.html) if no additional
  arguments are to be used when calling a function in `est_funs`.

- header_funs_args:

  If supplied, it must be a "list of list(s)". The length of this list
  must be equal to the number of functions in `header_funs`. Each
  sub-list is the list of arguments to be used when calling a function
  in `header_funs`. It must be an empty
  [`list()`](https://rdrr.io/r/base/list.html) if no additional
  arguments are to be used when calling a function in `header_funs`.

- footer_funs_args:

  If supplied, it must be a "list of list(s)". The length of this list
  must be equal to the number of functions in `footer_funs`. Each
  sub-list is the list of arguments to be used when calling a function
  in `footer_funs`. It must be an empty
  [`list()`](https://rdrr.io/r/base/list.html) if no additional
  arguments are to be used when calling a function in `footer_funs`.

- x:

  The object to be printed. Should be the output of
  `parameterEstimates_table_list()`.

- nd:

  The number of decimal places to be displayed for numeric cells.

- by_group:

  If `TRUE`, the default, tables will be grouped by groups first, and
  then by grouped by sections (e.g., `Latent Variables`, `Regressions`,
  etc.). If `FALSE`, then the tables will be grouped by sections first,
  and then grouped by groups. No effect if the number of groups is equal
  to one.

- na_str:

  The string to be used for cells with `NA`. Default is `" "`, a white
  space.

## Value

### `parameterEstimates_table_list()`

A list of data frames of the class `parameterEstimates_table_list`, with
this structure.

- `group`: A list of data frames for each group. It is a list of length
  equal to one if the model has only one group. For each group, the
  content is a list of data frames, one for each section of the
  estimates.

- `model`: A list of tables for sections such as user-defined parameters
  (`"Defined Parameters"`) or model constraints (`"Constraints"`).

- `header`: A list of header sections.

- `footer`: A list of footer sections.

The decision of not having a `print` method is intentional. It is
intended to be used by other the `print` methods of other classes, to
create the formatted list of tables, and then print it by calling
`print_parameterEstimates_table_list()` internally.

### `print_parameterEstimates_table_list()`

The original input, `x`, is returned invisibly. Called for its side
effect to print the content of `x`.

## Details

This function creates an output mimicking the output format when
[`lavaan::parameterEstimates()`](https://rdrr.io/pkg/lavaan/man/parameterEstimates.html)
is called with `output` set to `"text"`. It only creates the output as a
list of data frames, grouped in sections like `Latent Variables` and
`Regression`, as in the printout of
[`lavaan::parameterEstimates()`](https://rdrr.io/pkg/lavaan/man/parameterEstimates.html).
It does not format the content. The actual printing is to be done by
`print_parameterEstimates_table_list()`, which will format the cells
before printing them.

This function is not intended to be used by end-users. It is intended to
be used inside other functions, such as a print method. Functions that
add columns to the parameter estimates table of a `lavaan` object can
use it and PRINT_parameterEstimates_table_list() to print the output in
the `lavaan`-style, but with columns modified as needed and with
additional header and/or footer sections added.

Therefore, it was developed with flexibility in mind, at the expense of
user-friendliness.

### Header and Footer Functions

If a list of functions is supplied to `header_funs` or `footer_funs`,
they will be used to generate the headers and/or footers. The first
argument of these function will be one of the followings.

If `object` is a data frame like object, then the first argument is this
object when calling thews functions.

If `object` is a `lavaan` object, then the first argument is the
parameter estimates table generated by
[`lavaan::parameterEstimates()`](https://rdrr.io/pkg/lavaan/man/parameterEstimates.html)
with `output = "text", header = TRUE`.

The output of these functions should be one of the following formats.

It can be a data frame with two optional attributes: `section_title` and
`print_args`. If `section_title` is not null, it will be printed by
[`cat()`](https://rdrr.io/r/base/cat.html) before printing tbe section.
The header or footer section will then be printed by
[`print()`](https://rdrr.io/r/base/print.html). If `print_args` is set
to be a list of named arguments, then they will be used when calling
[`print()`](https://rdrr.io/r/base/print.html). For example, setting
`print_args` to `list(right = FALSE, row.names = FALSE)` will print the
data frame with these arguments.

It can also be any other object that can be printed. One possible case
is a character vector of footnotes. In this case, we can add this
attribute `print_fun` and set it to `"cat"`, the name of the function to
be used to print the section, and add the attribute `print_args` and set
it to be a named list of arguments to be passed to `print_fun`.

Special treatment when `print-fun` is "cat"\`:

- The default of `sep` is `"\n"`. To override this default, set the
  attribute `print_args` and set `sep` to something else.

- Each element in the object, which should be a character vector, is
  processed by [`strwrap()`](https://rdrr.io/r/base/strwrap.html) by
  default. Additional arguments to
  [`strwrap()`](https://rdrr.io/r/base/strwrap.html) can be passed by
  setting the attribute `strwrap_args` to a named list of the arguments
  for [`strwrap()`](https://rdrr.io/r/base/strwrap.html) (e.g.,
  `list(exdent = 2)`). To disable this feature, set the attribute
  `wrap_lines` to `FALSE`.

These arguments `header_funs` and `footer_args` allow users to add
header and footer sections and print them in the desired format.

## Limitations

These function do not yet support multilevel models.

## See also

`print_parameterEstimates_table_list()` for the printing function, and
[`lavaan::parameterEstimates()`](https://rdrr.io/pkg/lavaan/man/parameterEstimates.html)
for generating the parameter estimates table.

## Author

Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>

## Examples

``` r

# Adapted from the help of lavaan::cfa()
library(lavaan)
#> This is lavaan 0.7-2
#> lavaan is FREE software! Please report any bugs.
mod <- "
visual  =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed   =~ x7 + x8 + x9
"
fit <- cfa(mod,
           data = HolzingerSwineford1939)
est <- parameterEstimates_table_list(fit,
                                     rename_cols = c("P(>|z|)" = "pvalue",
                                                     "S.E." = "SE"))
print_parameterEstimates_table_list(est,
                                    drop = "Z")
#> 
#> Parameter Estimates Settings:
#>                                              
#>  Standard errors:                  Standard  
#>  Information:                      Expected  
#>  Information saturated (h1) model: Structured
#> 
#> Latent Variables:
#>             Estimate    SE pvalue CI.Lo CI.Up
#>  visual =~                                   
#>   x1           1.000                         
#>   x2           0.554 0.100  0.000 0.358 0.749
#>   x3           0.729 0.109  0.000 0.516 0.943
#>  textual =~                                  
#>   x4           1.000                         
#>   x5           1.113 0.065  0.000 0.985 1.241
#>   x6           0.926 0.055  0.000 0.817 1.035
#>  speed =~                                    
#>   x7           1.000                         
#>   x8           1.180 0.165  0.000 0.857 1.503
#>   x9           1.082 0.151  0.000 0.785 1.378
#> 
#> Covariances:
#>             Estimate    SE pvalue CI.Lo CI.Up
#>  visual ~~                                   
#>   textual      0.408 0.074  0.000 0.264 0.552
#>   speed        0.262 0.056  0.000 0.152 0.373
#>  textual ~~                                  
#>   speed        0.173 0.049  0.000 0.077 0.270
#> 
#> Variances:
#>             Estimate    SE pvalue CI.Lo CI.Up
#>  .x1           0.549 0.114  0.000 0.326 0.772
#>  .x2           1.134 0.102  0.000 0.934 1.333
#>  .x3           0.844 0.091  0.000 0.667 1.022
#>  .x4           0.371 0.048  0.000 0.278 0.465
#>  .x5           0.446 0.058  0.000 0.332 0.561
#>  .x6           0.356 0.043  0.000 0.272 0.441
#>  .x7           0.799 0.081  0.000 0.640 0.959
#>  .x8           0.488 0.074  0.000 0.342 0.633
#>  .x9           0.566 0.071  0.000 0.427 0.705
#>   visual       0.809 0.145  0.000 0.524 1.094
#>   textual      0.979 0.112  0.000 0.760 1.199
#>   speed        0.384 0.086  0.000 0.215 0.553
fit2 <- cfa(mod,
            data = HolzingerSwineford1939,
            group = "school")
est2 <- parameterEstimates_table_list(fit2)
# The tables in the same group are printed together (default)
print_parameterEstimates_table_list(est2,
                                    by_group = TRUE)
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
#>   x2           0.394 0.122  3.220   0.001  0.154 0.633
#>   x3           0.570 0.140  4.076   0.000  0.296 0.844
#>  textual =~                                           
#>   x4           1.000                                  
#>   x5           1.183 0.102 11.613   0.000  0.984 1.383
#>   x6           0.875 0.077 11.421   0.000  0.725 1.025
#>  speed =~                                             
#>   x7           1.000                                  
#>   x8           1.125 0.277  4.057   0.000  0.581 1.668
#>   x9           0.922 0.225  4.104   0.000  0.482 1.362
#> 
#> Group 1 [Pasteur]:
#> Covariances:
#>             Estimate  S.E.      Z P(>|z|)  CI.Lo CI.Up
#>  visual ~~                                            
#>   textual      0.479 0.106  4.531   0.000  0.272 0.686
#>   speed        0.185 0.077  2.397   0.017  0.034 0.337
#>  textual ~~                                           
#>   speed        0.182 0.069  2.628   0.009  0.046 0.317
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
#>  .x7           4.432 0.087 51.181   0.000  4.263 4.602
#>  .x8           5.563 0.078 71.214   0.000  5.410 5.716
#>  .x9           5.418 0.079 68.440   0.000  5.263 5.573
#>   visual       0.000                                  
#>   textual      0.000                                  
#>   speed        0.000                                  
#> 
#> Group 1 [Pasteur]:
#> Variances:
#>             Estimate  S.E.      Z P(>|z|)  CI.Lo CI.Up
#>  .x1           0.298 0.232  1.286   0.198 -0.156 0.753
#>  .x2           1.334 0.158  8.464   0.000  1.025 1.643
#>  .x3           0.989 0.136  7.271   0.000  0.723 1.256
#>  .x4           0.425 0.069  6.138   0.000  0.290 0.561
#>  .x5           0.456 0.086  5.292   0.000  0.287 0.624
#>  .x6           0.290 0.050  5.780   0.000  0.192 0.388
#>  .x7           0.820 0.125  6.580   0.000  0.576 1.065
#>  .x8           0.510 0.116  4.406   0.000  0.283 0.736
#>  .x9           0.680 0.104  6.516   0.000  0.476 0.885
#>   visual       1.097 0.276  3.967   0.000  0.555 1.639
#>   textual      0.894 0.150  5.963   0.000  0.600 1.188
#>   speed        0.350 0.126  2.778   0.005  0.103 0.596
#> 
#> Group 2 [Grant-White]:
#> Latent Variables:
#>             Estimate  S.E.      Z P(>|z|)  CI.Lo CI.Up
#>  visual =~                                            
#>   x1           1.000                                  
#>   x2           0.736 0.155  4.760   0.000  0.433 1.039
#>   x3           0.925 0.166  5.583   0.000  0.600 1.249
#>  textual =~                                           
#>   x4           1.000                                  
#>   x5           0.990 0.087 11.418   0.000  0.820 1.160
#>   x6           0.963 0.085 11.377   0.000  0.797 1.129
#>  speed =~                                             
#>   x7           1.000                                  
#>   x8           1.226 0.187  6.569   0.000  0.860 1.592
#>   x9           1.058 0.165  6.429   0.000  0.735 1.380
#> 
#> Group 2 [Grant-White]:
#> Covariances:
#>             Estimate  S.E.      Z P(>|z|)  CI.Lo CI.Up
#>  visual ~~                                            
#>   textual      0.408 0.098  4.153   0.000  0.215 0.600
#>   speed        0.276 0.076  3.639   0.000  0.127 0.425
#>  textual ~~                                           
#>   speed        0.222 0.073  3.022   0.003  0.078 0.365
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
#>  .x7           3.921 0.086 45.819   0.000  3.753 4.089
#>  .x8           5.488 0.087 63.174   0.000  5.318 5.659
#>  .x9           5.327 0.085 62.571   0.000  5.160 5.494
#>   visual       0.000                                  
#>   textual      0.000                                  
#>   speed        0.000                                  
#> 
#> Group 2 [Grant-White]:
#> Variances:
#>             Estimate  S.E.      Z P(>|z|)  CI.Lo CI.Up
#>  .x1           0.715 0.126  5.676   0.000  0.468 0.962
#>  .x2           0.899 0.123  7.339   0.000  0.659 1.139
#>  .x3           0.557 0.103  5.409   0.000  0.355 0.759
#>  .x4           0.315 0.065  4.870   0.000  0.188 0.442
#>  .x5           0.419 0.072  5.812   0.000  0.278 0.560
#>  .x6           0.406 0.069  5.880   0.000  0.271 0.541
#>  .x7           0.600 0.091  6.584   0.000  0.422 0.779
#>  .x8           0.401 0.094  4.249   0.000  0.216 0.586
#>  .x9           0.535 0.089  6.010   0.000  0.360 0.709
#>   visual       0.604 0.160  3.762   0.000  0.289 0.918
#>   textual      0.942 0.152  6.177   0.000  0.643 1.241
#>   speed        0.461 0.118  3.910   0.000  0.230 0.693
# The table are grouped by section then by group
print_parameterEstimates_table_list(est2,
                                    by_group = FALSE)
#> 
#> Parameter Estimates Settings:
#>                                              
#>  Standard errors:                  Standard  
#>  Information:                      Expected  
#>  Information saturated (h1) model: Structured
#> 
#> Latent Variables:
#> Group 1 [Pasteur]:
#>             Estimate  S.E.      Z P(>|z|)  CI.Lo CI.Up
#>  visual =~                                            
#>   x1           1.000                                  
#>   x2           0.394 0.122  3.220   0.001  0.154 0.633
#>   x3           0.570 0.140  4.076   0.000  0.296 0.844
#>  textual =~                                           
#>   x4           1.000                                  
#>   x5           1.183 0.102 11.613   0.000  0.984 1.383
#>   x6           0.875 0.077 11.421   0.000  0.725 1.025
#>  speed =~                                             
#>   x7           1.000                                  
#>   x8           1.125 0.277  4.057   0.000  0.581 1.668
#>   x9           0.922 0.225  4.104   0.000  0.482 1.362
#> 
#> Latent Variables:
#> Group 2 [Grant-White]:
#>             Estimate  S.E.      Z P(>|z|)  CI.Lo CI.Up
#>  visual =~                                            
#>   x1           1.000                                  
#>   x2           0.736 0.155  4.760   0.000  0.433 1.039
#>   x3           0.925 0.166  5.583   0.000  0.600 1.249
#>  textual =~                                           
#>   x4           1.000                                  
#>   x5           0.990 0.087 11.418   0.000  0.820 1.160
#>   x6           0.963 0.085 11.377   0.000  0.797 1.129
#>  speed =~                                             
#>   x7           1.000                                  
#>   x8           1.226 0.187  6.569   0.000  0.860 1.592
#>   x9           1.058 0.165  6.429   0.000  0.735 1.380
#> 
#> Covariances:
#> Group 1 [Pasteur]:
#>             Estimate  S.E.      Z P(>|z|)  CI.Lo CI.Up
#>  visual ~~                                            
#>   textual      0.479 0.106  4.531   0.000  0.272 0.686
#>   speed        0.185 0.077  2.397   0.017  0.034 0.337
#>  textual ~~                                           
#>   speed        0.182 0.069  2.628   0.009  0.046 0.317
#> 
#> Covariances:
#> Group 2 [Grant-White]:
#>             Estimate  S.E.      Z P(>|z|)  CI.Lo CI.Up
#>  visual ~~                                            
#>   textual      0.408 0.098  4.153   0.000  0.215 0.600
#>   speed        0.276 0.076  3.639   0.000  0.127 0.425
#>  textual ~~                                           
#>   speed        0.222 0.073  3.022   0.003  0.078 0.365
#> 
#> Intercepts:
#> Group 1 [Pasteur]:
#>             Estimate  S.E.      Z P(>|z|)  CI.Lo CI.Up
#>  .x1           4.941 0.095 52.249   0.000  4.756 5.127
#>  .x2           5.984 0.098 60.949   0.000  5.792 6.176
#>  .x3           2.487 0.093 26.778   0.000  2.305 2.669
#>  .x4           2.823 0.092 30.689   0.000  2.642 3.003
#>  .x5           3.995 0.105 38.183   0.000  3.790 4.200
#>  .x6           1.922 0.079 24.321   0.000  1.767 2.077
#>  .x7           4.432 0.087 51.181   0.000  4.263 4.602
#>  .x8           5.563 0.078 71.214   0.000  5.410 5.716
#>  .x9           5.418 0.079 68.440   0.000  5.263 5.573
#>   visual       0.000                                  
#>   textual      0.000                                  
#>   speed        0.000                                  
#> 
#> Intercepts:
#> Group 2 [Grant-White]:
#>             Estimate  S.E.      Z P(>|z|)  CI.Lo CI.Up
#>  .x1           4.930 0.095 51.696   0.000  4.743 5.117
#>  .x2           6.200 0.092 67.416   0.000  6.020 6.380
#>  .x3           1.996 0.086 23.195   0.000  1.827 2.164
#>  .x4           3.317 0.093 35.625   0.000  3.135 3.500
#>  .x5           4.712 0.096 48.986   0.000  4.524 4.901
#>  .x6           2.469 0.094 26.277   0.000  2.285 2.653
#>  .x7           3.921 0.086 45.819   0.000  3.753 4.089
#>  .x8           5.488 0.087 63.174   0.000  5.318 5.659
#>  .x9           5.327 0.085 62.571   0.000  5.160 5.494
#>   visual       0.000                                  
#>   textual      0.000                                  
#>   speed        0.000                                  
#> 
#> Variances:
#> Group 1 [Pasteur]:
#>             Estimate  S.E.      Z P(>|z|)  CI.Lo CI.Up
#>  .x1           0.298 0.232  1.286   0.198 -0.156 0.753
#>  .x2           1.334 0.158  8.464   0.000  1.025 1.643
#>  .x3           0.989 0.136  7.271   0.000  0.723 1.256
#>  .x4           0.425 0.069  6.138   0.000  0.290 0.561
#>  .x5           0.456 0.086  5.292   0.000  0.287 0.624
#>  .x6           0.290 0.050  5.780   0.000  0.192 0.388
#>  .x7           0.820 0.125  6.580   0.000  0.576 1.065
#>  .x8           0.510 0.116  4.406   0.000  0.283 0.736
#>  .x9           0.680 0.104  6.516   0.000  0.476 0.885
#>   visual       1.097 0.276  3.967   0.000  0.555 1.639
#>   textual      0.894 0.150  5.963   0.000  0.600 1.188
#>   speed        0.350 0.126  2.778   0.005  0.103 0.596
#> 
#> Variances:
#> Group 2 [Grant-White]:
#>             Estimate  S.E.      Z P(>|z|)  CI.Lo CI.Up
#>  .x1           0.715 0.126  5.676   0.000  0.468 0.962
#>  .x2           0.899 0.123  7.339   0.000  0.659 1.139
#>  .x3           0.557 0.103  5.409   0.000  0.355 0.759
#>  .x4           0.315 0.065  4.870   0.000  0.188 0.442
#>  .x5           0.419 0.072  5.812   0.000  0.278 0.560
#>  .x6           0.406 0.069  5.880   0.000  0.271 0.541
#>  .x7           0.600 0.091  6.584   0.000  0.422 0.779
#>  .x8           0.401 0.094  4.249   0.000  0.216 0.586
#>  .x9           0.535 0.089  6.010   0.000  0.360 0.709
#>   visual       0.604 0.160  3.762   0.000  0.289 0.918
#>   textual      0.942 0.152  6.177   0.000  0.643 1.241
#>   speed        0.461 0.118  3.910   0.000  0.230 0.693

# A simple function to add "***" for parameters with p-values < .001
add_sig <- function(object,
                    pvalue = "pvalue") {
    tmp <- object[, pvalue, drop = TRUE]
    if (!is.null(tmp)) {
        tmp[is.na(tmp)] <- 1
        tmp2 <- ifelse(tmp < .001, "***", "")
        i <- match(pvalue, colnames(object))
        out <- data.frame(object[, 1:i],
                          Sig = tmp2,
                          object[, seq(i + 1, ncol(object))])
      }
    out
  }

est3 <- parameterEstimates_table_list(fit2,
                                      est_funs = list(add_sig))
print_parameterEstimates_table_list(est3)
#> 
#> Parameter Estimates Settings:
#>                                              
#>  Standard errors:                  Standard  
#>  Information:                      Expected  
#>  Information saturated (h1) model: Structured
#> 
#> Group 1 [Pasteur]:
#> Latent Variables:
#>             Estimate  S.E.      Z P(>|z|) Sig  CI.Lo CI.Up
#>  visual =~                                                
#>   x1           1.000                                      
#>   x2           0.394 0.122  3.220   0.001      0.154 0.633
#>   x3           0.570 0.140  4.076   0.000 ***  0.296 0.844
#>  textual =~                                               
#>   x4           1.000                                      
#>   x5           1.183 0.102 11.613   0.000 ***  0.984 1.383
#>   x6           0.875 0.077 11.421   0.000 ***  0.725 1.025
#>  speed =~                                                 
#>   x7           1.000                                      
#>   x8           1.125 0.277  4.057   0.000 ***  0.581 1.668
#>   x9           0.922 0.225  4.104   0.000 ***  0.482 1.362
#> 
#> Group 1 [Pasteur]:
#> Covariances:
#>             Estimate  S.E.      Z P(>|z|) Sig  CI.Lo CI.Up
#>  visual ~~                                                
#>   textual      0.479 0.106  4.531   0.000 ***  0.272 0.686
#>   speed        0.185 0.077  2.397   0.017      0.034 0.337
#>  textual ~~                                               
#>   speed        0.182 0.069  2.628   0.009      0.046 0.317
#> 
#> Group 1 [Pasteur]:
#> Intercepts:
#>             Estimate  S.E.      Z P(>|z|) Sig  CI.Lo CI.Up
#>  .x1           4.941 0.095 52.249   0.000 ***  4.756 5.127
#>  .x2           5.984 0.098 60.949   0.000 ***  5.792 6.176
#>  .x3           2.487 0.093 26.778   0.000 ***  2.305 2.669
#>  .x4           2.823 0.092 30.689   0.000 ***  2.642 3.003
#>  .x5           3.995 0.105 38.183   0.000 ***  3.790 4.200
#>  .x6           1.922 0.079 24.321   0.000 ***  1.767 2.077
#>  .x7           4.432 0.087 51.181   0.000 ***  4.263 4.602
#>  .x8           5.563 0.078 71.214   0.000 ***  5.410 5.716
#>  .x9           5.418 0.079 68.440   0.000 ***  5.263 5.573
#>   visual       0.000                                      
#>   textual      0.000                                      
#>   speed        0.000                                      
#> 
#> Group 1 [Pasteur]:
#> Variances:
#>             Estimate  S.E.      Z P(>|z|) Sig  CI.Lo CI.Up
#>  .x1           0.298 0.232  1.286   0.198     -0.156 0.753
#>  .x2           1.334 0.158  8.464   0.000 ***  1.025 1.643
#>  .x3           0.989 0.136  7.271   0.000 ***  0.723 1.256
#>  .x4           0.425 0.069  6.138   0.000 ***  0.290 0.561
#>  .x5           0.456 0.086  5.292   0.000 ***  0.287 0.624
#>  .x6           0.290 0.050  5.780   0.000 ***  0.192 0.388
#>  .x7           0.820 0.125  6.580   0.000 ***  0.576 1.065
#>  .x8           0.510 0.116  4.406   0.000 ***  0.283 0.736
#>  .x9           0.680 0.104  6.516   0.000 ***  0.476 0.885
#>   visual       1.097 0.276  3.967   0.000 ***  0.555 1.639
#>   textual      0.894 0.150  5.963   0.000 ***  0.600 1.188
#>   speed        0.350 0.126  2.778   0.005      0.103 0.596
#> 
#> Group 2 [Grant-White]:
#> Latent Variables:
#>             Estimate  S.E.      Z P(>|z|) Sig  CI.Lo CI.Up
#>  visual =~                                                
#>   x1           1.000                                      
#>   x2           0.736 0.155  4.760   0.000 ***  0.433 1.039
#>   x3           0.925 0.166  5.583   0.000 ***  0.600 1.249
#>  textual =~                                               
#>   x4           1.000                                      
#>   x5           0.990 0.087 11.418   0.000 ***  0.820 1.160
#>   x6           0.963 0.085 11.377   0.000 ***  0.797 1.129
#>  speed =~                                                 
#>   x7           1.000                                      
#>   x8           1.226 0.187  6.569   0.000 ***  0.860 1.592
#>   x9           1.058 0.165  6.429   0.000 ***  0.735 1.380
#> 
#> Group 2 [Grant-White]:
#> Covariances:
#>             Estimate  S.E.      Z P(>|z|) Sig  CI.Lo CI.Up
#>  visual ~~                                                
#>   textual      0.408 0.098  4.153   0.000 ***  0.215 0.600
#>   speed        0.276 0.076  3.639   0.000 ***  0.127 0.425
#>  textual ~~                                               
#>   speed        0.222 0.073  3.022   0.003      0.078 0.365
#> 
#> Group 2 [Grant-White]:
#> Intercepts:
#>             Estimate  S.E.      Z P(>|z|) Sig  CI.Lo CI.Up
#>  .x1           4.930 0.095 51.696   0.000 ***  4.743 5.117
#>  .x2           6.200 0.092 67.416   0.000 ***  6.020 6.380
#>  .x3           1.996 0.086 23.195   0.000 ***  1.827 2.164
#>  .x4           3.317 0.093 35.625   0.000 ***  3.135 3.500
#>  .x5           4.712 0.096 48.986   0.000 ***  4.524 4.901
#>  .x6           2.469 0.094 26.277   0.000 ***  2.285 2.653
#>  .x7           3.921 0.086 45.819   0.000 ***  3.753 4.089
#>  .x8           5.488 0.087 63.174   0.000 ***  5.318 5.659
#>  .x9           5.327 0.085 62.571   0.000 ***  5.160 5.494
#>   visual       0.000                                      
#>   textual      0.000                                      
#>   speed        0.000                                      
#> 
#> Group 2 [Grant-White]:
#> Variances:
#>             Estimate  S.E.      Z P(>|z|) Sig  CI.Lo CI.Up
#>  .x1           0.715 0.126  5.676   0.000 ***  0.468 0.962
#>  .x2           0.899 0.123  7.339   0.000 ***  0.659 1.139
#>  .x3           0.557 0.103  5.409   0.000 ***  0.355 0.759
#>  .x4           0.315 0.065  4.870   0.000 ***  0.188 0.442
#>  .x5           0.419 0.072  5.812   0.000 ***  0.278 0.560
#>  .x6           0.406 0.069  5.880   0.000 ***  0.271 0.541
#>  .x7           0.600 0.091  6.584   0.000 ***  0.422 0.779
#>  .x8           0.401 0.094  4.249   0.000 ***  0.216 0.586
#>  .x9           0.535 0.089  6.010   0.000 ***  0.360 0.709
#>   visual       0.604 0.160  3.762   0.000 ***  0.289 0.918
#>   textual      0.942 0.152  6.177   0.000 ***  0.643 1.241
#>   speed        0.461 0.118  3.910   0.000 ***  0.230 0.693
```
