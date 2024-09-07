library(testthat)
library(lavaan.printer)

test_that("Unsupported objects", {
library(lavaan)
mod <-
"
level: 1
  x1 ~ x2 + x3
level: 2
  f1 ~ x2 + x3
"
fit <- sem(mod,
           do.fit = FALSE)
est <- parameterEstimates(fit)
expect_error(parameterEstimates_table_list(fit), "Multilevel")
expect_error(parameterEstimates_table_list(est), "Multilevel")
expect_error(parameterEstimates_table_list(mod), "lavaan")
})
