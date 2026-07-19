library(testthat)
library(lavaan.printer)

add_sig <- function(object,
                    pvalue = "pvalue",
                    breaks = c(1, .05, .01, .001, -Inf),
                    labels = c("***", "** ", "*  ", "  ")) {
    tmp <- object[, pvalue, drop = TRUE]
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

add_ci_sig <- function(object,
                       ci.lower = "ci.lower",
                       ci.upper = "ci.upper",
                       yes = "Sig.",
                       no = "n.s.") {
    tmp1 <- object[, ci.lower, drop = TRUE]
    tmp2 <- object[, ci.upper, drop = TRUE]
    if (!is.null(tmp1) && !is.null(tmp2)) {
        j0 <- ifelse(((tmp2 < 0) | (tmp1 > 0)),
                     yes = yes,
                     no = no)
        j0[(tmp1 == tmp2)] <- ""
        j0[is.na(tmp1) | is.na(tmp2)] <- ""
        i <- match(ci.upper, colnames(object))
        if (i == ncol(object)) {
            out <- data.frame(object,
                              CI.Sig = j0)
          } else {
            out <- data.frame(object[, 1:i],
                              CI.Sig = j0,
                              object[, seq(i + 1, ncol(object)), drop = FALSE])
          }
      }
    out
  }

test_that("SAM", {
library(lavaan)
data(PoliticalDemocracy)
# From sam() example
model <- '
  # latent variable definitions
     ind60 =~ x1 + x2 + x3
     dem60 =~ y1 + a*y2 + b*y3 + c*y4
     dem65 =~ y5 + a*y6 + b*y7 + c*y8

  # regressions
    dem60 ~ ind60
    dem65 ~ ind60 + dem60

  # residual correlations
    y1 ~~ y5
    y2 ~~ y4 + y6
    y3 ~~ y7
    y4 ~~ y8
    y6 ~~ y8
'

fit <- sam(model, data = PoliticalDemocracy,
           mm.list = list(ind = "ind60", dem = c("dem60", "dem65")))
est <- parameterEstimates(fit,
                          standardized = TRUE,
                          rsquare =  TRUE,
                          remove.system.eq = FALSE,
                          remove.eq = FALSE,
                          remove.ineq = FALSE,
                          add.attributes = TRUE,
                          head = TRUE)
# summary(fit, ci = TRUE, standardized = TRUE)
out <- parameterEstimates_table_list(fit,
                                     remove.eq = FALSE,
                                     standardized = TRUE,
                                     ci = TRUE,
                                     rsquare = TRUE)
tmp <- capture.output(print_parameterEstimates_table_list(out))
expect_true(
  any(grepl("Twostep", tmp, fixed = TRUE))
)
expect_true(any(grepl("0.0000",
                      capture.output(print_parameterEstimates_table_list(out, nd = 4)),
                      fixed = TRUE)))
expect_true(any(grepl("0.00",
                      capture.output(print_parameterEstimates_table_list(out, nd = 2)),
                      fixed = TRUE)))
expect_true(any(grepl("--",
                      capture.output(print_parameterEstimates_table_list(out, nd = 2, na_str = "--")),
                      fixed = TRUE)))
out <- parameterEstimates_table_list(est,
                                     fit_object = fit)
expect_false(any(grepl("Std.lv",
                       capture.output(print_parameterEstimates_table_list(out,
                                                                          nd = 5,
                                                                          drop_cols = "std.lv")),
                       fixed = TRUE)))
})

