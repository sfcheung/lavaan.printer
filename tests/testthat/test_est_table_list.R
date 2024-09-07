library(testthat)
library(lavaan.printer)

add_sig <- function(object) {
    tmp <- object$pvalue
    if (!is.null(tmp)) {
        tmp[is.na(tmp)] <- 1
        tmp2 <- cut(tmp,
                    breaks = c(1, .05, .01, .001, -Inf),
                    labels = c("***", "** ", "*  ", "  "))
        i <- match("pvalue", colnames(object))
        out <- data.frame(object[, 1:i],
                          Sig = tmp2,
                          object[, seq(i + 1, ncol(object))])
      }
    out
  }

add_ci_sig <- function(object,
                       ci.lower = "ci.lower",
                       ci.upper = "ci.upper") {
    tmp1 <- object[, ci.lower, drop = TRUE]
    tmp2 <- object[, ci.upper, drop = TRUE]
    if (!is.null(tmp1) && !is.null(tmp2)) {
        j0 <- ifelse(((tmp2 < 0) | (tmp1 > 0)),
                     yes = "Sig.",
                     no = "n.s.")
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


test_that("Models with thresholds", {
library(lavaan)
data(HolzingerSwineford1939)
HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6 + x8
              speed   =~ x7 + x8 + c(d1, d2)*x9
              textual ~ c(a1, a2)*speed + ageyr
              visual ~ c(b1, b2)*textual
              a1b1 := a1*b1
              a2b2 := a2*b2
              a1b1 == a2b2
              d1 == 1.35
              a1b1 > .20
              '
fit <- cfa(HS.model,
           data = HolzingerSwineford1939,
           meanstructure = TRUE,
           group = "school")
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
expect_true(max(which(grepl("Group 1", tmp, fixed = TRUE))) <
            min(which(grepl("Group 2", tmp, fixed = TRUE))))
tmp <- capture.output(print_parameterEstimates_table_list(out, by_group = FALSE))
expect_false(max(which(grepl("Group 1", tmp, fixed = TRUE))) <
             min(which(grepl("Group 2", tmp, fixed = TRUE))))
expect_true(all(which(grepl("Group 1", tmp, fixed = TRUE)) <
                which(grepl("Group 2", tmp, fixed = TRUE))))
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

test_that("Typical CFA with error covariances", {
library(lavaan)
data(HolzingerSwineford1939)
HS.model2 <- 'textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9
              x5 ~~ x9
              '
fit2 <- cfa(HS.model2, data = HolzingerSwineford1939, meanstructure = TRUE)
out <- parameterEstimates_table_list(fit2, remove.eq = FALSE, standardized = TRUE, ci = TRUE, rsquare = TRUE)
tmp <- capture.output(print_parameterEstimates_table_list(out))
expect_true(any(grepl(".x5 ~~", tmp, fixed = TRUE)))
expect_true(any(grepl(" .x9", tmp, fixed = TRUE)))
})

test_that("Multigroup CFA with equality constraints", {
library(lavaan)
data(HolzingerSwineford1939)
HolzingerSwineford1939$x2 <- cut(HolzingerSwineford1939$x2, breaks = 3)
head(HolzingerSwineford1939)
HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + c(d1, d2)*x9
              '
fit <- cfa(HS.model, data = HolzingerSwineford1939, meanstructure = TRUE, group = "school", ordered = "x2")
out <- parameterEstimates_table_list(fit, remove.eq = FALSE, standardized = TRUE, ci = TRUE, rsquare = TRUE)
tmp <- capture.output(print_parameterEstimates_table_list(out))
expect_true(any(grepl("Thresholds", tmp, fixed = TRUE)))
expect_true(any(grepl("(d2)", tmp, fixed = TRUE)))
})

test_that("EFA", {
skip("Internal tests")
# https://lavaan.ugent.be/tutorial/efa.html

ex5_25 <- read.table("http://statmodel.com/usersguide/chap5/ex5.25.dat")
names(ex5_25) = paste0("y",1:12)
model <- '
    # efa block
    efa("efa1")*f1 +
    efa("efa1")*f2 =~ y1 + y2 + y3 + y4 + y5 + y6

    # cfa block
    f3 =~ y7 + y8 + y9
    f4 =~ y10 + y11 + y12

    # regressions
    f3 ~ f1 + f2
    f4 ~ f3
'
fit <- sem(model = model, data = ex5_25, rotation = "geomin",
           # mimic Mplus
           information = "observed",
           rotation.args = list(rstarts = 30, row.weights = "none",
                                algorithm = "gpa", std.ov = TRUE,
                                geomin.epsilon = 0.0001),
           meanstructure = TRUE)
summary(fit)
est <- parameterEstimates(fit, standardized = TRUE, rsquare = TRUE, add.attributes = TRUE, head = TRUE)
out <- parameterEstimates_table_list(est,
                                     est_FUNs = list(add_sig,
                                                     add_ci_sig))
print_parameterEstimates_table_list(out,
                                    drop_cols = "Std.lv")
})

test_that("semlbci", {
skip_on_cran()
if (isFALSE(requireNamespace("semlbci", quietly = TRUE))) {
    skip("semlbci not installed")
  }
library(semlbci)
library(lavaan)
mod <-
"
m ~ a*x
y ~ b*m
ab := a * b
"
fit_med <- sem(mod, simple_med, fixed.x = FALSE)
p_table <- parameterTable(fit_med)
p_table
lbci_med <- semlbci(fit_med,
                    pars = c("m ~ x",
                             "y ~ m",
                             "ab :="),
                    use_pbapply = FALSE)
lbci_med

print(lbci_med, sem_out = fit_med, output = "text")

status_to_str <- function(object) {
    tmp <- object$post_check_lb
    if (!is.null(tmp)) {
        tmp <- ifelse(tmp, "OX", "Not OK")
        tmp[is.na(tmp)] <- ""
        object$post_check_lb <- tmp
      }
    tmp <- object$post_check_ub
    if (!is.null(tmp)) {
        tmp <- ifelse(tmp, "OX", "Not OK")
        tmp[is.na(tmp)] <- ""
        object$post_check_ub <- tmp
      }
    object
  }
out <- parameterEstimates_table_list(lbci_med,
                                     fit_object = fit_med,
                                     drop_cols = c("status_lb",
                                                   "status_ub",
                                                   "ratio_lb",
                                                   "ratio_ub",
                                                   "id",
                                                   "time_lb",
                                                   "time_ub",
                                                   "method"),
                                     rename_cols = c("post_check_lb" = "lbOK",
                                                     "post_check_ub" = "ubOK",
                                                     "ci_org_lb" = "Org_lb",
                                                     "ci_org_ub" = "Org_ub",
                                                     "lbci_lb" = "LB_lb",
                                                     "lbci_ub" = "LB_ub"),
                                     est_funs = list(status_to_str))
tmp <- capture.output(print_parameterEstimates_table_list(out,
                                                          na_str = "--"))
expect_true(any(grepl("LB_lb", tmp)))
expect_true(any(grepl("OX", tmp)))
expect_false(any(grepl("status_ub", tmp)))
})

test_that("semlrtp", {
skip_on_cran()
if (isFALSE(requireNamespace("semlrtp", quietly = TRUE))) {
    skip("semlrtp not installed")
  }
library(semlrtp)
library(lavaan)
data(data_sem16)
mod <-
"
f1 =~ x1 + x2 + x3
f2 =~ x4 + x5 + x6
f3 =~ x7 + x8 + x9
f4 =~ x10 + x11 + x12
f2 ~~ f1
f3 ~ f1 + f2
f4 ~ f3
"
fit <- sem(mod, data_sem16)
fit_lrtp <- lrtp(fit, progress = FALSE)

semlrtp_test_footer <- function(x) {
    # A test header
    out0 <- list(field = character(0),
                 val = character(0))
    ids <- attr(x, "ids")
    out0$field <- c(out0$field,
                    "The number of tests:")
    out0$val <- c(out0$val,
                  as.character(length(ids)))
    out <- data.frame(out0)
    colnames(out) <- c("", "")
    attr(out, "section_title") <- "LRTp Test Footer:"
    attr(out, "print_args") <- list(right = FALSE,
                                    row.names = FALSE)
    out
  }

semlrtp_test_footer2 <- function(x) {
    # A test header
    out <- c("- A long note.",
             "- A short note.")
    attr(out, "section_title") <- "Note:"
    attr(out, "print_args") <- list(sep = "\n")
    attr(out, "print_fun") <- "cat"
    out
  }

out <- parameterEstimates_table_list(fit_lrtp,
                                     drop_cols = c("se",
                                                   "z",
                                                   "ci.lower",
                                                   "ci.upper",
                                                   "fit0_ok",
                                                   "vcov_ok",
                                                   "LRT_ok",
                                                   "post_check_ok",
                                                   "LRT_id"),
                                     fit_object = fit,
                                     rename_cols = c(converge_ok = "New_OK"),
                                     est_funs = list(add_sig),
                                     header_funs = semlrtp_test_footer2,
                                     footer_funs = list(semlrtp_test_footer,
                                                        semlrtp_test_footer2))

tmp <- capture.output(print_parameterEstimates_table_list(out,
                                                          drop_cols = c("LRT", "converge_ok"),
                                                          na_str = "..."))
expect_true(any(grepl("Note:", tmp, fixed = TRUE)))
expect_true(any(grepl("A long note", tmp, fixed = TRUE)))
expect_false(any(grepl("converged_ok", tmp, fixed = TRUE)))
})

test_that("semhelpinghands", {
skip_on_cran()
if (isFALSE(requireNamespace("semhelpinghands", quietly = TRUE))) {
    skip("semhelpinghands not installed")
  }
if (isFALSE(requireNamespace("semlrtp", quietly = TRUE))) {
    skip("semlrtp not installed")
  }
library(lavaan)
library(semhelpinghands)
library(semlrtp)
mod <-
"
f1 =~ x1 + x2 + x3
f2 =~ x4 + x5 + x6
f3 =~ x7 + x8 + x9
f4 =~ x10 + x11 + x12
f2 ~~ f1
f3 ~ f1 + f2
f4 ~ f3
"
fit_boot <- sem(mod, data_sem16, se = "bootstrap", bootstrap = 50, iseed = 1234,
                warn = FALSE)
est <- standardizedSolution_boot_ci(fit_boot)

add_ci_sig_boot <- function(x) {add_ci_sig(x,
                                           ci.lower = "boot.ci.lower",
                                           ci.upper = "boot.ci.upper")}

fix_std_attributes <- function(object) {
    out1 <- attr(object, "pe_attrib")
    out1$names <- NULL
    out1$row.names <- NULL
    out1$class <- NULL
    # Do not overwrite existing attributes
    for (x in names(out1)) {
        if (!(x %in% names(attr(object, x)))) {
            attr(object, x) <- out1[[x]]
          }
      }
    object
  }
est <- fix_std_attributes(est)
names(attributes(est))
out <- parameterEstimates_table_list(est,
                                     fit_object = fit_boot,
                                     drop_cols = c("ci.lower",
                                                   "ci.upper",
                                                   "boot.se",
                                                   "se",
                                                   "z",
                                                   "pvalue"),
                                     rename_cols = c("boot.ci.lower" = "BootCI.Lo",
                                                     "boot.ci.upper" = "BootCI.Up",
                                                     "est.std" = "Standardized"),
                                     est_funs = add_ci_sig_boot)
expect_output(print_parameterEstimates_table_list(out),
              "Sig.")
})
