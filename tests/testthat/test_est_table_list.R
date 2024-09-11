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
out <- parameterEstimates_table_list(fit2,
                                     remove.eq = FALSE,
                                     standardized = TRUE,
                                     ci = TRUE,
                                     rsquare = TRUE,
                                     est_funs = list(add_sig,
                                                     add_ci_sig))
tmp <- capture.output(print_parameterEstimates_table_list(out))
expect_true(any(grepl("***", tmp, fixed = TRUE)))
expect_true(any(grepl("Sig.", tmp, fixed = TRUE)))
out <- parameterEstimates_table_list(fit2,
                                     remove.eq = FALSE,
                                     standardized = TRUE,
                                     ci = TRUE,
                                     rsquare = TRUE,
                                     est_funs = list(add_sig,
                                                     add_ci_sig),
                                     est_funs_args = list(list(),
                                                          list(yes = "Yes",
                                                               no = "No")))
tmp <- capture.output(print_parameterEstimates_table_list(out))
expect_true(any(grepl("Yes", tmp, fixed = TRUE)))
expect_true(any(grepl("No", tmp, fixed = TRUE)))
expect_error(parameterEstimates_table_list(fit2,
                                           est_funs = list(add_sig,
                                                           add_ci_sig),
                                           est_funs_args = list(list())))
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
library(lavaan)
data(HolzingerSwineford1939)
mod <- '
# efa block
efa("efa")*f1 + efa("efa")*f2 =~ x1 + x2 + x3 + x4 + x5 + x6
# cfa block
f3 =~ x7 + x8 + x9
# regression
f3 ~ f1 + f2
'
fit <- sem(model = mod,
           data = HolzingerSwineford1939,
           rotation = "geomin",
           rotation.args = list(geomin.epsilon = 0.01, rstarts = 1))
summary(fit)
est <- parameterEstimates(fit, standardized = TRUE, rsquare = TRUE, add.attributes = TRUE, head = TRUE)
out <- parameterEstimates_table_list(est,
                                     est_FUNs = list(add_sig,
                                                     add_ci_sig))
out <- capture.output(print_parameterEstimates_table_list(out,
                                                          drop_cols = "Std.lv"))
expect_true(any(grepl(" =~ efa", out)))
})

test_that("funs", {
library(lavaan)
data(HolzingerSwineford1939)
HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9
              '
fit <- cfa(HS.model, data = HolzingerSwineford1939)

test_section <- function(x,
                         arg1 = "field:",
                         arg2 = "value",
                         section_name = "Section Name:") {
    out0 <- list(field = character(0),
                 val = character(0))
    out0$field <- c(out0$field,
                    arg1)
    out0$val <- c(out0$val,
                  arg2)
    out <- data.frame(out0)
    colnames(out) <- c("", "")
    attr(out, "section_title") <- section_name
    attr(out, "print_args") <- list(right = FALSE,
                                    row.names = FALSE)
    out
  }
out <- parameterEstimates_table_list(fit,
                                     est_funs = list(add_ci_sig,
                                                     add_sig),
                                     header_funs = list(test_section,
                                                        test_section),
                                     est_funs_args = list(list(yes = "YES", no = "NO"),
                                                          list(breaks = c(1, .05, -Inf),
                                                               labels = c("*", "n.s."))),
                                     header_funs_args = list(list(section_name = "Header 1:"),
                                                             list(section_name = "Header 2::",
                                                                  arg2 = "Arg2_2",
                                                                  arg1 = "Arg2_1:")),
                                     footer_funs = list(test_section,
                                                        test_section,
                                                        test_section),
                                     footer_funs_args = list(list(),
                                                             list(arg1 = "Footer Field 1:",
                                                                  arg2 = "Footer Value 1",
                                                                  section_name = "Footer 2::"),
                                                             list()))
tmp <- capture.output(print_parameterEstimates_table_list(out))
expect_true(any(grepl("Footer 2::", tmp, fixed = TRUE)))
expect_true(any(grepl("Footer Field 1:", tmp, fixed = TRUE)))
expect_true(any(grepl("Header 2::", tmp, fixed = TRUE)))
expect_true(any(grepl("CI.Sig", tmp, fixed = TRUE)))
expect_true(any(grepl("*", tmp, fixed = TRUE)))
expect_true(any(grepl("n.s.", tmp, fixed = TRUE)))

# strwrap

test_section_note <- function(x,
                              notes = "- Note 1",
                              section_name = "Notes:",
                              strwrap_args = list(),
                              wrap_lines = TRUE) {
    attr(notes, "print_fun") <- "cat"
    attr(notes, "strwrap_args") <- strwrap_args
    attr(notes, "section_title") <- section_name
    attr(notes, "wrap_lines") <- wrap_lines
    notes
  }

out <- parameterEstimates_table_list(fit,
                                     est_funs = list(add_ci_sig,
                                                     add_sig),
                                     header_funs = list(test_section,
                                                        test_section_note),
                                     est_funs_args = list(list(yes = "YES", no = "NO"),
                                                          list(breaks = c(1, .05, -Inf),
                                                               labels = c("*", "n.s."))),
                                     header_funs_args = list(list(section_name = "Header 1:"),
                                                             list(section_name = "Header Note",
                                                                  notes = c("- 1: This is a very very long sentence for testing strwrap with in printing a header. It should be wrapped when printed",
                                                                           "- 2: This is a very very long sentence for testing strwrap with in printing a header. It should be wrapped when printed"))),
                                     footer_funs = list(test_section,
                                                        test_section_note,
                                                        test_section),
                                     footer_funs_args = list(list(),
                                                             list(notes = c("> Note 1: This is a very very long sentence for testing strwrap with in printing a header. It should be wrapped when printed",
                                                                           "> Note 2: This is a very very long sentence for testing strwrap with in printing a header. It should be wrapped when printed"),
                                                                  section_name = "Footer Note:",
                                                                  strwrap_args = list(exdent = 2)),
                                                             list()))
tmp <- capture.output(print_parameterEstimates_table_list(out))
expect_true(any(grepl("Footer Note:", tmp, fixed = TRUE)))
expect_true(max(sapply(tmp, nchar)) < 80)

out <- parameterEstimates_table_list(fit,
                                     est_funs = list(add_ci_sig,
                                                     add_sig),
                                     header_funs = list(test_section),
                                     est_funs_args = list(list(yes = "YES", no = "NO"),
                                                          list(breaks = c(1, .05, -Inf),
                                                               labels = c("*", "n.s."))),
                                     header_funs_args = list(list(section_name = "Header 1:")),
                                     footer_funs = list(test_section,
                                                        test_section_note,
                                                        test_section),
                                     footer_funs_args = list(list(),
                                                             list(notes = c("> Note 1: This is a very very long sentence for testing strwrap with in printing a header. It should be wrapped when printed",
                                                                           "> Note 2: This is a very very long sentence for testing strwrap with in printing a header. It should be wrapped when printed"),
                                                                  section_name = "Footer Note:",
                                                                  wrap_lines = FALSE),
                                                             list()))
tmp <- capture.output(print_parameterEstimates_table_list(out))
expect_true(max(sapply(tmp, nchar)) > 80)
})

