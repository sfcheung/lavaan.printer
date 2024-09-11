
#' @noRd
# Infer the group labels from the estimates table
group_labels_from_est <- function(x) {
    tmp <- attr(x, "group.label")
    if (!is.null(tmp)) {
        return(tmp)
      }
    group_ids <- groups_from_est(x)
    ngroups <- length(group_ids)
    if (ngroups == 1) {
        return(character(0))
      }
    as.character(group_ids)
  }

#' @noRd
# Infer the group numbers from the estimates table
groups_from_est <- function(x) {
    if (is.null(x$group)) {
        return(1)
      }
    i <- !(x$op %in% c("==", ":=", ">", "<"))
    j <- x$group > 0
    unique(stats::na.omit(x$group[i & j]))
  }

#' @noRd
# Infer the level ids from the estimates table
levels_from_est <- function(x) {
    if (is.null(x$level)) {
        return(1)
      }
    i <- !(x$op %in% c("==", ":=", ">", "<"))
    j <- x$level > 0
    unique(stats::na.omit(x$level[i & j]))
  }


#' @noRd
# Add lavaan header attributes
# Only add attributes not in object
add_header_attributes <- function(object,
                                  fit_object) {
    out0 <- lavaan::parameterEstimates(fit_object,
                                       output = "text",
                                       header = TRUE)
    out1 <- attributes(out0)
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

#' @noRd
# A wrapper to create the list of header sections
add_header <- function(object,
                       FUNs = list(),
                       args = list(list())) {
    out0 <- mapply(function(fun0, args0, object0) {
                      args1 <- utils::modifyList(list(object0),
                                                 args0)
                      do.call(fun0,
                              args1)
                    },
                   fun0 = FUNs,
                   args0 = args,
                   MoreArgs = list(object0 = object),
                   SIMPLIFY = FALSE)
    out0
  }


#' @noRd
# Create lavaan-style header for parameter estimates
# Should accept one object only.
# Output:
#  - Usually a data frame with these attributes:
#   - section_title: To be printed before the output.
#   - (Optional) print_fun: The name of the function to be
#     called when printing the section. If NULL, it is "print".
#     Useful when the section is not a data frame. For example,
#     "cat" can be used if the object is a list of character lines.
#   - (Optional) print_args: The list of arguments to be used when
#     calling print_fun.

add_header_lavaan <- function(object) {
    # Adapted from lavaan::print.lavaan.parameterEstimates()
    tmp <- attr(object, "header")
    if (is.null(tmp) || isFALSE(tmp)) {
        return(NULL)
      }
    out0 <- list(field = character(0),
                 val = character(0))
    # Categorical?
    if (attr(object, "categorical")) {
        out0$field <- c(out0$field,
                        "Parameterization (ordinal variables)")
        out0$val <- c(out0$val,
                      upper_first(attr(object, "parameterization")))

      }
    # SE?
    if (!is.null(object$se)) {
        object_se <- attr(object, "se")
        out0$field <- c(out0$field,
                        "Standard errors")
        tmp <- ifelse(object_se == "robust.huber.white",
                      "Sandwich (robust.huber.white)",
                      upper_first(object_se))
        out0$val <- c(out0$val,
                      tmp)
        # Information
        object_info <- attr(object, "information")
        object_obs_info <- attr(object, "observed.information")
        object_h1_info <- attr(object, "h1.information")
        object_h1_info_meat <- attr(object, "h1.information.meat")
        if (object_se == "robust.huber.white") {
            out0$field <- c(out0$field,
                            "Information Bread")
            out0$val <- c(out0$val,
                          upper_first(object_info))
          } else if (object_se == "bootstrap"){
            out0$field <- c(out0$field,
                            "Number of requested bootstrap draws",
                            "Number of successful bootstrap draws")
            out0$val <- c(out0$val,
                          attr(object, "bootstrap"),
                          attr(object, "bootstrap.successful"))
          } else {
            # Other SE methods
            out0$field <- c(out0$field,
                            "Information")
            out0$val <- c(out0$val,
                          upper_first(object_info))
          }
        if (object_se != "bootstrap") {
            if (object_info == "observed") {
                out0$field <- c(out0$field,
                                "Observed Information")
                out0$val <- c(out0$val,
                                upper_first(object_obs_info))
              }
            if (object_info %in% c("expected", "first.order") ||
                object_obs_info == "h1") {
                out0$field <- c(out0$field,
                                ifelse((object_se == "robust.huber.white") &&
                                      (object_h1_info != "h1.information.meat"),
                                      "Information bread saturated (h1) model",
                                      "Information saturated (h1) model"))
                out0$val <- c(out0$val,
                              upper_first(object_h1_info))
              }
            if (object_se == "robust.huber.white") {
                if (object_h1_info_meat != "first.order") {
                    out0$field <- c(out0$field,
                                    "Information meat")
                    out0$val <- c(out0$val,
                                  upper_first(object_h1_info_meat))
                  }
                if (object_h1_info_meat != object_h1_info) {
                    out0$field <- c(out0$field,
                                    "Information meat saturated (h1) model")
                    out0$val <- c(out0$val,
                                  upper_first(object_h1_info_meat))
                  }
              }
          }
      }
    if (length(out0$field) > 0) {
        tmp <- paste0(out0$field, ":")
      } else {
        tmp <- out0$field
      }
    out <- data.frame(Field = tmp,
                      Val = out0$val)
    colnames(out) <- c("", "")
    attr(out, "section_title") <- "Parameter Estimates Settings:"
    attr(out, "print_args") <- list(right = FALSE,
                                    row.names = FALSE)
    out
  }

#' @noRd
# Change the first letter of each word to upper case, and
# change all other letters to lower case.
# All extra white spaces will be deleted.
upper_first <- function(x) {
    x0 <- strsplit(x, " ")[[1]]
    x0 <- x0[x0 != ""]
    x1 <- sapply(x0,
                 function(xx) substring(toupper(xx), 1, 1),
                 USE.NAMES = FALSE)
    x2 <- sapply(x0,
                 function(xx) substring(tolower(xx), 2),
                 USE.NAMES = FALSE)
    paste(paste0(x1,
                 x2),
          collapse = " ")
  }

#' @noRd
# Sections for which "." will be added to residuals and
# intercepts.
section_add_dots <- function(section) {
    section %in% c("Covariances",
                   "Intercepts",
                   "Variances")
  }

#' @noRd
# Sections for which the left-hand-side, e.g.,
# latent factor or y-variables, will be moved
# to its row, indenting the right-hand side,
# as in lavaan-style.
section_insert_lhs <- function(section) {
    section %in% c("Latent Variables",
                   "Composites",
                   "Regressions",
                   "Covariances")
  }

#' @noRd
# How each section is to be identified.
# Return a logical vector to indicate
# which rows belong to a section.
section_rows <- function(section,
                         est_df,
                         group_id) {
    id <- switch(section,
                 "Latent Variables" = (est_df$op == "=~"),
                 "Composites" = (est_df$op == "<~"),
                 "Regressions" = (est_df$op == "~"),
                 "Covariances" = ((est_df$op == "~~") &
                                  (est_df$lhs != est_df$rhs)),
                 "Intercepts" = (est_df$op == "~1"),
                 "Thresholds" = (est_df$op == "|"),
                 "Variances" = ((est_df$op == "~~") &
                                (est_df$lhs == est_df$rhs)),
                 "Scales y*" = (est_df$op == "~*~"),
                 "Group Weights" = ((est_df$op == "%") &
                                    (est_df$lhs == "group")),
                 "R-Squares" = (est_df$op == "r2"),
                 "Defined Parameters" = (est_df$op == ":="),
                 "Constraints" = (est_df$op %in% c("==", "<", ">")))
    if (section %in% c("Defined Parameters", "Constraints")) {
        return(id)
      } else {
        id <- id & (est_df$group == group_id)
        return(id)
      }
  }

#' @noRd
# A wrapper to loop over sections to generate
# the table for each section.
to_tables <- function(group_id,
                      est_df,
                      group_labels,
                      sections,
                      object,
                      ...) {
    out <- sapply(sections,
                  to_tables_per_group,
                  est_df = est_df,
                  group_id = group_id,
                  group_labels = group_labels,
                  ...,
                  simplify = FALSE,
                  USE.NAMES = TRUE)
    out
  }

#' @noRd
# Generate the table for one section.
to_tables_per_group <- function(section,
                                est_df,
                                group_id = 0,
                                group_labels = NULL,
                                se_also_to_na = character(0),
                                se_not_to_na = character(0),
                                drop_cols = "std.nox",
                                rename_cols = character(0),
                                endo = character(0),
                                FUNs = list(),
                                args = list(list())) {
    i_rows <- section_rows(section,
                           est_df = est_df,
                           group_id = group_id)
    if (!any(i_rows)) {
        return(NULL)
      }
    out0 <- est_df[i_rows,
                   , drop = FALSE]
    # Always drop these columns
    out0$block <- NULL
    out0$group <- NULL
    out0$level <- NULL
    out0 <- fix_se_zero(out0,
                        also_to_na = se_also_to_na,
                        not_to_na = se_not_to_na)
    # Can use user-functions to format or add columns
    if (length(FUNs) > 0) {
        for (i in seq_along(FUNs)) {
            args0 <- utils::modifyList(list(out0),
                                       args[[i]])
            out0 <- do.call(FUNs[[i]],
                            args0)
          }
      }
    # TODO: Support for bounds to be added later
    # TODO: Handle estimates at upper or lower bounds
    out0$lower <- NULL
    out0$upper <- NULL

    if (section_add_dots(section)) {
        out0 <- add_dots(out0,
                         endo = endo)
      }
    if (section_insert_lhs(section)) {
        out0 <- insert_lhs(out0)
      }
    if (section == "Constraints") {
        out0 <- fix_constraints(out0)
      }
    if (section == "Thresholds") {
        out0$lhs <- paste0(out0$lhs, "|", out0$rhs)
      }

    out0 <- fix_labels(out0)
    if (section_add_dots(section)) {
        out0 <- add_dots(out0,
                         endo = endo)
      }
    # These columns are no longer needed after insert_lhs()
    out0$op <- NULL
    out0$rhs <- NULL
    out0$efa <- NULL
    out0$exo <- NULL
    if (length(drop_cols) != 0) {
        out0 <- out0[, !(colnames(out0) %in% drop_cols), drop = FALSE]
      }
    colnames(out0)[colnames(out0) == "lhs"] <- "Par"
    out0 <- set_col_names(out0,
                          rename_cols = rename_cols)
    out0 <- add_group_header(out0,
                             group_id = group_id,
                             group_labels = group_labels,
                             category = section)
    rownames(out0) <- NULL
    out0
  }

#' @noRd
# Print the header or footer, created by header_funs and/or footer_funs
print_header <- function(xx) {
    xx_org <- xx
    pfun <- attr(xx, "print_fun")
    ptmp2 <- attr(xx, "print_args")
    section_title <- attr(xx, "section_title")
    if (is.null(ptmp2)) {
        ptmp2 <- list()
      }
    if (is.null(pfun)) {
        pfun <- "print"
      }
    if (pfun == "cat") {
        ptmp2 <- utils::modifyList(list(sep = "\n"),
                                   ptmp2)
      }
    if (!isFALSE(attr(xx, "wrap_lines")) && is.character(xx)) {
        strwrap_args <- attr(xx, "strwrap_args")
        if (is.null(strwrap_args)) {
            strwrap_args <- list()
          }
        strwrap_args <- utils::modifyList(strwrap_args,
                                          list(x = xx))
        xx <- do.call(strwrap, strwrap_args)
      }
    ptmp1 <- list(x = xx)
    if ((pfun == "print") && is.data.frame(xx)) {
        ptmp1 <- c(ptmp1,
                    list(right = FALSE,
                        row.names = FALSE))
      }
    if (is.list(ptmp2)) {
        ptmp <- utils::modifyList(ptmp1,
                                  ptmp2)
      }
    tmp <- utils::capture.output(do.call(methods::getFunction(pfun), ptmp))
    cat("\n", section_title, "\n", sep = "")
    cat(tmp, sep = "\n")
    invisible(NULL)
  }

#' @noRd
# Drop columns using new or old names
drop_cols_by_names <- function(x,
                               original = NULL,
                               drop_cols = character(0)) {
    if (length(drop_cols) > 0) {
        y1 <- stats::na.omit(match(drop_cols, colnames(x)))
        if (!is.null(original)) {
            original_names <- sapply(original, function(yy) {
                tmp <- attr(yy, "original_name")
                return(ifelse(is.null(tmp),
                              yes = "",
                              no = tmp))
              })
            y2 <- stats::na.omit(match(drop_cols, original_names))
          }
        yy <- unique(c(y1, y2))
        if (length(yy) > 0) {
            x[yy] <- NULL
          }
      }
    x
  }


#' @noRd
# Add white spaces to the left or right
pad_white <- function(y,
                      max_width,
                      where = c("left", "right")) {
    if (missing(max_width)) {
        max_width <- max(nchar(y), 0, na.rm = TRUE)
      }
    where <- match.arg(where)
    out <- sapply(y, function(yy) {
              tmp <- max_width - nchar(yy)
              if (tmp > 0) {
                  yy <- switch(
                          where,
                          right = paste0(yy, paste(rep(" ", tmp), collapse = "")),
                          left = paste0(paste(rep(" ", tmp), collapse = ""), yy))
                }
              yy
            })
    unname(out)
  }

#' @noRd
# Check whether an object is supported by parameterEstimates_table_list()
check_parameterEstimates_table <- function(object) {
    # object must be a data frame or a lavaan object
    if (inherits(object, "lavaan")) {
        if (lavaan::inspect(object, "nlevels") > 1) {
            stop("Multilevel models are not supported.")
          }
      } else if (is.data.frame(object)) {
        if (!is.null(object$level)) {
            if (max(object$level, na.rm = TRUE) > 1) {
                stop("Multilevel models are not supported.")
              }
          }
      } else {
        stop("Only data-frame-like objects or lavaan objects supported.")
      }
    TRUE
  }

#' @noRd
# Format the constraint table
fix_constraints <- function(est_i) {
    # Adapted from lavaan:::.makeConNames()

    lhs0 <- est_i$lhs

    i <- (est_i$rhs == "0") & (est_i$op == ">")
    lhs0[i] <- paste0(est_i$lhs[i], " - 0")

    i <- (est_i$rhs == "0") & (est_i$op == "<")
    lhs0[i] <- paste0(est_i$rhs[i], " - (", est_i$lhs[i])

    i <- (est_i$rhs != "0") & (est_i$op == ">")
    lhs0[i] <- paste0(est_i$lhs[i], " - (", est_i$rhs[i], ")")

    i <- (est_i$rhs != "0") & (est_i$op == "<")
    lhs0[i] <- paste0(est_i$rhs[i], " - (", est_i$lhs[i], ")")

    i <- (est_i$rhs == "0") & (est_i$op == "==")
    lhs0[i] <- paste0(est_i$lhs[i], " - 0")

    i <- (est_i$rhs != "0") & (est_i$op == "==")
    lhs0[i] <- paste0(est_i$lhs[i], " - (", est_i$rhs[i], ")")

    est_i$lhs <- lhs0
    est_i <- est_i[i, c("lhs", "est"), drop = FALSE]
    colnames(est_i) <- c("Par", "|Slack|")
    est_i
  }

#' @noRd
# Add attributes to a section table. For printing
add_group_header <- function(x,
                             group_id = 0,
                             group_labels,
                             category) {
    if (is.null(group_labels) || (group_id == 0)) {
        tmp_header <- category
      } else {
        tmp_header <- paste0("Group ", group_id,
                             " [", group_labels[group_id], "]: ",
                             category)
      }
    attr(x, "section") <- tmp_header
    attr(x, "group_id") <- group_id
    attr(x, "group_labels") <- group_labels
    attr(x, "category") <- category
    x
  }

#' @noRd
# Add dots to residuals, intercepts, etc.
add_dots <- function(est_i,
                     endo = character(0)) {
    # With insert_lhs, rhs is in lhs
    i <- est_i$lhs %in% endo
    j <- est_i$rhs %in% endo
    if (any(i)) {
        est_i[i, "lhs"] <- paste0(".", est_i[i, "lhs"])
        if (!all(i)) {
            est_i[!i, "lhs"] <- paste0(" ", est_i[!i, "lhs"])
          }
      }
    if (any(j)) {
        est_i[j, "rhs"] <- paste0(".", est_i[j, "rhs"])
        if (!all(j)) {
            est_i[!j, "rhs"] <- paste0(" ", est_i[!j, "rhs"])
          }
      }
    est_i
  }

#' @noRd
# A wrapper to convert a column to a character column
format_other_cols <- function(x,
                              nd = 3,
                              na_str = na_str,
                              return_width = FALSE) {
    old_attributes <- attributes(x)
    out <- lapply(x,
                  FUN = format_other_col,
                  nd = nd,
                  na_str = na_str,
                  return_width = return_width)
    out <- as.data.frame(out,
                         check.names = FALSE)
    mostattributes(out) <- old_attributes
    return(out)
  }

#' @noRd
# Convert a column to a character column
format_other_col <- function(y,
                             nd = 3,
                             na_str = " ",
                             return_width = FALSE) {
    if (is.character(y) || is.numeric(y)) {
        if (return_width) {
            return(NA)
          } else {
            return(y)
          }
      }
    # If somehow the result has more than one element,
    # the first element will be used.
    # Not ideal but this should not happen in the first place.
    y <- sapply(y, function(yy) as.character(yy)[1])
    if (all(is.na(y))) {
        max_width <- nd
      } else {
        max_width <- max(nchar(y), nd, na.rm = TRUE)
      }
    na_str <- pad_white(na_str,
                        max_width = max_width,
                        where = "left")
    y[is.na(y)] <- na_str
    y <- pad_white(y,
                   max_width = max_width,
                   where = "left")
    if (return_width) {
        return(max(sapply(y, nchar), nd))
      } else {
        return(y)
      }
  }

#' @noRd
# A wrapper to format a column of strings
format_character_cols <- function(x,
                                  nd = 3,
                                  na_str = na_str,
                                  return_width = FALSE) {
    old_attributes <- attributes(x)
    out <- lapply(x,
                  FUN = format_character_col,
                  nd = nd,
                  na_str = na_str,
                  return_width = return_width)
    out <- as.data.frame(out,
                         check.names = FALSE)
    mostattributes(out) <- old_attributes
    return(out)
  }

#' @noRd
# Format a column of strings
format_character_col <- function(y,
                                 nd = 3,
                                 na_str = " ",
                                 return_width = FALSE) {
    if (!is.character(y)) {
        if (return_width) {
            return(NA)
          } else {
            return(y)
          }
      }
    if (all(is.na(y))) {
        max_width <- nd
      } else {
        max_width <- max(nchar(y), nd, na.rm = TRUE)
      }
    na_str <- pad_white(na_str,
                        max_width = max_width,
                        where = "left")
    y[is.na(y)] <- na_str
    y <- pad_white(y,
                   max_width = max_width,
                   where = "left")
    if (return_width) {
        return(max(sapply(y, nchar), nd))
      } else {
        return(y)
      }
  }

#' @noRd
# A wrapper to format a column of numbers
format_numeric_cols <- function(x,
                                nd = 3,
                                na_str = na_str,
                                return_width = FALSE) {
    old_attributes <- attributes(x)
    out <- lapply(x,
                  FUN = format_numeric_col,
                  nd = nd,
                  na_str = na_str,
                  return_width = return_width)
    out <- as.data.frame(out,
                         check.names = FALSE)
    mostattributes(out) <- old_attributes
    return(out)
  }

#' @noRd
# Format a column of numbers
format_numeric_col <- function(y,
                               nd = 3,
                               na_str = " ",
                               return_width = FALSE) {
    if (!is.numeric(y)) {
        if (return_width) {
            return(NA)
          } else {
            return(y)
          }
      }
    # NA can be handled by formatC()
    y <- formatC(y, digits = nd, format = "f")
    if (all(is.na(y))) {
        max_width <- nd
      } else {
        max_width <- max(nchar(y), nd, na.rm = TRUE)
      }
    na_str <- pad_white(na_str,
                        max_width = max_width,
                        where = "left")
    i <- (trimws(y) == "NA")
    y[i] <- na_str
    if (return_width) {
        return(max(sapply(y, nchar), nd))
      } else {
        return(y)
      }
  }

#' @noRd
# If SE is zero or NA, set some other cells to NA
# also_to_na: Columns also set to NA, in addition to the default columns.
# not_to_na: Columns not set to NA. Override also_to_na
fix_se_zero <- function(est_i,
                        also_to_na = character(0),
                        not_to_na = character(0)) {
    if (is.null(est_i$se)) {
        return(est_i)
      }
    out <- est_i
    tmp_fixed <- (abs(out$se) < .Machine$double.eps) | is.na(out$se)
    if (length(which(tmp_fixed)) == 0) {
        return(est_i)
      }
    out$se[tmp_fixed] <- NA
    tmp0 <- c("z",
              "pvalue",
              "t",
              "df",
              "ci.lower",
              "ci.upper")
    tmp <- unique(c(tmp0, also_to_na))
    tmp <- setdiff(tmp0, not_to_na)
    if  (length(tmp) == 0) {
        return(est_i)
      }
    cnames <- colnames(out)
    for (x in tmp) {
        if (x %in% cnames) {
            out[tmp_fixed, x] <- NA
          }
      }
    out
  }

#' @noRd
# Append label to a parameter name
fix_labels <- function(est_i) {
    has_label <- which(est_i$label != "")
    if (length(has_label) > 0) {
        est_i$lhs[has_label] <- paste0(est_i$lhs[has_label],
                                       " (",
                                       est_i$label[has_label],
                                       ")")
      }
    est_i$label <- NULL
    est_i
  }

#' @noRd
# Group rows by left-hand side.
insert_lhs <- function(est_i) {
    has_efa <- !is.null(est_i$efa)
    if (has_efa) {
        lhs0 <- unique(est_i$lhs)
        efa0 <- unique(est_i$efa)
        est_i$tmp0 <- match(est_i$lhs, lhs0)
        est_i$tmp0b <- match(est_i$efa, efa0)
        est_i$tmp1 <- seq_len(nrow(est_i))
        # R > 4.4.0
        est_i <- base::sort_by(est_i, ~ tmp0b + tmp0 + tmp1)
      } else {
        lhs0 <- unique(est_i$lhs)
        est_i$tmp0 <- match(est_i$lhs, lhs0)
        est_i$tmp1 <- seq_len(nrow(est_i))
        # R > 4.4.0
        est_i <- base::sort_by(est_i, ~ tmp0 + tmp1)
      }
    est_i1 <- split(est_i, est_i$tmp0)
    est_i2 <- lapply(est_i1, function(xx) {
                xxrhs <- xx$lhs[1]
                xxop <- xx$op[1]
                xxefa <- xx$efa[1]
                xx$lhs <- paste0(" ", xx$rhs)
                xx$op <- NULL
                xx$rhs <- NULL
                xx0 <- xx[1, ]
                xx0[] <- NA
                if (has_efa) {
                    xx0$lhs <- paste(xxrhs, xxop, xxefa)
                  } else {
                    xx0$lhs <- paste(xxrhs, xxop)
                  }
                rbind(xx0, xx)
              })
    out <- do.call(rbind, est_i2)
    out$tmp0 <- NULL
    out$tmp0b <- NULL
    out$tmp1 <- NULL
    out$efa <- NULL
    rownames(out) <- NULL
    out
  }

#' @noRd
# Rename column names.
# Also store the original name as the attribute of each column
set_col_names <- function(x,
                          rename_cols = character(0)) {
    rename_cols_0 <- c("lhs" = "Par",
                       "est" = "Estimate",
                       "se" = "S.E.",
                       "z" = "Z",
                       "pvalue" = "P(>|z|)",
                       "ci.lower" = "CI.Lo",
                       "ci.upper" = "CI.Up",
                       "std.all" = "Std.all",
                       "std.lv" = "Std.lv",
                       "step" = "Step",
                       "std.nox" = "Std.nox",
                       "prior" = "Prior",
                       "fmi" = "FMI",
                       "t" = "t-value",
                       "riv" = "RIV")
    if (length(rename_cols) > 0) {
        rename_cols_0 <- rename_cols_0[!(names(rename_cols_0) %in% names(rename_cols))]
        rename_cols_0 <- c(rename_cols_0, rename_cols)
      }
    # Store original names
    cnames_old <- colnames(x)
    for (xx in cnames_old) {
        attr(x[[xx]], "original_name") <- xx
      }
    if (length(rename_cols_0) > 0) {
        for (i in seq_along(rename_cols_0)) {
            j <- which(cnames_old == names(rename_cols_0)[i])
            if (length(j) > 0) {
                cnames_old[j] <- rename_cols_0[i]
              }
          }
      }
    colnames(x) <- cnames_old
    x
  }
