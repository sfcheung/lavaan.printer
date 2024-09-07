#' @param x The object to be printed.
#' Should be the output of
#' [parameterEstimates_table_list()].
#'
#' @param nd The number of decimal
#' places to be displayed for numeric
#' cells.
#'
#' @param by_group If `TRUE`, the
#' default, tables will be grouped by
#' groups first, and then by grouped by
#' sections (e.g., `Latent Variables`,
#' `Regressions`, etc.). If `FALSE`,
#' then the tables will be grouped by
#' sections first, and then grouped by
#' groups. No effect if the number of
#' groups is equal to one.
#'
#' @param drop_cols The names of columns
#' to be dropped from the printout. It
#' can be the names after being renamed
#' by `rename_cols`, or the original
#' names before being renamed (i.e., the
#' names in `object`), provided that the
#' names in `object` are stored in the
#' attribute `"original_name"` of each
#' column, which is done by default by
#' [parameterEstimates_table_list()].
#'
#' @param na_str The string to be used
#' for cells with `NA`. Default is
#' `" "`, a white space.
#'
#' @return
#' ## [print_parameterEstimates_table_list()]
#'
#' The original input, `x`, is returned
#' invisibly. Called for its side
#' effect to print the content of
#' `x`.
#'
#' @rdname parameterEstimates_table_list
#'
#' @export
# Print the output of parameterEstimates_table_list()
# drop_cols: Support using the new names or the original names.
print_parameterEstimates_table_list <- function(x,
                                                nd = 3,
                                                by_group = TRUE,
                                                drop_cols = character(0),
                                                na_str = " ") {

    out_header <- x$header
    out_group <- x$group
    out_model <- x$model
    out_footer <- x$footer

    # Print headers
    if (length(out_header) > 0) {
        for (xx in out_header) {
            if (isTRUE(nrow(xx) == 0) || is.null(xx)) next
            print_header(xx)
          }
      }

    # Order tables based on by_group
    ngroups <- length(out_group)
    if (by_group) {
        out0 <- unlist(out_group,
                       recursive = FALSE)
      } else {
        group_sections <- unique(as.vector(sapply(out_group, names)))
        out0 <- sapply(group_sections, function(xx) {
                      lapply(out_group, `[[`, xx)
                    },
                  simplify = FALSE)
        out0 <- unlist(out0,
                       recursive = FALSE)
      }

    # Handle constraints separately
    out1 <- c(out0, out_model["Defined Parameters"])
    out1 <- out1[!sapply(out1, is.null)]

    # Format all columns first

    xx_original_names <- out1[[1]]
    for (xx in seq_along(out1)) {
        yy <- out1[[xx]]
        yy$Par <- pad_white(yy$Par,
                            where = "right")
        yy <- format_numeric_cols(yy,
                                  nd = nd,
                                  na_str = na_str)
        yy <- format_character_cols(yy,
                                    nd = nd,
                                    na_str = na_str)
        yy <- format_other_cols(yy,
                                nd = nd,
                                na_str = na_str)
        out1[[xx]] <- yy
      }
    # Find the maximum column width of each column
    tmp1 <- do.call(rbind, out1)
    num_max_width <- apply(tmp1,
                           MARGIN = 2,
                           FUN = function(xx) max(nchar(xx), nd, na.rm = TRUE))

    # Print each section
    for (xx in out1) {
        xx_section <- attr(xx, "section")
        xx_category <- attr(xx, "category")
        xx_group_id <- attr(xx, "group_id")

        # Create and print the section title
        if ((ngroups > 1) && xx_group_id != 0) {
            xx_group_labels <- attr(xx, "group_labels")
            xx_group_label <- xx_group_labels[xx_group_id]
            if (by_group) {attr(xx, "category")
                xx_section <- paste0("Group ",
                                     xx_group_id,
                                     " [", xx_group_label, "]:\n",
                                     xx_category, ":")
              } else {
                xx_section <- paste0(xx_category,
                                     ":\n",
                                     "Group ",
                                     xx_group_id,
                                     " [", xx_group_label, "]:")
              }
          } else {
            xx_section <- paste0(xx_category, ":")
          }
        cat("\n", xx_section, "\n", sep = "")

        xx$Par <- pad_white(xx$Par,
                            max_width = num_max_width["Par"],
                            where = "right")
        xx1 <- xx
        xx1 <- drop_cols_by_names(xx1,
                                  original = xx_original_names,
                                  drop_cols = drop_cols)
        # Standardize the width of each column across sections
        for (yy in colnames(xx1)[-1]) {
            if (yy %in% names(num_max_width)) {
                xx1[, yy] <- pad_white(xx1[, yy],
                                       max_width = num_max_width[yy],
                                       where = "left")
              }
          }
        colnames(xx1)[colnames(xx1) == "Par"] <- ""
        # Prevent overriding the printing of a data frame
        tmp <- utils::capture.output(print(xx1,
                                    row.names = FALSE))
        cat(tmp, sep = "\n")
      }

    # Print constraints
    out_con <- out_model$Constraints
    if (!is.null(out_con)) {
        xx1 <- format_numeric_cols(out_con,
                                   nd = nd,
                                   na_str = na_str)
        xx1 <- format_character_cols(xx1,
                                     nd = nd,
                                     na_str = na_str)
        xx1 <- format_other_cols(xx1,
                                 nd = nd,
                                 na_str = na_str)
        xx1$Par <- pad_white(xx1$Par,
                             max_width = num_max_width["Par"],
                             where = "right")
        colnames(xx1)[colnames(xx1) == "Par"] <- ""
        tmp <- utils::capture.output(print(xx1,
                                    row.names = FALSE))
        cat("\n", "Constraints:", "\n", sep = "")
        cat(tmp, sep = "\n")
      }

    # Print footers
    if (length(out_footer) > 0) {
        for (xx in out_footer) {
            if (isTRUE(nrow(xx) == 0) || is.null(xx)) next
            print_header(xx)
          }
      }

    invisible(x)
  }
