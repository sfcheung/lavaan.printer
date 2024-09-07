# Workflow

# For non-lavaan functions:
# - Always add header information such that
#   there is no need to get the original lavaan object.
# - parameterEstimates_table_list()
#   - Work in these modes:
#       - Accept a parameter estimates table with header information.
#       - Accept a lavaan object and create the parameter estimates table with header information.
#       - Accept a parameter estimates table AND
#         the original lavaan object. This is for backward compatibility.
#   - Convert a parameter estimates table to a list of tables (data frames),
#     plus a list of header(s) and a list of footer(s).
#   - Use case:
#       - To be used in print methods.
#       - For customizing the printout.
# - print_parameterEstimates_table_list()
#   - A function to print the table list. Customizable.
#   - Responsible for formatting numbers and characters.
#   - It assumes everything to be printed is already available.
#   - Should not need anything other than the object to be printed.

#' @title Convert an Estimates Table to
#' a List of Data Frames
#'
#' @description Create a list of data
#' frames from the output of
#' [lavaan::parameterEstimates()],
#' formatted in nearly the same way the
#' argument `output = "text"` does.
#'
#' @details This function creates an
#' output mimicking the output format
#' when [lavaan::parameterEstimates()]
#' is called with `output` set to
#' `"text"`. It only creates the output
#' as a list of data frames, grouped in
#' sections like `Latent Variables` and
#' `Regression`, as in the printout of
#' [lavaan::parameterEstimates()]. It
#' does not format the content. The
#' actual printing is to be done by
#' [print_parameterEstimates_table_list()],
#' which will format the cells before
#' printing them.
#'
#' This function is not intended to be
#' used by end users. It is intended
#' to be used inside other functions,
#' such as a print method. Functions
#' that add columns to the parameter
#' estimates table of a `lavaan` object
#' can use it and
#' [PRINT_parameterEstimates_table_list()]
#' to print the output in the
#' `lavaan`-style, but with columns
#' modified as needed and with additional
#' header and/or footer sections added.
#'
#' Therefore, it was developed with
#' flexibility in mind, at the expense
#' of user-friendliness.
#'
#' ## Header and Footer Functions
#'
#' If a list of functions
#' is supplied to `header_funs` or
#' `footer_funs`, they will be used
#' to generate the headers and/or
#' footers. The input is `object`, or
#' the parameter estimates table
#' generated from object. The output
#' should be one of the following
#' formats.
#'
#' It can be a data frame with two
#' optional attributes: `section_title`
#' and `print_args`. If `section_title`
#' is not null, it will be printed by
#' [cat()] before printing tbe section.
#' The header or
#' footer section will then be printed by
#' [print()]. If `print_args` is set
#' to be a list of named arguments, then
#' they will be used when calling
#' [print()]. For example, setting
#' `print_args` to
#' `list(right = FALSE, row.names = FALSE)`
#' will print the data frame with these
#' arguments.
#'
#' It can also be any other object that
#' can be printed. One possible case is
#' a character vector of footnotes. In
#' this case, we can add this
#' attribute `print_fun` and set it to
#' `"cat"`, the name of the function
#' to be used to print the section,
#' and add the attribute `print_args`
#' and set it to `list(sep = "\n")`.
#' They will then be printed by
#' `cat(x, sep = "\n")`, `x`` being the
#' character vector.
#'
#' These two arguments allows users to
#' add header and footer sections and
#' print them in the desired format.
#'
#' @return
#' ## [parameterEstimates_table_list()]
#'
#' A list of data frames of the
#' class `parameterEstimates_table_list`,
#' with this
#' structure.
#'
#' - `group`: A list of data frames for
#'  each group. It is a list of length
#'  equal to one if the model has only
#'  one group. For each group, the
#'  content is a list of data frames,
#'  one for each section of the estimates.
#'
#'  - `model`: A list of tables for
#'  sections such as user-defined
#'  parameters (`"Defined Parameters"`)
#'  or model constraints (`"Constraints"`).
#'
#'  - `header`: A list of header sections.
#'
#'  - `footer`: A list of footer sections.
#'
#' @param object It can a data frame
#' similar in form to the output of
#' [lavaan::parameterEstimates()], or a
#' `lavaan` object (e.g., the output of
#' [lavaan::sem()]). If it is a `lavaan`
#' object, then
#' [lavaan::parameterEstimates()] will
#' be called to generate the parameter
#' estimates table.
#'
#' @param ... If `object` is a `lavaan`
#' object, then these are the optional
#' arguments to be passed to
#' [lavaan::parameterEstimates()] when
#' it is called.
#'
#' @param fit_object (Optional). The
#' `lavaan` object for getting
#' additional information, if they are
#' not available in `object`, and added
#' as attributes to `object`. It
#' essentially does what
#' [lavaan::parameterEstimates()] does
#' when setting `output` to `"text"`.
#'
#' @param se_also_to_na Columns for
#' which cells will be set to `NA` if
#' the standard error of a parameter is
#' zero, which is assumed to mean that
#' this parameter is fixed. By default,
#' these columns are included and no
#' need to specify them for this
#' argument: `"z"`, `"pvalue"`, `"t"`,
#' `"df"`, `"ci.lower"`, and
#' `"ci.upper"`. To exclude one of these
#' columns from `se_als_to_na`, add it
#' to `se_not_to_na`.
#'
#' @param se_not_to_na Columns for which
#' cells will *not* be set to `NA` even if
#' the standard error of a parameter is
#' zero. Column names that appear here
#' *override* `se_also_to_na`.
#' Therefore, if `"z"`, `"pvalue"`,
#' `"t"`, `"df"`, `"ci.lower"`, and
#' `"ci.upper"` are included in this
#' argument, they will also not be set
#' to `NA`.
#'
#' @param drop_cols The names of columns to
#' be dropped.
#'
#' @param rename_cols If any columns are
#' to be renamed, this is named
#' character vector, with the names
#' being the original names and the
#' values being the new names. For
#' example, `c("pvalue" = "P(|>z|)")`
#' renames the column `"pvalue"` to
#' `"P(|z|)"`. It is recommended to
#' quote the names too because they may
#' not be standard names.
#'
#' @param est_funs If supplied, it
#' should be a list of functions to be
#' applied to each parameter estimates
#' table, applied in the same order they
#' appear in the list. It can be used
#' create new columns or modify existing
#' columns. Usually, this should be done
#' *before* calling
#' [parameterEstimates_table_list()] but
#' provided as an option.
#'
#' @param header_funs If supplied, it
#' should be a list of functions to be
#' applied to `object` to generate the
#' header sections. See `Details` on the
#' expected format of the output of
#' these functions.
#'
#' @param footer_funs If supplied, it
#' should be a list of functions to be
#' applied to `object` to generate the
#' footer sections. See `Details` on the
#' expected format of the output of
#' these functions.
#'
#' @param est_funs_args If supplied, it
#' must be a "list of list(s)". The length
#' of this list must be equal to the
#' number of functions in `est_funs`.
#' Each sub-list is the list of arguments
#' to be used when calling a function in
#' `est_funs`. It must be an empty
#' `list()` if no additional arguments
#' are to be used when calling a function
#' in `est_funs`.
#'
#' @param header_funs_args If supplied, it
#' must be a "list of list(s)". The length
#' of this list must be equal to the
#' number of functions in `header_funs`.
#' Each sub-list is the list of arguments
#' to be used when calling a function in
#' `header_funs`. It must be an empty
#' `list()` if no additional arguments
#' are to be used when calling a function
#' in `header_funs`.
#'
#' @param footer_funs_args If supplied, it
#' must be a "list of list(s)". The length
#' of this list must be equal to the
#' number of functions in `footer_funs`.
#' Each sub-list is the list of arguments
#' to be used when calling a function in
#' `footer_funs`. It must be an empty
#' `list()` if no additional arguments
#' are to be used when calling a function
#' in `footer_funs`.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @seealso [print_parameterEstimates_table_list()]
#' for the printing function, and
#' [lavaan::parameterEstimates()] for
#' generating the parameter estimates
#' table.
#'
#' @examples
#'
#' # Adapted from the help of lavaan::cfa()
#' library(lavaan)
#' mod <- "
#' visual  =~ x1 + x2 + x3
#' textual =~ x4 + x5 + x6
#' speed   =~ x7 + x8 + x9
#' "
#' fit <- cfa(mod,
#'            data = HolzingerSwineford1939)
#' est <- parameterEstimates_table_list(fit,
#'                                      rename_cols = c("P(>|z|)" = "pvalue",
#'                                                      "S.E." = "SE"))
#' print_parameterEstimates_table_list(est,
#'                                     drop = "Z")
#' fit2 <- cfa(mod,
#'             data = HolzingerSwineford1939,
#'             group = "school")
#' est2 <- parameterEstimates_table_list(fit2)
#' # The tables in the same group are printed together (default)
#' print_parameterEstimates_table_list(est2,
#'                                     by_group = TRUE)
#' # The table are grouped by section then by group
#' print_parameterEstimates_table_list(est2,
#'                                     by_group = FALSE)

#' @export

parameterEstimates_table_list <- function(object,
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
                                          footer_funs_args = NULL) {
    if (!is.list(est_funs)) {
        # Assume one single function
        est_funs <- list(est_funs)
      }
    if (!is.list(header_funs)) {
        # Assume one single function
        header_funs <- list(header_funs)
      }
    if (!is.list(footer_funs)) {
        # Assume one single function
        footer_funs <- list(footer_funs)
      }

    if (is.null(est_funs_args)) {
        est_funs_args <- replicate(length(est_funs),
                                   list())
      }

    if (is.null(header_funs_args)) {
        header_funs_args <- replicate(length(header_funs),
                                   list())
      }

    if (is.null(footer_funs_args)) {
        footer_funs_args <- replicate(length(footer_funs),
                                   list())
      }

    if (length(est_funs_args) != length(est_funs)) {
        stop("The length of est_funs_args does not match the length of est_funs.")
      }
    if (!all(sapply(est_funs_args, is.list))) {
        stop("est_funs_args must be a 'list of list()'")
      }

    if (length(header_funs_args) != length(header_funs)) {
        stop("The length of header_funs_args does not match the length of header_funs.")
      }
    if (!all(sapply(header_funs_args, is.list))) {
        stop("header_funs_args must be a 'list of list()'")
      }

    if (length(footer_funs_args) != length(footer_funs)) {
        stop("The length of footer_funs_args does not match the length of footer_funs.")
      }
    if (!all(sapply(footer_funs_args, is.list))) {
        stop("footer_funs_args must be a 'list of list()'")
      }


    if (is.data.frame(object)) {
        # Assume it is a parameter estimates table.
        # For a table not generated by lavaan::parameterEstimates(),
        # that function should include all necessary attributes.
        if (!is.null(fit_object)) {
            # Add header information
            est <- add_header_attributes(object,
                                         fit_object)
          } else {
            est <- object
          }
        est_df <- as.data.frame(object)
      } else if (inherits(object, "lavaan")) {
        # If the object is a lavaan object,
        # then it can only use attributes available from lavaan.
        args0 <- list(...)
        args <- utils::modifyList(args0,
                                  list(header = TRUE,
                                      output = "text"))
        est <- do.call(lavaan::parameterEstimates,
                      c(list(object = object),
                        args))
        est_df <- as.data.frame(est)
      }

    # For add_dots
    # Use the undocumented trick of using lavNames on
    # the estimates table.
    endo <- unique(c(lavaan::lavNames(est_df, "eqs.y"),
                     lavaan::lavNames(est_df, "ov.ind"),
                     lavaan::lavNames(est_df, "lv.ind")))

    # ngroups
    if (is.null(est_df$group)) {
        group_ids <- 1
        ngroups <- 1
        est_df$group <- 1
        group_labels <- character(0)
      } else {
        group_ids <- groups_from_est(est_df)
        ngroups <- length(group_ids)
        group_labels <- group_labels_from_est(est_df)
      }

    # nlevels
    if (is.null(est_df$levels)) {
        est_df$level <- 1
        nlevels <- 1
      } else {
        nlevels <- length(levels_from_est(est_df))
      }

    # nblocks
    if (is.null(est_df$block)) {
        est_df$block <- 1
      }

    # Adapted from lavaan
    group_sections <- c(
        "Latent Variables",
        "Composites",
        "Regressions",
        "Covariances",
        "Intercepts",
        "Thresholds",
        "Variances",
        "Scales y*",
        "Group Weights",
        "R-Squares"
      )

    model_sections <- c(
        "Defined Parameters",
        "Constraints"
      )

    # Loop over sections
    out_group <- lapply(seq_len(ngroups),
                        to_tables,
                        est_df = est_df,
                        group_labels = group_labels,
                        sections = group_sections,
                        se_also_to_na = se_also_to_na,
                        se_not_to_na = se_not_to_na,
                        drop_cols = drop_cols,
                        rename_cols = rename_cols,
                        endo = endo,
                        FUNs = est_funs,
                        args = est_funs_args)
    if (ngroups > 1) {
        names(out_group) <- group_labels
      }

    out_model <- sapply(model_sections,
                        to_tables_per_group,
                        est_df = est_df,
                        group_id = 0,
                        se_also_to_na = se_also_to_na,
                        se_not_to_na = se_not_to_na,
                        drop_cols = drop_cols,
                        rename_cols = rename_cols,
                        endo = endo,
                        FUNs = est_funs,
                        simplify = FALSE)

    out_header <- add_header(est,
                             FUNs = c(header_funs,
                                      list(add_header_lavaan)),
                             args = c(header_funs_args,
                                      list(list())))

    out_footer <- add_header(est,
                             FUNs = footer_funs,
                             args = footer_funs_args)

    out <- list(group = out_group,
                model = out_model,
                header = out_header,
                footer = out_footer)
    class(out) <- "parameterEstimates_table_list"
    out
  }

