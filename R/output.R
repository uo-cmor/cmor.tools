# #' Render Rmd manuscript to docx
# #'
# #' This function renders an Rmarkdown report to Word .docx file, in the
# #'     appropriate directory and with reference, csl, and bib files as
# #'     specified.
# #'
# #' @param rmd Path to the Rmarkdown file (relative to project root)
# #' @param out Path to the output docx file to be created
# #' @param reference_docx Path to the reference docx (styles) file
# #' @param csl Path to the CSL styles file
# #' @param bib Path the the BibTex bibliography file
# #'
# #' @export
# render_manuscript <- function(rmd = NULL, out = NULL,
# 															reference_docx = NULL, csl = NULL, bib = NULL) {
# 	checkmate::assert_string(rmd, null.ok = TRUE)
# 	checkmate::assert_string(out, null.ok = TRUE)
# 	checkmate::assert_string(reference_docx, null.ok = TRUE)
# 	checkmate::assert_string(csl, null.ok = TRUE)
# 	checkmate::assert_string(bib, null.ok = TRUE)
#
# 	if (is.null(rmd)) rmd <- "reports/manuscript.Rmd"
# 	if (is.null(out)) out <- paste0("output/", basename(rmd))
# 	if (is.null(reference_docx)) reference_docx <- "reports/word-styles-reference-01.docx"
# 	if (is.null(csl)) csl <- "reports/vancouver.csl"
# 	if (is.null(bib)) bib <- "reports/references.bib"
#
# 	checkmate::assert_file_exists(rmd)
# 	checkmate::assert_path_for_output(out, overwrite = TRUE)
# 	checkmate::assert_file_exists(reference_docx)
# 	checkmate::assert_file_exists(csl)
# 	checkmate::assert_file_exists(bib)
#
# 	path <- rmarkdown::render(
# 		rmd,
# 		rmarkdown::word_document(
# 			reference_docx = usethis::proj_path(reference_docx),
# 			pandoc_args = list("--csl", usethis::proj_path(csl), "--bibliography", usethis::proj_path(bib))
# 		),
# 		output_file = out, output_dir = dirname(out), knit_root_dir = usethis::proj_path()
# 	)
#
# 	fs::path_rel(path)
# }

###################################
### Output formatting functions ###
###################################

#' Results formatting for tables and reports
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' These functions are deprecated in favour of the functions in
#'     \code{\link[formattr]{output-formatting}}.
#'
#' These functions are simple wrappers around `paste()` and `number()` to
#'     format results for printing.
#'
#' @param mean,sd,n,proportion,est,low,high Numeric vectors
#' @param ... Passed to `number()`
#' @param .sep Separator between lower and upper bounds of the CI; default is
#'     \code{' to '}.
#' @param .glue Logical. Should the ouput be \code{'glue'} vector
#'     (default=\code{TRUE}) or a regular character vector?
#'
#' @name output-formatting
NULL

#' @rdname output-formatting
#'
#' @export
mean_sd <- function(mean, sd, ..., .glue = TRUE) {
	lifecycle::deprecate_warn("0.5.0", "mean_sd()", "formattr::mean_sd()")

	checkmate::assert_numeric(mean)
	checkmate::assert_numeric(sd)
	checkmate::assert_flag(.glue)

	if (.glue) glue::glue("{formattr::nmbr(mean, ...)} ({formattr::nmbr(sd, ...)})")
	else paste0(formattr::nmbr(mean, ...), ' (', formattr::nmbr(sd, ...), ')')
}

#' @rdname output-formatting
#'
#' @export
n_percent <- function(n, proportion, ..., .glue = TRUE) {
	lifecycle::deprecate_warn("0.5.0", "n_percent()", "formattr::n_percent()")

	checkmate::assert_numeric(n)
	checkmate::assert_numeric(proportion)
	checkmate::assert_flag(.glue)

	if (.glue) glue::glue("{formattr::nmbr(n, accuracy = 1)} ",
												"({formattr::nmbr(proportion, scale = 100, suffix = '%', ...)})")
	else paste0(formattr::nmbr(n, accuracy = 1), ' (',
							formattr::nmbr(proportion, scale = 100, suffix = '%', ...), ')')
}

#' @rdname output-formatting
#'
#' @export
est_ci <- function(est, low, high, ..., .sep = ' to ', .glue = TRUE) {
	lifecycle::deprecate_warn("0.5.0", "est_ci()", "formattr::est_ci()")

	checkmate::assert_numeric(est)
	checkmate::assert_numeric(low)
	checkmate::assert_numeric(high)
	checkmate::assert_string(.sep)
	checkmate::assert_flag(.glue)

	if (.glue) glue::glue("{formattr::nmbr(est, ...)} ",
												"({formattr::nmbr(low, ...)}{.sep}{formattr::nmbr(high, ...)})")
	else paste0(formattr::nmbr(est, ...),
							' (', formattr::nmbr(low, ...), .sep, formattr::nmbr(high, ...), ')')
}

###################################################################
### Number formatting functions (based on scales::number() etc) ###
###################################################################

#' Format numbers using `scales::number()`-type functions
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' These functions are deprecated in favour of \code{\link[formattr]{nmbr}}.
#'
#' These functions replace/extend the `scales::number()-type` formatting
#'     functions.
#'
#' @param x Numeric vector to format
#' @param accuracy,scale,prefix,suffix,big.mark,decimal.mark,trim,... As in
#'     \code{scales::number()}
#' @param html Logical scalar. Whether to include formatting marks (minus
#'     signs) as HTML strings (the default) or unicode.
#'
#' @name number-formatting
NULL

#' @rdname number-formatting
#' @export
number <- function(x, accuracy = 1, scale = 1, prefix = "", suffix = "",
									 big.mark = "< >", decimal.mark = ".", trim = TRUE, html = TRUE, ...) {
	lifecycle::deprecate_warn("0.5.0", "number()", "formattr::nmbr()")

	checkmate::assert_numeric(x)
	checkmate::assert_number(accuracy)
	checkmate::assert_number(scale)
	checkmate::assert_string(prefix)
	checkmate::assert_string(suffix)
	checkmate::assert_string(big.mark)
	checkmate::assert_string(decimal.mark)
	checkmate::assert_flag(trim)
	checkmate::assert_flag(html)

	minus <- if (html) "&minus;" else "\u2212"
	neg <- rep("", length(x))
	neg[x < 0] <- minus

	narrow_space <- if (html) "&#x202F;" else "\u202F"

	stringi::stri_replace_all_regex(
		paste0(
			neg,
			scales::number(abs(x), accuracy = accuracy, scale = scale, prefix = prefix, suffix = suffix,
										 big.mark = big.mark, decimal.mark = decimal.mark, trim = trim, ...)
		),
		"< >", narrow_space
	)
}

#' @rdname number-formatting
#' @export
number_format <- function(accuracy = 1, scale = 1, prefix = "", suffix = "",
													big.mark = "< >", decimal.mark = ".", trim = TRUE, html = TRUE, ...) {
	lifecycle::deprecate_warn("0.5.0", "number_format()", "formattr::nmbr()")

	checkmate::assert_number(accuracy)
	checkmate::assert_number(scale)
	checkmate::assert_string(prefix)
	checkmate::assert_string(suffix)
	checkmate::assert_string(big.mark)
	checkmate::assert_string(decimal.mark)
	checkmate::assert_flag(trim)
	checkmate::assert_flag(html)

	list(...)

	function(x) number(x, accuracy = accuracy, scale = scale,
										 prefix = prefix, suffix = suffix, big.mark = big.mark,
										 decimal.mark = decimal.mark, trim = trim, html = html, ...)
}

#' @rdname number-formatting
#' @export
percent <- function(x, accuracy = 1, scale = 100, prefix = "", suffix = "%",
										big.mark = "< >", decimal.mark = ".", trim = TRUE, html = TRUE, ...) {
	lifecycle::deprecate_warn("0.5.0", "percent()", "formattr::prct()")

	checkmate::assert_numeric(x)
	checkmate::assert_number(accuracy)
	checkmate::assert_number(scale)
	checkmate::assert_string(prefix)
	checkmate::assert_string(suffix)
	checkmate::assert_string(big.mark)
	checkmate::assert_string(decimal.mark)
	checkmate::assert_flag(trim)
	checkmate::assert_flag(html)

	minus <- if (html) "&minus;" else "\u2212"
	neg <- rep("", length(x))
	neg[x < 0] <- minus

	narrow_space <- if (html) "&#x202F;" else "\u202F"

	stringi::stri_replace_all_regex(
		paste0(
			neg,
			scales::number(abs(x), accuracy = accuracy, scale = scale, prefix = prefix, suffix = suffix,
										 big.mark = big.mark, decimal.mark = decimal.mark, trim = trim, ...)
		),
		"< >", narrow_space
	)
}

#' @rdname number-formatting
#' @export
percent_format <- function(accuracy = 1, scale = 100, prefix = "", suffix = "%",
													 big.mark = "< >", decimal.mark = ".", trim = TRUE, html = TRUE, ...) {
	lifecycle::deprecate_warn("0.5.0", "percent_format()", "formattr::prct()")

	checkmate::assert_number(accuracy)
	checkmate::assert_number(scale)
	checkmate::assert_string(prefix)
	checkmate::assert_string(suffix)
	checkmate::assert_string(big.mark)
	checkmate::assert_string(decimal.mark)
	checkmate::assert_flag(trim)
	checkmate::assert_flag(html)

	list(...)

	function(x) percent(x, accuracy = accuracy, scale = scale,
											prefix = prefix, suffix = suffix, big.mark = big.mark,
											decimal.mark = decimal.mark, trim = trim, html = html, ...)
}

#' @rdname number-formatting
#' @export
comma <- function(x, accuracy = 1, scale = 1, prefix = "", suffix = "",
									big.mark = ",", decimal.mark = ".", trim = TRUE, html = TRUE, ...) {
	lifecycle::deprecate_warn("0.5.0", "comma()", "formattr::cmma()")

	checkmate::assert_numeric(x)
	checkmate::assert_number(accuracy)
	checkmate::assert_number(scale)
	checkmate::assert_string(prefix)
	checkmate::assert_string(suffix)
	checkmate::assert_string(big.mark)
	checkmate::assert_string(decimal.mark)
	checkmate::assert_flag(trim)
	checkmate::assert_flag(html)

	minus <- if (html) "&minus;" else "\u2212"
	neg <- rep("", length(x))
	neg[x < 0] <- minus

	paste0(
		neg,
		scales::number(abs(x), accuracy = accuracy, scale = scale, prefix = prefix, suffix = suffix,
									 big.mark = big.mark, decimal.mark = decimal.mark, trim = trim, ...)
	)
}

#' @rdname number-formatting
#' @export
comma_format <- function(accuracy = 1, scale = 1, prefix = "", suffix = "",
												 big.mark = ",", decimal.mark = ".", trim = TRUE, html = TRUE, ...) {
	lifecycle::deprecate_warn("0.5.0", "comma_format()", "formattr::cmma()")

	checkmate::assert_number(accuracy)
	checkmate::assert_number(scale)
	checkmate::assert_string(prefix)
	checkmate::assert_string(suffix)
	checkmate::assert_string(big.mark)
	checkmate::assert_string(decimal.mark)
	checkmate::assert_flag(trim)
	checkmate::assert_flag(html)

	list(...)

	function(x) comma(x, accuracy = accuracy, scale = scale,
										prefix = prefix, suffix = suffix, big.mark = big.mark,
										decimal.mark = decimal.mark, trim = trim, html = html, ...)
}
