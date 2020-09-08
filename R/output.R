#' Render Rmd manuscript to docx
#'
#' This function renders an Rmarkdown report to Word .docx file, in the
#'     appropriate directory and with reference, csl, and bib files as
#'     specified.
#'
#' @param rmd Path to the Rmarkdown file (relative to project root)
#' @param out Path to the output docx file to be created
#' @param reference_docx Path to the reference docx (styles) file
#' @param csl Path to the CSL styles file
#' @param bib Path the the BibTex bibliography file
#'
#' @export
render_manuscript <- function(rmd = NULL, out = NULL, reference_docx = NULL, csl = NULL, bib = NULL) {
	if (is_package()) prefix <- "inst/" else prefix <- ""
	if (is.null(rmd)) rmd <- paste0(prefix, "reports/manuscript.Rmd")
	if (is.null(out)) out <- paste0("output/", basename(rmd))
	if (is.null(reference_docx)) reference_docx <- paste0(prefix, "reports/word-styles-reference-01.docx")
	if (is.null(csl)) csl <- paste0(prefix, "reports/vancouver.csl")
	if (is.null(bib)) bib <- paste0(prefix, "reports/references.bib")

	rmarkdown::render(
		rmd,
		rmarkdown::word_document(
			reference_docx = usethis::proj_path(reference_docx),
			pandoc_args = list("--csl", usethis::proj_path(csl), "--bibliography", usethis::proj_path(bib))
		),
		output_file = out, output_dir = dirname(out), knit_root_dir = usethis::proj_path()
	)
}

###################################
### Output formatting functions ###
###################################

#' Results formatting for tables and reports
#'
#' These functions are simple wrappers around `paste()` and `number()` to
#'     format results for printing.
#'
#' @param mean,sd,n,proportion,est,low,high Numeric vectors
#' @param sep Separator between lower and upper bounds of the CI; default is \code{' to '}.
#' @param ... Passed to `number()`
#'
#' @name output-formatting
NULL

#' @rdname output-formatting
#'
#' @export
mean_sd <- function(mean, sd, ...) {
	paste0(number(mean, ...), ' (', number(sd, ...), ')')
}

#' @rdname output-formatting
#'
#' @export
n_percent <- function(n, proportion, ...) {
	paste0(number(n, accuracy = 1), ' (', number(proportion, scale = 100, suffix = '%', ...), ')')
}

#' @rdname output-formatting
#'
#' @export
est_ci <- function(est, low, high, sep = ' to ', ...) {
	paste0(number(est, ...), ' (', number(low, ...), sep, number(high, ...), ')')
}

##################################################################.
### Number formatting functions (based on scales::number() etc) ###
###################################################################

#' Format numbers using `scales::number()`-type functions
#'
#' These functions replace/extend the `scales::number()-type` formatting
#'     functions.
#'
#' @param x Numeric vector to format
#' @param accuracy,scale,prefix,suffix,big.mark,decimal.mark,trim,... As in
#'     \code{scales::number()}
#'
#' @name number-formatting
NULL

#' @rdname number-formatting
#' @export
number <- function(x, accuracy = 1, scale = 1, prefix = "", suffix = "",
									 big.mark = "< >", decimal.mark = ".", trim = TRUE, ...) {
	neg <- rep("", length(x))
	neg[x < 0] <- "&minus;"

	stringi::stri_replace_all_regex(
		paste0(
			neg,
			scales::number(abs(x), accuracy = accuracy, scale = scale, prefix = prefix, suffix = suffix,
										 big.mark = big.mark, decimal.mark = decimal.mark, trim = trim, ...)
		),
		"< >", "&#x202F;"
	)
}

#' @rdname number-formatting
#' @export
number_format <- function(accuracy = 1, scale = 1, prefix = "", suffix = "",
													big.mark = "< >", decimal.mark = ".", trim = TRUE, ...) {
	list(accuracy, scale, prefix, suffix, big.mark, decimal.mark, trim, ...)

	function(x) number(x, accuracy = accuracy, scale = scale,
										 prefix = prefix, suffix = suffix, big.mark = big.mark,
										 decimal.mark = decimal.mark, trim = trim, ...)
}

#' @rdname number-formatting
#' @export
percent <- function(x, accuracy = 1, scale = 100, prefix = "", suffix = "%",
										big.mark = "< >", decimal.mark = ".", trim = TRUE, ...) {
	neg <- rep("", length(x))
	neg[x < 0] <- "&minus;"

	stringi::stri_replace_all_regex(
		paste0(
			neg,
			scales::number(abs(x), accuracy = accuracy, scale = scale, prefix = prefix, suffix = suffix,
										 big.mark = big.mark, decimal.mark = decimal.mark, trim = trim, ...)
		),
		"< >", "&#x202F;"
	)
}

#' @rdname number-formatting
#' @export
percent_format <- function(accuracy = 1, scale = 100, prefix = "", suffix = "%",
													 big.mark = "< >", decimal.mark = ".", trim = TRUE, ...) {
	list(accuracy, scale, prefix, suffix, big.mark, decimal.mark, trim, ...)

	function(x) percent(x, accuracy = accuracy, scale = scale,
											prefix = prefix, suffix = suffix, big.mark = big.mark,
											decimal.mark = decimal.mark, trim = trim, ...)
}

#' @rdname number-formatting
#' @export
comma <- function(x, accuracy = 1, scale = 1, prefix = "", suffix = "",
									big.mark = ",", decimal.mark = ".", trim = TRUE, ...) {
	neg <- rep("", length(x))
	neg[x < 0] <- "&minus;"

	paste0(
		neg,
		scales::number(abs(x), accuracy = accuracy, scale = scale, prefix = prefix, suffix = suffix,
									 big.mark = big.mark, decimal.mark = decimal.mark, trim = trim, ...)
	)
}

#' @rdname number-formatting
#' @export
comma_format <- function(accuracy = 1, scale = 1, prefix = "", suffix = "",
												 big.mark = ",", decimal.mark = ".", trim = TRUE, ...) {
	list(accuracy, scale, prefix, suffix, big.mark, decimal.mark, trim, ...)

	function(x) comma(x, accuracy = accuracy, scale = scale,
										prefix = prefix, suffix = suffix, big.mark = big.mark,
										decimal.mark = decimal.mark, trim = trim, ...)
}
