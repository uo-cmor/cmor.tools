#' Target to render Rmd manuscript to docx
#'
#' Render an R Markdown report to Word .docx file in a \code{targets} pipeline.
#'
#' @param name Symbol, name of the target
#' @param path Character string, file path to the R Markdown source file. Must
#'     have length 1.
#' @param output_file Character string, file path to the docx output file to
#'     create.
#' @param file_reference_docx (optional) Symbol, existing target containing the
#'     file path to the reference styles file used to format the output.
#' @param file_csl (optional) Symbol, existing target containing the file path
#'     to the CSL file to use to format references.
#' @param file_bib (optional) Symbol, existing target containing the file path
#'     to the BibTeX file holding reference details.
#' @param pandoc_args (optional) Additional command-line options to pass to
#'     pandoc. See \code{\link[rmarkdown]{word_document}()}.
#' @param render_args (optional) Other render arguments passed to
#'     \code{\link[rmarkdown]{render}()}. Unlike the \code{render_arguments}
#'     argument to \code{\link[tarchetypes]{tar_render_raw}()}, this should be a
#'     named list of arguments, not a language object containing such a list.
#' @param packages,library,error,deployment,priority,resources,retrieval,cue,quiet
#'     See \code{\link[tarchetypes]{tar_render_raw}()}.
#' @export
tar_render_manuscript <- function(name, path, output_file,
																	file_reference_docx = NULL, file_csl = NULL, file_bib = NULL,
																	pandoc_args = NULL, render_args = NULL,
																	packages = c("rmarkdown"),
																	library = targets::tar_option_get("library"),
																	error = targets::tar_option_get("error"),
																	deployment = "main",
																	priority = targets::tar_option_get("priority"),
																	resources = targets::tar_option_get("resources"),
																	retrieval = targets::tar_option_get("retrieval"),
																	cue = targets::tar_option_get("cue"),
																	quiet = TRUE) {
	checkmate::assert_file_exists(path)
	checkmate::assert_path_for_output(output_file, overwrite = TRUE)
	checkmate::assert(checkmate::check_null(pandoc_args),
										checkmate::check_list(pandoc_args, types = "character"),
										checkmate::check_character(pandoc_args))
	checkmate::assert_list(render_args, types = "character", names = "strict", null.ok = TRUE)

	file_reference_docx <- rlang::enexpr(file_reference_docx)
	file_csl <- rlang::enexpr(file_csl)
	file_bib <- rlang::enexpr(file_bib)
	pandoc_args <- rlang::enexpr(pandoc_args)
	render_args <- rlang::enexpr(render_args)

	render_arguments <- rlang::expr(c(
		!!render_args,
		list(
			output_format = rmarkdown::word_document(
				pandoc_args = c(
					!!pandoc_args,
					if (!is.null(!!file_reference_docx))
						list("--reference-doc", fs::path_wd(!!file_reference_docx)),
					if (!is.null(!!file_csl)) list("--csl", fs::path_wd(!!file_csl)),
					if (!is.null(!!file_bib)) list("--bibliography", fs::path_wd(!!file_bib))
				)
			),
			output_file = !!output_file, output_dir = "output"
		)
	))

	tarchetypes::tar_render_raw(rlang::as_name(rlang::ensym(name)), path, packages,
															library, error, deployment, priority, resources, retrieval, cue,
															quiet, render_arguments)
}
