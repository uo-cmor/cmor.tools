#' Create the directory structure for a CMOR research project
#'
#' This function will create a directory structure to hold the data, code, and
#'     outputs for a research project package, and various template files to
#'     format output, etc.
#'
#' @export
use_project_directory <- function(pkg, raw_data_in_git = TRUE, data_in_git = FALSE) {
	# Add required packages to Imports/Suggests
	#usethis::use_package('pkgname')

	# Create directory structure
  add_directories()

  # Add template
  add_templates(pkg = pkg)

  # Add extdata and/or extdata/raw_data to .gitignore
  if(!data_in_git) usethis::use_git_ignore("inst/derived_data")
  if(!raw_data_in_git) usethis::use_git_ignore("inst/raw_data")

  invisible(TRUE)
}

#' Add directories and sub-directories
#'
#' This adds the directory structure
add_directories <- function() {
  usethis::use_directory("inst/raw_data")
	usethis::use_directory("inst/derived_data")
	usethis::use_directory("inst/code")
	usethis::use_directory("inst/reports")
	usethis::use_directory("output", ignore = TRUE)
}

#' Add template files
#'
#' This adds the template files (word-styles-reference-01.docx, vancouver.csl,
#'     _drake.R, etc) to the appropriate directories.
add_templates <- function(pkg) {
	# Project-specific .Rprofile
	file.copy(system.file("templates", ".Rprofile", package = "cmor.tools", mustWork = TRUE),
						usethis::proj_path())

	# Skeleton drake plan file
	file.copy(system.file("templates", "_drake.R", package = "cmor.tools", mustWork = TRUE),
						usethis::proj_path("inst"))

	# Manuscript templates and functions
  file.copy(system.file("templates", "manuscript.Rmd", package = "cmor.tools", mustWork = TRUE),
  					usethis::proj_path("inst", "reports")) # manuscript template - possibly include separate templates for figures/tables outputs as well
  file.copy(system.file("templates", "references.bib", package = "cmor.tools", mustWork = TRUE),
  					usethis::proj_path("inst", "reports")) # example bibtex references file
  file.copy(system.file("templates", "vancouver.csl", package = "cmor.tools", mustWork = TRUE),
  					usethis::proj_path("inst", "reports")) # CSL citation formatting (Vancouver style)
  file.copy(system.file("templates", "word-styles-reference-01.docx", package = "cmor.tools", mustWork = TRUE),
  					usethis::proj_path("inst", "reports")) # Reference .docx styles file

  # Template 'packages' file to load required packages (for _drake.R)
  template <- system.file("templates", "packages", package = "cmor.tools", mustWork = TRUE)
  template_out <- whisker::whisker.render(readLines(template), pkg)
  writeLines(template_out, "inst/code/00-packages.R")

  # file.copy(system.file("templates", "00-packages.R", package = "cmor.tools", mustWork = TRUE),
  # 					usethis::proj_path("inst", "code")) # core packages; add to this file when conducting analyses
}

