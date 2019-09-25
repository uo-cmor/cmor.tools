#' Create the directory structure for a CMOR research project
#'
#' This function will create a directory structure to hold the data, code, and
#'     outputs for a research project package, and various template files to
#'     format output, etc.
#'
#' @export
use_project_directory <- function(raw_data_in_git = TRUE, data_in_git = FALSE) {
	pkg <- as.package(".")

	# Add required packages to Imports/Suggests
	#usethis::use_package('pkgname')

	# Create directory structure
  add_directories()

  # Add template
  add_templates()

  # Add extdata and/or extdata/raw_data to .gitignore
  if(!data_in_git) usethis::use_git_ignore("extdata/*")
  if(!raw_data_in_git) usethis::use_git_ignore("extdata/raw_data")
  else usethis::use_git_ignore("!extdata/raw_data")

  # Ignore code and output directories when building the project
  usethis::use_build_ignore(c("code", "output"))

  invisible(TRUE)
}

#' Add directories and sub-directories
#'
#' This adds the directory structure
add_directories <- function() {
  usethis::use_directory("extdata/raw_data")
	usethis::use_directory("code")
	usethis::use_directory("R")
	usethis::use_directory("output")
}

#' Add template files
#'
#' This adds the template files (word-styles-reference-01.docx, vancouver.csl,
#'     _drake.R, etc) to the appropriate directories.
add_templates <- function() {
	# Skeleton drake plan file
	file.copy(system.file("templates", "_drake.R", package = "cmor.tools", mustWork = TRUE),
						usethis::proj_path())

	# Manuscript templates and functions
  file.copy(system.file("templates", "manuscript.Rmd", package = "cmor.tools", mustWork = TRUE),
  					usethis::proj_path("code", "manuscripts")) # manuscript template - possibly include separate templates for figures/tables outputs as well
  file.copy(system.file("templates", "references.bib", package = "cmor.tools", mustWork = TRUE),
  					usethis::proj_path("code", "manuscripts")) # example bibtex references file
  file.copy(system.file("templates", "vancouver.csl", package = "cmor.tools", mustWork = TRUE),
  					usethis::proj_path("code", "manuscripts")) # CSL citation formatting (Vancouver style)
  file.copy(system.file("templates", "word-styles-reference-01.docx", package = "cmor.tools", mustWork = TRUE),
  					usethis::proj_path("code", "manuscripts")) # Reference .docx styles file
  file.copy(system.file("templates", "render_manuscript.R", package = "cmor.tools", mustWork = TRUE),
  					usethis::proj_path("R")) # function to render the manuscript to .docx
  file.copy(system.file("templates", "formatting", package = "cmor.tools", mustWork = TRUE),
  					usethis::proj_path("R")) # various output formatting functions

  # Project infrastructure (to create 'results' project, replicate analyses, etc)
  file.copy(system.file("templates", "infrastructure.R", package = "cmor.tools", mustWork = TRUE),
  					usethis::proj_path("R")) # these are still under consideration
  file.copy(system.file("templates", "00-packages.R", package = "cmor.tools", mustWork = TRUE),
  					usethis::proj_path("code")) # core packages; add to this file when conducting analyses
}

