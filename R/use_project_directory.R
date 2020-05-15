#' Create the directory structure for a CMOR research project
#'
#' This function will create a directory structure to hold the data, code, and
#'     outputs for a research project package, and various template files to
#'     format output, etc.
#'
#' @param name
#'
#' @export
use_project_directory <- function(name, package, workflow = "drake", git = TRUE, raw_data_in_git = TRUE, data_in_git = FALSE) {
	# Add required packages to Imports/Suggests
	#usethis::use_package('pkgname')

	# Create directory structure
  add_directories(package)

  # Add template
  add_templates(package, workflow = workflow)

  # Add extdata and/or extdata/raw_data to .gitignore
  if (git) {
  	if (package) prefix <- "inst/" else prefix <- ""

  	if(!data_in_git) usethis::use_git_ignore(paste0(prefix, "derived_data"))
  	if(!raw_data_in_git) usethis::use_git_ignore(paste0(prefix, "raw_data"))
  }

  invisible(TRUE)
}

#' Add directories and sub-directories
#'
#' This adds the directory structure
add_directories <- function(package) {
	if (package) prefix <- "inst/" else prefix <- ""

  usethis::use_directory(paste0(prefix, "raw_data"))
	usethis::use_directory(paste0(prefix, "derived_data"))
	usethis::use_directory(paste0(prefix, "reports"))
	usethis::use_directory(paste0(prefix, "R"))
	usethis::use_directory("output", ignore = TRUE)
}

#' Add template files
#'
#' This adds the template files (word-styles-reference-01.docx, vancouver.csl,
#'     _drake.R, etc) to the appropriate directories.
add_templates <- function(package, workflow = "drake") {
	if (package) prefix = "inst/" else prefix = ""

	# Project-specific .Rprofile
	template <- system.file("templates", "rprofile", package = "cmor.tools", mustWork = TRUE)
	template_out <- whisker::whisker.render(readLines(template), list(is_package = package))
	writeLines(template_out, usethis::proj_path(".Rprofile"))

	if (workflow == "drake") {
		# Skeleton drake plan file
		template <- system.file("templates", "plan", package = "cmor.tools", mustWork = TRUE)
		template_out <- whisker::whisker.render(readLines(template), list(is_package = package))
		writeLines(template_out, usethis::proj_path(prefix, "_drake.R"))
	} else if (workflow == "make") {
		# Skeleton Makefile
		template <- system.file("templates", "makefile-template", package = "cmor.tools", mustWork = TRUE)
		template_out <- whisker::whisker.render(readLines(template), list(is_package = package))
		writeLines(template_out, usethis::proj_path(prefix, "Makefile"))
		if (package) {
			file.copy(system.file("templates", "Makefile-root", package = "cmor.tools", mustWork = TRUE),
													 usethis::proj_path("Makefile"))
		}
	}

	# Manuscript templates and functions
  file.copy(system.file("templates", "manuscript.Rmd", package = "cmor.tools", mustWork = TRUE),
  					usethis::proj_path(prefix, "reports")) # manuscript template - possibly include separate templates for figures/tables outputs as well
  file.copy(system.file("templates", "references.bib", package = "cmor.tools", mustWork = TRUE),
  					usethis::proj_path(prefix, "reports")) # example bibtex references file
  file.copy(system.file("templates", "vancouver.csl", package = "cmor.tools", mustWork = TRUE),
  					usethis::proj_path(prefix, "reports")) # CSL citation formatting (Vancouver style)
  file.copy(system.file("templates", "word-styles-reference-01.docx", package = "cmor.tools", mustWork = TRUE),
  					usethis::proj_path(prefix, "reports")) # Reference .docx styles file
  file.copy(system.file("templates", "word-styles-reference-med-care.docx", package = "cmor.tools", mustWork = TRUE),
  					usethis::proj_path(prefix, "reports")) # Reference .docx styles file for Medical Care journal

  # Template 'packages' file to load required packages (for _drake.R)
  template <- system.file("templates", "packages", package = "cmor.tools", mustWork = TRUE)
  template_out <- whisker::whisker.render(readLines(template),
  																				list(is_package = package, package = basename(usethis::proj_path())))
  writeLines(template_out, usethis::proj_path(prefix, "R", "load-packages.R"))

  # Template 'define-parameters' file to define fixed parameters of the analysis
  template <- system.file("templates", "define-parameters", package = "cmor.tools", mustWork = TRUE)
  template_out <- whisker::whisker.render(readLines(template),
  																				list(is_package = package, package = basename(usethis::proj_path())))
  writeLines(template_out, usethis::proj_path(prefix, "R", "define-parameters.R"))
}

