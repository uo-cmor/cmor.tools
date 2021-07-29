#' Create an R package to hold a research project
#'
#' This function wraps usethis::create_package() to create a new R package to
#'     hold the data, code, and output for a research project.
#'
#' @param path Path to the new package. The last component of the path will
#'     be used as the package name, and must follow standard R package naming
#'     conventions.
#' @param package Logical (default = \code{FALSE}). Whether the project should
#'     be created as an R package.
#' @param fields Named list of fields to add to the DESCRIPTION file. See
#'     \code{\link[usethis]{create_package}} for details.
#'
#' @export
use_research_project <- function(path, package, fields = NULL, rstudio = rstudioapi::isAvailable(),
																 open = rlang::is_interactive()) {
	name <- basename(path)

	# Provide welcome message at first start?

	# Create the new project
	usethis::create_project(path = path, rstudio = rstudio, open = open)

	if (open & !rstudio) {
		setwd(path)
		usethis::ui_done("The working directory is now {getwd()}.")
	}

	invisible(TRUE)
}
