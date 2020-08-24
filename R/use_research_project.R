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
use_research_project <- function(path, package, fields = NULL) {
	name <- basename(path)

	if (package) check_package_name(name)

	# Provide welcome message at first start?

	# Create a new package/project
	if (package) usethis::create_package(path = path, fields = fields, open = FALSE)
	else {
		usethis::create_project(path = path, open = FALSE)
	}

	#if(rstudioapi::isAvailable()) rstudioapi::navigateToFile("DESCRIPTION")

	invisible(TRUE)
}
