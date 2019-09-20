#' Create an R package to hold a research project
#'
#' This function wraps usethis::create_package() to create a new R package to
#'     hold the data, code, and output for a research project.
#'
#' @param path Path to the new package. The last component of the path will
#'     be used as the package name, and must follow standard R package naming
#'     conventions.
#'
#' @export
use_research_project() <- function(path) {
	name <- basename(path)

	check_package_name(name)

	# Provide welcome message at first start?

	# Create a new package
	usethis::create_package(path = path)

	usethis::ui_done("The package {name} has been created")

	usethis::ui_line("Next you need to:")
	usethis::ui_todo("Edit the DESCRIPTION file")
	#...

	if(rstudioapi::isAvailable()) rstudioapi::navigateToFile("DESCRIPTION")

	invisible(TRUE)
}
