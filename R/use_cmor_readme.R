#' Create a skeleton README file for CMOR research projects
#'
#' This function creates a skeleton README.Rmd file with basic project
#'     information for a CMOR research project package. \code{README.Rmd} will
#'     be automatically added to \code{.Rbuildignore}.
#'
#' @param data List of variables used by \code{cmor-readme} template.
#'
#' @export
use_cmor_readme <- function(data) {
	template <- system.file("templates", "cmor-readme", package = "cmor.tools", mustWork = TRUE)

	template_out <- whisker::whisker.render(readLines(template), data)
	writeLines(template_out, "README.Rmd")

	if (data$is_package) usethis::use_build_ignore("README.Rmd")

	usethis::ui_done("Basic README file created")
	usethis::ui_todo("Edit the README.Rmd file to provide an introduction to the project")

	if(rstudioapi::isAvailable()) rstudioapi::navigateToFile("README.Rmd")

	if (data$is_package) usethis::use_build_ignore("^README-.*\\.png$", escape = FALSE)

	usethis::ui_todo("Remember to render README.Rmd to README.md for GitHub.")

	invisible(TRUE)
}

