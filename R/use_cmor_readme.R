#' Create a skeleton README file for CMOR research projects
#'
#' This function creates a skeleton README.Rmd file with basic project
#'     information for a CMOR research project package. \code{README.Rmd} will
#'     be automatically added to \code{.Rbuildignore}.
#'
#' @export
use_cmor_readme <- function(pkg) {
	template <- system.file("templates", "cmor-readme", package = "cmor.tools", mustWork = TRUE)

	template_out <- whisker::whisker.render(readLines(template), pkg)
	writeLines(template_out, "README.Rmd")

	usethis::use_build_ignore("README.Rmd")

	usethis::ui_todo("Edit the README.Rmd file")

	if(rstudioapi::isAvailable()) rstudioapi::navigateToFile("README.Rmd")

	usethis::use_build_ignore("^README-.*\\.png$", escape = FALSE)

	usethis::ui_info("Remember to render README.Rmd to README.md for GitHub.")

	invisible(TRUE)
}

