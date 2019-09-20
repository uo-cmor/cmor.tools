#' @name valid_package_name
#' @title Check that a package name is valid
#'
#' @description Checks that a package name meets R package name requirements: it
#'     consists only of letters, numbers, and periods, starts with a letter, and
#'     does not end with a period.
#'
#' @param pkgname A character string
valid_package_name <- function (pkgname) {
	grepl("^[a-zA-Z][a-zA-Z0-9.]+$", pkgname) && !grepl("\\.$", pkgname)
}

#' @name check_package_name
#' @title Check that a package name is valid
#'
#' @description A wrapper around \code{\link{valid_package_name}} that returns
#'     an error with an informative message if the package name is not valid.
#'
#' @param pkgname
check_package_name <- function(pkgname) {
	if (!valid_package_name(pkgname))
		usethis::ui_stop(
			c("'{pkgname}' is not a valid package name. It should:",
				"* Contain only ASCII letters, numbers, and '.'",
				"* Have at least two characters",
				"* Start with a letter",
				"* Not end with '.'")
		)
}
