#' Create a research project using the CMOR project templates
#'
#' This function creates an R package with a 'proprietary' license,
#'     adds a basic README to it, initialises our standard research project
#'     directory structure and template files, and creates a git repository with
#'     an initial commit.
#'
#' @section License:
#' You can optionally set a license that will be attached to the new package.
#'     This should be specified as a character string from the following
#'     options:
#'     \describe{
#'       \item{\code{"proprietary"}}{This is the default license, to be used for
#'       projects that are not intended to be shared publicly in their working
#'       form}
#'       \item{\code{"mit"}, \code{"gpl3"}, \code{"lgpl"}, "\code{"apl2"},
#'       \code{"cc0"}, \code{"ccby"}}{Open source licenses; will use the
#'       corresponding function from the \code{\link[usethis]{usethis}} package,
#'       e.g. \code{\link[usethis]{use_mit_license}}}
#'       \item{\code{NULL}}{Do not attach any license}
#'     }
#'
#' @param pkgname Path to the new package. The last component of the path will
#'     be used as the package name, and must follow standard R package naming
#'     conventions.
#' @param license The type of license to attach to the package. See details.
#' @param git Logical (default = TRUE). Whether to create a git repository.
#' @param raw_data_in_git Logical (default = TRUE). If FALSE, data in the
#'     \code{data/raw_data/} directory will be excluded from the git repository.
#' @param data_in_git Logical. If FALSE (the default), data in the \code{data/}
#'     directory (but not the \code{data/raw_data/} subdirectory, unless
#'     \code{raw_data_in_git} is also set to \code{FALSE}) will be excluded from
#'     the git repository.
#'
#' @export
create_research_project <- function(pkgname, license = "proprietary", git = TRUE, raw_data_in_git = TRUE, data_in_git = FALSE) {

	# Create new project
	use_research_project(pkgname)

	# Move to the new project
	#setwd(pkgname) # This should be done already by use_research_project (?)

	# Set the package license
  if (identical(license, "proprietary")) use_proprietary_license()
	else if (identical(license, "mit")) usethis::use_mit_license()
	else if (identical(license, "gpl3")) usethis::use_gpl3_license()
	else if (identical(license, "lgpl")) usethis::use_lgpl_license()
	else if (identical(license, "apl2")) usethis::use_apl2_license()
	else if (identical(license, "cc0")) usethis::use_cc0_license()
	else if (identical(license, "ccby")) usethis::use_ccby_license()
	else if (is.function(license)) license()
	else if (!is.null(license)) stop("'", license, "' is not a recognised license type")

  # Create basic README file
	use_cmor_readme()

	# Initialise git repository
	usethis::use_git()

	# Create project directory and template files
	use_project_directory(raw_data_in_git = raw_data_in_git, data_in_git = data_in_git)

	# Install the package and its dependencies
  remotes::install_local()
}
