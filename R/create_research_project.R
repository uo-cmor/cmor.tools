#' Create a research project using the CMOR project templates
#'
#' These function creates an R project or package, adds a basic README,
#'     initialises our standard research project directory structure and
#'     template files, and creates a git repository with an initial commit.
#'
#' @section License:
#' If \code{package = TRUE}, you can optionally set a license that will be
#'     attached to the new package. This should be specified as a character
#'     string or function from the following options:
#'     \describe{
#'       \item{\code{"proprietary"}}{This is the default license, to be used for
#'       projects that are not intended to be shared publicly in their working
#'       form}
#'       \item{\code{"mit"}, \code{"gpl3"}, \code{"lgpl"}, "\code{"apl2"},
#'       \code{"cc0"}, \code{"ccby"}}{Open source licenses; will use the
#'       corresponding function from the \code{\link[usethis]{usethis}} package,
#'       e.g. \code{\link[usethis]{use_mit_license}}}
#'       \item{A function}{Use the provided function to create and attach a
#'       license}
#'       \item{\code{NA}}{Do not attach any license}
#'     }
#'
#' @param path Path to the new project. If `package = TRUE`, the last component
#'     of the path will be used as the package name, and must follow standard R
#'     package naming conventions.
#' @param package Logical (default = \code{FALSE}). Whether the project should
#'     be created as an R package.
#' @param license The type of license to attach to the package. See details.
#' @param git Logical (default = \code{TRUE}). Whether to create a git
#'     repository.
#' @param raw_data_in_git Logical (default = \code{TRUE}). If FALSE, data in the
#'     \code{data/raw_data/} directory will be excluded from the git repository.
#' @param data_in_git Logical. If \code{FALSE} (the default), data in the
#'     \code{data/} directory (but not the \code{data/raw_data/} subdirectory,
#'     unless \code{raw_data_in_git} is also set to \code{FALSE}) will be
#'     excluded from the git repository.
#'
#' @export
create_research_project <- function(path, package = FALSE, license = NULL,
																		git = TRUE, raw_data_in_git = TRUE, data_in_git = FALSE,
																		github = TRUE, private = TRUE) {

	# Create new package
	use_research_project(path, package)

	# Move to the new package
	oldwd <- getwd()
	usethis::proj_set(path)
	name <- basename(path)

	# Create project directory and template files
	use_project_directory(name, package, git = git, raw_data_in_git = raw_data_in_git, data_in_git = data_in_git)

	# Set the package license
	if (package) {
		if (is.null(license)) license <- "proprietary"
		if (identical(license, "proprietary")) use_proprietary_license()
		else if (identical(license, "mit")) usethis::use_mit_license()
		else if (identical(license, "gpl3")) usethis::use_gpl3_license()
		else if (identical(license, "lgpl")) usethis::use_lgpl_license()
		else if (identical(license, "apl2")) usethis::use_apl2_license()
		else if (identical(license, "cc0")) usethis::use_cc0_license()
		else if (identical(license, "ccby")) usethis::use_ccby_license()
		else if (is.function(license)) license()
		else if (!is.na(license)) stop("'", license, "' is not a recognised license type")
	} else if (!is.null(license)) ui_warn("Ignoring argument {ui_code(license)} for non-package projects")

  if(interactive()) {
  	usethis::proj_activate(path)
  	if (rstudioapi::isAvailable()) {
  		usethis::ui_line("creating temporary .Rprofile")
  		fileConn <- file(usethis::proj_path(".Rprofile"))
  		writeLines(
  			c(
  				"cat(crayon::bold('\\nThis project was created by cmor.tools.\\n'))",
  				"cat('\\nTo complete project set-up, you need to:\\n')",
  				if (package) "cat(crayon::red('*'), 'Edit the DESCRIPTION file\\n')",
  				"cat(crayon::red('*'), 'Run `complete_setup()`\\n')",
  				"suppressMessages(require(cmor.tools))",
  				"options(usethis.protocol = 'ssh')",
  				paste0("options(cmor.tools.git = ", git, ")"),
  				paste0("options(cmor.tools.git_data = ", data_in_git, ")"),
  				paste0("options(cmor.tools.git_rawdata = ", raw_data_in_git, ")"),
  				paste0("options(cmor.tools.github = ", github, ")"),
  				paste0("options(cmor.tools.github_private = ", private, ")"),
  				"invisible(file.copy(system.file('templates', '.Rprofile', package = 'cmor.tools', mustWork = TRUE), '.Rprofile', overwrite = TRUE))",
  				""
  			), fileConn
  		)
  		close(fileConn)
  		usethis::ui_todo("Go there to complete project set-up")
  		setwd(oldwd)
  	}
  	else {
  		ui_line("Next you need to:")
  		if (package) ui_todo("Edit the DESCRIPTION file")
  		ui_todo("Run `complete_setup()` to complete the project set-up")
  	}
  }
}

#' Finish project set-up using the CMOR research project templates
#'
#' This function should be used in the newly-created project after
#'     \code{create_research_project()} to create a git repository, connect to
#'     github, and add a basic project README.
#'
#' @param project Path to the project folder. Default is to use the current
#'     working directory.
#' @param git Logical (default = \code{TRUE}). Whether to create a git
#'     repository.
#' @param github Logical (default = \code{TRUE}). Whether to create a GitHub
#'     repository.
#' @param raw_data_in_git Logical (default = \code{TRUE}). If FALSE, data in the
#'     \code{data/raw_data/} directory will be excluded from the git repository.
#' @param data_in_git Logical. If \code{FALSE} (the default), data in the
#'     \code{data/} directory (but not the \code{data/raw_data/} subdirectory,
#'     unless \code{raw_data_in_git} is also set to \code{FALSE}) will be
#'     excluded from the git repository.
#' @param private Logical (default = \code{TRUE}). Should the GitHub repo be
#'     private or public? Ignored if \code{github = FALSE}.
complete_setup <- function(
	project = getwd(), git = getOption("cmor.tools.git"), raw_data_in_git = getOption("cmor.tools.git_rawdata"),
	data_in_git = getOption("cmor.tools.git_data"), github = getOption("cmor.tools.github"),
	private = getOption("cmor.tools.github_private")) {
	if (git) {
		# Initialise git repository
		use_git()

		if (github) {
			# Connect repository to github
			usethis::use_github(
				private = private,
				credentials = git2r::cred_ssh_key(publickey = git2r::ssh_path("id_rsa.pub"),
																					privatekey = git2r::ssh_path("id_rsa"))
			)
		}
	}

	data <- list(
		name = basename(normalizePath(project)),
		github = github,
		is_package = is_package(project)
	)
	gh_info <- gh::gh("GET /repos/:owner/:repo", owner = github_info()$username,
										repo = github_info()$repo, .api_url = "https://api.github.com",
										.token = check_github_token(usethis::github_token(), allow_empty = TRUE))
	if (github) {
		data <- append(data, github_info(project))
		data <- append(data, list(url = gh_info$html_url))
	}

	# pkg <- devtools::as.package(project)
	# if (uses_github(pkg$path)) pkg <- append(pkg, github_info(data$path))
	# pkg$Rmd <- TRUE
	# pkg$is_package <-
	# 	file.exists(rprojroot::find_package_root_file("DESCRIPTION", path = project)) &&
	# 	any(grepl("^Package:", readLines(rprojroot::find_package_root_file("DESCRIPTION", path = project))))

	# Create basic README file
	use_cmor_readme(data)

	if (git) restart_rstudio("A restart of RStudio is required to activate the Git pane")
}
