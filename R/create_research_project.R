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
#'       corresponding function from the
#'       \code{\link[usethis:usethis-package]{usethis}} package,
#'       e.g. \code{\link[usethis:licenses]{use_mit_license}}}
#'       \item{A function}{Use the provided function to create and attach a
#'       license}
#'       \item{\code{NA}}{Do not attach any license}
#'     }
#'
#' @param path Path to the new project. If `package = TRUE`, the last component
#'     of the path will be used as the package name, and must follow standard R
#'     package naming conventions.
#' @param title Title of the research project (used for the DESCRIPTION file).
#' @param description Short description of the research project (used for the
#'     DESCRIPTION file).
#' @param package Logical (default = \code{FALSE}). Whether the project should
#'     be created as an R package.
#' @param license The type of license to attach to the package. See details.
#' @param workflow Either \code{"drake"} or \code{"make"}, to create
#'     corresponding workflow template files.
#' @param git Logical (default = \code{TRUE}). Whether to create a git
#'     repository.
#' @param raw_data_in_git Logical (default = \code{TRUE}). If FALSE, data in the
#'     \code{data/raw_data/} directory will be excluded from the git repository.
#' @param data_in_git Logical. If \code{FALSE} (the default), data in the
#'     \code{data/} directory (but not the \code{data/raw_data/} subdirectory,
#'     unless \code{raw_data_in_git} is also set to \code{FALSE}) will be
#'     excluded from the git repository.
#' @param output_in_git Logical. If \code{FALSE} (the default), data in the
#'     \code{output/} directory will be excluded from the git repository.
#' @param github Logical. If \code{TRUE} (the default), create a GitHub
#'     repository and push the initial commit to GitHub.
#' @param private Logical. If \code{TRUE} (the default), a private GitHub
#'     repository will be created; otherwise, the repository will be publicly
#'     accessible. Ignored if \code{github = FALSE}.
#'
#' @export
create_research_project <- function(path, title, description = NULL,
																		package = FALSE, license = NULL, workflow = "drake",
																		git = TRUE, raw_data_in_git = TRUE, data_in_git = FALSE, output_in_git = FALSE,
																		github = TRUE, private = TRUE) {
	if (package) stop('package = TRUE is not supported in this version of cmor.tools')

	# Create new package
	if (missing(title)) title <- basename(path)
	use_research_project(path, package)

	# Move to the new package
	oldwd <- getwd()
	usethis::proj_set(path)

	# Create project directory and template files
	use_project_directory(package, workflow = workflow, git = git,
												raw_data_in_git = raw_data_in_git, data_in_git = data_in_git, output_in_git = output_in_git)

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
	} else if (!is.null(license)) usethis::ui_warn("Ignoring argument {ui_code(license)} for non-package projects")

  if(interactive()) {
  	usethis::proj_activate(path)
  	if (rstudioapi::isAvailable()) {
  		writeLines(
  			c(
  				readLines(fs::path_home_r(".Rprofile")),
  				"cat(crayon::bold('\\nThis project was created by `cmor.tools::create_research_project()`.\\n'))",
  				"cat('\\nTo complete project set-up, you need to:\\n')",
  				if (package) "cat(crayon::red('*'), 'Edit the DESCRIPTION file\\n')",
  				"cat(crayon::red('*'), 'Run `complete_setup(title = <title>, description = <description>)`\\n')",
  				"suppressMessages(require(cmor.tools))",
  				paste0("options(cmor.tools.git = ", git, ")"),
  				paste0("options(cmor.tools.git_data = ", data_in_git, ")"),
  				paste0("options(cmor.tools.git_rawdata = ", raw_data_in_git, ")"),
  				paste0("options(cmor.tools.git_output = ", output_in_git, ")"),
  				paste0("options(cmor.tools.github = ", github, ")"),
  				paste0("options(cmor.tools.github_private = ", private, ")"),
  				"invisible(file.remove('.Rprofile'))",
  				""
  			), usethis::proj_path(".Rprofile")
  		)
  		usethis::ui_todo("Go there to complete project set-up")
  		setwd(oldwd)
  	}
  	else {
  		usethis::ui_line("Next you need to:")
  		if (package) usethis::ui_todo("Edit the DESCRIPTION file")
  		usethis::ui_todo(
  			"Run `complete_setup(title = <title>, description = <description>)` to complete the project set-up"
  		)
  	}
  }
}

#' Finish project set-up using the CMOR research project templates
#'
#' This function should be used in the newly-created project after
#'     \code{create_research_project()} to create a git repository, connect to
#'     github, and add a basic project README.
#'
#' @param title Title of the research project (used for the DESCRIPTION file).
#' @param description Short description of the research project (used for the
#'     DESCRIPTION file).
#' @param project Path to the project folder. Default is to use the current
#'     working directory.
#' @param title (Optional) If non-null, will overwrite the default title
#'     (package name) in the DESCRIPTION file.
#' @param description (Optional) If non-null, will be added as the
#'     \code{description} field in the DESCRIPTION file.
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
#' @param output_in_git Logical. If \code{FALSE} (the default), the
#'     \code{output} folder will be excluded from the git repository.
#' @param private Logical (default = \code{TRUE}). Should the GitHub repo be
#'     private or public? Ignored if \code{github = FALSE}.
#'
#' @export
complete_setup <- function(
	title = NULL, description = NULL, project = getwd(),
	git = getOption("cmor.tools.git"), raw_data_in_git = getOption("cmor.tools.git_rawdata"),
	data_in_git = getOption("cmor.tools.git_data"), output_in_git = getOption("cmor.tools.git_output"),
	github = getOption("cmor.tools.github"), private = getOption("cmor.tools.github_private")
) {
	if (!file.exists("DESCRIPTION")) {
		projname <- basename(project)
		if (is.null(title)) title <- projname
		usethis::use_description(
			list(Project = projname, Title = title, Description = description,
					 Package = NULL, Version = NULL, License = NULL, LazyData = NULL),
			check_name = FALSE
		)
	}

	if (git) {
		# Initialise git repository
		use_git()

		if (github) {
			usethis::use_git_credentials(git2r::cred_ssh_key())
			# Connect repository to github
			usethis::use_github(private = private)
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

	# Create basic README file
	use_cmor_readme(data)

	if (git) {
		usethis::ui_todo("A restart of RStudio is required to activate the Git pane")
		usethis::ui_todo("You can do this by calling `openProject()`")
	}
}
