#' Create a research project using the CMOR project templates
#'
#' These function creates an R project, adds a basic README,
#'     initialises our standard research project directory structure and
#'     template files, and creates a git repository with an initial commit.
#'
#' @param path Path to the new project.
#' @param workflow Either \code{"drake"} or \code{"make"}, to create
#'     corresponding workflow template files.
#' @param git Logical (default = \code{TRUE}). Whether to create a git
#'     repository.
#' @param raw_data_in_git Logical (default = \code{TRUE}). If FALSE, data in the
#'     \code{data/raw_data/} directory will be excluded from the git repository.
#' @param github Logical. If \code{TRUE} (the default), create a GitHub
#'     repository and push the initial commit to GitHub.
#' @param private Logical. If \code{TRUE} (the default), a private GitHub
#'     repository will be created; otherwise, the repository will be publicly
#'     accessible. Ignored if \code{github = FALSE}.
#'
#' @export
create_research_project <- function(
	path, workflow = "targets", git = TRUE, raw_data_in_git = TRUE, github = TRUE, private = TRUE,
	organisation = NULL, rstudio = rstudioapi::isAvailable(), open = rlang::is_interactive()
) {
	path <- fs::path_expand(path)
	if (!fs::dir_exists(path)) {
		fs::dir_create(path)
		usethis::ui_done("The project directory {usethis::ui_path(path)} has been created.")
	} else {
		rlang::abort(c(
			paste0("The directory ", usethis::ui_path(path), " already exists."),
			x = paste0(usethis::ui_code('create_research_project()'),
								 " will not overwrite an existing directory.")
		))
	}

	if (!git & github) {
		github <- FALSE
		github_warn <- TRUE
		usethis::ui_warn(glue::glue(
			"{usethis::ui_code('github')} was set to {usethis::ui_code('TRUE')}, ",
			"but {usethis::ui_code('git')} is {usethis::ui_code('FALSE')}."
		))
		usethis::ui_info("Setting {usethis::ui_code('github')} to {usethis::ui_code(FALSE)}.")
	} else github_warn <- FALSE

	if (rstudio & open) {
		fileConn <- file(fs::path(path, ".Rprofile"))
		writeLines(
			c(
				## Copy previous Rprofile so we can continue working immediately
				readLines(fs::path_home_r(".Rprofile")),
				## Continue project setup
				"usethis::ui_line(crayon::bold('Initial project setup:'))",
				paste0("cmor.tools::use_project_directory(package = FALSE, ",
							 "git = ", git, ", raw_data_in_git = ", raw_data_in_git, ")"),
				if (git) "cmor.tools:::use_git()",
				if (github) paste0("if (usethis::ui_yeah('Is it OK to push these to GitHub?')) ",
													 "usethis::use_github(private = ", private, ")"),
				paste0("data <- list(name = fs::path_file('", path, "'), github = ", github,
							 ", is_package = FALSE)"),
				if (github) c(
					paste0("data <- append(data, gh::gh_tree_remote('", path, "'))"),
					paste0(
						"data <- append(data, ",
						"list(url = gh::gh('GET /repos/:owner/:repo', ",
						"owner = gh::gh_tree_remote()$username, repo = gh::gh_tree_remote()$repo)$html_url))"
					)
				),
				"cmor.tools::use_cmor_readme(data)",

				## Welcome message
				usethis::ui_line(),
				paste0("usethis::ui_line(crayon::bold('This project was created by ",
							 "`cmor.tools::create_research_project()`.'))"),
				"usethis::ui_line('This welcome message describes the directory structure and workflow')",
				"invisible(file.remove('.Rprofile'))"
			),
			fileConn
		)
		close(fileConn)

		usethis::create_project(path = path, rstudio = rstudio, open = open)
	} else {
		usethis::create_project(path = path, rstudio = rstudio, open = open)

		setwd(path)

		usethis::ui_done("The working directory is now {getwd()}")
	}

	# Create new package
	# if (missing(title)) title <- basename(path)
	# use_research_project(path, package)

	# Move to the new package
	# 	oldwd <- getwd()
	# 	usethis::proj_set(path)
	#
	# 	# Create project directory and template files
	# 	use_project_directory(package, workflow = workflow, git = git,
	# 												raw_data_in_git = raw_data_in_git, data_in_git = data_in_git, output_in_git = output_in_git)
	#
	# 	# Set the package license
	# 	if (package) {
	# 		if (is.null(license)) license <- "proprietary"
	# 		if (identical(license, "proprietary")) use_proprietary_license()
	# 		else if (identical(license, "mit")) usethis::use_mit_license()
	# 		else if (identical(license, "gpl3")) usethis::use_gpl3_license()
	# 		else if (identical(license, "lgpl")) usethis::use_lgpl_license()
	# 		else if (identical(license, "apl2")) usethis::use_apl2_license()
	# 		else if (identical(license, "cc0")) usethis::use_cc0_license()
	# 		else if (identical(license, "ccby")) usethis::use_ccby_license()
	# 		else if (is.function(license)) license()
	# 		else if (!is.na(license)) stop("'", license, "' is not a recognised license type")
	# 	} else if (!is.null(license)) usethis::ui_warn("Ignoring argument {ui_code(license)} for non-package projects")
	#
	#   if (interactive()) {
	#   	usethis::proj_activate(path)
	#   	if (rstudioapi::isAvailable()) {
	#   		writeLines(
	#   			c(
	#   				readLines(fs::path_home_r(".Rprofile")),
	#   				"cat(crayon::bold('\\nThis project was created by `cmor.tools::create_research_project()`.\\n'))",
	#   				"cat('\\nTo complete project set-up, you need to:\\n')",
	#   				if (package) "cat(crayon::red('*'), 'Edit the DESCRIPTION file\\n')",
	#   				"cat(crayon::red('*'), 'Run `complete_setup()`\\n')",
	#   				"suppressMessages(require(cmor.tools))",
	#   				paste0("options(cmor.tools.git = ", git, ")"),
	#   				paste0("options(cmor.tools.git_data = ", data_in_git, ")"),
	#   				paste0("options(cmor.tools.git_rawdata = ", raw_data_in_git, ")"),
	#   				paste0("options(cmor.tools.git_output = ", output_in_git, ")"),
	#   				paste0("options(cmor.tools.github = ", github, ")"),
	#   				paste0("options(cmor.tools.github_private = ", private, ")"),
	#   				""
	#   			), usethis::proj_path(".Rprofile")
	#   		)
	#   		usethis::ui_todo("Go there to complete project set-up")
	#   		setwd(oldwd)
	#   	}
	#   	else {
	#   		usethis::ui_line("Next you need to:")
	#   		if (package) usethis::ui_todo("Edit the DESCRIPTION file")
	#   		usethis::ui_todo(
	#   			"Run `complete_setup()` to complete the project set-up"
	#   		)
	#   	}
	#   }
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
#' @param output_in_git Logical. If \code{FALSE} (the default), the
#'     \code{output} folder will be excluded from the git repository.
#' @param private Logical (default = \code{TRUE}). Should the GitHub repo be
#'     private or public? Ignored if \code{github = FALSE}.
#'
#' @export
complete_setup <- function(
	project = getwd(),
	git = getOption("cmor.tools.git"), raw_data_in_git = getOption("cmor.tools.git_rawdata"),
	data_in_git = getOption("cmor.tools.git_data"), output_in_git = getOption("cmor.tools.git_output"),
	github = getOption("cmor.tools.github"), private = getOption("cmor.tools.github_private")
) {
	invisible(file.remove('.Rprofile'))

	if (git) {
		# Initialise git repository
		use_git()

		if (github) {
			#usethis::use_git_credentials(git2r::cred_ssh_key())
			# Connect repository to github
			usethis::use_github(private = private)
		}
	}

	data <- list(
		name = fs::path_file(project),
		github = github
	)
	gh_info <- gh::gh("GET /repos/:owner/:repo", owner = github_info()$username,
										repo = github_info()$repo, .api_url = "https://api.github.com",
										.token = check_github_token(gh::gh_token(), allow_empty = TRUE))
	if (github) {
		data <- append(data, github_info(project))
		data <- append(data, list(url = gh_info$html_url))
	}

	# Create basic README file
	use_cmor_readme(data)

	if (git) {
		usethis::ui_todo("A restart of RStudio is required to activate the Git pane")
		usethis::ui_todo("You can do this by calling `rstudioapi::openProject()`")
	}
}
