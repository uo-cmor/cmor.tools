#' Create a research project using the CMOR project templates
#'
#' These function creates an R project, adds a basic README,
#'     initialises our standard research project directory structure and
#'     template files, and creates a git repository with an initial commit.
#'
#' @param path Path to the new project.
#' @param workflow Character string: \code{"targets"}, \code{"drake"} or
#'     \code{"make"}, to create corresponding workflow template files.
#' @param git Logical (default = \code{TRUE}). Whether to create a git
#'     repository.
#' @param raw_data_in_git Logical (default = \code{TRUE}). If FALSE, data in the
#'     \code{data/raw_data/} directory will be excluded from the git repository.
#' @param github Logical. If \code{TRUE} (the default), create a GitHub
#'     repository and push the initial commit to GitHub.
#' @param private Logical. If \code{TRUE} (the default), a private GitHub
#'     repository will be created; otherwise, the repository will be publicly
#'     accessible. Ignored if \code{github = FALSE}.
#' @param organisation (Optional) Passed to \code{usethis::use_github()} to
#'     create the repo under this organisation instead of the login associated
#'     with the discovered GitHub token.
#' @param rstudio If \code{TRUE}, makes the new project into an Rtudio Project.
#' @param open If \code{TRUE}, activates the new project in a new RStudio
#'     session.
#'
#' @export
create_research_project <- function(
	path, workflow = "targets", git = TRUE, raw_data_in_git = TRUE, github = TRUE, private = TRUE,
	organisation = NULL, rstudio = rstudioapi::isAvailable(), open = rlang::is_interactive()
) {
	checkmate::assert_string(path)
	checkmate::assert_choice(workflow, c("targets", "drake", "make"))
	checkmate::assert_flag(git)
	checkmate::assert_flag(raw_data_in_git)
	checkmate::assert_flag(github)
	checkmate::assert_flag(private)
	checkmate::assert_string(organisation, null.ok = TRUE)
	checkmate::assert_flag(rstudio)
	checkmate::assert_flag(open)

	path <- fs::path_expand(path)
	if (!fs::dir_exists(path)) {
		fs::dir_create(path)
		usethis::ui_done("The project directory {usethis::ui_path(path)} has been created.")
	} else {
		usethis::ui_stop(c(
			"The directory {usethis::ui_path(path)} already exists.",
			glue::glue("{crayon::red(cli::symbol$cross)} ",
								 "{usethis::ui_code('create_research_project()')} ",
								 "will not overwrite an existing directory.")
		))
	}

	if (!git & github) {
		github <- FALSE
		github_warn <- TRUE
		usethis::ui_warn(glue::glue(
			"Argument {usethis::ui_code('github')} was set to {usethis::ui_code('TRUE')}, ",
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
				"",
				"usethis::ui_line(crayon::bold('Initial project setup:'))",
				paste0("cmor.tools::use_project_directory(package = FALSE, ",
							 "git = ", git, ", raw_data_in_git = ", raw_data_in_git, ")"),
				if (git) "cmor.tools:::use_git()",
				if (github) {
					if (!is.null(organisation))
						paste0("if (usethis::ui_yeah('Is it OK to push these to `",
									 organisation, "` on GitHub?')) ",
									 "usethis::use_github(private = ", private,
									 ", organisation = '", organisation, "')")
					else
						paste0("if (usethis::ui_yeah('Is it OK to push these to GitHub?')) ",
									 "usethis::use_github(private = ", private, ")")
				},
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
				"rm(list = 'data')",
				"",

				## Welcome message
				"usethis::ui_line()",
				paste0("usethis::ui_line(crayon::bold('This project was created by ",
							 "`cmor.tools::create_research_project()`.'))"),
				paste0("usethis::ui_todo('Edit the README.Rmd file to provide an ",
							 "introduction to the project')"),
				paste0("usethis::ui_todo('Remember to render README.Rmd to README.md for ",
							 "GitHub.')"),
				paste0("usethis::ui_todo('Put code in `/R`, raw data in `/raw_data`, and ",
							 "RMarkdown reports in `/reports`.')"),
				if (workflow == "targets") c(
					paste0("usethis::ui_todo('Use `_targets.R`, `_plan.R`, and `parameters.R` ",
								 "to specify the analysis workflow.')"),
					paste0("usethis::ui_info('(See https://books.ropensci.org/targets/ for more ",
								 "information on the `targets` package.)')")
				),
				".Last <- function() invisible(fs::file_delete('.Rprofile'))"
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
}
