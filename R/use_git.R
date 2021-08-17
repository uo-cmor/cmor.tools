use_git <- function(message = "Initial commit") {
	if (uses_git()) {
		return(invisible(NULL))
	}
	usethis::ui_done("Initialising Git repo")
	git2r::init(usethis::proj_get())
	usethis::use_git_ignore(c(".Rhistory", ".RData", ".Rproj.user", ".Rprofile",
														".httr-oauth", ".DS_Store"))
	git_ask_commit(message, untracked = TRUE)
	invisible(TRUE)
}

git_ask_commit <- function(message, untracked = FALSE) {
	if (!interactive() || !uses_git()) return(invisible(NULL))
	paths <- unlist(
		git2r::status(untracked = untracked, repo = git2r::repository(usethis::proj_get())),
		use.names = FALSE
	)
	if (length(paths) == 0) return(invisible(NULL))
	paths <- sort(paths)
	ui_paths <- purrr::map_chr(usethis::proj_path(paths), usethis::ui_path)
	n <- length(ui_paths)
	if (n > 20) ui_paths <- c(ui_paths[1:20], "...")
	usethis::ui_line(c("There are {n} uncommitted files:", paste0("* ", ui_paths)))
	if (usethis::ui_yeah("Is it ok to commit them?")) {
		usethis::ui_done("Adding files")
		repo <- git2r::repository(usethis::proj_get())
		git2r::add(repo, paths)
		usethis::ui_done("Commit with message {usethis::ui_value(message)}")
		git2r::commit(repo, message)
		if (rstudioapi::hasFun("executeCommand")) {
			rstudioapi::executeCommand("vcsRefresh")
		}
	}
	invisible(NULL)
}

uses_git <- function() {
	repo <- tryCatch(
		gert::git_find(usethis::proj_get()),
		error = function(e) NULL
	)
	!is.null(repo)
}
