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
#' @param pkgname Character string
check_package_name <- function(pkgname) {
	if (!valid_package_name(pkgname))
		usethis::ui_stop(
			c("'{ui_value(pkgname)}' is not a valid package name. It should:",
				"* Contain only ASCII letters, numbers, and '.'",
				"* Have at least two characters",
				"* Start with a letter",
				"* Not end with '.'")
		)
}

uses_git <- function(path = ".") {
	!is.null(git2r::discover_repository(path, ceiling = 0))
}

uses_github <- function(path = ".") {
	if (!uses_git(path)) return(FALSE)

	r <- git2r::repository(path, discover = TRUE)
	r_remote_urls <- git2r::remote_url(r)

	any(grepl("github", r_remote_urls))
}

github_info <- function(path = ".", remote_name = NULL) {
	if (!uses_github(path)) return(github_dummy)

	r <- git2r::repository(path, discover = TRUE)
	r_remote_urls <- grep("github", remote_urls(r), value = TRUE)
	if (!is.null(remote_name) && !remote_name %in% names(r_remote_urls))
		stop("no github-related remote named ", remote_name, " found")
	remote_name <- c(remote_name, "origin", names(r_remote_urls))
	x <- r_remote_urls[remote_name]
	x <- x[!is.na(x)][1]

	github_remote_parse(x)
}

github_dummy <- list(username = "<USERNAME>", repo = "<REPO>", fullname = "<USERNAME>/<REPO>")

github_remote_parse <- function(x) {
	if (length(x) == 0) return(github_dummy)
	if (!grepl("github", x)) return(github_dummy)
	if (grepl("^(https|git)", x)) re <- "github[^/:]*[/:]([^/]+)/(.*?)(?:\\.git)?$"
	else stop("Unknown GitHub repo format", call. = FALSE)

	m <- regexec(re, x)
	match <- regmatches(x, m)[[1]]

	list(username = match[2], repo = match[3], fullname = paste0(match[2], "/", match[3]))
}

check_github_token <- function (auth_token = usethis::github_token(), allow_empty = FALSE) {
	if (allow_empty && isTRUE(auth_token == "")) {
		return(invisible(auth_token))
	}
	local_stop <- function(msg) {
		usethis::ui_stop(
			c(msg, "See {usethis::ui_code('browse_github_token()')} for help storing a token as an environment variable.")
		)
	}
	if (!is_string(auth_token) || is.na(auth_token)) {
		local_stop("GitHub {usethis::ui_code('auth_token')} must be a single string.")
	}
	if (isTRUE(auth_token == "")) {
		local_stop("No GitHub {usethis::ui_code('auth_token')} is available.")
	}
	user <- github_user(auth_token)
	if (is.null(user)) {
		local_stop("GitHub {usethis::ui_code('auth_token')} is invalid.")
	}
	invisible(auth_token)
}

github_user <- function(auth_token = usethis::github_token()) {
	suppressMessages(tryCatch(gh::gh_whoami(auth_token), error = function(e) NULL))
}

remote_urls <- function(r) {
	remotes <- git2r::remotes(r)

	stats::setNames(git2r::remote_url(r, remotes), remotes)
}

restart_rstudio <- function(message = NULL) {
	if (!in_rstudio(usethis::proj_get())) {
		return(FALSE)
	}
	if (!interactive()) {
		return(FALSE)
	}
	if (!is.null(message)) {
		usethis::ui_todo(message)
	}
	if (!rstudioapi::hasFun("openProject")) {
		return(FALSE)
	}
	if (usethis::ui_nope("Restart now?")) {
		return(FALSE)
	}
	rstudioapi::openProject(usethis::proj_get())
}

in_rstudio <- function(base_path = usethis::proj_get()) {
	if (!rstudioapi::isAvailable()) return(FALSE)
	if (!rstudioapi::hasFun("getActiveProject")) return(FALSE)
	proj <- rstudioapi::getActiveProject()
	if (is.null(proj)) return(FALSE)
	fs::path_real(proj) == fs::path_real(base_path)
}

is_string <- function(x) length(x) == 1 && is.character(x)

is_package <- function(path) {
	res <- tryCatch(rprojroot::find_package_root_file(path = path), error = function(e) NULL)
	!is.null(res)
}
