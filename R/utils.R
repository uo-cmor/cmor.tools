#' Ensure only one non-missing value is supplied in a list of arguments
#'
#' @param ... Arguments to be checked. Only one of these should be non-\code{NULL}.
#'
#' @return The single non-\code{NULL} value passed to \code{...}
#'
#' @export
check_args <- function(...) {
	vars <- list(...)
	if (is.null(names(vars))) nm <- paste0("V", 1:(...length()))
	else nm <- dplyr::coalesce(names(vars), paste0("V", 1:(...length())))
	miss <- purrr::map_lgl(vars, is.null)
	if (sum(!miss) == 0)
		usethis::ui_stop(glue::glue("One of {glue::glue_collapse(usethis::ui_code(nm), ",
																"sep = ', ', last = ', or ')} must be provided."))
	if (sum(!miss) > 1) {
		nm_dup <- nm[!miss]
		usethis::ui_stop(glue::glue("Only one of {glue::glue_collapse(usethis::ui_code(nm_dup), ",
																"sep = ', ', last = ', or ')} should be provided."))
	}

	vars[!miss][[1]]
}

#' @importFrom magrittr %>%
NULL

#' @importFrom stats median quantile sd setNames
NULL

#' @importFrom rlang :=
NULL

lq <- function(x, ...) quantile(x, probs = 0.25, ...)
uq <- function(x, ...) quantile(x, probs = 0.75, ...)


#' Compute confidence interval bounds from confidence level
#'
#' @param conf.level Confidence level, expressed as a proportion in (0, 1); default is 0.95.
#'
#' @export
calculate_ci <- function(conf.level = 0.95) {
	checkmate::assert_number(conf.level, lower = 0, upper = 1)

	c((1 - conf.level) / 2, 1 - (1 - conf.level) / 2)
}

#' Does object have names?
#'
#' Similar to `rlang::is_named()`, but allows missing values for some names
#'
#' @param x An object to test
#'
#' @export
has_names <- function(x) {
	checkmate::assert_vector(x, null.ok = TRUE)
	nms <- names(x)
	if (rlang::is_null(nms)) return(FALSE)
	if (all(nms == "" | is.na(nms))) return(FALSE)
	TRUE
}

possibly_in_proj <- function(path = ".") !is.null(proj_find(path))

proj_find <- function(path = ".") {
	tryCatch(
		rprojroot::find_root(proj_crit(), path = path),
		error = function(e) NULL
	)
}

proj_crit <- function() {
	rprojroot::has_file(".here") |
		rprojroot::is_rstudio_project |
		rprojroot::is_r_package |
		rprojroot::is_git_root |
		rprojroot::is_remake_project |
		rprojroot::is_projectile_project
}

is_package <- function(base_path = usethis::proj_get()) {
	res <- tryCatch(
		rprojroot::find_package_root_file(path = base_path),
		error = function(e) NULL
	)
	!is.null(res)
}
