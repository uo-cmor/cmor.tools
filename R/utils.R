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
	miss <- vapply(list(...), is.null, TRUE)
	if (sum(!miss) == 0) stop (paste0("No non-missing values provided for {", paste(nm, collapse = ", "), "}"))
	if (sum(!miss) > 1) stop (paste0("More than one value provided for {", paste(nm, collapse = ", "), "}"))

	vars[!miss][[1]]
}

#' @importFrom magrittr %>%
NULL

lq <- function(x, ...) quantile(x, probs = 0.25, ...)
uq <- function(x, ...) quantile(x, probs = 0.75, ...)


#' Compute confidence interval bounds from confidence level
#'
#' @param conf.level Confidence level, expressed as a proportion in (0, 1); default is 0.95.
#'
#' @export
calculate_ci <- function(conf.level = 0.95) {
	stopifnot(is.numeric(conf.level), length(conf.level) == 1, conf.level > 0 & conf.level < 1)

	c((1 - conf.level) / 2, 1 - (1 - conf.level) / 2)
}
