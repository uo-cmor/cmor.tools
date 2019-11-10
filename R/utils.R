#' Replace NULLs with a default value
#'
#' This is just \code{rlang::`%||%`}.
#'
#' @param x,y If \code{x} is \code{NULL} will return \code{y}, otherwise returns \code{x}
#'
#' @export
`%||%` <- function(x, y) if (is.null(x)) y else x

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
