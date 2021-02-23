###############################################################
### Various functions for the analysis of regression models ###
###############################################################

#' Linear combination of regression coefficients
#'
#' This function calculates the point estimate and standard error for a linear
#' combination of regression coefficients (similar to Stata's "lincom"
#' function). At this stage, only a single combination is calculated, and the
#' combination must be specified as a numeric vector of weights -- future
#' updates will extend this to allow multiple combinations and more flexible
#' methods of specifying coefficient combinations (similar to \code{car}'s
#' \code{linearHypothesis} function).
#'
#' @param model A regression model with \code{coef} and \code{vcov} methods.
#' @param weights A numeric vector of coefficient weights to construct the
#'     combination. Each element specifies the weight applied to the
#'     corresponding element of \code{attr(terms(model), "term.labels")}.
#' @param vcov. (Optional) Either a function for estimating the covariance
#'     matrix of the regression coefficients, or the covariance matrix itself.
#'     If not specified, the default \code{vcov} method is used.
#'
#' @export
lincom <- function(model, weights, vcov. = NULL) {
	if (is.null(vcov.)) vcov <- vcov(model)
	else if (is.function(vcov.)) vcov <- vcov.(model)
	else if (is.matrix(vcov.)) vcov <- vcov.
	else stop("Invalid 'vcov.' argument")

	coefs <- coef(model)

	b <- c(crossprod(weights, coefs))
	var <- tcrossprod(crossprod(weights, vcov), weights)

	list(b = b, se = sqrt(diag(var)))
}
