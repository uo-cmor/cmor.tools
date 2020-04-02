###################################################################################################
### Functions to create formatted descriptive tables for both continuous and discrete variables ###
###################################################################################################

#' Format descriptive statistics tables
#'
#' Construct nicely formatted descriptive statistics tables (mean/sd, n/percent).
#'
#' @param df A data frame containing the variables to describe.
#' @param continuous,discrete Character vectors of continuous and discrete
#'     variables for which to calculate descriptive statistics. Can be named,
#'     in which case the provided names will be used in place of variable names
#'     in the first column of the table.
#' @param output (optional) Character vector of variables to include in the
#'     table (and their order). Elements should be the names of elements of
#'     `continuous` or `discrete`. Default is to report all variables from
#'     `continuous` followed by `discrete`.
#' @param by (optional) A grouping variable (as a character string) to split
#'     the sample. Default is to report for the entire sample.
#' @param value_continuous (optional) An expression describing how to format
#'     the outcome values for continuous variables. Can refer to
#'     mean/sd/min/lq/median/uq/max as `.mean`, `.sd`, etc. Default is to
#'     report mean (SD) to one decimanl place.
#' @param value_discrete (optional) An expression describing how to format the
#'     outcome values for discrete variables. Can refer to n/proportion as
#'     `.n`, `.proportion`. Default is to report n (percentage) to zero decimal
#'     places.
#' @param total (optional) Logical value; should a 'total' (full sample) column
#'     be included? Default is `TRUE` if `by` is specified, `FALSE` otherwise.
#'     Can also be a character string giving the column name to use for the
#'     total column (default is 'Entire sample').
#'
#' @export
create_descriptive_table <- function(df, continuous, discrete, output = c(names(continuous), names(discrete)),
																		 by = NULL, value_continuous = mean_sd(.mean, .sd, accuracy = 0.1),
																		 value_discrete = n_percent(.n, .proportion), total = !is.null(by)) {
	value_continuous <- rlang::enquo(value_continuous)
	value_discrete <- rlang::enquo(value_discrete)

	if (isTRUE(total)) total <- 'Entire sample'
	if (isFALSE(total)) total <- NULL

	if (is_named(continuous)) names(continuous) <- dplyr::if_else(names(continuous) == '', continuous,
																																dplyr::coalesce(names(continuous), continuous))
	else names(continuous) <- continuous

	if (is_named(discrete)) names(discrete) <- dplyr::if_else(names(discrete) == '', discrete,
																														dplyr::coalesce(names(discrete), discrete))
	else names(discrete) <- discrete

	continuous_tables <- purrr::imap(continuous,
																	 ~continuous_characteristics_table(df, by, .x, .y, value_continuous, total))
	discrete_tables <- purrr::imap(discrete,	~discrete_characteristics_table(df, by, .x, .y, value_discrete, total))

	dplyr::bind_rows(c(continuous_tables, discrete_tables)[output])
}

continuous_characteristics_table <- function(df, by, variable, name, value, total) {
	out <- dplyr::group_by_at(df, dplyr::vars(!!by)) %>%
		dplyr::summarise_at(dplyr::vars(!!variable),
												list(.mean = mean, .sd = sd, .min = min, .lq = lq, .median = median, .uq = uq, .max = max),
												na.rm = TRUE) %>%
		dplyr::mutate(value = !!value) %>%
		dplyr::select(-.mean, -.sd, -.min, -.lq, -.median, -.uq, -.max)
	if (!is.null(by)) out <- tidyr::pivot_wider(out, names_from = !!by)
	if (!is.null(total)) out <- dplyr::bind_cols(
		out,
		dplyr::summarise_at(df, dplyr::vars(!!variable),
												list(.mean = mean, .sd = sd, .min = min, .lq = lq, .median = median, .uq = uq, .max = max),
												na.rm = TRUE) %>%
			dplyr::transmute(!!total := !!value)
	)
	out %>%
		dplyr::mutate('Patient characteristic' = !!name) %>%
		dplyr::select('Patient characteristic', dplyr::everything())
}

discrete_characteristics_table <- function(df, by, variable, name, value, total) {
	out <- tidyr::drop_na(df, !!by, !!variable) %>%
		dplyr::group_by_at(dplyr::vars(!!by, 'Patient characteristic' = !!variable)) %>%
		dplyr::summarise(.n = n()) %>%
		tidyr::drop_na() %>%
		dplyr::mutate(.proportion = .n / sum(.n),
					 value = !!value,
					 'Patient characteristic' = paste0('\u2003', as.character(`Patient characteristic`))) %>%
		dplyr::ungroup() %>%
		dplyr::select(-.n, -.proportion)
	if (!is.null(by)) out <- tidyr::pivot_wider(out, id_cols = 'Patient characteristic', names_from = !!by)
	out <- out %>% dplyr::add_row('Patient characteristic' = !!name, .after = 0)
	if (!is.null(total)) out <- dplyr::bind_cols(
		out,
		tidyr::drop_na(df, !!by, !!variable) %>%
			dplyr::group_by_at(dplyr::vars(!!variable)) %>%
			dplyr::summarise(.n = n()) %>%
			tidyr::drop_na() %>%
			dplyr::mutate(.proportion = .n / sum(.n)) %>%
			dplyr::ungroup() %>%
			dplyr::transmute('Entire sample' = !!value) %>%
			dplyr::add_row('Entire sample' = NA_character_, .after = 0)
	)

	out
}
