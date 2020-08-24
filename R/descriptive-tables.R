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
#' @param multiresponse List of character vectors, each specifying the
#'     individual variables making up a combined multiple-response variable.
#'     Should be named, with the names giving the variable names to be used
#'     for the combined variables.
#' @param output (optional) Character vector of variables to include in the
#'     table (and their order). Elements should be the names of elements of
#'     `continuous` or `discrete`. Default is to report all variables from
#'     `continuous` followed by `discrete`.
#' @param by (optional) A grouping variable (as a character string) to split
#'     the sample. Default is to report for the entire sample.
#' @param value_continuous (optional) An expression describing how to format
#'     the outcome values for continuous variables. Can refer to
#'     mean/sd/min/lq/median/uq/max as `.mean`, `.sd`, etc, and the name of the
#'     variable being formatted as `.variable`. Default is to report mean (SD)
#'     to one decimanl place.
#' @param value_discrete (optional) An expression describing how to format the
#'     outcome values for discrete variables. Can refer to n/proportion as
#'     `.n`, `.proportion`, and the name of the variable being formatted as
#'     `.variable`. Default is to report n (percentage) to zero decimal places.
#' @param total (optional) Logical value; should a 'total' (full sample) column
#'     be included? Default is `TRUE` if `by` is specified, `FALSE` otherwise.
#'     Can also be a character string giving the column name to use for the
#'     total column (default is 'Entire sample').
#'
#' @export
create_descriptive_table <- function(df, continuous = NULL, discrete = NULL, multiresponse = NULL,
																		 output = c(names(continuous), names(discrete)), by = NULL,
																		 value_continuous = mean_sd(.mean, .sd, accuracy = 0.1),
																		 value_discrete = n_percent(.n, .proportion), total = !is.null(by)) {
	value_continuous <- rlang::enquo(value_continuous)
	value_discrete <- rlang::enquo(value_discrete)

	if (isTRUE(total)) total <- 'Entire sample'
	if (isFALSE(total)) total <- NULL

	if (has_names(continuous)) names(continuous) <- dplyr::if_else(names(continuous) == '', unname(continuous),
																																dplyr::coalesce(names(continuous), unname(continuous)))
	else names(continuous) <- continuous

	if (has_names(discrete)) names(discrete) <- dplyr::if_else(names(discrete) == '', unname(discrete),
																														dplyr::coalesce(names(discrete), unname(discrete)))
	else names(discrete) <- discrete

	for (x in multiresponse) {
		if (has_names(x)) names(x) <- dplyr::if_else(names(x) == '', unname(x), dplyr::coalesce(names(x), unname(x)))
		else names(x) <- x
	}

	variable_names <- c(names(continuous), names(discrete), names(multiresponse))

	continuous_tables <- purrr::imap(continuous,
																	 ~continuous_characteristics_table(df, by, .x, .y, value_continuous, total))
	discrete_tables <- purrr::imap(discrete,	~discrete_characteristics_table(df, by, .x, .y, value_discrete, total))
	multiresponse_tables <- purrr::imap(multiresponse,
																			~multiresponse_characteristics_table(df, by, .x, .y, value_discrete, total))

	header_rows <- purrr::map(setNames(nm = setdiff(output, variable_names)), make_header_row)

	dplyr::bind_rows(c(continuous_tables, discrete_tables, multiresponse_tables, header_rows)[output])
}

continuous_characteristics_table <- function(df, by, variable, name, value, total) {
	out <- dplyr::group_by_at(df, dplyr::vars(!!by)) %>%
		dplyr::summarise_at(dplyr::vars(!!variable),
												list(.mean = mean, .sd = sd, .min = min, .lq = lq, .median = median, .uq = uq, .max = max),
												na.rm = TRUE) %>%
		dplyr::mutate(.variable = !!variable, value = !!value) %>%
		dplyr::select(!dplyr::any_of(c(".mean", ".sd", ".min", ".lq", ".median", ".uq", ".max", ".variable")))
	if (!is.null(by)) out <- tidyr::pivot_wider(out, names_from = !!by)
	if (!is.null(total)) out <- dplyr::bind_cols(
		out,
		dplyr::summarise_at(df, dplyr::vars(!!variable),
												list(.mean = mean, .sd = sd, .min = min, .lq = lq, .median = median, .uq = uq, .max = max),
												na.rm = TRUE) %>%
			dplyr::mutate(.variable = !!variable) %>%
			dplyr::transmute(!!total := !!value)
	)
	out %>%
		dplyr::mutate('Patient characteristic' = !!name) %>%
		dplyr::select(dplyr::all_of('Patient characteristic'), dplyr::everything())
}

discrete_characteristics_table <- function(df, by, variable, name, value, total) {
	out <- tidyr::drop_na(df, !!by, !!variable) %>%
		dplyr::group_by_at(dplyr::vars(!!by, 'Patient characteristic' = !!variable)) %>%
		dplyr::summarise(.n = dplyr::n()) %>%
		tidyr::drop_na() %>%
		dplyr::mutate(.proportion = .n / sum(.n), .variable = !!variable, value = !!value,
									'Patient characteristic' = paste0('\u200B\u2003', as.character(`Patient characteristic`))) %>%
		dplyr::ungroup() %>%
		dplyr::select(!dplyr::any_of(c(".n", ".proportion",  ".variable")))
	if (!is.null(by)) out <- tidyr::pivot_wider(out, id_cols = 'Patient characteristic', names_from = !!by)
	out <- out %>% dplyr::add_row('Patient characteristic' = !!name, .after = 0)
	if (!is.null(total)) out <- dplyr::bind_cols(
		out,
		tidyr::drop_na(df, !!by, !!variable) %>%
			dplyr::group_by_at(dplyr::vars(!!variable)) %>%
			dplyr::summarise(.n = dplyr::n()) %>%
			tidyr::drop_na() %>%
			dplyr::mutate(.proportion = .n / sum(.n), .variable = !!variable) %>%
			dplyr::ungroup() %>%
			dplyr::transmute(!!total := !!value) %>%
			dplyr::add_row(!!total := NA_character_, .after = 0)
	)

	out
}

multiresponse_characteristics_table <- function(df, by, variables, name, value, total) {
	purrr::imap_dfr(
		variables,
		function(variable, label) {
			out <- dplyr::group_by_at(df, dplyr::vars(!!by)) %>%
				dplyr::summarise_at(dplyr::vars(!!variable), list(.n = sum, .proportion = mean), na.rm = TRUE) %>%
				dplyr::mutate(.variable = !!variable, value = !!value) %>%
				dplyr::select(!dplyr::any_of(c(".n", ".proportion",  ".variable")))
			if (!is.null(by)) out <- tidyr::pivot_wider(out, names_from = !!by)
			if (!is.null(total)) out <- dplyr::bind_cols(
				out,
				dplyr::summarise_at(df, dplyr::vars(!!variable), list(.n = sum, .proportion = mean), na.rm = TRUE) %>%
					dplyr::mutate(.variable = !!variable) %>%
					dplyr::transmute(!!total := !!value)
			)

			out %>%
				dplyr::mutate('Patient characteristic' = paste0('\u200B\u2003', !!label)) %>%
				dplyr::select(dplyr::all_of('Patient characteristic'), dplyr::everything())
		}
	) %>%
		dplyr::add_row('Patient characteristic' := !!name, .after = 0)
}

make_header_row <- function(x) tibble::tibble('Patient characteristic' = !!x)
