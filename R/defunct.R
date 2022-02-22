#' Format descriptive statistics tables
#'
#' @description
#' `r lifecycle::badge("defunct")`
#'
#' This function has been deprecated in favour of
#'     [formattr::create_descriptive_table()] and is now defunct.
#'
#' @keywords internal
#'
#' @param df,continuous,discrete,multiresponse,output,by,value_continuous,value_discrete,total
#'     See [formattr::create_descriptive_table()]
#'
#' @export
create_descriptive_table <- function(df, continuous, discrete, multiresponse, output, by,
																		 value_continuous, value_discrete, total) {
	lifecycle::deprecate_stop("0.6.0", "create_descriptive_table()",
														"formattr::create_descriptive_table()")
}

#' CMOR standard colour scales
#'
#' @description
#' `r lifecycle::badge("defunct")`
#'
#' These functions have been deprecated in favour of the functions in
#'     [CMORplots::scale_colour_cmor()] and are now defunct.
#'
#' @keywords internal
#'
#' @param ...,palette,direction,grey,aesthetics See [CMORplots::scale_colour_cmor()]
#'
#' @name scale_colour_cmor
NULL

#' @rdname scale_colour_cmor
#' @export
scale_colour_cmor <- function(..., palette, direction, grey, aesthetics) {
	lifecycle::deprecate_stop("0.7.0", "scale_colour_cmor()", "CMORplots::scale_colour_cmor()")
}

#' @rdname scale_colour_cmor
#' @export
scale_color_cmor <- function(..., palette, direction, grey, aesthetics) {
	lifecycle::deprecate_stop("0.7.0", "scale_color_cmor()", "CMORplots::scale_color_cmor()")
}

#' @rdname scale_colour_cmor
#' @export
scale_fill_cmor <- function(..., palette, direction, grey, aesthetics) {
	lifecycle::deprecate_stop("0.7.0", "scale_fill_cmor()", "CMORplots::scale_fill_cmor()")
}

#' Show available CMOR colour palettes
#'
#' @description
#' `r lifecycle::badge("defunct")`
#'
#' These functions have been deprecated in favour of the functions in
#'     [CMORplots::plot_cmor_colours()] and are now defunct.
#'
#' @keywords internal
#'
#' @export
plot_cmor_colours <- function() {
	lifecycle::deprecate_stop("0.7.0", "plot_cmor_colours()", "CMORplots::plot_cmor_colours()")
}

#' @rdname plot_cmor_colours
#' @export
plot_cmor_colors <- function() {
	lifecycle::deprecate_stop("0.7.0", "plot_cmor_colors()", "CMORplots::plot_cmor_colors()")
}

#' Standard `ggplot` Theme for CMOR Outputs
#'
#' @description
#' `r lifecycle::badge("defunct")`
#'
#' This function has been deprecated in favour of
#'     [CMORplots::theme_cmor_classic()] and is now defunct.
#'
#' @keywords internal
#'
#' @param base_size,base_family See [CMORplots::theme_cmor_classic()]
#'
#' @export
theme_cmor <- function(base_size, base_family) {
	lifecycle::deprecate_stop("0.7.0", "theme_cmor()", "CMORplots::theme_cmor_classic()")
}

#' SF-6D profile (SF-12 version)
#'
#' @description
#' `r lifecycle::badge("defunct")`
#'
#' This function has been deprecated in favour of [SF6Dvalues::sf6d_profile()]
#'     and is now defunct.
#'
#' @param ...,version,dimension See [SF6Dvalues::sf6d_profile()]
#'
#' @keywords internal
#'
#' @export
sf6d_profile <- function(..., version, dimension) {
	lifecycle::deprecate_stop("0.3.0", "sf6d_profile()", "SF6Dvalues::sf6d_profile()")
}

#' SF-6D utility values
#'
#' @description
#' `r lifecycle::badge("defunct")`
#'
#' This function has been deprecated in favour of [SF6Dvalues::utility()] (and
#'     [SF6Dvalues::sf6d_utility()]) and is now defunct.
#'
#' @param PF,RL,SF,PAIN,MH,VIT,values See [SF6Dvalues::utility()]
#'
#' @export
sf6d_utility <- function(PF, RL, SF, PAIN, MH, VIT, values) {
	lifecycle::deprecate_stop(
		"0.3.0", "sf6d_utility()", "SF6Dvalues::SF6D()",
		details = c(
			i = paste("You can also use SF6Dvalues::sf6d_profile() to calculate",
								"utility values directly from SF-12 responses")
		)
	)
}

#' SF-12 component summary scores
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function has been deprecated in favour of [SF6Dvalues::sf12_PCS()] and
#'     [SF6Dvalues::sf12_MCS()] and is now defunct.
#'
#' @param ...,version,dimension See [SF6Dvalues::SF12_scores]
#'
#' @export
sf12_scores <- function(..., version, dimension) {
	lifecycle::deprecate_stop(
		"0.3.0", "sf12_scores()",
		details = "Please use `SF6Dvalues::sf12_PCS()`/`SF6Dvalues::sf12_MCS()` instead."
	)
}

#' Results formatting for tables and reports
#'
#' @description
#' `r lifecycle::badge("defunct")`
#'
#' These functions have been deprecated in favour of the functions in
#'     [formattr::output-formatting].
#'
#' @keywords internal
#'
#' @param mean,sd,n,proportion,est,low,high,...,.sep,.glue See [formattr::output-formatting]
#'
#' @name output-formatting
NULL

#' @rdname output-formatting
#'
#' @export
mean_sd <- function(mean, sd, ..., .glue) {
	lifecycle::deprecate_stop("0.5.0", "mean_sd()", "formattr::mean_sd()")
}

#' @rdname output-formatting
#'
#' @export
n_percent <- function(n, proportion, ..., .glue) {
	lifecycle::deprecate_stop("0.5.0", "n_percent()", "formattr::n_percent()")
}

#' @rdname output-formatting
#'
#' @export
est_ci <- function(est, low, high, ..., .sep, .glue) {
	lifecycle::deprecate_stop("0.5.0", "est_ci()", "formattr::est_ci()")
}

#' Format numbers using `scales::number()`-type functions
#'
#' @description
#' `r lifecycle::badge("defunct")`
#'
#' These functions have been deprecated in favour of the functions in
#'     [formattr::number-formatting] and are now defunct.
#'
#' @keywords internal
#'
#' @param x,accuracy,scale,prefix,suffix,big.mark,decimal.mark,trim,...,html See [formattr::number-formatting]
#'
#' @name number-formatting
NULL

#' @rdname number-formatting
#' @export
number <- function(x, accuracy, scale, prefix, suffix, big.mark, decimal.mark, trim, html, ...) {
	lifecycle::deprecate_stop("0.5.0", "number()", "formattr::nmbr()")
}

#' @rdname number-formatting
#' @export
number_format <- function(accuracy, scale, prefix, suffix, big.mark, decimal.mark, trim, html, ...) {
	lifecycle::deprecate_stop("0.5.0", "number_format()", "formattr::nmbr()")
}

#' @rdname number-formatting
#' @export
percent <- function(x, accuracy, scale, prefix, suffix, big.mark, decimal.mark, trim, html, ...) {
	lifecycle::deprecate_stop("0.5.0", "percent()", "formattr::prct()")
}

#' @rdname number-formatting
#' @export
percent_format <- function(accuracy, scale, prefix, suffix, big.mark, decimal.mark, trim, html, ...) {
	lifecycle::deprecate_stop("0.5.0", "percent_format()", "formattr::prct()")
}

#' @rdname number-formatting
#' @export
comma <- function(x, accuracy, scale, prefix, suffix, big.mark, decimal.mark, trim, html, ...) {
	lifecycle::deprecate_stop("0.5.0", "comma()", "formattr::cmma()")
}

#' @rdname number-formatting
#' @export
comma_format <- function(accuracy, scale, prefix, suffix, big.mark, decimal.mark, trim, html, ...) {
	lifecycle::deprecate_stop("0.5.0", "comma_format()", "formattr::cmma()")
}

#' Create a research project using the CMOR project templates
#'
#' @description
#' `r lifecycle::badge("defunct")`
#'
#' This function has been deprecated in favour of
#'     [CMORprojects::create_research_project()] and is now defunct.
#'
#' @param path,workflow,git,raw_data_in_git,github,private,organisation,rstudio,open
#'     See [CMORprojects::create_research_project()]
#'
#' @export
create_research_project <- function(path, workflow, git, raw_data_in_git, github, private,
																		organisation, rstudio, open) {
	lifecycle::deprecate_stop("0.9.0", "create_research_project()",
														"CMORprojects::create_research_project()")
}

#' Target to render Rmd manuscript to docx
#'
#' @description
#' `r lifecycle::badge("defunct")`
#'
#' This function has been deprecated in favour of
#'     [CMORprojects::tar_render_manuscript()] and is now defunct.
#'
#' @param name,path,output_file,file_reference_docx,file_csl,file_bib,pandoc_args,render_args,packages,library,error,deployment,priority,resources,retrieval,cue,quiet
#'     See [CMORprojects::tar_render_manuscript()].
#' @export
tar_render_manuscript <- function(name, path, output_file, file_reference_docx, file_csl, file_bib,
																	pandoc_args, render_args, packages, library, error, deployment,
																	priority, resources, retrieval, cue, quiet) {
	lifecycle::deprecate_stop("0.9.0", "tar_render_manuscript()",
														"CMORprojects::tar_render_manuscript()")
}

#' Linear combination of regression coefficients
#'
#' @description
#' `r lifecycle::badge("defunct")`
#'
#' This function has been deprecated in favour of
#'     [regtools::lincom()] and is now defunct.
#'
#' @param model,weights,vcov. See [regtools::lincom()].
#' @export
lincom <- function(model, weights, vcov.) {
	lifecycle::deprecate_stop("0.9.0", "lincom()", "regtools::lincom()")
}

#' Compute confidence interval bounds from confidence level
#'
#' @description
#' `r lifecycle::badge("defunct")`
#'
#' This function has been deprecated in favour of
#'     [regtools::calculate_ci()] and is now defunct.
#'
#' @param conf.level See [regtools::calculate_ci()].
#' @export
calculate_ci <- function(conf.level) {
	lifecycle::deprecate_stop("0.9.0", "calculate_ci()", "regtools::calculate_ci()")
}
