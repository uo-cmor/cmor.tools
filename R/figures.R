#' CMOR standard colour scales
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' These functions are deprecated in favour of the functions in
#'     \code{\link[CMORplots]{scale_colour_cmor}}.
#'
#' @keywords internal
#'
#' Several standard colour schemes for CMOR outputs. Only qualitative
#'     (including paired) scales are currently implemented (sequential,
#'     diverging, and continuous scales are planned).
#'
#' All included palettes are colour-blind friendly.
#'
#' Sources:
#'
#' \code{CUD}: 'Color Universal Design' palette
#'     (https://jfly.uni-koeln.de/color/#pallet)
#'
#' \code{Bright}, \code{High-Contrast}, \code{Vibrant}, \code{Muted},
#'     \code{Medium-Contrast}, \code{Light}:
#'     https://personal.sron.nl/~pault/#sec:qualitative
#'
#' \code{Paired}: ColorBrewer 4-class PuOr
#'     (https://colorbrewer2.org/#type=diverging&scheme=PuOr&n=4)
#'
#' @param ... Passed on to \code{\link[ggplot2]{discrete_scale}} to control
#'     name, limits, breaks, etc.
#' @param palette If a string, will use that named palette; if a number, will
#'     index into the list of available palettes.
#' @param direction Sets the order of colours in the scale (1 for standard, -1
#'     for reversed).
#' @param grey If \code{TRUE}, a grey/black colour will always be used as the
#'     last group colour (regardless of the number of groups in the data). If
#'     \code{FALSE} (the default), the colours and their order is as in the
#'     original source palette.
#' @param aesthetics Which aesthetic(s) should the scale work with. This can be
#'     useful, for example, to apply the same settings to both the
#'     \code{colour} and \code{fill} aesthetics simultaneously.
#'
#' @name scale_colour_cmor
NULL

#' @rdname scale_colour_cmor
#' @export
scale_colour_cmor <- function(..., palette = 1, direction = 1, grey = FALSE,
															aesthetics = "colour") {
	lifecycle::deprecate_stop("0.7.0", "scale_colour_cmor()", "CMORplots::scale_colour_cmor()")

	# ggplot2::discrete_scale(aesthetics, "cmor", cmor_pal(palette, direction, grey), ...)
}

#' @rdname scale_colour_cmor
#' @export
scale_color_cmor <- function(..., palette = 1, direction = 1, grey = FALSE,
														 aesthetics = "colour") {
	lifecycle::deprecate_stop("0.7.0", "scale_color_cmor()", "CMORplots::scale_color_cmor()")

	# ggplot2::discrete_scale(aesthetics, "cmor", cmor_pal(palette, direction, grey), ...)
}

#' @rdname scale_colour_cmor
#' @export
scale_fill_cmor <- function(..., palette = 1, direction = 1, grey = FALSE,
														aesthetics = "fill") {
	lifecycle::deprecate_stop("0.7.0", "scale_fill_cmor()", "CMORplots::scale_fill_cmor()")

	# ggplot2::discrete_scale(aesthetics, "cmor", cmor_pal(palette, direction, grey), ...)
}

#' Show available CMOR colour palettes
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' These functions are deprecated in favour of the functions in
#'     \code{\link[CMORplots]{plot_cmor_colours}}.
#'
#' @keywords internal
#'
#' Plot the currently available CMOR colour palettes.
#'
#' @param palettes Optional character vector of palettes to include in the
#'     plot. Default is to include all available colour palettes.
#'
#' @export
plot_cmor_colours <- function(palettes) {
	lifecycle::deprecate_stop("0.7.0", "plot_cmor_colours()", "CMORplots::plot_cmor_colours()")

	# if (!missing(palettes)) {
	# 	pal <- cmor_pal_name(palettes)
	# 	pal <- cmor_palettes[pal]
	# } else pal <- cmor_palettes
	#
	# dplyr::bind_rows(lapply(pal, function(x) data.frame(value = x)), .id = "palette") %>%
	# 	dplyr::group_by(palette = factor(.data$palette, rev(unique(.data$palette)))) %>%
	# 	dplyr::mutate(y = dplyr::row_number()) %>%
	# 	dplyr::ungroup() %>%
	# 	ggplot2::ggplot(ggplot2::aes(.data$palette, .data$y, fill = .data$value)) +
	# 	ggplot2::geom_tile(width = 0.8, linetype = 1, colour = "black") +
	# 	ggplot2::scale_fill_identity() +
	# 	ggplot2::scale_x_discrete(NULL) +
	# 	ggplot2::scale_y_discrete(NULL) +
	# 	ggplot2::coord_flip() +
	# 	ggplot2::theme_void() +
	# 	ggplot2::theme(axis.text = ggplot2::element_text(hjust = 1))
}

#' @rdname plot_cmor_colours
#' @export
plot_cmor_colors <- function(palettes) {
	lifecycle::deprecate_stop("0.7.0", "plot_cmor_colors()", "CMORplots::plot_cmor_colors()")

	# plot_cmor_colours(palettes)
}

#' Standard `ggplot` Theme for CMOR Outputs
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated in favour of
#'     \code{\link[CMORplots]{theme_cmor_classic}}.
#'
#' @keywords internal
#'
#' This is a preliminary draft of a standard CMOR ggplot theme (just directly
#'     calls `theme_classic()` for now).
#'
#' @param base_size Base font size, in pts
#' @param base_family Base font family
#'
#' @export
theme_cmor <- function(base_size = 11, base_family = "") {
	lifecycle::deprecate_stop("0.7.0", "theme_cmor()", "CMORplots::theme_cmor_classic()")

	# ggplot2::theme_classic(base_size = base_size, base_family = base_family)
}

# cmor_pal <- function(palette = "CUD", direction = 1, grey = FALSE) {
# 	## Options to be added (?):
# 	### `type` String: similar to ColorBrewer's `type`
# 	pal <- cmor_pal_name(palette)
# 	force(direction)
# 	function(n) {
# 		max_n <- max_pal_n(pal)
# 		check_pal_n(n, max_n, pal)
# 		if (grey) n <- n - 1
# 		ret <- cmor_palettes[[pal]][seq_len(n)]
# 		if (direction == -1) ret <- rev(ret)
# 		if (grey) ret <- c(ret, cmor_palette_greys[[pal]])
# 		ret
# 	}
# }
#
# cmor_pal_name <- function(palette) {
# 	if (checkmate::test_string(palette))
# 		checkmate::assert_choice(palette, names(cmor_palettes))
# 	else if (checkmate::test_count(palette, positive = TRUE)) {
# 		if (!checkmate::test_int(palette, upper = 8))
# 			stop("Only 8 colour palettes are defined.\n",
# 					 crayon::red(cli::symbol$cross), " You've provided `palette = ", palette, "`.")
# 		names(cmor_palettes)[palette]
# 	} else stop("Unknown 'palette' value")
# }
#
# max_pal_n <- function(pal) {
# 	length(cmor_palettes[[pal]])
# }
#
# check_pal_n <- function(n, max_n, pal) {
# 	if (n > max_n) stop("Palette '", pal, "' has a maximum of ", max_n, " colours.\n",
# 											crayon::red(cli::symbol$cross), " You've provided `n = ", n, "`.")
# }
#
# cmor_palettes <- list(
# 	CUD = grDevices::rgb(c(230, 86, 0, 240, 0, 213, 204, 0),
# 											 c(159, 180, 158, 228, 114, 94, 121, 0),
# 											 c(0, 233, 115, 66, 178, 0, 167, 0), maxColorValue = 255),
# 	Bright = grDevices::rgb(c(68, 238, 34, 204, 102, 170, 187),
# 													c(119, 102, 136, 187, 204, 51, 187),
# 													c(170, 119, 51, 68, 238, 119, 187), maxColorValue = 255),
# 	"High-Contrast" = grDevices::rgb(c(0, 221, 187, 0), c(68, 170, 85, 0), c(136, 51, 102, 0),
# 																	 maxColorValue = 255),
# 	Vibrant = grDevices::rgb(c(238, 0, 51, 238, 204, 0, 187),
# 													 c(119, 119, 187, 51, 51, 153, 187),
# 													 c(51, 187, 238, 119, 17, 136, 187), maxColorValue = 255),
# 	Muted = grDevices::rgb(c(204, 51, 221, 17, 136, 136, 68, 153, 170, 221),
# 												 c(102, 34, 204, 119, 204, 34, 170, 153, 68, 221),
# 												 c(119, 136, 119, 51, 238, 85, 153, 51, 153, 221), maxColorValue = 255),
# 	"Medium-Contrast" = grDevices::rgb(c(102, 0, 238, 153, 238, 153, 255, 0),
# 																		 c(153, 68, 204, 119, 153, 68, 255, 0),
# 																		 c(204, 136, 102, 0, 170, 85, 255, 0), maxColorValue = 255),
# 	Light = grDevices::rgb(c(119, 238, 238, 255, 153, 68, 187, 170, 221),
# 												 c(170, 136, 221, 170, 221, 187, 204, 170, 221),
# 												 c(221, 102, 136, 187, 255, 153, 51, 0, 221), maxColorValue = 255),
# 	Paired = grDevices::rgb(c(178, 94, 253, 230, 247),
# 													c(171, 60, 187, 97, 247),
# 													c(210, 153, 99, 1, 247), maxColorValue = 255)
# )
#
# cmor_palette_greys <- list(
# 	CUD = grDevices::rgb(0, 0, 0),
# 	Bright = grDevices::rgb(187, 187, 187, maxColorValue = 255),
# 	"High-Contrast" = grDevices::rgb(0, 0, 0),
# 	Vibrant = grDevices::rgb(187, 187, 187, maxColorValue = 255),
# 	Muted = grDevices::rgb(221, 221, 221, maxColorValue = 255),
# 	"Medium-Contrast" = grDevices::rgb(0, 0, 0),
# 	Light = grDevices::rgb(221, 221, 221, maxColorValue = 255),
# 	Paired = grDevices::rgb(247, 247, 247, maxColorValue = 255)
# )
