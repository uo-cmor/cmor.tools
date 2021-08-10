#' CMOR standard colour scales
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
	ggplot2::discrete_scale(aesthetics, "cmor", cmor_pal(palette, direction, grey), ...)
}

#' @rdname scale_colour_cmor
#' @export
scale_color_cmor <- function(..., palette = 1, direction = 1, grey = FALSE,
														 aesthetics = "colour") {
	ggplot2::discrete_scale(aesthetics, "cmor", cmor_pal(palette, direction, grey), ...)
}

#' @rdname scale_colour_cmor
#' @export
scale_fill_cmor <- function(..., palette = 1, direction = 1, grey = FALSE,
														aesthetics = "fill") {
	ggplot2::discrete_scale(aesthetics, "cmor", cmor_pal(palette, direction, grey), ...)
}

#' Standard `ggplot` Theme for CMOR Outputs
#'
#' This is a preliminary draft of a standard CMOR ggplot theme (just directly
#'     calls `theme_classic()` for now).
#'
#' @param base_size Base font size, in pts
#' @param base_family Base font family
#'
#' @export
theme_cmor <- function(base_size = 11, base_family = "") {
	ggplot2::theme_classic(base_size = base_size, base_family = base_family)
}

cmor_pal <- function(palette = "CUD", direction = 1, grey = FALSE) {
	## Options to be added (?):
	### `grey` Logical: should the final colour in the palette be a grey/black
	###                 (i.e. regardless of the number of colours selected)
	### `type` String: similar to ColorBrewer's `type`
	pal <- cmor_pal_name(palette)
	force(direction)
	function(n) {
		max_n <- max_pal_n(pal)
		check_pal_n(n, max_n, pal)
		if (grey) n <- n - 1
		ret <- switch(
			pal,
			CUD = grDevices::rgb(c(230, 86, 0, 240, 0, 213, 204, 0),
													 c(159, 180, 158, 228, 114, 94, 121, 0),
													 c(0, 233, 115, 66, 178, 0, 167, 0), maxColorValue = 255),
			Bright = grDevices::rgb(c(68, 238, 34, 204, 102, 170, 187),
															c(119, 102, 136, 187, 204, 51, 187),
															c(170, 119, 51, 68, 238, 119, 187), maxColorValue = 255),
			"High-Contrast" = grDevices::rgb(c(0, 221, 187, 0), c(68, 170, 85, 0), c(136, 51, 102, 0),
																			 maxColorValue = 255),
			Vibrant = grDevices::rgb(c(238, 0, 51, 238, 204, 0, 187),
															 c(119, 119, 187, 51, 51, 153, 187),
															 c(51, 187, 238, 119, 17, 136, 187), maxColorValue = 255),
			Muted = grDevices::rgb(c(204, 51, 221, 17, 136, 136, 68, 153, 170, 221),
														 c(102, 34, 204, 119, 204, 34, 170, 153, 68, 221),
														 c(119, 136, 119, 51, 238, 85, 153, 51, 153, 221), maxColorValue = 255),
			"Medium-Contrast" = grDevices::rgb(c(102, 0, 238, 153, 238, 153, 255, 0),
																				 c(153, 68, 204, 119, 153, 68, 255, 0),
																				 c(204, 136, 102, 0, 170, 85, 255, 0), maxColorValue = 255),
			Light = grDevices::rgb(c(119, 238, 238, 255, 153, 68, 187, 170, 221),
														 c(170, 136, 221, 170, 221, 187, 204, 170, 221),
														 c(221, 102, 136, 187, 255, 153, 51, 0, 221), maxColorValue = 255),
			Paired = grDevices::rgb(c(178, 94, 253, 230, 247),
															c(171, 60, 187, 97, 247),
															c(210, 153, 99, 1, 247), maxColorValue = 255)
		)[seq_len(n)]
		if (direction == -1) ret <- rev(ret)
		if (grey) ret <- c(ret, switch(
			pal,
			CUD = grDevices::rgb(0, 0, 0),
			Bright = grDevices::rgb(187, 187, 187, maxColorValue = 255),
			"High-Contrast" = grDevices::rgb(0, 0, 0),
			Vibrant = grDevices::rgb(187, 187, 187, maxColorValue = 255),
			Muted = grDevices::rgb(221, 221, 221, maxColorValue = 255),
			"Medium-Contrast" = grDevices::rgb(0, 0, 0),
			Light = grDevices::rgb(221, 221, 221, maxColorValue = 255),
			Paired = grDevices::rgb(247, 247, 247, maxColorValue = 255)
		))
		ret
	}
}

cmor_pal_name <- function(palette) {
	if (checkmate::test_string(palette))
		checkmate::assert_choice(palette, c("CUD", "Bright", "High-Contrast", "Vibrant", "Muted",
																				"Medium-Contrast", "Light", "Paired"))
	else if (checkmate::test_count(palette, positive = TRUE)) {
		if (!checkmate::test_int(palette, upper = 8))
			stop("Only 8 colour palettes are defined.\n",
					 crayon::red(cli::symbol$cross), " You've provided `palette = ", palette, "`.")
		c("CUD", "Bright", "High-Contrast", "Vibrant", "Muted", "Medium-Contrast", "Light",
			"Paired")[palette]
	} else stop("Unknown 'palette' value")
}

max_pal_n <- function(pal) {
	switch(pal,
				 CUD = 8, Bright = 7, "High-Contrast" = 4, Vibrant = 7, Muted = 10, "Medium-Contrast" = 8,
				 Light = 9, Paired = 4)
}

check_pal_n <- function(n, max_n, pal) {
	if (n > max_n) stop("Palette '", pal, "' has a maximum of ", max_n, " colours.\n",
											crayon::red(cli::symbol$cross), " You've provided `n = ", n, "`.")
}
