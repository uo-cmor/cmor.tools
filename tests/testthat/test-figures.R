test_that("scale_colour_cmor & friends are deprecated", {
	expect_error(scale_colour_cmor(), class = "lifecycle_error_deprecated")
	expect_error(scale_color_cmor(), class = "lifecycle_error_deprecated")
	expect_error(scale_fill_cmor(), class = "lifecycle_error_deprecated")
})

test_that("theme_cmor is deprecated", {
	expect_error(theme_cmor(), class = "lifecycle_error_deprecated")
})
test_that("plot_cmor_colours is deprecated", {
	expect_error(plot_cmor_colours(), class = "lifecycle_error_deprecated")
})
