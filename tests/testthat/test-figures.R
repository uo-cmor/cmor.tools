test_that("cmor_pal works", {
	for (pal in c("CUD", "Bright", "High-Contrast", "Vibrant", "Muted",
								"Medium-Contrast", "Light", "Paired")) {
		p <- cmor_pal(pal)
		expect_type(p, "closure")
		for (i in 1:max_pal_n(pal)) {
			expect_hexcolour(p(i))
		}
	}
})

test_that("scale_colour_cmor equals scale_color_cmor", {
	expect_true(all.equal(scale_color_cmor(), scale_colour_cmor()))
})

test_that("scale_colour_cmor works", {
	expect_s3_class(scale_colour_cmor(), "ScaleDiscrete")
})

test_that("scale_fill_cmor works", {
	expect_s3_class(scale_fill_cmor(), "ScaleDiscrete")
})

test_that("theme_cmor works", {
	expect_s3_class(theme_cmor(), "theme")
})

test_that("plot_cmor_colours works", {
	p <- plot_cmor_colours()
	expect_s3_class(p, "gg")
	expect_setequal(names(p$mapping), c("x", "y", "fill"))
	expect_setequal(levels(p$data$palette), names(cmor_palettes))
	expect_s3_class(p$layers[[1]]$geom, "GeomTile")
	p <- plot_cmor_colors()
	expect_s3_class(p, "gg")
	expect_setequal(names(p$mapping), c("x", "y", "fill"))
	expect_setequal(levels(p$data$palette), names(cmor_palettes))
	expect_s3_class(p$layers[[1]]$geom, "GeomTile")
})
