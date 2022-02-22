test_that("tar_render_manuscript is deprecated", {
	expect_error(tar_render_manuscript(), class = "lifecycle_error_deprecated")
})
