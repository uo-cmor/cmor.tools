test_that("create_descriptive_table is deprecated", {
	expect_error(create_descriptive_table(), class = "lifecycle_error_deprecated")
})
