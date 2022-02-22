test_that("lincom is deprecated", {
	expect_error(lincom(), class = "lifecycle_error_deprecated")
})

test_that("calculate_ci is deprecated", {
	expect_error(calculate_ci(), class = "lifecycle_error_deprecated")
})
