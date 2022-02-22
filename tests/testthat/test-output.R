test_that("est_ci is deprecated", {
	expect_error(est_ci(), class = "lifecycle_error_deprecated")
})

test_that("number is deprecated", {
	expect_error(number(), class = "lifecycle_error_deprecated")
})

test_that("percent is deprecated", {
	expect_error(percent(), class = "lifecycle_error_deprecated")
})

test_that("comma is deprecated", {
	expect_error(comma(), class = "lifecycle_error_deprecated")
})
