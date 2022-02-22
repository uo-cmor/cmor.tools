test_that("sf6d_profile is deprecated", {
	expect_error(sf6d_profile(), class = "lifecycle_error_deprecated")
})

test_that("sf6d_utility is deprecated", {
	expect_error(sf6d_utility(), class = "lifecycle_error_deprecated")
})

test_that("sf12_scores is deprecated", {
	expect_error(sf12_scores(), class = "lifecycle_error_deprecated")
})
