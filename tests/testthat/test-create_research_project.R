test_that("create_research_project is deprecated", {
	expect_error(create_research_project(), class = "lifecycle_error_deprecated")
})
