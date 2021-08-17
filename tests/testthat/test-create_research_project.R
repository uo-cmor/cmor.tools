test_that("create_research_project works", {
	dir <- local_create_project()

  expect_true(possibly_in_proj(dir))
  expect_false(is_package(dir))
})

test_that("create_research_project generates appropriate error messages", {
	expect_error(local_create_project("."))
	expect_warning(local_create_project(git = FALSE, github = TRUE))

})
