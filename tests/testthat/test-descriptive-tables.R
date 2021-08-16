df <- tibble::tibble(a = 1:12, b = rep(c("b1", "b2", "b3"), c(4, 5, 3)),
										 c1 = rep(0:1, c(5, 7)), c2 = rep(0:1, c(4, 8)),
										 d = rep(c("d0", "d1"), 6))

test_that("continuous_characteristics_table works", {
  expect_equal(
  	continuous_characteristics_table(df, NULL, "a", "A",
  																	 rlang::quo(mean_sd(.mean, .sd, accuracy = 0.1)), NULL),
  	tibble::tibble(`Patient characteristic` = "A", value = glue::glue("6.5 (3.6)"))
  )
	expect_equal(
		continuous_characteristics_table(df, "d", "a", "A",
																		 rlang::quo(mean_sd(.mean, .sd, accuracy = 0.1)), "Total"),
		tibble::tibble(`Patient characteristic` = "A", d0 = glue::glue("6.0 (3.7)"),
									 d1 = glue::glue("7.0 (3.7)"), Total = glue::glue("6.5 (3.6)"))
	)
})

test_that("discrete_characteristics_table works", {
	expect_equal(
		discrete_characteristics_table(df, NULL, "b", "B", rlang::quo(n_percent(.n, .proportion)),
																	 NULL),
		tibble::tibble(`Patient characteristic` = c("B", "&emsp;b1", "&emsp;b2", "&emsp;b3"),
									 value = glue::glue("{c(NA, '4 (33%)', '5 (42%)', '3 (25%)')}"))
	)
	expect_equal(
		discrete_characteristics_table(df, "d", "b", "B", rlang::quo(n_percent(.n, .proportion)),
																	 "Total"),
		tibble::tibble(`Patient characteristic` = c("B", "&emsp;b1", "&emsp;b2", "&emsp;b3"),
									 d0 = glue::glue("{c(NA, '2 (33%)', '3 (50%)', '1 (17%)')}"),
									 d1 = glue::glue("{c(NA, '2 (33%)', '2 (33%)', '2 (33%)')}"),
									 Total = glue::glue("{c(NA, '4 (33%)', '5 (42%)', '3 (25%)')}"))
	)
})

test_that("multiresponse_characteristics_table works", {
	expect_equal(
		multiresponse_characteristics_table(df, NULL, c(C1 = "c1", C2 = "c2"), "C",
																				rlang::quo(n_percent(.n, .proportion)), NULL),
		tibble::tibble(`Patient characteristic` = c("C", "&emsp;C1", "&emsp;C2"),
									 value = glue::glue("{c(NA, '7 (58%)', '8 (67%)')}"))
	)
	expect_equal(
		multiresponse_characteristics_table(df, "d", c(C1 = "c1", C2 = "c2"), "C",
																				rlang::quo(n_percent(.n, .proportion)), "Total"),
		tibble::tibble(`Patient characteristic` = c("C", "&emsp;C1", "&emsp;C2"),
									 d0 = glue::glue("{c(NA, '3 (50%)', '4 (67%)')}"),
									 d1 = glue::glue("{c(NA, '4 (67%)', '4 (67%)')}"),
									 Total = glue::glue("{c(NA, '7 (58%)', '8 (67%)')}"))
	)
})

test_that("create_descriptive_table works", {
	expect_equal(
		create_descriptive_table(df, c(A = "a"), c(B = "b"), list(C = c(C1 = "c1", C2 = "c2"))),
		tibble::tibble(
			`Patient characteristic` = c("A", "B", "&emsp;b1", "&emsp;b2", "&emsp;b3",
																	 "C", "&emsp;C1", "&emsp;C2"),
			value = glue::glue("{c('6.5 (3.6)', NA, '4 (33%)', '5 (42%)', '3 (25%)', NA, '7 (58%)', ",
												 "'8 (67%)')}")
		)
	)
	expect_equal(
		create_descriptive_table(df, c(A = "a"), c(B = "b"), list(C = c(C1 = "c1", C2 = "c2")),
														 by = "d"),
		tibble::tibble(
			`Patient characteristic` = c("A", "B", "&emsp;b1", "&emsp;b2", "&emsp;b3",
																	 "C", "&emsp;C1", "&emsp;C2"),
			d0 = glue::glue("{c('6.0 (3.7)', NA, '2 (33%)', '3 (50%)', '1 (17%)', NA, '3 (50%)', ",
												 "'4 (67%)')}"),
			d1 = glue::glue("{c('7.0 (3.7)', NA, '2 (33%)', '2 (33%)', '2 (33%)', NA, '4 (67%)', ",
												 "'4 (67%)')}"),
			"Entire sample" = glue::glue("{c('6.5 (3.6)', NA, '4 (33%)', '5 (42%)', '3 (25%)', NA, ",
																	 "'7 (58%)', '8 (67%)')}")
		)
	)
})

test_that("make_header_row works", {
	expect_equal(make_header_row("A"), tibble::tibble(`Patient characteristic` = "A"))
})
