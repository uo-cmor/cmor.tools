test_that("est_ci works as expected", {
  expect_equal(est_ci(c(1, 2), c(0.5, 1), c(1.4, 3.1), accuracy = 0.1, .sep = " to ", .glue = TRUE),
  						 glue::glue("{c('1.0 (0.5 to 1.4)', '2.0 (1.0 to 3.1)')}"))
	expect_equal(est_ci(c(1, 2), c(0.5, 1), c(1.4, 3.1), accuracy = 0.1, .sep = " to ", .glue = FALSE),
							 c('1.0 (0.5 to 1.4)', '2.0 (1.0 to 3.1)'))
})

test_that("est_ci gives appropriate errors", {
	expect_error(est_ci("1"))
	expect_error(est_ci(1, "1"))
	expect_error(est_ci(1, 0.5, "1"))
	expect_error(est_ci(1, 0.5, 1.5, .sep = 1))
	expect_error(est_ci(1, 0.5, 1.5, .glue = 1))
})

test_that("number_format works as expected", {
	fn <- number_format()
	expect_type(fn, "closure")
	expect_equal(fn(3.21), "3")
	expect_error(fn("3.21"))

	fn2 <- number_format(accuracy = 0.001, scale = 100, suffix = "%")
	expect_type(fn2, "closure")
	expect_equal(fn2(0.123456), "12.346%")
})

test_that("number_format gives appropriate errors", {
	expect_error(number_format(accuracy = "1"))
	expect_error(number_format(scale =  "1"))
	expect_error(number_format(prefix = 1))
	expect_error(number_format(suffix = 1))
	expect_error(number_format(big.mark = 1))
	expect_error(number_format(decimal.mark = 1))
	expect_error(number_format(trim = 1))
	expect_error(number_format(html = 1))
})

test_that("percent works as expected", {
	expect_equal(percent(0.123456), "12%")
	expect_equal(percent(-0.123456, accuracy = 0.001), "&minus;12.346%")
	expect_equal(percent(-12.3456, accuracy = 0.1, html = FALSE), "\u22121\u202F234.6%")
})

test_that("percent gives appropriate errors", {
	expect_error(percent("1"))
	expect_error(percent(0.12345, accuracy = "1"))
	expect_error(percent(0.12345, scale =  "1"))
	expect_error(percent(0.12345, prefix = 1))
	expect_error(percent(0.12345, suffix = 1))
	expect_error(percent(0.12345, big.mark = 1))
	expect_error(percent(0.12345, decimal.mark = 1))
	expect_error(percent(0.12345, trim = 1))
	expect_error(percent(0.12345, html = 1))
})

test_that("percent_format works as expected", {
	fn <- percent_format(accuracy = 0.001)
	expect_type(fn, "closure")
	expect_equal(fn(0.123456), "12.346%")
	expect_error(fn("3.21"))
})

test_that("percent_format gives appropriate errors", {
	expect_error(percent_format(accuracy = "1"))
	expect_error(percent_format(scale =  "1"))
	expect_error(percent_format(prefix = 1))
	expect_error(percent_format(suffix = 1))
	expect_error(percent_format(big.mark = 1))
	expect_error(percent_format(decimal.mark = 1))
	expect_error(percent_format(trim = 1))
	expect_error(percent_format(html = 1))
})

test_that("comma works as expected", {
	expect_equal(comma(123456), "123,456")
	expect_equal(comma(-12345.6, accuracy = 0.1), "&minus;12,345.6")
	expect_equal(comma(-9876, accuracy = 10, html = FALSE), "\u22129,880")
})

test_that("comma gives appropriate errors", {
	expect_error(comma("12345"))
	expect_error(comma(12345, accuracy = "1"))
	expect_error(comma(12345, scale =  "1"))
	expect_error(comma(12345, prefix = 1))
	expect_error(comma(12345, suffix = 1))
	expect_error(comma(12345, big.mark = 1))
	expect_error(comma(12345, decimal.mark = 1))
	expect_error(comma(12345, trim = 1))
	expect_error(comma(12345, html = 1))
})

test_that("comma_format works as expected", {
	fn <- comma_format()
	expect_type(fn, "closure")
	expect_equal(fn(123456), "123,456")
	expect_error(fn("3.21"))
})

test_that("comma_format gives appropriate errors", {
	expect_error(comma_format(accuracy = "1"))
	expect_error(comma_format(scale =  "1"))
	expect_error(comma_format(prefix = 1))
	expect_error(comma_format(suffix = 1))
	expect_error(comma_format(big.mark = 1))
	expect_error(comma_format(decimal.mark = 1))
	expect_error(comma_format(trim = 1))
	expect_error(comma_format(html = 1))
})
