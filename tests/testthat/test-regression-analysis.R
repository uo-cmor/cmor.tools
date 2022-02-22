# df <- tibble::tibble(a = 1:12, b = rep(c("b1", "b2", "b3"), c(4, 5, 3)), c = rep(0:1, c(5, 7)))
# model <- lm(a ~ b + c, df)
#
# test_that("lincom works", {
#   expect_equal(lincom(model, c(1, 0.5, 0.3, 0.2)),
#   						 list(b = 6.05, se = sqrt(0.60375)))
# 	expect_equal(
# 		lincom(model, rbind(c(1, 0.5, 0.3, 0.2),
# 												c(0.2, 0.3, 0.5, 0.2))),
# 		list(
# 			b = c(6.05, 4.75),
# 			V = matrix(c(0.60375, 0.58125, 0.58125, 0.83375), nrow = 2),
# 			F.test = list(
# 				df.residual = 8, df = 2, F.stat = matrix(32.4183658057695),
# 				P = matrix(stats::pf(32.4183658057695, 2, 8, lower.tail = FALSE))
# 			)
# 		)
# 	)
# })

test_that("lincom is deprecated", {
	expect_error(lincom(), class = "lifecycle_error_deprecated")
})

test_that("calculate_ci is deprecated", {
	expect_error(calculate_ci(), class = "lifecycle_error_deprecated")
})
