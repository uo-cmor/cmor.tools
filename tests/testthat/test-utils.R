# test_that("check_args gives appropriate error if multiple values supplied", {
#   expect_error(check_args(a = 1, b = 2))
# })
#
# test_that("calculate_ci works", {
# 	expect_error(calculate_ci(1.1))
# 	expect_equal(calculate_ci(0.95), c(0.025, 0.975))
# 	expect_equal(calculate_ci(0.9), c(0.05, 0.95))
# })
