test_that("'pkgs' includes correct CMOR Tools packages", {
  expect_setequal(pkgs, c("formattr", "CMORprojects", "regtools", "CMORplots", "SF6Dvalues", "cea"))
})
