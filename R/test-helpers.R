# expect_hexcolour <- function(object) {
# 	act <- testthat::quasi_label(rlang::enquo(object), arg = "object")
#
# 	valid <- is_hexcolour(act$val)
# 	testthat::expect(
# 		all(valid),
# 		glue::glue("Not all elements of {act$lab} are hex colors.")
# 	)
#
# 	invisible(act$val)
# }
#
# is_hexcolour <- function(x) {
# 	pattern <- stringr::regex("^#[a-f0-9]{6}$", ignore_case = TRUE)
# 	out <- stringr::str_detect(x, pattern)
# 	out[is.na(out)] <- FALSE
# 	out
# }
