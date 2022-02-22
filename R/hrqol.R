### Assorted HRQoL value set and related functions ###
######################################################

#' SF-6D profile (SF-12 version)
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated in favour of [SF6Dvalues::sf6d_profile()].
#'
#' Construct SF-6D profile from SF-12 question-level responses.
#'
#' @param ... Vectors containing SF-12 question reponses; should be named as
#'     \code{\{Q1, Q2, ..., Q12\}}, \code{\{Q1, Q2a, ..., Q7\}}, or lower case
#'     equivalents.
#' @param version Which version (1 or 2) of the SF-12 are the data based on?
#' @param dimension One of \code{"PF"}, \code{"RL"}, \code{"SF"},
#'     \code{"PAIN"}, \code{"MH"}, or \code{"VIT"} to return the corresponding
#'     scores, or \code{"list"} (the default) to return a list with all
#'     dimension scores.
#'
#' @keywords internal
#'
#' @export
sf6d_profile <- function(..., version = 2, dimension = "list") {
	lifecycle::deprecate_stop("0.3.0", "sf6d_profile()", "SF6Dvalues::sf6d_profile()")

	# checkmate::assert_choice(version, 1:2)
	# checkmate::assert_choice(dimension, c("list", "PF", "RL", "SF", "PAIN", "MH", "VIT"))
	#
	# sf12_vars <- check_sf12(list(...), version = version)
	#
	# if (dimension == "PF") return(4 - sf12_vars$Q2)
	# if (dimension == "RL") {
	# 	RLp <- switch(version, 2 - sf12_vars$Q5, as.integer(sf12_vars$Q5 < 5))
	# 	RLe <- switch(version, 2 - sf12_vars$Q6, as.integer(sf12_vars$Q6 < 5))
	# 	return(1 + RLp + 2 * RLe)
	# }
	# if (dimension == "SF") return(switch(version, 6 - (sf12_vars$Q12 - (sf12_vars$Q12 > 2)), 6 - sf12_vars$Q12))
	# if (dimension == "PAIN") return(sf12_vars$Q8)
	# if (dimension == "MH") return(switch(version, 6 - (sf12_vars$Q11 - (sf12_vars$Q11 > 2)), 6 - sf12_vars$Q11))
	# if (dimension == "VIT") return(switch(version, sf12_vars$Q10 - (sf12_vars$Q10 > 2), sf12_vars$Q10))
	#
	# PF <- 4 - sf12_vars$Q2
	# RLp <- switch(version, 2 - sf12_vars$Q5, as.integer(sf12_vars$Q5 < 5))
	# RLe <- switch(version, 2 - sf12_vars$Q6, as.integer(sf12_vars$Q6 < 5))
	# RL <- 1 + RLp + 2 * RLe
	# SF <- switch(version, 6 - (sf12_vars$Q12 - (sf12_vars$Q12 > 2)), 6 - sf12_vars$Q12)
	# PAIN <- sf12_vars$Q8
	# MH <- switch(version, 6 - (sf12_vars$Q11 - (sf12_vars$Q11 > 2)), 6 - sf12_vars$Q11)
	# VIT <- switch(version, sf12_vars$Q10 - (sf12_vars$Q10 > 2), sf12_vars$Q10)
	#
	# if (dimension == "list") list(PF = PF, RL = RL, SF = SF, PAIN = PAIN, MH = MH, VIT = VIT)
}

#' SF-6D utility values
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated in favour of [SF6Dvalues::utility()] (and
#'     [SF6Dvalues::sf6d_utility()]).
#'
#' Calculate SF-6D (SF-12) utility values using Brazier & Roberts (2004) algorithm
#'
#' @param PF,RL,SF,PAIN,MH,VIT SF-6D profile
#' @param values Not used
#'
#' @export
sf6d_utility <- function(PF, RL, SF, PAIN, MH, VIT, values = "uk") {
	lifecycle::deprecate_stop(
		"0.3.0", "sf6d_utility()", "SF6Dvalues::SF6D()",
		details = c(
			i = paste("You can also use SF6Dvalues::sf6d_profile() to calculate",
								"utility values directly from SF-12 responses")
		)
	)

	# checkmate::assert_integerish(PF, lower = 1, upper = 3)
	# checkmate::assert_integerish(RL, lower = 1, upper = 4)
	# checkmate::assert_integerish(SF, lower = 1, upper = 5)
	# checkmate::assert_integerish(PAIN, lower = 1, upper = 5)
	# checkmate::assert_integerish(MH, lower = 1, upper = 5)
	# checkmate::assert_integerish(VIT, lower = 1, upper = 5)
	# if (!checkmate::test_choice(values, "uk")) {
	# 	usethis::ui_stop(c(
	# 		"{usethis::ui_code('values')} must be {usethis::ui_value('uk')}.",
	# 		glue::glue("{crayon::yellow(cli::symbol$info)} Only the original Brazier ",
	# 							 "and Roberts (2004) value set is currently implemented."),
	# 		glue::glue("{crayon::red(cli::symbol$cross)} You've supplied ",
	# 							 "{usethis::ui_value(values)}.")
	# 	))
	# }
	#
	# 1 - (
	# 	dplyr::recode(PF, 0, 0, 0.045) +
	# 		dplyr::recode(RL, 0, 0.063, 0.063, 0.063) +
	# 		dplyr::recode(SF, 0, 0.063, 0.066, 0.081, 0.093) +
	# 		dplyr::recode(PAIN, 0, 0, 0.042, 0.077, 0.137) +
	# 		dplyr::recode(MH, 0, 0.059, 0.059, 0.113, 0.134) +
	# 		dplyr::recode(VIT, 0, 0.078, 0.078, 0.078, 0.106) +
	# 		dplyr::if_else(PF == 3 | RL >= 3 | SF >= 4 | PAIN >= 4 | MH >= 4 | VIT == 5, 0.077, 0)
	# )
}

#' SF-12 component summary scores
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated in favour of [SF6Dvalues::sf12_PCS()] and
#'     [SF6Dvalues::sf12_MCS()].
#'
#' Calculate SF-12 PCS and MCS component summary scores from SF-12
#'     question-level responses
#'
#' @param ... Vectors containing SF-12 question reponses; should be named as
#'     \code{\{Q1, Q2, ..., Q12\}}, \code{\{Q1, Q2a, ..., Q7\}}, or lower case
#'     equivalents.
#' @param version Which version (1 or 2) of the SF-12 are the data based on?
#' @param dimension One of \code{"PCS"} or \code{"MCS"} to return the
#'     corresponding score, or \code{"list"} (the default) to return a list
#'     with both elements.
#'
#' @export
sf12_scores <- function(..., version = 2, dimension = "list") {
	lifecycle::deprecate_stop(
		"0.3.0", "sf12_scores()",
		details = "Please use `SF6Dvalues::sf12_PCS()`/`SF6Dvalues::sf12_MCS()` instead."
	)

	# if (length(version) != 1 || !(version %in% c(1L, 2L))) stop("'version' must be 1 or 2")
	# if (length(dimension) != 1 || !(dimension %in% c("list", "PCS", "MCS"))) stop(
	# 	paste0("'dimension' must be one of 'list' (return both PCS and MCS values in a list), 'PCS', or 'MCS'")
	# )
	#
	# sf12_vars <- check_sf12(list(...), version = version)
	#
	# if (version == 1) stop("Only version 2 scoring is implemented at this stage")
	#
	# if (version == 2) {
	# 	PF <- (dplyr::coalesce(sf12_vars$Q2, sf12_vars$Q3) + dplyr::coalesce(sf12_vars$Q3, sf12_vars$Q2) - 2) / 4 * 100
	# 	RP <- (dplyr::coalesce(sf12_vars$Q4, sf12_vars$Q5) + dplyr::coalesce(sf12_vars$Q5, sf12_vars$Q4) - 2) / 8 * 100
	# 	BP <- (6 - sf12_vars$Q8 - 1) / 4 * 100
	# 	GH <- (dplyr::recode(sf12_vars$Q1, 5, 4.4, 3.4, 2, 1) - 1) / 4 * 100
	# 	VT <- (6 - sf12_vars$Q10 - 1) / 4 * 100
	# 	SF <- (sf12_vars$Q12 - 1) / 4 * 100
	# 	RE <- (dplyr::coalesce(sf12_vars$Q6, sf12_vars$Q7) + dplyr::coalesce(sf12_vars$Q7, sf12_vars$Q6) - 2) / 8 * 100
	# 	MH <- (dplyr::coalesce(6L - sf12_vars$Q9, sf12_vars$Q11)
	# 				 + dplyr::coalesce(sf12_vars$Q11, 6L - sf12_vars$Q9) - 2) / 8 * 100
	#
	# 	PFz <- (PF - 81.18122) / 29.10558
	# 	RPz <- (RP - 80.52856) / 27.13526
	# 	BPz <- (BP - 81.74015) / 24.53019
	# 	GHz <- (GH - 72.19795) / 23.19041
	# 	VTz <- (VT - 55.59090) / 24.84380
	# 	SFz <- (SF - 83.73973) / 24.75775
	# 	REz <- (RE - 86.41051) / 22.35543
	# 	MHz <- (MH - 70.18217) / 20.50597
	#
	# 	if (dimension %in% c("list", "PCS")) {
	# 		PHYS <- (PFz * 0.42402 + RPz * 0.35119 + BPz * 0.31754 + GHz * 0.24954 + VTz * 0.02877 + SFz * -0.00753
	# 						 + REz * -0.19206 + MHz * -0.22069)
	# 	  PCS <- 50 + PHYS * 10
	# 	  if (dimension == "PCS") return(PCS)
	# 	}
	# 	if (dimension %in% c("list", "MCS")) {
	# 		MENT <- (PFz * -0.22999 + RPz * -0.12329 + BPz * -0.09731 + GHz * -0.01571 + VTz * 0.23534 + SFz * 0.26876
	# 					 + REz * 0.43407 + MHz * 0.48581)
	# 		MCS <- 50 + MENT * 10
	# 		if (dimension == "MCS") return(MCS)
	# 	}
	#
	# 	list(PCS = PCS, MCS = MCS)
	# }
}

# check_sf12 <- function(sf12_vars, version = 2) {
# 	checkmate::assert_choice(version, 1:2)
# 	checkmate::assert_named(sf12_vars)
#
# 	names_pattern <- paste0("^(", paste0(names(sf12_vars), collapse = "|"), ")$")
# 	qnums <- stringr::str_detect(paste0("q", 1:12), stringr::regex(names_pattern, ignore_case = TRUE))
# 	qchrs <- stringr::str_detect(
# 		c("q1", "q2a", "q2b", "q3a", "q3b", "q4a", "q4b", "q5", "q6a", "q6b", "q6c", "q7"),
# 		stringr::regex(names_pattern, ignore_case = TRUE)
# 	)
# 	if (all(qnums)) {
# 		if (all(qchrs)) usethis::ui_stop(glue::glue(
# 			"Either {{usethis::ui_code(c('Q1', 'Q2'))}}, ..., {{usethis::ui_code('Q12')}} or ",
# 			"{{usethis::ui_code(c('Q1', 'Q2a'))}}, ..., {{usethis::ui_code('Q7')}} should be provided, ",
# 			"not both."
# 		))
# 		Q1 <- check_args(Q1 = sf12_vars$Q1, q1 = sf12_vars$q1)
# 		Q2 <- check_args(Q2 = sf12_vars$Q2, q2 = sf12_vars$q2)
# 		Q3 <- check_args(Q3 = sf12_vars$Q3, q3 = sf12_vars$q3)
# 		Q4 <- check_args(Q4 = sf12_vars$Q4, q4 = sf12_vars$q4)
# 		Q5 <- check_args(Q5 = sf12_vars$Q5, q5 = sf12_vars$q5)
# 		Q6 <- check_args(Q6 = sf12_vars$Q6, q6 = sf12_vars$q6)
# 		Q7 <- check_args(Q7 = sf12_vars$Q7, q7 = sf12_vars$q7)
# 		Q8 <- check_args(Q8 = sf12_vars$Q8, q8 = sf12_vars$q8)
# 		Q9 <- check_args(Q9 = sf12_vars$Q9, q9 = sf12_vars$q9)
# 		Q10 <- check_args(Q10 = sf12_vars$Q10, q10 = sf12_vars$q10)
# 		Q11 <- check_args(Q11 = sf12_vars$Q11, q11 = sf12_vars$q11)
# 		Q12 <- check_args(Q12 = sf12_vars$Q12, q12 = sf12_vars$q12)
# 	} else if (all(qchrs)) {
# 		Q1 <- check_args(Q1 = sf12_vars$Q1, q1 = sf12_vars$q1)
# 		Q2 <- check_args(Q2a = sf12_vars$Q2a, q2a = sf12_vars$q2a, Q2A = sf12_vars$Q2A,
# 										 q2A = sf12_vars$q2A)
# 		Q3 <- check_args(Q2b = sf12_vars$Q2b, q2b = sf12_vars$q2b, Q2B = sf12_vars$Q2B,
# 										 q2B = sf12_vars$q2B)
# 		Q4 <- check_args(Q3a = sf12_vars$Q3a, q3a = sf12_vars$q3a, Q3A = sf12_vars$Q3A,
# 										 q3A = sf12_vars$q3A)
# 		Q5 <- check_args(Q3b = sf12_vars$Q3b, q3b = sf12_vars$q3b, Q3B = sf12_vars$Q3B,
# 										 q3B = sf12_vars$q3B)
# 		Q6 <- check_args(Q4a = sf12_vars$Q4a, q4a = sf12_vars$q4a, Q4A = sf12_vars$Q4A,
# 										 q4A = sf12_vars$q4A)
# 		Q7 <- check_args(Q4b = sf12_vars$Q4b, q4b = sf12_vars$q4b, Q4B = sf12_vars$Q4B,
# 										 q4B = sf12_vars$q4B)
# 		Q8 <- check_args(Q5 = sf12_vars$Q5, q5 = sf12_vars$q5)
# 		Q9 <- check_args(Q6a = sf12_vars$Q6a, q6a = sf12_vars$q6a, Q6A = sf12_vars$Q6A,
# 										 q6A = sf12_vars$q6A)
# 		Q10 <- check_args(Q6b = sf12_vars$Q6b, q6b = sf12_vars$q6b, Q6B = sf12_vars$Q6B,
# 											q6B = sf12_vars$q6B)
# 		Q11 <- check_args(Q6c = sf12_vars$Q6c, q6c = sf12_vars$q6c, Q6C = sf12_vars$Q6C,
# 											q6C = sf12_vars$q6C)
# 		Q12 <- check_args(Q7 = sf12_vars$Q7, q7 = sf12_vars$q7)
# 	} else usethis::ui_stop("Invalid SF-12 question numbers.")
#
# 	vnums <- purrr::map_lgl(list(Q1, Q2, Q3, Q4, Q5, Q6, Q7, Q8, Q9, Q10, Q11, Q12), is.numeric)
# 	vchrs <- purrr::map_lgl(list(Q1, Q2, Q3, Q4, Q5, Q6, Q7, Q8, Q9, Q10, Q11, Q12), is.character)
# 	if (any(vnums) && any(vchrs)) usethis::ui_stop(
# 		"SF-12 question responses must be coded as either numeric or character, not a combination."
# 	)
# 	if (all(vnums)) {
# 		out <- test_sf12_response_values(list(Q1, Q2, Q3, Q4, Q5, Q6, Q7, Q8, Q9, Q10, Q11, Q12),
# 																		 format = "numeric", version = version)
# 		usethis::ui_info(glue::glue("Numeric SF-12 values provided. Please ensure these are coded ",
# 																"with the correct ordering."))
# 	} else if (all(vchrs)) {
# 		out <- test_sf12_response_values(list(Q1, Q2, Q3, Q4, Q5, Q6, Q7, Q8, Q9, Q10, Q11, Q12),
# 																		 format = "character", version = version)
# 	} else usethis::ui_stop("Invalid SF-12 question responses.")
#
# 	out
# }
#
# test_sf12_response_values <- function(questions, format, version = 2) {
# 	valid_values <- sf12_response_options(format, version)
# 	for (q in seq_along(questions)) {
# 		if (!all(questions[[q]][!is.na(questions[[q]])] %in% valid_values[[q]]))
# 			usethis::ui_stop("All {format} Q{q} values must be one of {{{glue::glue_collapse(valid_values[q], sep = ', ')}}}.")
# 	  questions[[q]] <- switch(
# 	  	format,
# 	  	numeric = as.integer(questions[[q]]),
# 	  	character = as.integer(factor(questions[[q]], levels = valid_values[[q]]))
# 	  )
# 	}
# 	rlang::set_names(questions, paste0("Q", 1:12))
# }
#
# sf12_response_options <- function(format, version = 2) {
# 	switch(
# 		format,
# 		numeric = switch(
# 			version,
# 			rep(list(1:5, 1:3, 1:2, 1:5, 1:6), c(1, 2, 4, 1, 4)),
# 			rep(list(1:5, 1:3, 1:5), c(1, 2, 9))
# 		),
# 		character = switch(
# 			version,
# 			list(
# 				Q1 = c("Excellent", "Very good", "Good", "Fair", "Poor"),
# 				Q2 = c("Yes, limited a lot", "Yes, limited a little", "No, not limited at all"),
# 				Q3 = c("Yes, limited a lot", "Yes, limited a little", "No, not limited at all"),
# 				Q4 = c("Yes", "No"),
# 				Q5 = c("Yes", "No"),
# 				Q6 = c("Yes", "No"),
# 				Q7 = c("Yes", "No"),
# 				Q8 = c("Not at all", "A little bit", "Moderately", "Quite a bit", "Extremely"),
# 				Q9 = c("All of the time", "Most of the time", "A good bit of the time", "Some of the time", "A little of the time", "None of the time"),
# 				Q10 = c("All of the time", "Most of the time", "A good bit of the time", "Some of the time", "A little of the time", "None of the time"),
# 				Q11 = c("All of the time", "Most of the time", "A good bit of the time", "Some of the time", "A little of the time", "None of the time"),
# 				Q12 = c("All of the time", "Most of the time", "A good bit of the time", "Some of the time", "A little of the time", "None of the time")
# 			),
# 			list(
# 				Q1 = c("Excellent", "Very good", "Good", "Fair", "Poor"),
# 				Q2 = c("Yes, limited a lot", "Yes, limited a little", "No, not limited at all"),
# 				Q3 = c("Yes, limited a lot", "Yes, limited a little", "No, not limited at all"),
# 				Q4 = c("All of the time", "Most of the time", "Some of the time", "A little of the time", "None of the time"),
# 				Q5 = c("All of the time", "Most of the time", "Some of the time", "A little of the time", "None of the time"),
# 				Q6 = c("All of the time", "Most of the time", "Some of the time", "A little of the time", "None of the time"),
# 				Q7 = c("All of the time", "Most of the time", "Some of the time", "A little of the time", "None of the time"),
# 				Q8 = c("Not at all", "A little bit", "Moderately", "Quite a bit", "Extremely"),
# 				Q9 = c("All of the time", "Most of the time", "Some of the time", "A little of the time", "None of the time"),
# 				Q10 = c("All of the time", "Most of the time", "Some of the time", "A little of the time", "None of the time"),
# 				Q11 = c("All of the time", "Most of the time", "Some of the time", "A little of the time", "None of the time"),
# 				Q12 = c("All of the time", "Most of the time", "Some of the time", "A little of the time", "None of the time")
# 			)
# 		)
# 	)
# }
