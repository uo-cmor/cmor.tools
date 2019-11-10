### Assorted HRQoL value set and related functions ###
######################################################

#' SF-6D profile (SF-12 version)
#'
#' Construct SF-6D profile from SF-12 question-level responses
#'
#' @param ... Vectors containing SF-12 question reponses; should be named as \code{\{Q1, Q2, ..., Q12\}},
#'     \code{\{Q1, Q2a, ..., Q7\}}, or lower case equivalents.
#'
#' @export
sf6d_profile <- function(..., version = 2, dimension = "list") {
	sf12_vars <- list(...)

	if (length(version) != 1 || !(version %in% c(1L, 2L))) stop ("'version' must be 1 or 2")
	if (length(dimension) != 1 || !(dimension %in% c("list", "PF", "RL", "SF", "PAIN", "MH", "VIT"))) stop(
		paste0("'dimension' must be one of 'list' (return all SF-6D dimensions in a list) or one of the SF-6D dimension",
					 "names ('PF', 'RL', 'SF', 'PAIN', 'MH', 'VIT')")
	)

	if (
		all((c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12") %in% names(sf12_vars)) |
				(c("q1", "q2", "q3", "q4", "q5", "q6", "q7", "q8", "q9", "q10", "q11", "q12") %in% names(sf12_vars)))
	) {
		if (
			all((c("Q2a", "Q2b", "Q3a", "Q3b", "Q4a", "Q4b", "Q6a", "Q6b", "Q6c") %in% names(sf12_vars)) |
					(c("q2a", "q2b", "q3a", "q3b", "q4a", "q4b", "q6a", "q6b", "q6c") %in% names(sf12_vars)) |
					(c("Q2A", "Q2B", "Q3A", "Q3B", "Q4A", "Q4B", "Q6A", "Q6B", "Q6C") %in% names(sf12_vars)) |
					(c("q2A", "q2B", "q3A", "q3B", "q4A", "q4B", "q6A", "q6B", "q6C") %in% names(sf12_vars)))
		)
			warning("Both {Q1, Q2, ..., Q12} and {Q1, Q2a, ..., Q7} question numbers provided; using {Q1, Q2, ..., Q12}")
		Q1 <- check_args(Q1 = sf12_vars$Q1, q1 = sf12_vars$q1)
		Q2 <- check_args(Q2 = sf12_vars$Q2, q2 = sf12_vars$q2)
		Q3 <- check_args(Q3 = sf12_vars$Q3, q3 = sf12_vars$q3)
		Q4 <- check_args(Q4 = sf12_vars$Q4, q4 = sf12_vars$q4)
		Q5 <- check_args(Q5 = sf12_vars$Q5, q5 = sf12_vars$q5)
		Q6 <- check_args(Q6 = sf12_vars$Q6, q6 = sf12_vars$q6)
		Q7 <- check_args(Q7 = sf12_vars$Q7, q7 = sf12_vars$q7)
		Q8 <- check_args(Q8 = sf12_vars$Q8, q8 = sf12_vars$q8)
		Q9 <- check_args(Q9 = sf12_vars$Q9, q9 = sf12_vars$q9)
		Q10 <- check_args(Q10 = sf12_vars$Q10, q10 = sf12_vars$q10)
		Q11 <- check_args(Q11 = sf12_vars$Q11, q11 = sf12_vars$q11)
		Q12 <- check_args(Q12 = sf12_vars$Q12, q12 = sf12_vars$q12)
	} else if (
		all((c("Q1", "Q2a", "Q2b", "Q3a", "Q3b", "Q4a", "Q4b", "Q5", "Q6a", "Q6b", "Q6c", "Q7") %in% names(sf12_vars)) |
				(c("q1", "q2a", "q2b", "q3a", "q3b", "q4a", "q4b", "q5", "q6a", "q6b", "q6c", "q7") %in% names(sf12_vars)) |
				(c("Q1", "Q2A", "Q2B", "Q3A", "Q3B", "Q4A", "Q4B", "Q5", "Q6A", "Q6B", "Q6C", "Q7") %in% names(sf12_vars)) |
				(c("q1", "q2A", "q2B", "q3A", "q3B", "q4A", "q4B", "q5", "q6A", "q6B", "q6C", "q7") %in% names(sf12_vars)))
	) {
		Q1 <- check_args(Q1 = sf12_vars$Q1, q1 = sf12_vars$q1)
		Q2 <- check_args(Q2a = sf12_vars$Q2a, q2a = sf12_vars$q2a, Q2A = sf12_vars$Q2A, q2A = sf12_vars$q2A)
		Q3 <- check_args(Q2b = sf12_vars$Q2b, q2b = sf12_vars$q2b, Q2B = sf12_vars$Q2B, q2B = sf12_vars$q2B)
		Q4 <- check_args(Q3a = sf12_vars$Q3a, q3a = sf12_vars$q3a, Q3A = sf12_vars$Q3A, q3A = sf12_vars$q3A)
		Q5 <- check_args(Q3b = sf12_vars$Q3b, q3b = sf12_vars$q3b, Q3B = sf12_vars$Q3B, q3B = sf12_vars$q3B)
		Q6 <- check_args(Q4a = sf12_vars$Q4a, q4a = sf12_vars$q4a, Q4A = sf12_vars$Q4A, q4A = sf12_vars$q4A)
		Q7 <- check_args(Q4b = sf12_vars$Q4b, q4b = sf12_vars$q4b, Q4B = sf12_vars$Q4B, q4B = sf12_vars$q4B)
		Q8 <- check_args(Q5 = sf12_vars$Q5, q5 = sf12_vars$q5)
		Q9 <- check_args(Q6a = sf12_vars$Q6a, q6a = sf12_vars$q6a, Q6A = sf12_vars$Q6A, q6A = sf12_vars$q6A)
		Q10 <- check_args(Q6b = sf12_vars$Q6b, q6b = sf12_vars$q6b, Q6B = sf12_vars$Q6B, q6B = sf12_vars$q6B)
		Q11 <- check_args(Q6c = sf12_vars$Q6c, q6c = sf12_vars$q6c, Q6C = sf12_vars$Q6C, q6C = sf12_vars$q6C)
		Q12 <- check_args(Q7 = sf12_vars$Q7, q7 = sf12_vars$q7)
	} else {
    stop ("Invalid SF-12 question numbers")
	}

	num <- vapply(list(Q1, Q2, Q3, Q4, Q5, Q6, Q7, Q8, Q9, Q10, Q11, Q12), is.numeric, TRUE)
	if (num[[1]]) if (!all(Q1[!is.na(Q1)] %in% c(1L, 2L, 3L, 4L, 5L))) stop ("Numeric Q1 values must be in {1, 2, 3, 4, 5}")
	if (num[[2]]) if (!all(Q2[!is.na(Q2)] %in% c(1L, 2L, 3L))) stop ("Numeric Q2 values must be in {1, 2, 3}")
	if (num[[3]]) if (!all(Q3[!is.na(Q3)] %in% c(1L, 2L, 3L))) stop ("Numeric Q3 values must be in {1, 2, 3}")
	if (num[[4]]) {
		if (version == 2) if (!all(Q4[!is.na(Q4)] %in% c(1L, 2L, 3L, 4L, 5L))) stop ("Numeric Q4 values must be in {1, 2, 3, 4, 5}")
		if (version == 1) if (!all(Q4[!is.na(Q4)] %in% c(1L, 2L))) stop ("Numeric Q4 values must be in {1, 2}")
	}
	if (num[[5]]) {
		if (version == 2) if (!all(Q5[!is.na(Q5)] %in% c(1L, 2L, 3L, 4L, 5L))) stop ("Numeric Q5 values must be in {1, 2, 3, 4, 5}")
		if (version == 1) if (!all(Q5[!is.na(Q5)] %in% c(1L, 2L))) stop ("Numeric Q5 values must be in {1, 2}")
	}
	if (num[[6]]) {
		if (version == 2) if (!all(Q6[!is.na(Q6)] %in% c(1L, 2L, 3L, 4L, 5L))) stop ("Numeric Q6 values must be in {1, 2, 3, 4, 5}")
		if (version == 1) if (!all(Q6[!is.na(Q6)] %in% c(1L, 2L))) stop ("Numeric Q6 values must be in {1, 2}")
	}
	if (num[[7]]) {
		if (version == 2) if (!all(Q7[!is.na(Q7)] %in% c(1L, 2L, 3L, 4L, 5L))) stop ("Numeric Q7 values must be in {1, 2, 3, 4, 5}")
		if (version == 1) if (!all(Q7[!is.na(Q7)] %in% c(1L, 2L))) stop ("Numeric Q7 values must be in {1, 2}")
	}
	if (num[[8]]) if (!all(Q8[!is.na(Q8)] %in% c(1L, 2L, 3L, 4L, 5L))) stop ("Numeric Q8 values must be in {1, 2, 3, 4, 5}")
	if (num[[9]]) {
		if (version == 2) if (!all(Q9[!is.na(Q9)] %in% c(1L, 2L, 3L, 4L, 5L))) stop ("Numeric Q9 values must be in {1, 2, 3, 4, 5}")
		if (version == 1) if (!all(Q9[!is.na(Q9)] %in% c(1L, 2L, 3L, 4L, 5L, 6L))) stop ("Numeric Q9 values must be in {1, 2, 3, 4, 5, 6}")
	}
	if (num[[10]]) {
		if (version == 2) if (!all(Q10[!is.na(Q10)] %in% c(1L, 2L, 3L, 4L, 5L))) stop ("Numeric Q10 values must be in {1, 2, 3, 4, 5}")
		if (version == 1) if (!all(Q10[!is.na(Q10)] %in% c(1L, 2L, 3L, 4L, 5L, 6L))) stop ("Numeric Q10 values must be in {1, 2, 3, 4, 5, 6}")
	}
	if (num[[11]]) {
		if (version == 2) if (!all(Q11[!is.na(Q11)] %in% c(1L, 2L, 3L, 4L, 5L))) stop ("Numeric Q11 values must be in {1, 2, 3, 4, 5}")
		if (version == 1) if (!all(Q11[!is.na(Q11)] %in% c(1L, 2L, 3L, 4L, 5L, 6L))) stop ("Numeric Q11 values must be in {1, 2, 3, 4, 5, 6}")
	}
	if (num[[12]]) {
		if (version == 2) if (!all(Q12[!is.na(Q12)] %in% c(1L, 2L, 3L, 4L, 5L))) stop ("Numeric Q12 values must be in {1, 2, 3, 4, 5}")
		if (version == 1) if (!all(Q12[!is.na(Q12)] %in% c(1L, 2L, 3L, 4L, 5L, 6L))) stop ("Numeric Q12 values must be in {1, 2, 3, 4, 5, 6}")
	}
	if (any(num)) message("Numeric SF-12 values provided. Please ensure these are coded with the correct ordering")

	char <- vapply(list(Q1, Q2, Q3, Q4, Q5, Q6, Q7, Q8, Q9, Q10, Q11, Q12), is.character, TRUE)
	if (any(char)) label <- switch(
		version,
		list(
			Q1 = c("Excellent", "Very good", "Good", "Fair", "Poor"),
			Q2 = c("Yes, limited a lot", "Yes, limited a little", "No, not limited at all"),
			Q3 = c("Yes, limited a lot", "Yes, limited a little", "No, not limited at all"),
			Q4 = c("Yes", "No"),
			Q5 = c("Yes", "No"),
			Q6 = c("Yes", "No"),
			Q7 = c("Yes", "No"),
			Q8 = c("Not at all", "A little bit", "Moderately", "Quite a bit", "Extremely"),
			Q9 = c("All of the time", "Most of the time", "A good bit of the time", "Some of the time", "A little of the time", "None of the time"),
			Q10 = c("All of the time", "Most of the time", "A good bit of the time", "Some of the time", "A little of the time", "None of the time"),
			Q11 = c("All of the time", "Most of the time", "A good bit of the time", "Some of the time", "A little of the time", "None of the time"),
			Q12 = c("All of the time", "Most of the time", "A good bit of the time", "Some of the time", "A little of the time", "None of the time")
		),
		list(
			Q1 = c("Excellent", "Very good", "Good", "Fair", "Poor"),
			Q2 = c("Yes, limited a lot", "Yes, limited a little", "No, not limited at all"),
			Q3 = c("Yes, limited a lot", "Yes, limited a little", "No, not limited at all"),
			Q4 = c("All of the time", "Most of the time", "Some of the time", "A little of the time", "None of the time"),
			Q5 = c("All of the time", "Most of the time", "Some of the time", "A little of the time", "None of the time"),
			Q6 = c("All of the time", "Most of the time", "Some of the time", "A little of the time", "None of the time"),
			Q7 = c("All of the time", "Most of the time", "Some of the time", "A little of the time", "None of the time"),
			Q8 = c("Not at all", "A little bit", "Moderately", "Quite a bit", "Extremely"),
			Q9 = c("All of the time", "Most of the time", "Some of the time", "A little of the time", "None of the time"),
			Q10 = c("All of the time", "Most of the time", "Some of the time", "A little of the time", "None of the time"),
			Q11 = c("All of the time", "Most of the time", "Some of the time", "A little of the time", "None of the time"),
			Q12 = c("All of the time", "Most of the time", "Some of the time", "A little of the time", "None of the time")
		)
	)
	if (char[[1]]) {
		if (!all(Q1[!is.na(Q1)] %in% label$Q1)) stop (paste0("All Q1 values must be in {'", paste(label$Q1, collapse = "', '"), "'}"))
		Q1 <- as.integer(factor(Q1, levels = label$Q1))
	}
	if (char[[2]]) {
		if (!all(Q2[!is.na(Q2)] %in% label$Q2)) stop (paste0("All Q2 values must be in {'", paste(label$Q2, collapse = "', '"), "'}"))
		Q2 <- as.integer(factor(Q2, levels = label$Q2))
	}
	if (char[[3]]) {
		if (!all(Q3[!is.na(Q3)] %in% label$Q3)) stop (paste0("All Q3 values must be in {'", paste(label$Q3, collapse = "', '"), "'}"))
		Q3 <- as.integer(factor(Q3, levels = label$Q3))
	}
	if (char[[4]]) {
		if (!all(Q4[!is.na(Q4)] %in% label$Q4)) stop (paste0("All Q4 values must be in {'", paste(label$Q4, collapse = "', '"), "'}"))
		Q4 <- as.integer(factor(Q4, levels = label$Q4))
	}
	if (char[[5]]) {
		if (!all(Q5[!is.na(Q5)] %in% label$Q5)) stop (paste0("All Q5 values must be in {'", paste(label$Q5, collapse = "', '"), "'}"))
		Q5 <- as.integer(factor(Q5, levels = label$Q5))
	}
	if (char[[6]]) {
		if (!all(Q6[!is.na(Q6)] %in% label$Q6)) stop (paste0("All Q6 values must be in {'", paste(label$Q6, collapse = "', '"), "'}"))
		Q6 <- as.integer(factor(Q6, levels = label$Q6))
	}
	if (char[[7]]) {
		if (!all(Q7[!is.na(Q7)] %in% label$Q7)) stop (paste0("All Q7 values must be in {'", paste(label$Q7, collapse = "', '"), "'}"))
		Q7 <- as.integer(factor(Q7, levels = label$Q7))
	}
	if (char[[8]]) {
		if (!all(Q8[!is.na(Q8)] %in% label$Q8)) stop (paste0("All Q8 values must be in {'", paste(label$Q8, collapse = "', '"), "'}"))
		Q8 <- as.integer(factor(Q8, levels = label$Q8))
	}
	if (char[[9]]) {
		if (!all(Q9[!is.na(Q9)] %in% label$Q9)) stop (paste0("All Q9 values must be in {'", paste(label$Q9, collapse = "', '"), "'}"))
		Q9 <- as.integer(factor(Q9, levels = label$Q9))
	}
	if (char[[10]]) {
		if (!all(Q10[!is.na(Q10)] %in% label$Q10)) stop (paste0("All Q10 values must be in {'", paste(label$Q10, collapse = "', '"), "'}"))
		Q10 <- as.integer(factor(Q10, levels = label$Q10))
	}
	if (char[[11]]) {
		if (!all(Q11[!is.na(Q11)] %in% label$Q11)) stop (paste0("All Q11 values must be in {'", paste(label$Q11, collapse = "', '"), "'}"))
		Q11 <- as.integer(factor(Q11, levels = label$Q11))
	}
	if (char[[12]]) {
		if (!all(Q12[!is.na(Q12)] %in% label$Q12)) stop (paste0("All Q12 values must be in {'", paste(label$Q12, collapse = "', '"), "'}"))
		Q12 <- as.integer(factor(Q12, levels = label$Q12))
	}

	PF <- 4 - Q2
	RLp <- switch(version, 2 - Q5, as.integer(Q5 < 5))
	RLe <- switch(version, 2 - Q6, as.integer(Q6 < 5))
	RL <- 1 + RLp + 2 * RLe
	SF <- switch(version, 6 - (Q12 - (Q12 > 2)), 6 - Q12)
	PAIN <- Q8
	MH <- switch(version, 6 - (Q11 - (Q11 > 2)), 6 - Q11)
	VIT <- switch(version, Q10 - (Q10 > 2), Q10)

	if (dimension == "list") list(PF = PF, RL = RL, SF = SF, PAIN = PAIN, MH = MH, VIT = VIT)
}

#' SF-6D utility values
#'
#' Calculate SF-6D (SF-12) utility values using Brazier & Roberts (2004) algorithm
#'
#' @param PF,RL,SF,PAIN,MH,VIT SF-6D profile
#' @param values Not used
#'
#' @export
sf6d_utility <- function(PF, RL, SF, PAIN, MH, VIT, values = "uk") {
	if (values != "uk") stop ("Only the original Brazier and Roberts (2004) value set is currently implemented")

	1 - (
		dplyr::recode(PF, 0, 0, 0.045) +
			dplyr::recode(RL, 0, 0.063, 0.063, 0.063) +
			dplyr::recode(SF, 0, 0.063, 0.066, 0.081, 0.093) +
			dplyr::recode(PAIN, 0, 0, 0.042, 0.077, 0.137) +
			dplyr::recode(MH, 0, 0.059, 0.059, 0.113, 0.134) +
			dplyr::recode(VIT, 0, 0.078, 0.078, 0.078, 0.106) +
			dplyr::if_else(PF == 3 | RL >= 3 | SF >= 4 | PAIN >= 4 | MH >= 4 | VIT == 5, 0.077, 0)
	)
}
