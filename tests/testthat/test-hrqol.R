test_that("sf6d_profile works with numeric values", {
	withr::local_options(lifecycle_verbosity = "quiet")
	expect_message(
		sf6d_profile(Q1 = 1:3, Q2 = 1:3, Q3 = 1:3, Q4 = 1:3, Q5 = 3:5, Q6 = 5:3, Q7 = 1:3, Q8 = 1:3,
								 Q9 = 1:3, Q10 = 1:3, Q11 = 1:3, Q12 = 1:3),
		"Numeric SF-12 values provided. Please ensure these are coded with the correct ordering."
	)
	expect_equal(
		suppressMessages(sf6d_profile(Q1 = 1:3, Q2 = 1:3, Q3 = 1:3, Q4 = 1:3, Q5 = 3:5, Q6 = 5:3,
																	Q7 = 1:3, Q8 = 1:3, Q9 = 1:3, Q10 = 1:3, Q11 = 1:3, Q12 = 1:3)),
		list(PF = 3:1, RL = c(2, 4, 3), SF = 5:3, PAIN = 1:3, MH = 5:3, VIT = 1:3)
	)
	expect_equal(
		suppressMessages(sf6d_profile(Q1 = 1:3, Q2a = 1:3, Q2b = 1:3, Q3a = 1:3, Q3b = 3:5, Q4a = 5:3,
																	Q4b = 1:3, Q5 = 1:3, Q6a = 1:3, Q6b = 1:3, Q6c = 1:3, Q7 = 1:3)),
		list(PF = 3:1, RL = c(2, 4, 3), SF = 5:3, PAIN = 1:3, MH = 5:3, VIT = 1:3)
	)
})

test_that("sf6d_profile works with character strings", {
	withr::local_options(lifecycle_verbosity = "quiet")
	expect_silent(
		sf6d_profile(Q1 = c("Excellent", "Very good", "Good"),
								 Q2 = c("Yes, limited a lot", "Yes, limited a little", "No, not limited at all"),
								 Q3 = c("Yes, limited a lot", "Yes, limited a little", "No, not limited at all"),
								 Q4 = c("All of the time", "Most of the time", "Some of the time"),
								 Q5 = c("Some of the time", "A little of the time", "None of the time"),
								 Q6 = c("None of the time", "A little of the time", "Some of the time"),
								 Q7 = c("All of the time", "Most of the time", "Some of the time"),
								 Q8 = c("Not at all", "A little bit", "Moderately"),
								 Q9 = c("All of the time", "Most of the time", "Some of the time"),
								 Q10 = c("All of the time", "Most of the time", "Some of the time"),
								 Q11 = c("All of the time", "Most of the time", "Some of the time"),
								 Q12 = c("All of the time", "Most of the time", "Some of the time"))
	)
	expect_equal(
		sf6d_profile(Q1 = c("Excellent", "Very good", "Good"),
								 Q2 = c("Yes, limited a lot", "Yes, limited a little", "No, not limited at all"),
								 Q3 = c("Yes, limited a lot", "Yes, limited a little", "No, not limited at all"),
								 Q4 = c("All of the time", "Most of the time", "Some of the time"),
								 Q5 = c("Some of the time", "A little of the time", "None of the time"),
								 Q6 = c("None of the time", "A little of the time", "Some of the time"),
								 Q7 = c("All of the time", "Most of the time", "Some of the time"),
								 Q8 = c("Not at all", "A little bit", "Moderately"),
								 Q9 = c("All of the time", "Most of the time", "Some of the time"),
								 Q10 = c("All of the time", "Most of the time", "Some of the time"),
								 Q11 = c("All of the time", "Most of the time", "Some of the time"),
								 Q12 = c("All of the time", "Most of the time", "Some of the time")),
		list(PF = 3:1, RL = c(2, 4, 3), SF = 5:3, PAIN = 1:3, MH = 5:3, VIT = 1:3)
	)
	expect_equal(
		sf6d_profile(Q1 = c("Excellent", "Very good", "Good"),
								 Q2a = c("Yes, limited a lot", "Yes, limited a little", "No, not limited at all"),
								 Q2b = c("Yes, limited a lot", "Yes, limited a little", "No, not limited at all"),
								 Q3a = c("All of the time", "Most of the time", "Some of the time"),
								 Q3b = c("Some of the time", "A little of the time", "None of the time"),
								 Q4a = c("None of the time", "A little of the time", "Some of the time"),
								 Q4b = c("All of the time", "Most of the time", "Some of the time"),
								 Q5 = c("Not at all", "A little bit", "Moderately"),
								 Q6a = c("All of the time", "Most of the time", "Some of the time"),
								 Q6b = c("All of the time", "Most of the time", "Some of the time"),
								 Q6c = c("All of the time", "Most of the time", "Some of the time"),
								 Q7 = c("All of the time", "Most of the time", "Some of the time")),
		list(PF = 3:1, RL = c(2, 4, 3), SF = 5:3, PAIN = 1:3, MH = 5:3, VIT = 1:3)
	)
})

test_that("sf6d_profile is deprecated", {
	expect_snapshot(sf6d_profile(Q1 = 1:3, Q2 = 1:3, Q3 = 1:3, Q4 = 1:3, Q5 = 3:5, Q6 = 5:3,
															 Q7 = 1:3, Q8 = 1:3, Q9 = 1:3, Q10 = 1:3, Q11 = 1:3, Q12 = 1:3))
})


test_that("sf6d_utility gives error with invalid 'values' argument", {
	withr::local_options(lifecycle_verbosity = "quiet")
	expect_error(sf6d_utility(1, 1, 1, 1, 1, 1, values = "x"))
})

test_that("sf6d_utility works", {
	withr::local_options(lifecycle_verbosity = "quiet")
	expect_equal(sf6d_utility(1:3, 1:3, 1:3, 1:3, 1:3, 1:3), c(1, 0.737, 0.570))
})

test_that("sf6d_utility is deprecated", {
	expect_snapshot(sf6d_utility(1:3, 1:3, 1:3, 1:3, 1:3, 1:3))
})

test_that("sf12_scores works", {
	expect_equal(
		sf12_scores(Q1 = c("Excellent", "Very good", "Good"),
							 Q2 = c("Yes, limited a lot", "Yes, limited a little", "No, not limited at all"),
							 Q3 = c("Yes, limited a lot", "Yes, limited a little", "No, not limited at all"),
							 Q4 = c("All of the time", "Most of the time", "Some of the time"),
							 Q5 = c("Some of the time", "A little of the time", "None of the time"),
							 Q6 = c("None of the time", "A little of the time", "Some of the time"),
							 Q7 = c("All of the time", "Most of the time", "Some of the time"),
							 Q8 = c("Not at all", "A little bit", "Moderately"),
							 Q9 = c("All of the time", "Most of the time", "Some of the time"),
							 Q10 = c("All of the time", "Most of the time", "Some of the time"),
							 Q11 = c("All of the time", "Most of the time", "Some of the time"),
							 Q12 = c("All of the time", "Most of the time", "Some of the time")),
		list(PCS = c(42.4110903, 47.7149766, 51.9428147),
				 MCS = c(41.290343, 37.642553, 34.062505))
	)
})
