# sf6d_profile is deprecated

    Code
      sf6d_profile(Q1 = 1:3, Q2 = 1:3, Q3 = 1:3, Q4 = 1:3, Q5 = 3:5, Q6 = 5:3, Q7 = 1:
        3, Q8 = 1:3, Q9 = 1:3, Q10 = 1:3, Q11 = 1:3, Q12 = 1:3)
    Warning <lifecycle_warning_deprecated>
      `sf6d_profile()` was deprecated in cmor.tools 0.3.0.
      Please use `SF6Dvalues::sf6d_profile()` instead.
    Message <message>
      i Numeric SF-12 values provided. Please ensure these are coded with the correct ordering.
    Output
      $PF
      [1] 3 2 1
      
      $RL
      [1] 2 4 3
      
      $SF
      [1] 5 4 3
      
      $PAIN
      [1] 1 2 3
      
      $MH
      [1] 5 4 3
      
      $VIT
      [1] 1 2 3
      

# sf6d_utility is deprecated

    Code
      sf6d_utility(1:3, 1:3, 1:3, 1:3, 1:3, 1:3)
    Warning <lifecycle_warning_deprecated>
      `sf6d_utility()` was deprecated in cmor.tools 0.3.0.
      Please use `SF6Dvalues::SF6D()` instead.
      You can also use SF6Dvalues::sf6d_profile() to calculate utility values directly from SF-12 responses
    Output
      [1] 1.000 0.737 0.570

