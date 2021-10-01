# create_descriptive_table is deprecated

    Code
      create_descriptive_table(df, c(A = "a"), c(B = "b"), list(C = c(C1 = "c1", C2 = "c2")))
    Warning <lifecycle_warning_deprecated>
      `create_descriptive_table()` was deprecated in cmor.tools 0.6.0.
      Please use `formattr::create_descriptive_table()` instead.
    Output
      # A tibble: 8 x 2
        `Patient characteristic` value    
        <chr>                    <chr>    
      1 A                        6.5 (3.6)
      2 B                        <NA>     
      3 &emsp;b1                 4 (33%)  
      4 &emsp;b2                 5 (42%)  
      5 &emsp;b3                 3 (25%)  
      6 C                        <NA>     
      7 &emsp;C1                 7 (58%)  
      8 &emsp;C2                 8 (67%)  

