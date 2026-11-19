rxTest({
  # .rxIsReservedName() is the R side of the parser's isReservedName(); piping
  # behavior for reserved names is in test-piping-reserved-vars.R.

  test_that(".rxIsReservedName tracks the parser's reserved names", {
    expect_true(all(.rxIsReservedName(c(
      "t",
      "time",
      "tlast",
      "newind",
      "NEWIND",
      "rxFlag",
      "amt",
      "mixnum",
      "mixest",
      "mixunif",
      "M_PI",
      "M_E",
      "M_LN10",
      "pi",
      "NA",
      "NaN",
      "Inf"
    ))))
    # the reserved variables the parser matches case-insensitively
    expect_true(all(.rxIsReservedName(c("Time", "TIME", "AMT"))))
    # names the parser reserves through a pattern rather than a literal
    expect_true(all(.rxIsReservedName(c("rx_mixsel_1_2_", "rx_mixsel_2_2_"))))
    # ordinary model variables are not reserved
    expect_false(any(.rxIsReservedName(c("tka", "ka", "cl", "v", "eta.ka", "add.sd", "wt", "T"))))
    # names new_or_ith() drops or rewrites before the reserved check
    expect_true(all(.rxIsReservedName(c("lhs", "rxlin___", "cmt", "Cmt"))))
    # ... but the exact spelling CMT is an ordinary variable
    expect_false(.rxIsReservedName("CMT"))
    expect_equal(.rxIsReservedName(character(0)), logical(0))
    expect_equal(.rxIsReservedName(NA_character_), NA)
  })
})
