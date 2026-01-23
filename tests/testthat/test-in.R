rxTest({
  test_that(".in() function equivalence with %in%", {
    # Test basic functionality
    x <- c(1, 2, 3, 4, 5)
    table <- c(2, 4, 6, 8)
    
    # Test with match backend
    rxode2:::inUseFastMatch(FALSE)
    expect_equal(rxode2:::.in(x, table), x %in% table)
    
    # Test with character vectors
    x_char <- c("a", "b", "c", "d")
    table_char <- c("b", "d", "f")
    expect_equal(rxode2:::.in(x_char, table_char), x_char %in% table_char)
    
    # Test with empty vectors
    expect_equal(rxode2:::.in(integer(0), table), logical(0))
    expect_equal(rxode2:::.in(x, integer(0)), rep(FALSE, length(x)))
    
    # Test with NA values
    x_na <- c(1, NA, 3)
    table_na <- c(1, 2, NA)
    expect_equal(rxode2:::.in(x_na, table_na), x_na %in% table_na)
    
    # Test negation
    expect_equal(!rxode2:::.in(x, table), !(x %in% table))
  })
  
  test_that(".in() function with fastmatch backend (if available)", {
    skip_if_not_installed("fastmatch")
    
    # Enable fastmatch
    rxode2:::inUseFastMatch(TRUE)
    
    x <- c(1, 2, 3, 4, 5)
    table <- c(2, 4, 6, 8)
    
    # Test basic functionality with fastmatch
    expect_equal(rxode2:::.in(x, table), x %in% table)
    
    # Test with character vectors
    x_char <- c("a", "b", "c", "d")
    table_char <- c("b", "d", "f")
    expect_equal(rxode2:::.in(x_char, table_char), x_char %in% table_char)
    
    # Test with larger vectors for performance
    x_large <- 1:1000
    table_large <- seq(2, 1000, by = 2)
    expect_equal(rxode2:::.in(x_large, table_large), x_large %in% table_large)
  })
  
  test_that("inUseFastMatch() function", {
    # Test that function returns boolean
    result <- rxode2::inUseFastMatch(FALSE)
    expect_type(result, "logical")
    expect_length(result, 1)
    
    # Test enabling fastmatch (if available)
    if (requireNamespace("fastmatch", quietly = TRUE)) {
      result <- rxode2::inUseFastMatch(TRUE)
      expect_true(result)
    } else {
      # If fastmatch is not available, should return FALSE
      result <- rxode2::inUseFastMatch(TRUE)
      expect_false(result)
    }
    
    # Test disabling fastmatch
    result <- rxode2::inUseFastMatch(FALSE)
    expect_false(result)
  })
  
  test_that(".in() consistency between backends", {
    skip_if_not_installed("fastmatch")
    
    # Test vectors
    x <- c(1, 2, 3, 4, 5, NA, 7)
    table <- c(2, 4, 6, 8, NA)
    
    # Test with base match
    rxode2:::inUseFastMatch(FALSE)
    result_base <- rxode2:::.in(x, table)
    
    # Test with fastmatch
    rxode2:::inUseFastMatch(TRUE)
    result_fast <- rxode2:::.in(x, table)
    
    # Results should be identical
    expect_equal(result_base, result_fast)
    
    # Test with character vectors
    x_char <- c("apple", "banana", "cherry", NA, "date")
    table_char <- c("banana", "date", "fig", NA)
    
    rxode2:::inUseFastMatch(FALSE)
    result_base_char <- rxode2:::.in(x_char, table_char)
    
    rxode2:::inUseFastMatch(TRUE)
    result_fast_char <- rxode2:::.in(x_char, table_char)
    
    expect_equal(result_base_char, result_fast_char)
  })
  
  test_that(".in() handles edge cases", {
    # Single element vectors
    expect_equal(rxode2:::.in(1, c(1, 2, 3)), c(TRUE))
    expect_equal(rxode2:::.in(4, c(1, 2, 3)), c(FALSE))
    
    # All elements match
    x <- c(1, 2, 3)
    expect_equal(rxode2:::.in(x, x), rep(TRUE, length(x)))
    
    # No elements match
    expect_equal(rxode2:::.in(c(1, 2, 3), c(4, 5, 6)), rep(FALSE, 3))
    
    # Duplicates in input
    x <- c(1, 1, 2, 2, 3, 3)
    table <- c(2, 4)
    expect_equal(rxode2:::.in(x, table), x %in% table)
    
    # Duplicates in table
    x <- c(1, 2, 3)
    table <- c(2, 2, 4, 4)
    expect_equal(rxode2:::.in(x, table), x %in% table)
  })
})
