rxTest({
  test_that(".chin function with base match()", {
    # Set to use base match
    chinUseFastMatch(FALSE)
    
    # Test basic membership
    x <- c(1, 2, 3, 4, 5)
    table <- c(2, 4, 6)
    result <- rxode2:::.chin(x, table)
    expected <- c(FALSE, TRUE, FALSE, TRUE, FALSE)
    expect_equal(result, expected)
    
    # Test with characters
    x <- c("a", "b", "c", "d")
    table <- c("b", "d", "f")
    result <- rxode2:::.chin(x, table)
    expected <- c(FALSE, TRUE, FALSE, TRUE)
    expect_equal(result, expected)
    
    # Test with empty table
    x <- c(1, 2, 3)
    table <- numeric(0)
    result <- rxode2:::.chin(x, table)
    expected <- c(FALSE, FALSE, FALSE)
    expect_equal(result, expected)
    
    # Test with empty x
    x <- numeric(0)
    table <- c(1, 2, 3)
    result <- rxode2:::.chin(x, table)
    expected <- logical(0)
    expect_equal(result, expected)
    
    # Test with NA values
    x <- c(1, NA, 3)
    table <- c(1, 3)
    result <- rxode2:::.chin(x, table)
    expected <- c(TRUE, FALSE, TRUE)
    expect_equal(result, expected)
  })
  
  test_that(".chin function with fastmatch (if available)", {
    # Try to use fastmatch
    usedFastMatch <- chinUseFastMatch(TRUE)
    
    # Test basic membership
    x <- c(1, 2, 3, 4, 5)
    table <- c(2, 4, 6)
    result <- rxode2:::.chin(x, table)
    expected <- c(FALSE, TRUE, FALSE, TRUE, FALSE)
    expect_equal(result, expected)
    
    # Test with characters
    x <- c("a", "b", "c", "d")
    table <- c("b", "d", "f")
    result <- rxode2:::.chin(x, table)
    expected <- c(FALSE, TRUE, FALSE, TRUE)
    expect_equal(result, expected)
    
    # Test that results are consistent between base and fastmatch
    chinUseFastMatch(FALSE)
    x <- c(1, 2, 3, 4, 5)
    table <- c(2, 4, 6)
    result_base <- rxode2:::.chin(x, table)
    
    chinUseFastMatch(TRUE)
    result_fast <- rxode2:::.chin(x, table)
    
    expect_equal(result_base, result_fast)
  })
  
  test_that("chinUseFastMatch function", {
    # Test that chinUseFastMatch returns a logical value
    result <- chinUseFastMatch(TRUE)
    expect_type(result, "logical")
    
    # Test setting to FALSE
    result <- chinUseFastMatch(FALSE)
    expect_false(result)
    
    # Test that it's stored in environment
    expect_false(rxode2:::.forderEnv$useFastMatch)
    
    # Test setting back to TRUE
    result <- chinUseFastMatch(TRUE)
    expect_type(result, "logical")
    
    # If fastmatch is available, it should be TRUE, otherwise FALSE
    if (requireNamespace("fastmatch", quietly = TRUE)) {
      expect_true(result)
      expect_true(rxode2:::.forderEnv$useFastMatch)
    } else {
      expect_false(result)
      expect_false(rxode2:::.forderEnv$useFastMatch)
    }
  })
  
  test_that(".chin equivalence with %in%", {
    # Test that .chin produces same results as %in%
    chinUseFastMatch(FALSE)
    
    x <- c(1, 2, 3, 4, 5)
    table <- c(2, 4, 6)
    
    result_chin <- rxode2:::.chin(x, table)
    result_in <- x %in% table
    
    expect_equal(result_chin, result_in)
    
    # Test with characters
    x <- c("a", "b", "c", "d")
    table <- c("b", "d", "f")
    
    result_chin <- rxode2:::.chin(x, table)
    result_in <- x %in% table
    
    expect_equal(result_chin, result_in)
  })
})
