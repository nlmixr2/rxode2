test_that("phi/pnorm/qnorm", {
  expect_equal(phi(1:3), pnorm(1:3))
  expect_equal(phi(as.double(1:3)), pnorm(as.double(1:3)))
  
  o <- rxode2({
    o <- phi(a)
  })
  
  expect_equal(
    rxSolve(o, data.frame(a = 1:3), et(0))$o,
    pnorm(as.double(1:3))
  )
  
  o <- rxode2({
    o <- pnorm(a)
  })
  
  expect_equal(
    rxSolve(o, data.frame(a = 1:3), et(0))$o,
    pnorm(as.double(1:3))
  )
  
  o <- rxode2({
    o <- pnorm(a, 0.5)
  })
  
  expect_equal(
    rxSolve(o, data.frame(a = 1:3), et(0))$o,
    pnorm(as.double(1:3), 0.5)
  )
  
  o <- rxode2({
    o <- pnorm(a, 0.5, 2)
  })
  
  expect_equal(
    rxSolve(o, data.frame(a = 1:3), et(0))$o,
    pnorm(as.double(1:3), 0.5, 2)
  )
  
  suppressMessages(expect_error(rxode2({
    o <- pnorm()
  })))
  
  suppressMessages(expect_error(rxode2({
    o <- pnorm(a, b, c, d)
  })))
  
  o <- rxode2({
    o <- qnorm(a)
  })
  
  expect_equal(
    rxSolve(o, data.frame(a = 1:3), et(0))$o,
    suppressWarnings(qnorm(as.double(1:3)))
  )
  
  o <- rxode2({
    o <- qnorm(a, 0.5)
  })
  
  expect_equal(
    rxSolve(o, data.frame(a = 1:3), et(0))$o,
    suppressWarnings(qnorm(as.double(1:3), 0.5))
  )
  
  o <- rxode2({
    o <- qnorm(a, 0.5, 2)
  })
  
  expect_equal(
    rxSolve(o, data.frame(a = 1:3), et(0))$o,
    suppressWarnings(qnorm(as.double(1:3), 0.5, 2))
  )
  
  suppressMessages(expect_error(rxode2({
    o <- qnorm()
  })))
  
  suppressMessages(expect_error(rxode2({
    o <- qnorm(a, b, c, d)
  })))
  
  m <- rxode2({
    o <- pnorm(a)
  })

  skip_if_not_installed("units")
  expect_error(rxS(m), NA)
    
  m <- rxode2({
    o <- pnorm(a, b)
  })
  
  expect_error(rxS(m), NA)
  
  m <- rxode2({
    o <- pnorm(a, b, c)
  })
  
  expect_error(rxS(m), NA)
})
