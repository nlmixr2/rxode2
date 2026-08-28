rxTest({
  # ceiling() is the name R users write; rxode2 only knew C's ceil().  It was
  # not in the arity table, so it fell through to the R user-function branch,
  # found base R's PRIMITIVE ceiling, and formals() of a primitive is empty --
  # hence "user function 'ceiling' requires 0 arguments (supplied 1)" for a
  # perfectly ordinary call.  floor() worked the whole time, which is the
  # asymmetry that gave it away.

  test_that("ceiling() parses, compiles and matches ceil()", {
    .m <- rxode2::rxode2("b = t; y1 = ceiling(b); y2 = ceil(b)\nd/dt(x) = -x")
    .r <- rxode2::rxSolve(.m, rxode2::et(seq(0, 4, by = 0.5)) |> rxode2::et(amt = 1),
                          returnType = "data.frame")
    expect_equal(.r$y1, .r$y2)
    expect_equal(.r$y1, ceiling(.r$time))
  })

  test_that("ceiling() has a derivative, and it is 0", {
    # locally constant, like ceil()/floor(): 0 almost everywhere
    expect_equal(rxode2::rxFromSE("Derivative(ceiling(x), x)"), "0")
    expect_equal(rxode2::rxFromSE("Derivative(ceil(x), x)"), "0")
    expect_equal(rxode2::rxFromSE("Derivative(floor(x), x)"), "0")
  })

  test_that("ceiling() differentiates through the symbolic machinery", {
    .s <- rxode2::rxS("cl = ceiling(p)*2\nd/dt(A) = -cl*A\n")
    .j <- rxode2::.rxJacobian(.s)
    expect_true(any(grepl("df(A)/dy(A)=-2*ceiling(p)", .s$..jacobian, fixed = TRUE)))
    # the derivative with respect to p carries the 0
    expect_true(any(grepl("df(A)/dy(p)=", .s$..jacobian, fixed = TRUE)))
  })

  test_that("a sensitivity model using ceiling() builds and solves", {
    .m <- rxode2::rxode2("cl = ceiling(lcl)*0.1 + 0.5\nd/dt(A) = -cl*A",
                         calcSens = c("lcl"))
    .r <- rxode2::rxSolve(.m, rxode2::et(0:3) |> rxode2::et(amt = 1),
                          params = c(lcl = 1.7), returnType = "data.frame")
    expect_true("rx__sens_A_BY_lcl__" %in% names(.r))
    expect_true(all(.r$rx__sens_A_BY_lcl__ == 0))
  })

  test_that("ceiling() takes exactly one argument", {
    # enforced where it matters, by the parser, exactly as for ceil().  Note
    # Derivative(ceiling(x, y), x) does NOT raise: ceiling is locally constant,
    # so .rxSEzeroD collapses the whole node to 0 before any arity check --
    # the same shortcut ceil() and floor() have always had.
    expect_error(rxode2::rxode2("y=ceiling(a,b)\nd/dt(x)=-x"), "syntax")
    expect_equal(rxode2::rxFromSE("Derivative(ceiling(x, y), x)"), "0")
    expect_equal(rxode2::rxFromSE("Derivative(ceil(x, y), x)"), "0")
  })

  test_that("the whole locally-constant family round-trips", {
    for (.f in c("floor", "ceil", "ceiling", "round", "trunc", "ftrunc", "sign")) {
      .x <- paste0(.f, "(a)")
      expect_equal(rxode2::rxToSE(.x), .x)
    }
    for (.f in c("fround", "fprec")) {
      .x <- paste0(.f, "(a,b)")
      expect_equal(rxode2::rxToSE(.x), .x)
    }
  })
})
