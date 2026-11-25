rxTest({
  .n <- function(x, p = "linCmtB(") {
    lengths(regmatches(x, gregexpr(p, x, fixed = TRUE)))
  }
  .L <- function(w1, w2, args = "cl,v") {
    sprintf("linCmtB(rx__PTR__,t,1,1,0,%d,%d,1,%s,0,0,0,0,0)", w1, w2, args)
  }

  test_that("rxOptExpr() shares a pure linCmtB() read written several times", {
    txt <- paste0(
      "cl <- exp(tcl + eta.cl)\nv <- exp(tv)\n",
      "a = ", .L(-1, -1), " * 2\nb = ", .L(-1, -1), " + 3\n",
      "c = log(", .L(-1, -1), ")\nd = ", .L(-2, 0), " + ", .L(-2, 0), "\n"
    )
    o <- suppressMessages(rxOptExpr(txt, "x"))
    expect_equal(.n(o), 2L)
    m0 <- rxode2(txt)
    m1 <- rxode2(o)
    ev <- et(amt = 100, cmt = "central") |> et(seq(0.5, 12, by = 0.5))
    p <- c(tcl = log(2), tv = log(20), eta.cl = 0.1)
    expect_equal(
      as.data.frame(rxSolve(m1, p, ev))[, c("a", "b", "c", "d")],
      as.data.frame(rxSolve(m0, p, ev))[, c("a", "b", "c", "d")]
    )
  })

  test_that("a read with a linCmtB() sentinel between two copies is not merged", {
    txt <- paste0(
      "a = ", .L(-33, 0), " * 2\n",
      "z ~ ", .L(-32, 0), "\n",
      "b = ", .L(-33, 0), " + 3\n"
    )
    o <- suppressMessages(rxOptExpr(txt, "x"))
    expect_equal(.n(o), 3L)
    # and the sentinel itself is never shared
    txt2 <- paste0("z1 ~ ", .L(-32, 0), "\nz2 ~ ", .L(-32, 0), "\n")
    expect_equal(.n(suppressMessages(rxOptExpr(txt2, "x"))), 2L)
  })

  test_that("rxOptExpr() passes until nested repeats are shared too", {
    L <- .L(-1, -1, "exp(ETA[1]+THETA[2]),exp(THETA[3])")
    txt <- paste0(
      "a = ", L, " * 2\nb = ", L, " + 3\nc = log(", L, ")\n",
      "d = exp(ETA[1]+THETA[2])*(1+exp(ETA[1]+THETA[2]))^2\n",
      "e = (1+exp(ETA[1]+THETA[2]))^2 + 4\n"
    )
    o <- suppressMessages(rxOptExpr(txt, "x"))
    expect_equal(.n(o), 1L)
    expect_equal(.n(o, "^2"), 0L)
    # a second rxOptExpr() by the user still warns; the internal passes do not
    expect_no_warning(suppressMessages(rxOptExpr(txt, "x")))
    expect_warning(suppressMessages(rxOptExpr(o, "x")), "already has common subexpression")
  })
})
