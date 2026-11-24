## rxOptExpr() must not share a temporary across a reassignment of one of its
## variables (issue #1782), in either the C pass or the R walker.
rxTest({
  .reassign <- "d/dt(x) <- -k*x\np <- x\na <- 3 - p\np <- p * 2\nb <- 3 - p\nc <- 3 - p\n"

  for (.c in c(TRUE, FALSE)) {
    test_that(sprintf("no temporary is reused after a reassignment (optExprC = %s)", .c), {
      withr::local_options(list(rxode2.optExprC = .c))
      .o <- suppressMessages(rxOptExpr(.reassign, chunkLines = 0L))
      .l <- strsplit(.o, "\n", fixed = TRUE)[[1]]
      expect_true("a=3-p" %in% .l)
      ## b and c still share the post-reassignment `3-p`, defined after `p=p*2`
      .t <- grep("~3-p$", .l)
      expect_length(.t, 1L)
      expect_gt(.t, which(.l == "p=p*2"))
      expect_false(any(grepl("rx__ssa", .l, fixed = TRUE)))
      .ev <- et(amt = 100) |> et(0:3)
      .s0 <- rxSolve(rxode2(.reassign), .ev, c(k = 0.1), returnType = "data.frame")
      .s1 <- rxSolve(rxode2(.o), .ev, c(k = 0.1), returnType = "data.frame")
      expect_equal(.s1$a, .s0$a)
      expect_equal(.s1$b, .s0$b)
      expect_equal(.s1$c, .s0$c)
    })
  }

  test_that("a variable read before its assignment is versioned too", {
    .o <- suppressMessages(rxOptExpr("a=3-p\np=2\nb=3-p\nd/dt(x)=-x", chunkLines = 0L))
    expect_false(grepl("rx_expr_", .o, fixed = TRUE))
  })

  test_that("the default (chunked) call is fixed too", {
    .o <- suppressMessages(rxOptExpr(.reassign))
    expect_true("a=3-p" %in% strsplit(.o, "\n", fixed = TRUE)[[1]])
  })

  test_that(".rxOptSsa versions assignments, blocks and loops", {
    expect_null(.rxOptSsa("a=3-p;\nb=3-p;"))
    .s <- .rxOptSsa("p=x;\na=3-p;\np=p*2;\nb=3-p;")
    expect_identical(
      strsplit(.s$txt, "\n", fixed = TRUE)[[1]],
      c("rx__ssa1__p=x;", "a=3-rx__ssa1__p;", "rx__ssa2__p=rx__ssa1__p*2;", "b=3-rx__ssa2__p;")
    )
    expect_identical(.rxOptSsaUndo(.s$txt, .s), "p=x;\na=3-p;\np=p*2;\nb=3-p;")
    ## leaving an if block gives a fresh name to what it assigned
    .s <- .rxOptSsa("p=x;\nif (p>2){\np=2*p;\n}\nb=3-p;")
    .l <- strsplit(.s$txt, "\n", fixed = TRUE)[[1]]
    expect_identical(.l[3], "rx__ssa2__p=2*rx__ssa1__p;")
    expect_identical(.l[5], "b=3-rx__ssa3__p;")
    ## a while loop reads the previous iteration, so it starts a fresh name
    .s <- .rxOptSsa("p=x;\na=3-p;\nwhile (i<3){\nc=3-p;\np=2*p;\n}\nb=3-p;")
    .l <- strsplit(.s$txt, "\n", fixed = TRUE)[[1]]
    expect_identical(.l[4], "c=3-rx__ssa2__p;")
    expect_identical(.l[7], "b=3-rx__ssa4__p;")
    ## a model already using the prefix gets another one
    .s <- .rxOptSsa("rx__ssa1=1;\np=x;\np=p*2;")
    expect_identical(.s$prefix, "rx__ssax")
  })
})
