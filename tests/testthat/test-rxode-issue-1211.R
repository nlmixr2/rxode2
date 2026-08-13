rxTest({
  # rxode2#1211: the statement form of ifelse -- where each branch is a
  # statement rather than a value -- appended the opening brace to whatever the
  # previous statement had left in the code buffers, so it only compiled when it
  # was the first statement of the model.

  test_that("ifelse() statement form compiles after a preceding assignment", {
    m <- rxode2("
kin = 3
ifelse(t<2, b <- 1, b <- 2)
d/dt(a) <- -a + b*kin
")
    expect_s3_class(m, "rxode2")
    expect_equal(rxNorm(m),
                 "kin=3;\nif (t<2){\nb=1;\n}\nelse {\nb=2;\n}\nd/dt(a)=-a+b*kin;\n")
  })

  test_that("ifelse() statement form compiles after a d/dt()", {
    m <- rxode2("
d/dt(a) <- -a
ifelse(t<2, b <- 1, b <- 2)
d/dt(c2) <- b
")
    expect_s3_class(m, "rxode2")
    expect_equal(rxNorm(m),
                 "d/dt(a)=-a;\nif (t<2){\nb=1;\n}\nelse {\nb=2;\n}\nd/dt(c2)=b;\n")
  })

  test_that("ifelse() statement form normalizes to a re-parsable fixed point", {
    .norm <- function(txt) rxNorm(rxode2(txt))
    .models <- c(
      "kin = 3\nifelse(t<2, b <- 1, b <- 2)\nd/dt(a) <- -a + b",
      "q=1\nifelse(t<2, ifelse(t<1, b <- 1, b <- 2), b <- 3)\nd/dt(a) <- -a+b",
      "q=1\nif (q>0) {\n ifelse(t<2, b <- 1, b <- 2)\n}\nd/dt(a) <- -a+b",
      "q=1\nifelse(t<2, d/dt(a) <- -a, d/dt(a) <- -2*a)\n",
      "q=1\nifelse(t<2, b <- 1, b <- 2)\nifelse(t<3, c2 <- 1, c2 <- 2)\nd/dt(a) <- -a+b+c2"
    )
    for (txt in .models) {
      .n1 <- .norm(txt)
      expect_equal(.norm(.n1), .n1)
    }
  })

  test_that("ifelse() statement form is equivalent to the if/else statement", {
    .ie <- rxode2("
d/dt(a) <- -a
ifelse(t<2, b <- 1, b <- 2)
d/dt(c2) <- b
")
    .if <- rxode2("
d/dt(a) <- -a
if (t<2) { b <- 1 } else { b <- 2 }
d/dt(c2) <- b
")
    expect_equal(rxNorm(.ie), rxNorm(.if))
    .ev <- et(seq(0, 5, by = 0.5)) |> et(amt = 10, cmt = 1)
    expect_equal(rxSolve(.ie, .ev, returnType = "data.frame"),
                 rxSolve(.if, .ev, returnType = "data.frame"))
  })

  test_that("ifelse() statement form carries break inside a while()", {
    m <- rxode2("
q = 1
while (q > 0) {
  ifelse(q < 0.4, break, q <- q - 0.25)
}
d/dt(a) <- -a + q
")
    expect_equal(rxNorm(m),
                 paste0("q=1;\nwhile (q>0){\nif (q<0.4){\nbreak;\n}\nelse {\n",
                        "q=q-0.25;\n}\n}\nd/dt(a)=-a+q;\n"))
    .s <- rxSolve(m, et(0:3) |> et(amt = 10, cmt = 1), returnType = "data.frame")
    expect_equal(unique(.s$q), 0.25)
  })

  test_that("ifelse() statement form works in symengine derivatives", {
    .mod <- function(cond) {
      sprintf(paste0("ka <- exp(tka)\ncl <- exp(tcl)\nv <- exp(tv)\n%s\n",
                     "d/dt(depot) <- -ka*depot\n",
                     "d/dt(center) <- ka*depot - cl/v*center*fac\ncp <- center/v"),
              cond)
    }
    .ie <- .mod("ifelse(t < 2, fac <- 1, fac <- 2)")
    .if <- .mod("if (t < 2) { fac <- 1 } else { fac <- 2 }")
    expect_equal(rxPrune(rxode2(.ie)), rxPrune(rxode2(.if)))
    .sie <- suppressMessages(rxode2(.ie, calcSens = c("tka", "tcl", "tv")))
    .sif <- suppressMessages(rxode2(.if, calcSens = c("tka", "tcl", "tv")))
    expect_equal(rxNorm(.sie), rxNorm(.sif))
    expect_true(any(grepl("rx__sens_center_BY_tcl__", rxState(.sie), fixed = TRUE)))
  })

  test_that("the ifelse() expression form is unaffected", {
    m <- rxode2("
q = 1
b <- ifelse(t<2, 1, 2)
d/dt(a) <- -a + b*q
")
    expect_equal(rxNorm(m), "q=1;\nb=ifelse(t<2,1,2);\nd/dt(a)=-a+b*q;\n")
  })
})
