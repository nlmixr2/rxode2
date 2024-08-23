test_that("test lhs string assign lhs expression information", {
  p <- rxode2parse('a="oh no"')
  expect_equal(p$lhs, "a")
  expect_equal(p$slhs, character(0))
  expect_equal(p$strAssign, list(a = "oh no"))

  p <- rxode2parse('a<-"oh no"')
  expect_equal(p$lhs, "a")
  expect_equal(p$slhs, character(0))
  expect_equal(p$strAssign, list(a = "oh no"))

  p <- rxode2parse('a~"oh no"')
  expect_equal(p$slhs, "a")
  expect_equal(p$lhs, character(0))
  expect_equal(p$strAssign, list(a = "oh no"))

  p <- rxode2parse('a<-"oh no"\nb<-3+40')
  expect_equal(p$lhs, c("a", "b"))
  expect_equal(p$slhs, character(0))
  expect_equal(p$strAssign, list(a = "oh no"))
  expect_equal(p$lhsStr, c(a = TRUE, b = FALSE))

  p <- rxode2parse('
if (APGAR == 10 || APGAR == 8 || APGAR == 9) {
    tAPGAR <- "High"
  } else if (APGAR == 1 || APGAR == 2 || APGAR == 3) {
    tAPGAR <- "Low"
  } else if (APGAR == 4 || APGAR == 5 || APGAR == 6 || APGAR == 7) {
    tAPGAR <- "Med"
  } else {
    tAPGAR<- "Med"
  }
')

  expect_equal(p$strAssign, list(tAPGAR = c("High", "Low", "Med")))
  expect_equal(p$lhsStr, c(tAPGAR = TRUE))
  expect_equal(p$lhs, "tAPGAR")
  expect_equal(p$slhs, character(0))
  expect_equal(p$params , "APGAR")
  expect_equal(p$model["normModel"],
               c(normModel = "if (APGAR==10||APGAR==8||APGAR==9){\ntAPGAR <-\"High\";\n}\nelse {\nif (APGAR==1||APGAR==2||APGAR==3){\ntAPGAR <-\"Low\";\n}\nelse {\nif (APGAR==4||APGAR==5||APGAR==6||APGAR==7){\ntAPGAR <-\"Med\";\n}\nelse {\ntAPGAR <-\"Med\";\n}\n}\n}\n"))


  p <- rxode2parse('
if (APGAR == 10 || APGAR == 8 || APGAR == 9) {
    tAPGAR <- "High"
  } else if (APGAR == 1 || APGAR == 2 || APGAR == 3) {
    tAPGAR <- "Low"
  } else if (APGAR == 4 || APGAR == 5 || APGAR == 6 || APGAR == 7) {
    tAPGAR <- "Med"
  } else {
    tAPGAR<- "Med"
  }
   tAPGAR <- 1
')

  p <- rxode2parse('levels(tAPGAR) <- c("High", "Med", "Low")\ntAPGAR <- "Low"')

  expect_equal(p$strAssign, list(tAPGAR = c("High", "Med", "Low")))
  expect_equal(p$lhsStr, c(tAPGAR = TRUE))
  expect_equal(p$lhs, "tAPGAR")
  expect_equal(p$slhs, character(0))
  expect_equal(p$model["normModel"],
               c(normModel = "levels(tAPGAR) <- c(\"High\", \"Med\", \"Low\");\ntAPGAR <-\"Low\";\n"))

  f <- function() {
    expect_error(rxode2parse('a <- "matt"; a<- 2'))
    expect_error(rxode2parse('a <- "matt"; a<- 1'), NA)
    expect_error(rxode2parse('a <- "matt"; a <- "rxode2"; a<- 2'), NA)
    expect_error(rxode2parse('a <- "matt"; a <- "rxode2"; a<- 3'))
    expect_error(rxode2parse('a <- "matt"; a(0)<- 2'))
  }

  withr::with_options(list(rxode2.syntax.allow.ini=TRUE), {
    f()
  })

  withr::with_options(list(rxode2.syntax.allow.ini=FALSE), {
    f()
  })


})
