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

})


f <- function() {
  expect_error(rxode2parse('a <- "matt"; a<- 2'))
  expect_error(rxode2parse('a <- "matt"; a<- 1'), NA)
  expect_error(rxode2parse('a <- "matt"; a <- "rxode2"; a<- 2'), NA)
  expect_error(rxode2parse('a <- "matt"; a <- "rxode2"; a<- 3'))
  expect_error(rxode2parse('a <- "matt"; a(0)<- 2'))
  expect_error(rxode2parse('a <- b; a <- "str"'))
  expect_error(rxode2parse('a <- 1; a <- "str"'))
  expect_error(rxode2parse("d/dt(a) <- -kel; a <- \"str\""))
  expect_error(rxode2parse("rate(a) <- -kel; a <- \"str\""))
  expect_error(rxode2parse("dur(a) <- -kel; a <- \"str\""))
  expect_error(rxode2parse("alag(a) <- -kel; a <- \"str\""))
  expect_error(rxode2parse("a(0) <- -kel; a <- \"str\""))
  expect_error(rxode2parse("a(0) <- 1; a <- \"str\""))
  expect_error(rxode2parse('a <- "matt"; d/dt(a)<- 2'))
  expect_error(rxode2parse('a <- "matt"; rate(a)<- 2'))
  expect_error(rxode2parse('a <- "matt"; dur(a)<- 2'))
  expect_error(rxode2parse('a <- "matt"; alag(a)<- 2'))
}

test_that("test lhs string assign rxode2.syntax.allow.ini=TRUE", {
  withr::with_options(list(rxode2.syntax.allow.ini=TRUE,
                           rxode2.syntax.require.ode.first = FALSE), {
                             f()
                           })
})

test_that("test lhs string assign rxode2.syntax.allow.ini=FALSE", {
  withr::with_options(list(rxode2.syntax.allow.ini=TRUE,
                           rxode2.syntax.require.ode.first = FALSE), {
                             f()
                           })
})

test_that("lhs solve; tests lhs assign & str equals with lhs", {

  rx <- rxode2({
    if (t < 10) {
      a <- "<10"
    } else {
      a <- ">=10"
    }
    b <- 1
    if (a == "<10") {
      b <- 0;
    }
  })

  e <- et(1:20)

  s <-rxSolve(rx, e, returnType = "data.frame")

  expect_true(all(s$a[s$time < 10] == "<10"))
  expect_true(all(s$a[s$time >= 10] == ">=10"))
  expect_true(all(s$b[s$time < 10] == 0))
  expect_true(all(s$b[s$time >= 10] == 1))
})


test_that("lhs solve; tests lhs levels & str equals with lhs", {

  rx <- rxode2({
    levels(a) <- c("<10", ">=10")
    if (t < 10) {
      a <- 1
    } else {
      a <- 2
    }
    b <- 1
    if (a == "<10") {
      b <- 0;
    }
  })

  e <- et(1:20)

  s <-rxSolve(rx, e, returnType = "data.frame")

  expect_true(all(s$a[s$time < 10] == "<10"))
  expect_true(all(s$a[s$time >= 10] == ">=10"))
  expect_true(all(s$b[s$time < 10] == 0))
  expect_true(all(s$b[s$time >= 10] == 1))

})


test_that("levels1 statement solve", {

  rx <- rxode2({
    levels(a) <- "<10"
    if (t < 10) {
      a <- 1
    } else {
      a <- ">=10"
    }
    b <- 1
    if (a == "<10") {
      b <- 0;
    }
  })

  e <- et(1:20)

  s <-rxSolve(rx, e, returnType = "data.frame")

  expect_true(all(s$a[s$time < 10] == "<10"))
  expect_true(all(s$a[s$time >= 10] == ">=10"))
  expect_true(all(s$b[s$time < 10] == 0))
  expect_true(all(s$b[s$time >= 10] == 1))

})

test_that("levels extraction", {

  rx <- function() {
    model({
      levels(a) <- c("<10", ">=10")
      if (t < 10) {
        a <- 1
      } else {
        a <- 2
      }
      b <- 1
      if (a == "<10") {
        b <- 2;
      }
    })
  }

  rx <- rx()

  expect_equal(rx$levels,
               list(str2lang("levels(a) <- c(\"<10\", \">=10\")")))

  rx <- function() {
    model({
      levels(a) <- c("<10", ">=10")
      levels(b) <- c("low", "high")
      if (t < 10) {
        a <- 1
      } else {
        a <- 2
      }
      b <- 1
      if (a == "<10") {
        b <- 2;
      }
    })
  }

  rx <- rx()

  expect_equal(rx$levels,
               list(str2lang("levels(a) <- c(\"<10\", \">=10\")"),
                    str2lang("levels(b) <- c(\"low\", \"high\")")))


  rx <- function() {
    model({
      levels(a) <- c("<10", ">=10")
      levels(b) <- c("low", "high")
      levels(c) <- c("funny")
      if (t < 10) {
        a <- 1
      } else {
        a <- 2
      }
      b <- 1
      if (a == "<10") {
        b <- 2;
      }
      c <- 1
    })
  }

  rx <- rx()

  expect_equal(rx$levels,
               list(str2lang("levels(a) <- c(\"<10\", \">=10\")"),
                    str2lang("levels(b) <- c(\"low\", \"high\")"),
                    str2lang("levels(c) <- \"funny\"")))


})
