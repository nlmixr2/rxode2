rxodeTest({
  context("test id==\"item\"")

  f1 <- rxode2({
    a <- 1
    if (id == "matt") {
      a <- 2
    }
  })

  df <- data.frame(
    ID = c("matt", "not"),
    TIME = c(0, 1)
  )

  ref <- rxSolve(f1, df)

  test_that("id==matt #1", {
    expect_equal(ref$a, c(2, 1))
  })

  df <- data.frame(
    ID = c("not", "matt"),
    TIME = c(0, 1)
  )

  ref <- rxSolve(f1, df)

  test_that("id==matt #2", {
    expect_equal(ref$a, c(1, 2))
  })

  f2 <- rxode2({
    a <- 1
    if ("matt" == ID) {
      a <- 2
    }
  })

  df <- data.frame(
    ID = c("matt", "not"),
    TIME = c(0, 1)
  )

  ref <- rxSolve(f2, df)

  test_that("id==matt #3", {
    expect_equal(ref$a, c(2, 1))
  })

  df <- data.frame(
    ID = c("not", "matt"),
    TIME = c(0, 1)
  )

  ref <- rxSolve(f2, df)

  test_that("id==mat #4", {
    expect_equal(ref$a, c(1, 2))
  })

  f1 <- rxode2({
    a <- 1
    if (id != "matt") {
      a <- 2
    }
  })

  df <- data.frame(
    ID = c("matt", "not"),
    TIME = c(0, 1)
  )

  ref <- rxSolve(f1, df)

  test_that("id!=matt #1", {
    expect_equal(ref$a, c(1, 2))
  })

  df <- data.frame(
    ID = c("not", "matt"),
    TIME = c(0, 1)
  )

  ref <- rxSolve(f1, df)

  test_that("id!=matt #2", {
    expect_equal(ref$a, c(2, 1))
  })

  f1 <- rxode2({
    a <- 1
    if (id == "100") {
      a <- 2
    }
  })

  df <- data.frame(
    ID = c(100, 200),
    TIME = c(0, 1)
  )

  ref <- rxSolve(f1, df)

  test_that("id2", {
    expect_equal(ref$a, c(2, 1))
  })

  expect_error(rxode2({
    a <- 1
    if (id == 100) {
      a <- 2
    }
  }))


  f1 <- rxode2({
    a <- 1 + (id == "100") * 2
  })

  ref <- rxSolve(f1, df)

  test_that("id3", {
    expect_equal(ref$a, c(3, 1))
  })

  f1 <- rxode2({
    a <- 1 + (id == "100") * 2
  })

  ref <- rxSolve(f1, df)

  test_that("id3", {
    expect_equal(ref$a, c(3, 1))
  })

  context("test cmt==\"item\"/no.")

  f3 <- rxode2({
    a <- 3
    if (cmt == "depot") {
      a <- 2
    }
  })

  df <- data.frame(
    ID = c("not", "matt"),
    TIME = c(0, 1),
    cmt = structure(1:2, .Label = c("depot", "central"), class = "factor")
  )

  tmp <- rxSolve(f3, df)

  test_that("cmt==\"depot\"", {
    expect_equal(tmp$a, c(2, 3))
  })

  f3 <- rxode2({
    a <- 3
    if (cmt == 1) {
      a <- 2
    }
  })

  tmp <- rxSolve(f3, df)

  test_that("cmt==1", {
    expect_equal(tmp$a, c(2, 3))
  })

  f3 <- rxode2({
    a <- 3
    if (cmt != "depot") {
      a <- 2
    }
  })

  tmp <- rxSolve(f3, df)

  test_that("cmt!=\"depot\"", {
    expect_equal(tmp$a, c(3, 2))
  })

  f3 <- rxode2({
    a <- 3
    if (cmt != 1) {
      a <- 2
    }
  })

  test_that("cmt!=\"depot\"", {
    expect_equal(tmp$a, c(3, 2))
  })

  f3 <- rxode2({
    a <- 1
    if ("depot" == cmt) {
      a <- 2
    }
  })

  tmp <- rxSolve(f3, df)

  test_that("cmt==\"depot\"", {
    expect_equal(tmp$a, c(2, 1))
  })

  f3 <- rxode2({
    a <- 1
    if ("depot" != cmt) {
      a <- 2
    }
  })

  tmp <- rxSolve(f3, df)

  test_that("cmt==\"depot\"", {
    expect_equal(tmp$a, c(1, 2))
  })

  test_that("translation to and from SE", {
    a <- rxToSE("id==\"matt\"")
    expect_equal(a, "rxEq(id,rxQ__matt__rxQ)")
    b <- rxFromSE(a)
    expect_equal(b, "(id==\"matt\")")
  })

  tmp <- "C2=centr/V;\nC3=peri/V2;\nd/dt(depot)=-KA*depot;\nd/dt(centr)=KA*depot-CL*C2-Q*C2+Q*C3;\nd/dt(peri)=Q*C2-Q*C3;\nC4=CMT;\nif(CMT==\"depot\"){\nprd=depot;\n}\nif(CMT==\"centr\"){\nprd=centr;\n}\nif(CMT==\"peri\"){\nprd=peri;\n}\n"

  test_that("prune checks", {
    expect_equal(
      rxPrune(tmp),
      "C2=centr/V\nC3=peri/V2\nd/dt(depot)=-KA*depot\nd/dt(centr)=KA*depot-CL*C2-Q*C2+Q*C3\nd/dt(peri)=Q*C2-Q*C3\nC4=CMT\nprd=(CMT==\"depot\")*(depot)\nprd=(CMT==\"centr\")*(centr)+(1-((CMT==\"centr\")))*(prd)\nprd=(CMT==\"peri\")*(peri)+(1-((CMT==\"peri\")))*(prd)"
    )
  })
})
