rxTest({
  test_that("transPk -- k", {
    p3k <- readModelDb("PK_3cmt_des") |>
      pkTrans("k")
    expect_true(all(c("lkel", "lk12", "lk21", "lk13", "lk31") %in% names(p3k$theta)))

    # Issue #87 -- Make sure depot is not dropped
    expect_equal(modelExtract(p3k, "d/dt(depot)"), "d/dt(depot) <- -ka * depot")

    p2k <- readModelDb("PK_2cmt_des") |>
      pkTrans("k")
    expect_true(all(c("lkel", "lk12", "lk21") %in% names(p2k$theta)))
    expect_true(!any(c("lk13", "lk31") %in% names(p2k$theta)))

    p1k <- readModelDb("PK_1cmt_des") |>
      pkTrans("k")
    expect_true(all("lkel" %in% names(p1k$theta)))
    expect_true(!any(c("lk12", "lk21", "lk13", "lk31") %in% names(p1k$theta)))
  })

  test_that("transPk -- vss", {
    expect_error(
      readModelDb("PK_3cmt_des") |>
        pkTrans("vss"),
      "vss transformation only works for 2 compartment models"
    )

    expect_error(
      readModelDb("PK_1cmt_des") |>
        pkTrans("vss"),
      "vss transformation only works for 2 compartment models"
    )

    pk2 <- readModelDb("PK_2cmt_des") |>
      pkTrans("vss")

    expect_true(all(c("lcl", "lq", "lvc", "lvss") %in% names(pk2$theta)))
  })

  test_that("transPk -- aob", {
    expect_error(
      readModelDb("PK_3cmt_des") |>
        pkTrans("aob"),
      "aob transformation only works for 2 compartment models"
    )

    expect_error(
      readModelDb("PK_1cmt_des") |>
        pkTrans("aob"),
      "aob transformation only works for 2 compartment models"
    )

    pk2 <- readModelDb("PK_2cmt_des") |>
      pkTrans("aob")

    expect_true(all(c("laob", "lalpha", "lbeta", "lvc") %in% names(pk2$theta)))
  })

  test_that("transPk --k21", {
    pk3 <- readModelDb("PK_3cmt_des") |>
      pkTrans("k21")

    expect_true(all(c("lalpha", "lbeta", "lgam", "lk21", "lk31") %in% names(pk3$theta)))
    expect_true(!any(c("lk12", "lk13") %in% names(pk3$theta)))

    pk2 <- readModelDb("PK_2cmt_des") |>
      pkTrans("k21")

    expect_true(all(c("lalpha", "lbeta", "lk21") %in% names(pk2$theta)))
    expect_true(!any("lk12" %in% names(pk2$theta)))

    expect_error(
      readModelDb("PK_1cmt_des") |>
        pkTrans("k21"),
      "k21 transformation only works for 2 and 3 compartment models"
    )
  })

  test_that("transPk -- alpha", {
    pk3 <- readModelDb("PK_3cmt_des") |>
      pkTrans("alpha")

    expect_true(all(c("lalpha", "lbeta", "lgam", "lA", "lB", "lC") %in% names(pk3$theta)))

    pk2 <- readModelDb("PK_2cmt_des") |>
      pkTrans("alpha")

    expect_true(all(c("lalpha", "lbeta", "lA", "lB") %in% names(pk2$theta)))
    expect_true(!any(c("lgam", "lC") %in% names(pk2$theta)))

    pk1 <- readModelDb("PK_1cmt_des") |>
      pkTrans("alpha")

    expect_true(all(c("lalpha", "lA") %in% names(pk1$theta)))
    expect_true(!any(c("lbeta", "lgam", "lB", "lC") %in% names(pk1$theta)))
  })
})
