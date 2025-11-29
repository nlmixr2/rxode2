rxTest({

  mv <- rxModelVars("a=b")

  test_that("rxSerialize and rxDeserialize work with model variables", {
    r.qs2 <- rxSerialize(mv, "qs2")
    expect_equal(rxDeserialize(r.qs2), mv)
    r.qdata <- rxSerialize(mv, "qdata")
    expect_equal(rxDeserialize(r.qdata), mv)
    r.base <- rxSerialize(mv, "base")
    expect_equal(rxDeserialize(r.base), mv)
  })

  df <- data.frame(a=1:10, b=11:20)

  test_that("rxSerialize and rxDeserialize work with simple data", {
    r.qs2 <- rxSerialize(df, "qs2")
    expect_equal(rxDeserialize(r.qs2), df)
    r.qdata <- rxSerialize(df, "qdata")
    expect_equal(rxDeserialize(r.qdata), df)
    r.base <- rxSerialize(df, "base")
    expect_equal(rxDeserialize(r.base), df)
  })

  test_that("rxSerialize and rxDeserialize fail with bad input", {

    badInput <- "not a good object"

    expect_error(rxSerialize(badInput))

    expect_error(rxDeserialize(badInput))

    expect_error(rxDeserialize("av"))

    q <- qs2::qs_serialize(badInput)

    expect_error(rxDeserialize(q))

  })


})
