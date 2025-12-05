rxTest({

  mv <- rxModelVars("a=b")

  test_that("rxSerialize and rxDeserialize work with model variables", {
    r.qs2 <- rxSerialize(mv, "qs2")
    expect_equal(rxDeserialize(r.qs2), mv)
    r.qdata <- rxSerialize(mv, "qdata")
    expect_equal(rxDeserialize(r.qdata), mv)
    r.base <- rxSerialize(mv, "base")
    expect_equal(rxDeserialize(r.base), mv)
    r.base <- rxSerialize(mv, "bzip2")
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
    r.base <- rxSerialize(df, "bzip2")
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

  test_that("serial type", {
    expect_equal(rxGetSerialType_(as.raw("0")), "unknown")
    # qs is off CRAN, test by header only
    expect_equal(rxGetSerialType_(charToRaw("BZh")), "bzip2")
    expect_equal(rxGetSerialType_(as.raw(c(0x0B,0x0E,0x0A,0x0C))), "qs")
    expect_equal(rxGetSerialType_(qs2::qs_serialize("matt")), "qs2")
    expect_equal(rxGetSerialType_(qs2::qd_serialize("matt")), "qdata")
    expect_equal(rxGetSerialType_(serialize("matt", NULL)), "base")
    expect_error(rxGetSerialType_(4))
    expect_equal(rxGetSerialType_(as.raw(c(0x58, 0x0A))), "base")
    expect_equal(rxGetSerialType_(as.raw(c(0x41, 0x0A))), "base")
    expect_equal(rxGetSerialType_(as.raw(c(0x42, 0x0A))), "base")
    expect_equal(rxGetSerialType_(as.raw(c(0x43, 0x0A))), "unknown")
    expect_equal(rxGetSerialType_(as.raw(c(0x43, 0x0A, 0x0A))), "unknown")
  })


})
