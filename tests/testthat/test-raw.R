rxTest({

  mv <- rxModelVars("a=b")

  test_that("rxSerialize and rxDeserialize work with model variables", {
    r.base <- rxSerialize(mv, "base")
    expect_equal(rxDeserialize(r.base), mv)
    r.base <- rxSerialize(mv, "bzip2")
    expect_equal(rxDeserialize(r.base), mv)
    r.base <- rxSerialize(mv, "xz")
    expect_equal(rxDeserialize(r.base), mv)
  })

  df <- data.frame(a=1:10, b=11:20)

  test_that("rxSerialize and rxDeserialize work with simple data", {
    r.base <- rxSerialize(df, "base")
    expect_equal(rxDeserialize(r.base), df)
    r.base <- rxSerialize(df, "bzip2")
    expect_equal(rxDeserialize(r.base), df)
    r.base <- rxSerialize(df, "xz")
    expect_equal(rxDeserialize(r.base), df)
  })

  test_that("rxSerialize and rxDeserialize fail with bad input", {

    badInput <- "not a good object"

    expect_error(rxSerialize(badInput))

    expect_error(rxDeserialize(badInput))

    expect_error(rxDeserialize("av"))

    expect_error(rxSerialize(df, "qs2"))

  })

  test_that("serial type", {
    expect_equal(rxGetSerialType_(as.raw("0")), "unknown")
    # qs/qs2 are not dependencies, test by header only
    expect_equal(rxGetSerialType_(as.raw(c(0xFD, 0x37, 0x7A, 0x58, 0x5A, 0x00))), "xz")
    expect_equal(rxGetSerialType_(charToRaw("BZh")), "bzip2")
    expect_equal(rxGetSerialType_(as.raw(c(0x0B, 0x0E, 0x0A, 0x0C))), "qs")
    expect_equal(rxGetSerialType_(as.raw(c(0x0B, 0x0E, 0x0A, 0xC1))), "qs2")
    expect_equal(rxGetSerialType_(as.raw(c(0x0B, 0x0E, 0x0A, 0xCD))), "qdata")
    expect_equal(rxGetSerialType_(serialize("matt", NULL)), "base")
    expect_error(rxGetSerialType_(4))
    expect_equal(rxGetSerialType_(as.raw(c(0x58, 0x0A))), "base")
    expect_equal(rxGetSerialType_(as.raw(c(0x41, 0x0A))), "base")
    expect_equal(rxGetSerialType_(as.raw(c(0x42, 0x0A))), "base")
    expect_equal(rxGetSerialType_(as.raw(c(0x43, 0x0A))), "unknown")
    expect_equal(rxGetSerialType_(as.raw(c(0x43, 0x0A, 0x0A))), "unknown")
  })

  test_that("qs2 deserialization is supported when qs2 is installed", {
    skip_if_not_installed("qs2")
    qsSer <- getExportedValue("qs2", "qs_serialize")
    qdSer <- getExportedValue("qs2", "qd_serialize")
    expect_equal(rxDeserialize(qsSer(mv)), mv)
    expect_equal(rxDeserialize(qdSer(mv)), mv)
    expect_equal(rxDeserialize(qsSer(df)), df)
    expect_equal(rxDeserialize(qdSer(df)), df)
    expect_error(rxDeserialize(qsSer("not a good object")))
  })

})
