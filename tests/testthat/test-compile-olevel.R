rxTest({
  # `rxode2.compile.O` used to be inert: it lands in the generated model's
  # PKG_CFLAGS, and `R CMD SHLIB` builds
  # ALL_CFLAGS = $(PKG_CFLAGS) $(CPICFLAGS) $(SHLIB_CFLAGS) $(CFLAGS),
  # so R's own -O2 came last and the compiler took that one.
  test_that(".rxCompileOMakevars() puts the requested -O where CFLAGS wins", {
    .rcf <- gsub("\n", "", rawToChar(sys::exec_internal(
      file.path(R.home("bin"), "R"), c("CMD", "config", "CFLAGS"))$stdout))
    skip_if(.rcf == "", "no R CFLAGS to work from")

    withr::with_options(list(rxode2.compile.O = "3"), {
      .f <- .rxCompileOMakevars()
      # NULL only when R already compiles at the requested level
      skip_if(is.null(.f) && grepl("(^| )-O3( |$)", .rcf))
      expect_false(is.null(.f))
      on.exit(unlink(.f), add = TRUE)
      .l <- readLines(.f, warn = FALSE)
      .cf <- grep("^CFLAGS = ", .l, value = TRUE)[1]
      expect_true(!is.na(.cf))
      # exactly the requested level, and no leftover -O to override it
      expect_equal(regmatches(.cf, gregexpr("-O[0-9a-zA-Z]+", .cf))[[1]], "-O3")
      # R's other flags survive -- only the -O is swapped
      .kept <- setdiff(strsplit(trimws(.rcf), " +")[[1]], c("-O0","-O1","-O2","-O3","-Os","-Og"))
      expect_true(all(vapply(.kept, function(.k) grepl(.k, .cf, fixed = TRUE), logical(1))))
    })
  })

  test_that(".rxCompileOMakevars() declines when there is nothing to change", {
    expect_null(withr::with_options(list(rxode2.compile.O = character(0)),
                                    .rxCompileOMakevars()))
    expect_null(withr::with_options(list(rxode2.compile.O = ""),
                                    .rxCompileOMakevars()))
    expect_null(withr::with_options(list(rxode2.compile.O = NA_character_),
                                    .rxCompileOMakevars()))
  })

  test_that("a user CFLAGS still wins over the option", {
    .user <- tempfile("Makevars-user-")
    writeLines("CFLAGS = -O0 -my-flag", .user)
    on.exit(unlink(.user), add = TRUE)
    withr::with_envvar(list(R_MAKEVARS_USER = .user), {
      withr::with_options(list(rxode2.compile.O = "3"), {
        .f <- .rxCompileOMakevars()
        skip_if(is.null(.f))
        on.exit(unlink(.f), add = TRUE)
        .l <- readLines(.f, warn = FALSE)
        # ours first, the user's inlined after it, so make takes theirs
        .cf <- grep("^CFLAGS = ", .l, value = TRUE)
        expect_length(.cf, 2L)
        expect_match(.cf[1], "-O3")
        expect_identical(.cf[2], "CFLAGS = -O0 -my-flag")
      })
    })
  })
})
