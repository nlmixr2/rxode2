rxTest({

  test_that(".rxIsReservedName tracks the parser's reserved names", {
    expect_true(all(.rxIsReservedName(c("t", "time", "tlast", "newind", "NEWIND",
                                        "rxFlag", "amt", "mixnum", "mixest",
                                        "mixunif", "M_PI", "M_E", "M_LN10",
                                        "pi", "NA", "NaN", "Inf"))))
    # the reserved variables the parser matches case-insensitively
    expect_true(all(.rxIsReservedName(c("Time", "TIME", "AMT"))))
    # names the parser reserves through a pattern rather than a literal
    expect_true(all(.rxIsReservedName(c("rx_mixsel_1_2_", "rx_mixsel_2_2_"))))
    # ordinary model variables are not reserved
    expect_false(any(.rxIsReservedName(c("tka", "ka", "cl", "v", "eta.ka",
                                         "add.sd", "wt", "T"))))
    # names new_or_ith() drops or rewrites before the reserved check
    expect_true(all(.rxIsReservedName(c("lhs", "rxlin___", "cmt", "Cmt"))))
    # ... but the exact spelling CMT is an ordinary variable
    expect_false(.rxIsReservedName("CMT"))
    expect_equal(.rxIsReservedName(character(0)), logical(0))
    expect_equal(.rxIsReservedName(NA_character_), NA)
  })

  # every name .addVariableToIniDf must refuse, in one place.  The piping
  # branches reach it through different gates (the theta/eta regexes only let
  # some names through at all), so the guard itself is exercised directly here.
  .reserved <- c("t", "time", "Time", "tlast", "newind", "NEWIND", "rxFlag",
                 "amt", "AMT", "mixnum", "mixest", "mixunif", "rx_mixsel_1_2_",
                 "M_PI", "M_E", "M_LN10", "M_SQRT_2dPI", "pi", "NA", "NaN",
                 "Inf", "lhs", "rxlin___", "cmt", "E")

  # a model with no reserved names in it, used as the piping base below
  .base <- function() {
    ini({
      tka <- log(1.5)
      tcl <- log(1)
      add.sd <- 0.7
    })
    model({
      ka <- exp(tka)
      cl <- exp(tcl)
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - cl * center
      cp <- center
      cp ~ add(add.sd)
    })
  }

  test_that(".addVariableToIniDf refuses every reserved name", {
    for (v in .reserved) {
      ui <- rxUiDecompress(.base())
      .before <- ui$iniDf
      # promote=NA is the error-parameter path, which adds unconditionally;
      # promote=TRUE is the population-parameter path
      .addVariableToIniDf(v, ui, promote = NA)
      .addVariableToIniDf(v, ui, promote = TRUE)
      expect_equal(ui$iniDf, .before, info = v)
    }
    # a name that is not reserved does get added, so the loop above is not
    # passing because .addVariableToIniDf never adds anything
    .withCov <- function() {
      ini({
        tka <- log(1.5)
        tcl <- log(1)
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka)
        cl <- exp(tcl + tf)
        d/dt(depot) <- -ka * depot
        d/dt(center) <- ka * depot - cl * center
        cp <- center
        cp ~ add(add.sd)
      })
    }
    ui <- rxUiDecompress(.withCov())
    expect_true("tf" %in% ui$allCovs)
    .addVariableToIniDf("tf", ui, promote = TRUE)
    expect_true("tf" %in% ui$iniDf$name)
  })

  test_that("reserved variables are not promoted when appending", {
    for (v in c("t", "time", "tlast", "newind", "rxFlag", "M_PI", "pi",
                "NA", "NaN", "Inf")) {
      ui <- .base()
      expect_error(
        ui <- do.call(model, list(ui, str2lang(paste0("cp2 <- cp * ", v)),
                                  append = quote(cp))),
        NA, info = v)
      expect_equal(ui$iniDf$name, c("tka", "tcl", "add.sd"), info = v)
      expect_false(v %in% ui$allCovs, info = v)
    }
  })

  test_that("reserved variables are not promoted when prepending", {
    ui <- .base()
    ui <- do.call(model, list(ui, str2lang("f <- t * 2"), append = FALSE))
    expect_equal(ui$iniDf$name, c("tka", "tcl", "add.sd"))
    expect_false("t" %in% ui$allCovs)
  })

  test_that("a piped model using t still solves", {
    ui <- .base()
    ui <- do.call(model, list(ui, quote(cp2 <- cp * exp(-t / 10)),
                              append = quote(cp)))
    s <- rxSolve(ui, et(amt = 100, ii = 24, addl = 2) |> et(seq(0, 72, by = 12)),
                 params = c(tka = log(1.5), tcl = log(1), add.sd = 0),
                 returnType = "data.frame")
    expect_true(all(is.finite(s$cp2)))
    expect_equal(s$cp2, s$cp * exp(-s$time / 10))
  })

  test_that("reserved variables are not added as covariates with auto=FALSE", {
    for (v in c("t", "time", "pi", "M_PI", "NA", "Inf")) {
      ui <- .base()
      ui <- do.call(model, list(ui, str2lang(paste0("cp2 <- cp * ", v)),
                                append = quote(cp), auto = FALSE))
      expect_equal(ui$iniDf$name, c("tka", "tcl", "add.sd"), info = v)
      expect_false(v %in% ui$allCovs, info = v)
      expect_false(v %in% ui$mv$params, info = v)
    }
  })

  test_that("a reserved variable in an error line is rejected, not promoted", {
    # `t` cannot be an additive error standard deviation; the endpoint check
    # has to see that rather than an auto-promoted `t` in the ini block
    expect_error(do.call(model, list(.base(), quote(cp ~ add(t)))),
                 "estimated or modeled")
  })

  test_that("rxRename refuses to rename a parameter to a reserved variable", {
    expect_error(rxRename(.base(), t = tcl), "reserved rxode2 variable")
    # `lhs` and any spelling of CMT but the exact one are dropped or rewritten
    # by the parser, so the renamed parameter never reaches the model block
    expect_error(rxRename(.base(), lhs = tcl), "reserved rxode2 variable")
    expect_error(rxRename(.base(), cmt = tcl), "reserved rxode2 variable")
    # silent ones: `pi` parses as the constant and `E` reads back as Euler's
    # number in the estimation models, so in both cases the renamed parameter
    # would sit in the ini block doing nothing
    expect_error(rxRename(.base(), pi = tcl), "reserved rxode2 variable")
    expect_error(rxRename(.base(), E = tcl), "reserved rxode2 variable")
    # renaming to an ordinary name still works
    expect_true("tcl2" %in% rxRename(.base(), tcl2 = tcl)$iniDf$name)
  })

  test_that("E is kept out of the ini block but is still an ordinary covariate", {
    # E is not reserved by the parser, so a model may legitimately read it from
    # the data; it is only kept from becoming an ESTIMATED parameter, since
    # symengine reads it back as Euler's number in the estimation models
    ui <- .base()
    ui <- do.call(model, list(ui, quote(cp2 <- cp * E), append = quote(cp)))
    expect_false("E" %in% ui$iniDf$name)
    expect_true("E" %in% ui$allCovs)
    # and it is not silently accepted as a residual parameter either
    expect_error(do.call(model, list(.base(), quote(cp ~ add(E)))),
                 "estimated or modeled")
  })

  test_that("ini() piping rejects a reserved variable rather than adding it", {
    for (nm in c("t", "time", "pi", "lhs", "E")) {
      .call <- as.call(list(quote(ini), quote(.ui), call("<-", as.name(nm), 1)))
      .ui <- .base()
      expect_error(eval(.call), "cannot find parameter", info = nm)
    }
  })

  test_that("non-reserved variables are still promoted when appending", {
    ui <- .base()
    ui <- do.call(model, list(ui, quote(cp2 <- cp * exp(tf)), append = quote(cp)))
    expect_true("tf" %in% ui$iniDf$name)
    # a covariate-looking name still becomes a covariate rather than a parameter
    ui <- .base()
    ui <- do.call(model, list(ui, quote(cp2 <- cp * wt), append = quote(cp)))
    expect_false("wt" %in% ui$iniDf$name)
    expect_true("wt" %in% ui$allCovs)
  })

})
