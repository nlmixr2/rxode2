rxTest({
  test_that("comments are parsed correctly", {
    cmt <- c("function() {", "      ini({", "        ## You may label each parameter with a comment",
             "        tka <- 0.45 # Log Ka", "        tcl <- log(c(0, 2.7, 100)) # Log Cl",
             "        ## This works with interactive models", "        ## You may also label the preceding line with label(\"label text\")",
             "        tv <- 3.45; label(\"log V\")", "        ## the label(\"Label name\") works with all models",
             "        eta.ka ~ 0.6", "        eta.cl ~ 0.3", "        eta.v ~ 0.1",
             "        add.sd <- 0.7", "      })", "      model({", "        ka <- exp(tka + eta.ka)",
             "        cl <- exp(tcl + eta.cl)", "        v <- exp(tv + eta.v)",
             "        linCmt() ~ add(add.sd)", "      })", "    }")

    eq <- c("function () ", "{", "    ini({", "        tka <- 0.45", "        label(\"Log Ka\")",
            "        tcl <- log(c(0, 2.7, 100))", "        label(\"Log Cl\")",
            "        tv <- 3.45", "        label(\"log V\")", "        eta.ka ~ 0.6",
            "        eta.cl ~ 0.3", "        eta.v ~ 0.1", "        add.sd <- 0.7",
            "    })", "    model({", "        ka <- exp(tka + eta.ka)", "        cl <- exp(tcl + eta.cl)",
            "        v <- exp(tv + eta.v)", "        linCmt() ~ add(add.sd)",
            "    })", "}")

    suppressMessages(
      expect_equal(.rxReplaceCommentWithLabel(cmt), eq)
    )

    # Leave comment labels in here as they are required for equality testing below
    one.cmt <- function() {
      ini({
        ## You may label each parameter with a comment
        tka <- 0.45 # Log Ka
        tcl <- log(c(0, 2.7, 100)) # Log Cl
        ## This works with interactive models
        ## You may also label the preceding line with label("label text")
        tv <- 3.45; label("log V")
        ## the label("Label name") works with all models
        eta.ka ~ 0.6
        eta.cl ~ 0.3
        eta.v ~ 0.1
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        linCmt() ~ add(add.sd)
      })
    }

    suppressMessages(
      str <- .rxFunction2string(one.cmt)
    )
    if (!is.null(attr(one.cmt, "srcref"))) {
      expect_equal(str, eq)
      attr(one.cmt, "srcref") <- NULL
    }

    # Leave comment labels in here as they are required for equality testing below
    one.cmt <- function() {
      ini({
        ## You may label each parameter with a comment
        tka <- 0.45 # Log Ka
        tcl <- log(c(0, 2.7, 100)) # Log Cl
        ## This works with interactive models
        ## You may also label the preceding line with label("label text")
        tv <- 3.45; label("log V")
        ## the label("Label name") works with all models
        eta.ka ~ 0.6
        eta.cl ~ 0.3
        eta.v ~ 0.1
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        linCmt() ~ add(add.sd) | tmp
      })
    }

    suppressMessages(
      mkstr <- .rxFunction2string(one.cmt)
    )
    expect_equal(mkstr,
                 c("function () ", "{", "    ini({", "        tka <- 0.45", "        label(\"Log Ka\")",
                   "        tcl <- log(c(0, 2.7, 100))", "        label(\"Log Cl\")",
                   "        tv <- 3.45", "        label(\"log V\")", "        eta.ka ~ 0.6",
                   "        eta.cl ~ 0.3", "        eta.v ~ 0.1", "        add.sd <- 0.7",
                   "    })", "    model({", "        ka <- exp(tka + eta.ka)", "        cl <- exp(tcl + eta.cl)",
                   "        v <- exp(tv + eta.v)", "        linCmt() ~ add(add.sd) | tmp",
                   "    })", "}"))

  })

  test_that("comment labels with quotes/backslashes are escaped (rxode2 issue 1195)", {

    .mkSrc <- function(comment) {
      c("function() {", "  ini({",
        paste0("    tka <- 0.45 # ", comment),
        "    add.sd <- 0.7", "  })", "  model({",
        "    ka <- exp(tka)", "    linCmt() ~ add(add.sd)", "  })", "}")
    }

    # Every comment here must survive the comment -> label() promotion verbatim.
    # Before the fix a `"` or `\` terminated the generated label() string early
    # and the re-parse in .rxReplaceCommentWithLabel() failed.  The last three
    # are the silent variants: they re-parsed fine but sub() stripping one level
    # of backslash left a label carrying the *wrong* text -- an escape sequence
    # (`\n`, `\t`) that R then interpreted, or a `\1` that R read as octal.
    .comments <- c(
      plain           = "Log Ka",
      doubleQuote     = "fixed to a \"small value\"",
      unbalancedQuote = "6\" tall",
      singleQuote     = "fixed to a 'small value'",
      backslash       = "units are mg\\L",
      escapedQuote    = "already \\\"escaped\\\"",
      both            = "a \"quote\" and a \\ backslash",
      escapeN         = "line1\\nline2",
      escapeT         = "a\\tb",
      backReference   = "see \\1 here",
      trailingSlash   = "comment \\"
    )

    for (.n in names(.comments)) {
      .comment <- .comments[[.n]]
      suppressMessages(
        .res <- .rxReplaceCommentWithLabel(.mkSrc(.comment))
      )
      # the promoted label must re-parse ...
      expect_true(is.function(
        eval(parse(text = paste(.res, collapse = "\n"), keep.source = FALSE))
      ), info = .n)
      # ... and carry the comment text through unchanged
      .lbl <- .res[grepl("^ *label\\(", .res)]
      expect_equal(length(.lbl), 1L, info = .n)
      expect_equal(parse(text = .lbl)[[1]][[2]], .comment, info = .n)
    }

    # exact generated line for the reported failure
    suppressMessages(
      .res <- .rxReplaceCommentWithLabel(.mkSrc("fixed to a \"small value\""))
    )
    expect_equal(.res[grepl("^ *label\\(", .res)],
                 "        label(\"fixed to a \\\"small value\\\"\")")

    # a `"` in a comment on an eta line is promoted the same way
    .eta <- c("function() {", "  ini({", "    tka <- 0.45",
              "    eta.ka ~ fixed(0.0225) # IIV \"fixed to a small value\"",
              "    add.sd <- 0.7", "  })", "  model({",
              "    ka <- exp(tka + eta.ka)", "    linCmt() ~ add(add.sd)", "  })", "}")
    suppressMessages(.res <- .rxReplaceCommentWithLabel(.eta))
    expect_equal(.res[grepl("^ *label\\(", .res)],
                 "        label(\"IIV \\\"fixed to a small value\\\"\")")

    # a line that already has label() is left alone, quoted comment or not
    .already <- c("function() {", "  ini({",
                  "    tka <- 0.45; label(\"Log Ka\") # ignore this \"quoted\" text",
                  "    add.sd <- 0.7", "  })", "  model({",
                  "    ka <- exp(tka)", "    linCmt() ~ add(add.sd)", "  })", "}")
    suppressMessages(.res <- .rxReplaceCommentWithLabel(.already))
    expect_equal(.res[grepl("^ *label\\(", .res)], "        label(\"Log Ka\")")
  })

  test_that("a comment containing '#' keeps its label (rxode2 issue 1205)", {

    .mkSrc <- function(comment) {
      c("function() {", "  ini({",
        paste0("    tka <- 0.45 ", comment),
        "    add.sd <- 0.7", "  })", "  model({",
        "    ka <- exp(tka)", "    linCmt() ~ add(add.sd)", "  })", "}")
    }
    .labelOf <- function(comment) {
      suppressMessages(.res <- .rxReplaceCommentWithLabel(.mkSrc(comment)))
      .lbl <- .res[grepl("^ *label\\(", .res)]
      if (length(.lbl) != 1L) return(NA_character_)
      parse(text = .lbl)[[1]][[2]]
    }

    # The code group used to run on to the LAST `#`, leaving the first one in the
    # emitted code where it commented out the label() -- so these silently lost
    # their label while still parsing as a valid model.
    expect_equal(.labelOf("## Log Ka"), "Log Ka")
    expect_equal(.labelOf("### Log Ka"), "Log Ka")
    expect_equal(.labelOf("# rate # per hour"), "rate # per hour")
    expect_equal(.labelOf("# Patient #2 only"), "Patient #2 only")

    # unchanged for the single-`#` forms
    expect_equal(.labelOf("# Log Ka"), "Log Ka")
    expect_equal(.labelOf("#Log Ka"), "Log Ka")

    # a comment carrying BOTH a `#` and a quote needs the #1205 regex and the
    # #1195 escaping together: the regex keeps the label from being commented
    # out, the paste0() assembly keeps the quote from breaking the re-parse.
    expect_equal(.labelOf("## a \"quoted\" # note"), "a \"quoted\" # note")
    expect_equal(.labelOf("## mg\\L # per dose"), "mg\\L # per dose")
  })

  test_that("a comment in an unfinished ini({}) statement is not a label (rxode2 issue 1318)", {

    .mkSrc <- function(iniLines) {
      c("function() {", "  ini({",
        iniLines,
        "    add.sd <- 0.7", "  })", "  model({",
        "    ka <- exp(tka)", "    linCmt() ~ add(add.sd)", "  })", "}")
    }
    .resOf <- function(iniLines) {
      suppressMessages(.rxReplaceCommentWithLabel(.mkSrc(iniLines)))
    }
    .labelsOf <- function(res) {
      vapply(res[grepl("^ *label\\(", res)],
             function(.l) as.character(parse(text = .l)[[1]][[2]]),
             character(1), USE.NAMES = FALSE)
    }

    # `; label()` used to be appended to any commented line whether or not the
    # statement on it had finished, so a comment between an opening `(` and its
    # `)` -- or on a line ending in an operator -- put the `;` in the middle of
    # the statement and the re-parse died with `unexpected ';'`.  Only a comment
    # trailing a COMPLETE statement may be promoted.
    .cases <- list(
      noComment      = list(src = c("    tka <- c(", "      1.0", "    )"),
                            labels = character(0)),
      insideOpenCall = list(src = c("    tka <- c(", "      1.0   # note", "    )"),
                            labels = character(0)),
      onOpeningLine  = list(src = c("    tka <- c( # note", "      1.0", "    )"),
                            labels = character(0)),
      trailingPlus   = list(src = c("    tka <- 0.45 +  # note", "      0.1"),
                            labels = character(0)),
      trailingTilde  = list(src = c("    eta.cl + eta.v ~  # note",
                                    "      c(1, 0.01, 1)"),
                            labels = character(0)),
      onClosingLine  = list(src = c("    tka <- c(", "      1.0", "    ) # note"),
                            labels = "note"),
      singleLineCall = list(src = "    tka <- c(1.0) # note", labels = "note"),
      plainLine      = list(src = "    tka <- 1.0 # note", labels = "note")
    )
    for (.n in names(.cases)) {
      .res <- .resOf(.cases[[.n]]$src)
      expect_true(is.function(eval(parse(text = paste(.res, collapse = "\n"),
                                         keep.source = FALSE))), info = .n)
      expect_equal(.labelsOf(.res), .cases[[.n]]$labels, info = .n)
    }

    # the comment inside the call is dropped and nothing else about the model
    # changes
    expect_equal(.resOf(c("    tka <- c(", "      1.0   # note", "    )")),
                 c("function () ", "{", "    ini({", "        tka <- c(1)",
                   "        add.sd <- 0.7", "    })", "    model({",
                   "        ka <- exp(tka)", "        linCmt() ~ add(add.sd)",
                   "    })", "}"))
    # a comment on the line that closes the statement still labels it
    expect_equal(.resOf(c("    tka <- c(", "      1.0", "    ) # note")),
                 c("function () ", "{", "    ini({", "        tka <- c(1)",
                   "        label(\"note\")", "        add.sd <- 0.7", "    })",
                   "    model({", "        ka <- exp(tka)",
                   "        linCmt() ~ add(add.sd)", "    })", "}"))

    # the multi-line covariance form of test-pheno.R keeps both of its labels
    expect_equal(.labelsOf(.resOf(c(
      "    tka <- log(0.008) # typical value of clearance",
      "    eta.cl + eta.v ~ c(1,",
      "                       0.01, 1) ## cov(eta.cl, eta.v), var(eta.v)"))),
      c("typical value of clearance", "cov(eta.cl, eta.v), var(eta.v)"))
  })

  test_that("a tab-indented comment-only ini({}) line is not a label (rxode2 issue 1318)", {

    # The comment-only test allowed leading spaces only, so a tab-indented
    # comment fell through to the label branch, whose code group captured just
    # the tab.  The bare `; label()` that produced parses -- a leading `;` is
    # legal -- so there was no error: the comment silently became the label of
    # the PRECEDING parameter.
    .src <- c("function() {", "  ini({",
              "    tka <- 0.45",
              "\t# tab indented comment",
              "    tcl <- 1",
              "    tv <- 3.45",
              "    add.sd <- 0.7", "  })", "  model({",
              "    ka <- exp(tka)", "    cl <- exp(tcl)", "    v <- exp(tv)",
              "    linCmt() ~ add(add.sd)", "  })", "}")
    suppressMessages(.res <- .rxReplaceCommentWithLabel(.src))
    expect_equal(.res,
                 c("function () ", "{", "    ini({", "        tka <- 0.45",
                   "        tcl <- 1", "        tv <- 3.45",
                   "        add.sd <- 0.7", "    })", "    model({",
                   "        ka <- exp(tka)", "        cl <- exp(tcl)",
                   "        v <- exp(tv)", "        linCmt() ~ add(add.sd)",
                   "    })", "}"))

    # and the orphan label does not land on the parameter above it
    .ui <- suppressMessages(eval(parse(text = paste(.res, collapse = "\n")))())
    expect_equal(.ui$iniDf$name, c("tka", "tcl", "tv", "add.sd"))
    expect_equal(.ui$iniDf$label, rep(NA_character_, 4L))

    # a tab-indented line with code before the comment still gets its label
    .tabCode <- c("function() {", "  ini({", "\ttka <- 0.45 # Log Ka",
                  "    add.sd <- 0.7", "  })", "  model({",
                  "    ka <- exp(tka)", "    linCmt() ~ add(add.sd)", "  })", "}")
    suppressMessages(.res <- .rxReplaceCommentWithLabel(.tabCode))
    expect_equal(.res[grepl("^ *label\\(", .res)], "        label(\"Log Ka\")")
  })

  test_that("meta information parsing", {

    one.cmt <- function() {
      meta1 <- "meta"
      ini({
        tka <- 0.45
        tcl <- log(c(0, 2.7, 100))
        tv <- 3.45
        eta.ka ~ 0.6
        eta.cl ~ 0.3
        eta.v ~ 0.1
        add.sd <- 0.7
      })
      meta2 <- "meta2"
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        linCmt() ~ add(add.sd)
      })
    }

    tmp1 <- one.cmt()

    expect_true(inherits(as.function(tmp1), "function"))

    expect_equal(tmp1$meta$meta1, "meta")
    expect_equal(tmp1$meta$meta2, "meta2")

    one.cmt <- function() {
      ini({
        tka <- 0.45
        tcl <- log(c(0, 2.7, 100))
        tv <- 3.45
        eta.ka ~ 0.6
        eta.cl ~ 0.3
        eta.v ~ 0.1
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        linCmt() ~ add(add.sd)
      })
    }

    one.cmt <- function() {
      ini({
        tka <- 0.45
        tcl <- log(c(0, 2.7, 100))
        tv <- 3.45
        eta.ka ~ 0.6
        eta.cl ~ 0.3
        eta.v ~ 0.1
        add.sd <- 0.7
        lambda <- c(-2, 1, 2)
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        linCmt() ~ add(add.sd) + boxCox(lambda) | tmp
      })
    }

    one.cmt <- function() {
      ini({
        tka <- 0.45
        tcl <- log(c(0, 2.7, 100))
        tv <- 3.45
        eta.ka ~ 0.6
        eta.cl ~ 0.3
        eta.v ~ 0.1
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        lambda <- 3 + v
        linCmt() ~ add(add.sd) + boxCox(lambda) | tmp
      })
    }

    one.cmt <- function() {
      ini({
        tka <- 0.45
        tcl <- log(c(0, 2.7, 100))
        tv <- 3.45
        eta.ka ~ 0.6
        eta.cl ~ 0.3
        eta.v ~ 0.1
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        linCmt() ~ lnorm(add.sd) | tmp
      })
    }

    one.cmt <- function() {
      ini({
        tka <- 0.45
        tcl <- log(c(0, 2.7, 100))
        tv <- 3.45
        eta.ka ~ 0.6
        eta.cl ~ 0.3
        eta.v ~ 0.1
        add.sd <- 0.7
        bLambda <- c(0, 3)
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        linCmt() ~ lnorm(add.sd) | tmp
        tmp2 ~ dpois(bLambda)
      })
    }

    cov <- function() {
      ini({
        tka <- 0.45
        tcl <- log(c(0, 2.7, 100))
        tv <- 3.45
        tvp <- 3.45
        cl.wt <- 0.1
        v.wt <- 0.1
        cl.sex <- 0.1
        v.sex <- 0.1
        cl.age <- 0.1
        v.age <- 0.1
        vp.wt <- 1
        vp.sex <- 1
        vp.age <- 1
        eta.ka ~ 0.6
        eta.cl ~ 0.3
        eta.v ~ 0.1
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl + log(wt / 70) * cl.wt + sex * cl.sex + age * cl.age + 3)
        v  <- exp(tv + eta.v + wt * v.wt + sex * v.sex + age * v.age + 2)
        vp <- exp(tvp + wt * vp.wt + sex * vp.sex + age * vp.age)
        d/dt(depot) = -ka * depot
        d/dt(center) = ka * depot - cl/v * center
        cp = center/v
        cp ~ add(add.sd)
      })
    }

    pk.turnover.emax <- function() {
      ini({
        tktr <- log(1)
        tka <- log(1)
        tcl <- log(0.1)
        tv <- log(10)
        ##
        eta.ktr ~ 1
        eta.ka ~ 1
        eta.cl ~ 2
        eta.v ~ 1
        prop.err <- 0.1
        pkadd.err <- 0.1
        ##
        temax <- logit(0.8)
        #temax <- 7.5
        tec50 <- log(0.5)
        tkout <- log(0.05)
        te0 <- log(100)
        ##
        eta.emax ~ .5
        eta.ec50  ~ .5
        eta.kout ~ .5
        eta.e0 ~ .5
        ##
        pdadd.err <- 10
      })
      model({
        ktr <- exp(tktr + eta.ktr)
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        ##
        #poplogit = log(temax/(1-temax))
        emax=expit(temax+eta.emax)
        #logit=temax+eta.emax
        ec50 =  exp(tec50 + eta.ec50)
        kout = exp(tkout + eta.kout)
        e0 = exp(te0 + eta.e0)
        ##
        DCP = center/v
        PD=1-emax*DCP/(ec50+DCP)
        ##
        effect(0) = e0
        kin = e0*kout
        ##
        d/dt(depot) = -ktr * depot
        d/dt(gut) =  ktr * depot -ka * gut
        d/dt(center) =  ka * gut - cl / v * center
        d/dt(effect) = kin*PD -kout*effect
        ##
        cp = center / v
        cp ~ prop(prop.err) + add(pkadd.err)
        effect ~ add(pdadd.err)
      })
    }

    turnover.emax.noeta <- function() {
      ini({
        tktr <- log(1)
        tka <- log(1)
        tcl <- log(0.1)
        tv <- log(10)
        ##
        prop.err <- 0.1
        pkadd.err <- 0.1
        ##
        temax <- logit(0.8)
        #temax <- 7.5
        tec50 <- log(0.5)
        tkout <- log(0.05)
        te0 <- log(100)
        ##
        pdadd.err <- 10
      })
      model({
        ktr <- exp(tktr)
        ka <- exp(tka)
        cl <- exp(tcl)
        v <- exp(tv)
        ##
        #poplogit = log(temax/(1-temax))
        emax=expit(temax)
        ec50 =  exp(tec50)
        kout = exp(tkout)
        e0 = exp(te0)
        ##
        DCP = center/v
        PD=1-emax*DCP/(ec50+DCP)
        ##
        effect(0) = e0
        kin = e0*kout
        ##
        d/dt(depot) = -ktr * depot
        d/dt(gut) =  ktr * depot -ka * gut
        d/dt(center) =  ka * gut - cl / v * center
        d/dt(effect) = kin*PD -kout*effect
        ##
        cp = center / v
        cp ~ prop(prop.err) + add(pkadd.err)
        effect ~ add(pdadd.err)
      })
    }

    f <- function() {
      ini({
        lKA <- log(0.294)
        CL <- 18.6
        V2 <- 40.2
        Q <- 10.5
        V3 <- 297
        Kin <- 1
        Kout <- 1
        EC50 <- 200
        eta.ka ~ 0.12
        prop.sd ~ 0.2
      })
      model({
        KA <- exp(lKA + eta.ka)
        C2 <- centr/V2
        C3 <- peri/V3
        d/dt(depot) <- -KA*depot
        d/dt(centr) <- KA*depot - CL*C2 - Q*C2 + Q*C3
        d/dt(peri) <- Q*C2 - Q*C3
        d/dt(eff) <- Kin - Kout*(1-C2/(EC50+C2))*eff
        eff(0) <- 1
        C2 ~ prop(prop.sd)
      })
    }

    expect_error(f(), "prop.sd")

    one.cmt <- function() {
      ini({
        tka <- 0.45
        tcl <- log(c(0, 2.7, 100))
        tv <- 3.45
        eta.ka + eta.cl ~ c(0.6,
                            0.001, 0.3)
        eta.v ~ 0.1
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        linCmt() ~ add(add.sd) | tmp
        vv ~ add(add.sd)
      })
    }

    expect_error(one.cmt())
  })


  test_that("model only", {

    one.cmt <- function() {
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        add.sd <- 4 + 3
        linCmt() ~ add(add.sd)
      })
    }

    expect_error(one.cmt(), NA)


    one.cmt <- function() {
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        add.sd <- 4
        linCmt() ~ add(add.sd)
      })
    }

    expect_error(one.cmt(), NA)

  })

  test_that("linCmt ui normalization expands to ode systems", {

    pure.cmt <- function() {
      ini({
        tka <- 0.45
        tcl <- log(2.7)
        tv <- 3.45
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka)
        cl <- exp(tcl)
        v <- exp(tv)
        linCmt() ~ add(add.sd) | tmp
      })
    }

    mixed.cmt <- function() {
      ini({
        tka <- 0.45
        tcl <- log(2.7)
        tv <- 3.45
        kin <- 1
        kout <- 1
        ec50 <- 2
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka)
        cl <- exp(tcl)
        v <- exp(tv)
        cp <- linCmt()
        eff(0) <- 1
        d/dt(eff) <- kin - kout * (1 - cp/(ec50 + cp)) * eff
        cp ~ add(add.sd)
      })
    }

    pure.ui <- suppressMessages(pure.cmt())
    expect_true(any(pure.ui$predDf$linCmt))
    expect_true(grepl("linCmt\\s*\\(", paste(deparse(as.function(pure.ui)), collapse = "\n")))
    pure.ode <- suppressMessages(linToOde(pure.ui))
    pure.fun <- paste(deparse(as.function(pure.ode)), collapse = "\n")

    expect_false(grepl("linCmt\\s*\\(", pure.fun))
    expect_true(grepl("d/dt\\(depot\\)", pure.fun))
    expect_true(grepl("d/dt\\(central\\)", pure.fun))
    expect_true(grepl("rxLinCmt <- central/", pure.fun, fixed = TRUE))
    expect_true(grepl("rxLinCmt ~ add\\(add\\.sd\\) \\| tmp", pure.fun))

    pure.ui2 <- suppressMessages(as.function(pure.ode)())
    expect_false(any(pure.ui2$predDf$linCmt))

    mixed.ui <- suppressMessages(mixed.cmt())
    expect_true(any(mixed.ui$predDf$linCmt == FALSE))
    mixed.ode <- suppressMessages(linToOde(mixed.ui))
    mixed.fun <- paste(deparse(as.function(mixed.ode)), collapse = "\n")

    expect_false(grepl("linCmt\\s*\\(", mixed.fun))
    expect_true(grepl("d/dt\\(depot\\)", mixed.fun))
    expect_true(grepl("d/dt\\(central\\)", mixed.fun))
    expect_true(grepl("cp <- central/", mixed.fun, fixed = TRUE))
    expect_true(grepl("d/dt\\(eff\\) <- kin - kout \\* \\(1 - cp/\\(ec50 \\+ cp\\)\\) \\* eff", mixed.fun))

    mixed.ui2 <- suppressMessages(as.function(mixed.ode)())
    expect_false(any(mixed.ui2$predDf$linCmt))
  })

  test_that("linToOde covers all supported linCmt translations", {
    .makeLinToOdeUi <- function(params, withDepot=FALSE) {
      .params <- params
      if (withDepot) {
        .params <- c(.params, "ka")
      }
      .lines <- c(vapply(seq_along(.params), function(i) {
        sprintf("%s <- %s", .params[i], i)
      }, character(1), USE.NAMES = FALSE),
      "add.sd <- 0.7")
      .txt <- paste(c(
        "function() {",
        "  model({",
        paste0("    ", .lines),
        "    cp <- linCmt()",
        "    cp ~ add(add.sd)",
        "  })",
        "}"
      ), collapse = "\n")
      suppressMessages(eval(parse(text=.txt))())
    }

    .linMeta <- function(ui) {
      .expr <- as.list(str2lang(paste0("{", rxNorm(ui$mvL), "}")))[-1]
      .w <- which(vapply(.expr, function(x) {
        is.call(x) &&
          (identical(x[[1]], quote(`=`)) || identical(x[[1]], quote(`<-`))) &&
          is.call(x[[3]]) &&
          as.character(x[[3]][[1]]) %in% c("linCmtA", "linCmtB")
      }, logical(1), USE.NAMES = FALSE))
      expect_length(.w, 1)
      .rhs <- .expr[[.w]][[3]]
      list(
        ncmt = as.integer(eval(.rhs[[5]], envir=baseenv())),
        oral0 = as.integer(eval(.rhs[[6]], envir=baseenv())),
        trans = as.integer(eval(.rhs[[8]], envir=baseenv()))
      )
    }

    .cases <- list(
      list(name="1c trans1", ncmt=1L, trans=1L, params=c("cl", "v")),
      list(name="1c trans2", ncmt=1L, trans=2L, params=c("k", "v")),
      list(name="1c trans10", ncmt=1L, trans=10L, params=c("alpha", "a")),
      list(name="1c trans11", ncmt=1L, trans=11L, params=c("alpha", "v")),
      list(name="2c trans1", ncmt=2L, trans=1L, params=c("cl", "v", "q", "vp")),
      list(name="2c trans2", ncmt=2L, trans=2L, params=c("k", "v", "k12", "k21")),
      list(name="2c trans3", ncmt=2L, trans=3L, params=c("cl", "v", "q", "vss")),
      list(name="2c trans4", ncmt=2L, trans=4L, params=c("alpha", "v", "beta", "k21")),
      list(name="2c trans5", ncmt=2L, trans=5L, params=c("alpha", "v", "beta", "aob")),
      list(name="2c trans10", ncmt=2L, trans=10L, params=c("alpha", "a", "beta", "b")),
      list(name="2c trans11", ncmt=2L, trans=11L, params=c("alpha", "v", "beta", "b")),
      list(name="3c trans1", ncmt=3L, trans=1L, params=c("cl", "v", "q", "vp", "q2", "vp2")),
      list(name="3c trans2", ncmt=3L, trans=2L, params=c("k", "v", "k12", "k21", "k13", "k31")),
      list(name="3c trans10", ncmt=3L, trans=10L, params=c("alpha", "a", "beta", "b", "gamma", "c")),
      list(name="3c trans11", ncmt=3L, trans=11L, params=c("alpha", "v", "beta", "b", "gamma", "c"))
    )

    for (.case in .cases) {
      for (.depot in c(FALSE, TRUE)) {
        .lbl <- sprintf("%s %s", .case$name, ifelse(.depot, "with depot", "without depot"))
        .ui <- .makeLinToOdeUi(.case$params, withDepot=.depot)
        .meta <- .linMeta(.ui)
        expect_identical(.meta$ncmt, .case$ncmt, info=.lbl)
        expect_identical(.meta$trans, .case$trans, info=.lbl)
        expect_identical(.meta$oral0, as.integer(.depot), info=.lbl)

        .ode <- suppressMessages(linToOde(.ui))
        .fun <- paste(deparse(as.function(.ode)), collapse = "\n")
        .odeUi <- suppressMessages(as.function(.ode)())

        expect_false(grepl("linCmt\\s*\\(", .fun), info=.lbl)
        expect_true(grepl("cp <- central/", .fun, fixed = TRUE), info=.lbl)
        expect_true(grepl("d/dt\\(central\\)", .fun), info=.lbl)
        expect_identical(grepl("d/dt\\(depot\\)", .fun), .depot, info=.lbl)
        expect_identical(grepl("d/dt\\(peripheral1\\)", .fun), .case$ncmt >= 2L, info=.lbl)
        expect_identical(grepl("d/dt\\(peripheral2\\)", .fun), .case$ncmt >= 3L, info=.lbl)
        expect_false(any(.odeUi$predDf$linCmt), info=.lbl)
      }
    }
  })

  test_that("linToOde handles assigned oral linCmt ui forms", {
    .makeAssignedLinUi <- function(linCmtLine) {
      .txt <- paste(c(
        "function() {",
        "  ini({",
        "    tka <- 0.45",
        "    tcl <- log(2.7)",
        "    tv <- 3.45",
        "    add.sd <- 0.7",
        "  })",
        "  model({",
        "    ka <- exp(tka)",
        "    cl <- exp(tcl)",
        "    v <- exp(tv)",
        paste0("    ", linCmtLine),
        "    cp ~ add(add.sd)",
        "  })",
        "}"
      ), collapse = "\n")
      suppressMessages(eval(parse(text=.txt))())
    }

    .cases <- c("cp <- linCmt()", "cp <- linCmt(ka, cl, v)")

    for (case in .cases) {
      .ui <- .makeAssignedLinUi(case)
      .uiFun <- paste(deparse(as.function(.ui)), collapse = "\n")
      .ode <- suppressMessages(linToOde(.ui))
      .odeFun <- paste(deparse(as.function(.ode)), collapse = "\n")
      .odeUi <- suppressMessages(as.function(.ode)())

      expect_true(grepl(case, .uiFun, fixed = TRUE), info=case)
      expect_false(grepl("linCmt\\s*\\(", .odeFun), info=case)
      expect_true(grepl("d/dt\\(depot\\)", .odeFun), info=case)
      expect_true(grepl("d/dt\\(central\\)", .odeFun), info=case)
      expect_true(grepl("cp <- central/", .odeFun, fixed = TRUE), info=case)
      expect_false(any(.odeUi$predDf$linCmt), info=case)
    }
  })

  test_that("linToOde handles assigned oral linCmt ui with ini parameters", {
    .ui <- suppressMessages(eval(parse(text = paste(c(
      "function() {",
      "  ini({",
      "    ka <- 1",
      "    cl <- 2",
      "    v <- 3",
      "    add.sd <- 0.7",
      "  })",
      "  model({",
      "    cp <- linCmt(ka, cl, v)",
      "    cp ~ add(add.sd)",
      "  })",
      "}"
    ), collapse = "\n")))())

    .uiFun <- paste(deparse(as.function(.ui)), collapse = "\n")
    .ode <- suppressMessages(linToOde(.ui))
    .odeFun <- paste(deparse(as.function(.ode)), collapse = "\n")
    .odeUi <- suppressMessages(as.function(.ode)())

    expect_true(grepl("cp <- linCmt\\(ka, cl, v\\)", .uiFun), info="ini oral params")
    expect_false(grepl("linCmt\\s*\\(", .odeFun), info="ini oral params")
    expect_true(grepl("d/dt\\(depot\\)", .odeFun), info="ini oral params")
    expect_true(grepl("d/dt\\(central\\)", .odeFun), info="ini oral params")
    expect_true(grepl("cp <- central/", .odeFun, fixed = TRUE), info="ini oral params")
    expect_false(any(.odeUi$predDf$linCmt), info="ini oral params")
  })

  test_that("iov covariates handled correctly", {

    one.cmt <- function() {
      ini({
        ## You may label each parameter with a comment
        tka <- 0.45 # Log Ka
        tcl <- log(c(0, 2.7, 100)) # Log Cl
        ## This works with interactive models
        ## You may also label the preceding line with label("label text")
        tv <- 3.45; label("log V")
        ## the label("Label name") works with all models
        eta.ka ~ 0.6
        eta.cl ~ 0.3
        eta.v ~ 0.1
        iov.cl ~ 0.1 | occ
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl + iov.cl)
        v <- exp(tv + eta.v)
        linCmt() ~ add(add.sd)
      })
    }

    o <- suppressWarnings(one.cmt())

    expect_equal(o$covariates, character(0))

  })

  test_that("ui desc doesn't include compartments", {
    one.cmt <- function() {
      ini({
        ## You may label each parameter with a comment
        tka <- 0.45 # Log Ka
        tcl <- log(c(0, 2.7, 100)) # Log Cl
        ## This works with interactive models
        ## You may also label the preceding line with label("label text")
        tv <- 3.45; label("log V")
        ## the label("Label name") works with all models
        eta.ka ~ 0.6
        eta.cl ~ 0.3
        eta.v ~ 0.1
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        linCmt() ~ add(add.sd)
      })
    }
    m <- one.cmt()
    expect_equal(m$modelDesc, "rxode2-based solved PK 1-compartment model")
  })
})
