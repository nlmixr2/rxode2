rxode2Test(
  {
    # duplicate central/depot #246

    test_that("error with ambiguous central/depot", {
      expect_error(rxode2({
        C2 <- linCmt(V, CL, KA)
        d / dt(central) <- ka2 * depot - kel * central
      }))

      expect_error(rxode2({
        C2 <- linCmt(V, CL, KA)
        d / dt(central) <- ka2 * depot - kel * central
      }))

      expect_error(rxode2({
        C2 <- linCmt(V, CL, KA)
        d / dt(depot) <- -ka2 * depot
      }))

      expect_error(rxode2({
        C2 <- linCmt(V, CL)
        d / dt(depot) <- -ka2 * depot
      }), NA)
    })


    tran1 <- expand.grid(
      Ka = c("ka", NA),
      Vc = c("v", "vc", "v1", NA),
      Cl = c("cl", NA),
      Q = c("q", "q1", "cld", NA),
      Vp = c("v2", "vp", "vt", "vss", NA),
      Q2 = c("q2", "cld2", NA),
      Vp2 = c("vp2", "v3", "vt2", NA)
    )

    .clDf <- list()

    .fun <- function(x) {
      x <- setNames(x, names(tran1))
      .v1 <- as.character(na.omit(c(x["Vc"], x["Vp"], x["Vp2"])))
      .v2 <- as.character(na.omit(c(x["Cl"], x["Q"], x["Q2"])))
      .rx <- paste(c(
        ifelse(is.na(x["Ka"]), "", paste0(x["Ka"], "=tKa*exp(eta.ka)")),
        ifelse(is.na(x["Vc"]), "", paste0(x["Vc"], "=tVc*exp(eta.vc)")),
        ifelse(is.na(x["Cl"]), "", paste0(x["Cl"], "=tCl*exp(eta.cl)")),
        ifelse(is.na(x["Q"]), "", paste0(x["Q"], "=tQ*exp(eta.q)")),
        ifelse(is.na(x["Vp"]), "", paste0(x["Vp"], "=tVp*exp(eta.vp)")),
        ifelse(is.na(x["Q2"]), "", paste0(x["Q2"], "=tq2*exp(eta.q)")),
        ifelse(is.na(x["Vp2"]), "", paste0(x["Vp2"], "=tvp2*exp(eta.tvp2)")),
        "cp=linCmt()"
      ), collapse = "\n")
      .good <- FALSE
      if (length(.v1) == length(.v2)) {
        .good <- TRUE
        if (length(.v1) == 0) {
          test_that(sprintf("linCmt() should error without parameters"), {
            assign(".rx", .rx, globalenv())
            expect_error(rxode2(.rx))
          })
          .good <- NA
        } else {
          .varsUp <- toupper(c(.v1, .v2))
          if (!any(.varsUp == "CL")) {
            .good <- FALSE
          }
          .hasVc <- any(regexpr("^V(C|[1-9]|)$", .varsUp) != -1)
          if (!.hasVc) {
            .good <- FALSE
          }
          .hasVp <- any(regexpr("^(VP[1-9]*)$", .varsUp) != -1)
          .hasVt <- any(regexpr("^(VT[1-9]*)$", .varsUp) != -1)
          .hasVn <- any(regexpr("^V[1-9]+$", .varsUp) != -1)
          if (.hasVp && .hasVn) {
            .good <- FALSE
          }
          if (.hasVt && .hasVn) {
            .good <- FALSE
          }
          if (.hasVp && .hasVt) {
            .good <- FALSE
          }
          .hasQ <- any(regexpr("^(Q[1-9]*)$", .varsUp) != -1)
          .hasCld <- any(regexpr("^(CLD[1-9]*)$", .varsUp) != -1)
          if (.hasQ && .hasCld) {
            .good <- FALSE
          }
          ## if (any("VP2" == .varsUp) && !any("VP" == .varsUp)) {
          ##   .good <- FALSE
          ## }
          ## if (any("VT2" == .varsUp) && !any("VT" == .varsUp)) {
          ##   .good <- FALSE
          ##   print("gf4")
          ## }
          ## if (any("CLD2" == .varsUp) && !any("CLD" == .varsUp)) {
          ##   .good <- FALSE
          ##   print("gf3")
          ## }
          .hasVss <- any(.varsUp == "VSS")
          if (.hasVss && length(.v1) != 2) {
            .good <- FALSE
          }
          if (.hasVss && (.hasVp || .hasVt || sum(regexpr("^V[PT]?[1-9]+$", .varsUp) != -1) >= 1)) {
            .good <- FALSE
          }
          if ((any(.varsUp == "V1") && !any(.varsUp == "V2") && any(.varsUp == "V3")) ||
            (!any(.varsUp == "V1")) && !any(.varsUp == "V2") && any(.varsUp == "V3")) {
            if (!any(.varsUp == "V") && !any(.varsUp == "VC")) {
              .good <- FALSE
            }
          }
        }
      }
      if (is.na(.good)) {
      } else if (.good) {
        test_that(sprintf("linCmt() successful with parameters: %s", paste(na.omit(x), collapse = ", ")), {
          assign(".rx", .rx, globalenv())
          .rx <- rxode2(.rx)
          expect_true(inherits(.rx, "rxode2"))
          .tmp <- na.omit(c(x["Ka"], sort(c(.v1, .v2))))
          .tmp <- c(.tmp, rep("", 7 - length(.tmp)))
          names(.tmp) <- paste0("par", seq_along(.tmp))
          .tmp["ncmt"] <- length(.v1)
          .clDf[[length(.clDf) + 1]] <- as.data.frame(t(.tmp))
          .clDf <<- .clDf
        })
      } else {
        test_that(sprintf("linCmt() should error with parameters: %s", paste(na.omit(x), collapse = ", ")), {
          assign(".rx", .rx, globalenv())
          expect_error(rxode2(.rx))
        })
      }
    }

    # context("Cl style translations")
    apply(tran1, 1, .fun)

    .clDf <- do.call(rbind, .clDf)
    tran2 <- expand.grid(
      Ka = c("ka", NA),
      Vc = c("v", "vc", "v1", NA),
      k = c("k", "ke", "kel", NA),
      k12 = c("k12", NA),
      k21 = c("k21", NA),
      k13 = c("k13", NA),
      k31 = c("k31", NA)
    )



    .fun <- function(x) {
      x <- setNames(x, names(tran2))
      assign(".x", x, globalenv())
      .rx <- paste(c(
        ifelse(is.na(x["Ka"]), "", paste0(x["Ka"], "=tKa*exp(eta.ka)")),
        ifelse(is.na(x["Vc"]), "", paste0(x["Vc"], "=tVc*exp(eta.vc)")),
        ifelse(is.na(x["k"]), "", paste0(x["k"], "=tK*exp(eta.ka)")),
        ifelse(is.na(x["k12"]), "", paste0(x["k12"], "=tK12*exp(eta.k12)")),
        ifelse(is.na(x["k21"]), "", paste0(x["k21"], "=tK21*exp(eta.k21)")),
        ifelse(is.na(x["k13"]), "", paste0(x["k13"], "=tK13*exp(eta.k13)")),
        ifelse(is.na(x["k31"]), "", paste0(x["k31"], "=tK31*exp(eta.k31)")),
        "cp=linCmt()"
      ), collapse = "\n")
      assign(".rx", .rx, globalenv())
      .good <- TRUE
      .v1 <- as.character(na.omit(c(
        x["Ka"], x["Vc"], x["k"],
        x["k12"], x["k21"], x["k13"], x["k31"]
      )))
      .up <- toupper(.v1)
      .ncmt <- 1
      if (length(.up) == 0) {
      } else {
        if (is.na(x["k"])) {
          .good <- FALSE
        }
        if (is.na(x["Vc"])) {
          .good <- FALSE
        }
        if (any(.up == "K12")) {
          if (any(.up == "K21")) {
            .ncmt <- 2
          } else {
            .good <- FALSE
          }
        }
        if (any(.up == "K12")) {
          if (any(.up == "K21")) {
            .ncmt <- 2
          } else {
            .good <- FALSE
          }
        } else if (any(.up == "K21")) {
          .good <- FALSE
        }
        if (any(.up == "K13")) {
          if (any(.up == "K31")) {
            if (.ncmt != 2) .good <- FALSE
            .ncmt <- 3
          } else {
            .good <- FALSE
          }
        } else if (any(.up == "K31")) {
          .good <- FALSE
        }
        if (.good) {
          test_that(sprintf("linCmt() successful with parameters: %s", paste(na.omit(.v1), collapse = ", ")), {
            .rx <- rxode2(.rx)
            expect_true(inherits(.rx, "rxode2"))
            .tmp <- c(.v1, rep("", 7 - length(.v1)))
            names(.tmp) <- paste0("par", seq_along(.tmp))
            .tmp["ncmt"] <- .ncmt
            .kDf[[length(.kDf) + 1]] <- as.data.frame(t(.tmp))
            .kDf <<- .kDf
          })
        } else {
          test_that(sprintf("linCmt() should error with parameters: %s", paste(na.omit(x), collapse = ", ")), {
            assign(".rx", .rx, globalenv())
            expect_error(rxode2(.rx))
          })
        }
      }
    }

    .kDf <- list()
    # context("Kel style translations")
    apply(tran2, 1, .fun)

    .kDf <- do.call(rbind, .kDf)

    tran3 <- expand.grid(
      Ka = c("ka", NA),
      Vc = c("v", "vc", "v1", NA),
      alpha = c("alpha", NA),
      beta = c("beta", NA),
      aob = c("aob", "k21", NA)
    )


    .fun <- function(x) {
      x <- setNames(x, names(tran3))
      .rx <- paste(c(
        ifelse(is.na(x["Ka"]), "", paste0(x["Ka"], "=tKa*exp(eta.ka)")),
        ifelse(is.na(x["Vc"]), "", paste0(x["Vc"], "=tVc*exp(eta.vc)")),
        ifelse(is.na(x["alpha"]), "", paste0(x["alpha"], "=tAlpha*exp(eta.alpha)")),
        ifelse(is.na(x["beta"]), "", paste0(x["beta"], "=tBeta*exp(eta.beta)")),
        ifelse(is.na(x["aob"]), "", paste0(x["aob"], "=tAob*exp(eta.aob)")),
        "cp=linCmt()"
      ), collapse = "\n")
      .good <- TRUE
      .v1 <- as.character(na.omit(c(
        x["Ka"], x["Vc"], x["alpha"],
        x["beta"], x["aob"]
      )))
      .ncmt <- 1
      if (length(.v1) == 0) {
      } else {
        .ncmt <- 1
        if (is.na(x["Vc"])) {
          .good <- FALSE
        }
        if (is.na(x["alpha"])) {
          .good <- FALSE
        }
        .s <- sum(is.na(c(x["beta"], x["aob"])))
        if (.s == 1) {
          .good <- FALSE
        } else if (.s == 2) {
          .ncmt <- 2
        }
        if (.good) {
          test_that(sprintf("linCmt() successful with parameters: %s", paste(na.omit(.v1), collapse = ", ")), {
            .rx <- rxode2(.rx)
            expect_true(inherits(.rx, "rxode2"))
            .tmp <- c(.v1, rep("", 7 - length(.v1)))
            names(.tmp) <- paste0("par", seq_along(.tmp))
            .tmp["ncmt"] <- .ncmt
            .kAlpha[[length(.kAlpha) + 1]] <- as.data.frame(t(.tmp))
            .kAlpha <<- .kAlpha
          })
        } else {
          test_that(sprintf("linCmt() should error with parameters: %s", paste(na.omit(x), collapse = ", ")), {
            assign(".rx", .rx, globalenv())
            expect_error(rxode2(.rx))
          })
        }
      }
    }

    .kAlpha <- list()
    # context("alpha/V style translations")
    apply(tran3, 1, .fun)

    tran4 <- expand.grid(
      Ka = c("ka", NA),
      a = c("a", NA),
      alpha = c("alpha", NA),
      b = c("b", NA),
      beta = c("beta", NA),
      c = c("c", NA),
      gamma = c("gamma", NA)
    )

    .fun <- function(x) {
      x <- setNames(x, names(tran4))
      .rx <- paste(c(
        ifelse(is.na(x["Ka"]), "", paste0(x["Ka"], "=tKa*exp(eta.ka)")),
        ifelse(is.na(x["a"]), "", paste0(x["a"], "=tA*exp(eta.a)")),
        ifelse(is.na(x["alpha"]), "", paste0(x["alpha"], "=tAlpha*exp(eta.alpha)")),
        ifelse(is.na(x["b"]), "", paste0(x["b"], "=tB*exp(eta.b)")),
        ifelse(is.na(x["beta"]), "", paste0(x["beta"], "=tBeta*exp(eta.beta)")),
        ifelse(is.na(x["c"]), "", paste0(x["c"], "=tC*exp(eta.c)")),
        ifelse(is.na(x["gamma"]), "", paste0(x["gamma"], "=tGamma*exp(eta.gamma)")),
        "cp=linCmt()"
      ), collapse = "\n")
      .good <- TRUE
      .v1 <- as.character(na.omit(c(
        x["Ka"], x["a"], x["alpha"],
        x["b"], x["beta"], x["c"], x["gamma"]
      )))
      .ncmt <- 1
      if (length(.v1) == 0) {
      } else {
        .good <- TRUE
        .ncmt <- 0
        .s <- sum(!is.na(c(x["a"], x["alpha"])))
        if (.s == 2) {
          .ncmt <- 1
        } else {
          .good <- FALSE
        }
        .s <- sum(!is.na(c(x["b"], x["beta"])))
        if (.s == 2) {
          if (.ncmt != 1) .good <- FALSE
          .ncmt <- 2
        } else if (.s == 1) {
          .good <- FALSE
        }
        .s <- sum(!is.na(c(x["c"], x["gamma"])))
        if (.s == 2) {
          if (.ncmt != 2) .good <- FALSE
          .ncmt <- 3
        } else if (.s == 1) {
          .good <- FALSE
        }
        if (.good) {
          test_that(sprintf("linCmt() successful with parameters: %s", paste(na.omit(.v1), collapse = ", ")), {
            .rx <- rxode2(.rx)
            expect_true(inherits(.rx, "rxode2"))
            .tmp <- c(.v1, rep("", 7 - length(.v1)))
            names(.tmp) <- paste0("par", seq_along(.tmp))
            .tmp["ncmt"] <- .ncmt
            .kAlpha[[length(.kAlpha) + 1]] <- as.data.frame(t(.tmp))
            .kAlpha <<- .kAlpha
          })
        } else {
          test_that(sprintf("linCmt() should error with parameters: %s", paste(na.omit(x), collapse = ", ")), {
            assign(".rx", .rx, globalenv())
            expect_error(rxode2(.rx))
          })
        }
      }
    }

    # context("alpha/A style translations")
    apply(tran4, 1, .fun)
  },
  test = "parseLincmt"
)
