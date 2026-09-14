# Solve-level parity cases, sourced by bench/lincmt_split_parity.R
# (needs .keep() and .solve() from the driver).

# --- linCmt() models, 12 subjects, cores 1 and 4 -----------------------------
.mods <- list(
  c1 = function() {
    ini({ tcl <- log(2); tv <- log(20) })
    model({ cl <- exp(tcl); v <- exp(tv); cp <- linCmt() })
  },
  c1o = function() {
    ini({ tcl <- log(2); tv <- log(20); tka <- log(1.1) })
    model({ cl <- exp(tcl); v <- exp(tv); ka <- exp(tka); cp <- linCmt() })
  },
  c2 = function() {
    ini({ tcl <- log(2); tv <- log(20); tq <- log(3); tvp <- log(40) })
    model({ cl <- exp(tcl); v <- exp(tv); q <- exp(tq); vp <- exp(tvp); cp <- linCmt() })
  },
  c2o = function() {
    ini({ tcl <- log(2); tv <- log(20); tq <- log(3); tvp <- log(40); tka <- log(1.1) })
    model({ cl <- exp(tcl); v <- exp(tv); q <- exp(tq); vp <- exp(tvp); ka <- exp(tka); cp <- linCmt() })
  },
  c3 = function() {
    ini({ tcl <- log(2); tv <- log(20); tq <- log(3); tvp <- log(40); tq2 <- log(1); tvp2 <- log(80) })
    model({
      cl <- exp(tcl); v <- exp(tv); q <- exp(tq); vp <- exp(tvp); q2 <- exp(tq2); vp2 <- exp(tvp2)
      cp <- linCmt()
    })
  },
  c3o = function() {
    ini({
      tcl <- log(2); tv <- log(20); tq <- log(3); tvp <- log(40); tq2 <- log(1); tvp2 <- log(80)
      tka <- log(1.1)
    })
    model({
      cl <- exp(tcl); v <- exp(tv); q <- exp(tq); vp <- exp(tvp); q2 <- exp(tq2); vp2 <- exp(tvp2)
      ka <- exp(tka)
      cp <- linCmt()
    })
  },
  c1oLagF = function() {
    ini({ tcl <- log(2); tv <- log(20); tka <- log(1.1); tlag <- log(0.5); tf <- log(0.8) })
    model({
      cl <- exp(tcl); v <- exp(tv); ka <- exp(tka)
      cp <- linCmt()
      alag(depot) <- exp(tlag)
      f(depot) <- exp(tf)
    })
  }
)
.params <- data.frame(id = 1:12, tcl = log(2 * (1 + 0.05 * (1:12))), tv = log(20 * (1 + 0.02 * (1:12))))
.obs <- seq(0.5, 48, by = 1.5)
.events <- function(cmt, iv) {
  .e <- list(
    bolus = et(amt = 100, ii = 12, addl = 2, cmt = cmt) |> et(.obs),
    ss = et(amt = 100, ii = 12, ss = 1, cmt = cmt) |> et(seq(0.5, 12, by = 0.5)),
    reset = et(amt = 100, cmt = cmt) |> et(amt = 50, time = 10, evid = 4, cmt = cmt) |>
      et(seq(0.5, 24, by = 1)))
  if (iv) {
    .e$infusion <- et(amt = 100, rate = 25, ii = 12, addl = 2, cmt = cmt) |> et(.obs)
    .e$duration <- et(amt = 100, dur = 4, ii = 12, addl = 2, cmt = cmt) |> et(.obs)
  }
  lapply(.e, function(x) x |> et(id = 1:12))
}
for (.mn in names(.mods)) {
  .iv <- !grepl("o", .mn, fixed = TRUE)
  .ev <- .events(if (.iv) "central" else "depot", .iv)
  for (.en in names(.ev)) {
    for (.cores in c(1L, 4L)) {
      .keep(sprintf("linCmt_%s_%s_cores%d", .mn, .en, .cores),
            .solve(.mods[[.mn]], .ev[[.en]], params = .params, cores = .cores))
    }
  }
}
.keep("linCmt_mixedOde", .solve(function() {
  ini({ tcl <- log(2); tv <- log(20); tka <- log(1.1); kin <- 1; kout <- 0.1; ec50 <- 2 })
  model({
    cl <- exp(tcl); v <- exp(tv); ka <- exp(tka)
    cp <- linCmt()
    eff(0) <- 10
    d/dt(eff) <- kin - kout * (1 - cp / (ec50 + cp)) * eff
  })
}, et(amt = 100, ii = 12, addl = 2, cmt = "depot") |> et(.obs)))

# --- explicit linCmtB() sensitivities, every linCmtSensType -------------------
.sensModel <- function(ncmt, nd) {
  .args <- sprintf("rx__PTR__, t, 1, %d, 1, %%d, %%d, 1, cl, v, q, vp, q2, vp2, ka", ncmt)
  .lines <- c(sprintf("cp=linCmtB(%s)", sprintf(.args, -1L, -1L)),
              vapply(0:nd, function(k) sprintf("d%d=linCmtB(%s)", k, sprintf(.args, -2L, k)), ""))
  suppressWarnings(rxode2(paste(.lines, collapse = "\n")))
}
.sensEv <- do.call(rbind, lapply(1:24, function(i) {
  .dose <- data.frame(id = i, time = c(0, 5, 18.5, 26),
                      amt = c(100, 60, 140, 70) * (1 + 0.05 * i), evid = 1,
                      cmt = 1, rate = c(40, 0, 70, 0))
  .o <- data.frame(id = i, time = c(0.6, 1.9, 2.4, 4.7, 7.1, 9.3, 14.6, 19.5,
                                    21.1, 24.9, 28.8, 36.6, 49.9) + 0.1 * i,
                   amt = 0, evid = 0, cmt = 1, rate = 0)
  rbind(.dose, .o)
}))
.sensPars <- c(cl = 2.1, v = 21, q = 3.3, vp = 43, q2 = 0.7, vp2 = 60, ka = 1.3)
.m2s <- .sensModel(2L, 4L)
.m3s <- .sensModel(3L, 6L)
.sensTypes <- c("AD", "ADm", "ADr", "auto", "forward", "central", "forward3",
                "endpoint5", "forwardG", "forward3G", "endpoint5G", "forward3H",
                "forwardH", "centralH")
for (.st in .sensTypes) {
  for (.cores in c(1L, 4L)) {
    .keep(sprintf("sens2_%s_cores%d", .st, .cores),
          .solve(.m2s, .sensPars, .sensEv, linCmtSensType = .st, cores = .cores))
  }
}
for (.st in c("AD", "ADm", "ADr")) {
  .keep(sprintf("sens3_%s", .st), .solve(.m3s, .sensPars, .sensEv, linCmtSensType = .st))
}

# --- dose-time / origin / carry sentinels ------------------------------------
.pOrigin <- c(tcl = log(2), tv = log(20), tka = log(1.1), eta_lag = 0, eta_f = 0)
.mOrigin <- rxode2({
  cl <- exp(tcl); v <- exp(tv); ka <- exp(tka)
  lag <- 2 * exp(eta_lag)
  alag(depot) <- lag
  cp <- linCmtB(rx__PTR__, t, 2, 1, 1, -1, -1, 1, cl, v, 0, 0, 0, 0, ka)
  d3 <- lag * linCmtB(rx__PTR__, t, 2, 1, 1, -3, -3, 1, cl, v, 0, 0, 0, 0, ka)
  d9 <- lag * linCmtB(rx__PTR__, t, 2, 1, 1, -9, 7, 1, cl, v, 0, 0, 0, 0, ka)
})
.keep("sentinel_m3_m9", .solve(.mOrigin, et(amt = 100, cmt = "depot", ii = 12, addl = 1) |>
                                 et(seq(0.1, 30, 0.5)), params = .pOrigin))
.mF <- rxode2({
  cl <- exp(tcl); v <- exp(tv); ka <- exp(tka)
  fdep <- exp(eta_f)
  f(depot) <- fdep
  cp <- linCmtB(rx__PTR__, t, 2, 1, 1, -1, -1, 1, cl, v, 0, 0, 0, 0, ka)
  dF <- linCmtB(rx__PTR__, t, 2, 1, 1, -10, 7, 1, cl, v, 0, 0, 0, 0, ka)
})
.keep("sentinel_m10", .solve(.mF, et(amt = 100, cmt = "depot", ii = 12, addl = 1) |>
                               et(amt = 40, cmt = "central", time = 1) |>
                               et(seq(0.1, 30, 0.5)), params = .pOrigin))
.base <- "rx__PTR__, t, 1, 1, 0, %d, %d, 1, 1, 10, 0, 0, 0, 0, 0"
.mCarry <- suppressWarnings(rxode2(paste0(
  "cp=linCmtB(", sprintf(.base, -1L, -1L), ")\n",
  "pn=linCmtB(", sprintf(.base, -8L, 0L), ")\n",
  "ad=linCmtB(", sprintf(.base, -5L, 0L), ")")))
.keep("sentinel_m5_m8", .solve(.mCarry, et(amt = 100) |> et(c(1, 2, 3))))

# --- counters after a fixed single-core workload -------------------------------
.linCmtBSensTypesSeen <- utils::getFromNamespace("linCmtBSensTypesSeen", "rxode2")
invisible(linCmtSeqStats(TRUE))
invisible(linCmtCarryFastStats(TRUE))
invisible(.linCmtBSensTypesSeen(TRUE))
invisible(.solve(.m2s, .sensPars, .sensEv, linCmtSensType = "AD", cores = 1L))
.keep("counters_seqStats", linCmtSeqStats(FALSE))
.keep("counters_carryFast", linCmtCarryFastStats(FALSE))
.keep("counters_sensTypes", .linCmtBSensTypesSeen(FALSE))

# --- generated model C, md5 lines removed ---------------------------------------
# md5 lines and the embedded serialized model (a RAWSXP of hex byte rows)
# change with every compile, so they are not compared
.cText <- function(m) {
  .x <- utils::capture.output(summary(rxC(m)))
  .x <- grep("[0-9a-f]{32}", .x, value = TRUE, invert = TRUE)
  .x <- grep("RAWSXP", .x, value = TRUE, invert = TRUE, fixed = TRUE)
  grep("^\\s*(0x[0-9a-f]{2},?\\s*)+$", .x, value = TRUE, invert = TRUE)
}
.keep("cText_sens2", .cText(.m2s))
.keep("cText_origin", .cText(.mOrigin))
