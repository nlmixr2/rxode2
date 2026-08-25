# Forward vs reverse vs hybrid linCmt() sensitivities on an OPTIMIZED build:
# every kernel (1/2/3 cmt x iv/oral), every parameterization (trans), every
# requested-direction count plus the realistic eta masks, seven event
# shapes, single thread pinned to an idle core (threaded cells optional).
#
# devtools::load_all() compiles at -O0 (pkgbuild debug flags override the
# Makevars -O3), and every earlier forward-vs-reverse timing in this project
# was taken through it.  Build with
#   rm -f src/*.o src/*.so; Rscript -e 'pkgbuild::compile_dll(".", debug = FALSE)'
# (linCmt.cpp compiles with -O2 ... -O3 -fno-math-errno -mtune=native; the
# build log is checked for a stray -O0), load with compile = FALSE, and pin:
#   THREADS=0 MAXSEC=570 taskset -c <idle core> Rscript bench/lincmt_auto_optimized.R
# The run checkpoints one file per (config, trans) under bench/results/auto_opt/
# and skips finished ones, so it can be resumed; MAXSEC stops it cleanly after
# the chunk that crosses the budget.  THREADS=0 skips the all-thread cells
# (required while another job loads the machine; contended threaded cells
# must not decide a rule).  Provenance (affinity, load, busiest processes,
# build flags) is saved with every file.
#
# Every parameterization is derived from ONE base PK per kernel, so all trans
# variants solve the same concentrations; the per-cell `transDiff` column is
# the max relative difference of the forward solve against trans 1, and
# `hybDiff` / `revDiff` the same against the forward solve of the same cell.
suppressMessages(devtools::load_all(".", quiet = TRUE, compile = FALSE))

.base <- list(
  "1" = c(cl = 2.1, v = 21),
  "2" = c(cl = 2.1, v = 21, q = 3.3, vp = 43),
  "3" = c(cl = 2.1, v = 21, q = 3.3, vp = 43, q2 = 0.9, vp2 = 110))
.ka <- 1.3

# macro -> micro, then the alpha/beta(/gamma) + A/B(/C) residues of the
# IV-bolus unit response (standard sum-of-exponentials form)
.micro <- function(ncmt) {
  b <- .base[[as.character(ncmt)]]
  k <- b[["cl"]] / b[["v"]]
  if (ncmt == 1L) return(list(k = k, v = b[["v"]]))
  k12 <- b[["q"]] / b[["v"]]; k21 <- b[["q"]] / b[["vp"]]
  if (ncmt == 2L) return(list(k = k, v = b[["v"]], k12 = k12, k21 = k21))
  k13 <- b[["q2"]] / b[["v"]]; k31 <- b[["q2"]] / b[["vp2"]]
  list(k = k, v = b[["v"]], k12 = k12, k21 = k21, k13 = k13, k31 = k31)
}
.expo <- function(ncmt) {
  m <- .micro(ncmt)
  if (ncmt == 2L) {
    s <- m$k + m$k12 + m$k21; p <- m$k * m$k21
    alpha <- (s + sqrt(s^2 - 4 * p)) / 2; beta <- (s - sqrt(s^2 - 4 * p)) / 2
    A <- (m$k21 - alpha) / (beta - alpha) / m$v
    B <- (m$k21 - beta) / (alpha - beta) / m$v
    return(list(alpha = alpha, beta = beta, A = A, B = B))
  }
  K <- matrix(c(-(m$k + m$k12 + m$k13), m$k21, m$k31,
                m$k12, -m$k21, 0,
                m$k13, 0, -m$k31), 3, 3, byrow = TRUE)
  l <- sort(-Re(eigen(K)$values), decreasing = TRUE)  # alpha > beta > gamma
  res <- function(x, o1, o2) (m$k21 - x) * (m$k31 - x) / ((o1 - x) * (o2 - x)) / m$v
  list(alpha = l[1], beta = l[2], gamma = l[3],
       A = res(l[1], l[2], l[3]), B = res(l[2], l[1], l[3]), C = res(l[3], l[1], l[2]))
}

# (p1, v1, p2, p3, p4, p5) for a trans code, all describing the same PK
.transPars <- function(ncmt, trans) {
  b <- .base[[as.character(ncmt)]]; m <- .micro(ncmt)
  z <- c(p1 = 0, v1 = 0, p2 = 0, p3 = 0, p4 = 0, p5 = 0)
  if (ncmt == 1L) {
    if (trans == 1L) { z["p1"] <- b[["cl"]]; z["v1"] <- b[["v"]] }
    else if (trans %in% c(2L, 11L)) { z["p1"] <- m$k; z["v1"] <- m$v }
    else if (trans == 10L) { z["p1"] <- m$k; z["v1"] <- 1 / m$v }
    return(z)
  }
  e <- .expo(ncmt)
  if (ncmt == 2L) {
    switch(as.character(trans),
      "1" = { z["p1"] <- b[["cl"]]; z["v1"] <- b[["v"]]; z["p2"] <- b[["q"]]; z["p3"] <- b[["vp"]] },
      "2" = { z["p1"] <- m$k; z["v1"] <- m$v; z["p2"] <- m$k12; z["p3"] <- m$k21 },
      "3" = { z["p1"] <- b[["cl"]]; z["v1"] <- b[["v"]]; z["p2"] <- b[["q"]]; z["p3"] <- b[["v"]] + b[["vp"]] },
      "4" = { z["p1"] <- e$alpha; z["v1"] <- m$v; z["p2"] <- e$beta; z["p3"] <- m$k21 },
      "5" = { z["p1"] <- e$alpha; z["v1"] <- m$v; z["p2"] <- e$beta
              z["p3"] <- (e$alpha - m$k21) / (m$k21 - e$beta) },
      "11" = { z["p1"] <- e$alpha; z["v1"] <- 1 / e$A; z["p2"] <- e$beta; z["p3"] <- e$B },
      "10" = { z["p1"] <- e$alpha; z["v1"] <- e$A; z["p2"] <- e$beta; z["p3"] <- e$B })
    return(z)
  }
  switch(as.character(trans),
    "1" = { z["p1"] <- b[["cl"]]; z["v1"] <- b[["v"]]; z["p2"] <- b[["q"]]; z["p3"] <- b[["vp"]]
            z["p4"] <- b[["q2"]]; z["p5"] <- b[["vp2"]] },
    "2" = { z["p1"] <- m$k; z["v1"] <- m$v; z["p2"] <- m$k12; z["p3"] <- m$k21
            z["p4"] <- m$k13; z["p5"] <- m$k31 },
    "11" = { z["p1"] <- e$alpha; z["v1"] <- 1 / e$A; z["p2"] <- e$beta; z["p3"] <- e$B
             z["p4"] <- e$gamma; z["p5"] <- e$C },
    "10" = { z["p1"] <- e$alpha; z["v1"] <- e$A; z["p2"] <- e$beta; z["p3"] <- e$B
             z["p4"] <- e$gamma; z["p5"] <- e$C })
  z
}

.transFor <- function(ncmt) {
  if (ncmt == 2L) c(1L, 2L, 3L, 4L, 5L, 11L, 10L) else c(1L, 2L, 11L, 10L)
}

.cfgs <- do.call(rbind, lapply(1:3, function(ncmt) {
  do.call(rbind, lapply(0:1, function(oral0) {
    data.frame(name = sprintf("%dcmt-%s", ncmt, if (oral0) "oral" else "iv"),
               ncmt = ncmt, oral0 = oral0, trans = .transFor(ncmt))
  }))
}))

# requested-direction sets: k = 0..npars in theta order (p1, v1, p2, ..., ka)
# plus the realistic named masks
.dirSets <- function(ncmt, oral0) {
  npars <- 2L * ncmt + oral0
  sets <- lapply(0:npars, function(k) list(name = sprintf("k%d", k), dirs = seq_len(k) - 1L))
  if (ncmt >= 2L) sets <- c(sets, list(list(name = "clv", dirs = 0:1)))
  if (oral0 && ncmt >= 2L) sets <- c(sets, list(list(name = "clvka", dirs = c(0L, 1L, 2L * ncmt))))
  sets
}

.gradModel <- function(cfg, dirs) {
  args <- sprintf("rx__PTR__, t, 1, %d, %d, %%d, %%d, %d, p1, v1, p2, p3, p4, p5, ka",
                  cfg$ncmt, cfg$oral0, cfg$trans)
  lines <- c(sprintf("cp=linCmtB(%s)", sprintf(args, -1L, -1L)),
             vapply(dirs, function(d) sprintf("d%d=linCmtB(%s)", d, sprintf(args, -2L, d)), ""))
  suppressWarnings(rxode2(paste(lines, collapse = "\n")))
}

# event shapes, 44 subjects, non-uniform dose and observation spacing
.shape <- function(kind, nSub = 44L) {
  set.seed(20260823L)
  one <- function(i, doseT, obsT, rate = 0, ss = 0, ii = 0, amt = 100) {
    nd <- length(doseT)
    d <- data.frame(id = i, time = doseT, amt = amt * (1 + 0.01 * i), evid = 1, cmt = 1,
                    rate = rate, ss = c(ss, rep(0, nd - 1L)), ii = c(ii, rep(0, nd - 1L)))
    o <- data.frame(id = i, time = obsT, amt = 0, evid = 0, cmt = 1, rate = 0, ss = 0, ii = 0)
    rbind(d, o)
  }
  doses <- function(n, dt) cumsum(c(0, dt * runif(n - 1L, 0.6, 1.4)))
  obs <- function(n, from, span) sort(from + span * (runif(n)^1.5))
  do.call(rbind, lapply(seq_len(nSub), function(i) {
    switch(kind,
      small = one(i, 0, obs(20L, 0.1, 24)),
      fewDense = { dT <- doses(2L, 12); one(i, dT, obs(200L, max(dT) + 0.1, 36)) },
      heavySparse = { dT <- doses(100L, 0.5); one(i, dT, obs(50L, max(dT) + 0.1, 24)) },
      heavyDense = { dT <- doses(100L, 0.5); one(i, dT, obs(200L, max(dT) + 0.1, 24)) },
      heavyDense2 = { dT <- doses(200L, 0.5); one(i, dT, obs(400L, max(dT) + 0.1, 24)) },
      infusion = { dT <- doses(10L, 6); one(i, dT, obs(100L, 0.1, max(dT) + 12), rate = 50) },
      ss = { dT <- doses(6L, 12); one(i, dT, obs(100L, 0.1, max(dT) + 24), ss = 1, ii = 12) })
  }))
}
.shapes <- c("small", "fewDense", "heavySparse", "heavyDense", "heavyDense2", "infusion", "ss")

.tm <- function(fn, reps) median(vapply(seq_len(reps), function(i) system.time(fn())[["elapsed"]], 0))

.relDiff <- function(a, b, cols) {
  max(vapply(cols, function(cc) max(abs(a[[cc]] - b[[cc]]) / pmax(1e-8, abs(b[[cc]]))), 0))
}

.provenance <- function() {
  list(affinity = tryCatch(system("taskset -cp $PPID 2>/dev/null | sed 's/.*: //'", intern = TRUE), error = function(e) NA),
       loadavg = tryCatch(readLines("/proc/loadavg", n = 1L), error = function(e) NA),
       topProcesses = tryCatch(system("ps -eo pid,pcpu,etime,comm --sort=-pcpu --no-headers | head -3", intern = TRUE), error = function(e) NA),
       time = format(Sys.time()), threads = getRxThreads(),
       build = "pkgbuild::compile_dll(debug=FALSE): -O2 ... -O3 -fno-math-errno -mtune=native (no -O0 in the build log)",
       commit = tryCatch(system("git rev-parse --short HEAD", intern = TRUE), error = function(e) NA))
}

benchLinCmtAutoOptimized <- function(nSub = 44L, reps = 3L, threaded = TRUE,
                                     nThr = getRxThreads(), maxSec = Inf,
                                     configs = NULL, shapes = .shapes) {
  t0 <- proc.time()[["elapsed"]]
  dir.create("bench/results/auto_opt", showWarnings = FALSE, recursive = TRUE)
  evs <- lapply(shapes, .shape, nSub = nSub); names(evs) <- shapes
  rows <- .cfgs
  if (!is.null(configs)) rows <- rows[rows$name %in% configs, ]
  for (ci in seq_len(nrow(rows))) {
    cfg <- rows[ci, ]
    fn <- sprintf("bench/results/auto_opt/%s_trans%d.rds", cfg$name, cfg$trans)
    if (file.exists(fn)) next
    if (proc.time()[["elapsed"]] - t0 > maxSec) { cat("MAXSEC reached before", fn, "\n"); return(invisible(NULL)) }
    pars <- c(.transPars(cfg$ncmt, cfg$trans), ka = .ka)
    ref <- if (cfg$trans != 1L) c(.transPars(cfg$ncmt, 1L), ka = .ka) else NULL
    modRef <- if (!is.null(ref)) .gradModel(transform(cfg, trans = 1L), integer(0)) else NULL
    out <- list()
    for (ds in .dirSets(cfg$ncmt, cfg$oral0)) {
      mod <- .gradModel(cfg, ds$dirs)
      cols <- c("cp", sprintf("d%d", ds$dirs))
      for (sh in shapes) {
        evt <- evs[[sh]]
        solve1 <- function(st, strat, cores) {
          rxSolve(mod, pars, evt, linCmtSensType = st, linCmtSensStrategy = strat,
                  cores = cores, returnType = "data.frame")
        }
        fwd <- solve1("AD", "sequential", 1L)
        rev <- solve1("ADr", "sequential", 1L)
        invisible(linCmtHybStats(TRUE))
        hyb <- solve1("AD", "hybrid", 1L)
        hs <- linCmtHybStats(TRUE)
        transDiff <- if (is.null(modRef)) 0 else
          .relDiff(fwd, rxSolve(modRef, ref, evt, linCmtSensType = "AD", cores = 1L, returnType = "data.frame"), "cp")
        row <- data.frame(config = cfg$name, ncmt = cfg$ncmt, oral0 = cfg$oral0, trans = cfg$trans,
                          m = cfg$ncmt + cfg$oral0, npars = 2L * cfg$ncmt + cfg$oral0,
                          dirSet = ds$name, k = length(ds$dirs), shape = sh,
                          nRows = nrow(evt),
                          fwd1 = .tm(function() solve1("AD", "sequential", 1L), reps),
                          rev1 = .tm(function() solve1("ADr", "sequential", 1L), reps),
                          hyb1 = .tm(function() solve1("AD", "hybrid", 1L), reps),
                          fwdN = NA_real_, revN = NA_real_, hybN = NA_real_,
                          revDiff = .relDiff(rev, fwd, cols), hybDiff = .relDiff(hyb, fwd, cols),
                          transDiff = transDiff,
                          hybSubjects = unname(hs["subjects"]),
                          hybRows = unname(hs["rows"]))
        if (threaded) {
          row$fwdN <- .tm(function() solve1("AD", "sequential", nThr), reps)
          row$revN <- .tm(function() solve1("ADr", "sequential", nThr), reps)
          row$hybN <- .tm(function() solve1("AD", "hybrid", nThr), reps)
        }
        cat(sprintf("%-9s t%-2d %-6s k=%d %-11s fwd %.3f rev %.3f hyb %.3f  rev/fwd %.2f hyb/fwd %.2f  diffs %.1e/%.1e/%.1e win %s\n",
                    cfg$name, cfg$trans, ds$name, row$k, sh, row$fwd1, row$rev1, row$hyb1,
                    row$rev1 / row$fwd1, row$hyb1 / row$fwd1, row$revDiff, row$hybDiff, row$transDiff,
                    as.character(row$hybSubjects)))
        out[[length(out) + 1L]] <- row
      }
    }
    res <- do.call(rbind, out)
    attr(res, "provenance") <- .provenance()
    tmp <- paste0(fn, ".tmp"); saveRDS(res, tmp); file.rename(tmp, fn)
    cat("saved", fn, sprintf("(%.0fs elapsed)\n", proc.time()[["elapsed"]] - t0))
  }
  invisible(NULL)
}

# THREADS=only: fill the all-thread columns of existing checkpoints on an
# otherwise idle machine (the single-thread cells are taken in parallel on
# pinned cores; the threaded cells need the whole machine, one at a time).
fillThreadedLinCmtAutoOptimized <- function(nSub = 44L, reps = 3L, nThr = getRxThreads(),
                                            maxSec = Inf, maxLoad = 2) {
  t0 <- proc.time()[["elapsed"]]
  evs <- lapply(.shapes, .shape, nSub = nSub); names(evs) <- .shapes
  fs <- list.files("bench/results/auto_opt", pattern = "\\.rds$", full.names = TRUE)
  for (fn in fs) {
    res <- readRDS(fn)
    if (!anyNA(res$fwdN)) next
    if (proc.time()[["elapsed"]] - t0 > maxSec) { cat("MAXSEC reached before", fn, "\n"); return(invisible(NULL)) }
    load1 <- as.numeric(strsplit(readLines("/proc/loadavg", n = 1L), " ")[[1]][1])
    if (load1 > maxLoad) stop(sprintf("load average %.1f > %.1f: not measuring threaded cells", load1, maxLoad))
    cfg <- res[1, c("ncmt", "oral0", "trans")]
    pars <- c(.transPars(cfg$ncmt, cfg$trans), ka = .ka)
    for (i in seq_len(nrow(res))) {
      if (!is.na(res$fwdN[i])) next
      dirs <- .dirSets(cfg$ncmt, cfg$oral0)
      dirs <- dirs[[which(vapply(dirs, function(d) d$name == res$dirSet[i], TRUE))]]$dirs
      mod <- .gradModel(cfg, dirs)
      evt <- evs[[res$shape[i]]]
      solve1 <- function(st, strat) {
        rxSolve(mod, pars, evt, linCmtSensType = st, linCmtSensStrategy = strat,
                cores = nThr, returnType = "data.frame")
      }
      solve1("AD", "sequential")  # warm
      res$fwdN[i] <- .tm(function() solve1("AD", "sequential"), reps)
      res$revN[i] <- .tm(function() solve1("ADr", "sequential"), reps)
      res$hybN[i] <- .tm(function() solve1("AD", "hybrid"), reps)
    }
    prov <- attr(res, "provenance"); prov$threaded <- .provenance(); attr(res, "provenance") <- prov
    tmp <- paste0(fn, ".tmp"); saveRDS(res, tmp); file.rename(tmp, fn)
    cat("threaded", fn, sprintf("(%.0fs elapsed, load %.1f)\n", proc.time()[["elapsed"]] - t0, load1))
  }
  invisible(NULL)
}

# combine the checkpoints into one file
combineLinCmtAutoOptimized <- function() {
  fs <- list.files("bench/results/auto_opt", pattern = "\\.rds$", full.names = TRUE)
  parts <- lapply(fs, readRDS)
  res <- do.call(rbind, parts)
  attr(res, "provenance") <- lapply(parts, attr, "provenance")
  saveRDS(res, "bench/results/lincmt_auto_optimized.rds")
  invisible(res)
}

if (sys.nframe() == 0L) {
  cfgEnv <- Sys.getenv("CONFIGS", "")
  configs <- if (nzchar(cfgEnv)) strsplit(cfgEnv, ",")[[1]] else NULL
  thr <- Sys.getenv("THREADS", "1")
  maxSec <- as.numeric(Sys.getenv("MAXSEC", "Inf"))
  if (thr == "only") {
    fillThreadedLinCmtAutoOptimized(maxSec = maxSec)
  } else {
    benchLinCmtAutoOptimized(configs = configs, threaded = thr != "0", maxSec = maxSec)
  }
  combineLinCmtAutoOptimized()
}
