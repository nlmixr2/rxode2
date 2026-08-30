# Exploration: "solve the furthest observation and back fill" for linear
# compartment models (user idea), plus the closed-form adjoint analogue.
#
# Three formalizations compared against the SHIPPED sequential tail
# (rxode2 branch lincmt-carry-jump, which is already row-to-row:
# linCmtBsolveRow does setDt(_t - ind->tprior), so each row pays
# exp(-L*delta_row) per requested direction in fvar):
#   (a) shipped forward tail: k fvar tail passes per obs (m fvar exps each)
#   (b) semigroup forward recurrence: A_{i+1} = Phi(delta)*A_i + input;
#       sens: dA_{i+1}/dth_j = Phi*dA_i/dth_j + dPhi_j*A_i + dinput_j.
#       With UNIFORM spacing Phi and dPhi_j are constant -> multiply-only.
#       Non-uniform: m DOUBLE exps per row shared across directions
#       (tangents dE = -delta*dL_j*E by multiplication), vs the shipped
#       k*(m or m+1) fvar exps per row.
#   (c) closed-form ADJOINT for a scalar inner objective: values forward
#       once; lambda backward once (lambda_i = Phi^T lambda_{i+1} + seed_i);
#       parameter gradient via the accumulated outer-product matrix
#       S = sum_i A_i lambda_{i+1}^T, so per-obs cost ~3*m^2 flops
#       INDEPENDENT of k; the k contractions trace(dPhi_j S) happen once
#       per subject.  (Backward analogue of the ODE adjoint sensMethod.)
#   plus the literal "backfill" (backward reconstruction from the furthest
#   observation), which is shown UNSTABLE: the fast eigencomponent is lost
#   to roundoff at large t and multiplying by exp(+L2*delta) amplifies that
#   noise without bound.
#
# Validation discipline: values vs production linCmt() (rxSolve on the
# carry-jump tree, read-only) AND vs linToOde-style explicit ODE integrated
# with useLinCmt=FALSE at 1e-12; sensitivities vs central FD on the
# PRODUCTION values (h = 1e-6 relative), never vs hand-derived references
# alone.  2-cmt oral, trans=1 (cl,v,q,v2,ka).

message("== lincmt_backfill_explore ==")
rxTree <- Sys.getenv("LINCMT_RXODE2", "~/src/rxode2-lincmt-analytic")
suppressMessages(devtools::load_all(rxTree, compile = FALSE, quiet = TRUE))
rxode2::setRxThreads(1L)

## ---- model + designs -------------------------------------------------
th <- c(cl = 4, v = 20, q = 6, v2 = 60, ka = 1.2)
mLin <- rxode2::rxode2({
  cp <- linCmt(cl, v, q, v2, ka)
})
mOde <- rxode2::rxode2({
  d/dt(depot)   <- -ka*depot
  d/dt(central) <-  ka*depot - (cl/v + q/v)*central + (q/v2)*periph
  d/dt(periph)  <-  (q/v)*central - (q/v2)*periph
  cp <- central/v
})

mkEv <- function(obsT, doseT = 0, amt = 100) {
  ev <- rxode2::et(amt = amt, time = doseT)
  ev <- rxode2::et(ev, obsT)
  ev
}
designs <- list(
  uniform    = list(obs = seq(0.5, 100, by = 0.5), dose = 0),
  nonuniform = list(obs = sort(c(exp(seq(log(0.25), log(96), length.out = 150)),
                                 seq(1, 99, by = 3.7))), dose = 0),
  interleaved = list(obs = as.numeric(sapply(0:4, function(d) 24*d + c(0.5, 1, 2, 4, 8, 12, 16, 20))),
                     dose = c(0, 24, 48, 72, 96))
)

solveLin <- function(thv, des) {
  s <- rxode2::rxSolve(mLin, thv, mkEv(des$obs, des$dose), cores = 1L,
                       addDosing = FALSE, useLinCmt = TRUE)
  s$cp
}
solveOde <- function(thv, des) {
  s <- rxode2::rxSolve(mOde, thv, mkEv(des$obs, des$dose), cores = 1L,
                       addDosing = FALSE, useLinCmt = FALSE,
                       atol = 1e-12, rtol = 1e-12)
  s$cp
}

## ---- eigen pieces (transcribed from src/solComp.h:49-80, validated below) --
solComp2 <- function(k10, k12, k21) {
  sum2 <- k10 + k12 + k21
  disc <- sqrt((sum2 - 2*sqrt(k10*k21)) * (sum2 + 2*sqrt(k10*k21)))
  L <- c(0.5*(sum2 + disc), 0.5*(sum2 - disc))
  invD0 <- 1/(L[2] - L[1] + .Machine$double.eps); invD1 <- -invD0
  C1 <- cbind(c((k21 - L[1])*invD0, k12*invD0), c((k21 - L[2])*invD1, k12*invD1))
  C2 <- cbind(c(k21*invD0, (k10 + k12 - L[1])*invD0), c(k21*invD1, (k10 + k12 - L[2])*invD1))
  list(L = L, C1 = C1, C2 = C2)
}
micro <- function(thv) list(k10 = thv[["cl"]]/thv[["v"]], k12 = thv[["q"]]/thv[["v"]],
                            k21 = thv[["q"]]/thv[["v2"]], ka = thv[["ka"]], v = thv[["v"]])

# transition matrix over delta for state (depot, central, periph):
#   depot evolves by exp(-ka*delta); (central, periph) get
#   Phi2 = C1 %*% diag(E) %*% ... in solComp form the columns of C1/C2 are the
#   contributions of the two prior states; depot feeds via the Ea term.
phiMat <- function(sc, ka, delta) {
  E  <- exp(-sc$L*delta)
  Ea <- (E - exp(-ka*delta))/(ka - sc$L)
  # 3x3: rows/cols = depot, central, periph
  P <- matrix(0, 3, 3)
  P[1, 1] <- exp(-ka*delta)
  # contribution of prior central (yp[1]): C1 %*% E ; prior periph: C2 %*% E
  P[2:3, 2] <- sc$C1 %*% E
  P[2:3, 3] <- sc$C2 %*% E
  # depot -> central/periph: ka*yp0 * (C1 %*% Ea)
  P[2:3, 1] <- ka * (sc$C1 %*% Ea)
  P
}

## ---- (b) forward semigroup recurrence: values + forward sens ----------
recurEval <- function(thv, des, h = 1e-7) {
  mc <- micro(thv); sc <- solComp2(mc$k10, mc$k12, mc$k21)
  nm <- names(th)
  # dPhi_j by central FD on theta (exploration-level; a C version uses the
  # window tangents dL/dC already hoisted by linCmtWinFill)
  times <- sort(unique(c(des$dose, des$obs)))
  evs <- lapply(times, function(t0) if (t0 %in% des$dose) 100 else 0)
  A <- c(0, 0, 0); dA <- matrix(0, 3, length(nm), dimnames = list(NULL, nm))
  tPrev <- times[1]
  # dose at t=times[1] if it is a dose row
  cp <- numeric(0); dcp <- NULL; obsIdx <- integer(0)
  phiCache <- new.env(parent = emptyenv())
  getPhi <- function(delta, thv2) {
    key <- sprintf("%.17g", delta)
    got <- phiCache[[key]]
    if (!is.null(got)) return(got)
    mc2 <- micro(thv2); sc2 <- solComp2(mc2$k10, mc2$k12, mc2$k21)
    P <- phiMat(sc2, mc2$ka, delta)
    dP <- lapply(nm, function(p) {
      tp <- thv2; tm <- thv2
      tp[[p]] <- tp[[p]]*(1 + h); tm[[p]] <- tm[[p]]*(1 - h)
      mcp <- micro(tp); scp <- solComp2(mcp$k10, mcp$k12, mcp$k21)
      mcm <- micro(tm); scm <- solComp2(mcm$k10, mcm$k12, mcm$k21)
      (phiMat(scp, mcp$ka, delta) - phiMat(scm, mcm$ka, delta))/(2*thv2[[p]]*h)
    })
    names(dP) <- nm
    val <- list(P = P, dP = dP)
    phiCache[[key]] <- val
    val
  }
  nPhi <- 0L
  for (i in seq_along(times)) {
    t0 <- times[i]
    if (i > 1) {
      delta <- t0 - tPrev
      pc <- getPhi(delta, thv); nPhi <- nPhi + 1L
      for (p in nm) dA[, p] <- pc$P %*% dA[, p] + pc$dP[[p]] %*% A
      A <- as.numeric(pc$P %*% A)
    }
    if (t0 %in% des$dose) A[1] <- A[1] + 100
    if (t0 %in% des$obs) {
      cp <- c(cp, A[2]/thv[["v"]])
      row <- dA[2, ]/thv[["v"]]; row[["v"]] <- row[["v"]] - A[2]/thv[["v"]]^2
      dcp <- rbind(dcp, row)
    }
    tPrev <- t0
  }
  list(cp = cp, dcp = dcp, uniquePhi = length(ls(phiCache)), rows = length(times) - 1L)
}

## ---- (c) adjoint: scalar objective grad, k-independent per obs --------
adjointGrad <- function(thv, des, y) {
  # L = 0.5*sum((y_i - cp_i)^2); dL/dtheta via backward lambda + outer-product
  mc <- micro(thv)
  fw <- recurEval(thv, des)          # reuse forward values (and Phi cache logic)
  nm <- names(th)
  times <- sort(unique(c(des$dose, des$obs)))
  # rebuild per-interval Phi and store forward states
  sc <- solComp2(mc$k10, mc$k12, mc$k21)
  A <- c(0, 0, 0); Alist <- list(); Plist <- list()
  tPrev <- times[1]
  for (i in seq_along(times)) {
    t0 <- times[i]
    if (i > 1) {
      P <- phiMat(sc, mc$ka, t0 - tPrev)
      Plist[[i]] <- P
      A <- as.numeric(P %*% A)
    }
    if (t0 %in% des$dose) A[1] <- A[1] + 100
    Alist[[i]] <- A
    tPrev <- t0
  }
  cp <- fw$cp
  res <- cp - y
  # backward pass: lambda over states; S accumulates A_{i-1} lambda_i^T
  lam <- c(0, 0, 0); S <- matrix(0, 3, 3); dVextra <- 0
  oi <- length(res)
  for (i in rev(seq_along(times))) {
    t0 <- times[i]
    if (t0 %in% des$obs) {
      lam[2] <- lam[2] + res[oi]/thv[["v"]]
      dVextra <- dVextra - res[oi]*Alist[[i]][2]/thv[["v"]]^2
      oi <- oi - 1L
    }
    if (i > 1) {
      S <- S + outer(lam, Alist[[i - 1L]])   # lambda_i A_{i-1}^T : m^2/interval
      lam <- as.numeric(t(Plist[[i]]) %*% lam)
    }
  }
  # k contractions ONCE per subject: dL/dth_j = sum(S * dPhi_j) (+ direct V term)
  h <- 1e-7
  g <- sapply(nm, function(p) {
    tp <- thv; tm <- thv
    tp[[p]] <- tp[[p]]*(1 + h); tm[[p]] <- tm[[p]]*(1 - h)
    tot <- 0
    tPrev <- times[1]
    for (i in seq_along(times)) {
      t0 <- times[i]
      if (i > 1) {
        mcp <- micro(tp); scp <- solComp2(mcp$k10, mcp$k12, mcp$k21)
        mcm <- micro(tm); scm <- solComp2(mcm$k10, mcm$k12, mcm$k21)
        dP <- (phiMat(scp, mcp$ka, t0 - tPrev) - phiMat(scm, mcm$ka, t0 - tPrev))/(2*thv[[p]]*h)
        # trace(dP %*% S) with S = sum lambda_i A_{i-1}^T -> sum(S * dP)
        tot <- tot + sum(S * dP)
      }
      tPrev <- t0
    }
    tot
  })
  g[["v"]] <- g[["v"]] + dVextra
  g
}

## ---- (backfill) instability demo -------------------------------------
backfillDemo <- function(thv, span = 100, delta = 0.5) {
  mc <- micro(thv); sc <- solComp2(mc$k10, mc$k12, mc$k21)
  n <- span/delta
  P  <- phiMat(sc, mc$ka, delta)
  A0 <- c(100, 0, 0)
  fwd <- vector("list", n + 1); fwd[[1]] <- A0
  for (i in 1:n) fwd[[i + 1]] <- as.numeric(P %*% fwd[[i]])
  # backfill from the furthest state with the inverse transition
  Pi <- solve(P)
  back <- vector("list", n + 1); back[[n + 1]] <- fwd[[n + 1]]
  for (i in n:1) back[[i]] <- as.numeric(Pi %*% back[[i + 1]])
  relerr <- sapply(1:(n + 1), function(i) {
    a <- fwd[[i]]; b <- back[[i]]
    max(abs(b - a)/pmax(abs(a), 1e-300))
  })
  data.frame(t = (0:n)*delta, relerr = relerr,
             growthBound = exp(max(sc$L)*( (n:0)*delta ))*.Machine$double.eps)
}

## ---- run --------------------------------------------------------------
out <- list()
for (dn in names(designs)) {
  des <- designs[[dn]]
  vLin <- solveLin(th, des); vOde <- solveOde(th, des)
  pr <- recurEval(th, des)
  # FD sens on PRODUCTION values as the trusted sens reference
  fdS <- sapply(names(th), function(p) {
    h <- 1e-6; tp <- th; tm <- th
    tp[[p]] <- tp[[p]]*(1 + h); tm[[p]] <- tm[[p]]*(1 - h)
    (solveLin(tp, des) - solveLin(tm, des))/(2*th[[p]]*h)
  })
  # NOTE: an adjoint prototype (option (c)) was drafted here and REMOVED from
  # the run: the user confirmed likInner0 consumes PER-OBSERVATION gradient
  # columns, which a scalar adjoint cannot supply, so (c) cannot serve FOCEi's
  # inner problem and prototype effort on it was stopped before its assembly
  # was debugged to validation (adjointGrad above is kept as a sketch only --
  # it does NOT validate and must not be quoted).
  out[[dn]] <- list(
    valVsProd = max(abs(pr$cp - vLin)/pmax(abs(vLin), 1e-300)),
    valVsOde  = max(abs(pr$cp - vOde)/pmax(abs(vOde), 1e-300)),
    prodVsOde = max(abs(vLin - vOde)/pmax(abs(vOde), 1e-300)),
    sensVsFdMax = max(abs((pr$dcp - fdS)/pmax(abs(fdS), 1e-8))),
    uniquePhi = pr$uniquePhi, rows = pr$rows
  )
  message(sprintf("%-11s val|prod %.2e  val|ode %.2e  sens|FD %.2e  uniquePhi %d/%d rows",
                  dn, out[[dn]]$valVsProd, out[[dn]]$valVsOde,
                  out[[dn]]$sensVsFdMax,
                  out[[dn]]$uniquePhi, out[[dn]]$rows))
}
bf <- backfillDemo(th)
iBad <- which(bf$relerr > 1e-6)[1]
message(sprintf("backfill instability: relerr passes 1e-6 at t=%.1f (span 100, Lmax=%.3f); max relerr %.2e",
                if (is.na(iBad)) NA else bf$t[iBad], max(solComp2(micro(th)$k10, micro(th)$k12, micro(th)$k21)$L),
                max(bf$relerr)))

saveRDS(list(results = out, backfill = bf,
             provenance = list(date = as.character(Sys.Date()),
                               rxTree = rxTree,
                               note = "R prototype; FD dPhi (C version would use linCmtWinFill tangents)")),
        "~/src/linCmt-time-varying/bench-hybrid/lincmt_backfill_explore.rds")
message("saved lincmt_backfill_explore.rds")
