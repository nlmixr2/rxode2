# Shared setup for the linCmtB(which1 = -9 / -10) per-origin tests, which split
# in two: what the modes COMPUTE (test-lincmt-origin-sens.R) and what the
# decomposition REFUSES to answer (test-lincmt-origin-limits.R).
#
# which2 packs the origin compartment and the wanted output: q*8 + out, with
# out = 7 meaning the reported concentration.  Nothing here builds a model at
# load time, so this is inert until a test calls it.

.p <- c(tcl = log(4), tv = log(20), tka = log(1.1), tq = log(2),
        tv2 = log(50), tq2 = log(1), tv3 = log(80),
        eta_lag = 0.1, eta_f = 0.05)

# Build a pure linCmt() model of the requested shape whose dosed compartment
# (depot when oral, central otherwise) carries a modeled alag(), reporting
# the concentration, the shared-delay sensitivity (-3) and the
# per-compartment one (-9) for origin `q` and the next compartment up.
.rxOriginModel <- function(ncmt, oral0, q) {
  .pars <- c("cl, v, 0, 0, 0, 0", "cl, v, qq, v2, 0, 0",
             "cl, v, qq, v2, q2, v3")[ncmt]
  .ka <- if (oral0) "ka" else "0"
  .m <- ncmt + oral0
  .cmt <- if (oral0) "depot" else "central"
  .call <- function(w1, w2) {
    sprintf("linCmtB(rx__PTR__, t, %d, %d, %d, %s, %s, 1, %s, %s)",
            .m, ncmt, oral0, w1, w2, .pars, .ka)
  }
  .b <- if (q + 1L < .m) .call("-9", (q + 1L) * 8 + 7) else "0"
  rxode2(sprintf("
    cl <- exp(tcl); v <- exp(tv); qq <- exp(tq); v2 <- exp(tv2)
    q2 <- exp(tq2); v3 <- exp(tv3); ka <- exp(tka)
    lag <- 2 * exp(eta_lag)
    alag(%s) <- lag
    cp  <- %s
    d3  <- lag * %s
    d9  <- lag * %s
    d9b <- lag * (%s)
  ", .cmt, .call("-1", "-1"), .call("-3", "-3"),
     .call("-9", q * 8 + 7), .b))
}

.fd <- function(m, e, par, h = 1e-6) {
  .p1 <- .p; .p1[[par]] <- .p[[par]] + h
  .p0 <- .p; .p0[[par]] <- .p[[par]] - h
  (rxSolve(m, e, params = .p1)$cp - rxSolve(m, e, params = .p0)$cp) / (2 * h)
}
.rel <- function(a, b) max(abs(a - b)) / (max(abs(b)) + 1e-8)

