# Central finite difference of rxSolve() for each parameter in `cs` at a relative
# step h0, named like the adjoint output (rx__sens_<state>_BY_<param>__): dop853
# (or `method`) with tight solver and steady-state tolerances; `...` goes to
# rxSolve().  dop853 misplaces the state after a pushed infusion stop that falls
# between two records, so those tests use liblsoda.
.adjFdStep <- function(m, ev, p, cs, h0, method = "dop853", ...) {
  .ret <- list()
  for (.pn in cs) {
    .h <- max(abs(p[[.pn]]), 1) * h0
    .pu <- p
    .pd <- p
    .pu[.pn] <- .pu[.pn] + .h
    .pd[.pn] <- .pd[.pn] - .h
    .u <- as.data.frame(rxSolve(m, ev, params = .pu, method = method, atol = 1e-13, rtol = 1e-13,
      ssAtol = 1e-13, ssRtol = 1e-13, maxSS = 50000, ...))
    .d <- as.data.frame(rxSolve(m, ev, params = .pd, method = method, atol = 1e-13, rtol = 1e-13,
      ssAtol = 1e-13, ssRtol = 1e-13, maxSS = 50000, ...))
    for (.s in rxStateOde(m)) {
      .ret[[paste0("rx__sens_", .s, "_BY_", .pn, "__")]] <- (.u[[.s]] - .d[[.s]]) / (2 * .h)
    }
  }
  .ret
}

# The finite-difference reference for the adjoint tests: Richardson-extrapolated
# from relative steps of 1e-4 and 5e-5, which removes the O(h^2) error.
.adjFd <- function(txt, ev, p, cs = names(p), ...) {
  .m <- rxode2(txt)
  .a <- .adjFdStep(.m, ev, p, cs, 1e-4, ...)
  .b <- .adjFdStep(.m, ev, p, cs, 5e-5, ...)
  for (.n in names(.a)) .a[[.n]] <- (4 * .b[[.n]] - .a[[.n]]) / 3
  .a
}
