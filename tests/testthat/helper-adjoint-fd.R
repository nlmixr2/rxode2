# Central finite difference of rxSolve() for each parameter in `cs`, named like
# the adjoint output (rx__sens_<state>_BY_<param>__): dop853 at a relative step
# of 1e-5.
.adjFd <- function(txt, ev, p, cs = names(p)) {
  .m <- rxode2(txt)
  .ret <- list()
  for (.pn in cs) {
    .h <- max(abs(p[[.pn]]), 1) * 1e-5
    .pu <- p
    .pd <- p
    .pu[.pn] <- .pu[.pn] + .h
    .pd[.pn] <- .pd[.pn] - .h
    .u <- as.data.frame(rxSolve(.m, ev, params = .pu, method = "dop853", atol = 1e-13, rtol = 1e-13))
    .d <- as.data.frame(rxSolve(.m, ev, params = .pd, method = "dop853", atol = 1e-13, rtol = 1e-13))
    for (.s in rxStateOde(.m)) {
      .ret[[paste0("rx__sens_", .s, "_BY_", .pn, "__")]] <- (.u[[.s]] - .d[[.s]]) / (2 * .h)
    }
  }
  .ret
}
