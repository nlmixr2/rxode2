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

# The event table with observations added on a grid of step `by` and halfway
# between every two record times of each subject, carrying the covariates of the
# record before them.  Returns the table and, among its observations in solve
# order, the positions of the original ones.
.adjFdDense <- function(ev, by) {
  .d <- as.data.frame(ev)
  .idc <- which(tolower(names(.d)) == "id")[1]
  .tc <- which(tolower(names(.d)) == "time")[1]
  .ec <- which(tolower(names(.d)) == "evid")[1]
  if (is.na(.idc)) {
    .d$id <- 1L
    .idc <- ncol(.d)
  }
  .d$rxFdOrig <- TRUE
  .blank <- which(tolower(names(.d)) %in% c("amt", "rate", "dur", "cmt", "dv", "mdv"))
  .zero <- which(tolower(names(.d)) %in% c("ii", "addl", "ss"))
  .out <- lapply(split(.d, .d[[.idc]]), function(.s) {
    .s <- .s[order(.s[[.tc]]), , drop = FALSE]
    .t <- sort(unique(.s[[.tc]]))
    .new <- c(seq(min(.t), max(.t), by = by), (.t[-1] + .t[-length(.t)]) / 2)
    .new <- sort(unique(.new[vapply(.new, function(.x) all(abs(.x - .t) > 1e-8), logical(1))]))
    if (length(.new) == 0) return(.s)
    .add <- .s[findInterval(.new, .s[[.tc]]), , drop = FALSE]
    .add[[.tc]] <- .new
    .add[[.ec]] <- 0L
    for (.j in .blank) .add[[.j]] <- NA
    for (.j in .zero) .add[[.j]] <- 0
    .add$rxFdOrig <- FALSE
    .r <- rbind(.s, .add)
    .r[order(.r[[.tc]], !.r$rxFdOrig), , drop = FALSE]
  })
  .r <- do.call(rbind, .out)
  .obs <- .r[.r[[.ec]] == 0, , drop = FALSE]
  list(ev = .r[, names(.r) != "rxFdOrig"], orig = which(.obs$rxFdOrig))
}

# Richardson-extrapolated central difference from relative steps of 1e-4 and
# 5e-5, which removes the O(h^2) error.
.adjFdRich <- function(m, ev, p, cs, ...) {
  .a <- .adjFdStep(m, ev, p, cs, 1e-4, ...)
  .b <- .adjFdStep(m, ev, p, cs, 5e-5, ...)
  for (.n in names(.a)) .a[[.n]] <- (4 * .b[[.n]] - .a[[.n]]) / 3
  .a
}

# The finite-difference reference for the adjoint tests.  With `layout` (a grid
# step) it is computed again with the records of .adjFdDense() added and must
# agree to `layoutTol`, so a forward result that depends on where the records
# fall (an event lost between two records) cannot pass for the reference; the
# step must be finer than the closest two events.
.adjFd <- function(txt, ev, p, cs = names(p), ..., layout = NULL, layoutTol = 1e-6) {
  .m <- rxode2(txt)
  .a <- .adjFdRich(.m, ev, p, cs, ...)
  if (!is.null(layout)) {
    .dn <- .adjFdDense(ev, layout)
    .b <- .adjFdRich(.m, .dn$ev, p, cs, ...)
    for (.n in names(.a)) {
      .d <- max(abs(.b[[.n]][.dn$orig] - .a[[.n]]))
      if (!(.d <= layoutTol)) {
        stop(sprintf("the finite difference of %s depends on the record layout (%g)", .n, .d), call. = FALSE)
      }
    }
  }
  .a
}
