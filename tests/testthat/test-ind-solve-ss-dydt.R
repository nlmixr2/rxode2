rxTest({
  # ind_solve() integrates a subject with the caller's model (c_dydt), which
  # need not be the model rxSolve() installed -- nlmixr2est's solve pools run a
  # narrower pred model this way, under setIndNeqOverride().  The steady-state
  # cycles (solveWith1Pt -> _rxSolveOneInterval) called the INSTALLED model's
  # dydt by name instead: wrong steady state, and with the installed model wider
  # than the override, its dydt wrote past dop853's k-buffers and aborted in
  # dop853Next's R_Free(ctx.k10) ("free(): invalid next size").
  test_that("steady state in ind_solve() uses the caller's dydt", {
    skip_on_cran()
    .inc <- system.file("include", package = "rxode2")
    skip_if(!file.exists(file.path(.inc, "rxode2.h")), "rxode2 headers not installed")
    .dir <- tempfile("indss")
    dir.create(.dir)
    on.exit(unlink(.dir, recursive = TRUE), add = TRUE)
    .c <- file.path(.dir, "indss.c")
    .src <- c(
      "#include <rxode2.h>",
      "typedef void (*t_ind_solve)(rx_solve *, unsigned int, t_dydt_liblsoda,",
      "  t_dydt_lsoda_dum, t_jdum_lsoda, t_dydt, t_update_inis, int);",
      "typedef rx_solve *(*t_getRx)(void);",
      "typedef void (*t_setOv)(rx_solving_options_ind *, int);",
      "SEXP indss(SEXP trans, SEXP neqOv, SEXP jtS) {",
      "  const char *lib = CHAR(STRING_ELT(trans, 0));",
      "  t_dydt f = (t_dydt) R_GetCCallable(lib, CHAR(STRING_ELT(trans, 3)));",
      "  t_update_inis u = (t_update_inis) R_GetCCallable(lib, CHAR(STRING_ELT(trans, 8)));",
      "  t_dydt_lsoda_dum fl = (t_dydt_lsoda_dum) R_GetCCallable(lib, CHAR(STRING_ELT(trans, 9)));",
      "  t_jdum_lsoda jd = (t_jdum_lsoda) R_GetCCallable(lib, CHAR(STRING_ELT(trans, 10)));",
      "  t_dydt_liblsoda fll = (t_dydt_liblsoda) R_GetCCallable(lib, CHAR(STRING_ELT(trans, 13)));",
      "  t_ind_solve is = (t_ind_solve) R_GetCCallable(\"rxode2\", \"ind_solve\");",
      "  t_getRx gr = (t_getRx) R_GetCCallable(\"rxode2\", \"getRxSolve_\");",
      "  t_setOv so = (t_setOv) R_GetCCallable(\"rxode2\", \"setIndNeqOverride\");",
      "  rx_solve *rx = gr();",
      "  rx_solving_options_ind *ind = &(rx->subjects[0]);",
      "  int ov = INTEGER(neqOv)[0];",
      "  if (ov > 0) so(ind, ov);",
      "  is(rx, 0, fll, fl, jd, f, u, INTEGER(jtS)[0]);",
      "  int n = ov > 0 ? ov : rx->op->neq;",
      "  SEXP ret = PROTECT(Rf_allocVector(REALSXP, n));",
      "  double *last = ind->solve + (size_t)n * (ind->n_all_times - 1);",
      "  for (int k = 0; k < n; k++) REAL(ret)[k] = last[k];",
      "  if (ov > 0) so(ind, -1);",
      "  UNPROTECT(1);",
      "  return ret;",
      "}"
    )
    writeLines(.src, .c)
    .old <- setwd(.dir)
    on.exit(setwd(.old), add = TRUE)
    .r <- file.path(R.home("bin"), "R")
    .env <- paste0("PKG_CPPFLAGS=-I", shQuote(.inc))
    .out <- suppressWarnings(system2(.r, c("CMD", "SHLIB", "indss.c"), env = .env, stdout = TRUE, stderr = TRUE))
    .so <- file.path(.dir, paste0("indss", .Platform$dynlib.ext))
    skip_if(!file.exists(.so), paste(.out, collapse = "\n"))
    .dll <- dyn.load(.so)
    on.exit(dyn.unload(.so), add = TRUE)
    # installed model: wider, and its first two states eliminate 3x faster
    .wide <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - 3 * cl / v * center
      d/dt(x3) <- -x3
      d/dt(x4) <- x3 - x4
    })
    .same <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - cl / v * center
      d/dt(x3) <- -x3
      d/dt(x4) <- x3 - x4
    })
    .narrow <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - cl / v * center
    })
    .ev <- et(amt = 320, ii = 12, ss = 1) |> et(seq(0, 24, by = 2))
    .p <- c(ka = 1.5, cl = 2.7, v = 31)
    .jt <- 2L
    for (.meth in c("dop853", "lsoda", "liblsoda", "rk4")) {
      for (.case in c("same", "narrow")) {
        .m <- if (.case == "same") .same else .narrow
        .nOv <- if (.case == "same") -1L else 2L
        .ref <- rxSolve(.m, .p, .ev, method = .meth, returnType = "data.frame")
        .ref <- unlist(.ref[nrow(.ref), c("depot", "center")])
        rxSolve(.wide, .p, .ev, method = .meth, .setupOnly = 1L)
        .got <- .Call(.dll$indss, rxModelVars(.m)$trans, .nOv, .jt)
        rxSolveFree()
        expect_equal(.got[1:2], unname(.ref), tolerance = 1e-6, info = paste(.meth, .case))
      }
    }
  })
})
