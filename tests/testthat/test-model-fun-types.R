rxTest({
  # rxUpdateFuns() casts R_GetCCallable() results to these typedefs; a
  # mismatch with the generated definition is undefined behavior that UBSAN's
  # -fsanitize=function reports on every solve (#1420).
  test_that("generated model functions match the typedefs they are called through", {
    .hdr <- c(
      readLines(system.file("include", "rxode2.h", package = "rxode2")),
      readLines(system.file("include", "rxode2parseStruct.h", package = "rxode2")),
      readLines(system.file("include", "rxode2_model_shared.h", package = "rxode2"))
    )
    .m <- rxode2({
      ka <- 0.5
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - center
      f(depot) <- 1
    })
    .c <- paste(readLines(rxC(.m)), collapse = "\n")
    .prefix <- rxModelVars(.m)$trans["prefix"]
    .kw <- c("int", "unsigned", "long", "short", "char", "double", "float", "void", "const", "*")
    # Normalize a C parameter list to its types: drop names and restrict.
    .types <- function(args) {
      args <- trimws(strsplit(args, ",")[[1]])
      args <- gsub("__restrict__", "", args)
      args <- gsub("\\*", " * ", args)
      vapply(
        args,
        function(a) {
          .tok <- strsplit(trimws(a), "\\s+")[[1]]
          if (length(.tok) > 1 && !(.tok[length(.tok)] %in% .kw)) {
            .tok <- .tok[-length(.tok)]
          }
          paste(.tok, collapse = " ")
        },
        character(1),
        USE.NAMES = FALSE
      )
    }
    .sig <- function(ret, args) {
      c(gsub("\\s+", "", ret), .types(args))
    }
    .typedef <- function(t) {
      .re <- paste0("^\\s*typedef\\s+(.*?)\\(\\*", t, "\\)\\((.*)\\);")
      .l <- unique(grep(.re, .hdr, value = TRUE, perl = TRUE))
      expect_length(.l, 1)
      .sig(sub(.re, "\\1", .l, perl = TRUE), sub(.re, "\\2", .l, perl = TRUE))
    }
    # A definition is `<ret> <name>(<args>) {`; calls and prototypes end in `;`.
    .def <- function(f) {
      .re <- paste0(
        "\\b([A-Za-z_]\\w*(?:\\s*\\*)*)\\s*\\b",
        f,
        "\\s*\\(([^()]*)\\)\\s*\\{"
      )
      .l <- regmatches(.c, gregexpr(.re, .c, perl = TRUE))[[1]]
      expect_length(.l, 1)
      .sig(sub(.re, "\\1", .l, perl = TRUE), sub(.re, "\\2", .l, perl = TRUE))
    }
    .pairs <- c(
      assignFuns = "t_assignFuns",
      dydt = "t_dydt",
      calc_jac = "t_calc_jac",
      calc_lhs = "t_calc_lhs",
      inis = "t_update_inis",
      dydt_lsoda = "t_dydt_lsoda_dum",
      calc_jac_lsoda = "t_jdum_lsoda",
      dydt_liblsoda = "t_dydt_liblsoda",
      ode_solver_solvedata = "t_set_solve",
      ode_solver_get_solvedata = "t_get_solve",
      F = "t_F",
      Lag = "t_LAG",
      Rate = "t_RATE",
      Dur = "t_DUR",
      mtime = "t_calc_mtime",
      ME = "t_ME",
      IndF = "t_IndF",
      dLag = "t_dLag",
      dF = "t_dF",
      dRate = "t_dRate",
      dDur = "t_dDur",
      d2F = "t_dF",
      d2Lag = "t_dLag",
      d2Rate = "t_dRate",
      d2Dur = "t_dDur",
      d3F = "t_dF",
      dFQ = "t_dF",
      dLagJac = "t_dLag",
      dLagQ = "t_dLag",
      dDurQ = "t_dDur"
    )
    .pairs <- stats::setNames(.pairs, paste0(.prefix, names(.pairs)))
    .pairs <- c(.pairs, "__assignFuns2" = "rxode2_assignFuns2_t")
    for (.f in names(.pairs)) {
      expect_identical(.def(.f), .typedef(.pairs[[.f]]), label = .f)
    }
  })
})
