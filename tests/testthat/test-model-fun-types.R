rxTest({
  # Model code and rxode2 call each other through R_GetCCallable() results cast
  # to typedefs; a mismatch with the definition is undefined behavior (UBSAN's
  # -fsanitize=function), and a wrong return type gives wrong values (#1420).
  .inc <- function(f) {
    readLines(system.file("include", f, package = "rxode2"), warn = FALSE)
  }
  .hdr <- c(.inc("rxode2.h"), .inc("rxode2parseStruct.h"), .inc("rxode2_model_shared.h"))
  .hdr <- paste(gsub("//.*$", "", .hdr), collapse = "\n")
  .kw <- c("int", "unsigned", "long", "short", "char", "double", "float", "void", "const", "*")
  # Normalize a C parameter list to its types: drop names and restrict.
  .types <- function(args) {
    args <- trimws(strsplit(args, ",")[[1]])
    args <- gsub("__restrict__", "", args)
    args <- gsub("\\*", " * ", args)
    .ret <- vapply(
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
    if (length(.ret) == 0) "void" else .ret
  }
  .sig <- function(ret, args) {
    c(gsub("\\s+|\\b(extern|RcppExport)\\b|\"C\"", "", ret), .types(args))
  }
  .typedef <- function(t) {
    .re <- paste0("typedef\\s+([^;(]*?)\\(\\s*\\*\\s*", t, "\\s*\\)\\s*\\(([^;]*?)\\)\\s*;")
    .l <- unique(regmatches(.hdr, gregexpr(.re, .hdr, perl = TRUE))[[1]])
    expect_equal(length(.l), 1, label = paste("typedefs of", t))
    .sig(sub(.re, "\\1", .l, perl = TRUE), sub(.re, "\\2", .l, perl = TRUE))
  }
  # A definition is `<ret> <name>(<args>) {`; calls and prototypes end in `;`.
  .defs <- function(src, f) {
    .re <- paste0(
      "(?:^|[;}\n])\\s*((?:extern\\s+\"C\"\\s+)?[A-Za-z_][\\w ]*?(?:\\s*\\*)*)\\s*\\b",
      f,
      "\\s*\\(([^()]*)\\)\\s*\\{"
    )
    .l <- regmatches(src, gregexpr(.re, src, perl = TRUE))[[1]]
    .l <- .l[!grepl("\\bstatic\\b", .l)]
    # A C++ overload may share the name; the registered one is extern "C".
    if (length(.l) > 1) {
      .l <- .l[grepl("extern\\s+\"C\"", .l)]
    }
    expect_equal(length(.l), 1, label = paste("definitions of", f))
    .sig(sub(.re, "\\1", .l, perl = TRUE), sub(.re, "\\2", .l, perl = TRUE))
  }

  test_that("generated model functions match the typedefs rxode2 calls them through", {
    .m <- rxode2({
      ka <- 0.5
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - center
      f(depot) <- 1
    })
    .c <- paste(readLines(rxC(.m)), collapse = "\n")
    .prefix <- rxModelVars(.m)$trans["prefix"]
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
      expect_identical(.defs(.c, .f), .typedef(.pairs[[.f]]), label = .f)
    }
  })

  test_that("rxode2 functions match the typedefs model code calls them through", {
    # Needs rxode2's C sources, so only runs from a source checkout.
    .srcDir <- test_path("..", "..", "src")
    skip_if_not(file.exists(file.path(.srcDir, "init.c")))
    # init.c first: its load-time registration wins over RcppExports.cpp's.
    .files <- list.files(.srcDir, "\\.(c|cpp)$", full.names = TRUE)
    .files <- c(file.path(.srcDir, "init.c"), setdiff(.files, file.path(.srcDir, "init.c")))
    .src <- unlist(lapply(.files, readLines, warn = FALSE))
    .src <- paste(gsub("//.*$", "", .src), collapse = "\n")
    .src <- gsub("/\\*.*?\\*/", "", .src, perl = TRUE)
    .reReg <- paste0(
      "\\bR_RegisterCCallable\\(\\s*\"rxode2\"\\s*,\\s*\"(\\w+)\"\\s*,",
      "\\s*\\(DL_FUNC\\)\\s*&?\\s*(\\w+)\\s*\\)"
    )
    .reg <- regmatches(.src, gregexpr(.reReg, .src, perl = TRUE))[[1]]
    .reg <- stats::setNames(
      sub(.reReg, "\\2", .reg, perl = TRUE),
      sub(.reReg, "\\1", .reg, perl = TRUE)
    )
    .reCast <- "^\\s*\\w+\\s*=\\s*\\((\\w+)\\)\\s*R_GetCCallable\\(\\s*\"rxode2\"\\s*,\\s*\"(\\w+)\"\\s*\\).*$"
    .casts <- grep(.reCast, .inc("rxode2_model_shared.c"), value = TRUE, perl = TRUE)
    expect_gt(length(.casts), 50)
    for (.l in .casts) {
      .t <- sub(.reCast, "\\1", .l, perl = TRUE)
      .s <- sub(.reCast, "\\2", .l, perl = TRUE)
      expect_true(.s %in% names(.reg), label = .s)
      if (.s %in% names(.reg)) {
        expect_identical(.defs(.src, .reg[[.s]]), .typedef(.t), label = .s)
      }
    }
  })

  test_that("rigeom() and ripois() return their draws", {
    .m <- rxode2({
      g <- rigeom(0.3)
      p <- ripois(4)
    })
    .s <- rxWithSeed(42, rxSolve(.m, et(0:1, id = 1:200)))
    .s <- .s[.s$time == 0, ]
    expect_gt(mean(.s$g), 1.5)
    expect_gt(mean(.s$p), 3)
    expect_gt(length(unique(.s$p)), 3)
  })
})
