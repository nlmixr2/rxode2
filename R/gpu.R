## Device (OpenCL C) model code.  codegen writes <model>.cl next to the model's
## C file; its rxg_dydt()/rxg_jac() read only their arguments (see
## inst/include/rxode2gpu).  The same text compiles as host C, which is what
## the CPU build and the tests use.

#' Path of the device (OpenCL C) code written for a compiled model
#'
#' @param obj rxode2 model
#' @return path of the `.cl` file
#' @noRd
#' @author Matthew L. Fidler
.rxGpuModelFile <- function(obj) {
  paste0(as.character(rxC(obj)), "l")
}

#' Can a model's d/dt() run as device (OpenCL) code?
#'
#' Decided while parsing: every function the model calls is registered
#' `gpuSafe` in the translation tables and it reads no solver state.
#'
#' @param obj rxode2 model or model variables
#' @return logical
#' @noRd
#' @author Matthew L. Fidler
.rxGpuSafe <- function(obj) {
  .f <- rxModelVars(obj)$flags
  isTRUE(as.integer(.f["gpuSafe"]) == 1L)
}

#' Device (OpenCL C) code for a compiled model
#'
#' @param obj rxode2 model
#' @return list with `source` (the code, which `#include`s
#'   `rxode2gpu/rxg_model.h`) and `file`
#' @noRd
#' @author Matthew L. Fidler
.rxGpuModelSource <- function(obj) {
  if (!.rxGpuSafe(obj)) {
    stop("this model cannot run as device (GPU) code", call. = FALSE)
  }
  .f <- .rxGpuModelFile(obj)
  if (!file.exists(.f)) {
    stop("the device (OpenCL) code for this model is not available; recompile the model", call. = FALSE)
  }
  list(source = paste(readLines(.f, warn = FALSE), collapse = "\n"), file = .f)
}

#' Directory holding rxode2's installed C headers
#'
#' @return path
#' @noRd
#' @author Matthew L. Fidler
.rxGpuIncludeDir <- function() {
  system.file("include", package = "rxode2")
}

#' Replace `#include "rxode2gpu/..."` lines with the header text
#'
#' OpenCL programs are built from one source string, so the headers the model
#' code includes are pasted in.
#'
#' @param src source text
#' @param inc include directory
#' @param seen headers already pasted
#' @return source text with the rxode2gpu headers inlined
#' @noRd
#' @author Matthew L. Fidler
.rxGpuInline <- function(src, inc = .rxGpuIncludeDir(), seen = new.env(parent = emptyenv())) {
  .lines <- strsplit(src, "\n", fixed = TRUE)[[1]]
  .re <- "^\\s*#include\\s+\"(rxode2gpu/[^\"]+)\"\\s*$"
  .out <- lapply(.lines, function(.l) {
    if (!grepl(.re, .l)) {
      return(.l)
    }
    .h <- sub(.re, "\\1", .l)
    if (!is.null(seen[[.h]])) {
      return(character(0))
    }
    assign(.h, TRUE, envir = seen)
    .f <- file.path(inc, .h)
    if (!file.exists(.f)) {
      stop("cannot find device header '", .h, "'", call. = FALSE)
    }
    .rxGpuInline(paste(readLines(.f, warn = FALSE), collapse = "\n"), inc, seen)
  })
  paste(unlist(.out), collapse = "\n")
}

#' `#define`s that carry rxSolve() options into device code
#'
#' @param safeLog,safeZero,safePow,useStdPow as in `rxControl()`
#' @param float solve in single precision (OpenCL only)
#' @return character vector of `#define` lines
#' @noRd
#' @author Matthew L. Fidler
.rxGpuDefines <- function(safeLog = TRUE, safeZero = TRUE, safePow = TRUE, useStdPow = FALSE, float = FALSE) {
  c(
    sprintf("#define RXG_SAFE_LOG %d", as.integer(safeLog)),
    sprintf("#define RXG_SAFE_ZERO %d", as.integer(safeZero)),
    sprintf("#define RXG_SAFE_POW %d", as.integer(safePow)),
    sprintf("#define RXG_STD_POW %d", as.integer(useStdPow)),
    if (float) "#define RXG_FLOAT"
  )
}

#' One OpenCL C source string for a model's device code
#'
#' @param obj rxode2 model
#' @param ... passed to `.rxGpuDefines()`
#' @param extra extra source appended (eg a kernel); its `rxode2gpu/`
#'   includes are inlined too
#' @return source string
#' @noRd
#' @author Matthew L. Fidler
.rxGpuProgramSource <- function(obj, ..., extra = character(0)) {
  .s <- .rxGpuModelSource(obj)
  ## extra may #include the stepper (rxode2gpu/rxg_stepper.h)
  .src <- paste(c(.s$source, extra), collapse = "\n")
  paste(c(.rxGpuDefines(...), .rxGpuInline(.src)), collapse = "\n")
}

.rxGpuHostEnv <- new.env(parent = emptyenv())

#' Compile a model's device code as host C
#'
#' The device functions are compiled with the host compiler into a small
#' shared library with `.C()` entry points (`rxode2gpu/rxg_host.h`).
#'
#' @param obj rxode2 model
#' @param ... passed to `.rxGpuDefines()` (not `float`)
#' @return list with the `DLLInfo` (`dll`) and the dimensions `neq`,
#'   `nlhs`, `npar` and `hasJac`
#' @noRd
#' @author Matthew L. Fidler
.rxGpuHostLib <- function(obj, ...) {
  .s <- .rxGpuModelSource(obj)
  .src <- c(
    .rxGpuDefines(...),
    sprintf("#include \"%s\"", normalizePath(.s$file, winslash = "/")),
    "#include \"rxode2gpu/rxg_host.h\""
  )
  .hdr <- unlist(lapply(
    c("rxg_prelude.h", "rxg_special.h", "rxg_model.h", "rxg_stepper.h", "rxg_host.h"),
    function(.h) {
      readLines(file.path(.rxGpuIncludeDir(), "rxode2gpu", .h), warn = FALSE)
    }
  ))
  .md5 <- digest::digest(c(.src, .s$source, .hdr))
  .ret <- .rxGpuHostEnv[[.md5]]
  if (!is.null(.ret)) {
    return(.ret)
  }
  .dir <- file.path(rxTempDir(), "rxg")
  if (!dir.exists(.dir)) {
    dir.create(.dir, recursive = TRUE)
  }
  .cFile <- file.path(.dir, paste0("rxg_", .md5, ".c"))
  .dll <- file.path(.dir, paste0("rxg_", .md5, .Platform$dynlib.ext))
  writeLines(.src, .cFile)
  if (!file.exists(.dll)) {
    .mv <- file.path(.dir, "Makevars")
    writeLines(sprintf("PKG_CPPFLAGS=-I\"%s\"", .rxGpuIncludeDir()), .mv)
    .old <- setwd(.dir)
    on.exit(setwd(.old), add = TRUE)
    .out <- suppressWarnings(system2(
      file.path(R.home("bin"), "R"),
      c("CMD", "SHLIB", "-o", basename(.dll), basename(.cFile)),
      stdout = TRUE,
      stderr = TRUE
    ))
    if (!file.exists(.dll)) {
      stop("compiling the device code as host C failed:\n", paste(.out, collapse = "\n"), call. = FALSE)
    }
  }
  .info <- dyn.load(.dll)
  ## through do.call(): R CMD check resolves the symbol of a literal .C() in
  ## rxode2, but this one is in the model's host library
  .fn <- getNativeSymbolInfo("rxg_host_dims", .info)
  .dim <- do.call(.C, list(.fn, d = integer(4)))$d
  .ret <- list(dll = .info, neq = .dim[1], nlhs = .dim[2], npar = .dim[3], hasJac = as.logical(.dim[4]))
  assign(.md5, .ret, envir = .rxGpuHostEnv)
  .ret
}

#' Evaluate a model's device derivatives (or Jacobian) on the host
#'
#' @param lib result of `.rxGpuHostLib()`
#' @param t time
#' @param y states
#' @param p0 parameters at `t0`
#' @param p1 parameters at `t1`
#' @param pl last lhs values
#' @param ir infusion rates
#' @param on compartment on/off flags
#' @param t0,t1 interval ends
#' @param curShift time shift
#' @param tpk record time (used when `nonmem` is `TRUE`)
#' @param tbs transform-both-sides values `rx_lambda_`, `rx_yj_`, `rx_hi_`
#'   and `rx_low_`
#' @param covType 0 constant, 1 linear, 2 midpoint
#' @param nonmem rxControl(nonmem=)
#' @param jac return the Jacobian (row major, `neq` x `neq`) instead
#' @return derivatives, or the Jacobian matrix
#' @noRd
#' @author Matthew L. Fidler
.rxGpuHostEval <- function(
  lib,
  t,
  y,
  p0,
  p1 = p0,
  pl = rep(NA_real_, lib$nlhs),
  ir = rep(0, lib$neq),
  on = rep(1L, lib$neq),
  t0 = t,
  t1 = t,
  curShift = 0,
  tpk = t,
  tbs = c(1, 2, 1, 0),
  covType = 0L,
  nonmem = FALSE,
  jac = FALSE
) {
  .pad <- function(x, n) {
    x <- as.double(x)
    if (n == 0L) {
      return(0)
    }
    stopifnot(length(x) == n)
    x
  }
  .padInt <- function(x, n) {
    stopifnot(length(x) == n)
    x
  }
  .args <- list(
    as.double(t),
    .pad(y, lib$neq),
    if (jac) double(max(1L, lib$neq * lib$neq)) else double(max(1L, lib$neq)),
    .pad(p0, lib$npar),
    .pad(p1, lib$npar),
    .pad(pl, lib$nlhs),
    .pad(ir, lib$neq),
    as.integer(if (lib$neq == 0L) 0L else .padInt(on, lib$neq)),
    as.double(c(t0, t1, curShift, tpk, tbs)),
    as.integer(c(covType, nonmem))
  )
  .fn <- getNativeSymbolInfo(if (jac) "rxg_host_jac" else "rxg_host_dydt", lib$dll)
  .r <- do.call(.C, c(list(.fn), .args, list(NAOK = TRUE)))[[3]]
  if (jac) {
    return(matrix(.r, lib$neq, lib$neq, byrow = TRUE))
  }
  .r
}

## Device integrators (inst/include/rxode2gpu/rxg_stepper.h), named in
## rxode2's style; "+" joins an AutoSwitch composite
.rxGpuMethods <- c(rkt54 = 1L, vern76 = 2L, ros54p = 3L, "rkt54+ros54p" = 4L, "vern76+ros54p" = 5L)

#' Solve over a time grid with a device integrator built as host C
#'
#' @inheritParams .rxGpuHostEval
#' @param method one of `names(.rxGpuMethods)`
#' @param times output times (the first is the start)
#' @param h starting step size (0 chooses one)
#' @param atol,rtol tolerances (recycled to the number of states)
#' @param hmin,hmax step size limits (0: none)
#' @param mxstep most steps per interval
#' @param autoSwitchNonstifftol,autoSwitchStifftol as in `rxControl()`
#' @param autoSwitchMaxStiff,autoSwitchMaxNonstiff as in `rxControl()`
#' @param autoSwitchSwitchMax,autoSwitchStiffFirst as in `rxControl()`
#' @param histCap steps the `delay()` history holds
#' @return list with the states `y` (one row per time), the last step size
#'   `h`, `istate` (2 is success), the counters `cnt` (steps, accepted,
#'   rejected, derivative evaluations, Jacobians, LU factorizations),
#'   `stiffIntervals` (intervals solved with ros54p), `stiffEnd` (intervals a
#'   composite started on the stiff secondary, without probing) and `histN`
#'   (`delay()` history records used)
#' @noRd
#' @author Matthew L. Fidler
.rxGpuHostSolve <- function(
  lib,
  method,
  times,
  y,
  p0,
  p1 = p0,
  h = 0,
  atol = 1e-8,
  rtol = 1e-6,
  hmin = 0,
  hmax = 0,
  mxstep = 70000L,
  autoSwitchNonstifftol = 9 / 10,
  autoSwitchStifftol = 9 / 10,
  autoSwitchMaxStiff = 10L,
  autoSwitchMaxNonstiff = 3L,
  autoSwitchSwitchMax = 5L,
  autoSwitchStiffFirst = FALSE,
  histCap = 100000L,
  pl = rep(NA_real_, lib$nlhs),
  ir = rep(0, lib$neq),
  on = rep(1L, lib$neq),
  curShift = 0,
  tpk = times[1],
  tbs = c(1, 2, 1, 0),
  covType = 0L,
  nonmem = FALSE
) {
  .m <- .rxGpuMethods[match.arg(method, names(.rxGpuMethods))]
  .n <- max(1L, lib$neq)
  .nt <- length(times)
  stopifnot(.nt >= 1L, length(on) == lib$neq)
  .v <- function(x, n) {
    x <- as.double(x)
    if (n == 0L) {
      return(0)
    }
    stopifnot(length(x) == n)
    x
  }
  .y <- double(.n * .nt)
  .y[seq_len(lib$neq)] <- as.double(y)
  .r <- do.call(
    .C,
    list(
      getNativeSymbolInfo("rxg_host_solve", lib$dll),
      y = .y,
      as.double(times),
      as.integer(.nt),
      dw = as.double(c(hmin, hmax, autoSwitchNonstifftol, autoSwitchStifftol, h)),
      iw = as.integer(c(
        .m,
        mxstep,
        autoSwitchMaxStiff,
        autoSwitchMaxNonstiff,
        autoSwitchSwitchMax,
        autoSwitchStiffFirst,
        0L,
        0L,
        0L,
        histCap,
        0L
      )),
      as.double(rep_len(atol, .n)),
      as.double(rep_len(rtol, .n)),
      cnt = integer(6),
      .v(p0, lib$npar),
      .v(p1, lib$npar),
      .v(pl, lib$nlhs),
      .v(ir, lib$neq),
      as.integer(if (lib$neq == 0L) 0L else on),
      as.double(c(times[1], times[.nt], curShift, tpk, tbs)),
      as.integer(c(covType, nonmem)),
      NAOK = TRUE
    )
  )
  list(
    y = matrix(.r$y, .nt, .n, byrow = TRUE),
    h = .r$dw[5],
    istate = .r$iw[7],
    cnt = stats::setNames(.r$cnt, c("step", "accept", "reject", "fev", "jac", "lu")),
    stiffIntervals = .r$iw[8],
    stiffEnd = .r$iw[9],
    histN = .r$iw[11]
  )
}

#' Integrate one interval with a device integrator built as host C
#'
#' @inheritParams .rxGpuHostSolve
#' @param t0,t1 interval
#' @return list with the final `y`, the next step size `h`, `istate` and the
#'   counters `cnt`
#' @noRd
#' @author Matthew L. Fidler
.rxGpuHostIntegrate <- function(lib, method, t0, t1, y, p0, p1 = p0, ...) {
  .r <- .rxGpuHostSolve(lib, method, c(t0, t1), y, p0, p1, ...)
  list(y = .r$y[2, seq_len(lib$neq)], h = .r$h, istate = .r$istate, cnt = .r$cnt)
}
