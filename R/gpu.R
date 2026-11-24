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
#' @param method integrator code (`.rxGpuMethods`) the program is built for,
#'   or `NULL` for all of them
#' @return character vector of `#define` lines
#' @noRd
#' @author Matthew L. Fidler
.rxGpuDefines <- function(
  safeLog = TRUE,
  safeZero = TRUE,
  safePow = TRUE,
  useStdPow = FALSE,
  float = FALSE,
  method = NULL
) {
  c(
    if (!is.null(method)) sprintf("#define RXG_METHOD %d", as.integer(method)),
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

#' Does a model's device code carry its analytic Jacobian?
#'
#' @param obj rxode2 model
#' @return logical
#' @noRd
#' @author Matthew L. Fidler
.rxGpuHasJac <- function(obj) {
  .f <- .rxGpuModelFile(obj)
  file.exists(.f) && any(readLines(.f, warn = FALSE) == "#define RXG_HAS_JAC 1")
}

#' The model with its analytic state Jacobian for the device integrators
#'
#' ros54p (alone or as the stiff half of a composite) uses the model's
#' `df(state)/dy(state)` lines, generated symbolically as for every
#' Rosenbrock method in rxode2 (`.rxJacStateModel()`, shared with
#' `rxSolve()`'s cache).
#'
#' @param obj rxode2 model
#' @return the model with the Jacobian, or `NULL` when it cannot be
#'   generated as device code
#' @noRd
#' @author Matthew L. Fidler
.rxGpuJacModel <- function(obj) {
  if (.rxGpuHasJac(obj)) {
    return(obj)
  }
  .key <- paste0(rxModelVars(obj)$md5["parsed_md5"], "_jac")
  .c <- .rxSolveCacheGet(.key)
  if (is.null(.c) || !.rxJacCacheOk(.c)) {
    .c <- tryCatch(
      {
        ## symengine can warn while it differentiates special functions
        .e <- suppressWarnings(.rxJacStateModel(obj))
        .e$md5 <- rxModelVars(.e$obj)$md5["parsed_md5"]
        .e
      },
      error = function(e) NA_character_
    )
    .rxSolveCacheSet(.key, .c)
  }
  if (identical(.c, NA_character_) || !.rxGpuSafe(.c$obj) || !.rxGpuHasJac(.c$obj)) {
    return(NULL)
  }
  .c$obj
}

#' Model whose device code a ros54p method compiles
#'
#' @param obj rxode2 model
#' @param method integrator name
#' @return model (with the Jacobian for ros54p methods)
#' @noRd
#' @author Matthew L. Fidler
.rxGpuMethodModel <- function(obj, method) {
  if (!grepl("ros54p", method, fixed = TRUE)) {
    return(obj)
  }
  .j <- .rxGpuJacModel(obj)
  if (is.null(.j)) {
    stop("'", method, "' needs the model's analytic Jacobian, which cannot be generated as device code", call. = FALSE)
  }
  .j
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
  ## every method is built, so the Jacobian is added when it can be (ros54p)
  if (.rxGpuSafe(obj)) {
    .j <- .rxGpuJacModel(obj)
    if (!is.null(.j)) obj <- .j
  }
  ## the model, its defines and the headers name the library: look it up
  ## without rebuilding the source
  .fast <- paste0("fast_", digest::digest(c(rxModelVars(obj)$md5, .rxGpuDefines(...), .rxGpuHdrMd5())))
  .ret <- .rxGpuHostEnv[[.fast]]
  if (!is.null(.ret)) {
    return(.ret)
  }
  .s <- .rxGpuModelSource(obj)
  .src <- c(
    .rxGpuDefines(...),
    sprintf("#include \"%s\"", normalizePath(.s$file, winslash = "/")),
    "#include \"rxode2gpu/rxg_host.h\""
  )
  .hdr <- unlist(lapply(
    c("rxg_prelude.h", "rxg_special.h", "rxg_model.h", "rxg_stepper.h", "rxg_round.h", "rxg_host.h"),
    function(.h) {
      readLines(file.path(.rxGpuIncludeDir(), "rxode2gpu", .h), warn = FALSE)
    }
  ))
  .md5 <- digest::digest(c(.src, .s$source, .hdr))
  .ret <- .rxGpuHostEnv[[.md5]]
  if (!is.null(.ret)) {
    assign(.fast, .ret, envir = .rxGpuHostEnv)
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
    writeLines(
      c(
        sprintf("PKG_CPPFLAGS=-I\"%s\"", .rxGpuIncludeDir()),
        "PKG_CFLAGS=$(SHLIB_OPENMP_CFLAGS)",
        "PKG_LIBS=$(SHLIB_OPENMP_CFLAGS)"
      ),
      .mv
    )
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
  .dim <- do.call(.C, list(.fn, d = integer(14)))$d
  .ret <- list(
    dll = .info,
    neq = .dim[1],
    nlhs = .dim[2],
    npar = .dim[3],
    hasJac = as.logical(.dim[4]),
    hasDelay = as.logical(.dim[5]),
    ## the round layout (rxode2gpu/rxg_round.h)
    layout = stats::setNames(.dim[6:14], c("hrec", "hhdr", "rd", "ri", "ad", "ai", "od", "oi", "ncnt"))
  )
  assign(.md5, .ret, envir = .rxGpuHostEnv)
  assign(.fast, .ret, envir = .rxGpuHostEnv)
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

## rxSolve() method codes of the device integrators and their stepper names
.rxGpuSolveMethods <- c(
  "110" = "rkt54",
  "111" = "vern76",
  "112" = "ros54p",
  "113" = "rkt54+ros54p",
  "114" = "vern76+ros54p"
)
## Host counterpart of each GPU method: name = list(method, stiff2)
.rxGpuHostMethod <- list(
  "110" = list(t54 = 68L, 0L),
  "111" = list(vern76 = 28L, 0L),
  "112" = list(ros54p = 108L, 0L),
  "113" = list("t54+ros54p" = 68L, 108L),
  "114" = list("vern76+ros54p" = 28L, 108L)
)
## AutoSwitch composites that are single rxSolve() methods (they switch on
## the device, not through rxControl(stiff2=))
.rxGpuSolveComposites <- c("gpuT54+gpuRos54p", "gpuVern76+gpuRos54p")

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

## OpenCL runtime.  R builds the context, kernel and buffers once per solve
## (through the OpenCL package); each round runs in C (src/gpu_opencl.c), or
## on the host with rxg_host_round() for the CPU backend.

#' OpenCL devices available to the GPU solvers
#'
#' Lists the OpenCL devices the suggested `OpenCL` package can reach and
#' whether each supports double precision (`cl_khr_fp64`), which the GPU
#' solvers need unless they run in single precision.
#'
#' @return list with `available` (the `OpenCL` package is installed and
#'   rxode2 found the OpenCL library), `fp64` (some device has double
#'   precision) and `devices`, a data frame with one row per device: its
#'   `index`, `platform`, `device` name, `type` ("gpu", "cpu" or "other") and
#'   `fp64`.
#' @family Solver options and performance
#' @author Matthew L. Fidler
#' @export
#' @examples
#' rxGpuInfo()$available
rxGpuInfo <- function() {
  .devs <- .rxGpuDevices()
  .df <- data.frame(
    index = seq_along(.devs$devices),
    platform = vapply(.devs$devices, function(x) x$platform, character(1)),
    device = vapply(.devs$devices, function(x) x$name, character(1)),
    type = vapply(.devs$devices, function(x) x$type, character(1)),
    fp64 = vapply(.devs$devices, function(x) x$fp64, logical(1)),
    stringsAsFactors = FALSE
  )
  list(available = .devs$available, fp64 = any(.df$fp64), devices = .df)
}

#' OpenCL devices with their properties
#'
#' @return list with `available` and `devices` (each a list with the
#'   `OpenCL` device object `dev`, `platform`, `name`, `type` and `fp64`)
#' @noRd
#' @author Matthew L. Fidler
.rxGpuDevices <- function() {
  .none <- list(available = FALSE, devices = list())
  if (!requireNamespace("OpenCL", quietly = TRUE)) {
    return(.none)
  }
  .pls <- tryCatch(OpenCL::oclPlatforms(), error = function(e) list())
  .devs <- list()
  for (.pl in .pls) {
    .pn <- tryCatch(OpenCL::oclInfo(.pl)$name, error = function(e) "")
    .all <- tryCatch(OpenCL::oclDevices(.pl), error = function(e) list())
    .gpu <- tryCatch(OpenCL::oclDevices(.pl, "gpu"), error = function(e) list())
    .cpu <- tryCatch(OpenCL::oclDevices(.pl, "cpu"), error = function(e) list())
    .gpuN <- vapply(.gpu, function(d) OpenCL::oclInfo(d)$name, character(1))
    .cpuN <- vapply(.cpu, function(d) OpenCL::oclInfo(d)$name, character(1))
    for (.d in .all) {
      .i <- OpenCL::oclInfo(.d)
      .devs[[length(.devs) + 1L]] <- list(
        dev = .d,
        platform = .pn,
        name = .i$name,
        type = if (.i$name %in% .gpuN) {
          "gpu"
        } else if (.i$name %in% .cpuN) {
          "cpu"
        } else {
          "other"
        },
        fp64 = any(.i$exts == "cl_khr_fp64")
      )
    }
  }
  list(available = .Call(`_rxode2_rxGpuClAvailable`) && length(.devs) > 0L, devices = .devs)
}

#' Choose an OpenCL device
#'
#' @param device `NULL` (the first GPU that supports the precision, else the
#'   first other device that does) or an index into `rxGpuInfo()$devices`
#' @param precision "double" or "float"
#' @return list with the device index and object
#' @noRd
#' @author Matthew L. Fidler
.rxGpuDevice <- function(device = NULL, precision = c("double", "float")) {
  precision <- match.arg(precision)
  .devs <- .rxGpuDevices()
  if (!.devs$available) {
    stop("no OpenCL device is available (see rxGpuInfo())", call. = FALSE)
  }
  .ok <- vapply(.devs$devices, function(x) precision == "float" || x$fp64, logical(1))
  if (is.null(device)) {
    .gpu <- vapply(.devs$devices, function(x) x$type == "gpu", logical(1))
    .w <- c(which(.ok & .gpu), which(.ok & !.gpu))
    if (length(.w) == 0L) {
      stop("no OpenCL device supports double precision; use gpuPrecision=\"float\"", call. = FALSE)
    }
    device <- .w[1]
  }
  checkmate::assertIntegerish(device, lower = 1, upper = length(.devs$devices), len = 1, any.missing = FALSE)
  device <- as.integer(device)
  if (!.ok[device]) {
    stop("OpenCL device ", device, " does not support double precision", call. = FALSE)
  }
  list(index = device, dev = .devs$devices[[device]]$dev)
}

.rxGpuClEnv <- new.env(parent = emptyenv())

#' OpenCL context (cached per device and precision)
#'
#' @inheritParams .rxGpuDevice
#' @return the `OpenCL` context, with attribute `rxIndex` (the device index)
#' @noRd
#' @author Matthew L. Fidler
.rxGpuContext <- function(device = NULL, precision = c("double", "float")) {
  precision <- match.arg(precision)
  .d <- .rxGpuDevice(device, precision)
  .key <- paste0("ctx_", .d$index, "_", precision)
  .ctx <- .rxGpuClEnv[[.key]]
  if (is.null(.ctx)) {
    .ctx <- OpenCL::oclContext(.d$dev, precision = if (precision == "float") "single" else "double")
    assign(.key, .ctx, envir = .rxGpuClEnv)
  }
  attr(.ctx, "rxIndex") <- .d$index
  .ctx
}

#' Round kernel of a model for one integrator (cached)
#'
#' @param obj rxode2 model
#' @param method integrator name (`names(.rxGpuMethods)`)
#' @param ctx context from `.rxGpuContext()`
#' @param precision "double" or "float"
#' @param ... passed to `.rxGpuDefines()`
#' @return the `OpenCL` kernel
#' @noRd
#' @author Matthew L. Fidler
.rxGpuKernel <- function(obj, method, ctx, precision = c("double", "float"), ...) {
  precision <- match.arg(precision)
  method <- match.arg(method, names(.rxGpuMethods))
  .m <- .rxGpuMethods[method]
  obj <- .rxGpuMethodModel(obj, method)
  ## the model, the defines and the headers name the program
  .key <- paste0(
    "ker_",
    attr(ctx, "rxIndex"),
    "_",
    precision,
    "_",
    digest::digest(c(rxModelVars(obj)$md5, method, .rxGpuDefines(...), .rxGpuHdrMd5()))
  )
  .ker <- .rxGpuClEnv[[.key]]
  if (is.null(.ker)) {
    .src <- .rxGpuProgramSource(
      obj,
      ...,
      float = precision == "float",
      method = .m,
      extra = "#include \"rxode2gpu/rxg_round.h\""
    )
    .ker <- OpenCL::oclSimpleKernel(ctx, "rxg_kernel", .src, if (precision == "float") "single" else "double")
    attr(.ker, "rxKey") <- .key
    assign(.key, .ker, envir = .rxGpuClEnv)
  }
  .ker
}

#' md5 of the device headers (once a session)
#'
#' @return character
#' @noRd
#' @author Matthew L. Fidler
.rxGpuHdrMd5 <- function() {
  .h <- .rxGpuClEnv$hdrMd5
  if (is.null(.h)) {
    .f <- sort(list.files(file.path(.rxGpuIncludeDir(), "rxode2gpu"), full.names = TRUE))
    .h <- digest::digest(unname(tools::md5sum(.f)))
    assign("hdrMd5", .h, envir = .rxGpuClEnv)
  }
  .h
}

#' Shared integrator options of a round
#'
#' @inheritParams .rxGpuHostSolve
#' @param maxAtolRtolFactor cap on a request's loosened tolerances
#' @return list with the doubles `od` and integers `oi` (rxg_round.h)
#' @noRd
#' @author Matthew L. Fidler
.rxGpuOptions <- function(
  lib,
  method,
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
  histCap = 5000L,
  maxAtolRtolFactor = Inf
) {
  .n <- max(1L, lib$neq)
  .m <- .rxGpuMethods[match.arg(method, names(.rxGpuMethods))]
  ## as rxSolve() fills its tolerances: the given values, then the first
  .tol <- function(x) {
    .v <- rep(x[1], .n)
    .k <- min(.n, length(x))
    .v[seq_len(.k)] <- x[seq_len(.k)]
    .v
  }
  if (is.null(maxAtolRtolFactor)) {
    maxAtolRtolFactor <- Inf
  }
  list(
    od = as.double(c(
      .tol(atol),
      .tol(rtol),
      hmin,
      hmax,
      autoSwitchNonstifftol,
      autoSwitchStifftol,
      maxAtolRtolFactor
    )),
    oi = as.integer(c(
      .m,
      mxstep,
      autoSwitchMaxStiff,
      autoSwitchMaxNonstiff,
      autoSwitchSwitchMax,
      autoSwitchStiffFirst,
      if (lib$hasDelay) histCap else 0L
    ))
  )
}

#' Set up batched rounds of a device integrator
#'
#' @param obj rxode2 model
#' @param method integrator name (`names(.rxGpuMethods)`)
#' @param nsub subjects (the most requests in a round)
#' @param backend "opencl" or "cpu" (the same code built as host C, run on
#'   `cores` OpenMP threads)
#' @param precision "double" or "float" (OpenCL only)
#' @param device OpenCL device (see `.rxGpuDevice()`)
#' @param cores OpenMP threads of the CPU backend
#' @param ... options for `.rxGpuOptions()`, or `.rxGpuDefines()`
#' @return run object for `.rxGpuRound()`
#' @noRd
#' @author Matthew L. Fidler
.rxGpuRun <- function(
  obj,
  method,
  nsub,
  backend = c("opencl", "cpu"),
  precision = c("double", "float"),
  device = NULL,
  cores = 1L,
  reuse = FALSE,
  ...
) {
  backend <- match.arg(backend)
  precision <- match.arg(precision)
  method <- match.arg(method, names(.rxGpuMethods))
  checkmate::assertIntegerish(nsub, lower = 1, len = 1, any.missing = FALSE)
  obj <- .rxGpuMethodModel(obj, method)
  .args <- list(...)
  .defN <- intersect(names(.args), c("safeLog", "safeZero", "safePow", "useStdPow"))
  .lib <- do.call(.rxGpuHostLib, c(list(obj), .args[.defN]))
  .opt <- do.call(.rxGpuOptions, c(list(.lib, method), .args[setdiff(names(.args), .defN)]))
  .lay <- .lib$layout
  .hcap <- .opt$oi[7]
  .dims <- as.integer(c(
    nsub,
    max(1L, .lib$neq),
    max(1L, .lib$npar),
    max(1L, .lib$nlhs),
    .lib$hasDelay,
    .lay["hrec"],
    .hcap,
    .lay[c("hhdr", "rd", "ri", "ad", "ai", "od", "oi")]
  ))
  .ret <- list(lib = .lib, method = method, backend = backend, nsub = as.integer(nsub), dims = .dims, opt = .opt)
  if (backend == "cpu") {
    .st <- new.env(parent = emptyenv())
    .st$ad <- double(nsub * .lay["ad"])
    .st$ai <- integer(nsub * .lay["ai"])
    .st$hs <- double(nsub * .lay["hhdr"])
    .st$hrec <- double(if (.lib$hasDelay) nsub * .hcap * .lay["hrec"] else 1)
    .ret$state <- .st
    .ret$cores <- as.integer(cores)
    return(.ret)
  }
  .ctx <- .rxGpuContext(device, precision)
  .ker <- do.call(.rxGpuKernel, c(list(obj, method, .ctx, precision), .args[.defN]))
  ## a solve reuses the buffers of the last run of the same kernel, layout
  ## and options (every subject's first request restarts its state)
  ## one cached run per kernel and layout: other options replace (and
  ## release) it
  .runKey <- if (reuse) paste0("run_", digest::digest(list(attr(.ker, "rxKey"), .dims))) else NULL
  .optKey <- digest::digest(.opt)
  if (!is.null(.runKey)) {
    .old <- .rxGpuClEnv[[.runKey]]
    if (!is.null(.old) && !identical(.old$ptr, methods::new("externalptr"))) {
      if (identical(.old$opt, .optKey)) {
        .ret$ptr <- .old$ptr
        .ret$fin <- .old$fin
        return(.ret)
      }
      .rxGpuRunRelease(.old$fin)
    }
  }
  .ptr <- .Call(
    `_rxode2_rxGpuClNew`,
    .ctx,
    attr(.ctx, "queue"),
    .ker,
    .dims,
    .opt$od,
    .opt$oi,
    precision == "float",
    list(.ctx, .ker)
  )
  ## released from R while rxode2's DLL is loaded, never by a finalizer in
  ## the DLL (which would crash R after the DLL is unloaded)
  .fin <- new.env(parent = emptyenv())
  .fin$ptr <- .ptr
  reg.finalizer(.fin, .rxGpuRunRelease, onexit = TRUE)
  .ret$ptr <- .ptr
  .ret$fin <- .fin
  if (!is.null(.runKey)) {
    assign(.runKey, list(ptr = .ptr, fin = .fin, opt = .optKey), envir = .rxGpuClEnv)
  }
  .ret
}

#' Release the device buffers of an OpenCL run (its finalizer)
#'
#' @param e environment holding the run's external pointer `ptr`
#' @return `NULL`, invisibly
#' @noRd
#' @author Matthew L. Fidler
.rxGpuRunRelease <- function(e) {
  if (is.loaded("_rxode2_rxGpuClFree", PACKAGE = "rxode2")) {
    .Call("_rxode2_rxGpuClFree", e$ptr, PACKAGE = "rxode2")
  }
  invisible(NULL)
}

#' Requests of one round, in the layout of rxg_round.h
#'
#' Rows of the matrices are requests.
#'
#' @param run from `.rxGpuRun()`
#' @param sub subject of each request (1-based; at most one request each)
#' @param t0,t1 intervals
#' @param y states (matrix)
#' @param p0,p1 parameters at `t0` and `t1` (matrices)
#' @param h starting step sizes (0: chosen)
#' @param init reset the subject's AutoSwitch state and restart its delay()
#'   history at `t0` (first interval of a subject)
#' @inheritParams .rxGpuHostEval
#' @param tolFactor each subject's tolerance factor
#' @return list of flat request vectors
#' @noRd
#' @author Matthew L. Fidler
.rxGpuRequests <- function(
  run,
  sub,
  t0,
  t1,
  y,
  p0,
  p1 = p0,
  h = 0,
  init = FALSE,
  pl = NULL,
  ir = NULL,
  on = NULL,
  curShift = 0,
  tpk = t0,
  tbs = c(1, 2, 1, 0),
  covType = 0L,
  nonmem = FALSE,
  tolFactor = 1
) {
  checkmate::assertIntegerish(sub, lower = 1, upper = run$nsub, any.missing = FALSE, min.len = 1, unique = TRUE)
  .lib <- run$lib
  .n <- length(sub)
  .w <- function(x, k, w, fill) {
    .m <- matrix(fill, .n, w)
    if (k > 0L && !is.null(x)) {
      .m[, seq_len(k)] <- matrix(as.double(x), .n, k)
    }
    as.vector(t(.m))
  }
  .lay <- run$lib$layout
  .rd <- cbind(
    t0,
    t1,
    h,
    curShift,
    tpk,
    matrix(tbs, .n, 4, byrow = TRUE),
    rep_len(if (is.null(tolFactor)) 1 else as.double(tolFactor), .n)
  )
  .init <- ifelse(init, 3L, 0L)
  .ri <- cbind(as.integer(sub) - 1L, covType, as.integer(nonmem), .init, matrix(0L, .n, .lay["ri"] - 4L))
  list(
    n = .n,
    y = .w(y, .lib$neq, max(1L, .lib$neq), 0),
    rd = as.double(t(.rd)),
    ri = as.integer(t(.ri)),
    p0 = .w(p0, .lib$npar, max(1L, .lib$npar), 0),
    p1 = .w(p1, .lib$npar, max(1L, .lib$npar), 0),
    pl = .w(pl, .lib$nlhs, max(1L, .lib$nlhs), NA_real_),
    ir = .w(ir, .lib$neq, max(1L, .lib$neq), 0),
    on = as.integer(.w(if (is.null(on)) 1L else on, .lib$neq, max(1L, .lib$neq), 1))
  )
}

#' Run one round
#'
#' The solver runs its rounds in C; this entry serves the tests.
#'
#' @param run from `.rxGpuRun()`
#' @param req from `.rxGpuRequests()`
#' @return list with the states `y` (matrix), next step sizes `h`, `istate`
#'   and counters `cnt` (matrix) of each request
#' @noRd
#' @author Matthew L. Fidler
.rxGpuRound <- function(run, req) {
  if (run$backend == "cpu") {
    .st <- run$state
    .r <- do.call(
      .C,
      list(
        getNativeSymbolInfo("rxg_host_round", run$lib$dll),
        as.integer(req$n),
        as.integer(run$cores),
        y = req$y,
        rd = req$rd,
        ri = req$ri,
        req$p0,
        req$p1,
        req$pl,
        req$ir,
        req$on,
        ad = .st$ad,
        ai = .st$ai,
        hs = .st$hs,
        hrec = .st$hrec,
        run$opt$od,
        run$opt$oi,
        NAOK = TRUE
      )
    )
    .st$ad <- .r$ad
    .st$ai <- .r$ai
    .st$hs <- .r$hs
    .st$hrec <- .r$hrec
  } else {
    .r <- .Call(`_rxode2_rxGpuClRound`, run$ptr, req$n, req$y, req$rd, req$ri, req$p0, req$p1, req$pl, req$ir, req$on)
  }
  .lay <- run$lib$layout
  .rd <- matrix(.r$rd, req$n, .lay["rd"], byrow = TRUE)
  .ri <- matrix(.r$ri, req$n, .lay["ri"], byrow = TRUE)
  .cnt <- .ri[, 5L + seq_len(.lay["ncnt"]), drop = FALSE]
  colnames(.cnt) <- c("step", "accept", "reject", "fev", "jac", "lu")
  list(
    y = matrix(.r$y, req$n, max(1L, run$lib$neq), byrow = TRUE)[, seq_len(run$lib$neq), drop = FALSE],
    h = .rd[, 3L],
    istate = .ri[, 5L],
    cnt = .cnt
  )
}

## rxSolve() with a GPU method (codes 110-114).  R sets the solve up once:
## the model's host library (and the OpenCL run), installed for C by
## _rxode2_rxGpuSolveSet(); src/gpu_solve.cpp runs every round.

#' Model a GPU rxSolve() method solves
#'
#' @param object rxode2 model
#' @param method rxSolve() method code (110-114)
#' @return the model, with its analytic Jacobian for the ros54p methods
#' @noRd
#' @author Matthew L. Fidler
.rxGpuSolveModel <- function(object, method) {
  if (!.rxGpuSafe(object)) {
    stop(
      "the GPU methods need a model whose d/dt() can run as device code; ",
      "this one cannot (see rxGpuInfo()); use a host method such as \"t54\", \"vern76\" or \"ros54p\"",
      call. = FALSE
    )
  }
  .rxGpuMethodModel(object, .rxGpuSolveMethods[[as.character(method)]])
}

.rxGpuSolveEnv <- new.env(parent = emptyenv())

#' Do two models share their parameter, state and lhs order?
#'
#' @param a,b `rxModelVars()` of each
#' @return logical
#' @noRd
#' @author Matthew L. Fidler
.rxGpuSameLayout <- function(a, b) {
  identical(a$params, b$params) && identical(a$state, b$state) && identical(a$lhs, b$lhs)
}

#' Why a GPU method cannot solve this problem (beyond device code)
#'
#' @param object rxode2 model
#' @param ctl rxControl()
#' @param events rxSolve() events
#' @return `NULL`, or the reason
#' @noRd
#' @author Matthew L. Fidler
.rxGpuSolveUnsupported <- function(object, ctl, events = NULL) {
  ## the output step reads delay() from the host history, which a GPU solve
  ## does not keep
  .lines <- strsplit(rxNorm(object), "\n", fixed = TRUE)[[1]]
  if (any(grepl("delay(", .lines[!grepl("^d/dt\\(", .lines)], fixed = TRUE))) {
    return("delay() outside d/dt()")
  }
  ## steady state integrates each dosing interval on its own, which would
  ## restart the delay() history
  if (isTRUE(rxModelVars(object)$flags[["hasDelay"]] == 1L) && (is.data.frame(events) || is.rxEt(events))) {
    .e <- as.data.frame(events)
    .w <- which(tolower(names(.e)) == "ss")
    ## steady state is an ss column, or a classic evid whose last two digits
    ## are a steady-state flag (9, 10, 19, 20, 40)
    .v <- which(tolower(names(.e)) == "evid")
    .cl <- if (length(.v) > 0L) suppressWarnings(as.numeric(.e[[.v[1]]])) else numeric(0)
    if (
      (length(.w) > 0L && any(.e[[.w[1]]] %in% 1:2)) ||
        any(!is.na(.cl) & .cl >= 100 & (.cl %% 100) %in% c(9, 10, 19, 20, 40))
    ) {
      return("steady state in delay() models")
    }
  }
  ## the host build of the device code comes from the model with its
  ## Jacobian, which has to keep the parameter, state and lhs order
  if (.rxGpuSafe(object)) {
    .j <- .rxGpuJacModel(object)
    if (!is.null(.j) && !.rxGpuSameLayout(rxModelVars(object), rxModelVars(.j))) {
      return("a model whose Jacobian reorders its variables")
    }
  }
  ## the device code does not call registered dydt forcing hooks
  if (.Call(`_rxode2_rxDydtForceN`) > 0L) {
    return("registered dydt forcing (rxRegisterDydtForce())")
  }
  ## a resampled covariate may change inside an interval
  if (!is.null(ctl$resample) && !isFALSE(ctl$resample)) {
    return("resampled covariates")
  }
  NULL
}

#' Install the GPU solve of the next rxSolve()
#'
#' @param object rxode2 model (with its Jacobian when needed)
#' @param ctl rxControl()
#' @param nsolve subjects times simulations, an upper bound
#' @return `ctl` with `cores` set to the batch (the slots of
#'   src/gpu_solve.cpp)
#' @noRd
#' @author Matthew L. Fidler
.rxGpuSolveInstall <- function(object, ctl, nsolve, key = rxModelVars(object)$trans[["model_vars"]]) {
  ## the solve setup claims it for this model and method only
  key <- paste0(key, ":", as.integer(ctl$method))
  .meth <- .rxGpuSolveMethods[[as.character(ctl$method)]]
  .threads <- as.integer(ctl$cores)
  if (length(.threads) != 1L || is.na(.threads) || .threads < 1L) {
    .threads <- getRxThreads()
  }
  ## each round opens short parallel regions; a team on every logical CPU
  ## (hyperthreads included) spins at their barriers, so it is capped at
  ## rxode2's thread count
  .threads <- as.integer(max(1L, min(.threads, getRxThreads())))
  .batch <- as.integer(max(1L, min(ctl$gpuBatch, nsolve)))
  .backend <- ctl$gpuBackend
  if (.backend == "auto") {
    ## options(rxode2.gpuBackend = "cpu") pins what "auto" picks
    .backend <- match.arg(getOption("rxode2.gpuBackend", "auto"), c("auto", "opencl", "cpu"))
  }
  .dev <- ctl$gpuDevice
  if (.backend != "cpu") {
    .avail <- rxGpuInfo()
    .fp <- .avail$devices$fp64
    if (!is.null(.dev)) {
      .fp <- .fp[.avail$devices$index == .dev]
    }
    .ok <- .avail$available &&
      length(.fp) > 0L &&
      (ctl$gpuPrecision == "float" || any(.fp))
    if (!.ok) {
      if (.backend == "opencl") {
        stop("no OpenCL device with ", ctl$gpuPrecision, " precision is available (see rxGpuInfo())", call. = FALSE)
      }
      .backend <- "cpu"
    } else {
      .backend <- "opencl"
    }
  }
  .hmax <- ctl$hmax
  if (is.null(.hmax) || all(is.na(.hmax))) {
    .hmax <- 0
  }
  .run <- .rxGpuRun(
    object,
    .meth,
    .batch,
    backend = .backend,
    precision = ctl$gpuPrecision,
    device = .dev,
    cores = .threads,
    reuse = TRUE,
    safeLog = as.integer(ctl$safeLog),
    safeZero = as.integer(ctl$safeZero),
    safePow = as.integer(ctl$safePow),
    useStdPow = as.integer(ctl$useStdPow),
    atol = ctl$atol,
    rtol = ctl$rtol,
    hmin = if (is.null(ctl$hmin)) 0 else ctl$hmin,
    hmax = .hmax,
    mxstep = as.integer(ctl$maxsteps),
    autoSwitchNonstifftol = ctl$autoSwitchNonstifftol,
    autoSwitchStifftol = ctl$autoSwitchStifftol,
    autoSwitchMaxStiff = ctl$autoSwitchMaxStiff,
    autoSwitchMaxNonstiff = ctl$autoSwitchMaxNonstiff,
    autoSwitchSwitchMax = ctl$autoSwitchSwitchMax,
    autoSwitchStiffFirst = isTRUE(as.logical(ctl$autoSwitchStiffFirst)),
    maxAtolRtolFactor = ctl$maxAtolRtolFactor
  )
  .round <- getNativeSymbolInfo("rxg_host_round", .run$lib$dll)$address
  .Call(
    `_rxode2_rxGpuSolveSet`,
    .round,
    .run$ptr,
    .run$dims,
    .run$opt$od,
    .run$opt$oi,
    .threads,
    key
  )
  ## the run (and its OpenCL buffers) lives until the solve is cleared
  .rxGpuSolveEnv$run <- .run
  ## the per-thread pools serve both the slots and the output step's threads
  ctl$cores <- max(.batch, .threads)
  ctl
}

#' Clear the installed GPU solve
#'
#' @return `NULL`, invisibly
#' @noRd
#' @author Matthew L. Fidler
.rxGpuSolveClear <- function() {
  .Call(`_rxode2_rxGpuSolveSet`, NULL, NULL, NULL, NULL, NULL, NULL, NULL)
  .rxGpuSolveEnv$run <- NULL
  invisible(NULL)
}

#' Install the GPU setup for a solve set up from C
#'
#' rxSolve_() calls this once, at setup, for a GPU method whose model has no
#' GPU setup installed by rxSolve() (nlmixr2est sets its solves up from C).
#' The device code may come from the model with its Jacobian, which must keep
#' the solved model's parameter, state and lhs order.
#'
#' @param object the model being solved
#' @param ctl rxControl()
#' @param params,events the solve's inputs
#' @return `TRUE` when installed; `FALSE` (the host counterpart solves) when
#'   the model cannot run as device code or anything fails
#' @noRd
#' @author Matthew L. Fidler
.rxGpuSolveInstallC <- function(object, ctl, params, events) {
  tryCatch(
    {
      if (is.null(events) && rxIs(params, "event.data.frame")) {
        events <- params
      }
      if (!.rxGpuSafe(object) || !is.null(.rxGpuSolveUnsupported(object, ctl, events))) {
        return(FALSE)
      }
      .g <- suppressMessages(.rxGpuSolveModel(object, ctl$method))
      .mv <- rxModelVars(object)
      if (!.rxGpuSameLayout(.mv, rxModelVars(.g))) {
        return(FALSE)
      }
      suppressMessages(.rxGpuSolveInstall(
        .g,
        ctl,
        .rxGpuSolveCount(params, events, ctl),
        key = .mv$trans[["model_vars"]]
      ))
      TRUE
    },
    error = function(e) FALSE
  )
}

#' An upper bound of the subject solves of an rxSolve() call
#'
#' @param params,events rxSolve() inputs
#' @param ctl rxControl()
#' @return integer
#' @noRd
#' @author Matthew L. Fidler
.rxGpuSolveCount <- function(params, events, ctl) {
  .ids <- function(x) {
    if (!inherits(x, "data.frame")) {
      return(1L)
    }
    .w <- which(tolower(names(x)) == "id")
    if (length(.w) == 0L) {
      return(1L)
    }
    ## `[[` of a large et() table is empty; `$` reads its ids
    .id <- if (inherits(x, "rxEt")) x$id else x[[.w[1]]]
    max(1L, length(unique(.id)))
  }
  .n <- .ids(events)
  if (inherits(params, "data.frame") || is.matrix(params)) {
    .n <- max(.n, NROW(params))
  }
  .n <- .n * max(1L, as.integer(ctl$nSub)) * max(1L, as.integer(ctl$nStud))
  if (!is.null(ctl$nsim)) {
    .n <- .n * max(1L, as.integer(ctl$nsim))
  }
  as.integer(min(.n, .Machine$integer.max))
}
