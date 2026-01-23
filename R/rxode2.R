rex::register_shortcuts("rxode2")
# Hack for Rcpp->R initial values problem
R_NegInf <- -Inf # nolint
R_PosInf <- Inf # nolint
NA_LOGICAL <- NA # nolint

.linCmtSens <- NULL
.clearME <- function() {
  assignInMyNamespace(".rxMECode", "")
  assignInMyNamespace(".indLinInfo", list())
}
.rxFullPrint <- FALSE
#' Create an ODE-based model specification
#'
#' Create a dynamic ODE-based model object suitably for translation
#' into fast C code
#'
#' @param model This is the ODE model specification.  It can be:
#'
#'
#'  * a string containing the set of ordinary differential
#'     equations (ODE) and other expressions defining the changes in
#'     the dynamic system.
#'
#'  * a file name where the ODE system equation is contained
#'
#'   An ODE expression enclosed in `\{\}`
#'
#' (see also the `filename` argument). For
#'     details, see the sections \dQuote{Details} and
#'     `rxode2 Syntax` below.
#'
#' @param modName a string to be used as the model name. This string
#'     is used for naming various aspects of the computations,
#'     including generating C symbol names, dynamic libraries,
#'     etc. Therefore, it is necessary that `modName` consists of
#'     simple ASCII alphanumeric characters starting with a letter.
#'
#' @param wd character string with a working directory where to
#'     create a subdirectory according to `modName`. When
#'     specified, a subdirectory named after the
#'     \dQuote{`modName.d`} will be created and populated with a
#'     C file, a dynamic loading library, plus various other working
#'     files. If missing, the files are created (and removed) in the
#'     temporary directory, and the rxode2 DLL for the model is
#'     created in the current directory named `rx_????_platform`, for
#'     example `rx_129f8f97fb94a87ca49ca8dafe691e1e_i386.dll`
#'
#' @param filename A file name or connection object where the
#'     ODE-based model specification resides. Only one of `model`
#'     or `filename` may be specified.
#'
#' @param extraC  Extra c code to include in the model.  This can be
#'     useful to specify functions in the model.  These C functions
#'     should usually take `double` precision arguments, and
#'     return `double` precision values.
#'
#' @param debug is a boolean indicating if the executable should be
#'     compiled with verbose debugging information turned on.
#'
#' @param calcSens boolean indicating if rxode2 will calculate the
#'     sensitivities according to the specified ODEs.
#'
#' @param calcJac boolean indicating if rxode2 will calculate the
#'     Jacobain according to the specified ODEs.
#'
#' @param collapseModel boolean indicating if rxode2 will remove all
#'     LHS variables when calculating sensitivities.
#'
#' @param package Package name for pre-compiled binaries.
#'
#' @param ... ignored arguments.
#'
#' @param linCmtSens The method to calculate the linCmt() solutions
#'
#' @param indLin Calculate inductive linearization matrices and
#'     compile with inductive linearization support.
#'
#' @param verbose When `TRUE` be verbose with the linear
#'   compartmental model
#'
#' @param fullPrint When using `printf` within the model, if `TRUE`
#'   print on every step (except ME/indLin), otherwise when `FALSE`
#'   print only when calculating the `d/dt`
#'
#' @inheritParams rxode2parse
#'
#' @details
#'
#' The `Rx` in the name `rxode2` is meant to suggest the
#' abbreviation *Rx* for a medical prescription, and thus to
#' suggest the package emphasis on pharmacometrics modeling, including
#' pharmacokinetics (PK), pharmacodynamics (PD), disease progression,
#' drug-disease modeling, etc.
#'
#' @section Creating rxode2 models
#'
#' @includeRmd man/rmdhunks/rxode2-create-models.Rmd
#'
#' @includeRmd man/rmdhunks/rxode2-syntax-hunk.Rmd
#'
#' @return An object (environment) of class `rxode2` (see Chambers and Temple Lang (2001))
#'      consisting of the following list of strings and functions:
#'
#'     * `model` a character string holding the source model specification.
#'     * `get.modelVars`a function that returns a list with 3 character
#'         vectors, `params`, `state`, and `lhs` of variable names used in the model
#'         specification. These will be output when the model is computed (i.e., the ODE solved by integration).
#'
#'       * `solve`{this function solves (integrates) the ODE. This
#'           is done by passing the code to [rxSolve()].
#'           This is as if you called `rxSolve(rxode2object, ...)`,
#'           but returns a matrix instead of a rxSolve object.
#'
#'           `params`: a numeric named vector with values for every parameter
#'           in the ODE system; the names must correspond to the parameter
#'           identifiers used in the ODE specification;
#'
#'           `events`: an `eventTable` object describing the
#'           input (e.g., doses) to the dynamic system and observation
#'           sampling time points (see  [eventTable()]);
#'
#'           `inits`: a vector of initial values of the state variables
#'           (e.g., amounts in each compartment), and the order in this vector
#'           must be the same as the state variables (e.g., PK/PD compartments);
#'
#'
#'           `stiff`: a logical (`TRUE` by default) indicating whether
#'           the ODE system is stiff or not.
#'
#'           For stiff ODE systems (`stiff = TRUE`), `rxode2` uses
#'           the LSODA (Livermore Solver for Ordinary Differential Equations)
#'           Fortran package, which implements an automatic method switching
#'           for stiff and non-stiff problems along the integration interval,
#'           authored by Hindmarsh and Petzold (2003).
#'
#'           For non-stiff systems (`stiff = FALSE`), `rxode2` uses `DOP853`,
#'           an explicit Runge-Kutta method of order 8(5, 3) of Dormand and Prince
#'           as implemented in C by Hairer and Wanner (1993).
#'
#'           `trans_abs`: a logical (`FALSE` by default) indicating
#'           whether to fit a transit absorption term
#'           (TODO: need further documentation and example);
#'
#'           `atol`: a numeric absolute tolerance (1e-08 by default);
#'
#'           `rtol`: a numeric relative tolerance (1e-06 by default).
#'
#'           The output of \dQuote{solve} is a matrix with as many rows as there
#'           are sampled time points and as many columns as system variables
#'           (as defined by the ODEs and additional assignments in the rxode2 model
#'               code).}
#'
#'       * `isValid` a function that (naively) checks for model validity,
#'           namely that the C object code reflects the latest model
#'           specification.
#'       * `version` a string with the version of the `rxode2`
#'           object (not the package).
#'       * `dynLoad` a function with one `force = FALSE` argument
#'           that dynamically loads the object code if needed.
#'       * `dynUnload` a function with no argument that unloads
#'           the model object code.
#'       * `delete` removes all created model files, including C and DLL files.
#'           The model object is no longer valid and should be removed, e.g.,
#'           `rm(m1)`.
#'       * `run` deprecated, use `solve`.
#'       * `get.index` deprecated.
#'       * `getObj` internal (not user callable) function.
#'
#' @references
#'
#' Chamber, J. M. and Temple Lang, D. (2001)
#' *Object Oriented Programming in R*.
#' R News, Vol. 1, No. 3, September 2001.
#' <https://cran.r-project.org/doc/Rnews/Rnews_2001-3.pdf>.
#'
#' Hindmarsh, A. C.
#' *ODEPACK, A Systematized Collection of ODE Solvers*.
#' Scientific Computing, R. S. Stepleman et al. (Eds.),
#' North-Holland, Amsterdam, 1983, pp. 55-64.
#'
#' Petzold, L. R.
#' *Automatic Selection of Methods for Solving Stiff and Nonstiff
#' Systems of Ordinary Differential Equations*.
#' Siam J. Sci. Stat. Comput. 4 (1983), pp. 136-148.
#'
#' Hairer, E., Norsett, S. P., and Wanner, G.
#' *Solving ordinary differential equations I, nonstiff problems*.
#' 2nd edition, Springer Series in Computational Mathematics,
#' Springer-Verlag (1993).
#'
#' Plevyak, J.
#' *`dparser`*, <https://dparser.sourceforge.net/>. Web. 12 Oct. 2015.
#'
#' @author Melissa Hallow, Wenping Wang and Matthew Fidler
#'
#' @seealso [eventTable()], [et()], [add.sampling()], [add.dosing()]
#'
#' @examples
#' \donttest{
#'
#' mod <- function() {
#'   ini({
#'     KA   <- .291
#'     CL   <- 18.6
#'     V2   <- 40.2
#'     Q    <- 10.5
#'     V3   <- 297.0
#'     Kin  <- 1.0
#'     Kout <- 1.0
#'     EC50 <- 200.0
#'   })
#'   model({
#'     # A 4-compartment model, 3 PK and a PD (effect) compartment
#'     # (notice state variable names 'depot', 'centr', 'peri', 'eff')
#'     C2 <- centr/V2
#'     C3 <- peri/V3
#'     d/dt(depot) <- -KA*depot;
#'     d/dt(centr) <- KA*depot - CL*C2 - Q*C2 + Q*C3;
#'     d/dt(peri)  <-                    Q*C2 - Q*C3;
#'     d/dt(eff)   <- Kin - Kout*(1-C2/(EC50+C2))*eff;
#'     eff(0)      <- 1
#'   })
#' }
#'
#' m1 <- rxode2(mod)
#' print(m1)
#'
#' # Step 2 - Create the model input as an EventTable,
#' # including dosing and observation (sampling) events
#'
#' # QD (once daily) dosing for 5 days.
#'
#' qd <- et(amountUnits = "ug", timeUnits = "hours") |>
#'   et(amt = 10000, addl = 4, ii = 24)
#'
#' # Sample the system hourly during the first day, every 8 hours
#' # then after
#' qd <- qd |> et(0:24) |>
#'   et(from = 24 + 8, to = 5 * 24, by = 8)
#'
#' # Step 3 - solve the system
#'
#' qd.cp <- rxSolve(m1, qd)
#'
#' head(qd.cp)
#'
#' }
#'
#' @keywords models nonlinear
#' @concept Nonlinear regression
#' @concept ODE models
#' @concept Ordinary differential equations
#' @concept Pharmacokinetics (PK)
#' @concept Pharmacodynamics (PD)
#' @useDynLib rxode2, .registration=TRUE
#' @eval .rxodeBuildCode()
#' @importFrom PreciseSums fsum
#' @importFrom Rcpp evalCpp
#' @importFrom checkmate qassert
#' @importFrom utils getFromNamespace assignInMyNamespace download.file head sessionInfo compareVersion packageVersion
#' @importFrom stats setNames update dnorm integrate
#' @importFrom methods signature is
#' @importFrom memoise memoise is.memoised
#' @importFrom utils capture.output
#' @import tools
#' @import data.table
#' @export
rxode2 <- # nolint
  function(model, modName = basename(wd),
           wd = getwd(),
           filename = NULL, extraC = NULL, debug = FALSE, calcJac = NULL, calcSens = NULL,
           collapseModel = FALSE, package = NULL, ...,
           linCmtSens = c("linCmtA", "linCmtB"),
           indLin = FALSE,
           verbose = FALSE,
           fullPrint=getOption("rxode2.fullPrint", FALSE),
           envir=parent.frame()) {
    if (!missing(wd) && missing(modName)) {
      stop("working directory specified, but modName not declared, need to specify modName to create rxode2 c-files as a sub-directory of `wd`",
              call.=FALSE)
    }
    .udfEnvSet(envir)
    assignInMyNamespace(".rxFullPrint", fullPrint)
    rxSuppressMsg()
    rxParseSuppressMsg()
    .modelName <- try(as.character(substitute(model)), silent=TRUE)
    if (inherits(.modelName, "try-error")) .modelName <- NULL
    if (!missing(modName)) {
      if (!checkmate::testCharacter(modName, max.len = 1)) {
        stop("'modName' has to be a single length character", call. = FALSE)
      }
    }
    if (!missing(extraC)) {
      if (!checkmate::testAccess(extraC, "r")) {
        stop("'extraC' needs to point to a file that exists and is readable", call. = FALSE)
      }
    }
    if (!checkmate::checkLogical(collapseModel, max.len = 1, any.missing = FALSE)) {
      stop("'collapseModel' needs to be logical", call. = FALSE)
    }
    if (!checkmate::checkLogical(indLin, max.len = 1, any.missing = FALSE)) {
      stop("'indLin' needs to be logical", call. = FALSE)
    }
    if (!checkmate::checkLogical(debug, max.len = 1, any.missing = FALSE)) {
      stop("'debug' needs to be logical", call. = FALSE)
    }
    rxTempDir()
    if (!is.null(package)) {
      if (!checkmate::checkCharacter(package, max.len = 1, any.missing = FALSE)) {
        stop("'package' needs to a single character for the package name",
             call. = FALSE
             )
      }
      if (missing(modName)) {
        stop("with packages 'modName' is required",
             call. = FALSE
             )
      }
      modName <- paste0(package, "_", modName)
    }
    if (!missing(model) && !missing(filename)) {
      stop("must specify exactly one of 'model' or 'filename'",
           call. = FALSE
           )
    }
    if (missing(model) && !missing(filename)) {
      model <- filename
    }
    if (!missing(model) && missing(filename)) {
      if (is(substitute(model), "{")) {
        model <- deparse(substitute(model))
        if (model[1] == "{") {
          model <- model[-1]
          model <- model[-length(model)]
        }
        model <- paste(model, collapse = "\n")
      } else if (inherits(model, "rxUi")) {
        return(model)
      } else if (inherits(model, "function")) {
        .args <- as.list(match.call())[-1]
        if (length(.args) != 1L) {
          stop("model functions can only be called with one argument", call.=FALSE)
        }
        .tmp <- rxUiDecompress(.rxFunction2ui(model))
        assign("modelName", .modelName, envir=.tmp)
        return(rxUiCompress(.tmp))
      } else if (is(model, "rxode2")) {
        package <- get("package", model)
        if (!is.null(package)) {
          modName <- get("modName", model)
        }
        model <- model$.model
        class(model) <- NULL
      } else if ((is(model, "function") || is(model, "call"))) {
        model <- deparse(body(model))[-1]
        model <- paste(model[-length(model)], collapse = "\n")
      }
    }
    .env <- new.env(parent = baseenv())
    .env$.mv <- rxGetModel(model, calcSens = calcSens, calcJac = calcJac, collapseModel = collapseModel, indLin = indLin)
    assignInMyNamespace(".linCmtSens", linCmtSens)
    if (.Call(`_rxode2_isLinCmt`) == 1L) {
      .env$.linCmtM <- rxNorm(.env$.mv)
      .vars <- c(.env$.mv$params, .env$.mv$lhs, .env$.mv$slhs)
      .env$.mv <- rxGetModel(.Call(
        `_rxode2_linCmtGen`,
        length(.env$.mv$state),
        .vars,
        setNames(
          c(
            "linCmtA" = 1L, "linCmtB" = 2L
          )[match.arg(linCmtSens)],
          NULL
        ), verbose
      ))
    }
    model <- rxNorm(.env$.mv)
    class(model) <- "rxModelText"
    .env$.model <- model
    .env$missing.modName <- missing(modName)
    wd <- .normalizePath(wd, "/", mustWork = FALSE)
    if (.env$missing.modName) {
      if (rxode2.tempfiles) {
        .env$mdir <- suppressMessages(.normalizePath(rxTempDir(), mustWork = FALSE))
      } else {
        .env$mdir <- suppressMessages(.normalizePath(wd, mustWork = FALSE))
      }
    } else {
      .env$mdir <- suppressMessages(.normalizePath(file.path(wd, sprintf("%s.d", modName)), mustWork = FALSE))
    }

    if (!file.exists(wd)) {
      dir.create(wd, recursive = TRUE, showWarnings = FALSE)
    }

    .env$modName <- modName
    .env$model <- model
    .env$extraC <- extraC
    .env$debug <- debug
    .env$calcJac <- calcJac
    .env$calcSens <- calcSens
    .env$collapseModel <- collapseModel

    .env$wd <- wd
    .env$package <- package
    if (!is.null(.env$package)) {
      .env$mdir <- .rxPkgDir(.env)
    }
    .env$compile <- eval(bquote(function() {
      with(.(.env), {
        .rx <- base::loadNamespace("rxode2")
        if (!file.exists(wd)) {
          dir.create(wd, recursive = TRUE, showWarnings = FALSE)
        }
        on.exit({
          .rx$.clearME()
        })
        .rx$.rxWithWd(wd, {
          rxode2::.extraC(extraC)
          if (missing.modName) {
            .rxDll <- .rx$rxCompile(.mv,
                                    debug = debug,
                                    package = .(.env$package)
                                    )
          } else {
            .rxDll <- .rx$rxCompile(.mv,
                                    dir = mdir,
                                    debug = debug, modName = modName,
                                    package = .(.env$package)
                                    )
          }
          .rxDll$linCmtM <- .(ifelse(exists(".linCmtM", .env),
                                     get(".linCmtM", .env), NA
                                     ))
          assign("rxDll", .rxDll, envir = .(.env))
          assign(".mv", .rxDll$modVars, envir = .(.env))
        })
      })
    }))
    rxode2::.extraC(extraC)
    .env$compile()
    .env$get.modelVars <- eval(bquote(function() {
      with(.(.env), {
        .ret <- .mv[c("params", "state", "lhs")]
        .p <- .ret["params"]
        .ini <- names(.mv$.ini)
        .init <- rxode2::rxInit(rxDll)
        .ret$params <- .ret$params[!(.in(.ret$params, names(.init)))]
        class(.ret) <- "list"
        return(.ret)
      })
    }))
    .env$state <- .env$.mv$state
    if (.env$.mv$extraCmt == 1) {
      .extra <- c("central", .env$.mv$stateExtra)
    } else if (.env$.mv$extraCmt == 2) {
      .extra <- c("depot", "central", .env$.mv$stateExtra)
    } else {
      .extra <- .env$.mv$stateExtra
    }
    .env$stateExtra <- .extra
    .env$lhs <- .env$.mv$lhs
    .env$params <- .env$.mv$params
    .env$version <- rxode2::rxVersion()["version"]
    .env$solve <- eval(bquote(function(..., returnType= "matrix", object = NULL) {
      rxode2::rxSolve(object = get("rxDll", envir = .(.env)), ..., returnType = "matrix")
    }))
    .env$dll <- new.env(parent = baseenv())
    .env$assignPtr <- eval(bquote(function() {
      rxode2::rxAssignPtr(get("rxDll", envir = .(.env)))
    }))
    .env$run <- .env$solve
    .env$modName <- modName
    .env$model <- model # actual model code
    ## cmpMgr = cmpMgr,
    .env$dynLoad <- eval(bquote(function(force = FALSE) {
      rx <- .(.env)
      rxode2::rxDynLoad(rx)
    }))
    .env$load <- .env$dynLoad
    .env$dynUnload <- eval(bquote(function() {
      rx <- .(.env)
      rxode2::rxDynUnload(rx)
    }))
    .env$unload <- .env$dynUnload
    .pkgStuff <- FALSE
    if (!is.null(.env$package)) {
      if (regexpr("_new", .env$modName) == -1) {
        .pkgStuff <- TRUE
        .env$isValid <- eval(bquote(function() {
          if (!all(is.null(getLoadedDLLs()[[.(.env$package)]]))) {
            if (loadNamespace("rxode2")$.pkgModelCurrent &&
                                        utils::packageVersion("rxode2") == .(utils::packageVersion("rxode2"))) {
              return(TRUE)
            } else {
              return(FALSE)
            }
          } else {
            return(file.exists(rxode2::rxDll(get("rxDll", envir = .(.env)))))
          }
        }))
        .env$isLoaded <- eval(bquote(function() {
          if ((!all(is.null(getLoadedDLLs()[[.(.env$package)]]))) &&
                loadNamespace("rxode2")$.pkgModelCurrent &&
                                      utils::packageVersion("rxode2") == .(utils::packageVersion("rxode2"))) {
            return(TRUE)
          } else {
            rx <- .(.env)
            rxode2::rxIsLoaded(rx)
          }
        }))
        .env$delete <- eval(bquote(function() {
          if ((!all(is.null(getLoadedDLLs()[[.(.env$package)]]))) &&
                loadNamespace("rxode2")$.pkgModelCurrent &&
                                      utils::packageVersion("rxode2") == .(utils::packageVersion("rxode2"))) {
            stop("cannot delete Dll in package", call. = FALSE)
          } else {
            rx <- .(.env)
            rxode2::rxDelete(rx)
          }
        }))
      }
    }
    if (!.pkgStuff) {
      .env$isValid <- eval(bquote(function() {
        return(file.exists(rxode2::rxDll(get("rxDll", envir = .(.env)))))
      }))
      .env$isLoaded <- eval(bquote(function() {
        rx <- .(.env)
        rxode2::rxIsLoaded(rx)
      }))
      .env$delete <- eval(bquote(function() {
        rx <- .(.env)
        rxode2::rxDelete(rx)
      }))
    }

    .env$parse <- with(.env, function() {
      stop("'$parse' is no longer supported", call. = FALSE)
    })
    .env$get.index <- eval(bquote(function(s) {
      return(rxState(get("rxDll", envir = .(.env)), s))
    }))
    .mv <- .env$.mv
    .env$lib.name <- .mv$trans["lib.name"] # nolint
    tmp <- list(
      dllfile = rxode2::rxDll(.env$rxDll),
      ode_solver = as.vector(.mv$trans["ode_solver"]),
      ode_solver_ptr = as.vector(.mv$trans["ode_solver_ptr"]),
      prefix = as.vector(.mv$trans["prefix"]),
      model = model,
      isValid = eval(bquote(function() {
        with(.(.env), isValid())
      })),
      parse = eval(bquote(function() {
        with(.(.env), parse())
      })),
      compile = eval(bquote(function() {
        with(.(.env), compile())
      })),
      dynLoad = eval(bquote(function() {
        with(.(.env), dynLoad())
      })),
      dynUnload = eval(bquote(function() {
        with(.(.env), dynUnload())
      })),
      modelDir = .env$mdir, # model directory
      get.modelVars = eval(bquote(function() {
        with(.(.env), get.modelVars())
      })),
      delete = eval(bquote(function() {
        with(.(.env), delete())
      })),
      get.index = eval(bquote(function(...) {
        with(.(.env), get.index(...))
      })),
      .rxDll = .env$rxDll,
      rxDll = eval(bquote(function() {
        with(.(.env), return(rxDll))
      }))
    )
    tmp <- list2env(tmp, parent = .env)
    class(tmp) <- "RxCompilationManager"
    .env$cmpMgr <- tmp
    .env$calcJac <- (length(.mv$dfdy) > 0)
    .env$calcSens <- (length(.mv$sens) > 0)
    class(.env) <- "rxode2"
    rxode2::rxForget()
    if (!is.null(.env$package)) {
      .o <- rxDll(.env)
      .o <- paste0(substr(.o, 0, nchar(.o) - nchar(.Platform$dynlib.ext)), ".o")
      if (file.exists(.o)) {
        unlink(.o)
      }
      .make <- file.path(.env$mdir, "Makevars")
      if (file.exists(.make)) {
        unlink(.make)
      }
      if (.rxPkgLoaded(.env$package)) {
        .ns <- loadNamespace(.env$package)
        if (!exists(".rxUpdated", .ns)) {
          stop("cannot update package model", call. = FALSE)
        } else {
          .as <- .ns$.rxUpdated
          assign(.env$modName, .env)
        }
      }
    } else {
      rxode2::rxIsLoaded(.env) # Show this is loaded.
    }
    return(.env)
  }

#' @rdname rxode2
#' @export
RxODE <- rxode2

#' @rdname rxode2
#' @export
rxode <- rxode2

#' Get model properties without compiling it.
#'
#' @param model rxode2 specification
#' @inheritParams rxode2
#' @return rxode2 trans list
#' @author Matthew L. Fidler
#' @export
#' @keywords internal
rxGetModel <- function(model, calcSens = NULL, calcJac = NULL, collapseModel = NULL, indLin = FALSE) {
  if (is(substitute(model), "call")) {
    model <- model
  }
  if (is(substitute(model), "{")) {
    model <- deparse(substitute(model))
    if (model[1] == "{") {
      model <- model[-1]
      model <- model[-length(model)]
    }
    model <- paste(model, collapse = "\n")
  } else if (inherits(model, "function") || inherits(model, "call")) {
    model <- deparse(body(model))
    if (model[1] == "{") {
      model <- model[-1]
      model <- model[-length(model)]
    }
    model <- paste(model, collapse = "\n")
  } else if (inherits(model, "name")) {
    model <- eval(model)
  } else if (inherits(model, "character") || inherits(model, "rxModelText")) {
    model <- as.vector(model)
  } else if (inherits(model, "rxode2")) {
    model <- rxModelVars(model)
    ## class(model) <- NULL;
  } else if (inherits(model, "rxModelVars")) {
  } else if (inherits(model, "rxDll")) {
    model <- model$args$model
  } else {
    model <- rxModelVars(model)
    if (!inherits(model, "rxModelVars")) {
      stop("cannot figure out how to handle the model argument", call. = FALSE)
    }
  }
  .ret <- rxModelVars(model)
  if (!is.null(calcSens)) {
    .calcSens <- TRUE
    if (is(calcSens, "logical")) {
      if (!calcSens) {
        .calcSens <- FALSE
      }
    }
    if (.calcSens) {
      if (length(rxState(.ret)) == 0L) {
        stop("sensitivities do not make sense for models without ODEs", call. = FALSE)
      }
      .stateInfo <- .rxGenFunState(.ret)
      .s <- .rxLoadPrune(.ret, FALSE)
      .s$..stateInfo <- .stateInfo
      .rxJacobian(.s)
      if (!is(calcJac, "logical")) {
        calcJac <- FALSE
      }
      if (is.null(calcJac)) calcJac <- FALSE
      if (rxIs(calcSens, "logical")) {
        if (calcSens) {
          calcSens <- .rxParams(model, TRUE)
        }
      }
      .rxSens(.s, calcSens)
      .tmp1 <- .s$..jacobian
      if (!calcJac) .tmp1 <- ""
      .tmp2 <- .s$..lhs
      if (collapseModel) .tmp2 <- ""
      .new <- paste(c(
        .s$..stateInfo["state"],
        .s$..lhs0,
        .s$..ddt,
        .tmp1,
        .s$..sens,
        .tmp2,
        .s$..stateInfo["statef"],
        .s$..stateInfo["dvid"],
        ""
      ), collapse = "\n")
      .ret <- rxModelVars(.new)
    } else {
      ## calcSens=FALSE removes the sensitivity equations.
      .stateInfo <- .rxGenFunState(.ret)
      .s <- .rxLoadPrune(.ret, FALSE)
      .s$..stateInfo <- .stateInfo
      if (length(.ret$sens) != 0) {
        .new <- setNames(gsub(
          rex::rex("d/dt(", or(.ret$sens), ")=", anything, "\n"), "",
          .ret$model["normModel"]
        ), NULL)
        .ret <- rxModelVars(.new)
      }
      .calcJac <- FALSE
      if (!is.null(calcJac)) {
        if (is(calcJac, "logical")) {
          if (calcJac) {
            .calcJac <- TRUE
          }
        }
      }
      if (.calcJac) {
        .rxJacobian(.s)
        ## calcJac=TRUE, calcSens=FALSE
      }
      .tmp1 <- .s$..jacobian
      if (!.calcJac) .tmp1 <- ""
      .tmp2 <- .s$..lhs
      if (collapseModel) .tmp2 <- ""
      .new <- paste(c(
        .s$..stateInfo["state"],
        .s$..lhs0,
        .s$..ddt,
        .tmp1,
        .tmp2,
        .s$..stateInfo["statef"],
        .s$..stateInfo["dvid"],
        ""
      ), collapse = "\n")
      .ret <- rxModelVars(.new)
    }
  } else if (!is.null(calcJac)) {
    if (length(.ret$sens) != 0) {
      .new <- setNames(gsub(
        rex::rex("d/dt(", or(.ret$sens), ")=", anything, "\n"), "",
        .ret$model["normModel"]
      ), NULL)
      .ret <- rxModelVars(.new)
    }
    .calcJac <- TRUE
    if (is(calcJac, "logical")) {
      if (!calcJac) {
        .calcJac <- FALSE
      }
    }
    if (.calcJac) {
      if (length(rxState(.ret)) <= 0) {
        ## Jacobian capitalized because it should be spelled with a capital
        stop("Jacobians do not make sense for models without ODEs", call. = FALSE)
      }
      .stateInfo <- .rxGenFunState(.ret)
      .s <- .rxLoadPrune(.ret, FALSE)
      .s$..stateInfo <- .stateInfo
      .rxJacobian(.s)
      .tmp1 <- .s$..jacobian
      if (!.calcJac) .tmp1 <- ""
      .tmp2 <- .s$..lhs
      if (collapseModel) .tmp2 <- ""
      .new <- paste(c(
        .s$..stateInfo["state"],
        .s$..lhs0,
        .s$..ddt,
        .tmp1,
        .tmp2,
        .s$..stateInfo["statef"],
        .s$..stateInfo["dvid"],
        ""
      ), collapse = "\n")
      .ret <- rxModelVars(.new)
    } else {
      ## remove Jacobian
      .stateInfo <- .rxGenFunState(.ret)
      .s <- .rxLoadPrune(.ret, FALSE)
      .s$..stateInfo <- .stateInfo
      .tmp2 <- .s$..lhs
      if (collapseModel) .tmp2 <- ""
      .new <- paste(c(
        .s$..stateInfo["state"],
        .s$..lhs0,
        .s$..ddt,
        .tmp2,
        .s$..stateInfo["statef"],
        .s$..stateInfo["dvid"],
        ""
      ), collapse = "\n")
      .ret <- rxModelVars(.new)
    }
  }
  if (indLin) {
    .code <- .rxIndLin(.ret)
    .new <- paste0(rxNorm(.ret), "\n", .code)
    assignInMyNamespace(".rxMECode", .code)
    .ret <- rxModelVars(.new)
  }
  return(.ret)
}

#' Add item to solved system of equations
#'
#' @title rxChain  Chain or add item to solved system of equations
#'
#' @param obj1 Solved object.
#'
#' @param obj2 New object to be added/piped/chained to solved object.
#'
#' @return When `newObject` is an event table, return a new
#'     solved object with the new event table.
#'
#' @author Matthew L. Fidler
#'
#' @keywords internal
#'
#' @export
rxChain <- function(obj1, obj2) {
  .args <- rev(as.list(match.call())[-1])
  names(.args) <- c("obj", "solvedObject")
  return(do.call("rxChain2", .args, envir = parent.frame(1)))
}

#' @rdname rxChain
#' @export
"+.solveRxDll" <- function(obj1, obj2) {
  return(rxode2::rxChain(obj1, obj2))
}

#' Second command in chaining commands
#'
#' This is s3 method is called internally with `+` and `\%>\%` operators.
#'
#' @param obj the object being added/chained/piped to the solved object
#' @param solvedObject the solved object
#' @return chained operation
#' @keywords internal
#' @author Matthew L.Fidler
#' @export
rxChain2 <- function(obj, solvedObject) {
  UseMethod("rxChain2")
}

#' @rdname rxChain2
#' @export
rxChain2.default <- function(obj, solvedObject) {
  .args <- as.list(match.call())
  stop(sprintf(
    gettext("Do not know how to add %s to rxode2 solved object %s"),
    toString(.args[[2]]), toString(.args[[3]])
  ),
  call. = FALSE
  )
}

#' @rdname rxChain2
#' @export
rxChain2.EventTable <- function(obj, solvedObject) {
  .args <- rev(as.list(match.call())[-1])
  names(.args) <- c("object", "events")
  return(do.call("rxSolve", .args, envir = parent.frame(1)))
}

.isLatex <- function() {
  ## nocov start
  if (!("knitr" %in% loadedNamespaces())) {
    return(FALSE)
  }
  get("is_latex_output", asNamespace("knitr"))()
  ## nocov end
}
#' Internal function to figure out if this session supports Unicode
#'
#' @return boolean indicating if this session supports Unicode
#'
#' @keywords internal
#'
#' @export
.useUtf <- function() {
  ## nocov start
  opt <- getOption("cli.unicode", NULL)
  if (!is.null(opt)) {
    isTRUE(opt)
  } else {
    l10n_info()$`UTF-8` && !.isLatex()
  }
  ## nocov end
}
.getBoundRemember <- NULL
.getBound <- function(x, parent = parent.frame(2)) {
  ## nocov start
  if (!is.null(.getBoundRemember)) return(.getBoundRemember)
  .isRx <- try(rxIs(x, "rxode2"), silent = TRUE)
  if (inherits(.isRx, "try-error")) .isRx <- FALSE
  if (.isRx) {
    if (!is.null(x$package)) {
      return(substr(x$modName, nchar(x$package) + 2, nchar(x$modName)))
    }
  }
  bound <- do.call("c", lapply(ls(globalenv()), function(cur) {
    if (identical(parent[[cur]], x)) {
      return(cur)
    }
    return(NULL)
  }))
  if (length(bound) > 1) bound <- bound[1]
  if (length(bound) == 0) {
    bound <- do.call("c", lapply(ls(parent), function(cur) {
      if (identical(parent[[cur]], x)) {
        return(cur)
      }
      return(NULL)
    }))
    if (length(bound) > 1) bound <- bound[1]
    if (length(bound) == 0) {
      bound <- ""
    }
  }
  return(bound)
  ## nocov end
}
.getReal <- function(x) {
  ## Should always be in sync
  if (rxIs(x, "rxode2")) {
    if (!is.null(x$package)) {
      .ns <- loadNamespace(x$package)
      if (exists(".rxUpdated", .ns)) {
        .rxu <- get(".rxUpdated", .ns)
      }
      if (exists(x$modName, .rxu)) {
        return(get(x$modName, .rxu))
      }
    }
  }
  return(x)
}

#' Return the rxode2 coefficients
#'
#' This returns the parameters , state variables
#'
#' @param object is an rxode2 object
#' @param ... ignored arguments
#'
#' @return a rxCoef object with the following
#'
#' * `params`  is a list of strings for parameters for the rxode2 object
#' * `state` is a list of strings for the names of each state in
#'     the rxode2 object.
#' * `ini` is the model specified default values for the
#'     parameters.
#' * `rxode2` is the referring rxode2 object
#' @author Matthew L.Fidler
#' @keywords internal
#' @importFrom stats coef
#'
#' @export
coef.rxode2 <- function(object,
                       ...) {
  .ret <- rxode2::rxModelVars(object)[c("params", "state", "ini", "sens", "fn.ini")]
  .ret$rxode2 <- object
  class(.ret) <- "rxCoef"
  return(.ret)
}

.rxPre <- function(model,
                   modName = NULL) {
  if (!is.null(modName)) {
    if (is.null(.pkg)) {
      .modelPrefix <- paste0(gsub("\\W", "_", modName), "_", .Platform$r_arch, "_")
    } else {
      .modelPrefix <- paste0(gsub("\\W", "_", modName), "_")
    }
  } else {
    .mv <- rxModelVars(model)
    if (.Call(`_rxode2_codeLoaded`) == 0L) .rxModelVarsCharacter(setNames(rxNorm(.mv), NULL))
    .cache <- .rxModelVarsCCache
    .modelPrefix <- paste0("rx_", .mv$md5["parsed_md5"], "_", .Platform$r_arch, "_")
  }
  return(.modelPrefix)
}

.md5Rx <- NULL

#' Return the md5 of an rxode2 object or file
#'
#' This md5 is based on the model and possibly the extra c code
#' supplied for the model.  In addition the md5 is based on syntax
#' options, compiled rxode2 library md5, and the rxode2
#' version/repository.
#'
#' @inheritParams rxode2
#'
#'
#' @param ... ignored arguments
#'
#' @return If this is a rxode2 object, return a named list:
#'
#' * `file_md5` is the model's file's md5
#'
#' * `parsed_md5`  is the parsed model's file's md5.
#'
#' Otherwise return the md5 based on the arguments provided
#'
#' @keywords internal
#' @author Matthew L.Fidler
#' @export rxMd5
rxMd5 <- function(model, # Model File
                  ...) {
  ## rxMd5 returns MD5 of model file.
  ## digest(file = TRUE) includes file times, so it doesn't work for this needs.
  if (missing(model)) {
    return(rxode2.md5)
  } else if (is(model, "character")) {
    if (length(model) == 1) {
      if (file.exists(model)) {
        .ret <- readLines(model, warn = FALSE)
      } else {
        .ret <- model
      }
    } else {
      if (any(names(model) == "normModel")) {
        .ret <- setNames(model["normModel"], NULL)
        if (any(names(model) == "indLin")) {
          if (model["indLin"] != "") {
            .ret <- setNames(paste0(
              .ret, "\n",
              model["indLin"]
            ), NULL)
          }
        }
      } else {
        stop("unknown model", call. = FALSE)
      }
    }
    rxSyncOptions()
    .tmp <- c(
      rxode2.syntax.allow.ini, rxode2.calculate.jacobian,
      rxode2.calculate.sensitivity)
    .ret <- c(
      .ret, .tmp, .rxIndLinStrategy, .rxIndLinState,
      .linCmtSens, .udfMd5Info(), .rxFullPrint
    )
    if (is.null(.md5Rx)) {
      .tmp <- getLoadedDLLs()$rxode2
      class(.tmp) <- "list"
      assignInMyNamespace(".md5Rx", digest::digest(.tmp$path, serialize = TRUE, file = TRUE, algo = "md5"))
    }
    ## new rxode2 DLLs gives different digests.
    .ret <- c(.ret, .md5Rx)
    ## Add version and github repository information
    .ret <- c(.ret, rxode2::rxVersion())
    return(list(
      text = model,
      digest = digest::digest(list(.ret, .indLinInfo), serialize = TRUE, algo = "md5")
    ))
  } else {
    rxode2::rxModelVars(model)$md5
  }
} # end function rxMd5

.rxLastModels <- NULL

.rxShouldUnload <- function(parseMd5) {
  if (is.null(.rxLastModels)) return(TRUE)
  return(!(.in(parseMd5, .rxLastModels)))
}

.rxTimeId <- function(parseMd5) {
  if (exists(parseMd5, envir = .rxModels)) {
    .timeId <- get(parseMd5, envir = .rxModels)
  } else {
    .timeId <- as.integer(Sys.time())
    assign(parseMd5, .timeId, envir = .rxModels)
    .rxLastModels <- c(parseMd5, .rxLastModels)
    .nKeep <- getOption("rxode2.dontUnload", 10)
    .nKeep <- as.integer(.nKeep)
    if (.nKeep <= 0L) {
      .rxLastModels <- NULL
    } else if (length(.rxLastModels) < .nKeep) {
      .rxLastModels <- .rxLastModels[seq(1, .nKeep)]
    }
    assignInMyNamespace(".rxLastModels", .rxLastModels)
  }
  return(.timeId)
}

#' Translate the model to C code if needed
#'
#' This function translates the model to C code, if needed
#'
#'
#' @inheritParams rxode2
#'
#'
#' @param modelPrefix Prefix of the model functions that will be
#'     compiled to make sure that multiple rxode2 objects can coexist
#'     in the same R session.
#'
#' @param md5 Is the md5 of the model before parsing, and is used to
#'     embed the md5 into DLL, and then provide for functions like
#'     [rxModelVars()].
#'
#' @param ... Ignored parameters.
#'
#'
#' @param modVars returns the model variables instead of the named
#'     vector of translated properties.
#'
#'
#'
#' @return a named vector of translated model properties
#'       including what type of jacobian is specified, the `C` function prefixes,
#'       as well as the `C` functions names to be called through the compiled model.
#' @seealso [rxode2()], [rxCompile()].
#' @author Matthew L.Fidler
#' @export
rxTrans <- function(model,
                    modelPrefix = "", # Model Prefix
                    md5 = "", # Md5 of model
                    modName = NULL, # Model name for DLL
                    modVars = FALSE, # Return modVars
                    ...) {
  UseMethod("rxTrans")
} # end function rxTrans


#' @rdname rxTrans
#' @export
rxTrans.default <- function(model,
                            modelPrefix = "", # Model Prefix
                            md5 = "", # Md5 of model
                            modName = NULL, # Model name for DLL
                            modVars = FALSE, # Return modVars
                            ...) {
  .mv <- rxode2::rxModelVars(model)
  if (modVars) {
    return(.mv)
  } else {
    return(c(.mv$trans, .mv$md5))
  }
}

.rxMECode <- ""

#' @rdname rxTrans
#' @export
rxTrans.character <- memoise::memoise(function(model,
                                               modelPrefix = "", # Model Prefix
                                               md5 = "", # Md5 of model
                                               modName = NULL, # Model name for DLL
                                               modVars = FALSE, # Return modVars
                                               ...) {
  ## rxTrans returns a list of compiled properties
  if (file.exists(model)) {
    .isStr <- 0L
  } else {
    .isStr <- 1L
  }
  if (missing(md5)) {
    md5 <- rxMd5(model)$digest
  }
  .ret <- .Call(
    `_rxode2_trans`, model, modelPrefix, md5, .isStr,
    as.integer(crayon::has_color()),
    .rxMECode, .rxSupportedFuns(),
    .rxFullPrint
  )
  if (inherits(.ret, "try-error")) {
    message("model")
    if (.isStr == 0L) {
      message(suppressWarnings(readLines(model)))
    } else {
      message(model)
    }
    stop("cannot create rxode2 model", call. = FALSE)
  }
  md5 <- c(file_md5 = md5, parsed_md5 = rxMd5(c(
    .ret$model,
    .ret$ini,
    .ret$state,
    .ret$params,
    .ret$lhs
  ))$digest)
  .ret$timeId <- .rxTimeId(md5["parsed_md5"])
  .ret$md5 <- md5
  if (.isStr == 1L) {
    ## Now update trans.
    .prefix <- paste0("rx_", md5["parsed_md5"], "_", .Platform$r_arch, "_")
    .libName <- substr(.prefix, 0, nchar(.prefix) - 1)
    .ret <- .Call(`_rxode2_rxUpdateTrans_`, .ret, .prefix, .libName)
  }
  ## dparser::dpReload();
  ## rxReload()
  if (modVars) {
    return(.ret)
  } else {
    return(c(.ret$trans, .ret$md5))
  }
})

#' @rdname rxIsLoaded
#' @export
rxDllLoaded <- rxIsLoaded
#' Compile a model if needed
#'
#' This is the compilation workhorse creating the rxode2 model DLL
#' files.
#'
#' @inheritParams rxode2
#'
#' @param dir This is the model directory where the C file will be
#'     stored for compiling.
#'
#'     If unspecified, the C code is stored in a temporary directory,
#'     then the model is compiled and moved to the current directory.
#'     Afterwards the C code is removed.
#'
#'     If specified, the C code is stored in the specified directory
#'     and then compiled in that directory.  The C code is not removed
#'     after the DLL is created in the same directory.  This can be
#'     useful to debug the c-code outputs.
#'
#' @param prefix is a string indicating the prefix to use in the C
#'     based functions.  If missing, it is calculated based on file
#'     name, or md5 of parsed model.
#'
#'
#' @param force is a boolean stating if the (re)compile should be
#'     forced if rxode2 detects that the models are the same as already
#'     generated.
#'
#' @param ... Other arguments sent to the [rxTrans()]
#'     function.
#'
#' @return An rxDll object that has the following components
#'
#' * `dll` DLL path
#' * `model` model specification
#' * `.c` A function to call C code in the correct context from the DLL
#'          using the [.C()] function.
#' * `.call` A function to call C code in the correct context from the DLL
#'          using the [.Call()] function.
#' * `args` A list of the arguments used to create the rxDll object.
#' @inheritParams rxode2
#' @seealso [rxode2()]
#' @author Matthew L.Fidler
#' @importFrom sys exec_internal
#' @export
rxCompile <- function(model, dir, prefix, force = FALSE, modName = NULL,
                      package = NULL,
                      ...) {
  UseMethod("rxCompile")
}

.getIncludeDir <- function() {
  .cache <- R_user_dir("rxode2", "cache")
  .parseInclude <- system.file("include", package = "rxode2")
  if (dir.exists(.cache)) {
    .include <- .normalizePath(file.path(.cache, "include"))
    if (!dir.exists(.include)) {
      .malert("creating rxode2 include directory")
      dir.create(.include, recursive = TRUE, showWarnings = FALSE)
      .sysInclude <- system.file("include", package = "rxode2")
      .files <- list.files(.parseInclude)
      lapply(.files, function(file) {
        file.copy(file.path(.parseInclude, file), file.path(.include, file))
      })
      .files <- list.files(.sysInclude)
      lapply(.files, function(file) {
        file.copy(file.path(.sysInclude, file), file.path(.include, file))
      })
      .malert("getting R compile options")
      .cc <- rawToChar(sys::exec_internal(file.path(R.home("bin"), "R"), c("CMD", "config", "CC"))$stdout)
      .cc <- gsub("\n", "", .cc)
      .cflags <- rawToChar(sys::exec_internal(file.path(R.home("bin"), "R"), c("CMD", "config", "CFLAGS"))$stdout)
      .cflags <- gsub("\n", "", .cflags)
      .cflags <- paste0(.cflags, " -O", getOption("rxode2.compile.O", "2"))
      .shlibCflags <- rawToChar(sys::exec_internal(file.path(R.home("bin"), "R"), c("CMD", "config", "SHLIB_CFLAGS"))$stdout)
      .shlibCflags <- gsub("\n", "", .shlibCflags)
      .cpicflags <- rawToChar(sys::exec_internal(file.path(R.home("bin"), "R"), c("CMD", "config", "CPICFLAGS"))$stdout)
      .cpicflags <- gsub("\n", "", .cpicflags)

      .malert("precompiling headers")
      .args <- paste0(
        .cc, " -I", gsub("[\\]", "/", .normalizePath(R.home("include"))), " ",
        " -I\"", .normalizePath(.parseInclude), "\" ",
        .cflags, " ", .shlibCflags, " ", .cpicflags, " -I", gsub("[\\]", "/", .normalizePath(.include)), " ",
        paste(gsub("[\\]", "/", .normalizePath(.include)), "rxode2_model_shared.h", sep = "/"),
        ""
      )
      system(.args)
      .msuccess("done")
      return(.include)
    }
  }
  return(.normalizePath(system.file("include", package = "rxode2")))
}

.rxCompileEnv <- new.env(parent = emptyenv())
.rxCompileEnv$lst <- list()
#' Get the last compiled model information as alist
#'
#' @return A list contains the following elements:
#'
#' * `msg` the message for a bad compilation, or NULL if successful.
#'
#' * `stdout` the standard output from the compilation
#'
#' * `stderr` the standard error from the compilation
#'
#' * `c` the code code that was used
#'
#' This list will be returned invisibly, but the function will
#' also message the contents to the console.
#'
#' @export
#' @author Matthew L. Fidler
#' @examples
#' rxode2({
#'   a <- b
#' })
#' rxLastCompile()
rxLastCompile <- function() {
  lapply(names(.rxCompileEnv$lst), function(nm) {
    cli::rule(left = nm)
    message(.rxCompileEnv$lst[[nm]])
  })
  return(invisible(.rxCompileEnv$lst))
}
#' Get the number of loaded rxode2 DLLs
#'
#'
#' @return Number of loaded rxode2 DLLs
#' @export
#' @author Matthew L. Fidler
#' @examples
#' rxNumLoaded()
rxNumLoaded <- function() {
  .dlls <- getLoadedDLLs()
  length(grep(rex::rex(start, "rx_", n_times(any, 32), or("_x64", "_i386", "_", "")), names(.dlls)))
}
.pkg <- NULL
#' @rdname rxCompile
#' @export
rxCompile.rxModelVars <- function(model, # Model
                                  dir = NULL, # Directory
                                  prefix = NULL, # Prefix
                                  force = FALSE, # Force compile
                                  modName = NULL, # Model Name
                                  package = NULL,
                                  ...) {
  assignInMyNamespace(".pkg", package)
  ## rxCompile returns the DLL name that was created.
  model <- rxGetModel(model)

  if (is.null(prefix)) {
    prefix <- .rxPre(model, modName)
  }
  if (is.null(dir)) {
    if (rxode2.tempfiles) {
      .dir <- file.path(rxTempDir(), paste0(prefix, ".rxd"))
    } else {
      .dir <- getwd()
    }
  } else {
    .dir <- dir
    if (file.exists(.dir)) {
      if (!file.exists(file.path(.dir, paste0(rxode2.md5, ".md5")))) {
        .malert("remove old rxode2 dir {.file {.dir}}")
        unlink(.dir, recursive = TRUE, force = TRUE)
        .msuccess("done")
      }
    }
  }
  .dir <- suppressMessages(.normalizePath(.dir, mustWork = FALSE))
  if (!file.exists(.dir)) {
    dir.create(.dir, recursive = TRUE, showWarnings = FALSE)
    writeLines("rxode2", file.path(.dir, paste0(rxode2.md5, ".md5")))
  }

  .cFile <- file.path(.dir, sprintf("%s.c", substr(prefix, 0, nchar(prefix) - 1)))
  .cDllFile <- file.path(.dir, sprintf("%s%s", substr(prefix, 0, nchar(prefix) - 1), .Platform$dynlib.ext))
  .allModVars <- NULL
  .needCompile <- TRUE
  if (file.exists(.cDllFile)) {
    .modVars <- sprintf("%smodel_vars", prefix)
    if (!missing(prefix) && !missing(dir) &&
      regexpr(
        rex::rex(start, "rx_", n_times(any, 32), or("_x64", "_i386", "_", "")),
        prefix
      ) == -1 &&
        is.loaded(.modVars)) {
      try(dyn.unload(.cDllFile), silent=TRUE)
      unlink(.cFile)
      .tmp <- try(unlink(.cDllFile), silent=TRUE)
      if (inherits(.tmp, "try-error")) {
        if (file.exists(.cDllFile)) {
          stop("cannot seem to remove '", .cDllFile, "'",
               call.=FALSE)
        }
      }
    } else {
      try(dynLoad(.cDllFile), silent = TRUE)
      if (is.loaded(.modVars)) {
        .allModVars <- eval(parse(text = sprintf(".Call(\"%s\")", .modVars)), envir = .GlobalEnv)
        .modVars <- .allModVars$md5
        if (!any(names(.modVars) == "file_md5")) {
          .needCompile <- FALSE
        } else {
          .needCompile <- FALSE
        }
      } else {
        .needCompile <- FALSE
      }
    }
  }
  if (force || .needCompile) {
    .lock <- paste0(.cFile, ".lock")
    if (file.exists(.lock)) {
      message("rxode2 already building model, waiting for lock file removal")
      message(sprintf("lock file: \"%s\"", .lock))
      while (file.exists(.lock)) {
        Sys.sleep(0.5)
        message(".", appendLF = FALSE)
      }
      message("")
      if (!(file.exists(.cDllFile))) {
        stop("error building model on another thread", call. = FALSE)
      }
    } else {
      sink(.lock)
      cat("\n")
      sink()
      on.exit(
        {
          unlink(.lock)
        },
        add = TRUE
      )
      .Makevars <- .normalizePath(file.path(.dir, "Makevars"))
      if (file.exists(.Makevars)) {
        .firstMake <- readLines(.Makevars, 1)
        if (length(.firstMake) == 0) {
          unlink(.Makevars)
        } else if ("#rxode2 Makevars" == .firstMake) {
          unlink(.Makevars)
        } else {
          file.rename(.Makevars, paste0(.Makevars, ".bakrx"))
        }
      }
      .trans <- model
      if (file.exists(.cDllFile)) {
        if (inherits(.modVars, "list")) {
          if (.modVars["parsed_md5"] == .trans["parsed_md5"]) {
            message("do not need to recompile, minimal change to model detected")
            .needCompile <- FALSE
          }
        }
      }
      if (force || .needCompile) {
        ## Setup Makevars
        ## Now create C file
        .mv <- model
        .j <- 0
        .i <- 0
        .trans <- c(.mv$trans, .mv$md5)
        ## Load model into memory if needed
        if (.Call(`_rxode2_codeLoaded`) == 0L) .rxModelVarsCharacter(setNames(.mv$model, NULL))
        .prefix2 <- .rxModelVarsCCache[[3]]
        ## SEXP pMd5, SEXP timeId, SEXP fixInis
        .newMod <- FALSE
        if (!is.null(modName)) {
          .newMod <- regexpr("_new", modName) != -1
        }
        .rxModelVarsLast[[17]] <- .indLinInfo
        .model <- .rxModelVarsLast$model
        .model["indLin"] <- .rxMECode
        .rxModelVarsLast$model <- .model
        if (!is.null(package) && !.newMod) {
          .libname <- c(package, gsub(.Platform$dynlib.ext, "", basename(.cDllFile)))
          .Call(
            `_rxode2_codegen`, .cFile, prefix, .libname,
            .trans["parsed_md5"], paste(.rxTimeId(.trans["parsed_md5"])),
            .rxModelVarsLast, .rxSupportedFuns()
          )
        } else {
          .libname <- gsub(.Platform$dynlib.ext, "", basename(.cDllFile))
          .libname <- c(.libname, .libname)
          .Call(
            `_rxode2_codegen`, .cFile, prefix, .libname,
            .trans["parsed_md5"], paste(.rxTimeId(.trans["parsed_md5"])),
            .rxModelVarsLast, .rxSupportedFuns()
          )
        }
        .defs <- ""
        .ret <- sprintf(
          "#rxode2 Makevars\nPKG_CFLAGS=-O%s %s -I\"%s\" -I\"%s\"\nPKG_LIBS=$(BLAS_LIBS) $(LAPACK_LIBS) $(FLIBS)\n",
          getOption("rxode2.compile.O", "2"),
          .defs, .getIncludeDir(),
          system.file("include", package = "rxode2")
        )
        ## .ret <- paste(.ret, "-g")
        sink(.Makevars)
        cat(.ret)
        sink()
        sink(.normalizePath(file.path(.dir, "extraC.h")))
        cat(rxode2::.extraCnow())
        sink()
        try(dyn.unload(.cDllFile), silent = TRUE)
        try(unlink(.cDllFile))
        .cmd <- file.path(R.home("bin"), "R")
        .args <- c("CMD", "SHLIB", basename(.cFile))
        .rxBinpref <- Sys.getenv("rxBINPREF")
        if (.rxBinpref != "") {
          .oldBinpref <- Sys.getenv("BINPREF")
          Sys.setenv("BINPREF" = .rxBinpref)
          on.exit(Sys.setenv("BINPREF" = .oldBinpref), add = TRUE)
        }
        rxode2::rxReq("sys")
        .rxWithWd(.dir, {
          .out <- sys::exec_internal(cmd = .cmd, args = .args, error = FALSE)
        })
        .stderr <- rawToChar(.out$stderr)
        .rxCompileEnv$lst <- list()
        if (!(all(.stderr == "") && length(.stderr) == 1)) {
          .rxCompileEnv$lst[["stderr"]] <- paste(.stderr, sep = "\n")
        }
        .badBuild <- function(msg, cSrc = TRUE) {
          .rxCompileEnv$lst[["msg"]] <- gettext(msg)
          .rxCompileEnv$lst[["stdout"]] <- rawToChar(.out$stdout)
          if (cSrc) {
            .rxCompileEnv$lst[["c"]] <- paste(readLines(.cFile), collapse = "\n")
          } else {
            dyn.load(.cDllFile)
          }
          message("Error building the model: see rxode2::rxLastCompile()")
          if (.Platform$OS.type == "windows") {
            message("this could be because your Rtools is not set up correctly")
          } else {
            message("please make sure you have a working C compiler set up")
          }
          message("you may use nlmixr2::nlmixr2CheckInstall() to help diagnose installation issues")
          stop(msg, call. = FALSE)
        }
        if (!(.out$status == 0 && file.exists(.cDllFile))) {
          .badBuild("error building model")
        }
      }
    }
    .tmp <- try(dynLoad(.cDllFile), silent = FALSE)
    if (inherits(.tmp, "try-error")) {
      ## Try unloading rxode2 dlls now...
      rxUnloadAll()
      .tmp <- try(dynLoad(.cDllFile), silent = TRUE)
      if (inherits(.tmp, "try-error")) {
        .badBuild("Error loading model (though dll exists)", cSrc = FALSE)
      } else {
        warning("unloaded all rxode2 dlls before loading the current DLL", call. = FALSE)
      }
    }
    .modVars <- sprintf("%smodel_vars", prefix)
    if (is.loaded(.modVars)) {
      .allModVars <- eval(parse(text = sprintf(".Call(\"%s\")", .modVars)), envir = .GlobalEnv)
    } else {
      .badBuild("Error, model doesn't have correct model variables.")
    }
  }
  .call <- function(...) {
    return(.Call(...))
  }
  .args <- list(
    model = model, dir = .dir, prefix = prefix,
    force = force, modName = modName,
    ...
  )
  if (is.null(.allModVars)) {
    stop("something went wrong in compilation")
  }
  assign(.cDllFile, 0L, envir = .rxModels) ## Loaded model.
  ret <- suppressWarnings({
    list(
      dll = .cDllFile,
      c = .cFile,
      model = .allModVars$model["normModel"],
      modVars = .allModVars,
      .call = .call,
      args = .args
    )
  })
  class(ret) <- "rxDll"
  return(ret)
}

#' @rdname rxCompile
#' @export
rxCompile.character <- rxCompile.rxModelVars

#' @rdname rxCompile
#' @export
rxCompile.rxDll <- function(model, ...) {
  .args <- as.list(match.call(expand.dots = TRUE))
  .rxDllArgs <- model$args
  if (any(names(.rxDllArgs) == "dir")) {
    .args$dir <- .rxDllArgs$dir
  }
  if (any(names(.rxDllArgs) == "prefix")) {
    .args$prefix <- .rxDllArgs$prefix
  }
  if (any(names(.rxDllArgs) == "force")) {
    .args$force <- .rxDllArgs$force
  }
  if (any(names(.rxDllArgs) == "modName")) {
    .args$modName <- .rxDllArgs$modName
  }
  .args$model <- .rxDllArgs$model
  return(do.call(getFromNamespace("rxCompile", "rxode2"), .args, envir = parent.frame(1)))
}

#' @rdname rxCompile
#' @export
rxCompile.rxode2 <- function(model, ...) {
  model$compile()
}

#' @rdname rxDynLoad
#' @export
rxLoad <- rxDynLoad

#' @rdname rxDynUnload
#' @export
rxUnload <- rxDynUnload

.rxConditionLst <- list()
#' Current Condition for rxode2 object
#'
#' @param obj rxode2 object
#' @param condition If specified and is one of the conditions in the
#'     rxode2 object (as determined by [rxExpandIfElse()]),
#'     assign the rxode2 current condition to this parameter.  If the
#'     condition is not one of the known condition, the condition is
#'     set to `NULL`, implying no conditioning currently used.
#' @return Current condition for rxode2 object
#' @author Matthew L. Fidler
#' @keywords internal
#' @export
rxCondition <- function(obj, condition = NULL) {
  .key <- digest::digest(rxode2::rxNorm(obj, FALSE), algo = "md5", serialize = TRUE)
  if (!missing(condition) && is.null(condition)) {
    condition <- FALSE
  }
  if (is.null(condition)) {
    return(getFromNamespace(".rxConditionLst", "rxode2")[[.key]])
  } else if (any(condition == rxNorm(obj, TRUE))) {
    .lst <- getFromNamespace(".rxConditionLst", "rxode2")
    .lst[[.key]] <- condition
    assignInMyNamespace(".rxConditionLst", .lst)
    return(getFromNamespace(".rxConditionLst", "rxode2")[[.key]])
  } else {
    .lst <- getFromNamespace(".rxConditionLst", "rxode2")
    .lst[[.key]] <- NULL
    assignInMyNamespace(".rxConditionLst", .lst)
    return(getFromNamespace(".rxConditionLst", "rxode2")[[.key]])
  }
}

#' Get the normalized model
#'
#'
#' This get the syntax preferred model for processing
#'
#' @inheritParams rxModelVars
#' @param condition Character string of a logical condition to use
#'   for subsetting the normalized model.  When missing, and a
#'   condition is not set via `rxCondition`, return the whole
#'   code with all the conditional settings intact.  When a condition
#'   is set with `rxCondition`, use that condition.
#' @param removeInis A boolean indicating if parameter initialization
#'   will be removed from the model
#' @param removeJac A boolean indicating if the Jacobians will be
#'   removed.
#' @param removeSens A boolean indicating if the sensitivities will
#'   be removed.
#' @return Normalized Normal syntax (no comments)
#' @author Matthew L. Fidler
#' @export
rxNorm <- function(obj, condition = NULL, removeInis, removeJac, removeSens) {
  if (!missing(removeInis) || !missing(removeJac) || !missing(removeSens)) {
    .ret <- strsplit(rxNorm(obj, condition), "\n")[[1]]
    if (missing(removeInis)) {
      removeInis <- FALSE
    }
    if (missing(removeJac)) {
      removeJac <- FALSE
    }
    if (missing(removeSens)) {
      removeSens <- FALSE
    }
    if (removeInis) {
      .ret <- .rxRmIni(.ret)
    }
    if (removeJac) {
      stop("'removeJac' is no longer supported")
      ## .ret <- .rxRmJac(.ret)
    }
    if (removeSens) {
      stop("'removeSens' is no longer supported")
      ## .ret <- .rxRmSens(.ret)
    }
    return(paste(.ret, collapse = "\n"))
  } else {
    if (is(condition, "logical")) {
      if (!condition) {
        condition <- NULL
      } else {
        .tmp <- rxode2::rxExpandIfElse(obj)
        return(names(.tmp))
      }
    } else if (is.null(condition)) {
      condition <- rxode2::rxCondition(obj)
    }
    if (is.null(condition)) {
      .tmp <- rxode2::rxModelVars(obj)$model["normModel"]
      names(.tmp) <- NULL
      return(.tmp)
    } else {
      if (is(condition, "character")) {
        .tmp <- rxode2::rxExpandIfElse(obj)[condition]
        names(.tmp) <- NULL
        return(.tmp)
      } else {
        return(rxNorm(obj, FALSE))
      }
    }
  }
}



.rxModelVarsCCache <- NULL
.rxModelVarsLast <- NULL
.rxModelVarsCharacter <- function(obj) {
  if (length(obj) == 1) {
    .parseModel <- tempfile("parseModel4")
    .prefix <- paste0(basename(.parseModel), "_", .Platform$r_arch, "_")
    .exists <- try(file.exists(obj), silent = TRUE)
    if (inherits(.exists, "try-error")) {
      .exists <- FALSE
    } else {
      .exists <- TRUE
    }
    if (.exists) {
      .parseModel <- obj
    } else {
      .parseModel <- paste(obj, collapse = "\n")
    }
    .ret <- rxTrans(.parseModel, modelPrefix = .prefix, modVars = TRUE)
    .cFile <- list(.exists, ifelse(.exists, obj, ""), .prefix)
    assignInMyNamespace(".rxModelVarsCCache", .cFile)
    assignInMyNamespace(".rxModelVarsLast", .ret)
    return(.ret)
  } else {
    .rxModelVarsCharacter(paste(obj, collapse = "\n"))
  }
}

#' @rdname rxInits
#' @export
rxInit <- rxInits
#' Reload rxode2 DLL
#'
#' Can be useful for debugging
#'
#' @author Matthew L. Fidler
#' @keywords internal
#' @return boolean of if the object is reloaded
#' @export
rxReload <- function() {
  .tmp <- getLoadedDLLs()$rxode2
  class(.tmp) <- "list"
  dyn.unload(.tmp$path)
  .ret <- is.null(getLoadedDLLs()$rxode2)
  dynLoad(.tmp$path)
  .ret <- .ret && !is.null(getLoadedDLLs()$rxode2)
  return(.ret)
}

.rxModels <- new.env(parent = emptyenv())
#' Get the rxModels  information
#' @param env boolean that returns the environment where models are stored (TRUE), or the currently assigned rxode2 model variables (FALSE).
#' @keywords internal
#' @return internal rxModels information environment
#' @export
rxModels_ <- # nolint
  function(env = TRUE) {
    if (env) {
      return(getFromNamespace(".rxModels", "rxode2"))
    } else {
      return(.Call(rxode2_get_mv, PACKAGE = "rxode2"))
    }
  }

#' All model variables for a rxode2 object
#'
#' Return all the known model variables for a specified rxode2 object
#'
#' These items are only calculated after compilation; they are
#' built-into the rxode2 compiled DLL.
#'
#' To allow extension, an s3 hook is added in the function `rxModelVarsS3`.
#'
#' @param obj rxode2 family of objects
#'
#' @return A list of rxode2 model properties including:
#'
#' * `params`  a character vector of names of the model parameters
#' * `lhs` a character vector of the names of the model calculated parameters
#' * `state` a character vector of the compartments in rxode2 object
#' * `trans` a named vector of translated model properties
#'       including what type of jacobian is specified, the `C` function prefixes,
#'       as well as the `C` functions names to be called through the compiled model.
#' * `md5` a named vector that gives the digest of the model (`file_md5`) and the parsed model
#'      (`parsed_md5`)
#' * `model`  a named vector giving the input model (`model`),
#'    normalized model (no comments and standard syntax for parsing, `normModel`),
#'    and interim code that is used to generate the final C file `parseModel`
#'
#' @keywords internal
#' @family Query model information
#' @author Matthew L. Fidler
#' @export
rxModelVars <- function(obj) {
  if (is(substitute(obj), "{")) {
    .obj <- deparse(substitute(obj))
    if (.obj[1] == "{") {
      .obj <- .obj[-1]
      .obj <- .obj[-length(.obj)]
    }
    .obj <- paste(.obj, collapse = "\n")
    return(rxModelVars_(.obj))
  }
  if ((is.list(obj) &&
         inherits(obj, "rxUi")) ||
        inherits(obj, "raw")) {
    obj <- rxUiDecompress(obj)
  }
  if (is(obj, "rxModelVars")) {
    return(obj)
  }
  .tmp <- try(obj, silent = TRUE)
  if (inherits(.tmp, "try-error")) {
    obj <- as.character(substitute(obj))
  }
  rxModelVars_(obj)
}

#' @rdname rxModelVars
#' @export
rxModelVarsS3 <- function(obj) {
  UseMethod("rxModelVarsS3")
}

#' @rdname rxModelVars
#' @export
rxModelVarsS3.rxUi <- function(obj) {
  .ret <- rxUiDecompress(obj)
  get("mv0", .ret)
}


#' @rdname rxModelVars
#' @export
rxModelVarsS3.default <- function(obj) {
  stop("need an rxode2-type object to extract model variables",
       call.=FALSE)
}

.rxGetParseModel <- function(type = c("normal", "dt"),
                             collapse = TRUE) {
  .type.idx <- c("normal" = 0L, "dt" = 1L)
  if (is(type, "character")) {
    type <- .type.idx[match.arg(type)]
  }
  .ret <- .Call(`_rxode2_parseModel`, type)
  if (collapse) {
    .ret <- paste(.ret, collapse = "")
  }
  return(.ret)
}

.rxGetModelInfoFromDll <- function(dll) {
  .base <- basename(dll)
  if (nchar(.base) >= 36) {
    if (substr(.base, 36, 36) == "_") {
      .md5 <- substring(.base, 4, 35)
      return(c(.md5, paste0("rx_", .md5, "_", .Platform$r_arch, "_")))
    }
  }
  .extra <- nchar(.Platform$r_arch) + 1 + nchar(.Platform$dynlib.ext)
  .mod <- substring(.base, 0, nchar(.base) - .extra)
  return(c(.mod, paste0(.mod, "_", .Platform$r_arch, "_")))
}
