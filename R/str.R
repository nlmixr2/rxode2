#' @export
str.rxHidden <- function(object, ...) {
  cat("\r")
}


#' @importFrom utils str
#' @export
str.rxSolve <- function(object, ..., nchar.max = 128) {
  if (rxIs(object, "rxSolve")) {
    .dn <- .Call(`_rxode2_rxSolveDollarNames`, object)
    .max <- max(sapply(.dn, nchar))
    cat(sprintf(
      "Classes 'rxSolve' and 'data.frame':\t%s rows of  %s variables:\n",
      object$.check.nrow, object$.check.ncol
    ))
    for (.n in c(
      "sim.id", "id", "evid", "cmt", "ss", "amt", "rate", "dur",
      "ii"
    )) {
      if (any(names(object) == .n)) {
        cat(paste0(
          " $ ", .n,
          paste(rep(" ", .max - nchar(.n)), collapse = ""),
          ":"
        ))
        str(object[[.n]])
      }
    }
    cat(paste0(" $ time", paste(rep(" ", .max - 4), collapse = ""), ":"))
    str(object$time, nchar.max = nchar.max - .max - 8)
    .mv <- rxModelVars(object)
    if (length(.mv$lhs) > 0) {
      cat("Left Handed values ($lhs):\n")
      for (.l in .mv$lhs) {
        cat(paste0(
          sprintf(" $ %s", .l),
          paste(rep(" ", .max - nchar(.l)), collapse = ""),
          ":"
        ))
        str(object[[.l]], nchar.max = nchar.max - .max - nchar(.l) - 4)
      }
    }
    if (length(.mv$state) > 0) {
      cat("State values ($state):\n")
      for (.l in .mv$state) {
        cat(paste0(
          sprintf(" $ %s", .l),
          paste(rep(" ", .max - nchar(.l)), collapse = ""),
          ":"
        ))
        str(object[[.l]], nchar.max = nchar.max - .max - nchar(.l) - 4)
      }
      cat("State ini values:\n")
      for (.l in .mv$state) {
        cat(paste0(
          sprintf(" $ %s0", .l),
          paste(rep(" ", .max - nchar(.l) - 1), collapse = ""),
          ":"
        ))
        str(object[[paste0(.l, "0")]],
          nchar.max = nchar.max - .max - nchar(.l) - 5
        )
      }
    }
    if (length(.mv$params) > 0) {
      cat("Parameter values ($params):\n")
      for (.l in .mv$params) {
        cat(paste0(
          sprintf(" $ %s", .l),
          paste(rep(" ", .max - nchar(.l) - 1), collapse = ""),
          ":"
        ))
        str(object[[.l]], nchar.max = nchar.max - .max - nchar(.l) - 4)
      }
    }
    .vars <- c(
      "sim.id", "id", "evid", "cmt", "ss", "amt", "rate",
      "dur", "ii", "time", .mv$lhs, .mv$state
    )
    .n2 <- names(object)[!(names(object) %fin% .vars)]
    if (length(.n2) > 0) {
      cat("Other Variables:\n")
      for (.l in .n2) {
        cat(paste0(
          sprintf(" $ %s", .l),
          paste(rep(" ", .max - nchar(.l)), collapse = ""),
          ":"
        ))
        str(object[[.l]], nchar.max = nchar.max - .max - nchar(.l) - 4)
      }
    }
    .dn <- .dn[!(.dn %fin% c(
      names(object), paste0(.mv$state, "0"),
      "t", "params", "inits",
      .mv$params
    ))]
    .fns <- sapply(.dn, function(x) {
      inherits(`$.rxSolve`(object, x), "function")
    })
    .fns <- names(.fns[.fns])
    if (length(.fns) > 0) {
      cat("Functions:\n")
      for (.l in .fns) {
        cat(paste0(
          sprintf(" $ %s", .l),
          paste(rep(" ", .max - nchar(.l)), collapse = ""),
          ":"
        ))
        str(object[[.l]], nchar.max = nchar.max - .max - nchar(.l) - 4)
      }
    }
    .dn <- .dn[!(.dn %fin% .fns)]
    if (length(.dn) > 0) {
      cat("Other:\n")
      for (.l in .dn) {
        cat(paste0(
          sprintf(" $ %s", .l),
          paste(rep(" ", .max - nchar(.l)), collapse = ""),
          ":"
        ))
        str(object[[.l]], nchar.max = nchar.max - .max - nchar(.l) - 4)
      }
    }
  } else {
    NextMethod()
  }
}

#' @export
str.rxEt <- function(object, ...) {
  cat("rxEt methods and properties:\n")
  cat(" $ get.EventTable   :function ()\n")
  cat(" $ get.obs.rec      :function ()  \n")
  cat(" $ get.nobs         :function ()  \n")
  cat(" $ add.dosing       :function ()  \n")
  cat(" $ clear.dosing     :function ()  \n")
  cat(" $ get.dosing       :function ()  \n")
  cat(" $ add.sampling     :function ()  \n")
  cat(" $ clear.sampling   :function ()  \n")
  cat(" $ get.sampling     :function ()  \n")
  cat(" $ get.units        :function ()  \n")
  cat(" $ import.EventTable:function ()  \n")
  cat(" $ copy             :function ()  \n")
  cat(" $ expand           :function ()  \n")
  return(invisible(NextMethod("str", ...)))
}

#' @export
str.rxSymInvCholEnv <- function(object, ...) {
  cat("Derivatives and Inverse of a matrix; Assigning theta will change these values.\n")
  cat(" $ theta             : Current parameters (on inverse Cholesky)\n")
  cat(" $ ntheta            : Number of parameters\n")
  cat(" $ chol.omegaInv     : chol(Omega^-1)\n")
  cat(" $ omegaInv          : Omega^-1\n")
  cat(" $ d.omegaInv        : d(Omega^-1)\n")
  cat(" $ d.D.omegaInv      : gives the d(diagonal(Omega^-1))\n")
  cat(" $ chol.omega        : chol(Omega)\n")
  cat(" $ omega             : Omega\n")
  cat(" $ log.det.OMGAinv.5 : log(det(Omega^-1))\n")
  cat(" $ tr.28             : -0.5*tr(Omega^-1 %*% d(Omega)) = 0.5*tr(d(Omega^-1) %*% Omega); (Almquist 2015 #28)\n")
  cat(" $ omega.47          : d(Omega^-1)*d(eta) (Almquist 2015 #47)\n")
  cat(" $ theta.diag        : indicator of diagonal theta values\n")
}

#' @export
str.rxode2 <- function(object, ...) {
  cat("rxode2 object methods and properties:\n")
  cat(" $ assignPtr()    : Assign C pointers\n")
  cat(" $ compile()      : compile rxode2 model\n")
  cat(" $ delete()       : delete rxode2 dll\n")
  cat(" $ dynLoad()      : load dll for rxode2 model\n")
  cat(" $ dynUnload()    : unload dll for rxode2 model\n")
  cat(" $ get.index(...) : Get compartment number\n")
  cat(" $ get.modelVars(): Get model variables\n")
  cat(" $ isLoaded()     : Is rxode2 model dll loaded\n")
  cat(" $ isValid()      : Is rxode2 model dll valid\n")
  cat(" $ load()         : Load rxode2 model\n")
  cat(" $ parse()        : Parse model (doesn't do anything anymore)\n")
  cat(" $ run(...)       : Run ODE model\n")
  cat(" $ solve(...)     : Solve ODE model\n")
  cat(" $ unload()       : Unload DLL for rxode2 model\n")
  .out <- utils::capture.output(utils::str(list(
    calcJac = object$calcJac,
    calcSens = object$calcSens,
    collapseModel = object$collapseModel,
    debug = object$debug,
    extraC = object$extraC,
    lhs = object$lhs,
    lib.name = object$lib.name,
    mdir = object$mdir,
    missing.modName = object$missing.modName,
    model = object$model,
    modName = object$modName,
    package = object$package,
    params = object$params,
    rxDll = object$rxDll,
    state = object$state,
    stateExtra = object$stateExtra,
    version = object$version,
    wd = object$wd
  )))
  .out <- .out[-1]
  sapply(.out, function(x) {
    cat(paste0(x, "\n"))
  })
  invisible()
}
