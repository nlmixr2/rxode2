.udfEnv <- new.env(parent=emptyenv())
.udfEnv$fun <- list()
.udfEnv$udf <- integer(0)
.udfEnv$envir <- NULL
.udfEnv$envList <- list()
.udfEnv$searchList <- list()
.udfEnv$rxSEeqUsr <- NULL
.udfEnv$rxCcode <- NULL
.udfEnv$symengineFs <- new.env(parent = emptyenv())
.udfEnv$extraCnow <- ""
.udfEnv$bestFun <- NULL
.udfEnv$bestFunEnv <- NULL
.udfEnv$bestFunHasDots <- FALSE
.udfEnv$bestNargs <- NA_integer_
.udfEnv$bestEqArgs <- FALSE
#' Get the udf strings for creating model md5
#'
#' @return string vector
#' @export
#' @author Matthew L. Fidler
#' @keywords internal
.udfMd5Info <- function() {
  .tmp <- ls(.udfEnv$symengineFs, all.names=TRUE)
  .env <- new.env(parent=emptyenv())
  .env$found <- FALSE
  .ret <- vapply(.tmp, function(x) {
    .cur <- .udfEnv$fun[[x]]
    if (!is.null(.cur)) {
      .env$found <- TRUE
    }
    x
  }, character(1), USE.NAMES = FALSE)
  if (.env$found) {
    .ret <- c(.ret, data.table::address(.udfEnv$envir),
              # don't cache md5 changes every run:
              as.character(Sys.time()))
  }
  .ret
}
#' Generate extraC information for rxode2 models
#'
#' @param extraC Additional extraC from rxode2 compile optioioins
#' @return Nothing, called for side effects
#' @export
#' @author Matthew L. Fidler
#' @keywords internal
.extraC <- function(extraC = NULL) {
  if (!is.null(extraC)) {
    if (file.exists(extraC)) {
      .ret <- sprintf("#include \"%s\"\n", extraC)
    } else {
      .ret <- paste(extraC, collapse = "\n")
    }
  } else {
    .ret <- ""
  }
  if (length(.udfEnv$rxCcode) > 0L) {
    .ret <- sprintf("%s\n%s\n", .ret, paste(.udfEnv$rxCcode, collapse = "\n"))
  }
  .udfEnv$extraCnow <- .ret
  return(invisible())
}
#' Get the extraCnow for compiling
#'
#'
#' @return string of extraC information
#' @export
#' @author Matthew L. Fidler
#' @keywords internal
.extraCnow <- function() {
  .udfEnv$extraCnow
}

#' Add user function to rxode2
#'
#' This adds a user function to rxode2 that can be called.  If needed,
#' these functions can be differentiated by numerical differences or
#' by adding the derivatives to rxode2's internal derivative table
#' with rxode2's `rxD` function
#'
#' @param name This gives the name of the user function
#' @param args This gives the arguments of the user function
#' @param cCode This is the C-code for the new function
#' @return nothing
#' @author Matthew L. Fidler
#' @export
#' @author Matthew L. Fidler
#' @keywords internal
rxFunParse <- function(name, args, cCode) {
  if (!is.character(name) || length(name) != 1L) {
    stop("name argument must be a length-one character vector", call. = FALSE)
  }
  if (missing(cCode)) stop("a new function requires a C function so it can be used in rxode2", call. = FALSE)
  if (any(name == names(.udfEnv$rxSEeqUsr))) {
    stop("already defined user function '", name, "', remove it fist ('rxRmFun')",
         call. = FALSE
         )
  }
  suppressWarnings(rxRmFunParse(name))
  .udfEnv$rxSEeqUsr <- c(.udfEnv$rxSEeqUsr, setNames(length(args), name))
  .udfEnv$rxCcode <- c(.udfEnv$rxCcode, setNames(cCode, name))
  assign(name, symengine::Function(name), envir = .udfEnv$symengineFs)
  return(invisible())
}
#' Return the equivalents symengine user functions from C
#'
#' @return equivalent symengine user functions
#' @export
#' @author Matthew L. Fidler
#' @keywords internal
.rxSEeqUsr <- function() {
  .udfEnv$rxSEeqUsr
}

#' Return symengineFs from user functions
#'
#' @return symengineFs from user functions
#' @export
#' @author Matthew L. Fidler
#' @keywords internal
.symengineFs <- function() {
  .udfEnv$symengineFs
}

#' Return the C code of an internal function
#'
#' @param fun is the string of a function that you wish to get the C
#'   code for
#' @return C code if found (as a string) or NULL if not found
#' @export
#' @author Matthew Fider
#' @keywords internal
.rxC <- function(fun) {
  .w <- which(names(.udfEnv$rxCcode) == fun)
  if (length(.w) == 1) return(setNames(.udfEnv$rxCcode[fun], NULL))
  NULL
}

#' @rdname rxFunParse
#' @export
rxRmFunParse <- function(name) {
  if (!is.character(name) || length(name) != 1L) {
    stop("name argument must be a length-one character vector",
         call. = FALSE)
  }
  if (!any(name == names(.udfEnv$rxSEeqUsr))) {
    warning("no user function '", name, "' to remove", call. = FALSE)
  }
  .w <- which(name == names(.udfEnv$rxSEeqUsr))
  if (length(.w) == 1L) {
    .udfEnv$rxSEeqUsr <- .udfEnv$rxSEeqUsr[-.w]
  }
  .w <- which(name == names(.udfEnv$rxCcode))
  if (length(.w) == 1L) {
    .udfEnv$rxCcode <- .udfEnv$rxCcode[-.w]
  }
  .rxD <- rxode2parseD()
  if (exists(name, envir = .rxD)) {
    if (!grepl("^rx_", name)) {
      .d <- get(name, envir=.rxD)
      lapply(names(formals(.d[[1]])), function(v) {
        suppressWarnings(rxRmFunParse(paste0("rx_", name, "_d_", v)))
      })
    }
    rm(list = name, envir = .rxD)
  }
  if (exists(name, envir = .udfEnv$symengineFs)) {
    rm(list = name, envir = .udfEnv$symengineFs)
  }
  return(invisible())
}

.udfAddToSearch <- function(envir) {
  if (is.list(envir)) {
    lapply(seq_along(envir),
           function(i) {
             .udfAddToSearch(envir[[i]])
           })
    return(invisible())
  }
  if (length(.udfEnv$searchList) == 0L) {
    .udfEnv$searchList <- list(envir)
  }
  if (!any(vapply(seq_along(.udfEnv$searchList),
                  function(i) {
                    identical(.udfEnv$searchList[[i]], envir)
                  }, logical(1), USE.NAMES = FALSE))) {
    .udfEnv$searchList <- c(.udfEnv$searchList, list(envir))
  }
  invisible()
}

#' Setup the UDF environment (for querying user defined funtions)
#'
#' @param env environment where user defined functions are queried. If NULL return current environment
#' @return environment
#' @export
#' @author Matthew L. Fidler
#' @keywords internal
.udfEnvSet <- function(env) {
  if (is.null(.udfEnv$envir)) {
    if (is.list(env)) {
      .udfEnv$envir <- env[[1]]
    } else {
      .udfEnv$envir <- env

    }
  }
  .udfAddToSearch(env)
  return(invisible(.udfEnv$envir))
}
#' Lock/Unlock environment for getting R user functions
#'
#' @param lock logical to see if environment to look for user defined
#'   functions is locked.  If it is locked then environments are not
#'   assigned.  When NULL returns lock status
#'
#' @return lock status
#' @export
#' @author Matthew L. Fidler
#' @keywords internal
.udfEnvReset <- function(lock=TRUE) {
  .udfEnv$fun <- list()
  .udfEnv$searchList <- list()
}
#' See if the UI function exists in given environment.
#'
#' If other functions have been declared, make sure they exist too.
#'
#' @param fun Function to check
#' @param nargs Number of args to check
#' @param envir Environment to check
#' @param doList A boolean to see if the functions in .udfEnv$fun
#'   should be checked too.  By default TRUE, but this is called
#'   recursively for each function (and set to FALSE)
#' @return logical declaring if the udf function exists in this environment
#' @export
#' @author Matthew L. Fidler
#' @keywords internal
.udfExists <- function(fun, nargs, envir, doList=TRUE) {
  if (is.null(envir)) return(FALSE)
  .e <- exists(fun, mode="function", envir=envir)
  if (!.e) return(FALSE)
  # ok now see if it makes sense
  .fun <- get(fun, mode="function", envir=envir)
  .f <- formals(.fun)
  .bestHasDots <- any(names(.f) == "...")
  .nargs <- length(.f)
  .bestEqArgs <- .nargs == nargs
  if (.bestEqArgs) { # We want the function to match the declared number of arguments
    if (!.bestHasDots) { # We don't want ... arguments
      if (doList) {
        # In the case of multiple user functions, make sure the other
        # user functions also exist in this environment
        if (!all(vapply(seq_along(.udfEnv$fun), function(i) {
          .info <- .udfEnv$fun[[i]]
          return(.udfExists(.info[[1]], .info[[2]], envir=envir, doList=FALSE))
        }, logical(1), USE.NAMES = FALSE))) {
          if (is.null(.udfEnv$bestFun)) {
            .udfEnv$bestFun <- .fun
          }
          return(FALSE)
        }
        # Success, save function and environment
        .udfEnv$bestFun <- .fun
        .udfEnv$bestFunEnv <- envir
        .udfEnv$bestFunHasDots <- FALSE
        .udfEnv$bestEqArgs <- TRUE
        .udfEnv$bestNargs <- nargs
      }
      return(TRUE)
    }
  }
  if (doList && is.null(.udfEnv$bestFun)) {
    .udfEnv$bestFun <- .fun
    .udfEnv$bestFunEnv <- envir
    .udfEnv$bestFunHasDots <- .bestHasDots
    .udfEnv$bestEqArgs <- .bestEqArgs
    .udfEnv$bestNargs <- .nargs
  }
  FALSE
}
#' While parsing or setting up the solving, get information about the
#' user defined function
#'
#' @param fun function (character) to get information about
#' @param nargs Preferred number of arguments
#' @return A list with two elements
#'   - nargs = `NA` if the user function isn't supported, or the number of arguments suported
#'   - string = Error message when `NA` or function string
#' @noRd
#' @author Matthew L. Fidler
.getUdfInfo <- function(fun, nargs) {
  if (is.null(.udfEnv$envir)) {
    return(list(nargs=NA_integer_,
                "rxode2 cannot determine which environment the user defined functions are located"))
  }
  .udfEnv$bestFun <- NULL
  .udfEnv$bestFunHasDots <- FALSE
  .udfEnv$bestEqArgs <- TRUE
  .found <- FALSE
  if (!.udfExists(fun, nargs, .udfEnv$envir)) {
    # search prior environments with UDFs, assign the first one in the environments that match
    if (length(.udfEnv$searchList) > 0L) {
      if (any(vapply(seq_along(.udfEnv$searchList), function(i) {
        .udfExists(fun, nargs, .udfEnv$searchList[[i]])
      },  logical(1), USE.NAMES = FALSE))) {
        .found <- TRUE
      }
    }
  } else {
    .found <- TRUE
  }
  if (.udfEnv$bestFunHasDots) {
    return(list(nargs=NA_integer_,
                "rxode2 user defined R cannot have '...' arguments"))
  }
  if (!.udfEnv$bestEqArgs) {
    return(list(nargs=NA_integer_,
                sprintf("rxode2 user defined R function has %d arguments, but supplied %d",
                        .udfEnv$bestNargs, nargs)))
  }
  if (!.found) {
    .msg <- sprintf("function '%s' is not supported; user function not found",
                    fun)
    return(list(nargs=NA_integer_, .msg))
  }

  .fun <- .udfEnv$bestFun
  .udfEnv$envir <- .udfEnv$bestFunEnv
  .udfEnv$fun[[fun]] <- list(fun, nargs)
  .w <- which(names(.udfEnv$udf) == fun)
  if (length(.w) == 0L) {
    .udfEnv$udf <- c(.udfEnv$udf, setNames(nargs, fun))
  }
  return(list(nargs=nargs,
              fun))
}

#' This function is run before starting a rxode2 solve to make sure
#' the R-based user functions are setup correctly.
#'
#' This function also resets the udf-based run-time errors
#'
#' @param iv Named Integer Vector with the names representing the
#'   functions and the integers representing the number of arguments
#'   that were present when the model was compiled
#' @return nothing, called for side effect
#' @noRd
#' @author Matthew L. Fidler
.setupUdf <- function(iv) {
  if (!is.environment(.udfEnv$envir)) return(FALSE)
  .w <- which(is.na(iv))
  iv <- iv[-.w]
  .n <- names(iv)
  .env <- new.env(parent=emptyenv())
  .env$needRecompile <- FALSE
  lapply(.n,
         function(n) {
           .oldArg <- iv[n]
           .new <- .getUdfInfo(n, .oldArg)
           if (any(names(.udfEnv$rxSEeqUsr) == n)) {
             .c <- .udfEnv$rxSEeqUsr[n]
             if (.c == .new[[1]]) {
               message("compiled with R user function '", n, "'; now there is a clashing C user function")
               .env$needRecompile <- TRUE
               message("triggered a recompile to use the C user function (they are always preferred)")
             } else {
               stop("there is both C and R user functions '", n, "' with a different number of arguments\n  since rxode2 prefers C, you will need to rename your R user function to use it")

             }
           }
           if (is.na(.new[[1]])) {
             stop(.new[[2]], call.=FALSE)
           } else if (.new[[1]] != .oldArg) {
             stop("'", n,
                  "' had ", .oldArg, " arguments when model was compiled, now it has ",
                  .new[[1]], " arguments",
                  call.=FALSE)
           }
           NULL
         })
  .env$needRecompile
}
#' Reset the tracking of user defined functions
#'
#' This is called during parsing reset
#'
#' @return Nothing, called for side effects
#' @noRd
#' @author Matthew L. Fidler
.udfReset <- function() {
  .udfEnv$udf <- integer(0)
}

#' This gets the user defined functions information for incorporation
#' in the model variables
#'
#' @return A integer vector; The values are the number of arguments;
#'   the names are the function names
#' @author Matthew L. Fidler
#' @noRd
.udfInfo <- function() {
  if (length(.udfEnv$udf) == 0) return(integer(0))
  if (!is.environment(.udfEnv$envir)) return(integer(0))
  .addr <- data.table::address(.udfEnv$envir)
  .udfEnv$envList[[.addr]] <- .udfEnv$envir
  c(.udfEnv$udf, setNames(NA_integer_, .addr))
}

#' Use the udf model variable information to get the environment where
#' the functions exists
#'
#' @param udf modelVars$udf, integer vector with NA_integer_ for the
#'   address of the environment where the functions exist
#' @return nothing called for side effects
#' @export
#' @author Matthew L. Fidler
#' @keywords internal
.udfEnvSetUdf <- function(udf) {
  if (length(udf) == 0L) return(invisible())
  .w <- which(is.na(udf))
  .addr <- names(udf)[.w]
  .env <- .udfEnv$envList[[.addr]]
  if (is.environment(.env)) {
    .udfAddToSearch(.env)
    ## .udfEnv$envir <- .env
  } else {
    stop("environment were user functions were defined is no longer present")
  }
  invisible()
}
#' Get the function name with the current arguments as a string
#'
#' @param fun function name
#' @param args  arguments
#' @return string of the form 'fun(arg1, arg2)':
#' @export
#' @author Matthew L. Fidler
#' @keywords internal
.udfCallFunArg <- function(fun, args) {
  paste0("'", fun, "(",
         paste(vapply(seq_along(args),
                function(i) {
                  as.character(args[[i]])
                }, character(1), USE.NAMES=FALSE), collapse=", "),
         ")': ")
}
#' This is the function that is always called for every user function in rxode2
#'
#' @param fun A character vector representing the function
#' @param args A list of double numbers that will be used as the
#'   function arguments
#' @return A double numeric value, including `NA_real` when the
#'   function isn't working as expected
#' @noRd
#' @author Matthew L. Fidler
.udfCall <- function(fun, args) {
  .ret <- try(do.call(fun, args, envir=.udfEnv$envir), silent=TRUE)
  if (inherits(.ret, "try-error")) {
    .msg <- try(attr(.ret, "condition")$message, silent=TRUE)
    if (inherits(.msg, "try-error")) .msg <- "Unknown Error"
    # This can error since it isn't threaded
    stop(paste0(.udfCallFunArg(fun, args), .msg), call.=FALSE)
  }
  if (checkmate::testNumeric(.ret, len=1)) {
    return(as.double(.ret))
  }
  stop(paste0(.udfCallFunArg(fun, args), "needs to return a length 1 numeric"),
       call.=FALSE)
  .ret
}
