#' Parameters specified by the model
#'
#' This returns the model's parameters that are required to solve the
#' ODE system, and can be used to pipe parameters into an rxode2 solve
#'
#' @inheritParams rxModelVars
#'
#' @param constants is a boolean indicting if constants should be
#'     included in the list of parameters. Currently rxode2 parses
#'     constants into variables in case you wish to change them
#'     without recompiling the rxode2 model.
#'
#' @inheritParams rxControl
#'
#' @return When extracting the parameters from an rxode2 model, a
#'     character vector listing the parameters in the model.
#'
#' @author Matthew L.Fidler
#' @export
rxParams <- function(obj, ...) {
  UseMethod("rxParams")
}

#' @rdname rxParams
#' @export
rxParams.rxode2 <- function(obj, constants = TRUE, ...,
                           params = NULL, inits = NULL, iCov = NULL,
                           keep = NULL,
                           thetaMat = NULL,
                           omega = NULL, dfSub = NULL,
                           sigma = NULL, dfObs = NULL,
                           nSub = NULL, nStud = NULL) {
  if (!is.null(iCov)) {
    stop("'iCov' in a pipline is no longer supported", call. = FALSE)
  }
  .ret <- list(
    params = params, inits = inits, keep = keep,
    thetaMat = thetaMat,
    omega = omega, dfSub = dfSub,
    sigma = sigma, dfObs = dfObs,
    nSub = nSub, nStud = nStud
  )
  if (all(sapply(seq_along(.ret), function(x) {
    is.null(.ret[[x]])
  }))) {
    if (length(list(...)) > 0) {
      .clearPipe()
      stop("unknown arguments in 'rxParams'", call. = FALSE)
    }
    return(rxParams.default(obj, constants = constants))
  } else {
    .lst <- list(...)
    if (length(.lst) > 0) {
      .clearPipe()
      stop(sprintf(
        gettext("unknown arguments in 'rxParams': %s\ntry piping to 'rxSolve'"),
        paste(names(.lst), collapse = ", ")
      ), call. = FALSE)
    }
    ## Most likely
    ## rxode2() %>% rxParams() %>%
    assignInMyNamespace(".pipelineRx", obj)
    assignInMyNamespace(".pipelineInits", NULL)
    assignInMyNamespace(".pipelineEvents", NULL)
    assignInMyNamespace(".pipelineParams", NULL)
    assignInMyNamespace(".pipelineKeep", NULL)
    assignInMyNamespace(".pipelineThetaMat", NULL)
    assignInMyNamespace(".pipelineOmega", NULL)
    assignInMyNamespace(".pipelineSigma", NULL)
    assignInMyNamespace(".pipelineDfObs", NULL)
    assignInMyNamespace(".pipelineDfSub", NULL)
    assignInMyNamespace(".pipelineNSub", NULL)
    assignInMyNamespace(".pipelineNStud", NULL)


    class(.ret) <- "rxParams"
    return(.ret)
  }
}

#' @rdname rxParams
#' @export
rxParams.rxSolve <- function(obj, constants = TRUE, ...,
                             params = NULL, inits = NULL, iCov = NULL,
                             keep = NULL,
                             thetaMat = NULL,
                             omega = NULL, dfSub = NULL,
                             sigma = NULL, dfObs = NULL,
                             nSub = NULL, nStud = NULL) {
  if (!is.null(iCov)) {
    stop("'iCov' in a pipline is no longer supported", call. = FALSE)
  }
  .ret <- list(
    params = params, inits = inits, keep = keep,
    thetaMat = thetaMat,
    omega = omega, dfSub = dfSub,
    sigma = sigma, dfObs = dfObs,
    nSub = nSub, nStud = nStud
  )
  if (all(sapply(seq_along(.ret), function(x) {
    is.null(.ret[[x]])
  }))) {
    if (length(list(...)) > 0) {
      .clearPipe()
      stop("unknown arguments in 'rxParams'", call. = FALSE)
    }
    return(rxParams.default(obj, constants = constants))
  } else {
    .lst <- list(...)
    if (length(.lst) > 0) {
      .clearPipe()
      stop(sprintf(
        gettext("unknown arguments in 'rxParams': %s\ntry piping to 'rxSolve'"),
        paste(names(.lst), collapse = ", ")
      ),
      call. = FALSE
      )
    }
    ## Most likely
    ## solveObject %>% rxParams() %>%
    .x <- obj
    ## Assign prior information
    ## Need to extract:
    ## 1. rxode2 model
    assignInMyNamespace(".pipelineRx", .x$.args.object)
    ## Events
    assignInMyNamespace(".pipelineEvents", .x$.args.events)
    ## 2. rxode2 parameters
    assignInMyNamespace(".pipelineParams", .x$.args.par0)
    ## 3. rxode2 inits
    assignInMyNamespace(".pipelineInits", .x$.args.inits)
    ## 4. rxode2 thetaMat
    assignInMyNamespace(".pipelineThetaMat", .x$.args$thetaMat)
    ## 5. rxode2 omega
    assignInMyNamespace(".pipelineOmega", .x$.args$omega)
    ## 6. rxode2 sigma
    assignInMyNamespace(".pipelineSigma", .x$.args$sigma)
    ## 7. rxode2 dfObs
    assignInMyNamespace(".pipelineDfObs", .x$env$.args$dfObs)
    ## 8. rxode2 dfSub
    assignInMyNamespace(".pipelineDfSub", .x$env$.args$dfSub)
    class(.ret) <- "rxParams"
    return(.ret)
  }
}

#' @rdname rxParams
#' @export
rxParams.rxEt <- function(obj, ...,
                          params = NULL, inits = NULL, iCov = NULL,
                          keep = NULL,
                          thetaMat = NULL,
                          omega = NULL, dfSub = NULL,
                          sigma = NULL, dfObs = NULL,
                          nSub = NULL, nStud = NULL) {
  if (!is.null(iCov)) {
    stop("'iCov' in a pipline is no longer supported", call. = FALSE)
  }
  # et() %>% rxParams() %>%
  assignInMyNamespace(".pipelineEvents", obj)
  .lst <- list(...)
  if (length(.lst) > 0) {
    .clearPipe()
    stop(sprintf(gettext("unknown arguments in 'rxParams': %s\ntry piping to 'rxSolve'"), paste(names(.lst), collapse = ", ")),
      call. = FALSE
    )
  }
  .ret <- list(
    params = params, inits = inits, keep = keep,
    thetaMat = thetaMat, omega = omega, dfSub = dfSub,
    sigma = sigma, dfObs = dfObs,
    nSub = nSub, nStud = nStud
  )
  class(.ret) <- "rxParams"
  return(.ret)
}

.rxParams <- function(obj, constants = TRUE) {
  .ret <- rxParams_(obj)
  if (!constants) {
    .init <- rxode2::rxInit(obj)
    .ret <- .ret[!(.ret %in% names(.init))]
  }
  return(.ret)
}
#' @export
rxParams.default <- function(obj, ..., constants = TRUE) {
  if (!missing(obj)) {
    return(.rxParams(obj, constants))
  } else {
    .lst <- list(...)
    .nm <- c(
      "cov", "params", "inits", "keep",
      "thetaMat", "omega", "dfSub",
      "sigma", "dfObs", "nSub", "nStud"
    )
    .ret <- lapply(.nm, function(x) {
      return(.lst[[x]])
    })
    names(.ret) <- .nm
    class(.ret) <- "rxParams"
    return(.ret)
  }
}

#' @rdname rxParams
#' @export
rxParam <- rxParams
