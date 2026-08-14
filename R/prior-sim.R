## Reading the `ini({})` block priors for simulation
##
## The specification half of prior support lives in 'lotri', which parses
## `prior(name) ~ dist(...)` and stores it as the `prior` column of
## `$iniDf`.  Nothing here parses a prior; this turns the stored priors
## into the pieces `rxSolve()` needs to simulate from them:
##
## - a `thetaMat` for the population parameters (a NONMEM `$THETAPV`)
## - one degrees of freedom per omega block (a NONMEM `$OMEGAPD`)
##
## A prior must never be silently ignored, so anything this cannot turn
## into one of those is an error naming the parameter.

#' Prior distributions this can simulate from
#'
#' Normal and multivariate normal give the `thetaMat`; the Wishart family
#' gives the omega degrees of freedom.  Every other distribution 'lotri'
#' accepts is a clear error rather than a silent omission.
#'
#' @noRd
.rxPriorSimStanNames <- c(.rxNormalPriorStanNames, .rxOmegaDfStanNames)

#' Parse a stored prior into its pieces
#'
#' @param prior prior as stored in the `prior` column
#' @return list with `fn` (as written), `stanName` and `args` (a list of
#'   language objects), or `NULL` when the text is not a call
#' @noRd
#' @author Matthew L. Fidler
.rxPriorParse <- function(prior) {
  .e <- try(str2lang(prior), silent=TRUE)
  if (inherits(.e, "try-error") || !is.call(.e)) return(NULL)
  .fn <- as.character(.e[[1]])
  if (length(.fn) != 1L) return(NULL)
  list(fn=.fn, stanName=.rxPriorStanName(.fn), args=as.list(.e)[-1])
}

#' Names of the covariance a stored prior carries
#'
#' `multiNormal(mu, lotri(a + b ~ c(...)))` keeps its covariance as the
#' 'lotri' expression that built it, which names every member of the
#' block in order.  This is how the members of a block are recovered,
#' since a block prior is stored only once.
#'
#' @param prior prior as stored in the `prior` column
#' @return character vector of names, or `NULL`
#' @noRd
#' @author Matthew L. Fidler
.rxPriorCovNames <- function(prior) {
  .p <- .rxPriorParse(prior)
  if (is.null(.p)) return(NULL)
  for (.a in .p$args) {
    if (!(is.call(.a) && identical(.a[[1]], quote(`lotri`)))) next
    .b <- .a[[2]]
    if (is.call(.b) && identical(.b[[1]], quote(`{`))) .b <- .b[[2]]
    if (!(is.call(.b) && identical(.b[[1]], quote(`~`)))) return(NULL)
    return(all.vars(.b[[2]]))
  }
  NULL
}

#' Environment a stored prior's arguments are evaluated in
#'
#' A prior is data, not user code, so it is evaluated somewhere with the
#' one function it can legitimately call -- `lotri()`, which is how a
#' covariance is stored -- on top of the base environment, rather than in
#' the caller's frame.
#'
#' @return an environment
#' @noRd
#' @author Matthew L. Fidler
.rxPriorEvalEnv <- function() {
  .e <- new.env(parent=baseenv())
  assign("lotri", lotri::lotri, envir=.e)
  .e
}

#' The covariance matrix a `multiNormal()` prior carries
#'
#' @param prior prior as stored in the `prior` column
#' @return numeric matrix with dimnames, or `NULL`
#' @noRd
#' @author Matthew L. Fidler
.rxPriorCovMat <- function(prior) {
  .p <- .rxPriorParse(prior)
  if (is.null(.p)) return(NULL)
  for (.a in .p$args) {
    if (!(is.call(.a) && identical(.a[[1]], quote(`lotri`)))) next
    .m <- try(as.matrix(eval(.a, envir=.rxPriorEvalEnv())), silent=TRUE)
    if (inherits(.m, "try-error")) return(NULL)
    return(.m)
  }
  NULL
}

#' Does this prior span the omega elements as well as the thetas?
#'
#' A NONMEM `TNPRI` joint block names its omega entries with an `om.`
#' prefix, which is what marks the prior as joint.
#'
#' @param prior prior as stored in the `prior` column
#' @return logical
#' @noRd
#' @author Matthew L. Fidler
.rxPriorIsJoint <- function(prior) {
  vapply(prior, function(p) {
    .nm <- .rxPriorCovNames(p)
    !is.null(.nm) && any(grepl("^om[.].", .nm))
  }, logical(1), USE.NAMES=FALSE)
}

#' Complain about a prior this cannot simulate from
#'
#' @param name parameter name(s)
#' @param prior prior as stored in the `prior` column
#' @param why one line saying what is wrong
#' @return nothing, called for the error
#' @noRd
#' @author Matthew L. Fidler
.rxPriorStop <- function(name, prior, why) {
  stop("cannot simulate from the prior on '", paste(name, collapse="', '"),
       "' (", paste(prior, collapse=", "), "): ", why,
       call.=FALSE)
}

#' The `thetaMat` the population parameter priors describe
#'
#' Each normal prior contributes its variance, and each `multiNormal()`
#' contributes its whole block, so the result is block diagonal over the
#' parameters that carry a prior.  The prior mean has to be the initial
#' estimate: prior simulation samples around what the model says the
#' parameter is, so a prior centered anywhere else is an error rather
#' than a silently different simulation.
#'
#' @param ui rxode2 ui model
#' @return list with `thetaMat` (named matrix, or `NULL`) and `theta`
#'   (data frame of the parameters it covers)
#' @noRd
#' @author Matthew L. Fidler
.rxPriorThetaMat <- function(ui) {
  .iniDf <- ui$iniDf
  .w <- which(is.na(.iniDf$neta1) & !is.na(.iniDf$prior))
  if (length(.w) == 0L) {
    return(list(thetaMat=NULL, theta=NULL))
  }
  .est <- setNames(.iniDf$est, .iniDf$name)
  .blocks <- list()
  .seen <- character(0)
  for (.i in .w) {
    .name <- .iniDf$name[.i]
    .prior <- .iniDf$prior[.i]
    if (.name %in% .seen) next
    .p <- .rxPriorParse(.prior)
    if (is.null(.p) || is.na(.p$stanName)) {
      .rxPriorStop(.name, .prior, "the distribution is not known to 'lotri'")
    }
    if (!(.p$stanName %in% .rxNormalPriorStanNames)) {
      .rxPriorStop(.name, .prior,
                   paste0("only normal and multivariate normal priors can be ",
                          "simulated on a population parameter"))
    }
    if (.p$stanName == "multi_normal") {
      .nm <- .rxPriorCovNames(.prior)
      .cov <- .rxPriorCovMat(.prior)
      if (is.null(.nm) || is.null(.cov)) {
        .rxPriorStop(.name, .prior, "the covariance could not be read back")
      }
      .mu <- try(eval(.p$args[[1]], envir=.rxPriorEvalEnv()), silent=TRUE)
      if (inherits(.mu, "try-error")) {
        .rxPriorStop(.nm, .prior, "the mean vector could not be read back")
      }
      .mu <- rep_len(as.double(.mu), length(.nm))
      .rxPriorAssertMean(.nm, .prior, .mu, .est)
      dimnames(.cov) <- list(.nm, .nm)
      .blocks[[length(.blocks) + 1L]] <- .cov
      .seen <- c(.seen, .nm)
    } else {
      .mu <- 0.0
      .sd <- 1.0
      if (.p$stanName == "normal") {
        .mu <- try(eval(.p$args[[1]], envir=.rxPriorEvalEnv()), silent=TRUE)
        .sd <- try(eval(.p$args[[2]], envir=.rxPriorEvalEnv()), silent=TRUE)
        if (inherits(.mu, "try-error") || inherits(.sd, "try-error")) {
          .rxPriorStop(.name, .prior, "the mean and sd could not be read back")
        }
      }
      .rxPriorAssertMean(.name, .prior, as.double(.mu), .est)
      .m <- matrix(as.double(.sd)^2, 1L, 1L, dimnames=list(.name, .name))
      .blocks[[length(.blocks) + 1L]] <- .m
      .seen <- c(.seen, .name)
    }
  }
  if (length(.blocks) == 0L) {
    return(list(thetaMat=NULL, theta=NULL))
  }
  list(thetaMat=.rxPriorBlockDiag(.blocks),
       theta=data.frame(name=.seen, est=unname(.est[.seen]),
                        stringsAsFactors=FALSE))
}

#' The prior mean has to be the initial estimate
#'
#' @param name parameter name(s)
#' @param prior prior as stored in the `prior` column
#' @param mu numeric mean(s) the prior gives
#' @param est named numeric vector of initial estimates
#' @return nothing, called for the error
#' @noRd
#' @author Matthew L. Fidler
.rxPriorAssertMean <- function(name, prior, mu, est) {
  .e <- unname(est[name])
  .bad <- which(!is.na(.e) & abs(mu - .e) > 1e-8)
  if (length(.bad) == 0L) return(invisible())
  .rxPriorStop(name[.bad], prior,
               paste0("the prior mean (",
                      paste(mu[.bad], collapse=", "),
                      ") is not the initial estimate (",
                      paste(.e[.bad], collapse=", "),
                      "); prior simulation samples around the estimate, so ",
                      "either move the estimate or drop the mean from the prior"))
}

#' Assemble named blocks into one block diagonal matrix
#'
#' @param blocks list of named square matrices
#' @return one named square matrix
#' @noRd
#' @author Matthew L. Fidler
.rxPriorBlockDiag <- function(blocks) {
  .nm <- unlist(lapply(blocks, function(b) dimnames(b)[[1]]), use.names=FALSE)
  .ret <- matrix(0.0, length(.nm), length(.nm), dimnames=list(.nm, .nm))
  for (.b in blocks) {
    .i <- dimnames(.b)[[1]]
    .ret[.i, .i] <- .b
  }
  .ret
}

#' The prior degrees of freedom of each omega block
#'
#' `prior(eta.cl, eta.v) ~ invWishart(4)` says the block itself is the
#' inverse Wishart scale matrix and 4 is its degrees of freedom, which is
#' the `$OMEGAP`/`$OMEGAPD` pair of a NONMEM `NWPRI` model.  'lotri'
#' stores it on the first diagonal element of the block.
#'
#' @param ui rxode2 ui model
#' @return list of `list(names=, nu=)`, one entry per omega block that
#'   carries degrees of freedom; empty when none do
#' @noRd
#' @author Matthew L. Fidler
.rxPriorOmegaNu <- function(ui) {
  .omega <- ui$omega
  if (!is.matrix(.omega) || dim(.omega)[1] == 0L) return(list())
  .iniDf <- ui$iniDf
  .w <- which(!is.na(.iniDf$neta1) & .iniDf$neta1 == .iniDf$neta2)
  .prior <- setNames(.iniDf$prior[.w], .iniDf$name[.w])
  .ret <- list()
  for (.blk in lotri::lotriMatInv(.omega)) {
    .nm <- dimnames(.blk)[[1]]
    .p <- .prior[.nm[1]]
    if (is.na(.p)) next
    .info <- .rxPriorParse(.p)
    if (is.null(.info) || is.na(.info$stanName) ||
          !(.info$stanName %in% .rxOmegaDfStanNames)) {
      next
    }
    .nu <- try(eval(.info$args[[1]], envir=.rxPriorEvalEnv()), silent=TRUE)
    if (inherits(.nu, "try-error") || length(.nu) != 1L || !is.finite(.nu)) {
      .rxPriorStop(.nm, .p, "the degrees of freedom could not be read back")
    }
    ## 'lotri' checks this when the prior is written, but a piped model
    ## can reach here with a block that grew after the prior was set
    if (.nu <= length(.nm) - 1) {
      .rxPriorStop(.nm, .p,
                   paste0("an inverse Wishart on a ", length(.nm), "x",
                          length(.nm), " block needs degrees of freedom ",
                          "greater than ", length(.nm) - 1, ", but ", .nu,
                          " was given"))
    }
    .ret[[length(.ret) + 1L]] <- list(names=.nm, nu=as.double(.nu))
  }
  .ret
}

#' Priors that sit on the omega elements themselves
#'
#' A NONMEM `TNPRI` model puts a normal prior on the omega *values*,
#' either on its own (`om.eta.cl ~ 0.01`, which lands on the omega row)
#' or jointly with the thetas (`tcl + om.eta.cl ~ c(...)`, which lands
#' wherever the block starts).  Both are found here.
#'
#' @param ui rxode2 ui model
#' @return character vector of the parameter names carrying such a prior
#' @noRd
#' @author Matthew L. Fidler
.rxPriorOmegaElements <- function(ui) {
  .iniDf <- ui$iniDf
  .w <- which(!is.na(.iniDf$prior))
  if (length(.w) == 0L) return(character(0))
  .isEl <- !is.na(.iniDf$neta1[.w]) & .rxPriorIsNormal(.iniDf$prior[.w])
  .isJoint <- .rxPriorIsJoint(.iniDf$prior[.w])
  unique(.iniDf$name[.w][.isEl | .isJoint])
}

#' Reject the models this cannot simulate priors for
#'
#' A prior must never be silently ignored, so a model whose priors would
#' not reach the draw is an error rather than a quietly unpriored solve.
#'
#' @param ui rxode2 ui model
#' @param ctl `rxControl` list
#' @return nothing, called for the error
#' @noRd
#' @author Matthew L. Fidler
.rxPriorSimAssertSupported <- function(ui, ctl) {
  .iniDf <- ui$iniDf
  ## A conditioned block (`eta ~ 0.1 | id`, `| occ`) makes `$omega` a
  ## 'lotri' rather than a plain matrix, which routes the solve through
  ## `expandPars_()` -- a path that never reaches the prior draw and
  ## would silently fall back to one degree of freedom per block.
  .cnd <- .iniDf$condition[!is.na(.iniDf$neta1)]
  if (length(.cnd) > 0L && !all(.cnd %in% "id")) {
    stop("prior simulation does not yet support nested/occasion models (the ",
         "omega has the condition(s) '",
         paste(unique(.cnd[!(.cnd %in% "id")]), collapse="', '"),
         "'); see the rxode2 issue tracker",
         call.=FALSE)
  }
  ## A chunked solve pre-draws its parameters in `rxOom` and strips the
  ## omega from what each chunk is given, so the prior would never be
  ## drawn from at all.
  if (!is.null(ctl$file) || !is.null(ctl$chunkSize)) {
    stop("prior simulation does not yet support a chunked solve ('file=' or ",
         "'chunkSize='); see the rxode2 issue tracker",
         call.=FALSE)
  }
  invisible()
}

#' Everything `rxSolve()` needs to simulate from a model's priors
#'
#' @param ui rxode2 ui model
#' @param ctl `rxControl` list
#' @return `NULL` when the model carries no prior this uses, else a list
#'   with `thetaMat`, `theta`, `omegaNu` and `omegaEl`
#' @noRd
#' @author Matthew L. Fidler
.rxPriorSimSpec <- function(ui, ctl=NULL) {
  .iniDf <- ui$iniDf
  if (is.null(.iniDf) || !any(names(.iniDf) == "prior") ||
        !any(!is.na(.iniDf$prior))) {
    return(NULL)
  }
  .rxPriorSimAssertSupported(ui, ctl)
  ## TNPRI is detected here so it is rejected rather than silently
  ## simulated as an ordinary theta, which would be dropped for not
  ## matching a model parameter
  .el <- .rxPriorOmegaElements(ui)
  if (length(.el) > 0L) {
    stop("prior simulation of a normal prior on the omega values (a NONMEM ",
         "'TNPRI') is not implemented yet: '",
         paste(.el, collapse="', '"), "'",
         call.=FALSE)
  }
  .th <- .rxPriorThetaMat(ui)
  .nu <- .rxPriorOmegaNu(ui)
  if (is.null(.th$thetaMat) && length(.nu) == 0L) return(NULL)
  list(thetaMat=.th$thetaMat, theta=.th$theta, omegaNu=.nu, omegaEl=NULL)
}

#' Turn the per-block prior degrees of freedom into a 'lotri' omega
#'
#' `cvPost()` already draws per-block: given a 'lotri' it ignores the
#' scalar `nu` argument and reads each block's own from
#' `attr(omega, "lotri")[[block]]$nu`, then reassembles the full matrix.
#' So the whole of the `NWPRI` omega half is handing it an omega shaped
#' that way, which is what this builds.
#'
#' @param ui rxode2 ui model
#' @param omegaNu list from `.rxPriorOmegaNu()`
#' @return a `lotri` object carrying one `nu` per block, or `NULL` when
#'   no block has degrees of freedom
#' @noRd
#' @author Matthew L. Fidler
.rxPriorOmegaLotri <- function(ui, omegaNu) {
  if (length(omegaNu) == 0L) return(NULL)
  .omega <- ui$omega
  if (!is.matrix(.omega) || dim(.omega)[1] == 0L) return(NULL)
  ## the prior attributes would otherwise travel with the matrix and be
  ## re-read downstream as if they were the block structure
  attributes(.omega) <- attributes(.omega)[c("dim", "dimnames")]
  .nu <- setNames(vapply(omegaNu, function(x) x$nu, double(1)),
                  vapply(omegaNu, function(x) x$names[1], character(1)))
  .blk <- lotri::lotriMatInv(.omega)
  names(.blk) <- paste0("blk", seq_along(.blk))
  ## a block with no prior keeps `nu = 1`, which is how `cvPost()` spells
  ## "leave this one at its point estimate"
  .lst <- setNames(lapply(.blk, function(b) {
    list(nu=unname(.nu[dimnames(b)[[1]][1]]))
  }), names(.blk))
  .lst <- lapply(.lst, function(x) {
    if (is.na(x$nu)) list(nu=1.0) else x
  })
  .ret <- .blk
  attr(.ret, "lotri") <- .lst
  attr(.ret, "start") <- 1L
  class(.ret) <- "lotri"
  .ret
}

#' Should the model's priors drive this solve?
#'
#' The C++ side resolves `simVar = (simVariability == NA) ? (nStud > 1) :
#' simVariability`, and every gate that decides whether a drawn matrix is
#' used hangs off it.  Reproducing it here rather than testing `nStud > 1`
#' keeps the two from disagreeing: `nStud = 1, simVariability = TRUE`
#' really does simulate, and `simVariability = FALSE` really does not.
#'
#' @param ctl `rxControl` list
#' @return logical
#' @noRd
#' @author Matthew L. Fidler
.rxPriorEffSimVar <- function(ctl) {
  .sv <- ctl$simVariability
  if (is.null(.sv) || length(.sv) != 1L || is.na(.sv)) {
    return(isTRUE(ctl$nStud > 1))
  }
  isTRUE(.sv)
}

#' Did this control item come from the model's `meta` block?
#'
#' `.uiRxControl()` merges the `meta` block and the caller's arguments
#' into one list, and by the time the control exists the two look the
#' same -- `...` at that point is the whole expanded control, not what
#' the caller typed.  What still tells them apart is `meta` itself: an
#' item that matches what `meta` holds came from there, and anything else
#' non-default was given at the call site.
#'
#' @param ui rxode2 ui model
#' @param name control item name
#' @param value the value in the control
#' @return logical
#' @noRd
#' @author Matthew L. Fidler
.rxPriorFromMeta <- function(ui, name, value) {
  .meta <- try(ui$meta, silent=TRUE)
  if (!is.environment(.meta) || !exists(name, envir=.meta, inherits=FALSE)) {
    return(FALSE)
  }
  isTRUE(all.equal(get(name, envir=.meta), value))
}

#' Put the model's priors into the solve control
#'
#' @param ui rxode2 ui model
#' @param ctl `rxControl` list
#' @return the amended `rxControl` list
#' @noRd
#' @author Matthew L. Fidler
.rxPriorApplyControl <- function(ui, ctl) {
  .use <- ctl$usePrior
  if (isFALSE(.use)) return(ctl)
  .simVar <- .rxPriorEffSimVar(ctl)
  if (!isTRUE(.use) && !.simVar) return(ctl)
  .spec <- .rxPriorSimSpec(ui, ctl)
  if (is.null(.spec)) {
    if (isTRUE(.use)) {
      stop("'usePrior=TRUE' but the model specifies no prior distributions",
           call.=FALSE)
    }
    return(ctl)
  }
  if (!.simVar) {
    ## `usePrior=TRUE` got here, so say why the priors would vanish rather
    ## than passing them to a solve that draws nothing
    stop("'usePrior=TRUE' but no variability would be simulated; set ",
         "'nStud' greater than 1 or 'simVariability=TRUE'",
         call.=FALSE)
  }
  if (!is.null(.spec$thetaMat)) {
    if (is.null(ctl$thetaMat)) {
      ctl$thetaMat <- .spec$thetaMat
    } else if (.rxPriorFromMeta(ui, "thetaMat", ctl$thetaMat)) {
      warning("the prior distributions in 'ini({})' replace the 'thetaMat' ",
              "the model's 'meta' block carries", call.=FALSE)
      ctl$thetaMat <- .spec$thetaMat
    } else {
      warning("'thetaMat' was given, so the prior distributions on the ",
              "population parameters were not used; drop it or set ",
              "'usePrior=FALSE' to keep it without this warning",
              call.=FALSE)
    }
  }
  .omega <- .rxPriorOmegaLotri(ui, .spec$omegaNu)
  if (!is.null(.omega)) {
    if (is.null(ctl$dfSub) || ctl$dfSub == 0) {
      ctl$priorOmega <- .omega
    } else if (.rxPriorFromMeta(ui, "dfSub", ctl$dfSub)) {
      warning("the prior degrees of freedom in 'ini({})' replace the 'dfSub' ",
              "the model's 'meta' block carries", call.=FALSE)
      ctl$priorOmega <- .omega
    } else {
      warning("'dfSub' was given, so the prior degrees of freedom on the ",
              "omega block(s) were not used; drop it or set ",
              "'usePrior=FALSE' to keep it without this warning",
              call.=FALSE)
    }
  }
  ## wired here even though it stays NULL until the joint (TNPRI) draw
  ## exists, so that adding it is a C++ only change
  ctl$priorOmegaEl <- .spec$omegaEl
  ctl
}
