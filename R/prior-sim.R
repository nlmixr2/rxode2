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

#' What each name a prior can be written on is worth
#'
#' A prior's mean has to be what the model already says the entry is: the
#' initial estimate for a population parameter, and the omega value for an
#' `om.` omega element.  This is the lookup both come from.
#'
#' @param ui rxode2 ui model
#' @return named numeric, keyed by parameter name and by `om.<eta>`
#' @noRd
#' @author Matthew L. Fidler
.rxPriorEstLookup <- function(ui) {
  .iniDf <- ui$iniDf
  .ret <- setNames(.iniDf$est, .iniDf$name)
  .d <- which(!is.na(.iniDf$neta1) & .iniDf$neta1 == .iniDf$neta2)
  if (length(.d) > 0L) {
    .ret <- c(.ret, setNames(.iniDf$est[.d], paste0("om.", .iniDf$name[.d])))
  }
  .ret
}

#' Which omega element does each `om.` name address?
#'
#' `om.eta.cl` is the omega element of `eta.cl`, which is its diagonal --
#' an off diagonal has no `om.` spelling, so a joint prior covers the
#' omega variances rather than every element of the matrix.
#'
#' @param ui rxode2 ui model
#' @param nm character vector of `om.` prefixed names
#' @return data frame with `name`, `neta1` and `neta2`
#' @noRd
#' @author Matthew L. Fidler
.rxPriorOmegaElPos <- function(ui, nm) {
  .iniDf <- ui$iniDf
  .d <- which(!is.na(.iniDf$neta1) & .iniDf$neta1 == .iniDf$neta2)
  .idx <- setNames(.iniDf$neta1[.d], paste0("om.", .iniDf$name[.d]))
  .i <- unname(.idx[nm])
  if (anyNA(.i)) {
    stop("prior given for unknown omega element(s): '",
         paste(nm[is.na(.i)], collapse="', '"), "'", call.=FALSE)
  }
  data.frame(name=nm, neta1=.i, neta2=.i, stringsAsFactors=FALSE)
}

#' The `thetaMat` the model's normal priors describe
#'
#' Each normal prior contributes its variance and each `multiNormal()` its
#' whole block, so the result is block diagonal over the entries that
#' carry a prior.  An `om.` name is an omega element rather than a
#' population parameter, and a block may mix the two -- that joint
#' variance over the thetas and the omega values is what NONMEM calls
#' `TNPRI`.
#'
#' The prior mean has to be what the model already says the entry is,
#' since the draw is added to that value; a prior centered anywhere else
#' is an error rather than a silently different simulation.
#'
#' @param ui rxode2 ui model
#' @return list with `thetaMat` (named matrix, or `NULL`), `theta` (the
#'   entries it covers) and `omegaEl` (the omega elements among them, or
#'   `NULL`)
#' @noRd
#' @author Matthew L. Fidler
.rxPriorThetaMat <- function(ui) {
  .iniDf <- ui$iniDf
  .w <- which(!is.na(.iniDf$prior) &
                (is.na(.iniDf$neta1) | .rxPriorIsNormal(.iniDf$prior)))
  ## an omega row carrying a Wishart is degrees of freedom, not a normal
  ## prior, and is handled by `.rxPriorOmegaNu()`
  .w <- .w[is.na(.iniDf$neta1[.w]) | .rxPriorIsNormal(.iniDf$prior[.w])]
  if (length(.w) == 0L) {
    return(list(thetaMat=NULL, theta=NULL, omegaEl=NULL))
  }
  .est <- .rxPriorEstLookup(ui)
  .blocks <- list()
  .seen <- character(0)
  for (.i in .w) {
    ## a prior on an omega row is on that row's omega *element*, which is
    ## spelled with the `om.` prefix
    .name <- .iniDf$name[.i]
    if (!is.na(.iniDf$neta1[.i])) .name <- paste0("om.", .name)
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
    ## a `std_normal()` has no arguments to read, so it is a unit normal
    ## on something whose value must therefore be zero
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
    return(list(thetaMat=NULL, theta=NULL, omegaEl=NULL))
  }
  .om <- .seen[grepl("^om[.].", .seen)]
  list(thetaMat=.rxPriorBlockDiag(.blocks),
       theta=data.frame(name=.seen, est=unname(.est[.seen]),
                        stringsAsFactors=FALSE),
       omegaEl=if (length(.om) == 0L) NULL else .rxPriorOmegaElPos(ui, .om))
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
         "'); see rxode2 issue #1253",
         call.=FALSE)
  }
  ## A chunked solve pre-draws its parameters in `rxOom` and strips the
  ## omega from what each chunk is given, so the prior would never be
  ## drawn from at all.
  if (!is.null(ctl$file) || !is.null(ctl$chunkSize)) {
    stop("prior simulation does not yet support a chunked solve ('file=' or ",
         "'chunkSize='); see rxode2 issue #1263",
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
  .th <- .rxPriorThetaMat(ui)
  .nu <- .rxPriorOmegaNu(ui)
  if (is.null(.th$thetaMat) && length(.nu) == 0L) return(NULL)
  list(thetaMat=.th$thetaMat, theta=.th$theta, omegaNu=.nu,
       omegaEl=.th$omegaEl)
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
  ## the C++ side matches by row name, so a thetaMat column that gets
  ## pruned before the draw cannot shift the mapping
  if (!is.null(.spec$omegaEl)) {
    ctl$priorOmegaEl <-
      matrix(c(as.integer(.spec$omegaEl$neta1),
               as.integer(.spec$omegaEl$neta2)),
             ncol=2L,
             dimnames=list(.spec$omegaEl$name, c("neta1", "neta2")))
  }
  ctl
}

## Recognizing omega/sigma entries carried in a `thetaMat`
##
## A `thetaMat` from a real covariance step carries the omega and sigma
## entries next to the thetas, and simulating from the whole thing is
## what NONMEM calls `TNPRI`.  Every producer spells those entries
## differently, so all of the spellings in use are recognized:
##
##   nlmixr2est  `om.<eta>`            `cov.<eta1>.<eta2>`
##   nonmem2rx   `<eta>`               `omega<i>.<j>`, `omega.<i>.<j>`
##   ini({})     `om.<eta>`            --
##
## and the sigma equivalents.  An eta name contains dots of its own, so a
## name is matched by *building* the candidates for each element and
## comparing, never by splitting on dots.

#' Every spelling of the `(i, j)` entry of a named matrix
#'
#' @param nm dimnames of the omega/sigma matrix
#' @param i,j element position, 1 based
#' @param what `"omega"` or `"sigma"`
#' @return character vector of names that address that element
#' @noRd
#' @author Matthew L. Fidler
.rxJointElNames <- function(nm, i, j, what="omega") {
  .diagPre <- if (what == "omega") "om." else "sig."
  .idxPre <- if (what == "omega") c("omega", "omega.") else c("sigma", "sigma.")
  if (i == j) {
    ## the bare name is how a covariance step names a variance; the
    ## prefixed one is what nlmixr2est and the `ini({})` block write
    return(c(nm[i], paste0(.diagPre, nm[i]),
             paste0(.idxPre, i, ".", i)))
  }
  ## the matrix is symmetric, so either order addresses the same entry
  c(paste0("cov.", nm[i], ".", nm[j]),
    paste0("cov.", nm[j], ".", nm[i]),
    paste0(.idxPre, i, ".", j),
    paste0(.idxPre, j, ".", i))
}

#' Which `thetaMat` columns are entries of this matrix?
#'
#' @param mat omega/sigma matrix (named)
#' @param cols `thetaMat` column names
#' @param what `"omega"` or `"sigma"`
#' @param diagOnly when `TRUE` a bare eta name is *not* taken as an entry,
#'   which is how the existing separation strategy reads it
#' @return integer matrix of `(neta1, neta2)` with the matching column
#'   name as its row names, or `NULL` when nothing matched
#' @noRd
#' @author Matthew L. Fidler
.rxJointElFromNames <- function(mat, cols, what="omega", bareName=TRUE) {
  if (!is.matrix(mat) || dim(mat)[1] == 0L || length(cols) == 0L) return(NULL)
  .nm <- dimnames(mat)[[1]]
  if (is.null(.nm)) return(NULL)
  .n <- length(.nm)
  .name <- character(0)
  .i1 <- integer(0)
  .i2 <- integer(0)
  for (.i in seq_len(.n)) {
    for (.j in seq_len(.i)) {
      .cand <- .rxJointElNames(.nm, .i, .j, what)
      if (!bareName && .i == .j) .cand <- .cand[-1]
      .w <- which(cols %in% .cand)
      if (length(.w) == 0L) next
      if (length(.w) > 1L) {
        stop("more than one 'thetaMat' column addresses the same ", what,
             " entry (", .nm[.i], ", ", .nm[.j], "): '",
             paste(cols[.w], collapse="', '"), "'", call.=FALSE)
      }
      .name <- c(.name, cols[.w])
      .i1 <- c(.i1, .i)
      .i2 <- c(.i2, .j)
    }
  }
  if (length(.name) == 0L) return(NULL)
  matrix(c(.i1, .i2), ncol=2L,
         dimnames=list(.name, c("neta1", "neta2")))
}

#' Resolve `omegaSeparation`/`sigmaSeparation` of `"tnpri"`
#'
#' `"tnpri"` says the omega (or sigma) entries are carried in the
#' `thetaMat` and should be drawn from it jointly with the thetas, rather
#' than having their correlations redrawn by the separation strategy.
#' That is what a covariance step from NONMEM or nlmixr2 gives, and it
#' keeps the covariances between the thetas and the omega entries that
#' the separation strategy throws away.
#'
#' Resolved in R rather than C++ so the matched column names can be kept
#' out of the pruning that drops a `thetaMat` column the model has no
#' parameter for.
#'
#' @param ctl `rxControl` list
#' @return the amended `rxControl` list
#' @noRd
#' @author Matthew L. Fidler
.rxTnpriApplyControl <- function(ctl) {
  for (.w in c("omega", "sigma")) {
    .sep <- ctl[[paste0(.w, "Separation")]]
    if (!identical(.sep, "tnpri")) next
    .el <- paste0("prior", if (.w == "omega") "Omega" else "Sigma", "El")
    ## a prior in the model's `ini({})` block already said which entries
    ## these are, and it is the more specific statement
    if (!is.null(ctl[[.el]])) next
    if (!inherits(ctl$thetaMat, "matrix")) {
      stop("'", .w, "Separation=\"tnpri\"' needs a 'thetaMat' carrying the ",
           .w, " entries", call.=FALSE)
    }
    .mat <- ctl[[.w]]
    if (!inherits(.mat, "matrix")) {
      stop("'", .w, "Separation=\"tnpri\"' needs '", .w, "' to be a matrix, ",
           "since the drawn entries are added to it", call.=FALSE)
    }
    .pos <- .rxJointElFromNames(.mat, colnames(ctl$thetaMat), what=.w)
    if (is.null(.pos)) {
      stop("'", .w, "Separation=\"tnpri\"' was given but no 'thetaMat' column ",
           "names a ", .w, " entry; expected one of '", dimnames(.mat)[[1]][1],
           "', 'om.", dimnames(.mat)[[1]][1], "' or '", .w, "1.1'",
           call.=FALSE)
    }
    ctl[[.el]] <- .pos
  }
  ctl
}
