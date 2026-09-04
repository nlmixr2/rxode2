## Declared non-Gaussian random effect (eta) distributions.
##
## `lotri` parses and stores the declaration (`dist(eta.cl) ~ dgamma(...)`,
## the `etaDist` column of `$iniDf`); this file turns it into a model.
##
## The technique is Bauer's (NONMEM 7.5.1, `gamma_indpar.pdf`): keep the
## latent random effect standard normal and change the CDF.
##
##   z   ~ N(0, 1)              latent, unit variance, FIXED
##   u   = phiU(z)              normal CDF   ->  U(0, 1)
##   eta = Q(u; args)           inverse CDF of the declared family
##
## Correlation is induced on the LATENT scale through a Cholesky factor,
## which makes it a Gaussian copula.  Bauer estimates that factor directly
## (his `L21`, with `L22 = sqrt(1 - L21^2)`) and so does this: the declared
## correlation block becomes unconstrained `rxCor.*` thetas plus a fixed
## identity omega.
##
## That reparameterization is not cosmetic.  A declared random effect needs
## its omega to be a CORRELATION matrix -- unit diagonal, free off
## diagonals -- and nlmixr2 cannot fix single components of an omega block:
## FOCEi and friends parameterize omega through `rxSymInvCholCreate()`,
## whose Cholesky has no unit-diagonal mode, so a "fixed" diagonal would
## drift as soon as a neighbouring off diagonal moved.  Moving the
## correlation into thetas sidesteps that entirely -- what is left is a
## fixed identity omega, which every estimation method already handles --
## and the fit's correlation matrix is reconstructed afterwards.
##
## Because the rewrite happens here, on the UI, everything downstream
## inherits it unchanged: `rxSolve()` simulation, and (through nlmixr2est's
## pre-processing hook) every estimation method.

#' The random effects that declare a distribution
#'
#' The `etaDist` column only exists when the installed 'lotri' supports
#' declared random effect distributions AND the model uses one, so its
#' absence means "no declarations" rather than an error.
#'
#' @param ui rxode2 ui
#' @return data frame of the declaring random effects, with `name`,
#'   `etaDist`, `neta1` and `condition` columns; zero rows when there are
#'   none
#' @export
#' @author Matthew L. Fidler
rxUiEtaDists <- function(ui) {
  .iniDf <- ui$iniDf
  .empty <- data.frame(name=character(0), etaDist=character(0),
                       neta1=integer(0), condition=character(0),
                       stringsAsFactors=FALSE)
  if (is.null(.iniDf) || !any(names(.iniDf) == "etaDist")) return(.empty)
  .w <- which(!is.na(.iniDf$etaDist) & !is.na(.iniDf$neta1) &
                .iniDf$neta1 == .iniDf$neta2)
  if (length(.w) == 0L) return(.empty)
  data.frame(name=.iniDf$name[.w], etaDist=.iniDf$etaDist[.w],
             neta1=.iniDf$neta1[.w],
             condition=as.character(.iniDf$condition[.w]),
             stringsAsFactors=FALSE)
}

#' @rdname rxUiEtaDists
#' @export
testRxUiEtaDist <- function(ui) {
  nrow(rxUiEtaDists(ui)) > 0L
}

#' @rdname rxUiEtaDists
#' @param extra text appended to the error, naming what cannot use them
#' @export
assertRxUiNoEtaDist <- function(ui, extra="") {
  .d <- rxUiEtaDists(ui)
  if (nrow(.d) > 0L) {
    stop("declared non-normal random effect distribution(s) on '",
         paste(.d$name, collapse="', '"), "' are not supported", extra,
         call.=FALSE)
  }
  invisible(ui)
}

#' Build the inverse CDF expression for one declaration
#'
#' @param txt the declaration as stored, ie `"dgamma(aCl, bCl)"`
#' @param u the expression, as text, that supplies the uniform value
#' @param what the random effect name, for error messages
#' @return character, an rxode2 expression
#' @noRd
#' @author Matthew L. Fidler
.rxEtaDistQuantile <- function(txt, u, what) {
  .call <- str2lang(txt)
  .nm <- as.character(.call[[1]])
  .tab <- lotri::lotriEtaDists()
  .w <- which(.tab$name == .nm)
  if (length(.w) != 1L) {
    stop("'", what, "' declares '", .nm, # nocov
         "', which the installed 'lotri' has no quantile function for", # nocov
         call.=FALSE) # nocov
  }
  .q <- .tab$quantile[.w]
  .args <- as.list(.call)[-1]
  .parNames <- character(0)
  if (nzchar(.tab$parNames[.w])) {
    .parNames <- strsplit(.tab$parNames[.w], ",", fixed=TRUE)[[1]]
  }
  ## lotri stores the arguments in canonical positional order, so the
  ## template's `{name}` placeholders line up by position
  for (.i in seq_along(.args)) {
    .q <- gsub(paste0("{", .parNames[.i], "}"),
               paste0("(", deparse1(.args[[.i]]), ")"), .q, fixed=TRUE)
  }
  .q <- gsub("{u}", u, .q, fixed=TRUE)
  if (grepl("{", .q, fixed=TRUE)) {
    stop("'", what, "' does not supply every argument of '", .nm, "'", # nocov
         call.=FALSE) # nocov
  }
  .q
}

#' Unconstrained Cholesky parameters of a correlation matrix
#'
#' `L <- t(chol(R))` has unit-norm rows when `diag(R) == 1`, so each row
#' can be written with one unconstrained parameter per off diagonal:
#'
#'   L[i, j] = tanh(y[i, j]) * s[i, j - 1],  s[i, j] = s[i, j-1]*sqrt(1 - tanh(y[i,j])^2)
#'
#' with `s[i, 0] = 1` and `L[i, i] = s[i, i - 1]`.  The row norm is one by
#' construction, so `R = L L'` is always an exact correlation matrix no
#' matter what the optimizer does with `y`.  For a 2x2 this is exactly
#' Bauer's `L21`/`L22 = sqrt(1 - L21^2)`, with the bound removed.
#'
#' @param R correlation matrix
#' @return lower triangular matrix of `y` values (zero on and above the
#'   diagonal)
#' @noRd
#' @author Matthew L. Fidler
.rxEtaDistCorToY <- function(R) {
  .k <- dim(R)[1]
  .y <- matrix(0.0, .k, .k)
  if (.k < 2L) return(.y)
  .L <- t(chol(R))
  for (.i in seq(2L, .k)) {
    .s <- 1.0
    for (.j in seq_len(.i - 1L)) {
      .c <- .L[.i, .j] / .s
      .c <- max(-1 + 1e-10, min(1 - 1e-10, .c))
      .y[.i, .j] <- atanh(.c)
      .s <- .s * sqrt(1 - .c * .c)
    }
  }
  .y
}

#' The lines that rebuild one correlated latent normal
#'
#' @param nms the block's random effect names, in block order
#' @param i the row (1 based) to build
#' @return character vector of rxode2 lines
#' @noRd
#' @author Matthew L. Fidler
.rxEtaDistCorLines <- function(nms, i) {
  .z <- paste0("rxz.", nms)
  if (i == 1L) return(paste0("rxN.", nms[1], " <- ", .z[1]))
  .ret <- character(0)
  .terms <- character(0)
  .s <- NULL
  for (.j in seq_len(i - 1L)) {
    .y <- paste0("rxCor.", nms[i], ".", nms[.j])
    .t <- paste0("rxT.", nms[i], ".", nms[.j])
    .l <- paste0("rxL.", nms[i], ".", nms[.j])
    .ret <- c(.ret, paste0(.t, " <- tanh(", .y, ")"))
    .ret <- c(.ret,
              paste0(.l, " <- ", .t, if (is.null(.s)) "" else paste0("*", .s)))
    .sNew <- paste0("rxS.", nms[i], ".", .j)
    .ret <- c(.ret,
              paste0(.sNew, " <- ", if (is.null(.s)) "" else paste0(.s, "*"),
                     "sqrt(1 - ", .t, "*", .t, ")"))
    .terms <- c(.terms, paste0(.l, "*", .z[.j]))
    .s <- .sNew
  }
  .terms <- c(.terms, paste0(.s, "*", .z[i]))
  c(.ret, paste0("rxN.", nms[i], " <- ", paste(.terms, collapse=" + ")))
}

#' Expand declared non-normal random effect distributions into a model
#'
#' Rewrites a ui that carries `dist()` declarations into an ordinary ui:
#' the declared random effects become latent standard normals (a fixed
#' identity omega) plus unconstrained correlation thetas, and the model
#' block gains the `phiU()` + inverse CDF lines that recreate them under
#' their original names.  Everything downstream -- `rxSolve()`, and every
#' nlmixr2est estimation method -- then sees a model it already knows how
#' to handle.
#'
#' A ui with no declaration is returned unchanged.
#'
#' @param ui rxode2 ui
#' @return the rewritten rxode2 ui, or `ui` itself when there is nothing
#'   to expand
#' @export
#' @examples
#'
#' \donttest{
#' one.cmt <- function() {
#'   ini({
#'     lclm <- log(5)
#'     lclrv <- log(0.09)
#'     tv <- 3.45
#'     eta.v ~ 0.1
#'     eta.cl ~ 1
#'     dist(eta.cl) ~ dgamma(shape=1/exp(lclrv),
#'                           rate=1/(exp(lclrv)*exp(lclm)))
#'     add.sd <- 0.7
#'   })
#'   model({
#'     cl <- eta.cl
#'     v <- exp(tv + eta.v)
#'     linCmt() ~ add(add.sd)
#'   })
#' }
#'
#' rxEtaDistExpand(one.cmt())
#' }
#' @author Matthew L. Fidler
rxEtaDistExpand <- function(ui) {
  .ui <- rxUiDecompress(ui)
  .d <- rxUiEtaDists(.ui)
  if (nrow(.d) == 0L) return(ui)
  .iniDf <- .ui$iniDf
  ## IOV and other levels put the random effect in a different condition,
  ## where the latent/copula bookkeeping is not the same problem; refuse
  ## rather than quietly building the wrong model
  .cnd <- unique(lotri::lotriBaseCondition(.d$condition))
  .bad <- .cnd[!(.cnd %in% c("id", "ID", NA_character_))]
  if (length(.bad) > 0L) {
    stop("a declared non-normal random effect distribution is only ",
         "supported at the subject level, but '",
         paste(.d$name[lotri::lotriBaseCondition(.d$condition) %in% .bad],
               collapse="', '"),
         "' is at level '", paste(.bad, collapse="', '"), "'", call.=FALSE)
  }
  .omega <- .ui$omega
  if (!is.matrix(.omega)) .omega <- .omega[[1]]
  .dn <- dimnames(.omega)[[1]]
  ## every block that contains at least one declaration, in block order
  .blocks <- list()
  .i <- 1L
  while (.i <= length(.dn)) {
    .idx <- .rxEtaDistBlock(.omega, .i)
    if (any(.dn[.idx] %in% .d$name)) {
      .blocks[[length(.blocks) + 1L]] <- .idx
    }
    .i <- max(.idx) + 1L
  }
  .pre <- character(0)
  .newTheta <- data.frame(name=character(0), est=numeric(0),
                          stringsAsFactors=FALSE)
  .drop <- integer(0)
  for (.idx in .blocks) {
    .nms <- .dn[.idx]
    .R <- .omega[.idx, .idx, drop=FALSE]
    .y <- .rxEtaDistCorToY(.R)
    for (.i in seq_along(.nms)) {
      .pre <- c(.pre, .rxEtaDistCorLines(.nms, .i))
      for (.j in seq_len(.i - 1L)) {
        .newTheta <- rbind(.newTheta,
                           data.frame(name=paste0("rxCor.", .nms[.i], ".", .nms[.j]),
                                      est=.y[.i, .j], stringsAsFactors=FALSE))
      }
    }
    for (.nm in .nms) {
      .w <- which(.d$name == .nm)
      if (length(.w) == 1L) {
        .u <- paste0("phiU(rxN.", .nm, ")")
        .pre <- c(.pre, paste0(.nm, " <- ",
                               .rxEtaDistQuantile(.d$etaDist[.w], .u, .nm)))
      } else {
        ## an undeclared member of a declared block: its variance is one
        ## by the same rule, so it IS the correlated latent normal
        .pre <- c(.pre, paste0(.nm, " <- rxN.", .nm))
      }
    }
    ## the latent random effects: renamed, unit variance, fixed, and no
    ## covariance -- the correlation is in the `rxCor.*` thetas now
    for (.nm in .nms) {
      .w <- which(.iniDf$name == .nm & .iniDf$neta1 == .iniDf$neta2)
      .iniDf$name[.w] <- paste0("rxz.", .nm)
      .iniDf$est[.w] <- 1
      .iniDf$fix[.w] <- TRUE
    }
    .drop <- c(.drop,
               which(!is.na(.iniDf$neta1) & .iniDf$neta1 != .iniDf$neta2 &
                       .iniDf$neta1 %in% .idx & .iniDf$neta2 %in% .idx))
  }
  if (length(.drop) > 0L) .iniDf <- .iniDf[-.drop, , drop=FALSE]
  .iniDf$etaDist <- NULL
  if (nrow(.newTheta) > 0L) {
    .nTheta <- suppressWarnings(max(c(0L, .iniDf$ntheta), na.rm=TRUE))
    .add <- .iniDf[rep(which(!is.na(.iniDf$ntheta))[1], nrow(.newTheta)), ,
                   drop=FALSE]
    .add$ntheta <- .nTheta + seq_len(nrow(.newTheta))
    .add$name <- .newTheta$name
    .add$est <- .newTheta$est
    .add$lower <- -Inf
    .add$upper <- Inf
    .add$fix <- FALSE
    .add$label <- NA_character_
    ## tanh() of one of these is the partial correlation between its two
    ## random effects given the ones before them (the canonical partial
    ## correlation parameterization), and for a 2x2 block -- the usual
    ## case, and Bauer's -- it is plainly the correlation.  So the
    ## back-transformed column reads as a correlation without any special
    ## casing; `fit$etaDistCor` carries the whole matrix.
    .add$backTransform <- "tanh"
    if (any(names(.add) == "prior")) .add$prior <- NA_character_
    if (any(names(.add) == "err")) .add$err <- NA_character_
    .add$condition <- NA_character_
    rownames(.add) <- NULL
    .iniDf <- rbind(.iniDf, .add)
  }
  ## renumber the etas: dropping the covariance rows leaves gaps
  .we <- which(is.na(.iniDf$ntheta))
  if (length(.we) > 0L) {
    .lvl <- sort(unique(.iniDf$neta1[.we]))
    .iniDf$neta1[.we] <- match(.iniDf$neta1[.we], .lvl)
    .iniDf$neta2[.we] <- match(.iniDf$neta2[.we], .lvl)
  }
  rownames(.iniDf) <- NULL
  .new <- .rxEtaDistNewUi(.ui, .iniDf, c(lapply(.pre, str2lang), .ui$lstExpr))
  ## what the expansion did, so a fit can be reported on the scale the
  ## model was written on: the correlation blocks to rebuild from the
  ## `rxCor.*` thetas, and the declarations themselves
  assign("etaDistInfo",
         list(blocks=lapply(.blocks, function(.idx) .dn[.idx]),
              etaDist=.d, iniDf=.ui$iniDf),
         envir=.new)
  .new
}

#' The indexes of the covariance block an element belongs to
#'
#' @param mat omega
#' @param i index within the block
#' @return integer vector of the block's indexes
#' @noRd
#' @author Matthew L. Fidler
.rxEtaDistBlock <- function(mat, i) {
  .n <- dim(mat)[1]
  .lo <- i
  .hi <- i
  repeat {
    .changed <- FALSE
    if (.lo > 1L && any(mat[seq(.lo, .hi), .lo - 1L] != 0)) {
      .lo <- .lo - 1L
      .changed <- TRUE
    }
    if (.hi < .n && any(mat[seq(.lo, .hi), .hi + 1L] != 0)) {
      .hi <- .hi + 1L
      .changed <- TRUE
    }
    if (!.changed) break
  }
  seq(.lo, .hi)
}

#' Rebuild a ui from a new iniDf and model body
#'
#' @param ui the ui being rewritten (supplies `$meta` and the model name)
#' @param iniDf the new ini data frame
#' @param lstExpr the new list of model expressions
#' @return the new ui
#' @noRd
#' @author Matthew L. Fidler
.rxEtaDistNewUi <- function(ui, iniDf, lstExpr) {
  .ini <- as.expression(lotri::as.lotri(iniDf))
  .ini[[1]] <- quote(`ini`)
  .model <- str2lang(paste0("model({",
                            paste(vapply(lstExpr, deparse1, character(1),
                                         USE.NAMES=FALSE),
                                  collapse="\n"),
                            "})"))
  .ls <- ls(ui$meta, all.names=TRUE)
  .body <- vector("list", length(.ls) + 3L)
  .body[[1]] <- quote(`{`)
  for (.i in seq_along(.ls)) {
    .body[[.i + 1L]] <- str2lang(paste0(.ls[.i], " <- ",
                                        deparse1(ui$meta[[.ls[.i]]])))
  }
  .body[[length(.ls) + 2L]] <- .ini
  .body[[length(.ls) + 3L]] <- .model
  .f <- function() {}
  body(.f) <- as.call(.body)
  .new <- rxUiDecompress(.f())
  ## rebuilding through an anonymous function would otherwise report the
  ## model's name as `.f`
  assign("modelName", ui$modelName, envir=.new)
  .new
}

#' Names used inside `dist()` declarations
#'
#' A declaration's arguments are ordinary `ini({})` parameters, and
#' `rxEtaDistExpand()` writes them into the model's inverse CDF line -- so
#' they are used by the model even though they appear nowhere in the model
#' block until the declaration is expanded.
#'
#' @param iniDf ini data frame
#' @return character vector of every name referenced by a declaration,
#'   plus the declaring random effects themselves
#' @noRd
#' @author Matthew L. Fidler
.rxEtaDistVars <- function(iniDf) {
  if (is.null(iniDf) || !any(names(iniDf) == "etaDist")) return(character(0))
  .w <- which(!is.na(iniDf$etaDist))
  if (length(.w) == 0L) return(character(0))
  unique(c(iniDf$name[.w],
           unlist(lapply(iniDf$etaDist[.w], function(.t) {
             .e <- try(str2lang(.t), silent=TRUE)
             if (inherits(.e, "try-error")) return(character(0)) # nocov
             all.vars(.e)
           }), use.names=FALSE)))
}
