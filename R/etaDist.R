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
  ## accepts a model function as well as a built ui, the way the rest of
  ## the rxUi accessors do
  if (is.function(ui) || inherits(ui, c("rxode2", "rxode2tos"))) {
    ui <- suppressMessages(as.rxUi(ui))
  }
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
#' @param latent the latent normal expression, as text; when given, a
#'   normal-based family is collapsed onto it rather than going through
#'   `phiU()` and back
#' @return character, an rxode2 expression
#' @noRd
#' @author Matthew L. Fidler
.rxEtaDistQuantile <- function(txt, u, what, latent=NULL) {
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
  ## qnorm(phiU(z)) IS z.  A normal-based family therefore collapses to a
  ## plain function of the latent random effect, which is both faster and
  ## more accurate than the round trip through the two CDFs -- and it is
  ## what lets these families translate to software (NONMEM, Monolix) that
  ## has a normal CDF but no inverse for it.
  if (!is.null(latent) && .nm %in% c("dnorm", "stdNormal", "dlnorm")) {
    .q <- sub("qnorm({u})", latent, .q, fixed=TRUE)
  }
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
  if (is.function(ui) || inherits(ui, c("rxode2", "rxode2tos"))) {
    ui <- suppressMessages(as.rxUi(ui))
  }
  .ui <- rxUiDecompress(ui)
  .d <- rxUiEtaDists(.ui)
  if (nrow(.d) == 0L) return(ui)
  .rxEtaDistCheckLevel(.d)
  .iniDf <- .ui$iniDf
  .omega <- .ui$omega
  if (!is.matrix(.omega)) .omega <- .omega[[1]]
  .dn <- dimnames(.omega)[[1]]
  .blocks <- .rxEtaDistDeclBlocks(.omega, .d)
  .assigned <- .rxEtaDistModelAssigned(.ui)
  .pre <- character(0)
  .newTheta <- data.frame(name=character(0), est=numeric(0),
                          stringsAsFactors=FALSE)
  .drop <- integer(0)
  for (.idx in .blocks) {
    .nms <- .dn[.idx]
    .y <- .rxEtaDistCorToY(.omega[.idx, .idx, drop=FALSE])
    .pre <- c(.pre,
              .rxEtaDistCopulaLines(.nms),
              .rxEtaDistDecoderLines(.nms, .d, .assigned))
    .newTheta <- rbind(.newTheta, .rxEtaDistCorTheta(.nms, .y))
    .iniDf <- .rxEtaDistFixLatents(.iniDf, .nms)
    .drop <- c(.drop, .rxEtaDistCovRowsToDrop(.iniDf, .idx))
  }
  if (length(.drop) > 0L) .iniDf <- .iniDf[-.drop, , drop=FALSE]
  .iniDf$etaDist <- NULL
  .iniDf <- .rxEtaDistAddCorThetas(.iniDf, .newTheta)
  .iniDf <- .rxEtaDistRenumberEtas(.iniDf)
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

#' Refuse a declaration that is not at the subject level
#'
#' IOV and other levels put the random effect in a different condition, where
#' the latent/copula bookkeeping is not the same problem; refuse rather than
#' quietly building the wrong model.
#'
#' @param d declaration data.frame from `rxUiEtaDists()`
#' @return nothing, called for the error
#' @noRd
#' @author Matthew L. Fidler
.rxEtaDistCheckLevel <- function(d) {
  .cnd <- unique(lotri::lotriBaseCondition(d$condition))
  .bad <- .cnd[!(.cnd %in% c("id", "ID", NA_character_))]
  if (length(.bad) == 0L) return(invisible())
  stop("a declared non-normal random effect distribution is only ",
       "supported at the subject level, but '",
       paste(d$name[lotri::lotriBaseCondition(d$condition) %in% .bad],
             collapse="', '"),
       "' is at level '", paste(.bad, collapse="', '"), "'", call.=FALSE)
}

#' The omega blocks that contain at least one declaration, in block order
#'
#' @param omega omega matrix
#' @param d declaration data.frame from `rxUiEtaDists()`
#' @return list of integer index vectors
#' @noRd
#' @author Matthew L. Fidler
.rxEtaDistDeclBlocks <- function(omega, d) {
  .dn <- dimnames(omega)[[1]]
  .blocks <- list()
  .i <- 1L
  while (.i <= length(.dn)) {
    .idx <- .rxEtaDistBlock(omega, .i)
    if (any(.dn[.idx] %in% d$name)) {
      .blocks[[length(.blocks) + 1L]] <- .idx
    }
    .i <- max(.idx) + 1L
  }
  .blocks
}

#' Which declared random effects the model block already assigns
#'
#' `dist()` written in model({}) emits its own inverse-CDF line in place --
#' that is the whole point of the model-block form, since a distribution
#' parameter may be an expression the model computes from covariates, and that
#' expression is only in scope at the declaration.  Prepending a second copy
#' would both duplicate the assignment and put it ABOVE the covariate it reads.
#' So if the model already assigns this random effect, its transform is placed
#' and the expansion only owes it the latent and the copula.
#'
#' @param ui decompressed ui
#' @return character vector of assigned names
#' @noRd
#' @author Matthew L. Fidler
.rxEtaDistModelAssigned <- function(ui) {
  .assigned <- character(0)
  for (.e in ui$lstExpr) {
    if (is.call(.e) && length(.e) >= 3L &&
          (identical(.e[[1]], quote(`<-`)) || identical(.e[[1]], quote(`=`))) &&
          is.name(.e[[2]])) {
      .assigned <- c(.assigned, as.character(.e[[2]]))
    }
  }
  .assigned
}

#' The Gaussian copula lines for one block
#'
#' @param nms names in the block, in block order
#' @return character vector of model lines
#' @noRd
#' @author Matthew L. Fidler
.rxEtaDistCopulaLines <- function(nms) {
  unlist(lapply(seq_along(nms), function(.i) .rxEtaDistCorLines(nms, .i)),
         use.names=FALSE)
}

#' The `rxCor.*` thetas for one block, on the atanh scale
#'
#' @param nms names in the block, in block order
#' @param y the atanh-scale partial correlations for the block
#' @return data.frame of name/est
#' @noRd
#' @author Matthew L. Fidler
.rxEtaDistCorTheta <- function(nms, y) {
  .out <- data.frame(name=character(0), est=numeric(0), stringsAsFactors=FALSE)
  for (.i in seq_along(nms)) {
    for (.j in seq_len(.i - 1L)) {
      .out <- rbind(.out,
                    data.frame(name=paste0("rxCor.", nms[.i], ".", nms[.j]),
                               est=y[.i, .j], stringsAsFactors=FALSE))
    }
  }
  .out
}

#' The decoder lines that map each latent back to its declared scale
#'
#' @param nms names in the block, in block order
#' @param d declaration data.frame from `rxUiEtaDists()`
#' @param assigned names the model block already assigns
#' @return character vector of model lines
#' @noRd
#' @author Matthew L. Fidler
.rxEtaDistDecoderLines <- function(nms, d, assigned) {
  .pre <- character(0)
  for (.nm in nms) {
    if (.nm %in% assigned) next   # model-block dist() placed it already
    .w <- which(d$name == .nm)
    if (length(.w) == 1L) {
      .u <- paste0("phiU(rxN.", .nm, ")")
      .pre <- c(.pre, paste0(.nm, " <- ",
                             .rxEtaDistQuantile(d$etaDist[.w], .u, .nm,
                                                latent=paste0("rxN.", .nm))))
    } else {
      ## an undeclared member of a declared block: its variance is one
      ## by the same rule, so it IS the correlated latent normal
      .pre <- c(.pre, paste0(.nm, " <- rxN.", .nm))
    }
  }
  .pre
}

#' Rename a block's random effects to the latents: unit variance and fixed
#'
#' The covariance is not carried here -- the correlation is in the `rxCor.*`
#' thetas now.
#'
#' @param iniDf ini data.frame
#' @param nms names in the block
#' @return the modified ini data.frame
#' @noRd
#' @author Matthew L. Fidler
.rxEtaDistFixLatents <- function(iniDf, nms) {
  for (.nm in nms) {
    .w <- which(iniDf$name == .nm & iniDf$neta1 == iniDf$neta2)
    iniDf$name[.w] <- paste0("rxz.", .nm)
    iniDf$est[.w] <- 1
    iniDf$fix[.w] <- TRUE
  }
  iniDf
}

#' The off-diagonal rows of a block, which the copula replaces
#'
#' @param iniDf ini data.frame
#' @param idx the block's indexes
#' @return integer vector of row numbers
#' @noRd
#' @author Matthew L. Fidler
.rxEtaDistCovRowsToDrop <- function(iniDf, idx) {
  which(!is.na(iniDf$neta1) & iniDf$neta1 != iniDf$neta2 &
          iniDf$neta1 %in% idx & iniDf$neta2 %in% idx)
}

#' Append the `rxCor.*` theta rows to the ini data.frame
#'
#' @param iniDf ini data.frame
#' @param newTheta data.frame of name/est from `.rxEtaDistCorTheta()`
#' @return the modified ini data.frame
#' @noRd
#' @author Matthew L. Fidler
.rxEtaDistAddCorThetas <- function(iniDf, newTheta) {
  if (nrow(newTheta) == 0L) return(iniDf)
  .nTheta <- suppressWarnings(max(c(0L, iniDf$ntheta), na.rm=TRUE))
  .add <- iniDf[rep(which(!is.na(iniDf$ntheta))[1], nrow(newTheta)), ,
                drop=FALSE]
  .add$ntheta <- .nTheta + seq_len(nrow(newTheta))
  .add$name <- newTheta$name
  .add$est <- newTheta$est
  ## Bounded, not unbounded.  tanh() maps this to a partial correlation, so
  ## the parameterization is unconstrained in the sense that ANY finite value
  ## gives a valid correlation matrix -- but that is not the same as being
  ## safe to optimize over.  As |y| grows tanh(y) -> 1, the block approaches
  ## singularity, and a copula member's latent
  ##
  ##   w_k = tanh(y)*z_j + sqrt(1 - tanh(y)^2)*z_k
  ##
  ## collapses onto its partner's: two declared random effects become one.
  ## Any optimizer maximizing a likelihood CONDITIONAL on sampled etas -- with
  ## no prior term to penalize that degeneracy -- can walk straight to it.
  ## Measured in nlmixr2est's saem (refinePhi0Lik): rho pinned at 1.000 in 3
  ## of 7 fits across seeds and refinement start points on Bauer's gamma data,
  ## and a pinned rho alone contributed 128% of one of the eight relative
  ## errors.
  ##
  ## +/-5 keeps |rho| <= 0.9999 -- far wider than any correlation worth
  ## estimating, and enough that sqrt(1 - rho^2) never underflows the partner
  ## latent out of the model entirely.
  .add$lower <- -5
  .add$upper <- 5
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
  rbind(iniDf, .add)
}

#' Renumber the etas: dropping the covariance rows leaves gaps
#'
#' @param iniDf ini data.frame
#' @return the modified ini data.frame
#' @noRd
#' @author Matthew L. Fidler
.rxEtaDistRenumberEtas <- function(iniDf) {
  .we <- which(is.na(iniDf$ntheta))
  if (length(.we) == 0L) return(iniDf)
  .lvl <- sort(unique(iniDf$neta1[.we]))
  iniDf$neta1[.we] <- match(iniDf$neta1[.we], .lvl)
  iniDf$neta2[.we] <- match(iniDf$neta2[.we], .lvl)
  iniDf
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

#' Substitute a variable inside a model expression
#'
#' The left-hand side of an assignment is left alone; a target is only ever a
#' theta, and a theta is never assigned to in a model block, but skipping it
#' keeps the substitution honest about what it is allowed to touch.
#'
#' @param e expression to walk
#' @param map named list of replacements, keyed by variable name
#' @return `e` with every mapped name replaced
#' @noRd
#' @author Matthew L. Fidler
.rxEtaDistSubVar <- function(e, map) {
  if (is.name(e)) {
    .n <- as.character(e)
    if (!is.null(map[[.n]])) return(map[[.n]])
    return(e)
  }
  if (!is.call(e)) return(e)
  .start <- 2L
  if (length(e) > 2L && identical(e[[1]], quote(`<-`))) .start <- 3L
  if (length(e) >= .start) {
    for (.i in seq.int(.start, length(e))) {
      e[[.i]] <- .rxEtaDistSubVar(e[[.i]], map)
    }
  }
  e
}

#' Mu-reference the parameters of a declared eta distribution
#'
#' `rxEtaDistExpand()` writes the declared distribution's parameters into the
#' model as bare thetas inside an inverse-CDF call.  Nothing about that shape is
#' `theta + eta`, so every one of them comes out **non**-mu-referenced -- which
#' is the case both `saem` and the FOCEi family handle worst, and it is why a
#' cold-started fit of a declared-distribution model tends to settle a long way
#' from the answer.
#'
#' This carries each of those parameters on its own random effect with a small
#' FIXED variance, which is what puts them back into a `theta + eta` form and so
#' back onto the mu-referenced path.  It is the same structure NONMEM control
#' streams get from `MU_5 = THETA(5)` with `$OMEGA (0.0 FIXED)`, with one
#' important difference: the helper variance must **not** be ~0 here.
#' nlmixr2's mu-theta M-step is weighted by `omega^-1`, so a ~0 variance pins
#' the parameter at its starting value instead of freeing it (NONMEM updates
#' such a parameter by direct maximization, so the idiom works there).
#'
#' The result is a **different model** -- the helper variance is real
#' between-subject variability on the distribution's parameters -- so this is a
#' way to travel, not a way to finish.  Use it as the first stage of a chain and
#' refit the model you actually mean from its estimates:
#'
#' ```
#' stage1 <- nlmixr2(rxEtaDistMuRef(mod), data, est = "saem")
#' final  <- nlmixr2(mod |> ini(stage1), data, est = "focei",
#'                   control = foceiControl(mceta = 100))
#' ```
#'
#' Measured on Bauer's gamma-distributed CL/V1 data (300 subjects), that chain
#' recovers the structural parameters essentially exactly (CL 5.04 against a
#' simulation truth of 5.03, Q 2.15 against 2.13) where a cold start of either
#' method alone does not.  Stage two has to be a gradient method: `saem` as the
#' second stage moved the residual error further from the truth than stage one
#' had it.
#'
#' @param ui rxode2 model with at least one `dist()` declaration
#' @param variance variance to fix each helper random effect at.
#'
#'   **`variance = 0` is the preferred spelling**, and it is NONMEM's own:
#'   Bauer's control streams mu-reference every distribution parameter and put
#'   each helper on `$OMEGA (0.0 FIXED)`.  It declares what is true -- the
#'   helper carries no between-subject variability -- and hands the question of
#'   what to do about that to the estimation method, where
#'   `nlmixr2est::saemControl(zeroOmegaTune=, zeroOmegaAnneal=,
#'   zeroOmegaDirect=)` can act on it.
#'
#'   A NONZERO value writes a sampling width into the model itself and bypasses
#'   that machinery entirely.  It is what this function did before saem had a
#'   direct-maximization M-step for these thetas, and it is kept because it
#'   still works: the helper has to MOVE, or the conditional mean saem shifts
#'   its theta by is identically zero and the theta never budges.
#'
#'   Wider is not generally better.  Measured on Bauer's gamma model (300
#'   subjects, cold start) widening degraded every parameter monotonically:
#'   at 0.1 / 1 / 4 the residual SD came out 0.150 / 0.162 / 0.170 against a
#'   truth of 0.141, and Q came out 2.29 / 2.48 / 2.60 against 2.13.  0.1
#'   recovered CL 5.60 and V1 4.77 against truths of 5.03 and 4.66.  That a
#'   constant cannot be right twice -- wide enough early to explore, tight
#'   enough late to settle -- is what `zeroOmegaAnneal=` addresses.
#' @return an rxode2 model, already expanded, whose declared-distribution
#'   parameters are mu-referenced
#' @export
#' @author Matthew L. Fidler
#' @examples
#' \donttest{
#' mod <- function() {
#'   ini({
#'     lclm <- log(5)
#'     lclrv <- log(0.09)
#'     tv <- 3.45
#'     dist(eta.cl) ~ dgamma(shape = 1 / exp(lclrv),
#'                           rate = 1 / (exp(lclrv) * exp(lclm)))
#'     add.sd <- 0.7
#'   })
#'   model({
#'     cl <- eta.cl
#'     v <- exp(tv)
#'     linCmt() ~ add(add.sd)
#'   })
#' }
#' rxEtaDistMuRef(mod)
#' }
rxEtaDistMuRef <- function(ui, variance = 0.1) {
  .ui <- rxUiDecompress(assertRxUi(ui))
  if (nrow(rxUiEtaDists(.ui)) == 0L) {
    stop("'rxEtaDistMuRef()' needs a model with at least one 'dist()' declaration in 'ini({})'",
         call.=FALSE)
  }
  checkmate::assertNumeric(variance, lower=0, len=1, any.missing=FALSE,
                           .var.name="variance")
  ## `variance = 0` is NONMEM's own spelling of this idiom -- Bauer's control
  ## streams mu-reference every distribution parameter and give each helper
  ## `$OMEGA (0.0 FIXED)` -- and nlmixr2 now recognizes it: a mu-referenced
  ## random effect declared fix(0) is routed to
  ## `nlmixr2est:::.preProcessZeroOmegaMuRef()`, which substitutes
  ## `saemControl(zeroOmegaTune=)` as a sampling width and, with
  ## `saemControl(zeroOmegaDirect=TRUE)`, updates the theta by directly
  ## maximizing the observation likelihood instead of by the omega^-1-weighted
  ## regression that cannot move it.  A nonzero `variance` writes the width
  ## into the model itself instead, bypassing that machinery.
  .declared <- .ui$iniDf$name[!is.na(.ui$iniDf$etaDist)]
  .exp <- rxEtaDistExpand(.ui)
  .ini <- .exp$iniDf
  ## Thetas the expansion put inside an inverse-CDF/copula line -- that is,
  ## exactly the ones that came out non-mu-referenced.  Read off the generated
  ## lines rather than re-deriving them, so this cannot drift from what
  ## rxEtaDistExpand() actually wrote.
  .lst <- .exp$lstExpr
  .lhs <- vapply(.lst, function(.l) {
    if (is.call(.l) && length(.l) > 2L && identical(.l[[1]], quote(`<-`)) &&
          is.name(.l[[2]])) as.character(.l[[2]]) else ""
  }, character(1), USE.NAMES=FALSE)
  ## Every line rxEtaDistExpand() generates: the copula intermediates
  ## (rxT./rxL./rxS./rxN.), the uniform (rxu./rxU.), and the assignment to the
  ## declared eta itself.  The copula correlation theta only ever appears on an
  ## rxT. line, so missing that prefix silently leaves it non-mu-referenced --
  ## which is exactly the parameter NONMEM mu-references as MU_9.
  .isDistLine <- .lhs %in% .declared | grepl("^rx[NTLSUuc]\\.", .lhs)
  .vars <- unique(unlist(lapply(.lst[.isDistLine], all.vars), use.names=FALSE))
  .thetas <- .ini$name[!is.na(.ini$ntheta) & is.na(.ini$err) & !.ini$fix]
  .target <- intersect(.vars, .thetas)
  ## The copula correlation thetas are created BY the expansion and only ever
  ## appear in a generated line, so they are picked up above; keep them in a
  ## stable order alongside the declaration parameters.
  if (length(.target) == 0L) return(.exp)
  .helper <- paste0("eta.mu.", .target)
  .map <- stats::setNames(
    lapply(seq_along(.target),
           function(.i) str2lang(paste0("(", .target[.i], " + ", .helper[.i], ")"))),
    .target)
  .newLst <- lapply(.lst, .rxEtaDistSubVar, map=.map)
  ## Rebuild rather than pipe: the helper etas do not exist in ini() until the
  ## model block mentions them, and the model block cannot mention them until
  ## they exist, so the two have to be written at the same time.
  .iniTxt <- deparse(.exp$iniFun)
  .iniTxt <- .iniTxt[-length(.iniTxt)]           # drop the closing "})"
  .iniTxt <- c(.iniTxt,
               paste0("  ", .helper, " ~ fix(", variance, ")"),
               "})")
  .modTxt <- vapply(.newLst, function(.l) paste0("  ", deparse1(.l)),
                    character(1), USE.NAMES=FALSE)
  .txt <- paste0("function() {\n",
                 paste(.iniTxt, collapse="\n"), "\n",
                 "model({\n", paste(.modTxt, collapse="\n"), "\n})\n}")
  .fun <- try(eval(parse(text=.txt)), silent=TRUE)
  if (inherits(.fun, "try-error")) {
    message(.txt)
    stop("could not mu-reference the declared distribution parameters; the model this tried to build is echoed above",
         call.=FALSE)
  }
  rxUiDecompress(rxode2(.fun))
}

#' Declare a non-Gaussian random effect distribution in the model block
#'
#' The `model({})` form of `ini({})`'s `dist()` line:
#'
#'     dist(eta.cl) ~ dgamma(shape = 1/exp(lclrv), rate = 1/(exp(lclrv)*aCl))
#'
#' Reached through [rxUdfUiLhs()], a user-function dispatch on the LEFT of a
#' model line -- `dist(eta.cl)` is not rxode2 grammar and `~` is already
#' overloaded, so the UI claims the whole line before rxode2 ever sees it.
#'
#' The point of the model-block form is that a distribution parameter can be
#' any expression the model has already computed, including one built from
#' covariates:
#'
#'     aCl <- exp(lclm + bWT*(WT - 70))
#'     dist(eta.cl) ~ dgamma(shape = 1/exp(lclrv), rate = 1/(exp(lclrv)*aCl))
#'
#' The `ini({})` form cannot express that -- it is parsed before the model
#' block exists, so its arguments can only name population parameters.  This
#' one emits the inverse-CDF line IN PLACE, at the declaration, so everything
#' above it is in scope.
#'
#' What it leaves for [rxEtaDistExpand()] is the part that needs the whole
#' picture rather than one line: the latent normals, the Gaussian copula that
#' correlates them, and the `rxCor.*` thetas.  Those are prepended, so the
#' `rxN.*` this line reads are defined above it.
#'
#' @param fun the `dist(<eta>)` call
#' @param rhs the declared distribution, eg `dgamma(shape=a, rate=b)`
#' @return `rxUdfUiLhs()` list: the modified `iniDf` and the inverse-CDF line
#'   that replaces the declaration
#' @export
#' @author Matthew L. Fidler
rxUdfUiLhs.dist <- function(fun, rhs) {
  if (length(fun) != 2L) {
    stop("'dist()' takes exactly one random effect, as in 'dist(eta.cl)'",
         call.=FALSE)
  }
  .eta <- fun[[2]]
  if (!is.name(.eta)) {
    stop("'dist()' takes a random effect name, as in 'dist(eta.cl)'",
         call.=FALSE)
  }
  .eta <- as.character(.eta)
  .iniDf <- rxUdfUiIniDf()
  if (is.null(.iniDf)) {
    stop("'dist(", .eta, ")' needs the initial estimates to be available",
         call.=FALSE)
  }
  .w <- which(.iniDf$name == .eta & !is.na(.iniDf$neta1) &
                .iniDf$neta1 == .iniDf$neta2)
  if (length(.w) == 0L) {
    ## Not declared in ini({}) -- add it.  A declared distribution supplies its
    ## own spread, so the only variance this random effect could have been given
    ## is 1; making the user write `eta.cl ~ 1` alongside `dist(eta.cl)` asks
    ## them to repeat the one value the declaration already implies.  Added
    ## silently for the same reason: there is nothing for them to decide.
    ##
    ## `eta.cl + eta.v1 ~ c(1, 0.5, 1)` in ini({}) is still how a COPULA is
    ## written, because a correlation between two random effects is a real
    ## choice and cannot be inferred from either declaration alone.
    .iniDf <- .rxEtaDistAddEta(.iniDf, .eta)
    .w <- which(.iniDf$name == .eta & !is.na(.iniDf$neta1) &
                  .iniDf$neta1 == .iniDf$neta2)
  } else {
    ## Declared, so it must have been declared with a unit variance -- the
    ## latent IS a standard normal, and any other value is a spread the
    ## transformation has no way to honour: the family's own parameters set the
    ## spread, and phiU() assumes N(0,1) going in.
    .est <- .iniDf$est[.w]
    if (!isTRUE(is.finite(.est)) || abs(.est - 1) > 1e-8) {
      stop("'dist(", .eta, ")' needs '", .eta, "' to have a variance of 1, but ",
           "ini({}) declares ", format(.est), ".  A declared distribution ",
           "supplies its own spread through its parameters, so the underlying ",
           "random effect is a standard normal -- write '", .eta, " ~ 1' ",
           "(or leave it out entirely and it will be added)", call.=FALSE)
    }
  }
  if (!is.call(rhs) || !is.name(rhs[[1]])) {
    stop("'dist(", .eta, ")' must be given a distribution, as in ",
         "'dist(", .eta, ") ~ dgamma(shape=a, rate=b)'", call.=FALSE)
  }
  .fam <- as.character(rhs[[1]])
  .tab <- lotri::lotriEtaDists()
  .fw <- which(.tab$name == .fam)
  if (length(.fw) != 1L) {
    stop("'dist(", .eta, ")' declares '", .fam,
         "', which is not a distribution the installed 'lotri' knows",
         call.=FALSE)
  }
  .nArg <- length(as.list(rhs)) - 1L
  if (.nArg != .tab$nReq[.fw]) {
    stop("'dist(", .eta, ") ~ ", .fam, "()' needs ", .tab$nReq[.fw],
         " argument(s), not ", .nArg, call.=FALSE)
  }
  ## The declaration itself, recorded exactly as ini({})'s dist() records it,
  ## so rxUiEtaDists(), $etaDist and the babelmixr2 "native" path all read one
  ## representation regardless of which block it was written in.
  ##
  ## NORMALIZED, not deparsed as written.  Every consumer of `$etaDist`
  ## substitutes the arguments POSITIONALLY -- .rxEtaDistQuantile() below,
  ## nlmixr2est's .etaDistMstepCore(), its C++ RPN parser, the warm start --
  ## while ini({}) stores lotri's canonical order.  Storing the user's order
  ## instead meant `dgamma(rate = r, shape = s)` written in model({}) fitted a
  ## DIFFERENT DISTRIBUTION than written, silently, everywhere at once:
  ##
  ##   as written  cl <- gammapInv((1/exp(lclrv)), ...)/((1/(exp(lclrv)*exp(lclm))))
  ##   swapped     cl <- gammapInv((1/(exp(lclrv)*exp(lclm))), ...)/((1/exp(lclrv)))
  ##
  ## Normalizing at the STORAGE point fixes every consumer from one place.
  if (!any(names(.iniDf) == "etaDist")) .iniDf$etaDist <- NA_character_
  .rhsTxt <- .rxEtaDistNormalizeTxt(rhs, .eta)
  .iniDf$etaDist[.w] <- .rhsTxt
  ## A declared distribution supplies its own spread, so the latent is a
  ## standard normal with a FIXED unit variance -- the same rule the ini({})
  ## form applies, and what makes the correlation a Gaussian copula.
  .iniDf$est[.w] <- 1
  .iniDf$fix[.w] <- TRUE
  list(iniDf = .iniDf,
       replace = paste0(.eta, " <- ",
                        .rxEtaDistQuantile(.rhsTxt,
                                           paste0("phiU(rxN.", .eta, ")"),
                                           .eta,
                                           latent = paste0("rxN.", .eta))))
}

#' Canonical text for a declared distribution call
#'
#' `lotri::lotriEtaDistNormalize()` matches the arguments by NAME to the
#' family's canonical order and returns canonical positional text -- the same
#' normalization an `ini({})` declaration receives.  Falls back to the deparsed
#' call when the installed lotri is older than that export, but WARNS first if
#' any argument is named, since that is exactly the case the fallback gets
#' wrong.
#'
#' @param rhs the declared distribution call
#' @param eta the random effect being declared, for the message
#' @return canonical positional text
#' @noRd
#' @author Matthew L. Fidler
.rxEtaDistNormalizeTxt <- function(rhs, eta) {
  if (!is.null(getFromNamespace0("lotriEtaDistNormalize", "lotri"))) {
    .n <- try(lotri::lotriEtaDistNormalize(rhs), silent=TRUE)
    if (!inherits(.n, "try-error") && is.character(.n$text) &&
          length(.n$text) == 1L) {
      return(.n$text)
    }
  }
  .nm <- names(as.list(rhs)[-1])
  if (!is.null(.nm) && any(nzchar(.nm))) {
    warning("'dist(", eta, ")' has named arguments but the installed 'lotri' ",
            "cannot normalize them; they are matched POSITIONALLY, so write ",
            "them in the family's own order to be safe", call.=FALSE)
  }
  deparse1(rhs)
}

#' `getFromNamespace()` that returns NULL instead of erroring
#' @noRd
getFromNamespace0 <- function(x, ns) {
  tryCatch(utils::getFromNamespace(x, ns), error=function(e) NULL)
}

#' Add a latent random effect the model block declared a distribution for
#'
#' Built from a row the `iniDf` already has rather than from a template, so the
#' columns match whatever this `iniDf` actually carries -- an `etaDist` column
#' is present on some and not others, and a column-count mismatch is how
#' `rbind()` fails here.
#'
#' @param iniDf initial estimates
#' @param name random effect to add
#' @return `iniDf` with the random effect appended, unit variance and fixed
#' @noRd
#' @author Matthew L. Fidler
.rxEtaDistAddEta <- function(iniDf, name) {
  .we <- which(!is.na(iniDf$neta1) & iniDf$neta1 == iniDf$neta2)
  .row <- if (length(.we) > 0L) iniDf[.we[1L], , drop=FALSE] else iniDf[1L, , drop=FALSE]
  .n <- suppressWarnings(max(c(0, iniDf$neta1, iniDf$neta2), na.rm=TRUE))
  if (!is.finite(.n)) .n <- 0
  .row$ntheta <- NA_integer_
  .row$neta1 <- .n + 1
  .row$neta2 <- .n + 1
  .row$name <- name
  .row$lower <- -Inf
  .row$upper <- Inf
  .row$est <- 1
  .row$fix <- TRUE
  .row$label <- NA_character_
  .row$backTransform <- NA_character_
  ## the subject level, which is where a declared distribution is supported
  .row$condition <- "id"
  for (.c in c("prior", "err", "etaDist")) {
    if (any(names(.row) == .c)) .row[[.c]] <- NA_character_
  }
  rownames(.row) <- NULL
  .out <- rbind(iniDf, .row)
  rownames(.out) <- NULL
  .out
}
