#' Fork a model's meta environment for piping
#'
#' Shallow copy: only the meta environment itself is new, so the piped and
#' original model no longer share the `.simModelBase` cache slot (which is
#' dropped here, as in .copyEnv/.copyUi).  Bindings are carried over as-is --
#' nested environments stay shared and closures keep their environment -- so
#' reference-semantics metadata behaves exactly as it did when meta was shared.
#'
#' @param env meta environment to fork
#'
#' @return New meta environment
#' @author Matthew L. Fidler
#' @noRd
.copyMetaForPiping <- function(env) {
  .ret <- new.env(parent=parent.env(env))
  lapply(ls(envir=env, all.names=TRUE), function(item) {
    if (item == ".simModelBase") {
      return(NULL)
    }
    assign(item, get(item, envir=env), envir=.ret)
  })
  .ret
}

#' Copy an environment and all of its contents
#'
#' This is a recursive copy that creates a new environment for every
#' environment in the original environment.  This is used to copy the
#' rxUi object so that it can be modified without modifying the
#' original.
#'
#' @param env Environment to copy
#'
#' @return Copied environment
#' @author Matthew L. Fidler
#' @noRd
.copyEnv <- function(env, visited=new.env(hash=TRUE, parent=emptyenv())) {
  .addr <- environmentName(env)
  if (identical(.addr, "") || is.na(.addr)) .addr <- format(env)
  if (exists(.addr, envir=visited, inherits=FALSE)) {
    return(get(.addr, envir=visited, inherits=FALSE))
  }
  .ret <- new.env(parent=emptyenv())
  assign(.addr, .ret, envir=visited)
  lapply(ls(envir=env, all.names=TRUE), function(item) {
    if (item == ".simModelBase") {
      return(NULL)
    }
    if (is.environment(get(item, envir=env))) {
      assign(item, .copyEnv(get(item, envir=env), visited), envir=.ret)
    } else {
      assign(item, get(item, envir=env), envir=.ret)
    }
  })
  .ret
}

#' This copies the rxode2 UI object so it can be modified
#'
#' @param ui Original UI object
#' @return Copied UI object
#' @author Matthew L. Fidler
#' @export
.copyUi <- function(ui) {
  if (is.list(ui) ||
        inherits(ui, "raw")) {
    return(rxUiDecompress(ui))
  }
  .ret <- new.env(parent=emptyenv())
  .visited <- new.env(hash=TRUE, parent=emptyenv())
  lapply(ls(envir=ui, all.names=TRUE), function(item) {
    if (item == ".simModelBase") {
      return(NULL)
    }
    if (is.environment(get(item, envir=ui))) {
      assign(item, .copyEnv(get(item, envir=ui), .visited), envir=.ret)
    } else {
      assign(item, get(item, envir=ui), envir=.ret)
    }
  })
  class(.ret) <- class(ui)
  .ret
}

#' Subset one omega block down to the etas shared with the destination model
#'
#' @param mat a single `lotriFix` matrix from the source model
#'
#' @param iniDf This is the ini data frame from the prior ui
#'
#' @return list with `mat`, the subset block (`NULL` when no eta is shared),
#'   and `drop`, the names of the etas that were left out
#'
#' @author Matthew L Fidler
#' @noRd
.quoteExpandRxUiOmegaBlock <- function(mat, iniDf) {
  .dn <- dimnames(mat)
  if (is.null(.dn)) return(list(mat=NULL, drop=NULL))
  .dn <- .dn[[1]]
  .w <- which(.dn %in% iniDf$name)
  if (length(.w) == 0L) return(list(mat=NULL, drop=.dn))
  .fix <- attr(mat, "lotriFix")
  .labels <- attr(mat, "lotriLabels")
  # drop=FALSE keeps a single surviving eta a 1x1 matrix; as a bare scalar it
  # carries no dimnames, and as.data.frame() below then yields zero rows -- the
  # eta is silently left out of the piped ini()
  .mat <- mat[.w, .w, drop=FALSE]
  # `[` also strips the matrices carrying the fixed entries and the labels,
  # which would silently unfix a `fix()`ed eta and lose its label as it is
  # piped over
  if (!is.null(.fix)) attr(.mat, "lotriFix") <- .fix[.w, .w, drop=FALSE]
  if (length(.labels) == length(.dn)) attr(.mat, "lotriLabels") <- .labels[.w]
  class(.mat) <- c("lotriFix", "matrix", "array")
  list(mat=.mat, drop=.dn[-.w])
}

#' Expand the quoted lines to include relevant lines from UI
#'
#' @param cur This is the current piped in `rxUi` interface
#'
#' @param iniDf This is the ini data frame from the prior ui
#'
#' @param charExpression character vector of the current expression
#'
#' @return `NULL` if there is no lines to add OR a list of the lines
#'   from the `cur` UI removing parameters that are not in the
#'   destination `ini()`
#'
#' @author Matthew L Fidler
#' @noRd
.quoteExpandRxUi <- function(cur, iniDf, charExpression) {
  if (is.null(iniDf)) return(NULL)
  .cur <- try(as.rxUi(cur), silent=TRUE)
  if (!inherits(.cur, "rxUi")) return(NULL)
  .curLotri <- lotri::as.lotri(.cur$iniDf)
  .lotriEst <- lotri::lotriEst(.curLotri)
  attr(.curLotri, "lotriEst") <- NULL
  .ini1 <- NULL
  .drop <- NULL
  if (!is.null(.lotriEst)) {
    .w <- which(.lotriEst$name %in% iniDf$name)
    if (length(.w) == 0L) {
     .drop <- .lotriEst$name
    } else {
      .drop <- .lotriEst$name[-.w]
      .lotriEst <- .lotriEst[.w, ]
      .ret <- list()
      attr(.ret, "lotriEst") <- .lotriEst
      class(.ret) <- "lotriFix"
      .ini1 <- as.data.frame(.ret)
    }
  }
  .ini2 <- NULL
  if (inherits(.curLotri, "list")) {
    # random effects at more than one level come back as a list of blocks, one
    # per condition, and the list itself has no dimnames
    .blocks <- lapply(.curLotri, .quoteExpandRxUiOmegaBlock, iniDf=iniDf)
    .drop <- c(.drop, unlist(lapply(.blocks, function(b) b$drop), use.names=FALSE))
    .blocks <- lapply(.blocks, function(b) b$mat)
    .blocks <- .blocks[!vapply(.blocks, is.null, logical(1), USE.NAMES=FALSE)]
    if (length(.blocks) > 0L) {
      class(.blocks) <- c("lotriFix", "list")
      .ini2 <- as.data.frame(.blocks)
    }
  } else {
    .block <- .quoteExpandRxUiOmegaBlock(.curLotri, iniDf)
    .drop <- c(.drop, .block$drop)
    if (!is.null(.block$mat)) {
      .ini2 <- as.data.frame(.block$mat)
    }
  }
  .iniDf <- rbind(.ini1, .ini2)
  if (is.null(.iniDf)) {
    cli::cli_alert_info(paste0("piping '", charExpression, "' has no parameters in common with model and does nothing"))
    return(list())
  }
  if (length(.drop) > 0) {
    cli::cli_alert_info(paste0("ignoring following estimates in '", charExpression, "': ",
                               paste(.drop, collapse=", ")))
  }
  .ini <- lotri::lotriDataFrameToLotriExpression(.iniDf, useIni = TRUE)
  .ini <- .ini[[2]]
  .ini <- as.list(.ini)[-1]
  .env <- new.env(parent=emptyenv())
  .env$labels <- NULL
  .env$lastlhs <- NULL
  .env$lhsvars <- NULL
  .ini <- lapply(seq_along(.ini), function(i) {
    .cur <- .ini[[i]]
    if (is.call(.cur) && identical(.cur[[1]], quote(`label`))) {
      if (is.null(.env$lastlhs)) {
        stop("do not know where to put label", call.=FALSE) # should not get here
      }
      .env$labels <- c(.env$labels, i)
      return(as.call(list(quote(`<-`), str2lang(.env$lastlhs), .cur)))
    }
    if (is.call(.cur) && (identical(.cur[[1]], quote(`<-`)) ||
                            identical(.cur[[1]], quote(`~`)))) {
      if (is.call(.cur[[2]])) {
        if (identical(.cur[[2]], quote(`+`))) {
          .env$lhsvars <- c(.env$lhsvars, vapply(as.list(.cur)[-1], function(x) {
            as.character(x)
          }, character(1), USE.NAMES=FALSE))
        }
      } else {
        .char <- as.character(.cur[[2]])
        .env$lastlhs <- .char
        .env$lhsvars <- c(.env$lhsvars, .char)
      }
    }
    .cur
  })
  if (!is.null(.env$labels)) {
    if (getOption("rxode2.ignoreLabels", TRUE)) {
      .ini <- .ini[-.env$labels]
      .minfo("the labels from the piped model do not overwrite old labels\nto change use 'options(rxode2.ignoreLabels=FALSE)'")
    } else {
      .minfo("the labels from the piped model overwrite old labels\nto change use 'options(rxode2.ignoreLabels=TRUE)'")
    }
  }
  return(.ini)
}

#' This expands a list of expressions
#'
#' @param lines These are the expressions as a list
#'
#' @param bracketsOrCs This is the indicator of the bracket lines ie
#'   `{}` or concatenations ie `c()`, that are expanded
#'
#' @param iniDf initial conditions from the previous/parent rxUi
#'
#' @return Single list of expressions; `a=b` becomes `a<-b` in this
#'   expression
#'
#' @author Matthew L. Fidler
#' @noRd
.quoteExpandBracketsOrCs <- function(lines, bracketsOrCs, envir=envir, iniDf=NULL) {
  if (length(bracketsOrCs) == 0) return(lines)
  .expandedForm <- NULL
  .currentLine <- 1
  for (.b in bracketsOrCs) {
    .bracketExpression <- lines[[.b]]
    .cur <- NULL
    .curEta <- FALSE
    if (length(.bracketExpression) == 1) {
      # evaulate expression
      .cur <- try(eval(.bracketExpression, envir=envir), silent=TRUE)
      if (inherits(.cur, "try-error")) {
      } else if (length(.cur) > 1) {
        if (inherits(.cur, "character")) {
          if (is.null(names(.cur))) {
            .cur <- lapply(.cur, function(x) {
              str2lang(x)
            })
          } else {
            .cur <- lapply(names(.cur), function(x) {
              str2lang(paste0(x, "<-", .cur[[x]]))
            })
          }
          .cur <- as.call(c(list(quote(`{`)),.cur))
          .bracketExpression <- .cur
        } else if (identical(.cur[[1]], quote(`{`))) {
          .bracketExpression <- .cur
        }
      }
    } else if (length(.bracketExpression) == 2 &&
                 identical(.bracketExpression[[1]], quote(`~`))) {
      # evaulate expression
      .cur <- try(eval(.bracketExpression[[2]], envir=envir), silent=TRUE)
      .curEta <- TRUE
      if (inherits(.cur, "try-error")) {
      } else if (length(.cur) > 1) {
        if (inherits(.cur, "character")) {
          if (is.null(names(.cur))) {
            .cur <- lapply(.cur, function(x) {
              str2lang(x)
            })
          } else {
            .cur <- lapply(names(.cur), function(x) {
              str2lang(paste0(x, "~", .cur[[x]]))
            })
          }
          .cur <- as.call(c(list(quote(`{`)),.cur))
          .bracketExpression <- .cur
        } else if (identical(.cur[[1]], quote(`{`))) {
          .bracketExpression <- .cur
        }
      }
    }
    .unlistedBrackets <- NULL
    if (length(.bracketExpression) > 1) {
      if (identical(.bracketExpression[[1]], quote(`{`))) {
        .unlistedBrackets <- lapply(seq_along(.bracketExpression)[-1],
                                    function(i) {
                                      .c <- .bracketExpression[[i]]
                                      if (identical(.c[[1]], quote(`=`))) {
                                        .c[[1]] <- quote(`<-`)
                                      }
                                      .c
                                    })
      }
    }
    if (is.null(.unlistedBrackets)) {
      if (is.null(.cur)) {
        ## evaluate to vector and then put it in place
        .cur <- eval(.bracketExpression, envir=envir)
      }
      if (inherits(.cur, "<-") || inherits(.cur, "call")) {
        .unlistedBrackets <- .cur
      } else if (inherits(.cur, "numeric")) {
        if (is.null(names(.cur))) {
          stop("cannot figure out what to do with the unnamed vector", call.=FALSE)
        }
        .unlistedBrackets <- lapply(names(.cur), function(.n) {
          if (.curEta) {
            bquote(.(str2lang(.n)) ~ .(setNames(.cur[.n], NULL)))
          } else {
            bquote(.(str2lang(.n)) <- .(setNames(.cur[.n], NULL)))
          }
        })
      } else if (!inherits(.cur, "rxUi") &&
                   inherits(.cur, "list")) {
        if (is.null(names(.cur))) {
          stop("cannot figure out what to do with the unnamed list", call.=FALSE)
        }
        .unlistedBrackets <- lapply(names(.cur), function(.n) {
          .v <- .cur[[.n]]
          if (inherits(.v, "numeric")) {
            if (.curEta) {
              bquote(.(str2lang(.n)) ~ .(setNames(.cur[[.n]], NULL)))
            } else {
              bquote(.(str2lang(.n)) <- .(setNames(.cur[[.n]], NULL)))
            }
          } else {
            stop("one of the list items supplied to piping is non-numeric", call.=FALSE)
          }
        })
      } else if (inherits(.cur, "matrix")) {
        .cur2 <- .cur
        .unlistedBrackets <- as.list(lotri::lotriAsExpression(.cur2, plusNames=TRUE)[[-1]])[-1]
      } else if (inherits(.cur, "character") && !is.null(names(.cur))) {
        .unlistedBrackets <- lapply(paste(names(.cur),"=", setNames(.cur, NULL)),
                                    str2lang)
      } else if (inherits(.cur, "character") && length(.cur) == 1) {
        .unlistedBrackets <- try(str2lang(.cur), silent=TRUE)
        if (inherits(.unlistedBrackets, "try-error")) {
          stop("vectors and list need to be named numeric expression", call.=FALSE)
        }
        if (identical(.unlistedBrackets[[1]], quote(`=`))) {
          .unlistedBrackets[[1]] <- quote(`<-`)
        }
        .unlistedBrackets <- list(.unlistedBrackets)
      } else {
        .ini <- .quoteExpandRxUi(.cur, iniDf=iniDf, charExpression=deparse1(.bracketExpression))
        if (is.null(.ini)) stop("vectors and list need to be named numeric expression", call.=FALSE)
        .expandedForm <- c(.expandedForm, .ini)
      }
    }
    if (.currentLine == .b) {
      .expandedForm <- c(.expandedForm, .unlistedBrackets)
    } else {
      .expandedForm <- c(.expandedForm, lines[seq(.currentLine, .b - 1)],
                         .unlistedBrackets)
    }
    .currentLine <- .b + 1
  }
  if (.currentLine <= length(lines)) {
    .expandedForm <- c(.expandedForm, lines[seq(.currentLine, length(lines))])
  }
  .expandedForm
}

#' This function collapses the lotri line form to the plus form
#'
#' @param expressionList Expression list that is input to change into
#'   matrix expression form the new line expressions to the classic
#'   plus expressions.
#' @return expression list where lotri line for covariance matrices
#'   are translated to classic plus form.
#' @author Matthew L. Fidler
#' @noRd
#' @examples
#'
#' tmp <- list(str2lang("d ~ 1"),
#'             str2lang("e ~ c(0.5, 3)"),
#'             str2lang("cp ~ add(add.sd)"),
#'             str2lang("cp ~ add(add.sd) + prop(prop.sd)"),
#'             str2lang("cp ~ + add(add.sd)"))
#'
#' .collapseLotriLineFormToPlusForm(tmp)
#'
.collapseLotriLineFormToPlusForm <- function(expressionList) {
  .env <- new.env(parent=emptyenv())
  .env$ret <- expressionList
  .env$lst <- list()
  .env$last <- NA_integer_

  .f <- function() {
    if (!is.na(.env$last)) {
      .val <- as.call(c(list(quote(`{`)), .env$lst))
      .val <- as.call(c(str2lang("lotri::lotri"), .val))
      .val <- suppressMessages(try(eval(.val), silent=TRUE))
      if (inherits(.val, "try-error")) {
        for (.j in seq_along(.env$lst)) {
          .env$ret[[.env$last + .j - 1L]] <- .env$lst[[.j]]
        }
      } else {
        .val <- lotri::lotriAsExpression(.val, plusNames=TRUE)
        .val <- lapply(seq_along(.val)[-1],
                       function(i){
                         .val[[i]]
                       })[[1]]
        .val <- lapply(seq_along(.val)[-1],
                       function(i){
                         .val[[i]]
                       })
        for (.j in seq_along(.val)) {
          .env$ret[[.env$last + .j - 1L]] <- .val[[.j]]
        }
      }
      .env$lst <- list()
      .env$last <- NA_integer_
    }
  }
  for (.i in seq_along(.env$ret)) {
    .cur <- .env$ret[[.i]]
    if (is.call(.cur) && identical(.cur[[1]], quote(`~`)) &&
          length(.cur) == 3L &&
          length(.cur[[2]]) == 1L # excludes ll(cp) ~ 1
        ) {
      .isLotri <- TRUE
      # Check to see if this is an error call
      if (is.call(.cur[[3]])) {
        .rhs <- .cur[[3]]
        # a `| condition` wraps the estimate, so look at what it wraps
        if (identical(.rhs[[1]], quote(`|`)) && length(.rhs) == 3L &&
              is.call(.rhs[[2]])) {
          .rhs <- .rhs[[2]]
        }
        .call <- deparse1(.rhs[[1]])
        if (.call == "+" &&
              length(.rhs) >= 2 &&
              is.call(.rhs[[2]])) {
          .call <- deparse1(.rhs[[2]][[1]])
        }
        if (.call %in% names(.errDist)) {
          .isLotri <- FALSE
        } else if (.call == "unfix") {
          # the lotri round-trip below has no way to represent `unfix()`, so it
          # comes back as a plain estimate and would silently leave the eta fixed
          .isLotri <- FALSE
        }
      }
      if (.isLotri) {
        if (is.na(.env$last)) {
          .env$last <- .i
        }
        .env$ret[[.i]] <- NA
        .env$lst <- c(.env$lst, .cur)
      }
    } else {
      .f()
    }
  }
  .f()
  .w <- which(vapply(seq_along(.env$ret), function(i) {
    !(length(.env$ret[[i]]) == 1L && is.na(.env$ret[[i]]))
  }, logical(1), USE.NAMES=FALSE))
  lapply(.w, function(i) { .env$ret[[i]]})
}

.nsEnv <- new.env(parent=emptyenv())

.nsEnv$.quoteCallInfoLinesAppend <- NULL
#' Returns quoted call information
#'
#' @param callInfo Call information
#'
#' @param envir Environment for evaluation (if needed)
#'
#' @param iniDf The parent model `iniDf` when piping in a `ini` block
#'   (`NULL` otherwise)
#'
#' @return Quote call information.  for `name=expression`, change to
#'   `name<-expression` in quoted call list. For expressions that are
#'   within brackets ie `{}`, unlist the brackets as if they were
#'   called in one single sequence.
#'
#' @author Matthew L. Fidler
#'
#' @export
.quoteCallInfoLines <- function(callInfo, envir=parent.frame(), iniDf=NULL) {
  .bracket <- rep(FALSE, length.out=length(callInfo))
  .env <- environment()
  .nsEnv$.quoteCallInfoLinesAppend <- NULL
  .ret <- lapply(seq_along(callInfo), function(i) {
    .name <- names(callInfo)[i]
    if (!is.null(.name)) {
      if (.name == "append") {
        .append <- callInfo[[i]]
        if (identical(.append, quote(TRUE)) ||
              identical(.append, quote(FALSE)) ||
              identical(.append, quote(NA))) {
        } else {
          .nsEnv$.quoteCallInfoLinesAppend <- eval(call("quote", .append))
        }
        return(NULL)
      } else if (.name %in% c("envir",  "auto", "iniDf", "cov")) {
        return(NULL)
      } else if (.name != "") {
        # Changed named items to
        return(as.call(list(quote(`<-`), str2lang(.name),
                            eval(call("quote", callInfo[[i]])))))
      }
    }
    .quoted <- eval(call("quote", callInfo[[i]]))
    if (missing(.quoted)) {
      # Capture empty arguments (rxode2#688)
      warning("empty argument ignored")
      return(NULL)
    } else if (length(.quoted) == 1) {
      if (identical(.quoted, quote(`diag`)) ||
            (is.call(.quoted) && identical(.quoted[[1]], quote(`diag`)))) {
        .quoted <- str2lang("~diag()")
      } else {
        .bracket[i] <- TRUE
        assign(".bracket", .bracket, envir=.env)
      }
    } else if (length(.quoted) >= 1 &&
                 identical(.quoted[[1]], quote(`diag`))) {
      .quoted <- as.call(c(list(quote(`~`)), .quoted))
    } else if (identical(.quoted[[1]], quote(`diag`))) {
    } else if (identical(.quoted[[1]], quote(`{`)) ||
          identical(.quoted[[1]], quote(`c`)) ||
          identical(.quoted[[1]], quote(`list`))) {
      .bracket[i] <- TRUE
      assign(".bracket", .bracket, envir=.env)
    } else if (identical(.quoted[[1]], quote(`as.formula`))) {
      .quoted <- .quoted[[2]]
    } else if (identical(.quoted[[1]], quote(`~`))) {
      if (length(.quoted) == 2 &&
            exists(as.character(.quoted[[2]]), envir)) {
        # this is a "bracketed" assignment of etas
        .bracket[i] <- TRUE
        assign(".bracket", .bracket, envir=.env)
      } else {
        if (length(.quoted) == 3L && !is.null(.quoted[[3]])) {
          .quoted[[3]] <- .iniSimplifyFixUnfix(.quoted[[3]])
          if (identical(.quoted[[3]], quote(`fix`)) ||
                identical(.quoted[[3]], quote(`unfix`))) {
            .quoted <- as.call(list(quote(`<-`), .quoted[[2]], .quoted[[3]]))
          }
        }

      }
    } else if (identical(.quoted[[1]], quote(`$`))) {
      .tmp <- try(eval(.quoted), silent=TRUE)
      if (!inherits(.tmp, "try-error")) {
        .quoted <- .tmp
        if (inherits(.quoted, "character")) {
          .quoted <- str2lang(.quoted)
        }
      }
    }
    .quoted
  })
  .w <- which(.bracket)
  .ret <- .quoteExpandBracketsOrCs(.ret, .w, envir=envir, iniDf=iniDf)
  .ret <- lapply(seq_along(.ret), function(i) {
    if (identical(.ret[[i]][[1]], quote(`$`))) {
      .r <- eval(.ret[[i]], envir=envir)
      if (inherits(.r, "character")) {
        .r <- str2lang(.r)
      }
      return(.r)
    }
    .ret[[i]]
  })
  .collapseLotriLineFormToPlusForm(.ret[vapply(seq_along(.ret), function(i) {
    !is.null(.ret[[i]])
  }, logical(1), USE.NAMES=FALSE)])
}
