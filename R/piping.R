#'  This copies the rxode2 UI object so it can be modified
#'
#' @param ui Original UI object
#' @return Copied UI object
#' @author Matthew L. Fidler
#' @export
.copyUi <- function(ui) {
  .ret <- new.env(parent=emptyenv())
  lapply(ls(envir=ui, all.names=TRUE), function(item){
    assign(item, get(item, envir=ui), envir=.ret)
  })
  class(.ret) <- class(ui)
  .ret
}

#' This expands a list of expressions
#'
#' @param lines These are the expressions as a list
#' @param bracketsOrCs This is the indicator of the bracket lines ie
#'   `{}` or concatenations ie `c()`, that are expanded
#' @return Single list of expressions; `a=b` becomes `a<-b` in this
#'   expression
#' @author Matthew L. Fidler
#' @noRd
.quoteExpandBracketsOrCs <- function(lines, bracketsOrCs, envir=envir) {
  if (length(bracketsOrCs) == 0) return(lines)
  .expandedForm <- NULL
  .currentLine <- 1
  for (.b in bracketsOrCs) {
    .bracketExpression <- lines[[.b]]
    .cur <- NULL
    if (length(.bracketExpression) == 1) {
      # evaulate expression
      .cur <- eval(.bracketExpression, envir=envir)
      if (length(.cur) > 1) {
        if (identical(.cur[[1]], quote(`{`))) {
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
        ## evalute to vector and then put it in place
        .cur <- eval(.bracketExpression, envir=envir)
      }
      if (inherits(.cur, "<-") || inherits(.cur, "call")) {
        .unlistedBrackets <- .cur
      } else if (inherits(.cur, "numeric")) {
        if (is.null(names(.cur))) {
          stop("cannot figure out what to do with the unnamed vector", call.=FALSE)
        }
        .unlistedBrackets <- lapply(names(.cur), function(.n) {
          bquote(.(str2lang(.n)) <- .(setNames(.cur[.n], NULL)))
        })
      } else if (inherits(.cur, "list")) {
        if (is.null(names(.cur))) {
          stop("cannot figure out what to do with the unnamed list", call.=FALSE)
        }
        .unlistedBrackets <- lapply(names(.cur), function(.n) {
          .v <- .cur[[.n]]
          if (inherits(.v, "numeric")) {
            bquote(.(str2lang(.n)) <- .(setNames(.cur[[.n]], NULL)))
          } else {
            stop("one of the list items supplied to piping is non-numeric", call.=FALSE)
          }
        })
      } else if (inherits(.cur, "matrix")) {
        .cur2 <- .cur
        if (!inherits(.cur, "lotriFix")) {
          class(.cur2) <- c("lotriFix", class(.cur))
        }
        .unlistedBrackets <- as.list(as.expression(.cur2)[[-1]])[-1]
      } else if (inherits(.cur, "character") && !is.null(names(.cur))) {
        .unlistedBrackets <- lapply(paste(names(.cur),"=", setNames(.cur, NULL)),
                                    str2lang)
      } else if (inherits(.cur, "character") && length(.cur) == 1) {
        .unlistedBrackets <- try(str2lang(.cur), silent=TRUE)
        if (inherits(.unlistedBrackets, "try-error")) {
          stop("vectors and list need to named numeric expression", call.=FALSE)
        }
        if (identical(.unlistedBrackets[[1]], quote(`=`))) {
          .unlistedBrackets[[1]] <- quote(`<-`)
        }
        .unlistedBrackets <- list(.unlistedBrackets)
      } else {
        stop("vectors and list need to named numeric expression", call.=FALSE)
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


#' Returns quoted call information
#'
#' @param callInfo Call information
#'
#' @param envir Environment for evaluation (if needed)
#'
#' @return Quote call information.  for `name=expression`, change to
#'   `name<-expression` in quoted call list. For expressions that are
#'   within brackets ie `{}`, unlist the brackets as if they were
#'   called in one single sequence.
#'
#' @author Matthew L. Fidler
#'
#' @export
.quoteCallInfoLines <- function(callInfo, envir=parent.frame()) {
  .bracket <- rep(FALSE, length.out=length(callInfo))
  .env <- environment()
  .ret <- lapply(seq_along(callInfo), function(i) {
    .name <- names(callInfo)[i]
    if (!is.null(.name)) {
      if (.name %in% c("envir", "append", "auto")) {
        return(NULL)
      } else if (.name != "") {
        # Changed named items to
        return(as.call(list(quote(`<-`), str2lang(.name),
                            eval(call("quote", callInfo[[i]])))))
      }
    }
    .quoted <- eval(call("quote", callInfo[[i]]))
    if (length(.quoted) == 1) {
      .bracket[i] <- TRUE
      assign(".bracket", .bracket, envir=.env)
    } else if (identical(.quoted[[1]], quote(`{`)) ||
          identical(.quoted[[1]], quote(`c`)) ||
          identical(.quoted[[1]], quote(`list`))) {
      .bracket[i] <- TRUE
      assign(".bracket", .bracket, envir=.env)
    }
    .quoted
  })
  .w <- which(.bracket)
  .ret <- .quoteExpandBracketsOrCs(.ret, .w, envir=envir)
  .ret[vapply(seq_along(.ret), function(i) {
    !is.null(.ret[[i]])
  }, logical(1), USE.NAMES=FALSE)]
}
