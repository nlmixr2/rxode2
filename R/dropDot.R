#' Drop additive expression that is a .
#'
#' @param x expression
#' @return expression with dot removed
#' @author Matthew L. Fidler
#' @noRd
#' @examples
#' .dropDotAddExpr(str2lang("x+.+4"))
#' .dropDotAddExpr(str2lang("+.+4"))
#' .dropDotAddExpr(str2lang(".+4"))
#' .dropDotAddExpr(str2lang("4+x+."))
#' .dropDotAddExpr(str2lang("x-.+4"))
#' .dropDotAddExpr(str2lang("-.+4"))
#' .dropDotAddExpr(str2lang("+.-4"))
#' .dropDotAddExpr(str2lang("+4-."))
#' .dropDotAddExpr(str2lang("d/dt(central) <- . - kel * central"))
#' .dropDotAddExpr(list(str2lang("d/dt(central) <- . - kel * central")))
.dropDotAddExpr <- function(x) {
  if (is.list(x)) {
    return(lapply(seq_along(x), function(i) {
      .dropDotAddExpr(x[[i]])
    }))
  }
  if (is.call(x)) {
    if (length(x) == 2) {
      if (length(x[[1]]) != 1) {
        .x1 <- .dropDotAddExpr(x[[1]])
      } else {
        .x1 <- x[[1]]
      }
      if (length(x[[2]]) != 1) {
        .x2 <- .dropDotAddExpr(x[[2]])
      } else {
        .x2 <- x[[2]]
      }
      if (
        identical(.x1, quote(`+`)) ||
          identical(.x1, quote(`-`))
      ) {
        return(.x2)
      }
    }
    if (length(x) == 3) {
      if (length(x[[1]]) != 1) {
        .x1 <- .dropDotAddExpr(x[[1]])
      } else {
        .x1 <- x[[1]]
      }
      if (length(x[[2]]) != 1) {
        .x2 <- .dropDotAddExpr(x[[2]])
      } else {
        .x2 <- x[[2]]
      }
      if (length(x[[3]]) != 1) {
        .x3 <- .dropDotAddExpr(x[[3]])
      } else {
        .x3 <- x[[3]]
      }
      if (
        identical(.x1, quote(`=`)) ||
          identical(.x1, quote(`<-`))
      ) {
        return(as.call(list(.x1, .x2, .x3)))
      }
      if (identical(.x1, quote(`-`))) {
        if (identical(.x2, quote(`.`))) {
          return(str2lang(paste0("-", deparse1(.x3))))
        }
        if (identical(.x3, quote(`.`))) {
          return(.x2)
        }
      }
      if (identical(.x1, quote(`+`))) {
        if (identical(.x2, quote(`.`))) {
          return(.x3)
        }
        if (identical(.x3, quote(`.`))) {
          return(.x2)
        }
      }
    }
    as.call(lapply(x, .dropDotAddExpr))
  } else {
    x
  }
}
