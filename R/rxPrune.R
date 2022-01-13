#'  Internal Pruning function
#'
#'
#' @param x List of quoted lines
#' @param envir Environment where information is stored
#' @return Pruned model code
#' @author Matthew L. Fidler
#' @keywords internal
#' @export
.rxPrune <- function(x, envir = parent.frame()) {
  if (is.name(x) || is.atomic(x)) {
    if (is.character(x)) {
      return(deparse1(x))
    }
    return(as.character(x))
  } else if (is.call(x)) {
    if (identical(x[[1]], quote(`if`))) {
      .if <- envir$.if
      .if[length(.if) + 1] <- .rxPrune(x[[2]], envir = envir)
      envir$.if <- .if
      .x2 <- x[-(1:2)]
      if (length(.x2) == 2) {
        .ret1 <- .rxPrune(.x2[[1]], envir = envir)
        .if[length(.if)] <- paste0("1-(", .if[length(.if)], ")")
        envir$.if <- .if
        .else <- envir$.else
        envir$.else <- unique(c(findLhs(eval(parse(text = paste0("quote({", .ret1, "})"))))))
        .ret2 <- .rxPrune(.x2[[2]], envir = envir)
        envir$.else <- .else
        .ret <- paste0(.ret1, "\n", .ret2)
      } else if (length(.x2) == 1) {
        .ret <- .rxPrune(.x2[[1]], envir = envir)
      }
      .if <- .if[-length(.if)]
      envir$.if <- .if
      return(.ret)
    } else if (identical(x[[1]], quote(`(`))) {
      return(paste0("(", .rxPrune(x[[2]], envir = envir), ")"))
    } else if (identical(x[[1]], quote(`{`))) {
      .x2 <- x[-1]
      return(paste0(lapply(.x2, .rxPrune, envir = envir), collapse = "\n"))
    } else if (identical(x[[1]], quote(`==`)) ||
      identical(x[[1]], quote(`>=`)) ||
      identical(x[[1]], quote(`<=`)) ||
      identical(x[[1]], quote(`>`)) ||
      identical(x[[1]], quote(`<`)) ||
      identical(x[[1]], quote(`!=`)) ||
      identical(x[[1]], quote(`&&`)) ||
      identical(x[[1]], quote(`||`)) ||
      identical(x[[1]], quote(`&`)) ||
      identical(x[[1]], quote(`|`))) {
      .ret <- paste0(
        .rxPrune(x[[2]], envir = envir), as.character(x[[1]]),
        .rxPrune(x[[3]], envir = envir)
      )
      return(.ret)
    } else if (identical(x[[1]], quote(`=`)) ||
      identical(x[[1]], quote(`<-`)) ||
      identical(x[[1]], quote(`~`))) {
      if (length(envir$.if > 0)) {
        .f2 <- .rxPrune(x[[2]], envir = envir)
        .if <- paste0(paste0("(", envir$.if, ")"), collapse = "*")
        if (any(envir$.def1 == .f2)) {
          .ret <- paste0(
            .f2, as.character(x[[1]]), .if, "*(",
            .rxPrune(x[[3]], envir = envir), ")+(1-(", .if, "))*(",
            .f2, ")"
          )
        } else {
          .ret <- paste0(
            .f2, as.character(x[[1]]), .if, "*(",
            .rxPrune(x[[3]], envir = envir), ")",
            ifelse(any(envir$.else == .f2),
              paste0("+", .f2), ""
            )
          )
        }
        assign(".def1", unique(c(envir$.def1, .f2)), envir)
        return(.ret)
      } else {
        .f2 <- .rxPrune(x[[2]], envir = envir)
        assign(".def1", unique(c(envir$.def1, .f2)), envir)
        return(paste0(
          .f2, as.character(x[[1]]),
          .rxPrune(x[[3]], envir = envir)
        ))
      }
    } else if (identical(x[[1]], quote(`*`)) ||
      identical(x[[1]], quote(`^`)) ||
      identical(x[[1]], quote(`+`)) ||
      identical(x[[1]], quote(`-`)) ||
      identical(x[[1]], quote(`/`))) {
      if (length(x) == 3) {
        return(paste0(
          .rxPrune(x[[2]], envir = envir), as.character(x[[1]]),
          .rxPrune(x[[3]], envir = envir)
        ))
      } else {
        ## Unary Operators
        return(paste0(
          as.character(x[[1]]),
          .rxPrune(x[[2]], envir = envir)
        ))
      }
    } else if (identical(x[[1]], quote(`ifelse`))) {
      .f2 <- .rxPrune(x[[2]], envir = envir)
      .f3 <- .rxPrune(x[[3]], envir = envir)
      .f4 <- .rxPrune(x[[4]], envir = envir)
      return(paste0("((", .f2, ")*(", .f3, ")+(1-(", .f2, "))*(", .f4, "))"))
    } else if (identical(x[[1]], quote(`[`))) {
      .type <- toupper(as.character(x[[2]]))
      ## Since only THETA/ETA are allowed with rxode2 pruning
      ## only will take legal rxode2; Therefore just paste these.
      return(paste0(.type, "[", x[[3]], "]"))
    } else {
      .ret0 <- lapply(x, .rxPrune, envir = envir)
      .ret <- paste0(.ret0[[1]], "(")
      .ret0 <- .ret0[-1]
      .ret <- paste0(.ret, paste0(unlist(.ret0), collapse = ", "), ")")
      return(.ret)
    }
  } else { ## nocov start
    ## is.pairlist OR is.atomic OR unknown...
    stop("unsupported expression", call. = FALSE)
  } ## nocov end
}


#' Prune branches (ie if/else) from rxode2
#'
#' This prunes branches (ie if/else) from rxode2.
#'
#' @param x rxode2 model that can be accessed by rxNorm
#' @return Pruned rxode2 model text.
#' @author Matthew L. Fidler
#' @keywords internal
#' @export
rxPrune <- function(x) {
  .env <- new.env(parent = emptyenv())
  .env$.if <- NULL
  .env$.def1 <- NULL
  .ret <- .rxPrune(eval(parse(text = paste0("quote({", rxNorm(x), "})"))), envir = .env)
  return(.ret)
}
