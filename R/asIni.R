#' Turn into an ini block for initialization
#'
#' @param x Item to convert to a rxode2/nlmixr2 ui ini exression
#' @return rxode2 ini expression
#' @export 
#' @author Matthew L. Fidler
#' @examples
#' 
as.ini <- function(x) {
  UseMethod("as.ini")
}

#' @rdname as.ini
#' @export
as.ini.data.frame <- function(x) {
 lotri::lotriDataFrameToLotriExpression(x, useIni = TRUE) 
}

#' @rdname as.ini
#' @export
as.ini.call <- function(x) {
  if (!identical(x[[1]], quote(`ini`)) &&
        !identical(x[[1]], quote(`lotri`))) {
    stop("unsupported expression for ini({}) block",
         call.=FALSE)
  }
  if (identical(x[[1]], quote(`ini`))) {
    .tmp <- x
    .tmp[[1]] <- quote(`lotri`)
    .tmp <- eval(.tmp)
    return(as.ini(.tmp))
  } else if (identical(x[[1]], quote(`lotri`))) {
    .tmp <- eval(x)
    return(as.ini(.tmp))
  }
  stop("unsupported expression of ini({}) block",
       call.=FALSE)
}

#' @rdname as.ini
#' @export
as.ini.lotriFix <- function(x) {
  .ret <- as.expression(x)
  .ret[[1]] <- quote(`ini`)
  return(.ret)
}

#' @rdname as.ini
#' @export
as.ini.matrix <- function(x) {
  .ret <- x
  class(.ret) <- c("lotriFix", class(.ret))
  as.ini(.ret)
}

#' @rdname as.ini
#' @export
as.ini.default <- function(x) {
  .ini <- try(as.rxUi(x), silent=TRUE)
  if (inherits(.ini, "try-error")) {
    stop("do not know how to convert this to an `ini` expression",
         call.=FALSE)
  }
  ini(.ini)
}
