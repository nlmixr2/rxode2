#' Turn into an ini block for initialization
#'
#' @param x Item to convert to a rxode2/nlmixr2 ui ini expression
#' @return rxode2 ini expression
#' @export
#' @author Matthew L. Fidler
#' @examples
#'
#' ini <- quote(ini({
#'    tka <- log(1.57)
#'    tcl <- log(2.72)
#'    tv <- log(31.5)
#'    eta.ka ~ 0.6
#'    eta.cl ~ 0.3
#'    eta.v ~ 0.1
#'    add.sd <- 0.7
#'  }))
#'
#' as.ini(ini)
#'
#' l <- quote(lotri({
#'    tka <- log(1.57)
#'    tcl <- log(2.72)
#'    tv <- log(31.5)
#'    eta.ka ~ 0.6
#'    eta.cl ~ 0.3
#'    eta.v ~ 0.1
#'    add.sd <- 0.7
#'  }))
#'
#' as.ini(l)
#'
#' m <- lotri({
#'    eta.ka ~ 0.6
#'    eta.cl ~ 0.3
#'    eta.v ~ 0.1
#' })
#'
#' as.ini(m)
#'
#' one.compartment <- function() {
#'   ini({
#'      tka <- log(1.57)
#'      tcl <- log(2.72)
#'      tv <- log(31.5)
#'      eta.ka ~ 0.6
#'      eta.cl ~ 0.3
#'      eta.v ~ 0.1
#'      add.sd <- 0.7
#'    })
#'    model({
#'      ka <- exp(tka + eta.ka)
#'      cl <- exp(tcl + eta.cl)
#'      v <- exp(tv + eta.v)
#'      d/dt(depot) = -ka * depot
#'      d/dt(center) = ka * depot - cl / v * center
#'      cp = center / v
#'      cp ~ add(add.sd)
#'    })
#' }
#'
#' as.ini(one.compartment)
#'
#' ui <- one.compartment()
#'
#' as.ini(ui)
#'
#' ui$iniDf
#'
#' as.ini(ui$iniDf)
#'
#' ini <- c("ini({",
#'           "tka <- log(1.57)",
#'           "tcl <- log(2.72)",
#'           "tv <- log(31.5)",
#'           "eta.ka ~ 0.6",
#'           "eta.cl ~ 0.3",
#'           "eta.v ~ 0.1",
#'           "add.sd <- 0.7",
#'           "})")
#'
#' as.ini(ini)
#'
#' ini <- paste(ini, collapse="\n")
#'
#' as.ini(ini)
#'
as.ini <- function(x) {
  UseMethod("as.ini")
}

#' @rdname as.ini
#' @export
as.ini.character <- function(x) {
  as.ini(str2lang(paste(x, collapse="\n")))
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
  if (is.null(x)) {
    return(quote(ini({})))
  }
  .ini <- try(as.rxUi(x), silent=TRUE)
  if (inherits(.ini, "try-error")) {
    stop("do not know how to convert this to an `ini` expression",
         call.=FALSE)
  }
  ini(.ini)
}
