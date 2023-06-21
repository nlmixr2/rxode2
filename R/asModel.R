#' Turn into a model expression
#'
#' @param x item to convert to a `model({})` expression
#' @return model expression
#' @export
#' @author Matthew L. Fidler
#' @examples
#' 
#' model <- quote(model({
#'    ka <- exp(tka + eta.ka)
#'    cl <- exp(tcl + eta.cl)
#'    v <- exp(tv + eta.v)
#'    d/dt(depot) = -ka * depot
#'    d/dt(center) = ka * depot - cl / v * center
#'    cp = center / v
#'    cp ~ add(add.sd)
#' }))
#'  
#' as.model(model)
#'
#' one.compartment <- function() {
#'    ini({
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
#' as.model(one.compartment)
#'  
#' ui <- one.compartment()
#'
#' as.model(ui)
#'
#'  model <- c("model({",
#'             "ka <- exp(tka + eta.ka)",
#'             "cl <- exp(tcl + eta.cl)",
#'             "v <- exp(tv + eta.v)",
#'             "d/dt(depot) = -ka * depot",
#'             "d/dt(center) = ka * depot - cl / v * center",
#'             "cp = center / v",
#'             "cp ~ add(add.sd)",
#'             "})")
#'
#' as.model(model)
#'
#' model <- paste(model, collapse="\n")
#'
#' as.model(model)
#'
as.model <- function(x) {
  UseMethod("as.model")
}
#' @rdname as.model
#' @export
as.model.character <- function(x) {
  as.model(lapply(x, function(i) {str2lang(i)}))
}

#' @rdname as.model
#' @export
as.model.call <- function(x) {
  if (!identical(x[[1]], quote(`model`))) {
    stop("unsupported expression of model({}) block",
         call.=FALSE)
  }
  x
}

#' @rdname as.model
#' @export
as.model.list <- function(x) {
  .lst <- lapply(seq_along(x), function(i) {
    if (is.language(x[[i]])) return(x[[i]])
    if (is.character(x[[i]])) return(str2lang(x[[i]]))
    stop("unsupported expression of model({}) block",
         call.=FALSE)
  })
  as.call(c(quote(`model`), as.call(c(quote(`{`), .lst))))
}

#' @rdname as.model
#' @export
as.model.default <- function(x) {
  .model <- try(as.rxUi(x), silent=TRUE)
  if (inherits(.model, "try-error")) {
    stop("do not know how to convert this to an `model` expression",
         call.=FALSE)
  }
  model(.model)
}
