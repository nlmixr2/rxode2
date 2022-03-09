#' Assert properties of the rxUi models
#'
#' @param model Model to check
#' @details
#'
#' These functions have different types of assertions
#'
#'  - `assertRxUi` -- Make sure this is a proper rxode2 model (if not throw error)
#'
#'  - `assertRxUiSingleEndpoint` -- Make sure the rxode2 model is only
#'    a single endpoint model (if not throw error)
#'
#' @return the rxUi model
#' @inheritParams checkmate::assertIntegerish
#' @author Matthew L. Fidler
#' @export
#' @examples
#' one.cmt <- function() {
#'  ini({
#'    ## You may label each parameter with a comment
#'    tka <- 0.45 # Ka
#'    tcl <- log(c(0, 2.7, 100)) # Log Cl
#'    ## This works with interactive models
#'    ## You may also label the preceding line with label("label text")
#'    tv <- 3.45; label("log V")
#'    ## the label("Label name") works with all models
#'    eta.ka ~ 0.6
#'    eta.cl ~ 0.3
#'    eta.v ~ 0.1
#'    add.sd <- 0.7
#'  })
#'  model({
#'    ka <- exp(tka + eta.ka)
#'    cl <- exp(tcl + eta.cl)
#'    v <- exp(tv + eta.v)
#'     linCmt() ~ add(add.sd)
#'  })
#' }
#' assertRxUi(one.cmt)
#' # assertRxUi(rnorm)
#'
assertRxUi <- function(model, .var.name=checkmate::vname(model)) {
  if (inherits(model, "function")) {
    model <- try(rxode2(model), silent=TRUE)
    if (inherits(model, "try-error")) {
      stop("'", .var.name, "' needs to be a rxUi model", call.=FALSE)
    }
  }
  if (!inherits(model, "rxUi")) {
    stop("'", .var.name, "' needs to be a rxUi model", call.=FALSE)
  }
  invisible(model)
}

#' @export
#' @rdname
assertRxUiSingleEndpoint <- function(model, .var.name=checkmate::vname(model)) {
  model <- assertRxUi(model, .var.name=.var.name)
  .predDf <- model$predDf
  if (length(f$predDf$cond) > 1L) {
    stop("'", .var.name, "' needs to be a single endpoint model", call.=FALSE)
  }
  invisible(model)
}
