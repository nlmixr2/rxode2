#' Assert that the model is a rxUi function or model
#'
#' @param model Model to check
#' @param .var.name Var name to override if this needs a different name
#' @return the rxUi model
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
f <- nlmixr(one.cmt)

assertRxUi <- function(model, .var.name=NULL) {
  if (is.null(.var.name)) {
    .var.name <- as.character(substitute(model))
  }
  if (inherits(model, "function")) {
    model <- try(rxode2(model), silent=TRUE)
    if (inherits(model, "try-error")) {
      stop("'", .var.name, "' needs to be a rxUi model", call.=FALSE)
    }
  }
  if (!inherits(model, "rxUi")) {
      stop("'", .var.name, "' needs to be a rxUi model", call.=FALSE)
  }
  return(invisible(model))
}
