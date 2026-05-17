fixef.rxUi <- function(object, ...) {
  object$theta
}

fixef.function <- function(object, ...) {
  ui <- rxode2(object)
  ui$theta
}
