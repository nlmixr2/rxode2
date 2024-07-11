#' @rdname rxEvid
#' @export
format.rxEvid <- function(x, ...) {
  .x <- unclass(x)
  format(as.character.rxEvid(.x), align = "left", width = 12)
}

#' @rdname rxEvid
#' @export
format.rxRateDur <- function(x, ...) {
  .x <- unclass(x)
  format(as.character.rxRateDur(.x), align = "left")
}
