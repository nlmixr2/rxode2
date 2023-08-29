.asFunctionEnv <- new.env(parent=emptyenv())
.asFunctionEnv$rx <- NULL
#' @export
as.function.rxUi <- function(x, ...) {
  x$fun
}


#' @export
as.function.rxode2tos <- function(x, ...) {
  .asFunctionEnv$rx <- x
  suppressMessages(x$uiFun)
}

#' @export
as.function.rxode2 <- function(x, ...) {
  eval(str2lang(paste(c("function() {",
    "model({",
    rxNorm(x),
    "})",
    "}"), collapse="\n")))
}



#' @export
as.function.rxModelVars <- as.function.rxode2
