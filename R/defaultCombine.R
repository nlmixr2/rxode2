.combineEnv <- new.env(parent = emptyenv())
.combineEnv$default <- "camel"

#' Case combine two strings depending on .combineEnv$defualt
#'
#' @param v1 string 1
#' @param v2 string 2
#' @return defaultCase combined string
#' @noRd
#' @author Matthew L. Fidler
.defaultCombine2 <- function(v1, v2) {
  if (checkmate::testCharacter(v1, min.len = 2L, any.missing = FALSE)) {
    v1 <- do.call(defaultCombine, as.list(v1))
  }
  if (checkmate::testCharacter(v2, min.len = 2L, any.missing = FALSE)) {
    v2 <- do.call(defaultCombine, as.list(v2))
  }
  checkmate::assertCharacter(v1, len = 1L, any.missing = FALSE)
  checkmate::assertCharacter(v2, len = 1L, any.missing = FALSE)
  .n1 <- nchar(v2)
  if (.n1 == 0L) {
    v1
  } else if (.n1 == 1L) {
    switch(
      .combineEnv$default,
      camel = paste0(v1, toupper(v2)),
      snake = paste0(v1, "_", v2),
      dot = paste0(v1, ".", v2),
      blank = paste0(v1, v2)
    )
  } else {
    switch(
      .combineEnv$default,
      camel = paste0(v1, toupper(substr(v2, 1L, 1L)), substr(v2, 2L, .n1)),
      snake = paste0(v1, "_", v2),
      dot = paste0(v1, ".", v2),
      blank = paste0(v1, v2)
    )
  }
}

#' Default combine strings
#'
#' @param ... uses default to combine strings
#' @return combined strings
#' @family Parameter and naming building blocks
#' @export
#' @author Matthew L. Fidler
#' @examples
#'
#' # default combine
#'
#' defaultCombine("f", "depot")
#'
#' defaultCombine(list(c("a", "funny", "c")))
#'
#' defaultCombine(c("a", "funny", "c"))
#'
#' # snake combine
#'
#' snakeCombine("f", "depot")
#'
#' snakeCombine(list(c("a", "funny", "c")))
#'
#' snakeCombine(c("a", "funny", "c"))
#'
#' # dot combine
#'
#' dotCombine("f", "depot")
#'
#' dotCombine(list(c("a", "funny", "c")))
#'
#' dotCombine(c("a", "funny", "c"))
#'
#' # blank combine
#'
#' blankCombine("f", "depot")
#'
#' blankCombine(list(c("a", "funny", "c")))
#'
#' blankCombine(c("a", "funny", "c"))
defaultCombine <- function(...) {
  .args <- list(...)
  .n <- length(.args)
  if (.n == 0L) {
    stop("no arguments provided", call. = FALSE)
  } else if (.n == 1L) {
    .ret <- .args[[1L]]
    if (checkmate::testCharacter(.ret, len = 1, any.missing = FALSE)) {
      .ret
    } else if (checkmate::testCharacter(.ret, min.len = 2, any.missing = FALSE)) {
      do.call(defaultCombine, as.list(.ret))
    } else if (is.list(.ret)) {
      do.call(defaultCombine, .ret)
    } else {
      stop("invalid argument", call. = FALSE)
    }
  } else {
    Reduce(.defaultCombine2, .args)
  }
}

#' Combine two strings using a naming convention
#'
#' Combine two in a manner similar to `paste()` strings using the
#' default combine type
#'
#' @param a first string to combine
#' @param b second string to combine
#' @param combineType is the type of combination; can be:
#'
#' - \code{"default"}: default combine (set with `defaultCombine()`)
#'
#' - \code{"camel"}: camelCase combine
#'
#' - \code{"snake"}: snake_case combine
#'
#' - \code{"dot"}: dot combine (i.e. "a.b")
#'
#' - \code{"blank"}: no separator (i.e. "ab")
#'
#' @return Combined strings separated with `defaultCombine()`
#' @family Parameter and naming building blocks
#' @export
#' @author Matthew L. Fidler
#' @examples
#'
#' combinePaste2("f", "depot")
#'
#' combinePaste2("f", "depot", "snake")
#'
#' combinePaste2("f", "depot", "dot")
#'
#' combinePaste2("f", "depot", "blank")
combinePaste2 <- function(
  a,
  b,
  combineType = c(
    "default",
    "snake",
    "camel",
    "dot",
    "blank"
  )
) {
  if (missing(a) && missing(b)) {
    stop("no arguments provided", call. = FALSE)
  }
  checkmate::assertCharacter(a, min.len = 1L, any.missing = FALSE)
  if (missing(b)) {
    return(a)
  }
  checkmate::assertCharacter(b, min.len = 1L, any.missing = FALSE)
  combineType <- match.arg(combineType)
  if (combineType != "default") {
    .combineEnv$old <- .combineEnv$default
    .combineEnv$default <- combineType
    on.exit({
      .combineEnv$default <- .combineEnv$old
    })
  }
  if (checkmate::testCharacter(a, len = 1L, any.missing = FALSE)) {
    vapply(
      b,
      function(x) {
        .defaultCombine2(a, x)
      },
      character(1L),
      USE.NAMES = FALSE
    )
  } else if (checkmate::testCharacter(b, len = 1L, any.missing = FALSE)) {
    vapply(
      a,
      function(x) {
        .defaultCombine2(x, b)
      },
      character(1L),
      USE.NAMES = FALSE
    )
  } else if (
    checkmate::testCharacter(a, any.missing = FALSE) &&
      checkmate::testCharacter(b, any.missing = FALSE) &&
      length(a) == length(b)
  ) {
    mapply(.defaultCombine2, a, b, SIMPLIFY = TRUE, USE.NAMES = FALSE)
  } else {
    stop(
      "combinePaste2 needs arguments that are the same size or one of the arguments to be a single string",
      call. = FALSE
    )
  }
}
#' @describeIn defaultCombine use snake_case to combine 2 strings
#' @export
snakeCombine <- function(...) {
  .combineEnv$old <- .combineEnv$default
  .combineEnv$default <- "snake"
  on.exit({
    .combineEnv$default <- .combineEnv$old
  })
  defaultCombine(...)
}
#' @describeIn defaultCombine use camelCase to combine strings
#' @export
camelCombine <- function(...) {
  .combineEnv$old <- .combineEnv$default
  .combineEnv$default <- "camel"
  on.exit({
    .combineEnv$default <- .combineEnv$old
  })
  defaultCombine(...)
}

#' @describeIn defaultCombine use the default method for combining two strings
#' @export
dotCombine <- function(...) {
  .combineEnv$old <- .combineEnv$default
  .combineEnv$default <- "dot"
  on.exit({
    .combineEnv$default <- .combineEnv$old
  })
  defaultCombine(...)
}

#' @describeIn defaultCombine combine using a blank separator
#' @export
blankCombine <- function(...) {
  .combineEnv$old <- .combineEnv$default
  .combineEnv$default <- "blank"
  on.exit({
    .combineEnv$default <- .combineEnv$old
  })
  defaultCombine(...)
}

#' Change the default combine type for the package
#'
#' @param combineType this is the default combine type:
#' - \code{"default"}: default combine
#' - \code{"snake"}: snake_case combine
#' - \code{"camel"}: camelCase combine
#' - \code{"dot"}: dot combine (i.e. "a.b")
#' - \code{"blank"}: no separator (i.e. "ab")
#' @author Matthew L. Fidler
#' @family Parameter and naming building blocks
#' @export
#' @examples
#'
#' # Change to the popular snake_case
#' setCombineType("snake")
#' defaultCombine("a", "b")
#'
#' # Change back to nlmixr2/rxode2 default camelCase
#'
#' setCombineType("camel")
#' defaultCombine("a", "b")
#'
#' # This is used to change the naming convention for parameters
#' # produced by this package
setCombineType <- function(combineType = c("snake", "camel", "dot", "blank")) {
  .combineEnv$default <- combineType
}

#' @title Get the combine type from the R option
#' @param op the option to get the combine type from
#' @return the combine type
#' @author Matthew L. Fidler
#' @noRd
.getCombineTypeFromRoption <- function(op) {
  if (!checkmate::testCharacter(op, len = 1, any.missing = FALSE)) {
    return("default")
  }
  .tmp <- getOption(op, "default")
  if (
    checkmate::testCharacter(.tmp, len = 1, any.missing = FALSE) &&
      !(.tmp %in% c("default", "snake", "camel", "dot", "blank"))
  ) {
    .tmp <- "default"
  } else if (!checkmate::testCharacter(.tmp, len = 1L, any.missing = FALSE)) {
    .tmp <- "default"
  }
  .tmp
}
