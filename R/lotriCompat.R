## 'lotri' helpers that only exist in newer 'lotri' releases (>= 1.0.5).
## Looked up in the namespace (not `lotri::`) so an older 'lotri' still works.

#' Get a 'lotri' function if the installed 'lotri' has it
#'
#' @param name function name
#' @return the function, or `NULL` when the installed 'lotri' lacks it
#' @noRd
#' @author Matthew L. Fidler
.lotriFun <- function(name) {
  .ns <- asNamespace("lotri")
  if (!exists(name, envir = .ns, inherits = FALSE)) {
    return(NULL)
  }
  get(name, envir = .ns, inherits = FALSE)
}

#' Strip the `:same:` suffix from a condition (see `lotri::lotriBaseCondition()`)
#'
#' @param condition condition column
#' @return base condition
#' @noRd
#' @author Matthew L. Fidler
.lotriBaseCondition <- function(condition) {
  .f <- .lotriFun("lotriBaseCondition")
  if (!is.null(.f)) {
    return(.f(condition))
  }
  if (length(condition) == 0L) {
    return(character(0))
  }
  sub(":same:.*$", "", as.character(condition))
}

#' Map of etas that repeat an earlier block (see `lotri::lotriSameMap()`)
#'
#' An older 'lotri' has no `same()`, so no eta mirrors another.
#'
#' @param iniDf ini data frame
#' @return integer map, 0 for an eta that does not mirror another
#' @noRd
#' @author Matthew L. Fidler
.lotriSameMap <- function(iniDf) {
  .f <- .lotriFun("lotriSameMap")
  if (!is.null(.f)) {
    return(.f(iniDf))
  }
  .w <- which(!is.na(iniDf$neta1) & iniDf$neta1 == iniDf$neta2)
  if (length(.w) == 0L) {
    return(integer(0))
  }
  integer(max(iniDf$neta1[.w]))
}
