# The functions here support making digests stable when there are no model changes.

#' @export
sha1.rxUi <- function(x, digits = 14L, zapsmall = 7L, ..., dropSessionSpecific = TRUE, algo = "sha1") {
  object <- rxUiDecompress(x)
  nmCheck <- names(object)
  if (dropSessionSpecific) {
    nmCheck <- setdiff(nmCheck, "modelName")
  }
  objectSha1 <- character()
  for (nm in nmCheck) {
    objectSha1[[nm]] <- sha1(object[[nm]], digits = digits, zapsmall = zapsmall, ..., dropSessionSpecific = dropSessionSpecific, algo = algo)
  }
  sha1(objectSha1, digits = digits, zapsmall = zapsmall, ..., dropSessionSpecific = dropSessionSpecific, algo = algo)
}

#' @export
sha1.rxModelVars <- function(x, digits = 14L, zapsmall = 7L, ..., dropSessionSpecific = TRUE, algo = "sha1") {
  nmCheck <- names(x)
  if (dropSessionSpecific) {
    nmCheck <- setdiff(nmCheck, c("timeId", "trans", "sensProp", "normProp", "stateOrd", "md5"))
    x$version <- x$version[setdiff(names(x$version), "md5")]
  }
  objectSha1 <- character()
  for (nm in nmCheck) {
    objectSha1[[nm]] <- sha1(x[[nm]], digits = digits, zapsmall = zapsmall, ..., dropSessionSpecific = dropSessionSpecific, algo = algo)
  }
  sha1(objectSha1, digits = digits, zapsmall = zapsmall, ..., dropSessionSpecific = dropSessionSpecific, algo = algo)
}

#' @export
`sha1.<-` <- function(x, digits = 14L, zapsmall = 7L, ..., algo = "sha1") {
  digest::sha1_attr_digest(x, digits = digits, zapsmall = zapsmall, ..., algo = algo)
}

#' @export
sha1.environment <- function(x, digits = 14L, zapsmall = 7L, ..., algo = "sha1") {
  digest::sha1_attr_digest(x, digits = digits, zapsmall = zapsmall, ..., algo = algo)
}
