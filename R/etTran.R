.DTEnv <- NULL
.getDTEnv <- function() {
  if (is.null(.DTEnv)) {
    if (requireNamespace("data.table", quietly = TRUE)) {
      .env <- loadNamespace("data.table")
      if (utils::compareVersion(
        as.character(
          utils::packageVersion("data.table")
        ),
        "1.12.4"
      ) >= 0) {
        assignInMyNamespace(".DTEnv", .env)
        return(.env)
      }
    }
    .env <- new.env(parent = emptyenv())
    assignInMyNamespace(".DTEnv", .env)
    return(.env)
  } else {
    return(.DTEnv)
  }
}
