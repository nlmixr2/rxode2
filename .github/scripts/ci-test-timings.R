# Refresh tests/ci-timings.csv (file,seconds) from the JUnit XML files the
# full CI tier uploads (artifacts named "junit-*"), so .github/scripts/ci-shards.R can
# balance the shards.
#
#   gh run download <run-id> -p 'junit-*' -D junit
#   Rscript .github/scripts/ci-test-timings.R junit/*/*.xml
#
# Times from several platforms are combined by taking each file's maximum.
.xml <- commandArgs(trailingOnly = TRUE)
if (length(.xml) == 0L) {
  stop("usage: Rscript .github/scripts/ci-test-timings.R <junit.xml>...", call. = FALSE)
}
.rows <- do.call(rbind, lapply(.xml, function(.f) {
  .txt <- paste(readLines(.f, warn = FALSE), collapse = "\n")
  .tag <- regmatches(.txt, gregexpr("<testsuite [^>]*>", .txt))[[1]]
  .attr <- function(.name) {
    sub(paste0('.*\\b', .name, '="([^"]*)".*'), "\\1", .tag)
  }
  # testthat names each suite after the file, without "test-" and ".R"
  data.frame(xml = .f, file = .attr("name"), seconds = as.numeric(.attr("time")), stringsAsFactors = FALSE)
}))
.rows <- .rows[!is.na(.rows$seconds), , drop = FALSE]
.ret <- stats::aggregate(seconds ~ xml + file, data = .rows, FUN = sum)
.ret <- stats::aggregate(seconds ~ file, data = .ret, FUN = max)
.ret$seconds <- round(.ret$seconds, 1)
.ret <- .ret[order(-.ret$seconds), ]
utils::write.csv(.ret, "tests/ci-timings.csv", row.names = FALSE)
message("wrote tests/ci-timings.csv: ", nrow(.ret), " files, ", round(sum(.ret$seconds)), " s total")
