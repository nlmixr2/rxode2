# Comparison helpers for the symengine translation fixture tests
# (test-symengine-translate-fixture*.R).  Definitions only.

# Two renderings that differ ONLY in the last digits of a numeric literal are
# the same translation.  The irrational constants symengine emits -- EulerGamma,
# Catalan -- are rendered from the platform's own double, and macOS ARM
# disagrees with Linux x86 in the 16th significant digit
# (0.5772156649015329 vs 0.5772156649015330).  The fixture pins the
# TRANSLATION, not the last bit of a transcendental, so compare the
# non-numeric skeleton exactly and the numbers numerically.
.seFixtureSame <- function(a, b) {
  .num <- "[0-9]+\\.[0-9]+([eE][-+]?[0-9]+)?"
  if (!identical(gsub(.num, "<n>", a), gsub(.num, "<n>", b))) {
    return(FALSE)
  }
  .a <- as.numeric(regmatches(a, gregexpr(.num, a))[[1]])
  .b <- as.numeric(regmatches(b, gregexpr(.num, b))[[1]])
  if (length(.a) != length(.b) || length(.a) == 0L) {
    return(FALSE)
  }
  isTRUE(all.equal(.a, .b, tolerance = 1e-14))
}

# a diff here means the translator changed behavior; show the offending
# inputs rather than "576 != 576"
.seFixtureCmp <- function(df, fn, what) {
  .got <- character(nrow(df))
  .gotErr <- logical(nrow(df))
  for (.i in seq_len(nrow(df))) {
    .r <- tryCatch(fn(df$input[.i]), error = function(e) {
      .gotErr[.i] <<- TRUE
      conditionMessage(e)
    })
    .got[.i] <- if (is.character(.r) && length(.r) == 1L) .r else "<non-character>"
  }
  .bad <- which(.got != df$output | .gotErr != df$isError)
  .bad <- .bad[
    !vapply(
      .bad,
      function(.i) {
        .seFixtureSame(.got[.i], df$output[.i])
      },
      logical(1)
    )
  ]
  if (length(.bad) > 0L) {
    .n <- min(length(.bad), 10L)
    .msg <- paste0(
      what,
      ": ",
      length(.bad),
      " of ",
      nrow(df),
      " translations changed\n",
      paste0(
        "  input:    ",
        df$input[.bad[seq_len(.n)]],
        "\n",
        "  expected: ",
        df$output[.bad[seq_len(.n)]],
        "\n",
        "  got:      ",
        .got[.bad[seq_len(.n)]],
        collapse = "\n"
      )
    )
  } else {
    .msg <- ""
  }
  list(nbad = length(.bad), msg = .msg, n = nrow(df))
}
