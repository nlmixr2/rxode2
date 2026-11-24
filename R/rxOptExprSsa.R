# Versioning of reassigned variables for rxOptExpr() (issue #1782).
#
# Common subexpressions are matched by text, so `3 - p` before and after `p = p * 2`
# would share one temporary and the second use would read the stale `p`.  Before the
# search, every assignment of a variable that is reassigned (or read before it is
# assigned) gets a fresh name `<prefix><k>__<name>`, so the two `3 - p` no longer match;
# .rxOptSsaUndo() strips the prefix afterwards.  A temporary is defined just before its
# first use, so after the rename back it is only used while its variables keep the
# value it was computed from.  Leaving a `{}` block gives every variable assigned in it
# a fresh name (its value depends on the branch taken), and a `while` loop does the
# same on entry, since its body reads the values of the previous iteration.  Lines are
# scanned with string literals blanked (.rxCodeOnly()), so text in a string is never
# renamed or read as a brace.

.rxOptSsaIdRe <- "(?<![A-Za-z0-9_.])[A-Za-z_.][A-Za-z0-9_.]*(?![A-Za-z0-9_.])"
.rxOptSsaLhsRe <- "^[ \t]*([A-Za-z_.][A-Za-z0-9_.]*)[ \t]*(=(?!=)|~|<-)"

#' Plain left-hand side variable of each normalized line
#'
#' @param lines normalized model lines
#' @return character vector, `NA` where the line does not assign a plain variable
#' @noRd
.rxOptSsaLhs <- function(lines) {
  .m <- regmatches(lines, regexec(.rxOptSsaLhsRe, lines, perl = TRUE))
  vapply(.m, function(.x) if (length(.x) == 0L) NA_character_ else .x[2L], character(1))
}

#' Split each normalized line into its left-hand side and the part that is read
#'
#' @param lines normalized model lines
#' @param code the same lines with string literals blanked (.rxCodeOnly())
#' @param lhs output of .rxOptSsaLhs() on `code`
#' @return list of the leading (left-hand side) text `head`, the read text
#'   `read`, and the read text with string literals blanked `code`
#' @noRd
.rxOptSsaSplit <- function(lines, code, lhs) {
  .len <- ifelse(is.na(lhs), 0L, attr(regexpr(.rxOptSsaLhsRe, code, perl = TRUE), "match.length"))
  list(
    head = substr(lines, 1L, .len),
    read = substr(lines, .len + 1L, nchar(lines)),
    code = substr(code, .len + 1L, nchar(code))
  )
}

#' Variables that need versioning: assigned more than once, assigned inside a
#' block, or read before assigned
#'
#' @param lhs output of .rxOptSsaLhs()
#' @param read read text of each line, string literals blanked
#' @param inBlock is each line inside a `{}` block
#' @return character vector of variable names
#' @noRd
.rxOptSsaCandidates <- function(lhs, read, inBlock) {
  .assigned <- unique(lhs[!is.na(lhs)])
  if (length(.assigned) == 0L) {
    return(character(0))
  }
  .multi <- unique(lhs[!is.na(lhs) & duplicated(lhs)])
  .first <- match(.assigned, lhs)
  .tok <- regmatches(read, gregexpr(.rxOptSsaIdRe, read, perl = TRUE))
  .tokLine <- rep.int(seq_along(.tok), lengths(.tok))
  .tok <- unlist(.tok, use.names = FALSE)
  .w <- .tok %in% .assigned
  ## first line reading each assigned variable (a line reads before it assigns)
  .firstRead <- tapply(.tokLine[.w], factor(.tok[.w], levels = .assigned), min)
  .readBefore <- !is.na(.firstRead) & .firstRead <= .first
  unique(c(.multi, lhs[!is.na(lhs) & inBlock], .assigned[.readBefore]))
}

#' Line of the `}` that closes the block opened on line `i`
#'
#' @param opens,closes number of `{` and `}` on each line
#' @param i line opening the block
#' @return line index (the last line when unbalanced)
#' @noRd
.rxOptSsaBlockEnd <- function(opens, closes, i) {
  .depth <- 0L
  for (.j in seq.int(i, length(opens))) {
    if (.j > i) {
      .depth <- .depth - closes[.j]
    }
    if (.depth <= 0L && .j > i) {
      return(.j)
    }
    .depth <- .depth + opens[.j]
  }
  length(opens)
}

#' Give every assignment of a reassigned variable its own name
#'
#' @param norm normalized model text (rxNorm())
#' @return `NULL` when no variable needs versioning, otherwise a list with the
#'   versioned text `txt` and the `prefix` that .rxOptSsaUndo() removes
#' @author Matthew L. Fidler
#' @noRd
.rxOptSsa <- function(norm) {
  if (!is.character(norm) || length(norm) != 1L || is.na(norm)) {
    return(NULL)
  }
  .lines <- strsplit(norm, "\n", fixed = TRUE)[[1]]
  .code <- vapply(.lines, .rxCodeOnly, character(1), USE.NAMES = FALSE)
  .lhs <- .rxOptSsaLhs(.code)
  .sp <- .rxOptSsaSplit(.lines, .code, .lhs)
  .opens <- lengths(regmatches(.sp$code, gregexpr("{", .sp$code, fixed = TRUE)))
  .closes <- lengths(regmatches(.sp$code, gregexpr("}", .sp$code, fixed = TRUE)))
  .inBlock <- cumsum(.opens) - cumsum(.closes) - .opens > 0L
  .cand <- .rxOptSsaCandidates(.lhs, .sp$code, .inBlock)
  if (length(.cand) == 0L) {
    return(NULL)
  }
  .prefix <- "rx__ssa"
  while (grepl(.prefix, norm, fixed = TRUE)) {
    .prefix <- paste0(.prefix, "x")
  }
  .ver <- setNames(integer(length(.cand)), .cand)
  .cur <- setNames(.cand, .cand)
  .bump <- function(v) {
    for (.v in v) {
      .ver[.v] <<- .ver[.v] + 1L
      .cur[.v] <<- paste0(.prefix, .ver[.v], "__", .v)
    }
  }
  ## tokens are found in `code` (strings blanked, same columns) and replaced in `txt`
  .ren <- function(txt, code) {
    .m <- gregexpr(.rxOptSsaIdRe, code, perl = TRUE)
    .t <- regmatches(code, .m)[[1]]
    .w <- .t %in% .cand
    if (any(.w)) {
      .t[.w] <- unname(.cur[.t[.w]])
      regmatches(txt, .m) <- list(.t)
    }
    txt
  }
  .isLhs <- !is.na(.lhs) & .lhs %in% .cand
  .stack <- list()
  .out <- .lines
  ## only lines that read or assign a versioned variable, or open/close a block
  .candRe <- paste0(
    "(?<![A-Za-z0-9_.])(",
    paste(gsub(".", "\\.", .cand, fixed = TRUE), collapse = "|"),
    ")(?![A-Za-z0-9_.])"
  )
  .todo <- which(.isLhs | .opens > 0L | .closes > 0L | grepl(.candRe, .sp$code, perl = TRUE))
  for (.i in .todo) {
    for (.k in seq_len(.closes[.i])) {
      if (length(.stack) > 0L) {
        .set <- .stack[[length(.stack)]]
        .stack[[length(.stack)]] <- NULL
        .bump(.set)
        if (length(.stack) > 0L) {
          .stack[[length(.stack)]] <- unique(c(.stack[[length(.stack)]], .set))
        }
      }
    }
    if (grepl("^[ \t}]*while[ \t]*\\(", .sp$code[.i]) && .opens[.i] > 0L) {
      .end <- .rxOptSsaBlockEnd(.opens, .closes, .i)
      .bump(unique(.lhs[.i:.end][.isLhs[.i:.end]]))
    }
    .read <- .ren(.sp$read[.i], .sp$code[.i])
    .head <- .sp$head[.i]
    if (.isLhs[.i]) {
      .bump(.lhs[.i])
      .head <- sub(.lhs[.i], .cur[.lhs[.i]], .head, fixed = TRUE)
      if (length(.stack) > 0L) {
        .stack[[length(.stack)]] <- unique(c(.stack[[length(.stack)]], .lhs[.i]))
      }
    }
    .out[.i] <- paste0(.head, .read)
    for (.k in seq_len(.opens[.i])) {
      .stack[[length(.stack) + 1L]] <- character(0)
    }
  }
  list(txt = paste(.out, collapse = "\n"), prefix = .prefix)
}

#' Undo .rxOptSsa() in the optimized text
#'
#' @param txt optimized model text
#' @param ssa output of .rxOptSsa()
#' @return text with the original variable names
#' @noRd
.rxOptSsaUndo <- function(txt, ssa) {
  if (is.null(ssa) || !is.character(txt)) {
    return(txt)
  }
  gsub(paste0("(?<![A-Za-z0-9_.])", ssa$prefix, "[0-9]+__"), "", txt, perl = TRUE)
}
