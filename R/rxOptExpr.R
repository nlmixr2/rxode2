.addExpr <- function(.ret) {
  .new <- .rxOptEnv$.rep[[.ret]]
  if (!is.null(.new)) {
    if (length(.rxOptEnv$.exclude) != 1) .rxOptEnv$.exclude <- ""
    if (.new == .rxOptEnv$.exclude) {
      return(.ret)
    } else {
      .rxOptEnv$.new <- c(.rxOptEnv$.new, .new)
      return(.new)
    }
  } else {
    if (is.null(.rxOptEnv$.list[[.ret]])) {
      .rxOptEnv$.list[[.ret]] <- 1L
    } else {
      .rxOptEnv$.list[[.ret]] <- .rxOptEnv$.list[[.ret]] + 1L
    }
  }
  return(.ret)
}

.rxOptFn <- function(fn) {
  force(fn)
  function(...) {
    .ret <- paste0(fn, "(", paste(unlist(list(...)), collapse = ", "), ")")
    return(.addExpr(.ret))
  }
}
.rxOptBin <- function(sep) {
  force(sep)
  function(e1, e2) {
    .add <- TRUE
    if (missing(e2)) {
      if (sep == "+") {
        .ret <- paste0(e1)
      } else {
        .ret <- paste0(gsub(" ", "", sep), e1)
      }
      if (regexpr(rex::rex(start, any_spaces, regNum, any_spaces, end),
        .ret,
        perl = TRUE
      ) != -1) {
        .add <- FALSE
      }
    } else {
      if (sep == "^" && isTRUE(checkmate::checkIntegerish(suppressWarnings(as.numeric(e2)),
        lower = 2, any.missing = FALSE
      ))) {
        .ret <- paste0("(", paste(rep(paste0("(", e1, ")"), as.numeric(e2)), collapse = "*"), ")")
      } else {
        if ((regexpr(rex::rex(start, any_spaces, regNum, any_spaces, end),
          paste0(e1),
          perl = TRUE
        ) != -1) &&
          (regexpr(rex::rex(start, any_spaces, regNum, any_spaces, end),
            paste0(e2),
            perl = TRUE
          ) != -1)) {
          .add <- FALSE
        }
        .ret <- paste0(e1, sep, e2)
      }
    }
    if (.add) {
      return(.addExpr(.ret))
    } else {
      return(.ret)
    }
  }
}

.rxOptEnv <- new.env(parent = emptyenv())
.rxOptEnv[["^"]] <- .rxOptBin("^")
.rxOptEnv[["**"]] <- .rxOptBin("^")

.rxOptEnv[["*"]] <- .rxOptBin("*")
.rxOptEnv[["/"]] <- .rxOptBin("/")
.rxOptEnv[["+"]] <- .rxOptBin("+")
.rxOptEnv[["-"]] <- .rxOptBin("-")
.rxOptEnv[["&&"]] <- .rxOptBin("&&")
.rxOptEnv[["||"]] <- .rxOptBin("||")
.rxOptEnv[["|"]] <- .rxOptBin("|")
.rxOptEnv[["&"]] <- .rxOptBin("&")
.rxOptEnv[["<="]] <- .rxOptBin("<=")
.rxOptEnv[[">="]] <- .rxOptBin(">=")
.rxOptEnv[["<"]] <- .rxOptBin("<")
.rxOptEnv[[">"]] <- .rxOptBin(">")
.rxOptEnv[["=="]] <- .rxOptBin("==")
.rxOptEnv[["!="]] <- .rxOptBin("!=")
.rxOptEnv[["["]] <- function(name, val) {
  .n <- toupper(name)
  .err <- gettext("rxode2 only supports THETA[#] and ETA[#] numbers")
  if (any(.n == c("THETA", "ETA")) && is.numeric(val)) {
    if (round(val) == val && val > 0) {
      return(sprintf("%s[%s]", .n, val))
    } else {
      stop(.err, call. = FALSE)
    }
  } else {
    stop(.err, call. = FALSE)
  }
}
.rxOptEnv[["{"]] <- function(...) {
  return(sprintf("{\n%s\n}", paste(unlist(list(...)), collapse = "\n")))
}
.rxOptEnv[["["]] <- function(name, val) {
  .n <- toupper(name)
  .err <- gettext("rxode2 only supports THETA[#] and ETA[#] numbers")
  if (any(.n == c("THETA", "ETA")) && is.numeric(val)) {
    if (round(val) == val && val > 0) {
      return(sprintf("%s[%s]", .n, val))
    } else {
      stop(err, call. = FALSE)
    }
  } else {
    stop(err, call. = FALSE)
  }
}

.rxOptEnv[["("]] <- unaryOp("(", ")")

.rxOptEnv$.list <- list()
.rxOptEnv$.rep <- list()
.rxOptEnv$.exclude <- ""
.rxOptEnv$.new <- NULL
.rxOptEnv$.added <- NULL
.rxOptGetEnv <- function(expr) {
  ## Known functions
  .calls <- allCalls(expr)
  .callList <- setNames(lapply(.calls, .rxOptFn), .calls)
  .callEnv <- list2env(.callList)
  .currEnv <- cloneEnv(.rxOptEnv, .callEnv)
  .names <- allNames(expr)
  .n1 <- .names
  .n2 <- .names
  .symbolList <- setNames(as.list(.n2), .n1)
  .symbolEnv <- list2env(.symbolList, parent = .currEnv)
  return(.symbolEnv)
}

.rxOptExpr <- function(x) {
  x <- .convStr(x)
  .ret <- eval(x, .rxOptGetEnv(x))
  .ret <- eval(parse(text = paste0("quote(", .ret, ")")))
  return(..rxOpt(.ret))
}

..rxOptLhs <- function(x) {
  if (is.atomic(x) || is.name(x)) {
    return(as.character(x))
  } else if (is.call(x)) {
    if (identical(x[[1]], quote(`/`))) {
      return(paste0(..rxOptLhs(x[[2]]), "/", ..rxOptLhs(x[[3]])))
    } else if (identical(x[[1]], quote(`(`))) {
      return(paste0("(", ..rxOptLhs(x[[2]]), ")"))
    } else if (identical(x[[1]], quote(`dt`))) {
      return(paste0("dt(", ..rxOptLhs(x[[2]]), ")"))
    } else if (identical(x[[1]], quote(`f`))) {
      return(paste0("f(", ..rxOptLhs(x[[2]]), ")"))
    } else if (identical(x[[1]], quote(`F`))) {
      return(paste0("F(", ..rxOptLhs(x[[2]]), ")"))
    } else if (identical(x[[1]], quote(`rate`))) {
      return(paste0("rate(", ..rxOptLhs(x[[2]]), ")"))
    } else if (identical(x[[1]], quote(`alag`))) {
      return(paste0("alag(", ..rxOptLhs(x[[2]]), ")"))
    } else if (identical(x[[1]], quote(`lag`))) {
      return(paste0("alag(", ..rxOptLhs(x[[2]]), ")"))
    } else if (identical(x[[1]], quote(`dur`))) {
      return(paste0("dur(", ..rxOptLhs(x[[2]]), ")"))
    } else if (identical(x[[1]], quote(`past`))) {
      return(paste0("past(", ..rxOptLhs(x[[2]]), ",", ..rxOptLhs(x[[3]]), ")"))
    } else if (identical(x[[1]], quote(`dy`))) {
      return(paste0("dy(", ..rxOptLhs(x[[2]]), ")"))
    } else if (identical(x[[1]], quote(`df`))) {
      return(paste0("df(", ..rxOptLhs(x[[2]]), ")"))
    } else if (identical(x[[2]], 0)) {
      return(paste0(as.character(x[[1]]), "(0)"))
    } else {
      print(x)
      stop("unsupported lhs in optimize expression")
    }
  }
}

..rxOpt <- function(x, progress = FALSE) {
  if (is.atomic(x)) {
    return(as.character(x))
  } else if (is.name(x)) {
    return(.rxRepRxQ(as.character(x)))
  } else if (is.call(x)) {
    .x2 <- x[-1]
    if (identical(x[[1]], quote(`{`))) {
      if (progress) {
        rxProgress(length(.x2))
        on.exit({
          rxProgressAbort("stopped optimizing duplicate expressions")
        })
        .ret <- unlist(lapply(.x2, function(x) {
          rxTick()
          ..rxOpt(x)
        }))
        rxProgressStop()
        return(.ret)
      } else {
        return(unlist(lapply(.x2, ..rxOpt)))
      }
    } else if (identical(x[[1]], quote(`(`))) {
      .x2 <- x[[2]]
      .test <- TRUE
      while (.test) {
        if (length(.x2) == 2) {
          if (identical(.x2[[1]], quote(`(`))) {
            .x2 <- .x2[[2]]
          } else {
            .test <- FALSE
          }
        } else {
          .test <- FALSE
        }
      }
      return(paste0("(", ..rxOpt(.x2), ")"))
    } else if (identical(x[[1]], quote(`*`)) ||
      identical(x[[1]], quote(`^`)) ||
      identical(x[[1]], quote(`+`)) ||
      identical(x[[1]], quote(`-`)) ||
      identical(x[[1]], quote(`/`)) ||
      identical(x[[1]], quote(`==`)) ||
      identical(x[[1]], quote(`>=`)) ||
      identical(x[[1]], quote(`<=`)) ||
      identical(x[[1]], quote(`>`)) ||
      identical(x[[1]], quote(`<`)) ||
      identical(x[[1]], quote(`!=`)) ||
      identical(x[[1]], quote(`&&`)) ||
      identical(x[[1]], quote(`||`)) ||
      identical(x[[1]], quote(`&`)) ||
      identical(x[[1]], quote(`|`))) {
      if (length(x) == 3) {
        if (length(x[[2]]) == 2) {
          if (identical(x[[2]][[1]], quote(`-`)) &&
            is.atomic(x[[2]][[2]])) {
            if (is.atomic(x[[3]])) {
              if (identical(x[[1]], quote(`/`))) {
                return(as.character(-x[[2]][[2]] / x[[3]]))
              } else if (identical(x[[1]], quote(`+`))) {
                return(as.character(-x[[2]][[2]] + x[[3]]))
              } else if (identical(x[[1]], quote(`-`))) {
                return(as.character(-x[[2]][[2]] - x[[3]]))
              } else if (identical(x[[1]], quote(`*`))) {
                return(as.character(-x[[2]][[2]] * x[[3]]))
              }
            }
          }
          if (x[[2]][[2]] == 1 &&
            identical(x[[1]], quote(`*`))) {
            return(paste0("-", ..rxOpt(x[[3]])))
          }
        }
        if (is.atomic(x[[2]]) && is.atomic(x[[3]])) {
          if (identical(x[[1]], quote(`/`))) {
            return(as.character(x[[2]] / x[[3]]))
          } else if (identical(x[[1]], quote(`+`))) {
            return(as.character(x[[2]] + x[[3]]))
          } else if (identical(x[[1]], quote(`-`))) {
            return(as.character(x[[2]] - x[[3]]))
          } else if (identical(x[[1]], quote(`*`))) {
            return(as.character(x[[2]] * x[[3]]))
          }
        }
        if (is.atomic(x[[2]])) {
          if (x[[2]] == 1) {
            if (identical(x[[1]], quote(`*`))) {
              return(..rxOpt(x[[3]]))
            }
          }
          if (x[[2]] == 0) {
            if (identical(x[[1]], quote(`*`))) {
              return("0")
            } else if (identical(x[[1]], quote(`+`))) {
              return(..rxOpt(x[[3]]))
            } else if (identical(x[[1]], quote(`-`))) {
              return(paste0("-", ..rxOpt(x[[3]])))
            } else if (identical(x[[1]], quote(`/`))) {
              return("0")
            }
          }
        }
        if (is.atomic(x[[3]])) {
          if (x[[3]] == 1) {
            if (identical(x[[1]], quote(`*`))) {
              return(..rxOpt(x[[2]]))
            }
          }
          if (x[[3]] == 0) {
            if (identical(x[[1]], quote(`*`))) {
              return("0")
            } else if (identical(x[[1]], quote(`+`))) {
              return(..rxOpt(x[[2]]))
            } else if (identical(x[[1]], quote(`-`))) {
              return(..rxOpt(x[[2]]))
            } else if (identical(x[[2]], quote(`/`))) {
              stop("cannot divide by zero", call. = FALSE)
            }
          }
        }
        return(paste0(
          ..rxOpt(x[[2]]), as.character(x[[1]]),
          ..rxOpt(x[[3]])
        ))
      } else {
        ## Unary Operators
        return(paste0(
          as.character(x[[1]]),
          ..rxOpt(x[[2]])
        ))
      }
    } else if (identical(x[[1]], quote(`~`)) ||
      identical(x[[1]], quote(`=`)) ||
      identical(x[[1]], quote(`<-`))) {
      .rxOptEnv$.new <- NULL
      .x3 <- .rxOptExpr(x[[3]])
      if (length(.x3) == 3) {
        if (any(.x3[1] == c("/", "*", "+", "-"))) {
          .x3 <- paste0(.x3[2], .x3[1], .x3[3])
        }
      }
      if (length(.x3) != 1) {
        stop("error optimizing expression, try 'optExpression=FALSE'")
      }
      .ret <- paste0(
        ..rxOptLhs(x[[2]]),
        ifelse(identical(x[[1]], quote(`<-`)),
          "=", as.character(x[[1]])
        ),
        .x3
      )
      .extra <- NULL
      if (length(.rxOptEnv$.new) > 0) {
        for (.i in seq_along(.rxOptEnv$.rep)) {
          if (any(.rxOptEnv$.rep[[.i]] == .rxOptEnv$.new) &&
            !any(.rxOptEnv$.rep[[.i]] == .rxOptEnv$.added)) {
            .cur <- .rxOptEnv$.rep[[.i]]
            if (.i != 1) {
              for (.j in seq(1, .i - 1)) {
                while (!any(.rxOptEnv$.rep[[.j]] == .rxOptEnv$.added) &&
                  regexpr(
                    rex::rex(or(.cur)),
                    names(.rxOptEnv$.rep)[.j]
                  ) != -1) {
                  .extra <- c(.extra, paste0(
                    .rxOptEnv$.rep[[.j]],
                    "~", names(.rxOptEnv$.rep)[.j]
                  ))
                  .rxOptEnv$.added <- c(
                    .rxOptEnv$.added,
                    .rxOptEnv$.rep[.j]
                  )
                }
              }
            }
            .extra <- c(
              .extra,
              paste0(
                .rxOptEnv$.rep[[.i]], "~",
                ..rxOpt(eval(parse(text = paste0("quote(", names(.rxOptEnv$.rep)[.i], ")"))))
              )
            )
            .rxOptEnv$.added <- c(
              .rxOptEnv$.added,
              .rxOptEnv$.rep[.i]
            )
          }
        }
      }
      return(paste(c(.extra, .ret), collapse = "\n"))
    } else if (identical(x[[1]], quote(`[`))) {
      return(paste0(..rxOpt(x[[2]]), "[", ..rxOpt(x[[3]]), "]"))
    } else {
      .ret0 <- lapply(x, ..rxOpt)
      .ret <- paste0(.ret0[[1]], "(")
      if (.ret == "((") .ret <- "("
      .ret0 <- .ret0[-1]
      .ret <- paste0(.ret, paste(unlist(.ret0), collapse = ", "), ")")
      return(.ret)
    }
  }
}

# -- Chunked optimization ------------------------------------------------------

# Split the model into contiguous chunks of roughly `targetChars` characters.
# Optimizing a chunk is strongly superlinear in its size, so the largest chunk
# dominates the total; balancing on characters rather than lines minimizes it,
# since a sensitivity equation costs far more than a short assignment.
.rxBalancedChunks <- function(lines, targetChars) {
  .w <- pmax(1L, nchar(lines))
  .k <- max(1L, min(length(lines), as.integer(ceiling(sum(.w) / max(1, targetChars)))))
  if (.k <= 1L) return(list(lines))
  unname(split(lines, findInterval(cumsum(.w), seq_len(.k - 1L) * sum(.w) / .k)))
}

# A state initial condition (state(0)=), a dosing modifier (f/F/alag/lag/rate/dur(cmt)=)
# or a delay pre-history (past(cmt, tau)=)
# is a syntax error in a chunk that lacks the matching d/dt() ("'W(0)' present, but
# d/dt(W) not defined").  Such a line cannot simply be moved to the chunk that has the
# d/dt(): its right-hand side may read a variable that a later line reassigns, so its
# position is load-bearing.  Rewrite its left-hand side to a unique plain name *in place*
# instead -- which parses anywhere as an ordinary assignment -- and undo that with
# .rxRestoreCmt() once the chunks are optimized.  The line never moves, so semantics are
# preserved exactly.  Whitespace is kept verbatim (the caller's text need not be
# canonical, since the whole model is deliberately never normalized): a canonical
# left-hand side embeds the compartment name bare (readable), while a spacing variant
# the grammar also accepts (`depot (0)=`, `f( depot )=`) is hex-encoded whole, so the
# disguised name is always a valid identifier and the restore is byte-exact either way.
# past(cmt, tau)= always takes the hex form: its second argument is an arbitrary
# expression (`past(G, exp(THETA[8]))`), which no canonical bare-name form could carry.
# A construct not recognised here is left alone; its chunk then fails and falls back to
# the whole model.
.rxDisguiseCmt <- function(modTxt) {
  .ln <- strsplit(modTxt, "\n", fixed = TRUE)[[1]]
  .eq <- regexpr("=", .ln, fixed = TRUE)
  .raw <- ifelse(.eq > 0L, substr(.ln, 1L, .eq - 1L), "")
  .lhs <- trimws(.raw)
  .lead <- sub("^([ \t]*).*$", "\\1", .raw)
  .trail <- substr(.raw, nchar(.lead) + nchar(.lhs) + 1L, nchar(.raw))
  .rhs <- ifelse(.eq > 0L, substr(.ln, .eq, nchar(.ln)), "")
  .id <- "[a-zA-Z][a-zA-Z0-9_.]*"
  .icRe <- paste0("^(", .id, ")[ \t]*\\(0\\)$")
  .modRe <- paste0("^([fF]|alag|lag|rate|dur)[ \t]*\\([ \t]*(", .id, ")[ \t]*\\)$")
  .pastRe <- paste0("^past[ \t]*\\([ \t]*", .id, "[ \t]*,.*\\)$")
  .isIc <- .eq > 0L & grepl(.icRe, .lhs)
  .isMod <- .eq > 0L & grepl(.modRe, .lhs)
  .isPast <- .eq > 0L & grepl(.pastRe, .lhs)
  .icCan <- .isIc & grepl(paste0("^", .id, "\\(0\\)$"), .lhs)
  .modCan <- .isMod & grepl(paste0("^([fF]|alag|lag|rate|dur)\\(", .id, "\\)$"), .lhs)
  .hex <- (.isIc | .isMod | .isPast) & !(.icCan | .modCan)
  .new <- .lhs
  .new[.icCan] <- paste0("rx__disg_ic__", sub("\\(0\\)$", "", .lhs[.icCan]), "__")
  .mm <- regmatches(.lhs[.modCan], regexec("^([a-zA-Z]+)\\((.*)\\)$", .lhs[.modCan]))
  .new[.modCan] <- vapply(.mm, function(.m) paste0("rx__disg_mod__", .m[2L], "__", .m[3L], "__"),
                          character(1))
  .new[.hex] <- vapply(.lhs[.hex], function(.l) {
    paste0("rx__disg_lhs__", paste(as.character(charToRaw(.l)), collapse = ""), "__")
  }, character(1), USE.NAMES = FALSE)
  paste(ifelse(.isIc | .isMod | .isPast, paste0(.lead, .new, .trail, .rhs), .ln),
        collapse = "\n")
}

# Reverse .rxDisguiseCmt().  Only the left-hand side is rewritten (split at the first
# "="), so the optimized right-hand side is kept verbatim.  The rx__disg_lhs__ form is
# hex-decoded back to the original left-hand side, byte for byte.
.rxRestoreCmt <- function(txt) {
  .ln <- strsplit(txt, "\n", fixed = TRUE)[[1]]
  .disg <- grepl("^[ \t]*rx__disg_(ic|mod|lhs)__", .ln)
  if (any(.disg)) {
    .eq <- regexpr("=", .ln[.disg], fixed = TRUE)
    .raw <- substr(.ln[.disg], 1L, .eq - 1L)
    .rest <- substr(.ln[.disg], .eq, nchar(.ln[.disg]))
    .lead <- sub("^([ \t]*).*$", "\\1", .raw)
    .tok <- trimws(.raw)
    .trail <- substr(.raw, nchar(.lead) + nchar(.tok) + 1L, nchar(.raw))
    .tok <- sub("^rx__disg_ic__(.*)__$", "\\1(0)", .tok)
    .tok <- sub("^rx__disg_mod__([a-zA-Z]+)__(.*)__$", "\\1(\\2)", .tok)
    .isHex <- grepl("^rx__disg_lhs__([0-9a-f][0-9a-f])+__$", .tok)
    .tok[.isHex] <- vapply(sub("^rx__disg_lhs__([0-9a-f]+)__$", "\\1", .tok[.isHex]),
                           function(.h) {
                             rawToChar(as.raw(strtoi(substring(.h, seq(1L, nchar(.h), 2L),
                                                               seq(2L, nchar(.h), 2L)), 16L)))
                           }, character(1), USE.NAMES = FALSE)
    .ln[.disg] <- paste0(.lead, .tok, .trail, .rest)
  }
  paste(.ln, collapse = "\n")
}

# A chunk is itself a (smaller) model, so optimize it with rxOptExpr() and chunking off.
# Each chunk restarts its rx_expr_ counter at zero, so the names one chunk introduces would
# collide with another's once reassembled; prefix them per chunk.
#
# Only the names *this* call introduced may be renamed.  A rx_expr_ name already present in
# the chunk -- the model was optimized before, say -- must be left exactly as it is: its
# definition and its uses can sit in different chunks, and renaming them per chunk would
# rename them differently and pull them apart.  Matching whole words likewise keeps a
# variable that merely contains "rx_expr_" (say `my_rx_expr_var`) from being rewritten.
#
# Errors are deliberately not caught here: a chunk that cannot be optimized must reach
# .rxOptExprChunked(), which falls back to the whole model.
.rxOptExprChunk <- function(i, chunks, msg) {
  .txt <- paste(chunks[[i]], collapse = "\n")
  .o <- suppressMessages(rxOptExpr(.txt, msg, chunkLines = 0L))
  .re <- "\\brx_expr_[0-9]+\\b"
  .new <- setdiff(unique(regmatches(.o, gregexpr(.re, .o))[[1]]),
                  unique(regmatches(.txt, gregexpr(.re, .txt))[[1]]))
  for (.v in .new) {
    .o <- gsub(paste0("\\b", .v, "\\b"),
               sub("^rx_expr_", sprintf("rx_expr_c%d_", i), .v), .o)
  }
  .o
}

# Chunked (optionally parallel) common subexpression optimization.  Subexpressions are only
# shared within a chunk, so the optimized text is not the text the whole-model call would
# produce -- it carries more temporaries -- but it is an equivalent model, with the same
# states and parameters and the same solution, and a malformed model still raises the same
# error.  What is optimized changes; what the model *means* does not.
.rxOptExprChunked <- function(x, msg = "model", chunkLines = 40L, parallel = 0L) {
  # Never rxNorm() the whole model here.  Normalizing (i.e. parsing) is itself strongly
  # superlinear in model size, and is what actually dominates optimizing a large model: on a
  # 275-line augmented model the whole-model call is ~113s, of which the common subexpression
  # search is only ~15s.  Chunking is fast precisely because rxOptExpr() normalizes each
  # chunk on its own, so normalizing the whole model here would pay the very cost this is
  # avoiding.  Text is already line-oriented and is split as-is; only an object needs rxNorm.
  #
  # Mirror how rxModelVars() reads a character, in its order: a length-1 string may be a
  # filename (the file holds the model text; read it) or a registered model name (it has no
  # "=", "<-" or "~"; only rxNorm() can resolve it); anything else is literal model text.
  if (is.character(x) && length(x) == 1L &&
        isTRUE(tryCatch(file.exists(x), error = function(e) FALSE,
                        warning = function(w) FALSE))) {
    x <- readLines(x, warn = FALSE)
  } else if (is.character(x) && length(x) == 1L && !grepl("[=~]|<-", x)) {
    x <- rxNorm(x)
  }
  .txt <- if (is.character(x)) paste(x, collapse = "\n") else rxNorm(x)
  .ln <- strsplit(.txt, "\n", fixed = TRUE)[[1]]
  if (length(.ln) <= chunkLines) {
    return(rxOptExpr(.txt, msg = msg, chunkLines = 0L))
  }
  # Chunking introduces names of its own into the model's namespace: rx_expr_c<i>_ for the
  # temporaries a chunk contributes, and rx__disg_ while a compartment-scoped line is
  # disguised.  A model that already uses such a name would have it silently captured --
  # renaming or restoring would rewrite the model's own variable -- so do not chunk it.
  if (grepl("rx_expr_c[0-9]", .txt) || grepl("rx__disg_", .txt, fixed = TRUE)) {
    return(rxOptExpr(.txt, msg = msg, chunkLines = 0L))
  }

  # Disguise compartment-scoped left-hand sides so that every chunk parses standalone.
  .chunks <- .rxBalancedChunks(strsplit(.rxDisguiseCmt(.txt), "\n", fixed = TRUE)[[1]],
                               mean(pmax(1L, nchar(.ln))) * chunkLines)
  .nChunks <- length(.chunks)

  # `parallel` carries rxControl(cores=)'s semantics: 0 means the rxode2 thread setting
  # (rxCores(), which setRxThreads()/OMP_THREAD_LIMIT control -- so CRAN and users tune
  # this with the same knob as the solver), n > 0 means n.  Never use more daemons than
  # there are chunks, nor more than that thread setting; a single daemon has no
  # parallelism to offer, only dispatch overhead, so 1 runs serially.
  .nDaemons <- as.integer(parallel)
  if (is.na(.nDaemons) || .nDaemons < 0L) .nDaemons <- 0L
  if (.nDaemons == 0L) .nDaemons <- max(1L, as.integer(rxCores()))
  .nDaemons <- min(.nDaemons, .nChunks, max(1L, as.integer(rxCores())))
  .useMirai <- .nDaemons > 1L
  # A caller's existing mirai pool is used as-is and never shut down; a pool of our own
  # is only worth starting (a few seconds: each daemon loads rxode2) when there are
  # enough chunks for the parallel win to beat that startup.
  .ownDaemons <- FALSE
  if (.useMirai) {
    .have <- tryCatch(sum(mirai::status()$connections) > 0L, error = function(e) FALSE)
    if (!.have) {
      .ownDaemons <- .nChunks >= 4L
      .useMirai <- .ownDaemons
    }
  }
  .malert(sprintf("optimizing duplicate expressions in %s (%d chunks%s)...", msg, .nChunks,
                  if (.useMirai) sprintf(", %d daemons", .nDaemons) else ""))

  .opt <- tryCatch({
    if (.useMirai) {
      if (.ownDaemons) {
        mirai::daemons(.nDaemons)
        on.exit(mirai::daemons(0), add = TRUE)
      }
      # `.rxOptExprChunk` is passed as an argument, carrying the rxode2 namespace as its
      # environment, so a chunk is optimized by the same code path serially and in parallel.
      .tasks <- mirai::mirai_map(
        seq_len(.nChunks),
        function(.i, .chunks, .msg, .optOne) {
          library(rxode2)
          .optOne(.i, .chunks, .msg)
        },
        .args = list(.chunks = .chunks, .msg = msg, .optOne = .rxOptExprChunk)
      )
      # A chunk that failed in a daemon comes back as an error object rather than throwing,
      # and would otherwise be pasted into the model text; raise it so the fallback below
      # optimizes the whole model instead.
      .res <- character(.nChunks)
      for (.i in seq_len(.nChunks)) {
        .r <- .tasks[[.i]][]
        if (inherits(.r, "miraiError") || inherits(.r, "errorValue") || !is.character(.r)) {
          stop(sprintf("parallel chunk %d failed in a mirai daemon: %s", .i,
                       tryCatch(conditionMessage(.r),
                                error = function(e) paste(utils::head(unclass(.r), 1L),
                                                          collapse = ""))),
               call. = FALSE)
        }
        .res[.i] <- .r
      }
      .res
    } else {
      vapply(seq_len(.nChunks), .rxOptExprChunk, character(1), chunks = .chunks, msg = msg)
    }
  }, error = function(e) NULL)

  # A chunk is only a fragment of the model, so it can fail to optimize where the whole
  # model would not -- it may hold a compartment-scoped line the disguise did not recognise,
  # or only part of a statement.  It can equally fail because the model is malformed, and
  # the chunk alone cannot tell those apart.  The whole model can: it optimizes cleanly for
  # a mere fragment, and raises exactly what the unchunked call raises for a broken model.
  # (A chunk with nothing left to reduce returns its text and does not error, so an ordinary
  # model never lands here.)
  if (is.null(.opt)) {
    return(rxOptExpr(.txt, msg = msg, chunkLines = 0L))
  }
  .rxRestoreCmt(paste(.opt, collapse = "\n"))
}

#' Optimize rxode2 for computer evaluation
#'
#' This optimizes rxode2 code for computer evaluation by only
#' calculating redundant expressions once.
#'
#' @param x rxode2 model that can be accessed by rxNorm
#'
#' @param msg This is the name of type of object that rxode2 is
#'     optimizing that will in the message when optimizing.  For
#'     example "model" will produce the following message while
#'     optimizing the model:
#'
#'  finding duplicate expressions in model...
#'
#' @param chunkLines Integer; when positive (the default is 40),
#'     a model longer than this many lines is optimized in contiguous
#'     cost-balanced chunks of roughly this many lines instead of in a
#'     single pass; a model at or under it is optimized whole, exactly
#'     as before.  `0` always optimizes the whole model at once.
#'
#'     Chunking pays off for a large machine-generated model -- a
#'     sensitivity- or Jacobian-augmented model, say.  Normalizing a
#'     model (`rxNorm()`, i.e. parsing it) is strongly superlinear in
#'     its size, and for such a model it, not the common subexpression
#'     search, is what dominates: optimizing a 275-line augmented model
#'     takes ~113s, of which the subexpression search is only ~15s.
#'     Chunking amortizes that parse -- `rxOptExpr()` normalizes each
#'     chunk on its own -- taking the same model to ~11s:
#'
#'     \tabular{rrrr}{
#'       lines \tab whole \tab chunked \tab \cr
#'       34 \tab 0.5s \tab 0.5s \tab (a typical model: not chunked) \cr
#'       119 \tab 2.0s \tab 0.8s \tab 2.5x \cr
#'       149 \tab 22.2s \tab 3.9s \tab 5.7x \cr
#'       275 \tab 112.7s \tab 10.6s \tab 10.7x \cr
#'     }
#'
#'     Common subexpressions are then only shared within a chunk, so the
#'     model is equivalent but carries more temporaries.  That costs no
#'     measurable solve time, but it does make the C compilation of the
#'     model somewhat slower, which partly offsets the gain.
#'
#'     A chunk is a fragment, so it can fail to optimize where the whole
#'     model would not.  If any chunk fails, the whole model is optimized
#'     instead, so a malformed model still raises the error the unchunked
#'     call raises; falling back costs the unchunked time only on that
#'     rare path.
#'
#'     Chunking therefore does not give the same optimized text as the
#'     whole-model call -- it shares fewer subexpressions and so carries
#'     more temporaries -- but it gives an equivalent model: the same
#'     states and parameters, the same solution, and the same errors.
#'
#' @param parallel Integer; number of `mirai` daemons used to optimize
#'     the chunks in parallel.  Only used when the model is chunked.  It
#'     carries the same semantics as `rxControl(cores=)`: `0` (the
#'     default) means the rxode2 thread setting `rxCores()`, so CRAN and
#'     users tune it with the same knob as the solver (`setRxThreads()`,
#'     `OMP_THREAD_LIMIT`) or by passing `parallel=` directly; `1` runs
#'     the chunks serially.
#'     It is capped by the number of chunks and by `rxCores()`, so it
#'     will not oversubscribe past the threads the user asked for.
#'
#'     An existing `mirai` daemon pool is used as-is and left running.
#'     Otherwise a pool is started for the call and shut down when it
#'     returns; that startup (loading rxode2 into each daemon) costs a
#'     few seconds, so a pool is only started when the model splits into
#'     at least 4 chunks, where the parallel win covers it.
#'
#' @return Optimized rxode2 model text.  The order and type lhs and
#'     state variables is maintained while the evaluation is sped up.
#'     While parameters names are maintained, their order may be
#'     modified.
#'
#' @author Matthew L. Fidler
#' @export
rxOptExpr <- function(x, msg = "model", chunkLines = 40L,
                      parallel = 0L) {
  .chunkLines <- as.integer(chunkLines)
  if (!is.na(.chunkLines) && .chunkLines > 0L) {
    return(.rxOptExprChunked(x, msg = msg, chunkLines = .chunkLines, parallel = parallel))
  }
  .oldOpts <- options()
  options(digits = 22)
  on.exit(options(.oldOpts))
  .mv <- rxModelVars(x)
  .params <- .mv$params
  .rxOptEnv$.list <- list()
  .rxOptEnv$.rep <- list()
  .rxOptEnv$.added <- NULL
  .rxOptEnv$.exclude <- ""
  .malert(sprintf("finding duplicate expressions in %s...", msg))
  .p <- eval(parse(text = paste0("quote({", rxNorm(x), "})")))
  .lines <- ..rxOpt(.p, progress = TRUE)
  .rxOptEnv$.list <- .rxOptEnv$.list[which(unlist(.rxOptEnv$.list) > 1L)]
  .exprs <- names(.rxOptEnv$.list)[order(nchar(names(.rxOptEnv$.list)))]
  .exprs <- .exprs[regexpr(rex::rex(start, regNum, end), .exprs,
    perl = TRUE
  ) == -1]
  .thetaEtaR <- rex::rex(start, or("THETA[", "ETA["), any_numbers, "]", end)
  .exprs <- .exprs[regexpr(.thetaEtaR, .exprs, perl = TRUE) == -1]
  if (length(.exprs) > 0) {
    ## Take out unary [-] that way
    ## expr1=-ka       #nolint
    ## expr2 = expr-ka #nolint
    ## will not become expr = exprka where exprka isn't defined.
    .exprs <- .exprs[regexpr("^[-]", .exprs) == -1]
    if (length(.exprs) == 0) {
      return(x)
    }
    .rp <- rxOptRep_(.exprs)
    .rxOptEnv$.rep <- as.list(.rp[[1]])
    .rxOptEnv$.exclude <- ""
    .malert(sprintf("optimizing duplicate expressions in %s...", msg))
    .opt <- ..rxOpt(.p, progress = TRUE)
    return(paste(.opt, collapse = "\n"))
  } else {
    return(paste(.lines, collapse = "\n"))
  }
}
