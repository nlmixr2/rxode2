# Every allocation in rxode2's C/C++ goes through the tagged allocators of
# inst/include/rxMemAlloc.h, which name the rxSolve() memory-estimate component
# that counts it (rxMemoryComponents_() / rxMemoryGpuComponents_()), so the
# estimate cannot drift from what rxode2 allocates.  Allocations that predate
# the tags are held in mem-alloc-baseline.csv, a per-file count that may only
# shrink: tag an allocation and lower its file's count.  Regenerate with
# RXODE2_MEM_ALLOC_BASELINE=write only to lower counts.
.rxMemAllocRoot <- function() {
  .r <- normalizePath(test_path("..", ".."), mustWork = FALSE)
  if (
    file.exists(file.path(.r, "src", "rxMemoryComponents.cpp")) &&
      file.exists(file.path(.r, "inst", "include", "rxMemAlloc.h"))
  ) {
    .r
  } else {
    NULL
  }
}

.rxMemAllocFiles <- function(root) {
  .src <- list.files(file.path(root, "src"), "[.](c|cpp|h)$", full.names = TRUE)
  .inc <- list.files(file.path(root, "inst", "include"), "[.](c|cpp|h)$", full.names = TRUE, recursive = TRUE)
  .f <- c(.src, .inc)
  # generated, and the macros themselves
  .f[
    !basename(.f) %in% c("RcppExports.cpp", "tran.g.d_parser.h", "rxMemAlloc.h") &
      !grepl("_RcppExports[.]h$", .f)
  ]
}

## code lines without comments or string literals
.rxMemAllocCode <- function(file) {
  .l <- readLines(file, warn = FALSE)
  .txt <- paste(.l, collapse = "\n")
  .txt <- gsub("/[*].*?[*]/", " ", .txt, perl = TRUE)
  .l <- strsplit(.txt, "\n", fixed = TRUE)[[1]]
  .l <- sub("//.*$", "", .l)
  gsub("\"([^\"\\\\]|\\\\.)*\"", "\"\"", .l, perl = TRUE)
}

.rxMemAllocRaw <- paste0(
  "\\b(malloc|calloc|realloc|R_alloc|R_Calloc|R_Realloc|Calloc|Realloc)\\s*\\(",
  "|\\bnew\\s+[A-Za-z_][A-Za-z0-9_:<>]*\\s*\\[",
  "|[Cc]reateBuffer\\s*\\(",
  "|[.](resize|assign)\\s*\\("
)
.rxMemAllocTag <- "rxMem(Malloc|Calloc|Realloc|Ralloc|New|Assign|Resize|Tag)\\(\\s*([A-Za-z_][A-Za-z0-9_]*)"

.rxMemAllocScan <- function(root) {
  .f <- .rxMemAllocFiles(root)
  .rel <- substring(.f, nchar(root) + 2L)
  .res <- lapply(.f, function(file) {
    .l <- .rxMemAllocCode(file)
    .tagged <- grepl(.rxMemAllocTag, .l, perl = TRUE)
    .raw <- grepl(.rxMemAllocRaw, .l, perl = TRUE) & !.tagged
    .m <- regmatches(.l, gregexpr(.rxMemAllocTag, .l, perl = TRUE))
    list(raw = sum(.raw), tags = sub(.rxMemAllocTag, "\\2", unlist(.m), perl = TRUE))
  })
  names(.res) <- .rel
  .res
}

.rxMemAllocComponents <- function() {
  .cpu <- rxMemoryComponents_(
    1L,
    1L,
    1L,
    1L,
    1L,
    1L,
    1L,
    1L,
    1L,
    0L,
    0L,
    0L,
    0L,
    0L,
    0L,
    0L,
    1L,
    1,
    1,
    1,
    1L,
    0L,
    0L,
    0L,
    0L
  )
  .gpu <- rxMemoryGpuComponents_(rep(1L, 16L), 0L, 1L, 1L)
  setdiff(c(names(.cpu), names(.gpu), "fixed"), c("sizeofInd", "rxLlikSaveSize"))
}

test_that("every allocation tag names a memory-estimate component", {
  .root <- .rxMemAllocRoot()
  skip_if(is.null(.root), "needs the package source")
  .scan <- .rxMemAllocScan(.root)
  .tags <- unlist(lapply(names(.scan), function(f) {
    .t <- .scan[[f]]$tags
    if (length(.t) == 0L) character(0) else paste0(f, ": ", .t)
  }))
  expect_gt(length(.tags), 0L)
  .bad <- .tags[!sub("^.*: ", "", .tags) %in% .rxMemAllocComponents()]
  expect_identical(.bad, character(0))
})

test_that("no untagged allocation is added (rxMemAlloc.h)", {
  .root <- .rxMemAllocRoot()
  skip_if(is.null(.root), "needs the package source")
  .scan <- .rxMemAllocScan(.root)
  .now <- vapply(.scan, function(x) x$raw, integer(1))
  .now <- .now[.now > 0L]
  .bl <- test_path("mem-alloc-baseline.csv")
  if (identical(Sys.getenv("RXODE2_MEM_ALLOC_BASELINE"), "write")) {
    utils::write.csv(
      data.frame(file = names(.now), raw = unname(.now))[order(names(.now)), ],
      .bl,
      row.names = FALSE,
      quote = FALSE
    )
  }
  .base <- utils::read.csv(.bl, stringsAsFactors = FALSE)
  .was <- stats::setNames(.base$raw, .base$file)
  .files <- union(names(.now), names(.was))
  .n <- function(x, f) if (f %in% names(x)) unname(x[[f]]) else 0L
  .diff <- vapply(.files, function(f) .n(.now, f) - .n(.was, f), numeric(1))
  ## more: allocate through rxMemAlloc.h, naming the component that counts it
  ## (add one to rxMemoryComponents_() if none does)
  expect_identical(names(.diff)[.diff > 0], character(0))
  ## fewer: lower mem-alloc-baseline.csv (RXODE2_MEM_ALLOC_BASELINE=write)
  expect_identical(names(.diff)[.diff < 0], character(0))
})
