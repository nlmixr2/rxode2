rxTest({
  # .onLoad() registers the S3 methods for suggested packages without
  # loading them.  .s3register() registers immediately when the other
  # namespace is already loaded and otherwise installs an onLoad hook, so
  # check both halves: the method is reachable, and it landed in the other
  # package's S3 methods table once that package is loaded.

  .rxS3Registered <- function(pkg, generic, class) {
    if (!isNamespaceLoaded(pkg)) return(NA)
    # Resolve the generic the way .s3register() does.  Prefer the exported
    # object so an unrelated same-named generic cannot be inherited from
    # base/stats and passed on; fall back to what the namespace sees, since
    # `units` has no `units<-` of its own and that call targets base's.
    .gen <- try(getExportedValue(pkg, generic), silent = TRUE)
    if (inherits(.gen, "try-error")) {
      .gen <- try(get(generic, envir = asNamespace(pkg)), silent = TRUE)
    }
    if (inherits(.gen, "try-error")) return(NA)
    # registerS3method() files a method under the environment that DEFINES the
    # generic, which is not always `pkg`: base for a primitive, and base for a
    # generic a package re-exports (`units` re-exports base's `units<-`)
    .defenv <- if (typeof(.gen) == "closure") environment(.gen) else asNamespace("base")
    .tbl <- try(get(".__S3MethodsTable__.", envir = .defenv, inherits = FALSE),
                silent = TRUE)
    if (inherits(.tbl, "try-error")) return(NA)
    exists(paste0(generic, ".", class), envir = .tbl, inherits = FALSE)
  }

  # Read the cases out of .onLoad() itself rather than repeating them, so
  # adding a registration cannot leave this file behind.  Match over the whole
  # body at once: a wrapped or doubled-up line would defeat a per-line regex.
  .rxS3Src <- paste(deparse(body(rxode2:::.onLoad), width.cutoff = 500L), # nolint
                    collapse = "\n")
  .rxS3Found <- regmatches(
    .rxS3Src,
    gregexec('\\.s3register\\("([^":]+)::([^"]+)", *"([^"]+)"\\)', .rxS3Src))[[1]]
  .rxS3Cases <- if (length(.rxS3Found) == 0L) list() else {
    apply(.rxS3Found, 2L,
          function(.m) list(pkg = .m[2], generic = .m[3], class = .m[4]))
  }

  test_that("every .onLoad() registration is covered below", {
    # a call the regex cannot read would otherwise be skipped silently
    .nCalls <- gregexpr(".s3register(", .rxS3Src, fixed = TRUE)[[1]]
    .nCalls <- if (identical(as.integer(.nCalls), -1L)) 0L else length(.nCalls)
    expect_equal(length(.rxS3Cases), .nCalls)
    expect_true(length(.rxS3Cases) > 20L)
    expect_false(any(vapply(.rxS3Cases,
                            function(.c) anyNA(unlist(.c)), logical(1))))
  })

  # `units<-.rxEvid` is the one case NAMESPACE also registers statically, so
  # for it this only shows dispatch will work, not that the hook fired.
  for (.case in .rxS3Cases) {
    test_that(sprintf("%s.%s is registered for %s",
                      .case$generic, .case$class, .case$pkg), {
      skip_if_not_installed(.case$pkg)
      # loading the namespace now must fire the deferred registration hook
      loadNamespace(.case$pkg)
      expect_true(isTRUE(.rxS3Registered(.case$pkg, .case$generic, .case$class)))
    })
  }

  test_that("suggested packages dispatch on rxode2 classes", {
    skip_if_not_installed("tibble")
    skip_if_not_installed("dplyr")
    .ev <- et(amt = 10) |> et(0:5)
    expect_s3_class(tibble::as_tibble(.ev), "tbl_df")
    # mutate.rxEt() dispatches and returns a tibble, not an rxEt
    .mut <- dplyr::mutate(.ev, .zz = 1)
    expect_true(".zz" %in% names(.mut))
    expect_equal(nrow(.mut), nrow(as.data.frame(.ev)))
  })
})
