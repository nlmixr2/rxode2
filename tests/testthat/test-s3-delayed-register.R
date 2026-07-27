rxTest({
  # .onLoad() registers the S3 methods for suggested packages without
  # loading them.  .s3register() registers immediately when the other
  # namespace is already loaded and otherwise installs an onLoad hook, so
  # check both halves: the method is reachable, and it landed in the other
  # package's S3 methods table once that package is loaded.

  .rxS3Registered <- function(pkg, method) {
    if (!isNamespaceLoaded(pkg)) return(NA)
    .tbl <- try(get(".__S3MethodsTable__.", envir = asNamespace(pkg)),
                silent = TRUE)
    if (inherits(.tbl, "try-error")) return(NA)
    exists(method, envir = .tbl, inherits = FALSE)
  }

  .rxS3Cases <- list(
    list(pkg = "pillar", method = "type_sum.rxEvid"),
    list(pkg = "pillar", method = "pillar_shaft.rxEvid"),
    list(pkg = "tibble", method = "as_tibble.rxEt"),
    list(pkg = "data.table", method = "as.data.table.rxEt"),
    list(pkg = "arrow", method = "as_arrow_table.rxSolveOom"),
    list(pkg = "dplyr", method = "filter.rxEt"),
    list(pkg = "dplyr", method = "dplyr_reconstruct.rxEt"),
    list(pkg = "dplyr", method = "dplyr_reconstruct.rxEtPreview"),
    list(pkg = "nlme", method = "fixef.rxUi"),
    list(pkg = "units", method = "set_units.rxEt"),
    list(pkg = "units", method = "drop_units.rxSolve"),
    list(pkg = "digest", method = "sha1.rxUi")
  )

  for (.case in .rxS3Cases) {
    test_that(sprintf("%s is registered for %s", .case$method, .case$pkg), {
      skip_if_not_installed(.case$pkg)
      # loading the namespace now must fire the deferred registration hook
      loadNamespace(.case$pkg)
      expect_true(isTRUE(.rxS3Registered(.case$pkg, .case$method)))
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
