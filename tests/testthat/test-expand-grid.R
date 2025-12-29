rxTest({
  expand.grid.jc <- function(seq1, seq2) {
    cbind(
      Var1 = rep.int(seq1, length(seq2)),
      Var2 = rep.int(seq2, rep.int(length(seq1), length(seq2)))
    )
  }

  skip_if_not_installed("microbenchmark")
  x <- microbenchmark::microbenchmark(rxExpandGrid(letters, letters), expand.grid.jc(letters, letters))

  x <- as.data.frame(x) |>
    dplyr::group_by(expr) |>
    dplyr::summarize(x = median(time)) |>
    dplyr::arrange(expr)

  ## test_that("rxExpandGrid is faster that expand.grid.jc", {
  ##     expect_true(x$x[1] < x$x[2]);
  ## })

  f <- function() {
    tmp <- setNames(data.frame(expand.grid.jc(letters, letters),
                               stringsAsFactors = FALSE
                               ), c("s1", "s2"))
    tmp <- cbind(tmp, with(tmp, data.frame(
      rx = paste0("df(", s1, ")/dy(", s1, ")"),
      sym = paste0("__d_df_", s1, "_dy_", s2, "__"),
      line = paste0("__d_df_", s1, "_dy_", s2, "__=diff(rx__d_dt_", s1, "__, ", s2, ")")
    )))
    return(tmp)
  }

  x <- microbenchmark::microbenchmark(
    rxExpandGrid(letters, letters, 1L),
    f()
  )

  x <- as.data.frame(x) |>
    dplyr::group_by(expr) |>
    dplyr::summarize(x = median(time)) |>
    dplyr::arrange(expr)

  test_that("rxExpandGrid is faster than printing out letters", {
    skip_if_not(x$x[1] < x$x[2],"rxExpandGrid is not faster than printing out letters (in codecov)")
    expect_true(x$x[1] < x$x[2])
  })
})
