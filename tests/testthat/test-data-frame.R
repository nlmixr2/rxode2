## rxode2 instance 1
m1 <-
  rxode2(
    model = "
         C2 = centr/V2;
         C3 = peri/V3;
         d/dt(depot) =-KA*depot;
         d/dt(centr) = KA*depot - CL*C2 - Q*C2 + Q*C3;
         d/dt(peri)  =                    Q*C2 - Q*C3;
         d/dt(eff)  = Kin - Kout*(1-C2/(EC50+C2))*eff;"
  )

test_that("rxode2 instance 1 is created", {
  expect_equal(class(m1), "rxode2")
})

et1 <- eventTable(amount.units = "ug", time.units = "hours")
et1$add.dosing(dose = 10000, nbr.doses = 5, dosing.interval = 24)
et1$add.sampling(0:24)
et1$add.sampling(24)
et1$add.sampling(seq(from = 24 + 8, to = 5 * 24, by = 8))

test_that("rxode2 event table 1 was created", {
  expect_s3_class(et1, "rxEt")
  expect_equal(et1$get.nobs(), 38)
  expect_equal(length(et1$get.dosing()[, 1]), 1)
})

o1.first <-
  suppressWarnings(rxSolve(
    m1,
    params = c(
      KA = .291, CL = 18.6, V2 = 40.2, Q = 10.5, V3 = 297.0,
      Kin = 1.0, Kout = 1.0, EC50 = 200.0
    ),
    events = et1,
    inits = c(0, 0, 0, 1)
  ))

o1.df <- as.data.frame(o1.first)
o1.df2 <- tibble::as_tibble(o1.first)

test_that("Numeric Data frame lookup operators [] make sense", {
  expect_equal(o1.first[], o1.df[])
  expect_equal(o1.first[, 3], o1.df[, 3])
  expect_equal(o1.first[, 3, drop = FALSE], o1.df[, 3, drop = FALSE])
  expect_equal(o1.first[3, ], o1.df[3, ])
  expect_equal(o1.first[, c(1, 3)], o1.df[, c(1, 3)])
  expect_equal(o1.first[c(1, 3), ], o1.df[c(1, 3), ])
  expect_equal(o1.first[1, 3], o1.df[1, 3])
  expect_equal(o1.first[c(1, 3), c(1, 3)], o1.df[c(1, 3), c(1, 3)])
})

test_that("as_data_frame produces reasonable results.", {
  expect_equal(tibble::as_tibble(o1.df), o1.df2)
})

test_that("Character data frame lookup operators [] make sense", {
  expect_equal(o1.first[, "centr"], o1.df[, "centr"])
  expect_equal(o1.first[, "centr", drop = FALSE], o1.df[, "centr", drop = FALSE])
  expect_equal(o1.first[, c("centr", "depot")], o1.df[, c("centr", "depot")])
})

test_that("Character data frame assignment operators [] make sense", {
  o1.assign <- o1.first
  expect_s3_class(o1.assign, "data.frame")
  o1.assign[, "depot"] <- 0
  expect_equal(rep(0, times = length(as.data.frame(o1.assign)[, 1])), as.data.frame(o1.assign)[, "depot"])
  expect_equal(rep(0, times = length(o1.assign$depot)), o1.assign$depot)
  expect_false(any(as.vector(class(o1.assign)) == "rxSolve"))
})

test_that("Numeric data frame lookup operators [[]] make sense", {
  expect_equal(o1.first[[1]], o1.df[[1]])
  expect_equal(o1.first[[3]], o1.df[[3]])
})

test_that("Character data frame lookup operators [[]] make sense", {
  expect_equal(o1.first[["depot"]], o1.df[["depot"]])
  expect_equal(o1.first[["de"]], o1.df[["de"]])
  expect_equal(o1.first[["de"]], NULL)
  expect_equal(o1.first[["de", exact = FALSE]], o1.df[["de", exact = FALSE]])
  expect_equal(o1.first[["de", exact = FALSE]], o1.df[["depot"]])
  expect_warning(o1.first[["de", exact = NA]])
})

test_that("Character data frame assignment operators [[]] make sense", {
  o1.assign <- o1.first
  expect_s3_class(o1.assign, "data.frame")
  o1.assign[["depot"]] <- 0
  expect_equal(rep(0, times = length(as.data.frame(o1.assign)[, 1])), as.data.frame(o1.assign)[["depot"]])
  expect_equal(rep(0, times = length(o1.assign$depot)), o1.assign$depot)
  expect_false(any(as.vector(class(o1.assign)) == "rxSolve"))
})

test_that("Character data frame lookup operators $ make sense", {
  expect_equal(o1.first$centr, o1.df$centr)
  expect_equal(o1.first$depot, o1.df$depot)
})

test_that("Character data frame assignment operators $ make sense", {
  o1.assign <- o1.first
  expect_s3_class(o1.assign, "rxSolve")
  o1.assign$depot <- 0
  expect_equal(rep(0, times = length(as.data.frame(o1.assign)[, 1])), as.data.frame(o1.assign)$depot)
  expect_equal(rep(0, times = length(o1.assign$depot)), o1.assign$depot)
  expect_false(any(as.vector(class(o1.assign)) == "rxSolve"))
})

test_that("rownames lookup & assignment makes sense", {
  expect_equal(rownames(o1.first), paste(seq_len(length(o1.first[, 1]))))
  rownames(o1.first) <- paste("row", seq_len(length(o1.first$depot)))
  expect_equal(rownames(o1.first), paste("row", seq_len(length(o1.first$depot))))
  rownames(o1.first) <- NULL
  expect_equal(rownames(o1.first), paste(seq_len(length(o1.first[, 1]))))
})
