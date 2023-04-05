if (file.exists(test_path("test-nmtest.qs"))) {

  d <- qs::qread(test_path("test-nmtest.qs"))
  # internally rxode2 treats lag time evids differently than
  # non-lagged events

  # 2 different models to test then
  f <- rxode2({
    cl <- 1.1
    v <- 20
    ka <- 1.5
    d/dt(depot) <- -ka*depot
    d/dt(central) <- ka*depot - (cl/v)*central
    f(central) <- bioav
    if (mode == 1) rate(central) <- rat2
    if (mode == 2) dur(central) <- dur2
    cp <- central/(v/1000)
  })


  fl <- rxode2({
    cl <- 1.1
    v <- 20
    ka <- 1.5
    d/dt(depot) <- -ka*depot
    d/dt(central) <- ka*depot - (cl/v)*central
    lag(central) <- lagt
    f(central) <- bioav
    if (mode == 1) rate(central) <- rat2
    if (mode == 2) dur(central) <- dur2
    cp <- central/(v/1000)
  })

  library(ggplot2)

  p <- TRUE

  solveEqual <- function(id, plot = p) {
    noLag <-  d[d$id == id & d$evid != 0,]$lagt == 0
    if (plot) {
      print(etTrans(d[d$id == id,], fl))
      s1 <- rxSolve(fl, d[d$id == id,])
      print(plot(s1, cp) +
              geom_point(data=d[d$id == id, ], aes(x=time, y=cp), col="red"))
    } else {
      if (noLag) {
        test_that(paste0("nmtest id:", id, " no alag"), {
          s1 <- rxSolve(f, d[d$id == id,])
          expect_equal(s1$cp, d[d$id == id & d$evid == 0,]$cp, tolerance = 0.01)
        })
      }
      test_that(paste0("nmtest id:", id, " alag"), {
        s1 <- rxSolve(fl, d[d$id == id,])
        expect_equal(s1$cp, d[d$id == id & d$evid == 0,]$cp, tolerance = 0.01)
      })
    }
  }

  # with id 10, dur=50 which is greater than ii
  # with id 11, dur=50 which is grater than ii.
  # with id 12, dur=12, which is greater than ii
  # with id 13, dur=10 which equals ii
  # with id 14, dur=10=ii
  # 25 ss2 not working like nonmem; it seems that the addl ss=1 accumulates something...

  # 19 need to work on modeled rates/durations

  #invisible(lapply(seq_len(range(d$id)[2]), solveEqual))

  # for NONMEM add case for 25 where addl isn't present
  # Also add 2 more cases for 25 using infusions
  
  # for 9 add case where lag time is longer so that it goes through the infusion time

}
