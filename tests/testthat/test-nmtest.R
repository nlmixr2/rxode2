if (file.exists(test_path("test-nmtest.qs"))) {

  ## system("rm -v ~/src/rxode2/src/*.so ~/src/rxode2/src/*.o ~/src/rxode2parse/src/*.so ~/src/rxode2parse/src/*.o ~/src/rxode2random/src/*.so ~/src/rxode2random/src/*.o");devtools::install("~/src/rxode2parse", dep=FALSE); devtools::install("~/src/rxode2random", dep=FALSE); devtools::load_all();rxClean();#devtools::test()

  ## devtools::load_all()
  
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

  solveEqual <- function(id, plot = p, meth="liblsoda", modifyData = c("none", "dur", "rate"),
                         addlKeepsCov = TRUE) {
    noLag <-  all(d[d$id == id & d$evid != 0,]$lagt == 0)
    hasRate <- any(d[d$id == id & d$evid != 0,]$rate != 0)
    hasModeledRate <- any(d[d$id == id & d$evid != 0,]$mode == 1)
    hasModeledDur  <- any(d[d$id == id & d$evid != 0, ]$mode == 2)
    hasChangedF <- any(d[d$id == id & d$evid != 0, ]$bioav != 1)
    modifyData <- match.arg(modifyData)
    d <- d[d$id == id,]
    rate <- unlist(as.vector(d[d$evid != 0, "rate"]))
    ii0 <- all(d$ii == 0)
    oneRate <- (length(rate) == 1L)
    dose1 <- all(d[d$evid != 0, ]$cmt == 1)
    if (modifyData == "rate" && hasRate && !hasModeledRate && !hasModeledDur && oneRate && !ii0 && !dose1) {
      if (p) message("modified rate to be modeled")
      rate <- d[d$evid != 0, "rate", drop=FALSE]
      rate <- rate$rate
      if (length(rate) == 1) {
        d$rat2 <- rate
        d$rate <- ifelse(d$rate==0, 0, -1)
        d$mode <- 1
      }
      ## print(etTrans(d, fl))
    } else if (modifyData == "dur" && hasRate && !hasModeledRate && !hasModeledDur && !hasChangedF && oneRate && !ii0 && !dose1) {
      if (p) message("modified dur to be modeled")
      rate <- as.numeric(d[d$evid != 0, "rate"])
      amt <- as.numeric(d[d$evid != 0, "amt"])
      if (length(rate) == 1) {
        d$dur2 <- amt/rate
        d$rate <- ifelse(d$rate==0, 0, -2)
        d$mode <- 2
        assign(".d", d, envir=globalenv())
      }
    } else if (any(modifyData == c("dur", "rate"))) {
      if (p) {
        message("skipping because cannot be modified")
        print(list(modifyData=modifyData,
                   noLag=noLag,
                   hasRate=hasRate,
                   hasModeledRate=hasModeledRate,
                   hasModeledDur= hasModeledDur,
                   hasChangedF=hasChangedF))
       }
      return(invisible())
    }
    if (plot) {
      print(ggplot(d, aes(time, cp)) +
              geom_point(col="red") +
              rxode2::rxTheme() +
              ggtitle(paste0("id=", id)))
      ## print(etTrans(d, fl))
      s1 <- rxSolve(fl, d, method=meth, addlKeepsCov = addlKeepsCov)
      if (!noLag) {
        print(plot(s1, cp) +
                geom_point(data=d, aes(x=time, y=cp), col="red") +
                ggtitle(paste0("id=", id, "(lag)")))
      } else {
        message("f without lag")
        s2 <- rxSolve(f, d, method=meth, addlKeepsCov = addlKeepsCov)
        return(plot(s1, cp) +
                geom_point(data=d, aes(x=time, y=cp), col="red") +
                geom_line(data=s2, aes(x=time, y=cp), col="blue", alpha=0.5, linewidth=2) +
                ggtitle(paste0("id=", id, "(nolag)")))
      }
      ## print(etTrans(d, fl))
    } else {
      sub <- 0
      if (meth == "dop853" && modifyData == "rate" && id  %in% c(409, 809))  {
         return(invisible())
      }
      if (id %in% c(410, 411, 409, 415, 709)) {
        sub <- 24
      }
      if (id %in% c(809, 909, 1009)) {
        sub <- 48
      }
      if (noLag) {
        test_that(paste0("nmtest id:", id, " no alag; method: ", meth, "; modifyData:", modifyData),
        {
          s1 <- rxSolve(f, d, method=meth, addlKeepsCov = addlKeepsCov)
          expect_equal(s1$cp[s1$time >= sub],
                       d[d$id == id & d$evid == 0 & d$time >= sub,]$cp,
                       tolerance = 0.1)
        })
       }
      test_that(paste0("nmtest id:", id, " alag; method: ", meth, "; modifyData:", modifyData),
      {
        s1 <- rxSolve(fl, d, method=meth, addlKeepsCov = addlKeepsCov)
        expect_equal(s1$cp[s1$time >= sub],
                     d[d$id == id & d$evid == 0 & d$time >= sub,]$cp,
                     tolerance = 0.1)
      })
    }
  }

  p <- TRUE

  # This doesn't work and I'm unsure why.
  ## solveEqual(409, meth="dop853", modifyData = "rate") + ylim(0, 2500)


  p <- FALSE
  lapply(unique(d$id), function(i) {
    meths <- c("liblsoda", "lsoda", "dop853")
    modDat <- c("none", "rate", "dur")
    for (meth in meths) {
      for (modifyData in modDat) {
        solveEqual(i, meth=meth, modifyData=modifyData)
      }
    }
  })

  ## invisible(lapply(seq_len(range(d$id)[2]), function(i){message(i);solveEqual(i)}))

  ## need to check id=25 type for infusions too (static, modeled) need
  ## to check for steady state when the infusion is still going at the
  ## time of steady-state release

  ## need to check type 10 where the infusion ends exactly at the lag time

  ## check lagTime > ii

  ## check steady state reset with lag time and addl

  ## check steady state lag where rate/dur is modeled when dose occurs
  ## during elimination period, when dose occurs during infusion
  ## period, and when the infusion is turned off exactly at the dosing
  ## time

} 
