if (file.exists(test_path("test-nmtest.qs"))) {

  ## system("rm -v ~/src/rxode2/src/*.so ~/src/rxode2/src/*.o ~/src/rxode2parse/src/*.so ~/src/rxode2parse/src/*.o ~/src/rxode2random/src/*.so ~/src/rxode2random/src/*.o");devtools::install("~/src/rxode2parse"); devtools::install("~/src/rxode2random"); devtools::load_all();rxClean();#devtools::test()

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

  p <- TRUE

  solveEqual <- function(id, plot = p, meth="liblsoda", modifyData = c("rate", "none", "dur")) {
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
    if (modifyData == "rate" && hasRate && !hasModeledRate && !hasModeledDur && oneRate && !ii0) {
      if (p) message("modified rate to be modeled")
      rate <- as.numeric(d[d$evid != 0, "rate"])
      if (length(rate) == 1) {
        d$rat2 <- rate
        d$rate <- ifelse(rate==0, 0, -1)
        d$mode <- 1
        assign(".d", d, envir=globalenv())
      }
    } else if (modifyData == "dur" && hasRate && !hasModeledRate && !hasModeledDur && !hasChangedF && oneRate && !ii0) {
      if (p) message("modified dur to be modeled")
      rate <- as.numeric(d[d$evid != 0, "rate"])
      amt <- as.numeric(d[d$evid != 0, "amt"])
      if (length(rate) == 1) {
        d$dur2 <- amt/rate
        d$rate <- ifelse(rate==0, 0, -2)
        d$mode <- 2
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
      s1 <- rxSolve(fl, d, method=meth)
      if (!noLag) {
        print(plot(s1, cp) +
                geom_point(data=d, aes(x=time, y=cp), col="red") +
                ggtitle(paste0("id=", id, "(lag)")))
      } else {
        message("f without lag")
        s2 <- rxSolve(f, d, method=meth)
        return(plot(s1, cp) +
                geom_point(data=d, aes(x=time, y=cp), col="red") +
                geom_line(data=s2, aes(x=time, y=cp), col="blue", alpha=0.5, linewidth=2) +
                ggtitle(paste0("id=", id, "(nolag)")))
      }
      ## print(etTrans(d, fl))
    } else {
      if (noLag) {
        test_that(paste0("nmtest id:", id, " no alag; method: ", meth, "; modifyData:", modifyData), {
          s1 <- rxSolve(f, d, method=meth)
          expect_equal(s1$cp, d[d$id == id & d$evid == 0,]$cp, tolerance = 0.04)
        })
       }
      test_that(paste0("nmtest id:", id, " alag; method: ", meth, "; modifyData:", modifyData), {
        s1 <- rxSolve(fl, d, method=meth)
        expect_equal(s1$cp, d[d$id == id & d$evid == 0,]$cp, tolerance = 0.04)
      })
    }
  }

  p <- FALSE
  lapply(unique(d$id)[-25], function(i) {
    for (meth in c("liblsoda", "lsoda", "dop853")) {
      for (modifyData in c("none", "rate")) {
        solveEqual(i, meth=meth, modifyData=modifyData)
      }
    }
  })

  p <- TRUE

  ## solveEqual(6)
  
  ## solveEqual(9)
  
  solveEqual(10)
  
  ## solveEqual(11)
  ## solveEqual(12)

  ## solveEqual(13)

  ## solveEqual(14)
  
  ## solveEqual(20)
  ## solveEqual(23)
  ## solveEqual(24)
  ## solveEqual(26)
  ## solveEqual(10)

  ## p <- FALSE
  ## lapply(unique(d$id)[-25], function(i) {
  ##   for (meth in c("liblsoda", "lsoda", "dop853")) {
  ##     for (modifyData in c("none", "dur", "rate")) {
  ##       solveEqual(i, meth=meth, modifyData=modifyData)
  ##     }
  ##   }
  ## })

  # 
  
  ## invisible(lapply(seq_len(range(d$id)[2]), function(i){message(i);solveEqual(i)}))

  ## rxSolve(fl, d[d$id == 25,]) %>% dplyr::filter(time %in% c(0, 12, 24, 36,48))

  ## solveEqual(25)

  ## d <- d[d$id == 25, ]
  ## d$ss  <- 0

  ## solveEqual(25)

##   d <- data.frame(as.data.frame(etTrans(d[d$id == 25, ], fl)), mode=0, dur2=2, rat2=10, bioav=1, lagt=0)

##   names(d)[1] <- "id"
## d$id <- 25

  ## d$EVID <- ifelse(d$EVID==0, 0, 120)
  ## d$EVID[1] <- 110

  ## solveEqual(25)
  
  # 25
  # 27 is the end

  # Fixed rate, can be changed to modeled rate or duration
  #d[d$id == 3 & d$evid != 0,] %>% as.data.frame
  #d[d$id == 4 & d$evid != 0,] %>% as.data.frame
  #d[d$id == 6 & d$evid != 0,] %>% as.data.frame
  #d[d$id == 7 & d$evid != 0,] %>% as.data.frame
  #d[d$id == 8 & d$evid != 0,] %>% as.data.frame
  #d[d$id == 9 & d$evid != 0,] %>% as.data.frame
  #d[d$id == 10 & d$evid != 0,] %>% as.data.frame
  #d[d$id == 11 & d$evid != 0,] %>% as.data.frame
  #d[d$id == 12 & d$evid != 0,] %>% as.data.frame
  #d[d$id == 13 & d$evid != 0,] %>% as.data.frame
  #d[d$id == 13 & d$evid != 0,] %>% as.data.frame 
  
  ## invisible(lapply(seq_len(range(d$id)[2]), function(i){message(i);solveEqual(i)}))

  ## need to check id=25 type for infusions too (static, modeled) need
  ## to check for steady state when the infusion is still going at the
  ## time of steady-state release

  ## Need to check id=10 with a lag time as well
  
}
