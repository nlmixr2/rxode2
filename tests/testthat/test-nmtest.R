rxTest({

  ## devtools::load_all()

  d <- nlmixr2data::nmtest
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

  lf <- rxode2({
    cl <- 1.1
    v <- 20
    ka <- 1.5
    f(central) <- bioav
    if (mode == 1) rate(central) <- rat2
    if (mode == 2) dur(central) <- dur2
    cp <- linCmt()*1000
  })

  lbf <- rxode2({
    cl <- 1.1
    v <- 20
    ka <- 1.5
    f(central) <- bioav
    if (mode == 1) rate(central) <- rat2
    if (mode == 2) dur(central) <- dur2
    cp <- linCmt()*1000
  }, linCmtSens = "linCmtB")


  elf <- rxode2({
    cl <- 1.1
    v <- 20
    ka <- 1.5
    f(central) <- bioav
    if (mode == 1) rate(central) <- rat2
    if (mode == 2) dur(central) <- dur2
    cp <- linCmt()*1000
    ke0 <- log(2)/(50)
    d/dt(Ce) <- (Cp-Ce)*ke0
  })

  elbf <- rxode2({
    cl <- 1.1
    v <- 20
    ka <- 1.5
    f(central) <- bioav
    if (mode == 1) rate(central) <- rat2
    if (mode == 2) dur(central) <- dur2
    cp <- linCmt()*1000
    ke0 <- log(2)/(50)
    d/dt(Ce) <- (Cp-Ce)*ke0
  }, linCmtSens = "linCmtB")

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

  lfl <- rxode2({
    cl <- 1.1
    v <- 20
    ka <- 1.5
    lag(central) <- lagt
    f(central) <- bioav
    if (mode == 1) rate(central) <- rat2
    if (mode == 2) dur(central) <- dur2
    cp <- linCmt()*1000
  })

  lbfl <- rxode2({
    cl <- 1.1
    v <- 20
    ka <- 1.5
    lag(central) <- lagt
    f(central) <- bioav
    if (mode == 1) rate(central) <- rat2
    if (mode == 2) dur(central) <- dur2
    cp <- linCmt()*1000
  }, linCmtSens = "linCmtB")

  elfl <- rxode2({
    cl <- 1.1
    v <- 20
    ka <- 1.5
    lag(central) <- lagt
    f(central) <- bioav
    if (mode == 1) rate(central) <- rat2
    if (mode == 2) dur(central) <- dur2
    cp <- linCmt()*1000
    ke0 <- log(2)/(50)
    d/dt(Ce) <- (Cp-Ce)*ke0
  })

  elbfl <- rxode2({
    cl <- 1.1
    v <- 20
    ka <- 1.5
    lag(central) <- lagt
    f(central) <- bioav
    if (mode == 1) rate(central) <- rat2
    if (mode == 2) dur(central) <- dur2
    cp <- linCmt()*1000
    ke0 <- log(2)/(50)
    d/dt(Ce) <- (Cp-Ce)*ke0
  }, linCmtSens = "linCmtB")


  library(ggplot2)

  solveEqual <- function(id, plot = p, meth="A",
                         modifyData = c("none", "dur", "rate"),
                         addlKeepsCov = TRUE, addlDropSs=TRUE,
                         ss2cancelAllPending=FALSE) {
    if (meth == "Bs" || meth == "As") {
      ssSolved <- FALSE
    } else {
      ssSolved <- TRUE
    }
    if (meth == "A" || meth == "Ao" || meth == "As") {
      lin <- "A"
      meth <- "liblsoda"
    } else if (meth == "B" || meth == "Bo" || meth == "Bs") {
      lin <- "B"
      meth <- "liblsoda"
    } else  if (meth == "Ad") {
      lin <- "A"
      meth <- "dop853"
    } else  if (meth == "Bd") {
      lin <- "B"
      meth <- "dop853"
    } else  if (meth == "Al") {
      lin <- "A"
      meth <- "lsoda"
    } else  if (meth == "Bl") {
      lin <- "B"
      meth <- "lsoda"
    } else if (meth == "B" || meth == "Bo" || meth == "Bs") {
      lin <- "B"
      meth <- "liblsoda"
    } else {
      lin <- "ode"
    }

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
    if (!addlDropSs) {
      if (any(d$ss == 2)) {
        return()
      }
    }
    if (modifyData == "rate" && hasRate && !hasModeledRate && !hasModeledDur && oneRate && !ii0 && !dose1) {
      if (p) message("modified rate to be modeled")
      rate <- d[d$evid != 0, "rate", drop=FALSE]
      rate <- rate$rate
      if (length(rate) == 1) {
        d$rat2 <- rate
        d$rate <- ifelse(d$rate==0, 0, -1)
        d$mode <- 1
      }
      ## print(etTrans(d, f, addlDropSs=addlDropSs))
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
      if (lin == "A") {
        s1 <- rxSolve(lfl, d, method=meth, addlKeepsCov = addlKeepsCov,
                      addlDropSs=addlDropSs,
                      ss2cancelAllPending=ss2cancelAllPending,
                      ssSolved=ssSolved)
      } else if (lin == "B") {
        s1 <- rxSolve(lbfl, d, method=meth, addlKeepsCov = addlKeepsCov,
                      addlDropSs=addlDropSs,
                      ss2cancelAllPending=ss2cancelAllPending,
                      ssSolved=ssSolved)
      } else if (lin == "Ao") {
        s1 <- rxSolve(elfl, d, method=meth, addlKeepsCov = addlKeepsCov,
                      addlDropSs=addlDropSs,
                      ss2cancelAllPending=ss2cancelAllPending,
                      ssSolved=ssSolved)
      } else if (lin == "Bo") {
        s1 <- rxSolve(elbfl, d, method=meth, addlKeepsCov = addlKeepsCov,
                      addlDropSs=addlDropSs,
                      ss2cancelAllPending=ss2cancelAllPending,
                      ssSolved=ssSolved)
      } else {
        s1 <- rxSolve(fl, d, method=meth, addlKeepsCov = addlKeepsCov,
                      addlDropSs=addlDropSs,
                      ss2cancelAllPending=ss2cancelAllPending,
                      ssSolved=ssSolved)
      }
      if (!noLag) {
        print(plot(s1, cp) +
                geom_point(data=d, aes(x=time, y=cp), col="red") +
                ggtitle(paste0("id=", id, "(lag)")))
      } else {
        message("================================================== ")
        message("f without lag")
        message("================================================== ")
        if (lin == "A") {
          s2 <- rxSolve(lf, d, method=meth, addlKeepsCov = addlKeepsCov,
                        addlDropSs=addlDropSs,
                        ss2cancelAllPending=ss2cancelAllPending,
                        ssSolved=ssSolved)
        } else if (lin == "B") {
          s2 <- rxSolve(lbf, d, method=meth, addlKeepsCov = addlKeepsCov,
                        addlDropSs=addlDropSs,
                        ss2cancelAllPending=ss2cancelAllPending,
                        ssSolved=ssSolved)
        } else if (lin == "Ao") {
          s2 <- rxSolve(elf, d, method=meth, addlKeepsCov = addlKeepsCov,
                        addlDropSs=addlDropSs,
                        ss2cancelAllPending=ss2cancelAllPending,
                        ssSolved=ssSolved)
        } else if (lin == "Bo") {
          s2 <- rxSolve(elbf, d, method=meth, addlKeepsCov = addlKeepsCov,
                        addlDropSs=addlDropSs,
                        ss2cancelAllPending=ss2cancelAllPending,
                        ssSolved=ssSolved)
        } else {
          s2 <- rxSolve(f, d, method=meth, addlKeepsCov = addlKeepsCov,
                        addlDropSs=addlDropSs,
                        ss2cancelAllPending=ss2cancelAllPending,
                        ssSolved=ssSolved)
        }
        return(plot(s1, cp) +
                 geom_point(data=d, aes(x=time, y=cp), col="red") +
                 geom_line(data=s2, aes(x=time, y=cp), col="blue", alpha=0.5, linewidth=2) +
                 ggtitle(paste0("id=", id, "(nolag)")))
      }
      ## print(etTrans(d, fl))
    } else {
      sub <- 0
      if (id %in% c(410, 411, 409, 415, 709, 510, 610)) {
        sub <- 24
      }
      if (id %in% c(809, 909, 1009)) {
        sub <- 48
      }
      if (id %in% 825) {
        sub <- 96
      }
      if (noLag) {
        test_that(paste0("nmtest id:", id, " no alag; method: ", meth, "; modifyData:", modifyData, "; addlDropSs: ", addlDropSs, "; lin=", lin, "; ssSolved=", ssSolved),
        {
          if (lin == "A") {
            s1 <- rxSolve(lf, d, method=meth,
                          addlKeepsCov = addlKeepsCov,
                          addlDropSs=addlDropSs)
          } else if (lin == "B") {
            s1 <- rxSolve(lbf, d, method=meth,
                          addlKeepsCov = addlKeepsCov,
                          addlDropSs=addlDropSs)
          } else {
            s1 <- rxSolve(f, d, method=meth,
                          addlKeepsCov = addlKeepsCov,
                          addlDropSs=addlDropSs)
          }
          expect_equal(s1$cp[s1$time >= sub],
                       d[d$id == id & d$evid == 0 & d$time >= sub,]$cp,
                       tolerance = 0.1)
        })
      }
      test_that(paste0("nmtest id:", id, " alag; method: ", meth, "; modifyData:", modifyData,"; addlDropSs: ", addlDropSs, "; lin=", lin, "; ssSolved=", ssSolved),
      {
        if (lin == "A") {
          s1 <- rxSolve(lfl, d, method=meth,
                        addlKeepsCov = addlKeepsCov,
                        addlDropSs=addlDropSs)
        } else if (lin == "B") {
          s1 <- rxSolve(lfl, d, method=meth,
                        addlKeepsCov = addlKeepsCov,
                        addlDropSs=addlDropSs)
        } else if (lin == "Ao") {
          s1 <- rxSolve(elfl, d, method=meth,
                        addlKeepsCov = addlKeepsCov,
                        addlDropSs=addlDropSs)
        } else if (lin == "Bo") {
          s1 <- rxSolve(elfl, d, method=meth,
                        addlKeepsCov = addlKeepsCov,
                        addlDropSs=addlDropSs)
        } else {
          s1 <- rxSolve(fl, d, method=meth,
                        addlKeepsCov = addlKeepsCov,
                        addlDropSs=addlDropSs)
        }
        expect_equal(s1$cp[s1$time >= sub],
                     d[d$id == id & d$evid == 0 & d$time >= sub,]$cp,
                     tolerance = 0.1)
      })
    }
  }

  p <- TRUE

  id <- unique(d$id)

  # id = 12


  p <- FALSE

  lapply(id, function(i) {
    meths <- c("liblsoda", "lsoda", "dop853", "A", "B", "Ao", "Bo", "As", "Bs",
               "Ad", "Bd", "Al", "Bl")
    modDat <- c("none", "rate", "dur")
    for (meth in meths) {
      for (modifyData in modDat) {
        for (addlDropSs in c(TRUE, FALSE)) {
          solveEqual(i, meth=meth, modifyData=modifyData, addlDropSs=addlDropSs)
        }
      }
    }
  })

  ## invisible(lapply(unique(d$id), function(i){message(i);solveEqual(i);Sys.sleep(1)}))

  ## need to check id=25 type for infusions too (static, modeled) need
  ## to check for steady state when the infusion is still going at the
  ## time of steady-state release

  ## Need to check steady state infusion as well as infusions where ii=dur with lag times

  ## modeled equivalents of 425, 525

})

test_that("evid4", {

  df <- "unjXVBZLQAAAAAAAAA|;[2w|gWj2@.BAC\"><F{+,tB@nC\"OG>cYAAA:CVtAADHMA@AAAH7m0<fKSxAAAHAAAqCAA@QBALtBtaL:ulyT9D<6FlgSH3VY{:CzWmyT92D:3VKf^Uk0D/KlgSH)W3eN\"gA6}[aRBkMhY@DIAAAmyT92D/KQVv`B.zUsYp3dW9}c[HcX&&2x~Rmt>ole6)~[4w?8~<*_~002|T)NAkM(.P4sqVA$!_r+7ZIG\"&2OCt8octWple6|M#FZ4AS.~mTdE}e[4dAU\"AAlBAABtEAIA.AAAe+8^eQCELgO[aAk}B\"tWG$Y.GFCAFAAAbf=[]@|REvG92CEAAAl%?#%85plMbAAAmu:x2o1a,<][cCM%RzQLAA&a7SOQC2y1`Is;Hf6*&S0CU\"?0(ZD@$IHO+U1=mkG/2AEA.&jYcSa]fy>,SSOFp{}Zna%`zzxx3O*<{Qij[umQr4kz~%u\"9l7>H9X~X>s_^5hFC^l9;4I]_8A~Iia?11?yTGxIHel!1Rn*eek:?q~(S5f5^!T$_:%%h`2+v1t`Y5/no;uG\"0719JtYbx2`Dc/1C+6;rCx}P+r~b0P6G"

  df <- df |>
    qs2::base91_decode() |>
    qs2::qs_deserialize()


  evid4 <- function() {
    ini({
      # Where initial conditions/variables are specified
      lka  <- log(2.097)   #log ka (1/h)
      lcl  <- log(2.568724)#log Cl (L/h)
      lv   <- log(69.1925) #log V (L)
      allocl <- fix(0.75)  #WT on Cl
      allov  <- fix(1.00)  #WT on V
    })
    model({
      # Where the model is specified
      cl <- exp(lcl + allocl*(log(WT/70)))
      v  <- exp(lv + allov*(log(WT/70)))
      ka <- exp(lka)
      lin <- linCmt()
    })
  }

  evid4ode <- function() {
    ini({
      # Where initial conditions/variables are specified
      lka  <- log(2.097)   #log ka (1/h)
      lcl  <- log(2.568724)#log Cl (L/h)
      lv   <- log(69.1925) #log V (L)
      allocl <- fix(0.75)  #WT on Cl
      allov  <- fix(1.00)  #WT on V
    })
    model({
      # Where the model is specified
      cl <- exp(lcl + allocl*(log(WT/70)))
      v  <- exp(lv + allov*(log(WT/70)))
      ka <- exp(lka)
      kel <- cl/v
      d/dt(depot)   <- -ka*depot
      d/dt(central) <- ka*depot - kel*central
      lin <- central/v
    })
  }


  f1 <- rxSolve(evid4, df[1:5,], addDosing=TRUE, returnType="data.frame")

  f1o <- rxSolve(evid4ode, df[1:5,], addDosing=TRUE, returnType="data.frame")

  f2 <- rxSolve(evid4, df, addDosing=TRUE, returnType="data.frame")

  f2o <- rxSolve(evid4ode, df, addDosing=TRUE, returnType="data.frame")

  expect_equal(f1$lin, f2$lin[seq_along(f1$lin)])

  expect_equal(f1o$lin, f2o$lin[seq_along(f1o$lin)])

  expect_equal(f1$lin, f1o$lin, tolerance = 1e-6)

  expect_equal(f2$lin, f2o$lin, tolerance = 1e-6)

})
