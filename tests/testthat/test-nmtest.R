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

  df <- "un]\"BAAA@QRtHACAAAAAAAAAPAAA+:g6Ji,J_8DAE+d1RAK*mJsyNK+:DGqit,>jj(>ph{/KV80[Qh\"~\"=)h>UD~mN@pb$YTVW?CGwjB)v2W`/^kDbH[6nC\"Jpl,moPVY6kUoIDtWA!(@CPA8zJM3b6!,sU\"7<>1!q\"Nq~6eF8[#l2^efwtU]|DzBa)*L%(&X{c%v.`R`V%uy3&$Wq8e+#y)sv]^`f81+\"$M?1WILGf;rpV}@ITW>k,XKH{s[|/Jx7tl[g)ndEQBAH[&U_X=7$J|Z&rpb+Q|k>c%X[TSCxq9jwQO+d2E!C*t=JAoc$,RCiewrSb21kuUx?jgT%i6ffZptmaT3W+t{eq6nfb=U$Au+B%F>CEQK!\"I0ScDfzy<0023?.Gh~t:$e+&`8wLvv7[`d*Mye!{n=Nn:$F7?e)2uXw?u&uS?ALs6.rFs.IJ*B`2vx_m6J^Y$;SgEH},R*H9WB)#&=;*(gE)YJV%/kM81i&Vr9EB8yIoAvlRtWt*hUU0?ADHG}Ep+|6D,c$n^x5I{1Rbf6JdpE3,RM$@U=NFB%SR,F~d#Ku4juP*):i5R:Imx/I:Wh+yV9chv^a_EgRy.wRy?cx^mVgzeNest.yUFNaD"

  df <- df %>%
    qs::base91_decode() %>%
    qs::qdeserialize()

  f1 <- rxSolve(evid4, df[1:5,], addDosing=TRUE, returnType="data.frame")

  f2 <- rxSolve(evid4, df, addDosing=TRUE, returnType="data.frame")

  expect_equal(f1$lin, f2$lin[seq_along(f1$lin)])

})


v  <- exp(lv + allov*(log(WT/70)))
log(v) <- lv + allov*(log(WT/70))
log(27.677) - log(28.0/70)
