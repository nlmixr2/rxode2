rxTest({
  if (!.Call(`_rxode2_isIntel`)) {
    test_that("as.ini ini expression", {

      is.ini <- function(x) {
        expect_true(is.call(x))
        expect_true(identical(x[[1]], quote(`ini`)))
      }

      ini <- quote(ini({
        tka <- log(1.57)
        tcl <- log(2.72)
        tv <- log(31.5)
        eta.ka ~ 0.6
        eta.cl ~ 0.3
        eta.v ~ 0.1
        add.sd <- 0.7
      }))

      is.ini(as.ini(ini))

      suppressMessages(expect_error(ini %>% ini(tka=3), NA))

      l <- quote(lotri({
        tka <- log(1.57)
        tcl <- log(2.72)
        tv <- log(31.5)
        eta.ka ~ 0.6
        eta.cl ~ 0.3
        eta.v ~ 0.1
        add.sd <- 0.7
      }))

      is.ini(as.ini(l))

      suppressMessages(expect_error(l %>% ini(tka=3), NA))

      m <- lotri({
        eta.ka ~ 0.6
        eta.cl ~ 0.3
        eta.v ~ 0.1
      })

      is.ini(as.ini(m))

      suppressMessages(expect_error(m %>% ini(eta.ka=3), NA))

      suppressMessages(expect_true(inherits(m %>% ini(eta.ka=3), "matrix")))

      one.compartment <- function() {
        ini({
          tka <- log(1.57)
          tcl <- log(2.72)
          tv <- log(31.5)
          eta.ka ~ 0.6
          eta.cl ~ 0.3
          eta.v ~ 0.1
          add.sd <- 0.7
        })
        model({
          ka <- exp(tka + eta.ka)
          cl <- exp(tcl + eta.cl)
          v <- exp(tv + eta.v)
          d/dt(depot) = -ka * depot
          d/dt(center) = ka * depot - cl / v * center
          cp = center / v
          cp ~ add(add.sd)
        })
      }

      is.ini(as.ini(one.compartment))

      ui <- one.compartment()

      is.ini(as.ini(ui))

      is.ini(as.ini(ui$iniDf))

      ini <- c("ini({",
               "tka <- log(1.57)",
               "tcl <- log(2.72)",
               "tv <- log(31.5)",
               "eta.ka ~ 0.6",
               "eta.cl ~ 0.3",
               "eta.v ~ 0.1",
               "add.sd <- 0.7",
               "})")

      is.ini(as.ini(ini))

      suppressMessages(expect_error(ini %>% ini(tka=3), NA))

      suppressMessages(expect_true(inherits(ini %>% ini(tka=3) %>% lotri::as.lotri(.),
                                            "lotriFix")))

      ini <- paste(ini, collapse="\n")

      is.ini(as.ini(ini))

      suppressMessages(expect_error(ini %>% ini(tka=3), NA))

      l <- lotri({
        tka <- log(1.57)
        tcl <- log(2.72)
        tv <- log(31.5)
        eta.ka ~ 0.6
        eta.cl ~ 0.3
        eta.v ~ 0.1
        add.sd <- 0.7
      })

      suppressMessages(expect_error(l %>% ini(tka=3), NA))
      suppressMessages(expect_true(inherits(l %>% ini(tka=3), "lotriFix")))

    })
  }
})
