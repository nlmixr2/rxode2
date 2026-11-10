## Fixtures for the thetaMat-versus-prior precedence tests (#1502): a
## prior that pulls a MAP estimate without being centered on it, or (with
## `centered = TRUE`) one centered on the estimate
.priorMapMod <- function(meta = FALSE, centered = FALSE) {
  if (centered) {
    .f <- function() {
      ini({
        tka <- 0.43
        prior(tka) ~ dnorm(0.43, 0.25)
        tcl <- -3.2
        eta.ka ~ 0.4
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl)
        d / dt(depot) <- -ka * depot
        d / dt(center) <- ka * depot - cl / 30 * center
        cp <- center / 30
        cp ~ add(add.sd)
      })
    }
  } else {
    .f <- function() {
      ini({
        tka <- 0.43
        prior(tka) ~ dnorm(log(3), 0.25)
        tcl <- -3.2
        eta.ka ~ 0.4
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl)
        d / dt(depot) <- -ka * depot
        d / dt(center) <- ka * depot - cl / 30 * center
        cp <- center / 30
        cp ~ add(add.sd)
      })
    }
  }
  .ui <- rxode2::rxode2(.f)
  if (meta) {
    assign("thetaMat", .priorMapThetaMat(), envir = .ui$meta)
  }
  .ui
}

.priorMapThetaMat <- function() {
  lotri::lotri(tka + tcl ~ c(0.01, 0.001, 0.02))
}

.priorMapEv <- function() rxode2::et(amt = 320) |> rxode2::et(seq(0, 24, by = 4))
