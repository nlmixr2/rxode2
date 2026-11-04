# tte() estimation model lines: the per-record log-likelihood of a tte
# endpoint from its hidden cumulative hazard state.

#' Estimation lines of a tte() endpoint
#'
#' Each record of the endpoint scores the events and survival since its
#' previous record: `dH` is the change of the hidden cumulative hazard
#' `rx.tteH.<var>` since then, and `DV` is 1 for an event (or an interval
#' holding one) and 0 otherwise.  An exact event adds `log(h)`, an interval
#' with an event `log(1 - exp(-dH))`; survival adds `-dH`.  By default the
#' first record of the endpoint is the start of observation and scores
#' nothing; with `origin = TRUE` the cumulative hazard counts from the
#' subject's start.  The previous record's values come from `lag0()`, as in
#' `.rxArEstPrevLines()`.
#'
#' @param env parsed model environment
#' @param pred1 single predDf row of the tte endpoint
#' @param errNum endpoint number
#' @return list of quoted model lines
#' @author Matthew L. Fidler
#' @noRd
.rxTteEstLines <- function(env, pred1, errNum) {
  .o <- .rxTteOptions(env, pred1)
  .yj <- as.double(pred1$transform) - 1
  .dist <- as.integer(pred1$distribution)
  .haz <- .rxTteRewriteLines(list(.o$hazard), env$predDf, env)[[1]]
  # dot-free names, see .rxArEstLlikLines()
  .var <- gsub("[^A-Za-z0-9]", "_", pred1$var)
  .n <- function(.p) str2lang(paste0("rx_tte", .p, "_", .var))
  .O <- .n("O")
  .hc <- .n("Hc")
  .hz <- .n("Hz")
  .hk <- .n("Hk")
  .s <- .n("S")
  .hp <- .n("Hp")
  .sp <- .n("Sp")
  .dh <- .n("Dh")
  .ll <- if (.o$eventType == "intervalCensored") {
    bquote(log(DV * (1 - exp(-.(.dh))) + (1 - DV)) - (1 - DV) * .(.dh))
  } else {
    bquote(log(DV * .(.hz) + (1 - DV)) - .(.dh))
  }
  list(
    bquote(rx_yj_ ~ .(.yj + 10 * (.dist - 1))),
    quote(rx_lambda_ ~ 1),
    quote(rx_low_ ~ 0),
    quote(rx_hi_ ~ 1),
    if (length(env$predDf$line) > 1L) {
      bquote(.(.O) <- rx__isObs__ * (CMT == .(as.numeric(pred1$cmt))))
    } else {
      bquote(.(.O) <- rx__isObs__)
    },
    bquote(.(.hc) <- .(str2lang(.rxTteStateName(pred1$var)))),
    bquote(.(.hz) <- .(.haz)),
    bquote(.(.hk) <- .(.O) * .(.hc) + (1 - .(.O)) * lag0(.(.hk), 1)),
    bquote(.(.s) <- .(.O) + (1 - .(.O)) * lag0(.(.s), 1)),
    bquote(.(.hp) <- lag0(.(.hk), 1)),
    bquote(.(.sp) <- lag0(.(.s), 1)),
    bquote(.(.dh) <- .(.hc) - .(.sp) * .(.hp)),
    # only the endpoint's records score (in default mode from its second)
    if (.o$origin) {
      bquote(rx_pred_ ~ .(.O) * (.(.ll)))
    } else {
      bquote(rx_pred_ ~ .(.O) * .(.sp) * (.(.ll)))
    },
    quote(rx_r_ ~ 0)
  )
}
