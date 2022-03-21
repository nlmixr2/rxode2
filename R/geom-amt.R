.amtTrans <- function(data) {
  if (!any(names(data) == "amt")) {
    stop("need 'amt' aesthetic")
  }
  if (!any(names(data) == "x")) {
    if (any(names(data) == "time")) {
      data$x <- data$time
    } else {
      stop("need 'x' aesthetic")
    }
  }
  # .range <- range(data$y,na.rm=TRUE)
  .dat <- data
  .dat <- data[, c("x", "amt")]
  .dat <- data[!is.na(.dat$amt), ]
  ret <- data.frame(x = .dat$x, xend = .dat$x, y = -Inf, yend = Inf, amt = .dat$amt)
  return(ret)
}

GeomAmt <- ggplot2::ggproto("GeomAmt", ggplot2::GeomSegment,
  required_aes = c("x", "y", "xend", "yend"),
  default_aes = ggplot2::aes(colour = "black", linetype = "dotted", size = 0.5, alpha = 1, fill = "black")
)

StatAmt <- ggplot2::ggproto("StatAmt", ggplot2::Stat,
  compute_group = function(data, scales) {
    .amtTrans(data)
  },
  required_aes = c("x", "amt")
)


#' Dosing/Amt geom/stat
#'
#' This is a dosing geom that shows the vertical lines where a dose occurs
#'
#' Requires the following aesthetics:
#'
#' \itemize{
#' \item x representing the x values, usually time
#' \item amt representing the dosing values;  They are missing or zero when no dose is given
#' }
#'
#' @return This returns a stat_amt in context of a ggplot2 plot
#'
#' @examples
#'
#' library(rxode2)
#' library(units)
#'
#'
#' ## Model from RxODE tutorial
#' mod1 <-rxode2({
#'        KA=2.94E-01
#'        CL=1.86E+01
#'        V2=4.02E+01
#'        Q=1.05E+01
#'        V3=2.97E+02
#'        Kin=1
#'        Kout=1
#'        EC50=200
#'        C2 = centr/V2
#'        C3 = peri/V3
#'        d/dt(depot) =-KA*depot
#'        d/dt(centr) = KA*depot - CL*C2 - Q*C2 + Q*C3
#'        d/dt(peri)  =                    Q*C2 - Q*C3
#'        d/dt(eff)  = Kin - Kout*(1-C2/(EC50+C2))*eff
#' })
#'
#' ## These are making the more complex regimens of the rxode2 tutorial
#'
#' ## bid for 5 days
#' bid <- et(timeUnits="hr") %>%
#'       et(amt=10000,ii=12,until=set_units(5, "days"))
#'
#' ## qd for 5 days
#' qd <- et(timeUnits="hr") %>%
#'       et(amt=20000,ii=24,until=set_units(5, "days"))
#'
#' ## bid for 5 days followed by qd for 5 days
#'
#' et <- seq(bid,qd) %>% et(seq(0,11*24,length.out=100));
#'
#' bidQd <- rxSolve(mod1, et, addDosing=TRUE)
#'
#' # by default dotted and under-stated
#' plot(bidQd, C2) + geom_amt(aes(amt=amt))
#'
#' # of course you can make it a bit more visible
#'
#' plot(bidQd, C2) + geom_amt(aes(amt=amt), col="red", lty=1, size=1.2)
#' @export
#' @inheritParams ggplot2::stat_identity
stat_amt <- function(mapping = NULL, data = NULL,
                     position = "identity", show.legend = NA,
                     inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatAmt, data = data, mapping = mapping, geom = GeomAmt,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = TRUE, ...)
  )
}

#' @rdname stat_amt
#' @export
geom_amt <- stat_amt
