#' @title Convert event data to trial duration data
#' A helper function to create a custom event table.
#' The observation time will start from the first event time (baseline) and end at trial duration.
#' The interval is the spacing between each observation.
#' @param ev event data
#' @param trialEnd extend trial duration. Must be same time unit as event data
#' @param interval observation interval. Must be same time unit as event data
#' @param writeDir if not NULL, write the output to a csv file
#' @importFrom stats ave
#' @importFrom utils write.csv
#' @examples
#'  # Create event table with unique time for each ID
#'  ev = et(data.frame(id = rep(1:10, 3),  time = runif(min = 10, max = 20, n = 30)))
#'
#'  # select the duration and spacing interval (assuming time is in years)
#'  toTrialDuration(ev, trialEnd = 1.5, interval = 0.2)
#'
#' @author Omar Elashkar
#' @export
toTrialDuration <- function(ev, trialEnd, interval, writeDir = NULL) {
  checkmate::assertClass(ev, "rxEt")
  reg <- as.data.frame(ev[,c("id", "time")])
  reg <- reg[reg$time == ave(reg$time, reg$id, FUN=min), ]

  reg <- Map(function(id, time) {
    data.frame(id = id, time = seq(time, time+trialEnd, interval))
  }, id = reg$id, time = reg$time
  )
  reg <- do.call(rbind, reg) |>
    merge(unique(ev[, names(ev) != "time", drop = FALSE]), by = "id", all.x = T)
  reg <- et(reg)
  if(is.character(writeDir)){
    write.csv(reg, writeDir, row.names = F)
    }

  reg
}
