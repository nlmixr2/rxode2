## rxode2#1216: method="indLin" solves subjects in parallel.
## Two models: a linear matExp(), where each subject is cheap and the per-thread
## exponential cache does most of the work, and a Michaelis-Menten forcing,
## where the adaptive driver gives each subject real work to do.
suppressMessages(library(rxode2))

suppressMessages({
  .lin <- rxode2(paste("matExp()", "cmt(depot)", "cmt(central)",
                       "k_depot_central = ka", "k_central_output = ke",
                       sep = "\n"))
  .mm <- rxode2(rxToIndLin(paste0(
    "d/dt(depot) = -ka*depot\n",
    "d/dt(central) = ka*depot - vmax*(central/v)/(km + central/v)\n")))
})

.linPar <- c(ka = 1, ke = 0.2)
.mmPar <- c(ka = 1, km = 0.5, vmax = 0.2, v = 1)

nsub <- 1000
.ev <- as.data.frame(et(amt = 100, cmt = "depot") |>
                       et(seq(0, 48, by = 0.5)) |> et(id = seq_len(nsub)))

.solve <- function(model, params, nc) {
  suppressMessages(rxSolve(model, params = params, events = .ev,
                           method = "indLin", cores = nc,
                           returnType = "data.frame"))
}

for (.m in list(list(n = "linear matExp()", m = .lin, p = .linPar),
                list(n = "Michaelis-Menten indLin()", m = .mm, p = .mmPar))) {
  invisible(.solve(.m$m, .m$p, 1L))                    # warm up
  cat(sprintf("=== %s, %d subjects ===\n", .m$n, nsub))
  .base <- NA_real_
  for (nc in c(1L, 2L, 4L, 8L)) {
    ts <- vapply(1:3, function(i) system.time(.solve(.m$m, .m$p, nc))["elapsed"],
                 numeric(1))
    if (nc == 1L) .base <- median(ts)
    cat(sprintf("  cores=%d  median=%.3fs  speedup=%.2fx  (%.3f %.3f %.3f)\n",
                nc, median(ts), .base/median(ts), ts[1], ts[2], ts[3]))
  }
}
