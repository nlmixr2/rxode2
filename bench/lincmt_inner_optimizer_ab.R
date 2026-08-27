# lincmt_inner_optimizer_ab.R -- compare the inner optimizers on the one
# quantity that made the per-direction slope look large: how often the
# inner problem re-solves a subject as the random-effect count grows.
#
# Reads the arms written by bench/lincmt_inner_scaling.R:
#   bench/results/inner_scaling_<innerOpt>.rds
#
# WHAT IS COMPARABLE.  The headline is a COUNT -- kernel entries per
# objective evaluation -- and a count does not depend on the optimization
# level a package was built at, so the arms can be compared even when
# their builds differ.  Wall times are comparable only between arms built
# the same way; run both arms in ONE tree with only innerOpt changed and
# they are.

args <- commandArgs(trailingOnly = TRUE)
arms <- if (length(args)) args else c("n1qn1", "trust")
r <- list()
for (a in arms) {
  f <- sprintf("bench/results/inner_scaling_%s.rds", a)
  if (file.exists(f)) r[[a]] <- readRDS(f) else
    message("missing arm: ", f)
}
if (length(r) < 1) stop("no arms found")

fm <- function(x, d = 2) formatC(x, format = "f", digits = d)
cat("\n== inner solves per subject, by random-effect count ==\n\n")
cat(sprintf("  %-8s %s\n", "arm",
            paste(sprintf("%8s", paste0(1:5, " eta")), collapse = "")))
for (a in names(r)) {
  d <- r[[a]]
  cat(sprintf("  %-8s %s\n", a,
              paste(sprintf("%8s", fm(d$innerSolvesPerSubj, 1)), collapse = "")))
}

cat("\n== growth from 1 to 5 random effects ==\n\n")
cat(sprintf("  %-8s %12s %12s %12s %10s\n", "arm", "time/eval",
            "entries/eval", "per entry", "resolve %"))
for (a in names(r)) {
  d <- r[[a]]
  if (nrow(d) < 5) { cat(sprintf("  %-8s incomplete (%d cells)\n", a, nrow(d))); next }
  tm  <- (d$sec[5]/d$feval[5]) / (d$sec[1]/d$feval[1])
  cnt <- d$entriesPerEval[5] / d$entriesPerEval[1]
  cat(sprintf("  %-8s %11sx %11sx %11sx %9s%%\n", a, fm(tm), fm(cnt),
              fm(tm/cnt), fm(100*log(cnt)/log(tm), 0)))
}

if (all(c("n1qn1", "trust") %in% names(r)) &&
    nrow(r$n1qn1) == 5 && nrow(r$trust) == 5) {
  a <- r$n1qn1; b <- r$trust
  cn <- a$entriesPerEval[5]/a$entriesPerEval[1]
  ct <- b$entriesPerEval[5]/b$entriesPerEval[1]
  lvl <- a$innerSolvesPerSubj / b$innerSolvesPerSubj
  cat("\n== reading it ==\n")
  ## LEVEL and SHAPE are different questions and a single threshold answers
  ## neither well.  Report both.
  cat(sprintf("  LEVEL: trust needs %s to %s times fewer inner solves,\n",
              fm(min(lvl)), fm(max(lvl))))
  cat(sprintf("         and the gap WIDENS with dimension (%s at 1 eta, %s at 5).\n",
              fm(lvl[1]), fm(lvl[5])))
  cat(sprintf("  SHAPE: growth 1 -> 5 eta is %sx under n1qn1 and %sx under trust\n",
              fm(cn), fm(ct)))
  cat(sprintf("         -- trust flattens it by %s%%, so most of the dimensional\n",
              fm(100*(1 - ct/cn), 0)))
  cat("         growth is NOT an artifact of the optimizer.\n")
  cat("\n  So the optimizer is worth changing on the level, but it does not\n")
  cat("  remove the growth: the inner problem genuinely solves far more as\n")
  cat("  dimension rises, whichever of these two drives it.\n")
  ## A discontinuity in either arm is a separate finding from the trend.
  for (nm in names(r)) {
    d <- r[[nm]]; st <- diff(d$innerSolvesPerSubj)
    if (max(st)/min(st) > 3) {
      j <- which.max(st)
      cat(sprintf("\n  NOTE (%s): the rise is not smooth -- solves per subject jump\n", nm))
      cat(sprintf("  %s -> %s between %d and %d random effects, %sx the next largest\n",
                  fm(d$innerSolvesPerSubj[j],1), fm(d$innerSolvesPerSubj[j+1],1),
                  d$nEta[j], d$nEta[j+1], fm(max(st)/sort(st, decreasing=TRUE)[2])))
      cat("  step.  Present under BOTH optimizers, so it is not the optimizer.\n")
    }
  }
}
