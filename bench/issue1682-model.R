## The model generator from issue #1682, shared by bench/issue1682-corpus.R
## and bench/issue1682-symengine.R: an arm-level logit model with K drugs
## (Emax each) and P dosing periods, where each period's effect moves from the
## previous period's level along a Hill time course.

## the issue's model generator; `form = "power"` writes the Hill onset as
## D^g / (T50^g + D^g) instead of a logistic in log(D + 1e-6)
.fastCase <- function(P, K, form = "log", seed = 1L) {
  set.seed(seed)
  arms <- expand.grid(ARM = 1:3, STUDY = 1:(2 * K))
  arms$ID <- seq_len(nrow(arms))
  arms$DRUG <- ceiling(arms$STUDY / 2)
  start <- matrix(
    c(4 * (0:(P - 1)), 1e4),
    nrow(arms),
    P + 1,
    byrow = TRUE,
    dimnames = list(NULL, paste0("T", 1:(P + 1)))
  )
  d <- merge(cbind(arms, start), data.frame(TIME = seq(2, 4 * P, by = 2)))
  d <- d[order(d$ID, d$TIME), ]
  for (p in 1:P) {
    for (k in 1:K) {
      d[[sprintf("R%dD%d", p, k)]] <- ifelse(d$DRUG == k & d$ARM > 1, c(0, 0.5, 1)[d$ARM] * (1 + (p > 1)), 0)
    }
  }
  d$WEIGHT <- 50
  d$DV <- (rnorm(nrow(arms), -1, 1)[d$ID] + rnorm(nrow(d), 0, 0.3)) * sqrt(d$WEIGHT)
  .hill <- if (form == "log") {
    "    H%1$d <- 1 / (1 + exp(GHILL * (log(TD50) - log(D%1$d + 1e-06))))"
  } else {
    "    H%1$d <- D%1$d^GHILL / (TD50^GHILL + D%1$d^GHILL)"
  }
  per <- vapply(
    1:P,
    function(p) {
      paste(
        c(
          sprintf("    X%1$d <- TIME - T%1$d\n    D%1$d <- 0\n    if (X%1$d > 0) D%1$d <- X%1$d", p),
          if (p < P) sprintf("    L%1$d <- T%2$d - T%1$d\n    if (X%1$d > L%1$d) D%1$d <- L%1$d", p, p + 1),
          sprintf(
            "    A%d <- %s",
            p,
            paste(sprintf("emax%2$d * R%1$dD%2$d / (exp(led50%2$d) + R%1$dD%2$d)", p, 1:K), collapse = " + ")
          ),
          sprintf(.hill, p),
          sprintf("    E%1$d <- E%2$d + (A%1$d - E%2$d) * H%1$d", p, p - 1)
        ),
        collapse = "\n"
      )
    },
    ""
  )
  src <- paste0(
    "function() {\n  ini({\n    b0 <- -3\n    eta.b ~ 1\n    lton <- log(4)\n    lhill <- log(1.5)\n",
    "    ltd50 <- log(3)\n",
    paste0(sprintf("    emax%1$d <- 4\n    led50%1$d <- 0", 1:K), collapse = "\n"),
    "\n    sdw <- 2\n  })\n  model({\n    BASE <- b0 + eta.b\n    TON <- exp(lton)\n",
    "    ONSET <- (TIME + TON - abs(TIME - TON)) / 2 / TON\n    GHILL <- exp(lhill)\n",
    "    TD50 <- exp(ltd50)\n    E0 <- 0\n",
    paste(per, collapse = "\n"),
    sprintf("\n    LMAX <- BASE + E%d\n", P),
    "    PRESP <- ONSET / (1 + exp(-LMAX))\n    LOGIT <- log(PRESP / (1 - PRESP))\n",
    "    pred <- LOGIT * sqrt(WEIGHT)\n    pred ~ add(sdw)\n  })\n}\n"
  )
  list(model = eval(parse(text = src)), data = d)
}
