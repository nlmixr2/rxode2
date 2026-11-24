# Function-specific code generation: rxode2.fullOpt = "none" vs "slice".
#
# For each model: statements emitted per generated C function, and the time
# to solve it.  The FOCEi inner models need nlmixr2est; without it only the
# rxode2-only models run.
#
#   Rscript bench/fullopt.R [nsub] [reps]

suppressMessages(library(rxode2))
.args <- commandArgs(TRUE)
.nsub <- if (length(.args) >= 1) as.integer(.args[1]) else 200L
.reps <- if (length(.args) >= 2) as.integer(.args[2]) else 5L
.levels <- c("none", "slice")

## statements per generated function (assignments to model variables)
.fullOptCount <- function(cFile) {
  .l <- readLines(cFile)
  .st <- grep("^(void|double) [A-Za-z0-9_]+__[A-Za-z_]+\\(", .l)
  .nm <- sub("^(void|double) [A-Za-z0-9_]+__([A-Za-z_]+)\\(.*$", "\\2", .l[.st])
  .end <- c(.st[-1] - 1L, length(.l))
  setNames(
    vapply(
      seq_along(.st),
      function(i) {
        .b <- .l[.st[i]:.end[i]]
        .b <- .b[!grepl("_PL\\[|_PP\\[|NA_REAL|_lhs\\[|__zzStateVar__\\[__DDT[0-9]+__\\]\\*", .b)]
        sum(grepl("^\\s*[A-Za-z_][A-Za-z_0-9]*(\\[[^]]*\\])? *=[^=]", .b))
      },
      0L
    ),
    .nm
  )
}

.fullOptModels <- function() {
  .ret <- list(
    sens = list(
      model = rxNorm(rxode2({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        d/dt(depot) <- -ka * depot
        d/dt(center) <- ka * depot - cl / v * center
        cp <- center / v
      }, calcSens = TRUE)),
      params = c(tka = 0.45, tcl = 1, tv = 3.45, eta.ka = 0, eta.cl = 0, eta.v = 0)
    )
  )
  if (requireNamespace("nlmixr2est", quietly = TRUE)) {
    suppressMessages(loadNamespace("nlmixr2est"))
    .one <- function() {
      ini({
        tka <- 0.45; tcl <- 1; tv <- 3.45
        eta.ka ~ 0.6; eta.cl ~ 0.3; eta.v ~ 0.1
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka); cl <- exp(tcl + eta.cl); v <- exp(tv + eta.v)
        d/dt(depot) <- -ka * depot
        d/dt(center) <- ka * depot - cl / v * center
        cp <- center / v
        cp ~ add(add.sd)
      })
    }
    .two <- function() {
      ini({
        tka <- 0.45; tcl <- 1; tv <- 3.45; tq <- 0.5; tvp <- 4
        eta.ka ~ 0.6; eta.cl ~ 0.3; eta.v ~ 0.1; eta.q ~ 0.1; eta.vp ~ 0.1
        add.sd <- 0.7; prop.sd <- 0.1
      })
      model({
        ka <- exp(tka + eta.ka); cl <- exp(tcl + eta.cl); v <- exp(tv + eta.v)
        q <- exp(tq + eta.q); vp <- exp(tvp + eta.vp)
        d/dt(depot) <- -ka * depot
        d/dt(center) <- ka * depot - cl / v * center - q / v * center + q / vp * periph
        d/dt(periph) <- q / v * center - q / vp * periph
        cp <- center / v
        cp ~ add(add.sd) + prop(prop.sd)
      })
    }
    .inner <- function(f, theta, neta) {
      .ui <- rxode2(f)
      list(
        model = rxNorm(.ui$foceiModel$inner),
        params = c(
          setNames(theta, paste0("THETA[", seq_along(theta), "]")),
          setNames(rep(0, neta), paste0("ETA[", seq_len(neta), "]"))
        )
      )
    }
    .ret$focei1 <- .inner(.one, c(0.45, 1, 3.45, 0.7), 3L)
    .ret$focei2 <- .inner(.two, c(0.45, 1, 3.45, 0.5, 4, 0.7, 0.1), 5L)
  }
  .ret
}

.ev <- et(amt = 320, cmt = "depot") |>
  et(c(0.25, 0.5, 1, 2, 3.5, 5, 7, 9, 12, 24)) |>
  et(id = seq_len(.nsub))

.models <- .fullOptModels()
.out <- NULL
for (.n in names(.models)) {
  .cur <- .models[[.n]]
  .sol <- list()
  for (.lvl in .levels) {
    options(rxode2.fullOpt = .lvl)
    .m <- rxode2(.cur$model)
    .cnt <- .fullOptCount(rxC(.m))
    .s <- rxSolve(.m, .ev, params = .cur$params, returnType = "matrix")
    .t <- vapply(
      seq_len(.reps),
      function(i) {
        system.time(rxSolve(.m, .ev, params = .cur$params, returnType = "matrix"))[["elapsed"]]
      },
      0
    )
    .sol[[.lvl]] <- .s
    .out <- rbind(
      .out,
      data.frame(
        model = .n,
        level = .lvl,
        seconds = median(.t),
        dydt = .cnt["dydt"],
        calc_lhs = .cnt["calc_lhs"],
        calc_jac = .cnt["calc_jac"],
        row.names = NULL
      )
    )
  }
  if (!identical(.sol[["none"]], .sol[["slice"]])) {
    warning(sprintf("model '%s': solutions differ between levels", .n), call. = FALSE)
  }
}
options(rxode2.fullOpt = NULL)
print(.out, row.names = FALSE)
