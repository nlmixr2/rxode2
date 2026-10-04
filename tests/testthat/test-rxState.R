## Building and solving a model keeps its mutable state in `.rxState` rather
## than writing namespace variables with assignInMyNamespace(), which gets
## slow once other packages register S3 methods on rxode2 generics (#1425).
rxTest({
  test_that("building, solving and printing a model writes no rxode2 namespace variables", {
    .ns <- asNamespace("rxode2")
    .mod <- function() {
      ini({
        tka <- 0.45
        tcl <- 1
        tv <- 3.45
        eta.cl ~ 0.3
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv)
        d / dt(depot) <- -ka * depot
        d / dt(center) <- ka * depot - cl / v * center
        cp <- center / v
        cp ~ add(add.sd)
      })
    }
    .ev <- et(amt = 100) |> et(seq(0, 24, by = 4))
    .run <- function(k) {
      .m <- rxode2(.mod)
      .m <- eval(bquote(model(.m, cp2 <- cp * .(k), append = TRUE)))
      .s <- rxSolve(.m, .ev, nSub = 2, returnType = "data.frame")
      utils::capture.output(print(rxSolve(.m, .ev)))
      .s
    }
    # warm up the session-wide one-time caches
    .run(2)
    .calls <- character(0)
    .tracer <- bquote(assign(".calls", c(get(".calls", envir = .(environment())), x), envir = .(environment())))
    suppressMessages(trace(utils::assignInMyNamespace, .tracer, print = FALSE, where = asNamespace("utils")))
    on.exit(suppressMessages(untrace(utils::assignInMyNamespace, where = asNamespace("utils"))), add = TRUE)
    # a model not built before in this session
    .run(3)
    .calls <- .calls[vapply(.calls, exists, logical(1), envir = .ns, inherits = FALSE)]
    expect_identical(.calls, character(0))
  })
})
