rxTest({

  test_that("assignFuns() is safe under concurrent invocation", {
    ## Regression test for the data race on the compiled model's
    ## `_assignFuns()` initialiser.  Prior to the fix the template in
    ## `inst/include/rxode2_model_shared.c` used
    ##     if (_assign_ptr == NULL) { _assignFuns0(); }
    ## with no synchronisation.  `_assignFuns0()` makes many
    ## `R_GetCCallable` calls — R's symbol table is not documented as
    ## thread-safe, so multiple threads racing through that first-call
    ## path can corrupt R's internals.  The pattern is exercised in
    ## production by nlmixr2est's FOCEi inner loop, which calls
    ## `ind_solve()` for each subject from its own
    ## `#pragma omp parallel for` without a pre-init.
    ##
    ## The fix wraps `_assignFuns0()` in
    ## `#pragma omp critical(rxode2AssignFunsInit)` inside the
    ## generated `_assignFuns()` with a double-checked pointer load
    ## around it, so only one thread per compiled model enters
    ## `_assignFuns0()`.
    ##
    ## `rxTestParallelAssignFuns()` (exported from src/rxData.cpp)
    ## spawns `nThreads` OpenMP workers that each invoke
    ## `assignFuns()` concurrently.  Combined with freshly compiled
    ## models whose `_assign_ptr` is still NULL at test entry, this
    ## hits the exact first-call race path that the fix closes.
    ##
    ## Note on reproducibility: on x86-64 with recent glibc the racy
    ## writes in `_assignFuns0()` all land on the same values and
    ## aligned pointer reads/writes are atomic at the hardware level,
    ## so this test does NOT deterministically crash under the pre-fix
    ## template in a normal Linux build.  Under `-fsanitize=thread`
    ## the race is reported deterministically.  On architectures with
    ## weaker memory ordering (AArch64, POWER) the race can manifest
    ## as intermittent corruption.  The test provides coverage for
    ## the parallel path and keeps the contract documented; combined
    ## with a TSan CI run it also functions as an automatic
    ## regression detector.

    skip_on_os("windows") ## OpenMP stress tests are flaky on CRAN Windows builders
    skip_on_cran()        ## heavy multi-model stress loop; not CRAN-friendly

    ## Number of fresh models to compile.  Each model has its own
    ## `_assign_ptr` static, so the first parallel `assignFuns()` call
    ## for each model enters the racy first-call path.  More models →
    ## more opportunities for the race to manifest as a visible crash.
    nModels  <- 20L
    nThreads <- max(8L, parallel::detectCores())

    for (i in seq_len(nModels)) {
      ## Fresh unload drops all cached compiled libraries so each
      ## `rxode2()` call below produces a fresh `.so` whose
      ## `_assign_ptr` global is NULL on entry.
      rxUnloadAll()

      ## Slightly different code per iteration forces a fresh hash and
      ## a fresh compilation (rxode2 caches by code hash).  Both `kI`
      ## and the tag comment change so the generated source text and
      ## MD5 differ.
      kI <- 0.1 + i * 0.01
      mod <- rxode2(paste0(
        "# rxTestParallelAssignFuns iteration ", i, "\n",
        "d/dt(depot)   <- -ka * depot\n",
        "d/dt(central) <-  ka * depot - cl / v * central\n",
        "cp <- central / v\n",
        "rxExtra <- ", kI, "\n"
      ))

      ## Point rxode2's global `assignFuns` function pointer at this
      ## freshly compiled model.  `rxAssignPtr` does NOT call
      ## `assignFuns()` itself, so the model's `_assign_ptr` is still
      ## NULL when the parallel stress test starts — this is exactly
      ## the first-call-from-parallel path that triggers the race.
      rxAssignPtr(mod)

      ## `nThreads` workers all invoke `assignFuns()` concurrently.
      ## Only the first call per thread matters — subsequent calls
      ## see `_assign_ptr != NULL` and return immediately — but the
      ## key observation is that under the pre-fix template all
      ## `nThreads` threads enter `_assignFuns0()` simultaneously on
      ## their first call, racing on R's symbol table through many
      ## `R_GetCCallable` invocations.  Under the fix, only one
      ## thread enters `_assignFuns0()` under the critical section
      ## and the rest exit after the outer check.
      expect_silent(rxode2:::rxTestParallelAssignFuns(nThreads = nThreads, niter = 1L))
    }

    ## Sanity check: the last model is still solvable after the
    ## stress test.
    ev  <- et(amt = 100) |> et(seq(0, 24, by = 4))
    res <- rxSolve(mod, c(ka = 0.5, cl = 1, v = 10), ev)
    expect_true(nrow(res) > 0)
    expect_true(all(is.finite(res$cp)))
  })

})
