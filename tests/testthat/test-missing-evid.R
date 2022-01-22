ode.1c <- rxode2({
  V <- 20
  Cl <- 1
  fc <- 1
  C2 <- center / V
  d / dt(center) ~ -Cl * C2
  f(center) <- fc
})

dur <- 0.5
ii <- 2
m <- "liblsoda"
d <- 3

for (m in c("liblsoda", "lsoda", "dop853")) {
  test_that(sprintf("missing evid tests for %s", m), {
    for (dur in c(0.5, 1)) {
      for (ii in seq(2, 24, by = 2)) {
        et <- et() %>%
          et(amt = d, rate = d / dur) %>%
          et(time = ii, amt = d, ii = ii, addl = floor(24 / ii), rate = d / dur) %>%
          et(c(dur, seq(0, 24, length.out = 200)))
        
        x2 <- solve(ode.1c, et, method = m, maxsteps = 10000)
        
        et <- data.frame(et)
        et <- et[, names(et) != "evid"]
        
        x3 <- expect_warning(solve(ode.1c, et, method = m, maxsteps = 10000), NA)
        
        expect_equal(data.frame(x2), data.frame(x3))
        
        et2 <- et(et)
        
        expect_equal(data.frame(et), data.frame(et2)[, c("time", "amt", "rate", "ii", "addl")])
        
        
        d <- 3
        et <- et() %>%
          et(amt = d, dur = dur) %>%
          et(time = ii, amt = d, ii = ii, addl = floor(24 / ii), dur = dur) %>%
          et(c(dur, seq(0, 24, length.out = 200)))
        
        x2 <- solve(ode.1c, et, method = m, maxsteps = 10000)
        
        et <- data.frame(et)
        et <- et[, names(et) != "evid"]
        
        x3 <- expect_warning(solve(ode.1c, et, method = m, maxsteps = 10000), NA)
        
        expect_equal(data.frame(x2), data.frame(x3))
        
        et <- et() %>%
          et(amt = d, ii = ii, ss = 1, dur = dur) %>%
          et(c(dur, seq(0, 24, length.out = 200)))
        
        x2 <- solve(ode.1c, et, method = m, maxsteps = 10000)
        
        et <- data.frame(et)
        et <- et[, names(et) != "evid"]
        
        x3 <- expect_warning(solve(ode.1c, et, method = m, maxsteps = 10000), NA)
        
        expect_equal(data.frame(x2), data.frame(x3))
      }
    }
  })
}
