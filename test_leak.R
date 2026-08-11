library(rxode2)
# Compile a bad model
bad_mod <- "d/dt(A) = B; # B is undeclared"
try(rxode2(bad_mod))
print(rxode2::.rxLastCompileSuccess())

# Now compile a good model that we ALREADY compiled so it hits the cache
good_mod <- "d/dt(A) = 1"
# first compile (this will clear the error)
invisible(rxode2(good_mod))
print(rxode2::.rxLastCompileSuccess())

# Now compile a bad model again
try(rxode2(bad_mod))

# Now compile the good model again, it should hit cache
invisible(rxode2(good_mod))
print(rxode2::.rxLastCompileSuccess())
