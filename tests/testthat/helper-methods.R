# Helper functions for the test cases of the methods.
# general ode methods used
# euler excluded: 1st-order accuracy requires hmin << 0.01 to pass covariate tests
.methods0 <- c("t54+sdirk43", "lsode", "bdf", "dop853", "liblsoda", "midpoint", "heun", "ssp22", "rk3", "ssp53", "s4", "r4", "ls44", "ls54", "ssp54", "s5", "rk5", "c5", "l5", "lk5a", "lk5b", "b6", "rk7", "rk8_10", "cv8", "rk8_12", "s10", "z10", "o10", "h10", "ck54", "dp54", "v65e", "v76e", "dp87", "v98e", "ssp33", "bs32", "ssp43", "f45", "t54", "s54", "pp54", "pp54b", "bs54", "ss54", "dp65", "c65", "tp64", "v65r", "v65", "dverk65", "tf65", "tp75", "tmy7", "tmy7s", "v76r", "ss76", "v78", "dverk78", "dp85", "tp86", "v87e", "v87r", "ev87", "k87", "f89", "v89", "t98a", "v98r", "s98", "f108", "c108", "b109", "s1110a", "f1210", "o129", "f1412")
.methods0 <- c("lsode", "liblsoda", "dop853", "t54+sdirk43")
.methods0 <- "lsode"
# deSolve-derived single-threaded BDF solvers (lsode, bdf)
# These use non-reentrant Fortran COMMON blocks; always run on 1 core.
# dense methods used for nmtest
.methods1 <- c("ddop853", "ddop5", "dbs", "dros4")
#.methods1 <- c("ddop853")
# Linear compartment methods used for nmtest
#.methods2 <- c("A", "B", "Ao", "Bo", "As", "Bs", "Ad", "Bd", "Al", "Bl")
