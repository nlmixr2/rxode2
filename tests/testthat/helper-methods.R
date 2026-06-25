# Helper functions for the test cases of the methods.
# general ode methods used
# euler excluded: 1st-order accuracy requires hmin << 0.01 to pass covariate tests
# indLin excluded for now
.methods0 <- c("liblsoda", "lsoda", "dop853", "f78", "rk4", "ck54", "ab", "abm", "dop5", "bs", "ros4", "iem", "sem", "sb3a", "sb3am4", "mm", "em", "cvode", "trapz", "ssp3", "f32", "rk43", "dop54", "vern65", "vern76", "dop87", "vern98", "ros43", "ros6", "backwardEuler", "gauss6", "iiic6", "radauiia5", "geng5", "sdirk43", "euler", "midpoint", "heun", "ssp22", "rk3", "ssp53", "s4", "r4", "ls44", "ls54", "ssp54", "s5", "rk5", "c5", "l5", "lk5a", "lk5b", "b6", "s7", "s8_10", "cv8", "s8_12", "s10", "z10", "o10", "h10", "dp54", "v65e", "v76e", "dp87", "v98e", "ssp33", "bs32", "ssp43", "f45", "t54", "s54", "pp54", "pp54b", "bs54", "ss54", "dp65", "c65", "tp64", "v65r", "v65", "dverk65", "tf65", "tp75", "tmy7", "tmy7s", "v76r", "ss76", "v78", "dverk78", "dp85", "tp86", "v87e", "v87r", "ev87", "k87", "f89", "v89", "t98a", "v98r", "s98", "f108", "c108", "b109", "s1110a", "f1210", "o129", "f1412", "lsode", "bdf", "t54+sdirk43")
# AutoSwitch composite methods: dop853 primary paired with each stiff
# secondary that needs a Jacobian.  1st-order secondaries (iem,
# backwardEuler) are intentionally excluded -- when dop853 falls back to
# them they cannot meet the accuracy tolerances, which defeats the purpose
# of a high-order primary.  This set exercises the non-dense composite
# autoswitch dispatch for every higher-order stiff secondary.
.methods0 <- c("dop853+ros4", "dop853+ros43", "dop853+ros6",
               "dop853+gauss6", "dop853+iiic6",
               "dop853+radauiia5", "dop853+geng5", "dop853+sdirk43")
# All dense composite methods (ros4 is the only dense-output stiff secondary).
.methods1 <- c("ddop853+dros4")
.methods2 <- NULL
