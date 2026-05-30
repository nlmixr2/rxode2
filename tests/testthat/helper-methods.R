# Helper functions for the test cases of the methods.
# general ode methods used
.methods0 <- c("euler", "sdirk43","geng5", "radauiia5", "iiic6", "gauss6", "backwardEuler", "ros6", "ros43", "vern98", "dop87", "vern76", "vern65", "dop54", "rk43", "rkf32", "ssp3", "trapz", "sem", "ros4", "bs", "dop5", "ab", "abm", "ck54", "rk4", "dop853", "rkf78", "liblsoda", "cvode", "midpoint", "heun", "rkssp22", "rk3", "rkssp53", "rks4", "rkr4", "rkls44", "rkls54", "rkssp54", "rks5", "rk5", "rkc5", "rkl5", "rklk5a", "rklk5b", "rkb6", "rk7", "rk8_10", "rkcv8", "rk8_12", "rks10", "rkz10", "rko10", "rkh10", "rkck54", "rkdp54", "rkv65e", "rkv76e", "rkdp87", "rkv98e", "rkssp33", "rkbs32", "rkssp43", "rkf45", "rkt54", "rks54", "rkpp54", "rkpp54b", "rkbs54", "rkss54", "rkdp65", "rkc65", "rktp64", "rkv65r", "rkv65", "dverk65", "rktf65", "rktp75", "rktmy7", "rktmy7s", "rkv76r", "rkss76", "rkv78", "dverk78", "rkdp85", "rktp86", "rkv87e", "rkv87r", "rkev87", "rkk87", "rkf89", "rkv89", "rkt98a", "rkv98r", "rks98", "rkf108", "rkc108", "rkb109", "rks1110a", "rkf1210", "rko129", "rkf1412")
#.methods0 <- c("dop853", "liblsoda")
.methods0 <- c("midpoint", "heun", "rkssp22", "rk3", "rkssp53", "rks4", "rkr4", "rkls44", "rkls54", "rkssp54", "rks5", "rk5", "rkc5", "rkl5", "rklk5a", "rklk5b", "rkb6", "rk7", "rk8_10", "rkcv8", "rk8_12", "rks10", "rkz10", "rko10", "rkh10", "rkck54", "rkdp54", "rkv65e", "rkv76e", "rkdp87", "rkv98e", "rkssp33", "rkbs32", "rkssp43", "rkf45", "rkt54", "rks54", "rkpp54", "rkpp54b", "rkbs54", "rkss54", "rkdp65", "rkc65", "rktp64", "rkv65r", "rkv65", "dverk65", "rktf65", "rktp75", "rktmy7", "rktmy7s", "rkv76r", "rkss76", "rkv78", "dverk78", "rkdp85", "rktp86", "rkv87e", "rkv87r", "rkev87", "rkk87", "rkf89", "rkv89", "rkt98a", "rkv98r", "rks98", "rkf108", "rkc108", "rkb109", "rks1110a", "rkf1210", "rko129", "rkf1412")
# dense methods used for nmtest
.methods1 <- c("ddop853", "ddop5", "dbs", "dros4")
#.methods1 <- c("ddop853")
.methods1 <- NULL
# Linear compartment methods used for nmtest
.methods2 <- c("A", "B", "Ao", "Bo", "As", "Bs", "Ad", "Bd", "Al", "Bl")
.methods2 <- NULL
