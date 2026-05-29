# Helper functions for the test cases of the methods.
# general ode methods used
.methods0 <- c("sdirk43","geng5", "radauiia5", "iiic6", "gauss6", "backwardEuler", "ros6", "ros43", "vern98", "dop87", "vern76", "vern65", "dop54", "rk43", "rkf32", "ssp3", "trapz", "sem", "ros4", "bs", "dop5", "ab", "abm", "ck54", "rk4", "dop853", "rkf78", "liblsoda", "cvode")
.methods0 <- c("dop853", "liblsoda")
# dense methods used for nmtest
.methods1 <- c("ddop853", "ddop5", "dbs", "dros4")
.methods1 <- c("ddop853")
# Linear compartment methods used for nmtest
.methods2 <- c("A", "B", "Ao", "Bo", "As", "Bs", "Ad", "Bd", "Al", "Bl")
