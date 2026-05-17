ode <- "
   C2 = centr/V2;
   C3 = peri/V3;
   d/dt(depot) =-KA*depot;
   d/dt(centr) = KA*depot - CL*C2 - Q*C2 + Q*C3;
   d/dt(peri)  =                    Q*C2 - Q*C3;
   d/dt(eff)  = Kin - Kout*(1-C2/(EC50+C2))*eff;
"

m1 <- RxODE(model = ode)

# create dosing and observation (sampling) events
# QD (once daily) dosing, 5 days

qd <- eventTable(amount.units = "ug", time.units = "hours")
qd$add.dosing(dose = 10000, nbr.doses = 5, dosing.interval = 24)

# hourly sampling during 1st day, every 4 hours afterwards
qd$add.sampling(0:24) # hourly
qd$add.sampling(seq(from = 28, to = 96, by = 4)) # every 4 hours

# BID dosing, 5 days

bid <- eventTable(amount.units = "ug", time.units = "days") # only dosing
bid$add.dosing(
  dose = 10000,
  nbr.doses = 2 * 5,
  dosing.interval = 12
)

bid$add.sampling(0:24)
bid$add.sampling(96 + 0:24)

# init values
theta <-
  c(
    KA = 2.94E-01, CL = 1.86E+01, V2 = 4.02E+01, Q = 1.05E+01, V3 = 2.97E+02,
    Kin = 1, Kout = 1, EC50 = 200
  )

qd.cp <- m1$solve(theta, qd, inits = c(0, 0, 0, 1))
bid.cp <- m1$solve(theta, bid, inits = c(0, 0, 0, 1))

cp.plot <-
  function(cp, xlab = "Time (days)", ...) {
    xtime <- cp[, "time"]
    matplot(xtime, cp[, c("depot", "centr", "peri")],
      type = "l", ...,
      xlab = xlab, ylab = "Drug amount (ug)"
    )

    legend("topright",
      legend = c("Depot", "Central", "Peripheral"),
      title = "Compartment",
      col = 1:3, lty = 1:3, bty = "n"
    )

    plot(xtime, cp[, "eff"],
      type = "l", ...,
      xlab = xlab, ylab = "Effect compartment"
    )
  }

cp.plot(qd.cp, main = "QD dosing, 5 days")

cp.plot(bid.cp, main = "BID dosing, 5 days")
rxClean() # Remove dlls and extra files created by RxODE
