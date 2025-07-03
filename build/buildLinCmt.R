
library(rxode2)
library(symengine)

# This is where the 2 compartment code is generated from:

rx <- rxode2({
  k10 = p1/v1;
  v = v1;
  k12 = p2/v1;
  k21 = p2/p3;
  k13 = p4/v1;
  k31 = p4/p5;

  yp0 = 1
  dt_ = 1
  yp1 = 0


  sum  = k10 + k12 + k21;
  disc = sqrt(sum*sum - 4*k10*k21);

  L0 = 0.5*(sum + disc);
  L1 = 0.5*(sum - disc);

  invD0 = 1.0/(L1 - L0);
  invD1 = -invD0;

  tmpSum = k10 + k12;

  c1_0_0 = (k21 - L0)*invD0;
  c1_1_0 = k12*invD0;
  c1_0_1 = (k21 - L1)*invD1;
  c1_1_1 = k12*invD1;

  c2_0_0 = k21*invD0;
  c2_1_0 = (tmpSum - L0)*invD0;
  c2_0_1 = k21*invD1;
  c2_1_1 = (tmpSum - L1)*invD1;

  E0 = exp(-L0*dt_);
  E1 = exp(-L1*dt_);


  Xo0 = (E0*c1_0_0 + E1*c1_0_1)*yp0 +
    (E0*c2_0_0 + E1*c2_0_1)*yp1;

  Xo1 = (E0*c1_1_0 + E1*c1_1_1)*yp0 +
    (E0*c2_1_0 + E1*c2_1_1)*yp1;

  Cp = Xo0/v;
})


t <- rxS(rx)

tmp0 <- t$Cp

tmp0 <- log(tmp0)

tmpp <- D(tmp0, "p1")

m <- rxode2(rxOptExpr(paste0("A0=",rxFromSE(tmpp))))

summary(rxC(m))

tmpp <- D(tmp0, "v1")

m <- rxode2(rxOptExpr(paste0("A0=",rxFromSE(tmpp))))

summary(rxC(m))

tmpp <- D(tmp0, "p2")

m <- rxode2(rxOptExpr(paste0("A0=",rxFromSE(tmpp))))

summary(rxC(m))

tmpp <- D(tmp0, "p3")

m <- rxode2(rxOptExpr(paste0("A0=",rxFromSE(tmpp))))

summary(rxC(m))

# Oral case

rx <- rxode2({
  k10 = p1/v1;
  v = v1;
  k12 = p2/v1;
  k21 = p2/p3;
  k13 = p4/v1;
  k31 = p4/p5;

  ypd = 1;
  yp0 = 0
  dt_ = 1
  yp1 = 0
  rDepot = 0;

  sum  = k10 + k12 + k21;
  disc = sqrt(sum*sum - 4*k10*k21);

  L0 = 0.5*(sum + disc);
  L1 = 0.5*(sum - disc);

  invD0 = 1.0/(L1 - L0);
  invD1 = -invD0;

  tmpSum = k10 + k12;

  c1_0_0 = (k21 - L0)*invD0;
  c1_1_0 = k12*invD0;
  c1_0_1 = (k21 - L1)*invD1;
  c1_1_1 = k12*invD1;

  c2_0_0 = k21*invD0;
  c2_1_0 = (tmpSum - L0)*invD0;
  c2_0_1 = k21*invD1;
  c2_1_1 = (tmpSum - L1)*invD1;

  E0 = exp(-L0*dt_);
  E1 = exp(-L1*dt_);

  ## Xo0 = (E0*c1_0_0 + E1*c1_0_1)*yp0 +
  ##   (E0*c2_0_0 + E1*c2_0_1)*yp1;

  ## Xo1 = (E0*c1_1_0 + E1*c1_1_1)*yp0 +
  ##   (E0*c2_1_0 + E1*c2_1_1)*yp1;

  expa = exp(-ka*dt_);
  Ea0 = (E0 - expa)/(ka - L0);
  Ea1 = (E1 - expa)/(ka - L1);
  cf = ka*ypd - rDepot;
  Xo0 = (Ea0*c1_0_0 + Ea1*c1_0_1)*cf;
  Xo1 = (Ea0*c1_1_0 + Ea1*c1_1_1)*cf;

  Cp = Xo0/v;

})


t <- rxS(rx)

tmp0 <- t$Cp

tmp0 <- log(tmp0)

tmpp <- D(tmp0, "p1")

m <- rxode2(rxOptExpr(paste0("A0=",rxFromSE(tmpp))))

summary(rxC(m))

tmpp <- D(tmp0, "v1")

m <- rxode2(rxOptExpr(paste0("A0=",rxFromSE(tmpp))))

summary(rxC(m))

tmpp <- D(tmp0, "p2")

m <- rxode2(rxOptExpr(paste0("A0=",rxFromSE(tmpp))))

summary(rxC(m))

tmpp <- D(tmp0, "p3")

m <- rxode2(rxOptExpr(paste0("A0=",rxFromSE(tmpp))))

summary(rxC(m))

tmpp <- D(tmp0, "ka")

m <- rxode2(rxOptExpr(paste0("A0=",rxFromSE(tmpp))))

summary(rxC(m))



rx <- rxode2({
  sum  = k10 + k12 + k21;
  disc = sqrt(sum*sum - 4*k10*k21);

  L0 = 0.5*(sum + disc);
  L1 = 0.5*(sum - disc);

  invD0 = 1.0/(L1 - L0);
  invD1 = -invD0;

  tmpSum = k10 + k12;

  c1_0_0 = (k21 - L0)*invD0;
  c1_1_0 = k12*invD0;
  c1_0_1 = (k21 - L1)*invD1;
  c1_1_1 = k12*invD1;

  c2_0_0 = k21*invD0;
  c2_1_0 = (tmpSum - L0)*invD0;
  c2_0_1 = k21*invD1;
  c2_1_1 = (tmpSum - L1)*invD1;

  E0 = exp(-L0*dt_);
  E1 = exp(-L1*dt_);

  Xo0 = (E0*c1_0_0 + E1*c1_0_1)*yp0 +
    (E0*c2_0_0 + E1*c2_0_1)*yp1;

  Xo1 = (E0*c1_1_0 + E1*c1_1_1)*yp0 +
    (E0*c2_1_0 + E1*c2_1_1)*yp1;

  Rm0 = (1.0 - E0)/L0;
  Rm1 = (1.0 - E1)/L1;
  Xo0 = Xo0 + (Rm0*c1_0_0 + Rm1*c1_0_1)*R;
  Xo1 = Xo1 + (Rm0*c1_1_0 + Rm1*c1_1_1)*R;
})


t <- rxS(rx)

tmp0 <- t$Xo0
tmp1 <- t$Xo1

message(rxOptExpr(paste0("A0=",rxFromSE(tmp0), "\n",
                         "A1=",rxFromSE(tmp0), "\n")))


rx <- rxode2({
  sum  = k10 + k12 + k21;
  disc = sqrt(sum*sum - 4*k10*k21);

  L0 = 0.5*(sum + disc);
  L1 = 0.5*(sum - disc);

  invD0 = 1.0/(L1 - L0);
  invD1 = -invD0;

  tmpSum = k10 + k12;

  c1_0_0 = (k21 - L0)*invD0;
  c1_1_0 = k12*invD0;
  c1_0_1 = (k21 - L1)*invD1;
  c1_1_1 = k12*invD1;

  c2_0_0 = k21*invD0;
  c2_1_0 = (tmpSum - L0)*invD0;
  c2_0_1 = k21*invD1;
  c2_1_1 = (tmpSum - L1)*invD1;

  E0 = exp(-L0*dt_);
  E1 = exp(-L1*dt_);

  Xo0 = (E0*c1_0_0 + E1*c1_0_1)*yp0 +
    (E0*c2_0_0 + E1*c2_0_1)*yp1;

  Xo1 = (E0*c1_1_0 + E1*c1_1_1)*yp0 +
    (E0*c2_1_0 + E1*c2_1_1)*yp1;

  expa = exp(-ka*dt_);
  Ea0 = (E0 - expa)/(ka - L0);
  Ea1 = (E1 - expa)/(ka - L1);
  cf = ka*ypd;
  Xo0 = Xo0 + (Ea0*c1_0_0 + Ea1*c1_0_1)*cf;
  Xo1 = Xo1 + (Ea0*c1_1_0 + Ea1*c1_1_1)*cf;
  Xod = ypd*expa;

  Rm0 = (1.0 - E0)/L0;
  Rm1 = (1.0 - E1)/L1;
  Xo0 = Xo0 + (Rm0*c1_0_0 + Rm1*c1_0_1)*R;
  Xo1 = Xo1 + (Rm0*c1_1_0 + Rm1*c1_1_1)*R;

})

t <- rxS(rx)

tmpd <- t$Xod
tmp0 <- t$Xo0
tmp1 <- t$Xo1

message(rxOptExpr(paste0("Ad=",rxFromSE(tmpd), "\n",
                         "A0=",rxFromSE(tmp0), "\n",
                         "A1=",rxFromSE(tmp0), "\n")))


rx <- rxode2({
  sum  = k10 + k12 + k21;
  disc = sqrt(sum*sum - 4*k10*k21);

  L0 = 0.5*(sum + disc);
  L1 = 0.5*(sum - disc);

  invD0 = 1.0/(L1 - L0);
  invD1 = -invD0;

  tmpSum = k10 + k12;

  c1_0_0 = (k21 - L0)*invD0;
  c1_1_0 = k12*invD0;
  c1_0_1 = (k21 - L1)*invD1;
  c1_1_1 = k12*invD1;

  c2_0_0 = k21*invD0;
  c2_1_0 = (tmpSum - L0)*invD0;
  c2_0_1 = k21*invD1;
  c2_1_1 = (tmpSum - L1)*invD1;

  E0 = exp(-L0*dt_);
  E1 = exp(-L1*dt_);

  Xo0 = (E0*c1_0_0 + E1*c1_0_1)*yp0 +
    (E0*c2_0_0 + E1*c2_0_1)*yp1;

  Xo1 = (E0*c1_1_0 + E1*c1_1_1)*yp0 +
    (E0*c2_1_0 + E1*c2_1_1)*yp1;

  expa = exp(-ka*dt_);
  Ea0 = (E0 - expa)/(ka - L0);
  Ea1 = (E1 - expa)/(ka - L1);
  cf = ka*ypd - rDepot
  Xo0 = Xo0 + (Ea0*c1_0_0 + Ea1*c1_0_1)*cf;
  Xo1 = Xo1 + (Ea0*c1_1_0 + Ea1*c1_1_1)*cf;
  Xod = ypd*expa;
  Xod = Xod + rDepot*(1.0-expa)/ka;
})

t <- rxS(rx)

tmpd <- t$Xod
tmp0 <- t$Xo0
tmp1 <- t$Xo1

message(rxOptExpr(paste0("Ad=",rxFromSE(tmpd), "\n",
                         "A0=",rxFromSE(tmp0), "\n",
                         "A1=",rxFromSE(tmp0), "\n")))

rx <- rxode2({
  sum  = k10 + k12 + k21;
  disc = sqrt(sum*sum - 4*k10*k21);

  L0 = 0.5*(sum + disc);
  L1 = 0.5*(sum - disc);

  invD0 = 1.0/(L1 - L0);
  invD1 = -invD0;

  tmpSum = k10 + k12;

  c1_0_0 = (k21 - L0)*invD0;
  c1_1_0 = k12*invD0;
  c1_0_1 = (k21 - L1)*invD1;
  c1_1_1 = k12*invD1;

  c2_0_0 = k21*invD0;
  c2_1_0 = (tmpSum - L0)*invD0;
  c2_0_1 = k21*invD1;
  c2_1_1 = (tmpSum - L1)*invD1;

  E0 = exp(-L0*dt_);
  E1 = exp(-L1*dt_);

  Xo0 = (E0*c1_0_0 + E1*c1_0_1)*yp0 +
    (E0*c2_0_0 + E1*c2_0_1)*yp1;

  Xo1 = (E0*c1_1_0 + E1*c1_1_1)*yp0 +
    (E0*c2_1_0 + E1*c2_1_1)*yp1;

  expa = exp(-ka*dt_);
  Ea0 = (E0 - expa)/(ka - L0);
  Ea1 = (E1 - expa)/(ka - L1);
  cf = ka*ypd - rDepot;
  Xo0 = Xo0 + (Ea0*c1_0_0 + Ea1*c1_0_1)*cf;
  Xo1 = Xo1 + (Ea0*c1_1_0 + Ea1*c1_1_1)*cf;
  Xod = ypd*expa;
  Xod = Xod + rDepot*(1.0-expa)/ka;
  Rm0 = (1.0 - E0)/L0;
  Rm1 = (1.0 - E1)/L1;
  Xo0 = Xo0 + (Rm0*c1_0_0 + Rm1*c1_0_1)*R;
  Xo1 = Xo1 + (Rm0*c1_1_0 + Rm1*c1_1_1)*R;
})


t <- rxS(rx)

tmpd <- t$Xod
tmp0 <- t$Xo0
tmp1 <- t$Xo1


message(rxOptExpr(paste0("Ad=",rxFromSE(tmpd), "\n",
                         "A0=",rxFromSE(tmp0), "\n",
                         "A1=",rxFromSE(tmp0), "\n")))


rx <- rxode2({
  c1_0_0 = (k21 - L0)*invD0;
  c1_1_0 = k12*invD0;
  c1_0_1 = (k21 - L1)*invD1;
  c1_1_1 = k12*invD1;
  Xo0 = (Ea0*c1_0_0 + Ea1*c1_0_1)*cf;
  Xo1 = (Ea0*c1_1_0 + Ea1*c1_1_1)*cf;
})


s <- rxS(rx)

tmp0 <- s$Xo0
tmp1 <- s$Xo1



rx <- rxode2({
  c1_0_0 = (k21 - L0)*invD0;
  c1_1_0 = k12*invD0;
  c1_0_1 = (k21 - L1)*invD1;
  c1_1_1 = k12*invD1;
  Xo0  = (Rm0*c1_0_0 + Rm1*c1_0_1)*R;
  Xo1  = (Rm0*c1_1_0 + Rm1*c1_1_1)*R;
})

s <- rxS(rx)

tmp0 <- s$Xo0
tmp1 <- s$Xo1
