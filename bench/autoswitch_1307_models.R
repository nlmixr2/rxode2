## Model suite shared by the #1307 benchmarks (sourced, not run standalone).
mods <- list()

mods$oral1 <- list(
  out = "cp",
  m = rxode2({
    d/dt(depot)  <- -ka*depot
    d/dt(center) <-  ka*depot - (cl/v)*center
    cp <- center/v
  }),
  p = c(ka=1, cl=1, v=20),
  ev = function(n) {
    e <- et(amt=100, cmt="depot", ii=24, addl=6)
    e <- et(e, seq(0, 168, by=0.5))
    if (n > 1) e <- et(e, id=1:n)
    e
  })

mods$tmdd <- list(
  out = "cp",
  m = rxode2({
    d/dt(depot)  <- -ka*depot
    d/dt(L)      <-  ka*depot - kel*L - kon*L*R + koff*RL
    d/dt(R)      <-  ksyn - kdeg*R - kon*L*R + koff*RL
    d/dt(RL)     <-  kon*L*R - koff*RL - kint*RL
    cp <- L
  }),
  p = c(ka=0.5, kel=0.1, kon=100, koff=1, ksyn=1, kdeg=0.5, kint=0.2),
  ev = function(n) {
    e <- et(amt=50, cmt="depot", ii=24, addl=6)
    e <- et(e, seq(0, 168, by=0.5))
    et(e, id=1:max(n,1))
  })

mods$friberg <- list(
  out = "circ",
  m = rxode2({
    d/dt(depot)  <- -ka*depot
    d/dt(center) <-  ka*depot - (cl/v)*center
    conc <- center/v
    edrug <- 1 - slope*conc
    d/dt(prol)   <- ktr*prol*edrug*((circ0/circ)^gam) - ktr*prol
    d/dt(tr1)    <- ktr*prol - ktr*tr1
    d/dt(tr2)    <- ktr*tr1  - ktr*tr2
    d/dt(tr3)    <- ktr*tr2  - ktr*tr3
    d/dt(circ)   <- ktr*tr3  - ktr*circ
  }),
  p = c(ka=1, cl=1, v=20, slope=0.05, circ0=5, gam=0.16, ktr=0.05),
  inits = c(prol=5, tr1=5, tr2=5, tr3=5, circ=5),
  ev = function(n) {
    e <- et(amt=100, cmt="depot", ii=24, addl=6)
    e <- et(e, seq(0, 500, by=1))
    if (n > 1) e <- et(e, id=1:n)
    e
  })

mods$pbpk <- list(
  out = "cp",
  m = rxode2({
    d/dt(depot) <- -ka*depot
    cart <- art/vart
    d/dt(art) <- qlu*(lun/vlun/klu) - qlu*cart
    d/dt(ven) <- qli*(liv/vliv/kli) + qki*(kid/vkid/kki) + qmu*(mus/vmus/kmu) +
      qad*(adi/vadi/kad) + qbr*(bra/vbra/kbr) + qhe*(hrt/vhrt/khe) +
      qsk*(skn/vskn/ksk) + qre*(res/vres/kre) -
      (qli+qki+qmu+qad+qbr+qhe+qsk+qre)*(ven/vven) + ka*depot
    d/dt(lun) <- qlu*(ven/vven) - qlu*(lun/vlun/klu)
    d/dt(liv) <- qli*cart - qli*(liv/vliv/kli) - clint*(liv/vliv/kli)
    d/dt(kid) <- qki*cart - qki*(kid/vkid/kki) - clr*(kid/vkid/kki)
    d/dt(mus) <- qmu*cart - qmu*(mus/vmus/kmu)
    d/dt(adi) <- qad*cart - qad*(adi/vadi/kad)
    d/dt(bra) <- qbr*cart - qbr*(bra/vbra/kbr)
    d/dt(hrt) <- qhe*cart - qhe*(hrt/vhrt/khe)
    d/dt(skn) <- qsk*cart - qsk*(skn/vskn/ksk)
    d/dt(res) <- qre*cart - qre*(res/vres/kre)
    cp <- ven/vven
  }),
  p = c(ka=1,
        vart=1.7, vven=3.4, vlun=0.5, vliv=1.8, vkid=0.31, vmus=29, vadi=14.6,
        vbra=1.4, vhrt=0.33, vskn=3.3, vres=10,
        qlu=312, qli=25.5, qki=19.1, qmu=42, qad=13, qbr=42, qhe=15, qsk=18, qre=40,
        klu=1.2, kli=2.5, kki=2.0, kmu=1.0, kad=5.0, kbr=1.5, khe=1.3, ksk=1.1, kre=1.0,
        clint=10, clr=5),
  ev = function(n) {
    e <- et(amt=100, cmt="depot", ii=24, addl=6)
    e <- et(e, seq(0, 168, by=0.5))
    if (n > 1) e <- et(e, id=1:n)
    e
  })

.solve <- function(mod, opt, n) {
  args <- c(list(object=mod$m, params=mod$p, events=mod$ev(n)), opt)
  if (!is.null(mod$inits)) args$inits <- mod$inits
  do.call(rxSolve, args)
}

