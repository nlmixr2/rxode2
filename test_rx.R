library(rxode2)
model <- rxode2({
  KA <- 1
  CL <- 1
  V2 <- 1
  eff(0) <- 1
  d/dt(eff) <- Kin * linCmt() - Kout * eff
})
cat(rxSensMatExp(model$model["normModel"], calcSens=c("KA")))
