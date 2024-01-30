modelCode <- "
TVBASE=14.3;
BASE=TVBASE*exp(EBASE);
SLD(0)=BASE;
d/dt(SLD)=-0.5*SLD;
"
m1 <- rxode2(modelCode)
ev <- eventTable() %>% add.sampling(c(0, 1))
e <- rxSolve(m1, ev, params = c(EBASE = 0.3))
test_that("Make sure initial conditions work, Issue RxODE#67: baseline works", {
  expect_equal(c(19.3029809483368, 19.3029809483368), e$BASE, tolerance = 1e-6)
})
