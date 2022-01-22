## https://cran.r-project.org/web/packages/diffEq/vignettes/ODEinR.pdf p15

Vtpol <- rxode2("
d/dt(y) = dy
d/dt(dy) = mu*(1-y^2)*dy - y
## Initial conditions
y(0) = 2
dy(0) = 0
## mu
mu = 1 ## nonstiff; 10 moderately stiff; 1000 stiff
")


Vtpol2 <- rxode2("
d/dt(y)  = dy
d/dt(dy) = mu*(1-y^2)*dy - y
## Jacobian
df(y)/dy(dy)  = 1
df(dy)/dy(y)  = -2*dy*mu*y - 1
df(dy)/dy(dy) = mu*(1-y^2)
## Initial conditions
y(0) = 2
dy(0) = 0
## mu
mu = 1 ## nonstiff; 10 moderately stiff; 1000 stiff
")

et <- eventTable()
et$add.sampling(seq(0, 30, by = 0.01))

stiff <- solve(Vtpol, et)

## plot(stiff$time,stiff$y,type="l")
counts <- data.frame(stiff$counts, mu = 1, digest = digest::digest(round(as.data.frame(stiff), 3)))

for (i in 10^(1:7)) {
  stiff$mu <- i
  ## plot(stiff$time,stiff$y,type="l");
  ## title(sprintf("mu=%s",i));
  counts <- rbind(counts, data.frame(stiff$counts, mu = i, digest = digest::digest(round(as.data.frame(stiff), 3))))
}

test_that("No User jacobian are called.", {
  expect_true(all(counts$jac == 0))
})

## Data sets match

## Changes based on platform.

## test_that("Solving of stiff systems by automatic jacobians is expected",{
##     expect_equal(digest(counts),"982ce8816e9e1f5db6ee315c71b6a7dc")
## })

counts0 <- counts

stiff <- solve(Vtpol2, et)
## plot(stiff$time,stiff$y,type="l")
counts <- data.frame(stiff$counts, mu = 1, digest = digest::digest(round(as.data.frame(stiff), 3)))

for (i in 10^(1:7)) {
  stiff$mu <- i
  ## plot(stiff$time,stiff$y,type="l");
  ## title(sprintf("mu=%s",i));
  counts <- rbind(counts, data.frame(stiff$counts, mu = i, digest = digest::digest(round(as.data.frame(stiff), 3))))
}

## print(counts)

## changes based on platform.

## test_that("Some User jacobian are called.",{
##     expect_true(!all(counts$user_jac == 0))
## })

test_that("Same solutions", {
  expect_true(all(counts0$digest == counts$digest))
})
