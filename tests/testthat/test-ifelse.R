rx1 <- rxode2({
  b <- -1
  d / dt(X) <- a * X + Y * Z
  d / dt(Y) <- b * (Y - Z)
  d / dt(Z) <- -X * Y + c * Y - Z
  if (t < 0.02 | t > 99.98) {
    print("matt")
  } else {
    print("f")
  }
})

test_that("Simple if/else expansion works on model", {
  expect_equal(rxExpandIfElse(rx1, TRUE, FALSE), c("(!(t<0.02||t>99.98))" = "d/dt(X)=a*X+Y*Z;\nd/dt(Y)=b*(Y-Z);\nd/dt(Z)=-X*Y+c*Y-Z;\nprintf(\"f\");", "(t<0.02||t>99.98)" = "d/dt(X)=a*X+Y*Z;\nd/dt(Y)=b*(Y-Z);\nd/dt(Z)=-X*Y+c*Y-Z;\nprintf(\"matt\");"))
})

test_that("Keep initilizations if needed.", {
  expect_equal(
    rxExpandIfElse(rx1, FALSE, FALSE),
    c("(!(t<0.02||t>99.98))" = "b=-1;\nd/dt(X)=a*X+Y*Z;\nd/dt(Y)=b*(Y-Z);\nd/dt(Z)=-X*Y+c*Y-Z;\nprintf(\"f\");", "(t<0.02||t>99.98)" = "b=-1;\nd/dt(X)=a*X+Y*Z;\nd/dt(Y)=b*(Y-Z);\nd/dt(Z)=-X*Y+c*Y-Z;\nprintf(\"matt\");")
  )
})

rx2 <- rxode2({
  b <- -1
  d / dt(X) <- a * X + Y * Z
  d / dt(Y) <- b * (Y - Z)
  d / dt(Z) <- -X * Y + c * Y - Z
  if (t < 0.02 | t > 99.98) {
    print("me")
  }
})

test_that("Simple if expansion works on model", {
  expect_equal(rxExpandIfElse(rx2, TRUE, FALSE), c("(!(t<0.02||t>99.98))" = "d/dt(X)=a*X+Y*Z;\nd/dt(Y)=b*(Y-Z);\nd/dt(Z)=-X*Y+c*Y-Z;", "(t<0.02||t>99.98)" = "d/dt(X)=a*X+Y*Z;\nd/dt(Y)=b*(Y-Z);\nd/dt(Z)=-X*Y+c*Y-Z;\nprintf(\"me\");"))
})

test_that("Keep initilizations if needed.", {
  expect_equal(
    rxExpandIfElse(rx2, FALSE, FALSE),
    c("(!(t<0.02||t>99.98))" = "b=-1;\nd/dt(X)=a*X+Y*Z;\nd/dt(Y)=b*(Y-Z);\nd/dt(Z)=-X*Y+c*Y-Z;", "(t<0.02||t>99.98)" = "b=-1;\nd/dt(X)=a*X+Y*Z;\nd/dt(Y)=b*(Y-Z);\nd/dt(Z)=-X*Y+c*Y-Z;\nprintf(\"me\");")
  )
})

## Compound if/then
rx3 <- rxode2({
  b <- -1
  d / dt(X) <- a * X + Y * Z
  d / dt(Y) <- b * (Y - Z)
  d / dt(Z) <- -X * Y + c * Y - Z
  if (t < 0.02 | t > 99.98) {
    print("1")
    if (t > 100) {
      print("2")
    } else {
      print("3")
    }
  }
})

test_that("nested if/then", {
  expect_equal(
    rxExpandIfElse(rx3, TRUE, FALSE),
    c("(!(t<0.02||t>99.98))" = "d/dt(X)=a*X+Y*Z;\nd/dt(Y)=b*(Y-Z);\nd/dt(Z)=-X*Y+c*Y-Z;", "(t<0.02||t>99.98) && (!(t>100))" = "d/dt(X)=a*X+Y*Z;\nd/dt(Y)=b*(Y-Z);\nd/dt(Z)=-X*Y+c*Y-Z;\nprintf(\"1\");\nprintf(\"3\");", "(t<0.02||t>99.98) && (t>100)" = "d/dt(X)=a*X+Y*Z;\nd/dt(Y)=b*(Y-Z);\nd/dt(Z)=-X*Y+c*Y-Z;\nprintf(\"1\");\nprintf(\"2\");")
  )
})

test_that("Keep initilizations", {
  expect_equal(
    rxExpandIfElse(rx3, FALSE, FALSE),
    c("(!(t<0.02||t>99.98))" = "b=-1;\nd/dt(X)=a*X+Y*Z;\nd/dt(Y)=b*(Y-Z);\nd/dt(Z)=-X*Y+c*Y-Z;", "(t<0.02||t>99.98) && (!(t>100))" = "b=-1;\nd/dt(X)=a*X+Y*Z;\nd/dt(Y)=b*(Y-Z);\nd/dt(Z)=-X*Y+c*Y-Z;\nprintf(\"1\");\nprintf(\"3\");", "(t<0.02||t>99.98) && (t>100)" = "b=-1;\nd/dt(X)=a*X+Y*Z;\nd/dt(Y)=b*(Y-Z);\nd/dt(Z)=-X*Y+c*Y-Z;\nprintf(\"1\");\nprintf(\"2\");")
  )
})

rx4 <- rxode2({
  b <- -1
  d / dt(X) <- a * X + Y * Z
  d / dt(Y) <- b * (Y - Z)
  d / dt(Z) <- -X * Y + c * Y - Z
  if (t < 0.02 | t > 99.98) {
    print("1")
    if (t > 100) {
      print("2")
    }
  }
})

test_that("nested if/then #2", {
  expect_equal(
    rxExpandIfElse(rx4, TRUE, FALSE),
    c("(!(t<0.02||t>99.98))" = "d/dt(X)=a*X+Y*Z;\nd/dt(Y)=b*(Y-Z);\nd/dt(Z)=-X*Y+c*Y-Z;", "(t<0.02||t>99.98) && (!(t>100))" = "d/dt(X)=a*X+Y*Z;\nd/dt(Y)=b*(Y-Z);\nd/dt(Z)=-X*Y+c*Y-Z;\nprintf(\"1\");", "(t<0.02||t>99.98) && (t>100)" = "d/dt(X)=a*X+Y*Z;\nd/dt(Y)=b*(Y-Z);\nd/dt(Z)=-X*Y+c*Y-Z;\nprintf(\"1\");\nprintf(\"2\");")
  )
})

test_that("nested if/then #2 w/ini", {
  expect_equal(
    rxExpandIfElse(rx4, FALSE, FALSE),
    c("(!(t<0.02||t>99.98))" = "b=-1;\nd/dt(X)=a*X+Y*Z;\nd/dt(Y)=b*(Y-Z);\nd/dt(Z)=-X*Y+c*Y-Z;", "(t<0.02||t>99.98) && (!(t>100))" = "b=-1;\nd/dt(X)=a*X+Y*Z;\nd/dt(Y)=b*(Y-Z);\nd/dt(Z)=-X*Y+c*Y-Z;\nprintf(\"1\");", "(t<0.02||t>99.98) && (t>100)" = "b=-1;\nd/dt(X)=a*X+Y*Z;\nd/dt(Y)=b*(Y-Z);\nd/dt(Z)=-X*Y+c*Y-Z;\nprintf(\"1\");\nprintf(\"2\");")
  )
})

## Compound if/then
rx5 <- rxode2({
  b <- -1
  d / dt(X) <- a * X + Y * Z
  d / dt(Y) <- b * (Y - Z)
  d / dt(Z) <- -X * Y + c * Y - Z
  if (t < 0.02 | t > 99.98) {
    print("1")
    if (t > 100) {
      print("2")
    } else {
      print("3")
      if (t < 0.01) {
        printf("Less than <0.001\n")
      } else {
        printf("Between 0.01 and 0.02.\n")
      }
    }
  }
})

test_that("nested if/then #3", {
  expect_equal(
    rxExpandIfElse(rx5, TRUE, FALSE),
    c("(!(t<0.02||t>99.98))" = "d/dt(X)=a*X+Y*Z;\nd/dt(Y)=b*(Y-Z);\nd/dt(Z)=-X*Y+c*Y-Z;", "(t<0.02||t>99.98) && (t>100)" = "d/dt(X)=a*X+Y*Z;\nd/dt(Y)=b*(Y-Z);\nd/dt(Z)=-X*Y+c*Y-Z;\nprintf(\"1\");\nprintf(\"2\");", "(t<0.02||t>99.98) && (!(t>100)) && (!(t<0.01))" = "d/dt(X)=a*X+Y*Z;\nd/dt(Y)=b*(Y-Z);\nd/dt(Z)=-X*Y+c*Y-Z;\nprintf(\"1\");\nprintf(\"3\");\nprintf(\"Between 0.01 and 0.02.\\n\");", "(t<0.02||t>99.98) && (!(t>100)) && (t<0.01)" = "d/dt(X)=a*X+Y*Z;\nd/dt(Y)=b*(Y-Z);\nd/dt(Z)=-X*Y+c*Y-Z;\nprintf(\"1\");\nprintf(\"3\");\nprintf(\"Less than <0.001\\n\");")
  )
})

test_that("nested if/then #3 w/ini", {
  expect_equal(
    rxExpandIfElse(rx5, FALSE, FALSE),
    c("(!(t<0.02||t>99.98))" = "b=-1;\nd/dt(X)=a*X+Y*Z;\nd/dt(Y)=b*(Y-Z);\nd/dt(Z)=-X*Y+c*Y-Z;", "(t<0.02||t>99.98) && (t>100)" = "b=-1;\nd/dt(X)=a*X+Y*Z;\nd/dt(Y)=b*(Y-Z);\nd/dt(Z)=-X*Y+c*Y-Z;\nprintf(\"1\");\nprintf(\"2\");", "(t<0.02||t>99.98) && (!(t>100)) && (!(t<0.01))" = "b=-1;\nd/dt(X)=a*X+Y*Z;\nd/dt(Y)=b*(Y-Z);\nd/dt(Z)=-X*Y+c*Y-Z;\nprintf(\"1\");\nprintf(\"3\");\nprintf(\"Between 0.01 and 0.02.\\n\");", "(t<0.02||t>99.98) && (!(t>100)) && (t<0.01)" = "b=-1;\nd/dt(X)=a*X+Y*Z;\nd/dt(Y)=b*(Y-Z);\nd/dt(Z)=-X*Y+c*Y-Z;\nprintf(\"1\");\nprintf(\"3\");\nprintf(\"Less than <0.001\\n\");")
  )
})

rx6 <- rxode2({
  b <- -1
  d / dt(X) <- a * X + Y * Z
  d / dt(Y) <- b * (Y - Z)
  d / dt(Z) <- -X * Y + c * Y - Z
  if (t < 0.02 | t > 99.98) {
    print("1")
    if (t > 100) {
      print("2")
      if (t < 150) {
        printf("Less than 150\n")
      } else {
        printf(">= 150\n")
      }
    } else {
      print("3")
    }
  }
})

test_that("nested if/then #4", {
  expect_equal(
    rxExpandIfElse(rx6, TRUE, FALSE),
    c("(!(t<0.02||t>99.98))" = "d/dt(X)=a*X+Y*Z;\nd/dt(Y)=b*(Y-Z);\nd/dt(Z)=-X*Y+c*Y-Z;", "(t<0.02||t>99.98) && (!(t>100))" = "d/dt(X)=a*X+Y*Z;\nd/dt(Y)=b*(Y-Z);\nd/dt(Z)=-X*Y+c*Y-Z;\nprintf(\"1\");\nprintf(\"3\");", "(t<0.02||t>99.98) && (t>100) && (!(t<150))" = "d/dt(X)=a*X+Y*Z;\nd/dt(Y)=b*(Y-Z);\nd/dt(Z)=-X*Y+c*Y-Z;\nprintf(\"1\");\nprintf(\"2\");\nprintf(\">= 150\\n\");", "(t<0.02||t>99.98) && (t>100) && (t<150)" = "d/dt(X)=a*X+Y*Z;\nd/dt(Y)=b*(Y-Z);\nd/dt(Z)=-X*Y+c*Y-Z;\nprintf(\"1\");\nprintf(\"2\");\nprintf(\"Less than 150\\n\");")
  )
})

test_that("nested if/then #4 w/ini", {
  expect_equal(
    rxExpandIfElse(rx6, FALSE, FALSE),
    c("(!(t<0.02||t>99.98))" = "b=-1;\nd/dt(X)=a*X+Y*Z;\nd/dt(Y)=b*(Y-Z);\nd/dt(Z)=-X*Y+c*Y-Z;", "(t<0.02||t>99.98) && (!(t>100))" = "b=-1;\nd/dt(X)=a*X+Y*Z;\nd/dt(Y)=b*(Y-Z);\nd/dt(Z)=-X*Y+c*Y-Z;\nprintf(\"1\");\nprintf(\"3\");", "(t<0.02||t>99.98) && (t>100) && (!(t<150))" = "b=-1;\nd/dt(X)=a*X+Y*Z;\nd/dt(Y)=b*(Y-Z);\nd/dt(Z)=-X*Y+c*Y-Z;\nprintf(\"1\");\nprintf(\"2\");\nprintf(\">= 150\\n\");", "(t<0.02||t>99.98) && (t>100) && (t<150)" = "b=-1;\nd/dt(X)=a*X+Y*Z;\nd/dt(Y)=b*(Y-Z);\nd/dt(Z)=-X*Y+c*Y-Z;\nprintf(\"1\");\nprintf(\"2\");\nprintf(\"Less than 150\\n\");")
  )
})

ode <- "
   C2 = centr/V2;
   C3 = peri/V3;
   d/dt(depot) =-KA*depot;
   d/dt(centr) = KA*depot - CL*C2 - Q*C2 + Q*C3;
   d/dt(peri)  =                    Q*C2 - Q*C3;
   d/dt(eff)  = Kin - Kout*(1-C2/(EC50+C2))*eff;
"

ode1 <- rxode2(ode)
ode2 <- suppressMessages(rxode2(ode, calcSens = TRUE, collapseModel = TRUE))

ode3 <- rxode2({
  if (route == 1) {
    C2 <- centr / V2 * F
  } else {
    C2 <- centr / V2
  }
  C3 <- peri / V3
  d / dt(depot) <- -KA * depot
  d / dt(centr) <- KA * depot - CL * C2 - Q * C2 + Q * C3
  d / dt(peri) <- Q * C2 - Q * C3
  d / dt(eff) <- Kin - Kout * (1 - C2 / (EC50 + C2)) * eff
})

ode4 <- suppressMessages(rxode2(ode3, calcSens = TRUE, collapseModel = TRUE))

ode5 <- suppressMessages(rxode2(ode3, calcSens = TRUE))

## test_that("LHS variables can be removed", {
##     expect_true(length(rxLhs(ode3)) > 1);
##     expect_true(length(rxLhs(ode4)) == 0);
##     expect_equal(rxLhs(ode3), rxLhs(ode5))
## })

## pred <- function(){
##     if (cmt == 1){
##         return(cntr);
##     } else {
##         return(eff);
##     }
## }

## err <- function(){
##     if (cmt == 1){
##         return(add(0.1) + prop(0.1))
##     } else {
##         return(add(0.1))
##     }
## }

## pk <- function(){
##     KA <- theta[1];
##     CL <- exp(theta[2] + eta[1]);
##     V2 <- exp(theta[3] + eta[2]);
## }

## test_that("Can throw the warning", {
##     expect_warning(rxSymPySetupPred(ode3, pred, pk, err));
## })

## pred <- function(){
##     if (cmt == 1){
##         return(centr);
##     } else {
##         return(eff);
##     }
## }

## tmp <- rxSymPySetupPred(ode3, pred, pk, err)
