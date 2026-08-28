## Corpus for the symengine translation fixture -- DATA ONLY.
##
## Sourced by inst/tools/genSymengineFixture.R, which owns the harness (how
## expressions are harvested and how the oracle is captured).  Kept separate
## because growing the corpus and changing the capture procedure are different
## jobs: adding a model here must never mean re-reading the subprocess capture
## logic, and vice versa.
##
## .models  -- rxUi model functions, exercised through rxNorm(),
##             ui$symengineModelPrune and a real .rxJacobian()/.rxSens() run
## .exprs   -- rxode2-syntax expressions for the rxToSE direction
## .seRaw   -- raw symengine-syntax strings pinned straight at the rxFromSE
##             emitter, for branches a model round trip does not produce

## ---------------------------------------------------------------- corpus ----
## Models chosen to cover every emitter branch: ODEs, algebraic lhs, Emax,
## transit, linCmt, the llik family, the error-model transforms, lag()/diff(),
## delay() and the special-function table.
.models <- list(
  oral1 = function() {
    ini({tka <- 0.45; tcl <- 1; tv <- 3.45; eta.ka ~ 0.6; add.sd <- 0.7})
    model({
      ka <- exp(tka + eta.ka); cl <- exp(tcl); v <- exp(tv)
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - cl / v * center
      cp <- center / v
      cp ~ add(add.sd)
    })
  },
  pkpd2 = function() {
    ini({lka <- 0.45; lcl <- 1; lvc <- 3.45; lq <- 0.5; lvp <- 3
         lemax <- 1; lec50 <- 0.5; lkin <- 0.1; lkout <- 0.1
         eta.ka ~ 0.6; eta.cl ~ 0.3; prop.sd <- 0.1; pd.sd <- 0.3})
    model({
      ka <- exp(lka + eta.ka); cl <- exp(lcl + eta.cl); vc <- exp(lvc)
      q <- exp(lq); vp <- exp(lvp)
      emax <- exp(lemax); ec50 <- exp(lec50); kin <- exp(lkin); kout <- exp(lkout)
      d/dt(depot) <- -ka * depot
      d/dt(cent) <- ka * depot - cl / vc * cent - q / vc * cent + q / vp * per1
      d/dt(per1) <- q / vc * cent - q / vp * per1
      cp <- cent / vc
      d/dt(eff) <- kin * (1 + emax * cp / (ec50 + cp)) - kout * eff
      cp ~ prop(prop.sd)
      eff ~ add(pd.sd)
    })
  },
  lognormal = function() {
    ini({tcl <- 1; tv <- 3.45; eta.cl ~ 0.3; add.sd <- 0.7})
    model({
      cl <- exp(tcl + eta.cl); v <- exp(tv)
      d/dt(center) <- -cl / v * center
      cp <- center / v
      cp ~ lnorm(add.sd)
    })
  },
  logitnorm = function() {
    ini({tcl <- 1; tv <- 3.45; eta.cl ~ 0.3; add.sd <- 0.7})
    model({
      cl <- exp(tcl + eta.cl); v <- exp(tv)
      d/dt(center) <- -cl / v * center
      cp <- center / v
      cp ~ logitNorm(add.sd, 0, 10)
    })
  },
  boxcox = function() {
    ini({tcl <- 1; tv <- 3.45; eta.cl ~ 0.3; add.sd <- 0.7; lmbd <- 0.5})
    model({
      cl <- exp(tcl + eta.cl); v <- exp(tv)
      d/dt(center) <- -cl / v * center
      cp <- center / v
      cp ~ add(add.sd) + boxCox(lmbd)
    })
  },
  transit1 = function() {
    ini({tktr <- 1; tka <- 0.45; tcl <- 1; tv <- 3.45; eta.cl ~ 0.3; add.sd <- 0.7})
    model({
      ktr <- exp(tktr); ka <- exp(tka); cl <- exp(tcl + eta.cl); v <- exp(tv)
      d/dt(depot) <- transit(3, ktr) * depot - ka * depot
      d/dt(center) <- ka * depot - cl / v * center
      cp <- center / v
      cp ~ add(add.sd)
    })
  },
  studentT = function() {
    ini({tcl <- 1; tv <- 3.45; eta.cl ~ 0.3; add.sd <- 0.7; nu <- 5})
    model({
      cl <- exp(tcl + eta.cl); v <- exp(tv)
      d/dt(center) <- -cl / v * center
      cp <- center / v
      cp ~ add(add.sd) + t(nu)
    })
  },
  addProp = function() {
    ini({tka <- 0.45; tcl <- 1; tv <- 3.45; eta.ka ~ 0.6; eta.cl ~ 0.3
         add.sd <- 0.7; prop.sd <- 0.1})
    model({
      ka <- exp(tka + eta.ka); cl <- exp(tcl + eta.cl); v <- exp(tv)
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - cl / v * center
      cp <- center / v
      cp ~ add(add.sd) + prop(prop.sd)
    })
  }
)

## Extra rxode2-syntax expressions covering the special-function table.  These
## go through the rxToSE direction and, once in symengine, back out again.
.exprs <- c(
  "a*b+c/d-e", "a^2", "a^3", "a^0.5", "a^(-1)", "a^b", "a^(-2)", "2^a",
  "exp(a)", "log(a)", "log(1+a)", "log1p(a)", "sqrt(a)", "abs(a)",
  "sin(a)", "cos(a)", "tan(a)", "asin(a)", "acos(a)", "atan(a)", "atan2(a,b)",
  "sinh(a)", "cosh(a)", "tanh(a)", "expit(a)", "logit(a)", "probitInv(a)",
  "gamma(a)", "lgamma(a)", "digamma(a)", "trigamma(a)", "psigamma(a,b)",
  "beta(a,b)", "lbeta(a,b)", "choose(a,b)", "lchoose(a,b)", "factorial(a)",
  "erf(a)", "erfc(a)", "pnorm(a)", "pnorm(a,b)", "pnorm(a,b,c)",
  "max(a,b)", "min(a,b)", "floor(a)", "ceiling(a)", "round(a)", "trunc(a)",
  "sign(a)", "a %% b", "log(a)/log(b)", "exp(a+b)*exp(a-b)",
  "1/a", "(-1)*a", "-(a+b)", "a*(b+c)*(d+e)", "((a))",
  "pi", "pi*2", "pi/2", "pi/4", "1/pi", "2/pi", "sqrt(pi)", "sqrt(2)",
  "log(2)", "log(10)", "1/log(2)", "1/log(10)", "sqrt(2*pi)",
  "a*pi", "exp(1)", "cos(pi*a)", "sin(pi*a)",
  "llikNorm(a,b,c)", "llikPois(a,b)", "llikBinom(a,b,c)",
  "llikBeta(a,b,c)", "llikT(a,b,c,d)",
  "rxTBS(a,b,c,d,e)", "rxTBSi(a,b,c,d,e)"
)

## Raw symengine-syntax strings pinned directly at the rxFromSE emitter: the
## branches that a model round-trip does not reliably produce.
.seRaw <- c(
  ## An unregistered function must still report that it is unsupported.  The
  ## name has to be one nothing else can define: `f` was used here first and
  ## picked up a four-argument `f` left in scope by another test file, turning
  ## the expected "not supported" into an arity complaint depending on which
  ## files ran.
  "Derivative(rxNoSuchFn(x), x)", "Derivative(rxNoSuchFn(x, y), x)",
  "Derivative(rxNoSuchFn(x, y), y)",
  "Subs(Derivative(rxNoSuchFn(x), x), x, y)",
  "THETA_1_ + ETA_1_", "THETA_10_*ETA_2_",
  "rx__d_dt_depot__", "rx__df_center_dy_cl__", "rx__sens_center_BY_cl__",
  "rx_f_depot_", "rx_lag_depot_", "rx_rate_depot_", "rx_dur_depot_",
  "depot_ini0", "rx_SymPy_Res_gamma",
  "E", "EulerGamma", "Catalan", "GoldenRatio",
  "2.718281828459045", "3.141592653589793", "1.4142135623730951",
  "x**2", "x**(-1)", "x**0.5", "x**(1/2)", "x**(-1/2)", "x**3",
  "1/2", "3/4", "-1/2", "0", "1", "-1",
  "abs0(x)", "rxNot(x)", "rxEq(x, y)", "rxNeq(x, y)",
  "rxAnd(x, y)", "rxOr(x, y)", "rxLt(x, y)", "rxGt(x, y)",
  ## the .SEdouble spellings, which come back as infix operators
  "rxGeq(x, y)", "rxLeq(x, y)", "rxMod(x, y)", "lbeta(x, y)",
  "Rx_pow(x, y)", "Rx_pow_di(x, 2)", "R_pow(x, y)", "R_pow_di(x, 2)",
  "rxEq(a + b, c*d)", "rxAnd(rxLt(a, b), rxGt(c, d))",
  ## polygamma's small orders have their own names, the rest fold to psigamma
  "polygamma(2, x)", "polygamma(3, x)", "polygamma(4, x)", "polygamma(9, x)",
  ## .rxFunctionMake() renders a zero-argument call as f(NaN) on the way into
  ## symengine, so these have to come back as f() -- but only for the
  ## dual variable/function names; exp(NaN) really is exp(NaN)
  "tlast(NaN)", "tlast0(NaN)", "tad(NaN)", "tad0(NaN)", "tafd(NaN)",
  "tafd0(NaN)", "tfirst(NaN)", "tfirst0(NaN)", "dose(NaN)", "podo(NaN)",
  "dose0(NaN)", "podo0(NaN)", "dosenum(NaN)", "dosenum0(NaN)",
  "exp(NaN)", "sqrt(NaN)",
  ## the dosing-history family: 0 or 1 argument
  "tlast()", "tfirst()", "dose()", "podo()",
  "tlast0()", "first0()", "dose0()", "podo0()",
  "tlast(a)", "tfirst(a)", "dose(a)", "podo(a)",
  "tlast0(a)", "first0(a)", "dose0(a)", "podo0(a)",
  ## pi is bound in baseenv(), so R folds it -- and only on the RIGHT operand
  "1/pi", "2/pi", "a*pi", "pi*a", "a/pi", "pi/a", "a+pi", "pi+a",
  "polygamma(0, x)", "polygamma(1, x)",
  "lag(x, 1)", "lag0(x, 1)", "diff(x, 1)", "delay(x, tau)",
  "linCmtA(a, b, c)", "max(x, y)", "min(x, y)",
  "erf(x)", "erfc(x)", "gamma(x)", "loggamma(x)", "zeta(x)",
  ## the constant peepholes in .rxFromSE()'s binary branch; test-dsl.R checks
  ## some of these and the C emitter silently dropped two whole lists at first
  "pi**(1/2)", "pi^(1/2)", "(pi)^(1/2)", "pi^0.5", "(pi)^0.5",
  "log(pi**(1/2))", "log(pi^(1/2))", "log(pi^0.5)",
  "2/sqrt(pi)", "1/sqrt(2*pi)", "log(2)/log(10)", "1/log(2)", "1/log(10)",
  "sqrt(3)", "sqrt(32)", "exp(1)", "log(sqrt(2*pi))", "log(sqrt(pi/2))",
  "x + y*z - w/v", "(x + y)*(z - w)", "exp(x)*log(y)/sqrt(z)"
)

