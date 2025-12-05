# Combine Error Lines and create rxode2 expression

Combine Error Lines and create rxode2 expression

## Usage

``` r
rxCombineErrorLines(
  uiModel,
  errLines = NULL,
  prefixLines = NULL,
  paramsLine = NULL,
  modelVars = FALSE,
  cmtLines = TRUE,
  dvidLine = TRUE,
  lstExpr = NULL,
  useIf = TRUE,
  interpLines = NULL,
  levelLines = NULL
)
```

## Arguments

- uiModel:

  UI model

- errLines:

  Error lines; If missing, get the error lines from
  [`rxGetDistributionSimulationLines()`](https://nlmixr2.github.io/rxode2/reference/rxGetDistributionSimulationLines.md)

- prefixLines:

  Prefix lines, after param statement

- paramsLine:

  Params line, if not present.

- modelVars:

  Return model vars instead of rxode2 statement

- cmtLines:

  Include trailing `cmt` lines

- dvidLine:

  Include trailing `dvid()` specification

- lstExpr:

  A list of expressions for model, or NULL. When NULL defaults to the
  model expressions accessible by `uiModel$lstExpr`.

- useIf:

  Use an `if (CMT == X)` for endpoints

- interpLines:

  Interpolation lines, if not present

- levelLines:

  Levels lines for assigned strings. If not present, use the
  interpolation lines from the current model.

## Value

quoted expression that can be evaluated to compiled rxode2 model

## Details

This is exported to allow other functions to mangle the error lines to
make other types of estimation methods (if needed)

## Author

Matthew L. Fidler

## Examples

``` r
# \donttest{

one.cmt <- function() {
  ini({
    ## You may label each parameter with a comment
    tka <- 0.45 # Log Ka
    tcl <- log(c(0, 2.7, 100)) # Log Cl
    ## This works with interactive models
    ## You may also label the preceding line with label("label text")
    tv <- 3.45; label("log V")
    ## the label("Label name") works with all models
    eta.ka ~ 0.6
    eta.cl ~ 0.3
    eta.v ~ 0.1
    add.sd <- 0.7
  })
  model({
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl)
    v <- exp(tv + eta.v)
    linCmt() ~ add(add.sd)
  })
}

f <- rxode2(one.cmt)
#>  
#>  
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments

# You can get the simulation model easily by
rxCombineErrorLines(f)
#> rxode2({
#>     params(tka, tcl, tv, add.sd, eta.ka, eta.cl, eta.v)
#>     ka <- exp(tka + eta.ka)
#>     cl <- exp(tcl + eta.cl)
#>     v <- exp(tv + eta.v)
#>     rx_yj_ ~ 2
#>     rx_lambda_ ~ 1
#>     rx_low_ ~ 0
#>     rx_hi_ ~ 1
#>     rx_pred_f_ ~ linCmt()
#>     rx_pred_ ~ rx_pred_f_
#>     rx_r_ ~ (add.sd)^2
#>     ipredSim <- rxTBSi(rx_pred_, rx_lambda_, rx_yj_, rx_low_, 
#>         rx_hi_)
#>     sim <- rxTBSi(rx_pred_ + sqrt(rx_r_) * rxerr.rxLinCmt, rx_lambda_, 
#>         rx_yj_, rx_low_, rx_hi_)
#>     cmt(rxLinCmt)
#>     dvid(3)
#> })

# You can then get the compiled model by simply evaluting the model:
r <- eval(rxCombineErrorLines(f))
#>  
#>  

# This also works with multile endpoint models:
pk.turnover.emax <- function() {
  ini({
    tktr <- log(1)
    tka <- log(1)
    tcl <- log(0.1)
    tv <- log(10)
    ##
    eta.ktr ~ 1
    eta.ka ~ 1
    eta.cl ~ 2
    eta.v ~ 1
    prop.err <- 0.1
    pkadd.err <- 0.1
    ##
    temax <- logit(0.8)
    tec50 <- log(0.5)
    tkout <- log(0.05)
    te0 <- log(100)
    ##
    eta.emax ~ .5
    eta.ec50  ~ .5
    eta.kout ~ .5
    eta.e0 ~ .5
    ##
    pdadd.err <- 10
  })
  model({
    ktr <- exp(tktr + eta.ktr)
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl)
    v <- exp(tv + eta.v)
    ##
    emax=expit(temax+eta.emax)
    ec50 =  exp(tec50 + eta.ec50)
    kout = exp(tkout + eta.kout)
    e0 = exp(te0 + eta.e0)
    ##
    DCP = center/v
    PD=1-emax*DCP/(ec50+DCP)
    ##
    effect(0) = e0
    kin = e0*kout
    ##
    d/dt(depot) = -ktr * depot
    d/dt(gut) =  ktr * depot -ka * gut
    d/dt(center) =  ka * gut - cl / v * center
    d/dt(effect) = kin*PD -kout*effect
    ##
    cp = center / v
    cp ~ prop(prop.err) + add(pkadd.err)
    effect ~ add(pdadd.err)
  })
}

f <- rxode2(pk.turnover.emax)
#>  
#>  
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
rxCombineErrorLines(f)
#> rxode2({
#>     params(tktr, tka, tcl, tv, prop.err, pkadd.err, temax, tec50, 
#>         tkout, te0, pdadd.err, eta.ktr, eta.ka, eta.cl, eta.v, 
#>         eta.emax, eta.ec50, eta.kout, eta.e0)
#>     ktr <- exp(tktr + eta.ktr)
#>     ka <- exp(tka + eta.ka)
#>     cl <- exp(tcl + eta.cl)
#>     v <- exp(tv + eta.v)
#>     emax = expit(temax + eta.emax, 0, 1)
#>     ec50 = exp(tec50 + eta.ec50)
#>     kout = exp(tkout + eta.kout)
#>     e0 = exp(te0 + eta.e0)
#>     DCP = center/v
#>     PD = 1 - emax * DCP/(ec50 + DCP)
#>     effect(0) = e0
#>     kin = e0 * kout
#>     d/dt(depot) = -ktr * depot
#>     d/dt(gut) = ktr * depot - ka * gut
#>     d/dt(center) = ka * gut - cl/v * center
#>     d/dt(effect) = kin * PD - kout * effect
#>     cp = center/v
#>     if (CMT == 5) {
#>         rx_yj_ ~ 2
#>         rx_lambda_ ~ 1
#>         rx_low_ ~ 0
#>         rx_hi_ ~ 1
#>         rx_pred_f_ ~ cp
#>         rx_pred_ ~ rx_pred_f_
#>         rx_r_ ~ (pkadd.err)^2 + (rx_pred_f_)^2 * (prop.err)^2
#>         ipredSim <- rxTBSi(rx_pred_, rx_lambda_, rx_yj_, rx_low_, 
#>             rx_hi_)
#>         sim <- rxTBSi(rx_pred_ + sqrt(rx_r_) * rxerr.cp, rx_lambda_, 
#>             rx_yj_, rx_low_, rx_hi_)
#>     }
#>     if (CMT == 4) {
#>         rx_yj_ ~ 2
#>         rx_lambda_ ~ 1
#>         rx_low_ ~ 0
#>         rx_hi_ ~ 1
#>         rx_pred_f_ ~ effect
#>         rx_pred_ ~ rx_pred_f_
#>         rx_r_ ~ (pdadd.err)^2
#>         ipredSim <- rxTBSi(rx_pred_, rx_lambda_, rx_yj_, rx_low_, 
#>             rx_hi_)
#>         sim <- rxTBSi(rx_pred_ + sqrt(rx_r_) * rxerr.effect, 
#>             rx_lambda_, rx_yj_, rx_low_, rx_hi_)
#>     }
#>     cmt(cp)
#>     dvid(5, 4)
#> })

# Note that in the parsed form, you can also get the compiled rxode2
# model with $simulationModel

f$simulationModel
#>  
#>  
#> rxode2 5.0.0 model named rx_1394352e4154b1f387a0cde6ff02d6ef model (✔ ready). 
#> value$state: depot, gut, center, effect
#> value$stateExtra: cp
#> value$params: tktr, tka, tcl, tv, prop.err, pkadd.err, temax, tec50, tkout, te0, pdadd.err, eta.ktr, eta.ka, eta.cl, eta.v, eta.emax, eta.ec50, eta.kout, eta.e0, CMT, rxerr.cp, rxerr.effect
#> value$lhs: ktr, ka, cl, v, emax, ec50, kout, e0, DCP, PD, kin, cp, ipredSim, sim

# }
```
