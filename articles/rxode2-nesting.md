# Nesting levels in rxode2

## Nesting in rxode2

More than one level of nesting is possible in rxode2; In this example we
will be using the following uncertainties and sources of variability:

|                     Level | Variable           | Matrix specified | Integrated Matrix |
|--------------------------:|--------------------|------------------|-------------------|
|         Model uncertainty | NA                 | `thetaMat`       | `thetaMat`        |
|              Investigator | `inv.Cl`, `inv.Ka` | `omega`          | `theta`           |
|                   Subject | `eta.Cl`, `eta.Ka` | `omega`          | `omega`           |
|                       Eye | `eye.Cl`, `eye.Ka` | `omega`          | `omega`           |
|                  Occasion | `iov.Cl`, `occ.Ka` | `omega`          | `omega`           |
| Unexplained Concentration | `prop.sd`          | `sigma`          | `sigma`           |
|        Unexplained Effect | `add.sd`           | `sigma`          | `sigma`           |

### Event table

This event table contains nesting variables:

- inv: investigator id
- id: subject id
- eye: eye id (left or right)
- occ: occasion

``` r
library(rxode2)
```

    #> rxode2 5.0.1 using 2 threads (see ?getRxThreads)
    #>   no cache: create with `rxCreateCache()`

``` r
library(dplyr)
```

    #> 
    #> Attaching package: 'dplyr'

    #> The following objects are masked from 'package:stats':
    #> 
    #>     filter, lag

    #> The following objects are masked from 'package:base':
    #> 
    #>     intersect, setdiff, setequal, union

``` r
et(amountUnits="mg", timeUnits="hours") %>%
  et(amt=10000, addl=9,ii=12,cmt="depot") %>%
  et(time=120, amt=2000, addl=4, ii=14, cmt="depot") %>%
  et(seq(0, 240, by=4)) %>% # Assumes sampling when there is no dosing information
  et(seq(0, 240, by=4) + 0.1) %>% ## adds 0.1 for separate eye
  et(id=1:20) %>%
  ## Add an occasion per dose
  mutate(occ=cumsum(!is.na(amt))) %>%
  mutate(occ=ifelse(occ == 0, 1, occ)) %>%
  mutate(occ=2- occ %% 2) %>%
  mutate(eye=ifelse(round(time) == time, 1, 2)) %>%
  mutate(inv=ifelse(id < 10, 1, 2)) %>% as_tibble ->
  ev
```

### rxode2 model

This creates the `rxode2` model with multi-level nesting. Note the
variables `inv.Cl`, `inv.Ka`, `eta.Cl` etc; You only need one variable
for each level of nesting.

``` r
mod <- rxode2({
  ## Clearance with individuals
  eff(0) = 1
  C2 = centr/V2*(1+prop.sd)
  C3 = peri/V3
  CL =  TCl*exp(eta.Cl + eye.Cl + iov.Cl + inv.Cl)
  KA = TKA * exp(eta.Ka + eye.Ka + iov.Cl + inv.Ka)
  d/dt(depot) =-KA*depot
  d/dt(centr) = KA*depot - CL*C2 - Q*C2 + Q*C3
  d/dt(peri)  =                    Q*C2 - Q*C3
  d/dt(eff)  = Kin - Kout*(1-C2/(EC50+C2))*eff
  ef0 = eff + add.sd
})
```

### Uncertainty in Model parameters

``` r
theta <- c("TKA"=0.294, "TCl"=18.6, "V2"=40.2,
           "Q"=10.5, "V3"=297, "Kin"=1, "Kout"=1, "EC50"=200)

## Creating covariance matrix
tmp <- matrix(rnorm(8^2), 8, 8)
tMat <- tcrossprod(tmp, tmp) / (8 ^ 2)
dimnames(tMat) <- list(names(theta), names(theta))

tMat
```

    #>               TKA           TCl           V2            Q           V3
    #> TKA   0.173571236 -0.1003204607  0.038185010 -0.004108928 -0.095032973
    #> TCl  -0.100320461  0.2195710868 -0.043849095  0.013295549 -0.007477895
    #> V2    0.038185010 -0.0438490948  0.129784612 -0.017270432 -0.038004762
    #> Q    -0.004108928  0.0132955493 -0.017270432  0.022145634  0.020376451
    #> V3   -0.095032973 -0.0074778948 -0.038004762  0.020376451  0.165568340
    #> Kin  -0.040867119 -0.0492597458 -0.003056722 -0.033468634 -0.003021883
    #> Kout  0.035469225  0.0275087955  0.033725901  0.027668205  0.005497301
    #> EC50  0.026158042  0.0009434711  0.039426946 -0.036283167 -0.093134292
    #>               Kin         Kout          EC50
    #> TKA  -0.040867119  0.035469225  0.0261580416
    #> TCl  -0.049259746  0.027508796  0.0009434711
    #> V2   -0.003056722  0.033725901  0.0394269465
    #> Q    -0.033468634  0.027668205 -0.0362831667
    #> V3   -0.003021883  0.005497301 -0.0931342922
    #> Kin   0.226735493 -0.083447793  0.0659884544
    #> Kout -0.083447793  0.117195570 -0.0291684598
    #> EC50  0.065988454 -0.029168460  0.0928611407

### Nesting Variability

To specify multiple levels of nesting, you can specify it as a nested
`lotri` matrix; When using this approach you use the condition operator
`|` to specify what variable nesting occurs on; For the Bayesian
simulation we need to specify how much information we have for each
parameter; For `rxode2` this is the `nu` parameter.

In this case: - id, `nu=100` or the model came from 100 subjects - eye,
`nu=200` or the model came from 200 eyes - occ, `nu=200` or the model
came from 200 occasions - inv, `nu=10` or the model came from 10
investigators

To specify this in `lotri` you can use `| var(nu=X)`, or:

``` r
omega <- lotri(lotri(eta.Cl ~ 0.1,
                     eta.Ka ~ 0.1) | id(nu=100),
               lotri(eye.Cl ~ 0.05,
                     eye.Ka ~ 0.05) | eye(nu=200),
               lotri(iov.Cl ~ 0.01,
                     iov.Ka ~ 0.01) | occ(nu=200),
               lotri(inv.Cl ~ 0.02,
                     inv.Ka ~ 0.02) | inv(nu=10))
omega
```

    #> $id
    #>        eta.Cl eta.Ka
    #> eta.Cl    0.1    0.0
    #> eta.Ka    0.0    0.1
    #> 
    #> $eye
    #>        eye.Cl eye.Ka
    #> eye.Cl   0.05   0.00
    #> eye.Ka   0.00   0.05
    #> 
    #> $occ
    #>        iov.Cl iov.Ka
    #> iov.Cl   0.01   0.00
    #> iov.Ka   0.00   0.01
    #> 
    #> $inv
    #>        inv.Cl inv.Ka
    #> inv.Cl   0.02   0.00
    #> inv.Ka   0.00   0.02
    #> 
    #> Properties: nu

### Unexplained variability

The last piece of variability to specify is the unexplained variability

``` r
sigma <- lotri(prop.sd ~ .25,
               add.sd~ 0.125)
```

### Solving the problem

``` r
s <- rxSolve(mod, theta, ev,
             thetaMat=tMat, omega=omega,
             sigma=sigma, sigmaDf=400,
             nStud=400)
```

    #> unhandled error message: EE:[lsoda] 70000 steps taken before reaching tout
    #>  @(lsoda.c:748

    #> Warning: some ID(s) could not solve the ODEs correctly; These values are
    #> replaced with 'NA'

``` r
print(s)
```

    #> -- Solved rxode2 object --
    #> -- Parameters ($params): --
    #> # A tibble: 8,000 x 24
    #>    sim.id id    `inv.Cl(inv==1)` `inv.Cl(inv==2)` `inv.Ka(inv==1)`
    #>     <int> <fct>            <dbl>            <dbl>            <dbl>
    #>  1      1 1              0.00900            0.312           -0.117
    #>  2      1 2              0.00900            0.312           -0.117
    #>  3      1 3              0.00900            0.312           -0.117
    #>  4      1 4              0.00900            0.312           -0.117
    #>  5      1 5              0.00900            0.312           -0.117
    #>  6      1 6              0.00900            0.312           -0.117
    #>  7      1 7              0.00900            0.312           -0.117
    #>  8      1 8              0.00900            0.312           -0.117
    #>  9      1 9              0.00900            0.312           -0.117
    #> 10      1 10             0.00900            0.312           -0.117
    #> # i 7,990 more rows
    #> # i 19 more variables: `inv.Ka(inv==2)` <dbl>, `eye.Cl(eye==1)` <dbl>,
    #> #   `eye.Cl(eye==2)` <dbl>, `eye.Ka(eye==1)` <dbl>, `eye.Ka(eye==2)` <dbl>,
    #> #   `iov.Cl(occ==1)` <dbl>, `iov.Cl(occ==2)` <dbl>, `iov.Ka(occ==1)` <dbl>,
    #> #   `iov.Ka(occ==2)` <dbl>, V2 <dbl>, V3 <dbl>, TCl <dbl>, eta.Cl <dbl>,
    #> #   TKA <dbl>, eta.Ka <dbl>, Q <dbl>, Kin <dbl>, Kout <dbl>, EC50 <dbl>
    #> -- Initial Conditions ($inits): --
    #> depot centr  peri   eff 
    #>     0     0     0     1 
    #> 
    #> Simulation with uncertainty in:
    #> * parameters ($thetaMat for changes)
    #> * omega matrix ($omegaList)
    #> 
    #> -- First part of data (object): --
    #> # A tibble: 976,000 x 21
    #>   sim.id    id time  inv.Cl inv.Ka  eye.Cl eye.Ka iov.Cl iov.Ka    C2      C3
    #>    <int> <int>  [h]   <dbl>  <dbl>   <dbl>  <dbl>  <dbl>  <dbl> <dbl>   <dbl>
    #> 1      1     1  0   0.00900 -0.117 -0.330  0.0792 0.0370 -0.197  0    0      
    #> 2      1     1  0.1 0.00900 -0.117 -0.0547 0.185  0.0370 -0.197  8.12 0.00805
    #> 3      1     1  4   0.00900 -0.117 -0.330  0.0792 0.0370 -0.197 39.9  4.53   
    #> 4      1     1  4.1 0.00900 -0.117 -0.0547 0.185  0.0370 -0.197 20.0  4.64   
    #> 5      1     1  8   0.00900 -0.117 -0.330  0.0792 0.0370 -0.197  6.55 7.47   
    #> 6      1     1  8.1 0.00900 -0.117 -0.0547 0.185  0.0370 -0.197 18.2  7.51   
    #> # i 975,994 more rows
    #> # i 10 more variables: CL <dbl>, KA <dbl>, ef0 <dbl>, depot <dbl>, centr <dbl>,
    #> #   peri <dbl>, eff <dbl>, occ <fct>, eye <fct>, inv <fct>

There are multiple investigators in a study; Each investigator has a
number of individuals enrolled at their site. `rxode2` automatically
determines the number of investigators and then will simulate an effect
for each investigator. With the output, `inv.Cl(inv==1)` will be the
`inv.Cl` for investigator 1, `inv.Cl(inv==2)` will be the `inv.Cl` for
investigator 2, etc.

`inv.Cl(inv==1)`, `inv.Cl(inv==2)`, etc will be simulated for each study
and then combined to form the between investigator variability. In
equation form these represent the following:

    inv.Cl = (inv == 1) * `inv.Cl(inv==1)` + (inv == 2) * `inv.Cl(inv==2)`

If you look at the simulated parameters you can see `inv.Cl(inv==1)` and
`inv.Cl(inv==2)` are in the `s$params`; They are the same for each
study:

``` r
print(head(s$params))
```

    #>   sim.id id inv.Cl(inv==1) inv.Cl(inv==2) inv.Ka(inv==1) inv.Ka(inv==2)
    #> 1      1  1    0.008995575      0.3118205     -0.1165083   -0.001926179
    #> 2      1  2    0.008995575      0.3118205     -0.1165083   -0.001926179
    #> 3      1  3    0.008995575      0.3118205     -0.1165083   -0.001926179
    #> 4      1  4    0.008995575      0.3118205     -0.1165083   -0.001926179
    #> 5      1  5    0.008995575      0.3118205     -0.1165083   -0.001926179
    #> 6      1  6    0.008995575      0.3118205     -0.1165083   -0.001926179
    #>   eye.Cl(eye==1) eye.Cl(eye==2) eye.Ka(eye==1) eye.Ka(eye==2) iov.Cl(occ==1)
    #> 1    -0.33023666    -0.05471051    0.079190835     0.18487157    0.037021761
    #> 2    -0.42211135    -0.15399939   -0.006773841     0.22463048   -0.007762485
    #> 3    -0.05128596     0.18243012    0.123504458     0.13361259   -0.168921159
    #> 4    -0.06360621     0.65606292    0.340443016     0.04807945    0.157027939
    #> 5     0.08009126     0.06662396   -0.027627640    -0.05838949   -0.226341946
    #> 6    -0.11937190    -0.17320340    0.090540884     0.02039840   -0.211648032
    #>   iov.Cl(occ==2) iov.Ka(occ==1) iov.Ka(occ==2)       V2       V3      TCl
    #> 1     0.13932112    -0.19726803    0.088763868 40.26476 296.4748 19.25043
    #> 2     0.07716277     0.04555116    0.049148937 40.26476 296.4748 19.25043
    #> 3     0.08347085     0.17916513   -0.002990168 40.26476 296.4748 19.25043
    #> 4     0.24736740    -0.13444417   -0.077647794 40.26476 296.4748 19.25043
    #> 5    -0.09568733     0.02049190    0.054658457 40.26476 296.4748 19.25043
    #> 6    -0.01547841     0.12619350    0.040206608 40.26476 296.4748 19.25043
    #>        eta.Cl       TKA      eta.Ka        Q       Kin     Kout     EC50
    #> 1  0.09788109 0.2354877 -0.21526790 10.43157 0.9256189 1.044937 200.3679
    #> 2  0.33302598 0.2354877 -0.01028112 10.43157 0.9256189 1.044937 200.3679
    #> 3  0.08504325 0.2354877  0.37253705 10.43157 0.9256189 1.044937 200.3679
    #> 4  0.16521678 0.2354877 -0.01859974 10.43157 0.9256189 1.044937 200.3679
    #> 5  0.53131380 0.2354877 -0.37109644 10.43157 0.9256189 1.044937 200.3679
    #> 6 -0.30786713 0.2354877  0.14897588 10.43157 0.9256189 1.044937 200.3679

``` r
print(head(s$params %>% filter(sim.id == 2)))
```

    #>   sim.id id inv.Cl(inv==1) inv.Cl(inv==2) inv.Ka(inv==1) inv.Ka(inv==2)
    #> 1      2  1    -0.07227613     0.07620864    -0.01884171    -0.01962499
    #> 2      2  2    -0.07227613     0.07620864    -0.01884171    -0.01962499
    #> 3      2  3    -0.07227613     0.07620864    -0.01884171    -0.01962499
    #> 4      2  4    -0.07227613     0.07620864    -0.01884171    -0.01962499
    #> 5      2  5    -0.07227613     0.07620864    -0.01884171    -0.01962499
    #> 6      2  6    -0.07227613     0.07620864    -0.01884171    -0.01962499
    #>   eye.Cl(eye==1) eye.Cl(eye==2) eye.Ka(eye==1) eye.Ka(eye==2) iov.Cl(occ==1)
    #> 1    -0.25102634   -0.135230741      0.1354374    -0.01605664    0.081212198
    #> 2    -0.24832622   -0.380770273      0.2233348    -0.37101991    0.207985292
    #> 3     0.24081384    0.133361609     -0.3826770    -0.32496203   -0.099983807
    #> 4    -0.11021791    0.009423897      0.3655308     0.08736599   -0.002580804
    #> 5     0.21155058    0.194977611      0.3626918    -0.19452347    0.058392202
    #> 6     0.03663028    0.068047203     -0.1003754     0.32368023    0.167658084
    #>   iov.Cl(occ==2) iov.Ka(occ==1) iov.Ka(occ==2)       V2       V3      TCl
    #> 1   -0.032678168    -0.06556217   -0.061277262 40.21091 296.6424 18.86433
    #> 2    0.130258486    -0.05016896   -0.103721118 40.21091 296.6424 18.86433
    #> 3   -0.262726137     0.06144127   -0.104186144 40.21091 296.6424 18.86433
    #> 4   -0.037289852     0.03954265   -0.021344468 40.21091 296.6424 18.86433
    #> 5   -0.007530921     0.12713682    0.081238197 40.21091 296.6424 18.86433
    #> 6    0.026801132    -0.02811353   -0.002702762 40.21091 296.6424 18.86433
    #>        eta.Cl       TKA      eta.Ka        Q       Kin     Kout   EC50
    #> 1  0.19368101 0.5686675  0.26184170 10.54673 0.5434136 1.323825 200.09
    #> 2 -0.16262369 0.5686675  0.12969487 10.54673 0.5434136 1.323825 200.09
    #> 3  0.22164455 0.5686675  0.24501105 10.54673 0.5434136 1.323825 200.09
    #> 4  0.05997662 0.5686675  0.03059399 10.54673 0.5434136 1.323825 200.09
    #> 5  0.45691421 0.5686675 -0.20098716 10.54673 0.5434136 1.323825 200.09
    #> 6 -0.19943300 0.5686675 -0.36846082 10.54673 0.5434136 1.323825 200.09

For between eye variability and between occasion variability each
individual simulates a number of variables that become the between eye
and between occasion variability; In the case of the eye:

    eye.Cl = (eye == 1) * `eye.Cl(eye==1)` + (eye == 2) * `eye.Cl(eye==2)`

So when you look the simulation each of these variables (ie
`eye.Cl(eye==1)`, `eye.Cl(eye==2)`, etc) they change for each individual
and when combined make the between eye variability or the between
occasion variability that can be seen in some pharamcometric models.
