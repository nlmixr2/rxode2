# This function gets the currently assigned function pointer assignments

This function gets the currently assigned function pointer assignments

## Usage

``` r
rxode2parseGetPointerAssignment()
```

## Value

The currently assigned pointer assignments

## Author

Matthew L. Fidler

## Examples

``` r
rxode2parseGetTranslation()
#>                   rxFun                  fun                  type  package
#> 1               linCmtA              linCmtA             linCmtA_p   rxode2
#> 2               linCmtB              linCmtB             linCmtB_p   rxode2
#> 3                rxnorm               rxnorm           rxode2i_fn2   rxode2
#> 4               rxbinom              rxbinom       rxode2i_rxbinom   rxode2
#> 5              rxnbinom             rxnbinom       rxode2i_rxbinom   rxode2
#> 6            rxnbinomMu           rxnbinomMu       rxode2i_rxbinom   rxode2
#> 7              rxcauchy             rxcauchy            rxode2_fn2   rxode2
#> 8               rxchisq              rxchisq             rxode2_fn   rxode2
#> 9                 rxexp                rxexp             rxode2_fn   rxode2
#> 10                  rxf                  rxf            rxode2_fn2   rxode2
#> 11               rxgeom               rxgeom            rxode2_ifn   rxode2
#> 12              rxgamma              rxgamma            rxode2_fn2   rxode2
#> 13               rxbeta               rxbeta            rxode2_fn2   rxode2
#> 14               rxpois               rxpois            rxode2_ifn   rxode2
#> 15                 rxt_                 rxt_             rxode2_fn   rxode2
#> 16               rxunif               rxunif            rxode2_fn2   rxode2
#> 17            rxweibull            rxweibull            rxode2_fn2   rxode2
#> 18               rinorm               rinorm          rxode2i2_fn2   rxode2
#> 19              ribinom              ribinom      rxode2i2_ribinom   rxode2
#> 20             rinbinom             rinbinom      rxode2i2_ribinom   rxode2
#> 21           rinbinomMu           rinbinomMu      rxode2i2_ribinom   rxode2
#> 22             ricauchy             ricauchy          rxode2i2_fn2   rxode2
#> 23              richisq              richisq           rxode2i2_fn   rxode2
#> 24                riexp                riexp           rxode2i2_fn   rxode2
#> 25                  rif                  rif          rxode2i2_fn2   rxode2
#> 26               rigeom               rigeom          rxode2i2_ifn   rxode2
#> 27              rigamma              rigamma          rxode2i2_fn2   rxode2
#> 28               ribeta               ribeta          rxode2i2_fn2   rxode2
#> 29               ripois               ripois          rxode2i2_ifn   rxode2
#> 30                 rit_                 rit_           rxode2i2_fn   rxode2
#> 31               riunif               riunif          rxode2i2_fn2   rxode2
#> 32            riweibull            riweibull          rxode2i2_fn2   rxode2
#> 33                 ReLU                 ReLU             rxode2_fn   rxode2
#> 34                dReLU                dReLU             rxode2_fn   rxode2
#> 35                 GELU                 GELU             rxode2_fn   rxode2
#> 36                dGELU                dGELU             rxode2_fn   rxode2
#> 37               d2GELU               d2GELU             rxode2_fn   rxode2
#> 38               d3GELU               d3GELU             rxode2_fn   rxode2
#> 39               d4GELU               d4GELU             rxode2_fn   rxode2
#> 40             softplus             softplus             rxode2_fn   rxode2
#> 41            dsoftplus            dsoftplus             rxode2_fn   rxode2
#> 42           d2softplus           d2softplus             rxode2_fn   rxode2
#> 43           d3softplus           d3softplus             rxode2_fn   rxode2
#> 44           d4softplus           d4softplus             rxode2_fn   rxode2
#> 45                 SELU                 SELU             rxode2_fn   rxode2
#> 46                dSELU                dSELU             rxode2_fn   rxode2
#> 47                lReLU                lReLU             rxode2_fn   rxode2
#> 48               dlReLU               dlReLU             rxode2_fn   rxode2
#> 49                Swish                Swish             rxode2_fn   rxode2
#> 50               dSwish               dSwish             rxode2_fn   rxode2
#> 51                PReLU                PReLU            rxode2_fn2   rxode2
#> 52               dPReLU               dPReLU            rxode2_fn2   rxode2
#> 53              dPReLUa              dPReLUa            rxode2_fn2   rxode2
#> 54             dPReLUa1             dPReLUa1            rxode2_fn2   rxode2
#> 55                  ELU                  ELU            rxode2_fn2   rxode2
#> 56                 dELU                 dELU            rxode2_fn2   rxode2
#> 57                d2ELU                d2ELU            rxode2_fn2   rxode2
#> 58               d2aELU               d2aELU            rxode2_fn2   rxode2
#> 59                dELUa                dELUa            rxode2_fn2   rxode2
#> 60               d2ELUa               d2ELUa            rxode2_fn2   rxode2
#> 61                  phi                  phi             rxode2_fn   rxode2
#> 62               gammap               gammap            rxode2_fn2   rxode2
#> 63               gammaq               gammaq            rxode2_fn2   rxode2
#> 64            gammapInv            gammapInv            rxode2_fn2   rxode2
#> 65           gammapInva           gammapInva            rxode2_fn2   rxode2
#> 66            gammaqInv            gammaqInv            rxode2_fn2   rxode2
#> 67           gammaqInva           gammaqInva            rxode2_fn2   rxode2
#> 68           uppergamma           uppergamma            rxode2_fn2   rxode2
#> 69           lowergamma           lowergamma            rxode2_fn2   rxode2
#> 70            gammapDer            gammapDer            rxode2_fn2   rxode2
#> 71                logit                logit            rxode2_fn3   rxode2
#> 72               probit               probit            rxode2_fn3   rxode2
#> 73                expit                expit            rxode2_fn3   rxode2
#> 74            probitInv            probitInv            rxode2_fn3   rxode2
#> 75               simeta               simeta               _simfun   rxode2
#> 76               simeps               simeps               _simfun   rxode2
#> 77             llikNorm            _llikNorm    rxode2_llikNormFun rxode2ll
#> 78        llikNormDmean       _llikNormDmean    rxode2_llikNormFun rxode2ll
#> 79          llikNormDsd         _llikNormDsd    rxode2_llikNormFun rxode2ll
#> 80             llikPois            _llikPois    rxode2_llikPoisFun rxode2ll
#> 81      llikPoisDlambda     _llikPoisDlambda    rxode2_llikPoisFun rxode2ll
#> 82            llikBinom           _llikBinom   rxode2_llikBinomFun rxode2ll
#> 83       llikBinomDprob      _llikBinomDprob   rxode2_llikBinomFun rxode2ll
#> 84           llikNbinom          _llikNbinom   rxode2_llikBinomFun rxode2ll
#> 85      llikNbinomDprob     _llikNbinomDprob   rxode2_llikBinomFun rxode2ll
#> 86         llikNbinomMu        _llikNbinomMu   rxode2_llikBinomFun rxode2ll
#> 87      llikNbinomMuDmu     _llikNbinomMuDmu   rxode2_llikBinomFun rxode2ll
#> 88             llikBeta            _llikBeta    rxode2_llikBetaFun rxode2ll
#> 89      llikBetaDshape1     _llikBetaDshape1    rxode2_llikBetaFun rxode2ll
#> 90      llikBetaDshape2     _llikBetaDshape2    rxode2_llikBetaFun rxode2ll
#> 91                llikT               _llikT       rxode2_llikTFun rxode2ll
#> 92             llikTDdf            _llikTDdf       rxode2_llikTFun rxode2ll
#> 93           llikTDmean          _llikTDmean       rxode2_llikTFun rxode2ll
#> 94             llikTDsd            _llikTDsd       rxode2_llikTFun rxode2ll
#> 95            llikChisq           _llikChisq   rxode2_llikChisqFun rxode2ll
#> 96         llikChisqDdf        _llikChisqDdf   rxode2_llikChisqFun rxode2ll
#> 97              llikExp             _llikExp     rxode2_llikExpFun rxode2ll
#> 98         llikExpDrate        _llikExpDrate     rxode2_llikExpFun rxode2ll
#> 99                llikF               _llikF       rxode2_llikFFun rxode2ll
#> 100           llikFDdf1           _llikFDdf1       rxode2_llikFFun rxode2ll
#> 101           llikFDdf2           _llikFDdf2       rxode2_llikFFun rxode2ll
#> 102            llikGeom            _llikGeom    rxode2_llikGeomFun rxode2ll
#> 103          llikGeomDp          _llikGeomDp    rxode2_llikGeomFun rxode2ll
#> 104            llikUnif            _llikUnif    rxode2_llikUnifFun rxode2ll
#> 105      llikUnifDalpha      _llikUnifDalpha    rxode2_llikUnifFun rxode2ll
#> 106       llikUnifDbeta       _llikUnifDbeta    rxode2_llikUnifFun rxode2ll
#> 107         llikWeibull         _llikWeibull rxode2_llikWeibullFun rxode2ll
#> 108   llikWeibullDshape   _llikWeibullDshape rxode2_llikWeibullFun rxode2ll
#> 109   llikWeibullDscale   _llikWeibullDscale rxode2_llikWeibullFun rxode2ll
#> 110           llikGamma           _llikGamma   rxode2_llikGammaFun rxode2ll
#> 111     llikGammaDshape     _llikGammaDshape   rxode2_llikGammaFun rxode2ll
#> 112      llikGammaDrate      _llikGammaDrate   rxode2_llikGammaFun rxode2ll
#> 113          llikCauchy          _llikCauchy  rxode2_llikCauchyFun rxode2ll
#> 114 llikCauchyDlocation _llikCauchyDlocation  rxode2_llikCauchyFun rxode2ll
#> 115    llikCauchyDscale    _llikCauchyDscale  rxode2_llikCauchyFun rxode2ll
#>                packageFun argMin argMax threadSafe
#> 1                 linCmtA     14     14          1
#> 2                 linCmtB     15     15          1
#> 3                  rxnorm     NA     NA          1
#> 4                 rxbinom     NA     NA          1
#> 5                rxnbinom     NA     NA          1
#> 6              rxnbinomMu     NA     NA          1
#> 7                rxcauchy     NA     NA          1
#> 8                 rxchisq     NA     NA          1
#> 9                   rxexp     NA     NA          1
#> 10                    rxf     NA     NA          1
#> 11                 rxgeom     NA     NA          1
#> 12                rxgamma     NA     NA          1
#> 13                 rxbeta     NA     NA          1
#> 14                 rxpois     NA     NA          1
#> 15                   rxt_     NA     NA          1
#> 16                 rxunif     NA     NA          1
#> 17              rxweibull     NA     NA          1
#> 18                 rinorm     NA     NA          1
#> 19                ribinom     NA     NA          1
#> 20               rinbinom     NA     NA          1
#> 21             rinbinomMu     NA     NA          1
#> 22               ricauchy     NA     NA          1
#> 23                richisq     NA     NA          1
#> 24                  riexp     NA     NA          1
#> 25                    rif     NA     NA          1
#> 26                 rigeom     NA     NA          1
#> 27                rigamma     NA     NA          1
#> 28                 ribeta     NA     NA          1
#> 29                 ripois     NA     NA          1
#> 30                   rit_     NA     NA          1
#> 31                 riunif     NA     NA          1
#> 32              riweibull     NA     NA          1
#> 33                   ReLU      1      1          1
#> 34                  dReLU      1      1          1
#> 35                   GELU      1      1          1
#> 36                  dGELU      1      1          1
#> 37                 d2GELU      1      1          1
#> 38                 d3GELU      1      1          1
#> 39                 d4GELU      1      1          1
#> 40               softplus      1      1          1
#> 41              dsoftplus      1      1          1
#> 42             d2softplus      1      1          1
#> 43             d3softplus      1      1          1
#> 44             d4softplus      1      1          1
#> 45                   SELU      1      1          1
#> 46                  dSELU      1      1          1
#> 47                  lReLU      1      1          1
#> 48                 dlReLU      1      1          1
#> 49                  Swish      1      1          1
#> 50                 dSwish      1      1          1
#> 51                  PReLU      2      2          1
#> 52                 dPReLU      2      2          1
#> 53                dPReLUa      2      2          1
#> 54               dPReLUa1      2      2          1
#> 55                    ELU      2      2          1
#> 56                   dELU      2      2          1
#> 57                  d2ELU      2      2          1
#> 58                 d2aELU      2      2          1
#> 59                  dELUa      2      2          1
#> 60                 d2ELUa      2      2          1
#> 61                    phi     NA     NA          1
#> 62                 gammap      2      2          1
#> 63                 gammaq      2      2          1
#> 64              gammapInv      2      2          1
#> 65             gammapInva      2      2          1
#> 66              gammaqInv      2      2          1
#> 67             gammaqInva      2      2          1
#> 68             uppergamma      2      2          1
#> 69             lowergamma      2      2          1
#> 70              gammapDer      2      2          1
#> 71                  logit     NA     NA          1
#> 72                 probit     NA     NA          1
#> 73                  expit     NA     NA          1
#> 74              probitInv     NA     NA          1
#> 75                 simeta     NA     NA          1
#> 76                 simeps     NA     NA          1
#> 77             rxLlikNorm      3      3          1
#> 78        rxLlikNormDmean      3      3          1
#> 79          rxLlikNormDsd      3      3          1
#> 80             rxLlikPois      2      2          1
#> 81      rxLlikPoisDlambda      2      2          1
#> 82            rxLlikBinom      3      3          1
#> 83       rxLlikBinomDprob      3      3          1
#> 84           rxLlikNbinom      3      3          1
#> 85      rxLlikNbinomDprob      3      3          1
#> 86         rxLlikNbinomMu      3      3          1
#> 87      rxLlikNbinomMuDmu      3      3          1
#> 88             rxLlikBeta      3      3          1
#> 89      rxLlikBetaDshape1      3      3          1
#> 90      rxLlikBetaDshape2      3      3          1
#> 91                rxLlikT      4      4          1
#> 92             rxLlikTDdf      4      4          1
#> 93           rxLlikTDmean      4      4          1
#> 94             rxLlikTDsd      4      4          1
#> 95            rxLlikChisq      2      2          1
#> 96         rxLlikChisqDdf      2      2          1
#> 97              rxLlikExp      2      2          1
#> 98         rxLlikExpDrate      2      2          1
#> 99                rxLlikF      3      3          1
#> 100           rxLlikFDdf1      3      3          1
#> 101           rxLlikFDdf2      3      3          1
#> 102            rxLlikGeom      2      2          1
#> 103          rxLlikGeomDp     NA     NA          1
#> 104            rxLlikUnif      3      3          1
#> 105      rxLlikUnifDalpha      3      3          1
#> 106       rxLlikUnifDbeta      3      3          1
#> 107         rxLlikWeibull      3      3          1
#> 108   rxLlikWeibullDshape      3      3          1
#> 109   rxLlikWeibullDscale      3      3          1
#> 110           rxLlikGamma      3      3          1
#> 111     rxLlikGammaDshape      3      3          1
#> 112      rxLlikGammaDrate      3      3          1
#> 113          rxLlikCauchy      3      3          1
#> 114 rxLlikCauchyDlocation      3      3          1
#> 115    rxLlikCauchyDscale      3      3          1
```
