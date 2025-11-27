# Package index

## Creating/Deleting/Loading rxode2 compiled objects

- [`rxClean()`](https://nlmixr2.github.io/rxode2/reference/rxClean.md) :
  Cleanup anonymous DLLs by unloading them
- [`rxCompile()`](https://nlmixr2.github.io/rxode2/reference/rxCompile.md)
  : Compile a model if needed
- [`rxCreateCache()`](https://nlmixr2.github.io/rxode2/reference/rxCreateCache.md)
  : This will create the cache directory for rxode2 to save between
  sessions
- [`rxDelete()`](https://nlmixr2.github.io/rxode2/reference/rxDelete.md)
  : Delete the DLL for the model
- [`rxLastCompile()`](https://nlmixr2.github.io/rxode2/reference/rxLastCompile.md)
  : Get the last compiled model information as alist
- [`rxUnloadAll()`](https://nlmixr2.github.io/rxode2/reference/rxUnloadAll.md)
  : Unloads all rxode2 compiled DLLs
- [`rxode2()`](https://nlmixr2.github.io/rxode2/reference/rxode2.md)
  [`RxODE()`](https://nlmixr2.github.io/rxode2/reference/rxode2.md)
  [`rxode()`](https://nlmixr2.github.io/rxode2/reference/rxode2.md) :
  Create an ODE-based model specification

## Creating rxode2 event tables/data frames

- [`et()`](https://nlmixr2.github.io/rxode2/reference/et.md) : Event
  Table Function
- [`toTrialDuration()`](https://nlmixr2.github.io/rxode2/reference/toTrialDuration.md)
  : Convert event data to trial duration data A helper function to
  create a custom event table. The observation time will start from the
  first event time (baseline) and end at trial duration. The interval is
  the spacing between each observation.
- [`etRbind()`](https://nlmixr2.github.io/rxode2/reference/etRbind.md)
  [`rbind(`*`<rxEt>`*`)`](https://nlmixr2.github.io/rxode2/reference/etRbind.md)
  : Combining event tables
- [`etRep()`](https://nlmixr2.github.io/rxode2/reference/etRep.md)
  [`rep(`*`<rxEt>`*`)`](https://nlmixr2.github.io/rxode2/reference/etRep.md)
  : Repeat an rxode2 event table
- [`etSeq()`](https://nlmixr2.github.io/rxode2/reference/etSeq.md)
  [`seq(`*`<rxEt>`*`)`](https://nlmixr2.github.io/rxode2/reference/etSeq.md)
  : Sequence of event tables
- [`etExpand()`](https://nlmixr2.github.io/rxode2/reference/etExpand.md)
  : Expand additional doses
- [`as.et()`](https://nlmixr2.github.io/rxode2/reference/as.et.md) :
  Coerce object to data.frame
- [`eventTable()`](https://nlmixr2.github.io/rxode2/reference/eventTable.md)
  : Create an event table object
- [`add.dosing()`](https://nlmixr2.github.io/rxode2/reference/add.dosing.md)
  : Add dosing to eventTable
- [`add.sampling()`](https://nlmixr2.github.io/rxode2/reference/add.sampling.md)
  : Add sampling to eventTable

## Solving, Simulating and Solving options

- [`rxSolve()`](https://nlmixr2.github.io/rxode2/reference/rxSolve.md)
  [`update(`*`<rxSolve>`*`)`](https://nlmixr2.github.io/rxode2/reference/rxSolve.md)
  [`predict(`*`<rxode2>`*`)`](https://nlmixr2.github.io/rxode2/reference/rxSolve.md)
  [`predict(`*`<function>`*`)`](https://nlmixr2.github.io/rxode2/reference/rxSolve.md)
  [`predict(`*`<rxUi>`*`)`](https://nlmixr2.github.io/rxode2/reference/rxSolve.md)
  [`predict(`*`<rxSolve>`*`)`](https://nlmixr2.github.io/rxode2/reference/rxSolve.md)
  [`predict(`*`<rxEt>`*`)`](https://nlmixr2.github.io/rxode2/reference/rxSolve.md)
  [`predict(`*`<rxParams>`*`)`](https://nlmixr2.github.io/rxode2/reference/rxSolve.md)
  [`simulate(`*`<rxode2>`*`)`](https://nlmixr2.github.io/rxode2/reference/rxSolve.md)
  [`simulate(`*`<rxSolve>`*`)`](https://nlmixr2.github.io/rxode2/reference/rxSolve.md)
  [`simulate(`*`<rxParams>`*`)`](https://nlmixr2.github.io/rxode2/reference/rxSolve.md)
  [`solve(`*`<rxSolve>`*`)`](https://nlmixr2.github.io/rxode2/reference/rxSolve.md)
  [`solve(`*`<rxUi>`*`)`](https://nlmixr2.github.io/rxode2/reference/rxSolve.md)
  [`solve(`*`<function>`*`)`](https://nlmixr2.github.io/rxode2/reference/rxSolve.md)
  [`solve(`*`<rxode2>`*`)`](https://nlmixr2.github.io/rxode2/reference/rxSolve.md)
  [`solve(`*`<rxParams>`*`)`](https://nlmixr2.github.io/rxode2/reference/rxSolve.md)
  [`solve(`*`<rxEt>`*`)`](https://nlmixr2.github.io/rxode2/reference/rxSolve.md)
  [`rxControl()`](https://nlmixr2.github.io/rxode2/reference/rxSolve.md)
  : Options, Solving & Simulation of an ODE/solved system
- [`rxCbindStudyIndividual()`](https://nlmixr2.github.io/rxode2/reference/rxCbindStudyIndividual.md)
  : Bind the study parameters and individual parameters

## Functions for working with nlmixr2/rxode2 functions

- [`as.ini()`](https://nlmixr2.github.io/rxode2/reference/as.ini.md) :
  Turn into an ini block for initialization

- [`as.model()`](https://nlmixr2.github.io/rxode2/reference/as.model.md)
  : Turn into a model expression

- [`as.rxUi()`](https://nlmixr2.github.io/rxode2/reference/as.rxUi.md) :
  As rxode2 ui

- [`assertCompartmentExists()`](https://nlmixr2.github.io/rxode2/reference/assertCompartmentExists.md)
  [`testCompartmentExists()`](https://nlmixr2.github.io/rxode2/reference/assertCompartmentExists.md)
  : Verify that the compartment exists in a model

- [`assertCompartmentName()`](https://nlmixr2.github.io/rxode2/reference/assertCompartmentName.md)
  [`assertVariableName()`](https://nlmixr2.github.io/rxode2/reference/assertCompartmentName.md)
  [`assertParameterValue()`](https://nlmixr2.github.io/rxode2/reference/assertCompartmentName.md)
  [`assertExists()`](https://nlmixr2.github.io/rxode2/reference/assertCompartmentName.md)
  [`testExists()`](https://nlmixr2.github.io/rxode2/reference/assertCompartmentName.md)
  : Verify that a value is a valid nlmixr2 compartment name

- [`assertCompartmentNew()`](https://nlmixr2.github.io/rxode2/reference/assertCompartmentNew.md)
  : Verify that a compartment would be new to the model

- [`assertRxUi()`](https://nlmixr2.github.io/rxode2/reference/assertRxUi.md)
  [`assertRxUiPrediction()`](https://nlmixr2.github.io/rxode2/reference/assertRxUi.md)
  [`assertRxUiIovNoCor()`](https://nlmixr2.github.io/rxode2/reference/assertRxUi.md)
  [`assertRxUiNoMix()`](https://nlmixr2.github.io/rxode2/reference/assertRxUi.md)
  [`assertRxUiSingleEndpoint()`](https://nlmixr2.github.io/rxode2/reference/assertRxUi.md)
  [`assertRxUiTransformNormal()`](https://nlmixr2.github.io/rxode2/reference/assertRxUi.md)
  [`assertRxUiNormal()`](https://nlmixr2.github.io/rxode2/reference/assertRxUi.md)
  [`assertRxUiMuRefOnly()`](https://nlmixr2.github.io/rxode2/reference/assertRxUi.md)
  [`assertRxUiEstimatedResiduals()`](https://nlmixr2.github.io/rxode2/reference/assertRxUi.md)
  [`assertRxUiPopulationOnly()`](https://nlmixr2.github.io/rxode2/reference/assertRxUi.md)
  [`assertRxUiMixedOnly()`](https://nlmixr2.github.io/rxode2/reference/assertRxUi.md)
  [`assertRxUiRandomOnIdOnly()`](https://nlmixr2.github.io/rxode2/reference/assertRxUi.md)
  : Assert properties of the rxUi models

- [`assertVariableExists()`](https://nlmixr2.github.io/rxode2/reference/assertVariableExists.md)
  [`testVariableExists()`](https://nlmixr2.github.io/rxode2/reference/assertVariableExists.md)
  : Assert a variable exists in the model

- [`assertVariableNew()`](https://nlmixr2.github.io/rxode2/reference/assertVariableNew.md)
  : Assert a variable would be new to the model

- [`ini()`](https://nlmixr2.github.io/rxode2/reference/ini.md) : Ini
  block for rxode2/nlmixr models

- [`` `ini<-`() ``](https://nlmixr2.github.io/rxode2/reference/ini-set.md)
  : Assign the ini block in the rxode2 related object

- [`model()`](https://nlmixr2.github.io/rxode2/reference/model.md) :
  Model block for rxode2/nlmixr models

- [`` `model<-`() ``](https://nlmixr2.github.io/rxode2/reference/model-set.md)
  : Assign the model block in the rxode2 related object

- [`modelExtract()`](https://nlmixr2.github.io/rxode2/reference/modelExtract.md)
  : Extract model lines from a rxui model

- [`rxAppendModel()`](https://nlmixr2.github.io/rxode2/reference/rxAppendModel.md)
  : Append two rxui models together

- [`rxFixPop()`](https://nlmixr2.github.io/rxode2/reference/rxFixPop.md)
  : Apply the fixed population estimated parameters

- [`rxFixRes()`](https://nlmixr2.github.io/rxode2/reference/rxFixRes.md)
  : Literally fix residual parameters

- [`rxRename()`](https://nlmixr2.github.io/rxode2/reference/rxRename.md)
  [`.rxRename()`](https://nlmixr2.github.io/rxode2/reference/rxRename.md)
  [`rename.rxUi()`](https://nlmixr2.github.io/rxode2/reference/rxRename.md)
  [`rename.function()`](https://nlmixr2.github.io/rxode2/reference/rxRename.md)
  :

  Rename items inside of a `rxode2` ui model

- [`rxSetCovariateNamesForPiping()`](https://nlmixr2.github.io/rxode2/reference/rxSetCovariateNamesForPiping.md)
  : Assign covariates for piping

- [`rxSetPipingAuto()`](https://nlmixr2.github.io/rxode2/reference/rxSetPipingAuto.md)
  : Set the variables for the model piping automatic covarite selection

- [`rxUiDecompress()`](https://nlmixr2.github.io/rxode2/reference/rxUiDecompress.md)
  [`rxUiCompress()`](https://nlmixr2.github.io/rxode2/reference/rxUiDecompress.md)
  :

  Compress/Decompress `rxode2` ui

- [`` `rxode2<-`() ``](https://nlmixr2.github.io/rxode2/reference/rxode2-set.md)
  [`` `rxode<-`() ``](https://nlmixr2.github.io/rxode2/reference/rxode2-set.md)
  [`` `RxODE<-`() ``](https://nlmixr2.github.io/rxode2/reference/rxode2-set.md)
  : Set the function body of an rxUi object while retaining other object
  information (like data)

- [`testIniDf()`](https://nlmixr2.github.io/rxode2/reference/testIniDf.md)
  [`assertIniDf()`](https://nlmixr2.github.io/rxode2/reference/testIniDf.md)
  : This function tests if this object is a iniDf as needed by the UI

- [`testRxLinCmt()`](https://nlmixr2.github.io/rxode2/reference/testRxLinCmt.md)
  [`assertRxLinCmt()`](https://nlmixr2.github.io/rxode2/reference/testRxLinCmt.md)
  : Test if rxode2 uses linear solved systems

- [`testRxUnbounded()`](https://nlmixr2.github.io/rxode2/reference/testRxUnbounded.md)
  [`assertRxUnbounded()`](https://nlmixr2.github.io/rxode2/reference/testRxUnbounded.md)
  [`warnRxBounded()`](https://nlmixr2.github.io/rxode2/reference/testRxUnbounded.md)
  : Test if the rxode2 model has any parameters with user defined
  boundaries

- [`update(`*`<rxUi>`*`)`](https://nlmixr2.github.io/rxode2/reference/update.rxUi.md)
  : Update for rxUi

- [`zeroRe()`](https://nlmixr2.github.io/rxode2/reference/zeroRe.md) :
  Set random effects and residual error to zero

## ggplot2/plot support functions

- [`stat_cens()`](https://nlmixr2.github.io/rxode2/reference/stat_cens.md)
  [`geom_cens()`](https://nlmixr2.github.io/rxode2/reference/stat_cens.md)
  : Censoring geom/stat
- [`stat_amt()`](https://nlmixr2.github.io/rxode2/reference/stat_amt.md)
  [`geom_amt()`](https://nlmixr2.github.io/rxode2/reference/stat_amt.md)
  : Dosing/Amt geom/stat
- [`plot(`*`<rxSolve>`*`)`](https://nlmixr2.github.io/rxode2/reference/plot.rxSolve.md)
  [`plot(`*`<rxSolveConfint1>`*`)`](https://nlmixr2.github.io/rxode2/reference/plot.rxSolve.md)
  [`plot(`*`<rxSolveConfint2>`*`)`](https://nlmixr2.github.io/rxode2/reference/plot.rxSolve.md)
  : Plot rxode2 objects

## Exploring rxode2 and creating models with Shiny

- [`rxShiny()`](https://nlmixr2.github.io/rxode2/reference/rxShiny.md) :
  Use Shiny to help develop an rxode2 model
- [`genShinyApp.template()`](https://nlmixr2.github.io/rxode2/reference/genShinyApp.template.md)
  [`write.template.server()`](https://nlmixr2.github.io/rxode2/reference/genShinyApp.template.md)
  [`write.template.ui()`](https://nlmixr2.github.io/rxode2/reference/genShinyApp.template.md)
  : Generate an example (template) of a dosing regimen shiny app

## Using rxode2 in Packages

- [`rxUse()`](https://nlmixr2.github.io/rxode2/reference/rxUse.md) : Use
  model object in your package
- [`rxPkg()`](https://nlmixr2.github.io/rxode2/reference/rxPkg.md) :
  Creates a package from compiled rxode2 models

## Specialized Simulation functions

- [`binomProbs()`](https://nlmixr2.github.io/rxode2/reference/binomProbs.md)
  : Calculate expected confidence bands with binomial sampling
  distribution
- [`cvPost()`](https://nlmixr2.github.io/rxode2/reference/cvPost.md) :
  Sample a covariance Matrix from the Posterior Inverse Wishart
  distribution.
- [`dfWishart()`](https://nlmixr2.github.io/rxode2/reference/dfWishart.md)
  : This uses simulations to match the rse
- [`meanProbs()`](https://nlmixr2.github.io/rxode2/reference/meanProbs.md)
  : Calculate expected confidence bands or prediction intreval with
  normal or t sampling distribution
- [`rinvchisq()`](https://nlmixr2.github.io/rxode2/reference/rinvchisq.md)
  : Scaled Inverse Chi Squared distribution
- [`rxGetSeed()`](https://nlmixr2.github.io/rxode2/reference/rxGetSeed.md)
  : Get the rxode2 seed
- [`rxPp()`](https://nlmixr2.github.io/rxode2/reference/rxPp.md) :
  Simulate a from a Poisson process
- [`rxSetSeed()`](https://nlmixr2.github.io/rxode2/reference/rxSetSeed.md)
  : Set the parallel seed for rxode2 random number generation
- [`rxSimThetaOmega()`](https://nlmixr2.github.io/rxode2/reference/rxSimThetaOmega.md)
  : Simulate Parameters from a Theta/Omega specification
- [`rxWithSeed()`](https://nlmixr2.github.io/rxode2/reference/rxWithSeed.md)
  [`rxWithPreserveSeed()`](https://nlmixr2.github.io/rxode2/reference/rxWithSeed.md)
  : Preserved seed and possibly set the seed
- [`rxbeta()`](https://nlmixr2.github.io/rxode2/reference/rxbeta.md) :
  Simulate beta variable from threefry generator
- [`rxbinom()`](https://nlmixr2.github.io/rxode2/reference/rxbinom.md) :
  Simulate Binomial variable from threefry generator
- [`rxcauchy()`](https://nlmixr2.github.io/rxode2/reference/rxcauchy.md)
  : Simulate Cauchy variable from threefry generator
- [`rxchisq()`](https://nlmixr2.github.io/rxode2/reference/rxchisq.md) :
  Simulate chi-squared variable from threefry generator
- [`rxexp()`](https://nlmixr2.github.io/rxode2/reference/rxexp.md) :
  Simulate exponential variable from threefry generator
- [`rxf()`](https://nlmixr2.github.io/rxode2/reference/rxf.md) :
  Simulate F variable from threefry generator
- [`rxgamma()`](https://nlmixr2.github.io/rxode2/reference/rxgamma.md) :
  Simulate gamma variable from threefry generator
- [`rxgeom()`](https://nlmixr2.github.io/rxode2/reference/rxgeom.md) :
  Simulate geometric variable from threefry generator
- [`rxnbinom()`](https://nlmixr2.github.io/rxode2/reference/rxnbinom.md)
  [`rxnbinomMu()`](https://nlmixr2.github.io/rxode2/reference/rxnbinom.md)
  : Simulate Binomial variable from threefry generator
- [`rxnormV()`](https://nlmixr2.github.io/rxode2/reference/rxnormV.md)
  [`rxnorm()`](https://nlmixr2.github.io/rxode2/reference/rxnormV.md) :
  Simulate random normal variable from threefry generator
- [`rxord()`](https://nlmixr2.github.io/rxode2/reference/rxord.md) :
  Simulate ordinal value
- [`rxpois()`](https://nlmixr2.github.io/rxode2/reference/rxpois.md) :
  Simulate random Poisson variable from threefry generator
- [`rxt()`](https://nlmixr2.github.io/rxode2/reference/rxt.md) :
  Simulate student t variable from threefry generator
- [`rxunif()`](https://nlmixr2.github.io/rxode2/reference/rxunif.md) :
  Simulate uniform variable from threefry generator
- [`rxweibull()`](https://nlmixr2.github.io/rxode2/reference/rxweibull.md)
  : Simulate Weibull variable from threefry generator

## Optimizing models and/or adding precision

- [`rxOptExpr()`](https://nlmixr2.github.io/rxode2/reference/rxOptExpr.md)
  : Optimize rxode2 for computer evaluation
- [`rxSumProdModel()`](https://nlmixr2.github.io/rxode2/reference/rxSumProdModel.md)
  : Recast model in terms of sum/prod
- [`rxSetProd()`](https://nlmixr2.github.io/rxode2/reference/rxSetProd.md)
  : Defunct setting of product
- [`rxSetSum()`](https://nlmixr2.github.io/rxode2/reference/rxSetSum.md)
  : Defunct setting of sum

## Adding/Modifying rxode2 C-based functions and derivatives

- [`rxD()`](https://nlmixr2.github.io/rxode2/reference/rxD.md) : Add to
  rxode2's derivative tables

## Running unit tests in rxode2

- [`rxValidate()`](https://nlmixr2.github.io/rxode2/reference/rxValidate.md)
  [`rxTest()`](https://nlmixr2.github.io/rxode2/reference/rxValidate.md)
  : Validate rxode2 This allows easy validation/qualification of nlmixr
  by running the testing suite on your system.

## Special Functions for rxode2

- [`boxCox()`](https://nlmixr2.github.io/rxode2/reference/boxCox.md)
  [`boxCoxInv()`](https://nlmixr2.github.io/rxode2/reference/boxCox.md)
  [`yeoJohnson()`](https://nlmixr2.github.io/rxode2/reference/boxCox.md)
  [`yeoJohnsonInv()`](https://nlmixr2.github.io/rxode2/reference/boxCox.md)
  : boxCox/yeoJohnson and inverse boxCox/yeoJohnson functions
- [`erf()`](https://nlmixr2.github.io/rxode2/reference/erf.md) : Error
  function
- [`forderForceBase()`](https://nlmixr2.github.io/rxode2/reference/forderForceBase.md)
  : Force using base order for rxode2 radix sorting
- [`gammap()`](https://nlmixr2.github.io/rxode2/reference/gammap.md) :
  Gammap: normalized lower incomplete gamma function
- [`gammapDer()`](https://nlmixr2.github.io/rxode2/reference/gammapDer.md)
  : gammapDer: derivative of gammap
- [`gammapInv()`](https://nlmixr2.github.io/rxode2/reference/gammapInv.md)
  [`gammapInva()`](https://nlmixr2.github.io/rxode2/reference/gammapInv.md)
  : gammapInv and gammapInva: Inverses of normalized gammap function
- [`gammaq()`](https://nlmixr2.github.io/rxode2/reference/gammaq.md) :
  Gammaq: normalized upper incomplete gamma function
- [`gammaqInv()`](https://nlmixr2.github.io/rxode2/reference/gammaqInv.md)
  [`gammaqInva()`](https://nlmixr2.github.io/rxode2/reference/gammaqInv.md)
  : gammaqInv and gammaqInva: Inverses of normalized gammaq function
- [`logit()`](https://nlmixr2.github.io/rxode2/reference/logit.md)
  [`expit()`](https://nlmixr2.github.io/rxode2/reference/logit.md)
  [`logitNormInfo()`](https://nlmixr2.github.io/rxode2/reference/logit.md)
  [`probitNormInfo()`](https://nlmixr2.github.io/rxode2/reference/logit.md)
  : logit and inverse logit (expit) functions
- [`lowergamma()`](https://nlmixr2.github.io/rxode2/reference/lowergamma.md)
  : lowergamma: upper incomplete gamma function
- [`mexpit()`](https://nlmixr2.github.io/rxode2/reference/mexpit.md)
  [`dmexpit()`](https://nlmixr2.github.io/rxode2/reference/mexpit.md) :
  mexpit – Convert log-scale numbers to probabilities
- [`mlogit()`](https://nlmixr2.github.io/rxode2/reference/mlogit.md) :
  mlogit – Convert multiple probabilities to log-scale numbers
- [`phi()`](https://nlmixr2.github.io/rxode2/reference/phi.md) :
  Cumulative distribution of standard normal
- [`probit()`](https://nlmixr2.github.io/rxode2/reference/probit.md)
  [`probitInv()`](https://nlmixr2.github.io/rxode2/reference/probit.md)
  : probit and inverse probit functions
- [`rxRmvn()`](https://nlmixr2.github.io/rxode2/reference/rxRmvn.md) :
  Simulate from a (truncated) multivariate normal

## log likelihood functions supporting generalized likelihoods for focei methods

- [`llikBeta()`](https://nlmixr2.github.io/rxode2/reference/llikBeta.md)
  : Calculate the log likelihood of the binomial function (and its
  derivatives)
- [`llikBinom()`](https://nlmixr2.github.io/rxode2/reference/llikBinom.md)
  : Calculate the log likelihood of the binomial function (and its
  derivatives)
- [`llikCauchy()`](https://nlmixr2.github.io/rxode2/reference/llikCauchy.md)
  : log likelihood of Cauchy distribution and it's derivatives (from
  stan)
- [`llikChisq()`](https://nlmixr2.github.io/rxode2/reference/llikChisq.md)
  : log likelihood and derivatives for chi-squared distribution
- [`llikExp()`](https://nlmixr2.github.io/rxode2/reference/llikExp.md) :
  log likelihood and derivatives for exponential distribution
- [`llikF()`](https://nlmixr2.github.io/rxode2/reference/llikF.md) : log
  likelihood and derivatives for F distribution
- [`llikGamma()`](https://nlmixr2.github.io/rxode2/reference/llikGamma.md)
  : log likelihood and derivatives for Gamma distribution
- [`llikGeom()`](https://nlmixr2.github.io/rxode2/reference/llikGeom.md)
  : log likelihood and derivatives for Geom distribution
- [`llikNbinom()`](https://nlmixr2.github.io/rxode2/reference/llikNbinom.md)
  : Calculate the log likelihood of the negative binomial function (and
  its derivatives)
- [`llikNbinomMu()`](https://nlmixr2.github.io/rxode2/reference/llikNbinomMu.md)
  : Calculate the log likelihood of the negative binomial function (and
  its derivatives)
- [`llikNorm()`](https://nlmixr2.github.io/rxode2/reference/llikNorm.md)
  : Log likelihood for normal distribution
- [`llikPois()`](https://nlmixr2.github.io/rxode2/reference/llikPois.md)
  : log-likelihood for the Poisson distribution
- [`llikT()`](https://nlmixr2.github.io/rxode2/reference/llikT.md) : Log
  likelihood of T and it's derivatives (from stan)
- [`llikUnif()`](https://nlmixr2.github.io/rxode2/reference/llikUnif.md)
  : log likelihood and derivatives for Unif distribution
- [`llikWeibull()`](https://nlmixr2.github.io/rxode2/reference/llikWeibull.md)
  : log likelihood and derivatives for Weibull distribution

## Parallel processing support commands

- [`getRxThreads()`](https://nlmixr2.github.io/rxode2/reference/getRxThreads.md)
  [`setRxThreads()`](https://nlmixr2.github.io/rxode2/reference/getRxThreads.md)
  [`rxCores()`](https://nlmixr2.github.io/rxode2/reference/getRxThreads.md)
  : Get/Set the number of threads that rxode2 uses

## rxode2 utility functions

- [`rxDerived()`](https://nlmixr2.github.io/rxode2/reference/rxDerived.md)
  : Calculate derived parameters for the 1-, 2-, and 3- compartment
  linear models.
- [`rxInv()`](https://nlmixr2.github.io/rxode2/reference/rxInv.md) :
  Invert matrix using RcppArmadillo.
- [`rxIsCurrent()`](https://nlmixr2.github.io/rxode2/reference/rxIsCurrent.md)
  : Checks if the rxode2 object was built with the current build
- [`rxLhs()`](https://nlmixr2.github.io/rxode2/reference/rxLhs.md) :
  Left handed Variables
- [`rxLock()`](https://nlmixr2.github.io/rxode2/reference/rxLock.md)
  [`rxUnlock()`](https://nlmixr2.github.io/rxode2/reference/rxLock.md) :
  Lock/unlocking of rxode2 dll file
- [`rxNorm()`](https://nlmixr2.github.io/rxode2/reference/rxNorm.md) :
  Get the normalized model
- [`rxParams()`](https://nlmixr2.github.io/rxode2/reference/rxParams.md)
  [`rxParam()`](https://nlmixr2.github.io/rxode2/reference/rxParams.md)
  : Parameters specified by the model
- [`rxProgress()`](https://nlmixr2.github.io/rxode2/reference/rxProgress.md)
  [`rxTick()`](https://nlmixr2.github.io/rxode2/reference/rxProgress.md)
  [`rxProgressStop()`](https://nlmixr2.github.io/rxode2/reference/rxProgress.md)
  [`rxProgressAbort()`](https://nlmixr2.github.io/rxode2/reference/rxProgress.md)
  : rxode2 progress bar functions
- [`rxState()`](https://nlmixr2.github.io/rxode2/reference/rxState.md) :
  State variables
- [`rxStateOde()`](https://nlmixr2.github.io/rxode2/reference/rxStateOde.md)
  : Get the ODE states only
- [`rxSymInvChol()`](https://nlmixr2.github.io/rxode2/reference/rxSymInvChol.md)
  : Get Omega^-1 and derivatives
- [`swapMatListWithCube()`](https://nlmixr2.github.io/rxode2/reference/swapMatListWithCube.md)
  : Swaps the matrix list with a cube
- [`uppergamma()`](https://nlmixr2.github.io/rxode2/reference/uppergamma.md)
  : uppergamma: upper incomplete gamma function

## User functions

- [`linMod()`](https://nlmixr2.github.io/rxode2/reference/linMod.md)
  [`linMod0()`](https://nlmixr2.github.io/rxode2/reference/linMod.md)
  [`linModB()`](https://nlmixr2.github.io/rxode2/reference/linMod.md)
  [`linModB0()`](https://nlmixr2.github.io/rxode2/reference/linMod.md)
  [`linModA()`](https://nlmixr2.github.io/rxode2/reference/linMod.md)
  [`linModA0()`](https://nlmixr2.github.io/rxode2/reference/linMod.md)
  [`linModD()`](https://nlmixr2.github.io/rxode2/reference/linMod.md)
  [`linModD0()`](https://nlmixr2.github.io/rxode2/reference/linMod.md)
  [`linModM()`](https://nlmixr2.github.io/rxode2/reference/linMod.md)
  [`linModM0()`](https://nlmixr2.github.io/rxode2/reference/linMod.md) :
  Linear model to replace in rxode2 ui model
- [`mix()`](https://nlmixr2.github.io/rxode2/reference/mix.md) : Specify
  a mixture model of variables
- [`rxIntToBase()`](https://nlmixr2.github.io/rxode2/reference/rxIntToBase.md)
  : Convert a positive base
- [`rxIntToLetter()`](https://nlmixr2.github.io/rxode2/reference/rxIntToLetter.md)
  : Convert a positive integer to a letter series
- [`rxUdfUiControl()`](https://nlmixr2.github.io/rxode2/reference/rxUdfUiControl.md)
  : Return the control that is being processed or setup control for
  processing
- [`rxUdfUiData()`](https://nlmixr2.github.io/rxode2/reference/rxUdfUiData.md)
  : Return the data.frame that is being processed or setup data.frame
  for processing
- [`rxUdfUiEst()`](https://nlmixr2.github.io/rxode2/reference/rxUdfUiEst.md)
  : Return the current estimation method for the UI processing
- [`rxUdfUiIniDf()`](https://nlmixr2.github.io/rxode2/reference/rxUdfUiIniDf.md)
  : Get the rxode2 iniDf of the current UI being processed (or return
  NULL)
- [`rxUdfUiIniLhs()`](https://nlmixr2.github.io/rxode2/reference/rxUdfUiIniLhs.md)
  : Return the lhs parsed language expression
- [`rxUdfUiMv()`](https://nlmixr2.github.io/rxode2/reference/rxUdfUiMv.md)
  : Return the model variables that is being processed or setup model
  variables for processing
- [`rxUdfUiNum()`](https://nlmixr2.github.io/rxode2/reference/rxUdfUiNum.md)
  : This gives the current number in the ui of the particular function
  being called.
- [`rxUdfUiParsing()`](https://nlmixr2.github.io/rxode2/reference/rxUdfUiParsing.md)
  : Returns if the current ui function is being parsed

## Internal Functions

- [`.cbindOme()`](https://nlmixr2.github.io/rxode2/reference/dot-cbindOme.md)
  : cbind Ome

- [`.collectWarnings()`](https://nlmixr2.github.io/rxode2/reference/dot-collectWarnings.md)
  : Collect warnings and just warn once.

- [`.copyUi()`](https://nlmixr2.github.io/rxode2/reference/dot-copyUi.md)
  : This copies the rxode2 UI object so it can be modified

- [`.handleSingleErrTypeNormOrTFoceiBase()`](https://nlmixr2.github.io/rxode2/reference/dot-handleSingleErrTypeNormOrTFoceiBase.md)
  : Handle the single error for normal or t distributions

- [`.matchesLangTemplate()`](https://nlmixr2.github.io/rxode2/reference/dot-matchesLangTemplate.md)
  : Check if a language object matches a template language object

- [`.modelHandleModelLines()`](https://nlmixr2.github.io/rxode2/reference/dot-modelHandleModelLines.md)
  : Handle model lines

- [`.quoteCallInfoLines()`](https://nlmixr2.github.io/rxode2/reference/dot-quoteCallInfoLines.md)
  : Returns quoted call information

- [`.rxLinCmtGen()`](https://nlmixr2.github.io/rxode2/reference/dot-rxLinCmtGen.md)
  : Internal function to generate the model variables for a linCmt()
  model

- [`.rxWithOptions()`](https://nlmixr2.github.io/rxode2/reference/dot-rxWithOptions.md)
  : Temporarily set options then restore them while running code

- [`.rxWithWd()`](https://nlmixr2.github.io/rxode2/reference/dot-rxWithWd.md)
  : Temporarily set options then restore them while running code

- [`.rxode2ptrs()`](https://nlmixr2.github.io/rxode2/reference/dot-rxode2ptrs.md)
  : Get the rxode2 function pointers

- [`.toClassicEvid()`](https://nlmixr2.github.io/rxode2/reference/dot-toClassicEvid.md)
  : This converts NONMEM-style EVIDs to classic RxODE events

- [`.vecDf()`](https://nlmixr2.github.io/rxode2/reference/dot-vecDf.md)
  : Convert numeric vector to repeated data.frame

- [`invWR1d()`](https://nlmixr2.github.io/rxode2/reference/invWR1d.md) :
  One correlation sample from the Inverse Wishart distribution

- [`is.rxStackData()`](https://nlmixr2.github.io/rxode2/reference/is.rxStackData.md)
  : Return if the object can be stacked

- [`odeMethodToInt()`](https://nlmixr2.github.io/rxode2/reference/odeMethodToInt.md)
  : Conversion between character and integer ODE integration methods for
  rxode2

- [`print(`*`<rxModelVars>`*`)`](https://nlmixr2.github.io/rxode2/reference/print.rxModelVars.md)
  : Print Values

- [`rLKJ1()`](https://nlmixr2.github.io/rxode2/reference/rLKJ1.md) : One
  correlation sample from the LKJ distribution

- [`rxAllowUnload()`](https://nlmixr2.github.io/rxode2/reference/rxAllowUnload.md)
  : Allow unloading of dlls

- [`rxAssignControlValue()`](https://nlmixr2.github.io/rxode2/reference/rxAssignControlValue.md)
  : Assign Control Variable

- [`rxAssignPtr()`](https://nlmixr2.github.io/rxode2/reference/rxAssignPtr.md)
  : Assign pointer based on model variables

- [`rxChain()`](https://nlmixr2.github.io/rxode2/reference/rxChain.md)
  [`` `+`( ``*`<solveRxDll>`*`)`](https://nlmixr2.github.io/rxode2/reference/rxChain.md)
  : rxChain Chain or add item to solved system of equations

- [`rxControlUpdateSens()`](https://nlmixr2.github.io/rxode2/reference/rxControlUpdateSens.md)
  : This updates the tolerances based on the sensitivity equations

- [`rxDfdy()`](https://nlmixr2.github.io/rxode2/reference/rxDfdy.md) :
  Jacobian and parameter derivatives

- [`rxEtDispatchSolve()`](https://nlmixr2.github.io/rxode2/reference/rxEtDispatchSolve.md)
  : Dispatch solve to 'rxode2' solve

- [`rxEvid()`](https://nlmixr2.github.io/rxode2/reference/rxEvid.md)
  [`as.rxEvid()`](https://nlmixr2.github.io/rxode2/reference/rxEvid.md)
  [`c(`*`<rxEvid>`*`)`](https://nlmixr2.github.io/rxode2/reference/rxEvid.md)
  [`` `[`( ``*`<rxEvid>`*`)`](https://nlmixr2.github.io/rxode2/reference/rxEvid.md)
  [`as.character(`*`<rxEvid>`*`)`](https://nlmixr2.github.io/rxode2/reference/rxEvid.md)
  [`` `[[`( ``*`<rxEvid>`*`)`](https://nlmixr2.github.io/rxode2/reference/rxEvid.md)
  [`` `units<-`( ``*`<rxEvid>`*`)`](https://nlmixr2.github.io/rxode2/reference/rxEvid.md)
  [`c(`*`<rxRateDur>`*`)`](https://nlmixr2.github.io/rxode2/reference/rxEvid.md)
  [`format(`*`<rxEvid>`*`)`](https://nlmixr2.github.io/rxode2/reference/rxEvid.md)
  [`format(`*`<rxRateDur>`*`)`](https://nlmixr2.github.io/rxode2/reference/rxEvid.md)
  [`print(`*`<rxEvid>`*`)`](https://nlmixr2.github.io/rxode2/reference/rxEvid.md)
  : EVID formatting for tibble and other places.

- [`rxFun()`](https://nlmixr2.github.io/rxode2/reference/rxFun.md)
  [`rxRmFun()`](https://nlmixr2.github.io/rxode2/reference/rxFun.md) :
  Add/Create C functions for use in rxode2

- [`rxGetControl()`](https://nlmixr2.github.io/rxode2/reference/rxGetControl.md)
  : rxGetControl option from ui

- [`rxGetLin()`](https://nlmixr2.github.io/rxode2/reference/rxGetLin.md)
  : Get the linear compartment model true function

- [`rxGetrxode2()`](https://nlmixr2.github.io/rxode2/reference/rxGetrxode2.md)
  : Get rxode2 model from object

- [`rxHtml()`](https://nlmixr2.github.io/rxode2/reference/rxHtml.md) :
  Format rxSolve and related objects as html.

- [`rxIndLinState()`](https://nlmixr2.github.io/rxode2/reference/rxIndLinState.md)
  : Set the preferred factoring by state

- [`rxIndLinStrategy()`](https://nlmixr2.github.io/rxode2/reference/rxIndLinStrategy.md)
  : This sets the inductive linearization strategy for matrix building

- [`rxParseSuppressMsg()`](https://nlmixr2.github.io/rxode2/reference/rxParseSuppressMsg.md)
  : Respect suppress messages

- [`rxPreferredDistributionName()`](https://nlmixr2.github.io/rxode2/reference/rxPreferredDistributionName.md)
  : Change distribution name to the preferred distribution name term

- [`rxRateDur()`](https://nlmixr2.github.io/rxode2/reference/rxRateDur.md)
  [`` `[`( ``*`<rxRateDur>`*`)`](https://nlmixr2.github.io/rxode2/reference/rxRateDur.md)
  [`as.rxRateDur()`](https://nlmixr2.github.io/rxode2/reference/rxRateDur.md)
  [`as.character(`*`<rxRateDur>`*`)`](https://nlmixr2.github.io/rxode2/reference/rxRateDur.md)
  [`` `[[`( ``*`<rxRateDur>`*`)`](https://nlmixr2.github.io/rxode2/reference/rxRateDur.md)
  : Creates a rxRateDur object

- [`rxRemoveControl()`](https://nlmixr2.github.io/rxode2/reference/rxRemoveControl.md)
  : rxRemoveControl options for UI object

- [`rxReservedKeywords`](https://nlmixr2.github.io/rxode2/reference/rxReservedKeywords.md)
  : A list and description of rxode2 supported reserved keywords

- [`rxS()`](https://nlmixr2.github.io/rxode2/reference/rxS.md) : Load a
  model into a symengine environment

- [`rxSetControl()`](https://nlmixr2.github.io/rxode2/reference/rxSetControl.md)
  : rxSetControl options for UI object

- [`rxSetIni0()`](https://nlmixr2.github.io/rxode2/reference/rxSetIni0.md)
  : Set Initial conditions to time zero instead of the first
  observed/dosed time

- [`rxSetProgressBar()`](https://nlmixr2.github.io/rxode2/reference/rxSetProgressBar.md)
  : Set timing for progress bar

- [`rxStack()`](https://nlmixr2.github.io/rxode2/reference/rxStack.md) :
  Stack a solved object for things like default ggplot2 plot

- [`rxSupportedFuns()`](https://nlmixr2.github.io/rxode2/reference/rxSupportedFuns.md)
  : Get list of supported functions

- [`rxSuppressMsg()`](https://nlmixr2.github.io/rxode2/reference/rxSuppressMsg.md)
  : Respect suppress messages

- [`rxSyncOptions()`](https://nlmixr2.github.io/rxode2/reference/rxSyncOptions.md)
  : Sync options with rxode2 variables

- [`rxSyntaxFunctions`](https://nlmixr2.github.io/rxode2/reference/rxSyntaxFunctions.md)
  : A list and description of Rode supported syntax functions

- [`rxTempDir()`](https://nlmixr2.github.io/rxode2/reference/rxTempDir.md)
  : Get the rxode2 temporary directory

- [`rxTheme()`](https://nlmixr2.github.io/rxode2/reference/rxTheme.md) :
  rxTheme is the ggplot2 theme for rxode2 plots

- [`rxToSE()`](https://nlmixr2.github.io/rxode2/reference/rxToSE.md)
  [`.rxToSE()`](https://nlmixr2.github.io/rxode2/reference/rxToSE.md)
  [`rxFromSE()`](https://nlmixr2.github.io/rxode2/reference/rxToSE.md)
  [`.rxFromSE()`](https://nlmixr2.github.io/rxode2/reference/rxToSE.md)
  : rxode2 to symengine environment

- [`rxTrans()`](https://nlmixr2.github.io/rxode2/reference/rxTrans.md) :
  Translate the model to C code if needed

- [`rxUiDeparse()`](https://nlmixr2.github.io/rxode2/reference/rxUiDeparse.md)
  : This is a generic function for deparsing certain objects when
  printing out a rxode2 object. Currently it is used for any
  meta-information

- [`rxUiDevelop()`](https://nlmixr2.github.io/rxode2/reference/rxUiDevelop.md)
  : rxUiDevelop - Enable/Disable rxUi development. Here all \$
  completions are given

- [`rxUiGet()`](https://nlmixr2.github.io/rxode2/reference/rxUiGet.md) :
  S3 for getting information from UI model

- [`rxode2parse()`](https://nlmixr2.github.io/rxode2/reference/rxode2parse.md)
  : Internal translation to get model variables list

- [`rxode2parseAssignTranslation()`](https://nlmixr2.github.io/rxode2/reference/rxode2parseAssignTranslation.md)
  : This assigns the c level linkages for a roxde2 model

- [`rxode2parseD()`](https://nlmixr2.github.io/rxode2/reference/rxode2parseD.md)
  : This gives the derivative table for rxode2

- [`rxode2parseGetPackagesToLoad()`](https://nlmixr2.github.io/rxode2/reference/rxode2parseAssignPackagesToLoad.md)
  [`rxode2parseAssignPackagesToLoad()`](https://nlmixr2.github.io/rxode2/reference/rxode2parseAssignPackagesToLoad.md)
  :

  Control the packages that are loaded when a `rxode2` model dll is
  loaded

- [`rxode2parseGetPointerAssignment()`](https://nlmixr2.github.io/rxode2/reference/rxode2parseGetPointerAssignment.md)
  : This function gets the currently assigned function pointer
  assignments

- [`rxode2parseGetTranslation()`](https://nlmixr2.github.io/rxode2/reference/rxode2parseGetTranslation.md)
  : This function gets the currently assigned translations

- [`summary(`*`<rxode2>`*`)`](https://nlmixr2.github.io/rxode2/reference/summary.rxode2.md)
  : Print expanded information about the rxode2 object.

## Neural Network Activation functions

- [`ELU()`](https://nlmixr2.github.io/rxode2/reference/ELU.md) :
  Exponential Linear Unit (ELU) Activation Function
- [`GELU()`](https://nlmixr2.github.io/rxode2/reference/GELU.md) : GELU
  activation function
- [`PReLU()`](https://nlmixr2.github.io/rxode2/reference/PReLU.md) :
  Parametric ReLU Activation Function
- [`ReLU()`](https://nlmixr2.github.io/rxode2/reference/ReLU.md) :
  Rectified Linear Unit (ReLU) Activation Function
- [`SELU()`](https://nlmixr2.github.io/rxode2/reference/SELU.md) :
  Scaled Exponential Linear Unit (SELU) Activation Function
- [`Swish()`](https://nlmixr2.github.io/rxode2/reference/Swish.md) :
  Switch Activation Function
- [`dELU()`](https://nlmixr2.github.io/rxode2/reference/dELU.md)
  [`d2ELU()`](https://nlmixr2.github.io/rxode2/reference/dELU.md)
  [`d2aELU()`](https://nlmixr2.github.io/rxode2/reference/dELU.md)
  [`dELUa()`](https://nlmixr2.github.io/rxode2/reference/dELU.md)
  [`d2ELUa()`](https://nlmixr2.github.io/rxode2/reference/dELU.md) :
  Derivatives of the Exponential Linear Unit (ELU) Activation Function
- [`dGELU()`](https://nlmixr2.github.io/rxode2/reference/dGELU.md)
  [`d2GELU()`](https://nlmixr2.github.io/rxode2/reference/dGELU.md)
  [`d3GELU()`](https://nlmixr2.github.io/rxode2/reference/dGELU.md)
  [`d4GELU()`](https://nlmixr2.github.io/rxode2/reference/dGELU.md) :
  Derivatives of GELU
- [`dPReLU()`](https://nlmixr2.github.io/rxode2/reference/dPReLU.md)
  [`dPReLUa()`](https://nlmixr2.github.io/rxode2/reference/dPReLU.md)
  [`dPReLUa1()`](https://nlmixr2.github.io/rxode2/reference/dPReLU.md) :
  Derivatives Parametric ReLU Activation Function
- [`dReLU()`](https://nlmixr2.github.io/rxode2/reference/dReLU.md) :
  Derivative of the Rectified Linear Unit (ReLU) Activation Function
- [`dSELU()`](https://nlmixr2.github.io/rxode2/reference/dSELU.md) :
  Derivative of the Scaled Exponential Linear Unit (SELU) Activation
  Function
- [`dSwish()`](https://nlmixr2.github.io/rxode2/reference/dSwish.md) :
  Derivative of the Swish Activation Function
- [`dlReLU()`](https://nlmixr2.github.io/rxode2/reference/dlReLU.md) :
  Derivative of Leaky ReLU activation function
- [`dsoftplus()`](https://nlmixr2.github.io/rxode2/reference/dsoftplus.md)
  [`d2softplus()`](https://nlmixr2.github.io/rxode2/reference/dsoftplus.md)
  [`d3softplus()`](https://nlmixr2.github.io/rxode2/reference/dsoftplus.md)
  [`d4softplus()`](https://nlmixr2.github.io/rxode2/reference/dsoftplus.md)
  : Default Softplus Activation Function
- [`lReLU()`](https://nlmixr2.github.io/rxode2/reference/lReLU.md) :
  Leaky ReLU activation function
- [`softplus()`](https://nlmixr2.github.io/rxode2/reference/softplus.md)
  : Softplus Activation Function

## Rxode2 data sets

- [`rxResidualError`](https://nlmixr2.github.io/rxode2/reference/rxResidualError.md)
  : A description of Rode2 supported residual errors
