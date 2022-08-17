#ifndef __LLIK_H__
#define __LLIK_H__

#if defined(__cplusplus)
extern "C" {
#endif

  double rxLlikNorm(double* ret, double x, double mu, double sigma);
  double rxLlikNormDmean(double* ret, double x, double mu, double sigma);
  double rxLlikNormDsd(double* ret, double x, double mu, double sigma);

  double rxLlikPois(double* ret, double x, double lambda);
  double rxLlikPoisDlambda(double* ret, double x, double lambda);

  double rxLlikBinom(double* ret, double x, double size, double prob);
  double rxLlikBinomDprob(double* ret, double x, double size, double prob);

  double rxLlikNbinomMu(double* ret, double x, double size, double mu);
  double rxLlikNbinomMuDmu(double* ret, double x, double size, double mu);

  double rxLlikNbinom(double* ret, double x, double size, double prob);
  double rxLlikNbinomDprob(double* ret, double x, double size, double prob);

  double rxLlikBeta(double* ret, double x, double shape1, double shape2);
  double rxLlikBetaDshape1(double* ret, double x, double shape1, double shape2);
  double rxLlikBetaDshape2(double* ret, double x, double shape1, double shape2);

  double rxLlikT(double* ret, double x, double df, double mean, double sd);
  double rxLlikTDdf(double* ret, double x, double df, double mean, double sd);
  double rxLlikTDmean(double* ret, double x, double df, double mean, double sd);
  double rxLlikTDsd(double* ret, double x, double df, double mean, double sd);

  double rxLlikChisq(double* ret, double x, double df);
  double rxLlikChisqDdf(double* ret, double x, double df);

  double rxLlikExp(double* ret, double x, double rate);
  double rxLlikExpDrate(double* ret, double x, double rate);

  double rxLlikF(double* ret, double x,  double df1, double df2);
  double rxLlikFDdf1(double* ret, double x, double df1, double df2);
  double rxLlikFDdf2(double* ret, double x, double df1, double df2);

  double rxLlikGeom(double* ret, double x, double p);
  double rxLlikGeomDp(double* ret, double x, double p);

  double rxLlikUnif(double* ret, double x, double alpha, double beta);
  double rxLlikUnifDalpha(double* ret, double x, double alpha, double beta);
  double rxLlikUnifDbeta(double* ret, double x, double alpha, double beta);

  double rxLlikWeibull(double* ret, double x, double shape, double scale);
  double rxLlikWeibullDshape(double* ret, double x, double shape, double scale);
  double rxLlikWeibullDscale(double* ret, double x, double shape, double scale);

  double rxLlikGamma(double* ret, double x, double shape, double rate);
  double rxLlikGammaDshape(double* ret, double x, double shape, double rate);
  double rxLlikGammaDrate(double* ret, double x, double shape, double rate);

  double rxLlikCauchy(double* ret, double x, double location, double scale);
  double rxLlikCauchyDlocation(double* ret, double x, double location, double scale);
  double rxLlikCauchyDscale(double* ret, double x, double location, double scale);

#if defined(__cplusplus)
}
#endif

#endif // __LLIK_H__
