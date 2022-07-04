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

  double rxLlikBeta(double* ret, double x, double shape1, double shape2);
  double rxLlikBetaDshape1(double* ret, double x, double shape1, double shape2);
  double rxLlikBetaDshape2(double* ret, double x, double shape1, double shape2);

  double rxLlikT(double* ret, double x, double df, double mean, double sd);
  double rxLlikTDdf(double* ret, double x, double df, double mean, double sd);
  double rxLlikTDmean(double* ret, double x, double df, double mean, double sd);
  double rxLlikTDsd(double* ret, double x, double df, double mean, double sd);

  double rxLlikChisq(double* ret, double x, double df);
  double rxLlikChisqDdf(double* ret, double x, double df);

#if defined(__cplusplus)
}
#endif

#endif // __LLIK_H__
