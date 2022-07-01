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

#if defined(__cplusplus)
}
#endif

#endif // __LLIK_H__
