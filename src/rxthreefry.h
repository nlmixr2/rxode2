#ifndef __RXTHREEFRY_H__
#define __RXTHREEFRY_H__

#if defined(__cplusplus)
extern "C" {
#endif

  double rxbinom(int n, double prob);
  double rxnbinomMu(int size, double mu);
  double rxnbinom(int size, double mu);
  double rxcauchy(double location, double scale);
  double rxchisq(double df);
  double rxexp(double rate);
  double rxf(double df1, double df2);
  double rxgeom(double prob);
  double rxnorm(double mean, double sd);
  double rxpois(double lambda);
  double rxt_(double df);
  double rxunif(double low, double hi);
  double rxweibull(double shape, double scale);
  double rxgamma(double shape, double rate);
  double rxbeta(double shape1, double shape2);
  double rxnormV(double mean, double sd);
  double ribinom(int id, int n, double prob);
  double rinbinomMu(int id, int size, double mu);
  double rinbinom(int id, int size, double mu);
  double ricauchy(int id, double location, double scale);
  double richisq(int id, double df);
  double riexp(int id, double rate);
  double rif(int id, double df1, double df2);
  double rigeom(int id, double prob);
  double rinorm(int id, double mean, double sd);
  double ripois(int id, double lambda);
  double rit_(int id, double df);
  double riunif(int id, double low, double hi);
  double riweibull(int id, double shape, double scale);
  double rigamma(int id, double shape, double rate);
  double ribeta(int id, double shape1, double shape2);
  double rinormV(int id, double mean, double sd);

  void simeta(void);
  void simeps(void);
  double phi(double qn);
  void _setThreadInd(int cid);

#if defined(__cplusplus)

}
#endif
#endif // __THREEFRY_H__
