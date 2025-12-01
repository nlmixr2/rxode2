#ifndef __RXTHREEFRY_H__
#define __RXTHREEFRY_H__

#if defined(__cplusplus)
extern "C" {
#endif

  int rxbinom(void* indv, int n, double prob);
  int rxnbinomMu(void* indv, int size, double mu);
  int rxnbinom(void* indv, int size, double mu);
  double rxcauchy(void* indv, double location, double scale);
  double rxchisq(void* indv, double df);
  double rxexp(void* indv, double rate);
  double rxf(void* indv, double df1, double df2);
  int rxgeom(void* indv, double prob);
  double rxnorm(void* indv, double mean, double sd);
  int rxpois(void* indv, double lambda);
  double rxt_(void* indv, double df);
  double rxunif(void* indv, double low, double hi);
  double rxweibull(void* indv, double shape, double scale);
  double rxgamma(void* indv, double shape, double rate);
  double rxbeta(void* indv, double shape1, double shape2);
  double rxnormV(void* indv, double mean, double sd);
  int ribinom(void* indv, int id, int n, double prob);
  int rinbinomMu(void* indv, int id, int size, double mu);
  int rinbinom(void* indv, int id, int size, double mu);
  double ricauchy(void* indv, int id, double location, double scale);
  double richisq(void* indv, int id, double df);
  double riexp(void* indv, int id, double rate);
  double rif(void* indv, int id, double df1, double df2);
  int rigeom(void* indv, int id, double prob);
  double rinorm(void* indv, int id, double mean, double sd);
  int ripois(void* indv, int id, double lambda);
  double rit_(void* indv, int id, double df);
  double riunif(void* indv, int id, double low, double hi);
  double riweibull(void* indv, int id, double shape, double scale);
  double rigamma(void* indv, int id, double shape, double rate);
  double ribeta(void* indv, int id, double shape1, double shape2);
  double rinormV(void* indv, int id, double mean, double sd);

  void simeta(int id);
  void simeps(int id);
  double phi(double qn);

#if defined(__cplusplus)

}
#endif
#endif // __THREEFRY_H__
