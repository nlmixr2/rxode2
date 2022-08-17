#ifndef __RXTHREEFRY_H__
#define __RXTHREEFRY_H__

#if defined(__cplusplus)
extern "C" {
#endif

  int rxbinom(rx_solving_options_ind* ind, int n, double prob);
  int rxnbinomMu(rx_solving_options_ind* ind, int size, double mu);
  int rxnbinom(rx_solving_options_ind* ind, int size, double mu);
  double rxcauchy(rx_solving_options_ind* ind, double location, double scale);
  double rxchisq(rx_solving_options_ind* ind, double df);
  double rxexp(rx_solving_options_ind* ind, double rate);
  double rxf(rx_solving_options_ind* ind, double df1, double df2);
  int rxgeom(rx_solving_options_ind* ind, double prob);
  double rxnorm(rx_solving_options_ind* ind, double mean, double sd);
  int rxpois(rx_solving_options_ind* ind, double lambda);
  double rxt_(rx_solving_options_ind* ind, double df);
  double rxunif(rx_solving_options_ind* ind, double low, double hi);
  double rxweibull(rx_solving_options_ind* ind, double shape, double scale);
  double rxgamma(rx_solving_options_ind* ind, double shape, double rate);
  double rxbeta(rx_solving_options_ind* ind, double shape1, double shape2);
  double rxnormV(rx_solving_options_ind* ind, double mean, double sd);
  int ribinom(rx_solving_options_ind* ind, int id, int n, double prob);
  int rinbinomMu(rx_solving_options_ind* ind, int id, int size, double mu);
  int rinbinom(rx_solving_options_ind* ind, int id, int size, double mu);
  double ricauchy(rx_solving_options_ind* ind, int id, double location, double scale);
  double richisq(rx_solving_options_ind* ind, int id, double df);
  double riexp(rx_solving_options_ind* ind, int id, double rate);
  double rif(rx_solving_options_ind* ind, int id, double df1, double df2);
  int rigeom(rx_solving_options_ind* ind, int id, double prob);
  double rinorm(rx_solving_options_ind* ind, int id, double mean, double sd);
  int ripois(rx_solving_options_ind* ind, int id, double lambda);
  double rit_(rx_solving_options_ind* ind, int id, double df);
  double riunif(rx_solving_options_ind* ind, int id, double low, double hi);
  double riweibull(rx_solving_options_ind* ind, int id, double shape, double scale);
  double rigamma(rx_solving_options_ind* ind, int id, double shape, double rate);
  double ribeta(rx_solving_options_ind* ind, int id, double shape1, double shape2);
  double rinormV(rx_solving_options_ind* ind, int id, double mean, double sd);


#if defined(__cplusplus)
}
#endif
#endif // __THREEFRY_H__
