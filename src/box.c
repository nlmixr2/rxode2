#define USE_FC_LEN_T
#include "../inst/include/rxode2.h"

double powerDi(double x, double lambda, int yj){
  return _powerDi(x, lambda, yj, 0, 1);
}

double powerD(double x, double lambda, int yj){
  return _powerD(x, lambda, yj, 0, 1);
}

double powerDD(double x, double lambda, int yj){
  return _powerDD(x, lambda, yj, 0, 1);
}

double powerDDD(double x, double lambda, int yj){
  return _powerDDD(x, lambda, yj, 0, 1);
}

double powerL(double x, double lambda, int yj){
  return _powerL(x, lambda, yj, 0, 1);
}

double powerDL(double x, double lambda, int yj){
  return _powerDL(x, lambda, yj, 0, 1);
}
