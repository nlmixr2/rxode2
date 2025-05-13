#ifndef __SHI21E_H__
#define __SHI21E_H__
#if defined(__cplusplus)

#include <Rcpp.h>
#include <RcppEigen.h>


typedef Eigen::VectorXd (*shi21efn_type)(Eigen::VectorXd &t, int id);

double shi21eForward(shi21efn_type f, Eigen::VectorXd &t, double &h,
                    Eigen::VectorXd &f0, Eigen::VectorXd &gr, int id, int idx,
                    double ef = 7e-7, double rl = 1.5, double ru = 6.0,
                    int maxiter=15);

double shi21eCentral(shi21efn_type f, Eigen::VectorXd &t, double &h,
                    Eigen::VectorXd &f0, Eigen::VectorXd &gr, int id, int idx,
                    double ef = 7e-7, double rl = 1.5, double ru = 6.0,
                    double nu = 8.0,
                    int maxiter=15);

double shi21eStencil(shi21efn_type f, Eigen::VectorXd &t, double &h,
                    Eigen::VectorXd &f0, Eigen::VectorXd &gr, int id, int idx,
                    double ef, double rl, double ru, double nu,
                    int maxiter);


// 2/sqrt(3)
#define nm2divSqrt3 1.154700538379251684162

#endif
#endif
