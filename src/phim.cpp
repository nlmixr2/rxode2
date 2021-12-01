//#undef NDEBUG
#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include <Rcpp.h>
#include <RcppEigen.h>
#include "../inst/include/rxode2.h"
// #include <stan/math.hpp>
#include <stan/math.hpp>
using namespace Eigen;
using namespace std;

double* matrixExp(double *Hin, int n, double t, int& type,
		  int& order);

MatrixXd matrixExp(MatrixXd H, double t, int& type, int &order){
  // FIXME allow type to use STAN
  int n = H.cols();
  double *ret0 = matrixExp(H.data(), n, t, type, order);
  MatrixXd ret = Map<MatrixXd>(ret0, H.cols(), H.cols());
  Free(ret0);
  return ret;
}

template <typename T>
Matrix<T, Dynamic, 1> phiv3( double t, Matrix<T, Dynamic, Dynamic> A, VectorXd u, VectorXd v, ) {

  int n = A.rows(), mxrej=100, mb=m, k1=3;
  double btol=1.0e-7, gamma=0.9, delta=1.2;
  double t_out=fabs(t), t_new=0, t_now=0, s_error=0;
  double eps=std::numeric_limits<double>::epsilon();
  double rndoff=anorm*eps; if(tol < eps) tol = sqrt(eps);

  int mh=m+3;
  MatrixXd H0=MatrixXd::Zero(mh,mh);
  Matrix<T, Dynamic, 1> w = v;
  for (int istep = 0; t_now < t_out; ++istep) {
    double xm=1.0/double(m);
    double err_loc, avnorm;
    double s;
    T h;
    int i, j, mx;
    Matrix<T, Dynamic, Dynamic> F, V(n,m+1), H;
    H = H0;

    V.col(0) = A*w + u;
    T beta = V.col(0).norm(); 
    double beta_v = stan::math::value_of(beta);
    if (beta_v==0) break;
    V.col(0) /= beta;
    if (istep == 0) {
       double fact = (std::pow((m+1)/exp(1.0), m+1))*sqrt(2*3.14*(m+1));
       t_new = (1.0/anorm)*std::pow((fact*tol)/(4*beta_v*anorm),xm);
       s = std::pow(10.0,std::floor(log10(t_new))-1.0);
       t_new = std::ceil(t_new/s)*s;
    }
    double t_step = fmin( t_out-t_now,t_new );
    for (j = 0; j < m; ++j) {
       Matrix<T, Dynamic, 1> p = A*V.col(j);
       for (i = 0; i <= j; ++i) {
          H(i,j) = V.col(i).dot(p);
          p -= H(i,j)*V.col(i);
       }
       T s = p.norm();
       if (stan::math::value_of(s) < btol) {
          k1 = 0;
          mb = j;
          t_step = t_out-t_now;
          break;
       }
       H(j+1,j) = s;
       V.col(j+1) = p/s;
    }
    H(0,mb) = 1;
    if (k1 != 0) {
       H(m,m+1) = 1; H(m+1,m+2) = 1;
       h = H(m,m-1); H(m,m-1) = 0;
       Matrix<T, Dynamic, 1> avv=A*V.col(m);
       VectorXd av=stan::math::value_of(avv);
       avnorm = av.norm();
    }
    for (int irej = 0; irej < mxrej; ++irej) {
       //cout << t_step << endl;
       mx = mb + max(1,k1);
       Matrix<T, Dynamic, Dynamic> Ht = t_step*H.block(0,0,mx,mx);
       F = stan::math::matrix_exp(Ht);
       if (k1 == 0) {
          err_loc = btol;
          break;
       } else {
          F(m  ,m) = h*F(m-1,m+1);
          F(m+1,m) = h*F(m-1,m+2);
          double p1 = fabs( beta_v*stan::math::value_of(F(m,  m)) );
          double p2 = fabs( beta_v*stan::math::value_of(F(m+1,m)) * avnorm );
          if (p1 > 10.0*p2) {
             err_loc = p2;
             xm = 1.0/double(m+1);
          } else if (p1 > p2) {
             err_loc = (p1*p2)/(p1-p2);
             xm = 1.0/double(m+1);
          } else {
             err_loc = p1;
             xm = 1.0/double(m);
          }
       }
       if (err_loc <= delta*t_step*tol)
          break;
       else {
          t_step = gamma * t_step * std::pow(t_step*tol/err_loc, xm);
          double s = std::pow(10.0, std::floor(log10(t_step))-1.0);
          t_step = std::ceil(t_step/s) * s;
          if (irej == mxrej)
             cout << "The requested tolerance is too high.\n";
       }
    }
    mx = mb + max( 0,k1-2 );
    w += V.block(0, 0, n, mx)*(beta*F.block(0, mb, mx, 1));

    t_now += t_step;
    t_new = gamma * t_step * std::pow(t_step*tol/err_loc, xm);
    s = std::pow(10.0, std::floor(log10(t_new))-1.0);
    t_new = std::ceil(t_new/s) * s;

    err_loc = std::fmax(err_loc, rndoff);
    s_error += err_loc;
  }
  
  return w;
  //cout << w.adjoint() << endl;

}

