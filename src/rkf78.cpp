#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#define BOOST_MATH_DOMAIN_ERROR_POLICY ignore_error
#define BOOST_MATH_POLE_ERROR_POLICY ignore_error
#define BOOST_MATH_OVERFLOW_ERROR_POLICY ignore_error
#define BOOST_MATH_UNDERFLOW_ERROR_POLICY ignore_error
#define BOOST_MATH_DENORM_ERROR_POLICY ignore_error
#define BOOST_MATH_EVALUATION_ERROR_POLICY ignore_error
#define BOOST_MATH_INDETERMINATE_RESULT_ERROR_POLICY ignore_error

#include "rxomp.h"
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string>
#include <vector>
#include <algorithm>
#include <boost/numeric/odeint.hpp>

#include "strncmp.h"
#include "timsort.h"
#include "../inst/include/rxode2.h"
#include "../inst/include/rxode2dataErr.h"
#include "../inst/include/rxode2parseHandleEvid.h"
#include "../inst/include/rxode2parseGetTime.h"
#include "../inst/include/rxode2EventTranslate.h"

extern "C" uint32_t getRxSeed1(int ncores);
extern "C" void setSeedEng1(uint32_t seed);

#include "common.h"
#include "lsoda.h"
#include "rxode2_df.h"
#define SORT gfx::timsort
#include "par_solve.h"

extern t_calc_jac calc_jac;
extern t_dydt dydt;
extern t_update_inis update_inis;

// Zero-copy state wrapper to avoid memory allocations for the main state vector `yp`
// while allowing `odeint` to allocate and manage intermediate states via `resize()`.
struct zero_copy_state {
    typedef double value_type;
    typedef double* iterator;
    typedef const double* const_iterator;
    typedef double& reference;
    typedef const double& const_reference;
    typedef size_t size_type;
    typedef ptrdiff_t difference_type;

    double* data_;
    size_t size_;
    bool owned_;

    zero_copy_state() : data_(nullptr), size_(0), owned_(true) {}
    
    // Create an owned state (used by odeint for internal stages like k1, k2, etc.)
    explicit zero_copy_state(size_t n) : data_(new double[n]), size_(n), owned_(true) {}
    
    // Create a zero-copy wrapper around an existing pointer (used for the main state `yp`)
    zero_copy_state(double* p, size_t n) : data_(p), size_(n), owned_(false) {}

    ~zero_copy_state() { if (owned_ && data_) delete[] data_; }

    zero_copy_state(const zero_copy_state& other) : data_(nullptr), size_(0), owned_(true) {
        if (other.size_ > 0) {
            data_ = new double[other.size_];
            size_ = other.size_;
            std::copy(other.data_, other.data_ + size_, data_);
        }
    }
    
    zero_copy_state& operator=(const zero_copy_state& other) {
        if (this != &other) {
            if (owned_ && size_ != other.size_) {
                delete[] data_;
                data_ = nullptr;
                size_ = 0;
            }
            if (!data_ && other.size_ > 0) {
                data_ = new double[other.size_];
                size_ = other.size_;
                owned_ = true;
            }
            if (other.size_ > 0) {
                std::copy(other.data_, other.data_ + other.size_, data_);
            }
        }
        return *this;
    }

    double& operator[](size_t i) { return data_[i]; }
    const double& operator[](size_t i) const { return data_[i]; }

    size_t size() const { return size_; }

    double* begin() { return data_; }
    double* end() { return data_ + size_; }
    const double* begin() const { return data_; }
    const double* end() const { return data_ + size_; }

    void resize(size_t n) {
        if (size_ == n) return;
        if (owned_ && data_) delete[] data_;
        data_ = new double[n];
        size_ = n;
        owned_ = true;
    }
};

namespace boost { namespace numeric { namespace odeint {
    template<> struct is_resizeable<zero_copy_state> {
        typedef std::true_type type;
        const static bool value = true;
    };
}}}

// Functor adapter to connect rxode2's t_dydt callback to boost::numeric::odeint
struct rxode2_system {
    t_dydt dydt_;
    int neq_;

    rxode2_system(t_dydt dydt, int neq) : dydt_(dydt), neq_(neq) {}

    void operator()(const zero_copy_state& x, zero_copy_state& dxdt, const double t) {
        dydt_(&neq_, t, const_cast<double*>(x.begin()), dxdt.begin());
    }
};

// Error observer for tracking intermediate errors, similar to what rxode2 expects
struct error_checker {
    rx_solving_options_ind *ind_;
    int* rc_;

    error_checker(rx_solving_options_ind* ind, int* rc) : ind_(ind), rc_(rc) {}

    void operator()(const zero_copy_state& x, const double t) {
        if (ind_->err != 0) {
            *rc_ = -2019; // Signals a bad solve
        }
    }
};

// ================================================================================
// rkf78 implementation for a single subject
extern "C" void ind_rkf78_0(rx_solve *rx, rx_solving_options *op, int solveid, int *neq,
                            t_dydt c_dydt, t_update_inis u_inis) {
  
  clock_t t0 = clock();
  int i;
  
  // Use rx->ordId for sorting based on complexity/time if parallel
  neq[1] = rx->ordId[solveid]-1;
  rx_solving_options_ind *ind = &(rx->subjects[neq[1]]);
  
  // Per-individual effective neq
  neq[0] = rxEffNeq(ind, op);
  double xout;
  int *rc;
  int neqOde = *neq - op->numLin - op->numLinSens;
  
  if (!iniSubject(neq[1], 0, ind, op, rx, u_inis)) {
    return;
  }
  
  rc = ind->rc;
  double xp = ind->all_times[0];
  
  ind->solvedIdx = 0;
  
  typedef boost::numeric::odeint::runge_kutta_fehlberg78<zero_copy_state> error_stepper_type;
  
  // Create our controlled stepper
  auto stepper = boost::numeric::odeint::make_controlled(
      ind->atol2[0], ind->rtol2[0], error_stepper_type());
      
  rxode2_system sys(c_dydt, neqOde);
  
  for(i = 0; i < ind->n_all_times; i++) {
    xout = ind->all_times[i];
    int evid = ind->evid[i];
    
    // yp points to the start of the solution memory for this individual/timepoint
    double *yp = getSolve(i);
    
    if (i == 0) {
      if (rx->simflg) {
        c_dydt(neq, xout, ind->solve, ind->solve);
      }
      memcpy(yp, ind->solve, neq[0]*sizeof(double));
      if (ind->inLhs == 0 && neq[0] > 0) {
        if (calc_jac) calc_jac(neq, xout, yp, NULL, neq[0]);
      }
      if (rx->nobs2 == 0) {
        // LHS-only model or no observations, skip solver
        continue;
      }
    }
    
    zero_copy_state state(yp, neqOde);
    
    if (evid == 0 || evid == 2 || evid == 9) {
      if (!isSameTimeOp(xout, xp) && xout > xp) {
        if (neqOde > 0) {
            double dt = op->HMIN > 0.0 ? op->HMIN : 1e-4; // default initial step size if hmin is 0
            
            try {
                boost::numeric::odeint::integrate_adaptive(
                    stepper, sys, state, xp, xout, dt, error_checker(ind, rc));
            } catch(...) {
                *rc = -2019;
            }
            
            if (*rc < 0 || ind->err) {
                // Handle bad solve
                for (int j = rxEffNeq(ind, op)*(ind->n_all_times); j--;){ 
                    ind->solve[j] = NA_REAL;
                }
                _Pragma("omp atomic write") 
                op->badSolve = 1;
                i = ind->n_all_times-1; // Get out of here!
                break;
            }
            
            // postSolve trim bounds
            if (R_FINITE(rx->stateTrimU)){
                double top = fabs(rx->stateTrimU);
                for (int j = rxEffNeq(ind, op); j--;) {
                    yp[j] = (top < yp[j]) ? top : yp[j];
                }
            }
            if (R_FINITE(rx->stateTrimL)){
                double bottom = rx->stateTrimL;
                for (int j = rxEffNeq(ind, op); j--;) {
                    yp[j] = (bottom > yp[j]) ? bottom : yp[j];
                }
            }
        }
        xp = xout;
      }
    } else if (evid == 3) {
      // Reset
      int idid = 0;
      handleEvid3(ind, op, rx, neq, &xp, &xout, yp, &idid, u_inis);
    } else if (evid == 4) {
      // Reset and dose
      int idid = 0;
      handleEvid3(ind, op, rx, neq, &xp, &xout, yp, &idid, u_inis);
      handle_evidL(evid, yp, xout, ind->id, ind);
    } else {
      // Dose
      handle_evidL(evid, yp, xout, ind->id, ind);
    }
    
    if (op->numLin > 0) {
      // Evaluate linear compartment equations if present
      memcpy(ind->linCmtDummy, yp, op->neq*sizeof(double));
      c_dydt(neq, xout, ind->linCmtDummy, ind->linCmtDummy);
      memcpy(yp + op->linOffset, ind->linCmtSave, (op->numLin + op->numLinSens)*sizeof(double));
    }
    
    // Check extra doses
    int idxExtra = ind->idxExtra;
    int extraDoseN = ind->extraDoseN[0];
    if (idxExtra < extraDoseN) {
      // Simplified extra dose check; just ensuring basic progression
      ind->idx = i;
      ind->ixds = i;
    }
    
    ind->solvedIdx = i;
  }
  ind->solveTime += ((double)(clock() - t0))/CLOCKS_PER_SEC;
}

extern "C" void ind_rkf78(rx_solve *rx, int solveid,
                          t_dydt c_dydt, t_update_inis u_inis){
  rx_solving_options *op = rx->op;
  int neq[2];
  neq[0] = op->neq;
  neq[1] = 0;
  ind_rkf78_0(rx, op, solveid, neq, c_dydt, u_inis);
}

void par_rkf78(rx_solve *rx){
  rx_solving_options *op = rx->op;
#ifdef _OPENMP
  int cores = op->cores;
#else
  int cores = 1;
#endif
  uint32_t nsub = rx->nsub, nsim = rx->nsim;
  int nsolve = (int)(nsim*nsub);
  
  uint32_t seed0 = getRxSeed1(cores);
  int abort = 0;

#ifdef _OPENMP
#pragma omp parallel for num_threads(cores)
#endif
  for (int solveid = 0; solveid < nsolve; solveid++){
    int neq[2];
    neq[0] = op->neq;
    neq[1] = 0;
    int localAbort;
#pragma omp atomic read
    localAbort = abort;
    if (localAbort == 0){
      setSeedEng1(seed0 + rx->ordId[solveid] - 1);
      ind_rkf78_0(rx, op, solveid, neq, dydt, update_inis);
      
      if (op->badSolve) {
#pragma omp atomic write
        abort = 1;
      }
    }
  }
}
