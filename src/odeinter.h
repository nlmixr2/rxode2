#ifndef ODEINTER_H
#define ODEINTER_H

#include <boost/numeric/odeint.hpp>
#include <vector>
#include <stdexcept>

struct zero_copy_state {
    double* data_;
    size_t size_;
    std::vector<double> owned_data_;
    bool owned_;

    zero_copy_state() : data_(nullptr), size_(0), owned_(true) {}

    zero_copy_state(size_t size) : size_(size), owned_data_(size, 0.0), owned_(true) {
        data_ = owned_data_.data();
    }
    
    zero_copy_state(double* data, size_t size) : data_(data), size_(size), owned_(false) {}
    
    zero_copy_state(const zero_copy_state& other) : size_(other.size_), owned_(true) {
        if (other.size_ > 0 && other.data_) {
            owned_data_.assign(other.data_, other.data_ + other.size_);
            data_ = owned_data_.data();
        } else {
            data_ = nullptr;
        }
    }

    zero_copy_state& operator=(const zero_copy_state& other) {
        if (this != &other) {
            size_ = other.size_;
            if (owned_) {
                if (other.size_ > 0 && other.data_) {
                    owned_data_.assign(other.data_, other.data_ + other.size_);
                    data_ = owned_data_.data();
                } else {
                    owned_data_.clear();
                    data_ = nullptr;
                }
            } else {
                if (size_ > 0 && data_ && other.data_) {
                    std::copy(other.data_, other.data_ + size_, data_);
                }
            }
        }
        return *this;
    }

    ~zero_copy_state() {}

    typedef double value_type;
    typedef double* iterator;
    typedef const double* const_iterator;
    typedef double& reference;
    typedef const double& const_reference;

    double& operator[](size_t i) { return data_[i]; }
    const double& operator[](size_t i) const { return data_[i]; }
    
    double* begin() { return data_; }
    double* end() { return data_ + size_; }
    const double* begin() const { return data_; }
    const double* end() const { return data_ + size_; }
    
    size_t size() const { return size_; }
    
    void resize(size_t n) {
        if (size_ == n) return;
        if (owned_) {
            owned_data_.resize(n, 0.0);
            data_ = owned_data_.data();
            size_ = n;
        } else {
            // Cannot resize unowned state
        }
    }
};

namespace boost { namespace numeric { namespace odeint {
template<>
struct is_resizeable<zero_copy_state> {
    typedef std::true_type type;
    const static bool value = true;
};
}}}

namespace boost {
    template<> struct range_mutable_iterator<zero_copy_state> { typedef double* type; };
    template<> struct range_const_iterator<zero_copy_state> { typedef const double* type; };
}

struct rxode2_system {
    t_dydt dydt_;
    int* neq_;
    rx_solving_options_ind* ind_;
    bool cap_t_;
    double xout_;
    int sign_;

    rxode2_system(t_dydt dydt, int* neq, rx_solving_options_ind* ind, bool cap_t = false, double xout = 0.0, int sign = 1) 
        : dydt_(dydt), neq_(neq), ind_(ind), cap_t_(cap_t), xout_(xout), sign_(sign) {}

    template <class State, class Deriv>
    void operator()(const State& x, Deriv& dxdt, const double t) {
        dxdt.resize(x.size());
        if (ind_->err != 0) {
            for(size_t i=0; i<dxdt.size(); ++i) dxdt[i] = 0.0;
            return;
        }
        double teval = t;
        if (cap_t_) {
            if (sign_ == 1 && t > xout_ - 2.0e-7) {
                teval = xout_ - 2.0e-7;
                if (teval >= xout_) teval = xout_ - 3.0e-7;
            } else if (sign_ == -1 && t < xout_ + 2.0e-7) {
                teval = xout_ + 2.0e-7;
                if (teval <= xout_) teval = xout_ + 3.0e-7;
            }
        }
        dydt_(neq_, teval, const_cast<double*>(&x[0]), &dxdt[0]);
    }
};

struct rxode2_system_jac_second {
  rx_solving_options_ind *ind;
  t_calc_jac calc_jac;
  int *neq;
  int neqOde;
  int nRowPd;

  rxode2_system_jac_second(rx_solving_options_ind *ind_in, t_calc_jac calc_jac_in, int *neq_in) : 
    ind(ind_in), calc_jac(calc_jac_in), neq(neq_in), neqOde(neq_in[0]) {
    nRowPd = neqOde; 
  }

  template<class StateType, class MatrixType>
  void operator()(const StateType& x, MatrixType& jacobi, const double t) const {
    unsigned int nrowpd = neqOde;
    if (calc_jac != NULL) {
      calc_jac(neq, t, const_cast<double*>(&x[0]), &jacobi.data()[0], nrowpd);
    }
  }
};

struct rxode2_system_ros4_second {
  rx_solving_options_ind *ind;
  t_calc_jac calc_jac;
  int *neq;
  int neqOde;
  
  rxode2_system_ros4_second(rx_solving_options_ind *ind_in, t_calc_jac calc_jac_in, int *neq_in) :
    ind(ind_in), calc_jac(calc_jac_in), neq(neq_in), neqOde(neq_in[0]) {
  }

  template<class StateType, class MatrixType, class DfdtType>
  void operator()(const StateType& x, MatrixType& jacobi, const double t, DfdtType& dfdt) const {
    unsigned int nrowpd = neqOde;
    if (calc_jac != NULL) {
      calc_jac(neq, t, const_cast<double*>(&x[0]), &jacobi.data()[0], nrowpd);
    }
    
    // Set dfdt to zero because rxode2 doesn't calculate explicit time derivatives
    for(size_t i=0; i<dfdt.size(); ++i) {
      dfdt[i] = 0.0;
    }
  }
};

inline std::pair<rxode2_system, rxode2_system_ros4_second>
make_rxode2_system_ros4(rx_solving_options_ind* ind, t_dydt calc_dydt, t_calc_jac calc_jac, int* neq) {
    return std::make_pair(
        rxode2_system(calc_dydt, neq, ind),
        rxode2_system_ros4_second(ind, calc_jac, neq)
    );
}

// For implicit_euler, the Jacobian is called with 3 args: (state, matrix, time)
// rxode2_system_jac_second already has this signature.
inline std::pair<rxode2_system, rxode2_system_jac_second>
make_rxode2_system_iem(rx_solving_options_ind* ind, t_dydt calc_dydt, t_calc_jac calc_jac, int* neq) {
    return std::make_pair(
        rxode2_system(calc_dydt, neq, ind),
        rxode2_system_jac_second(ind, calc_jac, neq)
    );
}

struct error_checker {
    rx_solving_options_ind *ind_;
    int* rc_;
    int max_steps_;
    int steps_;

    error_checker(rx_solving_options_ind* ind, int* rc, int max_steps) : ind_(ind), rc_(rc), max_steps_(max_steps), steps_(0) {}

    template <class State>
    void operator()(const State& x, const double t) {
        steps_++;
        if (steps_ > max_steps_) {
            *rc_ = -2019;
            ind_->err = 1;
        } else if (ind_->err != 0) {
            *rc_ = -2019;
        }
    }
};

#endif // ODEINTER_H
