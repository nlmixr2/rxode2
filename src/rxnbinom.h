#include <iosfwd>

#include <boost/limits.hpp>
#include <boost/random/detail/config.hpp>
#include <boost/random/gamma_distribution.hpp>
#include <boost/random/poisson_distribution.hpp>

// This redoes the boost negative binomial distribution to use the gamma-poisson mixture
// with real size, just like R.

namespace boost {
  namespace random {

    // This is the same as boost's below
    //
    // https://www.boost.org/doc/libs/1_76_0/boost/random/negative_binomial_distribution.hpp
    //
    // with the following exceptions:
    //
    // - The size parameter is a RealType
    // - Uses the same conditions as R to return 0 or NaN without random draws

    /**
     * The negative binomial distribution is an integer valued
     * distribution with two parameters, @c k and @c p.  The
     * distribution produces non-negative values.
     *
     * The distribution function is
     * \f$\displaystyle P(i) = {k+i-1\choose i}p^k(1-p)^i\f$.
     *
     * This implementation uses a gamma-poisson mixture.
     */
    template<class IntType = int, class RealType = double>
      class negative_binomial_distribution_r {
    public:
    typedef IntType result_type;
    typedef RealType input_type;

    class param_type {
    public:
      typedef negative_binomial_distribution_r distribution_type;
      /**
       * Construct a param_type object.  @c k and @c p
       * are the parameters of the distribution.
       *
       * Requires: k >=0 && 0 <= p <= 1
       */
      explicit param_type(RealType k_arg = 1, RealType p_arg = RealType (0.5))
        : _k(k_arg), _p(p_arg)
        {}
      /** Returns the @c k parameter of the distribution. */
      RealType k() const { return _k; }
      /** Returns the @c p parameter of the distribution. */
      RealType p() const { return _p; }
#ifndef BOOST_RANDOM_NO_STREAM_OPERATORS
      /** Writes the parameters of the distribution to a @c std::ostream. */
      template<class CharT, class Traits>
        friend std::basic_ostream<CharT,Traits>&
        operator<<(std::basic_ostream<CharT,Traits>& os,
                   const param_type& parm)
        {
          os << parm._p << " " << parm._k;
          return os;
        }

      /** Reads the parameters of the distribution from a @c std::istream. */
      template<class CharT, class Traits>
        friend std::basic_istream<CharT,Traits>&
        operator>>(std::basic_istream<CharT,Traits>& is, param_type& parm)
        {
          is >> parm._p >> std::ws >> parm._k;
          return is;
        }
#endif
      /** Returns true if the parameters have the same values. */
      friend bool operator==(const param_type& lhs, const param_type& rhs)
        {
          return lhs._k == rhs._k && lhs._p == rhs._p;
        }
      /** Returns true if the parameters have different values. */
      friend bool operator!=(const param_type& lhs, const param_type& rhs)
        {
          return !(lhs == rhs);
        }
    private:
      RealType _k;
      RealType _p;
    };

    /**
     * Construct a @c negative_binomial_distribution_r object. @c k and @c p
     * are the parameters of the distribution.
     *
     * Requires: k >=0 && 0 <= p <= 1
     */
    explicit negative_binomial_distribution_r(RealType k_arg = 1,
                                              RealType p_arg = RealType(0.5))
    : _k(k_arg), _p(p_arg)
    {}

    /**
     * Construct an @c negative_binomial_distribution_r object from the
     * parameters.
     */
    explicit negative_binomial_distribution_r(const param_type& parm)
    : _k(parm.k()), _p(parm.p())
    {}

    /**
     * Returns a random variate distributed according to the
     * negative binomial distribution.
     */
    template<class URNG>
    IntType operator()(URNG& urng) const
    {
      if(!R_FINITE(_p) || ISNAN(_k) || _k <= 0 || _p <= 0 || _p > 1) {
        return std::numeric_limits<IntType>::quiet_NaN();
      }
      if (_p == 1) return 0;
      IntType k = _k;
      if(!R_FINITE(_k)) k = DBL_MAX / 2.;
      gamma_distribution<RealType> gamma(k, (1-_p)/_p);
      poisson_distribution<IntType, RealType> poisson(gamma(urng));
      return poisson(urng);
    }

    /**
     * Returns a random variate distributed according to the negative
     * binomial distribution with parameters specified by @c param.
     */
    template<class URNG>
    IntType operator()(URNG& urng, const param_type& parm) const
    {
      return negative_binomial_distribution_r(parm)(urng);
    }

    /** Returns the @c k parameter of the distribution. */
    RealType k() const { return _k; }
    /** Returns the @c p parameter of the distribution. */
    RealType p() const { return _p; }

    /** Returns the smallest value that the distribution can produce. */
    IntType min BOOST_PREVENT_MACRO_SUBSTITUTION() const { return 0; }
    /** Returns the largest value that the distribution can produce. */
    IntType max BOOST_PREVENT_MACRO_SUBSTITUTION() const
    { return (std::numeric_limits<IntType>::max)(); }

    /** Returns the parameters of the distribution. */
    param_type param() const { return param_type(_k, _p); }
    /** Sets parameters of the distribution. */
    void param(const param_type& parm)
    {
      _k = parm.k();
      _p = parm.p();
    }

    /**
     * Effects: Subsequent uses of the distribution do not depend
     * on values produced by any engine prior to invoking reset.
     */
    void reset() { }

#ifndef BOOST_RANDOM_NO_STREAM_OPERATORS
    /** Writes the parameters of the distribution to a @c std::ostream. */
    template<class CharT, class Traits>
    friend std::basic_ostream<CharT,Traits>&
    operator<<(std::basic_ostream<CharT,Traits>& os,
               const negative_binomial_distribution_r& bd)
    {
      os << bd.param();
      return os;
    }

    /** Reads the parameters of the distribution from a @c std::istream. */
    template<class CharT, class Traits>
    friend std::basic_istream<CharT,Traits>&
    operator>>(std::basic_istream<CharT,Traits>& is,
               negative_binomial_distribution_r& bd)
    {
      bd.read(is);
      return is;
    }
#endif

    /** Returns true if the two distributions will produce the same
        sequence of values, given equal generators. */
    friend bool operator==(const negative_binomial_distribution_r& lhs,
                           const negative_binomial_distribution_r& rhs)
    {
      return lhs._k == rhs._k && lhs._p == rhs._p;
    }
    /** Returns true if the two distributions could produce different
        sequences of values, given equal generators. */
    friend bool operator!=(const negative_binomial_distribution_r& lhs,
                           const negative_binomial_distribution_r& rhs)
    {
      return !(lhs == rhs);
    }

    private:

    /// @cond \show_private

    template<class CharT, class Traits>
    void read(std::basic_istream<CharT, Traits>& is) {
      param_type parm;
      if(is >> parm) {
        param(parm);
      }
    }

    // parameters
    RealType _k;
    RealType _p;

    /// @endcond
    };


    /// Now the mu distribution
    /**
     * The negative binomial distribution is an integer valued
     * distribution with two parameters, @c k and @c p.  The
     * distribution produces non-negative values.
     *
     * The distribution function is
     * \f$\displaystyle P(i) = {k+i-1\choose i}p^k(1-p)^i\f$.
     *
     * This implementation uses a gamma-poisson mixture.
     */
    template<class IntType = int, class RealType = double>
    class negative_binomial_distribution_mu {
    public:
      typedef IntType result_type;
      typedef RealType input_type;

      class param_type {
      public:
        typedef negative_binomial_distribution_mu distribution_type;
        /**
         * Construct a param_type object.  @c k and @c mu
         * are the parameters of the distribution.
         *
         * Requires: k >=0
         */
        explicit param_type(RealType k_arg = 1, RealType mu_arg = RealType (0.5))
          : _k(k_arg), _mu(mu_arg)
        {}
        /** Returns the @c k parameter of the distribution. */
        RealType k() const { return _k; }
        /** Returns the @c mu parameter of the distribution. */
        RealType mu() const { return _mu; }
#ifndef BOOST_RANDOM_NO_STREAM_OPERATORS
        /** Writes the parameters of the distribution to a @c std::ostream. */
        template<class CharT, class Traits>
        friend std::basic_ostream<CharT,Traits>&
        operator<<(std::basic_ostream<CharT,Traits>& os,
                   const param_type& parm)
        {
          os << parm._mu << " " << parm._k;
          return os;
        }

        /** Reads the parameters of the distribution from a @c std::istream. */
        template<class CharT, class Traits>
        friend std::basic_istream<CharT,Traits>&
        operator>>(std::basic_istream<CharT,Traits>& is, param_type& parm)
        {
          is >> parm._mu >> std::ws >> parm._k;
          return is;
        }
#endif
        /** Returns true if the parameters have the same values. */
        friend bool operator==(const param_type& lhs, const param_type& rhs)
        {
          return lhs._k == rhs._k && lhs._mu == rhs._mu;
        }
        /** Returns true if the parameters have different values. */
        friend bool operator!=(const param_type& lhs, const param_type& rhs)
        {
          return !(lhs == rhs);
        }
      private:
        RealType _k;
        RealType _mu;
      };

      /**
       * Construct a @c negative_binomial_distribution_mu object. @c k and @c p
       * are the parameters of the distribution.
       *
       * Requires: k >=0 && 0 <= p <= 1
       */
      explicit negative_binomial_distribution_mu(RealType k_arg = 1,
                                                 RealType mu_arg = RealType(0.5))
        : _k(k_arg), _mu(mu_arg)
      {}

      /**
       * Construct an @c negative_binomial_distribution_mu object from the
       * parameters.
       */
      explicit negative_binomial_distribution_mu(const param_type& parm)
        : _k(parm.k()), _mu(parm.mu())
      {}

      /**
       * Returns a random variate distributed according to the
       * negative binomial distribution.
       */
      template<class URNG>
      IntType operator()(URNG& urng) const
      {
        if(!R_FINITE(_mu) || ISNAN(_k) || _k <= 0 || _mu < 0) {
          return std::numeric_limits<IntType>::quiet_NaN();
        }
        RealType k = _k;
        if(!R_FINITE(_k)) k = DBL_MAX / 2.;
        if (_mu == 0) return 0;

        gamma_distribution<RealType> gamma(_k, _mu / _k);
        poisson_distribution<IntType, RealType> poisson(gamma(urng));
        return poisson(urng);
      }

      /**
       * Returns a random variate distributed according to the negative
       * binomial distribution with parameters specified by @c param.
       */
      template<class URNG>
      IntType operator()(URNG& urng, const param_type& parm) const
      {
        return negative_binomial_distribution_mu(parm)(urng);
      }

      /** Returns the @c k parameter of the distribution. */
      RealType k() const { return _k; }
      /** Returns the @c p parameter of the distribution. */
      RealType mu() const { return _mu; }

      /** Returns the smallest value that the distribution can produce. */
      IntType min BOOST_PREVENT_MACRO_SUBSTITUTION() const { return 0; }
      /** Returns the largest value that the distribution can produce. */
      IntType max BOOST_PREVENT_MACRO_SUBSTITUTION() const
      { return (std::numeric_limits<IntType>::max)(); }

      /** Returns the parameters of the distribution. */
      param_type param() const { return param_type(_k, _mu); }
      /** Sets parameters of the distribution. */
      void param(const param_type& parm)
      {
        _k = parm.k();
        _mu = parm.mu();
      }

      /**
       * Effects: Subsequent uses of the distribution do not depend
       * on values produced by any engine prior to invoking reset.
       */
      void reset() { }

#ifndef BOOST_RANDOM_NO_STREAM_OPERATORS
      /** Writes the parameters of the distribution to a @c std::ostream. */
      template<class CharT, class Traits>
      friend std::basic_ostream<CharT,Traits>&
      operator<<(std::basic_ostream<CharT,Traits>& os,
                 const negative_binomial_distribution_mu& bd)
      {
        os << bd.param();
        return os;
      }

      /** Reads the parameters of the distribution from a @c std::istream. */
      template<class CharT, class Traits>
      friend std::basic_istream<CharT,Traits>&
      operator>>(std::basic_istream<CharT,Traits>& is,
                 negative_binomial_distribution_mu& bd)
      {
        bd.read(is);
        return is;
      }
#endif

      /** Returns true if the two distributions will produce the same
          sequence of values, given equal generators. */
      friend bool operator==(const negative_binomial_distribution_mu& lhs,
                             const negative_binomial_distribution_mu& rhs)
      {
        return lhs._k == rhs._k && lhs._mu == rhs._mu;
      }
      /** Returns true if the two distributions could produce different
          sequences of values, given equal generators. */
      friend bool operator!=(const negative_binomial_distribution_mu& lhs,
                             const negative_binomial_distribution_mu& rhs)
      {
        return !(lhs == rhs);
      }

    private:

      /// @cond \show_private

      template<class CharT, class Traits>
      void read(std::basic_istream<CharT, Traits>& is) {
        param_type parm;
        if(is >> parm) {
          param(parm);
        }
      }

      // parameters
      RealType _k;
      RealType _mu;

      /// @endcond
    };


  }
}
