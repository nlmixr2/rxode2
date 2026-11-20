#ifndef RXG_SPECIAL_H
#define RXG_SPECIAL_H
// Device versions of the rxode2 functions that are not plain C math: the
// neural network activations, the transform-both-sides family, and the
// incomplete gamma/beta functions (boost on the host).  No recursion, no
// function pointers: everything here compiles as OpenCL C 1.2 and host C.
// Included by rxg_prelude.h.

#ifdef RXG_FLOAT
#define RXG_TINY 1e-30
#define RXG_MIN FLT_MIN
#else
#define RXG_TINY 1e-300
#define RXG_MIN DBL_MIN
#endif

////////////////////////////////////////////////////////////////////////////
// R's lgamma1p(), log1pmx() and lbeta() (nmath pgamma.c, lbeta.c)

RXG_FN double _rxg_logcf(double x, double i, double d, double eps) {
  const double scalefactor = 1.157920892373161954235709850086879e77; // 2^256
  double c1 = 2*d, c2 = i + d, c4 = c2 + d, a1 = c2;
  double b1 = i*(c2 - i*x), b2 = d*d*x, a2 = c4*c2 - b2;
  b2 = c4*b1 - i*b2;
  while (fabs(a2*b1 - a1*b2) > fabs(eps*b1*b2)) {
    double c3 = c2*c2*x;
    c2 += d;
    c4 += d;
    a1 = c4*a2 - c3*a1;
    b1 = c4*b2 - c3*b1;
    c3 = c1*c1*x;
    c1 += d;
    c4 += d;
    a2 = c4*a1 - c3*a2;
    b2 = c4*b1 - c3*b2;
    if (fabs(b2) > scalefactor) {
      a1 /= scalefactor;
      b1 /= scalefactor;
      a2 /= scalefactor;
      b2 /= scalefactor;
    } else if (fabs(b2) < 1/scalefactor) {
      a1 *= scalefactor;
      b1 *= scalefactor;
      a2 *= scalefactor;
      b2 *= scalefactor;
    }
  }
  return a2/b2;
}
// log(1 + x) - x
RXG_FN double _rxg_log1pmx(double x) {
  if (x > 1 || x < -0.79149064) return log1p(x) - x;
  double r = x/(2 + x), y = r*r;
  if (fabs(x) < 1e-2) {
    return r*((((2.0/9*y + 2.0/7)*y + 2.0/5)*y + 2.0/3)*y - x);
  }
  return r*(2*y*_rxg_logcf(y, 3, 2, 1e-14) - x);
}
// log(gamma(1 + a)), accurate for small a
RXG_FN double lgamma1p(double a) {
  if (fabs(a) >= 0.5) return lgamma(a + 1);
  // (zeta(i+2)-1)/(i+2)
  const double coeffs[40] = {
    0.3224670334241132182362075833230126e-0, 0.6735230105319809513324605383715000e-1,
    0.2058080842778454787900092413529198e-1, 0.7385551028673985266273097291406834e-2,
    0.2890510330741523285752988298486755e-2, 0.1192753911703260977113935692828109e-2,
    0.5096695247430424223356548135815582e-3, 0.2231547584535793797614188036013401e-3,
    0.9945751278180853371459589003190170e-4, 0.4492623673813314170020750240635786e-4,
    0.2050721277567069155316650397830591e-4, 0.9439488275268395903987425104415055e-5,
    0.4374866789907487804181793223952411e-5, 0.2039215753801366236781900709670839e-5,
    0.9551412130407419832857179772951265e-6, 0.4492469198764566043294290331193655e-6,
    0.2120718480555466586923135901077628e-6, 0.1004322482396809960872083050053344e-6,
    0.4769810169363980565760193417246730e-7, 0.2271109460894316491031998116062124e-7,
    0.1083865921489695409107491757968159e-7, 0.5183475041970046655121248647057669e-8,
    0.2483674543802478317185008663991718e-8, 0.1192140140586091207442548202774640e-8,
    0.5731367241678862013330194857961011e-9, 0.2759522885124233145178149692816341e-9,
    0.1330476437424448948149715720858008e-9, 0.6422964563838100022082448087644648e-10,
    0.3104424774732227276239215783404066e-10, 0.1502138408075414217093301048780668e-10,
    0.7275974480239079662504549924814047e-11, 0.3527742476575915083615072228655483e-11,
    0.1711991790559617908601084114443031e-11, 0.8315385841420284819798357793954418e-12,
    0.4042200525289440065536008957032895e-12, 0.1966475631096616490411045679010286e-12,
    0.9573630387838555763782200936508615e-13, 0.4664076026428374224576492565974577e-13,
    0.2273736960065972320633279596737272e-13, 0.1109139947083452201658320007192334e-13
  };
  double lgam = 0.2273736845824652515226821577978691e-12*_rxg_logcf(-a/2, 42, 1, 1e-14);
  for (int i = 39; i >= 0; i--) lgam = coeffs[i] - a*lgam;
  return (a*lgam - 0.5772156649015328606065120900824024)*a - _rxg_log1pmx(a);
}
// gamma(1 + a) - 1
RXG_FN double _rxg_g1pm1(double a) { return expm1(lgamma1p(a)); }

RXG_FN double _rxg_lgammacor(double x) {
  const double algmcs[5] = {
    +.1666389480451863247205729650822e+0, -.1384948176067563840732986059135e-4,
    +.9810825646924729426157171547487e-8, -.1809129475572494194263306266719e-10,
    +.6221098041892605227126015543416e-13
  };
  if (x < 10) return R_NaN;
  if (x >= 94906265.62425156) return 1/(x*12);
  double t = 10/x;
  double twox = (t*t*2 - 1)*2, b0 = 0, b1 = 0, b2 = 0;
  for (int i = 1; i <= 5; i++) {
    b2 = b1;
    b1 = b0;
    b0 = twox*b1 - b2 + algmcs[5 - i];
  }
  return (b0 - b2)*0.5/x;
}
RXG_FN double _rxg_lbeta(double a, double b) {
  if (ISNAN(a) || ISNAN(b)) return a + b;
  double p = fmin(a, b), q = fmax(a, b), corr;
  if (p < 0) return R_NaN;
  if (p == 0) return R_PosInf;
  if (!R_FINITE(q)) return R_NegInf;
  if (p >= 10) {
    corr = _rxg_lgammacor(p) + _rxg_lgammacor(q) - _rxg_lgammacor(p + q);
    return log(q)*-0.5 + 0.918938533204672741780329736406 + corr +
      (p - 0.5)*log(p/(p + q)) + q*log1p(-p/(p + q));
  }
  if (q >= 10) {
    corr = _rxg_lgammacor(q) - _rxg_lgammacor(p + q);
    return lgamma(p) + corr + p - p*log(p + q) + (q - 0.5)*log1p(-p/(p + q));
  }
  if (p < 1e-306) return lgamma(p) + (lgamma(q) - lgamma(p + q));
  return log(tgamma(p)*(tgamma(q)/tgamma(p + q)));
}

////////////////////////////////////////////////////////////////////////////
// Rmath helpers

// R's fmax2()/fmin2()/fsign(): NaN propagates
RXG_FN double fmax2(double x, double y) {
  if (ISNAN(x) || ISNAN(y)) return x + y;
  return (x < y) ? y : x;
}
RXG_FN double fmin2(double x, double y) {
  if (ISNAN(x) || ISNAN(y)) return x + y;
  return (x < y) ? x : y;
}
RXG_FN double fsign(double x, double y) {
  if (ISNAN(x) || ISNAN(y)) return x + y;
  return (y >= 0) ? fabs(x) : -fabs(x);
}
RXG_FN double ftrunc(double x) { return trunc(x); }

// R's fprec() (signif()) for x > 0 with |log10(x)| < 306
RXG_FN double _rxg_fprecMid(double x, int e10) {
  double pow10, p10 = 1.0;
  if (e10 > 308) {
    p10 = R_pow_di(10., e10 - 308);
    e10 = 308;
  }
  if (e10 > 0) {
    pow10 = R_pow_di(10., e10);
    return (rint((x*pow10)*p10)/pow10)/p10;
  }
  pow10 = R_pow_di(10., -e10);
  return rint((x/pow10))*pow10;
}
// ... and for very large or small x
RXG_FN double _rxg_fprecExt(double x, double l10, int e10, int dig) {
  int do_round = 308.25471555991674389 - l10 >= R_pow_di(10., -dig);
  int e2 = dig + ((e10 > 0) ? 1 : -1)*22;
  double p10 = R_pow_di(10., e2);
  double P10 = R_pow_di(10., e10 - e2);
  x *= p10;
  x *= P10;
  if (do_round) x += 0.5;
  x = floor(x)/p10;
  return x/P10;
}
// R's fprec() (signif())
RXG_FN double fprec(double x, double digits) {
  if (ISNAN(x) || ISNAN(digits)) return x + digits;
  if (!R_FINITE(x)) return x;
  if (!R_FINITE(digits)) {
    if (digits > 0.0) return x;
    digits = 1.0;
  }
  if (x == 0) return x;
  int dig = (int)round(digits);
  if (dig > 22) return x;
  if (dig < 1) dig = 1;
  double sgn = (x < 0.0) ? -1.0 : 1.0;
  x = fabs(x);
  double l10 = log10(x);
  int e10 = (int)(dig - 1 - floor(l10));
  if (fabs(l10) < 308 - 2) return sgn*_rxg_fprecMid(x, e10);
  return sgn*_rxg_fprecExt(x, l10, e10, dig);
}

// R's fround() (round(x, digits))
RXG_FN double fround(double x, double digits) {
  if (ISNAN(x) || ISNAN(digits)) return x + digits;
  if (!R_FINITE(x)) return x;
  if (digits > 323 || x == 0.) return x;
  else if (digits < -308) return 0.;
  else if (digits == 0.) return rint(x);
  int dig = (int)floor(digits + 0.5);
  double sgn = +1.;
  if (x < 0.) {
    sgn = -1.;
    x = -x;
  }
  double l10x = 0.301029995663981195213738894724*(0.5 + logb(x));
  if (l10x + dig > 15) return sgn*x;
  double pow10, x10, i10, xd, xu;
  if (dig <= 308) {
    pow10 = R_pow_di(10., dig);
    x10 = x*pow10;
    i10 = floor(x10);
    xd = i10/pow10;
    xu = ceil(x10)/pow10;
  } else {
    int e10 = dig - 308;
    double p10 = R_pow_di(10., e10);
    pow10 = R_pow_di(10., 308);
    x10 = (x*pow10)*p10;
    i10 = floor(x10);
    xd = i10/pow10/p10;
    xu = ceil(x10)/pow10/p10;
  }
  double du = xu - x, dd = x - xd;
  return sgn*((du < dd || (fmod(i10, 2.) == 1 && du == dd)) ? xu : xd);
}

RXG_FN double logspace_add(double logx, double logy) {
  return fmax2(logx, logy) + log1p(exp(-fabs(logx - logy)));
}
RXG_FN double logspace_sub(double logx, double logy) {
  double d = logy - logx;
  return logx + ((d > -M_LN2) ? log(-expm1(d)) : log1p(-exp(d)));
}

// R's beta()
RXG_FN double beta(double a, double b) {
  if (ISNAN(a) || ISNAN(b)) return a + b;
  if (a < 0 || b < 0) return R_NaN;
  else if (a == 0 || b == 0) return R_PosInf;
  else if (!R_FINITE(a) || !R_FINITE(b)) return 0;
  if (a + b < 171.61447887182298) return (1/tgamma(a + b))*tgamma(a)*tgamma(b);
  return exp(_rxg_lbeta(a, b));
}

// psi(x) and psi'(x): upward recurrence to x >= 10, then the asymptotic series
RXG_FN double digamma(double x) {
  if (ISNAN(x)) return x;
  if (x <= 0 && x == floor(x)) return R_NaN;
  double r = 0.0;
  if (x < 0) {
    // reflection: psi(x) = psi(1 - x) - pi/tan(pi x), with pi x reduced
    // by the nearest integer so the poles keep their accuracy
    double f = x - round(x);
    r = -M_PI*cos(M_PI*f)/sin(M_PI*f);
    x = 1.0 - x;
  }
  while (x < 10.0) {
    r -= 1.0/x;
    x += 1.0;
  }
  double f = 1.0/(x*x);
  double t = f*(-1.0/12 + f*(1.0/120 + f*(-1.0/252 + f*(1.0/240 + f*(-1.0/132 +
             f*(691.0/32760 + f*(-1.0/12 + f*(3617.0/8160))))))));
  return r + log(x) - 0.5/x + t;
}

RXG_FN double trigamma(double x) {
  if (ISNAN(x)) return x;
  if (x <= 0 && x == floor(x)) return R_PosInf;
  double r = 0.0, s = 1.0;
  if (x < 0) {
    // reflection: psi'(x) = pi^2/sin^2(pi x) - psi'(1 - x)
    double sp = sin(M_PI*(x - round(x))); // reduced as in digamma()
    r = M_PI*M_PI/(sp*sp);
    s = -1.0;
    x = 1.0 - x;
  }
  double a = 0.0;
  while (x < 10.0) {
    a += 1.0/(x*x);
    x += 1.0;
  }
  double f = 1.0/(x*x);
  double t = 1.0/x + 0.5*f + (1.0/x)*f*(1.0/6 + f*(-1.0/30 + f*(1.0/42 + f*(-1.0/30 +
             f*(5.0/66 + f*(-691.0/2730 + f*(7.0/6 + f*(-3617.0/510))))))));
  double ret = r + s*(a + t);
  // R's trigamma() gives NaN when it overflows
  return R_FINITE(ret) ? ret : R_NaN;
}

////////////////////////////////////////////////////////////////////////////
// Normal distribution

// R's qnorm() (Wichura's AS241), lower tail, not log
RXG_FN double rxg_qnorm(double p) {
  if (ISNAN(p)) return p;
  if (p < 0 || p > 1) return R_NaN;
  if (p == 0) return R_NegInf;
  if (p == 1) return R_PosInf;
  double q = p - 0.5, r, val;
  if (fabs(q) <= .425) {
    r = .180625 - q*q;
    val = q*(((((((r*2509.0809287301226727 +
                   33430.575583588128105)*r + 67265.770927008700853)*r +
                 45921.953931549871457)*r + 13731.693765509461125)*r +
               1971.5909503065514427)*r + 133.14166789178437745)*r +
             3.387132872796366608)
      /(((((((r*5226.495278852854561 +
              28729.085735721942674)*r + 39307.89580009271061)*r +
            21213.794301586595867)*r + 5394.1960214247511077)*r +
          687.1870074920579083)*r + 42.313330701600911252)*r + 1.);
    return val;
  }
  r = (q < 0) ? p : 1.0 - p;
  r = sqrt(-log(r));
  if (r <= 5.) {
    r += -1.6;
    val = (((((((r*7.7454501427834140764e-4 +
                 .0227238449892691845833)*r + .24178072517745061177)*
               r + 1.27045825245236838258)*r +
              3.64784832476320460504)*r + 5.7694972214606914055)*
            r + 4.6303378461565452959)*r +
           1.42343711074968357734)
      /(((((((r*
              1.05075007164441684324e-9 + 5.475938084995344946e-4)*
             r + .0151986665636164571966)*r +
            .14810397642748007459)*r + .68976733498510000455)*
          r + 1.6763848301838038494)*r +
         2.05319162663775882187)*r + 1.);
  } else {
    r += -5.;
    val = (((((((r*2.01033439929228813265e-7 +
                 2.71155556874348757815e-5)*r +
                .0012426609473880784386)*r + .026532189526576123093)*
              r + .29656057182850489123)*r +
             1.7848265399172913358)*r + 5.4637849111641143699)*
           r + 6.6579046435011037772)
      /(((((((r*
              2.04426310338993978564e-15 + 1.4215117583164458887e-7)*
             r + 1.8463183175100546818e-5)*r +
            7.868691311456132591e-4)*r + .0148753612908506148525)
          *r + .13692988092273580531)*r +
         .59983220655588793769)*r + 1.);
  }
  if (q < 0.0) val = -val;
  return val;
}

RXG_FN double erfinv(double x) {
  return rxg_qnorm((1 + x)/2.0)*M_SQRT1_2;
}

// phi() clamped away from 0 and 1
RXG_FN double phiU(double q) {
  double u = phi(q);
  if (u < 1e-15) return 1e-15;
  if (u > 1.0 - 1e-15) return 1.0 - 1e-15;
  return u;
}

////////////////////////////////////////////////////////////////////////////
// Neural network activations (src/utilc.cpp)

RXG_FN double dReLU(double x) { return (x > 0.0) ? 1.0 : 0.0; }
RXG_FN double GELU(double x) { return 0.5*x*(1.0 + erf(x*M_SQRT1_2)); }
RXG_FN double dGELU(double x) {
  return 0.5*(1.0 + erf(x*M_SQRT1_2)) + x*M_1_SQRT_2PI*exp(-0.5*x*x);
}
RXG_FN double d2GELU(double x) { return (2.0 - x*x)*exp(-0.5*x*x)*M_1_SQRT_2PI; }
RXG_FN double d3GELU(double x) {
  double x2 = x*x;
  return -8.0*x*exp(-x2*0.5)*0.5*M_1_SQRT_2PI + 4.0*x2*x*exp(-x2*0.5)*0.25*M_1_SQRT_2PI;
}
RXG_FN double d4GELU(double x) {
  return exp(-0.5*x*x)*M_1_SQRT_2PI*(7.0*x*x - 4.0 - x*x*x*x);
}
RXG_FN double ELU(double x, double alpha) { return (x > 0.0) ? x : (exp(x) - 1.0)*alpha; }
RXG_FN double dELU(double x, double alpha) { return (x > 0.0) ? 1.0 : exp(x)*alpha; }
RXG_FN double d2ELU(double x, double alpha) { return (x > 0.0) ? 0.0 : exp(x)*alpha; }
RXG_FN double d2aELU(double x, double alpha) { return (x > 0.0) ? 0.0 : exp(x); }
RXG_FN double dELUa(double x, double alpha) { return (x > 0.0) ? 0.0 : (exp(x) - 1.0); }
RXG_FN double d2ELUa(double x, double alpha) { return (x > 0.0) ? 0.0 : exp(x); }
RXG_FN double softplus(double x) { return log(1.0 + exp(x)); }
RXG_FN double dsoftplus(double x) { return 1.0/(1.0 + exp(-x)); }
RXG_FN double d2softplus(double x) {
  double ex = exp(x);
  return ex/((1.0 + ex)*(1.0 + ex));
}
RXG_FN double d3softplus(double x) {
  double ex = exp(-x);
  double ex1 = (1.0 + ex);
  return 2.0*exp(-2.0*x)/(ex1*ex1*ex1) - 1.0*ex/(ex1*ex1);
}
RXG_FN double d4softplus(double x) {
  double ex = exp(-x);
  double ex1 = (1.0 + ex);
  return 6.0*exp(-3.0*x)/(ex1*ex1*ex1*ex1) - 6.0*exp(-2.0*x)/(ex1*ex1*ex1) + ex/(ex1*ex1);
}
#define RXG_SELU_ALPHA 1.6732632423543772848170429916717
#define RXG_SELU_SCALE 1.0507009873554804934193349852946
RXG_FN double SELU(double x) {
  return (x > 0.0) ? RXG_SELU_SCALE*x : RXG_SELU_SCALE*RXG_SELU_ALPHA*(exp(x) - 1.0);
}
RXG_FN double dSELU(double x) {
  return (x > 0.0) ? RXG_SELU_SCALE : RXG_SELU_SCALE*RXG_SELU_ALPHA*exp(x);
}
RXG_FN double lReLU(double x) { return (x >= 0.0) ? x : 0.01*x; }
RXG_FN double dlReLU(double x) { return (x > 0.0) ? 1.0 : 0.01; }
RXG_FN double PReLU(double x, double alpha) { return (x > 0.0) ? x : alpha*x; }
RXG_FN double dPReLU(double x, double alpha) { return (x > 0.0) ? 1.0 : alpha; }
RXG_FN double dPReLUa(double x, double alpha) { return (x > 0.0) ? 0.0 : x; }
RXG_FN double dPReLUa1(double x, double alpha) { return (x > 0.0) ? 0.0 : 1.0; }
RXG_FN double Swish(double x) { return x/(1.0 + exp(-x)); }
RXG_FN double dSwish(double x) {
  double ex = exp(-x);
  double den = (1.0 + ex);
  return x*ex/(den*den) + 1.0/den;
}

////////////////////////////////////////////////////////////////////////////
// Transform both sides (inst/include/rxode2.h _powerD*).  The host versions
// recurse for the composed transforms; here the inner transforms are separate
// functions.

#define _rxgTbsEps sqrt(RXG_EPS)
#define _rxgTbsSplit(yj0) ((yj0) - ((yj0)/10)*10)

// Yeo-Johnson (transform 1)
RXG_FN double _rxg_yj(double x, double lambda) {
  if (!R_FINITE(x)) return NA_REAL;
  double l2;
  if (lambda == 1.0) return x;
  if (x >= 0) {
    if (lambda == 0) return log1p(x);
    return (pow(x + 1.0, lambda) - 1.0)/lambda;
  }
  if (lambda == 2.0) return -log1p(-x);
  l2 = 2.0 - lambda;
  return (1.0 - pow(1.0 - x, l2))/l2;
}
RXG_FN double _rxg_yji(double x, double lambda) {
  if (!R_FINITE(x)) return NA_REAL;
  double l2;
  if (lambda == 1.0) return x;
  if (x >= 0) {
    if (lambda == 0) return expm1(x);
    return pow(x*lambda + 1.0, 1.0/lambda) - 1.0;
  }
  if (lambda == 2.0) return -expm1(-x);
  l2 = (2.0 - lambda);
  return 1.0 - pow(1.0 - l2*x, 1.0/l2);
}
RXG_FN double _rxg_yjD(double x, double lambda) {
  if (!R_FINITE(x)) return NA_REAL;
  if (lambda == 1.0) return 1.0;
  if (x >= 0) {
    if (lambda == 0.0) return 1.0/(x + 1.0);
    return pow(x + 1.0, lambda - 1.0);
  }
  if (lambda == 2.0) return 1/(1.0 - x);
  return pow(1.0 - x, 1.0 - lambda);
}
RXG_FN double _rxg_yjDD(double x, double lambda) {
  if (!R_FINITE(x)) return NA_REAL;
  if (lambda == 1.0) return 0;
  if (x >= 0) {
    if (lambda == 0.0) return -1/((x + 1.0)*(x + 1.0));
    return (lambda - 1.0)*pow(x + 1.0, lambda - 2.0);
  }
  if (lambda == 2.0) return 1/((1.0 - x)*(1.0 - x));
  return -(1.0 - lambda)*pow(1.0 - x, -lambda);
}
// probit (transform 6) and logit (transform 4) and their derivatives
RXG_FN double _rxg_probitT(double x, double low, double high) {
  if (!R_FINITE(x)) return NA_REAL;
  double p = (x - low)/(high - low);
  if (p >= 1) return R_NaN;
  if (p <= 0) return R_NaN;
  return rxg_qnorm(p);
}
RXG_FN double _rxg_probitTD(double x, double low, double high) {
  if (!R_FINITE(x)) return NA_REAL;
  double hl = (high - low);
  double eri = erfinv(-1 + 2*(-low + x)/hl);
  return 2.506628274631000241612*exp(eri*eri)/hl;
}
RXG_FN double _rxg_probitTDD(double x, double low, double high) {
  if (!R_FINITE(x)) return NA_REAL;
  double hl = (high - low);
  double eri = erfinv(-1 + 2*(-low + x)/hl);
  return 8.885765876316728650863*exp(2*eri*eri)*eri/(hl*hl);
}
RXG_FN double _rxg_logitT(double x, double low, double high) {
  if (!R_FINITE(x)) return NA_REAL;
  if (!(x > low && x < high)) return R_NaN;
  return log(x - low) - log(high - x);
}
RXG_FN double _rxg_logitTD(double x, double low, double high) {
  if (!R_FINITE(x)) return NA_REAL;
  double xl = (x - low);
  double hl = (high - low);
  return hl/(xl*(hl - xl));
}
RXG_FN double _rxg_logitTDD(double x, double low, double high) {
  if (!R_FINITE(x)) return NA_REAL;
  double hl = (high - low);
  double xl = (-low + x);
  double t1 = hl - xl;
  return 1.0/(t1*t1) - 1.0/(xl*xl);
}

RXG_FN double _powerDi(double x, double lambda, int yj0, double low, double high) {
  if (!R_FINITE(x)) return NA_REAL;
  double x0, ret;
  switch (_rxgTbsSplit(yj0)) {
  case 7:
    return (high - low)*phi(_rxg_yji(x, lambda)) + low;
  case 6:
    return (high - low)*phi(x) + low;
  case 5:
    return (high - low)/(1 + exp(-_rxg_yji(x, lambda))) + low;
  case 4:
    return (high - low)/(1 + exp(-x)) + low;
  case 3:
    return exp(x);
  case 2:
    return x;
  case 0:
    if (lambda == 1.0) return (x + 1.0);
    if (lambda == 0) return exp(x);
    x0 = x*lambda + 1.0;
    if (x0 <= _rxgTbsEps) return _rxgTbsEps;
    ret = pow(x0, 1.0/lambda);
    if (ISNA(ret)) return _rxgTbsEps;
    return ret;
  case 1:
    return _rxg_yji(x, lambda);
  }
  return NA_REAL;
}

RXG_FN double _powerD(double x, double lambda, int yj0, double low, double high) {
  if (!R_FINITE(x)) return NA_REAL;
  double x0 = x;
  switch (_rxgTbsSplit(yj0)) {
  case 7:
    return _rxg_yj(_rxg_probitT(x, low, high), lambda);
  case 6:
    return _rxg_probitT(x, low, high);
  case 5:
    return _rxg_yj(_rxg_logitT(x, low, high), lambda);
  case 4:
    return _rxg_logitT(x, low, high);
  case 3:
    if (x <= _rxgTbsEps) x0 = _rxgTbsEps;
    return log(x0);
  case 2:
    return x;
  case 0:
    if (lambda == 1.0) return x - 1.0;
    if (x <= _rxgTbsEps) x0 = _rxgTbsEps;
    if (lambda == 0.0) return log(x0);
    return (pow(x0, lambda) - 1.0)/lambda;
  case 1:
    return _rxg_yj(x, lambda);
  }
  return NA_REAL;
}

RXG_FN double _powerDD(double x, double lambda, int yj0, double low, double high) {
  if (!R_FINITE(x)) return NA_REAL;
  double x0 = x;
  switch (_rxgTbsSplit(yj0)) {
  case 7:
    return _rxg_yjD(_rxg_probitT(x, low, high), lambda)*_rxg_probitTD(x, low, high);
  case 6:
    return _rxg_probitTD(x, low, high);
  case 5:
    return _rxg_yjD(_rxg_logitT(x, low, high), lambda)*_rxg_logitTD(x, low, high);
  case 4:
    return _rxg_logitTD(x, low, high);
  case 3:
    if (x <= _rxgTbsEps) x0 = _rxgTbsEps;
    return 1/x0;
  case 2:
    return 1.0;
  case 0:
    if (lambda == 1.0) return 1.0;
    if (x <= _rxgTbsEps) x0 = _rxgTbsEps;
    if (lambda == 0.0) return 1/x0;
    return pow(x0, lambda - 1);
  case 1:
    return _rxg_yjD(x, lambda);
  }
  return NA_REAL;
}

RXG_FN double _powerDDD(double x, double lambda, int yj0, double low, double high) {
  if (!R_FINITE(x)) return NA_REAL;
  double x0 = x, t1, dL;
  switch (_rxgTbsSplit(yj0)) {
  case 7:
    dL = _rxg_probitTD(x, low, high);
    t1 = _rxg_probitT(x, low, high);
    return dL*dL*_rxg_yjDD(t1, lambda) + _rxg_yjD(t1, lambda)*_rxg_probitTDD(x, low, high);
  case 6:
    return _rxg_probitTDD(x, low, high);
  case 5:
    dL = _rxg_logitTD(x, low, high);
    t1 = _rxg_logitT(x, low, high);
    return dL*dL*_rxg_yjDD(t1, lambda) + _rxg_yjD(t1, lambda)*_rxg_logitTDD(x, low, high);
  case 4:
    return _rxg_logitTDD(x, low, high);
  case 3:
    if (x <= _rxgTbsEps) x0 = _rxgTbsEps;
    return -1/(x0*x0);
  case 2:
    return 0;
  case 0:
    if (lambda == 1.0) return 0;
    if (x <= _rxgTbsEps) x0 = _rxgTbsEps;
    if (lambda == 0.0) return -1/(x0*x0);
    return (lambda - 1)*pow(x0, lambda - 2);
  case 1:
    return _rxg_yjDD(x, lambda);
  }
  return NA_REAL;
}

// Box-Cox and Yeo-Johnson lambda derivatives
RXG_FN double _bcDL(double b, double lam) {
  if (b <= _rxgTbsEps) b = _rxgTbsEps;
  double lb = log(b);
  if (lam == 0.0) return 0.5*lb*lb;
  double bl = pow(b, lam);
  return bl*lb/lam - (bl - 1.0)/(lam*lam);
}
RXG_FN double _bcDLx(double b, double lam) {
  if (b <= _rxgTbsEps) b = _rxgTbsEps;
  return pow(b, lam - 1.0)*log(b);
}
RXG_FN double _bcDL2(double b, double lam) {
  if (b <= _rxgTbsEps) b = _rxgTbsEps;
  double lb = log(b);
  if (lam == 0.0) return lb*lb*lb/3.0;
  double bl = pow(b, lam);
  return bl*lb*lb/lam - 2.0*bl*lb/(lam*lam) + 2.0*(bl - 1.0)/(lam*lam*lam);
}
RXG_FN double _yjDL(double x, double lam) {
  if (x >= 0) return _bcDL(x + 1.0, lam);
  return _bcDL(1.0 - x, 2.0 - lam);
}
RXG_FN double _yjDLx(double x, double lam) {
  if (x >= 0) return _bcDLx(x + 1.0, lam);
  return -_bcDLx(1.0 - x, 2.0 - lam);
}
RXG_FN double _yjDL2(double x, double lam) {
  if (x >= 0) return _bcDL2(x + 1.0, lam);
  return -_bcDL2(1.0 - x, 2.0 - lam);
}
// inner transform of the composed cases 5 and 7 (NaN outside the range)
RXG_FN double _rxg_tbsInner(double x, int yj, double low, double high) {
  double p;
  if (yj == 5) {
    if (!(x > low && x < high)) return R_NaN;
    return log(x - low) - log(high - x);
  }
  p = (x - low)/(high - low);
  if (p >= 1 || p <= 0) return R_NaN;
  return rxg_qnorm(p);
}
RXG_FN double _powerDLambda(double x, double lambda, int yj0, double low, double high) {
  if (!R_FINITE(x)) return NA_REAL;
  double p;
  int yj = _rxgTbsSplit(yj0);
  switch (yj) {
  case 0: return _bcDL(x, lambda);
  case 1: return _yjDL(x, lambda);
  case 2: case 3: case 4: case 6: return 0.0;
  case 5: case 7:
    p = _rxg_tbsInner(x, yj, low, high);
    if (ISNAN(p)) return p;
    return _yjDL(p, lambda);
  }
  return NA_REAL;
}
RXG_FN double _powerDLambda2(double x, double lambda, int yj0, double low, double high) {
  if (!R_FINITE(x)) return NA_REAL;
  double p;
  int yj = _rxgTbsSplit(yj0);
  switch (yj) {
  case 0: return _bcDL2(x, lambda);
  case 1: return _yjDL2(x, lambda);
  case 2: case 3: case 4: case 6: return 0.0;
  case 5: case 7:
    p = _rxg_tbsInner(x, yj, low, high);
    if (ISNAN(p)) return p;
    return _yjDL2(p, lambda);
  }
  return NA_REAL;
}
RXG_FN double _powerDLambdaX(double x, double lambda, int yj0, double low, double high) {
  if (!R_FINITE(x)) return NA_REAL;
  double p;
  int yj = _rxgTbsSplit(yj0);
  switch (yj) {
  case 0: return _bcDLx(x, lambda);
  case 1: return _yjDLx(x, lambda);
  case 2: case 3: case 4: case 6: return 0.0;
  case 5: case 7:
    p = _rxg_tbsInner(x, yj, low, high);
    if (ISNAN(p)) return p;
    return _yjDLx(p, lambda)*((yj == 5) ? _rxg_logitTD(x, low, high) : _rxg_probitTD(x, low, high));
  }
  return NA_REAL;
}

#define rxTBS(x, lm, yj, hi, low)  _powerD(x,  lm, (int)(yj), hi, low)
#define rxTBSi(x, lm, yj, hi, low) _powerDi(x,  lm, (int)(yj), hi, low)
#define rxTBSd(x, lm, yj, hi, low) _powerDD(x, lm, (int)(yj), hi, low)
#define rxTBSd2(x, lm, yj, hi, low) _powerDDD(x, lm, (int)(yj), hi, low)
#define rxTBSdL(x, lm, yj, hi, low) _powerDLambda(x, lm, (int)(yj), hi, low)
#define rxTBSdL2(x, lm, yj, hi, low) _powerDLambda2(x, lm, (int)(yj), hi, low)
#define rxTBSdLx(x, lm, yj, hi, low) _powerDLambdaX(x, lm, (int)(yj), hi, low)

// probit()/probitInv() (src/utilc.cpp)
RXG_FN double probit(double x, double low, double high) {
  return _powerD(x, 1.0, 6, low, high);
}
RXG_FN double probitInv(double alpha, double low, double high) {
  return _powerDi(alpha, 1.0, 6, low, high);
}
#define _probit1(x) probit(x, 0.0, 1.0)
#define _probit2(x, y) probit(x, y, 1.0)
#define _probitInv1(x) probitInv(x, 0.0, 1.0)
#define _probitInv2(x, y) probitInv(x, y, 1.0)

////////////////////////////////////////////////////////////////////////////
// abs() helpers (inst/include/rxode2.h)

RXG_FN double abs1(double x) { return (x == 0.0) ? 1.0 : fabs(x); }
RXG_FN double dabs1(double x) { return (double)((x > 0) - (x < 0)); }
RXG_FN double dabs(double x) { return (double)((x > 0) - (x < 0)); }
RXG_FN double dabs2(double x) { return 0.0; }

////////////////////////////////////////////////////////////////////////////
// Incomplete gamma (boost on the host): series for x < a + 1, Lentz's
// continued fraction otherwise, so each branch computes the smaller tail.

// sum such that P(a,x) = exp(a log x - x - lgamma(a))*sum
RXG_FN double _rxg_gser(double a, double x) {
  double ap = a, del = 1.0/a, sum = del;
  // about 10 sqrt(a) terms near x = a; the cap covers a up to ~1e10
  for (int n = 0; n < 1000000; n++) {
    ap += 1.0;
    del *= x/ap;
    sum += del;
    if (fabs(del) < fabs(sum)*RXG_EPS) break;
  }
  return sum;
}
// h such that Q(a,x) = exp(a log x - x - lgamma(a))*h
RXG_FN double _rxg_gcf(double a, double x) {
  double b = x + 1.0 - a, c = 1.0/RXG_TINY;
  if (fabs(b) < RXG_TINY) b = RXG_TINY; // x + 1 rounds to a for a huge a
  double d = 1.0/b, h = d;
  for (int i = 1; i < 1000000; i++) {
    double an = -i*(i - a);
    b += 2.0;
    d = an*d + b;
    if (fabs(d) < RXG_TINY) d = RXG_TINY;
    c = b + an/c;
    if (fabs(c) < RXG_TINY) c = RXG_TINY;
    d = 1.0/d;
    double del = d*c;
    h *= del;
    if (fabs(del - 1.0) < RXG_EPS) break;
  }
  return h;
}
// Gamma(a,x) for 0 < a < 1 and 0 < x < a + 1, where Q = 1 - P would cancel:
// Gamma(a,x) = ((Gamma(1+a) - 1) - (x^a - 1))/a - x^a sum_{n>=1} (-x)^n/(n!(a+n))
// (returned times a/Gamma(1+a), i.e. Q, when regularized)
RXG_FN double _rxg_igammaSmallA(double a, double x, int regularized) {
  double t = 1.0, sum = 0.0;
  for (int n = 1; n < 1000; n++) {
    t *= -x/n;
    double term = t/(a + n);
    sum += term;
    if (fabs(term) < fabs(sum)*RXG_EPS) break;
  }
  double g1 = _rxg_g1pm1(a), xa1 = expm1(a*log(x));
  if (regularized) return ((g1 - xa1) - a*(xa1 + 1)*sum)/(1 + g1);
  return (g1 - xa1)/a - (xa1 + 1)*sum;
}
// P or Q for 0 < x < Inf given g (1 or Gamma(a)) and the log prefactor;
// summed in logs so x^a e^-x may overflow when the result does not
RXG_FN double _rxg_igammaTail(double a, double x, int lower, int regularized,
                              double g, double lpre) {
  int series = (x < a + 1.0);
  if (series && !lower && a < 1) return _rxg_igammaSmallA(a, x, regularized);
  double v = exp(lpre + log(series ? _rxg_gser(a, x) : _rxg_gcf(a, x)));
  if (lower == series) return v;
  return R_FINITE(g) ? g - v : g;
}
// x = Inf: P = 1, Q = 0 (the host gives these for a NaN a too)
RXG_FN double _rxg_igammaInf(double a, int lower, int regularized) {
  if (regularized) return lower ? 1.0 : 0.0;
  if (ISNAN(a)) return lower ? a : 0.0;
  double g = tgamma(a);
  if (lower) return g;
  return R_FINITE(g) ? 0.0 : R_NaN;
}
// lower = 1 for P, 0 for Q; regularized = 0 multiplies by Gamma(a).
// Argument checks follow the host (boost with errors ignored).
RXG_FN double _rxg_igamma(double a, double x, int lower, int regularized) {
  if (ISNAN(x) || a <= 0 || x < 0) return R_NaN;
  if (x == R_PosInf) return _rxg_igammaInf(a, lower, regularized);
  if (ISNAN(a)) return a;
  double g = regularized ? 1.0 : tgamma(a);
  if (x == 0) return lower ? 0.0 : g;
  // the host gives lowergamma(Inf, x) = 0 for x < 1, NaN otherwise
  if (a == R_PosInf) return (lower && !regularized && x < 1) ? 0.0 : R_NaN;
  double lpre = a*log(x) - x - (regularized ? lgamma(a) : 0.0);
  return _rxg_igammaTail(a, x, lower, regularized, g, lpre);
}
// log P (lower) or log Q for 0 < a, x < Inf
RXG_FN double _rxg_igammaLog(double a, double x, int lower) {
  int series = (x < a + 1.0);
  if (series && !lower && a < 1) return log(_rxg_igammaSmallA(a, x, 1));
  double l = a*log(x) - x - lgamma(a) + log(series ? _rxg_gser(a, x) : _rxg_gcf(a, x));
  if (lower == series) return l;
  return log1p(-exp(l));
}
RXG_FN double gammap(double a, double x) { return _rxg_igamma(a, x, 1, 1); }
RXG_FN double gammaq(double a, double x) { return _rxg_igamma(a, x, 0, 1); }
RXG_FN double lowergamma(double a, double x) { return _rxg_igamma(a, x, 1, 0); }
RXG_FN double uppergamma(double a, double x) { return _rxg_igamma(a, x, 0, 0); }

// d P(a,x)/dx (argument checks follow the host)
RXG_FN double gammapDer(double a, double x) {
  if (ISNAN(x) || a <= 0 || x < 0) return R_NaN;
  if (x == 0) return (a > 1) ? 0.0 : ((a == 1) ? 1.0 : R_PosInf);
  if (ISNAN(a) || !R_FINITE(a) || !R_FINITE(x)) return R_NaN;
  return exp((a - 1)*log(x) - x - lgamma(a));
}

// starting value for _rxg_gammaInv() (Numerical Recipes' invgammp); for a
// tiny p the small-x limit P = x^a/Gamma(1+a)
RXG_FN double _rxg_gammaInvStart(double a, double p, double q) {
  double t, x;
  if (p < 1e-10) return exp((log(p) + lgamma1p(a))/a);
  if (a > 1) {
    double pp = (p < 0.5) ? p : q;
    t = sqrt(-2*log(pp));
    x = (2.30753 + t*0.27061)/(1 + t*(0.99229 + t*0.04481)) - t;
    if (p < 0.5) x = -x;
    double c = 1 - 1/(9*a) - x/(3*sqrt(a));
    return fmax(1e-3, a*c*c*c);
  }
  t = 1 - a*(0.253 + a*0.12);
  if (p < t) return pow(p/t, 1/a);
  // 1 - (p - t)/(1 - t) = q/(1 - t), kept accurate for tiny q
  return 1 - log(q/(1 - t));
}
// Newton iterations on log x for log P (or log Q in the upper tail) = log p,
// which stays accurate in the far tails where P and the density underflow
RXG_FN double _rxg_gammaInvRefine(double a, double p, double q, double x) {
  int lower = (p < 0.5);
  if (!(x > 0)) {
    // in the lower tail the answer underflows (the host gives DBL_MIN); in
    // the upper tail the starting value failed, so start from 1
    if (lower) return RXG_MIN;
    x = 1.0;
  }
  double lt = lower ? log(p) : log(q), gln = lgamma(a), y = log(x);
  for (int j = 0; j < 200; j++) {
    x = exp(y);
    double lf = _rxg_igammaLog(a, x, lower);
    // d log F/d log x = x f(x)/F (negative for Q)
    double dg = exp(a*log(x) - x - gln - lf);
    if (!lower) dg = -dg;
    if (!(fabs(dg) > 0) || !R_FINITE(dg) || !R_FINITE(lf)) break;
    double dy = (lf - lt)/dg;
    dy = fmax(-2.0, fmin(2.0, dy));
    y -= dy;
    if (fabs(dy) < 4*RXG_EPS) break;
  }
  x = exp(y);
  return (x < RXG_MIN) ? RXG_MIN : x;
}
// 1 when p or q is NaN or outside [0, 1]
RXG_FN int _rxg_probOut(double p, double q) {
  if (ISNAN(p) || ISNAN(q)) return 1;
  return (p < 0 || p > 1 || q < 0 || q > 1);
}
// x with P(a,x) = p (q = 1 - p, given separately to keep the upper tail)
// (argument checks follow the host)
RXG_FN double _rxg_gammaInv(double a, double p, double q) {
  if (a <= 0 || _rxg_probOut(p, q)) return R_NaN;
  if (p == 0) return 0.0;
  if (q == 0) return R_PosInf;
  if (ISNAN(a) || !R_FINITE(a)) return R_NaN;
  // x <= 1 has P ~ x^a/Gamma(1+a), so when that root underflows so does the
  // answer; the host gives DBL_MIN
  if ((log(p) + lgamma1p(a))/a < log(RXG_MIN)) return RXG_MIN;
  return _rxg_gammaInvRefine(a, p, q, _rxg_gammaInvStart(a, p, q));
}
RXG_FN double gammapInv(double a, double p) { return _rxg_gammaInv(a, p, 1 - p); }
RXG_FN double gammaqInv(double a, double q) { return _rxg_gammaInv(a, 1 - q, q); }

// f(u) = log P(e^u, x) - log p (or log q - log Q in the upper tail),
// decreasing in u = log a; logs keep the far tails accurate
RXG_FN double _rxg_gammaInvaF(double u, double x, double p, double q) {
  double a = exp(u);
  if (p < 0.5) return _rxg_igammaLog(a, x, 1) - log(p);
  return log(q) - _rxg_igammaLog(a, x, 0);
}
// bracket the root in u: on return f(lo) >= 0 >= f(hi) unless none was found
RXG_FN void _rxg_gammaInvaBracket(double x, double p, double q, double *lo,
                                  double *hi, double *flo, double *fhi) {
  *lo = log(fmax(x, 1.0));
  *flo = _rxg_gammaInvaF(*lo, x, p, q);
  *hi = *lo;
  *fhi = *flo;
  if (*flo > 0) {
    for (int i = 0; i < 2000 && *fhi > 0; i++) {
      *lo = *hi;
      *flo = *fhi;
      *hi += 1.0;
      *fhi = _rxg_gammaInvaF(*hi, x, p, q);
    }
    return;
  }
  for (int i = 0; i < 2000 && *flo < 0; i++) {
    *hi = *lo;
    *fhi = *flo;
    *lo -= 1.0;
    *flo = _rxg_gammaInvaF(*lo, x, p, q);
  }
}
// Illinois iterations on u within [lo, hi]
RXG_FN double _rxg_gammaInvaSolve(double x, double p, double q, double lo,
                                  double hi, double flo, double fhi) {
  int side = 0;
  double r = lo, r0;
  for (int i = 0; i < 300; i++) {
    r0 = r;
    r = (lo*fhi - hi*flo)/(fhi - flo);
    double fr = _rxg_gammaInvaF(r, x, p, q);
    if (fr == 0 || fabs(hi - lo) < 4*RXG_EPS*fmax(1.0, fabs(r)) ||
        (i > 0 && fabs(r - r0) < 4*RXG_EPS*fmax(1.0, fabs(r)))) break;
    if (fr > 0) {
      lo = r;
      flo = fr;
      if (side == 1) fhi *= 0.5;
      side = 1;
    } else {
      hi = r;
      fhi = fr;
      if (side == -1) flo *= 0.5;
      side = -1;
    }
  }
  return exp(r);
}
// a with P(a,x) = p: P decreases in a, so bracket and solve
RXG_FN double _rxg_gammaInva(double x, double p, double q) {
  // the host (boost with errors ignored) gives Inf at p = 0, DBL_MIN at
  // p = 1, 0 at x = 0, Inf at x = Inf and Inf for a NaN p, in that order
  if (p == 0) return R_PosInf;
  if (q == 0) return RXG_MIN;
  if (x == 0) return 0.0;
  if (x == R_PosInf) return R_PosInf;
  if (ISNAN(x) || x < 0) return R_NaN;
  if (ISNAN(p) || ISNAN(q)) return R_PosInf;
  if (p < 0 || p > 1) return R_NaN;
  double lo, hi, flo, fhi;
  _rxg_gammaInvaBracket(x, p, q, &lo, &hi, &flo, &fhi);
  if (flo == 0) return exp(lo);
  if (fhi == 0) return exp(hi);
  if (!(flo > 0 && fhi < 0)) return R_NaN;
  return _rxg_gammaInvaSolve(x, p, q, lo, hi, flo, fhi);
}
RXG_FN double gammapInva(double x, double p) { return _rxg_gammaInva(x, p, 1 - p); }
RXG_FN double gammaqInva(double x, double q) { return _rxg_gammaInva(x, 1 - q, q); }

////////////////////////////////////////////////////////////////////////////
// Regularized incomplete beta

RXG_FN double _rxg_betacf(double a, double b, double x) {
  double qab = a + b, qap = a + 1.0, qam = a - 1.0;
  double c = 1.0, d = 1.0 - qab*x/qap;
  if (fabs(d) < RXG_TINY) d = RXG_TINY;
  d = 1.0/d;
  double h = d;
  for (int m = 1; m < 10000; m++) {
    int m2 = 2*m;
    double aa = m*(b - m)*x/((qam + m2)*(a + m2));
    d = 1.0 + aa*d;
    if (fabs(d) < RXG_TINY) d = RXG_TINY;
    c = 1.0 + aa/c;
    if (fabs(c) < RXG_TINY) c = RXG_TINY;
    d = 1.0/d;
    h *= d*c;
    aa = -(a + m)*(qab + m)*x/((a + m2)*(qap + m2));
    d = 1.0 + aa*d;
    if (fabs(d) < RXG_TINY) d = RXG_TINY;
    c = 1.0 + aa/c;
    if (fabs(c) < RXG_TINY) c = RXG_TINY;
    d = 1.0/d;
    double del = d*c;
    h *= del;
    if (fabs(del - 1.0) < RXG_EPS) break;
  }
  return h;
}
// 1 - I_y(b, a) for b < 1e-6 a, where computing 1 - (1 - tiny) would lose it:
// I_y(b,a) = y^b Gamma(a+b)/(Gamma(1+b) Gamma(a)) (1 + b sum_{n>=1} (1-a)_n y^n/(n!(b+n)))
RXG_FN double _rxg_ibetaSmallB(double a, double b, double y) {
  double t = 1.0, sum = 0.0;
  for (int n = 1; n < 10000; n++) {
    t *= (n - a)*y/n;
    double term = t/(b + n);
    sum += term;
    if (fabs(term) <= fabs(sum)*RXG_EPS) break;
  }
  // log(Gamma(a+b)/Gamma(a)) to second order in b
  double dl = b*digamma(a) + 0.5*b*b*trigamma(a);
  return -expm1(b*log(y) + dl - lgamma1p(b) + log1p(b*sum));
}
// log I_x(a, b) for 0 < x < 1 and finite positive shapes
RXG_FN double _rxg_ibetaLog(double a, double b, double x) {
  double lbt = a*log(x) + b*log1p(-x) - _rxg_lbeta(a, b);
  if (x < (a + 1)/(a + b + 2)) return lbt + log(_rxg_betacf(a, b, x)/a);
  if (b < 1e-6*fmin(1.0, a)) return log(_rxg_ibetaSmallB(a, b, 1.0 - x));
  return log1p(-exp(lbt)*_rxg_betacf(b, a, 1.0 - x)/b);
}
// arguments (none NaN) outside ibeta()'s domain; an infinite shape is
// outside it, as on the host
RXG_FN int _rxg_ibetaOut(double a, double b, double x) {
  if (!R_FINITE(a) || !R_FINITE(b)) return 1;
  if (a < 0 || b < 0 || (a == 0 && b == 0)) return 1;
  return (x < 0 || x > 1);
}
// argument checks and edge values of ibeta(); 1 when *out holds the answer
RXG_FN int _rxg_ibetaEdge(double a, double b, double x, double *out) {
  if (ISNAN(a) || ISNAN(b) || ISNAN(x)) {
    *out = a + b + x;
  } else if (_rxg_ibetaOut(a, b, x)) {
    *out = R_NaN;
  } else if (a == 0) {
    *out = 1.0;
  } else if (b == 0 || x == 0) {
    *out = 0.0;
  } else if (x == 1) {
    *out = 1.0;
  } else {
    return 0;
  }
  return 1;
}
RXG_FN double ibeta(double a, double b, double x) {
  double out;
  if (_rxg_ibetaEdge(a, b, x, &out)) return out;
  double lbt = a*log(x) + b*log1p(-x) - _rxg_lbeta(a, b);
  if (x < (a + 1)/(a + b + 2)) return exp(lbt)*_rxg_betacf(a, b, x)/a;
  if (b < 1e-6*fmin(1.0, a)) return _rxg_ibetaSmallB(a, b, 1.0 - x);
  return 1.0 - exp(lbt)*_rxg_betacf(b, a, 1.0 - x)/b;
}
// the beta density at x = 0 for shape s at that end (the other shape o)
RXG_FN double _rxg_ibetaDerEnd(double s, double o) {
  if (s > 1) return 0.0;
  return (s == 1) ? 1.0/beta(s, o) : R_PosInf;
}
// arguments (none NaN) outside ibetaDer()'s domain
RXG_FN int _rxg_ibetaDerOut(double a, double b, double x) {
  if (!R_FINITE(a) || !R_FINITE(b)) return 1;
  if (a <= 0 || b <= 0) return 1;
  return (x < 0 || x > 1);
}
RXG_FN double ibetaDer(double a, double b, double x) {
  if (ISNAN(a) || ISNAN(b) || ISNAN(x)) return a + b + x;
  if (_rxg_ibetaDerOut(a, b, x)) return R_NaN;
  if (x == 0) return _rxg_ibetaDerEnd(a, b);
  if (x == 1) return _rxg_ibetaDerEnd(b, a);
  return exp((a - 1)*log(x) + (b - 1)*log1p(-x) - _rxg_lbeta(a, b));
}
// small-x limit of I_x(a,b) = x^a/(a B(a,b)), solved for x
RXG_FN double _rxg_ibetaSmallX(double a, double b, double p) {
  return exp((log(p) + log(a) + _rxg_lbeta(a, b))/a);
}
// starting value for ibetaInv() (Numerical Recipes' invbetai), or the
// small-x limit for a tiny p
RXG_FN double _rxg_ibetaInvStart(double a, double b, double p) {
  double t, u, w, x;
  if (p < 1e-10) return _rxg_ibetaSmallX(a, b, p);
  if (a >= 1 && b >= 1) {
    double pp = (p < 0.5) ? p : 1 - p;
    t = sqrt(-2*log(pp));
    x = (2.30753 + t*0.27061)/(1 + t*(0.99229 + t*0.04481)) - t;
    if (p < 0.5) x = -x;
    double al = (x*x - 3)/6;
    double h = 2/(1/(2*a - 1) + 1/(2*b - 1));
    w = (x*sqrt(al + h)/h) - (1/(2*b - 1) - 1/(2*a - 1))*(al + 5.0/6 - 2/(3*h));
    return a/(a + b*exp(2*w));
  }
  double lna = log(a/(a + b)), lnb = log(b/(a + b));
  t = exp(a*lna)/a;
  u = exp(b*lnb)/b;
  w = t + u;
  if (p < t/w) return pow(a*w*p, 1/a);
  return 1 - pow(b*w*(1 - p), 1/b);
}
// Newton iterations on log x for log I_x(a, b) = log p (p < 0.5; the
// upper half is solved through the symmetry I_x(a,b) = 1 - I_{1-x}(b,a))
RXG_FN double _rxg_ibetaInvRefine(double a, double b, double p, double x) {
  if (!(x > 0)) return RXG_MIN;
  if (!(x < 1)) x = 0.5; // the starting value rounded to 1
  double lt = log(p), lb = _rxg_lbeta(a, b), y = log(x);
  for (int j = 0; j < 200; j++) {
    x = exp(y);
    if (!(x < 1)) break;
    double li = _rxg_ibetaLog(a, b, x);
    // d log I/d log x = x f(x)/I
    double dg = exp(a*log(x) + (b - 1)*log1p(-x) - lb - li);
    if (!(dg > 0) || !R_FINITE(dg) || !R_FINITE(li)) break;
    double dy = fmax(-2.0, fmin(2.0, (li - lt)/dg));
    // stay below log(1) = 0
    if (y - dy >= 0) dy = 0.5*y;
    y -= dy;
    if (fabs(dy) < 4*RXG_EPS) break;
  }
  x = exp(y);
  return (x < RXG_MIN) ? RXG_MIN : x;
}
// argument checks, boundaries and closed forms of ibetaInv() (following the
// host: shapes <= 0, then the p = 0 and p = 1 boundaries, the a = 1 and b = 1
// closed forms, then infinite shapes give NaN); 1 when *out holds the answer
RXG_FN int _rxg_ibetaInvEdge(double a, double b, double p, double *out) {
  if (a <= 0 || b <= 0 || _rxg_probOut(p, 1 - p)) {
    *out = R_NaN;
  } else if (p == 0 || p == 1) {
    *out = p;
  } else if (ISNAN(a) || ISNAN(b)) {
    *out = R_NaN;
  } else if (b == 1) {
    *out = fmax(exp(log(p)/a), RXG_MIN);
  } else if (a == 1) {
    *out = -expm1(log1p(-p)/b);
  } else if (!R_FINITE(a) || !R_FINITE(b)) {
    *out = R_NaN;
  } else {
    return 0;
  }
  return 1;
}
// the upper half through I_x(a,b) = 1 - I_{1-x}(b,a); 1 - y cannot carry a
// small x, so those are refined directly
RXG_FN double _rxg_ibetaInvUpper(double a, double b, double p) {
  double x = 1.0 - _rxg_ibetaInvRefine(b, a, 1 - p, _rxg_ibetaInvStart(b, a, 1 - p));
  return (x < 1e-8) ? _rxg_ibetaInvRefine(a, b, p, _rxg_ibetaSmallX(a, b, p)) : x;
}
// (an underflowing quantile is DBL_MIN, as on the host)
RXG_FN double ibetaInv(double a, double b, double p) {
  double out;
  if (_rxg_ibetaInvEdge(a, b, p, &out)) return out;
  if (p > 0.5) return _rxg_ibetaInvUpper(a, b, p);
  return _rxg_ibetaInvRefine(a, b, p, _rxg_ibetaInvStart(a, b, p));
}

////////////////////////////////////////////////////////////////////////////
// Shape derivatives: the host's central difference with one Richardson
// extrapolation (src/boost.cpp)

RXG_FN double _rxg_shapeStep(double a) {
  double h = 1.0e-4*(fabs(a) > 1.0 ? fabs(a) : 1.0);
  volatile double tmp = a + h;
  h = tmp - a;
  return h;
}
#define _RXG_RICHARDSON(EP, EM, EP2, EM2, H) \
  ((4.0*(((EP2) - (EM2))/(H)) - ((EP) - (EM))/(2.0*(H)))/3.0)

RXG_FN double gammapDera(double a, double z) {
  double h = _rxg_shapeStep(a);
  return _RXG_RICHARDSON(gammap(a + h, z), gammap(a - h, z),
                         gammap(a + 0.5*h, z), gammap(a - 0.5*h, z), h);
}
RXG_FN double ibetaDera(double a, double b, double x) {
  double h = _rxg_shapeStep(a);
  return _RXG_RICHARDSON(ibeta(a + h, b, x), ibeta(a - h, b, x),
                         ibeta(a + 0.5*h, b, x), ibeta(a - 0.5*h, b, x), h);
}
RXG_FN double ibetaDerb(double a, double b, double x) {
  double h = _rxg_shapeStep(b);
  return _RXG_RICHARDSON(ibeta(a, b + h, x), ibeta(a, b - h, x),
                         ibeta(a, b + 0.5*h, x), ibeta(a, b - 0.5*h, x), h);
}

////////////////////////////////////////////////////////////////////////////
// Student t on the incomplete beta (src/boost.cpp)

RXG_FN double studentTDen(double x, double nu) {
  return exp(lgamma(0.5*(nu + 1.0)) - lgamma(0.5*nu) - 0.5*log(nu*M_PI) -
             0.5*(nu + 1.0)*log1p(x*x/nu));
}
RXG_FN double studentTCdf(double x, double nu) {
  double tail = 0.5*ibeta(0.5*nu, 0.5, nu/(nu + x*x));
  return (x <= 0.0) ? tail : 1.0 - tail;
}
RXG_FN double studentTCdfDnu(double x, double nu) {
  double h = _rxg_shapeStep(nu);
  return _RXG_RICHARDSON(studentTCdf(x, nu + h), studentTCdf(x, nu - h),
                         studentTCdf(x, nu + 0.5*h), studentTCdf(x, nu - 0.5*h), h);
}
RXG_FN double studentTInv(double p, double nu) {
  if (ISNAN(p) || ISNAN(nu)) return NA_REAL;
  if (p <= 0.0) return R_NegInf;
  if (p >= 1.0) return R_PosInf;
  if (p == 0.5) return 0.0;
  int lower = (p < 0.5);
  if (nu == R_PosInf) return lower ? R_NegInf : R_PosInf;
  double q = lower ? 2.0*p : 2.0*(1.0 - p);
  double x = ibetaInv(0.5*nu, 0.5, q);
  if (!(x > 0.0)) return lower ? R_NegInf : R_PosInf;
  double t = sqrt(nu*(1.0 - x)/x);
  return lower ? -t : t;
}

#endif // RXG_SPECIAL_H
