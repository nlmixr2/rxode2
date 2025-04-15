rxTest({
  # Test symengine<->rxode2 dsl
  test_that("d/dt(x) parsing", {
    expect_equal(rxToSE(d / dt(matt)), "rx__d_dt_matt__")
    expect_equal(rxFromSE(rx__d_dt_matt__), "d/dt(matt)")
    expect_equal(rxToSE(d / dt(matt)), "rx__d_dt_matt__")
    expect_equal(rxFromSE("rx__d_dt_matt__"), "d/dt(matt)")
    tmp <- symengine::Symbol("rx__d_dt_matt__")
    expect_equal(rxFromSE(tmp), "d/dt(matt)")
    expect_equal(rxToSE(d / dt(E)), "rx__d_dt_E__")
    expect_equal(rxFromSE("rx__d_dt_E__"), "d/dt(E)")
    tmp <- symengine::Symbol("rx__d_dt_E__")
    expect_equal(rxFromSE(tmp), "d/dt(E)")
  })

  test_that("df(x)/dy(x) parsing", {
    expect_equal(rxToSE(df(matt) / dy(ruth)), "rx__df_matt_dy_ruth__")
    expect_equal(rxFromSE(rx__df_matt_dy_ruth__), "df(matt)/dy(ruth)")
    expect_equal(rxToSE(df(matt) / dy(ruth)), "rx__df_matt_dy_ruth__")
    expect_equal(rxFromSE("rx__df_matt_dy_ruth__"), "df(matt)/dy(ruth)")
    tmp <- symengine::Symbol("rx__df_matt_dy_ruth__")
    expect_equal(rxFromSE(tmp), "df(matt)/dy(ruth)")
  })

  test_that("function and constant translation", {
    expect_equal(rxFromSE("x^1"), "x")
    expect_equal(rxToSE(gammafn(a)), "gamma(a)")
    expect_error(rxToSE(gammafn(a, b)))
    expect_error(rxFromSE(gamma(a, b)))

    expect_equal(rxToSE(lgammafn(a)), "lgamma(a)")
    expect_equal(rxToSE(lgammafn(a)), "lgamma(a)")
    expect_equal(rxFromSE(loggamma(a)), "lgamma(a)")
    expect_equal(rxFromSE(loggamma(1 + a)), "lgamma1p(a)")
    expect_equal(rxFromSE(lgamma(1 + a)), "lgamma1p(a)")

    expect_equal(rxToSE(digamma(a)), "polygamma(0,a)")
    expect_equal(rxFromSE(polygamma(0, a)), "digamma(a)")

    expect_equal(rxToSE(trigamma(a)), "polygamma(1,a)")
    expect_equal(rxFromSE(polygamma(1, a)), "trigamma(a)")

    expect_equal(rxToSE(tetragamma(a)), "polygamma(2,a)")
    expect_equal(rxFromSE(polygamma(2, a)), "tetragamma(a)")

    expect_equal(rxToSE(pentagamma(a)), "polygamma(3,a)")
    expect_equal(rxFromSE(polygamma(3, a)), "pentagamma(a)")

    expect_equal(rxToSE(lbeta(a, b)), "log(beta(a,b))")
    expect_equal(rxFromSE(log(beta(a, b))), "lbeta(a,b)")

    expect_equal(rxToSE(lgamma1p(a)), "lgamma(a+1)")
    expect_equal(rxFromSE(lgamma(a + 1)), "lgamma1p(a)")
    expect_equal(rxFromSE(lgamma(1 + a)), "lgamma1p(a)")
    expect_equal(rxFromSE(lgamma(a + 1 + b)), "lgamma1p(a+b)")

    expect_equal(rxToSE(log1p(a)), "log(1+a)")
    expect_equal(rxFromSE(log(a + 1)), "log1p(a)")

    expect_equal(rxToSE(cospi(a)), "cos(pi*(a))")
    expect_equal(rxFromSE(cos(pi * a)), "cospi(a)")
    expect_equal(rxFromSE(cos(pi * (a))), "cospi(a)")

    expect_equal(rxFromSE(cos(b * pi * a)), "cospi(b*a)")

    expect_equal(rxToSE(sinpi(a)), "sin(pi*(a))")
    expect_equal(rxFromSE(sin(pi * a)), "sinpi(a)")

    expect_equal(rxToSE(tanpi(a)), "tan(pi*(a))")
    expect_equal(rxFromSE(tan(pi * a)), "tanpi(a)")
    expect_equal(rxFromSE("tan(pi/2)"), "tanpi(1/2)")

    expect_equal(rxToSE(log1pmx(a)), "(log(1+a)-(a))")
    expect_equal(rxToSE(expm1(a)), "(exp(a)-1)")
    expect_equal(rxToSE(pow(a, b)), "(a)^(b)")
    expect_error(rxToSE(pow(a, b, c)))
    expect_error(rxToSE(pow(a)))
    expect_equal(rxToSE(R_pow(a, b)), "(a)^(b)")
    expect_equal(rxToSE(Rx_pow_di(a, b)), "(a)^(b)")
    expect_equal(rxToSE(Rx_pow(a, b)), "(a)^(b)")
    expect_equal(rxToSE(R_pow_di(a, b)), "(a)^(b)")
    expect_equal(rxToSE(factorial(n)), "gamma(n+1)")

    expect_equal(rxToSE(beta(a, b)), "beta(a,b)")
    expect_error(rxToSE(beta(a)))

    expect_equal(rxToSE(choose(n, k)), "gamma(n+1)/(gamma(k+1)*gamma(n-(k)+1))")
    expect_equal(rxToSE(lchoose(n, k)), "(lgamma(n+1)-lgamma(k+1)-lgamma(n-(k)+1))")

    expect_equal(rxFromSE(log(-x + 1)), "log1p(-x)")
    expect_equal(rxFromSE(log(1 - x)), "log1p(-x)")

    expect_equal(rxToSE(log1pexp(x)), "log(1+exp(x))")
    expect_equal(rxFromSE(log(1 + exp(x))), "log1pexp(x)")

    ## expect_equal(rxFromSymPy(log((1 + x)^2)), "log1p(Rx_pow_di(x, 2)+2 * x)")
    ## expect_equal(rxFromSymPy(log((0.75 + x)^2)), "log1p(Rx_pow_di(x, 2)+1.5 * x-0.4375)")

    expect_equal(rxFromSE(E), "2.7182818284590451")
    expect_equal(rxToSE(M_E), "E")

    expect_equal(rxToSE(exp(1)), "E")
    expect_equal(rxFromSE(exp(1)), "M_E")

    expect_equal(rxFromSE(log(2)), "M_LN2")
    expect_equal(rxFromSE(log(10)), "M_LN10")

    expect_equal(rxToSE("M_LN2"), "log(2)")
    expect_equal(rxFromSE("log(2)"), "M_LN2")

    expect_equal(rxToSE("M_LN10"), "log(10)")
    expect_equal(rxFromSE("log(10)"), "M_LN10")

    expect_equal(rxToSE("M_LN_SQRT_PI"), "log(sqrt(pi))")

    expect_equal(rxFromSE("log(sqrt(pi))"), "M_LN_SQRT_PI")
    expect_equal(rxFromSE("log(pi**0.5)"), "M_LN_SQRT_PI")
    expect_equal(rxFromSE("log(pi^0.5)"), "M_LN_SQRT_PI")
    expect_equal(rxFromSE("log(pi^(1/2))"), "M_LN_SQRT_PI")
    expect_equal(rxFromSE("log(pi**(1/2))"), "M_LN_SQRT_PI")

    expect_equal(rxToSE("M_LN_SQRT_PId2"), "log(sqrt(pi/2))")
    expect_equal(rxFromSE(log(sqrt(pi / 2))), "M_LN_SQRT_PId2")
    expect_equal(rxFromSE(log(sqrt(pi * 0.5))), "M_LN_SQRT_PId2")
    expect_equal(rxFromSE(log(sqrt(0.5 * pi))), "M_LN_SQRT_PId2")

    expect_equal(rxFromSE(log((pi / 2)^0.5)), "M_LN_SQRT_PId2")
    expect_equal(rxFromSE(log((pi * 0.5)^0.5)), "M_LN_SQRT_PId2")
    expect_equal(rxFromSE(log((0.5 * pi)^0.5)), "M_LN_SQRT_PId2")

    expect_equal(rxFromSE(log((pi / 2)**0.5)), "M_LN_SQRT_PId2")
    expect_equal(rxFromSE(log((pi * 0.5)**0.5)), "M_LN_SQRT_PId2")
    expect_equal(rxFromSE(log((0.5 * pi)**0.5)), "M_LN_SQRT_PId2")

    expect_equal(rxFromSE(log((pi / 2)**(1 / 2))), "M_LN_SQRT_PId2")
    expect_equal(rxFromSE(log((pi * 0.5)**(1 / 2))), "M_LN_SQRT_PId2")
    expect_equal(rxFromSE(log((0.5 * pi)**(1 / 2))), "M_LN_SQRT_PId2")

    expect_equal(rxFromSE(log((pi / 2)^(1 / 2))), "M_LN_SQRT_PId2")
    expect_equal(rxFromSE(log((pi * 0.5)^(1 / 2))), "M_LN_SQRT_PId2")
    expect_equal(rxFromSE(log((0.5 * pi)^(1 / 2))), "M_LN_SQRT_PId2")

    expect_equal(rxToSE("M_LN_SQRT_2PI"), "log(sqrt(2*pi))")
    expect_equal(rxFromSE(log((pi * 2)^0.5)), "M_LN_SQRT_2PI")
    expect_equal(rxFromSE(log((2 * pi)^0.5)), "M_LN_SQRT_2PI")

    expect_equal(rxFromSE(log((pi * 2)**0.5)), "M_LN_SQRT_2PI")
    expect_equal(rxFromSE(log((2 * pi)**0.5)), "M_LN_SQRT_2PI")

    expect_equal(rxFromSE(log((pi * 2)**(1 / 2))), "M_LN_SQRT_2PI")
    expect_equal(rxFromSE(log((2 * pi)**(1 / 2))), "M_LN_SQRT_2PI")

    expect_equal(rxFromSE(log((pi * 2)^(1 / 2))), "M_LN_SQRT_2PI")
    expect_equal(rxFromSE(log((2 * pi)^(1 / 2))), "M_LN_SQRT_2PI")


    expect_equal(rxToSE("M_SQRT_3"), "sqrt(3)")
    expect_equal(rxFromSE("sqrt(3)"), "M_SQRT_3")

    expect_equal(rxToSE("M_SQRT2"), "sqrt(2)")
    expect_equal(rxFromSE("sqrt(2)"), "M_SQRT2")

    expect_equal(rxToSE("M_SQRT_32"), "sqrt(32)")
    expect_equal(rxFromSE("sqrt(32)"), "M_SQRT_32")

    expect_equal(rxToSE("M_SQRT_PI"), "sqrt(pi)")
    expect_equal(rxFromSE("sqrt(pi)"), "M_SQRT_PI")

    expect_equal(rxToSE("M_SQRT_2dPI"), "sqrt(2/pi)")
    expect_equal(rxFromSE("sqrt(2/pi)"), "M_SQRT_2dPI")
    expect_equal(rxFromSE("(2/pi)^0.5"), "M_SQRT_2dPI")
    expect_equal(rxFromSE("(2/pi)^(1/2)"), "M_SQRT_2dPI")

    expect_equal(rxToSE("M_PI_2"), "pi/2")
    expect_equal(rxFromSE("pi*0.5"), "M_PI_2")
    expect_equal(rxFromSE("0.5*pi"), "M_PI_2")
    expect_equal(rxFromSE("pi/2"), "M_PI_2")

    expect_equal(rxToSE("M_PI_4"), "pi/4")
    expect_equal(rxFromSE("pi*0.25"), "M_PI_4")
    expect_equal(rxFromSE("0.25*pi"), "M_PI_4")
    expect_equal(rxFromSE("pi/4"), "M_PI_4")

    expect_equal(rxFromSE("1/pi"), "M_1_PI")
    expect_equal(rxToSE("M_1_PI"), "1/pi")

    expect_equal(rxFromSE("2/pi"), "M_2_PI")
    expect_equal(rxToSE("M_2_PI"), "2/pi")

    expect_equal(rxToSE("M_2_SQRTPI"), "2/sqrt(pi)")
    expect_equal(rxFromSE("2/sqrt(pi)"), "M_2_SQRTPI")
    expect_equal(rxFromSE("2/(pi^0.5)"), "M_2_SQRTPI")

    expect_equal(rxToSE(M_1_SQRT_2PI), "1/sqrt(2*pi)")
    expect_equal(rxFromSE("1/sqrt(2*pi)"), "M_1_SQRT_2PI")
    expect_equal(rxFromSE("1/sqrt(pi*2)"), "M_1_SQRT_2PI")
    expect_equal(rxFromSE("1/((pi*2)^(1/2))"), "M_1_SQRT_2PI")
    expect_equal(rxFromSE("1/((pi*2)^0.5)"), "M_1_SQRT_2PI")

    expect_equal(rxToSE(log10(a)), "log(a)/log(10)")
    expect_equal(rxToSE(log2(a)), "log(a)/log(2)")
    ## FIXME log10 log2? fromSE?


    ## expect_equal(rxFromSymPy("3 + 4*3+2+2*matt*pi"), "3 + 4 * 3 + 2 + matt * M_2PI")
    ## expect_equal(rxFromSymPy("3 + 4*3+2+pi*matt*2"), "3 + 4 * 3 + 2 + matt * M_2PI")
  })

  test_that("transit compartment translation.", {

    expect_equal(
      rxToSE(transit(n, mtt, bio)),
      "exp(log((bio)*(podo0()))+log(n + 1)-log(mtt)+(n)*((log(n+1)-log(mtt))+log(t-tlast0()))-((n+1)/(mtt))*(t-tlast0())-lgamma(1+n))")

    expect_equal(
      rxToSE(transit(n, mtt)),
      "exp(log(podo0())+(log(n+1)-log(mtt))+(n)*((log(n+1)-log(mtt))+ log(t-tlast0()))-((n + 1)/(mtt))*(t-tlast0())-lgamma(1+n))")

    tmp <- rxode("d/dt(depot) <- transit(n, mtt, bio)-ka*depot\nd/dt(center)=ka*depot-kel*center")

    tmp2 <- rxS(tmp)
    tmp3 <- tmp2$rx__d_dt_depot__
    expect_equal(rxFromSE(tmp3), "-ka*depot+exp(n*(-log(mtt)+log1p(n)+log(t-tlast0(depot)))-(1+n)*(t-tlast0(depot))/mtt-log(mtt)+log(bio*podo0(depot))+log1p(n)-lgamma1p(n))")

    tmp <- rxode("d/dt(depot) <- transit(n, mtt) - ka*depot\nd/dt(center)=ka*depot-kel*center")
    tmp2 <- rxS(tmp)
    tmp3 <- tmp2$rx__d_dt_depot__
    expect_equal(rxFromSE(tmp3), "-ka*depot+exp(n*(-log(mtt)+log1p(n)+log(t-tlast0(depot)))-(1+n)*(t-tlast0(depot))/mtt-log(mtt)+log1p(n)+log(podo0(depot))-lgamma1p(n))")

  })

  test_that("unknown functions throw errors. rxToSE", {
    expect_error(rxToSE(matt(3)))
  })

  test_that("Theta/eta conversion rxToSE", {
    expect_equal(rxToSE(THETA[1]), "THETA_1_")
    expect_equal(rxToSE(ETA[1]), "ETA_1_")
    expect_equal(rxToSE(df(matt) / dy(THETA[1])), "rx__df_matt_dy_THETA_1___")
    expect_equal(rxToSE(df(matt) / dy(ETA[1])), "rx__df_matt_dy_ETA_1___")
    expect_error(rxToSE(THETA[0]))
    expect_error(rxToSE(ETA[0]))
    expect_error(rxToSE(THETA[0.5]))
    expect_error(rxToSE(ETA[0.5]))
    expect_error(rxToSE(THETA[a]))
    expect_error(rxToSE(ETA[a]))
    expect_error(rxToSE(THETA["b"]))
    expect_error(rxToSE(ETA["b"]))
  })

  test_that("Extra Derivative Table Test", {
    expect_equal(rxFromSE("Derivative(rxTBS(a, b, c, d, f), a)"), "rxTBSd(a,b,c,d,f)")
    expect_equal(rxFromSE("Derivative(rxTBSd(a, b, c, d, f), a)"), "rxTBSd2(a,b,c,d,f)")
    expect_error(rxFromSE("Derivative(rxTBSd2(a, b, c, d, f), a)", unknownDerivatives = "error"))
    expect_error(rxFromSE("Derivative(rxTBS(a, b, c, d, f), d)", unknownDerivatives = "error"))
    expect_error(rxFromSE("Derivative(f(a, b, c), a)", unknownDerivatives = "forward"))
    expect_equal(
      rxFromSE("(2*a + b)*Subs(Derivative(rxTBS(_xi_1, b, c, d, f), _xi_1), (_xi_1), (a*b + a^2))"),
      "(2*a+b)*rxTBSd(a*b+Rx_pow_di(a,2),b,c,d,f)"
    )
  })

  test_that("NN Activation functions derivatives", {

    expect_equal(rxFromSE("Derivative(ReLU(x), x)"), "dReLU(x)")
    expect_equal(rxFromSE("Derivative(dReLU(x), x)"), "0")

    expect_equal(rxFromSE("Derivative(GELU(x), x)"), "dGELU(x)")
    expect_equal(rxFromSE("Derivative(dGELU(x), x)"), "d2GELU(x)")
    expect_equal(rxFromSE("Derivative(d2GELU(x), x)"), "d3GELU(x)")
    expect_equal(rxFromSE("Derivative(d3GELU(x), x)"), "d4GELU(x)")

    expect_equal(rxToSE("d4GELU(x)"), "exp(-(x)^2/2)*(7*(x)^2 - 4 - (x)^4)/sqrt(2*pi)")

    expect_equal(rxFromSE("Derivative(ELU(x, alpha), x)"), "dELU(x, alpha)")
    expect_equal(rxFromSE("Derivative(ELU(x, alpha), alpha)"), "dELUa(x, alpha)")

    expect_equal(rxFromSE("Derivative(dELU(x, alpha), x)"), "d2ELU(x, alpha)")
    expect_equal(rxFromSE("Derivative(dELU(x, alpha), alpha)"), "d2aELU(x, alpha)")

    expect_equal(rxFromSE("Derivative(dELUa(x, alpha), x)"), "d2ELUa(x, alpha)")
    expect_equal(rxFromSE("Derivative(dELUa(x, alpha), alpha)"), "0")

    expect_equal(rxFromSE("Derivative(d2ELUa(x, alpha), x)"), "d2ELUa(x, alpha)")
    expect_equal(rxFromSE("Derivative(d2ELUa(x, alpha), alpha)"), "0")

    expect_equal(rxFromSE("Derivative(d2aELU(x, alpha), x)"), "d2aELU(x, alpha)")
    expect_equal(rxFromSE("Derivative(d2aELU(x, alpha), alpha)"), "0")

    expect_equal(rxFromSE("Derivative(softplus(x), x)"),   "dsoftplus(x)")
    expect_equal(rxFromSE("Derivative(dsoftplus(x), x)"),  "d2softplus(x)")
    expect_equal(rxFromSE("Derivative(d2softplus(x), x)"), "d3softplus(x)")
    expect_equal(rxFromSE("Derivative(d3softplus(x), x)"), "d4softplus(x)")

    expect_equal(rxToSE("d4softplus(x)"),
                 "6.0*exp(-3.0*(x))/(((1.0 + exp(-(x))))^4) - 6.0*exp(-2.0*(x))/(((1.0 + exp(-(x))))^3) + exp(-(x))/(((1.0 + exp(-(x))))^2)")

    expect_equal(rxFromSE("Derivative(SELU(x), x)"), "dSELU(x)")

    expect_equal(rxToSE("dSELU(x)"),
                 "(rxGt(x, 0)*1.0507009873554804934193349852946 + 1.0507009873554804934193349852946*1.6732632423543772848170429916717*exp(x)*rxLeq(x, 0))")

    expect_equal(rxFromSE("Derivative(lReLU(x), x)"), "dlReLU(x)")
    expect_equal(rxFromSE("Derivative(dlReLU(x), x)"), "0")

    expect_equal(rxFromSE("Derivative(PReLU(x, alpha), x)"), "dPReLU(x,alpha)")
    expect_equal(rxFromSE("Derivative(PReLU(x, alpha), alpha)"), "dPReLUa(x,alpha)")

    expect_equal(rxFromSE("Derivative(dPReLU(x, alpha), x)"), "0")
    expect_equal(rxFromSE("Derivative(dPReLU(x, alpha), alpha)"), "dPReLUa1(x,alpha)")

    expect_equal(rxFromSE("Derivative(dPReLUa(x, alpha), x)"), "dPReLUa1(x,alpha)")
    expect_equal(rxFromSE("Derivative(dPReLUa(x, alpha), alpha)"), "0")

    expect_equal(rxFromSE("Derivative(dPReLUa1(x, alpha), x)"), "0")
    expect_equal(rxFromSE("Derivative(dPReLUa1(x, alpha), alpha)"), "0")

    expect_equal(rxFromSE("Derivative(Swish(x), x)"), "dSwish(x)")

    expect_equal(rxToSE("dSwish(x)"),
                 "((x)*exp(-(x))/(1.0 + exp(-(x)))^2 + 1.0/(1.0 + exp(-(x)))")

  })

  test_that("logic tests", {
    expect_equal(rxFromSE("rxEq(a,b)"), "(a==b)")
    expect_equal(rxFromSE("rxNeq(a,b)"), "(a!=b)")
    expect_equal(rxFromSE("rxLt(a,b)"), "(a<b)")
    expect_equal(rxFromSE("rxGt(a,b)"), "(a>b)")
    expect_equal(rxFromSE("rxGeq(a,b)"), "(a>=b)")
    expect_equal(rxFromSE("rxLeq(a,b)"), "(a<=b)")
    expect_equal(rxFromSE("rxAnd(a,b)"), "(a&&b)")
    expect_equal(rxFromSE("rxOr(a,b)"), "(a||b)")
    expect_equal(rxFromSE("rxNot(rxOr(a,b))"), "(!((a||b)))")

    expect_equal(rxToSE("(a==b)"), "(rxEq(a,b))")
    expect_equal(rxToSE("(a!=b)"), "(rxNeq(a,b))")
    expect_equal(rxToSE("(a>=b)"), "(rxGeq(a,b))")
    expect_equal(rxToSE("(a<=b)"), "(rxLeq(a,b))")
    expect_equal(rxToSE("(a<b)"), "(rxLt(a,b))")
    expect_equal(rxToSE("(a>b)"), "(rxGt(a,b))")
    expect_equal(rxToSE("(a&b)"), "(rxAnd(a,b))")
    expect_equal(rxToSE("(a&&b)"), "(rxAnd(a,b))")
    expect_equal(rxToSE("(a|b)"), "(rxOr(a,b))")
    expect_equal(rxToSE("(a||b)"), "(rxOr(a,b))")
    expect_equal(rxToSE("!(a||b)"), "rxNot((rxOr(a,b)))")

    expect_equal(
      rxFromSE("Derivative(rxEq(a,b), a)"),
      "(-20*tanh(10*(a-b))+20*tanh(10*(a-b))^3)"
    )
    expect_equal(
      rxFromSE("Derivative(rxEq(a,b), b)"),
      "(20*tanh(10*(a-b))-20*tanh(10*(a-b))^3)"
    )

    expect_equal(
      rxFromSE("Derivative(rxGeq(a,b), a)"),
      "(5-5*tanh(4.60512018348798+10*(a-b))^2)"
    )

    expect_equal(
      rxFromSE("Derivative(rxGeq(a,b), b)"),
      "(-5+5*tanh(4.60512018348798+10*(a-b))^2)"
    )

    expect_equal(
      rxFromSE("Derivative(rxLeq(a,b), a)"),
      "(-5+5*tanh(-4.60512018348798+10*(a-b))^2)"
    )
    expect_equal(
      rxFromSE("Derivative(rxLeq(a,b), b)"),
      "(5-5*tanh(-4.60512018348798+10*(a-b))^2)"
    )

    expect_equal(
      rxFromSE("Derivative(rxLt(a,b), a)"),
      "(-5+5*tanh(4.60512018348798+10*(a-b))^2)"
    )
    expect_equal(
      rxFromSE("Derivative(rxLt(a,b), b)"),
      "(5-5*tanh(4.60512018348798+10*(a-b))^2)"
    )

    expect_equal(
      rxFromSE("Derivative(rxGt(a,b), a)"),
      "(5-5*tanh(-4.60512018348798+10*(a-b))^2)"
    )
    expect_equal(
      rxFromSE("Derivative(rxGt(a,b), b)"),
      "(-5+5*tanh(-4.60512018348798+10*(a-b))^2)"
    )

    expect_equal(
      rxFromSE("Derivative(rxOr(a,b), a)"),
      "(1-(b))"
    )
    expect_equal(
      rxFromSE("Derivative(rxOr(a,b), b)"),
      "(1-(a))"
    )

    expect_equal(
      rxFromSE("Derivative(rxNot(a), a)"),
      "(-1)"
    )
  })

  # Test factor expansion by `rxSplitPlusQ'
  test_that("rxSplitPlusQ", {
    expect_equal(rxSplitPlusQ(quote(a * exp(b + c) + d * log(e - f) - g * f)), c("a * exp(b + c)", "d * log(e - f)", "- g * f"))
    expect_equal(rxSplitPlusQ(quote(-a * exp(b + c) + d * log(e - f) - g * f)), c("-a * exp(b + c)", "d * log(e - f)", "- g * f"))
    expect_equal(rxSplitPlusQ(quote(+a * exp(b + c) + d * log(e - f) - g * f)), c("+a * exp(b + c)", "d * log(e - f)", "- g * f"))
    expect_equal(rxSplitPlusQ(quote(+a * exp(b + c))), "+a * exp(b + c)")
    expect_equal(rxSplitPlusQ(quote(center)), "center")
    expect_equal(rxSplitPlusQ(quote(0)), "0")
  })

  # Test Error DSLs

  pk <- function() {
    KA <- exp(THETA[1])
    CL <- exp(THETA[2] + ETA[1])
    V <- exp(THETA[3] + ETA[2])
    return(1)
  }

  pred <- function() {
    if (cmt == 1) {
      return(centr)
    } else if (cmt == 2) {
      return(depot)
    } else {
      return(perip)
    }
  }


  pred.for <- function() {
    for (i in 1:10) {
      pred <- 1
    }
  }

  err <- function() {
    add(0.2) + prop(0.3)
  }

  test_that("PK/Pred/Error function parsing.", {
    expect_error(rxParsePk(pk))
    expect_error(rxParsePred(pred), NA)
    expect_error(rxParsePred(pred.for))
    expect_error(rxParsePk(err))
    expect_error(rxParsePred(err))
  })


  pk2 <- function() {
    KA <- exp(THETA[1])
    CL <- exp(THETA[2] + ETA[1])
    V <- exp(THETA[3] + ETA[2])
  }

  test_that("sum/prod testing", {
    expect_equal(rxToSE("sum(a,b,c)"), "((a)+(b)+(c))")
    expect_equal(rxToSE("prod(a,b,c)"), "((a)*(b)*(c))")
  })

  test_that("tlast", {
    expect_error(rxToSE("tlast(matt+3)"))
    expect_error(rxToSE("tlast(matt, 3)"))
    expect_equal(rxToSE("tlast(matt)"), "tlast(matt)")
    expect_equal(rxToSE("tlast()"), "tlast()")
    expect_equal(rxFromSE("Derivative(tlast(matt), matt)"), "0")
    expect_equal(rxFromSE("tlast(matt)"), "tlast(matt)")

    expect_error(rxFromSE("tlast(matt,2)"))
    expect_equal(rxFromSE("tlast(matt)"), "tlast(matt)")
    expect_equal(rxFromSE("tlast()"), "tlast()")
  })

  test_that("tfirst", {
    expect_error(rxToSE("tfirst(matt+3)"))
    expect_error(rxToSE("tfirst(matt, 3)"))
    expect_equal(rxToSE("tfirst(matt)"), "tfirst(matt)")
    expect_equal(rxToSE("tfirst()"), "tfirst()")
    expect_equal(rxFromSE("Derivative(tfirst(matt), matt)"), "0")
    expect_equal(rxFromSE("tfirst(matt)"), "tfirst(matt)")

    expect_error(rxFromSE("tfirst(matt,2)"))
    expect_equal(rxFromSE("tfirst(matt)"), "tfirst(matt)")
    expect_equal(rxFromSE("tfirst()"), "tfirst()")
  })

  test_that("dosenum", {
    expect_error(rxToSE("dosenum(a)"))
    expect_equal(rxToSE("dosenum()"), "dosenum()")
    expect_equal(rxFromSE("dosenum()"), "dosenum()")
  })

  test_that("tad()", {
    expect_equal(rxToSE("tad()"), "(t-tlast())")
    expect_equal(rxToSE("tad(matt)"), "(t-tlast(matt))")
    expect_error(rxToSE("tad(matt,f)"))
    expect_error(rxToSE("tad(matt+f)"))
  })

  test_that("tad0()", {
    expect_equal(rxToSE("tad0()"), "(t-tlast0())")
    expect_equal(rxToSE("tad0(matt)"), "(t-tlast0(matt))")
    expect_error(rxToSE("tad0(matt,f)"))
    expect_error(rxToSE("tad0(matt+f)"))
  })

  test_that("tafd()", {
    expect_equal(rxToSE("tafd()"), "(t-tfirst())")
    expect_equal(rxToSE("tafd(matt)"), "(t-tfirst(matt))")
    expect_error(rxToSE("tafd(matt,f)"))
    expect_error(rxToSE("tafd(matt+f)"))
  })

  test_that("lag()", {
    expect_error(rxToSE("lag()"))
    expect_equal(rxToSE("lag(b)"), "lag(b)")
    expect_error(rxToSE("lag(b+3)"))
    expect_equal(rxToSE("lag(b, 3)"), "lag(b, 3)")
    expect_error(rxToSE("lag(b, c)"))
    expect_error(rxToSE("lag(b, 3+4)"))
    expect_equal(rxFromSE("Derivative(lag(a,b), a)"), "0")
    expect_equal(rxFromSE("Derivative(lag(a,b), b)"), "0")
    expect_equal(rxFromSE("Derivative(lag(a), a)"), "0")
    expect_equal(rxFromSE("lag(a,b)"), "lag(a,b)")
    expect_equal(rxFromSE("lag(a)"), "lag(a)")
  })

  test_that("lead()", {
    expect_error(rxToSE("lead()"))
    expect_equal(rxToSE("lead(b)"), "lead(b)")
    expect_error(rxToSE("lead(b+3)"))
    expect_equal(rxToSE("lead(b, 3)"), "lead(b, 3)")
    expect_error(rxToSE("lead(b, c)"))
    expect_error(rxToSE("lead(b, 3+4)"))
    expect_equal(rxFromSE("Derivative(lead(a,b), a)"), "0")
    expect_equal(rxFromSE("Derivative(lead(a,b), b)"), "0")
    expect_equal(rxFromSE("Derivative(lead(a), a)"), "0")
    expect_equal(rxFromSE("lead(a,b)"), "lead(a,b)")
    expect_equal(rxFromSE("lead(a)"), "lead(a)")
  })

  test_that("first", {
    expect_error(rxToSE("first()"))
    expect_equal(rxToSE("first(v)"), "first(v)")
    expect_equal(rxFromSE("Derivative(first(a),a)"), "0")
    expect_equal(rxFromSE("first(v)"), "first(v)")
  })

  test_that("last", {
    expect_error(rxToSE("last()"))
    expect_equal(rxToSE("last(v)"), "last(v)")
    expect_equal(rxFromSE("Derivative(last(a),a)"), "0")
    expect_equal(rxFromSE("last(v)"), "last(v)")
  })

  test_that("diff", {
    expect_error(rxToSE("diff()"))
    expect_equal(rxToSE("diff(v)"), "diff(v)")
    expect_equal(rxFromSE("Derivative(diff(a),a)"), "0")
    expect_equal(rxFromSE("diff(v)"), "diff(v)")
  })

  test_that("is.nan", {
    expect_error(rxToSE("is.nan()"))
    expect_equal(rxToSE("is.nan(v)"), "is.nan(v)")
    expect_equal(rxFromSE("Derivative(is.nan(a),a)"), "0")
    expect_equal(rxFromSE("is.nan(v)"), "is.nan(v)")
  })

  test_that("is.na", {
    expect_error(rxToSE("is.na()"))
    expect_equal(rxToSE("is.na(v)"), "is.na(v)")
    expect_equal(rxFromSE("Derivative(is.na(a),a)"), "0")
    expect_equal(rxFromSE("is.na(v)"), "is.na(v)")
  })

  test_that("is.finite", {
    expect_error(rxToSE("is.finite()"))
    expect_equal(rxToSE("is.finite(v)"), "is.finite(v)")
    expect_equal(rxFromSE("Derivative(is.finite(a),a)"), "0")
    expect_equal(rxFromSE("is.finite(v)"), "is.finite(v)")
  })

  test_that("is.infinite", {
    expect_error(rxToSE("is.infinite()"))
    expect_equal(rxToSE("is.infinite(v)"), "is.infinite(v)")
    expect_equal(rxFromSE("Derivative(is.infinite(a),a)"), "0")
    expect_equal(rxFromSE("is.infinite(v)"), "is.infinite(v)")
  })

  test_that("dual function/variable promotion",{
    expect_equal(rxToSE("tlast"), "tlast()")
    expect_equal(rxFromSE("tlast()"), "tlast()")
    expect_equal(rxToSE("tad"), "(t-tlast())")
    expect_equal(rxFromSE("tlast(NaN)"), "tlast()")
  })

  test_that("max/min tests", {
    expect_equal(rxToSE("max(a)"), "(a)")
    expect_equal(rxToSE("max(a,0)"), "((a)*rxGt(a,0))")
    expect_equal(rxToSE("max(0,a)"), "((a)*rxGt(a,0))")
    expect_equal(rxToSE("max(a,b)"), "(((a)-(b))*rxGt(a,b)+(b))")
    expect_equal(rxToSE("max(a,b,c)"),
                 "((((((a)-(b))*rxGt(a,b)+(b)))-(c))*rxGt((((a)-(b))*rxGt(a,b)+(b)),c)+(c))")
    expect_equal(rxToSE("max()"), "")
    expect_equal(rxToSE("min(a)"), "(a)")
    expect_equal(rxToSE("min(a,0)"), "((a)*rxLt(a,0))")
    expect_equal(rxToSE("min(0,a)"), "((a)*rxLt(a,0))")
    expect_equal(rxToSE("min(a,b)"), "(((a)-(b))*rxLt(a,b)+(b))")
    expect_equal(rxToSE("min(a,b,c)"),
                 "((((((a)-(b))*rxLt(a,b)+(b)))-(c))*rxLt((((a)-(b))*rxLt(a,b)+(b)),c)+(c))")
    expect_equal(rxToSE("min()"), "")
  })

  test_that("abs tests", {
    expect_equal(rxToSE("abs(a)"), "(2.0*(a)*rxGt(a,0.0)-(a))")
    expect_equal(rxToSE("fabs(a)"), "(2.0*(a)*rxGt(a,0.0)-(a))")
    expect_equal(rxToSE("abs0(a)"), "(2.0*(a)*rxGt(a,0.0)-(a))")
  })

  test_that("parsing errors", {

    test <- "E0=THETA[1];\nEm=0.5;\nE50=THETA[2];\ng=2;\nv=E0+Em*t^g/(E50^g+t^g);\nrx_yj_~152;\nrx_lambda_~1;\nrx_low_~0;\nrx_hi_~1;\nrx_r_~0;\nrx_pred_~DV*v-log(1+exp(v));\n"

    expect_error(rxS(test), NA)

  })

  test_that("Promote linear solution linCmtA to linear solution linCmtB -- linCmt() sol", {
    expect_equal(str2lang(rxToSE("linCmtA(rx__PTR__,t,2,1,1,-1,1,cl,v,q,v2,q2,v3,ka)",
                    promoteLinSens = TRUE)),
                 str2lang("linCmtB(rx__PTR__, t, 2, 1, 1, -1, -1, 1, cl, v, q, v2, q2, v3, ka)"))
  })

  test_that("Promote linear solution linCmtA to linear solution linCmtB -- amts", {
    for (i in 0:7) {
      str <- paste0("linCmtA(rx__PTR__,t,2,1,1,",i,",1,cl,v,q,v2,q2,v3,ka)")
      expect_equal(str2lang(rxToSE(str, promoteLinSens = TRUE)),
                   str2lang(paste0("linCmtB(rx__PTR__, t, 2, 1, 1, ", i, ", -2, 1, cl, v, q, v2, q2, v3, ka)")))
    }
  })

  testVars <- c("b=depot",
                "c=central",
                "d=peripheral1",
                "ee=peripheral2",
                "f=rx__sens_central_BY_p1",
                "g=rx__sens_central_BY_v1",
                "h=rx__sens_central_BY_p2",
                "i=rx__sens_central_BY_p3",
                "j=rx__sens_central_BY_p4",
                "k=rx__sens_central_BY_p5",
                "l=rx__sens_central_BY_ka",
                "m=rx__sens_peripheral1_BY_p1",
                "n=rx__sens_peripheral1_BY_v1",
                "o=rx__sens_peripheral1_BY_p2",
                "p=rx__sens_peripheral1_BY_p3",
                "q=rx__sens_peripheral1_BY_p4",
                "r=rx__sens_peripheral1_BY_p5",
                "s=rx__sens_peripheral1_BY_ka",
                "tt=rx__sens_peripheral2_BY_p1",
                "u=rx__sens_peripheral2_BY_v1",
                "v=rx__sens_peripheral2_BY_p2",
                "w=rx__sens_peripheral2_BY_p3",
                "x=rx__sens_peripheral2_BY_p4",
                "y=rx__sens_peripheral2_BY_p5",
                "z=rx__sens_peripheral2_BY_ka",
                "aa=rx__sens_depot_BY_ka")

  # Testing function for translation
  f <- function(w, num=NULL, s=s) {
    w <- get(w, envir=s)
    v <- rxFromSE(w)
    l <- str2lang(v)
    if (is.symbol(l)) {
      if (is.null(num)) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    }
    if (is.call(l) && (identical(l[[1]], quote(`linCmtA`)) ||
                         identical(l[[1]], quote(`linCmtB`)))) {
      return(l[[7]] == num)
    }
    FALSE
  }

  test_that("3 compartment oral states translated in symengine", {

    s <- paste(c("linc=linCmtA(rx__PTR__,t,2,3,1,-1,1,cl,v,q,v2,q2,v3,ka)",
                 testVars), collapse="\n")
    s <- rxS(s)

    expect_true(f("b", 0, s=s)) # depot
    expect_true(f("c", 1, s=s)) # central
    expect_true(f("d", 2, s=s)) # peripheral1
    expect_true(f("ee", 3, s=s)) # peripheral2
    expect_true(f("f", s=s))
    expect_true(f("g", s=s))
    expect_true(f("h", s=s))
    expect_true(f("i", s=s))
    expect_true(f("j", s=s))
    expect_true(f("k", s=s))
    expect_true(f("l", s=s))
    expect_true(f("m", s=s))
    expect_true(f("n", s=s))
    expect_true(f("o", s=s))
    expect_true(f("p", s=s))
    expect_true(f("q", s=s))
    expect_true(f("r", s=s))
    expect_true(f("s", s=s))
    expect_true(f("tt", s=s))
    expect_true(f("u", s=s))
    expect_true(f("v", s=s))
    expect_true(f("w", s=s))
    expect_true(f("x", s=s))
    expect_true(f("y", s=s))
    expect_true(f("z", s=s))
    expect_true(f("aa", s=s))

    s <- paste(c("linc=linCmtA(rx__PTR__,t,2,3,1,-1,1,cl,v,q,v2,q2,v3,ka)",
                 testVars), collapse="\n")
    s <- rxS(s, promoteLinSens = TRUE)

    expect_true(f("b", 0, s=s)) # depot
    expect_true(f("c", 1, s=s)) # central
    expect_true(f("d", 2, s=s)) # peripheral1
    expect_true(f("ee", 3, s=s)) # peripheral2
    expect_true(f("f", 4, s=s))
    expect_true(f("g", 5, s=s))
    expect_true(f("h", 6, s=s))
    expect_true(f("i", 7, s=s))
    expect_true(f("j", 8, s=s))
    expect_true(f("k", 9, s=s))
    expect_true(f("l", 10, s=s))
    expect_true(f("m", 11, s=s))
    expect_true(f("n", 12, s=s))
    expect_true(f("o", 13, s=s))
    expect_true(f("p", 14, s=s))
    expect_true(f("q", 15, s=s))
    expect_true(f("r", 16, s=s))
    expect_true(f("s", 17, s=s))
    expect_true(f("tt", 18, s=s))
    expect_true(f("u", 19, s=s))
    expect_true(f("v", 20, s=s))
    expect_true(f("w", 21, s=s))
    expect_true(f("x", 22, s=s))
    expect_true(f("y", 23, s=s))
    expect_true(f("z", 24, s=s))
    expect_true(f("aa", 25, s=s))
  })

  test_that("3 compartment states translated in symengine", {

    s <- paste(c("linc=linCmtA(rx__PTR__,t,2,3,0,-1,1,cl,v,q,v2,q2,v3,0)",
                 testVars), collapse="\n")
    s <- rxS(s)

    expect_true(f("b", s=s)) # depot
    expect_true(f("c", 0, s=s)) # central
    expect_true(f("d", 1, s=s)) # peripheral1
    expect_true(f("ee", 2, s=s)) # peripheral2
    expect_true(f("f", s=s))
    expect_true(f("g", s=s))
    expect_true(f("h", s=s))
    expect_true(f("i", s=s))
    expect_true(f("j", s=s))
    expect_true(f("k", s=s))
    expect_true(f("l", s=s))
    expect_true(f("m", s=s))
    expect_true(f("n", s=s))
    expect_true(f("o", s=s))
    expect_true(f("p", s=s))
    expect_true(f("q", s=s))
    expect_true(f("r", s=s))
    expect_true(f("s", s=s))
    expect_true(f("tt", s=s))
    expect_true(f("u", s=s))
    expect_true(f("v", s=s))
    expect_true(f("w", s=s))
    expect_true(f("x", s=s))
    expect_true(f("y", s=s))
    expect_true(f("z", s=s))
    expect_true(f("aa", s=s))

    s <- paste(c("linc=linCmtA(rx__PTR__,t,2,3,0,-1,1,cl,v,q,v2,q2,v3,0)",
                 testVars), collapse="\n")
    s <- rxS(s, promoteLinSens = TRUE)

    expect_true(f("b", s=s)) # depot
    expect_true(f("c", 0, s=s)) # central
    expect_true(f("d", 1, s=s)) # peripheral1
    expect_true(f("ee", 2, s=s)) # peripheral2
    expect_true(f("f", 3, s=s)) # rx__sens_central_BY_p1
    expect_true(f("g", 4, s=s)) # rx__sens_central_BY_v1
    expect_true(f("h", 5, s=s)) # rx__sens_central_BY_p2
    expect_true(f("i", 6, s=s)) # rx__sens_central_BY_p3
    expect_true(f("j", 7, s=s)) # rx__sens_central_BY_p4
    expect_true(f("k", 8, s=s)) # rx__sens_central_BY_p5
    expect_true(f("l", s=s))    # rx__sens_central_BY_ka
    expect_true(f("m", 9, s=s)) # rx__sens_peripheral1_BY_p1
    expect_true(f("n", 10, s=s))# rx__sens_peripheral1_BY_v1
    expect_true(f("o", 11, s=s))# rx__sens_peripheral1_BY_p2
    expect_true(f("p", 12, s=s))# rx__sens_peripheral1_BY_p3
    expect_true(f("q", 13, s=s))# rx__sens_peripheral1_BY_p4
    expect_true(f("r", 14, s=s))# rx__sens_peripheral1_BY_p5
    expect_true(f("s", s=s))    # rx__sens_peripheral1_BY_ka
    expect_true(f("tt", 15, s=s))#rx__sens_peripheral2_BY_p1
    expect_true(f("u", 16, s=s))# rx__sens_peripheral2_BY_v1
    expect_true(f("v", 17, s=s))# rx__sens_peripheral2_BY_p2
    expect_true(f("w", 18, s=s))# rx__sens_peripheral2_BY_p3
    expect_true(f("x", 19, s=s))# rx__sens_peripheral2_BY_p4
    expect_true(f("y", 20, s=s))# rx__sens_peripheral2_BY_p5
    expect_true(f("z", s=s))# rx__sens_peripheral2_BY_ka
    expect_true(f("aa", s=s))#rx__sens_depot_BY_ka

  })

  test_that("2 compartment oral states translated in symengine", {

    s <- paste(c("linc=linCmtA(rx__PTR__,t,2,2,1,-1,1,cl,v,q,v2,0,0,ka)",
                 testVars), collapse="\n")
    s <- rxS(s)

    expect_true(f("b", 0, s=s)) # depot
    expect_true(f("c", 1, s=s)) # central
    expect_true(f("d", 2, s=s)) # peripheral1
    expect_true(f("ee", s=s)) # peripheral2
    expect_true(f("f", s=s)) # rx__sens_central_BY_p1
    expect_true(f("g", s=s)) # rx__sens_central_BY_v1
    expect_true(f("h", s=s)) # rx__sens_central_BY_p2
    expect_true(f("i", s=s)) # rx__sens_central_BY_p3
    expect_true(f("j", s=s)) # rx__sens_central_BY_p4
    expect_true(f("k", s=s)) # rx__sens_central_BY_p5
    expect_true(f("l", s=s)) # rx__sens_central_BY_ka
    expect_true(f("m", s=s))# rx__sens_peripheral1_BY_p1
    expect_true(f("n", s=s))# rx__sens_peripheral1_BY_v1
    expect_true(f("o", s=s))# rx__sens_peripheral1_BY_p2
    expect_true(f("p", s=s))# rx__sens_peripheral1_BY_p3
    expect_true(f("q", s=s))# rx__sens_peripheral1_BY_p4
    expect_true(f("r", s=s))# rx__sens_peripheral1_BY_p5
    expect_true(f("s", s=s))# rx__sens_peripheral1_BY_ka
    expect_true(f("tt", s=s))#rx__sens_peripheral2_BY_p1
    expect_true(f("u", s=s))# rx__sens_peripheral2_BY_v1
    expect_true(f("v", s=s))# rx__sens_peripheral2_BY_p2
    expect_true(f("w", s=s))# rx__sens_peripheral2_BY_p3
    expect_true(f("x", s=s))# rx__sens_peripheral2_BY_p4
    expect_true(f("y", s=s))# rx__sens_peripheral2_BY_p5
    expect_true(f("z", s=s))# rx__sens_peripheral2_BY_ka
    expect_true(f("aa", s=s))#rx__sens_depot_BY_ka

    s <- paste(c("linc=linCmtA(rx__PTR__,t,2,2,1,-1,1,cl,v,q,v2,0,0,ka)",
                 testVars), collapse="\n")
    s <- rxS(s, promoteLinSens = TRUE)
    expect_true(f("b", 0, s=s)) # depot
    expect_true(f("c", 1, s=s)) # central
    expect_true(f("d", 2, s=s)) # peripheral1
    expect_true(f("ee", s=s)) # peripheral2
    expect_true(f("f", 3, s=s)) # rx__sens_central_BY_p1
    expect_true(f("g", 4, s=s)) # rx__sens_central_BY_v1
    expect_true(f("h", 5, s=s)) # rx__sens_central_BY_p2
    expect_true(f("i", 6, s=s)) # rx__sens_central_BY_p3
    expect_true(f("j", s=s))    # rx__sens_central_BY_p4
    expect_true(f("k", s=s))    # rx__sens_central_BY_p5
    expect_true(f("l", 7, s=s)) # rx__sens_central_BY_ka
    expect_true(f("m", 8, s=s)) # rx__sens_peripheral1_BY_p1
    expect_true(f("n", 9, s=s)) # rx__sens_peripheral1_BY_v1
    expect_true(f("o", 10, s=s))# rx__sens_peripheral1_BY_p2
    expect_true(f("p", 11, s=s))# rx__sens_peripheral1_BY_p3
    expect_true(f("q", s=s))    # rx__sens_peripheral1_BY_p4
    expect_true(f("r", s=s))    # rx__sens_peripheral1_BY_p5
    expect_true(f("s", 12, s=s))# rx__sens_peripheral1_BY_ka
    expect_true(f("tt", s=s))   # rx__sens_peripheral2_BY_p1
    expect_true(f("u", s=s))    # rx__sens_peripheral2_BY_v1
    expect_true(f("v", s=s))    # rx__sens_peripheral2_BY_p2
    expect_true(f("w", s=s))    # rx__sens_peripheral2_BY_p3
    expect_true(f("x", s=s))    # rx__sens_peripheral2_BY_p4
    expect_true(f("y", s=s))    # rx__sens_peripheral2_BY_p5
    expect_true(f("z", s=s))    # rx__sens_peripheral2_BY_ka
    expect_true(f("aa", 13, s=s))#rx__sens_depot_BY_ka
  })

  test_that("2 compartment states translated in symengine", {

    s <- paste(c("linc=linCmtA(rx__PTR__,t,2,2,0,-1,1,cl,v,q,v2,0,0,0)",
                 testVars), collapse="\n")
    s <- rxS(s)

    expect_true(f("b", s=s)) # depot
    expect_true(f("c", 0, s=s)) # central
    expect_true(f("d", 1, s=s)) # peripheral1
    expect_true(f("ee", s=s)) # peripheral2
    expect_true(f("f", s=s)) # rx__sens_central_BY_p1
    expect_true(f("g", s=s)) # rx__sens_central_BY_v1
    expect_true(f("h", s=s)) # rx__sens_central_BY_p2
    expect_true(f("i", s=s)) # rx__sens_central_BY_p3
    expect_true(f("j", s=s)) # rx__sens_central_BY_p4
    expect_true(f("k", s=s)) # rx__sens_central_BY_p5
    expect_true(f("l", s=s)) # rx__sens_central_BY_ka
    expect_true(f("m", s=s))# rx__sens_peripheral1_BY_p1
    expect_true(f("n", s=s))# rx__sens_peripheral1_BY_v1
    expect_true(f("o", s=s))# rx__sens_peripheral1_BY_p2
    expect_true(f("p", s=s))# rx__sens_peripheral1_BY_p3
    expect_true(f("q", s=s))# rx__sens_peripheral1_BY_p4
    expect_true(f("r", s=s))# rx__sens_peripheral1_BY_p5
    expect_true(f("s", s=s))# rx__sens_peripheral1_BY_ka
    expect_true(f("tt", s=s))#rx__sens_peripheral2_BY_p1
    expect_true(f("u", s=s))# rx__sens_peripheral2_BY_v1
    expect_true(f("v", s=s))# rx__sens_peripheral2_BY_p2
    expect_true(f("w", s=s))# rx__sens_peripheral2_BY_p3
    expect_true(f("x", s=s))# rx__sens_peripheral2_BY_p4
    expect_true(f("y", s=s))# rx__sens_peripheral2_BY_p5
    expect_true(f("z", s=s))# rx__sens_peripheral2_BY_ka
    expect_true(f("aa", s=s))#rx__sens_depot_BY_ka

    s <- paste(c("linc=linCmtA(rx__PTR__,t,2,2,0,-1,1,cl,v,q,v2,0,0,ka)",
                 testVars), collapse="\n")
    s <- rxS(s, promoteLinSens = TRUE)

    expect_true(f("b", s=s)) # depot
    expect_true(f("c", 0, s=s)) # central
    expect_true(f("d", 1, s=s)) # peripheral1
    expect_true(f("ee", s=s)) # peripheral2
    expect_true(f("f", 2, s=s)) # rx__sens_central_BY_p1
    expect_true(f("g", 3, s=s)) # rx__sens_central_BY_v1
    expect_true(f("h", 4, s=s)) # rx__sens_central_BY_p2
    expect_true(f("i", 5, s=s)) # rx__sens_central_BY_p3
    expect_true(f("j", s=s))    # rx__sens_central_BY_p4
    expect_true(f("k", s=s))    # rx__sens_central_BY_p5
    expect_true(f("l", s=s))    # rx__sens_central_BY_ka
    expect_true(f("m", 6, s=s)) # rx__sens_peripheral1_BY_p1
    expect_true(f("n", 7, s=s)) # rx__sens_peripheral1_BY_v1
    expect_true(f("o", 8, s=s)) # rx__sens_peripheral1_BY_p2
    expect_true(f("p", 9, s=s))# rx__sens_peripheral1_BY_p3
    expect_true(f("q", s=s))    # rx__sens_peripheral1_BY_p4
    expect_true(f("r", s=s))    # rx__sens_peripheral1_BY_p5
    expect_true(f("s", s=s))    # rx__sens_peripheral1_BY_ka
    expect_true(f("tt", s=s))   # rx__sens_peripheral2_BY_p1
    expect_true(f("u", s=s))    # rx__sens_peripheral2_BY_v1
    expect_true(f("v", s=s))    # rx__sens_peripheral2_BY_p2
    expect_true(f("w", s=s))    # rx__sens_peripheral2_BY_p3
    expect_true(f("x", s=s))    # rx__sens_peripheral2_BY_p4
    expect_true(f("y", s=s))    # rx__sens_peripheral2_BY_p5
    expect_true(f("z", s=s))    # rx__sens_peripheral2_BY_ka
    expect_true(f("aa", 10, s=s))#rx__sens_depot_BY_ka

  })

  test_that("1 compartment oral states translated in symengine", {
    s <- paste(c("linc=linCmtA(rx__PTR__,t,2,1,1,-1,1,cl,v,0,0,0,0,ka)",
                 testVars), collapse="\n")
    s <- rxS(s)

    expect_true(f("b", 0, s=s)) # depot
    expect_true(f("c", 1, s=s)) # central
    expect_true(f("d", s=s))  # peripheral1
    expect_true(f("ee", s=s)) # peripheral2
    expect_true(f("f", s=s)) # rx__sens_central_BY_p1
    expect_true(f("g", s=s)) # rx__sens_central_BY_v1
    expect_true(f("h", s=s)) # rx__sens_central_BY_p2
    expect_true(f("i", s=s)) # rx__sens_central_BY_p3
    expect_true(f("j", s=s)) # rx__sens_central_BY_p4
    expect_true(f("k", s=s)) # rx__sens_central_BY_p5
    expect_true(f("l", s=s)) # rx__sens_central_BY_ka
    expect_true(f("m", s=s))# rx__sens_peripheral1_BY_p1
    expect_true(f("n", s=s))# rx__sens_peripheral1_BY_v1
    expect_true(f("o", s=s))# rx__sens_peripheral1_BY_p2
    expect_true(f("p", s=s))# rx__sens_peripheral1_BY_p3
    expect_true(f("q", s=s))# rx__sens_peripheral1_BY_p4
    expect_true(f("r", s=s))# rx__sens_peripheral1_BY_p5
    expect_true(f("s", s=s))# rx__sens_peripheral1_BY_ka
    expect_true(f("tt", s=s))#rx__sens_peripheral2_BY_p1
    expect_true(f("u", s=s))# rx__sens_peripheral2_BY_v1
    expect_true(f("v", s=s))# rx__sens_peripheral2_BY_p2
    expect_true(f("w", s=s))# rx__sens_peripheral2_BY_p3
    expect_true(f("x", s=s))# rx__sens_peripheral2_BY_p4
    expect_true(f("y", s=s))# rx__sens_peripheral2_BY_p5
    expect_true(f("z", s=s))# rx__sens_peripheral2_BY_ka
    expect_true(f("aa", s=s))#rx__sens_depot_BY_ka

    s <- paste(c("linc=linCmtA(rx__PTR__,t,2,1,1,-1,1,cl,v,0,0,0,0,ka)",
                 testVars), collapse="\n")
    s <- rxS(s, promoteLinSens = TRUE)

    expect_true(f("b", 0, s=s)) # depot
    expect_true(f("c", 1, s=s)) # central
    expect_true(f("d", s=s)) # peripheral1
    expect_true(f("ee", s=s)) # peripheral2
    expect_true(f("f", 2, s=s)) # rx__sens_central_BY_p1
    expect_true(f("g", 3, s=s)) # rx__sens_central_BY_v1
    expect_true(f("h", s=s))    # rx__sens_central_BY_p2
    expect_true(f("i", s=s))    # rx__sens_central_BY_p3
    expect_true(f("j", s=s))    # rx__sens_central_BY_p4
    expect_true(f("k", s=s))    # rx__sens_central_BY_p5
    expect_true(f("l", 4, s=s)) # rx__sens_central_BY_ka
    expect_true(f("m", s=s))    # rx__sens_peripheral1_BY_p1
    expect_true(f("n", s=s))    # rx__sens_peripheral1_BY_v1
    expect_true(f("o", s=s))    # rx__sens_peripheral1_BY_p2
    expect_true(f("p", s=s))    # rx__sens_peripheral1_BY_p3
    expect_true(f("q", s=s))    # rx__sens_peripheral1_BY_p4
    expect_true(f("r", s=s))    # rx__sens_peripheral1_BY_p5
    expect_true(f("s", s=s))    # rx__sens_peripheral1_BY_ka
    expect_true(f("tt", s=s))   # rx__sens_peripheral2_BY_p1
    expect_true(f("u", s=s))    # rx__sens_peripheral2_BY_v1
    expect_true(f("v", s=s))    # rx__sens_peripheral2_BY_p2
    expect_true(f("w", s=s))    # rx__sens_peripheral2_BY_p3
    expect_true(f("x", s=s))    # rx__sens_peripheral2_BY_p4
    expect_true(f("y", s=s))    # rx__sens_peripheral2_BY_p5
    expect_true(f("z", s=s))    # rx__sens_peripheral2_BY_ka
    expect_true(f("aa", 5, s=s))#rx__sens_depot_BY_ka
  })

  test_that("1 compartment states translated in symengine", {

    s <- paste(c("linc=linCmtA(rx__PTR__,t,2,1,0,-1,1,cl,v,0,0,0,0,0)",
                 testVars), collapse="\n")
    s <- rxS(s)

    expect_true(f("b", s=s)) # depot
    expect_true(f("c", 0, s=s)) # central
    expect_true(f("d", s=s))  # peripheral1
    expect_true(f("ee", s=s)) # peripheral2
    expect_true(f("f", s=s)) # rx__sens_central_BY_p1
    expect_true(f("g", s=s)) # rx__sens_central_BY_v1
    expect_true(f("h", s=s)) # rx__sens_central_BY_p2
    expect_true(f("i", s=s)) # rx__sens_central_BY_p3
    expect_true(f("j", s=s)) # rx__sens_central_BY_p4
    expect_true(f("k", s=s)) # rx__sens_central_BY_p5
    expect_true(f("l", s=s)) # rx__sens_central_BY_ka
    expect_true(f("m", s=s))# rx__sens_peripheral1_BY_p1
    expect_true(f("n", s=s))# rx__sens_peripheral1_BY_v1
    expect_true(f("o", s=s))# rx__sens_peripheral1_BY_p2
    expect_true(f("p", s=s))# rx__sens_peripheral1_BY_p3
    expect_true(f("q", s=s))# rx__sens_peripheral1_BY_p4
    expect_true(f("r", s=s))# rx__sens_peripheral1_BY_p5
    expect_true(f("s", s=s))# rx__sens_peripheral1_BY_ka
    expect_true(f("tt", s=s))#rx__sens_peripheral2_BY_p1
    expect_true(f("u", s=s))# rx__sens_peripheral2_BY_v1
    expect_true(f("v", s=s))# rx__sens_peripheral2_BY_p2
    expect_true(f("w", s=s))# rx__sens_peripheral2_BY_p3
    expect_true(f("x", s=s))# rx__sens_peripheral2_BY_p4
    expect_true(f("y", s=s))# rx__sens_peripheral2_BY_p5
    expect_true(f("z", s=s))# rx__sens_peripheral2_BY_ka
    expect_true(f("aa", s=s))#rx__sens_depot_BY_ka

    s <- paste(c("linc=linCmtA(rx__PTR__,t,2,1,0,-1,1,cl,v,0,0,0,0,0)",
                 testVars), collapse="\n")
    s <- rxS(s, promoteLinSens = TRUE)

    expect_true(f("b", s=s)) # depot
    expect_true(f("c", 0, s=s)) # central
    expect_true(f("d", s=s)) # peripheral1
    expect_true(f("ee", s=s)) # peripheral2
    expect_true(f("f", 1, s=s)) # rx__sens_central_BY_p1
    expect_true(f("g", 2, s=s)) # rx__sens_central_BY_v1
    expect_true(f("h", s=s))    # rx__sens_central_BY_p2
    expect_true(f("i", s=s))    # rx__sens_central_BY_p3
    expect_true(f("j", s=s))    # rx__sens_central_BY_p4
    expect_true(f("k", s=s))    # rx__sens_central_BY_p5
    expect_true(f("l", s=s))    # rx__sens_central_BY_ka
    expect_true(f("m", s=s))    # rx__sens_peripheral1_BY_p1
    expect_true(f("n", s=s))    # rx__sens_peripheral1_BY_v1
    expect_true(f("o", s=s))    # rx__sens_peripheral1_BY_p2
    expect_true(f("p", s=s))    # rx__sens_peripheral1_BY_p3
    expect_true(f("q", s=s))    # rx__sens_peripheral1_BY_p4
    expect_true(f("r", s=s))    # rx__sens_peripheral1_BY_p5
    expect_true(f("s", s=s))    # rx__sens_peripheral1_BY_ka
    expect_true(f("tt", s=s))   # rx__sens_peripheral2_BY_p1
    expect_true(f("u", s=s))    # rx__sens_peripheral2_BY_v1
    expect_true(f("v", s=s))    # rx__sens_peripheral2_BY_p2
    expect_true(f("w", s=s))    # rx__sens_peripheral2_BY_p3
    expect_true(f("x", s=s))    # rx__sens_peripheral2_BY_p4
    expect_true(f("y", s=s))    # rx__sens_peripheral2_BY_p5
    expect_true(f("z", s=s))    # rx__sens_peripheral2_BY_ka
    expect_true(f("aa", s=s))   #rx__sens_depot_BY_ka

  })

  f <- function(txt, var, w1, w2) {
    .txt <- paste0("Derivative(", txt, ",", var, ")")
    .txt <- str2lang(rxFromSE(.txt))
    if (is.null(w1)) {
      expect_equal(0, .txt)
    } else {
      expect_equal(eval(.txt[[7]]), w1)
      expect_equal(eval(.txt[[8]]), w2)
    }
  }

  test_that("test linCmtB 3 compartment oral derivatives", {

    # linCmt()
    f("linCmtB(rx__PTR__,t,2,3,1,-1,-1,1,cl,v,q,v2,q2,v3,ka)", "cl", -2, 0)
    f("linCmtB(rx__PTR__,t,2,3,1,-1,-1,1,cl,v,q,v2,q2,v3,ka)", "v",  -2, 1)
    f("linCmtB(rx__PTR__,t,2,3,1,-1,-1,1,cl,v,q,v2,q2,v3,ka)", "q",  -2, 2)
    f("linCmtB(rx__PTR__,t,2,3,1,-1,-1,1,cl,v,q,v2,q2,v3,ka)", "v2", -2, 3)
    f("linCmtB(rx__PTR__,t,2,3,1,-1,-1,1,cl,v,q,v2,q2,v3,ka)", "q2", -2, 4)
    f("linCmtB(rx__PTR__,t,2,3,1,-1,-1,1,cl,v,q,v2,q2,v3,ka)", "v3", -2, 5)
    f("linCmtB(rx__PTR__,t,2,3,1,-1,-1,1,cl,v,q,v2,q2,v3,ka)", "ka", -2, 6)

    # state values

    # depot:
    f("linCmtB(rx__PTR__,t,2,3,1,0,-2,1,cl,v,q,v2,q2,v3,ka)", "cl", 0, 0)
    f("linCmtB(rx__PTR__,t,2,3,1,0,-2,1,cl,v,q,v2,q2,v3,ka)", "v",  0, 1)
    f("linCmtB(rx__PTR__,t,2,3,1,0,-2,1,cl,v,q,v2,q2,v3,ka)", "q",  0, 2)
    f("linCmtB(rx__PTR__,t,2,3,1,0,-2,1,cl,v,q,v2,q2,v3,ka)", "v2", 0, 3)
    f("linCmtB(rx__PTR__,t,2,3,1,0,-2,1,cl,v,q,v2,q2,v3,ka)", "q2", 0, 4)
    f("linCmtB(rx__PTR__,t,2,3,1,0,-2,1,cl,v,q,v2,q2,v3,ka)", "v3", 0, 5)
    f("linCmtB(rx__PTR__,t,2,3,1,0,-2,1,cl,v,q,v2,q2,v3,ka)", "ka", 0, 6)

    # central:
    f("linCmtB(rx__PTR__,t,2,3,1,1,-2,1,cl,v,q,v2,q2,v3,ka)", "cl", 1, 0)
    f("linCmtB(rx__PTR__,t,2,3,1,1,-2,1,cl,v,q,v2,q2,v3,ka)", "v",  1, 1)
    f("linCmtB(rx__PTR__,t,2,3,1,1,-2,1,cl,v,q,v2,q2,v3,ka)", "q",  1, 2)
    f("linCmtB(rx__PTR__,t,2,3,1,1,-2,1,cl,v,q,v2,q2,v3,ka)", "v2", 1, 3)
    f("linCmtB(rx__PTR__,t,2,3,1,1,-2,1,cl,v,q,v2,q2,v3,ka)", "q2", 1, 4)
    f("linCmtB(rx__PTR__,t,2,3,1,1,-2,1,cl,v,q,v2,q2,v3,ka)", "v3", 1, 5)
    f("linCmtB(rx__PTR__,t,2,3,1,1,-2,1,cl,v,q,v2,q2,v3,ka)", "ka", 1, 6)

    # peripharal1
    f("linCmtB(rx__PTR__,t,2,3,1,2,-2,1,cl,v,q,v2,q2,v3,ka)", "cl", 2, 0)
    f("linCmtB(rx__PTR__,t,2,3,1,2,-2,1,cl,v,q,v2,q2,v3,ka)", "v",  2, 1)
    f("linCmtB(rx__PTR__,t,2,3,1,2,-2,1,cl,v,q,v2,q2,v3,ka)", "q",  2, 2)
    f("linCmtB(rx__PTR__,t,2,3,1,2,-2,1,cl,v,q,v2,q2,v3,ka)", "v2", 2, 3)
    f("linCmtB(rx__PTR__,t,2,3,1,2,-2,1,cl,v,q,v2,q2,v3,ka)", "q2", 2, 4)
    f("linCmtB(rx__PTR__,t,2,3,1,2,-2,1,cl,v,q,v2,q2,v3,ka)", "v3", 2, 5)
    f("linCmtB(rx__PTR__,t,2,3,1,2,-2,1,cl,v,q,v2,q2,v3,ka)", "ka", 2, 6)

    # peripharal2
    f("linCmtB(rx__PTR__,t,2,3,1,3,-2,1,cl,v,q,v2,q2,v3,ka)", "cl", 3, 0)
    f("linCmtB(rx__PTR__,t,2,3,1,3,-2,1,cl,v,q,v2,q2,v3,ka)", "v",  3, 1)
    f("linCmtB(rx__PTR__,t,2,3,1,3,-2,1,cl,v,q,v2,q2,v3,ka)", "q",  3, 2)
    f("linCmtB(rx__PTR__,t,2,3,1,3,-2,1,cl,v,q,v2,q2,v3,ka)", "v2", 3, 3)
    f("linCmtB(rx__PTR__,t,2,3,1,3,-2,1,cl,v,q,v2,q2,v3,ka)", "q2", 3, 4)
    f("linCmtB(rx__PTR__,t,2,3,1,3,-2,1,cl,v,q,v2,q2,v3,ka)", "v3", 3, 5)
    f("linCmtB(rx__PTR__,t,2,3,1,3,-2,1,cl,v,q,v2,q2,v3,ka)", "ka", 3, 6)

  })

  test_that("test linCmtB 3 compartment derivatives", {

    # linCmt()
    f("linCmtB(rx__PTR__,t,2,3,0,-1,-1,1,cl,v,q,v2,q2,v3,0)", "cl", -2, 0)
    f("linCmtB(rx__PTR__,t,2,3,0,-1,-1,1,cl,v,q,v2,q2,v3,0)", "v",  -2, 1)
    f("linCmtB(rx__PTR__,t,2,3,0,-1,-1,1,cl,v,q,v2,q2,v3,0)", "q",  -2, 2)
    f("linCmtB(rx__PTR__,t,2,3,0,-1,-1,1,cl,v,q,v2,q2,v3,0)", "v2", -2, 3)
    f("linCmtB(rx__PTR__,t,2,3,0,-1,-1,1,cl,v,q,v2,q2,v3,0)", "q2", -2, 4)
    f("linCmtB(rx__PTR__,t,2,3,0,-1,-1,1,cl,v,q,v2,q2,v3,0)", "v3", -2, 5)
    f("linCmtB(rx__PTR__,t,2,3,0,-1,-1,1,cl,v,q,v2,q2,v3,ka)", "ka", NULL)

    # state values

    # central:
    f("linCmtB(rx__PTR__,t,2,3,0,0,-2,1,cl,v,q,v2,q2,v3,0)", "cl", 0, 0)
    f("linCmtB(rx__PTR__,t,2,3,0,0,-2,1,cl,v,q,v2,q2,v3,0)", "v",  0, 1)
    f("linCmtB(rx__PTR__,t,2,3,0,0,-2,1,cl,v,q,v2,q2,v3,0)", "q",  0, 2)
    f("linCmtB(rx__PTR__,t,2,3,0,0,-2,1,cl,v,q,v2,q2,v3,0)", "v2", 0, 3)
    f("linCmtB(rx__PTR__,t,2,3,0,0,-2,1,cl,v,q,v2,q2,v3,0)", "q2", 0, 4)
    f("linCmtB(rx__PTR__,t,2,3,0,0,-2,1,cl,v,q,v2,q2,v3,0)", "v3", 0, 5)
    f("linCmtB(rx__PTR__,t,2,3,0,0,-2,1,cl,v,q,v2,q2,v3,ka)", "ka", NULL)

    # peripharal1:
    f("linCmtB(rx__PTR__,t,2,3,0,1,-2,1,cl,v,q,v2,q2,v3,ka)", "cl", 1, 0)
    f("linCmtB(rx__PTR__,t,2,3,0,1,-2,1,cl,v,q,v2,q2,v3,ka)", "v",  1, 1)
    f("linCmtB(rx__PTR__,t,2,3,0,1,-2,1,cl,v,q,v2,q2,v3,ka)", "q",  1, 2)
    f("linCmtB(rx__PTR__,t,2,3,0,1,-2,1,cl,v,q,v2,q2,v3,ka)", "v2", 1, 3)
    f("linCmtB(rx__PTR__,t,2,3,0,1,-2,1,cl,v,q,v2,q2,v3,ka)", "q2", 1, 4)
    f("linCmtB(rx__PTR__,t,2,3,0,1,-2,1,cl,v,q,v2,q2,v3,ka)", "v3", 1, 5)
    f("linCmtB(rx__PTR__,t,2,3,0,1,-2,1,cl,v,q,v2,q2,v3,ka)", "ka", NULL)

    # peripharal2
    f("linCmtB(rx__PTR__,t,2,3,0,2,-2,1,cl,v,q,v2,q2,v3,ka)", "cl", 2, 0)
    f("linCmtB(rx__PTR__,t,2,3,0,2,-2,1,cl,v,q,v2,q2,v3,ka)", "v",  2, 1)
    f("linCmtB(rx__PTR__,t,2,3,0,2,-2,1,cl,v,q,v2,q2,v3,ka)", "q",  2, 2)
    f("linCmtB(rx__PTR__,t,2,3,0,2,-2,1,cl,v,q,v2,q2,v3,ka)", "v2", 2, 3)
    f("linCmtB(rx__PTR__,t,2,3,0,2,-2,1,cl,v,q,v2,q2,v3,ka)", "q2", 2, 4)
    f("linCmtB(rx__PTR__,t,2,3,0,2,-2,1,cl,v,q,v2,q2,v3,ka)", "v3", 2, 5)
    f("linCmtB(rx__PTR__,t,2,3,0,2,-2,1,cl,v,q,v2,q2,v3,ka)", "ka", NULL)

  })


  test_that("test linCmtB 2 compartment oral derivatives", {

    # linCmt()
    f("linCmtB(rx__PTR__,t,2,2,1,-1,-1,1,cl,v,q,v2,q2,v3,ka)", "cl", -2, 0)
    f("linCmtB(rx__PTR__,t,2,2,1,-1,-1,1,cl,v,q,v2,q2,v3,ka)", "v",  -2, 1)
    f("linCmtB(rx__PTR__,t,2,2,1,-1,-1,1,cl,v,q,v2,q2,v3,ka)", "q",  -2, 2)
    f("linCmtB(rx__PTR__,t,2,2,1,-1,-1,1,cl,v,q,v2,q2,v3,ka)", "v2", -2, 3)
    f("linCmtB(rx__PTR__,t,2,2,1,-1,-1,1,cl,v,q,v2,q2,v3,ka)", "q2", NULL)
    f("linCmtB(rx__PTR__,t,2,2,1,-1,-1,1,cl,v,q,v2,q2,v3,ka)", "v3", NULL)
    f("linCmtB(rx__PTR__,t,2,2,1,-1,-1,1,cl,v,q,v2,q2,v3,ka)", "ka", -2, 4)

    # state values

    # depot:
    f("linCmtB(rx__PTR__,t,2,2,1,0,-2,1,cl,v,q,v2,q2,v3,ka)", "cl", 0, 0)
    f("linCmtB(rx__PTR__,t,2,2,1,0,-2,1,cl,v,q,v2,q2,v3,ka)", "v",  0, 1)
    f("linCmtB(rx__PTR__,t,2,2,1,0,-2,1,cl,v,q,v2,q2,v3,ka)", "q",  0, 2)
    f("linCmtB(rx__PTR__,t,2,2,1,0,-2,1,cl,v,q,v2,q2,v3,ka)", "v2", 0, 3)
    f("linCmtB(rx__PTR__,t,2,2,1,0,-2,1,cl,v,q,v2,q2,v3,ka)", "q2", NULL)
    f("linCmtB(rx__PTR__,t,2,2,1,0,-2,1,cl,v,q,v2,q2,v3,ka)", "v3", NULL)
    f("linCmtB(rx__PTR__,t,2,2,1,0,-2,1,cl,v,q,v2,q2,v3,ka)", "ka", 0, 4)

    # central:
    f("linCmtB(rx__PTR__,t,2,2,1,1,-2,1,cl,v,q,v2,q2,v3,ka)", "cl", 1, 0)
    f("linCmtB(rx__PTR__,t,2,2,1,1,-2,1,cl,v,q,v2,q2,v3,ka)", "v",  1, 1)
    f("linCmtB(rx__PTR__,t,2,2,1,1,-2,1,cl,v,q,v2,q2,v3,ka)", "q",  1, 2)
    f("linCmtB(rx__PTR__,t,2,2,1,1,-2,1,cl,v,q,v2,q2,v3,ka)", "v2", 1, 3)
    f("linCmtB(rx__PTR__,t,2,2,1,1,-2,1,cl,v,q,v2,q2,v3,ka)", "q2", NULL)
    f("linCmtB(rx__PTR__,t,2,2,1,1,-2,1,cl,v,q,v2,q2,v3,ka)", "v3", NULL)
    f("linCmtB(rx__PTR__,t,2,2,1,1,-2,1,cl,v,q,v2,q2,v3,ka)", "ka", 1, 4)

    # peripharal1
    f("linCmtB(rx__PTR__,t,2,2,1,2,-2,1,cl,v,q,v2,q2,v3,ka)", "cl", 2, 0)
    f("linCmtB(rx__PTR__,t,2,2,1,2,-2,1,cl,v,q,v2,q2,v3,ka)", "v",  2, 1)
    f("linCmtB(rx__PTR__,t,2,2,1,2,-2,1,cl,v,q,v2,q2,v3,ka)", "q",  2, 2)
    f("linCmtB(rx__PTR__,t,2,2,1,2,-2,1,cl,v,q,v2,q2,v3,ka)", "v2", 2, 3)
    f("linCmtB(rx__PTR__,t,2,2,1,2,-2,1,cl,v,q,v2,q2,v3,ka)", "q2", NULL)
    f("linCmtB(rx__PTR__,t,2,2,1,2,-2,1,cl,v,q,v2,q2,v3,ka)", "v3", NULL)
    f("linCmtB(rx__PTR__,t,2,2,1,2,-2,1,cl,v,q,v2,q2,v3,ka)", "ka", 2, 4)

    # peripharal2
    f("linCmtB(rx__PTR__,t,2,2,1,3,-2,1,cl,v,q,v2,q2,v3,ka)", "cl", NULL)
    f("linCmtB(rx__PTR__,t,2,2,1,3,-2,1,cl,v,q,v2,q2,v3,ka)", "v",  NULL)
    f("linCmtB(rx__PTR__,t,2,2,1,3,-2,1,cl,v,q,v2,q2,v3,ka)", "q",  NULL)
    f("linCmtB(rx__PTR__,t,2,2,1,3,-2,1,cl,v,q,v2,q2,v3,ka)", "v2", NULL)
    f("linCmtB(rx__PTR__,t,2,2,1,3,-2,1,cl,v,q,v2,q2,v3,ka)", "q2", NULL)
    f("linCmtB(rx__PTR__,t,2,2,1,3,-2,1,cl,v,q,v2,q2,v3,ka)", "v3", NULL)
    f("linCmtB(rx__PTR__,t,2,2,1,3,-2,1,cl,v,q,v2,q2,v3,ka)", "ka", NULL)

  })

  test_that("test linCmtB 2 compartment derivatives", {

    # linCmt()
    f("linCmtB(rx__PTR__,t,2,2,0,-1,-1,1,cl,v,q,v2,q2,v3,ka)", "cl", -2, 0)
    f("linCmtB(rx__PTR__,t,2,2,0,-1,-1,1,cl,v,q,v2,q2,v3,ka)", "v",  -2, 1)
    f("linCmtB(rx__PTR__,t,2,2,0,-1,-1,1,cl,v,q,v2,q2,v3,ka)", "q",  -2, 2)
    f("linCmtB(rx__PTR__,t,2,2,0,-1,-1,1,cl,v,q,v2,q2,v3,ka)", "v2", -2, 3)
    f("linCmtB(rx__PTR__,t,2,2,0,-1,-1,1,cl,v,q,v2,q2,v3,ka)", "q2", NULL)
    f("linCmtB(rx__PTR__,t,2,2,0,-1,-1,1,cl,v,q,v2,q2,v3,ka)", "v3", NULL)
    f("linCmtB(rx__PTR__,t,2,2,0,-1,-1,1,cl,v,q,v2,q2,v3,ka)", "ka", NULL)

    # state values

    # central:
    f("linCmtB(rx__PTR__,t,2,2,0,0,-2,1,cl,v,q,v2,q2,v3,ka)", "cl", 0, 0)
    f("linCmtB(rx__PTR__,t,2,2,0,0,-2,1,cl,v,q,v2,q2,v3,ka)", "v",  0, 1)
    f("linCmtB(rx__PTR__,t,2,2,0,0,-2,1,cl,v,q,v2,q2,v3,ka)", "q",  0, 2)
    f("linCmtB(rx__PTR__,t,2,2,0,0,-2,1,cl,v,q,v2,q2,v3,ka)", "v2", 0, 3)
    f("linCmtB(rx__PTR__,t,2,2,0,0,-2,1,cl,v,q,v2,q2,v3,ka)", "q2", NULL)
    f("linCmtB(rx__PTR__,t,2,2,0,0,-2,1,cl,v,q,v2,q2,v3,ka)", "v3", NULL)
    f("linCmtB(rx__PTR__,t,2,2,0,0,-2,1,cl,v,q,v2,q2,v3,ka)", "ka", NULL)

    # peripharal1
    f("linCmtB(rx__PTR__,t,2,2,0,1,-2,1,cl,v,q,v2,q2,v3,ka)", "cl", 1, 0)
    f("linCmtB(rx__PTR__,t,2,2,0,1,-2,1,cl,v,q,v2,q2,v3,ka)", "v",  1, 1)
    f("linCmtB(rx__PTR__,t,2,2,0,1,-2,1,cl,v,q,v2,q2,v3,ka)", "q",  1, 2)
    f("linCmtB(rx__PTR__,t,2,2,0,1,-2,1,cl,v,q,v2,q2,v3,ka)", "v2", 1, 3)
    f("linCmtB(rx__PTR__,t,2,2,0,1,-2,1,cl,v,q,v2,q2,v3,ka)", "q2", NULL)
    f("linCmtB(rx__PTR__,t,2,2,0,1,-2,1,cl,v,q,v2,q2,v3,ka)", "v3", NULL)
    f("linCmtB(rx__PTR__,t,2,2,0,1,-2,1,cl,v,q,v2,q2,v3,ka)", "ka", NULL)

    # peripharal2
    f("linCmtB(rx__PTR__,t,2,2,0,2,-2,1,cl,v,q,v2,q2,v3,ka)", "cl", NULL)
    f("linCmtB(rx__PTR__,t,2,2,0,2,-2,1,cl,v,q,v2,q2,v3,ka)", "v",  NULL)
    f("linCmtB(rx__PTR__,t,2,2,0,2,-2,1,cl,v,q,v2,q2,v3,ka)", "q",  NULL)
    f("linCmtB(rx__PTR__,t,2,2,0,2,-2,1,cl,v,q,v2,q2,v3,ka)", "v2", NULL)
    f("linCmtB(rx__PTR__,t,2,2,0,2,-2,1,cl,v,q,v2,q2,v3,ka)", "q2", NULL)
    f("linCmtB(rx__PTR__,t,2,2,0,2,-2,1,cl,v,q,v2,q2,v3,ka)", "v3", NULL)
    f("linCmtB(rx__PTR__,t,2,2,0,2,-2,1,cl,v,q,v2,q2,v3,ka)", "ka", NULL)

  })

  test_that("test linCmtB 1 compartment oral derivatives", {

    # linCmt()
    f("linCmtB(rx__PTR__,t,2,1,1,-1,-1,1,cl,v,q,v2,q2,v3,ka)", "cl", -2, 0)
    f("linCmtB(rx__PTR__,t,2,1,1,-1,-1,1,cl,v,q,v2,q2,v3,ka)", "v",  -2, 1)
    f("linCmtB(rx__PTR__,t,2,1,1,-1,-1,1,cl,v,q,v2,q2,v3,ka)", "q",  NULL)
    f("linCmtB(rx__PTR__,t,2,1,1,-1,-1,1,cl,v,q,v2,q2,v3,ka)", "v2", NULL)
    f("linCmtB(rx__PTR__,t,2,1,1,-1,-1,1,cl,v,q,v2,q2,v3,ka)", "q2", NULL)
    f("linCmtB(rx__PTR__,t,2,1,1,-1,-1,1,cl,v,q,v2,q2,v3,ka)", "v3", NULL)
    f("linCmtB(rx__PTR__,t,2,1,1,-1,-1,1,cl,v,q,v2,q2,v3,ka)", "ka", -2, 2)

    # state values

    # depot:
    f("linCmtB(rx__PTR__,t,2,1,1,0,-2,1,cl,v,q,v2,q2,v3,ka)", "cl", 0, 0)
    f("linCmtB(rx__PTR__,t,2,1,1,0,-2,1,cl,v,q,v2,q2,v3,ka)", "v",  0, 1)
    f("linCmtB(rx__PTR__,t,2,1,1,0,-2,1,cl,v,q,v2,q2,v3,ka)", "q",  NULL)
    f("linCmtB(rx__PTR__,t,2,1,1,0,-2,1,cl,v,q,v2,q2,v3,ka)", "v2", NULL)
    f("linCmtB(rx__PTR__,t,2,1,1,0,-2,1,cl,v,q,v2,q2,v3,ka)", "q2", NULL)
    f("linCmtB(rx__PTR__,t,2,1,1,0,-2,1,cl,v,q,v2,q2,v3,ka)", "v3", NULL)
    f("linCmtB(rx__PTR__,t,2,1,1,0,-2,1,cl,v,q,v2,q2,v3,ka)", "ka", 0, 2)

    # central:
    f("linCmtB(rx__PTR__,t,2,1,1,1,-2,1,cl,v,q,v2,q2,v3,ka)", "cl", 1, 0)
    f("linCmtB(rx__PTR__,t,2,1,1,1,-2,1,cl,v,q,v2,q2,v3,ka)", "v",  1, 1)
    f("linCmtB(rx__PTR__,t,2,1,1,1,-2,1,cl,v,q,v2,q2,v3,ka)", "q",  NULL)
    f("linCmtB(rx__PTR__,t,2,1,1,1,-2,1,cl,v,q,v2,q2,v3,ka)", "v2", NULL)
    f("linCmtB(rx__PTR__,t,2,1,1,1,-2,1,cl,v,q,v2,q2,v3,ka)", "q2", NULL)
    f("linCmtB(rx__PTR__,t,2,1,1,1,-2,1,cl,v,q,v2,q2,v3,ka)", "v3", NULL)
    f("linCmtB(rx__PTR__,t,2,1,1,1,-2,1,cl,v,q,v2,q2,v3,ka)", "ka", 1, 2)

    # peripharal1
    f("linCmtB(rx__PTR__,t,2,1,1,2,-2,1,cl,v,q,v2,q2,v3,ka)", "cl", NULL)
    f("linCmtB(rx__PTR__,t,2,1,1,2,-2,1,cl,v,q,v2,q2,v3,ka)", "v",  NULL)
    f("linCmtB(rx__PTR__,t,2,1,1,2,-2,1,cl,v,q,v2,q2,v3,ka)", "q",  NULL)
    f("linCmtB(rx__PTR__,t,2,1,1,2,-2,1,cl,v,q,v2,q2,v3,ka)", "v2", NULL)
    f("linCmtB(rx__PTR__,t,2,1,1,2,-2,1,cl,v,q,v2,q2,v3,ka)", "q2", NULL)
    f("linCmtB(rx__PTR__,t,2,1,1,2,-2,1,cl,v,q,v2,q2,v3,ka)", "v3", NULL)
    f("linCmtB(rx__PTR__,t,2,1,1,2,-2,1,cl,v,q,v2,q2,v3,ka)", "ka", NULL)

    # peripharal2
    f("linCmtB(rx__PTR__,t,2,1,1,3,-2,1,cl,v,q,v2,q2,v3,ka)", "cl", NULL)
    f("linCmtB(rx__PTR__,t,2,1,1,3,-2,1,cl,v,q,v2,q2,v3,ka)", "v",  NULL)
    f("linCmtB(rx__PTR__,t,2,1,1,3,-2,1,cl,v,q,v2,q2,v3,ka)", "q",  NULL)
    f("linCmtB(rx__PTR__,t,2,1,1,3,-2,1,cl,v,q,v2,q2,v3,ka)", "v2", NULL)
    f("linCmtB(rx__PTR__,t,2,1,1,3,-2,1,cl,v,q,v2,q2,v3,ka)", "q2", NULL)
    f("linCmtB(rx__PTR__,t,2,1,1,3,-2,1,cl,v,q,v2,q2,v3,ka)", "v3", NULL)
    f("linCmtB(rx__PTR__,t,2,1,1,3,-2,1,cl,v,q,v2,q2,v3,ka)", "ka", NULL)

  })

  test_that("test linCmtB 1 compartment derivatives", {

    # linCmt()
    f("linCmtB(rx__PTR__,t,2,1,0,-1,-1,1,cl,v,q,v2,q2,v3,ka)", "cl", -2, 0)
    f("linCmtB(rx__PTR__,t,2,1,0,-1,-1,1,cl,v,q,v2,q2,v3,ka)", "v",  -2, 1)
    f("linCmtB(rx__PTR__,t,2,1,0,-1,-1,1,cl,v,q,v2,q2,v3,ka)", "q",  NULL)
    f("linCmtB(rx__PTR__,t,2,1,0,-1,-1,1,cl,v,q,v2,q2,v3,ka)", "v2", NULL)
    f("linCmtB(rx__PTR__,t,2,1,0,-1,-1,1,cl,v,q,v2,q2,v3,ka)", "q2", NULL)
    f("linCmtB(rx__PTR__,t,2,1,0,-1,-1,1,cl,v,q,v2,q2,v3,ka)", "v3", NULL)
    f("linCmtB(rx__PTR__,t,2,1,0,-1,-1,1,cl,v,q,v2,q2,v3,ka)", "ka", NULL)

    # state values

    # central
    f("linCmtB(rx__PTR__,t,2,1,0,0,-2,1,cl,v,q,v2,q2,v3,ka)", "cl", 0, 0)
    f("linCmtB(rx__PTR__,t,2,1,0,0,-2,1,cl,v,q,v2,q2,v3,ka)", "v",  0, 1)
    f("linCmtB(rx__PTR__,t,2,1,0,0,-2,1,cl,v,q,v2,q2,v3,ka)", "q",  NULL)
    f("linCmtB(rx__PTR__,t,2,1,0,0,-2,1,cl,v,q,v2,q2,v3,ka)", "v2", NULL)
    f("linCmtB(rx__PTR__,t,2,1,0,0,-2,1,cl,v,q,v2,q2,v3,ka)", "q2", NULL)
    f("linCmtB(rx__PTR__,t,2,1,0,0,-2,1,cl,v,q,v2,q2,v3,ka)", "v3", NULL)
    f("linCmtB(rx__PTR__,t,2,1,0,0,-2,1,cl,v,q,v2,q2,v3,ka)", "ka", NULL)

    # peripharal1
    f("linCmtB(rx__PTR__,t,2,1,0,2,-2,1,cl,v,q,v2,q2,v3,ka)", "cl", NULL)
    f("linCmtB(rx__PTR__,t,2,1,0,2,-2,1,cl,v,q,v2,q2,v3,ka)", "v",  NULL)
    f("linCmtB(rx__PTR__,t,2,1,0,2,-2,1,cl,v,q,v2,q2,v3,ka)", "q",  NULL)
    f("linCmtB(rx__PTR__,t,2,1,0,2,-2,1,cl,v,q,v2,q2,v3,ka)", "v2", NULL)
    f("linCmtB(rx__PTR__,t,2,1,0,2,-2,1,cl,v,q,v2,q2,v3,ka)", "q2", NULL)
    f("linCmtB(rx__PTR__,t,2,1,0,2,-2,1,cl,v,q,v2,q2,v3,ka)", "v3", NULL)
    f("linCmtB(rx__PTR__,t,2,1,0,2,-2,1,cl,v,q,v2,q2,v3,ka)", "ka", NULL)

    # peripharal2
    f("linCmtB(rx__PTR__,t,2,1,0,3,-2,1,cl,v,q,v2,q2,v3,ka)", "cl", NULL)
    f("linCmtB(rx__PTR__,t,2,1,0,3,-2,1,cl,v,q,v2,q2,v3,ka)", "v",  NULL)
    f("linCmtB(rx__PTR__,t,2,1,0,3,-2,1,cl,v,q,v2,q2,v3,ka)", "q",  NULL)
    f("linCmtB(rx__PTR__,t,2,1,0,3,-2,1,cl,v,q,v2,q2,v3,ka)", "v2", NULL)
    f("linCmtB(rx__PTR__,t,2,1,0,3,-2,1,cl,v,q,v2,q2,v3,ka)", "q2", NULL)
    f("linCmtB(rx__PTR__,t,2,1,0,3,-2,1,cl,v,q,v2,q2,v3,ka)", "v3", NULL)
    f("linCmtB(rx__PTR__,t,2,1,0,3,-2,1,cl,v,q,v2,q2,v3,ka)", "ka", NULL)

  })


})
