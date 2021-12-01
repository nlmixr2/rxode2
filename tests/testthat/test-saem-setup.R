rxode2Test(
  {
    context("SAEM setup")

    ## wbc
    rx1 <- "    CIRC0 = exp(log_CIRC0)\n    MTT = exp(log_MTT)\n    SLOPU = exp(log_SLOPU)\n    GAMMA = exp(log_GAMMA)\n    CL = CLI\n    V1 = V1I\n    V2 = V2I\n    Q = 204\n    CONC = A_centr/V1\n    NN = 3\n    KTR = (NN + 1)/MTT\n    EDRUG = 1 - SLOPU * CONC\n    FDBK = (CIRC0/A_circ)^GAMMA\n    CIRC = A_circ\n    A_prol(0) = CIRC0\n    A_tr1(0) = CIRC0\n    A_tr2(0) = CIRC0\n    A_tr3(0) = CIRC0\n    A_circ(0) = CIRC0\n    d/dt(A_centr) = A_periph * Q/V2 - A_centr * (CL/V1 + Q/V1)\n    d/dt(A_periph) = A_centr * Q/V1 - A_periph * Q/V2\n    d/dt(A_prol) = KTR * A_prol * EDRUG * FDBK - KTR * A_prol\n    d/dt(A_tr1) = KTR * A_prol - KTR * A_tr1\n    d/dt(A_tr2) = KTR * A_tr1 - KTR * A_tr2\n    d/dt(A_tr3) = KTR * A_tr2 - KTR * A_tr3\n    d/dt(A_circ) = KTR * A_tr3 - KTR * A_circ\n    nlmixr_pred <- CIRC\ncmt(CIRC);\n"

    rx1 <- rxode2(rx1)

    expect_error(rxGenSaem(rx1, function() {
      return(nlmixr_pred)
    }, NULL,
    optExpression = FALSE,
    loadSymengine = FALSE
    ), NA)

    expect_error(rxGenSaem(rx1, function() {
      return(nlmixr_pred)
    }, NULL,
    optExpression = TRUE,
    loadSymengine = FALSE
    ), NA)

    expect_error(rxGenSaem(rx1, function() {
      return(nlmixr_pred)
    }, NULL,
    optExpression = FALSE,
    loadSymengine = TRUE
    ), NA)

    expect_error(rxGenSaem(rx1, function() {
      return(nlmixr_pred)
    }, NULL,
    optExpression = TRUE,
    loadSymengine = TRUE
    ), NA)

    rx1 <- "    CIRC0 = exp(log_CIRC0)\n    MTT = exp(log_MTT)\n    SLOPU = exp(log_SLOPU)\n    GAMMA = exp(log_GAMMA)\n    CL = CLI\n    V1 = V1I\n    V2 = V2I\n    Q = 204\n    CONC = A_centr/V1\n    NN = 3\n    KTR = (NN + 1)/MTT\n    EDRUG = 1 - SLOPU * CONC\n    FDBK = (CIRC0/A_circ)^GAMMA\n    CIRC = A_circ\n    A_prol(0) = CIRC0\n    A_tr1(0) = CIRC0\n    A_tr2(0) = CIRC0\n    A_tr3(0) = CIRC0\n    A_circ(0) = CIRC0\n    d/dt(A_centr) = A_periph * Q/V2 - A_centr * (CL/V1 + Q/V1)\n    d/dt(A_periph) = A_centr * Q/V1 - A_periph * Q/V2\n    d/dt(A_prol) = KTR * A_prol * EDRUG * FDBK - KTR * A_prol\n    d/dt(A_tr1) = KTR * A_prol - KTR * A_tr1\n    d/dt(A_tr2) = KTR * A_tr1 - KTR * A_tr2\n    d/dt(A_tr3) = KTR * A_tr2 - KTR * A_tr3\n    d/dt(A_circ) = KTR * A_tr3 - KTR * A_circ\n    nlmixr_m <- CIRC\ncmt(CIRC);\n"

    rx1 <- rxode2(rx1)


    expect_error(rxGenSaem(rx1, function() {
      return(nlmixr_m)
    }, NULL,
    optExpression = TRUE,
    loadSymengine = TRUE
    ), NA)

    expect_error(rxGenSaem(rx1, function() {
      return(nlmixr_m)
    }, NULL,
    optExpression = TRUE,
    loadSymengine = FALSE
    ), NA)

    ## gen_saem_user_fn(obj$rxode.pred, obj$saem.pars, obj$predSaem, inPars = inPars)
    rx <- "    CL = CLI\n    V1 = V1I\n    V2 = V2I\n    Q = 204\n    CONC = A_centr/V1\n    NN = 3\n    KTR = (NN + 1)/MTT\n    EDRUG = 1 - SLOPU * CONC\n    FDBK = (CIRC0/A_circ)^GAMMA\n    CIRC = A_circ\n    A_prol(0) = CIRC0\n    A_tr1(0) = CIRC0\n    A_tr2(0) = CIRC0\n    A_tr3(0) = CIRC0\n    A_circ(0) = CIRC0\n    d/dt(A_centr) = A_periph * Q/V2 - A_centr * (CL/V1 + Q/V1)\n    d/dt(A_periph) = A_centr * Q/V1 - A_periph * Q/V2\n    d/dt(A_prol) = KTR * A_prol * EDRUG * FDBK - KTR * A_prol\n    d/dt(A_tr1) = KTR * A_prol - KTR * A_tr1\n    d/dt(A_tr2) = KTR * A_tr1 - KTR * A_tr2\n    d/dt(A_tr3) = KTR * A_tr2 - KTR * A_tr3\n    d/dt(A_circ) = KTR * A_tr3 - KTR * A_circ;\ncmt(CIRC);\n\n    nlmixr_pred <- CIRC"

    rx <- rxode2(rx)

    pars <- function() {
      CIRC0 <- exp(log_CIRC0)
      MTT <- exp(log_MTT)
      SLOPU <- exp(log_SLOPU)
      GAMMA <- exp(log_GAMMA)
    }

    pred <- function() {
      CIRC
    }

    expect_error(rxGenSaem(rx, pred, pars,
      optExpression = TRUE,
      loadSymengine = TRUE
    ), NA)


    expect_error(rxGenSaem(rx, pred, pars,
      optExpression = FALSE,
      loadSymengine = TRUE
    ), NA)

    expect_error(rxGenSaem(rx, pred, pars,
      optExpression = TRUE,
      loadSymengine = FALSE
    ), NA)

    expect_error(rxGenSaem(rx, pred, pars,
      optExpression = FALSE,
      loadSymengine = FALSE
    ), NA)
  },
  silent = TRUE,
  test = "focei"
)
