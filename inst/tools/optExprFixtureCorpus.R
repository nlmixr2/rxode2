## Corpus for the rxOptExpr differential fixture.
##
## Sourced by inst/tools/genOptExprFixture.R.  Defines `.optModels`, a named
## list of rxode2 model TEXT (never objects -- the C path takes raw text, and
## text is what nlmixr2est hands rxOptExpr in every one of its call sites).
##
## Add here when new syntax gains a common-subexpression case; do not add here
## to make a failing diff go away.

.optModels <- list()

## ---------------------------------------------- the pinned test-opt-expr set
## These are the exact strings test-opt-expr.R pins output for, so the fixture
## carries the existing byte-exact contract too.
.optModels$foceiPred <- paste0(
  "rx_yj_~2;\nrx_lambda_~1;\n",
  "rx_pred_=10*exp(-THETA[1]*t*exp(ETA[1]));\n",
  "rx__sens_rx_pred__BY_ETA_1___=-10*THETA[1]*t*exp(ETA[1])*exp(-THETA[1]*t*exp(ETA[1]));\n",
  "rx_r_=100*Rx_pow_di(THETA[2],2)*exp(-2*THETA[1]*t);\ndvid(3,4)\n")

.optModels$infusion <-
  "A1=exp(-k10*(tau - tinf))*r1*(1.0 - exp(-k10*tinf))/(k10*(1.0 - exp(-tau*k10)))"

.optModels$linSens <- paste0(
  "A1=r1/ka\nA1ka=-r1/ka^2\nA1k20=0\nA1b1=0\nA1r1=ka^(-1)\n",
  "A2=r1/k20\nA2ka=0\nA2k20=-r1/k20^2\nA2b1=0\nA2r1=k20^(-1)")

## the simplification corners: unary minus, nested parentheses, ^(1/2)
.optModels$negHalf   <- "a=1+(-1/2)*b"
.optModels$negOne    <- "a=-1*exp(b)"
.optModels$nestParen <- "a=1+(((-1/2)))*b"
.optModels$halfPow   <- "a=1+(1/2)*b; c=d^(1/2); e=(1/2)*f^(1/2)"

## ------------------------------------------------- compartment-scoped heads
## d/dt(), name(0), f(), F(), alag(), rate(), dur() -- everything ..rxOptLhs()
## accepts, since the C grammar has to accept exactly the same set.
.optModels$cmtHeads <- paste(c(
  "d/dt(depot)=-exp(THETA[1]+ETA[1])*depot",
  "d/dt(center)=exp(THETA[1]+ETA[1])*depot-exp(THETA[2])*center",
  "depot(0)=exp(THETA[4])",
  "center(0)=exp(THETA[4])*2",
  "f(depot)=exp(THETA[5])",
  "F(center)=exp(THETA[5])",
  "alag(depot)=exp(THETA[6])",
  "lag(center)=exp(THETA[6])",
  "rate(depot)=exp(THETA[7])",
  "dur(depot)=exp(THETA[7])*2",
  "cp=center*exp(THETA[3])"), collapse = "\n")

## ------------------------------------------------------- operator coverage
## `%%` is never folded (rxode2 truncates where R floors) and its operands are
## always parenthesized; `^n` for integerish n >= 2 is expanded before counting.
.optModels$operators <- paste(c(
  "a=b%%c",
  "d=(b+1)%%(c+1)",
  "e=(b+c)^2",
  "g=(b+c)^3",
  "h=(b+c)^2.5",
  "i2=(b+c)^(-2)",
  "j=0*(b+c)",
  "k=1*(b+c)",
  "l=(b+c)+0",
  "m=-1*(b+c)",
  "n=2*3",
  "o=b/1",
  "p=1/1"), collapse = "\n")

## ------------------------------------------------------------- big models
## Above chunkLines the default path chunks; the fixture always asks for
## chunkLines = 0L, which is the reference the C path must reproduce.
.optModels$chunk60 <- local({
  .s <- vapply(seq_len(60L), function(i) {
    sprintf("v%d=exp(THETA[1]+ETA[1])*exp(THETA[2]*%d)+sin(THETA[3]*t)", i, i)
  }, character(1))
  paste(c("d/dt(depot)=-exp(THETA[1]+ETA[1])*depot",
          "d/dt(center)=exp(THETA[1]+ETA[1])*depot-exp(THETA[2])*center",
          "depot(0)=exp(THETA[4])",
          "f(depot)=exp(THETA[5])",
          "alag(depot)=exp(THETA[6])",
          .s,
          "rx_pred_=center*exp(THETA[3])"), collapse = "\n")
})

## --------------------------------------------------------- delay()/past()
## past() durations are optimized as ordinary expressions and must pick up the
## same rx_expr_ temporary the matching delay() call does (#1192).
.optModels$pastConst <- paste(c(
  "d/dt(G)=-kg*G+kg*delay(G,1.5)",
  "past(G,1.5)=1",
  "cp=G*exp(kg)"), collapse = "\n")

.optModels$pastExpr <- paste(c(
  "d/dt(G)=-kg*G+kg*delay(G,exp(lT))",
  "past(G,exp(lT))=1",
  "cp=G*exp(lT)"), collapse = "\n")

## ------------------------------------------------------------- conditionals
## ..rxOpt() has no if/else branch and renders these as garbage, so the C path
## must DECLINE rather than reproduce it.  Kept in the corpus so the fixture
## records what the reference actually does today.
.optModels$conditional <- paste(c(
  "if (a > 1) {",
  "  b=exp(c)*exp(d)",
  "} else {",
  "  b=exp(c)+exp(d)",
  "}",
  "cp=b*exp(c)"), collapse = "\n")

## ---------------------------------------------------- ui models to normalize
## rxNorm()ed real models -- the shape rxOptExpr sees from rxode2() itself.
.optUiModels <- list(
  oneCmtKa = function() {
    rxode2::rxode2({
      ke <- cl / v
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - ke * center
      cp <- center / v
    })
  },
  twoCmtKa = function() {
    rxode2::rxode2({
      k12 <- q / v1
      k21 <- q / v2
      ke <- cl / v1
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - ke * center - k12 * center + k21 * peri
      d/dt(peri) <- k12 * center - k21 * peri
      cp <- center / v1
    })
  }
)
