# Second derivatives of REGISTERED functions, i.e. the multi-variable
# `Derivative(f(...), v1, v2)` that symengine emits during a 2nd-order
# sensitivity expansion.
#
# Before the converter was fixed, every one of these aborted with
# "'Derivative' conversion only takes one function and one argument": the
# multi-variable loop applied `.rxD` for the first variable and then searched
# the RESULT's operands for the next, which only works while each derivative is
# itself a bare registered call.  A derivative that comes back as a product or
# a sum -- which most of them are -- found nothing and threw.  The consequence
# was silent rather than loud further up: a model needing these had no analytic
# 2nd-order sensitivities at all, so the FOCEi-family analytic outer gradient
# fell back to finite differences without saying so.

test_that("a second derivative of a registered function converts", {
  # gammapInv/gammapDer are registered here and their derivatives are NOT bare
  # registered calls, which is exactly the case that used to throw
  for (.e in c("Derivative(gammapInv(a, x), x, x)",
               "Derivative(gammapDer(a, x), x, x)")) {
    .r <- rxode2::rxFromSE(.e)
    expect_true(is.character(.r) && nzchar(.r), info = .e)
    # it must be parsable R, not a fragment
    expect_silent(str2lang(.r))
  }
})

test_that("a MIXED second partial converts, tuple-form Subs included", {
  # A mixed partial of a multi-argument registered function makes symengine
  # emit simultaneous substitution, `Subs(Derivative(f(a, b), a, b), (a, b),
  # (e1, e2))`.  `(a, b)` is not an R expression, so this used to fail in
  # parse() with "unexpected ','" before the converter ever saw it.
  .r <- rxode2::rxFromSE("Derivative(gammapInv(a, x), a, x)")
  expect_true(is.character(.r) && nzchar(.r))
  expect_silent(str2lang(.r))
})

test_that("the xi renaming is idempotent", {
  # Text passes through the converter twice whenever a converted expression is
  # handed back to symengine and re-converted, which is precisely what a second
  # derivative does.  A non-idempotent rename produced `rxrx_xi_1`, which
  # leaked into the generated model as a free parameter ("the following
  # parameter(s) are required for solving: rxrx_xi_1").
  .r <- rxode2::rxFromSE("Derivative(gammapInv(a, x), a, x)")
  expect_false(grepl("rxrx_xi_", .r, fixed = TRUE))
})

test_that("digamma and trigamma survive the from-symengine direction", {
  # Both are emitted BY registered derivatives (.rxD$gammapDer and
  # .rxD$ibetaDer produce digamma), so they arrive from symengine.  Missing
  # from the arity table they took the user-function path and were rejected as
  # "requires 0 arguments (supplied 1)".
  # literal strings: rxFromSE() treats a CALL as an expression to convert, so
  # paste0(.f, "(x)") would be converted rather than evaluated
  .rd <- rxode2::rxFromSE("digamma(x)")
  expect_true(grepl("digamma", .rd, fixed = TRUE))
  expect_silent(str2lang(.rd))
  .rt <- rxode2::rxFromSE("trigamma(x)")
  expect_true(grepl("trigamma", .rt, fixed = TRUE))
  expect_silent(str2lang(.rt))
})
