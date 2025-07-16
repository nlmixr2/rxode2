#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#define _(String) (String)

#include <R.h>
#include <Rinternals.h>

void RSprintf(const char *format, ...);

/*
  This is a reentrant-friendly version of the LSODA library.

  An example is in main.c. The public interface is declareed in lsoda.h.

  I acquired the original source code from this web page:

  http://lh3lh3.users.sourceforge.net/solveode.shtml

  Unlike the original code, this version of lsoda is reentrant friendly.
  One can use it with threads and OpenMP as long as different instances
  of the solver uses different context.
    All global variables are either
    (i) moved to a minimal local scope if possile.
      (ii) merged into a context variable that is passed around for all
           subroutine calls.

  Other differences:
  (i) this code the input variables follow the C convention
        y, opt->atol, rtol, as well as parameters of context->function
        all indexes from 0.
    (ii) itol is eliminated. rtol and atol all has to be the same length of
        number of variables.
    (iii) number of variables == number of equations is hard coded in the program
    (iv) lsoda also returns istate, and the error message is written to context->error

    The original source code came with an MIT/X11 license. Hereby I release this
  code with MIT/X11 license too. Most of original notes are kept with the source
  code, when they are applicable.

     - Yu Feng
 */

/* Contact: Yu Feng <rainwoodman@gmail.com> */

/* The MIT License

   Copyright (c) 2011 McWilliam Cosmology Center, Carnegie Mellon University.

   Permission is hereby granted, free of charge, to any person obtaining
   a copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to
   permit persons to whom the Software is furnished to do so, subject to
   the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE.
*/


#include <stdio.h>
#include <math.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>
#include <stddef.h>

#include "blas.h"
#include "lsoda.h"
#include "common.h"
#include "lsoda_internal.h"


/***********
 * lsoda.c *
 ***********/

/*
  From tam@dragonfly.wri.com Wed Apr 24 01:35:52 1991
Return-Path: <tam>
Date: Wed, 24 Apr 91 03:35:24 CDT
From: tam@dragonfly.wri.com
To: whitbeck@wheeler.wrc.unr.edu
Subject: lsoda.c
Cc: augenbau@sparc0.brc.uconn.edu


I'm told by Steve Nichols at Georgia Tech that you are interested in
a stiff integrator.  Here's a translation of the fortran code LSODA.

Please note
that there is no comment.  The interface is the same as the FORTRAN
code and I believe the documentation in LSODA will suffice.
As usual, a free software comes with no guarantee.

Hon Wah Tam
Wolfram Research, Inc.
tam@wri.com
*/

/* Terminate lsoda due to fatal errors*/
#define hardfailure(fmt,...) \
{ \
	ERROR(fmt, __VA_ARGS__); \
	ctx->state = -3 ; \
	return ctx->state; \
}

#define hardfailure0(fmt,...)                    \
  {                                             \
    ERROR0(fmt);                    \
    ctx->state = -3 ;                           \
    return ctx->state;				\
  }


/* Terminate lsoda due to various error conditions. */
#define softfailure(code, fmt,...) \
{ \
	int i=0; \
	int neq = ctx->neq; \
 \
	ERROR(fmt, __VA_ARGS__); \
	for (i = 1; i <= neq; i++) \
	  y[i] = _rxC(yh)[1][i];	   \
	*t = _rxC(tn); \
	ctx->state = code; \
	return ctx->state; \
}


/*
   The following block handles all successful returns from lsoda.
   If itask != 1, y is loaded from _rxC(yh) and t is set accordingly.
   *Istate is set to 2, the illegal input counter is zeroed, and the
   optional outputs are loaded into the work arrays before returning.
*/

#define successreturn() \
{ \
	int i=0; \
	int neq = ctx->neq; \
 \
	for (i = 1; i <= neq; i++) \
		y[i] = _rxC(yh)[1][i]; \
	*t = _rxC(tn); \
	if (itask == 4 || itask == 5) \
		if (ihit) \
			*t = tcrit; \
	ctx->state = 2; \
	return ctx->state; \
}

#define intdyreturn() \
{ \
	int iflag = intdy(ctx, tout, 0, y);  \
	if (iflag != 0) { \
		ERROR("[lsoda] trouble from intdy, itask = %d, tout = %g\n", itask, tout); \
		for (i = 1; i <= neq; i++) \
			y[i] = _rxC(yh)[1][i]; \
		*t = _rxC(tn); \
	} \
	*t = tout; \
	ctx->state = 2; \
	return ctx->state; \
}

/* check the consistency and set up default options */
static int check_opt(struct lsoda_context_t * ctx, struct lsoda_opt_t * opt) {
	const int mxstp0 = 500; /* mxhnl0 = 10; */
	const int      mord[3] = {0, 12, 5};

	if (ctx->state == 0) ctx->state = 1;
	if (ctx->state == 1) {
		opt->h0 = 0.;
		opt->mxordn = mord[1];
		opt->mxords = mord[2];
	}

	if (ctx->neq <= 0) {
		ERROR("[lsoda] neq = %d is less than 1\n", ctx->neq);
		return 0;
	}
	const double * rtol = opt->rtol - 1;
	const double * atol = opt->atol - 1;

	/*
	   Check rtol and atol for legality.
	 */
	if (ctx->state == 1 || ctx->state == 3) {
		/* c convention starts from 0. converted fortran code expects 1 */
		int i=0;
		for (i = 1; i <= ctx->neq; i++) {
			double rtoli = rtol[i];
			double atoli = atol[i];
			if (rtoli < 0.) {
				ERROR("[lsoda] rtol = %g is less than 0.\n", rtoli);
			}
			if (atoli < 0.) {
				ERROR("[lsoda] atol = %g is less than 0.\n", atoli);
				return 0;
			}
		}		/* end for   */
	}			/* end if ( ctx->state == 1 || ctx->state == 3 )   */
	/* default itask is 1 */
	if (opt->itask == 0) opt->itask = 1;
	if (opt->itask < 1 || opt->itask > 5) {
	  RSprintf(_("[lsoda] illegal itask = %d\n"), opt->itask);
	  return 0;
	}

	if (opt->ixpr < 0 || opt->ixpr > 1) {
	  RSprintf(_("[lsoda] ixpr = %d is illegal\n"), opt->ixpr);
	  return 0;
	}
	if (opt->mxstep < 0) {
	  RSprintf(_("[lsoda] mxstep < 0\n"));
	  return 0;
	}
	if (opt->mxstep == 0) opt->mxstep = mxstp0;
	if (opt->mxhnil < 0) {
	  RSprintf(_("[lsoda] mxhnil < 0\n"));
	  return 0;
	}
	if (ctx->state == 1) {
		if (opt->mxordn < 0) {
		  RSprintf(_("[lsoda] mxordn = %d is less than 0\n"), opt->mxordn);
		  return 0;
		}
		if (opt->mxordn == 0) opt->mxordn = 100;
		opt->mxordn = min(opt->mxordn, mord[1]);
		if (opt->mxords < 0) {
		  RSprintf(_("[lsoda] mxords = %d is less than 0\n"), opt->mxords);
		  return 0;
		}
		if (opt->mxords == 0) opt->mxords = 100;
		opt->mxords = min(opt->mxords, mord[2]);
	}	/* end if ( ctx->state == 1 )  */
	if (opt->hmax < 0.) {
	  RSprintf(_("[lsoda] hmax < 0.\n"));
	  return 0;
	}
	opt->hmxi = 0.;
	if (opt->hmax > 0)
		opt->hmxi = 1. / opt->hmax;
	if (opt->hmin < 0.) {
	  RSprintf(_("[lsoda] hmin < 0.\n"));
	  return 0;
	}
	return 1;
}
/* allocate memory with one malloc request and ensure compatibility with the solver's way of
 * accessing memory.
 *
 * only elements starting from 1 are useful in the solver, the 0th elements are allocated anyways
 *
 * the reason only 1 malloc is called is because malloc with threading of such large
 * block size may be slow. I wanted to minimize the overhead of starting the solver.
 * */
static int alloc_mem(struct lsoda_context_t * ctx) {
	int nyh = ctx->neq;
	int lenyh = 1 + max(ctx->opt->mxordn, ctx->opt->mxords);
	long offset = 0;
	int i=0;
	long yhoff = offset;
	/* _rxC(yh) */
	offset += (1 + lenyh) * sizeof(double *);
	long yh0off = offset;
	for(i = 0; i <= lenyh; i++) {
		offset += (1 + nyh) * sizeof(double);
	}

	long wmoff = offset;
	long wm0off = offset;
	/* _rxC(wm) */
	offset += (1 + nyh) * sizeof(double *);
	for(i = 0; i <= nyh; i++) {
		offset += (1 + nyh) * sizeof(double);
	}

	/* _rxC(ewt) */
	long ewtoff =  offset;
	offset += (1 + nyh) * sizeof(double);

	/* _rxC(savf) */
	long savfoff = offset;
	offset += (1 + nyh) * sizeof(double);

	/* _rxC(acor) */
	long acoroff = offset;
	offset += (1 + nyh) * sizeof(double);

	/* _rxC(ipvt) */
	long ipvtoff = offset;
	offset += (1 + nyh) * sizeof(int);

	_rxC(memory) = malloc(offset);

	_rxC(yh) = (double **)((char *)_rxC(memory) + yhoff);
	_rxC(wm) =  (double **)((char *)_rxC(memory) + wmoff);
	_rxC(ewt) = (double *)((char *)_rxC(memory) + ewtoff);
	_rxC(savf) =(double *)((char *)_rxC(memory) + savfoff);
	_rxC(acor) =(double *)((char *)_rxC(memory) + acoroff);
	_rxC(ipvt) =(int *)((char *)_rxC(memory) + ipvtoff);

	for(i = 0; i <= lenyh; i++) {
		_rxC(yh)[i] = (double *)((char *)_rxC(memory) + yh0off + i * (1 + nyh) * sizeof(double));
	}
	for(i = 0; i <= nyh; i++) {
		_rxC(wm)[i] = (double *)((char *)_rxC(memory) + wm0off + i * (1 + nyh) * sizeof(double));
	}

	return _rxC(memory) != NULL;
}

/*
c-----------------------------------------------------------------------
c this is the march 30, 1987 version of
c lsoda.. livermore solver for ordinary differential equations, with
c         automatic method switching for stiff and nonstiff problems.
c
c this version is in double precision.
c
c lsoda solves the initial value problem for stiff or nonstiff
c systems of first order ode-s,
c     dy/dt = f(t,y) ,  or, in component form,
c     dy(i)/dt = f(i) = f(i,t,y(1),y(2),...,y(neq)) (i = 1,...,neq).
c
c this a variant version of the lsode package.
c it switches automatically between stiff and nonstiff methods.
c this means that the user does not have to determine whether the
c problem is stiff or not, and the solver will automatically choose the
c appropriate method.  it always starts with the nonstiff method.
c
c authors..
c                linda r. petzold  and  alan c. hindmarsh,
c                computing and mathematics research division, l-316
c                lawrence livermore national laboratory
c                livermore, ca 94550.
c
c references..
c 1.  alan c. hindmarsh,  odepack, a systematized collection of ode
c     solvers, in scientific computing, r. s. stepleman et al. (eds.),
c     north-holland, amsterdam, 1983, pp. 55-64.
c 2.  linda r. petzold, automatic selection of methods for solving
c     stiff and nonstiff systems of ordinary differential equations,
c     siam j. sci. stat. comput. 4 (1983), pp. 136-148.
c-----------------------------------------------------------------------
c summary of usage.
c
c communication between the user and the lsoda package, for normal
c situations, is summarized here.  this summary describes only a subset
c of the full set of options available.  see the full description for
c details, including alternative treatment of the jacobian matrix,
c optional inputs and outputs, nonstandard options, and
c instructions for special situations.  see also the example
c problem (with program and output) following this summary.
c
c a. first provide a subroutine of the form..
c               subroutine f (neq, t, y, ydot)
c               dimension y(neq), ydot(neq)
c which supplies the vector function f by loading ydot(i) with f(i).
c
c b. write a main program which calls subroutine lsoda once for
c each point at which answers are desired.  this should also provide
c for possible use of logical unit 6 for output of error messages
c by lsoda.  on the first call to lsoda, supply arguments as follows..
c f      = name of subroutine for right-hand side vector f.
c          this name must be declared external in calling program.
c neq    = number of first order ode-s.
c y      = array of initial values, of length neq.
c t      = the initial value of the independent variable.
c tout   = first point where output is desired (.ne. t).
c rtol   = relative tolerance parameter (array, one for each dim).
c atol   = absolute tolerance parameter (array, one for each dim).
c          the estimated local error in y(i) will be controlled so as
c          to be less than
c             _rxC(ewt)(i) = rtol(i*abs(y(i)) + atol(i)
c          thus the local error test passes if, in each component,
c          either the absolute error is less than atol (or atol(i)),
c          or the relative error is less than rtol.
c          use rtol = 0.0 for pure absolute error control, and
c          use atol = 0.0 (or atol(i) = 0.0) for pure relative error
c          control.  caution.. actual (global) errors may exceed these
c          local tolerances, so choose them conservatively.
c itask  = 1 for normal computation of output values of y at t = tout.
c istate = integer flag (input and output).  set istate = 1.
c iopt   = 0 to indicate no optional inputs used.
c rwork  = real work array of length at least..
c             22 + neq * max(16, neq + 9).
c          see also paragraph e below.
c lrw    = declared length of rwork (in user-s dimension).
c iwork  = integer work array of length at least  20 + neq.
c liw    = declared length of iwork (in user-s dimension).
c jac    = name of subroutine for jacobian matrix.
c          use a dummy name.  see also paragraph e below.
c jt     = jacobian type indicator.  set jt = 2.
c          see also paragraph e below.
c note that the main program must declare arrays y, rwork, iwork,
c and possibly atol.
c
c c. the output from the first call (or any call) is..
c      y = array of computed values of y(t) vector.
c      t = corresponding value of independent variable (normally tout).
c istate = 2  if lsoda was successful, negative otherwise.
c          -1 means excess work done on this call (perhaps wrong jt).
c          -2 means excess accuracy requested (tolerances too small).
c          -3 means illegal input detected (see printed message).
c          -4 means repeated error test failures (check all inputs).
c          -5 means repeated convergence failures (perhaps bad jacobian
c             supplied or wrong choice of jt or tolerances).
c          -6 means error weight became zero during problem. (solution
c             component i vanished, and atol or atol(i) = 0.)
c          -7 means work space insufficient to finish (see messages).
c
c d. to continue the integration after a successful return, simply
c reset tout and call lsoda again.  no other parameters need be reset.
c
c e. note.. if and when lsoda regards the problem as stiff, and
c switches methods accordingly, it must make use of the neq by neq
c jacobian matrix, j = df/dy.  for the sake of simplicity, the
c inputs to lsoda recommended in paragraph b above cause lsoda to
c treat j as a full matrix, and to approximate it internally by
c difference quotients.  alternatively, j can be treated as a band
c matrix (with great potential reduction in the size of the rwork
c array).  also, in either the full or banded case, the user can supply
c j in closed form, with a routine whose name is passed as the jac
c argument.  these alternatives are described in the paragraphs on
c rwork, jac, and jt in the full description of the call sequence below.
c
c-----------------------------------------------------------------------
*/

/*
 * prepares a lsoda_context_t with options. use lsoda_free on the context
 * after the context is no longer used.
 *
 * notice that a context can be reused to solve different problems,
 * in that case call lsoda_reset before starting to solve the new problem
 * with lsoda.
 *
 */
int lsoda_prepare(struct lsoda_context_t * ctx, struct lsoda_opt_t * opt) {
	ctx->common = calloc(1, sizeof(struct lsoda_common_t));
	ctx->opt = opt;

	/* Default options.   */

	/* Next process and check the optional inpus.   */
	if(!check_opt(ctx, opt)) {
		return 0;
	}

	return alloc_mem(ctx);
}

/*
 * To reuse a context for a different problem (not continuing
 * the current problem!), call lsoda_reset.
 *
 * All options are preserved, no new memory allocation is performed.
 * this function merely resets all internal variables to their default value.
 */
void lsoda_reset(struct lsoda_context_t * ctx) {

	int offset = offsetof(struct lsoda_common_t, memory) + sizeof(void*);
	memset(((char*) ctx->common) + offset, 0, sizeof(struct lsoda_common_t) - offset);
}

/**
 * destory a context, notice that the memory holding the context is not freed,
 * since it may be an auto variable on the caller's stack.
 */
void lsoda_free(struct lsoda_context_t * ctx) {
	free(ctx->common->memory);
	if(ctx->error) {
	  RSprintf(_("unhandled error message: %s\n"), ctx->error);
	  free(ctx->error);
	}
	free(ctx->common);
}

#define ewset(ycur)  \
{ \
	int i=0; \
 \
	for (i = 1; i <= neq; i++) \
		_rxC(ewt)[i] = rtol[i] * fabs((ycur)[i]) + atol[i]; \
 \
	for (i = 1; i <= neq; i++) { \
		_rxC(ewt)[i] = 1. / _rxC(ewt)[i]; \
	} \
}

		/* end ewset   */

/**
 * integrate from t towards tout on context ctx.
 */
int lsoda(struct lsoda_context_t * ctx, double *y, double *t, double tout) {

		int kflag;
		int jstart;

		struct lsoda_common_t * common = ctx->common;
		const struct lsoda_opt_t * opt = ctx->opt;
		/* C convention to Fortran convention:
         * in C y[] starts from 0, but the converted fortran code starts from 1 */
		y--;

		int             i=0, ihit=0;
		const int neq = ctx->neq;
		double          big, h0, hmx, rh, tcrit, tdist, tnext, tol,
						tolsf, tp, size, sum, w0;

		if(common == NULL) {
		  hardfailure("[lsoda] illegal common block did you call lsoda_prepare?%s\n","");
		}
		/*
		   Block a.
		   This code block is executed on every call.
		   It tests ctx->state and itask for legality and branches appropriately.
		   If ctx->state > 1 but the flag _rxC(init) shows that initialization has not
		   yet been done, an error return occurs.
		   If ctx->state = 1 and tout = t, return immediately.
		 */

		/*
		   Block b.
		   The next code block is executed for the initial call ( ctx->state = 1 ),
		   or for a continuation call with parameter changes ( ctx->state = 3 ).
		   It contains checking of all inputs and various initializations.

		   First check legality of the non-optional inputs neq, itol, iopt,
		   jt, ml, and mu.
		 */

		if (ctx->state == 1 || ctx->state == 3) {
			h0 = opt->h0;
			if(ctx->state == 1) {
				if ((tout - *t) * h0 < 0.) {
					hardfailure("[lsoda] tout = %g behind t = %g. integration direction is given by %g\n",
							tout, *t, h0);
				}
			}
		}			/* end if ( ctx->state == 1 || ctx->state == 3 )   */

		const int itask = opt->itask;

		/*
		   If ctx->state = 1, _rxC(meth) is initialized to 1.

		 */
		/*
		   If ctx->state = 3, set flag to signal parameter changes to stoda.
		 */
		if (ctx->state == 3) {
			jstart = -1;
		}
		/*
		   Block c.
		   The next block is for the initial call only ( ctx->state = 1 ).
		   It contains all remaining initializations, the initial call to f,
		   and the calculation of the initial step size.
		   The error weights in _rxC(ewt) are inverted after being loaded.
		 */
		const double * rtol = opt->rtol - 1;
		const double * atol = opt->atol - 1;
		if (ctx->state == 1) {
			_rxC(meth) = 1;
			_rxC(tn) = *t;
			_rxC(tsw) = *t;
			if (itask == 4 || itask == 5) {
				tcrit = opt->tcrit;
				if ((tcrit - tout) * (tout - *t) < 0.) {
					hardfailure("[lsoda] itask = 4 or 5 and tcrit behind tout%s\n","");
				}
				if (h0 != 0. && (*t + h0 - tcrit) * h0 > 0.)
					h0 = tcrit - *t;
			}
			jstart = 0;
			/* set the order to 1*/
			_rxC(nq) = 1;

			/*
			   Initial call to f.
			 */
			(*ctx->function) (*t, y + 1, _rxC(yh)[2] + 1, ctx->data);
			_rxC(nfe) = 1;
			/*
			   Load the initial value vector in _rxC(yh).
			 */
			for (i = 1; i <= neq; i++)
				_rxC(yh)[1][i] = y[i];

			/*
			   Load and invert the _rxC(ewt) array.
			 */
			ewset(y);

			for (i = 1; i <= neq; i++) {
				if(_rxC(ewt)[i] <= 0.) {
					hardfailure("[lsoda] ewt[%d] = %g <= 0.\n", i, _rxC(ewt)[i]);
				}
			}

			/*
			   The coding below computes the step size, h0, to be attempted on the
			   first step, unless the user has supplied a value for this.
			   First check that tout - *t differs significantly from zero.
			   A scalar tolerance quantity tol is computed, as max(rtol[i])
			   if this is positive, or max(atol[i]/fabs(y[i])) otherwise, adjusted
			   so as to be between 100*ETA and 0.001.
			   Then the computed value h0 is given by

			   h0^(-2) = 1. / ( tol * w0^2 ) + tol * ( norm(f) )^2

			   where   w0     = max( fabs(*t), fabs(tout) ),
			   f      = the initial value of the vector f(t,y), and
			   norm() = the weighted vector norm used throughout, given by
			   the vmnorm function routine, and weighted by the
			   tolerances initially loaded into the _rxC(ewt) array.

			   The sign of h0 is inferred from the initial values of tout and *t.
			   fabs(h0) is made < fabs(tout-*t) in any case.
			 */
			if (h0 == 0.) {
				tdist = fabs(tout - *t);
				w0 = fmax(fabs(*t), fabs(tout));
				if (tdist < 2. * ETA * w0) {
					hardfailure("[lsoda] tout too close to t to start integration%s\n","");
				}
				tol = 0.;;
				for (i = 1; i <= neq; i++)
					tol = fmax(tol, rtol[i]);
				if (tol <= 0.) {
					for (i = 1; i <= neq; i++) {
						double atoli = atol[i];
						double ayi = fabs(y[i]);
						if (ayi != 0.)
							tol = fmax(tol, atoli / ayi);
					}
				}
				tol = fmax(tol, 100. * ETA);
				tol = fmin(tol, 0.001);
				sum = vmnorm0(neq, _rxC(yh)[2], _rxC(ewt));
				sum = 1. / (tol * w0 * w0) + tol * sum * sum;
				h0 = 1. / sqrt(sum);
				h0 = fmin(h0, tdist);
				h0 = h0 * ((tout - *t >= 0.) ? 1. : -1.);
			}		/* end if ( h0 == 0. )   */
			/*
			   Adjust h0 if necessary to meet hmax bound.
			 */
			rh = fabs(h0) * opt->hmxi;
			if (rh > 1.)
				h0 /= rh;
			/*
			   Load _rxC(h) with h0 and scale _rxC(yh)[2] by h0.
			 */
			_rxC(h) = h0;
			for (i = 1; i <= neq; i++)
				_rxC(yh)[2][i] *= h0;
		}			/* if ( ctx->state == 1 )   */
		/*
		   Block d.
		   The next code block is for continuation calls only ( ctx->state = 2 or 3 )
		   and is to check stop conditions before taking a step.
		 */
		if (ctx->state == 2 || ctx->state == 3) {
			jstart = 1;
			_rxC(nslast) = _rxC(nst);
			switch (itask) {
				case 1:
					if ((_rxC(tn) - tout) * _rxC(h) >= 0.) {
						intdyreturn();
					}
					break;
				case 2:
					break;
				case 3:
					tp = _rxC(tn) - _rxC(hu) * (1. + 100. * ETA);
					if ((tp - tout) * _rxC(h) > 0.) {
						hardfailure("[lsoda] itask = %d and tout behind tcur - _rxC(hu)\n", itask);
					}
					if ((_rxC(tn) - tout) * _rxC(h) < 0.) break;
					successreturn();
				case 4:
					tcrit = opt->tcrit;
					if ((_rxC(tn) - tcrit) * _rxC(h) > 0.) {
						hardfailure("[lsoda] itask = 4 or 5 and tcrit behind tcur%s\n","");
					}
					if ((tcrit - tout) * _rxC(h) < 0.) {
						hardfailure("[lsoda] itask = 4 or 5 and tcrit behind tout%s\n","");
					}
					if ((_rxC(tn) - tout) * _rxC(h) >= 0.) {
						intdyreturn();
					}
				case 5:
					if (itask == 5) {
						tcrit = opt->tcrit;
						if ((_rxC(tn) - tcrit) * _rxC(h) > 0.) {
							hardfailure("[lsoda] itask = 4 or 5 and tcrit behind tcur%s\n","");
						}
					}
					hmx = fabs(_rxC(tn)) + fabs(_rxC(h));
					int ihit = fabs(_rxC(tn) - tcrit) <= (100. * ETA * hmx);
					if (ihit) {
						*t = tcrit;
						successreturn();
					}
					tnext = _rxC(tn) + _rxC(h) * (1. + 4. * ETA);
					if ((tnext - tcrit) * _rxC(h) <= 0.)
						break;
					_rxC(h) = (tcrit - _rxC(tn)) * (1. - 4. * ETA);
					if (ctx->state == 2)
						jstart = -2;
					break;
			}		/* end switch   */
		}			/* end if ( ctx->state == 2 || ctx->state == 3 )   */
		/*
		   Block e.
		   The next block is normally executed for all calls and contains
		   the call to the one-step core integrator stoda.

		   This is a looping point for the integration steps.

		   First check for too many steps being taken, update _rxC(ewt) ( if not at
		   start of problem).  Check for too much accuracy being requested, and
		   check for _rxC(h) below the roundoff level in *t.
		 */
		while (1) {
			if (ctx->state != 1 || _rxC(nst) != 0) {
				if ((_rxC(nst) - _rxC(nslast)) >= opt->mxstep) {
					softfailure(-1, "[lsoda] %d steps taken before reaching tout\n", opt->mxstep);
				}
				ewset(_rxC(yh)[1]);
				for (i = 1; i <= neq; i++) {
					if (_rxC(ewt)[i] <= 0.) {
						softfailure(-6, "[lsoda] ewt[%d] = %g <= 0.\n", i, _rxC(ewt)[i]);
					}
				}
			}
			tolsf = ETA * vmnorm0(neq, _rxC(yh)[1], _rxC(ewt));
			if (tolsf > 0.01) {
				tolsf = tolsf * 200.;
				if (_rxC(nst) == 0) {
					hardfailure("lsoda -- at start of problem, too much accuracy\n requested for precision of machine,\n suggested scaling factor = %g\n", tolsf);
				}
				softfailure(-2, "lsoda -- at t = %g, too much accuracy requested\n  for precision of machine, suggested\n scaling factor = %g\n", *t, tolsf);
			}
			if ((_rxC(tn) + _rxC(h)) == _rxC(tn)) {
				_rxC(nhnil)++;
				if (_rxC(nhnil) <= opt->mxhnil) {
				  RSprintf(_("lsoda -- warning..internal t = %g and _rxC(h) = %g are\n"), _rxC(tn), _rxC(h));
				  RSprintf(_("         such that in the machine, t + _rxC(h) = t on the next step\n"));
				  RSprintf(_("         solver will continue anyway.\n"));
					if (_rxC(nhnil) == opt->mxhnil) {
					  RSprintf(_("lsoda -- above warning has been issued %d times,\n"), _rxC(nhnil));
					  RSprintf(_("         it will not be issued again for this problem\n"));
					}
				}
			}
			/*
			   Call stoda
			 */
			kflag = stoda(ctx, y, jstart);
			/*
			   RSprintf("_rxC(meth)= %d,   order= %d,   _rxC(nfe)= %d,   _rxC(nje)= %d\n",
			   _rxC(meth), _rxC(nq), _rxC(nfe), _rxC(nje) );
			   RSprintf("t= %20.15e,   _rxC(h)= %20.15e,   _rxC(nst)=%d\n", _rxC(tn), _rxC(h), _rxC(nst) );
			   RSprintf("y= %20.15e,   %20.15e,   %20.15e\n\n\n",
			   _rxC(yh)[1][1], _rxC(yh)[1][2], _rxC(yh)[1][3] );
			 */

			if (kflag == 0) {
				/*
				   Block f.
				   The following block handles the case of a successful return from the
				   core integrator ( kflag = 0 ).
				   If a method switch was just made, record _rxC(tsw), reset maxord,
				   set jstart to -1 to signal stoda to complete the switch,
				   and do extra printing of data if ixpr = 1.
				   Then, in any case, check for stop conditions.
				 */
				jstart = 1;
				if (_rxC(meth) != _rxC(mused)) {
					_rxC(tsw) = _rxC(tn);
					jstart = -1;
					if (opt->ixpr) {
						if (_rxC(meth) == 2)
						  RSprintf(_("[lsoda] a switch to the stiff method has occurred "));
						if (_rxC(meth) == 1)
						  RSprintf(_("[lsoda] a switch to the nonstiff method has occurred"));
						RSprintf(_("at t = %g, tentative step size _rxC(h) = %g, step _rxC(nst) = %d\n"), _rxC(tn), _rxC(h), _rxC(nst));
					}
				}	/* end if ( _rxC(meth) != _rxC(mused) )   */
				/*
				   itask = 1.
				   If tout has been reached, interpolate.
				 */
				if (itask == 1) {
					if ((_rxC(tn) - tout) * _rxC(h) < 0.)
						continue;
					intdyreturn();
				}
				/*
				   itask = 2.
				 */
				if (itask == 2) {
					successreturn();
				}
				/*
				   itask = 3.
				   Jump to exit if tout was reached.
				 */
				if (itask == 3) {
					if ((_rxC(tn) - tout) * _rxC(h) >= 0.) {
						successreturn();
					}
					continue;
				}
				/*
				   itask = 4.
				   See if tout or tcrit was reached.  Adjust _rxC(h) if necessary.
				 */
				if (itask == 4) {
					tcrit = opt->tcrit;
					if ((_rxC(tn) - tout) * _rxC(h) >= 0.) {
						intdyreturn();
					} else {
						hmx = fabs(_rxC(tn)) + fabs(_rxC(h));
						int ihit = fabs(_rxC(tn) - tcrit) <= (100. * ETA * hmx);
						if (ihit) {
							successreturn();
						}
						tnext = _rxC(tn) + _rxC(h) * (1. + 4. * ETA);
						if ((tnext - tcrit) * _rxC(h) <= 0.)
							continue;
						_rxC(h) = (tcrit - _rxC(tn)) * (1. - 4. * ETA);
						jstart = -2;
						continue;
					}
				}	/* end if ( itask == 4 )   */
				/*
				   itask = 5.
				   See if tcrit was reached and jump to exit.
				 */
				if (itask == 5) {
					tcrit = opt->tcrit;
					hmx = fabs(_rxC(tn)) + fabs(_rxC(h));
					int ihit = fabs(_rxC(tn) - tcrit) <= (100. * ETA * hmx);
					successreturn();
				}
			}		/* end if ( kflag == 0 )   */
			/*
			   kflag = -1, error test failed repeatedly or with fabs(_rxC(h)) = hmin.
			   kflag = -2, convergence failed repeatedly or with fabs(_rxC(h)) = hmin.
			 */
			if (kflag == -1 || kflag == -2) {
				big = 0.;
				_rxC(imxer) = 1;
				for (i = 1; i <= neq; i++) {
					size = fabs(_rxC(acor)[i]) * _rxC(ewt)[i];
					if (big < size) {
						big = size;
						_rxC(imxer) = i;
					}
				}
				if (kflag == -1) {
					softfailure(-4, "lsoda -- at t = %g and step size _rxC(h) = %g, the\n error test failed repeatedly or\n with fabs(_rxC(h)) = hmin\n", _rxC(tn), _rxC(h));
				}
				if (kflag == -2) {
					softfailure(-5, "lsoda -- at t = %g and step size _rxC(h) = %g, the\n corrector convergence failed repeatedly or\n with fabs(_rxC(h)) = hmin\n" , _rxC(tn), _rxC(h));
				}
			}		/* end if ( kflag == -1 || kflag == -2 )   */
		}			/* end while   */

	}				/* end lsoda   */

///////////////////////////////////
struct lsoda_context_t * lsoda_create_ctx(void)
{
	struct lsoda_context_t * mem = malloc(sizeof(struct lsoda_context_t));
	return mem;
}

struct lsoda_opt_t * lsoda_create_opt(void)
{
	struct lsoda_opt_t * mem = malloc(sizeof(struct lsoda_opt_t));
	return mem;
}

void lsoda_free_opt(struct lsoda_opt_t * opt)
{
	free(opt->atol);
	free(opt->rtol);
	free(opt);
}
