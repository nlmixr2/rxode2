* The following changes were made:
*     - removed any printout information
*     - Made matvec take the arguments matvec( x, y, A, n)
*----------------------------------------------------------------------|
      subroutine DSPHIV( n, m, t, u, v, w, tol, anorm,
     .     wsp,lwsp, iwsp,liwsp, matvec, iflag, A, ideg, mxstep)

      implicit none
      integer n, m, lwsp, liwsp, iflag, iwsp(liwsp), ideg, mxstep
      double precision t, tol, anorm, u(n), v(n), w(n), wsp(lwsp)
      double precision A(n,n)
      external matvec

*-----Purpose----------------------------------------------------------|
*
*---  DSPHIV computes w = exp(t*A)v + t*phi(tA)u which is the solution 
*     of the nonhomogeneous linear ODE problem w' = Aw + u, w(0) = v.
*     phi(z) = (exp(z)-1)/z and A is a Symmetric matrix.
*
*     The method used is based on Krylov subspace projection
*     techniques and the matrix under consideration interacts only
*     via the external routine `matvec' performing the matrix-vector 
*     product (matrix-free method).
*
*-----Arguments--------------------------------------------------------|
*
*     n      : (input) order of the principal matrix A.
*                      
*     m      : (input) maximum size for the Krylov basis.
*                      
*     t      : (input) time at wich the solution is needed (can be < 0).
*   
*     u(n)   : (input) operand vector with respect to the phi function
*              (forcing term of the ODE problem).
*
*     v(n)   : (input) operand vector with respect to the exp function
*              (initial condition of the ODE problem).
*  
*     w(n)   : (output) computed approximation of exp(t*A)v + t*phi(tA)u 
* 
*     tol    : (input/output) the requested accuracy tolerance on w. 
*              If on input tol=0.0d0 or tol is too small (tol.le.eps)
*              the internal value sqrt(eps) is used, and tol is set to
*              sqrt(eps) on output (`eps' denotes the machine epsilon).
*              (`Happy breakdown' is assumed if h(j+1,j) .le. anorm*tol)
*
*     anorm  : (input) an approximation of some norm of A.
*
*   wsp(lwsp): (workspace) lwsp .ge. n*(m+1)+n+(m+3)^2+4*(m+3)^2+ideg+1
*                                   +---------+-------+---------------+
*              (actually, ideg=6)        V        H      wsp for PADE
*                   
* iwsp(liwsp): (workspace) liwsp .ge. m+3
*
*     matvec : external subroutine for matrix-vector multiplication.
*              synopsis: matvec( x, y, A, n)
*                        double precision x(*), y(*)
*              computes: y(1:n) <- A*x(1:n)
*                        where A is the principal matrix.
*
*     iflag  : (output) exit flag.
*              <0 - bad input arguments 
*               0 - no problem
*               1 - maximum number of steps reached without convergence
*               2 - requested tolerance was too high
*
*-----Accounts on the computation--------------------------------------|
*     Upon exit, an interested user may retrieve accounts on the 
*     computations. They are located in the workspace arrays wsp and 
*     iwsp as indicated below: 
*
*     location  mnemonic                 description
*     -----------------------------------------------------------------|
*     iwsp(1) = nmult, number of matrix-vector multiplications used
*     iwsp(2) = nexph, number of Hessenberg matrix exponential evaluated
*     iwsp(3) = nscale, number of repeated squaring involved in Pade
*     iwsp(4) = nstep, number of integration steps used up to completion 
*     iwsp(5) = nreject, number of rejected step-sizes
*     iwsp(6) = ibrkflag, set to 1 if `happy breakdown' and 0 otherwise
*     iwsp(7) = mbrkdwn, if `happy brkdown', basis-size when it occured
*     -----------------------------------------------------------------|
*     wsp(1)  = step_min, minimum step-size used during integration
*     wsp(2)  = step_max, maximum step-size used during integration
*     wsp(3)  = dummy
*     wsp(4)  = dummy
*     wsp(5)  = x_error, maximum among all local truncation errors
*     wsp(6)  = s_error, global sum of local truncation errors
*     wsp(7)  = tbrkdwn, if `happy breakdown', time when it occured
*     wsp(8)  = t_now, integration domain successfully covered
*
*----------------------------------------------------------------------|
*-----The following parameters may also be adjusted herein-------------|
*
      integer mxreject
      double precision delta, gamma
      parameter( mxreject = 0,
     .           delta    = 1.2d0,
     .           gamma    = 0.9d0 )

*     mxstep  : maximum allowable number of integration steps.
*               The value 0 means an infinite number of steps.
* 
*     mxreject: maximum allowable number of rejections at each step. 
*               The value 0 means an infinite number of rejections.
*
*     ideg    : the Pade approximation of type (ideg,ideg) is used as 
*               an approximation to exp(H).
*
*     delta   : local truncation error `safety factor'
*
*     gamma   : stepsize `shrinking factor'
*
*----------------------------------------------------------------------|
*     Roger B. Sidje (rbs@maths.uq.edu.au)
*     EXPOKIT: Software Package for Computing Matrix Exponentials.
*     ACM - Transactions On Mathematical Software, 24(1):130-156, 1998
*----------------------------------------------------------------------|
*
      integer i, j, k1, mh, mx, iv, ih, j1v, ns, ifree, lfree, iexph,
     .        ireject,ibrkflag,mbrkdwn, nmult, nreject, nexph, nscale,
     .        nstep, iphih
      double precision sgn, t_out, tbrkdwn, step_min,step_max, err_loc,
     .                 s_error, x_error, t_now, t_new, t_step, t_old,
     .                 xm, beta, break_tol, p1, p2, p3, eps, rndoff,
     .                 avnorm, hj1j, hjj, SQR1

      intrinsic AINT,ABS,DBLE,LOG10,MAX,MIN,NINT,SIGN,SQRT
      double precision DDOT, DNRM2

*---  check restrictions on input parameters ...
      iflag = 0
      if ( lwsp.lt.n*(m+3)+5*(m+3)**2+ideg+1 ) iflag = -1
      if ( iflag.ne.0 ) then
         call rexit('bad sizes (in input of DSPHIV; -1)')
      end if
      if ( liwsp.lt.m+3 ) iflag = -2
      if ( iflag.ne.0 ) then
         call rexit('bad sizes (in input of DSPHIV; -2)')
      end if
      if ( m.ge.n .or. m.le.0 ) iflag = -3
      if ( iflag.ne.0 ) then
         call rexit('bad sizes (in input of DSPHIV; -3)')
      end if
*
*---  initialisations ...
*
      k1 = 3
      mh = m + 3
      iv = 1 
      ih = iv + n*(m+1) + n
      ifree = ih + mh*mh
      lfree = lwsp - ifree + 1

      ibrkflag = 0
      mbrkdwn  = m
      nmult    = 0
      nreject  = 0
      nexph    = 0
      nscale   = 0

      t_out    = ABS( t )
      tbrkdwn  = 0.0d0
      step_min = t_out
      step_max = 0.0d0
      nstep    = 0
      s_error  = 0.0d0
      x_error  = 0.0d0
      t_now    = 0.0d0
      t_new    = 0.0d0

      p1 = 4.0d0/3.0d0
 1    p2 = p1 - 1.0d0
      p3 = p2 + p2 + p2
      eps = ABS( p3-1.0d0 )
      if ( eps.eq.0.0d0 ) go to 1
      if ( tol.le.eps ) tol = SQRT( eps )
      rndoff = eps*anorm
 
      break_tol = 1.0d-7
*>>>  break_tol = tol
*>>>  break_tol = anorm*tol

*
*---  step-by-step integration ...
*
      sgn = SIGN( 1.0d0,t )
      SQR1 = SQRT( 0.1d0 )
      call DCOPY( n, v,1, w,1 )

 100  if ( t_now.ge.t_out ) goto 500

      nmult =  nmult + 1
      call matvec( w, wsp(iv) , A, n)
      call DAXPY( n, 1.0d0, u,1, wsp(iv),1 )
      beta = DNRM2( n, wsp(iv),1 )
      if ( beta.eq.0.0d0 ) goto 500
      call DSCAL( n, 1.0d0/beta, wsp(iv),1 )
      do i = 1,mh*mh
         wsp(ih+i-1) = 0.0d0
      enddo

      if ( nstep.eq.0 ) then
*---     obtain the very first stepsize ...
         xm = 1.0d0/DBLE( m )
         p1 = tol*(((m+1)/2.72D0)**(m+1))*SQRT(2.0D0*3.14D0*(m+1))
         t_new = (1.0d0/anorm)*(p1/(4.0d0*beta*anorm))**xm
         p1 = 10.0d0**(NINT( LOG10( t_new )-SQR1 )-1)
         t_new = AINT( t_new/p1 + 0.55d0 ) * p1
      endif
      nstep = nstep + 1
      t_step = MIN( t_out-t_now, t_new )
*
*---  Lanczos loop ...
*
      j1v = iv + n
      do 200 j = 1,m
         nmult = nmult + 1
         call matvec( wsp(j1v-n), wsp(j1v), A, n)
         if ( j.gt.1 )
     .     call DAXPY(n,-wsp(ih+(j-1)*mh+j-2),wsp(j1v-2*n),1,wsp(j1v),1)
         hjj = DDOT( n, wsp(j1v-n),1, wsp(j1v),1 )
         call DAXPY( n, -hjj, wsp(j1v-n),1, wsp(j1v),1 )
         hj1j = DNRM2( n, wsp(j1v),1 )
         wsp(ih+(j-1)*(mh+1)) = hjj
*---     if `happy breakdown' go straightforward at the end ... 
         if ( hj1j.le.break_tol ) then
            k1 = 0
            ibrkflag = 1
            mbrkdwn = j
            tbrkdwn = t_now
            t_step = t_out-t_now
            goto 300
         endif
         wsp(ih+(j-1)*mh+j) = hj1j
         wsp(ih+j*mh+j-1) = hj1j
         call DSCAL( n, 1.0d0/hj1j, wsp(j1v),1 )
         j1v = j1v + n
 200  continue
      nmult = nmult + 1
      call matvec( wsp(j1v-n), wsp(j1v), A, n)
      avnorm = DNRM2( n, wsp(j1v),1 )
*
*---  set 1's for the 3-extended scheme ...
*
 300  continue
      wsp(ih+mh*mbrkdwn) = 1.0d0
      wsp(ih+m*mh+m-1)   = 0.0d0
      wsp(ih+(m-1)*mh+m) = 0.0d0
      do i = 1,k1-1
         wsp(ih+(m+i)*mh+m+i-1) = 1.0d0
      enddo
*
*---  loop while ireject<mxreject until the tolerance is reached ...
*
      ireject = 0
 401  continue
*
*---  compute w = beta*t_step*V*phi(t_step*H)*e1 + w
*
      nexph = nexph + 1
      mx = mbrkdwn + MAX( 1,k1 )

*---  irreducible rational Pade approximation ...
      call DGPADM( ideg, mx, sgn*t_step, wsp(ih),mh,
     .              wsp(ifree),lfree, iwsp, iexph, ns, iflag)
      iexph = ifree + iexph - 1
      iphih = iexph + mbrkdwn*mx
      nscale = nscale + ns
      wsp(iphih+mbrkdwn)   = hj1j*wsp(iphih+mx+mbrkdwn-1)
      wsp(iphih+mbrkdwn+1) = hj1j*wsp(iphih+2*mx+mbrkdwn-1)
 
* 
*---  error estimate ...
*
      if ( k1.eq.0 ) then
         err_loc = tol
      else
         p1 = ABS( wsp(iphih+m) )   * beta
         p2 = ABS( wsp(iphih+m+1) ) * beta * avnorm 
         if ( p1.gt.10.0d0*p2 ) then
            err_loc = p2
            xm = 1.0d0/DBLE( m+1 )
         elseif ( p1.gt.p2 ) then
            err_loc = (p1*p2)/(p1-p2)
            xm = 1.0d0/DBLE( m+1 )
         else
            err_loc = p1
            xm = 1.0d0/DBLE( m )
         endif
      endif
*
*---  reject the step-size if the error is not acceptable ...
*   
      if ( (k1.ne.0) .and. (err_loc.gt.delta*t_step*tol) .and. 
     .     (mxreject.eq.0 .or. ireject.lt.mxreject) ) then
         t_old = t_step
         t_step = gamma * t_step * (t_step*tol/err_loc)**xm
         p1 = 10.0d0**(NINT( LOG10( t_step )-SQR1 )-1)
         t_step = AINT( t_step/p1 + 0.55d0 ) * p1
         ireject = ireject + 1
         nreject = nreject + 1
         if ( mxreject.ne.0 .and. ireject.gt.mxreject ) then
            iflag = 2
            return
         endif
         goto 401
      endif
*
      mx = mbrkdwn + MAX( 0,k1-2 )
      call DGEMV( 'n', n,mx,beta,wsp(iv),n,wsp(iphih),1,1.0d0,w,1 )
*
*---  suggested value for the next stepsize ...
*
      t_new = gamma * t_step * (t_step*tol/err_loc)**xm
      p1 = 10.0d0**(NINT( LOG10( t_new )-SQR1 )-1)
      t_new = AINT( t_new/p1 + 0.55d0 ) * p1 

      err_loc = MAX( err_loc,rndoff )
*
*---  update the time covered ...
*
      t_now = t_now + t_step 
*
*---  display and keep some information ...
*
      step_min = MIN( step_min, t_step ) 
      step_max = MAX( step_max, t_step )
      s_error = s_error + err_loc
      x_error = MAX( x_error, err_loc )
 
      if ( mxstep.eq.0 .or. nstep.lt.mxstep ) goto 100
      iflag = 1
 
 500  continue

      iwsp(1) = nmult
      iwsp(2) = nexph
      iwsp(3) = nscale
      iwsp(4) = nstep
      iwsp(5) = nreject
      iwsp(6) = ibrkflag
      iwsp(7) = mbrkdwn

      wsp(1)  = step_min
      wsp(2)  = step_max
      wsp(3)  = 0.0d0
      wsp(4)  = 0.0d0
      wsp(5)  = x_error
      wsp(6)  = s_error
      wsp(7)  = tbrkdwn
      wsp(8)  = sgn*t_now
      END
*----------------------------------------------------------------------|
