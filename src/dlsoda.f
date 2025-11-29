*DECK DUMACH
      DOUBLE PRECISION FUNCTION DUMACH ()
C***BEGIN PROLOGUE  DUMACH
C***PURPOSE  Compute the unit roundoff of the machine.
C***CATEGORY  R1
C***TYPE      DOUBLE PRECISION (RUMACH-S, DUMACH-D)
C***KEYWORDS  MACHINE CONSTANTS
C***AUTHOR  Hindmarsh, Alan C., (LLNL)
C***DESCRIPTION
C *Usage:
C        DOUBLE PRECISION  A, DUMACH
C        A = DUMACH()
C
C *Function Return Values:
C     A : the unit roundoff of the machine.
C
C *Description:
C     The unit roundoff is defined as the smallest positive machine
C     number u such that  1.0 + u .ne. 1.0.  This is computed by DUMACH
C     in a machine-independent manner.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  DUMSUM
C***REVISION HISTORY  (YYYYMMDD)
C   19930216  DATE WRITTEN
C   19930818  Added SLATEC-format prologue.  (FNF)
C   20030707  Added DUMSUM to force normal storage of COMP.  (ACH)
C***END PROLOGUE  DUMACH
C
      DOUBLE PRECISION U, COMP
C***FIRST EXECUTABLE STATEMENT  DUMACH
      U = 1.0D0
 10   U = U*0.5D0
      CALL DUMSUM(1.0D0, U, COMP)
      IF (COMP .NE. 1.0D0) GO TO 10
      DUMACH = U*2.0D0
      RETURN
C----------------------- End of Function DUMACH ------------------------
      END
      SUBROUTINE DUMSUM(A,B,C)
C     Routine to force normal storing of A + B, for DUMACH.
      DOUBLE PRECISION A, B, C
      C = A + B
      RETURN
      END
*DECK DCFODE
      SUBROUTINE DCFODE (METH, ELCO, TESCO)
C***BEGIN PROLOGUE  DCFODE
C***SUBSIDIARY
C***PURPOSE  Set ODE integrator coefficients.
C***TYPE      DOUBLE PRECISION (SCFODE-S, DCFODE-D)
C***AUTHOR  Hindmarsh, Alan C., (LLNL)
C***DESCRIPTION
C
C  DCFODE is called by the integrator routine to set coefficients
C  needed there.  The coefficients for the current method, as
C  given by the value of METH, are set for all orders and saved.
C  The maximum order assumed here is 12 if METH = 1 and 5 if METH = 2.
C  (A smaller value of the maximum order is also allowed.)
C  DCFODE is called once at the beginning of the problem,
C  and is not called again unless and until METH is changed.
C
C  The ELCO array contains the basic method coefficients.
C  The coefficients el(i), 1 .le. i .le. nq+1, for the method of
C  order nq are stored in ELCO(i,nq).  They are given by a genetrating
C  polynomial, i.e.,
C      l(x) = el(1) + el(2)*x + ... + el(nq+1)*x**nq.
C  For the implicit Adams methods, l(x) is given by
C      dl/dx = (x+1)*(x+2)*...*(x+nq-1)/factorial(nq-1),    l(-1) = 0.
C  For the BDF methods, l(x) is given by
C      l(x) = (x+1)*(x+2)* ... *(x+nq)/K,
C  where         K = factorial(nq)*(1 + 1/2 + ... + 1/nq).
C
C  The TESCO array contains test constants used for the
C  local error test and the selection of step size and/or order.
C  At order nq, TESCO(k,nq) is used for the selection of step
C  size at order nq - 1 if k = 1, at order nq if k = 2, and at order
C  nq + 1 if k = 3.
C
C***SEE ALSO  DLSODE
C***ROUTINES CALLED  (NONE)
C***REVISION HISTORY  (YYMMDD)
C   791129  DATE WRITTEN
C   890501  Modified prologue to SLATEC/LDOC format.  (FNF)
C   890503  Minor cosmetic changes.  (FNF)
C   930809  Renamed to allow single/double precision versions. (ACH)
C***END PROLOGUE  DCFODE
C**End
      INTEGER METH
      INTEGER I, IB, NQ, NQM1, NQP1
      DOUBLE PRECISION ELCO, TESCO
      DOUBLE PRECISION AGAMQ, FNQ, FNQM1, PC, PINT, RAGQ,
     1   RQFAC, RQ1FAC, TSIGN, XPIN
      DIMENSION ELCO(13,12), TESCO(3,12)
      DIMENSION PC(12)
C
C***FIRST EXECUTABLE STATEMENT  DCFODE
C     GO TO (100, 200), METH
      IF (METH .EQ. 1) GO TO 100
      IF (METH .EQ. 2) GO TO 200
C
 100  ELCO(1,1) = 1.0D0
      ELCO(2,1) = 1.0D0
      TESCO(1,1) = 0.0D0
      TESCO(2,1) = 2.0D0
      TESCO(1,2) = 1.0D0
      TESCO(3,12) = 0.0D0
      PC(1) = 1.0D0
      RQFAC = 1.0D0
      DO 140 NQ = 2,12
C-----------------------------------------------------------------------
C The PC array will contain the coefficients of the polynomial
C     p(x) = (x+1)*(x+2)*...*(x+nq-1).
C Initially, p(x) = 1.
C-----------------------------------------------------------------------
        RQ1FAC = RQFAC
        RQFAC = RQFAC/NQ
        NQM1 = NQ - 1
        FNQM1 = NQM1
        NQP1 = NQ + 1
C Form coefficients of p(x)*(x+nq-1). ----------------------------------
        PC(NQ) = 0.0D0
        DO 110 IB = 1,NQM1
          I = NQP1 - IB
C110      PC(I) = PC(I-1) + FNQM1*PC(I)
          PC(I) = PC(I-1) + FNQM1*PC(I)
 110      CONTINUE
        PC(1) = FNQM1*PC(1)
C Compute integral, -1 to 0, of p(x) and x*p(x). -----------------------
        PINT = PC(1)
        XPIN = PC(1)/2.0D0
        TSIGN = 1.0D0
        DO 120 I = 2,NQ
          TSIGN = -TSIGN
          PINT = PINT + TSIGN*PC(I)/I
C120      XPIN = XPIN + TSIGN*PC(I)/(I+1)
          XPIN = XPIN + TSIGN*PC(I)/(I+1)
 120      CONTINUE
C Store coefficients in ELCO and TESCO. --------------------------------
        ELCO(1,NQ) = PINT*RQ1FAC
        ELCO(2,NQ) = 1.0D0
        DO 130 I = 2,NQ
C130      ELCO(I+1,NQ) = RQ1FAC*PC(I)/I
          ELCO(I+1,NQ) = RQ1FAC*PC(I)/I
 130      CONTINUE
        AGAMQ = RQFAC*XPIN
        RAGQ = 1.0D0/AGAMQ
        TESCO(2,NQ) = RAGQ
        IF (NQ .LT. 12) TESCO(1,NQP1) = RAGQ*RQFAC/NQP1
        TESCO(3,NQM1) = RAGQ
 140    CONTINUE
      RETURN
C
 200  PC(1) = 1.0D0
      RQ1FAC = 1.0D0
      DO 230 NQ = 1,5
C-----------------------------------------------------------------------
C The PC array will contain the coefficients of the polynomial
C     p(x) = (x+1)*(x+2)*...*(x+nq).
C Initially, p(x) = 1.
C-----------------------------------------------------------------------
        FNQ = NQ
        NQP1 = NQ + 1
C Form coefficients of p(x)*(x+nq). ------------------------------------
        PC(NQP1) = 0.0D0
        DO 210 IB = 1,NQ
          I = NQ + 2 - IB
C210      PC(I) = PC(I-1) + FNQ*PC(I)
          PC(I) = PC(I-1) + FNQ*PC(I)
 210      CONTINUE
        PC(1) = FNQ*PC(1)
C Store coefficients in ELCO and TESCO. --------------------------------
        DO 220 I = 1,NQP1
C220      ELCO(I,NQ) = PC(I)/PC(2)
          ELCO(I,NQ) = PC(I)/PC(2)
 220      CONTINUE
        ELCO(2,NQ) = 1.0D0
        TESCO(1,NQ) = RQ1FAC
        TESCO(2,NQ) = NQP1/ELCO(1,NQ)
        TESCO(3,NQ) = (NQ+2)/ELCO(1,NQ)
        RQ1FAC = RQ1FAC/FNQ
 230    CONTINUE
      RETURN
C----------------------- END OF SUBROUTINE DCFODE ----------------------
      END
*DECK DINTDY
      SUBROUTINE DINTDY (T, K, YH, NYH, DKY, IFLAG)
C***BEGIN PROLOGUE  DINTDY
C***SUBSIDIARY
C***PURPOSE  Interpolate solution derivatives.
C***TYPE      DOUBLE PRECISION (SINTDY-S, DINTDY-D)
C***AUTHOR  Hindmarsh, Alan C., (LLNL)
C***DESCRIPTION
C
C  DINTDY computes interpolated values of the K-th derivative of the
C  dependent variable vector y, and stores it in DKY.  This routine
C  is called within the package with K = 0 and T = TOUT, but may
C  also be called by the user for any K up to the current order.
C  (See detailed instructions in the usage documentation.)
C
C  The computed values in DKY are gotten by interpolation using the
C  Nordsieck history array YH.  This array corresponds uniquely to a
C  vector-valued polynomial of degree NQCUR or less, and DKY is set
C  to the K-th derivative of this polynomial at T.
C  The formula for DKY is:
C               q
C   DKY(i)  =  sum  c(j,K) * (T - tn)**(j-K) * h**(-j) * YH(i,j+1)
C              j=K
C  where  c(j,K) = j*(j-1)*...*(j-K+1), q = NQCUR, tn = TCUR, h = HCUR.
C  The quantities  nq = NQCUR, l = nq+1, N = NEQ, tn, and h are
C  communicated by COMMON.  The above sum is done in reverse order.
C  IFLAG is returned negative if either K or T is out of bounds.
C
C***SEE ALSO  DLSODE
C***ROUTINES CALLED  XERRWD
C***COMMON BLOCKS    DLS001
C***REVISION HISTORY  (YYMMDD)
C   791129  DATE WRITTEN
C   890501  Modified prologue to SLATEC/LDOC format.  (FNF)
C   890503  Minor cosmetic changes.  (FNF)
C   930809  Renamed to allow single/double precision versions. (ACH)
C   010418  Reduced size of Common block /DLS001/. (ACH)
C   031105  Restored 'own' variables to Common block /DLS001/, to
C           enable interrupt/restart feature. (ACH)
C   050427  Corrected roundoff decrement in TP. (ACH)
C***END PROLOGUE  DINTDY
C**End
      INTEGER K, NYH, IFLAG
      DOUBLE PRECISION T, YH, DKY
      DIMENSION YH(NYH,*), DKY(*)
      INTEGER IOWND, IOWNS,
     1   ICF, IERPJ, IERSL, JCUR, JSTART, KFLAG, L,
     2   LYH, LEWT, LACOR, LSAVF, LWM, LIWM, METH, MITER,
     3   MAXORD, MAXCOR, MSBP, MXNCF, N, NQ, NST, NFE, NJE, NQU
      DOUBLE PRECISION ROWNS,
     1   CCMAX, EL0, H, HMIN, HMXI, HU, RC, TN, UROUND
      COMMON /DLS001/ ROWNS(209),
     1   CCMAX, EL0, H, HMIN, HMXI, HU, RC, TN, UROUND,
     2   IOWND(6), IOWNS(6),
     3   ICF, IERPJ, IERSL, JCUR, JSTART, KFLAG, L,
     4   LYH, LEWT, LACOR, LSAVF, LWM, LIWM, METH, MITER,
     5   MAXORD, MAXCOR, MSBP, MXNCF, N, NQ, NST, NFE, NJE, NQU
      INTEGER I, IC, J, JB, JB2, JJ, JJ1, JP1
      DOUBLE PRECISION C, R, S, TP
      CHARACTER (LEN=80) MSG
C
C***FIRST EXECUTABLE STATEMENT  DINTDY
      IFLAG = 0
      IF (K .LT. 0 .OR. K .GT. NQ) GO TO 80
      TP = TN - HU -  100.0D0*UROUND*SIGN(ABS(TN) + ABS(HU), HU)
      IF ((T-TP)*(T-TN) .GT. 0.0D0) GO TO 90
C
      S = (T - TN)/H
      IC = 1
      IF (K .EQ. 0) GO TO 15
      JJ1 = L - K
      DO 10 JJ = JJ1,NQ
C10     IC = IC*JJ
        IC = IC*JJ
 10     CONTINUE
 15   C = IC
      DO 20 I = 1,N
C20     DKY(I) = C*YH(I,L)
        DKY(I) = C*YH(I,L)
 20     CONTINUE
      IF (K .EQ. NQ) GO TO 55
      JB2 = NQ - K
      DO 50 JB = 1,JB2
        J = NQ - JB
        JP1 = J + 1
        IC = 1
        IF (K .EQ. 0) GO TO 35
        JJ1 = JP1 - K
        DO 30 JJ = JJ1,J
C30       IC = IC*JJ
          IC = IC*JJ
 30       CONTINUE
 35     C = IC
        DO 40 I = 1,N
C40       DKY(I) = C*YH(I,JP1) + S*DKY(I)
          DKY(I) = C*YH(I,JP1) + S*DKY(I)
 40       CONTINUE
 50     CONTINUE
      IF (K .EQ. 0) RETURN
 55   R = H**(-K)
      DO 60 I = 1,N
C60     DKY(I) = R*DKY(I)
        DKY(I) = R*DKY(I)
 60     CONTINUE
      RETURN
C
 80   MSG = 'DINTDY-  K (=I1) illegal      '
c     CALL XERRWD (MSG, 30, 51, 0, 1, K, 0, 0, 0.0D0, 0.0D0)
      CALL XERRWD (51, 0)
      IFLAG = -1
      RETURN
 90   MSG = 'DINTDY-  T (=R1) illegal      '
c     CALL XERRWD (MSG, 30, 52, 0, 0, 0, 0, 1, T, 0.0D0)
c     MSG='      T not in interval TCUR - HU (= R1) to TCUR (=R2)      '
c     CALL XERRWD (MSG, 60, 52, 0, 0, 0, 0, 2, TP, TN)
      CALL XERRWD (52, 0)
      IFLAG = -2
      RETURN
C----------------------- END OF SUBROUTINE DINTDY ----------------------
      END
*DECK DSOLSY
      SUBROUTINE DSOLSY (WM, IWM, X)
C     SUBROUTINE DSOLSY (WM, IWM, X, TEM)
C***BEGIN PROLOGUE  DSOLSY
C***SUBSIDIARY
C***PURPOSE  ODEPACK linear system solver.
C***TYPE      DOUBLE PRECISION (SSOLSY-S, DSOLSY-D)
C***AUTHOR  Hindmarsh, Alan C., (LLNL)
C***DESCRIPTION
C
C  This routine manages the solution of the linear system arising from
C  a chord iteration.  It is called if MITER .ne. 0.
C  If MITER is 1 or 2, it calls DGESL to accomplish this.
C  If MITER = 3 it updates the coefficient h*EL0 in the diagonal
C  matrix, and then computes the solution.
C  If MITER is 4 or 5, it calls DGBSL.
C  Communication with DSOLSY uses the following variables:
C  WM    = real work space containing the inverse diagonal matrix if
C          MITER = 3 and the LU decomposition of the matrix otherwise.
C          Storage of matrix elements starts at WM(3).
C          WM also contains the following matrix-related data:
C          WM(1) = SQRT(UROUND) (not used here),
C          WM(2) = HL0, the previous value of h*EL0, used if MITER = 3.
C  IWM   = integer work space containing pivot information, starting at
C          IWM(21), if MITER is 1, 2, 4, or 5.  IWM also contains band
C          parameters ML = IWM(1) and MU = IWM(2) if MITER is 4 or 5.
C  X     = the right-hand side vector on input, and the solution vector
C          on output, of length N.
C  TEM   = vector of work space of length N, not used in this version.
C  IERSL = output flag (in COMMON).  IERSL = 0 if no trouble occurred.
C          IERSL = 1 if a singular matrix arose with MITER = 3.
C  This routine also uses the COMMON variables EL0, H, MITER, and N.
C
C***SEE ALSO  DLSODE
C***ROUTINES CALLED  DGBSL, DGESL
C***COMMON BLOCKS    DLS001
C***REVISION HISTORY  (YYMMDD)
C   791129  DATE WRITTEN
C   890501  Modified prologue to SLATEC/LDOC format.  (FNF)
C   890503  Minor cosmetic changes.  (FNF)
C   930809  Renamed to allow single/double precision versions. (ACH)
C   010418  Reduced size of Common block /DLS001/. (ACH)
C   031105  Restored 'own' variables to Common block /DLS001/, to
C           enable interrupt/restart feature. (ACH)
C***END PROLOGUE  DSOLSY
C**End
      INTEGER IWM
      DOUBLE PRECISION WM, X
      DIMENSION WM(*), IWM(*), X(*)
C     DOUBLE PRECISION WM, X, TEM
C     DIMENSION WM(*), IWM(*), X(*), TEM(*)
      INTEGER IOWND, IOWNS,
     1   ICF, IERPJ, IERSL, JCUR, JSTART, KFLAG, L,
     2   LYH, LEWT, LACOR, LSAVF, LWM, LIWM, METH, MITER,
     3   MAXORD, MAXCOR, MSBP, MXNCF, N, NQ, NST, NFE, NJE, NQU
      DOUBLE PRECISION ROWNS,
     1   CCMAX, EL0, H, HMIN, HMXI, HU, RC, TN, UROUND
      COMMON /DLS001/ ROWNS(209),
     1   CCMAX, EL0, H, HMIN, HMXI, HU, RC, TN, UROUND,
     2   IOWND(6), IOWNS(6),
     3   ICF, IERPJ, IERSL, JCUR, JSTART, KFLAG, L,
     4   LYH, LEWT, LACOR, LSAVF, LWM, LIWM, METH, MITER,
     5   MAXORD, MAXCOR, MSBP, MXNCF, N, NQ, NST, NFE, NJE, NQU
      INTEGER I, MEBAND, ML, MU
      DOUBLE PRECISION DI, HL0, PHL0, R
C
C***FIRST EXECUTABLE STATEMENT  DSOLSY
      IERSL = 0
C     GO TO (100, 100, 300, 400, 400), MITER
      IF (MITER .EQ. 1) GO TO 100
      IF (MITER .EQ. 2) GO TO 100
      IF (MITER .EQ. 3) GO TO 300
      IF (MITER .EQ. 4) GO TO 400
      IF (MITER .EQ. 5) GO TO 400
 100  CALL DGESL (WM(3), N, N, IWM(21), X, 0)
      RETURN
C
 300  PHL0 = WM(2)
      HL0 = H*EL0
      WM(2) = HL0
      IF (HL0 .EQ. PHL0) GO TO 330
      R = HL0/PHL0
      DO 320 I = 1,N
        DI = 1.0D0 - R*(1.0D0 - 1.0D0/WM(I+2))
        IF (ABS(DI) .EQ. 0.0D0) GO TO 390
C320    WM(I+2) = 1.0D0/DI
        WM(I+2) = 1.0D0/DI
 320    CONTINUE
 330  DO 340 I = 1,N
C340    X(I) = WM(I+2)*X(I)
        X(I) = WM(I+2)*X(I)
 340    CONTINUE
      RETURN
 390  IERSL = 1
      RETURN
C
 400  ML = IWM(1)
      MU = IWM(2)
      MEBAND = 2*ML + MU + 1
      CALL DGBSL (WM(3), MEBAND, N, ML, MU, IWM(21), X, 0)
      RETURN
C----------------------- END OF SUBROUTINE DSOLSY ----------------------
      END
*DECK DEWSET
      SUBROUTINE DEWSET (N, ITOL, RTOL, ATOL, YCUR, EWT)
C***BEGIN PROLOGUE  DEWSET
C***SUBSIDIARY
C***PURPOSE  Set error weight vector.
C***TYPE      DOUBLE PRECISION (SEWSET-S, DEWSET-D)
C***AUTHOR  Hindmarsh, Alan C., (LLNL)
C***DESCRIPTION
C
C  This subroutine sets the error weight vector EWT according to
C      EWT(i) = RTOL(i)*ABS(YCUR(i)) + ATOL(i),  i = 1,...,N,
C  with the subscript on RTOL and/or ATOL possibly replaced by 1 above,
C  depending on the value of ITOL.
C
C***SEE ALSO  DLSODE
C***ROUTINES CALLED  (NONE)
C***REVISION HISTORY  (YYMMDD)
C   791129  DATE WRITTEN
C   890501  Modified prologue to SLATEC/LDOC format.  (FNF)
C   890503  Minor cosmetic changes.  (FNF)
C   930809  Renamed to allow single/double precision versions. (ACH)
C***END PROLOGUE  DEWSET
C**End
      INTEGER N, ITOL
      INTEGER I
      DOUBLE PRECISION RTOL, ATOL, YCUR, EWT
      DIMENSION RTOL(*), ATOL(*), YCUR(N), EWT(N)
C
C***FIRST EXECUTABLE STATEMENT  DEWSET
C     GO TO (10, 20, 30, 40), ITOL
      IF (ITOL .EQ. 1) GO TO 10
      IF (ITOL .EQ. 2) GO TO 20
      IF (ITOL .EQ. 3) GO TO 30
      IF (ITOL .EQ. 4) GO TO 40
 10   CONTINUE
      DO 15 I = 1,N
C15     EWT(I) = RTOL(1)*ABS(YCUR(I)) + ATOL(1)
        EWT(I) = RTOL(1)*ABS(YCUR(I)) + ATOL(1)
 15     CONTINUE
      RETURN
 20   CONTINUE
      DO 25 I = 1,N
C25     EWT(I) = RTOL(1)*ABS(YCUR(I)) + ATOL(I)
        EWT(I) = RTOL(1)*ABS(YCUR(I)) + ATOL(I)
 25     CONTINUE
      RETURN
 30   CONTINUE
      DO 35 I = 1,N
C35     EWT(I) = RTOL(I)*ABS(YCUR(I)) + ATOL(1)
        EWT(I) = RTOL(I)*ABS(YCUR(I)) + ATOL(1)
 35     CONTINUE
      RETURN
 40   CONTINUE
      DO 45 I = 1,N
C45     EWT(I) = RTOL(I)*ABS(YCUR(I)) + ATOL(I)
        EWT(I) = RTOL(I)*ABS(YCUR(I)) + ATOL(I)
 45     CONTINUE
      RETURN
C----------------------- END OF SUBROUTINE DEWSET ----------------------
      END
*DECK DSTODA
      SUBROUTINE DSTODA (NEQ, Y, YH, NYH, YH1, EWT, SAVF, ACOR,
     1   WM, IWM, F, JAC, PJAC, SLVS)
      EXTERNAL F, JAC, PJAC, SLVS
      INTEGER NEQ, NYH, IWM
      DOUBLE PRECISION Y, YH, YH1, EWT, SAVF, ACOR, WM
      DIMENSION NEQ(*), Y(*), YH(NYH,*), YH1(*), EWT(*), SAVF(*),
     1   ACOR(*), WM(*), IWM(*)
      INTEGER IOWND, IALTH, IPUP, LMAX, MEO, NQNYH, NSLP,
     1   ICF, IERPJ, IERSL, JCUR, JSTART, KFLAG, L,
     2   LYH, LEWT, LACOR, LSAVF, LWM, LIWM, METH, MITER,
     3   MAXORD, MAXCOR, MSBP, MXNCF, N, NQ, NST, NFE, NJE, NQU
      INTEGER IOWND2, ICOUNT, IRFLAG, JTYP, MUSED, MXORDN, MXORDS
      DOUBLE PRECISION CONIT, CRATE, EL, ELCO, HOLD, RMAX, TESCO,
     2   CCMAX, EL0, H, HMIN, HMXI, HU, RC, TN, UROUND
      DOUBLE PRECISION ROWND2, CM1, CM2, PDEST, PDLAST, RATIO,
     1   PDNORM
      COMMON /DLS001/ CONIT, CRATE, EL(13), ELCO(13,12),
     1   HOLD, RMAX, TESCO(3,12),
     2   CCMAX, EL0, H, HMIN, HMXI, HU, RC, TN, UROUND,
     3   IOWND(6), IALTH, IPUP, LMAX, MEO, NQNYH, NSLP,
     4   ICF, IERPJ, IERSL, JCUR, JSTART, KFLAG, L,
     5   LYH, LEWT, LACOR, LSAVF, LWM, LIWM, METH, MITER,
     6   MAXORD, MAXCOR, MSBP, MXNCF, N, NQ, NST, NFE, NJE, NQU
      COMMON /DLSA01/ ROWND2, CM1(12), CM2(5), PDEST, PDLAST, RATIO,
     1   PDNORM,
     2   IOWND2(3), ICOUNT, IRFLAG, JTYP, MUSED, MXORDN, MXORDS
      INTEGER I, I1, IREDO, IRET, J, JB, M, NCF, NEWQ
      INTEGER LM1, LM1P1, LM2, LM2P1, NQM1, NQM2
      DOUBLE PRECISION DCON, DDN, DEL, DELP, DSM, DUP, EXDN, EXSM, EXUP,
     1   R, RH, RHDN, RHSM, RHUP, TOLD, DMNORM
      DOUBLE PRECISION ALPHA, DM1,DM2, EXM1,EXM2,
     1   PDH, PNORM, RATE, RH1, RH1IT, RH2, RM, SM1(12)
      SAVE SM1
      DATA SM1/0.5D0, 0.575D0, 0.55D0, 0.45D0, 0.35D0, 0.25D0,
     1   0.20D0, 0.15D0, 0.10D0, 0.075D0, 0.050D0, 0.025D0/
C-----------------------------------------------------------------------
C DSTODA performs one step of the integration of an initial value
C problem for a system of ordinary differential equations.
C Note: DSTODA is independent of the value of the iteration method
C indicator MITER, when this is .ne. 0, and hence is independent
C of the type of chord method used, or the Jacobian structure.
C Communication with DSTODA is done with the following variables:
C
C Y      = an array of length .ge. N used as the Y argument in
C          all calls to F and JAC.
C NEQ    = integer array containing problem size in NEQ(1), and
C          passed as the NEQ argument in all calls to F and JAC.
C YH     = an NYH by LMAX array containing the dependent variables
C          and their approximate scaled derivatives, where
C          LMAX = MAXORD + 1.  YH(i,j+1) contains the approximate
C          j-th derivative of y(i), scaled by H**j/factorial(j)
C          (j = 0,1,...,NQ).  On entry for the first step, the first
C          two columns of YH must be set from the initial values.
C NYH    = a constant integer .ge. N, the first dimension of YH.
C YH1    = a one-dimensional array occupying the same space as YH.
C EWT    = an array of length N containing multiplicative weights
C          for local error measurements.  Local errors in y(i) are
C          compared to 1.0/EWT(i) in various error tests.
C SAVF   = an array of working storage, of length N.
C ACOR   = a work array of length N, used for the accumulated
C          corrections.  On a successful return, ACOR(i) contains
C          the estimated one-step local error in y(i).
C WM,IWM = real and integer work arrays associated with matrix
C          operations in chord iteration (MITER .ne. 0).
C PJAC   = name of routine to evaluate and preprocess Jacobian matrix
C          and P = I - H*EL0*Jac, if a chord method is being used.
C          It also returns an estimate of norm(Jac) in PDNORM.
C SLVS   = name of routine to solve linear system in chord iteration.
C CCMAX  = maximum relative change in H*EL0 before PJAC is called.
C H      = the step size to be attempted on the next step.
C          H is altered by the error control algorithm during the
C          problem.  H can be either positive or negative, but its
C          sign must remain constant throughout the problem.
C HMIN   = the minimum absolute value of the step size H to be used.
C HMXI   = inverse of the maximum absolute value of H to be used.
C          HMXI = 0.0 is allowed and corresponds to an infinite HMAX.
C          HMIN and HMXI may be changed at any time, but will not
C          take effect until the next change of H is considered.
C TN     = the independent variable. TN is updated on each step taken.
C JSTART = an integer used for input only, with the following
C          values and meanings:
C               0  perform the first step.
C           .gt.0  take a new step continuing from the last.
C              -1  take the next step with a new value of H,
C                    N, METH, MITER, and/or matrix parameters.
C              -2  take the next step with a new value of H,
C                    but with other inputs unchanged.
C          On return, JSTART is set to 1 to facilitate continuation.
C KFLAG  = a completion code with the following meanings:
C               0  the step was succesful.
C              -1  the requested error could not be achieved.
C              -2  corrector convergence could not be achieved.
C              -3  fatal error in PJAC or SLVS.
C          A return with KFLAG = -1 or -2 means either
C          ABS(H) = HMIN or 10 consecutive failures occurred.
C          On a return with KFLAG negative, the values of TN and
C          the YH array are as of the beginning of the last
C          step, and H is the last step size attempted.
C MAXORD = the maximum order of integration method to be allowed.
C MAXCOR = the maximum number of corrector iterations allowed.
C MSBP   = maximum number of steps between PJAC calls (MITER .gt. 0).
C MXNCF  = maximum number of convergence failures allowed.
C METH   = current method.
C          METH = 1 means Adams method (nonstiff)
C          METH = 2 means BDF method (stiff)
C          METH may be reset by DSTODA.
C MITER  = corrector iteration method.
C          MITER = 0 means functional iteration.
C          MITER = JT .gt. 0 means a chord iteration corresponding
C          to Jacobian type JT.  (The DLSODA/DLSODAR argument JT is
C          communicated here as JTYP, but is not used in DSTODA
C          except to load MITER following a method switch.)
C          MITER may be reset by DSTODA.
C N      = the number of first-order differential equations.
C-----------------------------------------------------------------------
      KFLAG = 0
      TOLD = TN
      NCF = 0
      IERPJ = 0
      IERSL = 0
      JCUR = 0
      ICF = 0
      DELP = 0.0D0
      IF (JSTART .GT. 0) GO TO 200
      IF (JSTART .EQ. -1) GO TO 100
      IF (JSTART .EQ. -2) GO TO 160
C-----------------------------------------------------------------------
C On the first call, the order is set to 1, and other variables are
C initialized.  RMAX is the maximum ratio by which H can be increased
C in a single step.  It is initially 1.E4 to compensate for the small
C initial H, but then is normally equal to 10.  If a failure
C occurs (in corrector convergence or error test), RMAX is set at 2
C for the next increase.
C DCFODE is called to get the needed coefficients for both methods.
C-----------------------------------------------------------------------
      LMAX = MAXORD + 1
      NQ = 1
      L = 2
      IALTH = 2
      RMAX = 10000.0D0
      RC = 0.0D0
      EL0 = 1.0D0
      CRATE = 0.7D0
      HOLD = H
      NSLP = 0
      IPUP = MITER
      IRET = 3
C Initialize switching parameters.  METH = 1 is assumed initially. -----
      ICOUNT = 20
      IRFLAG = 0
      PDEST = 0.0D0
      PDLAST = 0.0D0
      RATIO = 5.0D0
      CALL DCFODE (2, ELCO, TESCO)
      DO 10 I = 1,5
C10     CM2(I) = TESCO(2,I)*ELCO(I+1,I)
        CM2(I) = TESCO(2,I)*ELCO(I+1,I)
 10     CONTINUE
      CALL DCFODE (1, ELCO, TESCO)
      DO 20 I = 1,12
C20     CM1(I) = TESCO(2,I)*ELCO(I+1,I)
        CM1(I) = TESCO(2,I)*ELCO(I+1,I)
 20     CONTINUE
      GO TO 150
C-----------------------------------------------------------------------
C The following block handles preliminaries needed when JSTART = -1.
C IPUP is set to MITER to force a matrix update.
C If an order increase is about to be considered (IALTH = 1),
C IALTH is reset to 2 to postpone consideration one more step.
C If the caller has changed METH, DCFODE is called to reset
C the coefficients of the method.
C If H is to be changed, YH must be rescaled.
C If H or METH is being changed, IALTH is reset to L = NQ + 1
C to prevent further changes in H for that many steps.
C-----------------------------------------------------------------------
 100  IPUP = MITER
      LMAX = MAXORD + 1
      IF (IALTH .EQ. 1) IALTH = 2
      IF (METH .EQ. MUSED) GO TO 160
      CALL DCFODE (METH, ELCO, TESCO)
      IALTH = L
      IRET = 1
C-----------------------------------------------------------------------
C The el vector and related constants are reset
C whenever the order NQ is changed, or at the start of the problem.
C-----------------------------------------------------------------------
 150  DO 155 I = 1,L
C155    EL(I) = ELCO(I,NQ)
        EL(I) = ELCO(I,NQ)
 155    CONTINUE
      NQNYH = NQ*NYH
      RC = RC*EL(1)/EL0
      EL0 = EL(1)
      CONIT = 0.5D0/(NQ+2)
C     GO TO (160, 170, 200), IRET
      IF (IRET .EQ. 1) GO TO 160
      IF (IRET .EQ. 2) GO TO 170
      IF (IRET .EQ. 3) GO TO 200
C-----------------------------------------------------------------------
C If H is being changed, the H ratio RH is checked against
C RMAX, HMIN, and HMXI, and the YH array rescaled.  IALTH is set to
C L = NQ + 1 to prevent a change of H for that many steps, unless
C forced by a convergence or error test failure.
C-----------------------------------------------------------------------
 160  IF (H .EQ. HOLD) GO TO 200
      RH = H/HOLD
      H = HOLD
      IREDO = 3
      GO TO 175
 170  RH = MAX(RH,HMIN/ABS(H))
 175  RH = MIN(RH,RMAX)
      RH = RH/MAX(1.0D0,ABS(H)*HMXI*RH)
C-----------------------------------------------------------------------
C If METH = 1, also restrict the new step size by the stability region.
C If this reduces H, set IRFLAG to 1 so that if there are roundoff
C problems later, we can assume that is the cause of the trouble.
C-----------------------------------------------------------------------
      IF (METH .EQ. 2) GO TO 178
      IRFLAG = 0
      PDH = MAX(ABS(H)*PDLAST,0.000001D0)
      IF (RH*PDH*1.00001D0 .LT. SM1(NQ)) GO TO 178
      RH = SM1(NQ)/PDH
      IRFLAG = 1
 178  CONTINUE
      R = 1.0D0
      DO 181 J = 2,L
        R = R*RH
        DO 180 I = 1,N
C180      YH(I,J) = YH(I,J)*R
          YH(I,J) = YH(I,J)*R
 180      CONTINUE
 181      CONTINUE
      H = H*RH
      RC = RC*RH
      IALTH = L
      IF (IREDO .EQ. 0) GO TO 690
C-----------------------------------------------------------------------
C This section computes the predicted values by effectively
C multiplying the YH array by the Pascal triangle matrix.
C RC is the ratio of new to old values of the coefficient  H*EL(1).
C When RC differs from 1 by more than CCMAX, IPUP is set to MITER
C to force PJAC to be called, if a Jacobian is involved.
C In any case, PJAC is called at least every MSBP steps.
C-----------------------------------------------------------------------
 200  IF (ABS(RC-1.0D0) .GT. CCMAX) IPUP = MITER
      IF (NST .GE. NSLP+MSBP) IPUP = MITER
      TN = TN + H
      I1 = NQNYH + 1
      DO 215 JB = 1,NQ
        I1 = I1 - NYH
        DO 210 I = I1,NQNYH
C210      YH1(I) = YH1(I) + YH1(I+NYH)
          YH1(I) = YH1(I) + YH1(I+NYH)
 210      CONTINUE
 215    CONTINUE
      PNORM = DMNORM (N, YH1, EWT)
C-----------------------------------------------------------------------
C Up to MAXCOR corrector iterations are taken.  A convergence test is
C made on the RMS-norm of each correction, weighted by the error
C weight vector EWT.  The sum of the corrections is accumulated in the
C vector ACOR(i).  The YH array is not altered in the corrector loop.
C-----------------------------------------------------------------------
 220  M = 0
      RATE = 0.0D0
      DEL = 0.0D0
      DO 230 I = 1,N
C230    Y(I) = YH(I,1)
        Y(I) = YH(I,1)
 230    CONTINUE
      CALL F (NEQ, TN, Y, SAVF)
      NFE = NFE + 1
      IF (IPUP .LE. 0) GO TO 250
C-----------------------------------------------------------------------
C If indicated, the matrix P = I - H*EL(1)*J is reevaluated and
C preprocessed before starting the corrector iteration.  IPUP is set
C to 0 as an indicator that this has been done.
C-----------------------------------------------------------------------
      CALL PJAC (NEQ, Y, YH, NYH, EWT, ACOR, SAVF, WM, IWM, F, JAC)
      IPUP = 0
      RC = 1.0D0
      NSLP = NST
      CRATE = 0.7D0
      IF (IERPJ .NE. 0) GO TO 430
 250  DO 260 I = 1,N
C260    ACOR(I) = 0.0D0
        ACOR(I) = 0.0D0
 260    CONTINUE
 270  IF (MITER .NE. 0) GO TO 350
C-----------------------------------------------------------------------
C In the case of functional iteration, update Y directly from
C the result of the last function evaluation.
C-----------------------------------------------------------------------
      DO 290 I = 1,N
        SAVF(I) = H*SAVF(I) - YH(I,2)
C290    Y(I) = SAVF(I) - ACOR(I)
        Y(I) = SAVF(I) - ACOR(I)
 290    CONTINUE
      DEL = DMNORM (N, Y, EWT)
      DO 300 I = 1,N
        Y(I) = YH(I,1) + EL(1)*SAVF(I)
C300    ACOR(I) = SAVF(I)
        ACOR(I) = SAVF(I)
 300    CONTINUE
      GO TO 400
C-----------------------------------------------------------------------
C In the case of the chord method, compute the corrector error,
C and solve the linear system with that as right-hand side and
C P as coefficient matrix.
C-----------------------------------------------------------------------
 350  DO 360 I = 1,N
C360    Y(I) = H*SAVF(I) - (YH(I,2) + ACOR(I))
        Y(I) = H*SAVF(I) - (YH(I,2) + ACOR(I))
 360    CONTINUE
      CALL SLVS (WM, IWM, Y, SAVF)
      IF (IERSL .LT. 0) GO TO 430
      IF (IERSL .GT. 0) GO TO 410
      DEL = DMNORM (N, Y, EWT)
      DO 380 I = 1,N
        ACOR(I) = ACOR(I) + Y(I)
C380    Y(I) = YH(I,1) + EL(1)*ACOR(I)
        Y(I) = YH(I,1) + EL(1)*ACOR(I)
 380    CONTINUE
C-----------------------------------------------------------------------
C Test for convergence.  If M .gt. 0, an estimate of the convergence
C rate constant is stored in CRATE, and this is used in the test.
C
C We first check for a change of iterates that is the size of
C roundoff error.  If this occurs, the iteration has converged, and a
C new rate estimate is not formed.
C In all other cases, force at least two iterations to estimate a
C local Lipschitz constant estimate for Adams methods.
C On convergence, form PDEST = local maximum Lipschitz constant
C estimate.  PDLAST is the most recent nonzero estimate.
C-----------------------------------------------------------------------
 400  CONTINUE
      IF (DEL .LE. 100.0D0*PNORM*UROUND) GO TO 450
      IF (M .EQ. 0 .AND. METH .EQ. 1) GO TO 405
      IF (M .EQ. 0) GO TO 402
      RM = 1024.0D0
      IF (DEL .LE. 1024.0D0*DELP) RM = DEL/DELP
      RATE = MAX(RATE,RM)
      CRATE = MAX(0.2D0*CRATE,RM)
 402  DCON = DEL*MIN(1.0D0,1.5D0*CRATE)/(TESCO(2,NQ)*CONIT)
      IF (DCON .GT. 1.0D0) GO TO 405
      PDEST = MAX(PDEST,RATE/ABS(H*EL(1)))
      IF (PDEST .NE. 0.0D0) PDLAST = PDEST
      GO TO 450
 405  CONTINUE
      M = M + 1
      IF (M .EQ. MAXCOR) GO TO 410
      IF (M .GE. 2 .AND. DEL .GT. 2.0D0*DELP) GO TO 410
      DELP = DEL
      CALL F (NEQ, TN, Y, SAVF)
      NFE = NFE + 1
      GO TO 270
C-----------------------------------------------------------------------
C The corrector iteration failed to converge.
C If MITER .ne. 0 and the Jacobian is out of date, PJAC is called for
C the next try.  Otherwise the YH array is retracted to its values
C before prediction, and H is reduced, if possible.  If H cannot be
C reduced or MXNCF failures have occurred, exit with KFLAG = -2.
C-----------------------------------------------------------------------
 410  IF (MITER .EQ. 0 .OR. JCUR .EQ. 1) GO TO 430
      ICF = 1
      IPUP = MITER
      GO TO 220
 430  ICF = 2
      NCF = NCF + 1
      RMAX = 2.0D0
      TN = TOLD
      I1 = NQNYH + 1
      DO 445 JB = 1,NQ
        I1 = I1 - NYH
        DO 440 I = I1,NQNYH
C440      YH1(I) = YH1(I) - YH1(I+NYH)
          YH1(I) = YH1(I) - YH1(I+NYH)
 440      CONTINUE
 445    CONTINUE
      IF (IERPJ .LT. 0 .OR. IERSL .LT. 0) GO TO 680
      IF (ABS(H) .LE. HMIN*1.00001D0) GO TO 670
      IF (NCF .EQ. MXNCF) GO TO 670
      RH = 0.25D0
      IPUP = MITER
      IREDO = 1
      GO TO 170
C-----------------------------------------------------------------------
C The corrector has converged.  JCUR is set to 0
C to signal that the Jacobian involved may need updating later.
C The local error test is made and control passes to statement 500
C if it fails.
C-----------------------------------------------------------------------
 450  JCUR = 0
      IF (M .EQ. 0) DSM = DEL/TESCO(2,NQ)
      IF (M .GT. 0) DSM = DMNORM (N, ACOR, EWT)/TESCO(2,NQ)
      IF (DSM .GT. 1.0D0) GO TO 500
C-----------------------------------------------------------------------
C After a successful step, update the YH array.
C Decrease ICOUNT by 1, and if it is -1, consider switching methods.
C If a method switch is made, reset various parameters,
C rescale the YH array, and exit.  If there is no switch,
C consider changing H if IALTH = 1.  Otherwise decrease IALTH by 1.
C If IALTH is then 1 and NQ .lt. MAXORD, then ACOR is saved for
C use in a possible order increase on the next step.
C If a change in H is considered, an increase or decrease in order
C by one is considered also.  A change in H is made only if it is by a
C factor of at least 1.1.  If not, IALTH is set to 3 to prevent
C testing for that many steps.
C-----------------------------------------------------------------------
      KFLAG = 0
      IREDO = 0
      NST = NST + 1
      HU = H
      NQU = NQ
      MUSED = METH
      DO 461 J = 1,L
        DO 460 I = 1,N
C460      YH(I,J) = YH(I,J) + EL(J)*ACOR(I)
          YH(I,J) = YH(I,J) + EL(J)*ACOR(I)
 460      CONTINUE
 461      CONTINUE
      ICOUNT = ICOUNT - 1
      IF (ICOUNT .GE. 0) GO TO 488
      IF (METH .EQ. 2) GO TO 480
C-----------------------------------------------------------------------
C We are currently using an Adams method.  Consider switching to BDF.
C If the current order is greater than 5, assume the problem is
C not stiff, and skip this section.
C If the Lipschitz constant and error estimate are not polluted
C by roundoff, go to 470 and perform the usual test.
C Otherwise, switch to the BDF methods if the last step was
C restricted to insure stability (irflag = 1), and stay with Adams
C method if not.  When switching to BDF with polluted error estimates,
C in the absence of other information, double the step size.
C
C When the estimates are OK, we make the usual test by computing
C the step size we could have (ideally) used on this step,
C with the current (Adams) method, and also that for the BDF.
C If NQ .gt. MXORDS, we consider changing to order MXORDS on switching.
C Compare the two step sizes to decide whether to switch.
C The step size advantage must be at least RATIO = 5 to switch.
C-----------------------------------------------------------------------
      IF (NQ .GT. 5) GO TO 488
      IF (DSM .GT. 100.0D0*PNORM*UROUND .AND. PDEST .NE. 0.0D0)
     1   GO TO 470
      IF (IRFLAG .EQ. 0) GO TO 488
      RH2 = 2.0D0
      NQM2 = MIN(NQ,MXORDS)
      GO TO 478
 470  CONTINUE
      EXSM = 1.0D0/L
      RH1 = 1.0D0/(1.2D0*DSM**EXSM + 0.0000012D0)
      RH1IT = 2.0D0*RH1
      PDH = PDLAST*ABS(H)
      IF (PDH*RH1 .GT. 0.00001D0) RH1IT = SM1(NQ)/PDH
      RH1 = MIN(RH1,RH1IT)
      IF (NQ .LE. MXORDS) GO TO 474
         NQM2 = MXORDS
         LM2 = MXORDS + 1
         EXM2 = 1.0D0/LM2
         LM2P1 = LM2 + 1
         DM2 = DMNORM (N, YH(1,LM2P1), EWT)/CM2(MXORDS)
         RH2 = 1.0D0/(1.2D0*DM2**EXM2 + 0.0000012D0)
         GO TO 476
 474  DM2 = DSM*(CM1(NQ)/CM2(NQ))
      RH2 = 1.0D0/(1.2D0*DM2**EXSM + 0.0000012D0)
      NQM2 = NQ
 476  CONTINUE
      IF (RH2 .LT. RATIO*RH1) GO TO 488
C THE SWITCH TEST PASSED.  RESET RELEVANT QUANTITIES FOR BDF. ----------
 478  RH = RH2
      ICOUNT = 20
      METH = 2
      MITER = JTYP
      PDLAST = 0.0D0
      NQ = NQM2
      L = NQ + 1
      GO TO 170
C-----------------------------------------------------------------------
C We are currently using a BDF method.  Consider switching to Adams.
C Compute the step size we could have (ideally) used on this step,
C with the current (BDF) method, and also that for the Adams.
C If NQ .gt. MXORDN, we consider changing to order MXORDN on switching.
C Compare the two step sizes to decide whether to switch.
C The step size advantage must be at least 5/RATIO = 1 to switch.
C If the step size for Adams would be so small as to cause
C roundoff pollution, we stay with BDF.
C-----------------------------------------------------------------------
 480  CONTINUE
      EXSM = 1.0D0/L
      IF (MXORDN .GE. NQ) GO TO 484
         NQM1 = MXORDN
         LM1 = MXORDN + 1
         EXM1 = 1.0D0/LM1
         LM1P1 = LM1 + 1
         DM1 = DMNORM (N, YH(1,LM1P1), EWT)/CM1(MXORDN)
         RH1 = 1.0D0/(1.2D0*DM1**EXM1 + 0.0000012D0)
         GO TO 486
 484  DM1 = DSM*(CM2(NQ)/CM1(NQ))
      RH1 = 1.0D0/(1.2D0*DM1**EXSM + 0.0000012D0)
      NQM1 = NQ
      EXM1 = EXSM
 486  RH1IT = 2.0D0*RH1
      PDH = PDNORM*ABS(H)
      IF (PDH*RH1 .GT. 0.00001D0) RH1IT = SM1(NQM1)/PDH
      RH1 = MIN(RH1,RH1IT)
      RH2 = 1.0D0/(1.2D0*DSM**EXSM + 0.0000012D0)
      IF (RH1*RATIO .LT. 5.0D0*RH2) GO TO 488
      ALPHA = MAX(0.001D0,RH1)
      DM1 = (ALPHA**EXM1)*DM1
      IF (DM1 .LE. 1000.0D0*UROUND*PNORM) GO TO 488
C The switch test passed.  Reset relevant quantities for Adams. --------
      RH = RH1
      ICOUNT = 20
      METH = 1
      MITER = 0
      PDLAST = 0.0D0
      NQ = NQM1
      L = NQ + 1
      GO TO 170
C
C No method switch is being made.  Do the usual step/order selection. --
 488  CONTINUE
      IALTH = IALTH - 1
      IF (IALTH .EQ. 0) GO TO 520
      IF (IALTH .GT. 1) GO TO 700
      IF (L .EQ. LMAX) GO TO 700
      DO 490 I = 1,N
C490    YH(I,LMAX) = ACOR(I)
        YH(I,LMAX) = ACOR(I)
 490    CONTINUE
      GO TO 700
C-----------------------------------------------------------------------
C The error test failed.  KFLAG keeps track of multiple failures.
C Restore TN and the YH array to their previous values, and prepare
C to try the step again.  Compute the optimum step size for this or
C one lower order.  After 2 or more failures, H is forced to decrease
C by a factor of 0.2 or less.
C-----------------------------------------------------------------------
 500  KFLAG = KFLAG - 1
      TN = TOLD
      I1 = NQNYH + 1
      DO 515 JB = 1,NQ
        I1 = I1 - NYH
        DO 510 I = I1,NQNYH
C510      YH1(I) = YH1(I) - YH1(I+NYH)
          YH1(I) = YH1(I) - YH1(I+NYH)
 510      CONTINUE
 515    CONTINUE
      RMAX = 2.0D0
      IF (ABS(H) .LE. HMIN*1.00001D0) GO TO 660
      IF (KFLAG .LE. -3) GO TO 640
      IREDO = 2
      RHUP = 0.0D0
      GO TO 540
C-----------------------------------------------------------------------
C Regardless of the success or failure of the step, factors
C RHDN, RHSM, and RHUP are computed, by which H could be multiplied
C at order NQ - 1, order NQ, or order NQ + 1, respectively.
C In the case of failure, RHUP = 0.0 to avoid an order increase.
C The largest of these is determined and the new order chosen
C accordingly.  If the order is to be increased, we compute one
C additional scaled derivative.
C-----------------------------------------------------------------------
 520  RHUP = 0.0D0
      IF (L .EQ. LMAX) GO TO 540
      DO 530 I = 1,N
C530    SAVF(I) = ACOR(I) - YH(I,LMAX)
        SAVF(I) = ACOR(I) - YH(I,LMAX)
 530    CONTINUE
      DUP = DMNORM (N, SAVF, EWT)/TESCO(3,NQ)
      EXUP = 1.0D0/(L+1)
      RHUP = 1.0D0/(1.4D0*DUP**EXUP + 0.0000014D0)
 540  EXSM = 1.0D0/L
      RHSM = 1.0D0/(1.2D0*DSM**EXSM + 0.0000012D0)
      RHDN = 0.0D0
      IF (NQ .EQ. 1) GO TO 550
      DDN = DMNORM (N, YH(1,L), EWT)/TESCO(1,NQ)
      EXDN = 1.0D0/NQ
      RHDN = 1.0D0/(1.3D0*DDN**EXDN + 0.0000013D0)
C If METH = 1, limit RH according to the stability region also. --------
 550  IF (METH .EQ. 2) GO TO 560
      PDH = MAX(ABS(H)*PDLAST,0.000001D0)
      IF (L .LT. LMAX) RHUP = MIN(RHUP,SM1(L)/PDH)
      RHSM = MIN(RHSM,SM1(NQ)/PDH)
      IF (NQ .GT. 1) RHDN = MIN(RHDN,SM1(NQ-1)/PDH)
      PDEST = 0.0D0
 560  IF (RHSM .GE. RHUP) GO TO 570
      IF (RHUP .GT. RHDN) GO TO 590
      GO TO 580
 570  IF (RHSM .LT. RHDN) GO TO 580
      NEWQ = NQ
      RH = RHSM
      GO TO 620
 580  NEWQ = NQ - 1
      RH = RHDN
      IF (KFLAG .LT. 0 .AND. RH .GT. 1.0D0) RH = 1.0D0
      GO TO 620
 590  NEWQ = L
      RH = RHUP
      IF (RH .LT. 1.1D0) GO TO 610
      R = EL(L)/L
      DO 600 I = 1,N
C600    YH(I,NEWQ+1) = ACOR(I)*R
        YH(I,NEWQ+1) = ACOR(I)*R
 600    CONTINUE
      GO TO 630
 610  IALTH = 3
      GO TO 700
C If METH = 1 and H is restricted by stability, bypass 10 percent test.
 620  IF (METH .EQ. 2) GO TO 622
      IF (RH*PDH*1.00001D0 .GE. SM1(NEWQ)) GO TO 625
 622  IF (KFLAG .EQ. 0 .AND. RH .LT. 1.1D0) GO TO 610
 625  IF (KFLAG .LE. -2) RH = MIN(RH,0.2D0)
C-----------------------------------------------------------------------
C If there is a change of order, reset NQ, L, and the coefficients.
C In any case H is reset according to RH and the YH array is rescaled.
C Then exit from 690 if the step was OK, or redo the step otherwise.
C-----------------------------------------------------------------------
      IF (NEWQ .EQ. NQ) GO TO 170
 630  NQ = NEWQ
      L = NQ + 1
      IRET = 2
      GO TO 150
C-----------------------------------------------------------------------
C Control reaches this section if 3 or more failures have occured.
C If 10 failures have occurred, exit with KFLAG = -1.
C It is assumed that the derivatives that have accumulated in the
C YH array have errors of the wrong order.  Hence the first
C derivative is recomputed, and the order is set to 1.  Then
C H is reduced by a factor of 10, and the step is retried,
C until it succeeds or H reaches HMIN.
C-----------------------------------------------------------------------
 640  IF (KFLAG .EQ. -10) GO TO 660
      RH = 0.1D0
      RH = MAX(HMIN/ABS(H),RH)
      H = H*RH
      DO 645 I = 1,N
C645    Y(I) = YH(I,1)
        Y(I) = YH(I,1)
 645    CONTINUE
      CALL F (NEQ, TN, Y, SAVF)
      NFE = NFE + 1
      DO 650 I = 1,N
C650    YH(I,2) = H*SAVF(I)
        YH(I,2) = H*SAVF(I)
 650    CONTINUE
      IPUP = MITER
      IALTH = 5
      IF (NQ .EQ. 1) GO TO 200
      NQ = 1
      L = 2
      IRET = 3
      GO TO 150
C-----------------------------------------------------------------------
C All returns are made through this section.  H is saved in HOLD
C to allow the caller to change H on the next step.
C-----------------------------------------------------------------------
 660  KFLAG = -1
      GO TO 720
 670  KFLAG = -2
      GO TO 720
 680  KFLAG = -3
      GO TO 720
 690  RMAX = 10.0D0
 700  R = 1.0D0/TESCO(2,NQU)
      DO 710 I = 1,N
C710    ACOR(I) = ACOR(I)*R
        ACOR(I) = ACOR(I)*R
 710    CONTINUE
 720  HOLD = H
      JSTART = 1
      RETURN
C----------------------- End of Subroutine DSTODA ----------------------
      END
*DECK DPRJA
      SUBROUTINE DPRJA (NEQ, Y, YH, NYH, EWT, FTEM, SAVF, WM, IWM,
     1   F, JAC)
      EXTERNAL F, JAC
      INTEGER NEQ, NYH, IWM
      DOUBLE PRECISION Y, YH, EWT, FTEM, SAVF, WM
      DIMENSION NEQ(*), Y(*), YH(NYH,*), EWT(*), FTEM(*), SAVF(*),
     1   WM(*), IWM(*)
      INTEGER IOWND, IOWNS,
     1   ICF, IERPJ, IERSL, JCUR, JSTART, KFLAG, L,
     2   LYH, LEWT, LACOR, LSAVF, LWM, LIWM, METH, MITER,
     3   MAXORD, MAXCOR, MSBP, MXNCF, N, NQ, NST, NFE, NJE, NQU
      INTEGER IOWND2, IOWNS2, JTYP, MUSED, MXORDN, MXORDS
      DOUBLE PRECISION ROWNS,
     1   CCMAX, EL0, H, HMIN, HMXI, HU, RC, TN, UROUND
      DOUBLE PRECISION ROWND2, ROWNS2, PDNORM
      COMMON /DLS001/ ROWNS(209),
     1   CCMAX, EL0, H, HMIN, HMXI, HU, RC, TN, UROUND,
     2   IOWND(6), IOWNS(6),
     3   ICF, IERPJ, IERSL, JCUR, JSTART, KFLAG, L,
     4   LYH, LEWT, LACOR, LSAVF, LWM, LIWM, METH, MITER,
     5   MAXORD, MAXCOR, MSBP, MXNCF, N, NQ, NST, NFE, NJE, NQU
      COMMON /DLSA01/ ROWND2, ROWNS2(20), PDNORM,
     1   IOWND2(3), IOWNS2(2), JTYP, MUSED, MXORDN, MXORDS
      INTEGER I, I1, I2, IER, II, J, J1, JJ, LENP,
     1   MBA, MBAND, MEB1, MEBAND, ML, ML3, MU, NP1
      DOUBLE PRECISION CON, FAC, HL0, R, R0, SRUR, YI, YJ, YJJ,
     1   DMNORM, DFNORM, DBNORM
C-----------------------------------------------------------------------
C DPRJA is called by DSTODA to compute and process the matrix
C P = I - H*EL(1)*J , where J is an approximation to the Jacobian.
C Here J is computed by the user-supplied routine JAC if
C MITER = 1 or 4 or by finite differencing if MITER = 2 or 5.
C J, scaled by -H*EL(1), is stored in WM.  Then the norm of J (the
C matrix norm consistent with the weighted max-norm on vectors given
C by DMNORM) is computed, and J is overwritten by P.  P is then
C subjected to LU decomposition in preparation for later solution
C of linear systems with P as coefficient matrix.  This is done
C by DGEFA if MITER = 1 or 2, and by DGBFA if MITER = 4 or 5.
C
C In addition to variables described previously, communication
C with DPRJA uses the following:
C Y     = array containing predicted values on entry.
C FTEM  = work array of length N (ACOR in DSTODA).
C SAVF  = array containing f evaluated at predicted y.
C WM    = real work space for matrices.  On output it contains the
C         LU decomposition of P.
C         Storage of matrix elements starts at WM(3).
C         WM also contains the following matrix-related data:
C         WM(1) = SQRT(UROUND), used in numerical Jacobian increments.
C IWM   = integer work space containing pivot information, starting at
C         IWM(21).   IWM also contains the band parameters
C         ML = IWM(1) and MU = IWM(2) if MITER is 4 or 5.
C EL0   = EL(1) (input).
C PDNORM= norm of Jacobian matrix. (Output).
C IERPJ = output error flag,  = 0 if no trouble, .gt. 0 if
C         P matrix found to be singular.
C JCUR  = output flag = 1 to indicate that the Jacobian matrix
C         (or approximation) is now current.
C This routine also uses the Common variables EL0, H, TN, UROUND,
C MITER, N, NFE, and NJE.
C-----------------------------------------------------------------------
      NJE = NJE + 1
      IERPJ = 0
      JCUR = 1
      HL0 = H*EL0
C     GO TO (100, 200, 300, 400, 500), MITER
      IF (MITER .EQ. 1) GO TO 100
      IF (MITER .EQ. 2) GO TO 200
      IF (MITER .EQ. 3) GO TO 300
      IF (MITER .EQ. 4) GO TO 400
      IF (MITER .EQ. 5) GO TO 500
C If MITER = 1, call JAC and multiply by scalar. -----------------------
 100  LENP = N*N
      DO 110 I = 1,LENP
C110    WM(I+2) = 0.0D0
        WM(I+2) = 0.0D0
 110    CONTINUE
      CALL JAC (NEQ, TN, Y, 0, 0, WM(3), N)
      CON = -HL0
      DO 120 I = 1,LENP
C120    WM(I+2) = WM(I+2)*CON
        WM(I+2) = WM(I+2)*CON
 120    CONTINUE
      GO TO 240
C If MITER = 2, make N calls to F to approximate J. --------------------
 200  FAC = DMNORM (N, SAVF, EWT)
      R0 = 1000.0D0*ABS(H)*UROUND*N*FAC
      IF (R0 .EQ. 0.0D0) R0 = 1.0D0
      SRUR = WM(1)
      J1 = 2
      DO 230 J = 1,N
        YJ = Y(J)
        R = MAX(SRUR*ABS(YJ),R0/EWT(J))
        Y(J) = Y(J) + R
        FAC = -HL0/R
        CALL F (NEQ, TN, Y, FTEM)
        DO 220 I = 1,N
C220      WM(I+J1) = (FTEM(I) - SAVF(I))*FAC
          WM(I+J1) = (FTEM(I) - SAVF(I))*FAC
 220      CONTINUE
        Y(J) = YJ
        J1 = J1 + N
 230    CONTINUE
      NFE = NFE + N
 240  CONTINUE
C Compute norm of Jacobian. --------------------------------------------
      PDNORM = DFNORM (N, WM(3), EWT)/ABS(HL0)
C Add identity matrix. -------------------------------------------------
      J = 3
      NP1 = N + 1
      DO 250 I = 1,N
        WM(J) = WM(J) + 1.0D0
C250    J = J + NP1
        J = J + NP1
 250    CONTINUE
C Do LU decomposition on P. --------------------------------------------
      CALL DGEFA (WM(3), N, N, IWM(21), IER)
      IF (IER .NE. 0) IERPJ = 1
      RETURN
C Dummy block only, since MITER is never 3 in this routine. ------------
 300  RETURN
C If MITER = 4, call JAC and multiply by scalar. -----------------------
 400  ML = IWM(1)
      MU = IWM(2)
      ML3 = ML + 3
      MBAND = ML + MU + 1
      MEBAND = MBAND + ML
      LENP = MEBAND*N
      DO 410 I = 1,LENP
C410    WM(I+2) = 0.0D0
        WM(I+2) = 0.0D0
 410    CONTINUE
      CALL JAC (NEQ, TN, Y, ML, MU, WM(ML3), MEBAND)
      CON = -HL0
      DO 420 I = 1,LENP
C420    WM(I+2) = WM(I+2)*CON
        WM(I+2) = WM(I+2)*CON
 420    CONTINUE
      GO TO 570
C If MITER = 5, make MBAND calls to F to approximate J. ----------------
 500  ML = IWM(1)
      MU = IWM(2)
      MBAND = ML + MU + 1
      MBA = MIN(MBAND,N)
      MEBAND = MBAND + ML
      MEB1 = MEBAND - 1
      SRUR = WM(1)
      FAC = DMNORM (N, SAVF, EWT)
      R0 = 1000.0D0*ABS(H)*UROUND*N*FAC
      IF (R0 .EQ. 0.0D0) R0 = 1.0D0
      DO 560 J = 1,MBA
        DO 530 I = J,N,MBAND
          YI = Y(I)
          R = MAX(SRUR*ABS(YI),R0/EWT(I))
C530      Y(I) = Y(I) + R
          Y(I) = Y(I) + R
 530      CONTINUE
        CALL F (NEQ, TN, Y, FTEM)
        DO 550 JJ = J,N,MBAND
          Y(JJ) = YH(JJ,1)
          YJJ = Y(JJ)
          R = MAX(SRUR*ABS(YJJ),R0/EWT(JJ))
          FAC = -HL0/R
          I1 = MAX(JJ-MU,1)
          I2 = MIN(JJ+ML,N)
          II = JJ*MEB1 - ML + 2
          DO 540 I = I1,I2
C540        WM(II+I) = (FTEM(I) - SAVF(I))*FAC
            WM(II+I) = (FTEM(I) - SAVF(I))*FAC
 540        CONTINUE
 550      CONTINUE
 560    CONTINUE
      NFE = NFE + MBA
 570  CONTINUE
C Compute norm of Jacobian. --------------------------------------------
      PDNORM = DBNORM (N, WM(ML+3), MEBAND, ML, MU, EWT)/ABS(HL0)
C Add identity matrix. -------------------------------------------------
      II = MBAND + 2
      DO 580 I = 1,N
        WM(II) = WM(II) + 1.0D0
C580    II = II + MEBAND
        II = II + MEBAND
 580    CONTINUE
C Do LU decomposition of P. --------------------------------------------
      CALL DGBFA (WM(3), MEBAND, N, ML, MU, IWM(21), IER)
      IF (IER .NE. 0) IERPJ = 1
      RETURN
C----------------------- End of Subroutine DPRJA -----------------------
      END
*DECK DMNORM
      DOUBLE PRECISION FUNCTION DMNORM (N, V, W)
C-----------------------------------------------------------------------
C This function routine computes the weighted max-norm
C of the vector of length N contained in the array V, with weights
C contained in the array w of length N:
C   DMNORM = MAX(i=1,...,N) ABS(V(i))*W(i)
C-----------------------------------------------------------------------
      INTEGER N,   I
      DOUBLE PRECISION V, W,   VM
      DIMENSION V(N), W(N)
      VM = 0.0D0
      DO 10 I = 1,N
C10     VM = MAX(VM,ABS(V(I))*W(I))
        VM = MAX(VM,ABS(V(I))*W(I))
 10     CONTINUE
      DMNORM = VM
      RETURN
C----------------------- End of Function DMNORM ------------------------
      END
*DECK DFNORM
      DOUBLE PRECISION FUNCTION DFNORM (N, A, W)
C-----------------------------------------------------------------------
C This function computes the norm of a full N by N matrix,
C stored in the array A, that is consistent with the weighted max-norm
C on vectors, with weights stored in the array W:
C   DFNORM = MAX(i=1,...,N) ( W(i) * Sum(j=1,...,N) ABS(a(i,j))/W(j) )
C-----------------------------------------------------------------------
      INTEGER N,   I, J
      DOUBLE PRECISION A,   W, AN, SUM
      DIMENSION A(N,N), W(N)
      AN = 0.0D0
      DO 20 I = 1,N
        SUM = 0.0D0
        DO 10 J = 1,N
C10       SUM = SUM + ABS(A(I,J))/W(J)
          SUM = SUM + ABS(A(I,J))/W(J)
 10       CONTINUE
        AN = MAX(AN,SUM*W(I))
 20     CONTINUE
      DFNORM = AN
      RETURN
C----------------------- End of Function DFNORM ------------------------
      END
*DECK DBNORM
      DOUBLE PRECISION FUNCTION DBNORM (N, A, NRA, ML, MU, W)
C-----------------------------------------------------------------------
C This function computes the norm of a banded N by N matrix,
C stored in the array A, that is consistent with the weighted max-norm
C on vectors, with weights stored in the array W.
C ML and MU are the lower and upper half-bandwidths of the matrix.
C NRA is the first dimension of the A array, NRA .ge. ML+MU+1.
C In terms of the matrix elements a(i,j), the norm is given by:
C   DBNORM = MAX(i=1,...,N) ( W(i) * Sum(j=1,...,N) ABS(a(i,j))/W(j) )
C-----------------------------------------------------------------------
      INTEGER N, NRA, ML, MU
      INTEGER I, I1, JLO, JHI, J
      DOUBLE PRECISION A, W
      DOUBLE PRECISION AN, SUM
      DIMENSION A(NRA,N), W(N)
      AN = 0.0D0
      DO 20 I = 1,N
        SUM = 0.0D0
        I1 = I + MU + 1
        JLO = MAX(I-ML,1)
        JHI = MIN(I+MU,N)
        DO 10 J = JLO,JHI
C10       SUM = SUM + ABS(A(I1-J,J))/W(J)
          SUM = SUM + ABS(A(I1-J,J))/W(J)
 10       CONTINUE
        AN = MAX(AN,SUM*W(I))
 20     CONTINUE
      DBNORM = AN
      RETURN
C----------------------- End of Function DBNORM ------------------------
      END
*DECK DLSODA
      SUBROUTINE DLSODA (F, NEQ, Y, T, TOUT, ITOL, RTOL, ATOL, ITASK,
     1            ISTATE, IOPT, RWORK, LRW, IWORK, LIW, JAC, JT)
      EXTERNAL F, JAC
      INTEGER NEQ, ITOL, ITASK, ISTATE, IOPT, LRW, IWORK, LIW, JT
      DOUBLE PRECISION Y, T, TOUT, RTOL, ATOL, RWORK
      DIMENSION NEQ(*), Y(*), RTOL(*), ATOL(*), RWORK(LRW), IWORK(LIW)
C-----------------------------------------------------------------------
C This is the 12 November 2003 version of
C DLSODA: Livermore Solver for Ordinary Differential Equations, with
C         Automatic method switching for stiff and nonstiff problems.
C
C This version is in double precision.
C
C DLSODA solves the initial value problem for stiff or nonstiff
C systems of first order ODEs,
C     dy/dt = f(t,y) ,  or, in component form,
C     dy(i)/dt = f(i) = f(i,t,y(1),y(2),...,y(NEQ)) (i = 1,...,NEQ).
C
C This a variant version of the DLSODE package.
C It switches automatically between stiff and nonstiff methods.
C This means that the user does not have to determine whether the
C problem is stiff or not, and the solver will automatically choose the
C appropriate method.  It always starts with the nonstiff method.
C
C Authors:       Alan C. Hindmarsh
C                Center for Applied Scientific Computing, L-561
C                Lawrence Livermore National Laboratory
C                Livermore, CA 94551
C and
C                Linda R. Petzold
C                Univ. of California at Santa Barbara
C                Dept. of Computer Science
C                Santa Barbara, CA 93106
C
C References:
C 1.  Alan C. Hindmarsh,  ODEPACK, A Systematized Collection of ODE
C     Solvers, in Scientific Computing, R. S. Stepleman et al. (Eds.),
C     North-Holland, Amsterdam, 1983, pp. 55-64.
C 2.  Linda R. Petzold, Automatic Selection of Methods for Solving
C     Stiff and Nonstiff Systems of Ordinary Differential Equations,
C     Siam J. Sci. Stat. Comput. 4 (1983), pp. 136-148.
C-----------------------------------------------------------------------
C Summary of Usage.
C
C Communication between the user and the DLSODA package, for normal
C situations, is summarized here.  This summary describes only a subset
C of the full set of options available.  See the full description for
C details, including alternative treatment of the Jacobian matrix,
C optional inputs and outputs, nonstandard options, and
C instructions for special situations.  See also the example
C problem (with program and output) following this summary.
C
C A. First provide a subroutine of the form:
C               SUBROUTINE F (NEQ, T, Y, YDOT)
C               DOUBLE PRECISION T, Y(*), YDOT(*)
C which supplies the vector function f by loading YDOT(i) with f(i).
C
C B. Write a main program which calls Subroutine DLSODA once for
C each point at which answers are desired.  This should also provide
C for possible use of logical unit 6 for output of error messages
C by DLSODA.  On the first call to DLSODA, supply arguments as follows:
C F      = name of subroutine for right-hand side vector f.
C          This name must be declared External in calling program.
C NEQ    = number of first order ODEs.
C Y      = array of initial values, of length NEQ.
C T      = the initial value of the independent variable.
C TOUT   = first point where output is desired (.ne. T).
C ITOL   = 1 or 2 according as ATOL (below) is a scalar or array.
C RTOL   = relative tolerance parameter (scalar).
C ATOL   = absolute tolerance parameter (scalar or array).
C          the estimated local error in y(i) will be controlled so as
C          to be less than
C             EWT(i) = RTOL*ABS(Y(i)) + ATOL     if ITOL = 1, or
C             EWT(i) = RTOL*ABS(Y(i)) + ATOL(i)  if ITOL = 2.
C          Thus the local error test passes if, in each component,
C          either the absolute error is less than ATOL (or ATOL(i)),
C          or the relative error is less than RTOL.
C          Use RTOL = 0.0 for pure absolute error control, and
C          use ATOL = 0.0 (or ATOL(i) = 0.0) for pure relative error
C          control.  Caution: actual (global) errors may exceed these
C          local tolerances, so choose them conservatively.
C ITASK  = 1 for normal computation of output values of y at t = TOUT.
C ISTATE = integer flag (input and output).  Set ISTATE = 1.
C IOPT   = 0 to indicate no optional inputs used.
C RWORK  = real work array of length at least:
C             22 + NEQ * MAX(16, NEQ + 9).
C          See also Paragraph E below.
C LRW    = declared length of RWORK (in user's dimension).
C IWORK  = integer work array of length at least  20 + NEQ.
C LIW    = declared length of IWORK (in user's dimension).
C JAC    = name of subroutine for Jacobian matrix.
C          Use a dummy name.  See also Paragraph E below.
C JT     = Jacobian type indicator.  Set JT = 2.
C          See also Paragraph E below.
C Note that the main program must declare arrays Y, RWORK, IWORK,
C and possibly ATOL.
C
C C. The output from the first call (or any call) is:
C      Y = array of computed values of y(t) vector.
C      T = corresponding value of independent variable (normally TOUT).
C ISTATE = 2  if DLSODA was successful, negative otherwise.
C          -1 means excess work done on this call (perhaps wrong JT).
C          -2 means excess accuracy requested (tolerances too small).
C          -3 means illegal input detected (see printed message).
C          -4 means repeated error test failures (check all inputs).
C          -5 means repeated convergence failures (perhaps bad Jacobian
C             supplied or wrong choice of JT or tolerances).
C          -6 means error weight became zero during problem. (Solution
C             component i vanished, and ATOL or ATOL(i) = 0.)
C          -7 means work space insufficient to finish (see messages).
C
C D. To continue the integration after a successful return, simply
C reset TOUT and call DLSODA again.  No other parameters need be reset.
C
C E. Note: If and when DLSODA regards the problem as stiff, and
C switches methods accordingly, it must make use of the NEQ by NEQ
C Jacobian matrix, J = df/dy.  For the sake of simplicity, the
C inputs to DLSODA recommended in Paragraph B above cause DLSODA to
C treat J as a full matrix, and to approximate it internally by
C difference quotients.  Alternatively, J can be treated as a band
C matrix (with great potential reduction in the size of the RWORK
C array).  Also, in either the full or banded case, the user can supply
C J in closed form, with a routine whose name is passed as the JAC
C argument.  These alternatives are described in the paragraphs on
C RWORK, JAC, and JT in the full description of the call sequence below.
C
C-----------------------------------------------------------------------
C Example Problem.
C
C The following is a simple example problem, with the coding
C needed for its solution by DLSODA.  The problem is from chemical
C kinetics, and consists of the following three rate equations:
C     dy1/dt = -.04*y1 + 1.e4*y2*y3
C     dy2/dt = .04*y1 - 1.e4*y2*y3 - 3.e7*y2**2
C     dy3/dt = 3.e7*y2**2
C on the interval from t = 0.0 to t = 4.e10, with initial conditions
C y1 = 1.0, y2 = y3 = 0.  The problem is stiff.
C
C The following coding solves this problem with DLSODA,
C printing results at t = .4, 4., ..., 4.e10.  It uses
C ITOL = 2 and ATOL much smaller for y2 than y1 or y3 because
C y2 has much smaller values.
C At the end of the run, statistical quantities of interest are
C printed (see optional outputs in the full description below).
C
C     EXTERNAL FEX
C     DOUBLE PRECISION ATOL, RTOL, RWORK, T, TOUT, Y
C     DIMENSION Y(3), ATOL(3), RWORK(70), IWORK(23)
C     NEQ = 3
C     Y(1) = 1.
C     Y(2) = 0.
C     Y(3) = 0.
C     T = 0.
C     TOUT = .4
C     ITOL = 2
C     RTOL = 1.D-4
C     ATOL(1) = 1.D-6
C     ATOL(2) = 1.D-10
C     ATOL(3) = 1.D-6
C     ITASK = 1
C     ISTATE = 1
C     IOPT = 0
C     LRW = 70
C     LIW = 23
C     JT = 2
C     DO 40 IOUT = 1,12
C       CALL DLSODA(FEX,NEQ,Y,T,TOUT,ITOL,RTOL,ATOL,ITASK,ISTATE,
C    1     IOPT,RWORK,LRW,IWORK,LIW,JDUM,JT)
C       WRITE(6,20)T,Y(1),Y(2),Y(3)
C 20    FORMAT(' At t =',D12.4,'   Y =',3D14.6)
C       IF (ISTATE .LT. 0) GO TO 80
C 40    TOUT = TOUT*10.
C     WRITE(6,60)IWORK(11),IWORK(12),IWORK(13),IWORK(19),RWORK(15)
C 60  FORMAT(/' No. steps =',I4,'  No. f-s =',I4,'  No. J-s =',I4/
C    1   ' Method last used =',I2,'   Last switch was at t =',D12.4)
C     STOP
C 80  WRITE(6,90)ISTATE
C 90  FORMAT(///' Error halt.. ISTATE =',I3)
C     STOP
C     END
C
C     SUBROUTINE FEX (NEQ, T, Y, YDOT)
C     DOUBLE PRECISION T, Y, YDOT
C     DIMENSION Y(3), YDOT(3)
C     YDOT(1) = -.04*Y(1) + 1.D4*Y(2)*Y(3)
C     YDOT(3) = 3.D7*Y(2)*Y(2)
C     YDOT(2) = -YDOT(1) - YDOT(3)
C     RETURN
C     END
C
C The output of this program (on a CDC-7600 in single precision)
C is as follows:
C
C   At t =  4.0000e-01   y =  9.851712e-01  3.386380e-05  1.479493e-02
C   At t =  4.0000e+00   Y =  9.055333e-01  2.240655e-05  9.444430e-02
C   At t =  4.0000e+01   Y =  7.158403e-01  9.186334e-06  2.841505e-01
C   At t =  4.0000e+02   Y =  4.505250e-01  3.222964e-06  5.494717e-01
C   At t =  4.0000e+03   Y =  1.831975e-01  8.941774e-07  8.168016e-01
C   At t =  4.0000e+04   Y =  3.898730e-02  1.621940e-07  9.610125e-01
C   At t =  4.0000e+05   Y =  4.936363e-03  1.984221e-08  9.950636e-01
C   At t =  4.0000e+06   Y =  5.161831e-04  2.065786e-09  9.994838e-01
C   At t =  4.0000e+07   Y =  5.179817e-05  2.072032e-10  9.999482e-01
C   At t =  4.0000e+08   Y =  5.283401e-06  2.113371e-11  9.999947e-01
C   At t =  4.0000e+09   Y =  4.659031e-07  1.863613e-12  9.999995e-01
C   At t =  4.0000e+10   Y =  1.404280e-08  5.617126e-14  1.000000e+00
C
C   No. steps = 361  No. f-s = 693  No. J-s =  64
C   Method last used = 2   Last switch was at t =  6.0092e-03
C-----------------------------------------------------------------------
C Full description of user interface to DLSODA.
C
C The user interface to DLSODA consists of the following parts.
C
C 1.   The call sequence to Subroutine DLSODA, which is a driver
C      routine for the solver.  This includes descriptions of both
C      the call sequence arguments and of user-supplied routines.
C      following these descriptions is a description of
C      optional inputs available through the call sequence, and then
C      a description of optional outputs (in the work arrays).
C
C 2.   Descriptions of other routines in the DLSODA package that may be
C      (optionally) called by the user.  These provide the ability to
C      alter error message handling, save and restore the internal
C      Common, and obtain specified derivatives of the solution y(t).
C
C 3.   Descriptions of Common blocks to be declared in overlay
C      or similar environments, or to be saved when doing an interrupt
C      of the problem and continued solution later.
C
C 4.   Description of a subroutine in the DLSODA package,
C      which the user may replace with his/her own version, if desired.
C      this relates to the measurement of errors.
C
C-----------------------------------------------------------------------
C Part 1.  Call Sequence.
C
C The call sequence parameters used for input only are
C     F, NEQ, TOUT, ITOL, RTOL, ATOL, ITASK, IOPT, LRW, LIW, JAC, JT,
C and those used for both input and output are
C     Y, T, ISTATE.
C The work arrays RWORK and IWORK are also used for conditional and
C optional inputs and optional outputs.  (The term output here refers
C to the return from Subroutine DLSODA to the user's calling program.)
C
C The legality of input parameters will be thoroughly checked on the
C initial call for the problem, but not checked thereafter unless a
C change in input parameters is flagged by ISTATE = 3 on input.
C
C The descriptions of the call arguments are as follows.
C
C F      = the name of the user-supplied subroutine defining the
C          ODE system.  The system must be put in the first-order
C          form dy/dt = f(t,y), where f is a vector-valued function
C          of the scalar t and the vector y.  Subroutine F is to
C          compute the function f.  It is to have the form
C               SUBROUTINE F (NEQ, T, Y, YDOT)
C               DOUBLE PRECISION T, Y(*), YDOT(*)
C          where NEQ, T, and Y are input, and the array YDOT = f(t,y)
C          is output.  Y and YDOT are arrays of length NEQ.
C          Subroutine F should not alter Y(1),...,Y(NEQ).
C          F must be declared External in the calling program.
C
C          Subroutine F may access user-defined quantities in
C          NEQ(2),... and/or in Y(NEQ(1)+1),... if NEQ is an array
C          (dimensioned in F) and/or Y has length exceeding NEQ(1).
C          See the descriptions of NEQ and Y below.
C
C          If quantities computed in the F routine are needed
C          externally to DLSODA, an extra call to F should be made
C          for this purpose, for consistent and accurate results.
C          If only the derivative dy/dt is needed, use DINTDY instead.
C
C NEQ    = the size of the ODE system (number of first order
C          ordinary differential equations).  Used only for input.
C          NEQ may be decreased, but not increased, during the problem.
C          If NEQ is decreased (with ISTATE = 3 on input), the
C          remaining components of Y should be left undisturbed, if
C          these are to be accessed in F and/or JAC.
C
C          Normally, NEQ is a scalar, and it is generally referred to
C          as a scalar in this user interface description.  However,
C          NEQ may be an array, with NEQ(1) set to the system size.
C          (The DLSODA package accesses only NEQ(1).)  In either case,
C          this parameter is passed as the NEQ argument in all calls
C          to F and JAC.  Hence, if it is an array, locations
C          NEQ(2),... may be used to store other integer data and pass
C          it to F and/or JAC.  Subroutines F and/or JAC must include
C          NEQ in a Dimension statement in that case.
C
C Y      = a real array for the vector of dependent variables, of
C          length NEQ or more.  Used for both input and output on the
C          first call (ISTATE = 1), and only for output on other calls.
C          On the first call, Y must contain the vector of initial
C          values.  On output, Y contains the computed solution vector,
C          evaluated at T.  If desired, the Y array may be used
C          for other purposes between calls to the solver.
C
C          This array is passed as the Y argument in all calls to
C          F and JAC.  Hence its length may exceed NEQ, and locations
C          Y(NEQ+1),... may be used to store other real data and
C          pass it to F and/or JAC.  (The DLSODA package accesses only
C          Y(1),...,Y(NEQ).)
C
C T      = the independent variable.  On input, T is used only on the
C          first call, as the initial point of the integration.
C          on output, after each call, T is the value at which a
C          computed solution Y is evaluated (usually the same as TOUT).
C          on an error return, T is the farthest point reached.
C
C TOUT   = the next value of t at which a computed solution is desired.
C          Used only for input.
C
C          When starting the problem (ISTATE = 1), TOUT may be equal
C          to T for one call, then should .ne. T for the next call.
C          For the initial t, an input value of TOUT .ne. T is used
C          in order to determine the direction of the integration
C          (i.e. the algebraic sign of the step sizes) and the rough
C          scale of the problem.  Integration in either direction
C          (forward or backward in t) is permitted.
C
C          If ITASK = 2 or 5 (one-step modes), TOUT is ignored after
C          the first call (i.e. the first call with TOUT .ne. T).
C          Otherwise, TOUT is required on every call.
C
C          If ITASK = 1, 3, or 4, the values of TOUT need not be
C          monotone, but a value of TOUT which backs up is limited
C          to the current internal T interval, whose endpoints are
C          TCUR - HU and TCUR (see optional outputs, below, for
C          TCUR and HU).
C
C ITOL   = an indicator for the type of error control.  See
C          description below under ATOL.  Used only for input.
C
C RTOL   = a relative error tolerance parameter, either a scalar or
C          an array of length NEQ.  See description below under ATOL.
C          Input only.
C
C ATOL   = an absolute error tolerance parameter, either a scalar or
C          an array of length NEQ.  Input only.
C
C             The input parameters ITOL, RTOL, and ATOL determine
C          the error control performed by the solver.  The solver will
C          control the vector E = (E(i)) of estimated local errors
C          in y, according to an inequality of the form
C                      max-norm of ( E(i)/EWT(i) )   .le.   1,
C          where EWT = (EWT(i)) is a vector of positive error weights.
C          The values of RTOL and ATOL should all be non-negative.
C          The following table gives the types (scalar/array) of
C          RTOL and ATOL, and the corresponding form of EWT(i).
C
C             ITOL    RTOL       ATOL          EWT(i)
C              1     scalar     scalar     RTOL*ABS(Y(i)) + ATOL
C              2     scalar     array      RTOL*ABS(Y(i)) + ATOL(i)
C              3     array      scalar     RTOL(i)*ABS(Y(i)) + ATOL
C              4     array      array      RTOL(i)*ABS(Y(i)) + ATOL(i)
C
C          When either of these parameters is a scalar, it need not
C          be dimensioned in the user's calling program.
C
C          If none of the above choices (with ITOL, RTOL, and ATOL
C          fixed throughout the problem) is suitable, more general
C          error controls can be obtained by substituting a
C          user-supplied routine for the setting of EWT.
C          See Part 4 below.
C
C          If global errors are to be estimated by making a repeated
C          run on the same problem with smaller tolerances, then all
C          components of RTOL and ATOL (i.e. of EWT) should be scaled
C          down uniformly.
C
C ITASK  = an index specifying the task to be performed.
C          Input only.  ITASK has the following values and meanings.
C          1  means normal computation of output values of y(t) at
C             t = TOUT (by overshooting and interpolating).
C          2  means take one step only and return.
C          3  means stop at the first internal mesh point at or
C             beyond t = TOUT and return.
C          4  means normal computation of output values of y(t) at
C             t = TOUT but without overshooting t = TCRIT.
C             TCRIT must be input as RWORK(1).  TCRIT may be equal to
C             or beyond TOUT, but not behind it in the direction of
C             integration.  This option is useful if the problem
C             has a singularity at or beyond t = TCRIT.
C          5  means take one step, without passing TCRIT, and return.
C             TCRIT must be input as RWORK(1).
C
C          Note:  If ITASK = 4 or 5 and the solver reaches TCRIT
C          (within roundoff), it will return T = TCRIT (exactly) to
C          indicate this (unless ITASK = 4 and TOUT comes before TCRIT,
C          in which case answers at t = TOUT are returned first).
C
C ISTATE = an index used for input and output to specify the
C          the state of the calculation.
C
C          On input, the values of ISTATE are as follows.
C          1  means this is the first call for the problem
C             (initializations will be done).  See note below.
C          2  means this is not the first call, and the calculation
C             is to continue normally, with no change in any input
C             parameters except possibly TOUT and ITASK.
C             (If ITOL, RTOL, and/or ATOL are changed between calls
C             with ISTATE = 2, the new values will be used but not
C             tested for legality.)
C          3  means this is not the first call, and the
C             calculation is to continue normally, but with
C             a change in input parameters other than
C             TOUT and ITASK.  Changes are allowed in
C             NEQ, ITOL, RTOL, ATOL, IOPT, LRW, LIW, JT, ML, MU,
C             and any optional inputs except H0, MXORDN, and MXORDS.
C             (See IWORK description for ML and MU.)
C          Note:  A preliminary call with TOUT = T is not counted
C          as a first call here, as no initialization or checking of
C          input is done.  (Such a call is sometimes useful for the
C          purpose of outputting the initial conditions.)
C          Thus the first call for which TOUT .ne. T requires
C          ISTATE = 1 on input.
C
C          On output, ISTATE has the following values and meanings.
C           1  means nothing was done; TOUT = T and ISTATE = 1 on input.
C           2  means the integration was performed successfully.
C          -1  means an excessive amount of work (more than MXSTEP
C              steps) was done on this call, before completing the
C              requested task, but the integration was otherwise
C              successful as far as T.  (MXSTEP is an optional input
C              and is normally 500.)  To continue, the user may
C              simply reset ISTATE to a value .gt. 1 and call again
C              (the excess work step counter will be reset to 0).
C              In addition, the user may increase MXSTEP to avoid
C              this error return (see below on optional inputs).
C          -2  means too much accuracy was requested for the precision
C              of the machine being used.  This was detected before
C              completing the requested task, but the integration
C              was successful as far as T.  To continue, the tolerance
C              parameters must be reset, and ISTATE must be set
C              to 3.  The optional output TOLSF may be used for this
C              purpose.  (Note: If this condition is detected before
C              taking any steps, then an illegal input return
C              (ISTATE = -3) occurs instead.)
C          -3  means illegal input was detected, before taking any
C              integration steps.  See written message for details.
C              Note:  If the solver detects an infinite loop of calls
C              to the solver with illegal input, it will cause
C              the run to stop.
C          -4  means there were repeated error test failures on
C              one attempted step, before completing the requested
C              task, but the integration was successful as far as T.
C              The problem may have a singularity, or the input
C              may be inappropriate.
C          -5  means there were repeated convergence test failures on
C              one attempted step, before completing the requested
C              task, but the integration was successful as far as T.
C              This may be caused by an inaccurate Jacobian matrix,
C              if one is being used.
C          -6  means EWT(i) became zero for some i during the
C              integration.  Pure relative error control (ATOL(i)=0.0)
C              was requested on a variable which has now vanished.
C              The integration was successful as far as T.
C          -7  means the length of RWORK and/or IWORK was too small to
C              proceed, but the integration was successful as far as T.
C              This happens when DLSODA chooses to switch methods
C              but LRW and/or LIW is too small for the new method.
C
C          Note:  Since the normal output value of ISTATE is 2,
C          it does not need to be reset for normal continuation.
C          Also, since a negative input value of ISTATE will be
C          regarded as illegal, a negative output value requires the
C          user to change it, and possibly other inputs, before
C          calling the solver again.
C
C IOPT   = an integer flag to specify whether or not any optional
C          inputs are being used on this call.  Input only.
C          The optional inputs are listed separately below.
C          IOPT = 0 means no optional inputs are being used.
C                   default values will be used in all cases.
C          IOPT = 1 means one or more optional inputs are being used.
C
C RWORK  = a real array (double precision) for work space, and (in the
C          first 20 words) for conditional and optional inputs and
C          optional outputs.
C          As DLSODA switches automatically between stiff and nonstiff
C          methods, the required length of RWORK can change during the
C          problem.  Thus the RWORK array passed to DLSODA can either
C          have a static (fixed) length large enough for both methods,
C          or have a dynamic (changing) length altered by the calling
C          program in response to output from DLSODA.
C
C                       --- Fixed Length Case ---
C          If the RWORK length is to be fixed, it should be at least
C               MAX (LRN, LRS),
C          where LRN and LRS are the RWORK lengths required when the
C          current method is nonstiff or stiff, respectively.
C
C          The separate RWORK length requirements LRN and LRS are
C          as follows:
C          IF NEQ is constant and the maximum method orders have
C          their default values, then
C             LRN = 20 + 16*NEQ,
C             LRS = 22 + 9*NEQ + NEQ**2           if JT = 1 or 2,
C             LRS = 22 + 10*NEQ + (2*ML+MU)*NEQ   if JT = 4 or 5.
C          Under any other conditions, LRN and LRS are given by:
C             LRN = 20 + NYH*(MXORDN+1) + 3*NEQ,
C             LRS = 20 + NYH*(MXORDS+1) + 3*NEQ + LMAT,
C          where
C             NYH    = the initial value of NEQ,
C             MXORDN = 12, unless a smaller value is given as an
C                      optional input,
C             MXORDS = 5, unless a smaller value is given as an
C                      optional input,
C             LMAT   = length of matrix work space:
C             LMAT   = NEQ**2 + 2              if JT = 1 or 2,
C             LMAT   = (2*ML + MU + 1)*NEQ + 2 if JT = 4 or 5.
C
C                       --- Dynamic Length Case ---
C          If the length of RWORK is to be dynamic, then it should
C          be at least LRN or LRS, as defined above, depending on the
C          current method.  Initially, it must be at least LRN (since
C          DLSODA starts with the nonstiff method).  On any return
C          from DLSODA, the optional output MCUR indicates the current
C          method.  If MCUR differs from the value it had on the
C          previous return, or if there has only been one call to
C          DLSODA and MCUR is now 2, then DLSODA has switched
C          methods during the last call, and the length of RWORK
C          should be reset (to LRN if MCUR = 1, or to LRS if
C          MCUR = 2).  (An increase in the RWORK length is required
C          if DLSODA returned ISTATE = -7, but not otherwise.)
C          After resetting the length, call DLSODA with ISTATE = 3
C          to signal that change.
C
C LRW    = the length of the array RWORK, as declared by the user.
C          (This will be checked by the solver.)
C
C IWORK  = an integer array for work space.
C          As DLSODA switches automatically between stiff and nonstiff
C          methods, the required length of IWORK can change during
C          problem, between
C             LIS = 20 + NEQ   and   LIN = 20,
C          respectively.  Thus the IWORK array passed to DLSODA can
C          either have a fixed length of at least 20 + NEQ, or have a
C          dynamic length of at least LIN or LIS, depending on the
C          current method.  The comments on dynamic length under
C          RWORK above apply here.  Initially, this length need
C          only be at least LIN = 20.
C
C          The first few words of IWORK are used for conditional and
C          optional inputs and optional outputs.
C
C          The following 2 words in IWORK are conditional inputs:
C            IWORK(1) = ML     these are the lower and upper
C            IWORK(2) = MU     half-bandwidths, respectively, of the
C                       banded Jacobian, excluding the main diagonal.
C                       The band is defined by the matrix locations
C                       (i,j) with i-ML .le. j .le. i+MU.  ML and MU
C                       must satisfy  0 .le.  ML,MU  .le. NEQ-1.
C                       These are required if JT is 4 or 5, and
C                       ignored otherwise.  ML and MU may in fact be
C                       the band parameters for a matrix to which
C                       df/dy is only approximately equal.
C
C LIW    = the length of the array IWORK, as declared by the user.
C          (This will be checked by the solver.)
C
C Note: The base addresses of the work arrays must not be
C altered between calls to DLSODA for the same problem.
C The contents of the work arrays must not be altered
C between calls, except possibly for the conditional and
C optional inputs, and except for the last 3*NEQ words of RWORK.
C The latter space is used for internal scratch space, and so is
C available for use by the user outside DLSODA between calls, if
C desired (but not for use by F or JAC).
C
C JAC    = the name of the user-supplied routine to compute the
C          Jacobian matrix, df/dy, if JT = 1 or 4.  The JAC routine
C          is optional, but if the problem is expected to be stiff much
C          of the time, you are encouraged to supply JAC, for the sake
C          of efficiency.  (Alternatively, set JT = 2 or 5 to have
C          DLSODA compute df/dy internally by difference quotients.)
C          If and when DLSODA uses df/dy, it treats this NEQ by NEQ
C          matrix either as full (JT = 1 or 2), or as banded (JT =
C          4 or 5) with half-bandwidths ML and MU (discussed under
C          IWORK above).  In either case, if JT = 1 or 4, the JAC
C          routine must compute df/dy as a function of the scalar t
C          and the vector y.  It is to have the form
C               SUBROUTINE JAC (NEQ, T, Y, ML, MU, PD, NROWPD)
C               DOUBLE PRECISION T, Y(*), PD(NROWPD,*)
C          where NEQ, T, Y, ML, MU, and NROWPD are input and the array
C          PD is to be loaded with partial derivatives (elements of
C          the Jacobian matrix) on output.  PD must be given a first
C          dimension of NROWPD.  T and Y have the same meaning as in
C          Subroutine F.
C               In the full matrix case (JT = 1), ML and MU are
C          ignored, and the Jacobian is to be loaded into PD in
C          columnwise manner, with df(i)/dy(j) loaded into PD(i,j).
C               In the band matrix case (JT = 4), the elements
C          within the band are to be loaded into PD in columnwise
C          manner, with diagonal lines of df/dy loaded into the rows
C          of PD.  Thus df(i)/dy(j) is to be loaded into PD(i-j+MU+1,j).
C          ML and MU are the half-bandwidth parameters (see IWORK).
C          The locations in PD in the two triangular areas which
C          correspond to nonexistent matrix elements can be ignored
C          or loaded arbitrarily, as they are overwritten by DLSODA.
C               JAC need not provide df/dy exactly.  A crude
C          approximation (possibly with a smaller bandwidth) will do.
C               In either case, PD is preset to zero by the solver,
C          so that only the nonzero elements need be loaded by JAC.
C          Each call to JAC is preceded by a call to F with the same
C          arguments NEQ, T, and Y.  Thus to gain some efficiency,
C          intermediate quantities shared by both calculations may be
C          saved in a user Common block by F and not recomputed by JAC,
C          if desired.  Also, JAC may alter the Y array, if desired.
C          JAC must be declared External in the calling program.
C               Subroutine JAC may access user-defined quantities in
C          NEQ(2),... and/or in Y(NEQ(1)+1),... if NEQ is an array
C          (dimensioned in JAC) and/or Y has length exceeding NEQ(1).
C          See the descriptions of NEQ and Y above.
C
C JT     = Jacobian type indicator.  Used only for input.
C          JT specifies how the Jacobian matrix df/dy will be
C          treated, if and when DLSODA requires this matrix.
C          JT has the following values and meanings:
C           1 means a user-supplied full (NEQ by NEQ) Jacobian.
C           2 means an internally generated (difference quotient) full
C             Jacobian (using NEQ extra calls to F per df/dy value).
C           4 means a user-supplied banded Jacobian.
C           5 means an internally generated banded Jacobian (using
C             ML+MU+1 extra calls to F per df/dy evaluation).
C          If JT = 1 or 4, the user must supply a Subroutine JAC
C          (the name is arbitrary) as described above under JAC.
C          If JT = 2 or 5, a dummy argument can be used.
C-----------------------------------------------------------------------
C Optional Inputs.
C
C The following is a list of the optional inputs provided for in the
C call sequence.  (See also Part 2.)  For each such input variable,
C this table lists its name as used in this documentation, its
C location in the call sequence, its meaning, and the default value.
C The use of any of these inputs requires IOPT = 1, and in that
C case all of these inputs are examined.  A value of zero for any
C of these optional inputs will cause the default value to be used.
C Thus to use a subset of the optional inputs, simply preload
C locations 5 to 10 in RWORK and IWORK to 0.0 and 0 respectively, and
C then set those of interest to nonzero values.
C
C Name    Location      Meaning and Default Value
C
C H0      RWORK(5)  the step size to be attempted on the first step.
C                   The default value is determined by the solver.
C
C HMAX    RWORK(6)  the maximum absolute step size allowed.
C                   The default value is infinite.
C
C HMIN    RWORK(7)  the minimum absolute step size allowed.
C                   The default value is 0.  (This lower bound is not
C                   enforced on the final step before reaching TCRIT
C                   when ITASK = 4 or 5.)
C
C IXPR    IWORK(5)  flag to generate extra printing at method switches.
C                   IXPR = 0 means no extra printing (the default).
C                   IXPR = 1 means print data on each switch.
C                   T, H, and NST will be printed on the same logical
C                   unit as used for error messages.
C
C MXSTEP  IWORK(6)  maximum number of (internally defined) steps
C                   allowed during one call to the solver.
C                   The default value is 500.
C
C MXHNIL  IWORK(7)  maximum number of messages printed (per problem)
C                   warning that T + H = T on a step (H = step size).
C                   This must be positive to result in a non-default
C                   value.  The default value is 10.
C
C MXORDN  IWORK(8)  the maximum order to be allowed for the nonstiff
C                   (Adams) method.  the default value is 12.
C                   if MXORDN exceeds the default value, it will
C                   be reduced to the default value.
C                   MXORDN is held constant during the problem.
C
C MXORDS  IWORK(9)  the maximum order to be allowed for the stiff
C                   (BDF) method.  The default value is 5.
C                   If MXORDS exceeds the default value, it will
C                   be reduced to the default value.
C                   MXORDS is held constant during the problem.
C-----------------------------------------------------------------------
C Optional Outputs.
C
C As optional additional output from DLSODA, the variables listed
C below are quantities related to the performance of DLSODA
C which are available to the user.  These are communicated by way of
C the work arrays, but also have internal mnemonic names as shown.
C except where stated otherwise, all of these outputs are defined
C on any successful return from DLSODA, and on any return with
C ISTATE = -1, -2, -4, -5, or -6.  On an illegal input return
C (ISTATE = -3), they will be unchanged from their existing values
C (if any), except possibly for TOLSF, LENRW, and LENIW.
C On any error return, outputs relevant to the error will be defined,
C as noted below.
C
C Name    Location      Meaning
C
C HU      RWORK(11) the step size in t last used (successfully).
C
C HCUR    RWORK(12) the step size to be attempted on the next step.
C
C TCUR    RWORK(13) the current value of the independent variable
C                   which the solver has actually reached, i.e. the
C                   current internal mesh point in t.  On output, TCUR
C                   will always be at least as far as the argument
C                   T, but may be farther (if interpolation was done).
C
C TOLSF   RWORK(14) a tolerance scale factor, greater than 1.0,
C                   computed when a request for too much accuracy was
C                   detected (ISTATE = -3 if detected at the start of
C                   the problem, ISTATE = -2 otherwise).  If ITOL is
C                   left unaltered but RTOL and ATOL are uniformly
C                   scaled up by a factor of TOLSF for the next call,
C                   then the solver is deemed likely to succeed.
C                   (The user may also ignore TOLSF and alter the
C                   tolerance parameters in any other way appropriate.)
C
C TSW     RWORK(15) the value of t at the time of the last method
C                   switch, if any.
C
C NST     IWORK(11) the number of steps taken for the problem so far.
C
C NFE     IWORK(12) the number of f evaluations for the problem so far.
C
C NJE     IWORK(13) the number of Jacobian evaluations (and of matrix
C                   LU decompositions) for the problem so far.
C
C NQU     IWORK(14) the method order last used (successfully).
C
C NQCUR   IWORK(15) the order to be attempted on the next step.
C
C IMXER   IWORK(16) the index of the component of largest magnitude in
C                   the weighted local error vector ( E(i)/EWT(i) ),
C                   on an error return with ISTATE = -4 or -5.
C
C LENRW   IWORK(17) the length of RWORK actually required, assuming
C                   that the length of RWORK is to be fixed for the
C                   rest of the problem, and that switching may occur.
C                   This is defined on normal returns and on an illegal
C                   input return for insufficient storage.
C
C LENIW   IWORK(18) the length of IWORK actually required, assuming
C                   that the length of IWORK is to be fixed for the
C                   rest of the problem, and that switching may occur.
C                   This is defined on normal returns and on an illegal
C                   input return for insufficient storage.
C
C MUSED   IWORK(19) the method indicator for the last successful step:
C                   1 means Adams (nonstiff), 2 means BDF (stiff).
C
C MCUR    IWORK(20) the current method indicator:
C                   1 means Adams (nonstiff), 2 means BDF (stiff).
C                   This is the method to be attempted
C                   on the next step.  Thus it differs from MUSED
C                   only if a method switch has just been made.
C
C The following two arrays are segments of the RWORK array which
C may also be of interest to the user as optional outputs.
C For each array, the table below gives its internal name,
C its base address in RWORK, and its description.
C
C Name    Base Address      Description
C
C YH      21             the Nordsieck history array, of size NYH by
C                        (NQCUR + 1), where NYH is the initial value
C                        of NEQ.  For j = 0,1,...,NQCUR, column j+1
C                        of YH contains HCUR**j/factorial(j) times
C                        the j-th derivative of the interpolating
C                        polynomial currently representing the solution,
C                        evaluated at T = TCUR.
C
C ACOR     LACOR         array of size NEQ used for the accumulated
C         (from Common   corrections on each step, scaled on output
C           as noted)    to represent the estimated local error in y
C                        on the last step.  This is the vector E in
C                        the description of the error control.  It is
C                        defined only on a successful return from
C                        DLSODA.  The base address LACOR is obtained by
C                        including in the user's program the
C                        following 2 lines:
C                           COMMON /DLS001/ RLS(218), ILS(37)
C                           LACOR = ILS(22)
C
C-----------------------------------------------------------------------
C Part 2.  Other Routines Callable.
C
C The following are optional calls which the user may make to
C gain additional capabilities in conjunction with DLSODA.
C (The routines XSETUN and XSETF are designed to conform to the
C SLATEC error handling package.)
C
C     Form of Call                  Function
C   CALL XSETUN(LUN)          set the logical unit number, LUN, for
C                             output of messages from DLSODA, if
C                             the default is not desired.
C                             The default value of LUN is 6.
C
C   CALL XSETF(MFLAG)         set a flag to control the printing of
C                             messages by DLSODA.
C                             MFLAG = 0 means do not print. (Danger:
C                             This risks losing valuable information.)
C                             MFLAG = 1 means print (the default).
C
C                             Either of the above calls may be made at
C                             any time and will take effect immediately.
C
C   CALL DSRCMA(RSAV,ISAV,JOB) saves and restores the contents of
C                             the internal Common blocks used by
C                             DLSODA (see Part 3 below).
C                             RSAV must be a real array of length 240
C                             or more, and ISAV must be an integer
C                             array of length 46 or more.
C                             JOB=1 means save Common into RSAV/ISAV.
C                             JOB=2 means restore Common from RSAV/ISAV.
C                                DSRCMA is useful if one is
C                             interrupting a run and restarting
C                             later, or alternating between two or
C                             more problems solved with DLSODA.
C
C   CALL DINTDY(,,,,,)        provide derivatives of y, of various
C        (see below)          orders, at a specified point t, if
C                             desired.  It may be called only after
C                             a successful return from DLSODA.
C
C The detailed instructions for using DINTDY are as follows.
C The form of the call is:
C
C   CALL DINTDY (T, K, RWORK(21), NYH, DKY, IFLAG)
C
C The input parameters are:
C
C T         = value of independent variable where answers are desired
C             (normally the same as the T last returned by DLSODA).
C             For valid results, T must lie between TCUR - HU and TCUR.
C             (See optional outputs for TCUR and HU.)
C K         = integer order of the derivative desired.  K must satisfy
C             0 .le. K .le. NQCUR, where NQCUR is the current order
C             (see optional outputs).  The capability corresponding
C             to K = 0, i.e. computing y(T), is already provided
C             by DLSODA directly.  Since NQCUR .ge. 1, the first
C             derivative dy/dt is always available with DINTDY.
C RWORK(21) = the base address of the history array YH.
C NYH       = column length of YH, equal to the initial value of NEQ.
C
C The output parameters are:
C
C DKY       = a real array of length NEQ containing the computed value
C             of the K-th derivative of y(t).
C IFLAG     = integer flag, returned as 0 if K and T were legal,
C             -1 if K was illegal, and -2 if T was illegal.
C             On an error return, a message is also written.
C-----------------------------------------------------------------------
C Part 3.  Common Blocks.
C
C If DLSODA is to be used in an overlay situation, the user
C must declare, in the primary overlay, the variables in:
C   (1) the call sequence to DLSODA, and
C   (2) the two internal Common blocks
C         /DLS001/  of length  255  (218 double precision words
C                      followed by 37 integer words),
C         /DLSA01/  of length  31    (22 double precision words
C                      followed by  9 integer words).
C
C If DLSODA is used on a system in which the contents of internal
C Common blocks are not preserved between calls, the user should
C declare the above Common blocks in the calling program to insure
C that their contents are preserved.
C
C If the solution of a given problem by DLSODA is to be interrupted
C and then later continued, such as when restarting an interrupted run
C or alternating between two or more problems, the user should save,
C following the return from the last DLSODA call prior to the
C interruption, the contents of the call sequence variables and the
C internal Common blocks, and later restore these values before the
C next DLSODA call for that problem.  To save and restore the Common
C blocks, use Subroutine DSRCMA (see Part 2 above).
C
C-----------------------------------------------------------------------
C Part 4.  Optionally Replaceable Solver Routines.
C
C Below is a description of a routine in the DLSODA package which
C relates to the measurement of errors, and can be
C replaced by a user-supplied version, if desired.  However, since such
C a replacement may have a major impact on performance, it should be
C done only when absolutely necessary, and only with great caution.
C (Note: The means by which the package version of a routine is
C superseded by the user's version may be system-dependent.)
C
C (a) DEWSET.
C The following subroutine is called just before each internal
C integration step, and sets the array of error weights, EWT, as
C described under ITOL/RTOL/ATOL above:
C     Subroutine DEWSET (NEQ, ITOL, RTOL, ATOL, YCUR, EWT)
C where NEQ, ITOL, RTOL, and ATOL are as in the DLSODA call sequence,
C YCUR contains the current dependent variable vector, and
C EWT is the array of weights set by DEWSET.
C
C If the user supplies this subroutine, it must return in EWT(i)
C (i = 1,...,NEQ) a positive quantity suitable for comparing errors
C in y(i) to.  The EWT array returned by DEWSET is passed to the
C DMNORM routine, and also used by DLSODA in the computation
C of the optional output IMXER, and the increments for difference
C quotient Jacobians.
C
C In the user-supplied version of DEWSET, it may be desirable to use
C the current values of derivatives of y.  Derivatives up to order NQ
C are available from the history array YH, described above under
C optional outputs.  In DEWSET, YH is identical to the YCUR array,
C extended to NQ + 1 columns with a column length of NYH and scale
C factors of H**j/factorial(j).  On the first call for the problem,
C given by NST = 0, NQ is 1 and H is temporarily set to 1.0.
C NYH is the initial value of NEQ.  The quantities NQ, H, and NST
C can be obtained by including in DEWSET the statements:
C     DOUBLE PRECISION RLS
C     COMMON /DLS001/ RLS(218),ILS(37)
C     NQ = ILS(33)
C     NST = ILS(34)
C     H = RLS(212)
C Thus, for example, the current value of dy/dt can be obtained as
C YCUR(NYH+i)/H  (i=1,...,NEQ)  (and the division by H is
C unnecessary when NST = 0).
C-----------------------------------------------------------------------
C
C***REVISION HISTORY  (YYYYMMDD)
C 19811102  DATE WRITTEN
C 19820126  Fixed bug in tests of work space lengths;
C           minor corrections in main prologue and comments.
C 19870330  Major update: corrected comments throughout;
C           removed TRET from Common; rewrote EWSET with 4 loops;
C           fixed t test in INTDY; added Cray directives in STODA;
C           in STODA, fixed DELP init. and logic around PJAC call;
C           combined routines to save/restore Common;
C           passed LEVEL = 0 in error message calls (except run abort).
C 19970225  Fixed lines setting JSTART = -2 in Subroutine LSODA.
C 20010425  Major update: convert source lines to upper case;
C           added *DECK lines; changed from 1 to * in dummy dimensions;
C           changed names R1MACH/D1MACH to RUMACH/DUMACH;
C           renamed routines for uniqueness across single/double prec.;
C           converted intrinsic names to generic form;
C           removed ILLIN and NTREP (data loaded) from Common;
C           removed all 'own' variables from Common;
C           changed error messages to quoted strings;
C           replaced XERRWV/XERRWD with 1993 revised version;
C           converted prologues, comments, error messages to mixed case;
C           numerous corrections to prologues and internal comments.
C 20010507  Converted single precision source to double precision.
C 20010613  Revised excess accuracy test (to match rest of ODEPACK).
C 20010808  Fixed bug in DPRJA (matrix in DBNORM call).
C 20020502  Corrected declarations in descriptions of user routines.
C 20031105  Restored 'own' variables to Common blocks, to enable
C           interrupt/restart feature.
C 20031112  Added SAVE statements for data-loaded constants.
C
C-----------------------------------------------------------------------
C Other routines in the DLSODA package.
C
C In addition to Subroutine DLSODA, the DLSODA package includes the
C following subroutines and function routines:
C  DINTDY   computes an interpolated value of the y vector at t = TOUT.
C  DSTODA   is the core integrator, which does one step of the
C           integration and the associated error control.
C  DCFODE   sets all method coefficients and test constants.
C  DPRJA    computes and preprocesses the Jacobian matrix J = df/dy
C           and the Newton iteration matrix P = I - h*l0*J.
C  DSOLSY   manages solution of linear system in chord iteration.
C  DEWSET   sets the error weight vector EWT before each step.
C  DMNORM   computes the weighted max-norm of a vector.
C  DFNORM   computes the norm of a full matrix consistent with the
C           weighted max-norm on vectors.
C  DBNORM   computes the norm of a band matrix consistent with the
C           weighted max-norm on vectors.
C  DSRCMA   is a user-callable routine to save and restore
C           the contents of the internal Common blocks.
C  DGEFA and DGESL   are routines from LINPACK for solving full
C           systems of linear algebraic equations.
C  DGBFA and DGBSL   are routines from LINPACK for solving banded
C           linear systems.
C  DUMACH   computes the unit roundoff in a machine-independent manner.
C  XERRWD, XSETUN, XSETF, IXSAV, and IUMACH  handle the printing of all
C           error messages and warnings.  XERRWD is machine-dependent.
C Note:  DMNORM, DFNORM, DBNORM, DUMACH, IXSAV, and IUMACH are
C function routines.  All the others are subroutines.
C
C-----------------------------------------------------------------------
      EXTERNAL DPRJA, DSOLSY
      DOUBLE PRECISION DUMACH, DMNORM
      INTEGER INIT, MXSTEP, MXHNIL, NHNIL, NSLAST, NYH, IOWNS,
     1   ICF, IERPJ, IERSL, JCUR, JSTART, KFLAG, L,
     2   LYH, LEWT, LACOR, LSAVF, LWM, LIWM, METH, MITER,
     3   MAXORD, MAXCOR, MSBP, MXNCF, N, NQ, NST, NFE, NJE, NQU
      INTEGER INSUFR, INSUFI, IXPR, IOWNS2, JTYP, MUSED, MXORDN, MXORDS
      INTEGER I, I1, I2, IFLAG, IMXER, KGO, LF0,
     1   LENIW, LENRW, LENWM, ML, MORD, MU, MXHNL0, MXSTP0
      INTEGER LEN1, LEN1C, LEN1N, LEN1S, LEN2, LENIWC, LENRWC
      DOUBLE PRECISION ROWNS,
     1   CCMAX, EL0, H, HMIN, HMXI, HU, RC, TN, UROUND
      DOUBLE PRECISION TSW, ROWNS2, PDNORM
      DOUBLE PRECISION ATOLI, AYI, BIG, EWTI, H0, HMAX, HMX, RH, RTOLI,
     1   TCRIT, TDIST, TNEXT, TOL, TOLSF, TP, SIZE, SUM, W0
      DIMENSION MORD(2)
      LOGICAL IHIT
      CHARACTER (LEN=60) MSG
      SAVE MORD, MXSTP0, MXHNL0
C-----------------------------------------------------------------------
C The following two internal Common blocks contain
C (a) variables which are local to any subroutine but whose values must
C     be preserved between calls to the routine ("own" variables), and
C (b) variables which are communicated between subroutines.
C The block DLS001 is declared in subroutines DLSODA, DINTDY, DSTODA,
C DPRJA, and DSOLSY.
C The block DLSA01 is declared in subroutines DLSODA, DSTODA, and DPRJA.
C Groups of variables are replaced by dummy arrays in the Common
C declarations in routines where those variables are not used.
C-----------------------------------------------------------------------
      COMMON /DLS001/ ROWNS(209),
     1   CCMAX, EL0, H, HMIN, HMXI, HU, RC, TN, UROUND,
     2   INIT, MXSTEP, MXHNIL, NHNIL, NSLAST, NYH, IOWNS(6),
     3   ICF, IERPJ, IERSL, JCUR, JSTART, KFLAG, L,
     4   LYH, LEWT, LACOR, LSAVF, LWM, LIWM, METH, MITER,
     5   MAXORD, MAXCOR, MSBP, MXNCF, N, NQ, NST, NFE, NJE, NQU
C
      COMMON /DLSA01/ TSW, ROWNS2(20), PDNORM,
     1   INSUFR, INSUFI, IXPR, IOWNS2(2), JTYP, MUSED, MXORDN, MXORDS
C
      DATA MORD(1),MORD(2)/12,5/, MXSTP0/500/, MXHNL0/10/
C-----------------------------------------------------------------------
C Block A.
C This code block is executed on every call.
C It tests ISTATE and ITASK for legality and branches appropriately.
C If ISTATE .gt. 1 but the flag INIT shows that initialization has
C not yet been done, an error return occurs.
C If ISTATE = 1 and TOUT = T, return immediately.
C-----------------------------------------------------------------------
      IF (ISTATE .LT. 1 .OR. ISTATE .GT. 3) GO TO 601
      IF (ITASK .LT. 1 .OR. ITASK .GT. 5) GO TO 602
      IF (ISTATE .EQ. 1) GO TO 10
      IF (INIT .EQ. 0) GO TO 603
      IF (ISTATE .EQ. 2) GO TO 200
      GO TO 20
 10   INIT = 0
      IF (TOUT .EQ. T) RETURN
C-----------------------------------------------------------------------
C Block B.
C The next code block is executed for the initial call (ISTATE = 1),
C or for a continuation call with parameter changes (ISTATE = 3).
C It contains checking of all inputs and various initializations.
C
C First check legality of the non-optional inputs NEQ, ITOL, IOPT,
C JT, ML, and MU.
C-----------------------------------------------------------------------
 20   IF (NEQ(1) .LE. 0) GO TO 604
      IF (ISTATE .EQ. 1) GO TO 25
      IF (NEQ(1) .GT. N) GO TO 605
 25   N = NEQ(1)
      IF (ITOL .LT. 1 .OR. ITOL .GT. 4) GO TO 606
      IF (IOPT .LT. 0 .OR. IOPT .GT. 1) GO TO 607
      IF (JT .EQ. 3 .OR. JT .LT. 1 .OR. JT .GT. 5) GO TO 608
      JTYP = JT
      IF (JT .LE. 2) GO TO 30
      ML = IWORK(1)
      MU = IWORK(2)
      IF (ML .LT. 0 .OR. ML .GE. N) GO TO 609
      IF (MU .LT. 0 .OR. MU .GE. N) GO TO 610
 30   CONTINUE
C Next process and check the optional inputs. --------------------------
      IF (IOPT .EQ. 1) GO TO 40
      IXPR = 0
      MXSTEP = MXSTP0
      MXHNIL = MXHNL0
      HMXI = 0.0D0
      HMIN = 0.0D0
      IF (ISTATE .NE. 1) GO TO 60
      H0 = 0.0D0
      MXORDN = MORD(1)
      MXORDS = MORD(2)
      GO TO 60
 40   IXPR = IWORK(5)
      IF (IXPR .LT. 0 .OR. IXPR .GT. 1) GO TO 611
      MXSTEP = IWORK(6)
      IF (MXSTEP .LT. 0) GO TO 612
      IF (MXSTEP .EQ. 0) MXSTEP = MXSTP0
      MXHNIL = IWORK(7)
      IF (MXHNIL .LT. 0) GO TO 613
      IF (MXHNIL .EQ. 0) MXHNIL = MXHNL0
      IF (ISTATE .NE. 1) GO TO 50
      H0 = RWORK(5)
      MXORDN = IWORK(8)
      IF (MXORDN .LT. 0) GO TO 628
      IF (MXORDN .EQ. 0) MXORDN = 100
      MXORDN = MIN(MXORDN,MORD(1))
      MXORDS = IWORK(9)
      IF (MXORDS .LT. 0) GO TO 629
      IF (MXORDS .EQ. 0) MXORDS = 100
      MXORDS = MIN(MXORDS,MORD(2))
      IF ((TOUT - T)*H0 .LT. 0.0D0) GO TO 614
 50   HMAX = RWORK(6)
      IF (HMAX .LT. 0.0D0) GO TO 615
      HMXI = 0.0D0
      IF (HMAX .GT. 0.0D0) HMXI = 1.0D0/HMAX
      HMIN = RWORK(7)
      IF (HMIN .LT. 0.0D0) GO TO 616
C-----------------------------------------------------------------------
C Set work array pointers and check lengths LRW and LIW.
C If ISTATE = 1, METH is initialized to 1 here to facilitate the
C checking of work space lengths.
C Pointers to segments of RWORK and IWORK are named by prefixing L to
C the name of the segment.  E.g., the segment YH starts at RWORK(LYH).
C Segments of RWORK (in order) are denoted  YH, WM, EWT, SAVF, ACOR.
C If the lengths provided are insufficient for the current method,
C an error return occurs.  This is treated as illegal input on the
C first call, but as a problem interruption with ISTATE = -7 on a
C continuation call.  If the lengths are sufficient for the current
C method but not for both methods, a warning message is sent.
C-----------------------------------------------------------------------
 60   IF (ISTATE .EQ. 1) METH = 1
      IF (ISTATE .EQ. 1) NYH = N
      LYH = 21
      LEN1N = 20 + (MXORDN + 1)*NYH
      LEN1S = 20 + (MXORDS + 1)*NYH
      LWM = LEN1S + 1
      IF (JT .LE. 2) LENWM = N*N + 2
      IF (JT .GE. 4) LENWM = (2*ML + MU + 1)*N + 2
      LEN1S = LEN1S + LENWM
      LEN1C = LEN1N
      IF (METH .EQ. 2) LEN1C = LEN1S
      LEN1 = MAX(LEN1N,LEN1S)
      LEN2 = 3*N
      LENRW = LEN1 + LEN2
      LENRWC = LEN1C + LEN2
      IWORK(17) = LENRW
      LIWM = 1
      LENIW = 20 + N
      LENIWC = 20
      IF (METH .EQ. 2) LENIWC = LENIW
      IWORK(18) = LENIW
      IF (ISTATE .EQ. 1 .AND. LRW .LT. LENRWC) GO TO 617
      IF (ISTATE .EQ. 1 .AND. LIW .LT. LENIWC) GO TO 618
      IF (ISTATE .EQ. 3 .AND. LRW .LT. LENRWC) GO TO 550
      IF (ISTATE .EQ. 3 .AND. LIW .LT. LENIWC) GO TO 555
      LEWT = LEN1 + 1
      INSUFR = 0
      IF (LRW .GE. LENRW) GO TO 65
      INSUFR = 2
      LEWT = LEN1C + 1
c     MSG='DLSODA-  Warning.. RWORK length is sufficient for now, but  '
c     CALL XERRWD (MSG, 60, 103, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
c     MSG='      may not be later.  Integration will proceed anyway.   '
c     CALL XERRWD (MSG, 60, 103, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
c     MSG = '      Length needed is LENRW = I1, while LRW = I2.'
c     CALL XERRWD (MSG, 50, 103, 0, 2, LENRW, LRW, 0, 0.0D0, 0.0D0)
      CALL XERRWD (103, 0)
 65   LSAVF = LEWT + N
      LACOR = LSAVF + N
      INSUFI = 0
      IF (LIW .GE. LENIW) GO TO 70
      INSUFI = 2
c     MSG='DLSODA-  Warning.. IWORK length is sufficient for now, but  '
c     CALL XERRWD (MSG, 60, 104, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
c     MSG='      may not be later.  Integration will proceed anyway.   '
c     CALL XERRWD (MSG, 60, 104, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
c     MSG = '      Length needed is LENIW = I1, while LIW = I2.'
c     CALL XERRWD (MSG, 50, 104, 0, 2, LENIW, LIW, 0, 0.0D0, 0.0D0)
      CALL XERRWD (104, 0)
 70   CONTINUE
C Check RTOL and ATOL for legality. ------------------------------------
      RTOLI = RTOL(1)
      ATOLI = ATOL(1)
      DO 75 I = 1,N
        IF (ITOL .GE. 3) RTOLI = RTOL(I)
        IF (ITOL .EQ. 2 .OR. ITOL .EQ. 4) ATOLI = ATOL(I)
        IF (RTOLI .LT. 0.0D0) GO TO 619
        IF (ATOLI .LT. 0.0D0) GO TO 620
 75     CONTINUE
      IF (ISTATE .EQ. 1) GO TO 100
C If ISTATE = 3, set flag to signal parameter changes to DSTODA. -------
      JSTART = -1
      IF (N .EQ. NYH) GO TO 200
C NEQ was reduced.  Zero part of YH to avoid undefined references. -----
      I1 = LYH + L*NYH
      I2 = LYH + (MAXORD + 1)*NYH - 1
      IF (I1 .GT. I2) GO TO 200
      DO 95 I = I1,I2
C95     RWORK(I) = 0.0D0
        RWORK(I) = 0.0D0
 95     CONTINUE
      GO TO 200
C-----------------------------------------------------------------------
C Block C.
C The next block is for the initial call only (ISTATE = 1).
C It contains all remaining initializations, the initial call to F,
C and the calculation of the initial step size.
C The error weights in EWT are inverted after being loaded.
C-----------------------------------------------------------------------
 100  UROUND = DUMACH()
      TN = T
      TSW = T
      MAXORD = MXORDN
      IF (ITASK .NE. 4 .AND. ITASK .NE. 5) GO TO 110
      TCRIT = RWORK(1)
      IF ((TCRIT - TOUT)*(TOUT - T) .LT. 0.0D0) GO TO 625
      IF (H0 .NE. 0.0D0 .AND. (T + H0 - TCRIT)*H0 .GT. 0.0D0)
     1   H0 = TCRIT - T
 110  JSTART = 0
      NHNIL = 0
      NST = 0
      NJE = 0
      NSLAST = 0
      HU = 0.0D0
      NQU = 0
      MUSED = 0
      MITER = 0
      CCMAX = 0.3D0
      MAXCOR = 3
      MSBP = 20
      MXNCF = 10
C Initial call to F.  (LF0 points to YH(*,2).) -------------------------
      LF0 = LYH + NYH
      CALL F (NEQ, T, Y, RWORK(LF0))
      NFE = 1
C Load the initial value vector in YH. ---------------------------------
      DO 115 I = 1,N
C115    RWORK(I+LYH-1) = Y(I)
        RWORK(I+LYH-1) = Y(I)
 115    CONTINUE
C Load and invert the EWT array.  (H is temporarily set to 1.0.) -------
      NQ = 1
      H = 1.0D0
      CALL DEWSET (N, ITOL, RTOL, ATOL, RWORK(LYH), RWORK(LEWT))
      DO 120 I = 1,N
        IF (RWORK(I+LEWT-1) .LE. 0.0D0) GO TO 621
C120    RWORK(I+LEWT-1) = 1.0D0/RWORK(I+LEWT-1)
        RWORK(I+LEWT-1) = 1.0D0/RWORK(I+LEWT-1)
 120    CONTINUE
C-----------------------------------------------------------------------
C The coding below computes the step size, H0, to be attempted on the
C first step, unless the user has supplied a value for this.
C First check that TOUT - T differs significantly from zero.
C A scalar tolerance quantity TOL is computed, as MAX(RTOL(i))
C if this is positive, or MAX(ATOL(i)/ABS(Y(i))) otherwise, adjusted
C so as to be between 100*UROUND and 1.0E-3.
C Then the computed value H0 is given by:
C
C   H0**(-2)  =  1./(TOL * w0**2)  +  TOL * (norm(F))**2
C
C where   w0     = MAX ( ABS(T), ABS(TOUT) ),
C         F      = the initial value of the vector f(t,y), and
C         norm() = the weighted vector norm used throughout, given by
C                  the DMNORM function routine, and weighted by the
C                  tolerances initially loaded into the EWT array.
C The sign of H0 is inferred from the initial values of TOUT and T.
C ABS(H0) is made .le. ABS(TOUT-T) in any case.
C-----------------------------------------------------------------------
      IF (H0 .NE. 0.0D0) GO TO 180
      TDIST = ABS(TOUT - T)
      W0 = MAX(ABS(T),ABS(TOUT))
      IF (TDIST .LT. 2.0D0*UROUND*W0) GO TO 622
      TOL = RTOL(1)
      IF (ITOL .LE. 2) GO TO 140
      DO 130 I = 1,N
C130    TOL = MAX(TOL,RTOL(I))
        TOL = MAX(TOL,RTOL(I))
 130    CONTINUE
 140  IF (TOL .GT. 0.0D0) GO TO 160
      ATOLI = ATOL(1)
      DO 150 I = 1,N
        IF (ITOL .EQ. 2 .OR. ITOL .EQ. 4) ATOLI = ATOL(I)
        AYI = ABS(Y(I))
        IF (AYI .NE. 0.0D0) TOL = MAX(TOL,ATOLI/AYI)
 150    CONTINUE
 160  TOL = MAX(TOL,100.0D0*UROUND)
      TOL = MIN(TOL,0.001D0)
      SUM = DMNORM (N, RWORK(LF0), RWORK(LEWT))
      SUM = 1.0D0/(TOL*W0*W0) + TOL*SUM**2
      H0 = 1.0D0/SQRT(SUM)
      H0 = MIN(H0,TDIST)
      H0 = SIGN(H0,TOUT-T)
C Adjust H0 if necessary to meet HMAX bound. ---------------------------
 180  RH = ABS(H0)*HMXI
      IF (RH .GT. 1.0D0) H0 = H0/RH
C Load H with H0 and scale YH(*,2) by H0. ------------------------------
      H = H0
      DO 190 I = 1,N
C190    RWORK(I+LF0-1) = H0*RWORK(I+LF0-1)
        RWORK(I+LF0-1) = H0*RWORK(I+LF0-1)
 190    CONTINUE
      GO TO 270
C-----------------------------------------------------------------------
C Block D.
C The next code block is for continuation calls only (ISTATE = 2 or 3)
C and is to check stop conditions before taking a step.
C-----------------------------------------------------------------------
 200  NSLAST = NST
C     GO TO (210, 250, 220, 230, 240), ITASK
      IF (ITASK .EQ. 1) GO TO 210
      IF (ITASK .EQ. 2) GO TO 250
      IF (ITASK .EQ. 3) GO TO 220
      IF (ITASK .EQ. 4) GO TO 230
      IF (ITASK .EQ. 5) GO TO 240

 210  IF ((TN - TOUT)*H .LT. 0.0D0) GO TO 250
      CALL DINTDY (TOUT, 0, RWORK(LYH), NYH, Y, IFLAG)
      IF (IFLAG .NE. 0) GO TO 627
      T = TOUT
      GO TO 420
 220  TP = TN - HU*(1.0D0 + 100.0D0*UROUND)
      IF ((TP - TOUT)*H .GT. 0.0D0) GO TO 623
      IF ((TN - TOUT)*H .LT. 0.0D0) GO TO 250
      T = TN
      GO TO 400
 230  TCRIT = RWORK(1)
      IF ((TN - TCRIT)*H .GT. 0.0D0) GO TO 624
      IF ((TCRIT - TOUT)*H .LT. 0.0D0) GO TO 625
      IF ((TN - TOUT)*H .LT. 0.0D0) GO TO 245
      CALL DINTDY (TOUT, 0, RWORK(LYH), NYH, Y, IFLAG)
      IF (IFLAG .NE. 0) GO TO 627
      T = TOUT
      GO TO 420
 240  TCRIT = RWORK(1)
      IF ((TN - TCRIT)*H .GT. 0.0D0) GO TO 624
 245  HMX = ABS(TN) + ABS(H)
      IHIT = ABS(TN - TCRIT) .LE. 100.0D0*UROUND*HMX
      IF (IHIT) T = TCRIT
      IF (IHIT) GO TO 400
      TNEXT = TN + H*(1.0D0 + 4.0D0*UROUND)
      IF ((TNEXT - TCRIT)*H .LE. 0.0D0) GO TO 250
      H = (TCRIT - TN)*(1.0D0 - 4.0D0*UROUND)
      IF (ISTATE .EQ. 2 .AND. JSTART .GE. 0) JSTART = -2
C-----------------------------------------------------------------------
C Block E.
C The next block is normally executed for all calls and contains
C the call to the one-step core integrator DSTODA.
C
C This is a looping point for the integration steps.
C
C First check for too many steps being taken, update EWT (if not at
C start of problem), check for too much accuracy being requested, and
C check for H below the roundoff level in T.
C-----------------------------------------------------------------------
 250  CONTINUE
      IF (METH .EQ. MUSED) GO TO 255
      IF (INSUFR .EQ. 1) GO TO 550
      IF (INSUFI .EQ. 1) GO TO 555
 255  IF ((NST-NSLAST) .GE. MXSTEP) GO TO 500
      CALL DEWSET (N, ITOL, RTOL, ATOL, RWORK(LYH), RWORK(LEWT))
      DO 260 I = 1,N
        IF (RWORK(I+LEWT-1) .LE. 0.0D0) GO TO 510
C260    RWORK(I+LEWT-1) = 1.0D0/RWORK(I+LEWT-1)
        RWORK(I+LEWT-1) = 1.0D0/RWORK(I+LEWT-1)
 260    CONTINUE
 270  TOLSF = UROUND*DMNORM (N, RWORK(LYH), RWORK(LEWT))
      IF (TOLSF .LE. 1.0D0) GO TO 280
      TOLSF = TOLSF*2.0D0
      IF (NST .EQ. 0) GO TO 626
      GO TO 520
 280  IF ((TN + H) .NE. TN) GO TO 290
      NHNIL = NHNIL + 1
      IF (NHNIL .GT. MXHNIL) GO TO 290
c     MSG = 'DLSODA-  Warning..Internal T (=R1) and H (=R2) are'
c     CALL XERRWD (MSG, 60, 101, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
c     MSG='      such that in the machine, T + H = T on the next step  '
c     CALL XERRWD (MSG, 60, 101, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
c     MSG = '     (H = step size). Solver will continue anyway.'
c     CALL XERRWD (MSG, 50, 101, 0, 0, 0, 0, 2, TN, H)
      CALL XERRWD (101, 0)
      IF (NHNIL .LT. MXHNIL) GO TO 290
c     MSG = 'DLSODA-  Above warning has been issued I1 times.  '
c     CALL XERRWD (MSG, 50, 102, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
c     MSG = '     It will not be issued again for this problem.'
c     CALL XERRWD (MSG, 50, 102, 0, 1, MXHNIL, 0, 0, 0.0D0, 0.0D0)
      CALL XERRWD (102, 0)
 290  CONTINUE
C-----------------------------------------------------------------------
C   CALL DSTODA(NEQ,Y,YH,NYH,YH,EWT,SAVF,ACOR,WM,IWM,F,JAC,DPRJA,DSOLSY)
C-----------------------------------------------------------------------
      CALL DSTODA (NEQ, Y, RWORK(LYH), NYH, RWORK(LYH), RWORK(LEWT),
     1   RWORK(LSAVF), RWORK(LACOR), RWORK(LWM), IWORK(LIWM),
     2   F, JAC, DPRJA, DSOLSY)
      KGO = 1 - KFLAG
C     GO TO (300, 530, 540), KGO
      IF (KGO .EQ. 1) GO TO 300
      IF (KGO .EQ. 2) GO TO 530
      IF (KGO .EQ. 3) GO TO 540

C-----------------------------------------------------------------------
C Block F.
C The following block handles the case of a successful return from the
C core integrator (KFLAG = 0).
C If a method switch was just made, record TSW, reset MAXORD,
C set JSTART to -1 to signal DSTODA to complete the switch,
C and do extra printing of data if IXPR = 1.
C Then, in any case, check for stop conditions.
C-----------------------------------------------------------------------
 300  INIT = 1
      IF (METH .EQ. MUSED) GO TO 310
      TSW = TN
      MAXORD = MXORDN
      IF (METH .EQ. 2) MAXORD = MXORDS
      IF (METH .EQ. 2) RWORK(LWM) = SQRT(UROUND)
      INSUFR = MIN(INSUFR,1)
      INSUFI = MIN(INSUFI,1)
      JSTART = -1
      IF (IXPR .EQ. 0) GO TO 310
      IF (METH .EQ. 2) THEN
c     MSG='DLSODA- A switch to the BDF (stiff) method has occurred     '
c     CALL XERRWD (MSG, 60, 105, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
      CALL XERRWD (105, 0)
      ENDIF
      IF (METH .EQ. 1) THEN
c     MSG='DLSODA- A switch to the Adams (nonstiff) method has occurred'
c     CALL XERRWD (MSG, 60, 106, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
      CALL XERRWD (106, 0)
      ENDIF
c     MSG='     at T = R1,  tentative step size H = R2,  step NST = I1 '
c     CALL XERRWD (MSG, 60, 107, 0, 1, NST, 0, 2, TN, H)
      CALL XERRWD (107, 0)
C310  GO TO (320, 400, 330, 340, 350), ITASK
 310  IF (ITASK .EQ. 1) GO TO 320
      IF (ITASK .EQ. 2) GO TO 400
      IF (ITASK .EQ. 3) GO TO 330
      IF (ITASK .EQ. 4) GO TO 340
      IF (ITASK .EQ. 5) GO TO 350

C ITASK = 1.  If TOUT has been reached, interpolate. -------------------
 320  IF ((TN - TOUT)*H .LT. 0.0D0) GO TO 250
      CALL DINTDY (TOUT, 0, RWORK(LYH), NYH, Y, IFLAG)
      T = TOUT
      GO TO 420
C ITASK = 3.  Jump to exit if TOUT was reached. ------------------------
 330  IF ((TN - TOUT)*H .GE. 0.0D0) GO TO 400
      GO TO 250
C ITASK = 4.  See if TOUT or TCRIT was reached.  Adjust H if necessary.
 340  IF ((TN - TOUT)*H .LT. 0.0D0) GO TO 345
      CALL DINTDY (TOUT, 0, RWORK(LYH), NYH, Y, IFLAG)
      T = TOUT
      GO TO 420
 345  HMX = ABS(TN) + ABS(H)
      IHIT = ABS(TN - TCRIT) .LE. 100.0D0*UROUND*HMX
      IF (IHIT) GO TO 400
      TNEXT = TN + H*(1.0D0 + 4.0D0*UROUND)
      IF ((TNEXT - TCRIT)*H .LE. 0.0D0) GO TO 250
      H = (TCRIT - TN)*(1.0D0 - 4.0D0*UROUND)
      IF (JSTART .GE. 0) JSTART = -2
      GO TO 250
C ITASK = 5.  See if TCRIT was reached and jump to exit. ---------------
 350  HMX = ABS(TN) + ABS(H)
      IHIT = ABS(TN - TCRIT) .LE. 100.0D0*UROUND*HMX
C-----------------------------------------------------------------------
C Block G.
C The following block handles all successful returns from DLSODA.
C If ITASK .ne. 1, Y is loaded from YH and T is set accordingly.
C ISTATE is set to 2, and the optional outputs are loaded into the
C work arrays before returning.
C-----------------------------------------------------------------------
 400  DO 410 I = 1,N
C410    Y(I) = RWORK(I+LYH-1)
        Y(I) = RWORK(I+LYH-1)
 410    CONTINUE
      T = TN
      IF (ITASK .NE. 4 .AND. ITASK .NE. 5) GO TO 420
      IF (IHIT) T = TCRIT
 420  ISTATE = 2
      RWORK(11) = HU
      RWORK(12) = H
      RWORK(13) = TN
      RWORK(15) = TSW
      IWORK(11) = NST
      IWORK(12) = NFE
      IWORK(13) = NJE
      IWORK(14) = NQU
      IWORK(15) = NQ
      IWORK(19) = MUSED
      IWORK(20) = METH
      RETURN
C-----------------------------------------------------------------------
C Block H.
C The following block handles all unsuccessful returns other than
C those for illegal input.  First the error message routine is called.
C If there was an error test or convergence test failure, IMXER is set.
C Then Y is loaded from YH and T is set to TN.
C The optional outputs are loaded into the work arrays before returning.
C-----------------------------------------------------------------------
C The maximum number of steps was taken before reaching TOUT. ----------
 500  MSG = 'DLSODA-  At current T (=R1), MXSTEP (=I1) steps   '
c     CALL XERRWD (MSG, 50, 201, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
c     MSG = '      taken on this call before reaching TOUT     '
c     CALL XERRWD (MSG, 50, 201, 0, 1, MXSTEP, 0, 1, TN, 0.0D0)
      CALL XERRWD (201, 0)
      ISTATE = -1
      GO TO 580
C EWT(i) .le. 0.0 for some i (not at start of problem). ----------------
 510  EWTI = RWORK(LEWT+I-1)
c     MSG = 'DLSODA-  At T (=R1), EWT(I1) has become R2 .le. 0.'
c     CALL XERRWD (MSG, 50, 202, 0, 1, I, 0, 2, TN, EWTI)
      CALL XERRWD (202, 0)
      ISTATE = -6
      GO TO 580
C Too much accuracy requested for machine precision. -------------------
 520  MSG = 'DLSODA-  At T (=R1), too much accuracy requested  '
c     CALL XERRWD (MSG, 50, 203, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
c     MSG = '      for precision of machine..  See TOLSF (=R2) '
c     CALL XERRWD (MSG, 50, 203, 0, 0, 0, 0, 2, TN, TOLSF)
      CALL XERRWD (203, 0)
      RWORK(14) = TOLSF
      ISTATE = -2
      GO TO 580
C KFLAG = -1.  Error test failed repeatedly or with ABS(H) = HMIN. -----
 530  MSG = 'DLSODA-  At T(=R1) and step size H(=R2), the error'
c     CALL XERRWD (MSG, 50, 204, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
c     MSG = '      test failed repeatedly or with ABS(H) = HMIN'
c     CALL XERRWD (MSG, 50, 204, 0, 0, 0, 0, 2, TN, H)
      CALL XERRWD (204, 0)
      ISTATE = -4
      GO TO 560
C KFLAG = -2.  Convergence failed repeatedly or with ABS(H) = HMIN. ----
 540  MSG = 'DLSODA-  At T (=R1) and step size H (=R2), the    '
c     CALL XERRWD (MSG, 50, 205, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
c     MSG = '      corrector convergence failed repeatedly     '
c     CALL XERRWD (MSG, 50, 205, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
c     MSG = '      or with ABS(H) = HMIN   '
c     CALL XERRWD (MSG, 30, 205, 0, 0, 0, 0, 2, TN, H)
      CALL XERRWD (205, 0)
      ISTATE = -5
      GO TO 560
C RWORK length too small to proceed. -----------------------------------
 550  MSG = 'DLSODA-  At current T(=R1), RWORK length too small'
c     CALL XERRWD (MSG, 50, 206, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
c     MSG='      to proceed.  The integration was otherwise successful.'
c     CALL XERRWD (MSG, 60, 206, 0, 0, 0, 0, 1, TN, 0.0D0)
      CALL XERRWD (206, 0)
      ISTATE = -7
      GO TO 580
C IWORK length too small to proceed. -----------------------------------
 555  MSG = 'DLSODA-  At current T(=R1), IWORK length too small'
c     CALL XERRWD (MSG, 50, 207, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
c     MSG='      to proceed.  The integration was otherwise successful.'
c     CALL XERRWD (MSG, 60, 207, 0, 0, 0, 0, 1, TN, 0.0D0)
      CALL XERRWD (207, 0)
      ISTATE = -7
      GO TO 580
C Compute IMXER if relevant. -------------------------------------------
 560  BIG = 0.0D0
      IMXER = 1
      DO 570 I = 1,N
        SIZE = ABS(RWORK(I+LACOR-1)*RWORK(I+LEWT-1))
        IF (BIG .GE. SIZE) GO TO 570
        BIG = SIZE
        IMXER = I
 570    CONTINUE
      IWORK(16) = IMXER
C Set Y vector, T, and optional outputs. -------------------------------
 580  DO 590 I = 1,N
C590    Y(I) = RWORK(I+LYH-1)
        Y(I) = RWORK(I+LYH-1)
 590    CONTINUE
      T = TN
      RWORK(11) = HU
      RWORK(12) = H
      RWORK(13) = TN
      RWORK(15) = TSW
      IWORK(11) = NST
      IWORK(12) = NFE
      IWORK(13) = NJE
      IWORK(14) = NQU
      IWORK(15) = NQ
      IWORK(19) = MUSED
      IWORK(20) = METH
      RETURN
C-----------------------------------------------------------------------
C Block I.
C The following block handles all error returns due to illegal input
C (ISTATE = -3), as detected before calling the core integrator.
C First the error message routine is called.  If the illegal input
C is a negative ISTATE, the run is aborted (apparent infinite loop).
C-----------------------------------------------------------------------
 601  MSG = 'DLSODA-  ISTATE (=I1) illegal.'
c     CALL XERRWD (MSG, 30, 1, 0, 1, ISTATE, 0, 0, 0.0D0, 0.0D0)
      CALL XERRWD (1, 0)
      IF (ISTATE .LT. 0) GO TO 800
      GO TO 700
 602  MSG = 'DLSODA-  ITASK (=I1) illegal. '
c     CALL XERRWD (MSG, 30, 2, 0, 1, ITASK, 0, 0, 0.0D0, 0.0D0)
      CALL XERRWD (2, 0)
      GO TO 700
 603  MSG = 'DLSODA-  ISTATE .gt. 1 but DLSODA not initialized.'
c     CALL XERRWD (MSG, 50, 3, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
      CALL XERRWD (3, 0)
      GO TO 700
 604  MSG = 'DLSODA-  NEQ (=I1) .lt. 1     '
c     CALL XERRWD (MSG, 30, 4, 0, 1, NEQ(1), 0, 0, 0.0D0, 0.0D0)
      CALL XERRWD (4, 0)
      GO TO 700
 605  MSG = 'DLSODA-  ISTATE = 3 and NEQ increased (I1 to I2). '
c     CALL XERRWD (MSG, 50, 5, 0, 2, N, NEQ(1), 0, 0.0D0, 0.0D0)
      CALL XERRWD (5, 0)
      GO TO 700
 606  MSG = 'DLSODA-  ITOL (=I1) illegal.  '
c     CALL XERRWD (MSG, 30, 6, 0, 1, ITOL, 0, 0, 0.0D0, 0.0D0)
      CALL XERRWD (6, 0)
      GO TO 700
 607  MSG = 'DLSODA-  IOPT (=I1) illegal.  '
c     CALL XERRWD (MSG, 30, 7, 0, 1, IOPT, 0, 0, 0.0D0, 0.0D0)
      CALL XERRWD (7, 0)
      GO TO 700
 608  MSG = 'DLSODA-  JT (=I1) illegal.    '
c     CALL XERRWD (MSG, 30, 8, 0, 1, JT, 0, 0, 0.0D0, 0.0D0)
      CALL XERRWD (8, 0)
      GO TO 700
 609  MSG = 'DLSODA-  ML (=I1) illegal: .lt.0 or .ge.NEQ (=I2) '
c     CALL XERRWD (MSG, 50, 9, 0, 2, ML, NEQ(1), 0, 0.0D0, 0.0D0)
      CALL XERRWD (9, 0)
      GO TO 700
 610  MSG = 'DLSODA-  MU (=I1) illegal: .lt.0 or .ge.NEQ (=I2) '
c     CALL XERRWD (MSG, 50, 10, 0, 2, MU, NEQ(1), 0, 0.0D0, 0.0D0)
      CALL XERRWD (10, 0)
      GO TO 700
 611  MSG = 'DLSODA-  IXPR (=I1) illegal.  '
c     CALL XERRWD (MSG, 30, 11, 0, 1, IXPR, 0, 0, 0.0D0, 0.0D0)
      CALL XERRWD (11, 0)
      GO TO 700
 612  MSG = 'DLSODA-  MXSTEP (=I1) .lt. 0  '
c     CALL XERRWD (MSG, 30, 12, 0, 1, MXSTEP, 0, 0, 0.0D0, 0.0D0)
      CALL XERRWD (12, 0)
      GO TO 700
 613  MSG = 'DLSODA-  MXHNIL (=I1) .lt. 0  '
c     CALL XERRWD (MSG, 30, 13, 0, 1, MXHNIL, 0, 0, 0.0D0, 0.0D0)
      CALL XERRWD (13, 0)
      GO TO 700
 614  MSG = 'DLSODA-  TOUT (=R1) behind T (=R2)      '
c     CALL XERRWD (MSG, 40, 14, 0, 0, 0, 0, 2, TOUT, T)
c     MSG = '      Integration direction is given by H0 (=R1)  '
c     CALL XERRWD (MSG, 50, 14, 0, 0, 0, 0, 1, H0, 0.0D0)
      CALL XERRWD (14, 0)
      GO TO 700
 615  MSG = 'DLSODA-  HMAX (=R1) .lt. 0.0  '
c     CALL XERRWD (MSG, 30, 15, 0, 0, 0, 0, 1, HMAX, 0.0D0)
      CALL XERRWD (15, 0)
      GO TO 700
 616  MSG = 'DLSODA-  HMIN (=R1) .lt. 0.0  '
c     CALL XERRWD (MSG, 30, 16, 0, 0, 0, 0, 1, HMIN, 0.0D0)
      CALL XERRWD (16, 0)
      GO TO 700
 617  MSG='DLSODA-  RWORK length needed, LENRW (=I1), exceeds LRW (=I2)'
c     CALL XERRWD (MSG, 60, 17, 0, 2, LENRW, LRW, 0, 0.0D0, 0.0D0)
      CALL XERRWD (17, 0)
      GO TO 700
 618  MSG='DLSODA-  IWORK length needed, LENIW (=I1), exceeds LIW (=I2)'
c     CALL XERRWD (MSG, 60, 18, 0, 2, LENIW, LIW, 0, 0.0D0, 0.0D0)
      CALL XERRWD (18, 0)
      GO TO 700
 619  MSG = 'DLSODA-  RTOL(I1) is R1 .lt. 0.0        '
c     CALL XERRWD (MSG, 40, 19, 0, 1, I, 0, 1, RTOLI, 0.0D0)
      CALL XERRWD (19, 0)
      GO TO 700
 620  MSG = 'DLSODA-  ATOL(I1) is R1 .lt. 0.0        '
c     CALL XERRWD (MSG, 40, 20, 0, 1, I, 0, 1, ATOLI, 0.0D0)
      CALL XERRWD (20, 0)
      GO TO 700
 621  EWTI = RWORK(LEWT+I-1)
c     MSG = 'DLSODA-  EWT(I1) is R1 .le. 0.0         '
c     CALL XERRWD (MSG, 40, 21, 0, 1, I, 0, 1, EWTI, 0.0D0)
      CALL XERRWD (21, 0)
      GO TO 700
 622  MSG='DLSODA-  TOUT(=R1) too close to T(=R2) to start integration.'
c     CALL XERRWD (MSG, 60, 22, 0, 0, 0, 0, 2, TOUT, T)
      CALL XERRWD (22, 0)
      GO TO 700
 623  MSG='DLSODA-  ITASK = I1 and TOUT (=R1) behind TCUR - HU (= R2)  '
c     CALL XERRWD (MSG, 60, 23, 0, 1, ITASK, 0, 2, TOUT, TP)
      CALL XERRWD (23, 0)
      GO TO 700
 624  MSG='DLSODA-  ITASK = 4 or 5 and TCRIT (=R1) behind TCUR (=R2)   '
c     CALL XERRWD (MSG, 60, 24, 0, 0, 0, 0, 2, TCRIT, TN)
      CALL XERRWD (24, 0)
      GO TO 700
 625  MSG='DLSODA-  ITASK = 4 or 5 and TCRIT (=R1) behind TOUT (=R2)   '
c     CALL XERRWD (MSG, 60, 25, 0, 0, 0, 0, 2, TCRIT, TOUT)
      CALL XERRWD (25, 0)
      GO TO 700
 626  MSG = 'DLSODA-  At start of problem, too much accuracy   '
c     CALL XERRWD (MSG, 50, 26, 0, 0, 0, 0, 0, 0.0D0, 0.0D0)
c     MSG='      requested for precision of machine..  See TOLSF (=R1) '
c     CALL XERRWD (MSG, 60, 26, 0, 0, 0, 0, 1, TOLSF, 0.0D0)
      CALL XERRWD (26, 0)
      RWORK(14) = TOLSF
      GO TO 700
 627  MSG = 'DLSODA-  Trouble in DINTDY.  ITASK = I1, TOUT = R1'
c     CALL XERRWD (MSG, 50, 27, 0, 1, ITASK, 0, 1, TOUT, 0.0D0)
      CALL XERRWD (27, 0)
      GO TO 700
 628  MSG = 'DLSODA-  MXORDN (=I1) .lt. 0  '
c     CALL XERRWD (MSG, 30, 28, 0, 1, MXORDN, 0, 0, 0.0D0, 0.0D0)
      CALL XERRWD (28, 0)
      GO TO 700
 629  MSG = 'DLSODA-  MXORDS (=I1) .lt. 0  '
c     CALL XERRWD (MSG, 30, 29, 0, 1, MXORDS, 0, 0, 0.0D0, 0.0D0)
      CALL XERRWD (29, 0)
C
 700  ISTATE = -3
      RETURN
C
 800  MSG = 'DLSODA-  Run aborted.. apparent infinite loop.    '
c     CALL XERRWD (MSG, 50, 303, 2, 0, 0, 0, 0, 0.0D0, 0.0D0)
      CALL XERRWD (303, 2)
      RETURN
C----------------------- End of Subroutine DLSODA ----------------------
      END
