!     MLF: Modification to return I when norm is 0
!-----------------------------------------------------------------------!
!                                                                       !
!     V A R I O U S    M A T H E M A T I C A L    U T I L I T I E S     !
!                                                                       !
!                       FORTRAN 77 PROCEDURES                           !
!-----------------------------------------------------------------------!

*     DGEMM -> DGEXX for LTO
* Idea comes from https://github.com/nmatzke/rexpokit/blob/2e7712fb5e6d3c9786e412a8b7f2ec7cced097ef/src/lapack/blas_mod.f#L1259-L1580
*----------------------------------------------------------------------|
      SUBROUTINE DGEXX ( TRANSA, TRANSB, M, N, K, ALPHA, A, LDA, B, LDB,
     $                   BETA, C, LDC )
*     .. Scalar Arguments ..
      CHARACTER(LEN=1)   TRANSA, TRANSB
      INTEGER            M, N, K, LDA, LDB, LDC
      DOUBLE PRECISION   ALPHA, BETA
*     .. Array Arguments ..
*     2019-06-26_NJM: fix
*
* Error: Variable 'ldb' cannot appear in the expression at (1)
*
      DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), C( LDC, * )

*
*     ..
*
*  Purpose
*  =======
*
*  DGEXX  performs one of the matrix-matrix operations
*
*     C := alpha*op( A )*op( B ) + beta*C,
*
*  where  op( X ) is one of
*
*     op( X ) = X   or   op( X ) = X',
*
*  alpha and beta are scalars, and A, B and C are matrices, with op( A )
*  an m by k matrix,  op( B )  a  k by n matrix and  C an m by n matrix.
*
*  Parameters
*  ==========
*
*  TRANSA - CHARACTER*1.
*           On entry, TRANSA specifies the form of op( A ) to be used in
*           the matrix multiplication as follows:
*
*              TRANSA = 'N' or 'n',  op( A ) = A.
*
*              TRANSA = 'T' or 't',  op( A ) = A'.
*
*              TRANSA = 'C' or 'c',  op( A ) = A'.
*
*           Unchanged on exit.
*
*  TRANSB - CHARACTER*1.
*           On entry, TRANSB specifies the form of op( B ) to be used in
*           the matrix multiplication as follows:
*
*              TRANSB = 'N' or 'n',  op( B ) = B.
*
*              TRANSB = 'T' or 't',  op( B ) = B'.
*
*              TRANSB = 'C' or 'c',  op( B ) = B'.
*
*           Unchanged on exit.
*
*  M      - INTEGER.
*           On entry,  M  specifies  the number  of rows  of the  matrix
*           op( A )  and of the  matrix  C.  M  must  be at least  zero.
*           Unchanged on exit.
*
*  N      - INTEGER.
*           On entry,  N  specifies the number  of columns of the matrix
*           op( B ) and the number of columns of the matrix C. N must be
*           at least zero.
*           Unchanged on exit.
*
*  K      - INTEGER.
*           On entry,  K  specifies  the number of columns of the matrix
*           op( A ) and the number of rows of the matrix op( B ). K must
*           be at least  zero.
*           Unchanged on exit.
*
*  ALPHA  - DOUBLE PRECISION.
*           On entry, ALPHA specifies the scalar alpha.
*           Unchanged on exit.
*
*  A      - DOUBLE PRECISION array of DIMENSION ( LDA, ka ), where ka is
*           k  when  TRANSA = 'N' or 'n',  and is  m  otherwise.
*           Before entry with  TRANSA = 'N' or 'n',  the leading  m by k
*           part of the array  A  must contain the matrix  A,  otherwise
*           the leading  k by m  part of the array  A  must contain  the
*           matrix A.
*           Unchanged on exit.
*
*  LDA    - INTEGER.
*           On entry, LDA specifies the first dimension of A as declared
*           in the calling (sub) program. When  TRANSA = 'N' or 'n' then
*           LDA must be at least  max( 1, m ), otherwise  LDA must be at
*           least  max( 1, k ).
*           Unchanged on exit.
*
*  B      - DOUBLE PRECISION array of DIMENSION ( LDB, kb ), where kb is
*           n  when  TRANSB = 'N' or 'n',  and is  k  otherwise.
*           Before entry with  TRANSB = 'N' or 'n',  the leading  k by n
*           part of the array  B  must contain the matrix  B,  otherwise
*           the leading  n by k  part of the array  B  must contain  the
*           matrix B.
*           Unchanged on exit.
*
*  LDB    - INTEGER.
*           On entry, LDB specifies the first dimension of B as declared
*           in the calling (sub) program. When  TRANSB = 'N' or 'n' then
*           LDB must be at least  max( 1, k ), otherwise  LDB must be at
*           least  max( 1, n ).
*           Unchanged on exit.
*
*  BETA   - DOUBLE PRECISION.
*           On entry,  BETA  specifies the scalar  beta.  When  BETA  is
*           supplied as zero then C need not be set on input.
*           Unchanged on exit.
*
*  C      - DOUBLE PRECISION array of DIMENSION ( LDC, n ).
*           Before entry, the leading  m by n  part of the array  C must
*           contain the matrix  C,  except when  beta  is zero, in which
*           case C need not be set on entry.
*           On exit, the array  C  is overwritten by the  m by n  matrix
*           ( alpha*op( A )*op( B ) + beta*C ).
*
*  LDC    - INTEGER.
*           On entry, LDC specifies the first dimension of C as declared
*           in  the  calling  (sub)  program.   LDC  must  be  at  least
*           max( 1, m ).
*           Unchanged on exit.
*
*
*  Level 3 Blas routine.
*
*  -- Written on 8-February-1989.
*     Jack Dongarra, Argonne National Laboratory.
*     Iain Duff, AERE Harwell.
*     Jeremy Du Croz, Numerical Algorithms Group Ltd.
*     Sven Hammarling, Numerical Algorithms Group Ltd.
*
*
*     .. External Functions ..
      LOGICAL            LSAMEX
      EXTERNAL           LSAMEX
*     .. External Subroutines ..
      EXTERNAL           XERBLA
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     .. Local Scalars ..
      LOGICAL            NOTA, NOTB
      INTEGER            I, INFO, J, L, NCOLA, NROWA, NROWB
      DOUBLE PRECISION   TEMP
*     .. Parameters ..
      DOUBLE PRECISION   ONE         , ZERO
      PARAMETER        ( ONE = 1.0D+0, ZERO = 0.0D+0 )
*     ..
*     .. Executable Statements ..
*
*     Set  NOTA  and  NOTB  as  true if  A  and  B  respectively are not
*     transposed and set  NROWA, NCOLA and  NROWB  as the number of rows
*     and  columns of  A  and the  number of  rows  of  B  respectively.
*
      NOTA  = LSAMEX( TRANSA, 'N' )
      NOTB  = LSAMEX( TRANSB, 'N' )
      IF( NOTA )THEN
         NROWA = M
         NCOLA = K
      ELSE
         NROWA = K
         NCOLA = M
      END IF
      IF( NOTB )THEN
         NROWB = K
      ELSE
         NROWB = N
      END IF
*
*     Test the input parameters.
*
      INFO = 0
      IF(      ( .NOT.NOTA                 ).AND.
     $         ( .NOT.LSAMEX( TRANSA, 'C' ) ).AND.
     $         ( .NOT.LSAMEX( TRANSA, 'T' ) )      )THEN
         INFO = 1
      ELSE IF( ( .NOT.NOTB                 ).AND.
     $         ( .NOT.LSAMEX( TRANSB, 'C' ) ).AND.
     $         ( .NOT.LSAMEX( TRANSB, 'T' ) )      )THEN
         INFO = 2
      ELSE IF( M  .LT.0               )THEN
         INFO = 3
      ELSE IF( N  .LT.0               )THEN
         INFO = 4
      ELSE IF( K  .LT.0               )THEN
         INFO = 5
      ELSE IF( LDA.LT.MAX( 1, NROWA ) )THEN
         INFO = 8
      ELSE IF( LDB.LT.MAX( 1, NROWB ) )THEN
         INFO = 10
      ELSE IF( LDC.LT.MAX( 1, M     ) )THEN
         INFO = 13
      END IF
      IF( INFO.NE.0 )THEN
         CALL XERBLA( 'DGEXX ', INFO )
         RETURN
      END IF
*
*     Quick return if possible.
*
      IF( ( M.EQ.0 ).OR.( N.EQ.0 ).OR.
     $    ( ( ( ALPHA.EQ.ZERO ).OR.( K.EQ.0 ) ).AND.( BETA.EQ.ONE ) ) )
     $   RETURN
*
*     And if  alpha.eq.zero.
*
      IF( ALPHA.EQ.ZERO )THEN
         IF( BETA.EQ.ZERO )THEN
            DO 20, J = 1, N
               DO 10, I = 1, M
                  C( I, J ) = ZERO
   10          CONTINUE
   20       CONTINUE
         ELSE
            DO 40, J = 1, N
               DO 30, I = 1, M
                  C( I, J ) = BETA*C( I, J )
   30          CONTINUE
   40       CONTINUE
         END IF
         RETURN
      END IF
*
*     Start the operations.
*
      IF( NOTB )THEN
         IF( NOTA )THEN
*
*           Form  C := alpha*A*B + beta*C.
*
            DO 90, J = 1, N
               IF( BETA.EQ.ZERO )THEN
                  DO 50, I = 1, M
                     C( I, J ) = ZERO
   50             CONTINUE
               ELSE IF( BETA.NE.ONE )THEN
                  DO 60, I = 1, M
                     C( I, J ) = BETA*C( I, J )
   60             CONTINUE
               END IF
               DO 80, L = 1, K
                  IF( B( L, J ).NE.ZERO )THEN
                     TEMP = ALPHA*B( L, J )
                     DO 70, I = 1, M
                        C( I, J ) = C( I, J ) + TEMP*A( I, L )
   70                CONTINUE
                  END IF
   80          CONTINUE
   90       CONTINUE
         ELSE
*
*           Form  C := alpha*A'*B + beta*C
*
            DO 120, J = 1, N
               DO 110, I = 1, M
                  TEMP = ZERO
                  DO 100, L = 1, K
                     TEMP = TEMP + A( L, I )*B( L, J )
  100             CONTINUE
                  IF( BETA.EQ.ZERO )THEN
                     C( I, J ) = ALPHA*TEMP
                  ELSE
                     C( I, J ) = ALPHA*TEMP + BETA*C( I, J )
                  END IF
  110          CONTINUE
  120       CONTINUE
         END IF
      ELSE
         IF( NOTA )THEN
*
*           Form  C := alpha*A*B' + beta*C
*
            DO 170, J = 1, N
               IF( BETA.EQ.ZERO )THEN
                  DO 130, I = 1, M
                     C( I, J ) = ZERO
  130             CONTINUE
               ELSE IF( BETA.NE.ONE )THEN
                  DO 140, I = 1, M
                     C( I, J ) = BETA*C( I, J )
  140             CONTINUE
               END IF
               DO 160, L = 1, K
                  IF( B( J, L ).NE.ZERO )THEN
                     TEMP = ALPHA*B( J, L )
                     DO 150, I = 1, M
                        C( I, J ) = C( I, J ) + TEMP*A( I, L )
  150                CONTINUE
                  END IF
  160          CONTINUE
  170       CONTINUE
         ELSE
*
*           Form  C := alpha*A'*B' + beta*C
*
            DO 200, J = 1, N
               DO 190, I = 1, M
                  TEMP = ZERO
                  DO 180, L = 1, K
                     TEMP = TEMP + A( L, I )*B( J, L )
  180             CONTINUE
                  IF( BETA.EQ.ZERO )THEN
                     C( I, J ) = ALPHA*TEMP
                  ELSE
                     C( I, J ) = ALPHA*TEMP + BETA*C( I, J )
                  END IF
  190          CONTINUE
  200       CONTINUE
         END IF
      END IF
*
      RETURN
*
*     End of DGEXX .
*
      END
*----------------------------------------------------------------------|

*----------------------------------------------------------------------|
      LOGICAL          FUNCTION LSAMEX( CA, CB )
*
*  -- LAPACK auxiliary routine (version 1.1) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      CHARACTER          CA, CB
*     ..
*
*  Purpose
*  =======
*
*  LSAMEX returns .TRUE. if CA is the same letter as CB regardless of
*  case.
*
*  Arguments
*  =========
*
*  CA      (input) CHARACTER*1
*  CB      (input) CHARACTER*1
*          CA and CB specify the single characters to be compared.
*
*     .. Intrinsic Functions ..
      INTRINSIC          ICHAR
*     ..
*     .. Local Scalars ..
      INTEGER            INTA, INTB, ZCODE
*     ..
*     .. Executable Statements ..
*
*     Test if the characters are equal
*
      LSAMEX = CA.EQ.CB
      IF( LSAMEX )
     $   RETURN
*
*     Now test for equivalence if both characters are alphabetic.
*
      ZCODE = ICHAR( 'Z' )
*
*     Use 'Z' rather than 'A' so that ASCII can be detected on Prime
*     machines, on which ICHAR returns a value with bit 8 set.
*     ICHAR('A') on Prime machines returns 193 which is the same as
*     ICHAR('A') on an EBCDIC machine.
*
      INTA = ICHAR( CA )
      INTB = ICHAR( CB )
*
      IF( ZCODE.EQ.90 .OR. ZCODE.EQ.122 ) THEN
*
*        ASCII is assumed - ZCODE is the ASCII code of either lower or
*        upper case 'Z'.
*
         IF( INTA.GE.97 .AND. INTA.LE.122 ) INTA = INTA - 32
         IF( INTB.GE.97 .AND. INTB.LE.122 ) INTB = INTB - 32
*
      ELSE IF( ZCODE.EQ.233 .OR. ZCODE.EQ.169 ) THEN
*
*        EBCDIC is assumed - ZCODE is the EBCDIC code of either lower or
*        upper case 'Z'.
*
         IF( INTA.GE.129 .AND. INTA.LE.137 .OR.
     $       INTA.GE.145 .AND. INTA.LE.153 .OR.
     $       INTA.GE.162 .AND. INTA.LE.169 ) INTA = INTA + 64
         IF( INTB.GE.129 .AND. INTB.LE.137 .OR.
     $       INTB.GE.145 .AND. INTB.LE.153 .OR.
     $       INTB.GE.162 .AND. INTB.LE.169 ) INTB = INTB + 64
*
      ELSE IF( ZCODE.EQ.218 .OR. ZCODE.EQ.250 ) THEN
*
*        ASCII is assumed, on Prime machines - ZCODE is the ASCII code
*        plus 128 of either lower or upper case 'Z'.
*
         IF( INTA.GE.225 .AND. INTA.LE.250 ) INTA = INTA - 32
         IF( INTB.GE.225 .AND. INTB.LE.250 ) INTB = INTB - 32
      END IF
      LSAMEX = INTA.EQ.INTB
*
*     RETURN
*
*     End of LSAMEX
*
      END


!-----------------------------------------------------------------------!
      subroutine matexpRBS (ideg, m, t, H, iflag)

      IMPLICIT NONE
      INTEGER ideg, m, iflag
      DOUBLE PRECISION t, H(m,m)

!-----PURPOSE-----------------------------------------------------------!
!
!     COMPUTES EXP(T*H), THE MATRIX EXPONENTIAL OF A GENERAL MATRIX IN
!     FULL, USING THE IRREDUCIBLE RATIONAL PADE APPROXIMATION TO THE
!     EXPONENTIAL FUNCTION EXP(X) = R(X) = (+/-)( I + 2*(Q(X)/P(X)) ),
!     COMBINED WITH SCALING-AND-SQUARING.
!
!-----ARGUMENTS---------------------------------------------------------!
!
!     IDEG      : (INPUT) THE DEGREE OF THE DIAGONAL PADE TO BE USED.
!                 A VALUE OF 6 IS GENERALLY SATISFACTORY.
!
!     M         : (INPUT) ORDER OF H.
!
!     T         : (INPUT) TIME-SCALE (CAN BE < 0).
!
!     H(M,M)    : (INPUT) ARGUMENT MATRIX.
!
!     IFLAG     : (OUTPUT) EXIT FLAG.
!                      0 - NO PROBLEM
!                     <0 - PROBLEM
!
!-----------------------------------------------------------------------!
!     ROGER B. SIDJE (RBS@MATHS.UQ.EDU.AU) - 'RBS'
!     EXPOKIT: SOFTWARE PACKAGE FOR COMPUTING MATRIX EXPONENTIALS.
!     ACM - TRANSACTIONS ON MATHEMATICAL SOFTWARE, 24(1):130-156, 1998
!-----------------------------------------------------------------------!
!     MODIFIED TO RETURN A FLAG INSTEAD OF TERMINATING, WHEN TRYING
!     TO COMPUTE THE EXPONENTIAL OF A MATRIX WITH TOO LARGE ELEMENTS.
!
!     1) NIELS RODE KRISTENSEN, TECHNICAL UNIVERSITY OF DENMARK, 2000
!     2) ANDREAS S. CHRISTENSEN,TECHNICAL UNIVERSITY OF DENMARK, 2006
!     3) SØREN KLIM, IMM-DTU, 2007
!-----------------------------------------------------------------------!
!
      INTEGER  LWSP, NS, IPIV(M)
      DOUBLE PRECISION WSP(4*M*M+IDEG+1)
      INTEGER MM,I,J,K,IH2,IP,IQ,IUSED,IFREE,IODD,ICOEF,IPUT,IGET
      DOUBLE PRECISION HNORM,SCALE,SCALE2,CP,CQ

! "External" routines:
      INTRINSIC INT,ABS,DBLE,LOG,MAX
!---  CHECK RESTRICTIONS ON INPUT PARAMETERS ...
      MM = M*M
      IFLAG = 0
      LWSP = 4*M*M + IDEG +1
!
!---  INITIALISE POINTERS ...
!
      ICOEF = 1
      IH2 = ICOEF + (IDEG+1)
      IP  = IH2 + MM
      IQ  = IP + MM
      IFREE = IQ + MM

      NS=0

!
!---  INITIALISE ARRAYS ...
!
      DO I = 1, LWSP
         WSP(I) = 0.0D0
      ENDDO

      DO I = 1, M
         IPIV(I) = 0
      ENDDO

!
!---  SCALING: SEEK NS SUCH THAT ||T*H/2^NS|| < 1/2;
!     AND SET SCALE = T/2^NS ...
!

      DO J = 1,M
         DO I = 1,M
            WSP(I) = WSP(I) + ABS( H(I,J) )
         ENDDO
      ENDDO
      HNORM = 0.0D0
      DO I = 1,M
         HNORM = MAX( HNORM,WSP(I) )
      ENDDO
      HNORM = ABS( T*HNORM )
      IF (HNORM .EQ. 0.D0) THEN
! Deviation from original algorithm, use identity
         DO I = 1, M
            H(I,I)=1.D0
         ENDDO
         GOTO 200
      ENDIF
      NS = MAX(0, INT(LOG(HNORM)/LOG(2.))+2)
      SCALE = T / DBLE(2**NS)
      SCALE2 = SCALE*SCALE
!
!---  COMPUTE PADE COEFFICIENTS ...
!
      I = IDEG+1
      J = 2*IDEG+1
      WSP(ICOEF) = 1.0D0
      DO K = 1,IDEG
         WSP(ICOEF+K) = (WSP(ICOEF+K-1)*DBLE( I-K ))/DBLE( K*(J-K) )
      ENDDO
!
!---  H2 = SCALE2*H*H ...
!
      CALL DGEXX( 'N','N',M,M,M,SCALE2,H,M,H,M,0.0D0,WSP(IH2),M )
!
!---  INITIALIZE P (NUMERATOR) AND Q (DENOMINATOR) ...
!
      CP = WSP(ICOEF+IDEG-1)
      CQ = WSP(ICOEF+IDEG)
      DO J = 1,M
         DO I = 1,M
            WSP(IP + (J-1)*M + I-1) = 0.0D0
            WSP(IQ + (J-1)*M + I-1) = 0.0D0
         ENDDO
         WSP(IP + (J-1)*(M+1)) = CP
         WSP(IQ + (J-1)*(M+1)) = CQ
      ENDDO
!
!---  APPLY HORNER RULE ...
!
      IODD = 1
      K = IDEG - 1
 100  CONTINUE
      IUSED = IODD*IQ + (1-IODD)*IP
      CALL DGEXX( 'N','N',M,M,M, 1.0D0,WSP(IUSED),M,
     .             WSP(IH2),M, 0.0D0,WSP(IFREE),M )
      DO J = 1,M
         WSP(IFREE+(J-1)*(M+1)) = WSP(IFREE+(J-1)*(M+1))+WSP(ICOEF+K-1)
      ENDDO
      IP = (1-IODD)*IFREE + IODD*IP
      IQ = IODD*IFREE + (1-IODD)*IQ
      IFREE = IUSED
      IODD = 1-IODD
      K = K-1
      IF ( K.GT.0 )  GOTO 100
!
!---  OBTAIN (+/-)(I + 2*(P\Q)) ...
!
      IF ( IODD .EQ. 1 ) THEN
         CALL DGEXX( 'N','N',M,M,M, SCALE,WSP(IQ),M,
     .                H,M, 0.0D0,WSP(IFREE),M )
         IQ = IFREE
      ELSE
         CALL DGEXX( 'N','N',M,M,M, SCALE,WSP(IP),M,
     .                H,M, 0.0D0,WSP(IFREE),M )
         IP = IFREE
      ENDIF
      CALL DAXPY( MM, -1.0D0,WSP(IP),1, WSP(IQ),1 )
      CALL DGESV( M,M, WSP(IQ),M, IPIV, WSP(IP),M, IFLAG )
      IF ( IFLAG.NE.0 ) CALL RWarn ('PROBLEM IN DGESV (WITHIN DGPADM)')
      CALL DSCAL( MM, 2.0D0, WSP(IP), 1 )
      DO J = 1,M
         WSP(IP+(J-1)*(M+1)) = WSP(IP+(J-1)*(M+1)) + 1.0D0
      ENDDO
      IPUT = IP
      IF ( NS.EQ.0 .AND. IODD.EQ.1 ) THEN
         CALL DSCAL( MM, -1.0D0, WSP(IP), 1 )
      ELSE
!---  		SQUARING : EXP(T*H) = (EXP(T*H))^(2^NS) ...
!
         IODD = 1
         DO K = 1,NS
            IGET = IODD*IP + (1-IODD)*IQ
            IPUT = (1-IODD)*IP + IODD*IQ
            CALL DGEXX( 'N','N',M,M,M, 1.0D0,WSP(IGET),M, WSP(IGET),M,
     .           0.0D0,WSP(IPUT),M )
            IODD = 1-IODD
         ENDDO
      ENDIF

!---  COPY EXP(H*T) into H
      DO I= 1,M
         DO J=1,M
            H(I,J) = WSP(IPUT +(I-1) + (J-1)*M)
         ENDDO
      ENDDO
!-----------------------------------------------------------------------!
200   END
