c*** genin2.f
      PROGRAM GENIN2
C
C        *****  This is the driver for the base-2 programs.
C        *****  More accurately, it is a driver skeleton.
C        *****  There is a default set of test integrals
C        *****  in TESTF.  The user can replace TESTF with
C        *****  another subroutine called TESTF containing
C        *****  integrals of interest to him.
C
C   This version :  2 Feb 1992
C
C   This program tests the accuracy of numerical integration
C   using the low-discrepancy binary sequences of
C   H. Niederreiter (1988) as implemented in INLO2, GOLO2, and
C   related programs.  Various possible test integrals are
C   provided by the function TESTF.  GENIN2 generates only
C   sequences with base 2.
C
C   Interactive input and output go through the Fortran units
C   denoted by * ;  at most installations, * corresponds to
C   the user's terminal.  Fortran unit 2 is used to save the output.
C
C   These programs assume that your computer's word length
C   is 31 bits, excluding sign.  If this is not the case,
C   modify the parameter NBITS throughout accordingly.
C
C
C
C    GENIN2 and its associated subroutines are listed below.
C
C       GENIN2
C          INLO2
C          GOLO2
C          CALCC2
C          CALCV
C          CHARAC
C          SETFLD
C          PLYMUL
C          TESTF
C
C    The suffix 2 indicates  routines for use only by
C    the set of programs tailored for base 2.  
C
C    The other routines are also used by the general-base programs.
C
      INTEGER MAXDIM, OUTUNT, POWER
      PARAMETER (MAXDIM=12, OUTUNT=2, POWER  = 12)
C
C   The parameter MAXDIM gives the maximum dimension that will
C   be used.  OUTUNT is the unit to save the output.
C   POWER is used in a possible warm-up formula.
C
      INTEGER I, NUM, DIMEN, SEQLEN, SKIP, STEP, PBASE
      REAL QUASI(MAXDIM), TESTF, EXACTF 
      DOUBLE PRECISION SUM
C
      WRITE (*,*) ' This is program GENIN2'
      WRITE (*,*) ' Output file name is OUTFIL.DAT '
      OPEN (unit = OUTUNT, file = 'OUTFIL.DAT', status = 'UNKNOWN')
C
C      *****   OPEN statements may well have to be modified
C      *****   to conform to local computer-center requirements.
C
C
    5 WRITE (*,*) ' Choose a test integral (1 to 4) or 0 to quit :'
      READ (*,*) NUM
      IF (NUM.LE.0) THEN
        WRITE (*,*) ' End of program GENIN2'
        CLOSE (OUTUNT)
        STOP
      ENDIF
      IF (NUM.GT.4) THEN
        WRITE (*,*) ' No such test integral'
        GOTO 5
      ENDIF
C
C       *****  Each test integral is parameterized by
C       *****  its dimension.
C
   10 WRITE (*,*) ' Enter dimension :'
      READ (*,*) DIMEN
      IF (DIMEN.GT.MAXDIM) THEN
        WRITE (*,*) ' Dimension may not exceed', MAXDIM
        GOTO 10
      ENDIF
C
C        *****  The sequence length is the number of
C        *****  quasi-random points used to estimate the
C        *****  integral, excluding warm-up.
C
C        *****  Some users may wish to rewrite
C        *****  the driver to test a [heuristic] "convergence"
C        *****  criterion, stopping the generation of points
C        *****  when it is passed or when a specified number of
C        *****  points have been generated  -- whichever occurs
C        *****  first.
C
   15 WRITE (*,*) ' Choose sequence length :'
C
C         *****  Except when comparing results with other
C         *****  bases, we suggest taking SEQLEN to be a power
C         *****  of 2.  Examples:
C
      WRITE (*,*) ' 2 ** 10 =  ',  2 ** 10
      WRITE (*,*) ' 2 ** 15 =  ',  2 ** 15
      WRITE (*,*) ' 2 ** 20 =  ',  2 ** 20
      WRITE (*,*) ' Enter SEQLEN  (possibly a power of 2 above) '
      READ (*,*) SEQLEN
      IF (SEQLEN.LE.0) THEN
        WRITE (*,*) ' Length must be strictly positive'
        GOTO 15
      ENDIF
C
   20 WRITE (*,*) ' Choose the number of values to skip :'
      WRITE (*,*) ' There is reason to believe that BASE * e, '
      WRITE (*,*) ' where e is defined for example in '
      WRITE (*,*) ' Bratley, Fox, and Niederreiter [1991], '
      WRITE (*,*) ' would be a good choice.  Our formula has '
      WRITE (*,*) ' has the form  SKIP = 2 ** POWER, where '
      WRITE (*,*) ' POWER is  chosen so that SKIP comes nowhere '
      WRITE (*,*) ' near the largest possible machine-representable'
      WRITE (*,*) ' integer.  It does not hurt to choose '
      WRITE (*,*) ' POWER larger than e, because skipping is '
      WRITE (*,*) ' done implicitly in O(1) time. '
      WRITE (*,*) ' The maximum value of e for a fixed dimension '
      WRITE (*,*) ' s grows like log s.  We allow some "fat" for '
      WRITE (*,*) ' the implicit constant. '
      WRITE (*,*) ' Numerically, 2 ** POWER = ', 2 ** POWER
      WRITE (*,*) ' Enter SKIP (not necessarily the value above)'
      READ (*,*) SKIP
      IF (SKIP.LT.0) THEN
        WRITE (*,*) ' Number must be nonnegative'
        GOTO 20
      ENDIF
C
C
      CALL INLO2 (DIMEN, SKIP)
      WRITE (*,*) ' GENIN2 :  Initialization complete'
C
C Write titles and the exact value of the integral
C
      WRITE (OUTUNT,27) NUM
   27 FORMAT (/,' Test integral ',I2)
      WRITE (OUTUNT,28) DIMEN, SEQLEN, SKIP
   28 FORMAT (/,' Dimension ',I6,',    Base 2 (GENIN2)',
     1 /,' Sequence ',I7,',    Skipped ',I4)
      WRITE (OUTUNT,29) EXACTF(NUM, DIMEN)
   29 FORMAT (/,' Correct value is ',G16.7)
      WRITE (OUTUNT,30)
   30 FORMAT(/,'      Iteration     Estimate of integral',/)
C
C Now estimate the integral
C
      SUM = 0
      PBASE = 2 ** 6
      WRITE (*,*) ' Odd-looking iteration numbers are powers of 2 '
      STEP = 500
      DO 100 I = 1, SEQLEN
        CALL GOLO2 (QUASI)
        SUM = SUM + TESTF(NUM, DIMEN, QUASI)
        IF (MOD(I,STEP).EQ.0) THEN
          WRITE (OUTUNT,99) I, SUM/I
        ENDIF
        IF (MOD(I,PBASE) .EQ. 0) THEN
          WRITE (OUTUNT,99) I, SUM/I
          PBASE = 2 * PBASE
C
C            There is reason to believe that the subsequence
C            of estimates along powers of the base [here 2]
C            converges faster than the original sequence or
C            the subsequence corresponding to STEP.
C
        ENDIF
C
   99     FORMAT (I12,G24.7)
          IF (I .EQ. 5000) STEP = 1000
          IF (I .EQ. 10000) STEP = 5000
  100 CONTINUE
C
      WRITE (*,*) ' GENIN2 :  iteration ends'
      GOTO 5
C
      END
C
C     ***** end of PROGRAM GENIN2
      SUBROUTINE INLO2 (DIM, SKIP)
C
C   This version :  12 February 1992
C
C   See the general comments on implementing Niederreiter's
C   low-discrepancy sequences.
C
C   This subroutine calculates the values of Niederreiter's
C   C(I,J,R) and performs other initialisation necessary
C   before calling GOLO2.
C
C INPUT :
C   DIMEN - The dimension of the sequence to be generated.
C        {DIMEN is called DIM in the argument of INLO2,
C        because DIMEN is subsequently passed via COMMON
C        and is called DIMEN there.}
C
C   SKIP  - The number of values to throw away at the beginning
C           of the sequence.
C
C OUTPUT :
C   To GOLO2, labelled common /COMM2/.
C
C USES :
C   Calls CALCC2 to calculate the values of CJ.
C   ***** A non-standard function is used to compute *****
C   ***** bitwise exclusive-ors.                     *****
C
C
C   ------------------------------------------------------------
C
C
C   This file defines the common block /COMM2/ and some
C   associated parameters.  These are for use in the base 2
C   version of the generator.
C
      INTEGER MAXDIM, NBITS
      PARAMETER (MAXDIM=12, NBITS=31)
C
C   The parameter MAXDIM is the maximum dimension that will be used.
C   NBITS is the number of bits (not counting the sign) in a
C   fixed-point integer.
C
      INTEGER CJ(MAXDIM, 0:NBITS - 1), DIMEN, COUNT
      INTEGER NEXTQ(MAXDIM)
      COMMON /COMM2/ CJ, DIMEN, COUNT, NEXTQ
      SAVE   /COMM2/
C
C   The common block /COMM2/ :
C     CJ    - Contains the packed values of Niederreiter's C(I,J,R)
C     DIMEN   - The dimension of the sequence to be generated
C     COUNT - The index of the current item in the sequence,
C             expressed as an array of bits.  COUNT(R) is the same
C             as Niederreiter's AR(N) (page 54) except that N is
C             implicit.
C     NEXTQ - The numerators of the next item in the series.  These
C             are like Niederreiter's XI(N) (page 54) except that
C             N is implicit, and the NEXTQ are integers.  To obtain
C             the values of XI(N), multiply by RECIP (see GOLO2).
C
C   Array CJ of the common block is set up by subroutine CALCC2.
C   The other items in the common block are set up by INLO2.
C
C   ------------------------------------------------------------
C
C
C
      INTEGER I, R, DIM, SKIP, GRAY
C
      DIMEN = DIM
C
C       This assignment just relabels the variable for
C       subsequent use.
C
      IF (DIMEN.LE.0 .OR. DIMEN.GT.MAXDIM) THEN
        WRITE (*,*) ' INLO2 :  Bad dimension'
        STOP
      ENDIF
C
      CALL CALCC2
C
C   Translate SKIP into Gray code
C
C   ***** The bitwise exclusive-or is not standard in Fortran
C   ***** This is the Vax version :
      GRAY = IEOR (SKIP, SKIP/2)
C   ***** THIS is the Unix version
C     GRAY = XOR (SKIP, SKIP/2)
C
C   Now set up NEXTQ appropriately for this value of the Gray code
C
      DO 5 I = 1, DIMEN
    5   NEXTQ(I) = 0
C
      R = 0
   10 IF (GRAY .NE. 0) THEN
        IF (MOD(GRAY,2) .NE. 0) THEN
          DO 20 I = 1, DIMEN
C           ***** This is the non-standard exclusive-or again
C           ***** Vax version :
            NEXTQ(I) = IEOR(NEXTQ(I), CJ(I,R))
C           ***** Unix version :
C           NEXTQ(I) = XOR(NEXTQ(I), CJ(I,R))
   20     CONTINUE
        ENDIF
        GRAY = GRAY / 2
        R = R + 1
        GOTO 10
      ENDIF
C
      COUNT = SKIP
      RETURN
      END
C
C     *****  end of SUBROUTINE INLO2
      SUBROUTINE GOLO2 (QUASI)
C
C   This version :  21 February 1992
C
C        This routine is for base 2 only.  The driver, GENIN2,
C        calls it after proper set-up.
C
C See the general comments on implementing Niederreiter's
C low-discrepancy sequences.
C
C This subroutine generates a new quasi-random vector
C on each call.
C
C INPUT
C   From INLO2, labelled common /COMM2/, properly initialized.
C
C OUTPUT
C   To the caller, the next vector in the sequence in the
C   array QUASI.
C
C USES
C   ***** A non-standard function is used to compute *****
C   ***** bitwise exclusive-ors.                     *****
C
C
C   ------------------------------------------------------------
C
C
C   This file defines the common block /COMM2/ and some
C   associated parameters.  These are for use in the base 2
C   version of the generator.
C
      INTEGER MAXDIM, NBITS
      PARAMETER (MAXDIM=12, NBITS=31)
C
C   The parameter MAXDIM is the maximum dimension that will be used.
C   NBITS is the number of bits (not counting the sign) in a
C   fixed-point integer.
C
      INTEGER CJ(MAXDIM, 0:NBITS-1), DIMEN, COUNT, NEXTQ(MAXDIM)
      COMMON /COMM2/ CJ, DIMEN, COUNT, NEXTQ
      SAVE   /COMM2/
C
C   The common block /COMM2/ :
C     CJ    - Contains the packed values of Niederreiter's C(I,J,R)
C     DIMEN   - The dimension of the sequence to be generated
C     COUNT - The index of the current item in the sequence,
C             expressed as an array of bits.  COUNT(R) is the same
C             as Niederreiter's AR(N) (page 54) except that N is
C             implicit.
C     NEXTQ - The numerators of the next item in the series.  These
C             are like Niederreiter's XI(N) (page 54) except that
C             N is implicit, and the NEXTQ are integers.  To obtain
C             the values of XI(N), multiply by RECIP (see GOLO2).
C
C   Array CJ of the common block is set up by subroutine CALCC2.
C   The other items in the common block are set up by INLO2.
C
C   ------------------------------------------------------------
C
C
C
      REAL RECIP
      PARAMETER (RECIP=2.0**(-NBITS))
C
C   The parameter RECIP is the multiplier which changes the
C   integers in NEXTQ into the required real values in QUASI.
C
      INTEGER I, R
      REAL    QUASI(*)
C
C Multiply the numerators in NEXTQ by RECIP to get the next
C quasi-random vector
C
      DO 5 I = 1, DIMEN
        QUASI(I) = NEXTQ(I) * RECIP
    5 CONTINUE
C
C Find the position of the right-hand zero in COUNT.  This
C is the bit that changes in the Gray-code representation as
C we go from COUNT to COUNT+1.
C
      R = 0
      I = COUNT
   10 IF (MOD(I,2).NE.0) THEN
        R = R + 1
        I = I/2
        GOTO 10
      ENDIF
C
C Check that we have not passed 2**NBITS calls on GOLO2
C
      IF (R .GE. NBITS) THEN
        WRITE (*,*) ' GOLO2 :  Too many calls'
        STOP
      ENDIF
C
C Compute the new numerators in vector NEXTQ
C
      DO 20 I = 1, DIMEN
C       ***** Bitwise exclusive-or is not standard in Fortran
C       ***** This is the Vax version :
       NEXTQ(I) = IEOR(NEXTQ(I), CJ(I,R))
C       ***** This is the Unix version
C      NEXTQ(I) = XOR(NEXTQ(I), CJ(I,R))
   20 CONTINUE
C
      COUNT = COUNT + 1
      RETURN
      END
C
C     *****   end of PROCEDURE GOLO2
      SUBROUTINE CALCC2
C
C   This version :  12 February 1992
C
C      *****  For base-2 only.
C
C   See the general comments on implementing Niederreiter's
C   low-discrepancy sequences.
C
C   This program calculates the values of the constants C(I,J,R).
C   As far as possible, we use Niederreiter's notation.
C   For each value of I, we first calculate all the corresponding
C   values of C :  these are held in the array CI.  All these
C   values are either 0 or 1.  Next we pack the values into the
C   array CJ, in such a way that CJ(I,R) holds the values of C
C   for the indicated values of I and R and for every value of
C   J from 1 to NBITS.  The most significant bit of CJ(I,R)
C   (not counting the sign bit) is C(I,1,R) and the least
C   significant bit is C(I,NBITS,R).
C     When all the values of CJ have been calculated, we return
C   this array to the calling program.
C
C  --------------------------------------------------------------
C
C   We define the common block /COMM2/ and some
C   associated parameters.  These are for use in the base 2
C   version of the generator.
C
      INTEGER MAXDIM, NBITS
      PARAMETER (MAXDIM=12, NBITS=31)
C
C   The parameter MAXDIM is the maximum dimension that will be used.
C   NBITS is the number of bits (not counting the sign) in a
C   fixed-point integer.
C
      INTEGER CJ(MAXDIM, 0:NBITS-1), DIMEN, COUNT, NEXTQ(MAXDIM)
      COMMON /COMM2/ CJ, DIMEN, COUNT, NEXTQ
      SAVE   /COMM2/
C
C   The common block /COMM2/ :
C     CJ    - Contains the packed values of Niederreiter's C(I,J,R)
C     DIMEN   - The dimension of the sequence to be generated
C     COUNT - The index of the current item in the sequence,
C             expressed as an array of bits.  COUNT(R) is the same
C             as Niederreiter's AR(N) (page 54) except that N is
C             implicit.
C     NEXTQ - The numerators of the next item in the series.  These
C             are like Niederreiter's XI(N) (page 54) except that
C             N is implicit, and the NEXTQ are integers.  To obtain
C             the values of XI(N), multiply by RECIP (see GOLO2).
C
C   Array CJ of the common block is set up by subroutine CALCC2.
C   The other items in the common block are set up by INLO2.
C
C   --------------------------------------------------------------
C
C   The following COMMON block, used by many subroutines,
C   gives the order Q of a field, its characteristic P, and its
C   addition, multiplication and subtraction tables.
C   The parameter MAXQ gives the order of the largest field to
C   be handled.
C
      INTEGER MAXQ
      PARAMETER (MAXQ=50)
 
      INTEGER P, Q, ADD(0:MAXQ-1,0:MAXQ-1)
      INTEGER MUL(0:MAXQ-1, 0:MAXQ-1), SUB(0:MAXQ-1, 0:MAXQ-1)
      COMMON /FIELD/ P, Q, ADD, MUL, SUB
      SAVE /FIELD/
C
C   The following definitions concern the representation of
C   polynomials.
C
      INTEGER MAXDEG, DEG
      PARAMETER (MAXDEG=50, DEG=-1)
C
C   The parameter MAXDEG gives the highest degree of polynomial
C   to be handled.  Polynomials stored as arrays have the
C   coefficient of degree n in POLY(N), and the degree of the
C   polynomial in POLY(-1).  The parameter DEG is just to remind
C   us of this last fact.  A polynomial which is identically 0
C   is given degree -1.
C
C   A polynomial can also be stored in an integer I, with
C        I = AN*Q**N + ... + A0.
C   Routines ITOP and PTOI convert between these two formats.
C
C   ---------------------------------------------------------------
C
C
C
C   MAXE   - We need MAXDIM irreducible polynomials over Z2.
C            MAXE is the highest degree among these.
C   MAXV   - The maximum possible index used in V.
C
      INTEGER MAXE, MAXV
      PARAMETER (MAXE=5, MAXV=NBITS+MAXE)
C
C INPUT :
C   The array CJ to be initialised, and DIMEN the number of
C   dimensions we are using, are transmitted through /COMM2/.
C
C OUTPUT :
C   The array CJ is returned to the calling program.
C
C USES :
C   Subroutine SETFLD is used to set up field arithmetic tables.
C   (Although this is a little heavy-handed for the field of
C   order 2, it makes for uniformity with the general program.)
C   Subroutine CALCV is used to for the auxiliary calculation
C   of the values V needed to get the Cs.
C
      INTEGER PX(-1:MAXDEG), B(-1:MAXDEG)
      INTEGER V(0:MAXV), CI(NBITS, 0:NBITS-1)
      INTEGER E, I, J, R, U, TERM
C
      INTEGER IRRED(MAXDIM, -1:MAXE)
      SAVE IRRED
C
C   This DATA statement supplies the coefficients and the
C   degrees of the first 12 irreducible polynomials over Z2.
C   They are taken from Lidl and Niederreiter, FINITE FIELDS,
C   Cambridge University Press (1984), page 553.
C   The order of these polynomials is the same as the order in
C   file 'irrtabs.dat' used by the general program.
C
C   In this block PX(I, -1) is the degree of the Ith polynomial,
C   and PX(I, N) is the coefficient of x**n in the Ith polynomial.
C
      DATA (IRRED(1,I), I=-1,1)  / 1,0,1 /
      DATA (IRRED(2,I), I=-1,1)  / 1,1,1 /
      DATA (IRRED(3,I), I=-1,2)  / 2,1,1,1 /
      DATA (IRRED(4,I), I=-1,3)  / 3,1,1,0,1 /
      DATA (IRRED(5,I), I=-1,3)  / 3,1,0,1,1 /
      DATA (IRRED(6,I), I=-1,4)  / 4,1,1,0,0,1 /
      DATA (IRRED(7,I), I=-1,4)  / 4,1,0,0,1,1 /
      DATA (IRRED(8,I), I=-1,4)  / 4,1,1,1,1,1 /
      DATA (IRRED(9,I), I=-1,5)  / 5,1,0,1,0,0,1 /
      DATA (IRRED(10,I), I=-1,5) / 5,1,0,0,1,0,1 /
      DATA (IRRED(11,I), I=-1,5) / 5,1,1,1,1,0,1 /
      DATA (IRRED(12,I), I=-1,5) / 5,1,1,1,0,1,1 /
C
C   Prepare to work in Z2
C
      CALL SETFLD (2)
C
      DO 1000 I = 1, DIMEN
C
C   For each dimension, we need to calculate powers of an
C   appropriate irreducible polynomial :  see Niederreiter
C   page 65, just below equation (19).
C     Copy the appropriate irreducible polynomial into PX,
C   and its degree into E.  Set polynomial B = PX ** 0 = 1.
C   M is the degree of B.  Subsequently B will hold higher
C   powers of PX.
C
        E = IRRED(I, DEG)
        DO 10 J = -1, E
          PX(J) = IRRED(I,J)
   10   CONTINUE
        B(DEG) = 0
        B(0) = 1
C
C   Niederreiter (page 56, after equation (7), defines two
C   variables Q and U.  We do not need Q explicitly, but we
C   do need U.
C
        U = 0
C
        DO 90 J = 1, NBITS
C
C   If U = 0, we need to set B to the next power of PX
C   and recalculate V.  This is done by subroutine CALCV.
C
          IF (U .EQ. 0) CALL CALCV (PX, B, V, MAXV)
C
C Now C is obtained from V.  Niederreiter
C obtains A from V (page 65, near the bottom), and then gets
C C from A (page 56, equation (7)).  However this can be done
C in one step.  Here CI(J,R) corresponds to
C Niederreiter's C(I,J,R).
C
          DO 50 R = 0, NBITS-1
            CI(J,R) = V(R+U)
   50     CONTINUE
C
C Increment U.  If U = E, then U = 0 and in Niederreiter's
C paper Q = Q + 1.  Here, however, Q is not used explicitly.
C
          U = U + 1
          IF (U .EQ. E) U = 0
  90    CONTINUE
C
C  The array CI now holds the values of C(I,J,R) for this value
C  of I.  We pack them into array CJ so that CJ(I,R) holds all
C  the values of C(I,J,R) for J from 1 to NBITS.
C
        DO 120 R = 0, NBITS-1
          TERM = 0
          DO 110 J = 1, NBITS
            TERM = 2 * TERM + CI(J,R)
  110     CONTINUE
          CJ(I,R) = TERM
  120   CONTINUE
C
 1000 CONTINUE
      END
C
C      *****  end of CALCC2
      SUBROUTINE CALCV (PX, B, V, MAXV)
C
C   This version :  12 February 1991
C
C   See the general comments on implementing Niederreiter's
C   low-discrepancy sequences.
C
C   This program calculates the values of the constants V(J,R) as
C   described in BFN section 3.3.  It is called from either CALCC or
C   CALCC2.  The values transmitted through common /FIELD/ determine
C   which field we are working in.
C
C INPUT :
C   PX is the appropriate irreducible polynomial for the dimension
C     currently being considered.  The degree of PX will be called E.
C   B is the polynomial defined in section 2.3 of BFN.  On entry to
C     the subroutine, the degree of B implicitly defines the parameter
C     J of section 3.3, by degree(B) = E*(J-1).
C   MAXV gives the dimension of the array V.
C   On entry, we suppose that the common block /FIELD/ has been set
C     up correctly (using SETFLD).
C
C OUTPUT :
C   On output B has been multiplied by PX, so its degree is now E*J.
C   V contains the values required.
C
C USES :
C   The subroutine PLYMUL is used to multiply polynomials.
C
C
C   ------------------------------------------------------------
C
C   The following COMMON block, used by many subroutines,
C   gives the order Q of a field, its characteristic P, and its
C   addition, multiplication, and subtraction tables.
C   The parameter MAXQ gives the order of the largest field to
C   be handled.
C
      INTEGER MAXQ
      PARAMETER (MAXQ=50)
 
      INTEGER P, Q, ADD(0:MAXQ-1,0:MAXQ-1)
      INTEGER MUL(0:MAXQ-1, 0:MAXQ-1), SUB(0:MAXQ-1, 0:MAXQ-1)
      COMMON /FIELD/ P, Q, ADD, MUL, SUB
      SAVE /FIELD/
C
C   The following definitions concern the representation of
C   polynomials.
C
      INTEGER MAXDEG, DEG
      PARAMETER (MAXDEG=50, DEG=-1)
C
C   The parameter MAXDEG gives the highest degree of polynomial
C   to be handled.  Polynomials stored as arrays have the
C   coefficient of degree n in POLY(N), and the degree of the
C   polynomial in POLY(-1).  The parameter DEG is just to remind
C   us of this last fact.  A polynomial which is identically 0
C   is given degree -1.
C
C   A polynomial can also be stored in an integer I, with
C        I = AN*Q**N + ... + A0.
C   Routines ITOP and PTOI convert between these two formats.
C
C   -----------------------------------------------------------
C
C
C
      INTEGER MAXV, E, I, J, KJ, M, BIGM, R, TERM
      INTEGER PX(-1:MAXDEG), B(-1:MAXDEG), V(0:MAXV)
      INTEGER H(-1:MAXDEG)
C
      INTEGER ARBIT, NONZER
      ARBIT() = 1
C
C   We use ARBIT() to indicate where the user can place
C   an arbitrary element of the field of order Q, while NONZER
C   shows where he must put an arbitrary non-zero element
C   of the same field.  For the code,
C   this means 0 <= ARBIT < Q and 0 < NONZER < Q.  Within these
C   limits, the user can do what he likes.  ARBIT is declared as
C   a function as a reminder that a different arbitrary value may
C   be returned each time ARBIT is referenced.
C
C    BIGM is the M used in section 3.3.
C    It differs from the [little] m used in section 2.3,
C    denoted here by M.
C
      NONZER = 1
C
      E = PX(DEG)
C
C   The poly H is PX**(J-1), which is the value of B on arrival.
C   In section 3.3, the values of Hi are defined with a minus sign :
C   don't forget this if you use them later !
C
      DO 10 I = -1, B(DEG)
   10   H(I) = B(I)
      BIGM = H(DEG)
C
C   Now multiply B by PX so B becomes PX**J.
C   In section 2.3, the values of Bi are defined with a minus sign :
C   don't forget this if you use them later !
C
      CALL PLYMUL (PX, B, B)
      M = B(DEG)
C
C   We don't use J explicitly anywhere, but here it is just in case.
C
      J = M / E
C
C   Now choose a value of Kj as defined in section 3.3.
C   We must have 0 <= Kj < E*J = M.
C   The limit condition on Kj does not seem very relevant
C   in this program.
C
      KJ = BIGM
C
C   Now choose values of V in accordance with the conditions in
C   section 3.3
C
      DO 20 R = 0, KJ-1
   20   V(R) = 0
      V(KJ) = 1
C
      IF (KJ .LT. BIGM) THEN
C
        TERM = SUB (0, H(KJ))
C
        DO 30 R = KJ+1, BIGM-1
          V(R) = ARBIT()
C
C         Check the condition of section 3.3,
C         remembering that the H's have the opposite sign.
C
          TERM = SUB (TERM, MUL (H(R), V(R)))
   30   CONTINUE
C
C         Now V(BIGM) is anything but TERM
C
          V(BIGM) = ADD (NONZER, TERM)
C
        DO 40 R = BIGM+1, M-1
   40     V(R) = ARBIT()
C
      ELSE
C       This is the case KJ .GE. BIGM
C
        DO 50 R = KJ+1, M-1
   50     V(R) = ARBIT()
C
      ENDIF
C
C   Calculate the remaining V's using the recursion of section 2.3,
C   remembering that the B's have the opposite sign.
C
      DO 70 R = 0, MAXV-M
        TERM = 0
        DO 60 I = 0, M-1
          TERM = SUB (TERM, MUL (B(I), V(R+I)))
   60   CONTINUE
        V(R+M) = TERM
   70 CONTINUE
C
      RETURN
      END
C
C     ***** end of SUBROUTINE CALCV
      INTEGER FUNCTION CHARAC (QIN)
C
C   This version :  12 December 1991
C
C   This function gives the characteristic for a field of
C   order QIN.  If no such field exists, or if QIN is out of
C   the range we can handle, returns 0.
C
C
C   ------------------------------------------------------------
C
C   The following COMMON block, used by many subroutines,
C   gives the order Q of a field, its characteristic P, and its
C   addition, multiplication and subtraction tables.
C   The parameter MAXQ gives the order of the largest field to
C   be handled.
C
      INTEGER MAXQ
      PARAMETER (MAXQ=50)
 
      INTEGER P, Q, ADD(0:MAXQ-1,0:MAXQ-1)
      INTEGER MUL(0:MAXQ-1, 0:MAXQ-1), SUB(0:MAXQ-1, 0:MAXQ-1)
      COMMON /FIELD/ P, Q, ADD, MUL, SUB
      SAVE /FIELD/
C
C   The following definitions concern the representation of
C   polynomials.
C
      INTEGER MAXDEG, DEG
      PARAMETER (MAXDEG=50, DEG=-1)
C
C   The parameter MAXDEG gives the highest degree of polynomial
C   to be handled.  Polynomials stored as arrays have the
C   coefficient of degree n in POLY(N), and the degree of the
C   polynomial in POLY(-1).  The parameter DEG is just to remind
C   us of this last fact.  A polynomial which is identically 0
C   is given degree -1.
C
C   A polynomial can also be stored in an integer I, with
C        I = AN*Q**N + ... + A0.
C   Routines ITOP and PTOI convert between these two formats.
C
C   -----------------------------------------------------------
C
C
C
      INTEGER QIN, CH(MAXQ)
      SAVE CH
C
      DATA CH  / 0,  2,  3,  2,  5,  0,  7,  2,  3,  0,
     1          11,  0, 13,  0,  0,  2, 17,  0, 19,  0,
     2           0,  0, 23,  0,  5,  0,  3,  0, 29,  0,
     3          31,  2,  0,  0,  0,  0, 37,  0,  0,  0,
     4          41,  0, 43,  0,  0,  0, 47,  0,  7,  0/
C
      IF (QIN .LE. 1 .OR. QIN .GT. MAXQ) THEN
        CHARAC = 0
      ELSE
        CHARAC = CH(QIN)
      ENDIF
C
      END
C
C     ***** end of INTEGER FUNCTION CHARAC
      SUBROUTINE SETFLD (QIN)
      INTEGER QIN
C
C   This version : 12 December 1991
C
C   This subroutine sets up addition, multiplication, and
C   subtraction tables for the finite field of order QIN.
C   If necessary, it reads precalculated tables from the file
C   'gftabs.dat' using unit 1.  These precalculated tables
C   are supposed to have been put there by GFARIT.
C
C      *****  For the base-2 programs, these precalculated
C      *****  tables are not needed and, therefore, neither
C      *****  is GFARIT.
C
C
C   Unit 1 is closed both before and after the call of SETFLD.
C
C USES
C   Integer function CHARAC gets the characteristic of a field.
C
C
C   ------------------------------------------------------------
C
C   The following COMMON block, used by many subroutines,
C   gives the order Q of a field, its characteristic P, and its
C   addition, multiplication and subtraction tables.
C   The parameter MAXQ gives the order of the largest field to
C   be handled.
C
      INTEGER MAXQ
      PARAMETER (MAXQ=50)
 
      INTEGER P, Q, ADD(0:MAXQ-1,0:MAXQ-1)
      INTEGER MUL(0:MAXQ-1, 0:MAXQ-1), SUB(0:MAXQ-1, 0:MAXQ-1)
      COMMON /FIELD/ P, Q, ADD, MUL, SUB
      SAVE /FIELD/
C
C   The following definitions concern the representation of
C   polynomials.
C
      INTEGER MAXDEG, DEG
      PARAMETER (MAXDEG=50, DEG=-1)
C
C   The parameter MAXDEG gives the highest degree of polynomial
C   to be handled.  Polynomials stored as arrays have the
C   coefficient of degree n in POLY(N), and the degree of the
C   polynomial in POLY(-1).  The parameter DEG is just to remind
C   us of this last fact.  A polynomial which is identically 0
C   is given degree -1.
C
C   A polynomial can also be stored in an integer I, with
C        I = AN*Q**N + ... + A0.
C   Routines ITOP and PTOI convert between these two formats.
C
C   -----------------------------------------------------------
C
C
C
      INTEGER I, J, N, CHARAC
C
      IF (QIN .LE. 1 .OR. QIN .GT. MAXQ) THEN
        WRITE (*,*) ' SETFLD :  Bad value of Q'
        STOP
      ENDIF
C
      Q = QIN
      P = CHARAC(Q)
C
      IF (P .EQ. 0) THEN
        WRITE (*,*) ' SETFLD :  There is no field of order', Q
        STOP
      ENDIF
C
C Set up to handle a field of prime order :  calculate ADD and MUL.
C
      IF (P .EQ. Q) THEN
        DO 10 I = 0, Q-1
          DO 10 J = 0, Q-1
            ADD(I,J) = MOD(I+J, P)
            MUL(I,J) = MOD(I*J, P)
   10   CONTINUE
C
C Set up to handle a field of prime-power order :  tables for
C ADD and MUL are in the file 'gftabs.dat'.
C
      ELSE
        OPEN (UNIT=1, FILE='gftabs.dat', STATUS='old')
C
C    *****  OPEN statements are system dependent.
C
   20   READ (1, 900, END=500) N
  900   FORMAT (20I3)
        DO 30 I = 0, N-1
          READ (1, 900) (ADD(I,J), J = 0, N-1)
   30   CONTINUE
        DO 40 I = 0, N-1
          READ (1, 900) (MUL(I,J), J = 0, N-1)
   40   CONTINUE
        IF (N .NE. Q) GOTO 20
        CLOSE (1)
      ENDIF
C
C Now use the addition table to set the subtraction table.
C
      DO 60 I = 0, Q-1
        DO 50 J = 0, Q-1
          SUB(ADD(I,J), I) = J
   50   CONTINUE
   60 CONTINUE
      RETURN
C
  500 WRITE (*,*) ' SETFLD :  Tables for q =', Q, ' not found'
      STOP
C
      END
C
C     ***** end of SUBROUTINE SETFLD
      SUBROUTINE PLYMUL (PA, PB, PC)
C
C   This version :  12 December 1991
C
C
C   ------------------------------------------------------------
C
C   The following COMMON block, used by many subroutines,
C   gives the order Q of a field, its characteristic P, and its
C   addition, multiplication and subtraction tables.
C   The parameter MAXQ gives the order of the largest field to
C   be handled.
C
      INTEGER MAXQ
      PARAMETER (MAXQ=50)
 
      INTEGER P, Q, ADD(0:MAXQ-1,0:MAXQ-1)
      INTEGER MUL(0:MAXQ-1, 0:MAXQ-1), SUB(0:MAXQ-1, 0:MAXQ-1)
      COMMON /FIELD/ P, Q, ADD, MUL, SUB
      SAVE /FIELD/
C
C   The following definitions concern the representation of
C   polynomials.
C
      INTEGER MAXDEG, DEG
      PARAMETER (MAXDEG=50, DEG=-1)
C
C   The parameter MAXDEG gives the highest degree of polynomial
C   to be handled.  Polynomials stored as arrays have the
C   coefficient of degree n in POLY(N), and the degree of the
C   polynomial in POLY(-1).  The parameter DEG is just to remind
C   us of this last fact.  A polynomial which is identically 0
C   is given degree -1.
C
C   A polynomial can also be stored in an integer I, with
C        I = AN*Q**N + ... + A0.
C   Routines ITOP and PTOI convert between these two formats.
C
C   -----------------------------------------------------------
C
C
C
      INTEGER I, J, DEGA, DEGB, DEGC, TERM
      INTEGER PA(-1:MAXDEG), PB(-1:MAXDEG), PC(-1:MAXDEG)
      INTEGER PT(-1:MAXDEG)
C
C   Multiplies polynomial PA by PB putting the result in PC.
C   Coefficients are elements of the field of order Q.
C
      DEGA = PA(DEG)
      DEGB = PB(DEG)
      IF (DEGA .EQ. -1 .OR. DEGB .EQ. -1) THEN
        DEGC = -1
      ELSE
        DEGC = DEGA + DEGB
      ENDIF
      IF (DEGC .GT. MAXDEG) THEN
        WRITE (*,*) ' PLYMUL :  Degree of product exceeds MAXDEG'
        STOP
      ENDIF
C
      DO 20 I = 0, DEGC
        TERM = 0
        DO 10 J = MAX(0, I-DEGA), MIN(DEGB, I)
   10     TERM = ADD(TERM, MUL(PA(I-J), PB(J)))
   20   PT(I) = TERM
C
      PC(DEG) = DEGC
      DO 30 I = 0, DEGC
   30   PC(I) = PT(I)
      DO 40 I = DEGC+1, MAXDEG
   40   PC(I) = 0
      RETURN
      END
C
C     *****   end of SUBROUTINE PLYMUL
      REAL FUNCTION TESTF (N, DIMEN, QUASI)
      INTEGER I, N, DIMEN
      REAL X, EXACTF, QUASI(*)
C
C This version :  4 Mar 1992
C
C Provides a variety of test integrals for quasi-random
C sequences.  A call on TESTF computes an estimate of the
C integral ;  a call on EXACTF computes the exact value.
C
      GOTO (100, 200, 300, 400) N
C
      ENTRY EXACTF (N, DIMEN)
      GOTO (1100, 1200, 1300, 1400) N
C
C Test integral 1
C
  100 TESTF = 1.0
      DO 110 I = 1, DIMEN
        TESTF = TESTF * ABS(4 * QUASI(I) - 2)
  110 CONTINUE
      RETURN
C
 1100 EXACTF = 1.0
      RETURN
C
C Test integral 2
C
  200 TESTF = 1.0
      DO 210 I = 1, DIMEN
        TESTF = TESTF * I * COS(I * QUASI(I))
  210 CONTINUE
      RETURN
C
 1200 EXACTF = 1.0
      DO 1210 I = 1, DIMEN
        EXACTF = EXACTF * SIN(FLOAT(I))
 1210 CONTINUE
      RETURN
C
C Test integral 3
C
  300 TESTF = 1.0
      DO 350 I = 1, DIMEN
        X = 2 * QUASI(I) - 1
        GOTO (310, 320, 330, 340) MOD(I, 4)
  310   TESTF = TESTF * X
        GOTO 350
  320   TESTF = TESTF * (2*X*X - 1)
        GOTO 350
  330   TESTF = TESTF * (4*X*X - 3) * X
        GOTO 350
  340   X = X * X
        TESTF = TESTF * (8*X*X - 8*X + 1)
  350 CONTINUE
      RETURN
C
 1300 EXACTF = 0.0
      RETURN
C
C Test integral 4
C
  400        TESTF = 0
             X = 1
             DO 410   I = 1, DIMEN
                  X = - X * QUASI(I)
                  TESTF = TESTF + X
  410        CONTINUE
             RETURN
C
C
 1400 X = 1.0 / (2 ** (DIMEN ))
      IF (MOD(DIMEN, 2) .EQ. 0) THEN
        EXACTF = (X - 1) / 3
      ELSE
        EXACTF = (X + 1) / 3
      ENDIF
      RETURN
C
      END
