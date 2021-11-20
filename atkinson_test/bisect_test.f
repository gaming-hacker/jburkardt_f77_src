      program bisect_test

C     THIS IS A DEMONSTRATION PROGRAM FOR THE
C     ROOTFINDING SUBROUTINE 'BISECT'.
C
      EXTERNAL bisect_fcn
C
C     INPUT PROBLEM PARAMETERS.

      a = 0.0
      b = 1.0
      epsilon = 1.0e-05
C
C     CALCULATE ROOT
      CALL BISECT ( bisect_fcn, A ,B ,EPSLON, ROOT, IER)
C
C     PRINT ANSWERS.
      PRINT 1000, A, B, EPSLON
      PRINT 1001, ROOT, IER
1000  FORMAT(/,' A=',E11.4,5X,'B=',E11.4,5X,'EPSILON=',E9.3)
1001  FORMAT(' ROOT=',E14.7,5X,'IER=',I1)

      stop 0
      END

      FUNCTION bisect_fcn (X)
C
      bisect_fcn = EXP(-X) - X
      RETURN    
      END

