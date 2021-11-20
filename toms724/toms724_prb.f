      PROGRAM main

c*********************************************************************72
c
cc MAIN is the main program for TOMS724_PRB.
c
c  Discussion:
c
c    TOMS724_PRB tests the TOMS724 library.
c
      implicit none

      double precision fi
      double precision finv
      integer i
      integer j
      integer k
      integer M(8)
      integer m1
      integer N(8)
      integer n1
      double precision P(5)
      double precision pr
      double precision X(8,8)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TOMS724_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the TOMS724 library.'

      P(1) = 9.0D-1
      P(2) = 99.0D-2
      P(3) = 999.0D-3
      P(4) = 9999.0D-4
      P(5) = 99999.0D-5

      M(1) = 1
      M(2) = 2
      M(3) = 3
      M(4) = 10
      M(5) = 20
      M(6) = 50
      M(7) = 100
      M(8) = 200

      DO I = 1, 8
        N(I) = M(I)
      end do

      DO I = 1, 5

        DO J = 1, 8
          DO K = 1, 8
            X(J,K) = FINV ( M(J), N(K), P(I) )
            if ( X(J,K) .LT. 0.0D+00 ) then
              write ( *, '(A,2I4,F9.6,A)')
     *            'FINV (',M(J),N(K),P(I),' ) fails'
            end if
          end do
        end do

        WRITE(*,*)
        WRITE(*,'(A5,F8.5)') ' P = ',P(I)
        WRITE(*,*)
        WRITE(*,1) 'N:M',(M(J),J=1,4)
        WRITE(*,*)
        DO K=1,8
          WRITE(*,2)  N(K),(X(J,K),J=1,4)
        end do
        WRITE(*,*)
        WRITE(*,1) 'N:M',(M(J),J=5,8)
        WRITE(*,*)
        DO K = 1, 8
          WRITE(*,2)  N(K),(X(J,K),J=5,8)
        end do
        WRITE(*,*)

      end do

 1    FORMAT(2X,A5,1X,4(6X,I5,6X))
 2    FORMAT(2X,I5,1X,4(E17.8E3))
 
      M1 = -1
      N1 = 3
      PR = 0.9D0
      FI = FINV(M1,N1,PR)
      WRITE(*,'(1X,A11,I3,I3,F17.9)') 'M,N,P = ',M1,N1,PR
      WRITE(*,'(1X,A11,F17.9)') ' FINV = ',FI

      M1 = 3
      N1 = -1
      PR = 0.9D0
      FI = FINV(M1,N1,PR)
      WRITE(*,'(1X,A11,I3,I3,F17.9)') 'M,N,P = ',M1,N1,PR
      WRITE(*,'(1X,A11,F17.9)') ' FINV = ',FI

      M1 = 3
      N1 = 3
      PR = -0.5D0
      FI = FINV(M1,N1,PR)
      WRITE(*,'(1X,A11,I3,I3,F17.9)') 'M,N,P = ',M1,N1,PR
      WRITE(*,'(1X,A11,F17.9)') ' FINV = ',FI

      M1 = 3
      N1 = 3
      PR = 0.0D0
      FI = FINV(M1,N1,PR)
      WRITE(*,'(1X,A11,I3,I3,F17.9)') 'M,N,P = ',M1,N1,PR
      WRITE(*,'(1X,A11,F17.9)') ' FINV = ',FI
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TOMS724_PRB:'
      write ( *, '(a)' ) '  Normal end of execution.'

      stop
      END

