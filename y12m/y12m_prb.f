      program main

c*********************************************************************72
c
cc MAIN is the main program for Y12M_PRB.
c
c  Modified:
c
c    10 December 2007
c
      implicit none

      write ( *, '(a)' ) ' '
      call timestamp ( )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'Y12M_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the Y12M library.'

      call test01 ( )
      call test02 ( )
      call test03 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'Y12M_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'

      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01
c
c  Solve a single linear system.
c
c  The linear system to be solved is
c
c  10  0  0  0  0  0      1     10
c   0 12 -3 -1  0  0      2     11
c   0  0 15  0  0  0  *   3  =  45
c  -2  0  0 10 -1  0      4     33
c  -1  0  0 -5  1 -1      5    -22
c  -1 -2  0  0  0  6      6     31
c
c  Modified:
c
c    10 December 2007
c
      integer iha
      integer n
      integer nn
      integer nn1
      integer nonz_max

      parameter (n=6)
      parameter (iha=n)
      parameter (nonz_max=15)
      parameter (nn=3*nonz_max)
      parameter (nn1=3*nonz_max)

      real a(nn)
      real aflag(8)
      real b(n)
      integer ha(iha,11)
      integer i
      integer ifail
      integer iflag(10)
      integer nonz
      real pivot(iha)
      integer rnr(nn1)
      integer snr(nn)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  Factor a sparse matrix and solve a related'
      write ( *, '(a)' ) '  linear system.'

      do i=1,8
        aflag(i)=0.0
      enddo

      do i=1,10
        iflag(i)=0
      enddo
c
c  Set the row and column indices, and the values, of each nonzer
c  entry of A.
c
      rnr(1)=1
      snr(1)=1
      a(1)=10.0

      rnr(2)=6
      snr(2)=6
      a(2)= 6.0

      rnr(3)=6
      snr(3)=2
      a(3)=-2.0

      rnr(4)=6
      snr(4)=1
      a(4)=-1.0

      rnr(5)=2
      snr(5)=2
      a(5)=12.0

      rnr(6)=2
      snr(6)=3
      a(6)=-3.0

      rnr(7)=2
      snr(7)=4
      a(7)=-1.0

      rnr(8)=4
      snr(8)=1
      a(8)=-2.0

      rnr(9)=5
      snr(9)=1
      a(9)=-1.0

      rnr(10)=5
      snr(10)=6
      a(10)=-1.0

      rnr(11)=5
      snr(11)=5
      a(11)= 1.0

      rnr(12)=5
      snr(12)=4
      a(12)=-5.0

      rnr(13)=4
      snr(13)=4
      a(13)=10.0

      rnr(14)=4
      snr(14)=5
      a(14)=-1.0

      rnr(15)=3
      snr(15)=3
      a(15)=15.0

      b(1)= 10.0
      b(2)= 11.0
      b(3)= 45.0
      b(4)= 33.0
      b(5)=-22.0
      b(6)= 31.0
c
c  Solve the system.
c
c  Surprisingly, the input variable NONZ must be a VARIABLE, not
c  a constant or parameter.
c
      nonz = nonz_max

      call y12mae(n,nonz,a,snr,nn,rnr,nn1,pivot,ha,iha,aflag,
     &  iflag,b,ifail)
c
c  Print the results.
c
      write(*,*)' '
      write(*,*)'Y12MAE error flag IFAIL=',ifail

      if(ifail.eq.0)then

        write(*,*)' '
        write(*,*)'The solution vector:'
        write(*,*)' '

        do i=1,n
          write(*,*)i,b(i)
        enddo

        write(*,*)' '

        write(*,*)'The largest element in the original matrix '
     &    //'is AFLAG(6)=',aflag(6)

        write(*,*)'The largest element during elimination was '
     &    //'AFLAG(7)=',aflag(7)

        write(*,*)'The minimal pivot element was AFLAG(8)=',aflag(8)

        write(*,*)'The growth factor was AFLAG(5)=',aflag(5)

        write(*,*)'The number of collections in the row list ',
     &    iflag(6)

        write(*,*)'The number of collections in the column list ',
     &    iflag(7)

        write(*,*)'The largest number of elements found in A ',
     &    iflag(8)

      endif

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02
c
c  Solve two linear systems.
c
c  The linear systems to be solved are
c
c  10  0  0  0  0  0      1     10
c   0 12 -3 -1  0  0      2     11
c   0  0 15  0  0  0  *   3  =  45
c  -2  0  0 10 -1  0      4     33
c  -1  0  0 -5  1 -1      5    -22
c  -1 -2  0  0  0  6      6     31
c
c  and
c
c  10  0  0  0  0  0      6     60
c   0 12 -3 -1  0  0      5     45
c   0  0 15  0  0  0  *   4  =  60
c  -2  0  0 10 -1  0      3     16
c  -1  0  0 -5  1 -1      2    -20
c  -1 -2  0  0  0  6      1    -10
c
c  Modified:
c
c    10 December 2007
c
      integer iha
      integer n
      integer nn
      integer nn1
      integer nonz_max

      parameter (n=6)
      parameter (iha=n)
      parameter (nonz_max=15)
      parameter (nn=3*nonz_max)
      parameter (nn1=3*nonz_max)

      real a(nn)
      real aflag(8)
      real b(n)
      integer ha(iha,11)
      integer i
      integer ifail
      integer iflag(10)
      integer nonz
      real pivot(iha)
      integer rnr(nn1)
      integer snr(nn)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) '  Factor a sparse matrix and solve'
      write ( *, '(a)' ) '  a linear system;'
      write ( *, '(a)' ) '  Then solve a second linear system using'
      write ( *, '(a)' ) '  the saved factor information.'

      do i=1,8
        aflag(i)=0.0
      enddo

      do i=1,10
        iflag(i)=0
      enddo
c
c  Set the row and column indices, and the values, of each nonzer
c  entry of A.
c
      rnr(1)=1
      snr(1)=1
      a(1)=10.0

      rnr(2)=6
      snr(2)=6
      a(2)= 6.0

      rnr(3)=6
      snr(3)=2
      a(3)=-2.0

      rnr(4)=6
      snr(4)=1
      a(4)=-1.0

      rnr(5)=2
      snr(5)=2
      a(5)=12.0

      rnr(6)=2
      snr(6)=3
      a(6)=-3.0

      rnr(7)=2
      snr(7)=4
      a(7)=-1.0

      rnr(8)=4
      snr(8)=1
      a(8)=-2.0

      rnr(9)=5
      snr(9)=1
      a(9)=-1.0

      rnr(10)=5
      snr(10)=6
      a(10)=-1.0

      rnr(11)=5
      snr(11)=5
      a(11)= 1.0

      rnr(12)=5
      snr(12)=4
      a(12)=-5.0

      rnr(13)=4
      snr(13)=4
      a(13)=10.0

      rnr(14)=4
      snr(14)=5
      a(14)=-1.0

      rnr(15)=3
      snr(15)=3
      a(15)=15.0

      b(1)= 10.0
      b(2)= 11.0
      b(3)= 45.0
      b(4)= 33.0
      b(5)=-22.0
      b(6)= 31.0
c
c  Fctor the matrix and solve system #1.
c
      nonz = nonz_max

      call y12mxe(n,nonz,a,snr,nn,rnr,nn1,pivot,ha,iha,aflag,
     &  iflag,b,ifail)
c
c  Print the results.
c
      write(*,*)' '
      write(*,*)'Y12MXE error flag IFAIL=',ifail

      if(ifail.eq.0)then

        write(*,*)' '
        write(*,*)'The solution vector:'
        write(*,*)' '

        do i=1,n
          write(*,*)i,b(i)
        enddo

        write(*,*)' '

        write(*,*)'The largest element in the original matrix '
     &    //'is AFLAG(6)=',aflag(6)

        write(*,*)'The largest element during elimination was '
     &    //'AFLAG(7)=',aflag(7)

        write(*,*)'The minimal pivot element was AFLAG(8)=',aflag(8)

        write(*,*)'The growth factor was AFLAG(5)=',aflag(5)

        write(*,*)'The number of collections in the row list ',
     &    iflag(6)

        write(*,*)'The number of collections in the column list ',
     &    iflag(7)

        write(*,*)'The largest number of elements found in A ',
     &    iflag(8)

      endif

      b(1)= 60.0
      b(2)= 45.0
      b(3)= 60.0
      b(4)= 16.0
      b(5)=-20.0
      b(6)=-10.0
c
c  Solve system #2.
c
      iflag(5)=3
      call y12mde(n,a,nn,b,pivot,snr,ha,iha,iflag,ifail)
c
c  Print the results.
c
      write(*,*)' '
      write(*,*)'Solve the second linear system.'
      write(*,*)' '
      write(*,*)'Y12MXE error flag IFAIL=',ifail

      if(ifail.eq.0)then

        write(*,*)' '
        write(*,*)'The solution vector:'
        write(*,*)' '

        do i=1,n
          write(*,*)i,b(i)
        enddo

        write(*,*)' '

        write(*,*)'The largest element in the original matrix '
     &    //'is AFLAG(6)=',aflag(6)

        write(*,*)'The largest element during elimination was '
     &    //'AFLAG(7)=',aflag(7)

        write(*,*)'The minimal pivot element was AFLAG(8)=',aflag(8)

        write(*,*)'The growth factor was AFLAG(5)=',aflag(5)

        write(*,*)'The number of collections in the row list ',
     &    iflag(6)

        write(*,*)'The number of collections in the column list ',
     &    iflag(7)

        write(*,*)'The largest number of elements found in A ',
     &    iflag(8)

      endif

      return
      end
      subroutine test03 ( )

c*********************************************************************72
c
cc TEST03
c
c  Solve two linear systems.
c  Do the factor first.
c  Then do the solve.
c
c  The linear systems to be solved are
c
c  10  0  0  0  0  0      1     10
c   0 12 -3 -1  0  0      2     11
c   0  0 15  0  0  0  *   3  =  45
c  -2  0  0 10 -1  0      4     33
c  -1  0  0 -5  1 -1      5    -22
c  -1 -2  0  0  0  6      6     31
c
c  and
c
c  10  0  0  0  0  0      6     60
c   0 12 -3 -1  0  0      5     45
c   0  0 15  0  0  0  *   4  =  60
c  -2  0  0 10 -1  0      3     16
c  -1  0  0 -5  1 -1      2    -20
c  -1 -2  0  0  0  6      1    -10
c
c  Modified:
c
c    10 December 2007
c
      integer iha
      integer n
      integer nn
      integer nn1
      integer nonz_max

      parameter (n=6)
      parameter (iha=n)
      parameter (nonz_max=15)
      parameter (nn=3*nonz_max)
      parameter (nn1=3*nonz_max)

      real a(nn)
      real aflag(8)
      real b(n)
      integer ha(iha,11)
      integer i
      integer ifail
      integer iflag(10)
      integer nonz
      real pivot(iha)
      integer rnr(nn1)
      integer snr(nn)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST03'
      write ( *, '(a)' ) '  Factor a sparse matrix,'
      write ( *, '(a)' ) '  then solve two linear systems.'

      do i=1,8
        aflag(i)=0.0
      enddo

      do i=1,10
        iflag(i)=0
      enddo
c
c  Set the row and column indices, and the values, of each nonzer
c  entry of A.
c
      rnr(1)=1
      snr(1)=1
      a(1)=10.0

      rnr(2)=6
      snr(2)=6
      a(2)= 6.0

      rnr(3)=6
      snr(3)=2
      a(3)=-2.0

      rnr(4)=6
      snr(4)=1
      a(4)=-1.0

      rnr(5)=2
      snr(5)=2
      a(5)=12.0

      rnr(6)=2
      snr(6)=3
      a(6)=-3.0

      rnr(7)=2
      snr(7)=4
      a(7)=-1.0

      rnr(8)=4
      snr(8)=1
      a(8)=-2.0

      rnr(9)=5
      snr(9)=1
      a(9)=-1.0

      rnr(10)=5
      snr(10)=6
      a(10)=-1.0

      rnr(11)=5
      snr(11)=5
      a(11)= 1.0

      rnr(12)=5
      snr(12)=4
      a(12)=-5.0

      rnr(13)=4
      snr(13)=4
      a(13)=10.0

      rnr(14)=4
      snr(14)=5
      a(14)=-1.0

      rnr(15)=3
      snr(15)=3
      a(15)=15.0
c
c  Factor the matrix.
c
      nonz = nonz_max

      call y12mwe(n,nonz,a,snr,nn,rnr,nn1,pivot,ha,iha,aflag,
     &  iflag,ifail)

      write(*,*)' '
      write(*,*)'Y12MWE error flag IFAIL=',ifail
c
c   Solve system #1.
c
      b(1)= 10.0
      b(2)= 11.0
      b(3)= 45.0
      b(4)= 33.0
      b(5)=-22.0
      b(6)= 31.0

      iflag(5)=3
      call y12mde(n,a,nn,b,pivot,snr,ha,iha,iflag,ifail)
c
c  Print the results.
c
      write(*,*)' '
      write(*,*)'Y12MDE error flag IFAIL=',ifail

      if(ifail.eq.0)then

        write(*,*)' '
        write(*,*)'The solution vector:'
        write(*,*)' '

        do i=1,n
          write(*,*)i,b(i)
        enddo

        write(*,*)' '

        write(*,*)'The largest element in the original matrix '
     &    //'is AFLAG(6)=',aflag(6)

        write(*,*)'The largest element during elimination was '
     &    //'AFLAG(7)=',aflag(7)

        write(*,*)'The minimal pivot element was AFLAG(8)=',aflag(8)

        write(*,*)'The growth factor was AFLAG(5)=',aflag(5)

        write(*,*)'The number of collections in the row list ',
     &    iflag(6)

        write(*,*)'The number of collections in the column list ',
     &    iflag(7)

        write(*,*)'The largest number of elements found in A ',
     &    iflag(8)

      endif

      b(1)= 60.0
      b(2)= 45.0
      b(3)= 60.0
      b(4)= 16.0
      b(5)=-20.0
      b(6)=-10.0
c
c  Solve the system.
c
      iflag(5)=3
      call y12mde(n,a,nn,b,pivot,snr,ha,iha,iflag,ifail)
c
c  Print the results.
c
      write(*,*)' '
      write(*,*)'Solve the second linear system.'
      write(*,*)' '
      write(*,*)'Y12MDE error flag IFAIL=',ifail

      if(ifail.eq.0)then

        write(*,*)' '
        write(*,*)'The solution vector:'
        write(*,*)' '

        do i=1,n
          write(*,*)i,b(i)
        enddo

        write(*,*)' '

        write(*,*)'The largest element in the original matrix '
     &    //'is AFLAG(6)=',aflag(6)

        write(*,*)'The largest element during elimination was '
     &    //'AFLAG(7)=',aflag(7)

        write(*,*)'The minimal pivot element was AFLAG(8)=',aflag(8)

        write(*,*)'The growth factor was AFLAG(5)=',aflag(5)

        write(*,*)'The number of collections in the row list ',
     &    iflag(6)

        write(*,*)'The number of collections in the column list ',
     &    iflag(7)

        write(*,*)'The largest number of elements found in A ',
     &    iflag(8)

      endif

      return
      end

