      program main

c*********************************************************************72
c
cc MAIN is the main program for MGRID.
c
c  Modified:
c
c    21 November 2011
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer k

      call timestamp ( )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'MGRID:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the MGRID multigrid library.'

      do k = 2, 8
        call mgrid ( k )
      end do
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'MGRID:'
      write ( *, '(a)' ) '  Normal end of execution.'

      write ( *, '(a)' ) ' '
      call timestamp ( )
      stop
      end
      subroutine mgrid ( k )

c*********************************************************************72
c                                                    
cc MGRID solves a 1D PDE using the multigrid method.
c
c  Discussion:
c
c    Solves  -x''(t) = 1, 
c    with the boundary condtions x(0) = x(1) = 0  by the multigrid method.  
c
c  Modified:
c
c    16 November 2011
c
c  Author:
c
c    William Hager
c
c  Reference:
c
c    William Hager,
c    Applied Numerical Linear Algebra,
c    Prentice-Hall, 1988,
c    ISBN13: 978-0130412942,
c    LC: QA184.H33.
c
c  Parameters:
c
      implicit none

      real d0
      real d1
      real difmax
      integer i
      integer it
      integer j
      integer k
      integer l
      integer ll
      integer m
      integer n
      integer nl
      real r(518)
      real s
      real tol
      real u(518)
      real utol

      write ( *, * ) ' '
      write ( *, * ) 'MGRID'
      write ( *, * ) '  example of multigrid method.'
      write ( *, * ) '  solve -u''(t)=1, u(0)=u(1)=0'
      write ( *, * ) '  solution is u(t)=0.5*(-t*t+t)'
      write ( *, * ) ' '

      write ( *, '(a,i4)' ) '  Mesh index K = ', k
      n = 2**k
      write ( *, '(a,i6)' ) '  Number of intervals N=2^K = ', n
      it = 4
      tol = 0.0001
      utol = 0.7
c 
c  Set the right hand side.
c 
      do i = 1, n
        r(i) = 1.0E+00
      end do
c 
c  Initialize.
c 
      s = (1.0/n)**2
      do i = 1, n
        r(i) = s * r(i)
      end do
      m = n
      l = 1
      ll = n
      nl = n + n + k - 2
      do i = 1, nl
        u(i) = 0.0E+00
      end do
      d1 = 0.0E+00
c 
c  Gauss-seidel iteration
c 
10    continue

      j = 0

50    continue

      d0 = d1
      d1 = 0.0E+00
      j = j + 1
      i = l

60    continue

        i = i + 1
        s = 0.5E+00 * ( u(i-1) + u(i+1) + r(i) )
        d1 = d1 + abs ( s - u(i) )
        u(i) = s
        if ( i .lt. ll ) then
          go to 60
        end if

      write(6,70) d1
70    format(' dif:',f20.10)

      if ( j .lt. it ) goto 50

      if ( d1  .lt.  tol ) then
        write ( *, * ) '  Time to go up!'
        go to 100
      end if

      if ( d1 / d0 .lt. utol ) then
        write ( *, * ) '  D1/D0 = ', d1/d0
        goto 50
      end if

      if ( n .eq. 2 ) then
        write ( *, *) 'HEY WHAT THE HELL?'
        go to 50
      end if
c
c  coarser mesh (slow convergence)
c
      i = ll + 2

80    continue

      l = l + 2
      i = i + 1

      if ( l .le. ll ) then
        u(i) = 0.0E+00
        r(i) = 4.0E+00 * ( r(l) + u(l-1) - 2.0E+00 * u(l) + u(l+1) )
        go to 80
      end if

      n = n / 2
      write(6,*) '  Go down to mesh intervals:', n
      ll = ll + n + 1
      l = l + 1
      go to 10
c     
c  finer mesh (fast convergence)
c     
100   if ( n .eq. m ) goto 120
      i = l - 3
      j = ll

110   continue

        u(i) = u(i) + u(j)
        u(i+1) = u(i+1) + 0.5E+00 * ( u(j) + u(j+1) )
        i = i - 2
        j = j - 1

        if ( j .gt. l ) then
          go to 110
        end if

      n = n + n

      write(6,*) '  Go up to mesh intervals:',n

      ll = l - 2
      l = i
      u(i+1) = u(i+1) + 0.5E+00 * u(j+1)

      go to 10
c 
c  Computation completed.
c 
120   continue

      do i = 1, n + 1
        s = real ( i - 1 ) / real ( n )
        write(6,'(i5,f10.5,f15.6)') i, s, u(i)
      end do

      difmax = 0.0E+00
      do i = 1, n + 1
        s = real ( i - 1 ) / real ( n )
        difmax=max ( difmax, abs ( u(i) - 0.5 * ( - s * s + s ) ) )
      end do
      write ( *, * ) 'maximum error=',difmax

      return
      end
