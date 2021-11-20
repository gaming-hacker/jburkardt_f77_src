      program main

c*********************************************************************72
c
cc MAIN is the main program for COLNEW_PRB.
c
c  Discussion:
c
c    COLNEW_PRB tests the COLNEW library.
c
c  Modified:
c
c    10 March 2013
c
c  Reference:
c
c    Uri Ascher, Robert Mattheij, Robert Russell,
c    Numerical Solution of Boundary Value Problems for
c    Ordinary Differential Equations,
c    Prentice Hall, 1988,
c    ISBN: 0-13-627266-5,
c    LC: QA379.A83.
c
      implicit none

      double precision aleft
      double precision aright
      external dfsub
      external dgsub
      double precision fixpnt
      double precision fspace(52000)
      external fsub
      external gsub
      external guess
      integer i
      integer iflag
      integer ipar(11)
      integer ispace(3000)
      integer j
      integer ltol(3)
      integer m(2)
      integer ncomp
      integer n_out
      double precision tol(3)
      double precision x
      double precision z(6)
      double precision zeta(5)

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'COLNEW_PRB:'
      write ( *, '(a)' ) '  FORTRAN77 version.'
      write ( *, '(a)' ) '  Test the COLNEW library.'

      ncomp = 2
      m(1) = 1
      m(2) = 4
      aleft = 0.0D+00
      aright = 1.0D+00

      zeta(1) = 0.0D+00
      zeta(2) = 0.0D+00
      zeta(3) = 0.0D+00
      zeta(4) = 1.0D+00
      zeta(5) = 1.0D+00

      ipar(1) = 0
      ipar(2) = 5
      ipar(3) = 10
      ipar(4) = 3
      ipar(5) = 52000
      ipar(6) = 3000
      ipar(7) = - 1
      ipar(8) = 0
      ipar(9) = 0
      ipar(10) = 0
      ipar(11) = 0

      ltol(1) = 1
      ltol(2) = 2
      ltol(3) = 4

      tol(1) = 1.0D-07
      tol(2) = 1.0D-07
      tol(3) = 1.0D-07

      call colsys ( ncomp, m, aleft, aright, zeta, ipar, ltol,
     &  tol, fixpnt, ispace, fspace, iflag,
     &  fsub, dfsub, gsub, dgsub, guess )
c
c  On return, the arrays FSPACE and ISPACE contain information specifying
c  the approximate solution, which can be evaluated at any point by
c  callying APPSLN.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Approximate solution sampled in interval'
      write ( *, '(a)' ) ' '
      n_out = 11
      do i = 1, n_out

        x = ( dble ( n_out - i     ) * aleft 
     &      + dble (         i - 1 ) * aright ) 
     &      / dble ( n_out - 1     )

        call appsln ( x, z, fspace, ispace )

        write ( *, '(f8.4,6g12.4)' ) x, ( z(j), j = 1, 5 )

      end do
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'COLNEW_PRB:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine guess ( x, z, dmval )

c*********************************************************************72
c
cc GUESS defines an initial approximate solution.
c
c  Modified:
c
c    10 March 2013
c
      implicit none

      double precision dmval(*)
      double precision x
      double precision z(*)

      return
      end
      subroutine fsub ( x, z, f )

c*********************************************************************72
c
cc FSUB
c
c  Modified:
c
c    10 March 2013
c
      implicit none

      double precision eps
      double precision f(2)
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision pix
      double precision x
      double precision z(5)

      eps = 1.0D-04
      pix = pi * x

      f(1) = ( - z(1) + z(2) + cos ( pix ) 
     &         - ( 1.0D+00 + eps * pi ) * sin ( pix ) 
     &       ) / eps

      f(2) = z(1) + z(2) + ( pi ** 4 - 1.0D+00 ) * sin ( pix )
     &  - cos ( pix ) - exp ( - x / eps )

      return
      end
      subroutine dfsub ( x, z, df )

c*********************************************************************72
c
cc DFSUB
c
c  Modified:
c
c    10 March 2013
c
      implicit none

      double precision df(2,5)
      double precision eps
      integer i
      integer j
      double precision x
      double precision z(5)

      eps = 1.0D-04

      do j = 1, 5
        do i = 1, 2
          df(i,j) = 0.0D+00
        end do
      end do

      df(1,1) = - 1.0D+00 / eps
      df(1,2) = 1.0D+00 / eps
      df(2,1) = 1.0D+00
      df(2,2) = 1.0D+00

      return
      end
      subroutine gsub ( i, z, g )

c*********************************************************************72
c
cc GSUB evaluates the I-th side condition.
c
c  Modified:
c
c    10 March 2013
c
      implicit none

      double precision g
      integer i
      double precision z(5)

      if ( i == 1 ) then
        g = z(1) - 2.0D+00
      else if ( i == 2 ) then
        g = z(2)
      else if ( i == 3 ) then
        g = z(4)
      else if ( i == 4 ) then
        g = z(2)
      else if ( i == 5 ) then
        g = z(4)
      end if

      return
      end
      subroutine dgsub ( i, z, dg )

c*********************************************************************72
c
cc DGSUB evaluates the I-th row of the jacobian of G.
c
c  Modified:
c
c    10 March 2013
c
      implicit none

      double precision dg(5)
      integer i
      integer j
      double precision z(5)

      do j = 1, 5
        dg(j) = 0.0D+00
      end do

      if ( i == 1 ) then
        dg(1) = 1.0D+00
      else if ( i == 2 ) then
        dg(2) = 1.0D+00
      else if ( i == 3 ) then
        dg(4) = 1.0D+00
      else if ( i == 4 ) then
        dg(2) = 1.0D+00
      else if ( i == 5 ) then
        dg(4) = 1.0D+00
      end if

      return
      end
