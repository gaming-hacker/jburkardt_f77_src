      program main

c*********************************************************************72
c
cc MAIN is the main program for NEWTON_RC_PRB.
c
c  Discussion:
c
c    NEWTON_RC_PRB tests the NEWTON_RC library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 December 2008
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'NEWTON_RC_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the REVNEW library.'
      write ( *, '(a)' ) '  which solves a nonlinear equation'
      write ( *, '(a)' ) '  using reverse communication.'

      call test01 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'NEWTON_RC_PRB:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 calls NEWTON_RC for a specific problem.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 January 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 4 )

      double precision fx(n)
      double precision fxtol
      integer i
      integer ido
      double precision x(n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  Seek solution of F(X) = 0'
c
c  Initialization.
c
      ido = 0
      fxtol = 0.000001D+00
      do i = 1, n
        x(i) = 0.0D+00
      end do

      do i = 1, n
        fx(i) = ( x(i) - dble ( i ) ) * ( x(i) - dble ( i ) )
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Initial Values:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     I      X               FX'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i4,2x,g14.6,2x,g14.6)' ) i, x(i), fx(i)
      end do
      write ( *, '(a)' ) ' '

      fx(1) = fxtol
c
c  The solution loop.
c
10    continue

      call newton_rc ( fx, ido, n, x )

      if ( ido .eq. 0 ) then

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  Convergence:'
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '     I      X               FX'
        write ( *, '(a)' ) ' '
        do i = 1, n
          write ( *, '(2x,i4,2x,g14.6,2x,g14.6)' ) i, x(i), fx(i)
        end do

        go to 20

      else if ( ido .eq. 1 .or. ido .eq. 2 ) then

        do i = 1, n
          fx(i) = ( x(i) - dble ( i ) ) * ( x(i) - dble ( i ) )
        end do

        go to 10

      end if

20    continue

      return
      end
