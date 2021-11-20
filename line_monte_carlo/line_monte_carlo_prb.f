      program main

c*********************************************************************72
c
cc MAIN is the main program for LINE_MONTE_CARLO_PRB.
c
c  Discussion:
c
c    LINE_MONTE_CARLO_PRB tests the LINE_MONTE_CARLO library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 January 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LINE_MONTE_CARLO_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the LINE_MONTE_CARLO library.'

      call test01 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LINE_MONTE_CARLO_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 uses Monte Carlo sampling to estimate integrals in 1D.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 January 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 65536 )

      integer e
      double precision error
      double precision exact
      integer j
      double precision line01_length
      integer n
      double precision r8vec_sum
      double precision result(7)
      integer seed
      double precision value(n_max)
      double precision x(n_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  Use LINE01_SAMPLE to estimate integrals '
      write ( *, '(a)' ) '  along the length of the unit line in 1D.'

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '         N' // 
     &  '        1' //
     &  '               X' // 
     &  '              X^2' //
     &  '             X^3' // 
     &  '             X^4' // 
     &  '             X^5' // 
     &  '           X^6'
      write ( *, '(a)' ) ' '

      n = 1

10    continue

      if ( n .le. 65536 ) then

        call line01_sample ( n, seed, x )

        do j = 1, 7

          e = j - 1

          call monomial_value_1d ( n, e, x, value )

          result(j) = line01_length ( ) * r8vec_sum ( n, value )
     &      / dble ( n )

        end do

        write ( *, '(2x,i8,7(2x,g14.6))' ) n, result(1:7)

        n = 2 * n

        go to 10

      end if

      write ( *, '(a)' ) ' '

      do j = 1, 7

        e = j - 1

        call line01_monomial_integral ( e, result(j) )

      end do

      write ( *, '(2x,a8,7(2x,g14.6))' ) '   Exact', result(1:7)

      return
      end
