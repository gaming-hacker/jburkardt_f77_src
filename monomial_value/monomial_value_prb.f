      program main

c*********************************************************************72
c
cc MAIN is the main program for MONOMIAL_VALUE_PRB.
c
c  Discussion:
c
c    MONOMIAL_VALUE_PRB tests the MONOMIAL_VALUE library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 August 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'MONOMIAL_VALUE_PRB'
      write ( *, '(a)' ) '  FORTRAN90 version'
      write ( *, '(a)' ) '  Test the MONOMIAL_VALUE library.'

      call test01 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'MONOMIAL_VALUE_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 tests MONOMIAL_VALUE on sets of data in various dimensions.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 August 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      integer m_max
      parameter ( m_max = 3 )
      integer n
      parameter ( n = 5 )

      integer e(m_max)
      integer e_max
      integer e_min
      integer i
      integer j
      integer seed
      double precision v(n)
      double precision x(m_max*n)
      double precision x_max
      double precision x_min

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  Usine monomial_value to evaluate monomials'
      write ( *, '(a)' ) '  in dimensions 1 through 3.' 

      e_min = -3
      e_max = 6
      seed = 123456789
      x_min = -2.0D+00
      x_max = +10.0D+00

      do m = 1, m_max

        write ( *, '(a)' ) ''
        write ( *, '(a,i1)' ) '  Spatial dimension M = ', m

        call i4vec_uniform_ab ( m, e_min, e_max, seed, e )
        call i4vec_transpose_print ( m, e, '  Exponents:' )
        call r8mat_uniform_ab ( m, n, x_min, x_max, seed, x )
c
c  To make checking easier, make the X values integers.
c
        call r8mat_nint ( m, n, x )
        call monomial_value ( m, n, e, x, v )

        write ( *, '(a)' ) ''
        write ( *, '(a)', advance = 'no' ) '   V(X)         '
        do i = 1, m
          write ( *, '(a,i1,a)', advance = 'no' ) '      X(', i, ')'
        end do
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) ''
        do j = 1, n
          write ( *, '(g14.6,2x)', advance = 'no' ) v(j)
          do i = 1, m
            write ( *, '(2x,f10.4)', advance = 'no' ) x(i+(j-1)*m)
          end do
          write ( *, '(a)' ) ''
        end do

      end do

      return
      end
