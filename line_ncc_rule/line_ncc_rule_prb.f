      program main

c*********************************************************************72
c
cc MAIN is the main program for LINE_NCC_RULE_PRB.
c
c  Discussion:
c
c    LINE_NCC_RULE_PRB tests the LINE_NCC_RULE library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    09 April 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LINE_NCC_RULE_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version:'
      write ( *, '(a)' ) '  Test the LINE_NCC_RULE library.'

      call test01 ( )
      call test02 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'LINE_NCC_RULE_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 computes and prints NCC rules.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    09 April 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 12 )

      double precision a
      double precision b
      integer i
      integer n
      double precision w(n_max)
      double precision w_sum
      double precision x(n_max)

      a = -1.0D+00
      b = +1.0D+00

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) 
     &  '  LINE_NCC_RULE computes the Newton-Cotes (closed) rule'
      write ( *, '(a)' ) 
     &  '  using N equally spaced points for an interval [A,B].'

      do n = 1, n_max

        call line_ncc_rule ( n, a, b, x, w )
        write ( *, '(a)' ) ''
        write ( *, '(a,i2)' ) '  Newton-Cotes (Closed) Rule #', n
        write ( *, '(a)' ) '   I       X(I)            W(I)'
        write ( *, '(a)' ) ' '
        w_sum = 0.0D+00
        do i = 1, n
          write ( *, '(2x,i2,2x,g14.6,2x,g14.6)' ) i, x(i), w(i)
          w_sum = w_sum + abs ( w(i) )
        end do
        write ( *, '(2x,2x,2x,a,2x,g14.6)' ) '  Sum(|W)|) = ', w_sum

      end do

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 uses NCC rules to estimate the integral of exp(x) from 0 to 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    09 April 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 22 )

      double precision a
      double precision b
      double precision error
      double precision exact
      integer i
      integer n
      double precision q
      double precision w(n_max)
      double precision x(n_max)

      a =  0.0D+00
      b = +1.0D+00
      exact = exp ( b ) - exp ( a )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) '  Use a sequence of NCC rules to compute an'
      write ( *, '(a)' ) '  estimate Q of the integral:'
      write ( *, '(a)' ) '    I = integral ( 0 <= x <= 1 ) exp(x) dx.'
      write ( *, '(a)' ) '  The exact value is:'
      write ( *, '(a,g14.6)' ) '    I = ', exact

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   N       Q             |Q-I|'
      write ( *, '(a)' ) ' '

      do n = 1, n_max

        call line_ncc_rule ( n, a, b, x, w )

        q = 0.0D+00
        do i = 1, n
          q = q + w(i) * exp ( x(i) )
        end do
        error = abs ( exact - q )
        write ( *, '(2x,i2,2x,g14.6,2x,e14.6)' ) n, q, error

      end do

      return
      end