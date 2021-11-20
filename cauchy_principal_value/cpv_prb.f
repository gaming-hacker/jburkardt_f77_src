      program main

c*********************************************************************72
c
cc CPV_PRB tests the CPV library.
c
c  Location:
c
c    http://people.sc.fsu.edu/~jburkardt/f77_src/cauchy_principal_value/cpv_prb.f
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 April 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'CPV_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the CPV library.'

      call cpv_test01 ( )
      call cpv_test02 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'CPV_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      return
      end
      subroutine cpv_test01 ( )

c*********************************************************************72
c
cc CPV_TEST01 seeks the CPV of Integral ( -1 <= t <= 1 ) exp(t) / t dt
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 April 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision b
      double precision cpv
      double precision exact
      double precision f01
      external f01
      integer n
      double precision value

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'CPV_TEST01:'
      write ( *, '(a)' ) 
     &  '  CPV of Integral ( -1 <= t <= 1 ) exp(t) / t dt'

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '   N           Estimate             Error'
      write ( *, '(a)' ) ''

      exact = 2.11450175075D+00
      a = -1.0D+00
      b = +1.0D+00
      do n = 2, 8, 2
        value = cpv ( f01, a, b, n )
        write ( *, '(2x,i2,2x,g24.16,2x,g14.6)' ) 
     &    n, value, abs ( value - exact )
      end do

      return
      end
      function f01 ( t )

c*********************************************************************72
c
cc F01 evaluates the integrand of Integral ( -1 <= t <= 1 ) exp(t) / t dt
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 April 2015
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, real T, the argument.
c
c    Output, real VALUE, the value of the integrand.
c
      implicit none

      double precision f01
      double precision t

      f01 = exp ( t )

      return
      end
      subroutine cpv_test02 ( )

c*********************************************************************72
c
cc CPV_TEST02 is another test.
c
c  Discussion:
c
c    We seek
c      CPV ( Integral ( 1-delta <= t <= 1+delta ) 1/(1-t)^3 dt )
c    which we must rewrite as
c      CPV ( Integral ( 1-delta <= t <= 1+delta ) 1/(1+t+t^2) 1/(1-t) dt )
c    so that our "integrand" is 1/(1+t+t^2).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 April 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision b
      double precision cpv
      double precision delta
      double precision exact
      double precision f02
      external f02
      integer k
      integer n
      double precision r1
      double precision r2
      double precision r3
      double precision r4
      double precision value

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'CPV_TEST02:'
      write ( *, '(a)' ) 
     &  '  CPV ( Integral ( 1-delta <= t <= 1+delta ) 1/(1-t)^3 dt )'
      write ( *, '(a)' ) '  Try this for delta = 1, 1/2, 1/4.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '   N          Estimate                  ' //
     &  'Exact                  Error'
      delta = 1.0D+00
      do k = 1, 3
        write ( *, '(a)' ) ''
        r1 = (   delta + 1.5D+00 ) ** 2 + 0.75D+00
        r2 = ( - delta + 1.5D+00 ) ** 2 + 0.75D+00
        r3 = atan ( sqrt ( 0.75D+00 ) / (   delta + 1.5D+00 ) )
        r4 = atan ( sqrt ( 0.75D+00 ) / ( - delta + 1.5D+00 ) )
        exact = - log ( r1 / r2 ) / 6.0D+00 + ( r3 - r4 ) / 
     &    sqrt ( 3.0D+00 )
        do n = 2, 8, 2
          a = 1.0D+00 - delta
          b = 1.0D+00 + delta
          value = cpv ( f02, a, b, n )
          write ( *, '(2x,i2,g24.16,2x,g24.16,2x,g14.6)' )
     &      n, value, exact, abs ( exact - value )
        end do
        delta = delta / 2.0D+00
      end do

      return
      end
      function f02 ( t )

c*********************************************************************72
c
cc F02: integrand of Integral ( 1-delta <= t <= 1+delta ) 1/(1-t^3) dt
c
c  Discussion:
c
c    1/(1-t^3) = 1/(1+t+t^2) * 1/(1-t)
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 April 2015
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
      implicit none

      double precision f02
      double precision t

      f02 = 1.0D+00 / ( 1.0D+00 + t + t * t )

      return
      end
