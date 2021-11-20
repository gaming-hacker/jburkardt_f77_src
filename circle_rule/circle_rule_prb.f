      program main

c*********************************************************************72
c
cc MAIN is the main program for CIRCLE_RULE_PRB.
c
c  Discussion:
c
c    CIRCLE_RULE_PRB tests the CIRCLE_RULE library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 April 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer nt

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'CIRCLE_RULE:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the CIRCLE_RULE library.'

      nt = 8
      call test01 ( nt )

      nt = 32
      call test01 ( nt )
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'CIRCLE_RULE:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      stop
      end
      subroutine test01 ( nt )

c*********************************************************************72
c
cc TEST01 tests CIRCLE_RULE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 April 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer nt

      integer e(2)
      integer e1
      integer e2
      double precision exact
      integer i
      double precision q
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision t(nt)
      double precision w(nt)
      double precision x
      double precision y

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) 
     &  '  CIRCLE_RULE can compute a rule Q(f) for the unit circle'
      write ( *, '(a)' ) '  using NT equally spaced angles.'
      write ( *, '(a)' ) 
     &  '  Estimate integrals I(f) where f = x^e(1) * y^e(2)'
      write ( *, '(a,i4,a)' ) '  using ', nt, ' points.'
c
c  Compute the quadrature rule.
c
      call circle_rule ( nt, w, t )
c
c  Apply it to integrands.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  E(1)  E(2)    I(f)            Q(f)' 
      write ( *, '(a)' ) ' '
c
c  Specify a monomial.
c
      do e1 = 0, 6, 2

        e(1) = e1

        do e2 = e1, 6, 2

          e(2) = e2

          q = 0.0D+00
          do i = 1, nt
            x = cos ( t(i) )
            y = sin ( t(i) )
            q = q + w(i) * x ** e(1) * y ** e(2)
          end do

          q = 2.0D+00 * r8_pi * q

          call circle01_monomial_integral ( e, exact )

          write ( *, '(3x,i2,3x,i2,2x,g14.6,2x,g14.6)' ) 
     &      e(1), e(2), exact, q

        end do

      end do

      return
      end

