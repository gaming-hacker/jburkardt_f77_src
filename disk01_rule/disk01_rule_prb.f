      program main

c*********************************************************************72
c
cc MAIN is the main program for DISK01_RULE_PRB.
c
c  Discussion:
c
c    DISK01_RULE_PRB tests the DISK01_RULE library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 March 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'DISK01_RULE:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the DISK01_RULE library.'

      call test01 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'DISK01_RULE:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 tests DISK01_RULE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 March 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer nr
      parameter ( nr = 4 )
      integer nt
      parameter ( nt = 8 )

      integer e(2)
      integer e1
      integer e2
      double precision exact
      integer i
      integer j
      double precision q
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision r(nr)
      double precision t(nt)
      double precision w(nr)
      double precision x
      double precision y

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) 
     &  '  DISK01_RULE can compute a rule Q(f) for the unit disk'
      write ( *, '(a)' ) 
     &  '  using NT equally spaced angles and NR radial distances.'
      write ( *, '(a)' ) ''
      write ( *, '(a,i4)' ) '  NT = ', nt
      write ( *, '(a,i4)' ) '  NR = ', nr
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  Estimate integrals I(f) where f = x^e(1) * y^e(2).'
c
c  Compute the quadrature rule.
c
      call disk01_rule ( nr, nt, w, r, t )
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
          do j = 1, nt
            do i = 1, nr
              x = r(i) * cos ( t(j) )
              y = r(i) * sin ( t(j) )
              q = q + w(i) * x ** e(1) * y ** e(2)
            end do
          end do

          q = r8_pi * q

          call disk01_monomial_integral ( e, exact )

          write ( *, '(3x,i2,3x,i2,2x,g14.6,2x,g14.6)' ) 
     &      e(1), e(2), exact, q

        end do

      end do

      return
      end

