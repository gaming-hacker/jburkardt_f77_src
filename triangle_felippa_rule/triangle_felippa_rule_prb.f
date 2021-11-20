      program main

c*********************************************************************72
c
cc MAIN is the main program for FELIPPA_PRB.
c
c  Discussion:
c
c    FELIPPA_PRB tests the FELIPPA library.
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

      integer degree_max

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FELIPPA_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the FELIPPA library.'

      degree_max = 4
      call triangle_unit_monomial_test ( degree_max )

      degree_max = 7
      call triangle_unit_quad_test ( degree_max )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FELIPPA_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine triangle_unit_monomial_test ( degree_max )

c*********************************************************************72
c
cc TRIANGLE_UNIT_MONOMIAL_TEST tests TRIANGLE_UNIT_MONOMIAL.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 April 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer DEGREE_MAX, the maximum total degree of the
c    monomials to check.
c
      implicit none

      integer alpha
      integer beta
      integer degree_max
      integer expon(2)
      double precision triangle_unit_volume
      double precision value

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TRIANGLE_UNIT_MONOMIAL_TEST'
      write ( *, '(a)' ) '  For the unit triangle,'
      write ( *, '(a)' ) 
     &  '  TRIANGLE_UNIT_MONOMIAL returns the exact value of the'
      write ( *, '(a)' ) '  integral of X^ALPHA Y^BETA'
      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  Volume = ', triangle_unit_volume ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     ALPHA      BETA      INTEGRAL'
      write ( *, '(a)' ) ' '

      do alpha = 0, degree_max
        expon(1) = alpha
        do beta = 0, degree_max - alpha
          expon(2) = beta
          call triangle_unit_monomial ( expon, value )
          write ( *, '(2x,i8,2x,i8,2x,g14.6)' ) expon(1:2), value
        end do
      end do

      return
      end
      subroutine triangle_unit_quad_test ( degree_max )

c*********************************************************************72
c
cc TRIANGLE_UNIT_QUAD_TEST tests the rules for the unit triangle.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 April 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer DEGREE_MAX, the maximum total degree of the
c    monomials to check.
c
      implicit none

      integer dim_num
      parameter ( dim_num = 2 )
      integer order_max
      parameter ( order_max = 12 )

      integer degree_max
      integer expon(dim_num)
      integer h
      logical more
      integer order
      double precision quad
      double precision r8vec_dot_product
      integer t
      double precision triangle_unit_volume
      double precision v(order_max)
      double precision w(order_max)
      double precision xy(dim_num,order_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TRIANGLE_UNIT_QUAD_TEST'
      write ( *, '(a)' ) '  For the unit triangle,'
      write ( *, '(a)' ) '  we approximate monomial integrals with:'
      write ( *, '(a)' ) '  TRIANGLE_UNIT_O01,'
      write ( *, '(a)' ) '  TRIANGLE_UNIT_O03,'
      write ( *, '(a)' ) '  TRIANGLE_UNIT_O03b,'
      write ( *, '(a)' ) '  TRIANGLE_UNIT_O06,'
      write ( *, '(a)' ) '  TRIANGLE_UNIT_O06b,'
      write ( *, '(a)' ) '  TRIANGLE_UNIT_O07,'
      write ( *, '(a)' ) '  TRIANGLE_UNIT_O012,'

      more = .false.

10    continue

        call subcomp_next ( degree_max, dim_num, expon, more, h, t )

        write ( *, '(a)' ) ' '
        write ( *, '(a,2x,i2,2x,i2)' ) 
     &    '  Monomial exponents: ', expon(1:dim_num)
        write ( *, '(a)' ) ' '

        order = 1
        call triangle_unit_o01 ( w, xy )
        call monomial_value ( dim_num, order, expon, xy, v )
        quad = triangle_unit_volume ( ) 
     &    * r8vec_dot_product ( order, w, v )
        write ( *, '(2x,i6,2x,g14.6)' ) order, quad

        order = 3
        call triangle_unit_o03 ( w, xy )
        call monomial_value ( dim_num, order, expon, xy, v )
        quad = triangle_unit_volume ( ) 
     &    * r8vec_dot_product ( order, w, v )
        write ( *, '(2x,i6,2x,g14.6)' ) order, quad

        order = 3
        call triangle_unit_o03b ( w, xy )
        call monomial_value ( dim_num, order, expon, xy, v )
        quad = triangle_unit_volume ( ) 
     &    * r8vec_dot_product ( order, w, v )
        write ( *, '(2x,i6,2x,g14.6)' ) order, quad

        order = 6
        call triangle_unit_o06 ( w, xy )
        call monomial_value ( dim_num, order, expon, xy, v )
        quad = triangle_unit_volume ( ) 
     &    * r8vec_dot_product ( order, w, v )
        write ( *, '(2x,i6,2x,g14.6)' ) order, quad

        order = 6
        call triangle_unit_o06b ( w, xy )
        call monomial_value ( dim_num, order, expon, xy, v )
        quad = triangle_unit_volume ( ) 
     &    * r8vec_dot_product ( order, w, v )
        write ( *, '(2x,i6,2x,g14.6)' ) order, quad

        order = 7
        call triangle_unit_o07 ( w, xy )
        call monomial_value ( dim_num, order, expon, xy, v )
        quad = triangle_unit_volume ( ) 
     &    * r8vec_dot_product ( order, w, v )
        write ( *, '(2x,i6,2x,g14.6)' ) order, quad

        order = 12
        call triangle_unit_o12 ( w, xy )
        call monomial_value ( dim_num, order, expon, xy, v )
        quad = triangle_unit_volume ( ) 
     &    * r8vec_dot_product ( order, w, v )
        write ( *, '(2x,i6,2x,g14.6)' ) order, quad

        write ( *, '(a)' ) ' '
        call triangle_unit_monomial ( expon, quad )
        write ( *, '(2x,a,2x,g14.6)' ) ' Exact', quad

        if ( .not. more ) then
          go to 20
        end if

      go to 10

20    continue

      return
      end
