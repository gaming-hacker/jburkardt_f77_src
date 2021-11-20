      program main

c*********************************************************************72
c
cc MAIN is the main program for TETRAHEDRON_FELIPPA_RULE_PRB.
c
c  Discussion:
c
c    TETRAHEDRON_FELIPPA_RULE_PRB tests the TETRAHEDRON_FELIPPA_RULE library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 August 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer degree_max

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TETRAHEDRON_FELIPPA_RULE_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the TETRAHEDRON_FELIPPA_RULE library.'

      degree_max = 4
      call tetrahedron_unit_monomial_test ( degree_max )

      degree_max = 4
      call tetrahedron_unit_quad_test ( degree_max )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TETRAHEDRON_FELIPPA_RULE_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine tetrahedron_unit_monomial_test ( degree_max )

c*********************************************************************72
c
cc TETRAHEDRON_UNIT_MONOMIAL_TEST tests TETRAHEDRON_UNIT_MONOMIAL.
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
      integer expon(3)
      integer gamma
      double precision tetrahedron_unit_volume
      double precision value

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TETRAHEDRON_UNIT_MONOMIAL_TEST'
      write ( *, '(a)' ) '  For the unit tetrahedron,'
      write ( *, '(a)' ) 
     &  '  TETRAHEDRON_UNIT_MONOMIAL returns the exact value of the'
      write ( *, '(a)' ) '  integral of X^ALPHA Y^BETA Z^GAMMA'
      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) 
     &  '  Volume = ', tetrahedron_unit_volume ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     ALPHA      BETA     GAMMA      INTEGRAL'
      write ( *, '(a)' ) ' '

      do alpha = 0, degree_max
        expon(1) = alpha
        do beta = 0, degree_max - alpha
          expon(2) = beta
          do gamma = 0, degree_max - alpha - beta
            expon(3) = gamma
            call tetrahedron_unit_monomial ( expon, value )
            write ( *, '(2x,i8,2x,i8,2x,i8,2x,g14.6)' ) 
     &        expon(1:3), value
          end do
        end do
      end do

      return
      end
      subroutine tetrahedron_unit_quad_test ( degree_max )

c*********************************************************************72
c
cc TETRAHEDRON_UNIT_QUAD_TEST tests the rules for the unit tetrahedron.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 April 2008
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
      parameter ( dim_num = 3 )
      integer order_max
      parameter ( order_max = 24 )

      integer degree_max
      integer expon(dim_num)
      integer h
      logical more
      integer order
      double precision quad
      double precision r8vec_dot_product
      integer t
      double precision tetrahedron_unit_volume
      double precision v(order_max)
      double precision w(order_max)
      double precision xyz(dim_num,order_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TETRAHEDRON_UNIT_QUAD_TEST'
      write ( *, '(a)' ) '  For the unit tetrahedron,'
      write ( *, '(a)' ) '  we approximate monomial integrals with:'
      write ( *, '(a)' ) '  TETRAHEDRON_UNIT_O01,'
      write ( *, '(a)' ) '  TETRAHEDRON_UNIT_O04,'
      write ( *, '(a)' ) '  TETRAHEDRON_UNIT_O08,'
      write ( *, '(a)' ) '  TETRAHEDRON_UNIT_O08b,'
      write ( *, '(a)' ) '  TETRAHEDRON_UNIT_O14,'
      write ( *, '(a)' ) '  TETRAHEDRON_UNIT_O14b,'
      write ( *, '(a)' ) '  TETRAHEDRON_UNIT_O15,'
      write ( *, '(a)' ) '  TETRAHEDRON_UNIT_O15b,'
      write ( *, '(a)' ) '  TETRAHEDRON_UNIT_O24.'

      more = .false.

10    continue

        call subcomp_next ( degree_max, dim_num, expon, more, h, t )

        write ( *, '(a)' ) ' '
        write ( *, '(a,2x,i2,2x,i2,2x,i2)' )
     &    '  Monomial exponents: ', expon(1:dim_num)
        write ( *, '(a)' ) ' '

        order = 1
        call tetrahedron_unit_o01 ( w, xyz )
        call monomial_value ( dim_num, order, expon, xyz, v )
        quad = tetrahedron_unit_volume ( ) * 
     &    r8vec_dot_product ( order, w, v )
        write ( *, '(2x,i6,2x,g14.6)' ) order, quad

        order = 4
        call tetrahedron_unit_o04 ( w, xyz )
        call monomial_value ( dim_num, order, expon, xyz, v )
        quad = tetrahedron_unit_volume ( ) * 
     &    r8vec_dot_product ( order, w, v )
        write ( *, '(2x,i6,2x,g14.6)' ) order, quad

        order = 8
        call tetrahedron_unit_o08 ( w, xyz )
        call monomial_value ( dim_num, order, expon, xyz, v )
        quad = tetrahedron_unit_volume ( ) * 
     &    r8vec_dot_product ( order, w, v )
        write ( *, '(2x,i6,2x,g14.6)' ) order, quad

        order = 8
        call tetrahedron_unit_o08b ( w, xyz )
        call monomial_value ( dim_num, order, expon, xyz, v )
        quad = tetrahedron_unit_volume ( ) * 
     &    r8vec_dot_product ( order, w, v )
        write ( *, '(2x,i6,2x,g14.6)' ) order, quad

        order = 14
        call tetrahedron_unit_o14 ( w, xyz )
        call monomial_value ( dim_num, order, expon, xyz, v )
        quad = tetrahedron_unit_volume ( ) * 
     &    r8vec_dot_product ( order, w, v )
        write ( *, '(2x,i6,2x,g14.6)' ) order, quad

        order = 14
        call tetrahedron_unit_o14b ( w, xyz )
        call monomial_value ( dim_num, order, expon, xyz, v )
        quad = tetrahedron_unit_volume ( ) * 
     &    r8vec_dot_product ( order, w, v )
        write ( *, '(2x,i6,2x,g14.6)' ) order, quad

        order = 15
        call tetrahedron_unit_o15 ( w, xyz )
        call monomial_value ( dim_num, order, expon, xyz, v )
        quad = tetrahedron_unit_volume ( ) * 
     &    r8vec_dot_product ( order, w, v )
        write ( *, '(2x,i6,2x,g14.6)' ) order, quad

        order = 15
        call tetrahedron_unit_o15b ( w, xyz )
        call monomial_value ( dim_num, order, expon, xyz, v )
        quad = tetrahedron_unit_volume ( ) * 
     &    r8vec_dot_product ( order, w, v )
        write ( *, '(2x,i6,2x,g14.6)' ) order, quad

        order = 24
        call tetrahedron_unit_o24 ( w, xyz )
        call monomial_value ( dim_num, order, expon, xyz, v )
        quad = tetrahedron_unit_volume ( ) * 
     &    r8vec_dot_product ( order, w, v )
        write ( *, '(2x,i6,2x,g14.6)' ) order, quad

        write ( *, '(a)' ) ' '
        call tetrahedron_unit_monomial ( expon, quad )
        write ( *, '(2x,a,2x,g14.6)' ) ' Exact', quad

        if ( .not. more ) then
          go to 20
        end if

      go to 10

20    continue

      return
      end




