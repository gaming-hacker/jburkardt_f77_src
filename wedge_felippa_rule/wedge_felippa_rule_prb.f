      program main

c*********************************************************************72
c
cc MAIN is the main program for WEDGE_FELIPPA_RULE_PRB.
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
c    18 August 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer degree_max

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'WEDGE_FELIPPA_RULE_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the WEDGE_FELIPPA_RULE library.'

      degree_max = 4

      call test01 ( degree_max )
      call test02 ( degree_max )

      call test03 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'WEDGE_FELIPPA_RULE_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( degree_max )

c*********************************************************************72
c
cc TEST01 tests WEDGE_INTEGRAL.
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
      double precision value
      double precision wedge_volume

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  For the unit wedge,'
      write ( *, '(a)' ) 
     &  '  WEDGE_INTEGRAL returns the exact value of the'
      write ( *, '(a)' ) '  integral of X^ALPHA Y^BETA Z^GAMMA'
      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  Volume = ', wedge_volume ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     ALPHA      BETA     GAMMA      INTEGRAL'
      write ( *, '(a)' ) ' '

      do alpha = 0, degree_max
        expon(1) = alpha
        do beta = 0, degree_max - alpha
          expon(2) = beta
          do gamma = 0, degree_max - alpha - beta
            expon(3) = gamma
            call wedge_integral ( expon, value )
            write ( *, '(2x,i8,2x,i8,2x,i8,2x,g14.6)' ) 
     &        expon(1:3), value
          end do
        end do
      end do

      return
      end
      subroutine test02 ( degree_max )

c*********************************************************************72
c
cc TEST02 tests the rules for the unit wedge.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 July 2009
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
      integer test_num
      parameter ( test_num = 7 )
      integer order_max
      parameter ( order_max = 48 )

      integer degree_max
      integer expon(dim_num)
      integer h
      integer line_order
      integer line_order_array(test_num)
      logical more
      integer order
      double precision quad
      double precision r8vec_dot_product
      integer t
      integer test
      integer triangle_order
      integer triangle_order_index
      integer triangle_order_array(test_num)
      double precision wedge_volume
      double precision v(order_max)
      double precision w(order_max)
      double precision xyz(dim_num,order_max)

      save line_order_array
      save triangle_order_array

      data line_order_array /
     &  1, 2, 2, 3, 2, 3, 4 /
      data triangle_order_array /
     &  1, 3, -3, 6, -6, 7, 12 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) '  For the unit wedge,'
      write ( *, '(a)' ) 
     &  '  we approximate monomial integrals with WEDG_UNIT_RULE.'

      more = .false.

10    continue

        call subcomp_next ( degree_max, dim_num, expon, more, h, t )

        if ( mod ( expon(3), 2 ) .eq. 1 ) then
          if ( .not. more ) then
            go to 20
          else
            go to 10
          end if
        end if

        write ( *, '(a)' ) ' '
        write ( *, '(a,2x,i2,2x,i2,2x,i2)' )
     &    '  Monomial exponents:', expon(1:dim_num)
        write ( *, '(a)' ) ' '

        do test = 1, test_num

          line_order = line_order_array(test)
          triangle_order = triangle_order_array(test)

          order = line_order * abs ( triangle_order )

          call wedge_rule ( line_order, triangle_order, w, xyz )
          call monomial_value ( dim_num, order, expon, xyz, v )
          quad = wedge_volume ( ) 
     &      * r8vec_dot_product ( order, w, v )
          write ( *, '(2x,i6,2x,i6,2x,i6,2x,g14.6)' )
     &      triangle_order, line_order, order, quad

        end do

        write ( *, '(a)' ) ' '
        call wedge_integral ( expon, quad )
        write ( *, '(2x,a,2x,6x,2x,6x,2x,g14.6)' ) ' Exact', quad

        if ( .not. more ) then
          go to 20
        end if

      go to 10

20    continue

      return
      end
      subroutine test03 ( )

c*********************************************************************72
c
cc TEST03 writes out some rules for the unit wedge.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 July 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
      implicit none

      integer dim_num
      parameter ( dim_num = 3 )
      integer rule_num
      parameter ( rule_num = 7 )
      integer order_max
      parameter ( order_max = 48 )

      integer line_order
      integer line_order_array(rule_num)
      integer order
      integer rule
      integer triangle_order
      integer triangle_order_array(rule_num)
      double precision w(order_max)
      character * ( 255 ) w_filename
      double precision x(dim_num,order_max)
      character * ( 255 ) x_filename

      save line_order_array
      save triangle_order_array

      data line_order_array /
     &  1, 2, 2, 3, 2, 3, 4 /
      data triangle_order_array /
     &  1, 3, -3, 6, -6, 7, 12 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST03'
      write ( *, '(a)' ) '  For the unit wedge,'
      write ( *, '(a)' ) '  write some rules to a file'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   Rule  Trig    Line   Total  W_File X_File'
      write ( *, '(a)' ) '         Order   Order  Order'
      write ( *, '(a)' ) ' '

      do rule = 1, rule_num

        if ( rule .eq. 1 ) then
          w_filename = 'wedge_felippa_1x1_w.txt'
          x_filename = 'wedge_felippa_1x1_x.txt'
        else if ( rule .eq. 2 ) then
          w_filename = 'wedge_felippa_3x2_w.txt'
          x_filename = 'wedge_felippa_3x2_x.txt'
        else if ( rule .eq. 3 ) then
          w_filename = 'wedge_felippa_3bx2_w.txt'
          x_filename = 'wedge_felippa_3bx2_x.txt'
        else if ( rule .eq. 4 ) then
          w_filename = 'wedge_felippa_6x3_w.txt'
          x_filename = 'wedge_felippa_6x3_x.txt'
        else if ( rule .eq. 5 ) then
          w_filename = 'wedge_felippa_6bx2_w.txt'
          x_filename = 'wedge_felippa_6bx2_x.txt'
        else if ( rule .eq. 6 ) then
          w_filename = 'wedge_felippa_7x3_w.txt'
          x_filename = 'wedge_felippa_7x3_x.txt'
        else if ( rule .eq. 7 ) then
          w_filename = 'wedge_felippa_12x4_w.txt'
          x_filename = 'wedge_felippa_12x4_x.txt'
        end if

        line_order = line_order_array(rule)
        triangle_order = triangle_order_array(rule)

        order = line_order * abs ( triangle_order )

        call wedge_rule ( line_order, triangle_order, w, x )
        call r8mat_write ( w_filename, 1, order, w )
        call r8mat_write ( x_filename, dim_num, order, x )
        write ( *, '(2x,i6,2x,i6,2x,i6,2x,i6,2x,a25,2x,a25)' )
     &    rule, triangle_order, line_order, order, w_filename, 
     &    x_filename

      end do

      return
      end
