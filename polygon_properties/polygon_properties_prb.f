      program main

c*********************************************************************72
c
cc MAIN is the main program for POLYGON_PROPERTIES_PRB.
c
c  Discussion:
c
c    POLYGON_PROPERTIES_PRB tests the POLYGON_PROPERTIES library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 May 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'POLYGON_PROPERTIES_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the POLYGON_PROPERTIES library.'

      call test01 ( )
      call test02 ( )
      call test03 ( )
      call test04 ( )
      call test05 ( )
      call test06 ( )
      call test07 ( )
      call test08 ( )
      call test09 ( )

      call test10 ( )
      call test11 ( )
      call test12 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'POLYGON_PROPERTIES_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 tests POLYGON_ANGLES.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 May 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 6 )

      double precision angle(n)
      integer i
      double precision r8_degrees
      double precision v(2,n)

      save v

      data v /
     &  0.0D+00, 0.0D+00,
     &  1.0D+00, 0.0D+00,
     &  2.0D+00, 1.0D+00,
     &  3.0D+00, 0.0D+00,
     &  3.0D+00, 2.0D+00,
     &  1.0D+00, 2.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  For a polygon:'
      write ( *, '(a)' ) '  POLYGON_ANGLES computes the angles.'

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Number of polygonal vertices = ', n

      call r8mat_transpose_print ( 2, n, v, '  The polygon vertices:' )

      call polygon_angles ( n, v, angle )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Polygonal angles in degrees:'
      write ( *, '(a)' ) ' '

      do i = 1, n
        write ( *, '(2x,i8,2x,g14.6)' ) i, r8_degrees ( angle(i) )
      end do

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 tests POLYGON_AREA.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 October 2005
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer test_num
      parameter ( test_num = 2 )

      double precision area
      double precision area_exact
      double precision area_exact_test(test_num)
      integer n
      integer n_test(test_num)
      integer test
      double precision v1(2,4)
      double precision v2(2,8)

      save area_exact_test
      save n_test
      save v1
      save v2

      data area_exact_test / 2.0D+00, 6.0D+00 /
      data n_test / 4, 8 /
      data v1 /
     &  1.0D+00, 0.0D+00,
     &  2.0D+00, 1.0D+00,
     &  1.0D+00, 2.0D+00,
     &  0.0D+00, 1.0D+00 /
      data v2 /
     &  0.0D+00, 0.0D+00,
     &  3.0D+00, 0.0D+00,
     &  3.0D+00, 3.0D+00,
     &  2.0D+00, 3.0D+00,
     &  2.0D+00, 1.0D+00,
     &  1.0D+00, 1.0D+00,
     &  1.0D+00, 2.0D+00,
     &  0.0D+00, 2.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) '  For a polygon:'
      write ( *, '(a)' ) '  POLYGON_AREA computes the area.'

      do test = 1, test_num

        n = n_test(test)
        area_exact = area_exact_test(test)

        if ( test .eq. 1 ) then

          call r8mat_transpose_print ( 2, n, v1, 
     &      '  The polygon vertices:' )

          call polygon_area ( n, v1, area )

        else if ( test .eq. 2 ) then

          call r8mat_transpose_print ( 2, n, v2, 
     &      '  The polygon vertices:' )

          call polygon_area ( n, v2, area )

        end if

        write ( *, '(a)' ) ' '
        write ( *, '(a,i8)' ) '  Number of polygonal vertices = ', n
        write ( *, '(a,g14.6)' ) '  Exact area is        ', area_exact
        write ( *, '(a,g14.6)' ) '  The computed area is ', area

      end do

      return
      end
      subroutine test03 ( )

c*********************************************************************72
c
cc TEST03 tests POLYGON_AREA_2;
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 October 2005
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer test_num
      parameter ( test_num = 2 )

      double precision area
      double precision area_exact
      double precision area_exact_test(test_num)
      integer n
      integer n_test(test_num)
      integer test
      double precision v1(2,4)
      double precision v2(2,8)

      save area_exact_test
      save n_test
      save v1
      save v2

      data area_exact_test / 2.0D+00, 6.0D+00 /
      data n_test / 4, 8 /
      data v1 /
     &  1.0D+00, 0.0D+00,
     &  2.0D+00, 1.0D+00,
     &  1.0D+00, 2.0D+00,
     &  0.0D+00, 1.0D+00 /
      data v2 /
     &  0.0D+00, 0.0D+00,
     &  3.0D+00, 0.0D+00,
     &  3.0D+00, 3.0D+00,
     &  2.0D+00, 3.0D+00,
     &  2.0D+00, 1.0D+00,
     &  1.0D+00, 1.0D+00,
     &  1.0D+00, 2.0D+00,
     &  0.0D+00, 2.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST03'
      write ( *, '(a)' ) '  For a polygon:'
      write ( *, '(a)' ) '  POLYGON_AREA_2 computes the area.'

      do test = 1, test_num

        n = n_test(test)
        area_exact = area_exact_test(test)

        if ( test .eq. 1 ) then

          call r8mat_transpose_print ( 2, n, v1, '  Vertices:' )

          call polygon_area_2 ( n, v1, area )

        else if ( test .eq. 2 ) then

          call r8mat_transpose_print ( 2, n, v2, '  Vertices:' )

          call polygon_area_2 ( n, v2, area )

        end if

        write ( *, '(a)' ) ' '
        write ( *, '(a,g14.6)' ) '  Exact area is        ', area_exact
        write ( *, '(a,g14.6)' ) '  The computed area is ', area

      end do

      return
      end
      subroutine test04 ( )

c*********************************************************************72
c
cc TEST04 tests POLYGON_CENTROID and POLYGON_CENTROID_2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 October 2005
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 4 )

      double precision centroid(2)
      double precision v(2,n)

      save v

      data v /
     &  1.0D+00, 0.0D+00,
     &  2.0D+00, 1.0D+00,
     &  1.0D+00, 2.0D+00,
     &  0.0D+00, 1.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST04'
      write ( *, '(a)' ) '  For a polygon:'
      write ( *, '(a)' ) '  POLYGON_CENTROID computes the centroid.'
      write ( *, '(a)' ) '  POLYGON_CENTROID_2 computes the centroid.'

      call r8mat_transpose_print ( 2, n, v, '  The polygon vertices:' )

      call polygon_centroid ( n, v, centroid )

      call r8vec_print ( 2, centroid, '  POLYGON_CENTROID:' )

      call polygon_centroid_2 ( n, v, centroid )

      call r8vec_print ( 2, centroid, '  POLYGON_CENTROID_2:' )

      return
      end
      subroutine test05 ( )

c*********************************************************************72
c
cc TEST05 tests POLYGON_CONTAINS_POINT and POLYGON_CONTAINS_POINT_2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 October 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 )
      integer test_num
      parameter ( test_num = 4 )

      integer i
      logical inside1
      logical inside2
      double precision p(2)
      double precision p_test(2,test_num)
      integer test
      double precision v(2,n)

      save p_test
      save v

      data p_test /
     &  1.0D+00,  1.0D+00,
     &  3.0D+00,  4.0D+00,
     &  0.0D+00,  2.0D+00,
     &  0.5D+00, -0.25D+00 /
      data v /
     &  0.0D+00, 0.0D+00,
     &  1.0D+00, 0.0D+00,
     &  2.0D+00, 1.0D+00,
     &  1.0D+00, 2.0D+00,
     &  0.0D+00, 2.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST05'
      write ( *, '(a)' ) '  POLYGON_CONTAINS_POINT determines if '
      write ( *, '(a)' ) '  a point is in a polygon.'
      write ( *, '(a)' ) '  POLYGON_CONTAINS_POINT_2 determines if'
      write ( *, '(a)' ) '  a point is in a polygon.'

      call r8mat_transpose_print ( 2, n, v, '  The polygon vertices:' )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '          P          In1  In2'
      write ( *, '(a)' ) ' '

      do test = 1, test_num

        p(1:2) = p_test(1:2,test)

        call polygon_contains_point ( n, v, p, inside1 )

        call polygon_contains_point_2 ( n, v, p, inside2 )

        write ( *, '(2x,2g14.6,4x,l1,4x,l1)' ) p(1:2), inside1, inside2

      end do

      return
      end
      subroutine test06 ( )

c*********************************************************************72
c
cc TEST06 tests POLYGON_DIAMETER;
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 December 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 4 )

      double precision diameter
      double precision diameter_exact
      parameter ( diameter_exact = 2.0D+00 )
      double precision v(2,n)

      save v

      data v /
     &  1.0D+00, 0.0D+00,
     &  2.0D+00, 1.0D+00,
     &  1.0D+00, 2.0D+00,
     &  0.0D+00, 1.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST06'
      write ( *, '(a)' ) '  For a polygon:'
      write ( *, '(a)' ) '  POLYGON_DIAMETER computes the diameter;'

      call r8mat_transpose_print ( 2, n, v, '  The polygon vertices:' )

      call polygon_diameter ( n, v, diameter )

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  Diameter ( computed ) ', diameter
      write ( *, '(a,g14.6)' ) 
     &  '  Diameter ( exact )    ', diameter_exact

      return
      end
      subroutine test07 ( )

c*********************************************************************72
c
cc TEST07 tests POLYGON_EXPAND;
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 December 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 4 )

      double precision h
      double precision v(2,n)
      double precision w(2,n)

      save v

      data v /
     &  1.0D+00, 1.0D+00,
     &  5.0D+00, 1.0D+00,
     &  2.0D+00, 4.0D+00,
     &  1.0D+00, 3.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST07'
      write ( *, '(a)' ) '  For a polygon:'
      write ( *, '(a)' ) '  POLYGON_EXPAND "expands" it by an amount H.'

      h = 0.5D+00

      call r8mat_transpose_print ( 2, n, v, '  The polygon vertices:' )

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  The expansion amount H = ', h

      call polygon_expand ( n, v, h, w )

      call r8mat_transpose_print ( 2, n, w, '  The expanded polygon:' )

      return
      end
      subroutine test08 ( )

c*********************************************************************72
c
cc TEST08 tests POLYGON_INRAD_DATA, POLYGON_OUTRAD_DATA, POLYGON_SIDE_DATA;
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 December 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision area
      integer n
      double precision radin
      double precision radout
      double precision side

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST08'
      write ( *, '(a)' ) '  For a REGULAR polygon:'
      write ( *, '(a)' ) '  inradius, outradius and side are related.'
      write ( *, '(a)' ) '  POLYGON_INRAD_DATA uses the inradius;'
      write ( *, '(a)' ) '  POLYGON_OUTRAD_DATA uses the inradius;'
      write ( *, '(a)' ) '  POLYGON_SIDE_DATA uses the inradius;'

      do n = 3, 5

        write ( *, '(a)' ) ' '
        write ( *, '(a,i8)' ) '  Number of polygonal sides = ', n

        side = 1.0D+00
        write ( *, '(a)' ) ' '
        write ( *, '(a,g14.6)' ) '  Assuming SIDE = ', side
        call polygon_side_data ( n, side, area, radin, radout )
        write ( *, '(a,g14.6)' ) '    AREA =   ', area
        write ( *, '(a,g14.6)' ) '    RADIN =  ', radin
        write ( *, '(a,g14.6)' ) '    RADOUT = ', radout
        write ( *, '(a)' ) ' '
        write ( *, '(a,g14.6)' ) '  Assuming RADIN = ', radin
        call polygon_inrad_data ( n, radin, area, radout, side )
        write ( *, '(a,g14.6)' ) '    AREA =   ', area
        write ( *, '(a,g14.6)' ) '    RADOUT = ', radout
        write ( *, '(a,g14.6)' ) '    SIDE =   ', side
        write ( *, '(a)' ) ' '
        write ( *, '(a,g14.6)' ) '  Assuming RADOUT = ', radout
        call polygon_outrad_data ( n, radout, area, radin, side )
        write ( *, '(a,g14.6)' ) '    AREA =   ', area
        write ( *, '(a,g14.6)' ) '    RADIN =  ', radin
        write ( *, '(a,g14.6)' ) '    SIDE =   ', side

      end do

      return
      end
      subroutine test09 ( )

c*********************************************************************72
c
cc TEST09 tests POLYGON_INTEGRAL_*.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 July 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 4 )

      double precision result
      double precision v(2,n)

      save v

      data v /
     & 0.0D+00, 0.0D+00,
     & 1.0D+00, 0.0D+00,
     & 1.0D+00, 1.0D+00,
     & 0.0D+00, 1.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST09'
      write ( *, '(a)' ) '  For a polygon:'
      write ( *, '(a)' ) '  POLYGON_INTEGRAL_1 integrates 1'
      write ( *, '(a)' ) '  POLYGON_INTEGRAL_X integrates X'
      write ( *, '(a)' ) '  POLYGON_INTEGRAL_Y integrates Y'
      write ( *, '(a)' ) '  POLYGON_INTEGRAL_XX integrates X*X'
      write ( *, '(a)' ) '  POLYGON_INTEGRAL_XY integrates X*Y'
      write ( *, '(a)' ) '  POLYGON_INTEGRAL_YY integrates Y*Y'

      call r8mat_transpose_print ( 2, n, v, '  The polygon vertices:' )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  F(X,Y)    Integral'
      write ( *, '(a)' ) ' '

      call polygon_integral_1 ( n, v, result )
      write ( *, '(2x,a4,4x,g14.6)' ) '  1', result

      call polygon_integral_x ( n, v, result )
      write ( *, '(2x,a4,4x,g14.6)' ) '  X', result

      call polygon_integral_y ( n, v, result )
      write ( *, '(2x,a4,4x,g14.6)' ) '  Y', result

      call polygon_integral_xx ( n, v, result )
      write ( *, '(2x,a4,4x,g14.6)' ) 'X*X', result

      call polygon_integral_xy ( n, v, result )
      write ( *, '(2x,a4,4x,g14.6)' ) 'X*Y', result

      call polygon_integral_yy ( n, v, result )
      write ( *, '(2x,a4,4x,g14.6)' ) 'Y*Y', result

      return
      end
      subroutine test10 ( )

c*********************************************************************72
c
cc TEST10 tests POLYGON_IS_CONVEX.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 December 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer, parameter :: n_max = 10

      double precision angle
      integer i
      character * ( 80 ) message(-1:2)
      integer n
      integer polygon_is_convex
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      integer result
      integer test
      integer test_num
      parameter ( test_num = 11 )
      character * ( 255 ) title
      double precision v(2,n_max)
      double precision v1(2,1)
      double precision v2(2,2)
      double precision v3(2,3)
      double precision v4(2,3)
      double precision v5(2,3)
      double precision v6(2,4)
      double precision v7(2,5)
      double precision v10(2,6)
      double precision v11(2,8)

      save v1
      save v2
      save v3
      save v4
      save v5
      save v6
      save v7
      save v10
      save v11

      data v1 /
     &  0.0D+00, 0.0D+00 /
      data v2 /
     &  0.0D+00, 0.0D+00,
     &  1.0D+00, 2.0D+00 /
      data v3 /
     &  0.0D+00, 0.0D+00,
     &  2.0D+00, 0.0D+00,
     &  1.0D+00, 0.0D+00 /
      data v4 /
     &  0.0D+00, 0.0D+00,
     &  1.0D+00, 0.0D+00,
     &  0.0D+00, 2.0D+00 /
      data v5 /
     &  0.0D+00, 0.0D+00,
     &  0.0D+00, 2.0D+00,
     &  1.0D+00, 0.0D+00 /
      data v6 /
     &  1.0D+00, 0.0D+00,
     &  2.0D+00, 0.0D+00,
     &  3.0D+00, 1.0D+00,
     &  0.0D+00, 1.0D+00 /
      data v7 /
     &  0.0D+00, 0.0D+00,
     &  0.5D+00, 0.5D+00,
     &  1.0D+00, 0.0D+00,
     &  1.0D+00, 1.0D+00,
     &  0.0D+00, 1.0D+00 /
      data v10 /
     &  0.0D+00, 0.0D+00,
     &  2.0D+00, 0.0D+00,
     &  1.0D+00, 1.0D+00,
     &  0.0D+00, 0.0D+00,
     &  2.0D+00, 0.0D+00,
     &  1.0D+00, 1.0D+00 /
      data v11 /
     &  1.0D+00, 0.0D+00,
     &  3.0D+00, 0.0D+00,
     &  3.0D+00, 3.0D+00,
     &  0.0D+00, 3.0D+00,
     &  0.0D+00, 1.0D+00,
     &  2.0D+00, 1.0D+00,
     &  2.0D+00, 2.0D+00,
     &  1.0D+00, 2.0D+00 /

      message(-1) = 'The polygon is not convex.'
      message( 0) = 'The polygon is degenerate and convex.'
      message( 1) = 'The polygon is convex and counterclockwise.'
      message( 2) = 'The polygon is convex and clockwise.'

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST10'
      write ( *, '(a)' ) '  POLYGON_IS_CONVEX determines if a polygon'
      write ( *, '(a)' ) '  is convex.'

      do test = 1, test_num

        if ( test .eq. 1 ) then
          n = 1
          call r8mat_copy ( 2, n, v1, v )
          title = '  A point:'
        else if ( test .eq. 2 ) then
          n = 2
          call r8mat_copy ( 2, n, v2, v )
          title = '  A line:'
        else if ( test .eq. 3 ) then
          n = 3
          call r8mat_copy ( 2, n, v3, v )
          title = '  A triangle:'
        else if ( test .eq. 4 ) then
          n = 3
          call r8mat_copy ( 2, n, v4, v )
          title = '  A CCW triangle:'
        else if ( test .eq. 5 ) then
          n = 3
          call r8mat_copy ( 2, n, v5, v )
          title = '  A CW triangle:'
        else if ( test .eq. 6 ) then
          n = 4
          call r8mat_copy ( 2, n, v6, v )
          title = '  Polygon with large angle:'
        else if ( test .eq. 7 ) then
          n = 5
          call r8mat_copy ( 2, n, v7, v )
          title = '  Polygon with huge angle:'
        else if ( test .eq. 8 ) then
          n = 5
          do i = 1, n
            angle = dble ( i - 1 ) * 4.0D+00 * r8_pi / dble ( n )
            v(1,i) = cos ( angle )
            v(2,i) = sin ( angle )
          end do
          title = '  A five-pointed star:'
        else if ( test .eq. 9 ) then
          n = 6
          do i = 1, n
            angle = dble ( i - 1 ) * 2.0D+00 * r8_pi / dble ( n )
            v(1,i) = cos ( angle )
            v(2,i) = sin ( angle )
          end do
          title = '  A hexagon:'
        else if ( test .eq. 10 ) then
          n = 6
          call r8mat_copy ( 2, n, v10, v )
          title = '  A triangle twice:'
        else if ( test .eq. 11 ) then
          n = 8
          call r8mat_copy ( 2, n, v11, v )
          title = '  Square knot:'
        end if

        call r8mat_transpose_print ( 2, n, v, title )
        result = polygon_is_convex ( n, v )
        write ( *, '(2x,a)' ) message(result)

      end do

      return
      end
      subroutine test11 ( )

c*********************************************************************72
c
cc TEST11 tests POLYGON_LATTICE_AREA.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 May 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision area
      integer b
      integer i

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST11'
      write ( *, '(a)' ) '  POLYGON_LATTICE_AREA returns the "area"'
      write ( *, '(a)' ) '  of a polygon, measured in lattice points.'

      i = 5
      b = 6

      write ( *, '(a)' ) ' '
      write ( *, '(a,i4)' ) '  Number of interior lattice points = ', i
      write ( *, '(a,i4)' ) '  Number of boundary lattice points = ', b

      call polygon_lattice_area ( i, b, area )

      write ( *, '(a,g14.6)' ) '  Area of polygon is ', area

      return
      end
      subroutine test12 ( )

c*********************************************************************72
c
cc TEST12 tests POLYGON_SAMPLE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 May 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 20 )
      integer nv
      parameter ( nv = 6 )

      integer seed
      double precision v(2,nv)
      double precision x(2,n)

      save v

      data v /
     &  0.0D+00, 0.0D+00,
     &  2.0D+00, 0.0D+00,
     &  2.0D+00, 1.0D+00,
     &  1.0D+00, 1.0D+00,
     &  1.0D+00, 2.0D+00,
     &  0.0D+00, 1.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST12'
      write ( *, '(a)' ) '  POLYGON_SAMPLE samples a polygon.'

      seed = 123456789

      call polygon_sample ( nv, v, n, seed, x )

      call r8mat_transpose_print ( 2, n, x, '  Sample points:' )

      return
      end

