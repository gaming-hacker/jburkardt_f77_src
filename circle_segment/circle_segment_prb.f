      program main

c*********************************************************************72
c
cc MAIN is the main program for CIRCLE_SEGMENT_PRB.
c
c  Discussion:
c
c    CIRCLE_SEGMENT_PRB tests the CIRCLE_SEGMENT library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 July 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CIRCLE_SEGMENT_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the CIRCLE_SEGMENT library.'

      call test01 ( )
      call test05 ( )
      call test06 ( )
      call test07 ( )
      call test08 ( )
      call test09 ( )
      call test11 ( )
      call test13 ( )
      call test14 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CIRCLE_SEGMENT_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 tests CIRCLE_SEGMENT_AREA_FROM_HEIGHT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 May 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision area
      double precision h
      integer i
      double precision r

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) 
     &  '  CIRCLE_SEGMENT_AREA_FROM_HEIGHT computes the ' // 
     &  'area of a circle segment.'

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '          R               H               Area'
      write ( *, '(a)' ) ' '
      r = 1.0D+00
      h = 1.0D+00
      do i = 0, 10
        call circle_segment_area_from_height ( r, h, area )
        write ( *, '(2x,f14.6,2x,f14.6,2x,f14.6)' ) r, h, area
        h = h / 2.0D+00
      end do

      return
      end
      subroutine test05 ( )

c*********************************************************************72
c
cc TEST05 tests the AREA and HEIGHT functions.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 May 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision a2
      double precision h
      double precision h2
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision r
      double precision r8_uniform_01
      integer seed
      integer test

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'CIRCLE_SEGMENT_TEST05'
      write ( *, '(a)' ) '  For circle segment with a given radius R,'
      write ( *, '(a)' ) 
     &  '  CIRCLE_SEGMENT_AREA_FROM_HEIGHT computes the area A, ' //
     &  'given the height.'
      write ( *, '(a)' ) 
     &  '  CIRCLE_SEGMENT_HEIGHT_FROM_AREA computes height H, ' //
     &  'given the area.'
      write ( *, '(a)' ) 
     &  '  Check that these functions are inverses of each other'
      write ( *, '(a)' ) '  using random values of R, A, and H.'

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '        R             H      =>     A    =>       H2'
      write ( *, '(a)' ) ''

      seed = 123456789

      do test = 1, 5
        r = 5.0D+00 * r8_uniform_01 ( seed )
        h = 2.0D+00 * r * r8_uniform_01 ( seed )
        call circle_segment_area_from_height ( r, h, a )
        call circle_segment_height_from_area ( r, a, h2 )
        write ( *, '(2x,f12.6,2x,f12.6,2x,f12.6,2x,f12.6)' ) 
     &    r, h, a, h2
      end do

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '        R             A      =>     H    =>       A2'
      write ( *, '(a)' ) ''

      do test = 1, 5
        r = 5.0D+00 * r8_uniform_01 ( seed )
        a = pi * r * r * r8_uniform_01 ( seed )
        call circle_segment_height_from_area ( r, a, h )
        call circle_segment_area_from_height ( r, h, a2 )
        write ( *, '(2x,f12.6,2x,f12.6,2x,f12.6,2x,f12.6)' ) 
     &    r, a, h, a2
      end do

      return
      end
      subroutine test06 ( )

c*********************************************************************72
c
cc TEST06 samples using CIRCLE_SEGMENT_SAMPLE_FROM_HEIGHT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 May 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer an_num
      parameter ( an_num = 51 )
      integer boundary_num
      parameter ( boundary_num = an_num + 1 )
      integer data_num
      parameter ( data_num = 100 )

      double precision an(an_num)
      character * ( 255 ) boundary_filename
      integer boundary_unit
      double precision boundary_x(boundary_num)
      double precision boundary_y(boundary_num)
      character * ( 255 ) command_filename
      integer command_unit
      character * ( 255 ) data_filename
      integer data_unit
      double precision data_x(data_num)
      double precision data_y(data_num)
      character * ( 255 ) graphics_filename
      double precision h
      integer i
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision r
      integer seed
      integer test
      double precision theta
      double precision thetah

      seed = 123456789

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'CIRCLE_SEGMENT_TEST06'
      write ( *, '(a)' ) 
     &  '  CIRCLE_SEGMENT_SAMPLE_FROM_HEIGHT samples a circle segment.'
      write ( *, '(a)' ) ''
      write ( *, '(a,i4,a)' ) 
     &  '  Plot ', data_num, ' points from several segments.'
      write ( *, '(a)' ) ''

      r = 1.0D+00
      theta = pi

      data_filename = 'sample00_data.txt'
      boundary_filename = 'sample00_boundary.txt'
      command_filename = 'sample00_commands.txt'
      graphics_filename = 'sample00.png'

      do test = 1, 4

        call circle_segment_height_from_angle ( r, theta, h )

        thetah = theta / 2.0D+00
c
c  Create boundary.
c
        call r8vec_linspace ( an_num, -thetah, +thetah, an )
        do i = 1, an_num
          an(i) = an(i) + 0.5D+00 * pi
          boundary_x(i) = r * cos ( an(i) )
          boundary_y(i) = r * sin ( an(i) )
        end do

        boundary_x(an_num+1) = boundary_x(1)
        boundary_y(an_num+1) = boundary_y(1)

        call get_unit ( boundary_unit )
        call filename_inc ( boundary_filename )
        open ( unit = boundary_unit, file = boundary_filename, 
     &    status = 'replace' )
        do i = 1, boundary_num
          write ( boundary_unit, '(2x,g14.6,2x,g14.6)' ) 
     &      boundary_x(i), boundary_y(i)
        end do
        close ( unit = boundary_unit )
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  Created boundary file "' // 
     &    trim ( boundary_filename ) // '".'
c
c  Create data.
c
        call circle_segment_sample_from_height ( r, h, data_num, 
     &    seed, data_x, data_y )

        call get_unit ( data_unit )
        call filename_inc ( data_filename )
        open ( unit = data_unit, file = data_filename, 
     &    status = 'replace' )
        do i = 1, data_num
          write ( data_unit, '(2x,g14.6,2x,g14.6)' )
     &      data_x(i), data_y(i)
        end do
        close ( unit = data_unit )
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  Created data file "' // 
     &    trim ( data_filename ) // '".'
c
c  Create commands.
c
        call get_unit ( command_unit )
        call filename_inc ( command_filename )
        open ( unit = command_unit, file = command_filename, 
     &    status = 'replace' )
        write ( command_unit, '(a)' ) '# ' // 
     &    trim ( command_filename )
        write ( command_unit, '(a)' ) '#'
        write ( command_unit, '(a)' ) '# Usage:'
        write ( command_unit, '(a)' ) '#  gnuplot < ' // 
     &    trim ( command_filename )
        write ( command_unit, '(a)' ) '#'
        write ( command_unit, '(a)' ) 'set term png'
        call filename_inc ( graphics_filename )
        write ( command_unit, '(a)' ) 
     &    'set output "' // trim ( graphics_filename ) // '"'
        write ( command_unit, '(a)' ) 'set xlabel "<--- X --->"'
        write ( command_unit, '(a)' ) 'set ylabel "<--- Y --->"'
        write ( command_unit, '(a)' ) 
     &    'set title "Circle Segment Sample"'
        write ( command_unit, '(a)' ) 'set grid'
        write ( command_unit, '(a)' ) 'set key off'
        write ( command_unit, '(a)' ) 'set size ratio -1'
        write ( command_unit, '(a)' ) 'set style data lines'
        write ( command_unit, '(a)' ) 
     &    'plot "' // trim ( data_filename ) // 
     &    '" using 1:2 with points lt 3 pt 3,\'
        write ( command_unit, '(a)' ) '    "' // 
     &    trim ( boundary_filename ) // 
     &    '" using 1:2 lw 3 linecolor rgb "black"'
        write ( command_unit, '(a)' ) 'quit'
        close ( unit = command_unit )

        write ( *, '(a)' ) 
     &    '  Created command file "' // 
     &    trim ( command_filename ) // '".'

        theta = theta / 2.0D+00

      end do
     
      return
      end
      subroutine test07 ( )

c*********************************************************************72
c
cc TEST07 tests the ANGLE and HEIGHT functions.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 May 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision h
      double precision h2
      double precision, parameter :: pi = 3.141592653589793D+00
      double precision r
      double precision r8_uniform_01
      integer seed
      double precision t
      double precision t2
      integer test

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST07'
      write ( *, '(a)' ) '  For circle segment with a given radius R,'
      write ( *, '(a)' ) 
     &  '  CIRCLE_SEGMENT_ANGLE_FROM_HEIGHT computes the angle ' //
     &  '  THETA, given the height.'
      write ( *, '(a)' ) 
     &  '  CIRCLE_SEGMENT_HEIGHT_FROM_ANGLE computes height H, ' //
     &  'given the angle.'
      write ( *, '(a)' ) 
     &  '  Check that these functions are inverses of each other'
      write ( *, '(a)' ) '  using random values of R, T, and H.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '        R             H      =>     T    =>       H2'
      write ( *, '(a)' ) ''

      seed = 123456789

      do test = 1, 5
        r = 5.0D+00 * r8_uniform_01 ( seed )
        h = 2.0D+00 * r * r8_uniform_01 ( seed )
        call circle_segment_angle_from_height ( r, h, t )
        call circle_segment_height_from_angle ( r, t, h2 )
        write ( *, '(2x,f12.6,2x,f12.6,2x,f12.6,2x,f12.6)' ) 
     &    r, h, t, h2
      end do

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '        R             T      =>     H    =>       T2'
      write ( *, '(a)' ) ''
      do test = 1, 5
        r = 5.0D+00 * r8_uniform_01 ( seed )
        t = 2.0D+00 * pi * r8_uniform_01 ( seed )
        call circle_segment_height_from_angle ( r, t, h )
        call circle_segment_angle_from_height ( r, h, t2 )
        write ( *, '(2x,f12.6,2x,f12.6,2x,f12.6,2x,f12.6)' ) 
     &    r, t, h, t2
      end do

      return
      end
      subroutine test08 ( )

c*********************************************************************72
c
cc TEST08 tests CIRCLE_SEGMENT_CONTAINS_POINT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 May 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 1000 )

      double precision area
      double precision area_est
      double precision c(2)
      integer i4vec_sum
      integer inout(n)
      integer j
      double precision omega1
      double precision omega2
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision r
      double precision r8_uniform_01
      integer seed
      integer test
      double precision theta
      double precision xy(2,n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST08'
      write ( *, '(a)' ) 
     &  '  CIRCLE_SEGMENT_CONTAINS_POINT reports whether'
      write ( *, '(a)' ) '  a circle segment contains a point.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Pick a circle segment at random.'
      write ( *, '(a,i4,a)' ) 
     &  '  Compute ', n, ' sample points in the surrounding box.'
      write ( *, '(a)' ) 
     &  '  Compare the area of the segment to the percentage of points'
      write ( *, '(a)' ) '  contained in the circle segment.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '       N       Omega1          Omega2           Area' //
     &  '         Estimate'
      write ( *, '(a)' ) ' '

      r = 1.0D+00
      c(1) = 0.0D+00
      c(2) = 0.0D+00
      seed = 123456789

      do test = 1, 5

        omega1 = 2.0D+00 * pi * r8_uniform_01 ( seed )
        omega2 = 2.0D+00 * pi * r8_uniform_01 ( seed )
      
        if ( omega2 .lt. omega1 ) then
          omega2 = omega2 + 2.0D+00 * pi
        end if

        call r8mat_uniform_01 ( 2, n, seed, xy )
        xy(1:2,1:n) = 2.0D+00 * xy(1:2,1:n) - 1.0D+00

        do j = 1, n
          call circle_segment_contains_point ( r, c, omega1, omega2, 
     &      xy(1:2,j), inout(j) )
        end do

        call circle_segment_angle_from_chord_angles ( omega1, 
     &    omega2, theta )
        call circle_segment_area_from_angle ( r, theta, area )
        area_est = 4.0D+00 * dble ( i4vec_sum ( n, inout ) ) 
     &    / dble ( n )

        write ( *, '(2x,i6,2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &    n, omega1, omega2, area, area_est

      end do

      return
      end
      subroutine test09 ( )

c*********************************************************************72
c
cc CIRCLE_SEGMENT_TEST09 looks at the area and centroid calculations.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 May 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a1
      double precision a2
      double precision a3
      double precision c(2)
      double precision d1(2)
      double precision d2(2)
      double precision d3(2)
      double precision h
      integer n
      double precision omega1
      double precision omega2
      double precision p1(2)
      double precision p2(2)
      double precision pi 
      parameter ( pi = 3.141592653589793D+00 )
      double precision r
      integer seed
      double precision theta

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'CIRCLE_SEGMENT_TEST09'
      write ( *, '(a)' ) '  CIRCLE_SEGMENT_AREA_FROM_CHORD and'
      write ( *, '(a)' ) 
     &  '  CIRCLE_SEGMENT_CENTROID_FROM_CHORD evaluate the area'
      write ( *, '(a)' ) 
     &  '  and centroid of a circle segment, given R, C and P1:P2.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  CIRCLE_SEGMENT_AREA_FROM_SAMPLE and'
      write ( *, '(a)' ) 
     &  '  CIRCLE_SEGMENT_CENTROID_FROM_SAMPLE give us ' //
     &  'Monte Carlo estimates.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  GQCIRCSEGM can estimate these values by quadrature.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  Start easy, with R = 1, C = (0,0), and Theta centered.'

      seed = 123456789
      r = 1.0D+00
      c(1) = 0.0D+00
      c(2) = 0.0D+00
      theta = pi / 4.0D+00
      call circle_segment_height_from_angle ( r, theta, h )
      omega1 = - theta / 2.0D+00
      omega2 = + theta / 2.0D+00
      p1(1) = c(1) + r * cos ( omega1 )
      p1(2) = c(2) + r * sin ( omega1 )
      p2(1) = c(1) + r * cos ( omega2 )
      p2(2) = c(2) + r * sin ( omega2 )

      call circle_segment_area_from_chord ( r, c, p1, p2, a1 )
      call circle_segment_centroid_from_chord ( r, c, p1, p2, d1 )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '         Area          CentroidX    CentroidY'
      write ( *, '(a)' ) ''

      write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) a1, d1(1), d1(2)
c
c  This only works because the centroid of the height-based circle segment 
c  is easily transformed to the centroid of the chord based circle segment.
c
      call circle_segment_area_from_height ( r, h, a2 )
      call circle_segment_centroid_from_height ( r, h, d2 )

      write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) a2, d2(2), -d2(1)

      n = 10000
      call circle_segment_area_from_sample ( r, c, p1, p2, n, seed, a3 )
      call circle_segment_centroid_from_sample ( r, c, p1, p2, n, 
     &  seed, d3 )
      write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) a3, d3(1), d3(2)

      return
      end
      subroutine test11 ( )

c*********************************************************************72
c
cc TEST11 demonstrates CIRCLE_SEGMENT_ROTATION_FROM_CHORD.
c
c  Discussion:
c
c    We make a table of all pairs of angles that are multiples of pi/12.
c
c    For each pair, we compute the rotation, that is, the angle of the
c    central radius of the circle segment.  We print out the result in
c    terms of multiples of pi/12.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 July 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision alpha
      double precision c(2)
      integer i
      integer j
      double precision p1(2)
      double precision p2(2)
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision r
      double precision rho1
      double precision rho2
      double precision t(0:12)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST11:'
      write ( *, '(a)' ) 
     &  '  CIRCLE_SEGMENT_ROTATION_FROM_CHORD is given the endpoints'
      write ( *, '(a)' ) 
     &  '  of a chord, and is asked to determine the angle of the'
      write ( *, '(a)' ) '  central radius vector.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  We make a table of all pairs of angles that are multiples'
      write ( *, '(a)' ) 
     &  '  of pi/12, determine the corresponding chord endpoints, and'
      write ( *, '(a)' ) 
     &  '  compute the rotation angle, also printed as a ' // 
     &  'multiple of pi/12.'

      r = 2.0D+00
      c(1) = 3.0D+00
      c(2) = 5.0D+00
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '     0.0   1.0   2.0   3.0   4.0   5.0   6.0   7.0' 
     &  // '   8.0   9.0  10.0  11.0  12.0'
      write ( *, '(a)' ) ''
      do i = 0, 12
        rho1 = dble ( i ) * pi / 6.0D+00
        p1(1) = c(1) + r * cos ( rho1 )
        p1(2) = c(2) + r * sin ( rho1 )
        do j = 0, 12
          rho2 = dble ( j ) * pi / 6.0D+00
          p2(1) = c(1) + r * cos ( rho2 )
          p2(2) = c(2) + r * sin ( rho2 )
          call circle_segment_rotation_from_chord ( r, c, p1, p2, 
     &      alpha )
          t(j) = 6.0D+00 * alpha / pi
        end do
        write ( *, '(i2,13(2x,f4.1))' ) i, t(0:12)
      end do

      return
      end
      subroutine test13 ( )

c*********************************************************************72
c
cc TEST13 demonstrates GAUSS for quadrature rules.
c
c  Discussion:
c
c    Some recursion coefficients ALPHA and BETA are listed in Kautsky
c    and Elhay.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 July 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference
c
c    Jaroslav Kautsky, Sylvan Elhay,
c    Calculation of the Weights of Interpolatory Quadratures,
c    Numerische Mathematik,
c    Volume 40, Number 3, October 1982, pages 407-422.
c
      implicit none

      integer n_max
      parameter ( n_max = 10 )

      double precision alpha(n_max)
      double precision beta(n_max)
      integer i
      integer n
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision w(n_max)
      double precision x(n_max)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST13'
      write ( *, '(a)' ) '  GAUSS computes the points and weights for a'
      write ( *, '(a)' ) '  Gauss quadrature rule, given the ALPHA'
      write ( *, '(a)' ) '  and BETA recursion coefficients.'
c
c  Legendre rule.
c
      n = 10

      do i = 1, n
        alpha(i) = 0.0D+00
        if ( i .eq. 1 ) then
          beta(i) = 2.0D+00
        else
          beta(i) = 1.0D+00 
     &      / ( 4.0D+00 - 1.0D+00 / dble ( ( i - 1 )**2 ) )
        end if
      end do

      call gauss ( n, alpha, beta, x, w )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  LEGENDRE RULE'
      write ( *, '(a)' ) '  Point   Weight'
      write ( *, '(a)' ) ''
      do i = 1, n
        write ( *, '(2x,g14.6,2x,g14.6)' ) x(i), w(i)
      end do
c
c  Hermite rule.
c
      n = 10

      do i = 1, n
        alpha(i) = 0.0D+00
        if ( i .eq. 1 ) then
          beta(i) = sqrt ( pi )
        else
          beta(i) = dble ( i - 1 ) / 2.0D+00
        end if
      end do

      call gauss ( n, alpha, beta, x, w )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  HERMITE RULE'
      write ( *, '(a)' ) '  Point   Weight'
      write ( *, '(a)' ) ''
      do i = 1, n
        write ( *, '(2x,g14.6,2x,g14.6)' ) x(i), w(i)
      end do
c
c  Laguerre rule.
c
      n = 10

      do i = 1, n
        alpha(i) = 2.0D+00 * dble ( i ) - 1.0D+00
        if ( i .eq. 1 ) then
          beta(i) = 1.0D+00
        else
          beta(i) = dble ( ( i - 1 ) ** 2 )
        end if
      end do

      call gauss ( n, alpha, beta, x, w )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  LAGUERRE RULE'
      write ( *, '(a)' ) '  Point   Weight'
      write ( *, '(a)' ) ''
      do i = 1, n
        write ( *, '(2x,g14.6,2x,g14.6)' ) x(i), w(i)
      end do

      return
      end
      subroutine test14 ( )

c*********************************************************************72
c
cc TEST14 demonstrates R_JACOBI.
c
c  Discussion:
c
c    R_JACOBI returns recursion coefficients ALPHA and BETA for rules
c    using a Jacobi type weight w(x) = (1-x)^A * (1+x)^B.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 July 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference
c
c    Walter Gautschi,
c    Orthogonal Polynomials: Computation and Approximation,
c    Oxford, 2004,
c    ISBN: 0-19-850672-4,
c    LC: QA404.5 G3555.
c
      implicit none

      integer n_max
      parameter ( n_max = 10 )

      double precision a
      double precision alpha(n_max)
      double precision b
      double precision beta(n_max)
      integer i
      integer n

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST14'
      write ( *, '(a)' ) 
     &  '  R_JACOBI computes recursion coefficients ALPHA and BETA'
      write ( *, '(a)' ) 
     &  '  Gauss quadrature rule, given the ALPHA and BETA'
      write ( *, '(a)' ) '  recursion coefficients.'
c
c  Legendre rule.
c
      n = 10

      a = 0.0D+00
      b = 0.0D+00

      call r_jacobi ( n, a, b, alpha, beta )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Legendre weight'
      write ( *, '(a,g14.6,a,g14.6)' ) '  A = ', a, '  B = ', b
      write ( *, '(a)' ) '  Alpha          Beta'
      write ( *, '(a)' ) ''
      do i = 1, n
        write ( *, '(2x,g14.6,2x,g14.6)' ) alpha(i), beta(i)
      end do
c
c  Chebyshev Type 1 rule.
c
      n = 10

      a = -0.5D+00
      b = -0.5D+00

      call r_jacobi ( n, a, b, alpha, beta )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Chebyshev Type 1 weight'
      write ( *, '(a,g14.6,a,g14.6)' ) '  A = ', a, '  B = ', b
      write ( *, '(a)' ) '  Alpha          Beta'
      write ( *, '(a)' ) ''
      do i = 1, n
        write ( *, '(2x,g14.6,2x,g14.6)' ) alpha(i), beta(i)
      end do
c
c  Chebyshev Type 2 rule.
c
      n = 10

      a = +0.5D+00
      b = +0.5D+00

      call r_jacobi ( n, a, b, alpha, beta )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Chebyshev Type 2 weight'
      write ( *, '(a,g14.6,a,g14.6)' ) '  A = ', a, '  B = ', b
      write ( *, '(a)' ) '  Alpha          Beta'
      write ( *, '(a)' ) ''
      do i = 1, n
        write ( *, '(2x,g14.6,2x,g14.6)' ) alpha(i), beta(i)
      end do
c
c  General Jacobi rule.
c
      n = 10

      a = +0.5D+00
      b = +1.5D+00

      call r_jacobi ( n, a, b, alpha, beta )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  General Jacobi weight'
      write ( *, '(a,g14.6,a,g14.6)' ) '  A = ', a, '  B = ', b
      write ( *, '(a)' ) '  Alpha          Beta'
      write ( *, '(a)' ) ''
      do i = 1, n
        write ( *, '(2x,g14.6,2x,g14.6)' ) alpha(i), beta(i)
      end do

      return
      end
