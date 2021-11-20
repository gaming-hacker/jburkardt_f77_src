      program main

c*********************************************************************72
c
cc EDGE_PRB tests the EDGE library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 September 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'EDGE_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version.'
      write ( *, '(a)' ) '  Test the EDGE library.'

      call test01 ( )
      call test02 ( )
      call test03 ( )
      call test035 ( )
      call test036 ( )
      call test037 ( )
      call test04 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'EDGE_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      return
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 plots functions with jump discontinuities.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 September 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Rick Archibald, Anne Gelb, Jungho Yoon,
c    Polynomial fitting for edge detection in irregularly sampled signals 
c    and images,
c    SIAM Journal on Numerical Analysis,
c    Volume 43, Number 1, 2006, pages 259-279.
c
      implicit none

      integer n
      parameter ( n = 101 )

      character * ( 80 ) command_filename
      integer command_unit
      character * ( 80 ) data_filename
      integer data_unit
      double precision f(n)
      character * ( 80 ) header
      integer i
      integer test
      integer test_num
      character * ( 80 ) title
      double precision x(n)
      double precision x_max
      double precision x_min

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST01:'
      write ( *, '(a)' ) '  Plot 1D test functions.'
    
      test_num = 7

      do test = 1, test_num

        if ( test .eq. 1 ) then
          x_min = -1.0D+00
          x_max = +1.0D+00
          call r8vec_linspace ( n, x_min, x_max, x )
          header = 'fx1'
          call fx1 ( n, x, f )
          title = '1D Test Function #1'
        else if ( test .eq. 2 ) then
          x_min = -1.0D+00
          x_max = +1.0D+00
          call r8vec_linspace ( n, x_min, x_max, x )
          header = 'fx2'
          call fx2 ( n, x, f )
          title = '1D Test Function #2'
        else if ( test .eq. 3 ) then
          x_min = -1.0D+00
          x_max = +1.0D+00
          call r8vec_linspace ( n, x_min, x_max, x )
          header = 'fx3'
          call fx3 ( n, x, f )
          title = '1D Test Function #3'
        else if ( test .eq. 4 ) then
          x_min = 0.0D+00
          x_max = +1.0D+00
          call r8vec_linspace ( n, x_min, x_max, x )
          header = 'fx4'
          call fx4 ( n, x, f )
          title = '1D Test Function #4'
        else if ( test .eq. 5 ) then
          x_min = -1.0D+00
          x_max = +1.0D+00
          call r8vec_linspace ( n, x_min, x_max, x )
          header = 'fx5'
          call fx5 ( n, x, f )
          title = '1D Test Function #5'
        else if ( test .eq. 6 ) then
          x_min = 0.0D+00
          x_max = +1.0D+00
          call r8vec_linspace ( n, x_min, x_max, x )
          header = 'fx6'
          call fx6 ( n, x, f )
          title = '1D Test Function #6'
        else if ( test .eq. 7 ) then
          x_min = 0.0D+00
          x_max = +1.0D+00
          call r8vec_linspace ( n, x_min, x_max, x )
          header = 'fx7'
          call fx7 ( n, x, f )
          title = '1D Test Function #7'
        end if

        call get_unit ( data_unit )
        data_filename = trim ( header ) // '_data.txt'
        open ( unit = data_unit, file = data_filename, 
     &    status = 'replace' )
        do i = 1, n
          write ( data_unit, '(2x,g14.6,2x,g14.6)' ) x(i), f(i)
        end do
        close ( unit = data_unit )
        write ( *, '(a)' ) 
     &  '  Created data file "' // trim ( data_filename ) // '".'

        call get_unit ( command_unit )
        command_filename = trim ( header ) // '_commands.txt'
        open ( unit = command_unit, file = command_filename, 
     &    status = 'replace' )
        write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
        write ( command_unit, '(a)' ) '#'
        write ( command_unit, '(a)' ) '# Usage:'
        write ( command_unit, '(a)' ) 
     &  '#  gnuplot < ' // trim ( command_filename )
        write ( command_unit, '(a)' ) '#'
        write ( command_unit, '(a)' ) 'set term png'
        write ( command_unit, '(a)' ) 
     &    'set output "' // trim ( header ) // '.png"'
        write ( command_unit, '(a)' ) 'set xlabel "<--- X --->"'
        write ( command_unit, '(a)' ) 'set ylabel "<--- Y --->"'
        write ( command_unit, '(a)' ) 
     &    'set title "' // trim ( title ) // '"'
        write ( command_unit, '(a)' ) 'set grid'
        write ( command_unit, '(a)' ) 'set style data lines'
        write ( command_unit, '(a)' ) 
     &    'plot "' // trim ( data_filename ) // 
     &    '" using 1:2 with points lt 3 pt 4 linecolor rgb "blue"'
        write ( command_unit, '(a)' ) 'quit'
        close ( unit = command_unit )
        write ( *, '(a)' ) 
     &    '  Created command file "' 
     &    // trim ( command_filename ) // '".'

      end do

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 plots a function with a jump discontinuity along a circle.
c
c  Discussion:
c
c    This is example 4.1 in the reference.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Rick Archibald, Anne Gelb, Jungho Yoon,
c    Polynomial fitting for edge detection in irregularly sampled signals 
c    and images,
c    SIAM Journal on Numerical Analysis,
c    Volume 43, Number 1, 2006, pages 259-279.
c
      implicit none

      integer n
      parameter ( n = 101 )

      character * ( 80 ) command_filename
      integer command_unit
      character * ( 80 ) data_filename
      integer data_unit
      double precision fxy
      character * ( 80 ) header
      integer i
      integer j
      integer test
      integer test_num
      character * ( 80 ) title
      double precision x(n)
      double precision x_max
      double precision x_min
      double precision y(n)
      double precision y_max
      double precision y_min

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST02:'
      write ( *, '(a)' ) 
     &  '  Plot 2D test function #1 with jump along circle.'

      header = 'fxy1'
      title = '2D test function #1 with discontinuity along circle'

      x_min = -1.0D+00
      x_max = +1.0D+00
      call r8vec_linspace ( n, x_min, x_max, x )
      y_min = -1.0D+00
      y_max = +1.0D+00
      call r8vec_linspace ( n, y_min, y_max, y )

      call get_unit ( data_unit )
      data_filename = trim ( header ) // '_data.txt'
      open ( unit = data_unit, file = data_filename, 
     &  status = 'replace' )
      do i = 1, n
        do j = 1, n
          call fxy1 ( 1, x(i), y(j), fxy )
          write ( data_unit, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &      x(i), y(j), fxy
        end do
        write ( data_unit, '(a)' ) ''
      end do
      close ( unit = data_unit )
      write ( *, '(a)' ) '  Created data file "' // 
     &  trim ( data_filename ) // '".'

      call get_unit ( command_unit )
      command_filename = trim ( header ) // '_commands.txt'
      open ( unit = command_unit, file = command_filename, 
     &  status = 'replace' )
      write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) '# Usage:'
      write ( command_unit, '(a)' ) 
     &  '#  gnuplot < ' // trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) 'set term png'
      write ( command_unit, '(a)' ) 
     &  'set output "' // trim ( header ) // '.png"'
      write ( command_unit, '(a)' ) 'set view 120, 77'
      write ( command_unit, '(a)' ) 'set hidden3d'
      write ( command_unit, '(a)' ) 'set timestamp'
      write ( command_unit, '(a)' ) 'set xlabel "<--- X --->"'
      write ( command_unit, '(a)' ) 'set ylabel "<--- Y --->"'
      write ( command_unit, '(a)' ) 'set zlabel "<--- Z --->"'
      write ( command_unit, '(a)' ) 
     &  'set title "' // trim ( title ) // '"'
      write ( command_unit, '(a)' ) 'set grid'
      write ( command_unit, '(a)' ) 'set style data lines'
      write ( command_unit, '(a)' ) 'splot "' // 
     &  trim ( data_filename ) // '" with lines'
      write ( command_unit, '(a)' ) 'quit'
      close ( unit = command_unit )
      write ( *, '(a)' ) 
     &  '  Created command file "' // trim ( command_filename ) // '".'

      return
      end
      subroutine test03 ( )

c*********************************************************************72
c
cc TEST03 plots a function with a jump discontinuity along a circle.
c
c  Discussion:
c
c    This is example 4.2 in the reference.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Rick Archibald, Anne Gelb, Jungho Yoon,
c    Polynomial fitting for edge detection in irregularly sampled signals 
c    and images,
c    SIAM Journal on Numerical Analysis,
c    Volume 43, Number 1, 2006, pages 259-279.
c
      implicit none

      integer n
      parameter ( n = 101 )

      character * ( 80 ) command_filename
      integer command_unit
      character * ( 80 ) data_filename
      integer data_unit
      double precision fxy
      character * ( 80 ) header
      integer i
      integer j
      integer test
      integer test_num
      character * ( 80 ) title
      double precision x(n)
      double precision x_max
      double precision x_min
      double precision y(n)
      double precision y_max
      double precision y_min

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST03:'
      write ( *, '(a)' ) 
     &  '  Plot 2D test function #2, the Shepp Logan phantom.'

      header = 'fxy2'
      title = '2D test function #2, the Shepp Logan phantom'

      x_min = -1.0D+00
      x_max = +1.0D+00
      call r8vec_linspace ( n, x_min, x_max, x )
      y_min = -1.0D+00
      y_max = +1.0D+00
      call r8vec_linspace ( n, y_min, y_max, y )

      call get_unit ( data_unit )
      data_filename = trim ( header ) // '_data.txt'
      open ( unit = data_unit, file = data_filename, 
     &  status = 'replace' )
      do i = 1, n
        do j = 1, n
          call fxy2 ( 1, x(i), y(j), fxy )
          write ( data_unit, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &      x(i), y(j), fxy
        end do
        write ( data_unit, '(a)' ) ''
      end do
      close ( unit = data_unit )
      write ( *, '(a)' ) '  Created data file "' 
     &  // trim ( data_filename ) // '".'

      call get_unit ( command_unit )
      command_filename = trim ( header ) // '_commands.txt'
      open ( unit = command_unit, file = command_filename, 
     &  status = 'replace' )
      write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) '# Usage:'
      write ( command_unit, '(a)' ) 
     &  '#  gnuplot < ' // trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) 'set term png'
      write ( command_unit, '(a)' ) 
     &  'set output "' // trim ( header ) // '.png"'
      write ( command_unit, '(a)' ) 'set view 30, 75'
      write ( command_unit, '(a)' ) 'set hidden3d'
      write ( command_unit, '(a)' ) 'set timestamp'
      write ( command_unit, '(a)' ) 'set xlabel "<--- X --->"'
      write ( command_unit, '(a)' ) 'set ylabel "<--- Y --->"'
      write ( command_unit, '(a)' ) 'set zlabel "<--- Z --->"'
      write ( command_unit, '(a)' ) 
     &  'set title "' // trim ( title ) // '"'
      write ( command_unit, '(a)' ) 'set grid'
      write ( command_unit, '(a)' ) 'set style data lines'
      write ( command_unit, '(a)' ) 'splot "' // 
     &  trim ( data_filename ) // '" with lines'
      write ( command_unit, '(a)' ) 'quit'
      close ( unit = command_unit )
      write ( *, '(a)' ) 
     &  '  Created command file "' // trim ( command_filename ) // '".'

      return
      end
      subroutine test035 ( )

c*********************************************************************72
c
cc TEST035 plots a function with a jump discontinuity along a circle.
c
c  Discussion:
c
c    This is example 3.2 in the reference.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 September 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Rick Archibald, Anne Gelb, Jungho Yoon,
c    Determining the location of discontinuities in the derivatives
c    of functions,
c    Applied Numerical Mathematics,
c    Volume 58, 2008, pages 577-592.
c
      implicit none

      integer n
      parameter ( n = 101 )

      character * ( 80 ) command_filename
      integer command_unit
      character * ( 80 ) data_filename
      integer data_unit
      double precision fxy
      character * ( 80 ) header
      integer i
      integer j
      integer test
      integer test_num
      character * ( 80 ) title
      double precision x(n)
      double precision x_max
      double precision x_min
      double precision y(n)
      double precision y_max
      double precision y_min

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST035:'
      write ( *, '(a)' ) 
     &  '  Plot 2D test function #3, the modified 2D Harten function.'

      header = 'fxy3'
      title = '2D test function #3, the modified 2D Harten function'

      x_min = -1.0D+00
      x_max = +1.0D+00
      call r8vec_linspace ( n, x_min, x_max, x )
      y_min = -1.0D+00
      y_max = +1.0D+00
      call r8vec_linspace ( n, y_min, y_max, y )

      call get_unit ( data_unit )
      data_filename = trim ( header ) // '_data.txt'
      open ( unit = data_unit, file = data_filename, 
     &  status = 'replace' )
      do i = 1, n
        do j = 1, n
          call fxy3 ( 1, x(i), y(j), fxy )
          write ( data_unit, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &      x(i), y(j), fxy
        end do
        write ( data_unit, '(a)' ) ''
      end do
      close ( unit = data_unit )
      write ( *, '(a)' ) '  Created data file "' 
     &  // trim ( data_filename ) // '".'

      call get_unit ( command_unit )
      command_filename = trim ( header ) // '_commands.txt'
      open ( unit = command_unit, file = command_filename, 
     &  status = 'replace' )
      write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) '# Usage:'
      write ( command_unit, '(a)' ) 
     &  '#  gnuplot < ' // trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) 'set term png'
      write ( command_unit, '(a)' ) 
     &  'set output "' // trim ( header ) // '.png"'
      write ( command_unit, '(a)' ) 'set view 30, 75'
      write ( command_unit, '(a)' ) 'set hidden3d'
      write ( command_unit, '(a)' ) 'set timestamp'
      write ( command_unit, '(a)' ) 'set xlabel "<--- X --->"'
      write ( command_unit, '(a)' ) 'set ylabel "<--- Y --->"'
      write ( command_unit, '(a)' ) 'set zlabel "<--- Z --->"'
      write ( command_unit, '(a)' ) 
     &  'set title "' // trim ( title ) // '"'
      write ( command_unit, '(a)' ) 'set grid'
      write ( command_unit, '(a)' ) 'set style data lines'
      write ( command_unit, '(a)' ) 'splot "' // 
     &  trim ( data_filename ) // '" with lines'
      write ( command_unit, '(a)' ) 'quit'
      close ( unit = command_unit )
      write ( *, '(a)' ) 
     &  '  Created command file "' // trim ( command_filename ) // '".'

      return
      end
      subroutine test036 ( )

c*********************************************************************72
c
cc TEST036 plots a function with a derivative discontinuity.
c
c  Discussion:
c
c    This is example 3.1 in the reference.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 September 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Rick Archibald, Anne Gelb, Jungho Yoon,
c    Determining the location of discontinuities in the derivatives
c    of functions,
c    Applied Numerical Mathematics,
c    Volume 58, 2008, pages 577-592.
c
      implicit none

      integer n
      parameter ( n = 101 )

      character * ( 80 ) command_filename
      integer command_unit
      character * ( 80 ) data_filename
      integer data_unit
      double precision fxy
      character * ( 80 ) header
      integer i
      integer j
      integer test
      integer test_num
      character * ( 80 ) title
      double precision x(n)
      double precision x_max
      double precision x_min
      double precision y(n)
      double precision y_max
      double precision y_min

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST036:'
      write ( *, '(a)' ) 
     &  '  Plot 2D test function #4, discontinuous medium wave P(x,t).'

      header = 'fxy4'
      title = '2D test function #4, discontinuous medium wave P(x,t).'

      x_min = -1.0D+00
      x_max = +0.0D+00
      call r8vec_linspace ( n, x_min, x_max, x )
      y_min = 0.0D+00
      y_max = +0.1D+00
      call r8vec_linspace ( n, y_min, y_max, y )

      call get_unit ( data_unit )
      data_filename = trim ( header ) // '_data.txt'
      open ( unit = data_unit, file = data_filename, 
     &  status = 'replace' )
      do i = 1, n
        do j = 1, n
          call fxy4 ( 1, x(i), y(j), fxy )
          write ( data_unit, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &      x(i), y(j), fxy
        end do
        write ( data_unit, '(a)' ) ''
      end do
      close ( unit = data_unit )
      write ( *, '(a)' ) '  Created data file "' 
     &  // trim ( data_filename ) // '".'

      call get_unit ( command_unit )
      command_filename = trim ( header ) // '_commands.txt'
      open ( unit = command_unit, file = command_filename, 
     &  status = 'replace' )
      write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) '# Usage:'
      write ( command_unit, '(a)' ) 
     &  '#  gnuplot < ' // trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) 'set term png'
      write ( command_unit, '(a)' ) 
     &  'set output "' // trim ( header ) // '.png"'
      write ( command_unit, '(a)' ) 'set view 30, 45'
      write ( command_unit, '(a)' ) 'set hidden3d'
      write ( command_unit, '(a)' ) 'set timestamp'
      write ( command_unit, '(a)' ) 'set xlabel "<--- X --->"'
      write ( command_unit, '(a)' ) 'set ylabel "<--- Y --->"'
      write ( command_unit, '(a)' ) 'set zlabel "<--- Z --->"'
      write ( command_unit, '(a)' ) 
     &  'set title "' // trim ( title ) // '"'
      write ( command_unit, '(a)' ) 'set grid'
      write ( command_unit, '(a)' ) 'set style data lines'
      write ( command_unit, '(a)' ) 'splot "' // 
     &  trim ( data_filename ) // '" with lines'
      write ( command_unit, '(a)' ) 'quit'
      close ( unit = command_unit )
      write ( *, '(a)' ) 
     &  '  Created command file "' // trim ( command_filename ) // '".'

      return
      end
      subroutine test037 ( )

c*********************************************************************72
c
cc TEST037 plots a function with a derivative discontinuity.
c
c  Discussion:
c
c    This is example 3.1 in the reference.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 September 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Rick Archibald, Anne Gelb, Jungho Yoon,
c    Determining the location of discontinuities in the derivatives
c    of functions,
c    Applied Numerical Mathematics,
c    Volume 58, 2008, pages 577-592.
c
      implicit none

      integer n
      parameter ( n = 101 )

      character * ( 80 ) command_filename
      integer command_unit
      character * ( 80 ) data_filename
      integer data_unit
      double precision fxy
      character * ( 80 ) header
      integer i
      integer j
      integer test
      integer test_num
      character * ( 80 ) title
      double precision x(n)
      double precision x_max
      double precision x_min
      double precision y(n)
      double precision y_max
      double precision y_min

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST037:'
      write ( *, '(a)' ) 
     &  '  Plot 2D test function #5, discontinuous medium wave U(x,t).'

      header = 'fxy5'
      title = '2D test function #5, discontinuous medium wave U(x,t).'

      x_min = -1.0D+00
      x_max = +0.0D+00
      call r8vec_linspace ( n, x_min, x_max, x )
      y_min = 0.0D+00
      y_max = +0.1D+00
      call r8vec_linspace ( n, y_min, y_max, y )

      call get_unit ( data_unit )
      data_filename = trim ( header ) // '_data.txt'
      open ( unit = data_unit, file = data_filename, 
     &  status = 'replace' )
      do i = 1, n
        do j = 1, n
          call fxy5 ( 1, x(i), y(j), fxy )
          write ( data_unit, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &      x(i), y(j), fxy
        end do
        write ( data_unit, '(a)' ) ''
      end do
      close ( unit = data_unit )
      write ( *, '(a)' ) '  Created data file "' 
     &  // trim ( data_filename ) // '".'

      call get_unit ( command_unit )
      command_filename = trim ( header ) // '_commands.txt'
      open ( unit = command_unit, file = command_filename, 
     &  status = 'replace' )
      write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) '# Usage:'
      write ( command_unit, '(a)' ) 
     &  '#  gnuplot < ' // trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) 'set term png'
      write ( command_unit, '(a)' ) 
     &  'set output "' // trim ( header ) // '.png"'
      write ( command_unit, '(a)' ) 'set view 30, 45'
      write ( command_unit, '(a)' ) 'set hidden3d'
      write ( command_unit, '(a)' ) 'set timestamp'
      write ( command_unit, '(a)' ) 'set xlabel "<--- X --->"'
      write ( command_unit, '(a)' ) 'set ylabel "<--- Y --->"'
      write ( command_unit, '(a)' ) 'set zlabel "<--- Z --->"'
      write ( command_unit, '(a)' ) 
     &  'set title "' // trim ( title ) // '"'
      write ( command_unit, '(a)' ) 'set grid'
      write ( command_unit, '(a)' ) 'set style data lines'
      write ( command_unit, '(a)' ) 'splot "' // 
     &  trim ( data_filename ) // '" with lines'
      write ( command_unit, '(a)' ) 'quit'
      close ( unit = command_unit )
      write ( *, '(a)' ) 
     &  '  Created command file "' // trim ( command_filename ) // '".'

      return
      end
      subroutine test04 ( )

c*********************************************************************72
c
cc TEST04 plots slices of a 3D function.
c
c  Discussion:
c
c    Although the slice plots look uninteresting, there is a lot of detail
c    hidden in the data in variations that are not obvious at first.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Larry Shepp,
c    Computerized tomography and nuclear magnetic resonance,
c    Journal of Computer Assisted Tomography,
c    Volume 4, Number 1, February 1980, pages 94-107.
c
      implicit none

      integer n
      parameter ( n = 101 )

      character * ( 80 ) command_filename
      integer command_unit
      character * ( 80 ) data_filename
      integer data_unit
      double precision fxyz
      character * ( 80 ) header
      integer i
      integer j
      integer k
      integer test
      integer test_num
      character * ( 80 ) title
      double precision x(n)
      double precision x_max
      double precision x_min
      double precision x_val
      double precision y(n)
      double precision y_max
      double precision y_min
      double precision y_val
      double precision z(n)
      double precision z_max
      double precision z_min
      double precision z_val

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST04:'
      write ( *, '(a)' ) 
     &  '  Plot 3D test function #1, the Shepp Logan 3D phantom.'

      test_num = 3

      x_min = -1.5D+00
      x_max = +1.5D+00
      call r8vec_linspace ( n, x_min, x_max, x )
      y_min = -1.5D+00
      y_max = +1.5D+00
      call r8vec_linspace ( n, y_min, y_max, y )
      z_min = -1.5D+00
      z_max = +1.5D+00
      call r8vec_linspace ( n, z_min, z_max, z )

      do test = 1, test_num

        if ( test .eq. 1 ) then
          x_val = 0.0D+00
          title = 'Slice X = 0.0'
          header = 'fxyz1_x'
        else if ( test .eq. 2 ) then
          y_val = 0.0D+00
          title = 'Slice Y = 0.0'
          header = 'fxyz1_y'
        else if ( test .eq. 3 ) then
          z_val = - 0.1D+00
          title = 'Slice Z = - 0.1'
          header = 'fxyz1_z'
        end if

        call get_unit ( data_unit )
        data_filename = trim ( header ) // '_data.txt'
        open ( unit = data_unit, file = data_filename, 
     &    status = 'replace' )
        do i = 1, n
          do j = 1, n
            if ( test .eq. 1 ) then
              call fxyz1 ( 1, x_val, y(j), z(i), fxyz )
              write ( data_unit, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &          y(j), z(i), fxyz
            else if ( test .eq. 2 ) then
              call fxyz1 ( 1, x(j), y_val, z(i), fxyz )
              write ( data_unit, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &          x(j), z(i), fxyz
            else if ( test .eq. 3 ) then
              call fxyz1 ( 1, x(j), y(i), z_val, fxyz )
              write ( data_unit, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &          x(j), y(i), fxyz
            end if

         end do
          write ( data_unit, '(a)' ) ''
        end do
        close ( unit = data_unit )
        write ( *, '(a)' ) '  Created data file "' // 
     &    trim ( data_filename ) // '".'

        call get_unit ( command_unit )
        command_filename = trim ( header ) // '_commands.txt'
        open ( unit = command_unit, file = command_filename, 
     &    status = 'replace' )
        write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
        write ( command_unit, '(a)' ) '#'
        write ( command_unit, '(a)' ) '# Usage:'
        write ( command_unit, '(a)' ) 
     &    '#  gnuplot < ' // trim ( command_filename )
        write ( command_unit, '(a)' ) '#'
        write ( command_unit, '(a)' ) 'set term png'
        write ( command_unit, '(a)' ) 
     &    'set output "' // trim ( header ) // '.png"'
        write ( command_unit, '(a)' ) 'set view 20, 75'
        write ( command_unit, '(a)' ) 'set hidden3d'
        write ( command_unit, '(a)' ) 'set timestamp'
        if ( test .eq. 1 ) then
          write ( command_unit, '(a)' ) 'set xlabel "<--- Y --->"'
          write ( command_unit, '(a)' ) 'set ylabel "<--- Z --->"'
          write ( command_unit, '(a)' ) 'set zlabel "<--- X --->"'
        else if ( test .eq. 2 ) then
          write ( command_unit, '(a)' ) 'set xlabel "<--- X --->"'
          write ( command_unit, '(a)' ) 'set ylabel "<--- Z --->"'
          write ( command_unit, '(a)' ) 'set zlabel "<--- Y --->"'
        else if ( test .eq. 3 ) then
          write ( command_unit, '(a)' ) 'set xlabel "<--- X --->"'
          write ( command_unit, '(a)' ) 'set ylabel "<--- Y --->"'
          write ( command_unit, '(a)' ) 'set zlabel "<--- Z --->"'
        end if
        write ( command_unit, '(a)' ) 
     &    'set title "' // trim ( title ) // '"'
        write ( command_unit, '(a)' ) 'set grid'
        write ( command_unit, '(a)' ) 'set style data lines'
        write ( command_unit, '(a)' ) 
     &    'splot "' // trim ( data_filename ) // '" with lines'
        write ( command_unit, '(a)' ) 'quit'
        close ( unit = command_unit )
        write ( *, '(a)' ) 
     &    '  Created command file "' // 
     &    trim ( command_filename ) // '".'

      end do

      return
      end
