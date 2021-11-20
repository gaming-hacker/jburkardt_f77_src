      program main

c*********************************************************************72
c
cc MAIN is the main program for NEAREST_INTERP_1D_PRB.
c
c  Discussion:
c
c    NEAREST_INTERP_1D_PRB tests the NEAREST_INTERP_1D library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 May 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer ni
      integer prob
      integer prob_num

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'NEAREST_INTERP_1D_TEST:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the NEAREST_INTERP_1D library.'
      write ( *, '(a)' ) '  The R8LIB library is needed.'
      write ( *, '(a)' ) 
     &  '  The test needs the TEST_INTERP library.'

      call p00_prob_num ( prob_num )

      ni = 11
      do prob = 1, prob_num
        call nearest_interp_1d_test01 ( prob, ni )
      end do

      do prob = 1, prob_num
        call nearest_interp_1d_test02 ( prob )
      end do
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'NEAREST_INTERP_1D_TEST:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      return
      end
      subroutine nearest_interp_1d_test01 ( prob, ni )

c*********************************************************************72
c
cc NEAREST_INTERP_1D_TEST01 tests NEAREST_INTERP_1D.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 September 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer PROB, the index of the problem.
c
c    Input, integer NI, the number of interpolation points.
c
      implicit none

      integer nd_max
      parameter ( nd_max = 49 )

      double precision d(2,nd_max)
      integer j
      integer ni
      integer nd
      integer prob
      character * ( 80 ) title
      double precision xd(nd_max)
      double precision xi(ni)
      double precision xd_max
      double precision xd_min
      double precision yd(nd_max)
      double precision yi(ni)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'NEAREST_INTERP_1D_TEST01'
      write ( *, '(a,i2)' ) 
     &  '  Sample the nearest neighbor interpolant for problem # ', prob

      call p00_data_num ( prob, nd )

      call p00_data ( prob, 2, nd, d )

      do j = 1, nd
        xd(j) = d(1,j)
        yd(j) = d(2,j)
      end do

      call r8vec_min ( nd, xd, xd_min )
      call r8vec_max ( nd, xd, xd_max )

      call r8vec_linspace ( ni, xd_min, xd_max, xi )
      call nearest_interp_1d ( nd, xd, yd, ni, xi, yi )

      write ( title, '(a,i2)' ) 'X, Y for problem ', prob

      call r8vec2_print ( ni, xi, yi, title )

      return
      end
      subroutine nearest_interp_1d_test02 ( prob )

c*********************************************************************72
c
cc NEAREST_INTERP_1D_TEST02 tests NEAREST_INTERP_1D.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 May 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer PROB, the index of the problem.
c
      implicit none

      integer nd_max
      parameter ( nd_max = 49 )
      integer ni_max
      parameter ( ni_max = 501 )

      character * ( 255 ) command_filename
      integer command_unit
      character * ( 255 ) data_filename
      integer data_unit
      integer i
      double precision interp_error
      character * ( 255 ) interp_filename
      integer interp_unit
      integer j
      integer ni
      integer nd
      character * ( 255 ) output_filename
      integer prob
      double precision r8vec_diff_norm
      character * ( 80 ) title
      double precision xd(nd_max)
      double precision xi(ni_max)
      double precision xmax
      double precision xmin
      double precision xy(2,nd_max)
      double precision yd(nd_max)
      double precision yi(ni_max)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'NEAREST_INTERP_1D_TEST02:'
      write ( *, '(a,i2)' ) 
     &  '  Interpolate data from TEST_INTERP problem #', prob

      call p00_data_num ( prob, nd )
      write ( *, '(a,i6)' ) '  Number of data points = ', nd

      call p00_data ( prob, 2, nd, xy )
      
      call r8mat_transpose_print ( 2, nd, xy, '  Data array:' )

      do i = 1, nd
        xd(i) = xy(1,i)
        yd(i) = xy(2,i)
      end do
c
c  #1:  Does interpolant match function at interpolation points?
c
      ni = nd
      do i = 1, ni
        xi(i) = xd(i)
      end do

      call nearest_interp_1d ( nd, xd, yd, ni, xi, yi )
 
      interp_error = r8vec_diff_norm ( ni, yi, yd ) / dble ( ni )

      write ( *, '(a)' ) ''
      write ( *, '(a,g14.6)' ) 
     &  '  Node-averaged L2 interpolation error = ', interp_error
c
c  Create data file.
c
      write ( data_filename, '(a,i2.2,a)' ) 'data', prob, '.txt'
      call get_unit ( data_unit )
      open ( unit = data_unit, file = data_filename, 
     &  status = 'replace' )
      do j = 1, nd
        write ( data_unit, '(2x,g14.6,2x,g14.6)' ) xd(j), yd(j)
      end do
      close ( unit = data_unit )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Created graphics data file "' 
     &  // trim ( data_filename ) // '".'
c
c  Create interp file.
c
      ni = 501
      call r8vec_min ( nd, xd, xmin )
      call r8vec_max ( nd, xd, xmax )
      call r8vec_linspace ( ni, xmin, xmax, xi )
      call nearest_interp_1d ( nd, xd, yd, ni, xi, yi )

      write ( interp_filename, '(a,i2.2,a)' ) 'interp', prob, '.txt'
      call get_unit ( interp_unit )
      open ( unit = interp_unit, file = interp_filename, 
     &  status = 'replace' )
      do j = 1, ni
        write ( interp_unit, '(2x,g14.6,2x,g14.6)' ) xi(j), yi(j)
      end do
      close ( unit = interp_unit )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Created graphics interp file "' 
     &  // trim ( interp_filename ) // '".'
c
c  Plot the data and the interpolant.
c
      write ( command_filename, '(a,i2.2,a)' ) 'commands', prob, '.txt'
      call get_unit ( command_unit )
      open ( unit = command_unit, file = command_filename, 
     &  status = 'replace' )

      write ( output_filename, '(a,i2.2,a)' ) 'plot', prob, '.png'

      write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) '# Usage:'
      write ( command_unit, '(a)' ) 
     &  '#      gnuplot < ' // trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) 'set term png'
      write ( command_unit, '(a)' ) 
     &  'set output "' // trim ( output_filename ) // '"'
      write ( command_unit, '(a)' ) 'set xlabel "<---X--->"'
      write ( command_unit, '(a)' ) 'set ylabel "<---Y--->"'
      write ( command_unit, '(a)' ) 
     &  'set title "Data versus Nearest Neighbor Interpolant"'
      write ( command_unit, '(a)' ) 'set grid'
      write ( command_unit, '(a)' ) 'set style data lines'
      write ( command_unit, '(a)' ) 
     &  'plot "' // trim ( data_filename ) // 
     &  '" using 1:2 with points pt 7 ps 2 lc rgb "blue",\'
      write ( command_unit, '(a)' ) 
     &  '     "' // trim ( interp_filename ) // 
     &  '" using 1:2 lw 3 linecolor rgb "red"'

      close ( unit = command_unit )
      write ( *, '(a)' ) 
     &  '  Created graphics command file "' 
     &  // trim ( command_filename ) // '".'

      return
      end
