      program main

c*********************************************************************72
c
cc MAIN is the main program for VANDERMONDE_INTERP_1D_PRB.
c
c  Discussion:
c
c    VANDERMONDE_INTERP_1D_PRB tests the VANDERMONDE_INTERP_1D library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 September 2012
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer prob
      integer prob_num

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'VANDERMONDE_INTERP_1D_PRB:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the VANDERMONDE_INTERP_1D library.'
      write ( *, '(a)' ) '  The R8LIB library is needed.'
      write ( *, '(a)' ) '  The QR_SOLVE library is needed.'
      write ( *, '(a)' ) '  This test needs the TEST_INTERP library.'
      write ( *, '(a)' ) '  This test needs the CONDITION library.'

      call p00_prob_num ( prob_num )

      do prob = 1, prob_num
        call test01 ( prob )
      end do

      do prob = 1, prob_num
        call test02 ( prob )
      end do
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'VANDERMONDE_INTERP_1D_PRB:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      return
      end
      subroutine test01 ( prob )

c*********************************************************************72
c
cc TEST01 tests VANDERMONDE_INTERP_1D_MATRIX.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 October 2012
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer nd_max
      parameter ( nd_max = 49 )
      integer ni_max
      parameter ( ni_max = 501 )

      double precision a(nd_max,nd_max)
      double precision c(nd_max)
      double precision condition
      logical debug
      parameter ( debug = .false. )
      integer i
      double precision int_error
      double precision ld
      double precision li
      integer m
      integer nd
      integer ni
      integer prob
      double precision r8vec_norm_affine
      double precision xd(nd_max)
      double precision xi(ni_max)
      double precision xmax
      double precision xmin
      double precision xy(2,nd_max)
      double precision yd(nd_max)
      double precision yi(ni_max)
      double precision ymax
      double precision ymin

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST01:'
      write ( *, '(a,i2)' ) 
     &  '  Interpolate data from TEST_INTERP problem #', prob

      call p00_data_num ( prob, nd )
      write ( *, '(a,i2)' ) '  Number of data points = ', nd

      call p00_data ( prob, 2, nd, xy )
      
      if ( debug ) then
        call r8mat_transpose_print ( 2, nd, xy, '  Data array:' )
      end if

      do i = 1, nd
        xd(i) = xy(1,i)
        yd(i) = xy(2,i)
      end do
c
c  Choose the degree of the polynomial to be ND - 1.
c
      m = nd - 1
c
c  Compute Vandermonde matrix and get condition number.
c
      call vandermonde_interp_1d_matrix ( nd, xd, a )

      call condition_hager ( nd, a, condition )

      write ( *, '(a)' ) ''
      write ( *, '(a,g14.6)' ) 
     &  '  Condition of Vandermonde matrix is ', condition
c
c  Solve linear system.
c
      call qr_solve ( nd, nd, a, yd, c )
c
c  #1:  Does interpolant match function at interpolation points?
c
      ni = nd
      do i = 1, ni
        xi(i) = xd(i)
      end do
      call r8poly_value ( m, c, ni, xi, yi )

      int_error = r8vec_norm_affine ( ni, yi, yd ) / dble ( ni )

      write ( *, '(a)' ) ''
      write ( *, '(a,g14.6)' ) 
     &  '  L2 interpolation error averaged per interpolant node = ', 
     &  int_error
c
c  #2: Compare estimated curve length to piecewise linear (minimal) curve length.
c  Assume data is sorted, and normalize X and Y dimensions by (XMAX-XMIN) and
c  (YMAX-YMIN).
c
      call r8vec_min ( nd, xd, xmin )
      call r8vec_max ( nd, xd, xmax )
      call r8vec_min ( nd, yd, ymin )
      call r8vec_max ( nd, yd, ymax )

      ni = 501
      call r8vec_linspace ( ni, xmin, xmax, xi )
      call r8poly_value ( m, c, ni, xi, yi )

      ld = 0.0D+00
      do i = 1, nd - 1
        ld = ld + sqrt 
     &    ( ( ( xd(i+1) - xd(i) ) / ( xmax - xmin ) ) ** 2 
     &    + ( ( yd(i+1) - yd(i) ) / ( ymax - ymin ) ) ** 2 ) 
      end do

      li = 0.0D+00
      do i = 1, ni - 1
        li = li + sqrt 
     &    ( ( ( xi(i+1) - xi(i) ) / ( xmax - xmin ) ) ** 2 
     &    + ( ( yi(i+1) - yi(i) ) / ( ymax - ymin ) ) ** 2 ) 
      end do

      write ( *, '(a)' ) ''
      write ( *, '(a,g14.6)' ) 
     &  '  Normalized length of piecewise linear interpolant = ', ld
      write ( *, '(a,g14.6)' ) 
     &  '  Normalized length of polynomial interpolant       = ', li

      return
      end
      subroutine test02 ( prob )

c*********************************************************************72
c
cc TEST02 tests VANDERMONDE_INTERP_1D_MATRIX.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 June 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer PROB, the problem index.
c
      implicit none

      integer nd_max
      parameter ( nd_max = 49 )
      integer ni_max
      parameter ( ni_max = 501 )

      double precision a(nd_max,nd_max)
      double precision c(nd_max)
      character * ( 255 ) command_filename
      integer command_unit
      character * ( 255 ) data_filename
      integer data_unit
      integer i
      character * ( 255 ) interp_filename
      integer interp_unit
      integer j
      integer nd
      integer ni
      character * ( 255 ) output_filename
      integer prob
      character * ( 255 ) title
      double precision xd(nd_max)
      double precision xi(ni_max)
      double precision xmax
      double precision xmin
      double precision xy(2,nd_max)
      double precision yd(nd_max)
      double precision yi(ni_max)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST02:'
      write ( *, '(a)' ) 
     &  '  VANDERMONDE_INTERP_1D_MATRIX sets the Vandermonde linear'
      write ( *, '(a)' ) '  system for the interpolating polynomial.'
      write ( *, '(a,i2)' ) 
     &  '  Interpolate data from TEST_INTERP problem #', prob

      call p00_data_num ( prob, nd )
      write ( *, '(a,i4)' ) '  Number of data points = ', nd

      call p00_data ( prob, 2, nd, xy )
      
      call r8mat_transpose_print ( 2, nd, xy, '  Data array:' )

      do i = 1, nd
        xd(i) = xy(1,i)
        yd(i) = xy(2,i)
      end do
c
c  Set up the Vandermonde matrix A.
c
      call vandermonde_interp_1d_matrix ( nd, xd, a )
c
c  Solve the linear system for the polynomial coefficients C.
c
      call qr_solve ( nd, nd, a, yd, c )
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
      call r8poly_value ( nd - 1, c, ni, xi, yi )

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
     &  '#  gnuplot < ' // trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) 'set term png'
      write ( command_unit, '(a)' ) 
     &  'set output "' // trim ( output_filename ) // '"'
      write ( command_unit, '(a)' ) 'set xlabel "<---X--->"'
      write ( command_unit, '(a)' ) 'set ylabel "<---Y--->"'
      write ( command_unit, '(a)' ) 
     &  'set title "Data versus Vandermonde Polynomial Interpolant"'
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
