      program main

c*********************************************************************72
c
cc MAIN is the main program for PCHIP_PRB.
c
c  Discussion:
c
c    PCHIP_PRB tests the PCHIP library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 April 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer ipass
      integer kprint
      integer lun

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'PCHIP_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the PCHIP library.'

      call dpchev_test ( )
      call dpchev_test02 ( )
      call dpchev_test03 ( )
      call dpchqa_test ( )
c
c  Call built in tests.
c
      lun = 6
      kprint = 1

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Calling built-in check routines:'
      write ( *, '(a)' ) ''

      call dpchq1 ( lun, kprint, ipass )

      if ( ipass .eq. 1 ) then
        write ( *, '(a)' ) '  DPCHQ1 passed.'
      else
        write ( *, '(a)' ) '  DPCHQ1 failed.'
      end if

      call dpchq2 ( lun, kprint, ipass )

      if ( ipass .eq. 1 ) then
        write ( *, '(a)' ) '  DPCHQ2 passed.'
      else
        write ( *, '(a)' ) '  DPCHQ2 failed.'
      end if

      call dpchq3 ( lun, kprint, ipass )

      if ( ipass .eq. 1 ) then
        write ( *, '(a)' ) '  DPCHQ3 passed.'
      else
        write ( *, '(a)' ) '  DPCHQ3 failed.'
      end if

      call dpchq4 ( lun, kprint, ipass )

      if ( ipass .eq. 1 ) then
        write ( *, '(a)' ) '  DPCHQ4 passed.'
      else
        write ( *, '(a)' ) '  DPCHQ4 failed.'
      end if

      call dpchq5 ( lun, kprint, ipass )

      if ( ipass .eq. 1 ) then
        write ( *, '(a)' ) '  DPCHQ5 passed.'
      else
        write ( *, '(a)' ) '  DPCHQ5 failed.'
      end if

      call pchqk1 ( lun, kprint, ipass )

      if ( ipass .eq. 1 ) then
        write ( *, '(a)' ) '  PCHQK1 passed.'
      else
        write ( *, '(a)' ) '  PCHQK1 failed.'
      end if

      call pchqk2 ( lun, kprint, ipass )

      if ( ipass .eq. 1 ) then
        write ( *, '(a)' ) '  PCHQK2 passed.'
      else
        write ( *, '(a)' ) '  PCHQK2 failed.'
      end if

      call pchqk3 ( lun, kprint, ipass )

      if ( ipass .eq. 1 ) then
        write ( *, '(a)' ) '  PCHQK3 passed.'
      else
        write ( *, '(a)' ) '  PCHQK3 failed.'
      end if

      call pchqk4 ( lun, kprint, ipass )

      if ( ipass .eq. 1 ) then
        write ( *, '(a)' ) '  PCHQK4 passed.'
      else
        write ( *, '(a)' ) '  PCHQK4 failed.'
      end if

      call pchqk5 ( lun, kprint, ipass )

      if ( ipass .eq. 1 ) then
        write ( *, '(a)' ) '  PCHQK5 passed.'
      else
        write ( *, '(a)' ) '  PCHQK5 failed.'
      end if
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'PCHIP_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine dpchev_test ( )

c*********************************************************************72
c
cc DPCHEV_TEST tests DPCHEV.
c
c  Modified:
c
c    04 April 2015
c
c  Reference:
c
c    Fred Fritsch, Ralph Carlson,
c    Monotone Piecewise Cubic Interpolation,
c    SIAM Journal on Numerical Analysis,
c    Volume 17, Number 2, April 1980, pages 238-246.
c
c    David Kahaner, Cleve Moler, Steven Nash,
c    Numerical Methods and Software,
c    Prentice Hall, 1989,
c    ISBN: 0-13-627258-4,
c    LC: TA345.K34.
c
      implicit none

      integer n
      parameter ( n = 21 )
      integer ne
      parameter ( ne = 101 )
      integer nwk
      parameter ( nwk = 2 * n )

      double precision d(n)
      double precision edans(4)
      double precision erans(4)
      double precision error
      double precision errord
      double precision f(n)
      double precision fans(4)
      double precision fd(ne)
      double precision fe(ne) 
      integer i
      integer ierr
      double precision r
      double precision rp
      logical spline
      double precision u
      double precision wk(nwk)
      double precision x(n)
      double precision xans(4)
      double precision xe(ne)

      save edans
      save erans
      save fans
      save xans

      data edans /
     &  0.2932883472D+00, 0.2677039506D+00, 0.1744906562D+00, 0.0D+00 /
      data erans/
     &  -0.6075110024D-02, -0.3219009901D-02,
     &  -0.946234414D-03, 0.0D+00 /
      data fans / 0.971920D+00, 0.986880D+00, 0.996560D+00, 1.0D+00 /
      data xans / -0.3D-01, -0.2D-01, -0.1D-01, 0.0D+00 /
c
c  Arithmetic statement functions for Runge's function and derivative.
c
      r ( u ) = 1.0D+00 / ( 1.0D+00 + 25.0D+00 * u * u )
      rp ( u ) = -50.0D+00 * u * r ( u ) ** 2

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'DPCHEV_TEST'
      write ( *, '(a)' ) 
     &  '  DPCHEV evaluates the interpolant.'
      write ( *, '(a)' ) ' '
c
c  Compute Runge's function at N points in [-1,1].
c
      do i = 1, n
        x(i) = -1.0D+00 + dble ( i - 1 ) / 10.0D+00
        f(i) = r ( x(i) )
      end do
c
c  Set SPLINE FALSE, to choose the cubic Hermite interpolant.
c
      spline = .false.
c
c  Compute the interpolant.
c
      call dpchez ( n, x, f, d, spline, wk, nwk, ierr )

      if ( ierr .lt. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'DPCHEV_TEST - Fatal error!'
        write ( *, '(a,i6)' ) 
     &    '  DPCHEZ returned error flag IERR= ', ierr
        stop 1
      end if
c
c  Evaluate interpolant and derivative at NE points from -1 to 0.
c
      do i = 1, ne
        xe(i) = -1.0D+00 + dble ( i - 1 ) / dble ( ne - 1 )
      end do

      call dpchev ( n, x, f, d, ne, xe, fe, fd, ierr )

      if ( ierr .ne. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'DPCHEV_TEST - Fatal error!'
        write ( *, '(a,i6)' ) 
     &    '  DPCHEV returned error flag IERR= ', ierr
        stop 1
      end if

      do i = 1, ne
        error = fe(i) - r ( xe(i) )
        errord = fd(i) - rp ( xe(i) )
        write ( *, '(1x,d17.10,3x,d17.10,3x,d17.10,3x,d17.10)' ) 
     &    xe(i), fe(i), error, errord
      end do
c
c  Print reference results.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Reference results:'

      do i = 1, 4
        write ( *, '(1x,d17.10,3x,d17.10,3x,d17.10,3x,d17.10)' ) 
     &    xans(i), fans(i), erans(i), edans(i)
      end do

      return
      end
      subroutine dpchev_test02 ( )

c*********************************************************************72
c
cc DPCHEV_TEST02 tests DPCHEV.
c
c  Discussion:
c
c    This example shows how a piecewise cubic Hermite interpolant
c    can monotonically interpolate data.
c
c  Modified:
c
c    04 April 2015
c
c  Reference:
c
c    Mathworks documentation for the MATLAB PCHIP command.
c
      implicit none

      integer nd
      parameter ( nd = 7 )
      integer ni
      parameter ( ni = 101 )
      integer nwk
      parameter ( nwk = 2 * nd )

      character * ( 255 ) command_filename
      integer command_unit
      character * ( 255 ) data_filename
      integer data_unit
      double precision dd(nd)
      double precision di(ni)
      integer i
      integer ierr
      character * ( 255 ) interp_filename
      integer interp_unit
      integer j
      character * ( 255 ) output_filename
      logical spline
      double precision u
      double precision wk(nwk)
      double precision xd(nd)
      double precision xi(ni)
      double precision yd(nd)
      double precision yi(ni)

      save xd
      save yd
      
      data xd /
     &  -3.0D+00, -2.0D+00, -1.0D+00, 0.0D+00, 1.0D+00, 
     &   2.0D+00, 3.0D+00 /
      data yd /
     &  -1.0D+00, -1.0D+00, -1.0D+00, 0.0D+00, 1.0D+00, 
     &   1.0D+00, 1.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'DPCHEV_TEST02'
      write ( *, '(a)' ) 
     &  '  DPCHEV evaluates a piecewise cubic Hermite interpolant.'
      write ( *, '(a)' ) '  Use monotonic data.'
      write ( *, '(a)' ) '  See if interpolant is monotonic.'
      write ( *, '(a)' ) ' '
c
c  Set SPLINE FALSE, to choose the cubic Hermite interpolant.
c
      spline = .false.
c
c  Compute the interpolant.
c
      call dpchez ( nd, xd, yd, dd, spline, wk, nwk, ierr )

      if ( ierr .lt. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'DPCHEV_TEST01 - Fatal error!'
        write ( *, '(a,i6)' ) 
     &    '  DPCHEZ returned error flag IERR= ', ierr
        stop 1
      end if
c
c  Evaluate interpolant and derivative at NE points from -3 to 3.
c
      do i = 1, ni
        xi(i) = ( ( ni - i     ) * ( -3.0D+00 )
     &          + (      i - 1 ) * ( +3.0D+00 ) ) 
     &          / ( ni     - 1 )
      end do

      call dpchev ( nd, xd, yd, dd, ni, xi, yi, di, ierr )

      if ( ierr .ne. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'DPCHEV_TEST02 - Fatal error!'
        write ( *, '(a,i6)' ) 
     &    '  DPCHEV returned error flag IERR= ', ierr
        stop 1
      end if
c
c  Create GNUPLOT data file.
c
      data_filename = 'test02_data.txt'
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
c  Create GNUPLOT interp file.
c
      interp_filename = 'test02_interp.txt'
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
      command_filename = 'test02_commands.txt'
      call get_unit ( command_unit )
      open ( unit = command_unit, file = command_filename, 
     &  status = 'replace' )

      output_filename = 'test02.png'

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
     &  'set title "Data and Piecewise Cubic Hermite Interpolant"'
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
      subroutine dpchev_test03 ( )

c*********************************************************************72
c
cc DPCHEV_TEST03 tests DPCHEV.
c
c  Discussion:
c
c    This example shows how a spline interpolant
c    can interpolate data.
c
c  Modified:
c
c    05 April 2015
c
c  Reference:
c
c    Mathworks documentation for the MATLAB PCHIP command.
c
      implicit none

      integer nd
      parameter ( nd = 7 )
      integer ni
      parameter ( ni = 101 )
      integer nwk
      parameter ( nwk = 2 * nd )

      character * ( 255 ) command_filename
      integer command_unit
      character * ( 255 ) data_filename
      integer data_unit
      double precision dd(nd)
      double precision di(ni)
      integer i
      integer ierr
      character * ( 255 ) interp_filename
      integer interp_unit
      integer j
      character * ( 255 ) output_filename
      logical spline
      double precision u
      double precision wk(nwk)
      double precision xd(nd)
      double precision xi(ni)
      double precision yd(nd)
      double precision yi(ni)

      save xd
      save yd
      
      data xd /
     &  -3.0D+00, -2.0D+00, -1.0D+00, 0.0D+00, 1.0D+00, 
     &   2.0D+00, 3.0D+00 /
      data yd /
     &  -1.0D+00, -1.0D+00, -1.0D+00, 0.0D+00, 1.0D+00, 
     &   1.0D+00, 1.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'DPCHEV_TEST03'
      write ( *, '(a)' ) 
     &  '  DPCHEV evaluates a cubic spline interpolant.'
      write ( *, '(a)' ) '  Use monotonic data.'
      write ( *, '(a)' ) '  See if interpolant is monotonic.'
      write ( *, '(a)' ) ' '
c
c  Set SPLINE TRUE, to choose the cubic spline interpolant.
c
      spline = .true.
c
c  Compute the interpolant.
c
      call dpchez ( nd, xd, yd, dd, spline, wk, nwk, ierr )

      if ( ierr .lt. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'DPCHEV_TEST01 - Fatal error!'
        write ( *, '(a,i6)' ) 
     &    '  DPCHEZ returned error flag IERR= ', ierr
        stop 1
      end if
c
c  Evaluate interpolant and derivative at NE points from -3 to 3.
c
      do i = 1, ni
        xi(i) = ( ( ni - i     ) * ( -3.0D+00 )
     &          + (      i - 1 ) * ( +3.0D+00 ) ) 
     &          / ( ni     - 1 )
      end do

      call dpchev ( nd, xd, yd, dd, ni, xi, yi, di, ierr )

      if ( ierr .ne. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'DPCHEV_TEST03 - Fatal error!'
        write ( *, '(a,i6)' ) 
     &    '  DPCHEV returned error flag IERR= ', ierr
        stop 1
      end if
c
c  Create GNUPLOT data file.
c
      data_filename = 'test03_data.txt'
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
c  Create GNUPLOT interp file.
c
      interp_filename = 'test03_interp.txt'
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
      command_filename = 'test03_commands.txt'
      call get_unit ( command_unit )
      open ( unit = command_unit, file = command_filename, 
     &  status = 'replace' )

      output_filename = 'test03.png'

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
     &  'set title "Data and Cubic Spline interpolant"'
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
      subroutine dpchqa_test ( )

c*********************************************************************72
c
cc DPCHQA_TEST tests DPCHQA.
c
c  Modified:
c
c    04 April 2015
c
c  Reference:
c
c    Fred Fritsch, Ralph Carlson,
c    Monotone Piecewise Cubic Interpolation,
c    SIAM Journal on Numerical Analysis,
c    Volume 17, Number 2, April 1980, pages 238-246.
c
c    David Kahaner, Cleve Moler, Steven Nash,
c    Numerical Methods and Software,
c    Prentice Hall, 1989,
c    ISBN: 0-13-627258-4,
c    LC: TA345.K34.
c
      implicit none

      integer n
      parameter ( n = 21 )
      integer nwk
      parameter ( nwk = 2 * n )

      double precision a
      double precision b
      double precision d(n)
      double precision dpchqa
      double precision error
      double precision errord
      double precision f(n)
      integer i
      integer ians
      integer ierr
      double precision q
      double precision qans
      double precision r
      double precision rp
      logical spline
      double precision u
      double precision wk(nwk)
      double precision x(n)

      save ians
      save qans

      data ians / 0 /
      data qans / 0.274679262701527D+00 /
c
c  Arithmetic statement functions for Runge's function and derivative.
c
      r ( u ) = 1.0D+00 / ( 1.0D+00 + 25.0D+00 * u * u )
      rp ( u ) = -50.0D+00 * u * r ( u ) ** 2

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'DPCHQA_TEST'
      write ( *, '(a)' ) 
     &  '  DPCHQA integrates the interpolant.'
      write ( *, '(a)' ) ' '
c
c  Compute Runge's function at N points in [-1,1].
c
      do i = 1, n
        x(i) = -1.0D+00 + dble ( i - 1 ) / 10.0D+00
        f(i) = r ( x(i) )
      end do
c
c  Set SPLINE FALSE, to choose the cubic Hermite interpolant.
c
      spline = .false.
c
c  Compute the interpolant.
c
      call dpchez ( n, x, f, d, spline, wk, nwk, ierr )

      if ( ierr .lt. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'DPCHEZ_TEST - Fatal error!'
        write ( *, '(a,i6)' ) 
     &    '  DPCHEZ returned error flag IERR= ', ierr
        stop 1
      end if
c
c  Compute integral of the interpolant over the interval [0,1] 
c
      a = 0.0D+00
      b = 1.0D+00
      q = dpchqa ( n, x, f, d, a, b, ierr )

      write ( *, '(2x,a,d20.12,3x,a,i5)' ) 
     &  'Integral from 0 to 1 = ', q, ' IERR = ', ierr
c
c  Print reference results.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Reference results:'
      write ( *, '(a)' ) ''

      write ( *, '(2X,a,d20.12,3x,a,i5)' ) 
     &  'Integral from 0 to 1 = ', qans, ' IERR = ', ians           

      return
      end
