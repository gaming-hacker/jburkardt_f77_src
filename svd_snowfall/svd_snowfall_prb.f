      program main

c*********************************************************************72
c
cc SVD_SNOWFALL_TEST tests the SVD_SNOWFALL library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 May 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 8 )
      integer n
      parameter ( n = 123 )

      character * ( 255 ) filename
      double precision x(m,n)

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'SVD_SNOWFALL_TEST'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the SVD_SNOWFALL library.'
c
c  Retrieve the data.
c  It's really easier to do this in the main program.
c
      filename = 'snowfall.txt'

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'SVD_SNOWFALL_TEST01'
      write ( *, '(a)' ) 
     &  '  Read, process, and return snowfall data in "' // 
     &  trim ( filename ) // '".'
c
c  Determine the size of the data.
c
      write ( *, '(a)' ) ''
      write ( *, '(a,i4)' ) '  Number of data rows    M = ', m
      write ( *, '(a,i4)' ) '  Number of data columns N = ', n

      call r8mat_data_read ( filename, m, n, x )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Data has been read from the file.'

      call svd_snowfall_test02 ( m, n, x )
      call svd_snowfall_test03 ( m, n, x )
      call svd_snowfall_test04 ( m, n, x )
      call svd_snowfall_test05 ( m, n, x )
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'SVD_SNOWFALL_TEST'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      stop
      end
      subroutine svd_snowfall_test02 ( m, n, x )

c*********************************************************************72
c
cc SVD_SNOWFALL_TEST02 looks at the singular values.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 May 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns.
c
c    Input, double precision X(M,N), the snowfall data.
c
      implicit none

      integer m
      integer n

      character * ( 255 ) command_filename
      integer command_unit
      character * ( 255 ) data_filename
      integer data_unit
      double precision e(m)
      double precision e_cum(m+1)
      double precision e_sum
      integer i
      integer mn
      double precision r8vec_sum
      double precision s(m,n)
      double precision s_diag(m)
      double precision u(m,m)
      double precision v(n,n)
      double precision x(m,n)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'SVD_SNOWFALL_TEST02'
      write ( *, '(a)' ) '  Look at the singular values.'
      write ( *, '(a)' ) 
     &  '  If the singular values are close, then the data is'
      write ( *, '(a)' ) 
     &  '  well spread out.  If the singular values decay rapidly,'
      write ( *, '(a)' ) 
     &  '  then the data exhibits patterns, or is constrained to'
      write ( *, '(a)' ) '  a lower-dimensional subspace.'
c
c  Compute the SVD.
c
      call r8mat_svd_linpack ( m, n, x, u, s, v )
c
c  Extract the diagonal of S.
c
      mn = min ( m, n )
      do i = 1, mn
        s_diag(i) = s(i,i)
      end do
c
c  Print the singular values.
c
      call r8vec_print ( mn, s_diag, '  The singular values:' )
c
c  Plot the singular values.
c
      call get_unit ( data_unit )
      data_filename = 'singular_values_data.txt'
      open ( unit = data_unit, file = data_filename, 
     &  status = 'replace' )
      do i = 1, mn
        write ( data_unit, '(2x,i4,2x,g14.6)' ) i, s_diag(i)
      end do
      close ( unit = data_unit )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Created data file "' // trim ( data_filename ) // '".'

      call get_unit ( command_unit )
      command_filename = 'singular_values_commands.txt'
      open ( unit = command_unit, file = command_filename, 
     &  status = 'replace' )
      write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) '# Usage:'
      write ( command_unit, '(a)' ) 
     &  '#  gnuplot < ' // trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) 'set term png'
      write ( command_unit, '(a)' ) 'set output "singular_values.png"'
      write ( command_unit, '(a)' ) 'set xlabel "Index I"'
      write ( command_unit, '(a)' ) 'set ylabel "S(I)"'
      write ( command_unit, '(a)' ) 
     &  'set title "Snowfall Singular Values"'
      write ( command_unit, '(a)' ) 'set grid'
      write ( command_unit, '(a)' ) 'set style data lines'
      write ( command_unit, '(a)' ) 
     &  'plot "' // trim ( data_filename ) // 
     &  '" using 1:2 lw 3 linecolor rgb "blue"'
      write ( command_unit, '(a)' ) 'quit'
      close ( unit = command_unit )
      write ( *, '(a)' ) 
     &  '  Created command file "' // trim ( command_filename ) // '".'
c
c  Print the cumulative "energy" of the singular values.
c
      do i = 1, mn
        e(i) = s_diag(i) ** 2
      end do
      e_sum = r8vec_sum ( mn, e )
      do i = 1, mn
        e(i) = e(i) / e_sum
      end do
      call r8vec_cum0 ( mn, e, e_cum )

      call r8vec_print ( mn + 1, e_cum, '  The cumulative energy:' )

      return
      end
      subroutine svd_snowfall_test03 ( m, n, x )

c*********************************************************************72
c
cc SVD_SNOWFALL_TEST03 computes low rank approximations to the matrix.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 May 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns.
c
c    Input, double precision X(M,N), the snowfall data.
c
      implicit none

      integer m
      integer n

      double precision a1(m,n)
      double precision a2(m,n)
      double precision a3(m,n)
      double precision a4(m,n)
      double precision a5(m,n)
      character * ( 255 ) command_filename
      integer command_unit
      character * ( 255 ) data_filename
      integer data_unit
      integer i
      double precision s(m,n)
      double precision sv1(1,n)
      double precision sv2(2,n)
      double precision sv3(3,n)
      double precision sv4(4,n)
      double precision sv5(5,n)
      double precision u(m,m)
      double precision v(n,n)
      double precision x(m,n)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'SVD_SNOWFALL_TEST03';
      write ( *, '(a)' ) 
     &  '  Compute rank 1 through rank 5 approximations to the data.'
      write ( *, '(a)' ) 
     &  '  Compare each of these to the 2012 snowfall data.'
c
c  Compute the SVD.
c
      call r8mat_svd_linpack ( m, n, x, u, s, v )
c
c  Form the rank 1, 2, 3, 4, 5 approximants to A.
c
      call r8mat_svd_low_rank ( m, n, 1, u, s, v, a1 )
      call r8mat_svd_low_rank ( m, n, 2, u, s, v, a2 )
      call r8mat_svd_low_rank ( m, n, 3, u, s, v, a3 )
      call r8mat_svd_low_rank ( m, n, 4, u, s, v, a4 )
      call r8mat_svd_low_rank ( m, n, 5, u, s, v, a5 )
c
c  Column 1 of X is the 2012 snowfall.
c  Column 1 of A1 is the rank 1 approximant to 2012 snowfall.
c
      call get_unit ( data_unit )
      data_filename = 'approx_data.txt'
      open ( unit = data_unit, file = data_filename, 
     &  status = 'replace' )
      do i = 1, m
        write ( data_unit, '(2x,i4,6(2x,g14.6))' ) 
     &    i, x(i,1), a1(i,1), a2(i,1), a3(i,1), a4(i,1), a5(i,1)
      end do
      close ( unit = data_unit )
      write ( *, '(a)' ) '  Created data file "' 
     &  // trim ( data_filename ) // '".'

      call get_unit ( command_unit )
      command_filename = 'approx_commands.txt'
      open ( unit = command_unit, file = command_filename, 
     &  status = 'replace' )
      write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) '# Usage:'
      write ( command_unit, '(a)' ) 
     &  '#  gnuplot < ' // trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) 'set term png'
      write ( command_unit, '(a)' ) 'set output "approx0.png"'
      write ( command_unit, '(a)' ) 'set xlabel "Month"'
      write ( command_unit, '(a)' ) 'set ylabel "Snowfall"'
      write ( command_unit, '(a)' ) 'set title "2012 Snowfall"'
      write ( command_unit, '(a)' ) 'set grid'
      write ( command_unit, '(a)' ) 'set style data lines'
      write ( command_unit, '(a)' ) 
     &  'plot "' // trim ( data_filename ) // 
     &  '" using 1:2 lw 3 linecolor rgb "blue"'

      write ( command_unit, '(a)' ) 'set output "approx1.png"'
      write ( command_unit, '(a)' ) 'set xlabel "Month"'
      write ( command_unit, '(a)' ) 'set ylabel "Snowfall"'
      write ( command_unit, '(a)' ) '
     &  set title "Rank 1 Approx to 2012 Snowfall"'
      write ( command_unit, '(a)' ) 'set grid'
      write ( command_unit, '(a)' ) 'set style data lines'
      write ( command_unit, '(a)' ) 
     &  'plot "' // trim ( data_filename ) // 
     &  '" using 1:2 lw 3 linecolor rgb "blue",\'
      write ( command_unit, '(a)' ) 
     &  '     "' // trim ( data_filename ) // 
     &  '" using 1:3 lw 3 linecolor rgb "red"'

      write ( command_unit, '(a)' ) 'set output "approx2.png"'
      write ( command_unit, '(a)' ) 'set xlabel "Month"'
      write ( command_unit, '(a)' ) 'set ylabel "Snowfall"'
      write ( command_unit, '(a)' ) 
     &  'set title "Rank 2 Approx to 2012 Snowfall"'
      write ( command_unit, '(a)' ) 'set grid'
      write ( command_unit, '(a)' ) 'set style data lines'
      write ( command_unit, '(a)' ) 
     &  'plot "' // trim ( data_filename ) // 
     &  '" using 1:2 lw 3 linecolor rgb "blue",\'
      write ( command_unit, '(a)' ) 
     &  '     "' // trim ( data_filename ) // 
     &  '" using 1:3 lw 3 linecolor rgb "gray",\'
      write ( command_unit, '(a)' ) 
     &  '     "' // trim ( data_filename ) // 
     &  '" using 1:4 lw 3 linecolor rgb "red"'

      write ( command_unit, '(a)' ) 'set output "approx3.png"'
      write ( command_unit, '(a)' ) 'set xlabel "Month"'
      write ( command_unit, '(a)' ) 'set ylabel "Snowfall"'
      write ( command_unit, '(a)' ) 
     &  'set title "Rank 3 Approx to 2012 Snowfall"'
      write ( command_unit, '(a)' ) 'set grid'
      write ( command_unit, '(a)' ) 'set style data lines'
      write ( command_unit, '(a)' ) 
     &  'plot "' // trim ( data_filename ) // 
     &  '" using 1:2 lw 3 linecolor rgb "blue",\'
      write ( command_unit, '(a)' ) 
     &  '     "' // trim ( data_filename ) // 
     &  '" using 1:3 lw 3 linecolor rgb "gray",\'
      write ( command_unit, '(a)' ) 
     &  '     "' // trim ( data_filename ) // 
     &  '" using 1:4 lw 3 linecolor rgb "gray",\'
      write ( command_unit, '(a)' ) 
     &  '     "' // trim ( data_filename ) // 
     &  '" using 1:5 lw 3 linecolor rgb "red"'

      write ( command_unit, '(a)' ) 'set output "approx4.png"'
      write ( command_unit, '(a)' ) 'set xlabel "Month"'
      write ( command_unit, '(a)' ) 'set ylabel "Snowfall"'
      write ( command_unit, '(a)' ) 
     &  'set title "Rank 4 Approx to 2012 Snowfall"'
      write ( command_unit, '(a)' ) 'set grid'
      write ( command_unit, '(a)' ) 'set style data lines'
      write ( command_unit, '(a)' ) 
     &  'plot "' // trim ( data_filename ) // 
     &  '" using 1:2 lw 3 linecolor rgb "blue",\'
      write ( command_unit, '(a)' ) 
     &  '     "' // trim ( data_filename ) // 
     &  '" using 1:3 lw 3 linecolor rgb "gray",\'
      write ( command_unit, '(a)' ) 
     &  '     "' // trim ( data_filename ) // 
     &  '" using 1:4 lw 3 linecolor rgb "gray",\'
      write ( command_unit, '(a)' ) 
     &  '     "' // trim ( data_filename ) // 
     &  '" using 1:5 lw 3 linecolor rgb "gray",\'
      write ( command_unit, '(a)' ) 
     &  '     "' // trim ( data_filename ) // 
     &  '" using 1:6 lw 3 linecolor rgb "red"'

      write ( command_unit, '(a)' ) 'set output "approx5.png"'
      write ( command_unit, '(a)' ) 'set xlabel "Month"'
      write ( command_unit, '(a)' ) 'set ylabel "Snowfall"'
      write ( command_unit, '(a)' ) 
     &  'set title "Rank 5 Approx to 2012 Snowfall"'
      write ( command_unit, '(a)' ) 'set grid'
      write ( command_unit, '(a)' ) 'set style data lines'
      write ( command_unit, '(a)' ) 
     &  'plot "' // trim ( data_filename ) // 
     &  '" using 1:2 lw 3 linecolor rgb "blue",\'
      write ( command_unit, '(a)' ) 
     &  '     "' // trim ( data_filename ) // 
     &  '" using 1:3 lw 3 linecolor rgb "gray",\'
      write ( command_unit, '(a)' ) 
     &  '     "' // trim ( data_filename ) // 
     &  '" using 1:4 lw 3 linecolor rgb "gray",\'
      write ( command_unit, '(a)' ) 
     &  '     "' // trim ( data_filename ) // 
     &  '" using 1:5 lw 3 linecolor rgb "gray",\'
      write ( command_unit, '(a)' ) 
     &  '     "' // trim ( data_filename ) // 
     &  '" using 1:6 lw 3 linecolor rgb "gray",\'
      write ( command_unit, '(a)' ) 
     &  '     "' // trim ( data_filename ) // 
     &  '" using 1:7 lw 3 linecolor rgb "red"'

      write ( command_unit, '(a)' ) 'quit'
      close ( unit = command_unit )
      write ( *, '(a)' ) 
     &  '  Created command file "' // trim ( command_filename ) // '".'

      return
      end
      subroutine svd_snowfall_test04 ( m, n, x )

c*********************************************************************72
c
cc SVD_SNOWFALL_TEST04 looks at the first 6 modes in the U matrix.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 May 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns.
c
c    Input, double precision X(M,N), the snowfall data.
c
      implicit none

      integer m
      integer n

      character * ( 255 ) command_filename
      integer command_unit
      character * ( 255 ) data_filename
      integer data_unit
      integer i
      double precision s(m,n)
      double precision u(m,m)
      double precision v(n,n)
      double precision x(m,n)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'SVD_SNOWFALL_TEST04'
      write ( *, '(a)' ) 
     &  '  Look at the first 6 modes in the U matrix.'
      write ( *, '(a)' ) 
     &  '  Each of these represents a pattern for snowfall over a year.'
      write ( *, '(a)' ) 
     &  '  The first mode is the pattern that is strongest in the data.'
c
c  Compute the SVD.
c
      call r8mat_svd_linpack ( m, n, x, u, s, v )
c
c  Normalize the patterns so that each column has maximum entry 1.
c
      call r8col_normalize_li ( m, m, u )
c
c  Plot the U modes.
c
      call get_unit ( data_unit )
      data_filename = 'umode_data.txt'
      open ( unit = data_unit, file = data_filename, 
     &  status = 'replace' )
      do i = 1, m
        write ( data_unit, '(2x,i4,6(2x,g14.6))' ) i, u(i,1:6)
      end do
      close ( unit = data_unit )
      write ( *, '(a)' ) 
     &  '  Created data file "' // trim ( data_filename ) // '".'

      call get_unit ( command_unit )
      command_filename = 'umode_commands.txt'
      open ( unit = command_unit, file = command_filename, 
     &  status = 'replace' )
      write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) '# Usage:'
      write ( command_unit, '(a)' ) 
     &  '#  gnuplot < ' // trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) 'set term png'
      write ( command_unit, '(a)' ) 'set output "umode1.png"'
      write ( command_unit, '(a)' ) 'set xlabel "Month"'
      write ( command_unit, '(a)' ) 'set ylabel "Snowfall"'
      write ( command_unit, '(a)' ) 
     &  'set title "Monthly Snowfall Mode 1"'
      write ( command_unit, '(a)' ) 'set grid'
      write ( command_unit, '(a)' ) 'set style data lines'
      write ( command_unit, '(a)' ) 
     &  'plot "' // trim ( data_filename ) // 
     &  '" using 1:2 lw 3 linecolor rgb "blue"'

      write ( command_unit, '(a)' ) 'set output "umode2.png"'
      write ( command_unit, '(a)' ) 'set xlabel "Month"'
      write ( command_unit, '(a)' ) 'set ylabel "Snowfall"'
      write ( command_unit, '(a)' ) 
     &  'set title "Monthly Snowfall Mode 2"'
      write ( command_unit, '(a)' ) 'set grid'
      write ( command_unit, '(a)' ) 'set style data lines'
      write ( command_unit, '(a)' ) 
     &  'plot "' // trim ( data_filename ) // 
     &  '" using 1:3 lw 3 linecolor rgb "blue"'

      write ( command_unit, '(a)' ) 'set output "umode3.png"'
      write ( command_unit, '(a)' ) 'set xlabel "Month"'
      write ( command_unit, '(a)' ) 'set ylabel "Snowfall"'
      write ( command_unit, '(a)' ) 
     &  'set title "Monthly Snowfall Mode 3"'
      write ( command_unit, '(a)' ) 'set grid'
      write ( command_unit, '(a)' ) 'set style data lines'
      write ( command_unit, '(a)' ) 
     &  'plot "' // trim ( data_filename ) // 
     &  '" using 1:4 lw 3 linecolor rgb "blue"'

      write ( command_unit, '(a)' ) 'set output "umode4.png"'
      write ( command_unit, '(a)' ) 'set xlabel "Month"'
      write ( command_unit, '(a)' ) 'set ylabel "Snowfall"'
      write ( command_unit, '(a)' ) 
     &  'set title "Monthly Snowfall Mode 4"'
      write ( command_unit, '(a)' ) 'set grid'
      write ( command_unit, '(a)' ) 'set style data lines'
      write ( command_unit, '(a)' ) 
     &  'plot "' // trim ( data_filename ) // 
     &  '" using 1:5 lw 3 linecolor rgb "blue"'

      write ( command_unit, '(a)' ) 'set output "umode5.png"'
      write ( command_unit, '(a)' ) 'set xlabel "Month"'
      write ( command_unit, '(a)' ) 'set ylabel "Snowfall"'
      write ( command_unit, '(a)' ) 
     &  'set title "Monthly Snowfall Mode 5"'
      write ( command_unit, '(a)' ) 'set grid'
      write ( command_unit, '(a)' ) 'set style data lines'
      write ( command_unit, '(a)' ) 
     &  'plot "' // trim ( data_filename ) // 
     &  '" using 1:6 lw 3 linecolor rgb "blue"'

      write ( command_unit, '(a)' ) 'set output "umode6.png"'
      write ( command_unit, '(a)' ) 'set xlabel "Month"'
      write ( command_unit, '(a)' ) 'set ylabel "Snowfall"'
      write ( command_unit, '(a)' ) 
     &  'set title "Monthly Snowfall Mode 6"'
      write ( command_unit, '(a)' ) 'set grid'
      write ( command_unit, '(a)' ) 'set style data lines'
      write ( command_unit, '(a)' ) 
     &  'plot "' // trim ( data_filename ) // 
     &  '" using 1:7 lw 3 linecolor rgb "blue"'

      write ( command_unit, '(a)' ) 'quit'
      close ( unit = command_unit )
      write ( *, '(a)' ) 
     &  '  Created command file "' // trim ( command_filename ) // '".'

      return
      end
      subroutine svd_snowfall_test05 ( m, n, x )

c*********************************************************************72
c
cc SVD_SNOWFALL_TEST05 looks at the first 6 modes in the V matrix.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 May 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns.
c
c    Input, double precision X(M,N), the snowfall data.
c
      implicit none

      integer m
      integer n

      character * ( 255 ) command_filename
      integer command_unit
      character * ( 255 ) data_filename
      integer data_unit
      integer i
      double precision s(m,n)
      double precision u(m,m)
      double precision v(n,n)
      double precision x(m,n)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'SVD_SNOWFALL_TEST05'
      write ( *, '(a)' ) 
     &  '  Look at the first 6 modes in the V matrix.'
      write ( *, '(a)' ) 
     &  '  Each of these represents a pattern shared by all the months,'
      write ( *, '(a)' ) 
     &  '  and extending across the 123 sampling years.'
c
c  Compute the SVD.
c
      call r8mat_svd_linpack ( m, n, x, u, s, v )
c
c  Normalize the patterns so that each column has maximum entry 1.
c
      call r8col_normalize_li ( n, n, v )
c
c  Reverse the row ordering.
c
      call r8row_reverse ( n, n, v )
c
c  Plot the V modes.
c
      call get_unit ( data_unit )
      data_filename = 'vmode_data.txt'
      open ( unit = data_unit, file = data_filename, 
     &  status = 'replace' )
      do i = 1, n
        write ( data_unit, '(2x,i4,6(2x,g14.6))' ) i, v(i,1:6)
      end do
      close ( unit = data_unit )
      write ( *, '(a)' ) 
     &  '  Created data file "' // trim ( data_filename ) // '".'

      call get_unit ( command_unit )
      command_filename = 'vmode_commands.txt'
      open ( unit = command_unit, file = command_filename, 
     &  status = 'replace' )
      write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) '# Usage:'
      write ( command_unit, '(a)' ) 
     &  '#  gnuplot < ' // trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) 'set term png'
      write ( command_unit, '(a)' ) 'set output "vmode1.png"'
      write ( command_unit, '(a)' ) 'set xlabel "Year"'
      write ( command_unit, '(a)' ) 'set ylabel "Snowfall"'
      write ( command_unit, '(a)' ) 
     &  'set title "Yearly Snowfall Mode 1"'
      write ( command_unit, '(a)' ) 'set grid'
      write ( command_unit, '(a)' ) 'set style data points'
      write ( command_unit, '(a)' ) 
     &  'plot "' // trim ( data_filename ) // 
     &  '" using 1:2 with points lt 3 pt 3'

      write ( command_unit, '(a)' ) 'set output "vmode2.png"'
      write ( command_unit, '(a)' ) 'set xlabel "Year"'
      write ( command_unit, '(a)' ) 'set ylabel "Snowfall"'
      write ( command_unit, '(a)' ) 
     &  'set title "Yearly Snowfall Mode 2"'
      write ( command_unit, '(a)' ) 'set grid'
      write ( command_unit, '(a)' ) 'set style data points'
      write ( command_unit, '(a)' ) 
     &  'plot "' // trim ( data_filename ) // 
     &  '" using 1:3 with points lt 3 pt 3'

      write ( command_unit, '(a)' ) 'set output "vmode3.png"'
      write ( command_unit, '(a)' ) 'set xlabel "Year"'
      write ( command_unit, '(a)' ) 'set ylabel "Snowfall"'
      write ( command_unit, '(a)' ) 
     &  'set title "Yearly Snowfall Mode 3"'
      write ( command_unit, '(a)' ) 'set grid'
      write ( command_unit, '(a)' ) 'set style data points'
      write ( command_unit, '(a)' ) 
     &  'plot "' // trim ( data_filename ) // 
     &  '" using 1:4 with points lt 3 pt 3'

      write ( command_unit, '(a)' ) 'set output "vmode4.png"'
      write ( command_unit, '(a)' ) 'set xlabel "Year"'
      write ( command_unit, '(a)' ) 'set ylabel "Snowfall"'
      write ( command_unit, '(a)' ) 
     &  'set title "Yearly Snowfall Mode 4"'
      write ( command_unit, '(a)' ) 'set grid'
      write ( command_unit, '(a)' ) 'set style data points'
      write ( command_unit, '(a)' ) 
     &  'plot "' // trim ( data_filename ) // 
     &  '" using 1:5 with points lt 3 pt 3'

      write ( command_unit, '(a)' ) 'set output "vmode5.png"'
      write ( command_unit, '(a)' ) 'set xlabel "Year"'
      write ( command_unit, '(a)' ) 'set ylabel "Snowfall"'
      write ( command_unit, '(a)' ) 
     &  'set title "Yearly Snowfall Mode 5"'
      write ( command_unit, '(a)' ) 'set grid'
      write ( command_unit, '(a)' ) 'set style data points'
      write ( command_unit, '(a)' ) 
     &  'plot "' // trim ( data_filename ) // 
     &  '" using 1:6 with points lt 3 pt 3'

      write ( command_unit, '(a)' ) 'set output "vmode6.png"'
      write ( command_unit, '(a)' ) 'set xlabel "Year"'
      write ( command_unit, '(a)' ) 'set ylabel "Snowfall"'
      write ( command_unit, '(a)' ) 
     &  'set title "Yearly Snowfall Mode 6"'
      write ( command_unit, '(a)' ) 'set grid'
      write ( command_unit, '(a)' ) 'set style data points'
      write ( command_unit, '(a)' ) 
     &  'plot "' // trim ( data_filename ) // 
     &  '" using 1:7 with points lt 3 pt 3'

      write ( command_unit, '(a)' ) 'quit'
      close ( unit = command_unit )
      write ( *, '(a)' ) 
     &  '  Created command file "' // trim ( command_filename ) // '".'

      return
      end
