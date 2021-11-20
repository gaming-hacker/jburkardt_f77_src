      subroutine get_unit ( iunit )

c*********************************************************************72
c
cc GET_UNIT returns a free FORTRAN unit number.
c
c  Discussion:
c
c    A "free" FORTRAN unit number is a value between 1 and 99 which
c    is not currently associated with an I/O device.  A free FORTRAN unit
c    number is needed in order to open a file with the OPEN command.
c
c    If IUNIT = 0, then no free FORTRAN unit could be found, although
c    all 99 units were checked (except for units 5, 6 and 9, which
c    are commonly reserved for console I/O).
c
c    Otherwise, IUNIT is a value between 1 and 99, representing a
c    free FORTRAN unit.  Note that GET_UNIT assumes that units 5 and 6
c    are special, and will never return those values.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 September 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, integer IUNIT, the free unit number.
c
      implicit none

      integer i
      integer iunit
      logical value

      iunit = 0

      do i = 1, 99

        if ( i .ne. 5 .and. i .ne. 6 .and. i .ne. 9 ) then

          inquire ( unit = i, opened = value, err = 10 )

          if ( .not. value ) then
            iunit = i
            return
          end if

        end if

10      continue

      end do

      return
      end
      subroutine lobatto_polynomial_derivative ( m, n, x, lp )

c*********************************************************************72
c
cc LOBATTO_POLYNOMIAL_DERIVATIVE: derivative of completed Lobatto polynomial.
c
c  Discussion:
c
c    L(N,X)  =  N * ( P(N-1,X) - X * P(N,X) ) 
c    L'(N,X) =  N * ( P'(N-1,X) - P(N,X) - X * P'(N,X) )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    18 November 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Milton Abramowitz, Irene Stegun,
c    Handbook of Mathematical Functions,
c    National Bureau of Standards, 1964,
c    ISBN: 0-486-61272-4,
c    LC: QA47.A34.
c
c    Larry Andrews,
c    Special Functions of Mathematics for Engineers,
c    Second Edition,
c    Oxford University Press, 1998,
c    ISBN: 0819426164,
c    LC: QA351.A75.
c
c    Daniel Zwillinger, editor,
c    CRC Standard Mathematical Tables and Formulae,
c    30th Edition,
c    CRC Press, 1996.
c
c  Parameters:
c
c    Input, integer M, the number of evaluation points.
c
c    Input, integer N, the highest order polynomial to evaluate.
c    Note that polynomials 0 through N will be evaluated.
c
c    Input, double precision X(M), the evaluation points.
c
c    Output, double precision LP(M,N), the derivative of the completed Lobatto
c    polynomials of order 1 through N at the point X.
c
      implicit none

      integer m
      integer n

      integer i
      integer j
      double precision lp(m,n)
      double precision p(m,n+1)
      double precision pp(m,n+1)
      double precision x(m)
    
      do i = 1, m

        if ( 1 .le. n ) then

          lp(i,1) = - 2.0D+00 * x(i)

          if ( 2 .le. n ) then

            p(i,1) = 1.0D+00
            p(i,2) = x(i)
            do j = 2, n
              p(i,j+1) = 
     &          ( dble ( 2 * j - 1 ) * x(i) * p(i,j)     
     &          - dble (     j - 1 ) *        p(i,j-1) ) 
     &          / dble (     j     )
            end do

            pp(i,1) = 0.0D+00
            pp(i,2) = 1.0D+00
            do j = 2, n
              pp(i,j+1) = 
     &          ( dble ( 2 * j - 1 ) * ( p(i,j) + x(i) * pp(i,j) )   
     &          - dble (     j - 1 ) *                   pp(i,j-1) ) 
     &          / dble (     j     )
            end do

            do j = 2, n
              lp(i,j) = 
     &          dble ( j ) * ( pp(i,j) - p(i,j+1) - x(i) * pp(i,j+1) )
            end do
 
          end if

        end if

      end do

      return
      end
      subroutine lobatto_polynomial_derivatives ( n_data, n, x, fx )

c********************************************************************72
c
cc LOBATTO_POLYNOMIAL_DERIVATIVES: derivatives of completed Lobatto polynomials.
c
c  Discussion:
c
c    In Mathematica, the function can be evaluated by:
c
c      n * LegendreP [ n - 1, x ] - n * x * LegendreP [ n, x ]
c
c    In Mathematica, the completed Lobatto polynomial can be evaluated by:
c
c       n * LegendreP [ n - 1, x ] - n * x * LegendreP [ n, x ]
c
c    The derivative is:
c
c         n * D[LegendreP [ n - 1, x ], {x} ] 
c       - n * LegendreP [ n, x ] 
c       - n * x * D[LegendreP [ n, x ], {x}]
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 November 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input/output, integer N_DATA.  The user sets N_DATA to 0
c    before the first call.  On each call, the routine increments N_DATA by 1,
c    and returns the corresponding data; when there is no more data, the
c    output value of N_DATA will be 0 again.
c
c    Output, integer N, the order of the function.
c
c    Output, double precision X, the point where the function is evaluated.
c
c    Output, double precision FX, the value of the function.
c
      implicit none

      integer n_max
      parameter ( n_max = 31 )

      double precision fx
      double precision fx_vec(n_max)
      integer n
      integer n_data
      integer n_vec(n_max)
      double precision x
      double precision x_vec(n_max)

      save fx_vec
      save n_vec
      save x_vec

      data fx_vec /
     &   -0.5D+00, 
     &    2.437500000000000D+00, 
     &    4.031250000000000D+00, 
     &   -3.154296875000000D+00, 
     &  -10.19165039062500D+00, 
     &   -1.019622802734375D+00, 
     &   15.67544555664063D+00, 
     &   10.97668933868408D+00, 
     &  -15.91419786214828D+00, 
     &  -24.33202382177114D+00, 
     &   12.00000000000000D+00, 
     &    5.670000000000000D+00, 
     &    0.9600000000000000D+00, 
     &   -2.310000000000000D+00, 
     &   -4.320000000000000D+00, 
     &   -5.250000000000000D+00, 
     &   -5.280000000000000D+00, 
     &   -4.590000000000000D+00, 
     &   -3.360000000000000D+00, 
     &   -1.770000000000000D+00, 
     &    0.0D+00, 
     &    1.770000000000000D+00, 
     &    3.360000000000000D+00, 
     &    4.590000000000000D+00, 
     &    5.280000000000000D+00, 
     &    5.250000000000000D+00, 
     &    4.320000000000000D+00, 
     &    2.310000000000000D+00, 
     &   -0.9600000000000000D+00, 
     &   -5.670000000000000D+00, 
     &  -12.00000000000000D+00 /

      data n_vec /
     &   1,  2, 
     &   3,  4,  5, 
     &   6,  7,  8, 
     &   9, 10,  3, 
     &   3,  3,  3, 
     &   3,  3,  3, 
     &   3,  3,  3, 
     &   3,  3,  3, 
     &   3,  3,  3, 
     &   3,  3,  3, 
     &   3,  3 /

      data x_vec /
     &  0.25D+00, 
     &  0.25D+00, 
     &  0.25D+00, 
     &  0.25D+00, 
     &  0.25D+00, 
     &  0.25D+00, 
     &  0.25D+00, 
     &  0.25D+00, 
     &  0.25D+00, 
     &  0.25D+00, 
     & -1.00D+00, 
     & -0.90D+00, 
     & -0.80D+00, 
     & -0.70D+00, 
     & -0.60D+00, 
     & -0.50D+00, 
     & -0.40D+00, 
     & -0.30D+00, 
     & -0.20D+00, 
     & -0.10D+00, 
     &  0.00D+00, 
     &  0.10D+00, 
     &  0.20D+00, 
     &  0.30D+00, 
     &  0.40D+00, 
     &  0.50D+00, 
     &  0.60D+00, 
     &  0.70D+00, 
     &  0.80D+00, 
     &  0.90D+00, 
     &  1.00D+00 /

      if ( n_data .lt. 0 ) then
        n_data = 0
      end if

      n_data = n_data + 1

      if ( n_max .lt. n_data ) then
        n_data = 0
        n = 0
        x = 0.0D+00
        fx = 0.0D+00
      else
        n = n_vec(n_data)
        x = x_vec(n_data)
        fx = fx_vec(n_data)
      end if

      return
      end
      subroutine lobatto_polynomial_plot ( ndx_num, ndx, prefix )

c*********************************************************************72
c
cc LOBATTO_POLYNOMIAL_PLOT plots one or more completed Lobatto polynomials.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 November 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer  NDX_NUM, the number of polynomials to plot.
c
c    Input, integer NDX(NDX_NUM), the orders of 1 or more 
c    Legendre polynomials to be plotted together.
c
c    Input, character * ( * ) PREFIX. the filename prefix.
c
      implicit none

      integer ndx_num
      integer x_num
      parameter ( x_num = 501 )
      integer n_max
      parameter ( n_max = 25 )

      character * ( 255 ) command_filename
      integer command_unit
      character * ( 255 ) data_filename
      integer data_unit
      integer i
      integer j
      double precision l(x_num,n_max)
      double precision lp(x_num,n_max)
      integer n
      integer ndx(ndx_num)
      character * ( 255 ) plot_filename
      character * ( * ) prefix
      double precision x(x_num)
      double precision x_hi
      double precision x_lo
      double precision y(x_num,ndx_num)
      double precision yp(x_num,ndx_num)

      x_lo = -1.0D+00
      x_hi = +1.0D+00
      call r8vec_linspace ( x_num, x_lo, x_hi, x )
c
c  Collect the data.
c
      do j = 1, ndx_num

        n = ndx(j)

        call lobatto_polynomial_value ( x_num, n, x, l )
        do i = 1, x_num
          y(i,j) = l(i,n)
        end do

        call lobatto_polynomial_derivative ( x_num, n, x, lp )
        do i = 1, x_num
          yp(i,j) = lp(i,n)
        end do

      end do

      write ( *, '(a)' ) ''
c
c  Make data file for values.
c
      call get_unit ( data_unit )

      data_filename = trim ( prefix ) // '_value_data.txt'

      open ( unit = data_unit, file = data_filename, 
     &  status = 'replace' )
      do i = 1, x_num
        write ( data_unit, '(g14.6)', advance = 'no' ) x(i)
        do j = 1, ndx_num
          write ( data_unit, '(2x,g14.6)', advance = 'no' ) y(i,j)
        end do
        write ( data_unit, '(a)' ) ''
      end do
      close ( unit = data_unit )

      write ( *, '(a)' ) '  Lobatto value data in "' 
     &  // trim ( data_filename ) // '".'
c
c  Make command file for values.
c
      call get_unit ( command_unit )

      command_filename = trim ( prefix ) // '_value_commands.txt'

      open ( unit = command_unit, file = command_filename, 
     &  status = 'replace' )

      write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) '# Usage:'
      write ( command_unit, '(a)' ) '#  gnuplot < ' // 
     &  trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) 'unset key'
      write ( command_unit, '(a)' ) 'set term png'
      write ( command_unit, '(a)' ) 'set timestamp'
      plot_filename = trim ( prefix ) // '_value.png'
      write ( command_unit, '(a)' ) 'set output "' // 
     &  trim ( plot_filename ) // '"'
      write ( command_unit, '(a)' ) 'set xlabel "x"'
      write ( command_unit, '(a)' ) 'set ylabel "L(n,x)"'
      write ( command_unit, '(a)' ) 'set title "Lobatto values"'
      write ( command_unit, '(a)' ) 'set grid'
      write ( command_unit, '(a)' ) 'set style data lines'
      do j = 1, ndx_num
        if ( j .eq. 1 ) then
          write ( command_unit, '(a,i2,a)', advance = 'no' ) 'plot "'
        else
          write ( command_unit, '(a,i2,a)', advance = 'no' ) '     "'
        end if
        write ( command_unit, '(a,i2,a)', advance = 'no' ) 
     &    trim ( data_filename ) // '" using 1:', j + 1
        if ( j .lt. ndx_num ) then
          write ( command_unit, '(a)', advance = 'no' ) ', \'
        end if
        write ( command_unit, '(a)' ) ''
      end do
      write ( command_unit, '(a)' ) 'quit'

      close ( unit = command_unit )

      write ( *, '(a)' ) '  Lobatto value commands in "' 
     &  // trim ( command_filename ) // '".'
c
c  Make data file for derivatives.
c
      call get_unit ( data_unit )

      data_filename = trim ( prefix ) // '_derivative_data.txt'

      open ( unit = data_unit, file = data_filename, 
     &  status = 'replace' )
      do i = 1, x_num
        write ( data_unit, '(g14.6)', advance = 'no' ) x(i)
        do j = 1, ndx_num
          write ( data_unit, '(2x,g14.6)', advance = 'no' ) yp(i,j)
        end do
        write ( data_unit, '(a)' ) ''
      end do
      close ( unit = data_unit )

      write ( *, '(a)' ) '  Lobatto derivative data stored in "' 
     &  // trim ( data_filename ) // '".'
c
c  Make command file for derivatives.
c
      call get_unit ( command_unit )

      command_filename = trim ( prefix ) // '_derivative_commands.txt'

      open ( unit = command_unit, file = command_filename, 
     &  status = 'replace' )

      write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) '# Usage:'
      write ( command_unit, '(a)' ) '#  gnuplot < ' // 
     &  trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) 'unset key'
      write ( command_unit, '(a)' ) 'set term png'
      write ( command_unit, '(a)' ) 'set timestamp'
      plot_filename = trim ( prefix ) // '_derivative.png'
      write ( command_unit, '(a)' ) 'set output "' // 
     &  trim ( plot_filename ) // '"'
      write ( command_unit, '(a)' ) 'set xlabel "x"'
      write ( command_unit, '(a)' ) 'set ylabel "L(n,x)"'
      write ( command_unit, '(a)' ) 'set title "Lobatto derivatives"'
      write ( command_unit, '(a)' ) 'set grid'
      write ( command_unit, '(a)' ) 'set style data lines'
  
      do j = 1, ndx_num
        if ( j .eq. 1 ) then
          write ( command_unit, '(a,i2,a)', advance = 'no' ) 'plot "'
        else
          write ( command_unit, '(a,i2,a)', advance = 'no' ) '     "'
        end if
        write ( command_unit, '(a,i2,a)', advance = 'no' ) 
     &    trim ( data_filename ) // '" using 1:', j + 1
        if ( j .lt. ndx_num ) then
          write ( command_unit, '(a)', advance = 'no' ) ', \'
        end if
        write ( command_unit, '(a)' ) ''
      end do

      write ( command_unit, '(a)' ) 'quit'

      close ( unit = command_unit )

      write ( *, '(a)' ) '  Lobatto derivative commands in "' 
     &  // trim ( command_filename ) // '".'

      return
      end
      subroutine lobatto_polynomial_value ( m, n, x, l )

c*********************************************************************72
c
cc LOBATTO_POLYNOMIAL_VALUE evaluates the completed Lobatto polynomials Lo(n,x).
c
c  Discussion:
c
c    L(N,X) = ( 1 - X^2 ) * P'(N,X)
c           = N * ( P(N-1,X) - X * P(N,X) ) 
c
c    The Lobatto polynomials are 0 at -1 and +1.
c
c      (1-x^2) * 1
c      (1-x^2) * 3X
c      (1-x^2) * ( -3 + 15x^2 ) / 2
c      (1-x^2) * ( -60x + 140x^3 ) / 8
c      (1-x^2) * ( -15 - 210x^2 + 315x^4 ) / 8
c      (1-x^2) * ( 210x - 1260x^3 + 1386x^5 ) / 16
c      (1-x^2) * ( -35 + 945x^2 - 3465x^4 + 3003x^6 ) / 16
c      (1-x^2) * ( -2520x + 27720x^3 - 72072x^5 + 51480x^7 ) / 128
c      (1-x^2) * ( 315 - 13860x^2 + 90090x^4 - 180180x^6 + 109395x^8 ) / 128
c      (1-x^2) * ( 6930x - 120120x^3 + 540540x^5 - 875160x^7 + 461890x^9 ) / 256
c
c    Mathematica: (replacing "n" by desired index):
c
c      Expand [ ( 1-x^2) * D [ LegendreP[n,x], {x} ] ]
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    18 November 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Milton Abramowitz, Irene Stegun,
c    Handbook of Mathematical Functions,
c    National Bureau of Standards, 1964,
c    ISBN: 0-486-61272-4,
c    LC: QA47.A34.
c
c    Larry Andrews,
c    Special Functions of Mathematics for Engineers,
c    Second Edition,
c    Oxford University Press, 1998,
c    ISBN: 0819426164,
c    LC: QA351.A75.
c
c    Daniel Zwillinger, editor,
c    CRC Standard Mathematical Tables and Formulae,
c    30th Edition,
c    CRC Press, 1996.
c
c  Parameters:
c
c    Input, integer M, the number of evaluation points.
c
c    Input, integer N, the highest order polynomial to evaluate.
c    Note that polynomials 0 through N will be evaluated.
c
c    Input, double precision X(M), the evaluation points.
c
c    Output, double precision L(M,N), the values of the completed Lobatto
c    polynomials of order 1 through N at the point X.
c
      implicit none

      integer m
      integer n

      integer i
      integer j
      double precision l(m,n)
      double precision p(m,n+1)
      double precision x(m)

      do i = 1, m

        if ( 1 .le. n ) then

          l(i,1) = 1.0D+00 - x(i) ** 2

          if ( 2 .le. n ) then

            p(i,1) = 1.0D+00
            p(i,2) = x(i)

            do j = 2, n
              p(i,j+1) = 
     &          ( dble ( 2 * j - 1 ) * x(i) * p(i,j)     
     &          - dble (     j - 1 ) *        p(i,j-1) ) 
     &          / dble (     j     )
            end do

            do j = 2, n
              l(i,j) = dble ( j ) * ( p(i,j) - x(i) * p(i,j+1) )
            end do

          end if

        end if

      end do

      return
      end
      subroutine lobatto_polynomial_values ( n_data, n, x, fx )

c********************************************************************72
c
cc LOBATTO_POLYNOMIAL_VALUES returns values of the completed Lobatto polynomials.
c
c  Discussion:
c
c    In Mathematica, the function can be evaluated by:
c
c      n * LegendreP [ n - 1, x ] - n * x * LegendreP [ n, x ]
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 May 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input/output, integer N_DATA.  The user sets N_DATA to 0
c    before the first call.  On each call, the routine increments N_DATA by 1,
c    and returns the corresponding data; when there is no more data, the
c    output value of N_DATA will be 0 again.
c
c    Output, integer N, the order of the function.
c
c    Output, double precision X, the point where the function is evaluated.
c
c    Output, double precision FX, the value of the function.
c
      implicit none

      integer n_max
      parameter ( n_max = 31 )

      double precision fx
      double precision fx_vec(n_max)
      integer n
      integer n_data
      integer n_vec(n_max)
      double precision x
      double precision x_vec(n_max)

      save fx_vec
      save n_vec
      save x_vec

      data fx_vec /
     &  0.9375000000000000D+00, 
     &  0.7031250000000000D+00, 
     & -0.9667968750000000D+00, 
     & -1.501464843750000D+00, 
     &  0.3639221191406250D+00, 
     &  2.001914978027344D+00, 
     &  0.6597948074340820D+00, 
     & -1.934441328048706D+00, 
     & -1.769941113889217D+00, 
     &  1.215243665501475D+00, 
     &  0.000000000000000D+00, 
     &  0.8692500000000000D+00, 
     &  1.188000000000000D+00, 
     &  1.109250000000000D+00, 
     &  0.7680000000000000D+00, 
     &  0.2812500000000000D+00, 
     & -0.2520000000000000D+00, 
     & -0.7507500000000000D+00, 
     & -1.152000000000000D+00, 
     & -1.410750000000000D+00, 
     & -1.500000000000000D+00, 
     & -1.410750000000000D+00, 
     & -1.152000000000000D+00, 
     & -0.7507500000000000D+00, 
     & -0.2520000000000000D+00, 
     &  0.2812500000000000D+00, 
     &  0.7680000000000000D+00, 
     &  1.109250000000000D+00, 
     &  1.188000000000000D+00, 
     &  0.8692500000000000D+00, 
     &  0.000000000000000D+00 /

      data n_vec /
     &   1,  2, 
     &   3,  4,  5, 
     &   6,  7,  8, 
     &   9, 10,  3, 
     &   3,  3,  3, 
     &   3,  3,  3, 
     &   3,  3,  3, 
     &   3,  3,  3, 
     &   3,  3,  3, 
     &   3,  3,  3, 
     &   3,  3 /

      data x_vec /
     &  0.25D+00, 
     &  0.25D+00, 
     &  0.25D+00, 
     &  0.25D+00, 
     &  0.25D+00, 
     &  0.25D+00, 
     &  0.25D+00, 
     &  0.25D+00, 
     &  0.25D+00, 
     &  0.25D+00, 
     & -1.00D+00, 
     & -0.90D+00, 
     & -0.80D+00, 
     & -0.70D+00, 
     & -0.60D+00, 
     & -0.50D+00, 
     & -0.40D+00, 
     & -0.30D+00, 
     & -0.20D+00, 
     & -0.10D+00, 
     &  0.00D+00, 
     &  0.10D+00, 
     &  0.20D+00, 
     &  0.30D+00, 
     &  0.40D+00, 
     &  0.50D+00, 
     &  0.60D+00, 
     &  0.70D+00, 
     &  0.80D+00, 
     &  0.90D+00, 
     &  1.00D+00 /

      if ( n_data .lt. 0 ) then
        n_data = 0
      end if

      n_data = n_data + 1

      if ( n_max .lt. n_data ) then
        n_data = 0
        n = 0
        x = 0.0D+00
        fx = 0.0D+00
      else
        n = n_vec(n_data)
        x = x_vec(n_data)
        fx = fx_vec(n_data)
      end if

      return
      end
      subroutine r8vec_linspace ( n, a, b, x )

c*********************************************************************72
c
cc R8VEC_LINSPACE creates a vector of linearly spaced values.
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
c
c    4 points evenly spaced between 0 and 12 will yield 0, 4, 8, 12.
c
c    In other words, the interval is divided into N-1 even subintervals,
c    and the endpoints of intervals are used as the points.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 March 2011
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in the vector.
c
c    Input, double precision A, B, the first and last entries.
c
c    Output, double precision X(N), a vector of linearly spaced data.
c
      implicit none

      integer n

      double precision a
      double precision b
      integer i
      double precision x(n)

      if ( n .eq. 1 ) then

        x(1) = ( a + b ) / 2.0D+00

      else

        do i = 1, n
          x(i) = ( dble ( n - i     ) * a
     &           + dble (     i - 1 ) * b )
     &           / dble ( n     - 1 )
        end do

      end if

      return
      end
      subroutine timestamp ( )

c*********************************************************************72
c
cc TIMESTAMP prints out the current YMDHMS date as a timestamp.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    None
c
      implicit none

      character * ( 8 ) ampm
      integer d
      character * ( 8 ) date
      integer h
      integer m
      integer mm
      character * ( 9 ) month(12)
      integer n
      integer s
      character * ( 10 ) time
      integer y

      save month

      data month /
     &  'January  ', 'February ', 'March    ', 'April    ',
     &  'May      ', 'June     ', 'July     ', 'August   ',
     &  'September', 'October  ', 'November ', 'December ' /

      call date_and_time ( date, time )

      read ( date, '(i4,i2,i2)' ) y, m, d
      read ( time, '(i2,i2,i2,1x,i3)' ) h, n, s, mm

      if ( h .lt. 12 ) then
        ampm = 'AM'
      else if ( h .eq. 12 ) then
        if ( n .eq. 0 .and. s .eq. 0 ) then
          ampm = 'Noon'
        else
          ampm = 'PM'
        end if
      else
        h = h - 12
        if ( h .lt. 12 ) then
          ampm = 'PM'
        else if ( h .eq. 12 ) then
          if ( n .eq. 0 .and. s .eq. 0 ) then
            ampm = 'Midnight'
          else
            ampm = 'AM'
          end if
        end if
      end if

      write ( *,
     &  '(i2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' )
     &  d, month(m), y, h, ':', n, ':', s, '.', mm, ampm

      return
      end
