      subroutine line_ncc_rule ( n, a, b, x, w )

c*****************************************************************************80
c
cc LINE_NCC_RULE computes a Newton-Cotes Closed (NCC) quadrature rule.
c
c  Discussion:
c
c    The integral:
c
c      Integral ( A <= X <= B ) F(X) dx
c
c    The quadrature rule:
c
c      Sum ( 1 <= I <= N ) W(I) * F ( X(I) ).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 April 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the order.
c
c    Input, double precision A, B, the endpoints of the interval.
c
c    Input, double precision X(N), the abscissas.
c
c    Output, double precision W(N), the weights.
c
      implicit none

      integer n

      double precision a
      double precision b
      double precision d(n)
      integer i
      integer j
      integer k
      double precision w(n)
      double precision x(n)
      double precision y_a
      double precision y_b
c
c  Define the points X.
c
      call r8vec_linspace ( n, a, b, x )
c
c  Compute the Lagrange basis polynomial which is 1 at X(I),
c  and zero at the other nodes.
c
      do i = 1, n

        do j = 1, n
          d(j) = 0.0D+00
        end do
        d(i) = 1.0D+00

        do j = 2, n
          do k = j, n
            d(n+j-k) = ( d(n+j-k-1) - d(n+j-k) ) 
     &        / ( x(n+1-k) - x(n+j-k) )
          end do
        end do

        do j = 1, n - 1
          do k = 1, n - j
            d(n-k) = d(n-k) - x(n-k-j+1) * d(n-k+1)
          end do
        end do
c
c  Evaluate the antiderivative of the polynomial at the endpoints.
c
        y_a = d(n) / dble ( n )
        do j = n - 1, 1, -1
          y_a = y_a * a + d(j) / dble ( j )
        end do
        y_a = y_a * a

        y_b = d(n) / dble ( n )
        do j = n - 1, 1, -1
          y_b = y_b * b + d(j) / dble ( j )
        end do
        y_b = y_b * b

        w(i) = y_b - y_a

      end do

      return
      end
      subroutine r8vec_linspace ( n, a_first, a_last, a )

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
c    Input, double precision A_FIRST, A_LAST, the first and last entries.
c
c    Output, double precision A(N), a vector of linearly spaced data.
c
      implicit none

      integer n

      double precision a(n)
      double precision a_first
      double precision a_last
      integer i

      if ( n .eq. 1 ) then

        a(1) = ( a_first + a_last ) / 2.0D+00

      else

        do i = 1, n
          a(i) = ( dble ( n - i     ) * a_first 
     &           + dble (     i - 1 ) * a_last )
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
