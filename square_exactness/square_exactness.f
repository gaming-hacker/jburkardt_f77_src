      subroutine legendre_2d_exactness ( a, b, n, x, y, w, t )

c*********************************************************************72
c
cc LEGENDRE_2D_EXACTNESS: monomial exactness for the 2D Legendre integral.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    30 May 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A(2), the lower limits of integration.
c
c    Input, double precision B(2), the upper limits of integration.
c
c    Input, integer N, the number of points in the rule.
c
c    Input, double precision X(N), Y(N), the quadrature points.
c
c    Input, double precision W(N), the quadrature weights.
c
c    Input, integer T, the maximum total degree.
c    0 <= T.
c
      implicit none

      integer n

      double precision a(2)
      double precision b(2)
      double precision e
      integer i
      integer j
      integer p(2)
      double precision q
      double precision r8vec_dot_product
      double precision s
      integer t
      integer tt
      double precision v(n)
      double precision w(n)
      double precision x(n)
      double precision y(n)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  Quadrature rule for the 2D Legendre integral.'
      write ( *, '(a,i3)' ) '  Number of points in rule is ', n
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   D   I       J          Relative Error'

      do tt = 0, t

        write ( *, '(2x,i2)' )  tt

        do j = 0, tt

          i = tt - j

          p(1) = i
          p(2) = j
 
          call legendre_2d_monomial_integral ( a, b, p, s )

          do i = 1, n
            v(i) = x(i) ** p(1) * y(i) ** p(2)
          end do

          q = r8vec_dot_product ( n, w, v )

          if ( s .eq. 0.0D+00 ) then
            e = abs ( q )
          else
            e = abs ( q - s ) / abs ( s )
          end if

          write ( *, '(2x,i6,2x,i6,2x,f24.16)' ) p(1:2), e

        end do

      end do

      return
      end
      subroutine legendre_2d_monomial_integral ( a, b, p, exact )

c*********************************************************************72
c
cc LEGENDRE_2D_MONOMIAL_INTEGRAL the Legendre integral of a monomial.
c
c  Discussion:
c
c    The Legendre integral to be evaluated has the form
c
c      I(f) = integral ( y1 <= y <= y2 ) 
c             integral ( x1 <= x <= x2 ) x^i y^j dx dy
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    30 May 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A(2), the lower limits of integration.
c
c    Input, double precision B(2), the upper limits of integration.
c
c    Input, integer P(2), the exponents of X and Y.
c
c    Output, double precision EXACT, the value of the exact integral.
c
      implicit none

      double precision a(2)
      double precision b(2)
      double precision exact
      integer p(2)

      exact = ( b(1) ** ( p(1) + 1 ) - a(1) ** ( p(1) + 1 ) ) 
     &      / dble ( p(1) + 1 ) 
     &      * ( b(2) ** ( p(2) + 1 ) - a(2) ** ( p(2) + 1 ) ) 
     &      / dble ( p(2) + 1 )

      return
      end
      function r8vec_dot_product ( n, v1, v2 )

c*********************************************************************72
c
cc R8VEC_DOT_PRODUCT finds the dot product of a pair of R8VEC's.
c
c  Discussion:
c
c    An R8VEC is a vector of R8 values.
c
c    In FORTRAN90, the system routine DOT_PRODUCT should be called
c    directly.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 May 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the dimension of the vectors.
c
c    Input, double precision V1(N), V2(N), the vectors.
c
c    Output, double precision R8VEC_DOT_PRODUCT, the dot product.
c
      implicit none

      integer n

      integer i
      double precision r8vec_dot_product
      double precision v1(n)
      double precision v2(n)
      double precision value

      value = 0.0D+00
      do i = 1, n
        value = value + v1(i) * v2(i)
      end do

      r8vec_dot_product = value

      return
      end
      subroutine r8vec_print ( n, a, title )

c*********************************************************************72
c
cc R8VEC_PRINT prints an R8VEC.
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
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
c    Input, integer N, the number of components of the vector.
c
c    Input, double precision A(N), the vector to be printed.
c
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer n

      double precision a(n)
      integer i
      character * ( * ) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i8,a,1x,g16.8)' ) i, ':', a(i)
      end do

      return
      end
      subroutine r8vec2_print ( n, a1, a2, title )

c*********************************************************************72
c
cc R8VEC2_PRINT prints an R8VEC2.
c
c  Discussion:
c
c    An R8VEC2 is a dataset consisting of N pairs of R8s, stored
c    as two separate vectors A1 and A2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 February 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of components of the vector.
c
c    Input, double precision A1(N), A2(N), the vectors to be printed.
c
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer n

      double precision a1(n)
      double precision a2(n)
      integer i
      character * ( * ) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i8,a,1x,g14.6,2x,g14.6)' ) i, ':', a1(i), a2(i)
      end do

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
