      function line01_length ( )

c*********************************************************************72
c
cc LINE01_LENGTH: length of the unit line in 1D.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 January 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision LINE01_LENGTH, the length.
c
      implicit none

      double precision line01_length

      line01_length = 1.0D+00

      return
      end
      subroutine line01_monomial_integral ( e, integral )

c*********************************************************************72
c
cc LINE01_MONOMIAL_INTEGRAL: monomial integral over the unit line in 1D.
c
c  Discussion:
c
c    The integration region is
c
c      0 <= X <= 1.
c
c    The monomial is F(X) = X^E.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 January 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Philip Davis, Philip Rabinowitz,
c    Methods of Numerical Integration,
c    Second Edition,
c    Academic Press, 1984, page 263.
c
c  Parameters:
c
c    Input, integer E, the exponent.
c    E must not equal -1.
c
c    Output, double precision INTEGRAL, the integral.
c
      implicit none

      integer e
      double precision integral

      if ( e .eq. -1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'LINE01_MONOMIAL_INTEGRAL - Fatal error!'
        write ( *, '(a)' ) '  Exponent E = -1 is not allowed!'
        stop 1
      end if

      integral = 1.0D+00 / dble ( e + 1 )

      return
      end
      subroutine line01_sample ( n, seed, x )

c*********************************************************************72
c
cc LINE01_SAMPLE samples the unit line in 1D.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 January 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of points.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision X(N), the points.
c
      implicit none

      integer n

      integer seed
      double precision x(n)

      call r8vec_uniform_01 ( n, seed, x )

      return
      end
      subroutine monomial_value_1d ( n, e, x, v )

c*********************************************************************72
c
cc MONOMIAL_VALUE_1D evaluates a monomial in 1D.
c
c  Discussion:
c
c    This routine evaluates a monomial of the form
c
c      x^expon
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 January 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of points.
c
c    Input, integer E, the exponent.
c
c    Input, double precision X(N), the point coordinates.
c
c    Output, double precision V(N), the value of the monomial.
c
      implicit none

      integer n

      integer e
      integer j
      double precision v(n)
      double precision x(n)

      do j = 1, n
        v(j) = x(j) ** e
      end do

      return
      end
      function r8vec_sum ( n, v1 )

c*********************************************************************72
c
cc R8VEC_SUM sums the entries of an R8VEC.
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
c
c    In FORTRAN90, the system routine SUM should be called
c    directly.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 July 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the dimension of the vectors.
c
c    Input, double precision V1(N), the vector.
c
c    Output, double precision R8VEC_SUM, the sum of the entries.
c
      implicit none

      integer n

      integer i
      double precision r8vec_sum
      double precision v1(n)
      double precision value

      value = 0.0D+00
      do i = 1, n
        value = value + v1(i)
      end do

      r8vec_sum = value

      return
      end
      subroutine r8vec_uniform_01 ( n, seed, r )

c*********************************************************************72
c
cc R8VEC_UNIFORM_01 returns a unit pseudorandom R8VEC.
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
c    17 July 2006
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Paul Bratley, Bennett Fox, Linus Schrage,
c    A Guide to Simulation,
c    Springer Verlag, pages 201-202, 1983.
c
c    Bennett Fox,
c    Algorithm 647:
c    Implementation and Relative Efficiency of Quasirandom
c    Sequence Generators,
c    ACM Transactions on Mathematical Software,
c    Volume 12, Number 4, pages 362-376, 1986.
c
c    Peter Lewis, Allen Goodman, James Miller,
c    A Pseudo-Random Number Generator for the System/360,
c    IBM Systems Journal,
c    Volume 8, pages 136-143, 1969.
c
c  Parameters:
c
c    Input, integer N, the number of entries in the vector.
c
c    Input/output, integer SEED, the "seed" value, which should NOT be 0.
c    On output, SEED has been updated.
c
c    Output, double precision R(N), the vector of pseudorandom values.
c
      implicit none

      integer n

      integer i
      integer k
      integer seed
      double precision r(n)

      do i = 1, n

        k = seed / 127773

        seed = 16807 * ( seed - k * 127773 ) - k * 2836

        if ( seed .lt. 0 ) then
          seed = seed + 2147483647
        end if

        r(i) = dble ( seed ) * 4.656612875D-10

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
