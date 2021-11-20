      subroutine monomial_value ( m, n, e, x, v )

c*********************************************************************72
c
cc MONOMIAL_VALUE evaluates a monomial.
c
c  Discussion:
c
c    This routine evaluates a monomial of the form
c
c      product ( 1 <= i <= m ) x(i)^e(i)
c
c    where the exponents are nonnegative integers.  Note that
c    if the combination 0^0 is encountered, it should be treated
c    as 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 April 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer N, the number of points at which the
c    monomial is to be evaluated.
c
c    Input, integer E(M), the exponents.
c
c    Input, double precision X(M,N), the point coordinates.
c
c    Output, double precision V(N), the value of the monomial.
c
      implicit none

      integer m
      integer n

      integer e(m)
      integer i
      integer j
      double precision v(n)
      double precision x(m,n)

      do j = 1, n
        v(j) = 1.0D+00
      end do

      do i = 1, m
        if ( 0 .ne. e(i) ) then
          do j = 1, n
            v(j) = v(j) * x(i,j) ** e(i)
          end do
        end if
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
      integer i4_huge
      parameter ( i4_huge = 2147483647 )
      integer k
      integer seed
      double precision r(n)

      do i = 1, n

        k = seed / 127773

        seed = 16807 * ( seed - k * 127773 ) - k * 2836

        if ( seed .lt. 0 ) then
          seed = seed + i4_huge
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
      function wedge01_volume ( )

c*********************************************************************72
c
cc WEDGE01_VOLUME returns the volume of the unit wedge in 3D.
c
c  Discussion:
c
c    The unit wedge is:
c
c      0 <= X
c      0 <= Y
c      X + Y <= 1
c      -1 <= Z <= 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 August 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision WEDGE01_VOLUME, the volume of the unit wedge.
c
      implicit none

      double precision wedge01_volume

      wedge01_volume = 1.0D+00

      return
      end
      subroutine wedge01_integral ( e, value )

c*********************************************************************72
c
cc WEDGE01_INTEGRAL returns the integral of a monomial in the unit wedge in 3D.
c
c  Discussion:
c
c    This routine returns the integral of
c
c      product ( 1 <= I <= 3 ) X(I)^E(I)
c
c    over the unit wedge.
c
c    The integration region is:
c
c      0 <= X
c      0 <= Y
c      X + Y <= 1
c      -1 <= Z <= 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 August 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Arthur Stroud,
c    Approximate Calculation of Multiple Integrals,
c    Prentice Hall, 1971,
c    ISBN: 0130438936,
c    LC: QA311.S85.
c
c  Parameters:
c
c    Input, integer E(3), the exponents.
c
c    Output, double precision VALUE, the integral of the monomial.
c
      implicit none

      integer e(3)
      integer i
      integer k
      double precision value

      value = 1.0D+00

      k = e(1)

      do i = 1, e(2)
        k = k + 1
        value = value * dble ( i ) / dble ( k )
      end do

      k = k + 1
      value = value / dble ( k )

      k = k + 1
      value = value / dble ( k )
c
c  Now account for integration in Z.
c
      if ( e(3) .eq. - 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'WEDGE01_INTEGRAL - Fatal error!'
        write ( *, '(a)' ) '  E(3) = -1 is not a legal input.'
        stop 1
      else if ( mod ( e(3), 2 ) .eq. 1 ) then
        value = 0.0D+00
      else
        value = value * 2.0D+00 / dble ( e(3) + 1 )
      end if

      return
      end
      subroutine wedge01_sample ( n, seed, x )

c*********************************************************************72
c
cc WEDGE01_SAMPLE samples points uniformly from the unit wedge in 3D.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 August 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Reuven Rubinstein,
c    Monte Carlo Optimization, Simulation, and Sensitivity
c    of Queueing Networks,
c    Krieger, 1992,
c    ISBN: 0894647644,
c    LC: QA298.R79.
c
c  Parameters:
c
c    Input, integer N, the number of points.
c
c    Input/output, integer SEED, a seed for the random
c    number generator.
c
c    Output, double precision X(3,N), the points.
c
      implicit none

      integer m
      parameter ( m = 3 )
      integer n

      double precision e(4)
      double precision e_sum
      integer i
      integer j
      integer seed
      double precision x(m,n)

      do j = 1, n

        call r8vec_uniform_01 ( 4, seed, e )

        do i = 1, 3
          e(i) = - log ( e(i) )
        end do

        e_sum = 0.0D+00
        do i = 1, 3
          e_sum = e_sum + e(i)
        end do

        x(1,j) = e(1) / e_sum
        x(2,j) = e(2) / e_sum
        x(3,j) = 2.0D+00 * e(4) - 1.0D+00

      end do

      return
      end
