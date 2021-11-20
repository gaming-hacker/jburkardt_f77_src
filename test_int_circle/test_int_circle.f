      function circle_unit_area ( )

c*********************************************************************72
c
cc CIRCLE_UNIT_AREA returns the area of the unit circle.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    27 December 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision CIRCLE_UNIT_AREA, the area.
c
      implicit none

      double precision circle_unit_area
      double precision r
      parameter ( r = 1.0D+00 )
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )

      circle_unit_area = r8_pi * r * r

      return
      end
      subroutine circle_unit_sample ( n, seed, xy )

c*********************************************************************72
c
cc CIRCLE_UNIT_SAMPLE returns sample points from the unit circle.
c
c  Discussion:
c
c    The unit circle has center at the origin, and radius 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    18 December 2013
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
c    Output, double precision XY(2,N), the points.
c
      implicit none

      integer n

      integer j
      double precision r(n)
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      integer seed
      double precision t(n)
      double precision xy(2,n)

      call r8vec_uniform_01 ( n, seed, r )
      do j = 1, n
        r(j) = sqrt ( r(j) )
      end do

      call r8vec_uniform_01 ( n, seed, t )
      do j = 1, n
        t(j) = 2.0D+00 * r8_pi * t(j)
      end do

      do j = 1, n
        xy(1,j) = r(j) * cos ( t(j) )
        xy(2,j) = r(j) * sin ( t(j) )
      end do

      return
      end
      function cos_power_int ( a, b, n )

c*********************************************************************72
c
cc COS_POWER_INT evaluates the cosine power integral.
c
c  Discussion:
c
c    The function is defined by
c
c      COS_POWER_INT(A,B,N) = Integral ( A <= T <= B ) ( cos ( t ))^n dt
c
c    The algorithm uses the following fact:
c
c      Integral cos^n ( t ) = -(1/n) * (
c        cos^(n-1)(t) * sin(t) + ( n-1 ) * Integral cos^(n-2) ( t ) dt )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 March 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters
c
c    Input, double precision A, B, the limits of integration.
c
c    Input, integer N, the power of the function.
c
c    Output, double precision COS_POWER_INT, the value of the integral.
c
      implicit none

      double precision a
      double precision b
      double precision ca
      double precision cb
      double precision cos_power_int
      integer m
      integer mlo
      integer n
      double precision sa
      double precision sb
      integer seed
      double precision value

      if ( n .lt. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'COS_POWER_INT - Fatal error!'
        write ( *, '(a)' ) '  Power N < 0.'
        stop 1
      end if

      sa = sin ( a )
      sb = sin ( b )
      ca = cos ( a )
      cb = cos ( b )

      if ( mod ( n, 2 ) .eq. 0 ) then
        value = b - a
        mlo = 2
      else
        value = sb - sa
        mlo = 3
      end if

      do m = mlo, n, 2
        value = ( dble ( m - 1 ) * value 
     &    - ca ** ( m - 1 ) * sa 
     &    + cb ** ( m - 1 ) * sb ) / dble ( m )
      end do

      cos_power_int = value

      return
      end
      function p01_exact ( )

c*********************************************************************72
c
cc P01_EXACT returns the exact integral of function 1 over the unit circle.
c
c  Discussion:
c
c    The integral depends on a parameter E which must be set by calling
c    P01_PARAM_SET ( E ) before calling this function.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 December 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision P01_EXACT, the exact integral.
c
      implicit none

      double precision cos_power_int
      integer e
      double precision p01_exact
      integer p01_param_get
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision r8_zero
      parameter ( r8_zero = 0.0D+00 )

      e = p01_param_get ( )

      p01_exact = cos_power_int ( r8_zero, 2.0D+00 * r8_pi, e ) 
     &  / dble ( e + 2 )

      return
      end
      subroutine p01_f ( n, x, fx )

c*********************************************************************72
c
cc P01_F evaluates test function #1 in the unit circle.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 December 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of variables.
c
c    Input, double precision X(2,N), the arguments.
c
c    Output, double precision FX(N), the values of the integrand.
c
      implicit none

      integer n

      integer e
      double precision fx(n)
      integer j
      integer p01_param_get
      double precision x(2,n)

      e = p01_param_get ( )

      do j = 1, n
        fx(j) = x(1,j) ** e
      end do

      return
      end
      subroutine p01_param ( action, e )

c*********************************************************************72
c
cc P01_PARAM sets or gets the value of the parameter for test function 1.
c
c  Discussion:
c
c    The parameter E is the exponent of X^E.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 December 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) ACTION, indicates which action is desired.
c    'G' will get the parameter value.
c    'S' will set the parameter value.
c
c    Input/output, integer E.
c    If ACTION is 'S', then on input this is the new value of the parameter,
c    If ACTION is 'G', then on output this is the stored value of the parameter.
c
      implicit none

      character * ( * ) action
      integer e
      integer e_saved

      save e_saved

      data e_saved / 0 /

      if ( action(1:1) .eq. 'g' .or. action(1:1) .eq. 'G' ) then
        e = e_saved
      else if ( action(1:1) .eq. 's' .or. action(1:1) .eq. 'S' ) then
        e_saved = e
      else
        write ( *, '(a,i4)' ) '  E = ', e_saved
      end if

      return
      end
      function p01_param_get ( )

c*********************************************************************72
c
cc P01_PARAM_GET gets the value of the parameter for test function 1.
c
c  Discussion:
c
c    The parameter E is the exponent of X^E.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 December 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, integer P01_PARAM_GET, the value of the parameter.
c
      implicit none

      integer e
      integer p01_param_get

      call p01_param ( 'GET', e )
      p01_param_get = e

      return
      end
      subroutine p01_param_set ( e )

c*********************************************************************72
c
cc P01_PARAM_SET sets the parameter for test function 1.
c
c  Discussion:
c
c    The parameter E is the exponent of X^E.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 December 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer E, the value of the parameter.
c
      implicit none

      integer e

      call p01_param ( 'SET', e )

      return
      end
      function p02_exact ( )

c*********************************************************************72
c
cc P02_EXACT returns the exact integral of function 2 over the unit circle.
c
c  Discussion:
c
c    The integral depends on a parameter E which must be set by calling
c    P02_PARAM_SET ( E ) before calling this function.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 December 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision P02_EXACT, the exact integral.
c
      implicit none

      integer e
      double precision p02_exact
      integer p02_param_get
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )

      e = p02_param_get ( )

      p02_exact = 2.0D+00 * r8_pi / dble ( e + 2 )

      return
      end
      subroutine p02_f ( n, x, fx )

c*********************************************************************72
c
cc P02_F evaluates test function #2 in the unit circle.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 December 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of variables.
c
c    Input, double precision X(2,N), the arguments.
c
c    Output, double precision FX(N), the values of the integrand.
c
      implicit none

      integer n

      integer e
      double precision fx(n)
      integer j
      integer p02_param_get
      double precision r(n)
      double precision x(2,n)

      e = p02_param_get ( )

      do j = 1, n
        r(j) = sqrt ( x(1,j) ** 2 + x(2,j) ** 2 )
      end do

      do j = 1, n
        fx(j) = r(j) ** e
      end do

      return
      end
      subroutine p02_param ( action, e )

c*********************************************************************72
c
cc P02_PARAM sets or gets the value of the parameter for test function 2.
c
c  Discussion:
c
c    The parameter E is the exponent of X^E.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 December 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) ACTION, indicates which action is desired.
c    'G' will get the parameter value.
c    'S' will set the parameter value.
c
c    Input/output, integer E.
c    If ACTION is 'S', then on input this is the new value of the parameter,
c    If ACTION is 'G', then on output this is the stored value of the parameter.
c
      implicit none

      character * ( * ) action
      integer e
      integer e_saved

      save e_saved

      data e_saved / 0 /

      if ( action(1:1) .eq. 'g' .or. action(1:1) .eq. 'G' ) then
        e = e_saved
      elseif ( action(1:1) .eq. 's' .or. action(1:1) .eq. 'S' ) then
        e_saved = e
      else
        write ( *, '(a,i4)' ) '  E = ', e_saved
      end if

      return
      end
      function p02_param_get ( )

c*********************************************************************72
c
cc P02_PARAM_GET gets the value of the parameter for test function 2.
c
c  Discussion:
c
c    The parameter E is the exponent of R^E.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 December 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, integer P02_PARAM_GET, the value of the parameter.
c
      implicit none

      integer e
      integer p02_param_get

      call p02_param ( 'GET', e )
      p02_param_get = e

      return
      end
      subroutine p02_param_set ( e )

c*********************************************************************72
c
cc P02_PARAM_SET sets the parameter for test function 2.
c
c  Discussion:
c
c    The parameter E is the exponent of R^E.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 December 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer E, the value of the parameter.
c
      implicit none

      integer e

      call p02_param ( 'SET', e )

      return
      end
      function p03_exact ( )

c*********************************************************************72
c
cc P03_EXACT returns the exact integral of function 3 over the unit circle.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 December 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision P03_EXACT, the exact integral.
c
      implicit none

      double precision p03_exact
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      
c     nu = 1.0
c     z = 1.0
c     p03_exact = 2.0D+00 * r8_pi * besseli ( nu, z )

      p03_exact = 2.0D+00 * r8_pi * 0.565159103992485D+00

      return
      end
      subroutine p03_f ( n, x, fx )

c*********************************************************************72
c
cc P03_F evaluates test function #3 in the unit circle.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 December 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of variables.
c
c    Input, double precision X(2,N), the arguments.
c
c    Output, double precision FX(N), the values of the integrand.
c
      implicit none

      integer n

      double precision fx(n)
      integer j
      double precision x(2,n)

      do j = 1, n
        fx(j) = exp ( x(1,j) )
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
      function sin_power_int ( a, b, n )

c*********************************************************************72
c
cc SIN_POWER_INT evaluates the sine power integral.
c
c  Discussion:
c
c    The function is defined by
c
c      SIN_POWER_INT(A,B,N) = Integral ( A <= T <= B ) ( sin ( t ))^n dt
c
c    The algorithm uses the following fact:
c
c      Integral sin^n ( t ) = (1/n) * (
c        sin^(n-1)(t) * cos(t) + ( n-1 ) * Integral sin^(n-2) ( t ) dt )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 December 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters
c
c    Input, double precision A, B, the limits of integration.
c
c    Input, integer N, the power of the sine function.
c
c    Output, double precision SIN_POWER_INT, the value of the integral.
c
      implicit none

      double precision a
      double precision b
      double precision ca
      double precision cb
      integer m
      integer mlo
      integer n
      double precision sa
      double precision sb
      double precision sin_power_int
      double precision value

      if ( n .lt. 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'SIN_POWER_INT - Fatal error!'
        write ( *, '(a)' ) '  Power N < 0.'
        stop 1
      end if

      sa = sin ( a )
      sb = sin ( b )
      ca = cos ( a )
      cb = cos ( b )

      if ( mod ( n, 2 ) .eq. 0 ) then
        value = b - a
        mlo = 2
      else
        value = ca - cb
        mlo = 3
      end if

      do m = mlo, n, 2
        value = ( dble ( m - 1 ) * value 
     &    + sa ** ( m - 1 ) * ca 
     &    - sb ** ( m - 1 ) * cb ) / dble ( m )
      end do

      sin_power_int = value

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
