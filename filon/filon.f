      subroutine filon_fun_cos ( n, f, a, b, t, value )

c*********************************************************************72
c
cc FILON_FUN_COS uses Filon's method on integrals with a cosine factor.
c
c  Discussion:
c
c    The integral to be approximated has the form:
c
c      Integral ( A .le. X .le. B ) F(X) * COS(T*X) dX
c
c    where T is user specified.
c
c    The function is interpolated over each subinterval by
c    a parabolic arc.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 May 2014
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
c    Stephen Chase, Lloyd Fosdick,
c    An Algorithm for Filon Quadrature,
c    Communications of the Association for Computing Machinery,
c    Volume 12, Number 8, August 1969, pages 453-457.
c
c    Stephen Chase, Lloyd Fosdick,
c    Algorithm 353:
c    Filon Quadrature,
c    Communications of the Association for Computing Machinery,
c    Volume 12, Number 8, August 1969, pages 457-458.
c
c    Philip Davis, Philip Rabinowitz,
c    Methods of Numerical Integration,
c    Second Edition,
c    Dover, 2007,
c    ISBN: 0486453391,
c    LC: QA299.3.D28.
c
c  Parameters:
c
c    Input, integer N, the number of data points.
c    N must be odd, and greater than 1.
c
c    Input, external F, the subroutine which evaluates the integrand,
c    of the form subroutine F ( N, X, FX ).
c
c    Input, double precision A, B, the limits of integration.
c
c    Input, double precision T, the multiplier of the X argument of the cosine.
c
c    Output, double precision VALUE, the approximate value of the integral.
c
      implicit none

      integer n

      double precision a
      double precision alpha
      double precision b
      double precision beta
      double precision c2n
      double precision c2nm1
      double precision cost
      external f
      double precision ftab(n)
      double precision gamma
      double precision h
      integer i
      double precision sint
      double precision t
      double precision theta
      double precision value
      double precision x(n)

      if ( a .eq. b ) then
        value = 0.0D+00
        return
      end if
     
      if ( n .le. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'FILON_FUN_COS - Fatal error!'
        write ( *, '(a)' ) '  N < 2'
        write ( *, '(a,i8)' ) '  N = ', n
        stop 1
      end if
     
      if ( mod ( n, 2 ) .ne. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'FILON_FUN_COS - Fatal error!'
        write ( *, '(a)' ) '  N must be odd.'
        write ( *, '(a,i8)' ) '  N = ', n
        stop 1
      end if
c
c  Set the X values.
c
      do i = 1, n
        x(i) = ( dble ( n - i     ) * a   
     &         + dble (     i - 1 ) * b ) 
     &         / dble ( n     - 1 )
      end do

      h = ( b - a ) / dble ( n - 1 )

      theta = t * h
      sint = sin ( theta )
      cost = cos ( theta )

      if ( 6.0D+00 * abs ( theta ) .le. 1.0D+00 ) then

        alpha = 2.0D+00 * theta**3 /   45.0D+00 
     &        - 2.0D+00 * theta**5 /  315.0D+00 
     &        + 2.0D+00 * theta**7 / 4725.0D+00
      
        beta =  2.0D+00            /     3.0D+00 
     &        + 2.0D+00 * theta**2 /    15.0D+00 
     &        - 4.0D+00 * theta**4 /   105.0D+00 
     &        + 2.0D+00 * theta**6 /   567.0D+00 
     &        - 4.0D+00 * theta**8 / 22275.0D+00

        gamma = 4.0D+00            /      3.0D+00 
     &        - 2.0D+00 * theta**2 /     15.0D+00 
     &        +           theta**4 /    210.0D+00 
     &        -           theta**6 /  11340.0D+00

      else

        alpha = ( theta**2 + theta * sint * cost - 2.0D+00 * sint**2 ) 
     &    / theta**3

        beta = ( 2.0D+00 * theta + 2.0D+00 * theta * cost**2 
     &    - 4.0D+00 * sint * cost ) / theta**3

        gamma = 4.0D+00 * ( sint - theta * cost ) / theta**3
      
      end if
c
c  Tabulate the function.
c
      call f ( n, x, ftab )

      c2n = 0.0D+00
      do i = 1, n, 2
        c2n = c2n + ftab(i) * cos ( t * x(i) )
      end do
      c2n = c2n - 0.5D+00 * ( ftab(n) * cos ( t * x(n) ) 
     &                      + ftab(1) * cos ( t * x(1) ) )

      c2nm1 = 0.0D+00
      do i = 2, n - 1, 2
        c2nm1 = c2nm1 + ftab(i) * cos ( t * x(i) )
      end do
     
      value = h * ( 
     &    alpha * ( ftab(n) * sin ( t * x(n) )  
     &            - ftab(1) * sin ( t * x(1) ) ) 
     &  + beta * c2n 
     &  + gamma * c2nm1 )

      return
      end
      subroutine filon_tab_cos ( n, ftab, a, b, t, value )

c*********************************************************************72
c
cc FILON_TAB_COS uses Filon's method on integrals with a cosine factor.
c
c  Discussion:
c
c    The integral to be approximated has the form:
c
c      Integral ( A .le. X .le. B ) F(X) * COS(T*X) dX
c
c    where T is user specified.
c
c    The function is interpolated over each subinterval by
c    a parabolic arc.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 February 2006
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
c    Stephen Chase, Lloyd Fosdick,
c    An Algorithm for Filon Quadrature,
c    Communications of the Association for Computing Machinery,
c    Volume 12, Number 8, August 1969, pages 453-457.
c
c    Stephen Chase, Lloyd Fosdick,
c    Algorithm 353:
c    Filon Quadrature,
c    Communications of the Association for Computing Machinery,
c    Volume 12, Number 8, August 1969, pages 457-458.
c
c    Philip Davis, Philip Rabinowitz,
c    Methods of Numerical Integration,
c    Second Edition,
c    Dover, 2007,
c    ISBN: 0486453391,
c    LC: QA299.3.D28.
c
c  Parameters:
c
c    Input, integer N, the number of data points.
c    N must be odd, and greater than 1.
c
c    Input, double precision FTAB(N), contains the value of the function
c    at A, A+H, A+2*H, ... , B-H, B, where H = (B-A)/(N-1).
c
c    Input, double precision A, B, the limits of integration.
c
c    Input, double precision T, the multiplier of the X argument of the cosine.
c
c    Output, double precision VALUE, the approximate value of the integral.
c
      implicit none

      integer n

      double precision a
      double precision alpha
      double precision b
      double precision beta
      double precision c2n
      double precision c2nm1
      double precision cost
      double precision ftab(n)
      double precision gamma
      double precision h
      integer i
      double precision sint
      double precision t
      double precision theta
      double precision value
      double precision x(n)

      if ( a .eq. b ) then
        value = 0.0D+00
        return
      end if
     
      if ( n .le. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'FILON_TAB_COS - Fatal error!'
        write ( *, '(a)' ) '  N < 2'
        write ( *, '(a,i8)' ) '  N = ', n
        stop 1
      end if
     
      if ( mod ( n, 2 ) .ne. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'FILON_TAB_COS - Fatal error!'
        write ( *, '(a)' ) '  N must be odd.'
        write ( *, '(a,i8)' ) '  N = ', n
        stop 1
      end if
c
c  Set the X values.
c
      do i = 1, n
        x(i) = ( dble ( n - i     ) * a   
     &         + dble (     i - 1 ) * b ) 
     &         / dble ( n     - 1 )
      end do

      h = ( b - a ) / dble ( n - 1 )

      theta = t * h
      sint = sin ( theta )
      cost = cos ( theta )

      if ( 6.0D+00 * abs ( theta ) .le. 1.0D+00 ) then

        alpha = 2.0D+00 * theta**3 /   45.0D+00 
     &        - 2.0D+00 * theta**5 /  315.0D+00 
     &        + 2.0D+00 * theta**7 / 4725.0D+00
      
        beta =  2.0D+00            /     3.0D+00 
     &        + 2.0D+00 * theta**2 /    15.0D+00 
     &        - 4.0D+00 * theta**4 /   105.0D+00 
     &        + 2.0D+00 * theta**6 /   567.0D+00 
     &        - 4.0D+00 * theta**8 / 22275.0D+00

        gamma = 4.0D+00            /      3.0D+00 
     &        - 2.0D+00 * theta**2 /     15.0D+00 
     &        +           theta**4 /    210.0D+00 
     &        -           theta**6 /  11340.0D+00

      else

        alpha = ( theta**2 + theta * sint * cost - 2.0D+00 * sint**2 ) 
     &    / theta**3

        beta = ( 2.0D+00 * theta + 2.0D+00 * theta * cost**2 
     &    - 4.0D+00 * sint * cost ) / theta**3

        gamma = 4.0D+00 * ( sint - theta * cost ) / theta**3
      
      end if

      c2n = 0.0D+00
      do i = 1, n, 2
        c2n = c2n + ftab(i) * cos ( t * x(i) )
      end do
      c2n = c2n - 0.5D+00 * ( ftab(n) * cos ( t * x(n) ) 
     &                      + ftab(1) * cos ( t * x(1) ) )

      c2nm1 = 0.0D+00
      do i = 2, n - 1, 2
        c2nm1 = c2nm1 + ftab(i) * cos ( t * x(i) )
      end do
     
      value = h * ( 
     &    alpha * ( ftab(n) * sin ( t * x(n) )  
     &            - ftab(1) * sin ( t * x(1) ) ) 
     &  + beta * c2n 
     &  + gamma * c2nm1 )

      return
      end
      subroutine filon_fun_sin ( n, f, a, b, t, value )

c*********************************************************************72
c
cc FILON_FUN_SIN uses Filon's method on integrals with a sine factor.
c
c  Discussion:
c
c    The integral to be approximated has the form
c
c      Integral ( A .le. X .le. B ) F(X) * SIN(T*X) dX
c
c    where T is user specified.
c
c    The function is interpolated over each subinterval by
c    a parabolic arc.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 February 2006
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
c    Stephen Chase, Lloyd Fosdick,
c    An Algorithm for Filon Quadrature,
c    Communications of the Association for Computing Machinery,
c    Volume 12, Number 8, August 1969, pages 453-457.
c
c    Stephen Chase, Lloyd Fosdick,
c    Algorithm 353:
c    Filon Quadrature,
c    Communications of the Association for Computing Machinery,
c    Volume 12, Number 8, August 1969, pages 457-458.
c
c    Philip Davis, Philip Rabinowitz,
c    Methods of Numerical Integration,
c    Second Edition,
c    Dover, 2007,
c    ISBN: 0486453391,
c    LC: QA299.3.D28.
c
c  Parameters:
c
c    Input, integer N, the number of data points, 
c    including the endpoints.  N must be odd, and greater than 1.
c
c    Input, external F, the subroutine which evaluates the integrand,
c    of the form subroutine F ( N, X, FX ).
c
c    Input, double precision A, B, the limits of integration.
c
c    Input, double precision T, multiplier of the X argument of the sine.
c
c    Output, double precision VALUE, the approximate value of the integral.
c
      implicit none

      integer n

      double precision a
      double precision alpha
      double precision b
      double precision beta
      double precision cost
      external f
      double precision ftab(n)
      double precision gamma
      double precision h
      integer i
      double precision s2n
      double precision s2nm1
      double precision sint
      double precision t
      double precision theta
      double precision value
      double precision x(n)

      if ( a .eq. b ) then
        value = 0.0D+00
        return
      end if
     
      if ( n .le. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'FILON_FUN_SIN - Fatal error!'
        write ( *, '(a)' ) '  N < 2'
        write ( *, '(a,i8)' ) '  N = ',n
        stop 1
      end if
     
      if ( mod ( n, 2 ) .ne. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'FILON_FUN_SIN - Fatal error!'
        write ( *, '(a)' ) '  N must be odd.'
        write ( *, '(a,i8)' ) '  N = ',n
        stop 1
      end if
c
c  Set the X values.
c
      do i = 1, n
        x(i) = ( dble ( n - i     ) * a   
     &         + dble (     i - 1 ) * b ) 
     &         / dble ( n     - 1 )
      end do

      h = ( b - a ) / dble ( n - 1 )
      theta = t * h

      sint = sin ( theta )
      cost = cos ( theta )

      if ( 6.0D+00 * abs ( theta ) .le. 1.0D+00 ) then

        alpha = 2.0D+00 * theta**3 /   45.0D+00 
     &        - 2.0D+00 * theta**5 /  315.0D+00 
     &        + 2.0D+00 * theta**7 / 4725.0D+00
      
        beta =  2.0D+00            /     3.0D+00 
     &        + 2.0D+00 * theta**2 /    15.0D+00 
     &        - 4.0D+00 * theta**4 /   105.0D+00 
     &        + 2.0D+00 * theta**6 /   567.0D+00 
     &        - 4.0D+00 * theta**8 / 22275.0D+00

        gamma = 4.0D+00            /      3.0D+00 
     &        - 2.0D+00 * theta**2 /     15.0D+00 
     &        +           theta**4 /    210.0D+00 
     &        -           theta**6 /  11340.0D+00

      else
     
        alpha = ( theta**2 + theta * sint * cost 
     &    - 2.0D+00 * sint**2 ) / theta**3

        beta = ( 2.0D+00 * theta + 2.0D+00 * theta * cost**2 
     &    - 4.0D+00 * sint * cost ) / theta**3

        gamma = 4.0D+00 * ( sint - theta * cost ) / theta**3
     
      end if
c
c  Tabulate the function.
c
      call f ( n, x, ftab )

      s2n = 0.0D+00
      do i = 1, n, 2
        s2n = s2n + ftab(i) * sin ( t * x(i) )
      end do
      s2n = s2n - 0.5D+00 * ( ftab(n) * sin ( t * x(n) ) 
     &                      + ftab(1) * sin ( t * x(1) ) )

      s2nm1 = 0.0D+00
      do i = 2, n - 1, 2
        s2nm1 = s2nm1 + ftab(i) * sin ( t * x(i) )
      end do

      value = h * ( 
     &    alpha * ( ftab(1) * cos ( t * x(1) ) 
     &            - ftab(n) * cos ( t * x(n) ) ) 
     &  + beta * s2n 
     &  + gamma * s2nm1 )
     
      return
      end
      subroutine filon_tab_sin ( n, ftab, a, b, t, value )

c*********************************************************************72
c
cc FILON_TAB_SIN uses Filon's method on integrals with a sine factor.
c
c  Discussion:
c
c    The integral to be approximated has the form
c
c      Integral ( A .le. X .le. B ) F(X) * SIN(T*X) dX
c
c    where T is user specified.
c
c    The function is interpolated over each subinterval by
c    a parabolic arc.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 February 2006
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
c    Stephen Chase, Lloyd Fosdick,
c    An Algorithm for Filon Quadrature,
c    Communications of the Association for Computing Machinery,
c    Volume 12, Number 8, August 1969, pages 453-457.
c
c    Stephen Chase, Lloyd Fosdick,
c    Algorithm 353:
c    Filon Quadrature,
c    Communications of the Association for Computing Machinery,
c    Volume 12, Number 8, August 1969, pages 457-458.
c
c    Philip Davis, Philip Rabinowitz,
c    Methods of Numerical Integration,
c    Second Edition,
c    Dover, 2007,
c    ISBN: 0486453391,
c    LC: QA299.3.D28.
c
c  Parameters:
c
c    Input, integer N, the number of data points, 
c    including the endpoints.  N must be odd, and greater than 1.
c
c    Input, double precision FTAB(N), contains the value of the function
c    at A, A+H, A+2*H, ... , B-H, B, where H = (B-A)/(N-1).
c
c    Input, double precision A, B, the limits of integration.
c
c    Input, double precision T, multiplier of the X argument of the sine.
c
c    Output, double precision VALUE, the approximate value of the integral.
c
      implicit none

      integer n

      double precision a
      double precision alpha
      double precision b
      double precision beta
      double precision cost
      double precision ftab(n)
      double precision gamma
      double precision h
      integer i
      double precision s2n
      double precision s2nm1
      double precision sint
      double precision t
      double precision theta
      double precision value
      double precision x(n)

      if ( a .eq. b ) then
        value = 0.0D+00
        return
      end if
     
      if ( n .le. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'FILON_TAB_SIN - Fatal error!'
        write ( *, '(a)' ) '  N < 2'
        write ( *, '(a,i8)' ) '  N = ',n
        stop 1
      end if
     
      if ( mod ( n, 2 ) .ne. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'FILON_TAB_SIN - Fatal error!'
        write ( *, '(a)' ) '  N must be odd.'
        write ( *, '(a,i8)' ) '  N = ',n
        stop 1
      end if
c
c  Set the X values.
c
      do i = 1, n
        x(i) = ( dble ( n - i     ) * a   
     &         + dble (     i - 1 ) * b ) 
     &         / dble ( n     - 1 )
      end do

      h = ( b - a ) / dble ( n - 1 )
      theta = t * h

      sint = sin ( theta )
      cost = cos ( theta )

      if ( 6.0D+00 * abs ( theta ) .le. 1.0D+00 ) then

        alpha = 2.0D+00 * theta**3 /   45.0D+00 
     &        - 2.0D+00 * theta**5 /  315.0D+00 
     &        + 2.0D+00 * theta**7 / 4725.0D+00
      
        beta =  2.0D+00            /     3.0D+00 
     &        + 2.0D+00 * theta**2 /    15.0D+00 
     &        - 4.0D+00 * theta**4 /   105.0D+00 
     &        + 2.0D+00 * theta**6 /   567.0D+00 
     &        - 4.0D+00 * theta**8 / 22275.0D+00

        gamma = 4.0D+00            /      3.0D+00 
     &        - 2.0D+00 * theta**2 /     15.0D+00 
     &        +           theta**4 /    210.0D+00 
     &        -           theta**6 /  11340.0D+00

      else
     
        alpha = ( theta**2 + theta * sint * cost 
     &    - 2.0D+00 * sint**2 ) / theta**3

        beta = ( 2.0D+00 * theta + 2.0D+00 * theta * cost**2 
     &    - 4.0D+00 * sint * cost ) / theta**3

        gamma = 4.0D+00 * ( sint - theta * cost ) / theta**3
     
      end if
      
      s2n = 0.0D+00
      do i = 1, n, 2
        s2n = s2n + ftab(i) * sin ( t * x(i) )
      end do
      s2n = s2n - 0.5D+00 * ( ftab(n) * sin ( t * x(n) ) 
     &                      + ftab(1) * sin ( t * x(1) ) )

      s2nm1 = 0.0D+00
      do i = 2, n - 1, 2
        s2nm1 = s2nm1 + ftab(i) * sin ( t * x(i) )
      end do

      value = h * ( 
     &    alpha * ( ftab(1) * cos ( t * x(1) ) 
     &            - ftab(n) * cos ( t * x(n) ) ) 
     &  + beta * s2n 
     &  + gamma * s2nm1 )
     
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
