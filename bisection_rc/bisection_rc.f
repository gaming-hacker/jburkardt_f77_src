      subroutine bisection_rc ( a, b, x, fx, job )

c*********************************************************************72
c
cc BISECTION_RC seeks a zero of f(x) in a change of sign interval.
c
c  Discussion:
c
c    The bisection method is used.
c
c    This routine uses reverse communication, so that the function is always
c    evaluated in the calling program.
c
c    On the first call, the user sets JOB = 0, and the values of A and B.
c    Thereafter, the user checks the returned value of JOB and follows 
c    directions.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input/output, double precision A, B, the endpoints of the change of 
c    sign interval.  These values will change from call to call as the
c    interval size is reduced.
c
c    Output, double precision X, a point at which the function is to 
c    be evaluated.
c
c    Input, double precision FX, the function value at X.
c
c    Input/output, integer JOB, a communication flag.
c    The user sets JOB to 0 before the first call.  Thereafter, the program
c    controls setting the value of JOB, whose output values mean:
c    
      implicit none

      double precision a
      double precision b
      double precision fa
      save fa
      double precision fb
      save fb
      double precision fx
      integer job
      double precision r8_sign
      integer state
      save state
      double precision x

      if ( job .eq. 0 ) then

        fa = 0.0D+00
        fb = 0.0D+00
        state = 1
        x = a
        job = 1

      else if ( state .eq. 1 ) then

        fa = fx
        x = b
        state = 2

      else if ( state .eq. 2 ) then

        fb = fx

        if ( r8_sign ( fa ) .eq. r8_sign ( fb ) ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'BISECTION_RC - Fatal error!'
          write ( *, '(a)' ) '  F(A) and F(B) have the same sign.'
          stop
        end if

        x = ( a + b ) / 2.0D+00
        state = 3

      else

        if ( r8_sign ( fx ) .eq. r8_sign ( fa ) ) then
          a = x
          fa = fx
        else
          b = x
          fb = fx
        end if
        x = ( a + b ) / 2.0D+00
        state = 3

      end if

      return
      end
      function r8_sign ( x )

c*********************************************************************72
c
cc R8_SIGN returns the sign of an R8.
c
c  Discussion:
c
c    value = -1 if X < 0;
c    value = +1 if X => 0.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X, the number whose sign is desired.
c
c    Output, double precision R8_SIGN, the sign of X.
c
      implicit none

      double precision r8_sign
      double precision x

      if ( x .lt. 0.0D+00 ) then
        r8_sign = -1.0D+00
      else
        r8_sign = +1.0D+00
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
