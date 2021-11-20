      program main

c*********************************************************************72
c
cc MAIN is the main program for TOMS418_PRB.
c
c  Discussion:
c
c    TOMS418_PRB tests the TOMS418 library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 January 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TOMS418_PRB:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the TOMS418 library.'

      call test01 ( )
      call test02 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TOMS418_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 tests FSPL2 with integrands of the form F(X)*COS(W*X).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 May 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision eps
      double precision error
      double precision exact
      external f1
      double precision f1_p
      double precision f1_pp
      external f2
      double precision f2_p
      double precision f2_pp
      external f3
      double precision f3_p
      double precision f3_pp
      double precision fba
      double precision fbb
      double precision fpa
      double precision fpb
      integer i
      integer j
      integer k
      integer lc
      integer ls
      integer maxh
      double precision r8_pi
      double precision s
      double precision w

      save r8_pi

      data r8_pi / 3.141592653589793D+00 /

      a = 0.0D+00
      b = 2.0D+00 * r8_pi

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  Use FSPL2 to estimate the integrals'
      write ( *, '(a)' ) '    integral ( A<=X<=B) F(X) * COS(W*X) dX'
      write ( *, '(a)' ) '  Use integrands:'
      write ( *, '(a)' ) '    F(X) = 1, X, X*X.'
      write ( *, '(a,g24.16)' ) '  A = ', a
      write ( *, '(a,g24.16)' ) '  B = ', b
      write ( *, '(a)' ) '  Use several values of W.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,a)' )
     &  '             W                      Approximate',
     &  '             Exact'
      write ( *, '(a)' ) ' '

      maxh = 12
      lc = 1
      ls = -1

      do k = 1, 3

        if ( k .eq. 1 ) then
          w = 1.0D+00
        else if ( k .eq. 2 ) then
          w = 2.0D+00
        else if ( k .eq. 3 ) then
          w = 10.0D+00
        end if

        do i = 1, 3

          eps = 0.00001D+00

          if ( i .eq. 1 )then

            fpa = f1_p ( a )
            fpb = f1_p ( b )
            fba = f1_pp ( a )
            fbb = f1_pp ( b )

            call fspl2 ( f1, a, b, fpa, fpb, fba, fbb, w, eps, maxh,
     &        lc, ls, c, s )

            exact = ( sin ( w * b ) - sin ( w * a ) ) / w

            if ( eps < 0.0D+00 ) then
              write ( *, '(a)' ) '  Next result did not converge:'
            end if

            write ( *, '(a,3g24.16)' ) '    1:  ', w, c, exact

          else if ( i .eq. 2 ) then

            fpa = f2_p ( a )
            fpb = f2_p ( b )
            fba = f2_pp ( a )
            fbb = f2_pp ( b )

            call fspl2 ( f2, a, b, fpa, fpb, fba, fbb, w, eps, maxh,
     &        lc, ls, c, s )

            exact = ( ( cos ( w * b ) + w * b * sin ( w * b ) )
     &               - ( cos ( w * a ) + w * a * sin ( w * a ) ) )
     &               / w**2

            if ( eps < 0.0D+00 ) then
              write ( *, '(a)' ) '  Next result did not converge:'
            end if

            write ( *, '(a,3g24.16)' ) '    X:  ', w, c, exact

          else if ( i .eq. 3 ) then

            fpa = f3_p ( a )
            fpb = f3_p ( b )
            fba = f3_pp ( a )
            fbb = f3_pp ( b )

            call fspl2 ( f3, a, b, fpa, fpb, fba, fbb, w, eps, maxh,
     &        lc, ls, c, s )

            exact = ( ( 2.0D+00 * w * b * cos ( w * b )
     &            + ( w * w * b * b - 2.0D+00 ) * sin ( w * b ) )
     &              - ( 2.0D+00 * w * a * cos ( w * a )
     &            + ( w * w * a * a - 2.0D+00 ) * sin ( w * a ) ) )
     &            / w**3

            if ( eps < 0.0D+00 ) then
              write ( *, '(a)' ) '  Next result did not converge:'
            end if

            write ( *, '(a,3g24.16)' ) '  X*X:  ', w, c, exact

          end if

        end do

        write ( *, '(a)' ) ' '

      end do

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 tests FSPL2 with integrands of the form F(X)*SIN(W*X).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 January 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision b
      double precision c
      double precision eps
      double precision error
      double precision exact
      external f1
      double precision f1_p
      double precision f1_pp
      external f2
      double precision f2_p
      double precision f2_pp
      external f3
      double precision f3_p
      double precision f3_pp
      double precision fba
      double precision fbb
      double precision fpa
      double precision fpb
      integer i
      integer j
      integer k
      integer lc
      integer ls
      integer maxh
      double precision r8_pi
      double precision s
      double precision w

      save r8_pi

      data r8_pi / 3.141592653589793D+00 /

      a = 0.0D+00
      b = 2.0D+00 * r8_pi

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) '  Use FSPL2 to estimate the integrals'
      write ( *, '(a)' ) '    integral ( A<=X<=B) F(X) * SIN(W*X) dX'
      write ( *, '(a)' ) '  Use integrands:'
      write ( *, '(a)' ) '    F(X) = 1, X, X*X.'
      write ( *, '(a,g24.16)' ) '  A = ', a
      write ( *, '(a,g24.16)' ) '  B = ', b
      write ( *, '(a)' ) '  Use several values of W.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,a)' )
     &  '             W                      Approximate',
     &  '             Exact'
      write ( *, '(a)' ) ' '

      maxh = 12
      lc = -1
      ls = 1

      do k = 1, 3

        if ( k .eq. 1 ) then
          w = 1.0D+00
        else if ( k .eq. 2 ) then
          w = 2.0D+00
        else if ( k .eq. 3 ) then
          w = 10.0D+00
        end if

        do i = 1, 3

          eps = 0.00001D+00

          if ( i .eq. 1 )then

            fpa = f1_p ( a )
            fpb = f1_p ( b )
            fba = f1_pp ( a )
            fbb = f1_pp ( b )

            call fspl2 ( f1, a, b, fpa, fpb, fba, fbb, w, eps, maxh,
     &        lc, ls, c, s )

            exact = ( - cos ( w * b ) + cos ( w * a ) ) / w

            if ( eps < 0.0D+00 ) then
              write ( *, '(a)' ) '  Next result did not converge:'
            end if

            write ( *, '(a,3g24.16)' ) '    1:  ', w, s, exact

          else if ( i .eq. 2 ) then

            fpa = f2_p ( a )
            fpb = f2_p ( b )
            fba = f2_pp ( a )
            fbb = f2_pp ( b )

            call fspl2 ( f2, a, b, fpa, fpb, fba, fbb, w, eps, maxh,
     &        lc, ls, c, s )

            exact = ( ( sin ( w * b ) - w * b * cos ( w * b ) )
     &              - ( sin ( w * a ) - w * a * cos ( w * a ) ) )
     &               / w / w

            if ( eps < 0.0D+00 ) then
              write ( *, '(a)' ) '  Next result did not converge:'
            end if

            write ( *, '(a,3g24.16)' ) '    X:  ', w, s, exact

          else if ( i .eq. 3 ) then

            fpa = f3_p ( a )
            fpb = f3_p ( b )
            fba = f3_pp ( a )
            fbb = f3_pp ( b )

            call fspl2 ( f3, a, b, fpa, fpb, fba, fbb, w, eps, maxh,
     &        lc, ls, c, s )

            exact = ( ( 2.0D+00 * w * b * sin ( w * b )
     &            + ( 2.0D+00 - w * w * b * b ) * cos ( w * b ) )
     &            - ( 2.0D+00 * w * a * sin ( w * a )
     &            + ( 2.0D+00 - w * w * a * a ) * cos ( w * a ) ) )
     &            / w**3

            if ( eps < 0.0D+00 ) then
              write ( *, '(a)' ) '  Next result did not converge:'
            end if

            write ( *, '(a,3g24.16)' ) '  X*X:  ', w, s, exact

          end if

        end do

        write ( *, '(a)' ) ' '

      end do

      return
      end
      function f1 ( x )

c*********************************************************************72
c
cc F1 evaluates the integrand factor F(X) = 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 January 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision f1
      double precision x

      f1 = 1.0D+00

      return
      end
      function f1_p ( x )

c*********************************************************************72
c
cc F1_P evaluates the first derivative of the integrand factor F(X) = 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 January 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision f1_p
      double precision x

      f1_p = 0.0D+00

      return
      end
      function f1_pp ( x )

c*********************************************************************72
c
cc F1_PP evaluates the second derivative of the integrand factor F(X) = 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 January 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision f1_pp
      double precision x

      f1_pp = 0.0D+00

      return
      end
      function f2 ( x )

c*********************************************************************72
c
cc F2 evaluates the integrand factor F(X) = X.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 January 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision f2
      double precision x

      f2 = x

      return
      end
      function f2_p ( x )

c*********************************************************************72
c
cc F2_P evaluates the first derivative of the integrand factor F(X) = X.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 January 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision f2_p
      double precision x

      f2_p = 1.0D+00

      return
      end
      function f2_pp ( x )

c*********************************************************************72
c
cc F2_PP evaluates the second derivative of the integrand factor F(X) = X.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 January 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision f2_pp
      double precision x

      f2_pp = 0.0D+00

      return
      end
      function f3 ( x )

c*********************************************************************72
c
cc F3 evaluates the integrand factor F(X) = X*X.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 January 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision f3
      double precision x

      f3 = x*x

      return
      end
      function f3_p ( x )

c*********************************************************************72
c
cc F3_P evaluates the first derivative of the integrand factor F(X) = X*X.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 January 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision f3_p
      double precision x

      f3_p = x

      return
      end
      function f3_pp ( x )

c*********************************************************************72
c
cc F3_PP evaluates the second derivative of the integrand factor F(X) = X*X.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 January 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision f3_pp
      double precision x

      f3_pp = 1.0D+00

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
