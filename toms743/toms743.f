      function bisect ( xx, nb, ner, l )

c*********************************************************************72
c
cc BISECT approximates the W function using bisection.
c
c  Discussion:
c
c    The parameter TOL, which determines the accuracy of the bisection
c    method, is calculated using NBITS (assuming the final bit is lost
c    due to rounding error).
c
c    N0 is the maximum number of iterations used in the bisection
c    method.
c
c    For XX close to 0 for Wp, the exponential approximation is used.
c    The approximation is exact to O(XX^8) so, depending on the value
c    of NBITS, the range of application of this formula varies. Outside
c    this range, the usual bisection method is used.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 June 2014
c
c  Author:
c
c    Original FORTRAN77 version by Andrew Barry, S. J. Barry, 
c    Patricia Culligan-Hensley.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Andrew Barry, S. J. Barry, Patricia Culligan-Hensley,
c    Algorithm 743: WAPR - A Fortran routine for calculating real 
c    values of the W-function,
c    ACM Transactions on Mathematical Software,
c    Volume 21, Number 2, June 1995, pages 172-181.
c
c  Parameters:
c
c    Input, double precision XX, the argument.
c
c    Input, integer NB, indicates the branch of the W function.
c    0, the upper branch;
c    nonzero, the lower branch.
c
c    Output, integer NER, the error flag.
c    0, success;
c    1, the routine did not converge.  Perhaps reduce NBITS and try again.
c
c    Input, integer L, the offset indicator.
c    1, XX represents the offset of the argument from -exp(-1).
c    not 1, XX is the actual argument.
c
c    Output, double precision BISECT, the value of W(X), as determined
c    by bisection.
c
      implicit none

      double precision bisect
      double precision crude
      double precision d
      double precision f
      double precision fd
      integer i
      integer l
      integer n0
      parameter ( n0 = 500 )
      integer nb
      integer nbits
      integer ner
      double precision r
      double precision test
      double precision tol
      double precision u
      double precision x
      double precision xx

      save nbits

      data nbits / 0 /

      bisect = 0.0D+00
      ner = 0

      if ( nbits == 0 ) then
        call nbits_compute ( nbits )
      end if

      if ( l .eq. 1 ) then
        x = xx - dexp ( -1.0D+00 )
      else
        x = xx
      end if

      if ( nb .eq. 0 ) then

        test = 1.0D+00 / ( 2.0D+00 ** nbits ) ** ( 1.0D+00 / 7.0D+00 )

        if ( dabs ( x ) .lt. test ) then

          bisect = x 
     &      * dexp ( - x
     &      * dexp ( - x 
     &      * dexp ( - x 
     &      * dexp ( - x
     &      * dexp ( - x
     &      * dexp ( - x ))))))

          return

        else

          u = crude ( x, nb ) + 1.0D-03
          tol = dabs ( u ) / 2.0D+00 ** nbits
          d = max ( u - 2.0D-03, -1.0D+00 )

          do i = 1, n0

            r = 0.5D+00 * ( u - d )
            bisect = d + r
c
c  Find root using w*exp(w)-x to avoid ln(0) error.
c
            if ( x .lt. dexp ( 1.0D+00 ) ) then

              f = bisect * dexp ( bisect ) - x
              fd = d * dexp ( d ) - x
c
c  Find root using ln(w/x)+w to avoid overflow error.
c
            else

              f = dlog ( bisect / x ) + bisect
              fd = dlog ( d / x ) + d

            end if

            if ( f .eq. 0.0D+00 ) then
              return
            end if

            if ( dabs ( r ) .le. tol ) then
              return
            end if

            if ( 0.0D+00 .lt. fd * f ) then
              d = bisect
            else
              u = bisect
            end if

          end do

        end if

      else

        d = crude ( x, nb ) - 1.0D-03
        u = min ( d + 2.0D-03, -1.0D+00 )
        tol = dabs ( u ) / 2.0D+00 ** nbits

        do i = 1, n0

          r = 0.5D+00 * ( u - d )
          bisect = d + r
          f = bisect * dexp ( bisect ) - x

          if ( f .eq. 0.0D+00 ) then
            return
          end if

          if ( dabs ( r ) .le. tol ) then
            return
          end if

          fd = d * dexp ( d ) - x

          if ( 0.0D+00 .lt. fd * f ) then
            d = bisect
          else
            u = bisect
          end if

        end do

      end if
c
c  The iteration did not converge.
c
      ner = 1

      return
      end
      function crude ( xx, nb )

c*********************************************************************72
c
cc CRUDE returns a crude approximation for the W function.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 June 2014
c
c  Author:
c
c    Original FORTRAN77 version by Andrew Barry, S. J. Barry, 
c    Patricia Culligan-Hensley.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Andrew Barry, S. J. Barry, Patricia Culligan-Hensley,
c    Algorithm 743: WAPR - A Fortran routine for calculating real 
c    values of the W-function,
c    ACM Transactions on Mathematical Software,
c    Volume 21, Number 2, June 1995, pages 172-181.
c
c  Parameters:
c
c    Input, double precision XX, the argument.
c
c    Input, integer NB, indicates the desired branch of the W function.
c    * 0, the upper branch;
c    * nonzero, the lower branch.
c
c    Output, double precision CRUDE, the crude approximation to W at XX.
c
      implicit none

      double precision an2
      double precision c13
      double precision crude
      double precision em
      double precision em2
      double precision em9
      double precision eta
      integer init
      integer nb
      double precision reta
      double precision s2
      double precision s21
      double precision s22
      double precision s23
      double precision t
      double precision ts
      double precision xx
      double precision zl

      save c13
      save em
      save em2
      save em9
      save init
      save s2
      save s21
      save s22
      save s23

      data init / 0 /

      crude = 0.0D+00
c
c  Various mathematical constants.
c
      if ( init .eq. 0 ) then
        init = 1
        em = - dexp ( -1.0D+00 )
        em9 = - dexp ( -9.0D+00 )
        c13 = 1.0D+00 / 3.0D+00
        em2 = 2.0D+00 / em
        s2 = dsqrt ( 2.0D+00 )
        s21 = 2.0D+00 * s2 - 3.0D+00
        s22 = 4.0D+00 - 3.0D+00 * s2
        s23 = s2 - 2.0D+00
      end if
c
c  Crude Wp.
c
      if ( nb .eq. 0 ) then

        if ( xx .le. 20.0D+00 ) then
          reta = s2 * dsqrt ( 1.0D+00 - xx / em )
          an2 = 4.612634277343749D+00 * dsqrt ( dsqrt ( reta +
     &      1.09556884765625D+00 ) )
          crude = reta / ( 1.0D+00 + reta / ( 3.0D+00 
     &      + ( s21 * an2 + s22 ) * reta / ( s23 * ( an2 + reta )))) 
     &      - 1.0D+00
        else
          zl = dlog ( xx )
          crude = dlog ( xx / dlog ( xx 
     &      / zl ** dexp ( -1.124491989777808D+00 /
     &      ( 0.4225028202459761D+00 + zl ))))
        end if

      else
c
c  Crude Wm.
c
        if ( xx .le. em9 ) then
          zl = dlog ( -xx )
          t = -1.0D+00 - zl
          ts = dsqrt ( t )
          crude = zl - ( 2.0D+00 * ts ) / ( s2 + ( c13 - t 
     &      / ( 270.0D+00 + ts * 127.0471381349219D+00 ) ) * ts )
        else
          zl = dlog ( -xx )
          eta = 2.0D+00 - em2 * xx
          crude = dlog ( xx / dlog ( - xx / ( ( 1.0D+00
     &      - 0.5043921323068457D+00 * ( zl + 1.0D+00 ) ) 
     &      * ( dsqrt ( eta ) + eta / 3.0D+00 ) + 1.0D+00 ) ) )
         end if

      end if

      return
      end
      subroutine nbits_compute ( nbits )

c*********************************************************************72
c
cc NBITS_COMPUTE computes the mantissa length minus one.
c
c  Discussion:
c
c    NBITS is the number of bits (less 1) in the mantissa of the
c    floating point number number representation of your machine.
c    It is used to determine the level of accuracy to which the W
c    function should be calculated.
c
c    Most machines use a 24-bit matissa for single precision and
c    53-56 bits for double precision. The IEEE standard is 53
c    bits. The Fujitsu VP2200 uses 56 bits. Long word length
c    machines vary, e.g., the Cray X/MP has a 48-bit mantissa for
c    single precision.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 June 2014
c
c  Author:
c
c    Original FORTRAN77 version by Andrew Barry, S. J. Barry, 
c    Patricia Culligan-Hensley.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Andrew Barry, S. J. Barry, Patricia Culligan-Hensley,
c    Algorithm 743: WAPR - A Fortran routine for calculating real 
c    values of the W-function,
c    ACM Transactions on Mathematical Software,
c    Volume 21, Number 2, June 1995, pages 172-181.
c
c  Parameters:
c
c    Output, integer NBITS, the mantissa length, in bits, minus one.
c
      implicit none

      double precision b
      integer i
      integer nbits
      double precision v

      nbits = 0

      b = 1.0D+00

10    continue

        b = b / 2.0D+00
        v = b + 1.0D+00

        if ( v .eq. 1.0D+00 ) then
          go to 20
        end if

        nbits = nbits + 1

      go to 10

20    continue

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
      function wapr ( x, nb, nerror, l )

c*********************************************************************72
c
cc WAPR approximates the W function.
c
c  Discussion:
c
c    The call will fail if the input value X is out of range.
c    The range requirement for the upper branch is:
c      -exp(-1) <= X.
c    The range requirement for the lower branch is:
c      -exp(-1) < X < 0.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 June 2014
c
c  Author:
c
c    Original FORTRAN77 version by Andrew Barry, S. J. Barry, 
c    Patricia Culligan-Hensley.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Andrew Barry, S. J. Barry, Patricia Culligan-Hensley,
c    Algorithm 743: WAPR - A Fortran routine for calculating real 
c    values of the W-function,
c    ACM Transactions on Mathematical Software,
c    Volume 21, Number 2, June 1995, pages 172-181.
c
c  Parameters:
c
c    Input, double precision X, the argument.
c
c    Input, integer NB, indicates the desired branch of the W function.
c    * 0, the upper branch;
c    * nonzero, the lower branch.
c
c    Output, integer NERROR, the error flag.
c    * 0, successful call.
c    * 1, failure, the input X is out of range.
c
c    Input, integer L, indicates the interpretation of X.
c    * 1, X is actually the offset from -(exp-1), so compute W(X-exp(-1)).
c    * not 1, X is the argument; compute W(X);
c
c    Output, double precision WAPR, the approximate value of W(X).
c
      implicit none

      double precision an2
      double precision an3
      double precision an4
      double precision an5
      double precision an6
      double precision c13
      double precision c23
      double precision d12
      double precision delx
      double precision em
      double precision em2
      double precision em9
      double precision eta
      integer i
      integer init
      integer l
      integer m
      integer nb
      integer nbits
      integer nerror
      integer niter
      double precision reta
      double precision s2
      double precision s21
      double precision s22
      double precision s23
      double precision t
      double precision tb
      double precision tb2
      double precision temp
      double precision temp2
      double precision ts
      double precision wapr
      double precision x
      double precision x0
      double precision x1
      double precision xx
      double precision zl
      double precision zn

      save an3
      save an4
      save an5
      save an6
      save c13
      save c23
      save d12
      save em
      save em2
      save em9
      save init
      save nbits
      save niter
      save s2
      save s21
      save s22
      save s23
      save tb
      save tb2
      save x0
      save x1

      data init / 0 /
      data niter / 1 /

      wapr = 0.0D+00
      nerror = 0

      if ( init .eq. 0 ) then

        init = 1

        call nbits_compute ( nbits )

        if ( 56 .le. nbits ) then
          niter = 2
        end if
c
c  Various mathematical constants.
c
        em = -dexp ( -1.0D+00 )
        em9 = -dexp ( -9.0D+00 )
        c13 = 1.0D+00 / 3.0D+00
        c23 = 2.0D+00 * c13
        em2 = 2.0D+00 / em
        d12 = -em2
        tb = 0.5D+00 ** nbits
        tb2 = dsqrt ( tb )
        x0 = tb ** ( 1.0D+00 / 6.0D+00 ) * 0.5D+00
        x1 = ( 1.0D+00 - 17.0D+00 * tb ** ( 2.0D+00 / 7.0D+00 ) ) * em
        an3 = 8.0D+00 / 3.0D+00
        an4 = 135.0D+00 / 83.0D+00
        an5 = 166.0D+00 / 39.0D+00
        an6 = 3167.0D+00 / 3549.0D+00
        s2 = dsqrt ( 2.0D+00 )
        s21 = 2.0D+00 * s2 - 3.0D+00
        s22 = 4.0D+00 - 3.0D+00 * s2
        s23 = s2 - 2.0D+00

      end if

      if ( l .eq. 1 ) then

        delx = x

        if ( delx .lt. 0.0D+00 ) then
          nerror = 1
          write ( *, '(a)' ) ''
          write ( *, '(a)' ) 'WAPR - Fatal error!'
          write ( *, '(a)' ) '  The offset X is negative.'
          write ( *, '(a)' ) '  It must be nonnegative.'
          stop 1
        end if

        xx = x + em

      else

        if ( x .lt. em ) then
          nerror = 1
          return
        else if ( x .eq. em ) then
          wapr = -1.0D+00
          return
        end if

        xx = x
        delx = xx - em

      end if

      if ( nb .eq. 0 ) then
c
c  Calculations for Wp.
c
        if ( dabs ( xx ) .le. x0 ) then
          wapr = xx / ( 1.0D+00 + xx / ( 1.0D+00 + xx 
     &      / ( 2.0D+00 + xx / ( 0.6D+00 + 0.34D+00 * xx ))))
          return
        else if ( xx .le. x1 ) then
          reta = dsqrt ( d12 * delx )
          wapr = reta / ( 1.0D+00 + reta / ( 3.0D+00 + reta / ( reta
     &      / ( an4 + reta / ( reta * an6 + an5 ) ) + an3 ) ) ) 
     &      - 1.0D+00
          return
        else if ( xx .le. 20.0D+00 ) then
          reta = s2 * dsqrt ( 1.0D+00 - xx / em )
          an2 = 4.612634277343749D+00 * dsqrt ( dsqrt ( reta +
     &      1.09556884765625D+00 ))
          wapr = reta / ( 1.0D+00 + reta / ( 3.0D+00 + ( s21 * an2 
     &      + s22 ) * reta / ( s23 * ( an2 + reta )))) - 1.0D+00
        else
          zl = dlog ( xx )
          wapr = dlog ( xx / dlog ( xx 
     &      / zl ** dexp ( -1.124491989777808D+00 /
     &      ( 0.4225028202459761D+00 + zl ))))
        end if
c
c  Calculations for Wm.
c
      else

        if ( 0.0D+00 .le. xx ) then
          nerror = 1
          return
        else if ( xx .le. x1 ) then
          reta = dsqrt ( d12 * delx )
          wapr = reta / ( reta / ( 3.0D+00 + reta / ( reta / ( an4 
     &      + reta / ( reta * an6 - an5 ) ) - an3 ) ) - 1.0D+00 ) 
     &      - 1.0D+00
          return
        else if ( xx .le. em9 ) then
          zl = dlog ( -xx )
          t = -1.0D+00 - zl
          ts = dsqrt ( t )
          wapr = zl - ( 2.0D+00 * ts ) / ( s2 + ( c13 - t 
     &      / ( 270.0D+00 + ts * 127.0471381349219D+00 )) * ts )
        else
          zl = dlog ( -xx )
          eta = 2.0D+00 - em2 * xx
          wapr = dlog ( xx / dlog ( -xx / ( ( 1.0D+00 
     &      - 0.5043921323068457D+00 * ( zl + 1.0D+00 ) ) 
     &      * ( dsqrt ( eta ) + eta / 3.0D+00 ) + 1.0D+00 )))
        end if

      end if

      do i = 1, niter
        zn = dlog ( xx / wapr ) - wapr
        temp = 1.0D+00 + wapr
        temp2 = temp + c23 * zn
        temp2 = 2.0D+00 * temp * temp2
        wapr = wapr * ( 1.0D+00 + ( zn / temp ) * ( temp2 - zn ) 
     &    / ( temp2 - 2.0D+00 * zn ) )
      end do

      return
      end

