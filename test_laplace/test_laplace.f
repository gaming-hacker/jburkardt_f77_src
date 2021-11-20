      function besj0 ( x )

c*********************************************************************72
c
cc BESJ0 evaluates the Bessel J0(X) function.
c
c  Discussion:
c
c    This routine computes approximate values for Bessel functions
c    of the first kind of order zero for arguments  |X| <= XMAX
c
c    See comments heading CALJY0.
c
c  Modified:
c
c    03 April 2007
c
c  Author:
c
c    William Cody
c
c  Parameters:
c
c    Input, double precision X, the argument of the function.
c
c    Output, double precision BESJ0, the value of the function.
c
      implicit none

      double precision besj0
      integer jint
      double precision result
      double precision x

      jint = 0
      call caljy0 ( x, result, jint )
      besj0 = result

      return
      end
      function c8_argument ( x )

c*********************************************************************72
c
cc C8_ARGUMENT returns the argument of a C8.
c
c  Discussion:
c
c    A C8 is a double complex value.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    30 November 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double complex X, the value whose argument is desired.
c
c    Output, double precision C8_ARGUMENT, the argument of X.
c
      implicit none

      double precision c8_argument
      double complex   x
      double precision xi
      double precision xr

      xr = dreal ( x )
      xi = dimag ( x )

      if ( xi .eq. 0.0D+00 .and. xr .eq. 0.0D+00 ) then

        c8_argument = 0.0D+00

      else

        c8_argument = datan2 ( xi, xr )

      end if

      return
      end
      function c8_atan ( z )

c*********************************************************************72
c
cc C8_ATAN evaluates the inverse tangent of a C8.
c
c  Discussion:
c
c    FORTRAN77 does not have an intrinsic inverse tangent function
c    for C8 arguments.
c
c    FORTRAN77 does not have a logarithm function for C8 arguments!
c
c    Here we use the relationship:
c
c      C8_ATAN ( Z ) = ( i / 2 ) * log ( ( 1 - i * z ) / ( 1 + i * z ) )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    30 November 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double complex Z, the argument.
c
c    Output, double complex C8_ATAN, the function value.
c
      implicit none

      double complex arg
      double complex c8_atan
      double complex c8_log
      double complex i
      double complex z

      i = dcmplx ( 0.0D+00, 1.0D+00 )

      arg = ( 1.0D+00 - i * z ) / ( 1.0D+00 + i * z )

      c8_atan = 0.5D+00 * i * c8_log ( arg ) 

      return
      end
      function c8_log ( z )

c*********************************************************************72
c
cc C8_LOG evaluates the logarithm of a C8.
c
c  Discussion:
c
c    FORTRAN77 does not have a logarithm function for C8 arguments!
c
c    Here we use the relationship:
c
c      C8_LOG ( Z ) = LOG ( R ) + i * ARG ( R )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    30 November 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double complex Z, the argument.
c
c    Output, double complex C8_LOG, the function value.
c
      implicit none

      double precision arg
      double precision c8_argument
      double complex   c8_log
      double precision c8_magnitude
      double complex   i
      double precision r
      double complex   z

      i = dcmplx ( 0.0D+00, 1.0D+00 )

      arg = c8_argument ( arg )
      r = c8_magnitude ( arg )

      c8_log = dlog ( r ) + i * arg
 
      return
      end
      function c8_magnitude ( x )

c*****************************************************************************80
c
cc C8_MAGNITUDE returns the magnitude of a C8.
c
c  Discussion:
c
c    A C8 is a double complex value.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    30 November 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double complex X, the value whose magnitude is desired.
c
c    Output, double precision C8_MAGNITUDE, the magnitude of X.
c
      implicit none

      double precision c8_magnitude
      double complex   x
      double precision xi
      double precision xr

      xr = dreal ( x )
      xi = dimag ( x )

      c8_magnitude = dsqrt ( xr * xr + xi * xi )

      return
      end
      subroutine caljy0 ( arg, result, jint )

c*********************************************************************72
c
cc CALJY0 computes various J0 and Y0 Bessel functions.
c
c  Discussion:
c
c    This routine computes zero-order Bessel functions of the first and
c    second kind (J0 and Y0), for real arguments X, where 0 < X <= XMAX
c    for Y0, and |X| <= XMAX for J0.  
c
c  Modified:
c
c    03 April 2007
c
c  Author:
c
c    William Cody
c
c  Reference:
c
c    John Hart, Ward Cheney, Charles Lawson, Hans Maehly, 
c    Charles Mesztenyi, John Rice, Henry Thatcher, 
c    Christoph Witzgall,
c    Computer Approximations,
c    Wiley, 1968,
c    LC: QA297.C64.
c
c  Parameters:
c
c    Input, double precision ARG, the argument.  If JINT = 0, ARG
c    must satisfy 
c     -XMAX < ARG < XMAX;
c    If JINT = 1, then ARG must satisfy
c      0 < ARG < XMAX.
c
c    Output, double precision RESULT, the value of the function,
c    which depends on the input value of JINT:
c    0, RESULT = J0(x);
c    1, RESULT = Y0(x);
c
c    Input, integer JINT, chooses the function to be computed.
c    0, J0(x);
c    1, Y0(x);
c
      implicit none

      integer i
      integer jint
      double precision arg
      double precision ax
      double precision cons
      double precision down
      double precision eight
      double precision five5
      double precision four
      double precision one
      double precision oneov8
      double precision pi2
      double precision pj0(7)
      double precision pj1(8)
      double precision plg(4)
      double precision prod
      double precision py0(6)
      double precision py1(7)
      double precision py2(8)
      double precision p0(6)
      double precision p1(6)
      double precision p17
      double precision qj0(5)
      double precision qj1(7)
      double precision qlg(4)
      double precision qy0(5)
      double precision qy1(6)
      double precision qy2(7)
      double precision q0(5)
      double precision q1(5)
      double precision resj
      double precision result
      double precision r0
      double precision r1
      double precision sixty4
      double precision three
      double precision twopi
      double precision twopi1
      double precision twopi2
      double precision two56
      double precision up
      double precision w
      double precision wsq
      double precision xden
      double precision xinf
      double precision xmax
      double precision xnum
      double precision xsmall
      double precision xj0
      double precision xj1
      double precision xj01
      double precision xj02
      double precision xj11
      double precision xj12
      double precision xy
      double precision xy0
      double precision xy01
      double precision xy02
      double precision xy1
      double precision xy11
      double precision xy12
      double precision xy2
      double precision xy21
      double precision xy22
      double precision z
      double precision zero
      double precision zsq
c
c  Mathematical constants
c  CONS = ln(.5) + Euler's gamma
c
      data zero / 0.0d0 /
      data one /1.0d0 /
      data three /3.0d0 /
      data four /4.0d0 /
      data eight /8.0d0/
      data five5 / 5.5d0 /
      data sixty4 /64.0d0 /
      data oneov8 /0.125d0 /
      data p17 /1.716d-1/
      data two56 /256.0d0/
      data cons / -1.1593151565841244881d-1/
      data pi2 /6.3661977236758134308d-1/
      data twopi /6.2831853071795864769d0/
      data twopi1 /6.28125d0 /
      data twopi2 / 1.9353071795864769253d-3/
c
c  Machine-dependent constants
c
      data xmax /1.07d+09/
      data xsmall /9.31d-10/
      data xinf /1.7d+38/
c
c  Zeroes of Bessel functions
c
      data xj0 /2.4048255576957727686d+0/
      data xj1 /5.5200781102863106496d+0/
      data xy0 /8.9357696627916752158d-1/
      data xy1 /3.9576784193148578684d+0/
      data xy2 /7.0860510603017726976d+0/
      data xj01 / 616.0d+0/
      data xj02 /-1.4244423042272313784d-03/
      data xj11 /1413.0d+0/
      data xj12 / 5.4686028631064959660d-04/
      data xy01 / 228.0d+0/
      data xy02 / 2.9519662791675215849d-03/
      data xy11 /1013.0d+0/
      data xy12 / 6.4716931485786837568d-04/
      data xy21 /1814.0d+0/
      data xy22 / 1.1356030177269762362d-04/
c
c  Coefficients for rational approximation to ln(x/a)
c
      data plg/-2.4562334077563243311d+01,2.3642701335621505212d+02,
     &         -5.4989956895857911039d+02,3.5687548468071500413d+02/
      data qlg/-3.5553900764052419184d+01,1.9400230218539473193d+02,
     &         -3.3442903192607538956d+02,1.7843774234035750207d+02/
c
c  Coefficients for rational approximation of
c  J0(X) / (X**2 - XJ0**2),  XSMALL < |X| <= 4.0
c
      data pj0/6.6302997904833794242d+06,-6.2140700423540120665d+08,
     &         2.7282507878605942706d+10,-4.1298668500990866786d+11,
     &        -1.2117036164593528341d-01, 1.0344222815443188943d+02,
     &        -3.6629814655107086448d+04/
      data qj0/4.5612696224219938200d+05, 1.3985097372263433271d+08,
     &         2.6328198300859648632d+10, 2.3883787996332290397d+12,
     &         9.3614022392337710626d+02/
c
c  Coefficients for rational approximation of
c  J0(X) / (X**2 - XJ1**2), 4.0 < |X| <= 8.0
c
      data pj1/4.4176707025325087628d+03, 1.1725046279757103576d+04,
     &         1.0341910641583726701d+04,-7.2879702464464618998d+03,
     &        -1.2254078161378989535d+04,-1.8319397969392084011d+03,
     &         4.8591703355916499363d+01, 7.4321196680624245801d+02/
      data qj1/3.3307310774649071172d+02,-2.9458766545509337327d+03,
     &         1.8680990008359188352d+04,-8.4055062591169562211d+04,
     &         2.4599102262586308984d+05,-3.5783478026152301072d+05,
     &        -2.5258076240801555057d+01/
c
c  Coefficients for rational approximation of
c  (Y0(X) - 2 LN(X/XY0) J0(X)) / (X**2 - XY0**2),
c  XSMALL < |X| <= 3.0
c
      data py0/1.0102532948020907590d+04,-2.1287548474401797963d+06,
     &         2.0422274357376619816d+08,-8.3716255451260504098d+09,
     &         1.0723538782003176831d+11,-1.8402381979244993524d+01/
      data qy0/6.6475986689240190091d+02, 2.3889393209447253406d+05,
     &         5.5662956624278251596d+07, 8.1617187777290363573d+09,
     &         5.8873865738997033405d+11/
c
c  Coefficients for rational approximation of
c  (Y0(X) - 2 LN(X/XY1) J0(X)) / (X**2 - XY1**2),
c  3.0 < |X| <= 5.5
c
      data py1/-1.4566865832663635920d+04, 4.6905288611678631510d+06,
     &         -6.9590439394619619534d+08, 4.3600098638603061642d+10,
     &         -5.5107435206722644429d+11,-2.2213976967566192242d+13,
     &          1.7427031242901594547d+01/
      data qy1/ 8.3030857612070288823d+02, 4.0669982352539552018d+05,
     &          1.3960202770986831075d+08, 3.4015103849971240096d+10,
     &          5.4266824419412347550d+12, 4.3386146580707264428d+14/
c
c  Coefficients for rational approximation of
c  (Y0(X) - 2 LN(X/XY2) J0(X)) / (X**2 - XY2**2),
c  5.5 < |X| <= 8.0
c
      data py2/ 2.1363534169313901632d+04,-1.0085539923498211426d+07,
     &          2.1958827170518100757d+09,-1.9363051266772083678d+11,
     &         -1.2829912364088687306d+11, 6.7016641869173237784d+14,
     &         -8.0728726905150210443d+15,-1.7439661319197499338d+01/
      data qy2/ 8.7903362168128450017d+02, 5.3924739209768057030d+05,
     &          2.4727219475672302327d+08, 8.6926121104209825246d+10,
     &          2.2598377924042897629d+13, 3.9272425569640309819d+15,
     &          3.4563724628846457519d+17/
c
c  Coefficients for Hart,s approximation, 8.0 < |X|.
c
      data p0/3.4806486443249270347d+03, 2.1170523380864944322d+04,
     &        4.1345386639580765797d+04, 2.2779090197304684302d+04,
     &        8.8961548424210455236d-01, 1.5376201909008354296d+02/
      data q0/3.5028735138235608207d+03, 2.1215350561880115730d+04,
     &        4.1370412495510416640d+04, 2.2779090197304684318d+04,
     &        1.5711159858080893649d+02/
      data p1/-2.2300261666214198472d+01,-1.1183429920482737611d+02,
     &        -1.8591953644342993800d+02,-8.9226600200800094098d+01,
     &        -8.8033303048680751817d-03,-1.2441026745835638459d+00/
      data q1/1.4887231232283756582d+03, 7.2642780169211018836d+03,
     &        1.1951131543434613647d+04, 5.7105024128512061905d+03,
     &        9.0593769594993125859d+01/
c
c  Check for error conditions.
c
      ax = abs ( arg )

      if ( jint .eq. 1 .and. arg .le. zero ) then
        result = -xinf
        return
      else if ( xmax .lt. ax ) then
        result = zero
        return
      end if

      if ( eight .lt. ax ) then
        go to 800
      end if

      if ( ax .le. xsmall ) then
        if ( jint .eq. 0 ) then
          result = one
        else
          result = pi2 * ( log ( ax ) + cons )
        end if
        return
      end if
c
c  Calculate J0 for appropriate interval, preserving
c  accuracy near the zero of J0.
c
      zsq = ax * ax

      if ( ax .le. four ) then
        xnum = ( pj0(5) * zsq + pj0(6) ) * zsq + pj0(7)
        xden = zsq + qj0(5)
        do i = 1, 4
          xnum = xnum * zsq + pj0(i)
          xden = xden * zsq + qj0(i)
        end do
        prod = ( ( ax - xj01 / two56 ) - xj02 ) * ( ax + xj0 )
      else
        wsq = one - zsq / sixty4
        xnum = pj1(7) * wsq + pj1(8)
        xden = wsq + qj1(7)
        do i = 1, 6
          xnum = xnum * wsq + pj1(i)
          xden = xden * wsq + qj1(i)
        end do
        prod = ( ax + xj1 ) * ( ( ax - xj11 / two56 ) - xj12 )
      end if

      result = prod * xnum / xden

      if ( jint .eq. 0 ) then
        return
      end if
c
c  Calculate Y0.  First find  RESJ = pi/2 ln(x/xn) J0(x),
c  where xn is a zero of Y0.
c
      if ( ax .le. three ) then
        up = ( ax - xy01 / two56 ) - xy02
        xy = xy0
      else if ( ax .le. five5 ) then
        up = ( ax - xy11 / two56 ) - xy12
        xy = xy1
      else
        up = ( ax - xy21 / two56 ) - xy22
        xy = xy2
      end if

      down = ax + xy

      if ( abs ( up ) .lt. p17 * down ) then
        w = up / down
        wsq = w * w
        xnum = plg(1)
        xden = wsq + qlg(1)
        do i = 2, 4
          xnum = xnum * wsq + plg(i)
          xden = xden * wsq + qlg(i)
        end do
        resj = pi2 * result * w * xnum / xden
      else
        resj = pi2 * result * log ( ax / xy )
      end if
c
c  Now calculate Y0 for appropriate interval, preserving
c  accuracy near the zero of Y0.
c
      if ( ax .le. three ) then
        xnum = py0(6) * zsq + py0(1)
        xden = zsq + qy0(1)
        do i = 2, 5
          xnum = xnum * zsq + py0(i)
          xden = xden * zsq + qy0(i)
        end do
      else if ( ax .le. five5 ) then
        xnum = py1(7) * zsq + py1(1)
        xden = zsq + qy1(1)
        do i = 2, 6
          xnum = xnum * zsq + py1(i)
          xden = xden * zsq + qy1(i)
        end do
      else
        xnum = py2(8) * zsq + py2(1)
        xden = zsq + qy2(1)
        do i = 2, 7
          xnum = xnum * zsq + py2(i)
          xden = xden * zsq + qy2(i)
        end do
      end if

      result = resj + up * down * xnum / xden

      return
c
c  Calculate J0 or Y0 for 8.0 < |ARG|.
c
  800 continue

      z = eight / ax
      w = ax / twopi
      w = aint ( w ) + oneov8
      w = ( ax - w * twopi1 ) - w * twopi2
      zsq = z * z
      xnum = p0(5) * zsq + p0(6)
      xden = zsq + q0(5)
      up = p1(5) * zsq + p1(6)
      down = zsq + q1(5)

      do i = 1, 4
        xnum = xnum * zsq + p0(i)
        xden = xden * zsq + q0(i)
        up = up * zsq + p1(i)
        down = down * zsq + q1(i)
      end do

      r0 = xnum / xden
      r1 = up / down

      if ( jint .eq. 0 ) then
        result = sqrt ( pi2 / ax ) 
     &    * ( r0 * cos ( w ) - z * r1 * sin ( w ) )
      else
        result = sqrt ( pi2 / ax ) 
     &    * ( r0 * sin ( w ) + z * r1 * cos ( w ) )
      end if

      return
      end
      subroutine p00_problem_num ( problem_num )

c*********************************************************************72
c
cc P00_PROBLEM_NUM returns the number of problems available.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, integer PROBLEM_NUM, the number of problems.
c
      implicit none

      integer problem_num
 
      problem_num = 16

      return
      end
      subroutine p00_f_backward ( problem, z, fz )

c*********************************************************************72
c
cc P00_F_BACKWARD evaluates the backward function.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 November 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer PROBLEM, the problem index.
c
c    Input, double complex S, the evaluation point.
c
c    Output, double complex FS, the value of the function.
c
      implicit none

      double complex fz
      integer problem
      double complex z

      if (  problem .eq. 1 ) then
        call p01_f_backward ( z, fz )
      else if ( problem .eq. 2 ) then
        call p02_f_backward ( z, fz )
      else if ( problem .eq. 3 ) then
        call p03_f_backward ( z, fz )
      else if ( problem .eq. 4 ) then
        call p04_f_backward ( z, fz )
      else if ( problem .eq. 5 ) then
        call p05_f_backward ( z, fz )
      else if ( problem .eq. 6 ) then
        call p06_f_backward ( z, fz )
      else if ( problem .eq. 7 ) then
        call p07_f_backward ( z, fz )
      else if ( problem .eq. 8 ) then
        call p08_f_backward ( z, fz )
      else if ( problem .eq. 9 ) then
        call p09_f_backward ( z, fz )
      else if ( problem .eq. 10 ) then
        call p10_f_backward ( z, fz )
      else if ( problem .eq. 11 ) then
        call p11_f_backward ( z, fz )
      else if ( problem .eq. 12 ) then
        call p12_f_backward ( z, fz )
      else if ( problem .eq. 13 ) then
        call p13_f_backward ( z, fz )
      else if ( problem .eq. 14 ) then
        call p14_f_backward ( z, fz )
      else if ( problem .eq. 15 ) then
        call p15_f_backward ( z, fz )
      else if ( problem .eq. 16 ) then
        call p16_f_backward ( z, fz )
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'P00_F_BACKWARD - Fatal error!'
        write ( *, '(a,i8)' ) '  Unrecognized problem index = ', problem
        stop
      end if

      return
      end
      subroutine p00_f_forward ( problem, t, ft )

c*********************************************************************72
c
cc P00_F_FORWARD evaluates the forward function.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 November 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer PROBLEM, the problem index.
c
c    Input, double precision T, the evaluation point.
c
c    Output, double precision FT, the value of the function.
c
      implicit none

      double precision ft
      integer problem
      double precision t

      if (  problem .eq. 1 ) then
        call p01_f_forward ( t, ft )
      else if ( problem .eq. 2 ) then
        call p02_f_forward ( t, ft )
      else if ( problem .eq. 3 ) then
        call p03_f_forward ( t, ft )
      else if ( problem .eq. 4 ) then
        call p04_f_forward ( t, ft )
      else if ( problem .eq. 5 ) then
        call p05_f_forward ( t, ft )
      else if ( problem .eq. 6 ) then
        call p06_f_forward ( t, ft )
      else if ( problem .eq. 7 ) then
        call p07_f_forward ( t, ft )
      else if ( problem .eq. 8 ) then
        call p08_f_forward ( t, ft )
      else if ( problem .eq. 9 ) then
        call p09_f_forward ( t, ft )
      else if ( problem .eq. 10 ) then
        call p10_f_forward ( t, ft )
      else if ( problem .eq. 11 ) then
        call p11_f_forward ( t, ft )
      else if ( problem .eq. 12 ) then
        call p12_f_forward ( t, ft )
      else if ( problem .eq. 13 ) then
        call p13_f_forward ( t, ft )
      else if ( problem .eq. 14 ) then
        call p14_f_forward ( t, ft )
      else if ( problem .eq. 15 ) then
        call p15_f_forward ( t, ft )
      else if ( problem .eq. 16 ) then
        call p16_f_forward ( t, ft )
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'P00_F_FORWARD - Fatal error!'
        write ( *, '(a,i8)' ) '  Unrecognized problem index = ', problem
        stop
      end if

      return
      end
      subroutine p00_title_backward ( problem, title_backward )

c*********************************************************************72
c
cc P00_TITLE_BACKWARD sets the title for the backward transform of any problem.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer PROBLEM, the problem index.
c
c    Output, character*(*) TITLE_BACKWARD, the title of the backward transform
c    for the problem.
c
      implicit none

      integer problem
      character*(*) title_backward

      if (  problem .eq. 1 ) then
        call p01_title_backward ( title_backward )
      else if ( problem .eq. 2 ) then
        call p02_title_backward ( title_backward )
      else if ( problem .eq. 3 ) then
        call p03_title_backward ( title_backward )
      else if ( problem .eq. 4 ) then
        call p04_title_backward ( title_backward )
      else if ( problem .eq. 5 ) then
        call p05_title_backward ( title_backward )
      else if ( problem .eq. 6 ) then
        call p06_title_backward ( title_backward )
      else if ( problem .eq. 7 ) then
        call p07_title_backward ( title_backward )
      else if ( problem .eq. 8 ) then
        call p08_title_backward ( title_backward )
      else if ( problem .eq. 9 ) then
        call p09_title_backward ( title_backward )
      else if ( problem .eq. 10 ) then
        call p10_title_backward ( title_backward )
      else if ( problem .eq. 11 ) then
        call p11_title_backward ( title_backward )
      else if ( problem .eq. 12 ) then
        call p12_title_backward ( title_backward )
      else if ( problem .eq. 13 ) then
        call p13_title_backward ( title_backward )
      else if ( problem .eq. 14 ) then
        call p14_title_backward ( title_backward )
      else if ( problem .eq. 15 ) then
        call p15_title_backward ( title_backward )
      else if ( problem .eq. 16 ) then
        call p16_title_backward ( title_backward )
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'P00_TITLE_BACKWARD - Fatal error!'
        write ( *, '(a,i8)' ) '  Unrecognized problem index = ', problem
        stop
      end if

      return
      end
      subroutine p00_title_forward ( problem, title_forward )

c*********************************************************************72
c
cc P00_TITLE_FORWARD sets the title for the forward transform of any problem.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer PROBLEM, the problem index.
c
c    Output, character*(*) TITLE_FORWARD, the title of the forward transform
c    for the problem.
c
      implicit none

      integer problem
      character*(*) title_forward

      if (  problem .eq. 1 ) then
        call p01_title_forward ( title_forward )
      else if ( problem .eq. 2 ) then
        call p02_title_forward ( title_forward )
      else if ( problem .eq. 3 ) then
        call p03_title_forward ( title_forward )
      else if ( problem .eq. 4 ) then
        call p04_title_forward ( title_forward )
      else if ( problem .eq. 5 ) then
        call p05_title_forward ( title_forward )
      else if ( problem .eq. 6 ) then
        call p06_title_forward ( title_forward )
      else if ( problem .eq. 7 ) then
        call p07_title_forward ( title_forward )
      else if ( problem .eq. 8 ) then
        call p08_title_forward ( title_forward )
      else if ( problem .eq. 9 ) then
        call p09_title_forward ( title_forward )
      else if ( problem .eq. 10 ) then
        call p10_title_forward ( title_forward )
      else if ( problem .eq. 11 ) then
        call p11_title_forward ( title_forward )
      else if ( problem .eq. 12 ) then
        call p12_title_forward ( title_forward )
      else if ( problem .eq. 13 ) then
        call p13_title_forward ( title_forward )
      else if ( problem .eq. 14 ) then
        call p14_title_forward ( title_forward )
      else if ( problem .eq. 15 ) then
        call p15_title_forward ( title_forward )
      else if ( problem .eq. 16 ) then
        call p16_title_forward ( title_forward )
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'P00_TITLE_FORWARD - Fatal error!'
        write ( *, '(a,i8)' ) '  Unrecognized problem index = ', problem
        stop
      end if

      return
      end
      subroutine p01_f_backward ( z, fz )

c*********************************************************************72
c
cc P01_F_BACKWARD evaluates the backward function for problem 01.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 November 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double complex Z, the evaluation point.
c
c    Output, double complex FZ, the value of the function.
c
      implicit none

      double complex fz
      double complex z

      fz = 1.0D+00 / z

      return
      end
      subroutine p01_f_forward ( t, ft )

c*********************************************************************72
c
cc P01_F_FORWARD evaluates the forward function for problem 01.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 November 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision T, the evaluation point.
c
c    Output, double precision FT, the value of the function.
c
      implicit none

      double precision ft
      double precision t

      ft = 1.0D+00

      return
      end
      subroutine p01_title_backward ( title_backward )

c*********************************************************************72
c
cc P01_TITLE_BACKWARD sets the title for the backward transform for problem 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, character*(*) TITLE_BACKWARD, the title of the backward transform
c    for the problem.
c
      implicit none

      character*(*) title_backward

      title_backward = 'G(Z) = 1/z'

      return
      end
      subroutine p01_title_forward ( title_forward )

c*********************************************************************72
c
cc P01_TITLE_FORWARD sets the title for the forward transform for problem 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, character*(*) TITLE_FORWARD, the title of the forward transform
c    for the problem.
c
      implicit none

      character*(*) title_forward

      title_forward = 'F(T) = 1'

      return
      end
      subroutine p02_f_backward ( z, fz )

c*********************************************************************72
c
cc P02_F_BACKWARD evaluates the backward function for problem 02.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 November 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double complex Z, the evaluation point.
c
c    Output, double complex FZ, the value of the function.
c
      implicit none

      double complex fz
      double complex z

      fz = 1.0D+00 / z / z

      return
      end
      subroutine p02_f_forward ( t, ft )

c*********************************************************************72
c
cc P02_F_FORWARD evaluates the forward function for problem 02.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 November 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision T, the evaluation point.
c
c    Output, double precision FT, the value of the function.
c
      implicit none

      double precision ft
      double precision t

      ft = t

      return
      end
      subroutine p02_title_backward ( title_backward )

c*********************************************************************72
c
cc P02_TITLE_BACKWARD sets the title for the backward transform for problem 2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, character*(*) TITLE_BACKWARD, the title of the backward transform
c    for the problem.
c
      implicit none

      character*(*) title_backward

      title_backward = 'G(Z) = 1/z**2'

      return
      end
      subroutine p02_title_forward ( title_forward )

c*********************************************************************72
c
cc P02_TITLE_FORWARD sets the title for the forward transform for problem 2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, character*(*) TITLE_FORWARD, the title of the forward transform
c    for the problem.
c
      implicit none

      character*(*) title_forward

      title_forward = 'F(T) = t' 

      return
      end
      subroutine p03_f_backward ( z, fz )

c*********************************************************************72
c
cc P03_F_BACKWARD evaluates the backward function for problem 03.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 November 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double complex Z, the evaluation point.
c
c    Output, double complex FZ, the value of the function.
c
      implicit none

      double complex fz
      double complex z

      fz = 1.0D+00 / z / ( z + 1.0D+00 )

      return
      end
      subroutine p03_f_forward ( t, ft )

c*********************************************************************72
c
cc P03_F_FORWARD evaluates the forward function for problem 03.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 November 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision T, the evaluation point.
c
c    Output, double precision FT, the value of the function.
c
      implicit none

      double precision ft
      double precision t

      ft = sin ( t )

      return
      end
      subroutine p03_title_backward ( title_backward )

c*********************************************************************72
c
cc P03_TITLE_BACKWARD sets the title for the backward transform for problem 3.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, character*(*) TITLE_BACKWARD, the title of the backward transform
c    for the problem.
c
      implicit none

      character*(*) title_backward

      title_backward = 'G(Z) = 1/(z*z+1)'

      return
      end
      subroutine p03_title_forward ( title_forward )

c*********************************************************************72
c
cc P03_TITLE_FORWARD sets the title for the forward transform for problem 3.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, character*(*) TITLE_FORWARD, the title of the forward transform
c    for the problem.
c
      implicit none

      character*(*) title_forward

      title_forward = 'F(T) = sin(t)'

      return
      end
      subroutine p04_f_backward ( z, fz )

c*********************************************************************72
c
cc P04_F_BACKWARD evaluates the backward function for problem 04.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 November 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double complex Z, the evaluation point.
c
c    Output, double complex FZ, the value of the function.
c
      implicit none

      double complex fz
      double complex z

      fz = 1.0D+00 / ( z + 1.0D+00 ) / ( z + 1.0D+00 )

      return
      end
      subroutine p04_f_forward ( t, ft )

c*********************************************************************72
c
cc P04_F_FORWARD evaluates the forward function for problem 04.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 November 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision T, the evaluation point.
c
c    Output, double precision FT, the value of the function.
c
      implicit none

      double precision ft
      double precision t

      ft = t / exp ( t )

      return
      end
      subroutine p04_title_backward ( title_backward )

c*********************************************************************72
c
cc P04_TITLE_BACKWARD sets the title for the backward transform for problem 4.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, character*(*) TITLE_BACKWARD, the title of the backward transform
c    for the problem.
c
      implicit none

      character*(*) title_backward

      title_backward = 'G(Z) = 1/(1+z)**2'

      return
      end
      subroutine p04_title_forward ( title_forward )

c*********************************************************************72
c
cc P04_TITLE_FORWARD sets the title for the forward transform for problem 4.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, character*(*) TITLE_FORWARD, the title of the forward transform
c    for the problem.
c
      implicit none

      character*(*) title_forward

      title_forward = 'F(T) = t*exp(-t)'

      return
      end
      subroutine p05_f_backward ( z, fz )

c*********************************************************************72
c
cc P05_F_BACKWARD evaluates the backward function for problem 05.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 November 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double complex Z, the evaluation point.
c
c    Output, double complex FZ, the value of the function.
c
      implicit none

      double complex fz
      double complex z

      fz = 1.0D+00 / sqrt ( z )

      return
      end
      subroutine p05_f_forward ( t, ft )

c*********************************************************************72
c
cc P05_F_FORWARD evaluates the forward function for problem 05.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 November 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision T, the evaluation point.
c
c    Output, double precision FT, the value of the function.
c
      implicit none

      double precision ft
      double precision, parameter :: pi = 3.141592653589793D+00
      double precision t

      ft = 1.0D+00 / sqrt ( pi * t )

      return
      end
      subroutine p05_title_backward ( title_backward )

c*********************************************************************72
c
cc P05_TITLE_BACKWARD sets the title for the backward transform for problem 5.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, character*(*) TITLE_BACKWARD, the title of the backward transform
c    for the problem.
c
      implicit none

      character*(*) title_backward

      title_backward = 'G(Z) = 1/sqrt(z)'

      return
      end
      subroutine p05_title_forward ( title_forward )

c*********************************************************************72
c
cc P05_TITLE_FORWARD sets the title for the forward transform for problem 5.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, character*(*) TITLE_FORWARD, the title of the forward transform
c    for the problem.
c
      implicit none

      character*(*) title_forward

      title_forward = 'F(T) = 1/sqrt(pi*t)'

      return
      end
      subroutine p06_f_backward ( z, fz )

c*********************************************************************72
c
cc P06_F_BACKWARD evaluates the backward function for problem 06.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 November 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double complex Z, the evaluation point.
c
c    Output, double complex FZ, the value of the function.
c
      implicit none

      double complex fz
      double complex z

      fz = 1.0D+00 / z / exp ( 5.0D+00 * z )

      return
      end
      subroutine p06_f_forward ( t, ft )

c*********************************************************************72
c
cc P06_F_FORWARD evaluates the forward function for problem 06.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 November 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision T, the evaluation point.
c
c    Output, double precision FT, the value of the function.
c
      implicit none

      double precision ft
      double precision t

      if ( t .lt. 5.0D+00 ) then
        ft = 0.0D+00
      else
        ft = 1.0D+00
      end if

      return
      end
      subroutine p06_title_backward ( title_backward )

c*********************************************************************72
c
cc P06_TITLE_BACKWARD sets the title for the backward transform for problem 6.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, character*(*) TITLE_BACKWARD, the title of the backward transform
c    for the problem.
c
      implicit none

      character*(*) title_backward

      title_backward = 'G(Z) = 1/(z*exp(5*z))'

      return
      end
      subroutine p06_title_forward ( title_forward )

c*********************************************************************72
c
cc P06_TITLE_FORWARD sets the title for the forward transform for problem 6.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, character*(*) TITLE_FORWARD, the title of the forward transform
c    for the problem.
c
      implicit none

      character*(*) title_forward

      title_forward = 'F(T) = theta(t-5)'

      return
      end
      subroutine p07_f_backward ( z, fz )

c*********************************************************************72
c
cc P07_F_BACKWARD evaluates the backward function for problem 07.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 November 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double complex Z, the evaluation point.
c
c    Output, double complex FZ, the value of the function.
c
      implicit none

      double complex fz
      double complex z

      fz = log ( z ) / z

      return
      end
      subroutine p07_f_forward ( t, ft )

c*********************************************************************72
c
cc P07_F_FORWARD evaluates the forward function for problem 07.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 November 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision T, the evaluation point.
c
c    Output, double precision FT, the value of the function.
c
      implicit none

      double precision, parameter :: 
     &  euler_constant = 0.57721566490153286061D+00
      double precision ft
      double precision t

      ft = - euler_constant - log ( t )

      return
      end
      subroutine p07_title_backward ( title_backward )

c*********************************************************************72
c
cc P07_TITLE_BACKWARD sets the title for the backward transform for problem 7.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, character*(*) TITLE_BACKWARD, the title of the backward transform
c    for the problem.
c
      implicit none

      character*(*) title_backward

      title_backward = 'G(Z) = log(z)/z'

      return
      end
      subroutine p07_title_forward ( title_forward )

c*********************************************************************72
c
cc P07_TITLE_FORWARD sets the title for the forward transform for problem 7.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, character*(*) TITLE_FORWARD, the title of the forward transform
c    for the problem.
c
      implicit none

      character*(*) title_forward

      title_forward = 'F(T) = -gamma-log(t)'

      return
      end
      subroutine p08_f_backward ( z, fz )

c*********************************************************************72
c
cc P08_F_BACKWARD evaluates the backward function for problem 08.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 November 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double complex Z, the evaluation point.
c
c    Output, double complex FZ, the value of the function.
c
      implicit none

      double complex fz
      double complex z

      fz = 1.0D+00 / z / ( 1.0D+00 + exp ( - z ) )

      return
      end
      subroutine p08_f_forward ( t, ft )

c*********************************************************************72
c
cc P08_F_FORWARD evaluates the forward function for problem 08.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 November 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision T, the evaluation point.
c
c    Output, double precision FT, the value of the function.
c
      implicit none

      double precision ft
      double precision t

      if ( mod ( int ( t ), 2 ) .eq. 1 ) then
        ft = 0.0D+00
      else
        ft = 1.0D+00
      end if

      return
      end
      subroutine p08_title_backward ( title_backward )

c*********************************************************************72
c
cc P08_TITLE_BACKWARD sets the title for the backward transform for problem 8.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, character*(*) TITLE_BACKWARD, the title of the backward transform
c    for the problem.
c
      implicit none

      character*(*) title_backward

      title_backward = 'G(Z) = 1/(z*(1+exp(-z)))'

      return
      end
      subroutine p08_title_forward ( title_forward )

c*********************************************************************72
c
cc P08_TITLE_FORWARD sets the title for the forward transform for problem 8.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, character*(*) TITLE_FORWARD, the title of the forward transform
c    for the problem.
c
      implicit none

      character*(*) title_forward

      title_forward = 'F(T) = square wave'

      return
      end
      subroutine p09_f_backward ( z, fz )

c*********************************************************************72
c
cc P09_F_BACKWARD evaluates the backward function for problem 09.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 November 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double complex Z, the evaluation point.
c
c    Output, double complex FZ, the value of the function.
c
      implicit none

      double complex fz
      double complex z

      fz = ( z * z - 1.0D+00 ) / ( z * z + 1.0D+00 ) 
     &  / ( z * z + 1.0D+00 )

      return
      end
      subroutine p09_f_forward ( t, ft )

c*********************************************************************72
c
cc P09_F_FORWARD evaluates the forward function for problem 09.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 November 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision T, the evaluation point.
c
c    Output, double precision FT, the value of the function.
c
      implicit none

      double precision ft
      double precision t

      ft = t * cos ( t )

      return
      end
      subroutine p09_title_backward ( title_backward )

c*********************************************************************72
c
cc P09_TITLE_BACKWARD sets the title for the backward transform for problem 9.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, character*(*) TITLE_BACKWARD, the title of the backward transform
c    for the problem.
c
      implicit none

      character*(*) title_backward

      title_backward = 'G(Z) = (z*z-1)/(z*z+1)**2'

      return
      end
      subroutine p09_title_forward ( title_forward )

c*********************************************************************72
c
cc P09_TITLE_FORWARD sets the title for the forward transform for problem 9.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, character*(*) TITLE_FORWARD, the title of the forward transform
c    for the problem.
c
      implicit none

      character*(*) title_forward

      title_forward = 'F(T) = t*cos(t)'

      return
      end
      subroutine p10_f_backward ( z, fz )

c*********************************************************************72
c
cc P10_F_BACKWARD evaluates the backward function for problem 10.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 November 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double complex Z, the evaluation point.
c
c    Output, double complex FZ, the value of the function.
c
      implicit none

      double complex fz
      double complex z

      fz = sqrt ( z + 0.5D+00) - sqrt ( z + 0.25D+00 )

      return
      end
      subroutine p10_f_forward ( t, ft )

c*********************************************************************72
c
cc P10_F_FORWARD evaluates the forward function for problem 10.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 November 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision T, the evaluation point.
c
c    Output, double precision FT, the value of the function.
c
      implicit none

      double precision ft
      double precision, parameter :: pi = 3.141592653589793D+00
      double precision t

      ft = ( exp ( - 0.25D+00 * t ) - exp ( - 0.5D+00 * t ) )
     &  / sqrt ( 4.0D+00 * pi * t * t * t )

      return
      end
      subroutine p10_title_backward ( title_backward )

c*********************************************************************72
c
cc P10_TITLE_BACKWARD sets the title for the backward transform for problem 10.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, character*(*) TITLE_BACKWARD, the title of the backward transform
c    for the problem.
c
      implicit none

      character*(*) title_backward

      title_backward ='G(Z) = sqrt(z+1/2)-sqrt(z+1/4)'

      return
      end
      subroutine p10_title_forward ( title_forward )

c*********************************************************************72
c
cc P10_TITLE_FORWARD sets the title for the forward transform for problem 10.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, character*(*) TITLE_FORWARD, the title of the forward transform
c    for the problem.
c
      implicit none

      character*(*) title_forward

      title_forward = 'F(T) = (exp(-t/4)-exp(-t/2))/sqrt(4*pi*t**3)'

      return
      end
      subroutine p11_f_backward ( z, fz )

c*********************************************************************72
c
cc P11_F_BACKWARD evaluates the backward function for problem 11.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 November 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double complex Z, the evaluation point.
c
c    Output, double complex FZ, the value of the function.
c
      implicit none

      double complex fz
      double complex z

      fz = exp ( - 4.0D+00 * sqrt ( z ) )

      return
      end
      subroutine p11_f_forward ( t, ft )

c*********************************************************************72
c
cc P11_F_FORWARD evaluates the forward function for problem 11.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 November 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision T, the evaluation point.
c
c    Output, double precision FT, the value of the function.
c
      implicit none

      double precision ft
      double precision, parameter :: pi = 3.141592653589793D+00
      double precision t

      ft = 2.0D+00 * exp ( - 4.0D+00 / t ) / sqrt ( pi * t * t * t )

      return
      end
      subroutine p11_title_backward ( title_backward )

c*********************************************************************72
c
cc P11_TITLE_BACKWARD sets the title for the backward transform for problem 11.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, character*(*) TITLE_BACKWARD, the title of the backward transform
c    for the problem.
c
      implicit none

      character*(*) title_backward

      title_backward = 'G(Z) = exp(-4*sqrt(z))'

      return
      end
      subroutine p11_title_forward ( title_forward )

c*********************************************************************72
c
cc P11_TITLE_FORWARD sets the title for the forward transform for problem 11.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, character*(*) TITLE_FORWARD, the title of the forward transform
c    for the problem.
c
      implicit none

      character*(*) title_forward

      title_forward = 'F(T) = 2*exp(-4/t)/sqrt(pi*t**3)'

      return
      end
      subroutine p12_f_backward ( z, fz )

c*********************************************************************72
c
cc P12_F_BACKWARD evaluates the backward function for problem 12.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 November 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double complex Z, the evaluation point.
c
c    Output, double complex FZ, the value of the function.
c
      implicit none

      double complex c8_atan
      double complex fz
      double complex z

      fz = c8_atan ( 1.0D+00 / z )

      return
      end
      subroutine p12_f_forward ( t, ft )

c*********************************************************************72
c
cc P12_F_FORWARD evaluates the forward function for problem 12.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 November 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision T, the evaluation point.
c
c    Output, double precision FT, the value of the function.
c
      implicit none

      double precision ft
      double precision t

      ft = sin ( t ) / t

      return
      end
      subroutine p12_title_backward ( title_backward )

c*********************************************************************72
c
cc P12_TITLE_BACKWARD sets the title for the backward transform for problem 12.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, character*(*) TITLE_BACKWARD, the title of the backward transform
c    for the problem.
c
      implicit none

      character*(*) title_backward

      title_backward = 'G(Z) = atan(1/z)'

      return
      end
      subroutine p12_title_forward ( title_forward )

c*********************************************************************72
c
cc P12_TITLE_FORWARD sets the title for the forward transform for problem 12.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, character*(*) TITLE_FORWARD, the title of the forward transform
c    for the problem.
c
      implicit none

      character*(*) title_forward

      title_forward = 'F(T) = sin(t)/t'

      return
      end
      subroutine p13_f_backward ( z, fz )

c*********************************************************************72
c
cc P13_F_BACKWARD evaluates the backward function for problem 13.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 November 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double complex Z, the evaluation point.
c
c    Output, double complex FZ, the value of the function.
c
      implicit none

      double complex fz
      double complex z

      fz = 1.0D+00 / sqrt ( 1.0D+00 + z * z )

      return
      end
      subroutine p13_f_forward ( t, ft )

c*********************************************************************72
c
cc P13_F_FORWARD evaluates the forward function for problem 13.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 November 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision T, the evaluation point.
c
c    Output, double precision FT, the value of the function.
c
      implicit none

      double precision besj0
      double precision ft
      double precision t

      ft = besj0 ( t )

      return
      end
      subroutine p13_title_backward ( title_backward )

c*********************************************************************72
c
cc P13_TITLE_BACKWARD sets the title for the backward transform for problem 13.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, character*(*) TITLE_BACKWARD, the title of the backward transform
c    for the problem.
c
      implicit none

      character*(*) title_backward

      title_backward = 'G(Z) = 1/sqrt(1+z*z)'

      return
      end
      subroutine p13_title_forward ( title_forward )

c*********************************************************************72
c
cc P13_TITLE_FORWARD sets the title for the forward transform for problem 13.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, character*(*) TITLE_FORWARD, the title of the forward transform
c    for the problem.
c
      implicit none

      character*(*) title_forward

      title_forward = 'F(T) = j0(t)'

      return
      end
      subroutine p14_f_backward ( z, fz )

c*********************************************************************72
c
cc P14_F_BACKWARD evaluates the backward function for problem 14.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 November 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double complex Z, the evaluation point.
c
c    Output, double complex FZ, the value of the function.
c
      implicit none

      double complex fz
      double complex z

      fz = 1.0D+00 / sqrt ( z ) / exp ( 1.0D+00 / z )

      return
      end
      subroutine p14_f_forward ( t, ft )

c*********************************************************************72
c
cc P14_F_FORWARD evaluates the forward function for problem 14.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 November 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision T, the evaluation point.
c
c    Output, double precision FT, the value of the function.
c
      implicit none

      double precision ft
      double precision, parameter :: pi = 3.141592653589793D+00
      double precision t

      ft = cos ( 2.0D+00 * sqrt ( t ) ) / sqrt ( pi * t )

      return
      end
      subroutine p14_title_backward ( title_backward )

c*********************************************************************72
c
cc P14_TITLE_BACKWARD sets the title for the backward transform for problem 14.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, character*(*) TITLE_BACKWARD, the title of the backward transform
c    for the problem.
c
      implicit none

      character*(*) title_backward

      title_backward = 'G(Z) = 1/(sqrt(z)*exp(1/z))'

      return
      end
      subroutine p14_title_forward ( title_forward )

c*********************************************************************72
c
cc P14_TITLE_FORWARD sets the title for the forward transform for problem 14.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, character*(*) TITLE_FORWARD, the title of the forward transform
c    for the problem.
c
      implicit none

      character*(*) title_forward

      title_forward = 'F(T) = cos(2*sqrt(t))/sqrt(pi*t)'

      return
      end
      subroutine p15_f_backward ( z, fz )

c*********************************************************************72
c
cc P15_F_BACKWARD evaluates the backward function for problem 15.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 November 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double complex Z, the evaluation point.
c
c    Output, double complex FZ, the value of the function.
c
      implicit none

      double complex fz
      double complex z

      fz = 1.0D+00 / ( z + 0.5D+00 )

      return
      end
      subroutine p15_f_forward ( t, ft )

c*********************************************************************72
c
cc P15_F_FORWARD evaluates the forward function for problem 15.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 November 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision T, the evaluation point.
c
c    Output, double precision FT, the value of the function.
c
      implicit none

      double precision ft
      double precision t

      ft = exp ( - 0.5D+00 * t )

      return
      end
      subroutine p15_title_backward ( title_backward )

c*********************************************************************72
c
cc P15_TITLE_BACKWARD sets the title for the backward transform for problem 15.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, character*(*) TITLE_BACKWARD, the title of the backward transform
c    for the problem.
c
      implicit none

      character*(*) title_backward

      title_backward = 'G(Z) = 1/(z+0.5)'

      return
      end
      subroutine p15_title_forward ( title_forward )

c*********************************************************************72
c
cc P15_TITLE_FORWARD sets the title for the forward transform for problem 15.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, character*(*) TITLE_FORWARD, the title of the forward transform
c    for the problem.
c
      implicit none

      character*(*) title_forward

      title_forward = 'F(T) = exp(-0.5*t)'

      return
      end
      subroutine p16_f_backward ( z, fz )

c*********************************************************************72
c
cc P16_F_BACKWARD evaluates the backward function for problem 16.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 November 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double complex Z, the evaluation point.
c
c    Output, double complex FZ, the value of the function.
c
      implicit none

      double complex fz
      double complex z

      fz = 1.0D+00 / ( 1.0D+00 + ( z + 0.2D+00 ) * ( z + 0.2D+00 ) )

      return
      end
      subroutine p16_f_forward ( t, ft )

c*********************************************************************72
c
cc P16_F_FORWARD evaluates the forward function for problem 16.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 November 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision T, the evaluation point.
c
c    Output, double precision FT, the value of the function.
c
      implicit none

      double precision ft
      double precision t

      ft = exp ( - 0.2D+00 * t ) * sin ( t )

      return
      end
      subroutine p16_title_backward ( title_backward )

c*********************************************************************72
c
cc P16_TITLE_BACKWARD sets the title for the backward transform for problem 16.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, character*(*) TITLE_BACKWARD, the title of the backward transform
c    for the problem.
c
      implicit none

      character*(*) title_backward

      title_backward = 'G(Z) = 1/(1+(z+0.2)**2)'

      return
      end
      subroutine p16_title_forward ( title_forward )

c*********************************************************************72
c
cc P16_TITLE_FORWARD sets the title for the forward transform for problem 16.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 October 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, character*(*) TITLE_FORWARD, the title of the forward transform
c    for the problem.
c
      implicit none

      character*(*) title_forward

      title_forward = 'F(T) = exp(-0.2*t)*sin(t)'

      return
      end
      subroutine timestamp ( )

c*********************************************************************72
c
cc TIMESTAMP prints out the current YMDHMS date as a timestamp.
c
c  Discussion:
c
c    This FORTRAN77 version is made available for cases where the
c    FORTRAN90 version cannot be used.
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
