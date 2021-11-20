      function r8vec_amax ( n, a )

c*********************************************************************72
c
cc R8VEC_AMAX returns the maximum absolute value in an R8VEC.
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
c    02 June 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in the array.
c
c    Input, double precision A(N), the array.
c
c    Output, double precision R8VEC_AMAX, the value of the entry
c    of largest magnitude.
c
      implicit none

      integer n

      double precision a(n)
      integer i
      double precision r8vec_amax
      double precision value

      value = 0.0D+00
      do i = 1, n
        value = max ( value, abs ( a(i) ) )
      end do

      r8vec_amax = value

      return
      end
      function r8vec_amin ( n, a )

c*********************************************************************72
c
cc R8VEC_AMIN returns the minimum absolute value in an R8VEC.
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
c    02 June 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in the array.
c
c    Input, double precisionA(N), the array.
c
c    Output, double precision R8VEC_AMIN, the value of the entry
c    of smallest magnitude.
c
      implicit none

      integer n

      double precision a(n)
      integer i
      double precision r8_huge
      parameter ( r8_huge = 1.79769313486231571D+308 )
      double precision r8vec_amin
      double precision value

      value = r8_huge
      do i = 1, n
        value = min ( value, abs ( a(i) ) )
      end do

      r8vec_amin = value

      return
      end
      function r8vec_max ( n, a )

c*********************************************************************72
c
cc R8VEC_MAX returns the maximum value in an R8VEC.
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
c    12 May 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in the array.
c
c    Input, double precision A(N), the array.
c
c    Output, double precision R8VEC_MAX, the value of the largest entry.
c
      implicit none

      integer n

      double precision a(n)
      integer i
      double precision r8vec_max
      double precision value

      value = a(1)
      do i = 2, n
        value = max ( value, a(i) )
      end do

      r8vec_max = value

      return
      end
      function r8vec_min ( n, a )

c*********************************************************************72
c
cc R8VEC_MIN returns the minimum value in an R8VEC.
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
c    31 May 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in the array.
c
c    Input, double precision A(N), the array.
c
c    Output, double precision R8VEC_MIN, the value of the smallest entry.
c
      implicit none

      integer n

      double precision a(n)
      integer i
      double precision r8vec_min
      double precision value

      value = a(1)
      do i = 2, n
        value = min ( value, a(i) )
      end do

      r8vec_min = value

      return
      end
      subroutine r8vec_uniform_ab ( n, a, b, seed, r )

c*********************************************************************72
c
cc R8VEC_UNIFORM_AB returns a scaled pseudorandom R8VEC.
c
c  Discussion:
c
c    Each dimension ranges from A to B.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    29 January 2005
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Paul Bratley, Bennett Fox, Linus Schrage,
c    A Guide to Simulation,
c    Second Edition,
c    Springer, 1987,
c    ISBN: 0387964673,
c    LC: QA76.9.C65.B73.
c
c    Bennett Fox,
c    Algorithm 647:
c    Implementation and Relative Efficiency of Quasirandom
c    Sequence Generators,
c    ACM Transactions on Mathematical Software,
c    Volume 12, Number 4, December 1986, pages 362-376.
c
c    Pierre L'Ecuyer,
c    Random Number Generation,
c    in Handbook of Simulation,
c    edited by Jerry Banks,
c    Wiley, 1998,
c    ISBN: 0471134031,
c    LC: T57.62.H37.
c
c    Peter Lewis, Allen Goodman, James Miller,
c    A Pseudo-Random Number Generator for the System/360,
c    IBM Systems Journal,
c    Volume 8, Number 2, 1969, pages 136-143.
c
c  Parameters:
c
c    Input, integer N, the number of entries in the vector.
c
c    Input, double precision A, B, the lower and upper limits.
c
c    Input/output, integer SEED, the "seed" value, which should NOT be 0.
c    On output, SEED has been updated.
c
c    Output, double precision R(N), the vector of pseudorandom values.
c
      implicit none

      integer n

      double precision a
      double precision b
      integer i
      integer i4_huge
      parameter ( i4_huge = 2147483647 )
      integer k
      integer seed
      double precision r(n)

      if ( seed .eq. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8VEC_UNIFORM_AB - Fatal error!'
        write ( *, '(a)' ) '  Input value of SEED = 0.'
        stop 1
      end if

      do i = 1, n

        k = seed / 127773

        seed = 16807 * ( seed - k * 127773 ) - k * 2836

        if ( seed .lt. 0 ) then
          seed = seed + i4_huge
        end if

        r(i) = a + ( b - a ) * dble ( seed ) * 4.656612875D-10

      end do

      return
      end
      subroutine resid_ethier ( a, d, n, x, y, z, t, ur, vr, wr, pr )

c*********************************************************************72
c
cc RESID_ETHIER evaluates the residual of the Ethier exact Navier Stokes solution.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 January 2015
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    C Ross Ethier, David Steinman,
c    Exact fully 3D Navier-Stokes solutions for benchmarking,
c    International Journal for Numerical Methods in Fluids,
c    Volume 19, Number 5, March 1994, pages 369-375.
c
c  Parameters:
c
c    Input, double precision A, D, the parameters.  Sample values are A = PI/4
c    and D = PI/2.
c
c    Input, integer N, the number of points at which the solution
c    is to be evaluated.
c
c    Input, double precision X(N), Y(N), Z(N), the coordinates of the points.
c
c    Input, double precision T, the time coordinate or coordinates.
c
c    Output, double precision UR(N), VR(N), WR(N), PR(N), the residuals.
c
      implicit none

      integer n

      double precision a
      double precision cxy(n)
      double precision cyz(n)
      double precision czx(n)
      double precision d
      double precision e2t
      double precision e2x(n)
      double precision e2y(n)
      double precision e2z(n)
      double precision e4t
      double precision ex(n)
      double precision exy(n)
      double precision ey(n)
      double precision eyz(n)
      double precision ez(n)
      double precision ezx(n)
      integer i
      double precision p(n)
      double precision pr(n)
      double precision px(n)
      double precision py(n)
      double precision pz(n)
      double precision sxy(n)
      double precision syz(n)
      double precision szx(n)
      double precision t
      double precision u(n)
      double precision ur(n)
      double precision ut(n)
      double precision ux(n)
      double precision uxx(n)
      double precision uy(n)
      double precision uyy(n)
      double precision uz(n)
      double precision uzz(n)
      double precision v(n)
      double precision vr(n)
      double precision vt(n)
      double precision vx(n)
      double precision vxx(n)
      double precision vy(n)
      double precision vyy(n)
      double precision vz(n)
      double precision vzz(n)
      double precision w(n)
      double precision wr(n)
      double precision wt(n)
      double precision wx(n)
      double precision wxx(n)
      double precision wy(n)
      double precision wyy(n)
      double precision wz(n)
      double precision wzz(n)
      double precision x(n)
      double precision y(n)
      double precision z(n)
c
c  Make some temporaries.
c
      do i = 1, n

        ex(i) = exp ( a * x(i) )
        ey(i) = exp ( a * y(i) )
        ez(i) = exp ( a * z(i) )

        e2x(i) = exp ( 2.0D+00 * a * x(i) )
        e2y(i) = exp ( 2.0D+00 * a * y(i) )
        e2z(i) = exp ( 2.0D+00 * a * z(i) )

        e2t = exp  ( -       d * d * t )
        e4t = exp  ( - 2.0 * d * d * t )

        exy(i) = exp ( a * ( x(i) + y(i) ) )
        eyz(i) = exp ( a * ( y(i) + z(i) ) )
        ezx(i) = exp ( a * ( z(i) + x(i) ) )

        sxy(i) = sin ( a * x(i) + d * y(i) )
        syz(i) = sin ( a * y(i) + d * z(i) )
        szx(i) = sin ( a * z(i) + d * x(i) )

        cxy(i) = cos ( a * x(i) + d * y(i) )
        cyz(i) = cos ( a * y(i) + d * z(i) )
        czx(i) = cos ( a * z(i) + d * x(i) )
c
c  Form the functions and derivatives.
c
        u(i) =   -         a * (           ex(i) * syz(i)           
     &                           +         ez(i) * cxy(i) ) * e2t
        ux(i) =  -         a * (       a * ex(i) * syz(i)           
     &                           -     a * ez(i) * sxy(i) ) * e2t
        uxx(i) = -         a * (   a * a * ex(i) * syz(i)           
     &                           - a * a * ez(i) * cxy(i) ) * e2t
        uy(i) =  -         a * (       a * ex(i) * cyz(i)           
     &                           -     d * ez(i) * sxy(i) ) * e2t
        uyy(i) = -         a * ( - a * a * ex(i) * syz(i)           
     &                           - d * d * ez(i) * cxy(i) ) * e2t
        uz(i) =  -         a * (       d * ex(i) * cyz(i)           
     &                           +     a * ez(i) * cxy(i) ) * e2t
        uzz(i) =  -        a * ( - d * d * ex(i) * syz(i)           
     &                           + a * a * ez(i) * cxy(i) ) * e2t
        ut(i) =  + d * d * a * (           ex(i) * syz(i)           
     &                           +         ez(i) * cxy(i) ) * e2t

        v(i) =   -         a * (           ey(i) * szx(i)           
     &                           +         ex(i) * cyz(i) ) * e2t
        vx(i) =  -         a * (       d * ey(i) * czx(i)           
     &                           +     a * ex(i) * cyz(i) ) * e2t
        vxx(i) = -         a * ( - d * d * ey(i) * szx(i)           
     &                           + a * a * ex(i) * cyz(i) ) * e2t
        vy(i) =  -         a * (       a * ey(i) * szx(i)           
     &                           -     a * ex(i) * syz(i) ) * e2t
        vyy(i) = -         a * (   a * a * ey(i) * szx(i)           
     &                           - a * a * ex(i) * cyz(i) ) * e2t
        vz(i) =  -         a * (       a * ey(i) * czx(i)           
     &                           -     d * ex(i) * syz(i) ) * e2t
        vzz(i) =  -        a * ( - a * a * ey(i) * szx(i)           
     &                           - d * d * ex(i) * cyz(i) ) * e2t
        vt(i) =  + d * d * a * (           ey(i) * szx(i)           
     &                           +         ex(i) * cyz(i) ) * e2t

        w(i) =   -         a * (           ez(i) * sxy(i)           
     &                           +         ey(i) * czx(i) ) * e2t
        wx(i) =  -         a * (       a * ez(i) * cxy(i)           
     &                           -     d * ey(i) * szx(i) ) * e2t
        wxx(i) = -         a * ( - a * a * ez(i) * sxy(i)           
     &                           - d * d * ey(i) * czx(i) ) * e2t
        wy(i) =  -         a * (       d * ez(i) * cxy(i)           
     &                           +     a * ey(i) * czx(i) ) * e2t
        wyy(i) = -         a * ( - d * d * ez(i) * sxy(i)           
     &                           + a * a * ey(i) * czx(i) ) * e2t
        wz(i) =  -         a * (       a * ez(i) * sxy(i)           
     &                           -     a * ey(i) * szx(i) ) * e2t
        wzz(i) = -         a * (   a * a * ez(i) * sxy(i)           
     &                           - a * a * ey(i) * czx(i) ) * e2t
        wt(i) =  + d * d * a * (           ez(i) * sxy(i)           
     &                           +         ey(i) * czx(i) ) * e2t

        p(i) = - 0.5D+00 * a * a * e4t * (
     &    + e2x(i) + 2.0D+00 * sxy(i) * czx(i) * eyz(i)
     &    + e2y(i) + 2.0D+00 * syz(i) * cxy(i) * ezx(i)
     &    + e2z(i) + 2.0D+00 * szx(i) * cyz(i) * exy(i) )

        px(i) = - 0.5D+00 * a * a * e4t * (
     &    + 2.0D+00 * a * e2x(i)
     &    + 2.0D+00 * a * cxy(i) * czx(i) * eyz(i)
     &    - 2.0D+00 * d * sxy(i) * szx(i) * eyz(i)
     &    - 2.0D+00 * a * syz(i) * sxy(i) * ezx(i)
     &    + 2.0D+00 * a * syz(i) * cxy(i) * ezx(i)
     &    + 2.0D+00 * d * czx(i) * cyz(i) * exy(i)
     &    + 2.0D+00 * a * szx(i) * cyz(i) * exy(i) )

        py(i) = - 0.5D+00 * a * a * e4t * (
     &    + 2.0D+00 * d * cxy(i) * czx(i) * eyz(i)
     &    + 2.0D+00 * a * sxy(i) * czx(i) * eyz(i)
     &    + 2.0D+00 * a * e2y(i)
     &    + 2.0D+00 * a * cyz(i) * cxy(i) * ezx(i)
     &    - 2.0D+00 * d * syz(i) * sxy(i) * ezx(i)
     &    - 2.0D+00 * a * szx(i) * syz(i) * exy(i)
     &    + 2.0D+00 * a * szx(i) * cyz(i) * exy(i) )

        pz(i) = - 0.5D+00 * a * a * e4t * (
     &    - 2.0D+00 * a * sxy(i) * szx(i) * eyz(i)
     &    + 2.0D+00 * a * sxy(i) * czx(i) * eyz(i)
     &    + 2.0D+00 * d * cyz(i) * cxy(i) * ezx(i)
     &    + 2.0D+00 * a * syz(i) * cxy(i) * ezx(i)
     &    + 2.0D+00 * a * e2z(i)
     &    + 2.0D+00 * a * czx(i) * cyz(i) * exy(i)
     &    - 2.0D+00 * d * szx(i) * syz(i) * exy(i) )
c
c  Evaluate the residuals.
c
        ur(i) = ut(i)
     &    + u(i) * ux(i) + v(i) * uy(i) + w(i) * uz(i) + px(i)
     &    - ( uxx(i) + uyy(i) + uzz(i) )

        vr(i) = vt(i)
     &    + u(i) * vx(i) + v(i) * vy(i) + w(i) * vz(i) + py(i)
     &    - ( vxx(i) + vyy(i) + vzz(i) )

        wr(i) = wt(i)
     &    + u(i) * wx(i) + v(i) * wy(i) + w(i) * wz(i) + pz(i)
     &    - ( wxx(i) + wyy(i) + wzz(i) )

        pr(i) = ux(i) + vy(i) + wz(i)

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
      subroutine uvwp_ethier ( a, d, n, x, y, z, t, u, v, w, p )

c*********************************************************************72
c
cc UVWP_ETHIER evaluates the Ethier exact Navier Stokes solution.
c
c  Discussion:
c
c    The reference asserts that the given velocity and pressure fields
c    are exact solutions for the 3D incompressible time-dependent
c    Navier Stokes equations over any region.
c
c    To define a typical problem, one chooses a bounded spatial region
c    and a starting time, and then imposes boundary and initial conditions
c    by referencing the exact solution appropriately.
c
c    In the reference paper, a calculation is made for the cube centered
c    at (0,0,0) with a "radius" of 1 unit, and over the time interval
c    from t = 0 to t = 0.1, with parameters a = PI/4 and d = PI/2,
c    and with Dirichlet boundary conditions on all faces of the cube.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 January 2015
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    C Ross Ethier, David Steinman,
c    Exact fully 3D Navier-Stokes solutions for benchmarking,
c    International Journal for Numerical Methods in Fluids,
c    Volume 19, Number 5, March 1994, pages 369-375.
c
c  Parameters:
c
c    Input, double precision A, D, the parameters.  Sample values are A = PI/4 
c    and D = PI/2.
c
c    Input, integer ( kind  N, the number of points at which the solution is to
c    be evaluated.
c
c    Input, double precision X(N), Y(N), Z(N), the coordinates of the points.
c
c    Input, double precision T, the time coordinate or coordinates.
c
c    Output, double precision U(N), V(N), W(N), P(N), the velocity components 
c    and pressure at each of the points.
c
      implicit none

      integer n

      double precision a
      double precision cxy(n)
      double precision cyz(n)
      double precision czx(n)
      double precision d
      double precision e2t
      double precision ex(n)
      double precision exy(n)
      double precision ey(n)
      double precision eyz(n)
      double precision ez(n)
      double precision ezx(n)
      integer i
      double precision p(n)
      double precision sxy(n)
      double precision syz(n)
      double precision szx(n)
      double precision t
      double precision u(n)
      double precision v(n)
      double precision w(n)
      double precision x(n)
      double precision y(n)
      double precision z(n)

      do i = 1, n

        ex(i) = exp ( a * x(i) )
        ey(i) = exp ( a * y(i) )
        ez(i) = exp ( a * z(i) )

        e2t = exp  ( - d * d * t )

        exy(i) = exp ( a * ( x(i) + y(i) ) )
        eyz(i) = exp ( a * ( y(i) + z(i) ) )
        ezx(i) = exp ( a * ( z(i) + x(i) ) )

        sxy(i) = sin ( a * x(i) + d * y(i) )
        syz(i) = sin ( a * y(i) + d * z(i) )
        szx(i) = sin ( a * z(i) + d * x(i) )

        cxy(i) = cos ( a * x(i) + d * y(i) )
        cyz(i) = cos ( a * y(i) + d * z(i) )
        czx(i) = cos ( a * z(i) + d * x(i) )

        u(i) = - a * ( ex(i) * syz(i) + ez(i) * cxy(i) ) * e2t
        v(i) = - a * ( ey(i) * szx(i) + ex(i) * cyz(i) ) * e2t
        w(i) = - a * ( ez(i) * sxy(i) + ey(i) * czx(i) ) * e2t
        p(i) = 0.5D+00 * a * a * e2t * e2t * (
     &    + ex(i) * ex(i) + 2.0D+00 * sxy(i) * czx(i) * eyz(i)
     &    + ey(i) * ey(i) + 2.0D+00 * syz(i) * cxy(i) * ezx(i)
     &    + ez(i) * ez(i) + 2.0D+00 * szx(i) * cyz(i) * exy(i) )

      end do

      return
      end

