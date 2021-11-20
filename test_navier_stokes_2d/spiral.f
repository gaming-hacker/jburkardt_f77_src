      subroutine get_unit ( iunit )

c*********************************************************************72
c
cc GET_UNIT returns a free FORTRAN unit number.
c
c  Discussion:
c
c    A "free" FORTRAN unit number is a value between 1 and 99 which
c    is not currently associated with an I/O device.  A free FORTRAN unit
c    number is needed in order to open a file with the OPEN command.
c
c    If IUNIT = 0, then no free FORTRAN unit could be found, although
c    all 99 units were checked (except for units 5, 6 and 9, which
c    are commonly reserved for console I/O).
c
c    Otherwise, IUNIT is a value between 1 and 99, representing a
c    free FORTRAN unit.  Note that GET_UNIT assumes that units 5 and 6
c    are special, and will never return those values.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 September 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, integer IUNIT, the free unit number.
c
      implicit none

      integer i
      integer iunit
      logical value

      iunit = 0

      do i = 1, 99

        if ( i .ne. 5 .and. i .ne. 6 .and. i .ne. 9 ) then

          inquire ( unit = i, opened = value, err = 10 )

          if ( .not. value ) then
            iunit = i
            return
          end if

        end if

10      continue

      end do

      return
      end
      subroutine res ( mn, nu, x, y, t, u, v, p, res_u, res_v, res_p )

c*********************************************************************72
c
cc RES evaluates the pointwise residual of the spiral flow problem.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 February 2011
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Maxim Olshanskii, Leo Rebholz,
c    Application of barycenter refined meshes in linear elasticity
c    and incompressible fluid dynamics,
c    ETNA: Electronic Transactions in Numerical Analysis,
c    Volume 38, pages 258-274, 2011.
c
c  Parameters:
c
c    Input, integer MN, the number of nodes.
c
c    Input, double precision NU, the kinematic viscosity.
c
c    Input, double precision X(MN), Y(MN), the coordinates of nodes.
c
c    Input, double precision T, the current time.
c
c    Input, double precision U(MN), V(MN), the X and Y velocity.
c
c    Input, double precision P(MN), the pressure.
c
c    Output, double precision RES_U(MN), the U-equation residual.
c
c    Output, double precision RES_V(MN), the V-equation residual.
c
c    Output, double precision RES_P(MN), the P-equation residual.
c
      implicit none

      integer mn

      integer i
      double precision lhs_p(mn)
      double precision lhs_u(mn)
      double precision lhs_v(mn)
      double precision nu
      double precision p(mn)
      double precision res_p(mn)
      double precision res_u(mn)
      double precision res_v(mn)
      double precision rhs_p(mn)
      double precision rhs_u(mn)
      double precision rhs_v(mn)
      double precision t
      double precision u(mn)
      double precision v(mn)
      double precision x(mn)
      double precision y(mn)

      call lhs ( mn, nu, x, y, t, u, v, p, lhs_u, lhs_v, lhs_p )

      call rhs ( mn, nu, x, y, t, rhs_u, rhs_v, rhs_p )

      do i = 1, mn
        res_u(i) = lhs_u(i) - rhs_u(i)
        res_v(i) = lhs_v(i) - rhs_v(i)
        res_p(i) = lhs_p(i) - rhs_p(i)
      end do

      return
      end
      subroutine rhs ( mn, nu, x, y, t, rhs_u, rhs_v, rhs_p )

c*********************************************************************72
c
cc RHS evaluates the right hand side of the spiral flow problem.
c
c  Discussion:
c
c    The right hand side is artificially determined by the requirement
c    that the specified values of U, V and P satisfy the discretized
c    Navier Stokes and continuity equations.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 February 2011
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Maxim Olshanskii, Leo Rebholz,
c    Application of barycenter refined meshes in linear elasticity
c    and incompressible fluid dynamics,
c    ETNA: Electronic Transactions in Numerical Analysis,
c    Volume 38, pages 258-274, 2011.
c
c  Parameters:
c
c    Input, integer MN, the number of nodes.
c
c    Input, double precision NU, the kinematic viscosity.
c
c    Input, double precision X(MN), Y(MN), the coordinates of nodes.
c
c    Input, double precision T, the current time.
c
c    Output, double precision RHS_U(MN), the U-equation right hand side.
c
c    Output, double precision RHS_V(MN), the V-equation right hand side.
c
c    Output, double precision RHS_P(MN), the P-equation right hand side.
c
      implicit none

      integer mn

      double precision dpdx(mn)
      double precision dpdy(mn)
      double precision dudt(mn)
      double precision dudx(mn)
      double precision dudxx(mn)
      double precision dudy(mn)
      double precision dudyy(mn)
      double precision dvdt(mn)
      double precision dvdx(mn)
      double precision dvdxx(mn)
      double precision dvdy(mn)
      double precision dvdyy(mn)
      integer i
      double precision nu
      double precision p(mn)
      double precision rhs_p(mn)
      double precision rhs_u(mn)
      double precision rhs_v(mn)
      double precision t
      double precision u(mn)
      double precision v(mn)
      double precision x(mn)
      double precision y(mn)

      do i = 1, mn

        u(i) = ( 1.0D+00 + 0.01D+00 * t ) * 2.0D+00 
     &    * x(i)**2 * ( x(i) - 1.0D+00 )**2 
     &    * y(i) * ( 2.0D+00 * y(i) - 1.0D+00 ) * ( y(i) - 1.0D+00 )

        dudt(i) = 0.01D+00 * 2.0D+00 
     &    * x(i)**2 * ( x(i) - 1.0D+00 )**2 
     &    * y(i) * ( 2.0D+00 * y(i) - 1.0D+00 ) * ( y(i) - 1.0D+00 )

        dudx(i) = ( 1.0D+00 + 0.01D+00 * t ) * 2.0D+00 
     &    * x(i) * ( 2.0D+00 + x(i) * ( - 6.0D+00 + 4.0D+00 * x(i) ) ) 
     &    * y(i) * ( 2.0D+00 * y(i) - 1.0D+00 ) * ( y(i) - 1.0D+00 )

        dudxx(i) = ( 1.0D+00 + 0.01D+00 * t ) * 2.0D+00 
     &    * ( 2.0D+00 + x(i) * ( - 12.0D+00 + 12.0D+00 * x(i) ) ) 
     &    * y(i) * ( 2.0D+00 * y(i) - 1.0D+00 ) * ( y(i) - 1.0D+00 )

        dudy(i) = ( 1.0D+00 + 0.01D+00 * t ) * 2.0D+00 
     &    * x(i)**2 * ( x(i) - 1.0D+00 )**2 
     &    * ( 1.0D+00 + y(i) * ( - 6.0D+00 + y(i) * 6.0D+00 ) )

        dudyy(i) = ( 1.0D+00 + 0.01D+00 * t ) * 2.0D+00 
     &    * x(i)**2 * ( x(i) - 1.0D+00 )**2 
     &    * ( 12.0D+00 * y(i) - 6.0D+00 )

        v(i) = - ( 1.0D+00 + 0.01D+00 * t ) * 2.0D+00 
     &    * x(i) * ( 2.0D+00 * x(i) - 1.0D+00 ) * ( x(i) - 1.0D+00 )  
     &    * y(i)**2 * ( y(i) - 1.0D+00 )**2

        dvdt(i) = - 0.01D+00 * 2.0D+00 
     &    * x(i) * ( 2.0D+00 * x(i) - 1.0D+00 ) * ( x(i) - 1.0D+00 )  
     &    * y(i)**2 * ( y(i) - 1.0D+00 )**2

        dvdx(i) = - ( 1.0D+00 + 0.01D+00 * t ) * 2.0D+00 
     &    * x(i) * ( 1.0D+00 + x(i) * ( - 3.0D+00 + x(i) * 2.0D+00 ) )  
     &    * y(i)**2 * ( y(i) - 1.0D+00 )**2

        dvdxx(i) = - ( 1.0D+00 + 0.01D+00 * t ) * 2.0D+00 
     &    * ( 12.0D+00 * x(i) - 6.0D+00 ) 
     &    * y(i)**2 * ( y(i) - 1.0D+00 )**2

        dvdy(i) = - ( 1.0D+00 + 0.01D+00 * t ) * 2.0D+00 
     &    * x(i) * ( 2.0D+00 * x(i) - 1.0D+00 ) * ( x(i) - 1.0D+00 )  
     &    * y(i)**2 
     &    * ( 2.0D+00 + y(i) * ( - 6.0D+00 + y(i) * 4.0D+00 ) )

        dvdyy(i) = - ( 1.0D+00 + 0.01D+00 * t ) * 2.0D+00 
     &    * x(i) * ( 2.0D+00 * x(i) - 1.0D+00 ) * ( x(i) - 1.0D+00 )  
     &    * ( 2.0D+00 + y(i) * ( - 12.0D+00 + y(i) * 12.0D+00 ) )

        p(i) = y(i)
        dpdx(i) = 0.0D+00
        dpdy(i) = 1.0D+00

        rhs_u(i) = dudt(i) - nu * ( dudxx(i) + dudyy(i) ) 
     &    + u(i) * dudx(i) + v(i) * dudy(i) + dpdx(i)

        rhs_v(i) = dvdt(i) - nu * ( dvdxx(i) + dvdyy(i) ) 
     &    + u(i) * dvdx(i) + v(i) * dvdy(i) + dpdy(i)

        rhs_p(i) = 0.0D+00

      end do

      return
      end
      subroutine uvp ( mn, nu, x, y, t, u, v, p )

c*********************************************************************72
c
cc UVP returns velocity and pressure for the spiral flow.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 February 2011
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Maxim Olshanskii, Leo Rebholz,
c    Application of barycenter refined meshes in linear elasticity
c    and incompressible fluid dynamics,
c    ETNA: Electronic Transactions in Numerical Analysis,
c    Volume 38, pages 258-274, 2011.
c
c  Parameters:
c
c    Input, integer MN, the number of nodes.
c
c    Input, double precision NU, the kinematic viscosity.
c
c    Input, double precision X(MN), Y(MN), the coordinates of nodes.
c
c    Input, double precision T, the current time.
c
c    Output, double precision U(MN), V(MN), the X and Y velocity.
c
c    Output, double precision P(MN), the pressure.
c
      implicit none

      integer mn

      integer i
      double precision nu
      double precision p(mn)
      double precision t
      double precision u(mn)
      double precision v(mn)
      double precision x(mn)
      double precision y(mn)

      do i = 1, mn

        u(i) = ( 1.0D+00 + 0.01D+00 * t ) * 2.0D+00
     &    * x(i)**2 * ( x(i) - 1.0D+00 )**2
     &    * y(i) * ( 2.0D+00 * y(i) - 1.0D+00 ) * ( y(i) - 1.0D+00 )

        v(i) = - ( 1.0D+00 + 0.01D+00 * t ) * 2.0D+00
     &    * x(i) * ( 2.0D+00 * x(i) - 1.0D+00 ) * ( x(i) - 1.0D+00 )
     &    * y(i)**2 * ( y(i) - 1.0D+00 )**2

        p(i) = y(i)

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
