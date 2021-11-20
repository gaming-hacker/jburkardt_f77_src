      subroutine fx1 ( n, x, f )

c*********************************************************************72
c
cc FX1 is 1D example #1.
c
c  Discussion:
c
c    This is example 3.1 in the reference.
c
c    The function should be plotted over [-1.0,+1.0].
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Rick Archibald, Anne Gelb, Jungho Yoon,
c    Polynomial fitting for edge detection in irregularly sampled signals
c    and images,
c    SIAM Journal on Numerical Analysis,
c    Volume 43, Number 1, 2006, pages 259-279.
c
c  Parameters:
c
c    Input, integer N, the number of points.
c
c    Input, double precision X(N), the arguments.
c
c    Output, double precision F(N), the function values.
c
c  Local parameters:
c
c    Local, real STEEP, controls the steepness of the slope.
c    The default value is a moderate 5.  For a sharp rise, use 25 instead.
c
      implicit none

      integer n

      double precision f(n)
      integer i
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision steep
      double precision x(n)

      steep = 5.0D+00

      do i = 1, n

        if ( x(i) .lt. 0.0D+00 ) then
          f(i) = cos ( 3.0D+00 * r8_pi * x(i) );
        else if ( 0.0D+00 .le. x(i) ) then
          f(i) = - 1.0D+00 + 2.0D+00 / ( 1.0D+00 + 3.0D+00
     &      * exp ( - steep * ( 2.0D+00 * x(i) - 1.0D+00 ) ) )
        end if

      end do

      return
      end
      subroutine fx2 ( n, x, f )

c*********************************************************************72
c
cc FX2 is 1D example #2.
c
c  Discussion:
c
c    The function should be plotted over [-1,+1].
c
c    The "internal" coordinate range will be [-2.0,6.0*pi].
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Rick Archibald, Anne Gelb, Jungho Yoon,
c    Polynomial fitting for edge detection in irregularly sampled signals
c    and images,
c    SIAM Journal on Numerical Analysis,
c    Volume 43, Number 1, 2006, pages 259-279.
c
c  Parameters:
c
c    Input, integer N, the number of points.
c
c    Input, double precision X(N), the arguments.
c
c    Output, double precision F(N), the function values.
c
      implicit none

      integer n

      double precision f(n)
      integer i
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision x(n)
      double precision x2
c
c  Map from the convenient range [-1,+1] to the physical range [-2,6pi].
c
      do i = 1, n

        x2 = ( ( 1.0D+00 - x(i) ) * ( - 2.0D+00 )
     &       + ( 1.0D+00 + x(i) ) * 6.0D+00 * r8_pi )
     &       /   2.0D+00

        if ( x2 .lt. 0.0D+00 ) then
          f(i) = exp ( x2 )
        else if ( 0.0D+00 .le. x2 .and. 
     &            x2 .lt. 3.0D+00 * r8_pi / 2.0D+00 ) then
          f(i) = - exp ( - x2 )
        else if ( 3.0D+00 * r8_pi / 2.0D+00 .le. x2 ) then
          f(i) = -1.5D+00 * sin ( x2 )
        end if

      end do

      return
      end
      subroutine fx3 ( n, x, f )

c*********************************************************************72
c
cc FX3 is 1D example #3.
c
c  Discussion:
c
c    The function should be plotted over [-1.0,+1.0].
c
c    Internally, this range is mapped to [-3.0,+3.0].
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Rick Archibald, Anne Gelb, Jungho Yoon,
c    Polynomial fitting for edge detection in irregularly sampled signals
c    and images,
c    SIAM Journal on Numerical Analysis,
c    Volume 43, Number 1, 2006, pages 259-279.
c
c  Parameters:
c
c    Input, integer N, the number of points.
c
c    Input, double precision X(N), the arguments.
c
c    Output, double precision F(N), the function values.
c
      implicit none

      integer n

      double precision f(n)
      integer i
      double precision x(n)
      double precision x2
c
c  Map from the convenient range [-1,+1] to the physical range [-3,+3].
c
      do i = 1, n

        x2 = ( ( 1.0D+00 - x(i) ) * ( -3.0D+00 )
     &       + ( 1.0D+00 + x(i) ) * ( +3.0D+00 ) )
     &       /   2.0D+00

        if ( -2.0D+00 .le. x2 .and. x2 .le. -1.0D+00 ) then
          f(i) = 1.0D+00
        else if ( -0.5D+00 .le. x2 .and. x2 .le. 0.5D+00 ) then
          f(i) = 0.5D+00 + 4.0D+00 * ( x2 + 0.5D+00 ) ** 2
        else if ( 1.25D+00 .le. x2 .and. 
     &            3.0D+00 * x2 .le. 7.0D+00 ) then
          f(i) = 3.0D+00 * ( 2.0D+00 - x2 )
        else
          f(i) = 0.0D+00
        end if

      end do

      return
      end
      subroutine fx4 ( n, x, f )

c*********************************************************************72
c
cc FX4 is 1D example #4.
c
c  Discussion:
c
c    The function should be plotted over [0.0,+1.0].
c
c    The function is continuous, but the derivative has a discontinuity at 0.5.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Rick Archibald, Anne Gelb, Jungho Yoon,
c    Polynomial fitting for edge detection in irregularly sampled signals
c    and images,
c    SIAM Journal on Numerical Analysis,
c    Volume 43, Number 1, 2006, pages 259-279.
c
c  Parameters:
c
c    Input, integer N, the number of points.
c
c    Input, double precision X(N), the arguments.
c
c    Output, double precision F(N), the function values.
c
      implicit none

      integer n

      double precision f(n)
      integer i
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision x(n)
      double precision x2
c
c  Convert from -1 <= x <= 1 to 0 <= x <= 1:
c
      do i = 1, n

        x2 = ( x(i) + 1.0D+00 ) / 2.0D+00

        if ( x2 .le. 0.5D+00 ) then
          f(i) = - ( x2 - 0.5D+00 ) 
     &      + sin ( 4.0D+00 * r8_pi * x2 ) / 6.0D+00
        else if ( 0.5D+00 .lt. x2 ) then
          f(i) =   ( x2 - 0.5D+00 ) 
     &      + sin ( 4.0D+00 * r8_pi * x2 ) / 6.0D+00
        end if

      end do

      return
      end
      subroutine fx5 ( n, x, f )

c*********************************************************************72
c
cc FX5 is 1D example #5.
c
c  Discussion:
c
c    The function should be plotted over [-1.0,+1.0].
c
c    The function actually has no discontinuities, but does have a
c    steep rise.  The local parameter S controls the steepness of the rise.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Rick Archibald, Anne Gelb, Jungho Yoon,
c    Polynomial fitting for edge detection in irregularly sampled signals
c    and images,
c    SIAM Journal on Numerical Analysis,
c    Volume 43, Number 1, 2006, pages 259-279.
c
c  Parameters:
c
c    Input, integer N, the number of points.
c
c    Input, double precision X(N), the arguments.
c
c    Output, double precision F(N), the function values.
c
      implicit none

      integer n

      double precision f(n)
      integer i
      double precision steep
      double precision x(n)

      steep = 20.0D+00

      do i = 1, n
        f(i) = tanh ( steep * x(i) )
      end do

      return
      end
      subroutine fx6 ( n, x, f )

c*********************************************************************72
c
cc FX6 is the 1D example #6.
c
c  Discussion:
c
c    This is example 2.1 in the reference.
c
c    The function should be plotted over [0.0,+1.0].
c
c    The function has a discontinuous first derivative at 1/2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 September 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Rick Archibald, Anne Gelb, Jungho Yoon,
c    Determining the location of discontinuities in the derivatives
c    of functions,
c    Applied Numerical Mathematics,
c    Volume 58, 2008, pages 577-592.
c
c  Parameters:
c
c    Input, integer N, the number of points.
c
c    Input, double precision X(N), the arguments.
c
c    Output, double precision F(N), the function values.
c
      implicit none

      integer n

      double precision f(n)
      integer i
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision x(n)

      do i = 1, n
        f(i) = sin ( 2.0D+00 * r8_pi * x(i) ) / 6.0D+00
        if ( x(i) .lt. 0.5D+00 ) then
          f(i) = f(i) - ( x(i) - 0.5D+00 )
        else
          f(i) = f(i) + ( x(i) - 0.5D+00 )
        end if
      end do

      return
      end
      subroutine fx7 ( n, x, f )

c*********************************************************************72
c
cc FX7 is the 1D example #7.
c
c  Discussion:
c
c    This is example 2.1 in the reference.
c
c    The function should be plotted over [0.0,+1.0].
c
c    The function has a discontinuous second derivative at 1/2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 September 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Rick Archibald, Anne Gelb, Jungho Yoon,
c    Determining the location of discontinuities in the derivatives
c    of functions,
c    Applied Numerical Mathematics,
c    Volume 58, 2008, pages 577-592.
c
c  Parameters:
c
c    Input, integer N, the number of points.
c
c    Input, double precision X(N), the arguments.
c
c    Output, double precision F(N), the function values.
c
      implicit none

      integer n

      double precision f(n)
      integer i
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision x(n)

      do i = 1, n
        f(i) = sin ( 2.0D+00 * r8_pi * x(i) ) / 6.0D+00
        if ( x(i) .lt. 0.5D+00 ) then
          f(i) = f(i) - 0.5D+00 * ( x(i) - 0.5D+00 ) ** 2
        else
          f(i) = f(i) + 0.5D+00 * ( x(i) - 0.5D+00 ) ** 2
        end if
      end do

      return
      end
      subroutine fxy1 ( n, x, y, f )

c*********************************************************************72
c
cc FXY1 is 2D example #1.
c
c  Discussion:
c
c    This is example 4.1 in the reference.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Rick Archibald, Anne Gelb, Jungho Yoon,
c    Polynomial fitting for edge detection in irregularly sampled signals
c    and images,
c    SIAM Journal on Numerical Analysis,
c    Volume 43, Number 1, 2006, pages 259-279.
c
c  Parameters:
c
c    Input, integer N, the number of points.
c
c    Input, double precision X(N), Y(N), the arguments.
c
c    Output, double precision F(N), the function values.
c
      implicit none

      integer n

      double precision f(n)
      integer i
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision x(n)
      double precision y(n)

      do i = 1,  n

        f(i) = x(i) * y(i) 
     &    + cos ( 2.0D+00 * r8_pi * x(i) ** 2 )
     &    - sin ( 2.0D+00 * r8_pi * x(i) ** 2 )

        if ( 0.25D+00 .lt. x(i) ** 2 + y(i) ** 2 ) then
          f(i) = f(i) + 10.0D+00 * x(i) - 5.0D+00
        end if

      end do

      return
      end
      subroutine fxy2 ( n, x, y, f )

c*********************************************************************72
c
cc FXY2 is 2D example #2.
c
c  Discussion:
c
c    This is example 4.2 in the reference.
c
c    It is known as the Shepp-Logan phantom.
c
c    It should be plotted on [-1,+1] x [-1,+1].
c
c    Note that the Archibald reference incorrectly describes the divisor
c    of x in the second ellipse as 0.06624, when it should be 0.6624.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Rick Archibald, Anne Gelb, Jungho Yoon,
c    Polynomial fitting for edge detection in irregularly sampled signals
c    and images,
c    SIAM Journal on Numerical Analysis,
c    Volume 43, Number 1, 2006, pages 259-279.
c
c    Larry Shepp, Ben Logan,
c    The Fourier reconstruction of a head section,
c    IEEE Transactions on Nuclear Science,
c    Volume  NS-21, June 1974, pages 21-43.
c
c  Parameters:
c
c    Input, integer N, the number of points.
c
c    Input, double precision X(N), Y(N), the arguments.
c
c    Output, double precision F(N), the function values.
c
c  Local parameters:
c
c    Local, integer CHOICE:
c    1, use Archibald's (and Shepp and Logan's) level values;
c    2, use Matlab's level values;
c    3, use Matlab's enhanced contrast level values.
c
      implicit none

      integer n

      double precision c(4)
      double precision c1(4)
      double precision c2(4)
      double precision c3(4)
      integer choice
      double precision eta1
      double precision eta2
      double precision f(n)
      integer i
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision x(n)
      double precision xi1
      double precision xi2
      double precision y(n)

      save c1
      save c2
      save c3

      data c1 /
     &  2.0D+00, -0.98D+00, -0.02D+00, +0.01D+00 /
      data c2 /
     &  1.0D+00, -0.98D+00, -0.02D+00, +0.01D+00 /
      data c3 /
     &  1.0D+00, -0.8D+00, -0.2D+00, +0.1D+00 /

      choice = 3

      if ( choice .eq. 1 ) then
        call r8vec_copy ( 4, c1, c )
      else if ( choice .eq. 2 ) then
        call r8vec_copy ( 4, c2, c )
      else
        call r8vec_copy ( 4, c3, c )
      end if

      do i = 1, n

        f(i) = 0.0D+00

        xi1  =   ( x(i) - 0.22D+00 ) * cos ( 0.4D+00 * r8_pi )          
     &           + y(i)              * sin ( 0.4D+00 * r8_pi )
        eta1 = - ( x(i) - 0.22D+00 ) * sin ( 0.4D+00 * r8_pi )          
     &           + y(i)              * cos ( 0.4D+00 * r8_pi )

        xi2  =   ( x(i) + 0.22D+00 ) * cos ( 0.6D+00 * r8_pi )          
     &           + y(i)              * sin ( 0.6D+00 * r8_pi )
        eta2 = - ( x(i) + 0.22D+00 ) * sin ( 0.6D+00 * r8_pi )          
     &           + y(i)              * cos ( 0.6D+00 * r8_pi )

        if ( ( x(i) / 0.69D+00 )**2 
     &     + ( y(i) / 0.92D+00 )**2 .le. 1.0D+00 ) then
          f(i) = f(i) + c(1)
        end if

        if ( ( x(i) / 0.6624D+00 )**2 
     &    + ( ( y(i) + 0.0184D+00 ) / 0.874D+00 )**2 .le. 1.0D+00 ) 
     &    then
          f(i) = f(i) + c(2)
        end if

        if ( ( xi1 / 0.31D+00 )**2 
     &     + ( eta1 / 0.11D+00 )**2 .le. 1.0D+00 .or.
     &       ( xi2 / 0.41D+00 )**2 
     &     + ( eta2 / 0.16D+00 )**2 .le. 1.0D+00 ) then
          f(i) = f(i) + c(3)
        end if

        if ( 
     &    ( ( ( x(i) - 0.35D+00 )  / 0.3D+00   )**2 
     &    + (   y(i)               / 0.6D+00   )**2 .le. 1.0D+00 ) .or. 
     &    ( (   x(i)               / 0.21D+00  )**2 
     &    + ( ( y(i) - 0.35D+00  ) / 0.25D+00  )**2 .le. 1.0D+00 ) .or. 
     &    ( (   x(i)               / 0.046D+00 )**2 
     &    + ( ( y(i) - 0.1D+00   ) / 0.046D+00 )**2 .le. 1.0D+00 ) .or. 
     &    ( (   x(i)               / 0.046D+00 )**2 
     &    + ( ( y(i) + 0.1D+00   ) / 0.046D+00 )**2 .le. 1.0D+00 ) .or. 
     &    ( ( ( x(i) + 0.08D+00  ) / 0.046D+00 )**2 
     &    + ( ( y(i) + 0.605D+00 ) / 0.023D+00 )**2 .le. 1.0D+00 ) .or. 
     &    ( (   x(i)               / 0.023D+00 )**2 
     &    + ( ( y(i) + 0.605D+00 ) / 0.023D+00 )**2 .le. 1.0D+00 ) .or. 
     &    ( ( ( x(i) - 0.06D+00  ) / 0.023D+00 )**2 
     &    + ( ( y(i) + 0.605D+00 ) / 0.023D+00 )**2 .le. 1.0D+00 ) ) 
     &    then
          f(i) = f(i) + c(4)
        end if
      end do

      return
      end
      subroutine fxy3 ( n, x, y, f )

c*********************************************************************72
c
cc FXY3 is 2D example #3..
c
c  Discussion:
c
c    This is example 3.2 in the reference.
c
c    It is known as the modified two-dimensional Harten function.
c
c    It should be plotted on [-1,+1] x [-1,+1].
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 September 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Rick Archibald, Anne Gelb, Jungho Yoon,
c    Determining the location of discontinuities in the derivatives
c    of functions,
c    Applied Numerical Mathematics,
c    Volume 58, 2008, pages 577-592.
c
c  Parameters:
c
c    Input, integer N, the number of points.
c
c    Input, double precision X(N), Y(N), the arguments.
c
c    Output, double precision F(N), the function values.
c
      implicit none

      integer n

      double precision f(n)
      integer i
      double precision r
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision x(n)
      double precision y(n)

      do i = 1, n

        r = ( 4.0 * x(i) ** 2 + 4.0 * y(i) ** 2 - 1.0D+00 ) / 6.0D+00

        if ( 3.0D+00 * r .le. -1.0D+00 ) then
          f(i) = - r * sin ( 0.5D+00 * r8_pi * r ** 2 )
        else if ( 3.0D+00 * r .lt. 1.0D+00 ) then
          f(i) = abs ( sin ( 2.0D+00 * r8_pi * r ) )
        else
          f(i) = 2.0D+00 * r 
     &      - 1.0D+00 - sin ( 3.0D+00 * r8_pi * r ) / 6.0D+00
        end if

      end do

      return
      end
      subroutine fxy4 ( n, x, y, f )

c*********************************************************************72
c
cc FXY4 is 2D example #4.
c
c  Discussion:
c
c    This is example 3.1 in the reference.
c
c    It is known as the discontinuous medium wave function.
c
c    Here, we are computing the first component of the solution, P(X,Y).
c
c    It should be plotted on (x,y) in [-1,0]x[0,0.1].
c
c    The second variable y actually represents time.
c
c    Note that in the reference, the formula reads:
c     f(i) = 2.0D+00 * rhor * cr / ( rhol * cl + rhor * cr ) &
c          * sin ( r8_pi * omega * ( y(i) - ( x(i) + 0.5D+00 ) / cr ) )
c    but I believe this should be:
c     f(i) = 2.0D+00 * rhor * cr / ( rhol * cl + rhor * cr ) &
c          * sin ( r8_pi * omega * ( y(i) - ( x(i) + 0.5D+00 ) / cl ) )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 September 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Rick Archibald, Anne Gelb, Jungho Yoon,
c    Determining the location of discontinuities in the derivatives
c    of functions,
c    Applied Numerical Mathematics,
c    Volume 58, 2008, pages 577-592.
c
c  Parameters:
c
c    Input, integer N, the number of points.
c
c    Input, double precision X(N), Y(N), the arguments.
c
c    Output, double precision F(N), the function values.
c
      implicit none

      integer n

      double precision cl
      parameter ( cl = 0.87879D+00 )
      double precision cr
      parameter ( cr = 1.0D+00 )
      double precision f(n)
      integer i
      double precision omega
      parameter ( omega = 12.0D+00 )
      double precision rhol
      parameter ( rhol = 0.55556D+00 )
      double precision rhor
      parameter ( rhor = 1.0D+00 )
      double precision r(n)
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision x(n)
      double precision y(n)

      do i = 1, n
        if ( x(i) .le. -0.5D+00 ) then
          f(i) = 
     &      sin ( r8_pi * omega * ( y(i) - ( x(i) + 0.5D+00 ) / cl ) ) 
     &      - ( rhol * cl - rhor * cr ) / ( rhol * cl + rhor * cr )      
     &      * sin ( r8_pi * omega * ( y(i) + ( x(i) + 0.5D+00 ) / cl ) )
        else
          f(i) = 2.0D+00 * rhor * cr / ( rhol * cl + rhor * cr ) 
     &      * sin ( r8_pi * omega * ( y(i) - ( x(i) + 0.5D+00 ) / cl ) )
        end if
      end do

      return
      end
      subroutine fxy5 ( n, x, y, f )

!*********************************************************************72
!
!! FXY5 is 2D example #5.
!
!  Discussion:
!
!    This is example 3.1 in the reference.
!
!    It is known as the discontinuous medium wave function.
!
!    Here, we are computing the second component of the solution, U(X,Y).
!
!    It should be plotted on (x,y) in [-1,0]x[0,0.1].
!
!    The second variable y actually represents time.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 September 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Rick Archibald, Anne Gelb, Jungho Yoon,
!    Determining the location of discontinuities in the derivatives
!    of functions,
!    Applied Numerical Mathematics,
!    Volume 58, 2008, pages 577-592.
!
!  Parameters:
!
!    Input, integer N, the number of points.
!
!    Input, double precision X(N), Y(N), the arguments.
!
!    Output, double precision F(N), the function values.
!
      implicit none

      integer n

      double precision cl
      parameter ( cl = 0.87879D+00 )
      double precision cr
      parameter ( cr = 1.0D+00 )
      double precision f(n)
      integer i
      double precision omega
      parameter ( omega = 12.0D+00 )
      double precision rhol
      parameter ( rhol = 0.55556D+00 )
      double precision rhor
      parameter ( rhor = 1.0D+00 )
      double precision r(n)
      double precision r8_pi
      parameter ( r8_pi = 3.141592653589793D+00 )
      double precision x(n)
      double precision y(n)

      do i = 1, n
        if ( x(i) .le. -0.5D+00 ) then
          f(i) = 
     &      sin ( r8_pi * omega * ( y(i) - ( x(i) + 0.5D+00 ) / cl ) ) 
     &      + ( rhol * cl - rhor * cr ) / ( rhol * cl + rhor * cr ) 
     &      / ( rhol * cl ) 
     &      * sin ( r8_pi * omega * ( y(i) + ( x(i) + 0.5D+00 ) / cl ) )
        else
          f(i) = 2.0D+00 / ( rhol * cl + rhor * cr ) 
     &      * sin ( r8_pi * omega * ( y(i) - ( x(i) + 0.5D+00 ) / cl ) )
        end if
      end do

      return
      end
      subroutine fxyz1 ( n, x, y, z, f )

c*********************************************************************72
c
cc FXYZ1 is 3D example #1.
c
c  Discussion:
c
c    This example is known as the 3D Shepp-Logan phantom.
c
c    It should be plotted on [-1,+1] x [-1,+1.5] x [-1.5,+1.5].
c
c    Seventeen objects are modeled by ellipses of various gray levels,
c    including:
c
c     1: Outer skull
c     2: Inner skull
c     3: Left eye
c     4: Right eye
c     5: Nose
c     6: Mouth
c     7: Left ear
c     8: Right ear
c     9: Left small tumor
c    10: Center small tumor
c    11: Right small tumor
c    12: Old f
c    13: Old g
c    14: Old e
c    15: Right ventricle
c    16: Left ventricle
c    17: Blood clot
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Larry Shepp,
c    Computerized tomography and nuclear magnetic resonance,
c    Journal of Computer Assisted Tomography,
c    Volume 4, Number 1, February 1980, pages 94-107.
c
c  Parameters:
c
c    Input, integer N, the number of points.
c
c    Input, double precision X(N), Y(N), Z(N), the arguments.
c
c    Output, double precision F(N), the function values.
c
      implicit none

      integer n

      double precision a1(17)
      double precision a2(17)
      double precision a3(17)
      double precision c
      double precision f(n)
      double precision g(17)
      integer e
      integer i
      double precision v11(17)
      double precision v12(17)
      double precision v13(17)
      double precision v21(17)
      double precision v22(17)
      double precision v23(17)
      double precision v31(17)
      double precision v32(17)
      double precision v33(17)
      double precision x(n)
      double precision y(n)
      double precision z(n)
      double precision x0(17)
      double precision y0(17)
      double precision z0(17)

      save a1
      save a2
      save a3
      save g
      save v11
      save v12
      save v13
      save v21
      save v22
      save v23
      save v31
      save v32
      save v33
      save x0
      save y0
      save z0

      data a1 /
     &    0.7233,  0.7008,  0.1270,  0.1270,  0.1270, 
     &    0.4575,  0.0635,  0.0635,  0.0460,  0.0230, 
     &    0.0230,  0.0460,  0.2100,  0.1100,  0.1600, 
     &    0.1600,  0.0300 /
      data a2 /
     &    0.9644,  0.9246,  0.1270,  0.1270,  0.3400, 
     &    0.6099,  0.3175,  0.3175,  0.0230,  0.0230, 
     &    0.0460,  0.0460,  0.2581,  0.2500,  0.3100, 
     &    0.4100,  0.2000 /
      data a3 /
     &    1.2700,  1.2241,  0.1270,  0.1270,  0.1700, 
     &    0.5080,  0.3175,  0.3175,  0.0230,  0.0460, 
     &    0.0230,  0.0460,  0.2581,  0.2300,  0.2540, 
     &    0.3810,  0.2000 /
      data g /
     &    2.0000, -0.9800, -1.0000, -1.0000,  1.5000, 
     &   -1.0000,  1.0000,  1.0000,  0.0100,  0.0100, 
     &    0.0100,  0.0100,  0.0100,  0.0100, -0.0200, 
     &   -0.0200,  0.0300 /
      data v11 /
     &    1.0000,  1.0000,  1.0000,  1.0000,  1.0000, 
     &    1.0000,  0.9903, -0.9903,  1.0000,  1.0000, 
     &    1.0000,  1.0000,  1.0000,  1.0000,  0.9511, 
     &   -0.9511,  0.9192 /
       data v12 /
     &    0.0000,  0.0000,  0.0000,  0.0000,  0.0000, 
     &    0.0000, -0.1085, -0.1085,  0.0000,  0.0000, 
     &    0.0000,  0.0000,  0.0000,  0.0000, -0.3090, 
     &   -0.3090, -0.3381 /
      data v13 /
     &    0.0000,  0.0000,  0.0000,  0.0000,  0.0000, 
     &    0.0000, -0.0865, -0.0865,  0.0000,  0.0000, 
     &    0.0000,  0.0000,  0.0000,  0.0000,  0.0000, 
     &    0.0000,  0.2020 /
      data v21 /
     &    0.0000,  0.0000,  0.0000,  0.0000,  0.0000, 
     &    0.0000,  0.1089, -0.1089,  0.0000,  0.0000, 
     &    0.0000,  0.0000,  0.0000,  0.0000,  0.3090, 
     &   -0.3090,  0.3452 /
      data v22 /
     &    1.0000,  1.0000,  1.0000,  1.0000,  0.5446, 
     &    1.0000,  0.9941,  0.9941,  1.0000,  1.0000, 
     &    1.0000,  1.0000,  1.0000,  1.0000,  0.9511, 
     &    0.9511,  0.9385 /
      data v23 /
     &    0.0000,  0.0000,  0.0000,  0.0000, -0.8387, 
     &    0.0000,  0.0000,  0.0000,  0.0000,  0.0000, 
     &    0.0000,  0.0000,  0.0000,  0.0000,  0.0000, 
     &    0.0000,  0.0000 /
      data v31 /
     &    0.0000,  0.0000,  0.0000,  0.0000,  0.0000, 
     &    0.0000,  0.0860, -0.0860,  0.0000,  0.0000, 
     &    0.0000,  0.0000,  0.0000,  0.0000,  0.0000, 
     &    0.0000,  0.1896 /
      data v32 /
     &    0.0000,  0.0000,  0.0000,  0.0000,  0.8387, 
     &    0.0000, -0.0094, -0.0094,  0.0000,  0.0000, 
     &    0.0000,  0.0000,  0.0000,  0.0000,  0.0000, 
     &    0.0000, -0.0697 /
      data v33 /
     &    1.0000,  1.0000,  1.0000,  1.0000,  0.5446, 
     &    1.0000,  0.9963,  0.9963,  1.0000,  1.0000, 
     &    1.0000,  1.0000,  1.0000,  1.0000,  1.0000, 
     &    1.0000, -0.9794 /
      data x0 /
     &    0.0000,  0.0000,  0.2583, -0.2583,  0.0000, 
     &    0.0000,  0.7076, -0.7076, -0.0800,  0.0000, 
     &    0.0600,  0.0000,  0.0000,  0.0000,  0.2200, 
     &   -0.2200,  0.5600 /
      data y0 /
     &    0.0000, -0.0184,  0.7534,  0.7534,  1.1398, 
     &    0.0000, -0.1378, -0.1378, -0.6050, -0.6050, 
     &   -0.6050,  0.1000, -0.1000,  0.3500,  0.0000, 
     &    0.0000, -0.4000 /
      data z0 /
     &    0.0000, -0.0185,  0.0000,  0.0000, -0.1957, 
     &   -0.7620, -0.1905, -0.1905,  0.3810,  0.3810, 
     &    0.3810,  0.3810,  0.1270,  0.3810,  0.3810, 
     &    0.3810,  0.3810 /

      do i = 1, n

        f(i) = 0.0D+00

        do e = 1, 17

          c = ( ( ( x(i) - x0(e) ) * v11(e) 
     &          + ( y(i) - y0(e) ) * v12(e) 
     &          + ( z(i) - z0(e) ) * v13(e) ) / a1(e) )**2 
     &      + ( ( ( x(i) - x0(e) ) * v21(e) 
     &          + ( y(i) - y0(e) ) * v22(e) 
     &          + ( z(i) - z0(e) ) * v23(e) ) / a2(e) )**2 
     &      + ( ( ( x(i) - x0(e) ) * v31(e) 
     &          + ( y(i) - y0(e) ) * v32(e) 
     &          + ( z(i) - z0(e) ) * v33(e) ) / a3(e) )**2

          if ( c .le. 1.0D+00 ) then
            f(i) = f(i) + g(e)
          end if

        end do

      end do

      return
      end
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
      subroutine r8vec_copy ( n, a1, a2 )

c*********************************************************************72
c
cc R8VEC_COPY copies an R8VEC.
c
c  Discussion:
c
c    An R8VEC is a vector of R8 values.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the length of the vectors.
c
c    Input, double precision A1(N), the vector to be copied.
c
c    Output, double precision A2(N), a copy of A1.
c
      implicit none

      integer n

      double precision a1(n)
      double precision a2(n)
      integer i

      do i = 1, n
        a2(i) = a1(i)
      end do

      return
      end
      subroutine r8vec_linspace ( n, a_first, a_last, a )

c*********************************************************************72
c
cc R8VEC_LINSPACE creates a vector of linearly spaced values.
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
c
c    4 points evenly spaced between 0 and 12 will yield 0, 4, 8, 12.
c
c    In other words, the interval is divided into N-1 even subintervals,
c    and the endpoints of intervals are used as the points.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 March 2011
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in the vector.
c
c    Input, double precision A_FIRST, A_LAST, the first and last entries.
c
c    Output, double precision A(N), a vector of linearly spaced data.
c
      implicit none

      integer n

      double precision a(n)
      double precision a_first
      double precision a_last
      integer i

      if ( n .eq. 1 ) then

        a(1) = ( a_first + a_last ) / 2.0D+00

      else

        do i = 1, n
          a(i) = ( dble ( n - i     ) * a_first 
     &           + dble (     i - 1 ) * a_last )
     &           / dble ( n     - 1 )
        end do

      end if

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
