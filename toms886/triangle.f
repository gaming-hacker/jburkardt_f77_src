      program main

c*********************************************************************72
c
cc MAIN is the main program for TRIANGLE.
c
c  Discussion:
c
c    This driver computes the interpolation of the Franke function
c    on the triangle T(U,V,W) with vertices U=(U1,U2)=(0,0), 
c    V=(V1,V2)=(1,0) and W=(W1,W2)=(0,1) (unit triangle) 
c    at the first family of Padua points. 
c
c    The degree of interpolation is DEG = 60 and the number of target 
c    points is NTG = NTG1 ** 2 - NTG1 + 1, NTG1 = 100.
c
c    The maps from the reference square [-1,1]^2 to the triangle
c    are SIGMA1 and SIGMA2 with inverses ISIGM1 and ISIGM2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c  
c  Modified:
c
c    12 February 2014
c
c  Author:
c
c    Original FORTRAN77 version by Marco Caliari, Stefano De Marchi, 
c    Marco Vianello.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Marco Caliari, Stefano de Marchi, Marco Vianello,
c    Algorithm 886:
c    Padua2D: Lagrange Interpolation at Padua Points on Bivariate Domains,
c    ACM Transactions on Mathematical Software,
c    Volume 35, Number 3, October 2008, Article 21, 11 pages.
c
c  Parameters:
c
c    Local, integer DEGMAX, the maximum degree of interpolation.
c
c    Local, integer NPDMAX, the maximum number of Padua points
c    = (DEGMAX + 1) * (DEGMAX + 2) / 2.
c
c    Local, integer NTG1MX, the maximum value of the parameter determining 
c    the number of target points.
c
c    Local, integer NTGMAX, the maximum number of target points,
c    dependent on NTG1MX.
c
c    Local, integer DEG, the degree of interpolation.
c
c    Local, integer NTG1, the parameter determining the number of target points.
c
c    Local, integer NPD, the number of Padua points = (DEG + 1) * (DEG + 2) / 2.
c
c    Local, integer NTG, the number of target points, dependent on NTG1.
c
c    Local, double precision PD1(NPDMAX), the first coordinates of 
c    the Padua points.
c
c    Local, double precision PD2(NPDMAX), the second coordinates of the 
c    Padua points.
c
c    Local, double precision WPD(NPDMAX), the weights.
c
c    Local, double precision FPD(NPDMAX), the function at the Padua points.
c
c    Workspace, double precision RAUX1(DEGMAX+1)*(DEGMAX+2)).
c
c    Workspace, double precision RAUX2(DEGMAX+1)*(DEGMAX+2)).
c
c    Local, double precision C0(0:DEGMAX+1,0:DEGMAX+1), the coefficient matrix.
c
c    Local, double precision TG1(NTGMAX), the first coordinates of the 
c    target points.
c
c    Local, double precision TG2(NTGMAX), the second coordinates of the 
c    target points.
c
c    Local, double precision INTFTG(NTGMAX), the values of the 
c    interpolated function.
c
c    Local, double precision MAXERR, the maximum norm of the error at target 
c    points.
c
c    Local, double precision ESTERR, the estimated error.
c
      implicit none

      integer degmax
      parameter ( degmax = 60 )
      integer ntg1mx
      parameter ( ntg1mx = 100 )

      integer npdmax
      parameter ( npdmax = ( degmax + 1 ) * ( degmax + 2 ) / 2 )
      integer ntgmax
      parameter ( ntgmax = ntg1mx ** 2 - ntg1mx + 1 )

      double precision c0(0:degmax+1,0:degmax+1)
      integer deg
      double precision esterr
      integer family
      character * ( 255 ) filename
      double precision fmax
      double precision fmin
      double precision fpd(npdmax)
      double precision franke
      double precision fxy
      integer i
      double precision intftg(ntgmax)
      double precision isigm1
      double precision isigm2
      double precision ixy
      double precision maxdev
      double precision maxerr
      double precision mean
      integer npd
      integer ntg
      integer ntg1
      double precision pd1(npdmax)
      double precision pd2(npdmax)
      double precision pd2val
      double precision r8_huge
      double precision raux1((degmax+1)*(degmax+2))
      double precision raux2((degmax+1)*(degmax+2))
      double precision sigma1
      double precision sigma2
      double precision tg1(ntgmax)
      double precision tg2(ntgmax)
      double precision u1
      double precision u2
      double precision v1
      double precision v2
      double precision w1
      double precision w2
      double precision wpd(npdmax)
      double precision x
      double precision y

      u1 = 0.0D+00
      u2 = 0.0D+00
      v1 = 1.0D+00
      v2 = 0.0D+00
      w1 = 0.0D+00
      w2 = 1.0D+00
      family = 1
      deg = 60
      ntg1 = 100

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TRIANGLE:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Interpolation of the Franke function'
      write ( *, '(a)' ) '  on the unit triangle T((0,0),(1,0),(0,1))'
      write ( *, '(a,i4)' ) '  at degree = ', deg

      if ( degmax .lt. deg ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'TRIANGLE - Fatal error!'
        write ( *, '(a)' ) '  DEGMAX < DEG.'
        write ( *, '(a,i4)' ) '  DEG =    ', deg
        write ( *, '(a,i4)' ) '  DEGMAX = ', degmax
        stop 1
      end if
c
c  Build the first family of Padua points in the square [-1,1]^2
c     
      call pdpts ( deg, pd1, pd2, wpd, npd )
c     
c  Compute the Franke function at Padua points mapped to T(U,V,W).
c   
      do i = 1, npd
        x = sigma1 ( pd1(i), pd2(i), u1, u2, v1, v2, w1, w2 )
        y = sigma2 ( pd1(i), pd2(i), u1, u2, v1, v2, w1, w2 )
        fpd(i) = franke ( x, y )
      end do
c
c  Write X, Y, F(X,Y) to a file.
c
      filename = 'triangle_fpd.txt'
      open ( unit = 10, file = filename, status = 'replace' )
      do i = 1, npd
        x = sigma1 ( pd1(i), pd2(i), u1, u2, v1, v2, w1, w2 )
        y = sigma2 ( pd1(i), pd2(i), u1, u2, v1, v2, w1, w2 )
        write ( 10, '(3g14.6)' ) x, y, fpd(i)
      end do
      close ( unit = 10 )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Wrote F(x,y) at Padua points in "' 
     &  // trim ( filename ) // '".'
c     
c  Compute the matrix C0 of the coefficients in the bivariate
c  orthonormal Chebyshev basis
c     
      call padua2 ( deg, degmax, npd, wpd, fpd, raux1, raux2, c0, 
     &  esterr )
c     
c  Build the set of target points on T(U,V,W)
c     
      call target ( u1, u2, v1, v2, w1, w2, ntg1, ntgmax, tg1, tg2, 
     &  ntg )
c     
c  Evaluate the interpolant at the target points.
c    
      do i = 1, ntg
        x = isigm1 ( tg1(i), tg2(i), u1, u2, v1, v2, w1, w2 )
        y = isigm2 ( tg1(i), tg2(i), u1, u2, v1, v2, w1, w2 )
        intftg(i) = pd2val ( deg, degmax, c0, x, y, raux1, raux2 )
      end do
c
c  Write the function value at target points to a file.
c
      filename = 'triangle_ftg.txt'
      open ( unit = 10, file = filename, status = 'replace' )
      do i = 1, ntg
        write ( 10, '(3g14.6)' ) 
     &    tg1(i), tg2(i), franke ( tg1(i), tg2(i) )
      end do
      close ( unit = 10 )
      write ( *, '(a)' ) '  Wrote F(x,y) at target points in "' 
     &  // trim ( filename ) // '".'
c
c  Write the interpolated function value at target points to a file.
c
      filename = 'triangle_itg.txt'
      open ( unit = 10, file = filename, status = 'replace' )
      do i = 1, ntg
        write ( 10, '(3g14.6)' ) tg1(i), tg2(i), intftg(i)
      end do
      close ( unit = 10 )
      write ( *, '(a)' ) '  Wrote I(F)(x,y) at target points in "' 
     &  // trim ( filename ) // '".'
c
c  Compute the error relative to the max deviation from the mean.
c     
      maxerr = 0.0D+00
      mean = 0.0D+00
      fmax = - r8_huge ( )
      fmin = + r8_huge ( )

      do i = 1, ntg
        fxy = franke ( tg1(i), tg2(i) )
        ixy = intftg(i)
        maxerr = max ( maxerr, abs ( fxy - ixy ) )
        mean = mean + fxy
        fmax = max ( fmax, fxy )
        fmin = min ( fmin, fxy )
      end do
 
      if ( fmax .eq. fmin ) then
        maxdev = 1.0D+00
      else
        mean = mean / dble ( ntg )
        maxdev = max ( fmax - mean, mean - fmin )
      end if
c
c  Print error ratios.
c
      write ( *, '(a)' ) ''
      write ( *, '(a,e10.4)' ) '  Estimated error:  ', esterr / maxdev
      write ( *, '(a,e10.4)' ) '  Actual error:     ', maxerr / maxdev
      write ( *, '(a,e10.4)' ) '  Expected error:   ', 0.1226D-09
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TRIANGLE:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      function sigma1 ( t1, t2, u1, u2, v1, v2, w1, w2 )

c*********************************************************************72
c
cc SIGMA1 maps first coordinate from square to triangle.
c
c  Discussion:
c
c    This function returns the first component of the map
c    from the square [-1,1]^2 to the triangle T(U,V,W). 
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c  
c  Modified:
c
c    11 February 2014
c
c  Author:
c
c    Original FORTRAN77 version by Marco Caliari, Stefano De Marchi, 
c    Marco Vianello.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Marco Caliari, Stefano de Marchi, Marco Vianello,
c    Algorithm 886:
c    Padua2D: Lagrange Interpolation at Padua Points on Bivariate Domains,
c    ACM Transactions on Mathematical Software,
c    Volume 35, Number 3, October 2008, Article 21, 11 pages.
c
c  Parameters:
c
c    Input, double precision T1, T2, the coordinates of a point in the square.
c
c    Input, double precision U1, U2, V1, V2, W1, W2, the coordinates of the 
c    vertices of the triangle.
c
c    Output, double precision SIGMA1, the X coordinate of the corresponding
c    point in the triangle.
c
      implicit none

      double precision sigma1
      double precision t1
      double precision t2
      double precision u1
      double precision u2
      double precision v1
      double precision v2
      double precision w1
      double precision w2

      sigma1 = ( v1 - u1 ) * ( 1.0D+00 + t1 ) 
     &  * ( 1.0D+00 - t2 ) / 4.0D+00 
     &  + ( w1 - u1 ) * ( 1.0D+00 + t2 ) / 2.0D+00 + u1

      return
      end
      function isigm1 ( sigma1, sigma2, u1, u2, v1, v2, w1, w2 )

c*********************************************************************72
c
cc ISIGM1 maps first coordinate from triangle to the square.
c
c  Discussion:
c
c    This functions returns the first component of the map
c    from the the triangle T(U,V,W) to the square [-1,1]^2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c  
c  Modified:
c
c    11 February 2014
c
c  Author:
c
c    Original FORTRAN77 version by Marco Caliari, Stefano De Marchi, 
c    Marco Vianello.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Marco Caliari, Stefano de Marchi, Marco Vianello,
c    Algorithm 886:
c    Padua2D: Lagrange Interpolation at Padua Points on Bivariate Domains,
c    ACM Transactions on Mathematical Software,
c    Volume 35, Number 3, October 2008, Article 21, 11 pages.
c
c  Parameters:
c
c    Input, double precision SIGMA1, SIGMA2, the coordinates of a point 
c    in the triangle.
c
c    Input, double precision U1, U2, V1, V2, W1, W2, the coordinates of the 
c    vertices of the triangle.
c
c    Output, double precision ISIGM1, the X coordinate of the corresponding
c    point in the square.
c
      implicit none

      double precision isigm1
      double precision rho1
      double precision rho2
      double precision sigma1
      double precision sigma2
      double precision u1
      double precision u2
      double precision v1
      double precision v2
      double precision w1
      double precision w2

      rho1 = ( sigma1 * ( w2 - u2 ) - sigma2 * ( w1 - u1 ) 
     &  + ( w1 - u1 ) * u2 - ( w2 - u2 ) * u1 ) /
     &  ( ( v1 - u1 ) * ( w2 - u2 ) - ( v2 - u2 ) * ( w1 - u1 ) )

      rho2 = ( sigma1 * ( v2 - u2 ) - sigma2 * ( v1 - u1 ) 
     &  + ( v1 - u1 ) * u2 - ( v2 - u2 ) * u1 ) /
     &  ( ( w1 - u1 ) * ( v2 - u2 ) - ( w2 - u2 ) * ( v1 - u1 ) )

      if ( rho2 .eq. 1.0D+00 ) then
        isigm1 = 0.0D+00
      else
        isigm1 = 2.0D+00 * rho1 / ( 1.0D+00 - rho2 ) - 1.0D+00
      end if  
 
      return
      end
      function sigma2 ( t1, t2, u1, u2, v1, v2, w1, w2 )

c*********************************************************************72
c
cc SIGMA2 maps the second coordinate from square to triangle.
c
c  Discussion:
c
c    This functions returns the second component of the map
c    from the square [-1,1]^2 to the triangle T(U,V,W).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c  
c  Modified:
c
c    11 February 2014
c
c  Author:
c
c    Original FORTRAN77 version by Marco Caliari, Stefano De Marchi, 
c    Marco Vianello.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Marco Caliari, Stefano de Marchi, Marco Vianello,
c    Algorithm 886:
c    Padua2D: Lagrange Interpolation at Padua Points on Bivariate Domains,
c    ACM Transactions on Mathematical Software,
c    Volume 35, Number 3, October 2008, Article 21, 11 pages.
c
c  Parameters:
c
c    Input, double precision T1, T2, the coordinates of a point in the square.
c
c    Input, double precision U1, U2, V1, V2, W1, W2, the coordinates of the 
c    vertices of the triangle.
c
c    Output, double precision SIGMA2, the Y coordinate of the corresponding
c    point in the triangle.
c
      implicit none

      double precision sigma2
      double precision t1
      double precision t2
      double precision u1
      double precision u2
      double precision v1
      double precision v2
      double precision w1
      double precision w2

      sigma2 = ( v2 - u2 ) * ( 1.0D+00 + t1 ) 
     &  * ( 1.0D+00 - t2 ) / 4.0D+00 + ( w2 - u2 ) 
     &  * ( 1.0D+00 + t2 ) / 2.0D+00 + u2

      return
      end
      function isigm2 ( sigma1, sigma2, u1, u2, v1, v2, w1, w2 )

c*********************************************************************72
c
cc ISIGM2 maps second coordinate from triangle to the square.
c
c  Discussion:
c
c    This functions returns the second component of the map
c    from the the triangle T(U,V,W) to the square [-1,1]^2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c  
c  Modified:
c
c    11 February 2014
c
c  Author:
c
c    Original FORTRAN77 version by Marco Caliari, Stefano De Marchi, 
c    Marco Vianello.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Marco Caliari, Stefano de Marchi, Marco Vianello,
c    Algorithm 886:
c    Padua2D: Lagrange Interpolation at Padua Points on Bivariate Domains,
c    ACM Transactions on Mathematical Software,
c    Volume 35, Number 3, October 2008, Article 21, 11 pages.
c
c  Parameters:
c
c    Input, double precision SIGMA1, SIGMA2, the coordinates of a point 
c    in the ellipse.
c
c    Input, double precision U1, U2, V1, V2, W1, W2, the coordinates of the 
c    vertices of the triangle.
c
c    Output, double precision ISIGM2, the Y coordinate of the corresponding
c    point in the triangle.
c
      implicit none

      double precision isigm2
      double precision rho2
      double precision sigma1
      double precision sigma2
      double precision u1
      double precision u2
      double precision v1
      double precision v2
      double precision w1
      double precision w2

      rho2 = ( sigma1 * ( v2 - u2 ) - 
     &  sigma2 * ( v1 - u1) + ( v1 - u1 ) * u2 - ( v2 - u2 ) * u1 ) /
     &  ( ( w1 - u1 ) * ( v2 - u2 ) - ( w2 - u2 ) * ( v1 - u1 ) )

      if ( rho2 .eq. 1.0D+00 ) then
        isigm2 = 1.0D+00
      else
        isigm2 = 2.0D+00 * rho2 - 1.0D+00
      end if 
  
      return
      end
      subroutine target ( u1, u2, v1, v2, w1, w2, ntg1, ntgmax, tg1, 
     &  tg2, ntg )

c*********************************************************************72
c
cc TARGET returns the target points on the triangle.
c
c  Discussion:
c
c    Target points on the triangle T(U,V,W).
c    The number of target points is NTG = NTG1^2 - NGT1 + 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c  
c  Modified:
c
c    11 February 2014
c
c  Author:
c
c    Original FORTRAN77 version by Marco Caliari, Stefano De Marchi, 
c    Marco Vianello.
c    This FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Marco Caliari, Stefano de Marchi, Marco Vianello,
c    Algorithm 886:
c    Padua2D: Lagrange Interpolation at Padua Points on Bivariate Domains,
c    ACM Transactions on Mathematical Software,
c    Volume 35, Number 3, October 2008, Article 21, 11 pages.
c
c  Parameters:
c
c    Input, double precision U1, U2, V1, V2, W1, W2, the coordinates of the 
c    vertices of the triangle.
c
c    Input, integer NTG1, a parameter determining the number 
c    of target points
c
c    Input, integer NTGMAX, the maximum number of target points.
c
c    Output, double precision TG1(NTG), TG2(NTG), the X and Y coordinates
c    of the target points.
c
c    Output, integer NTG, the number of target points computed.
c
      implicit none

      integer ntgmax

      integer i
      integer j
      integer ntg
      integer ntg1
      double precision tg1(ntgmax)
      double precision tg2(ntgmax)
      double precision u1
      double precision u2
      double precision v1
      double precision v2
      double precision w1
      double precision w2

      if ( ntg1 .lt. 2 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'TARGET - Fatal error!'
        write ( *, '(a)' ) '  NTG1 < 2'
        write ( *, '(a,i4)' ) '  NTG1 = ', ntg1
        stop 1
      end if  
   
      if ( ntgmax .lt. ntg1 ** 2 - ntg1 + 1 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'TARGET - Fatal error!'
        write ( *, '(a)' ) '  NTGMAX < NTG1 * NTG1 - NTG1 + 1.'
        write ( *, '(a,i4)' ) '  NTG1 = ', ntg1
        write ( *, '(a,i4)' ) '  NTGMAX = ', ntgmax
        stop 1
      end if    
    
      ntg = 0
      do i = 1, ntg1 - 1
        do j = 1, ntg1

          ntg = ntg + 1

          tg1(ntg) = ( v1 - u1 ) * dble ( i - 1 ) / dble ( ntg1 - 1 )  
     &      + ( w1 - u1 ) * ( dble ( j - 1 ) / dble ( ntg1 - 1 ) ) 
     &      * ( 1.0D+00 - dble ( i - 1 ) / dble ( ntg1 - 1 ) ) + u1

          tg2(ntg) = ( v2 - u2 ) * dble ( i - 1 ) / dble ( ntg1 - 1 ) 
     &      + ( w2 - u2 ) * ( dble ( j - 1 ) / dble ( ntg1 - 1 ) ) 
     &      * ( 1.0D+00 - dble ( i - 1 ) / dble ( ntg1 - 1 ) ) + u2

        end do
      end do

      i = ntg1
      j = 1
      ntg = ntg + 1

      tg1(ntg) = ( v1 - u1 ) * dble ( i - 1 ) / dble ( ntg1 - 1 ) 
     &  + ( w1 - u1 ) * ( dble ( j - 1 ) / dble ( ntg1 - 1 ) ) 
     &  * ( 1.0D+00 - dble ( i - 1 ) / dble ( ntg1 - 1 ) ) + u1

      tg2(ntg) = ( v2 - u2 ) * dble ( i - 1 ) / dble ( ntg1 - 1 ) 
     &  + ( w2 - u2 ) * ( dble ( j - 1 ) / dble ( ntg1 - 1 ) ) 
     &  * ( 1.0D+00 - dble ( i - 1 ) / dble ( ntg1 - 1 ) ) + u2

      return
      end
