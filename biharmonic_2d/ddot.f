      function ddot ( n, dx, incx, dy, incy )

c*********************************************************************72
c
cc ddot() forms the dot product of two vectors.
c
c  Discussion:
c
c    This routine uses double precision real arithmetic.
c
c    This routine uses unrolled loops for increments equal to one.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c    
c  Modified:
c
c    09 February 2014
c
c  Author:
c
c    Original FORTRAN77 version by Jack Dongarra.
c    This version by John Burkardt
c
c  Reference:
c
c    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
c    LINPACK User's Guide,
c    SIAM, 1979,
c    ISBN13: 978-0-898711-72-1,
c    LC: QA214.L56.
c
c    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
c    Basic Linear Algebra Subprograms for FORTRAN usage,
c    ACM Transactions on Mathematical Software,
c    Volume 5, Number 3, pages 308-323, 1979.
c
c  Parameters:
c
c    Input, integer N, the number of entries in the vectors.
c
c    Input, double precision DX(*), the first vector.
c
c    Input, integer INCX, the increment between successive entries in DX.
c
c    Input, double precision DY(*), the second vector.
c
c    Input, integer INCY, the increment between successive entries in DY.
c
c    Output, double precision DDOT, the sum of the product of the 
c    corresponding entries of DX and DY.
c
      implicit none

      double precision ddot
      double precision dx(*)
      double precision dy(*)
      double precision dtemp
      integer i
      integer incx
      integer incy
      integer ix
      integer iy
      integer m
      integer n

      ddot = 0.0D+00
      dtemp = 0.0D+00

      if ( n .le. 0 ) then
        return
      end if

      if ( incx .eq. 1 .and. incy .eq. 1 ) then

        m = mod ( n, 5 )

        do i = 1, m
          dtemp = dtemp + dx(i) * dy(i)
        end do

        do i = m + 1, n, 5
          dtemp = dtemp 
     &      + dx(i)   * dy(i) 
     &      + dx(i+1) * dy(i+1) 
     &      + dx(i+2) * dy(i+2) 
     &      + dx(i+3) * dy(i+3) 
     &      + dx(i+4) * dy(i+4)
        end do
c
c  Code for unequal increments or equal increments not equal to 1
c
      else

        if ( incx .lt. 0 ) then
          ix = ( - n + 1 ) * incx + 1
        else
          ix = 1
        end if

        if ( incy .lt. 0 ) then
          iy = ( - n + 1 ) * incy + 1
        else
          iy = 1
        end if

        do i = 1, n
          dtemp = dtemp + dx(ix) * dy(iy)
          ix = ix + incx
          iy = iy + incy
        end do

      end if

      ddot = dtemp

      return
      end
