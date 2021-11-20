      subroutine dcopy ( n, dx, incx, dy, incy )

c*********************************************************************72
c
cc dcopy() copies a vector.
c
c  Discussion:
c
c    This routine uses double precision real arithmetic.
c
c    The routine uses unrolled loops for increments equal to one.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 July 2007
c
c  Author:
c
c    Jack Dongarra
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
c    Input, integer N, the number of elements in DX and DY.
c
c    Input, double precision DX(*), the first vector.
c
c    Input, integer INCX, the increment between successive entries of DX.
c
c    Output, double precision DY(*), the second vector.
c
c    Input, integer INCY, the increment between successive entries of DY.
c
      implicit none

      double precision dx(*)
      double precision dy(*)
      integer i
      integer incx
      integer incy
      integer ix
      integer iy
      integer m
      integer n

      if ( n .le. 0 ) then
        return
      end if

      if ( incx .ne. 1 .or. incy .ne. 1 ) then

        ix = 1
        iy = 1
        if(incx.lt.0)ix = (-n+1)*incx + 1
        if(incy.lt.0)iy = (-n+1)*incy + 1
        do i = 1, n
          dy(iy) = dx(ix)
          ix = ix + incx
          iy = iy + incy
        end do

      else

        m = mod(n,7)

        do i = 1,m
          dy(i) = dx(i)
        end do

        do i = m + 1, n, 7
          dy(i) = dx(i)
          dy(i + 1) = dx(i + 1)
          dy(i + 2) = dx(i + 2)
          dy(i + 3) = dx(i + 3)
          dy(i + 4) = dx(i + 4)
          dy(i + 5) = dx(i + 5)
          dy(i + 6) = dx(i + 6)
        end do

      end if

      return
      end
