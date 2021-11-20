      subroutine dscal ( n, da, dx, incx )

c*********************************************************************72
c
cc dscal() scales a vector by a constant.
c
c  Discussion:
c
c    This routine uses double precision real arithmetic.
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
c    This version by John Burkardt.
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
c    Input, integer N, the number of entries in the vector.
c
c    Input, double precision SA, the multiplier.
c
c    Input/output, double precision X(*), the vector to be scaled.
c
c    Input, integer INCX, the increment between successive entries of X.
c
      implicit none

      double precision da
      double precision dx(*)
      integer i
      integer incx
      integer m
      integer n
      integer nincx

      if ( n .le. 0 ) then
        return
      end if

      if ( incx .le. 0 ) then
        return
      end if

      if ( incx .eq. 1 ) then

        m = mod ( n, 5 )

        do i = 1, m
          dx(i) = da * dx(i)
        end do

        do i = m + 1, n, 5
          dx(i) =     da * dx(i)
          dx(i+1) = da * dx(i+1)
          dx(i+2) = da * dx(i+2)
          dx(i+3) = da * dx(i+3)
          dx(i+4) = da * dx(i+4)
        end do

      else

        nincx = n * incx
        do i = 1, nincx, incx
          dx(i) = da * dx(i)
        end do
      end if

      return
      end

