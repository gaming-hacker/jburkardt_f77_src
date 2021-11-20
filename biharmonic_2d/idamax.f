      function idamax ( n, dx, incx )

c*********************************************************************72
c
cc idamax() finds the index of element having maximum absolute value.
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
c    Input, integer N, the number of entries in the vector.
c
c    Input, double precision X(*), the vector to be examined.
c
c    Input, integer INCX, the increment between successive entries of SX.
c
c    Output, integer IDAMAX, the index of the element of SX of maximum
c    absolute value.
c
      implicit none

      double precision dmax
      double precision dx(*)
      integer i
      integer idamax
      integer incx
      integer ix
      integer n

      idamax = 0
      if ( n .lt. 1 .or. incx .le. 0 ) then
        return
      end if

      idamax = 1
      if ( n .eq. 1 ) then
        return
      end if

      if ( incx .eq. 1 ) then

        dmax = dabs ( dx(1) )
        do i = 2, n
          if( dmax .lt. dabs ( dx(i) ) ) then
            idamax = i
            dmax = dabs ( dx(i) )
          end if
        end do

      else

        ix = 1
        dmax = dabs ( dx(1) )
        ix = ix + incx
        do  i = 2, n
          if ( dmax .lt. dabs ( dx(ix) ) ) then
            idamax = i
            dmax = dabs ( dx(ix) )
          end if
          ix = ix + incx
        end do

      end if

      return
      end

