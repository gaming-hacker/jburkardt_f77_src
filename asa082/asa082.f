      subroutine detq ( a, n, d, ifault )

c*********************************************************************72
c
cc detq computes the determinant of an orthogonal matrix.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 January 2020
c
c  Author:
c
c    Original FORTRAN77 version by J C Gower.
c    This version by John Burkardt
c
c  Reference:
c
c    J C Gower,
c    Algorithm AS 82:
c    The determinant of an orthogonal matrix,
c    Applied Statistics,
c    Volume 24, Number 1, 1975, page 150-153.
c
c  Input:
c
c    real A(N,N), the orthogonal matrix stored by rows or columns.
c
c    integer N, the order of the matrix.
c
c  Output:
c
c    real D, the determinant of A.
c
c    integer IFAULT, 
c    0, no error occurred.
c    1, an error was detected.
c
      implicit none

      integer n

      double precision a(n,n)
      double precision a2(n*n)
      double precision d
      integer i
      integer ifault
      integer j
      integer k
      integer p
      integer q
      integer r
      integer s
      double precision tol
      double precision x
      double precision y

      ifault = 0
      tol = 0.0001D+00

      if ( n <= 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'detq - Fatal error!'
        write ( *, '(a)' ) '  n <= 0'
        ifault = 1
        return
      end if

      k = 0
      do i = 1, n
        do j = 1, n
          k = k + 1
          a2(k) = a(i,j)
        end do
      end do

      d = 1.0D+00
      r = 1

      do k = 2, n + 1

        q = r
        x = a2(r)
        y = sign ( 1.0D+00, x )
        d = d * y
        y = - 1.0D+00 / ( x + y )
        x = abs ( x ) - 1.0D+00

        if ( tol .lt. abs ( x ) ) then

          if ( 0.0D+00 < x ) then
            write ( *, '(a)' ) ''
            write ( *, '(a)' ) 'detq - Fatal error!'
            write ( *, '(a)' ) '  x < 0.0'
            write ( *, '(a,g14.6)' ) '  x = ', x
            ifault = 1
            return
          end if

          if ( k == n + 1 ) then
            write ( *, '(a)' ) ''
            write ( *, '(a)' ) 'detq - Fatal error!'
            write ( *, '(a)' ) '  k == n + 1'
            ifault = 1
            return
          end if

          do i = k, n
            q = q + n
            x = a2(q) * y
            p = r
            s = q
            do j = k, n
              p = p + 1
              s = s + 1
              a2(s) = a2(s) + x * a2(p)
            end do

          end do

        end if

        r = r + n + 1

      end do

      return
      end
      subroutine r8mat_print ( m, n, a, title )

c*********************************************************************72
c
cc R8MAT_PRINT prints an R8MAT.
c
c  Discussion:
c
c    An R8MAT is an array of R8's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 May 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the number of rows in A.
c
c    Input, integer N, the number of columns in A.
c
c    Input, double precision A(M,N), the matrix.
c
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer m
      integer n

      double precision a(m,n)
      character * ( * ) title

      call r8mat_print_some ( m, n, a, 1, 1, m, n, title )

      return
      end
      subroutine r8mat_print_some ( m, n, a, ilo, jlo, ihi, jhi,
     &  title )

c*********************************************************************72
c
cc R8MAT_PRINT_SOME prints some of an R8MAT.
c
c  Discussion:
c
c    An R8MAT is an array of R8's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns.
c
c    Input, double precision A(M,N), an M by N matrix to be printed.
c
c    Input, integer ILO, JLO, the first row and column to print.
c
c    Input, integer IHI, JHI, the last row and column to print.
c
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer incx
      parameter ( incx = 5 )
      integer m
      integer n

      double precision a(m,n)
      character * ( 14 ) ctemp(incx)
      integer i
      integer i2hi
      integer i2lo
      integer ihi
      integer ilo
      integer inc
      integer j
      integer j2
      integer j2hi
      integer j2lo
      integer jhi
      integer jlo
      character * ( * ) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )

      if ( m .le. 0 .or. n .le. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  (None)'
        return
      end if

      do j2lo = max ( jlo, 1 ), min ( jhi, n ), incx

        j2hi = j2lo + incx - 1
        j2hi = min ( j2hi, n )
        j2hi = min ( j2hi, jhi )

        inc = j2hi + 1 - j2lo

        write ( *, '(a)' ) ' '

        do j = j2lo, j2hi
          j2 = j + 1 - j2lo
          write ( ctemp(j2), '(i7,7x)') j
        end do

        write ( *, '(''  Col   '',5a14)' ) ( ctemp(j), j = 1, inc )
        write ( *, '(a)' ) '  Row'
        write ( *, '(a)' ) ' '

        i2lo = max ( ilo, 1 )
        i2hi = min ( ihi, m )

        do i = i2lo, i2hi

          do j2 = 1, inc

            j = j2lo - 1 + j2

            write ( ctemp(j2), '(g14.6)' ) a(i,j)

          end do

          write ( *, '(i5,a,5a14)' ) i, ':', ( ctemp(j), j = 1, inc )

        end do

      end do

      return
      end
      subroutine timestamp ( )

c*********************************************************************72
c
cc timestamp prints out the current YMDHMS date as a timestamp.
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
c    John Burkardt
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
     &  d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, 
     &  trim ( ampm )

      return
      end
