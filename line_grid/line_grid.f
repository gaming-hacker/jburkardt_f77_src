      subroutine line_grid ( n, a, b, c, x )

c*********************************************************************72
c
cc LINE_GRID: grid points over the interior of a line segment in 1D.
c
c  Discussion:
c
c    In 1D, a grid is to be created using N points.
c
c    Over the interval [A,B], we have 5 choices for grid centering:
c      1: 0,   1/3, 2/3, 1
c      2: 1/5, 2/5, 3/5, 4/5
c      3: 0,   1/4, 2/4, 3/4
c      4: 1/4, 2/4, 3/4, 1
c      5: 1/8, 3/8, 5/8, 7/8
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 August 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of points.
c
c    Input, double precision A, B, the endpoints for each dimension.
c
c    Input, integer C, the grid centering for each dimension.
c    1 <= C <= 5.
c
c    Output, double precision X(N) = X(N), the points.
c
      implicit none

      integer n

      double precision a
      double precision b
      integer c
      integer j
      double precision x(n)

      do j = 1, n

        if ( c .eq. 1 ) then
          if ( n .eq. 1 ) then
            x(j) = 0.5D+00 * ( a + b )
          else
            x(j) = (   dble ( n - j     ) * a   
     &               + dble (     j - 1 ) * b ) 
     &               / dble ( n     - 1 )
          end if
        else if ( c .eq. 2 ) then
          x(j) = (   dble ( n - j + 1 ) * a   
     &             + dble (     j     ) * b ) 
     &             / dble ( n     + 1 )
        else if ( c .eq. 3 ) then
          x(j) = (   dble ( n - j + 1 ) * a   
     &             + dble (     j - 1 ) * b ) 
     &             / dble ( n         )
        else if ( c .eq. 4 ) then
          x(j) = (   dble ( n - j ) * a   
     &             + dble (     j ) * b )  
     &             / dble ( n     )
        else if ( c .eq. 5 ) then
          x(j) = (   dble ( 2 * n - 2 * j + 1 ) * a   
     &             + dble (         2 * j - 1 ) * b ) 
     &             / dble ( 2 * n             )
        end if
 
      end do

      return
      end
      subroutine r8vec_print ( n, a, title )

c*********************************************************************72
c
cc R8VEC_PRINT prints an R8VEC.
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
c    12 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of components of the vector.
c
c    Input, double precision A(N), the vector to be printed.
c
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer n

      double precision a(n)
      integer i
      character * ( * ) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i8,a,1x,g16.8)' ) i, ':', a(i)
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
