      function i4_huge ( )

c*********************************************************************72
c
cc I4_HUGE returns a "huge" I4.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 November 2006
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, integer I4_HUGE, a huge number.
c
      implicit none

      integer i4_huge

      i4_huge = 2147483647

      return
      end
      subroutine i4mat_shortest_path ( n, m )

c*********************************************************************72
c
cc I4MAT_SHORTEST_PATH computes the shortest distance between all pairs of points.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 March 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Robert Floyd,
c    Algorithm 97, Shortest Path,
c    Communications of the ACM,
c    Volume 5, Number 6, June 1962, page 345.
c
c  Parameters:
c
c    Input, integer N, the number of points.
c
c    Input/output, integer M(N,N).
c    On input, M(I,J) contains the length of the direct link between
c    nodes I and J, or HUGE if there is no direct link.
c    On output, M(I,J) contains the distance between nodes I and J,
c    that is, the length of the shortest path between them.  If there
c    is no such path, then M(I,J) will remain HUGE.
c
      implicit none

      integer n

      integer i
      integer i4_huge
      integer i4_inf
      integer j
      integer k
      integer m(n,n)
      integer s

      i4_inf = i4_huge ( )

      do i = 1, n
        do j = 1, n
          if ( m(j,i) .lt. i4_inf ) then
            do k = 1, n
              if ( m(i,k) .lt. i4_inf ) then
                s = m(j,i) + m(i,k)
                if ( s .lt. m(j,k) ) then
                  m(j,k) = s
                end if
              end if
            end do
          end if
        end do
      end do

      return
      end
      function r8_huge ( )

c*********************************************************************72
c
cc R8_HUGE returns a "huge" R8.
c
c  Discussion:
c
c    The value returned by this function is NOT required to be the
c    maximum representable R8.  This value varies from machine to machine,
c    from compiler to compiler, and may cause problems when being printed.
c    We simply want a "very large" but non-infinite number.
c
c    FORTRAN90 provides a built-in routine HUGE ( X ) that
c    can return the maximum representable number of the same datatype
c    as X, if that is what is really desired.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 April 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, double precision R8_HUGE, a huge number.
c
      implicit none

      double precision r8_huge

      r8_huge = 1.0D+30

      return
      end
      subroutine r8mat_shortest_path ( n, m )

c*********************************************************************72
c
cc R8MAT_SHORTEST_PATH computes the shortest distance between all pairs of points.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 March 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Robert Floyd,
c    Algorithm 97, Shortest Path,
c    Communications of the ACM,
c    Volume 5, Number 6, June 1962, page 345.
c
c  Parameters:
c
c    Input, integer N, the number of points.
c
c    Input/output, double precision M(N,N).
c    On input, M(I,J) contains the length of the direct link between
c    nodes I and J, or HUGE if there is no direct link.
c    On output, M(I,J) contains the distance between nodes I and J,
c    that is, the length of the shortest path between them.  If there
c    is no such path, then M(I,J) will remain HUGE.
c
      implicit none

      integer n

      integer i
      integer j
      integer k
      double precision m(n,n)
      double precision r8_huge
      double precision r8_inf
      double precision s

      r8_inf = r8_huge ( )

      do i = 1, n
        do j = 1, n
          if ( m(j,i) .lt. r8_inf ) then
            do k = 1, n
              if ( m(i,k) .lt. r8_inf ) then
                s = m(j,i) + m(i,k)
                if ( s .lt. m(j,k) ) then
                  m(j,k) = s
                end if
              end if
            end do
          end if
        end do
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
