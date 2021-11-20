      subroutine cc_print ( m, n, ncc, icc, ccc, acc, title )

c*********************************************************************72
c
cc CC_PRINT prints a sparse matrix in CC format.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    23 July 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the number of rows in the matrix.
c
c    Input, integer N, the number of columns in the matrix.
c
c    Input, integer NCC, the number of CC elements.
c
c    Input, integer ICC(NCC), the CC rows.
c
c    Input, integer CCC(N+1), the compressed CC columns.
c
c    Input, double precision ACC(NCC), the CC values.
c
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer n
      integer ncc

      double precision acc(ncc)
      integer ccc(n+1)
      integer i
      integer icc(ncc)
      integer j
      integer k
      integer m
      character * ( * ) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
      write ( *, '(a)' ) '     #     I     J         A'
      write ( *, '(a)' ) '  ----  ----  ----  ----------------'
      write ( *, '(a)' ) ' '

      if ( ccc(1) .eq. 0 ) then

        j = 0
        do k = 1, ncc
          i = icc(k)
10        continue
          if ( ccc(j+2) .le. k - 1 ) then
            j = j + 1
            go to 10
          end if
          write ( *, '(2x,i4,2x,i4,2x,i4,2x,g16.8)' ) 
     &      k - 1, i, j, acc(k)
        end do

      else

        j = 1
        do k = 1, ncc
          i = icc(k)
20        continue
          if ( ccc(j+1) .le. k ) then
            j = j + 1
            go to 20
          end if
          write ( *, '(2x,i4,2x,i4,2x,i4,2x,g16.8)' ) k, i, j, acc(k)
        end do

      end if

      return
      end
      subroutine cc_to_st ( m, n, ncc, icc, ccc, acc, nst, ist, jst, 
     &  ast )

c*********************************************************************72
c
cc CC_TO_ST converts sparse matrix information from CC to ST format.
c
c  Discussion:
c
c    Only JST actually needs to be computed.  The other three output 
c    quantities are simply copies.  
c    
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    23 July 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the number of rows.
c
c    Input, integer N, the number of columns.
c
c    Input, integer NCC, the number of CC elements.
c
c    Input, integer ICC(NCC), the CC rows.
c
c    Input, integer CCC(N+1), the CC compressed columns.
c
c    Input, double precision ACC(NCC), the CC values.
c
c    Output, integer NST, the number of ST elements.
c
c    Output, integer IST(NST), JST(NST), the ST rows and columns.
c
c    Output, double precision AST(NST), the ST values.
c
      implicit none

      integer n
      integer ncc

      double precision ast(ncc)
      double precision acc(ncc)
      integer ccc(n+1)
      integer icc(ncc)
      integer ist(ncc)
      integer j
      integer jhi
      integer jlo
      integer jst(ncc)
      integer k
      integer khi
      integer klo
      integer m
      integer nst

      nst = 0

      if ( ccc(1) .eq. 0 ) then

        jlo = 0
        jhi = n - 1
  
        do j = jlo, jhi

          klo = ccc(j+1)
          khi = ccc(j+2) - 1

          do k = klo, khi

            nst = nst + 1
            ist(nst) = icc(k+1)
            jst(nst) = j
            ast(nst) = acc(k+1)

          end do

        end do

      else

        jlo = 1
        jhi = n
  
        do j = jlo, jhi

          klo = ccc(j)
          khi = ccc(j+1) - 1

          do k = klo, khi

            nst = nst + 1
            ist(nst) = icc(k)
            jst(nst) = j
            ast(nst) = acc(k)

          end do

        end do

      end if

      return
      end
      subroutine st_print ( m, n, nst, ist, jst, ast, title )

c*********************************************************************72
c
cc ST_PRINT prints a sparse matrix in ST format.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 July 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the number of rows.
c
c    Input, integer N, the number of columns.
c
c    Input, integer NST, the number of ST elements.
c
c    Input, integer IST(NST), JST(NST), the ST rows and columns.
c
c    Input, double precision AST(NST), the ST values.
c
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer nst

      double precision ast(nst)
      integer ist(nst)
      integer jst(nst)
      integer k
      integer m
      integer n
      character * ( * ) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
      write ( *, '(a)' ) '     #     I     J       A'
      write ( *, '(a)' ) '  ----  ----  ----  --------------'
      write ( *, '(a)' ) ' '

      do k = 1, nst
        write ( *, '(2x,i4,2x,i4,2x,i4,2x,g16.8)' ) 
     &    k, ist(k), jst(k), ast(k)
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
