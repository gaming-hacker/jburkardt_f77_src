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
      subroutine spy_file ( header, data_filename )

c*********************************************************************72
c
cc SPY_FILE plots a sparsity pattern stored in a file.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 September 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character * ( * ) HEADER, the name to be used for the
c    title of the plot, and as part of the names of the command
c    and plot files.
c
c    Input, character * ( * ) DATA_FILENAME, the name of the file
c    containing the indices of nonzero matrix entries.
c
      implicit none

      character * ( 255 ) command_filename
      integer command_unit
      character * ( 255 ) data_filename
      integer data_status
      integer data_unit
      character * ( * ) header
      integer i
      integer i4_huge
      parameter ( i4_huge = 2147483647 )
      integer j
      integer m0
      character * ( 6 ) m0_s
      integer m1
      character * ( 6 ) m1_s
      integer n0
      character * ( 6 ) n0_s
      integer n1
      character * ( 6 ) n1_s
      integer nz_num
      character * ( 255 ) png_filename

      n0 = + i4_huge
      n1 = - i4_huge
      m0 = + i4_huge
      m1 = - i4_huge
      nz_num = 0

      call get_unit ( data_unit )
      open ( unit = data_unit, file = data_filename, status = 'old' )

10    continue

        read ( data_unit, *, end = 20 ) i, j

        nz_num = nz_num + 1
        m0 = min ( m0, i )
        m1 = max ( m1, i )
        n0 = min ( n0, j )
        n1 = max ( n1, j )

      go to 10

20    continue

      close ( unit = data_unit )
c
c  Create command file.
c
      command_filename = trim ( header ) // '_commands.txt'
      call get_unit ( command_unit )
      open ( unit = command_unit, file = command_filename, 
     &  status = 'replace' )

      write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) '# Usage:'
      write ( command_unit, '(a)' ) 
     &  '#  gnuplot < ' // trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) 'unset key'
      write ( command_unit, '(a)' ) 'set term png'

      png_filename = trim ( header ) // '.png'
      write ( command_unit, '(a)' ) 'set output "' // 
     &  trim ( png_filename ) // '"'
      write ( command_unit, '(a)' ) 'set size ratio -1'
      write ( command_unit, '(a)' ) 'set xlabel "<--- J --->"'
      write ( command_unit, '(a)' ) 'set ylabel "<--- I --->"'

      write ( command_unit, '(a,i6,a)' )     'set title "', nz_num, 
     &  ' nonzeros for ''' // trim ( header ) // '''"'
      write ( command_unit, '(a)' ) 'set timestamp'
      write ( n0_s, '(i6)' ) n0
      write ( n1_s, '(i6)' ) n1
      write ( m0_s, '(i6)' ) m0
      write ( m1_s, '(i6)' ) m1
      m0_s = adjustl ( m0_s )
      m1_s = adjustl ( m1_s )
      n0_s = adjustl ( n0_s )
      n1_s = adjustl ( n1_s )
      write ( command_unit, '(a)' )
     &  'plot [y=' // trim ( m0_s ) // ':' // trim ( m1_s ) //
     &  '] [x=' // trim ( n0_s ) // ':' // trim ( n1_s ) // ' ] "' //
     &  trim ( data_filename ) // '" with points pt 5'

      close ( unit = command_unit )
      write ( *, '(a)' )     '  Created graphics command file "' // 
     &  trim ( command_filename ) // '".'

      return
      end
      subroutine spy_ge ( m, n, a, header )

c*********************************************************************72
c
cc SPY_GE plots a sparsity pattern for a general storage (GE) matrix.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 September 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns
c    in the matrix.
c
c    Input, double precision A(M,N), the matrix.
c
c    Input, character * ( * ) HEADER, the name to be used for the
c    title of the plot, and as part of the names of the data, command
c    and plot files.
c
      implicit none

      integer m
      integer n

      double precision a(m,n)
      character * ( 255 ) command_filename
      integer command_unit
      character * ( 255 ) data_filename
      integer data_unit
      character * ( * ) header
      integer i
      integer j
      character * ( 6 ) m_s
      character * ( 6 ) n_s
      integer nz_num
      character * ( 255 ) png_filename
c
c  Create data file.
c
      data_filename = trim ( header ) // '_data.txt'
      call get_unit ( data_unit )
      open ( unit = data_unit, file = data_filename, 
     &  status = 'replace' )
      nz_num = 0
      do j = 1, n
        do i = 1, m
          if ( a(i,j) .ne. 0.0D+00 ) then
            write ( data_unit, '(2x,i6,2x,i6)' ) j, i
            nz_num = nz_num + 1
          end if
        end do
      end do
      close ( unit = data_unit )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' )     '  Created sparsity data file "' 
     &  // trim ( data_filename ) // '".'
c
c  Create command file.
c
      command_filename = trim ( header ) // '_commands.txt'
      call get_unit ( command_unit )
      open ( unit = command_unit, file = command_filename, 
     &  status = 'replace' )

      write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) '# Usage:'
      write ( command_unit, '(a)' ) '#  gnuplot < ' // 
     &  trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) 'unset key'
      write ( command_unit, '(a)' ) 'set term png'

      png_filename = trim ( header ) // '.png'
      write ( command_unit, '(a)' ) 'set output "' // 
     &  trim ( png_filename ) // '"'
      write ( command_unit, '(a)' ) 'set size ratio -1'
      write ( command_unit, '(a)' ) 'set xlabel "<--- J --->"'
      write ( command_unit, '(a)' ) 'set ylabel "<--- I --->"'

      write ( command_unit, '(a,i6,a)' )
     &  'set title "', nz_num, ' nonzeros for ''' // 
     &  trim ( header ) // '''"'
      write ( command_unit, '(a)' ) 'set timestamp'
      write ( n_s, '(i6)' ) n
      write ( m_s, '(i6)' ) m
      m_s = adjustl ( m_s )
      n_s = adjustl ( n_s )
      write ( command_unit, '(a)' )
     &  'plot [x=1:' // trim ( n_s) // '] [y=' // trim ( m_s ) // 
     &  ':1] "' //     trim ( data_filename ) //
     &  '" with points pt 5'

      close ( unit = command_unit )
      write ( *, '(a)' )     '  Created graphics command file "' // 
     &  trim ( command_filename ) // '".'

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
