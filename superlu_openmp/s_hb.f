      program main

c********************************************************************72
c
cc MAIN is the main program for S_HB.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 March 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer maxn
      parameter ( maxn = 10000 )
      integer maxnz
      parameter ( maxnz = 100000 )

      real b(maxn)
      integer colptr(maxn)
      integer i
      integer info
      integer ldb
      integer n
      integer nnz
      integer nprocs
      integer nrhs
      integer rowind(maxnz)
      real values(maxnz)

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'S_HB'
      write ( *, '(a)' ) '  FORTRAN77/OpenMP version'

      call shbcode1 ( n, n, nnz, values, rowind, colptr )

      nrhs = 1
      ldb = n
      nprocs = 2

      do i = 1, n
        b(i) = 1.0E+00
      end do

      call c_bridge_psgssv ( nprocs, n, nnz, nrhs, values, rowind, 
     &  colptr, b, ldb, info )
c
c  Stop if a nonzero INFO value was returned.
c
      if ( info .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'S_HB - Fatal error!'
        write ( *, '(a,i6)' ) '  C_BRIDGE_PSGSSV returns INFO = ', info
        stop 1
      end if
c
c  Print first 10 entries of solution.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  First 10 entries of solution:'
      write ( *, '(a)' ) ' '
      do i = 1, 10
        write ( *, '(2x,i2,2x,g14.6)' ) i, b(i)
      end do
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'S_HB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine shbcode1 ( nrow, ncol, nnzero, values, rowind, colptr )

c*********************************************************************72
c
cc SHBCODE1 is a sample code for reading a Harwell-Boeing sparse matrix file.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    25 January 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, integer NROW, number of rows.
c
c    Output, integer NCOL, number of columns.
c
c    Output, integer NNZERO, number of nonzeros.
c
c    Output, real VALUES(NNZERO), the nonzero matrix values.
c
c    Output, integer ROWIND(NNZERO), the row indices of the nonzeros.
c
c    Output, integer COLPTR(NCOL+1), the compressed column indices.
c
      implicit none

      integer colptr(*)
      integer i
      integer indcrd
      character * ( 16 ) indfmt
      character * ( 8 ) key
      character * ( 3 ) mxtype
      integer ncol
      integer neltvl
      integer nnzero
      integer nrow
      integer ptrcrd
      character * ( 16 ) ptrfmt
      integer rhscrd
      character * ( 20 ) rhsfmt
      integer rowind(*)
      character * ( 72 ) title
      integer totcrd
      integer valcrd
      character * ( 20 ) valfmt
      real values(*)
c
c  Read header.
c
      read ( *, '( a72,a8 / 5i14 / a3,11x,4i14 /a16,a16,a20,a20 )' ) 
     &  title, key, 
     &  totcrd, ptrcrd, indcrd, valcrd, rhscrd, 
     &  mxtype, nrow, ncol, nnzero, neltvl, 
     &  ptrfmt, indfmt, valfmt, rhsfmt
c
c  Read matrix structure.
c
      read ( *, ptrfmt ) ( colptr(i), i = 1, ncol + 1 )
      read ( *, indfmt ) ( rowind(i), i = 1, nnzero )
c
c  Read matrix values.
c
      if ( 0 .lt. valcrd ) then
        read ( *, valfmt ) ( values(i), i = 1, nnzero )
      end if

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


