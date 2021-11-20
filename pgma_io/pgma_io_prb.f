      program main

c*********************************************************************72
c
cc MAIN is the main program for PGMA_IO_PRB.
c
c  Discussion:
c
c    PGMA_IO_PRB tests the PGMA_IO library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 June 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'PGMA_IO_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the PGMA_IO library.'

      call test01 ( )
      call test02 ( )
      call test03 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'PGMA_IO_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 tests PGMA_EXAMPLE, PGMA_WRITE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 February 2012
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer ncol
      parameter ( ncol = 300 )
      integer nrow
      parameter ( nrow = 300 )

      character * ( 80 ) file_name
      parameter ( file_name = 'pgma_io_prb_01.ascii.pgm' )
      integer g(nrow,ncol)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  PGMA_EXAMPLE sets up ASCII PGM data.'
      write ( *, '(a)' ) '  PGMA_WRITE writes an ASCII PGM file.'

      call pgma_example ( nrow, ncol, g )

      call pgma_write ( file_name, nrow, ncol, g )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Wrote the header and data for "'
     &  // trim ( file_name ) //'".'
      write ( *, '(a,i8)' ) '  Number of rows of data =    ', nrow
      write ( *, '(a,i8)' ) '  Number of columns of data = ', ncol

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 tests PGMA_READ_DATA, PGMA_READ_HEADER.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 June 2010
c
c  Author:
c
c    John Burkardt
c
      implicit none

      character * ( 80 ) file_name
      parameter ( file_name = 'pgma_io_prb_02.ascii.pgm' )
      integer file_unit
      integer g(300,300)
      integer i
      integer ierror
      integer ios
      integer j
      integer k
      integer maxg
      integer ncol
      integer nrow

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) '  PGMA_READ reads an ASCII PGM file.'

      call pgma_write_test ( file_name )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  PGMA_WRITE_TEST created some data.'

      call get_unit ( file_unit )

      open ( unit = file_unit, file = file_name, status = 'old',
     &  err = 10 )

      call pgma_read_header ( file_unit, nrow, ncol, maxg )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  PGMA_READ_HEADER read the header.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Number of rows of data =    ', nrow
      write ( *, '(a,i8)' ) '  Number of columns of data = ', ncol
      write ( *, '(a,i8)' ) '  Maximum G value =           ', maxg

      call pgma_read_data ( file_unit, nrow, ncol, g )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  PGMA_READ_DATA read the data.'

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Sample data:'
      write ( *, '(a)' ) ' '

      do k = 1, 10
        i = ( ( 10 - k ) * 1 + ( k - 1 ) * nrow ) / ( 10 - 1 )
        j = ( ( 10 - k ) * 1 + ( k - 1 ) * ncol ) / ( 10 - 1 )
        write ( *, '(i4,2x,i4,2x,i6)' ) i, j, g(i,j)
      end do

      call pgma_check_data ( nrow, ncol, maxg, g, ierror )

      if ( ierror .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'TEST02'
        write ( *, '(a,i8)' )
     &    '  The data was not accepted by PGMA_CHECK_DATA.'
        return
      end if

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  The data was accepted by PGMA_CHECK_DATA.'

      return

10    continue

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST02 - Fatal error!'
      write ( *, '(a)' ) '  Could not open the file.'
      return
      end
      subroutine test03 ( )

c*********************************************************************72
c
cc TEST03 tests PGMA_WRITE.
c
c  Discussion:
c
c    This example makes a sort of grayscale checkerboard.
c
c    The gray scale values were computed by the routine
c    GRAYSCALE_RGB in the COLORS library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 February 2012
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer ncol
      parameter ( ncol = 300 )
      integer ngray
      parameter ( ngray = 11 )
      integer nrow
      parameter ( nrow = 300 )

      character * ( 80 ) file_name
      parameter ( file_name = 'pgma_io_prb_03.ascii.pgm' )
      integer g(nrow,ncol)
      double precision gray(ngray)
      integer i
      integer j
      integer k

      save gray

      data gray /
     &  0.000D+00, 0.291D+00, 0.434D+00, 0.540D+00, 0.629D+00,
     &  0.706D+00, 0.774D+00, 0.837D+00, 0.895D+00, 0.949D+00,
     &  1.000D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST03'
      write ( *, '(a)' ) '  PGMA_WRITE writes an ASCII PGM file.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  In this example, we make a grayscale'
      write ( *, '(a)' ) '  checkerboard.'

      do i = 1, nrow
        do j = 1, ncol
          k = ( i - 1 + j - 1 ) * ngray / min ( nrow, ncol )
          k = 1 + mod ( k, ngray )
          g(i,j) = int ( 255.0E+00 * gray(k) )
        end do
      end do

      call pgma_write ( file_name, nrow, ncol, g )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Wrote the header and data for "'
     &  // trim ( file_name ) //'".'
      write ( *, '(a,i8)' ) '  Number of rows of data =    ', nrow
      write ( *, '(a,i8)' ) '  Number of columns of data = ', ncol

      return
      end
