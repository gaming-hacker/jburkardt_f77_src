      program main

c*********************************************************************72
c
cc MAIN is the main program for ST_IO_PRB.
c
c  Discussion:
c
c    ST_IO_PRB tests the ST_IO library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 January 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'ST_IO_PRB:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the ST_IO library.'

      call test01 ( )
      call test02 ( )
      call test03 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'ST_IO_PRB:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 tests ST_WRITE.
c
c  Discussion:
c
c    The matrix is:
c
c      11  12   0   0  15
c      21  22   0   0   0
c       0   0  33   0  35
c       0   0   0  44   0
c      51   0  53   0  55
c
c    The index vectors are 1 based, and so have to be converted to
c    0-base before being written.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 July 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 5 )
      integer n
      parameter ( n = 5 )
      integer nst
      parameter ( nst = 11 )

      double precision ast(nst)
      integer i_max
      integer i_min
      integer i4vec_max
      integer i4vec_min
      integer ist(nst)
      integer j_max
      integer j_min
      integer jst(nst)
      character * ( 255 ) output_filename

      save ast
      save ist
      save jst

      data ast /
     &  51.0D+00, 12.0D+00, 11.0D+00, 33.0D+00, 15.0D+00,
     &  53.0D+00, 55.0D+00, 22.0D+00, 35.0D+00, 44.0D+00,
     &  21.0D+00 /
      data ist /
     &  5, 1, 1, 3, 1, 5, 5, 2, 3, 4, 2 /
      data jst /
     &  1, 2, 1, 3, 5, 3, 5, 2, 5, 4, 1 /

      output_filename = 'a5by5.st'

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  ST_WRITE writes an ST file.'

      call i4vec_dec ( nst, ist )
      call i4vec_dec ( nst, jst )

      i_min = i4vec_min ( nst, ist )
      i_max = i4vec_max ( nst, ist )
      j_min = i4vec_min ( nst, jst )
      j_max = i4vec_max ( nst, jst )

      call st_header_print ( i_min, i_max, j_min, j_max, m, n, nst )

      call st_print ( m, n, nst, ist, jst, ast,
     &  '  Sparse Triplet (ST) data:' )

      call st_write ( output_filename, m, n, nst, ist, jst, ast )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Wrote the matrix data to "' // 
     &  trim ( output_filename ) // '".'

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 tests ST_HEADER_READ, ST_DATA_READ.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 July 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer nst_max
      parameter ( nst_max = 1000 )

      double precision ast(nst_max)
      character * ( 80 ) input_filename
      integer i_max
      integer i_min
      integer ist(nst_max)
      integer j_max
      integer j_min
      integer jst(nst_max)
      integer m
      integer n
      integer nst

      input_filename = 'kershaw.st'

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) 
     &  '  ST_HEADER_READ reads the header from an ST file.'
      write ( *, '(a)' ) 
     &  '  ST_DATA_READ reads the data from an ST file.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Read the data from "' // trim ( input_filename ) // '".'

      call st_header_read ( input_filename, i_min, i_max, j_min, j_max, 
     &  m, n, nst )

      call st_header_print ( i_min, i_max, j_min, j_max, m, n, nst )

      call st_data_read ( input_filename, m, n, nst, ist, jst, ast )

      call st_print ( m, n, nst, ist, jst, ast,
     &  '  Sparse Triplet (ST) data read from file' )

      return
      end
      subroutine test03 ( )

c*********************************************************************72
c
cc TEST03 tests ST_SORT_A.
c
c  Discussion:
c
c    The matrix is:
c
c      11  12   0   0  15
c      21  22   0   0   0
c       0   0  33   0  35
c       0   0   0  44   0
c      51   0  53   0  55
c
c    The index vectors are 1 based, and so have to be converted to
c    0-base before being written.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 July 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 5 )
      integer n
      parameter ( n = 5 )
      integer nst
      parameter ( nst = 11 )

      double precision ast(nst)
      integer i_max
      integer i_min
      integer i4vec_max
      integer i4vec_min
      integer ist(nst)
      integer j_max
      integer j_min
      integer jst(nst)

      save ast
      save ist
      save jst

      data ast /
     &  51.0D+00, 12.0D+00, 11.0D+00, 33.0D+00, 15.0D+00,
     &  53.0D+00, 55.0D+00, 22.0D+00, 35.0D+00, 44.0D+00,
     &  21.0D+00 /
      data ist /
     &  5, 1, 1, 3, 1, 5, 5, 2, 3, 4, 2 /
      data jst /
     &  1, 2, 1, 3, 5, 3, 5, 2, 5, 4, 1 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST03'
      write ( *, '(a)' ) '  ST_SORT_A sorts an ST matrix by columns.'

      i_min = i4vec_min ( nst, ist )
      i_max = i4vec_max ( nst, ist )
      j_min = i4vec_min ( nst, jst )
      j_max = i4vec_max ( nst, jst )

      call st_header_print ( i_min, i_max, j_min, j_max, m, n, nst )

      call st_print ( m, n, nst, ist, jst, ast,
     &  '  Matrix data before sorting:' )

      call st_sort_a ( m, n, nst, ist, jst, ast )

      call st_print ( m, n, nst, ist, jst, ast,
     &  '  Matrix data sorted by column:' )

      return
      end
