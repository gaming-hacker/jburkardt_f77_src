      program main

c*********************************************************************72
c
cc MAIN is the main program for CC_IO_PRB.
c
c  Discussion:
c
c    CC_IO_PRB tests the CC_IO library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 July 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'CC_IO_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the CC_IO library.'

      call test01 ( )
      call test02 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'CC_IO_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      return
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 tests CC_WRITE using a tiny matrix.
c
c  Discussion:
c
c    This test uses a trivial matrix whose full representation is:
c
c          2  3  0  0  0
c          3  0  4  0  6
c      A = 0 -1 -3  2  0
c          0  0  1  0  0
c          0  4  2  0  1
c
c    The 1-based CC representation is
c
c      #  ICC  CCC  ACC
c     --  ---  ---  ---
c      1    1    1    2
c      2    2         3
c
c      3    1    3    3
c      4    3        -1
c      5    5         4
c
c      6    2    6    4
c      7    3        -3
c      8    4         1
c      9    5         2
c
c     10    3   10    2
c
c     11    2   11    6
c     12    5         1
c
c     13    *   13
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 July 2014
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
      integer ncc
      parameter ( ncc = 12 )

      double precision acc(ncc)
      integer ccc(n+1)
      integer i
      integer icc(ncc)
      character * ( 255 ) prefix

      save acc
      save ccc
      save icc

      data acc /
     &  2.0,  3.0, 
     &  3.0, -1.0,  4.0, 
     &  4.0, -3.0,  1.0, 2.0, 
     &  2.0, 
     &  6.0, 1.0 /
      data ccc /
     &  1, 3, 6, 10, 11, 13 /
      data icc /
     &  1, 2, 
     &  1, 3, 5, 
     &  2, 3, 4, 5, 
     &  3, 
     &  2, 5 /

      prefix = 'simple'

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) 
     &  '  Write a sparse matrix in CC format to 3 files.'
c
c  Full storage statistics
c
      write ( *, '(a)' ) ''
      write ( *, '(a,i4)' ) '  Full rows    M = ', m
      write ( *, '(a,i4)' ) '  Full columns N = ', n
      write ( *, '(a,i4)' ) '  Full storage   = ', m * n
c
c  Print the CC matrix.
c
      call cc_print ( m, n, ncc, icc, ccc, acc, 
     &  '  The matrix in 1-based CC format:' )
c
c  Write the matrix to 3 files.
c
      call cc_write ( prefix, ncc, n, icc, ccc, acc )

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 tests CC_HEADER_READ and CC_DATA_READ.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 July 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision acc(12)
      integer ccc(6)
      integer icc(12)
      integer m
      integer n
      integer ncc
      character * ( 255 ) prefix

      prefix = 'simple'

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) 
     &  '  Read a sparse matrix in CC format from 3 files.'
c
c  Read the header.
c
      call cc_header_read ( prefix, ncc, n )
c
c  Read the matrix data.
c
      call cc_data_read ( prefix, ncc, n, icc, ccc, acc )
c
c  Print the CC matrix.
c
      m = n
      call cc_print ( m, n, ncc, icc, ccc, acc, 
     &  '  The matrix in 1-based CC format:' )

      return
      end