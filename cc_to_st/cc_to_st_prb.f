      program main

c*********************************************************************72
c
cc MAIN is the main program for CC_TO_ST_PRB.
c
c  Discussion:
c
c    CC_TO_ST_PRB tests the CC_TO_ST library.
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
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CC_TO_ST_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the CC_TO_ST library.'

      call test01 ( )
      call test02 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CC_TO_ST_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 tests CC_TO_ST using a 1-based matrix.
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
c    23 July 2014
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
      double precision ast(ncc)
      integer ccc(n+1)
      integer i
      integer icc(ncc)
      integer ist(ncc)
      integer jst(ncc)
      integer nst

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

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  Convert a 1-based CC matrix to ST format.'
c
c  Print the CC matrix.
c
      call cc_print ( m, n, ncc, icc, ccc, acc, '  The CC matrix:' )
c
c  Convert it.
c
      call cc_to_st ( m, n, ncc, icc, ccc, acc, nst, ist, jst, ast )
c
c  Print the ST matrix.
c
      call st_print ( m, n, nst, ist, jst, ast, '  The ST matrix:' )

      return
      end
      subroutine test02 ( )

c*****************************************************************************80
c
cc TEST02  tests CC_TO_ST using a 0-based matrix.
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
c    The 0-based CC representation is
c
c      #  ICC  CCC  ACC
c     --  ---  ---  ---
c      0    0    0    2
c      1    1         3
c
c      2    0    2    3
c      3    2        -1
c      4    4         4
c
c      5    1    5    4
c      6    2        -3
c      7    3         1
c      8    4         2
c
c      9    2    9    2
c
c     10    1   10    6
c     11    4         1
c
c     12    *   12
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
      implicit none

      integer m
      parameter ( m = 5 )
      integer n
      parameter ( n = 5 )
      integer ncc
      parameter ( ncc = 12 )

      double precision acc(ncc)
      double precision ast(ncc)
      integer ccc(n+1)
      integer i
      integer icc(ncc)
      integer ist(ncc)
      integer jst(ncc)
      integer nst

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
     &  0, 2, 5, 9, 10, 12 /
      data icc /
     &  0, 1,
     &  0, 2, 4,
     &  1, 2, 3, 4,
     &  2,
     &  1, 4 /

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) '  Convert a 0-based CC matrix to ST format.'
c
c  Print the CC matrix.
c
      call cc_print ( m, n, ncc, icc, ccc, acc, '  The CC matrix:' )
c
c  Convert it.
c
      call cc_to_st ( m, n, ncc, icc, ccc, acc, nst, ist, jst, ast )
c
c  Print the ST matrix.
c
      call st_print ( m, n, nst, ist, jst, ast, '  The ST matrix:' )

      return
      end
