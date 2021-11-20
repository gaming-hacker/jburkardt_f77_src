      program main

c*********************************************************************72
c
cc MAIN is the main program for ST_TO_CC_PRB.
c
c  Discussion:
c
c    ST_TO_CC_PRB tests the ST_TO_CC library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 July 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'ST_TO_CC_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the ST_TO_CC library.'

      call test01 ( )
      call test02 ( )
      call test03 ( )
      call test04 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'ST_TO_CC_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 tests ST_TO_CC using a tiny matrix.
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
c    A (1-based) ST representation, reading in order by rows is:
c
c      I  J   A
c     -- --  --
c      1  1   2
c      1  2   3
c
c      2  1   3
c      2  3   4
c      2  5   6
c
c      3  2  -1
c      3  3  -3
c      3  4   2
c
c      4  3   1
c
c      5  2   4
c      5  3   2
c      5  5   1
c
c    The CC representation (which goes in order by columns) is
c
c      #   I  JC   A
c     --  --  --  --
c      1   1   1   2
c      2   2       3
c
c      3   1   3   3
c      4   3      -1
c      5   5       4
c
c      6   2   6   4
c      7   3      -3
c      8   4       1
c      9   5       2
c
c     10   3  10   2
c
c     11   2  11   6
c     12   5       1
c
c     13   *  13
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 July 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer nst
      parameter ( nst = 12 )

      double precision acc(12)
      double precision ast(nst)
      integer ccc(12)
      integer icc(12)
      integer i_max
      integer i_min
      integer i4vec_max
      integer i4vec_min
      integer ist(nst)
      integer j_max
      integer j_min
      integer jst(nst)
      integer m
      parameter ( m = 5 )
      integer n
      parameter ( n = 5 )
      integer ncc

      save ast
      save ist
      save jst

      data ast /
     &  2.0,  3.0,   
     &  3.0,  4.0,  6.0,
     & -1.0, -3.0,  2.0,
     &  1.0,
     &  4.0,  2.0,  1.0 /
      data ist /
     &  1, 1,     
     &  2, 2, 2,
     &  3, 3, 3,
     &  4,
     &  5, 5, 5 /
      data jst /
     &  1, 2,     
     &  1, 3, 5,
     &  2, 3, 4,
     &  3,
     &  2, 3, 5 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) 
     &  '  Convert a sparse matrix from ST to CC format.'
      write ( *, '(a)' ) '  ST: sparse triplet,    I, J,  A.'
      write ( *, '(a)' ) '  CC: compressed column, I, CC, A.'

      i_min = i4vec_min ( nst, ist )
      i_max = i4vec_max ( nst, ist )
      j_min = i4vec_min ( nst, jst )
      j_max = i4vec_max ( nst, jst )

      call st_header_print ( i_min, i_max, j_min, j_max, m, n, nst )
c
c  Print the ST matrix.
c
      call st_print ( m, n, nst, ist, jst, ast, 
     &  '  The matrix in ST format:' )
c
c  Get the CC size.
c
      call st_to_cc_size ( nst, ist, jst, ncc )

      write ( *, '(a)' ) ''
      write ( *, '(a,i4)' ) '  Number of CC values = ', ncc
c
c  Create the CC indices.
c
      call st_to_cc_index ( nst, ist, jst, ncc, n, icc, ccc )
c
c  Create the CC values.
c
      call st_to_cc_values ( nst, ist, jst, ast, ncc, n, icc, ccc, acc )
c
c  Print the CC matrix.
c
      call cc_print ( m, n, ncc, icc, ccc, acc, '  CC Matrix:' )

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 tests ST_TO_CC on a matrix stored in a file.
c
c  Discussion:
c
c    We assume no prior knowledge about the matrix except the filename.
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

      integer n_max
      parameter ( n_max = 100 )
      integer ncc_max
      parameter ( ncc_max = 1000 )
      integer nst_max
      parameter ( nst_max = 1000 )

      double precision acc(ncc_max)
      double precision ast(nst_max)
      integer ccc(n_max+1)
      character * ( 255 ) filename_st
      integer i_max
      integer i_min
      integer icc(ncc_max)
      integer ist(nst_max)
      integer j_max
      integer j_min
      integer jst(nst_max)
      integer m
      integer n
      integer ncc
      integer nst

      filename_st = 'west_st.txt'

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) 
     &  '  Convert a sparse matrix from ST to CC format.'
      write ( *, '(a)' ) '  ST: sparse triplet,    I, J,  A.'
      write ( *, '(a)' ) '  CC: compressed column, I, CC, A.'
      write ( *, '(a)' ) '  This matrix is read from the file "'
     &  // trim ( filename_st ) // '".'
c
c  Get the size of the ST matrix.
c
      call st_header_read ( filename_st, i_min, i_max, j_min, 
     &  j_max, m, n, nst )

      call st_header_print ( i_min, i_max, j_min, j_max, m, n, nst )
c
c  Read the ST matrix.
c
      call st_data_read ( filename_st, m, n, nst, ist, jst, ast )
c
c  Print the ST matrix.
c
      call st_print ( m, n, nst, ist, jst, ast, 
     &  '  The matrix in ST format:' )
c
c  Get the CC size.
c
      call st_to_cc_size ( nst, ist, jst, ncc )

      write ( *, '(a)' ) ''
      write ( *, '(a,i4)' ) '  Number of CC values = ', ncc
c
c  Create the CC indices.
c
      call st_to_cc_index ( nst, ist, jst, ncc, n, icc, ccc )
c
c  Create the CC values.
c
      call st_to_cc_values ( nst, ist, jst, ast, ncc, n, icc, ccc, acc )
c
c  Print the CC matrix.
c
      call cc_print ( m, n, ncc, icc, ccc, acc, '  CC Matrix:' )

      return
      end
      subroutine test03 ( )

c*********************************************************************72
c
cc TEST03 creates a CC sparse matrix file from an ST file.
c
c  Discussion:
c
c    We assume no prior knowledge about the matrix except the filename.
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

      integer n_max
      parameter ( n_max = 100 )
      integer ncc_max
      parameter ( ncc_max = 1000 )
      integer nst_max
      parameter ( nst_max = 1000 )

      double precision acc(ncc_max)
      double precision ast(nst_max)
      integer ccc(n_max+1)
      character * ( 255 ) filename_acc
      character * ( 255 ) filename_ccc
      character * ( 255 ) filename_icc
      character * ( 255 ) filename_st
      integer i_max
      integer i_min
      integer icc(ncc_max)
      integer j_max
      integer j_min
      integer ist(nst_max)
      integer jst(nst_max)
      integer m
      integer n
      integer ncc
      integer nst

      filename_st = 'west_st.txt'
      filename_acc = 'west_acc.txt'
      filename_ccc = 'west_ccc.txt'
      filename_icc = 'west_icc.txt'

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST03'
      write ( *, '(a)' ) 
     &  '  Convert a sparse matrix from ST to CC format.'
      write ( *, '(a)' ) '  ST: sparse triplet,    I, J,  A.'
      write ( *, '(a)' ) '  CC: compressed column, I, CC, A.'
      write ( *, '(a)' ) '  The ST matrix is read from the file "'
     &  // trim ( filename_st ) // '",'
      write ( *, '(a)' ) '  and the CC matrix is written to the files:'
      write ( *, '(a)' ) '    "'// trim ( filename_icc ) // '",'
      write ( *, '(a)' ) '    "'// trim ( filename_ccc ) // '", and'
      write ( *, '(a)' ) '    "'// trim ( filename_acc ) // '",'
c
c  Get the size of the ST matrix.
c
      call st_header_read ( filename_st, i_min, i_max, j_min, 
     &  j_max, m, n, nst )

      call st_header_print ( i_min, i_max, j_min, j_max, m, n, nst )
c
c  Read the ST matrix.
c
      call st_data_read ( filename_st, m, n, nst, ist, jst, ast )
c
c  Get the CC size.
c
      call st_to_cc_size ( nst, ist, jst, ncc )

      write ( *, '(a)' ) ''
      write ( *, '(a,i4)' ) '  Number of CC values = ', ncc
c
c  Create the CC indices.
c
      call st_to_cc_index ( nst, ist, jst, ncc, n, icc, ccc )
c
c  Create the CC values.
c
      call st_to_cc_values ( nst, ist, jst, ast, ncc, n, icc, ccc, acc )
c
c  Write the CC matrix.
c
      call i4vec_write ( filename_icc, ncc, icc )
      call i4vec_write ( filename_ccc, n + 1, ccc )
      call r8vec_write ( filename_acc, ncc, acc )

      return
      end
      subroutine test04 ( )

c*********************************************************************72
c
cc TEST04 works with a CC sparse matrix with many repeated index pairs.
c
c  Discussion:
c
c    To complete this test, I want to compare AST * X and ACC * X.
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

      integer n_max
      parameter ( n_max = 100 )
      integer ncc_max
      parameter ( ncc_max = 1000 )
      integer nst_max
      parameter ( nst_max = 1000 )

      double precision acc(ncc_max)
      double precision ast(nst_max)
      double precision b1(n_max)
      double precision b2(n_max)
      integer ccc(n_max+1)
      integer i_max
      integer i_min
      integer i4vec_max
      integer i4vec_min
      integer icc(ncc_max)
      integer ist(nst_max)
      integer j_max
      integer j_min
      integer jst(nst_max)
      integer m
      integer n
      integer ncc
      integer nst
      integer nx
      integer ny
      double precision r
      double precision r8vec_diff_norm
      integer seed
      double precision  x(n_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST04'
      write ( *, '(a)' ) 
     &  '  Convert a sparse matrix from ST to CC format.'
      write ( *, '(a)' ) '  ST: sparse triplet,    I, J,  A.'
      write ( *, '(a)' ) '  CC: compressed column, I, CC, A.'
      write ( *, '(a)' ) 
     &  '  The ST matrix is the Wathen finite element matrix.'
      write ( *, '(a)' ) '  It has many repeated index pairs.'
      write ( *, '(a)' ) 
     &  '  To check, compare ACC*X - AST*X for a random X.'
c
c  Get the size of the ST matrix.
c
      nx = 3
      ny = 3
      call wathen_st_size ( nx, ny, nst )

      write ( *, '(a)' ) ''
      write ( *, '(a,i4)' ) '  Number of ST values = ', nst
c
c  Set the formal matrix size
c
      m = 3 * nx * ny + 2 * nx + 2 * ny + 1
      n = m
c
c  Set a random vector.
c
      seed = 123456789
      call r8vec_uniform_01 ( n, seed, x )
c
c  Create the ST matrix.
c
      seed = 123456789
      call wathen_st ( nx, ny, nst, seed, ist, jst, ast )

      i_min = i4vec_min ( nst, ist )
      i_max = i4vec_max ( nst, ist )
      j_min = i4vec_min ( nst, jst )
      j_max = i4vec_max ( nst, jst )

      call st_header_print ( i_min, i_max, j_min, j_max, m, n, nst )
c
c  Compute B1 = AST * X
c
      call st_mv ( m, n, nst, ist, jst, ast, x, b1 )
c
c  Get the CC size.
c
      call st_to_cc_size ( nst, ist, jst, ncc )

      write ( *, '(a,i4)' ) '  Number of CC values = ', ncc
c
c  Create the CC indices.
c
      call st_to_cc_index ( nst, ist, jst, ncc, n, icc, ccc )
c
c  Create the CC values.
c
      call st_to_cc_values ( nst, ist, jst, ast, ncc, n, icc, ccc, acc )
c
c  Compute B2 = ACC * X.
c
      call cc_mv ( m, n, ncc, icc, ccc, acc, x, b2 )
c
c  Compare B1 and B2.
c
      r = r8vec_diff_norm ( n, b1, b2 )
      write ( *, '(a,g14.6)' ) '  || ACC*X - AST*X|| = ', r

      return
      end
