      program main

c*********************************************************************72
c
cc MAIN is the main program for FLOYD_PRB.
c
c  Discussion:
c
c    FLOYD_PRB tests the FLOYD library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 March 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FLOYD_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the FLOYD library.'

      call test01 ( )
      call test02 ( )
      call test03 ( )
      call test04 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FLOYD_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 tests I4MAT_FLOYD.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 July 2011
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 6 )

      integer a(6,6)
      integer huge
      integer i
      integer i4_huge
      integer j

      save a

      data a /
     &   0, -1, -1, -1, -1, -1, 
     &   2,  0, -1, -1, -1,  5, 
     &   5,  7,  0, -1,  2, -1, 
     &  -1,  1,  4,  0, -1,  2, 
     &  -1, -1, -1,  3,  0,  4, 
     &  -1,  8, -1, -1,  3,  0  /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) 
     &  '  I4MAT_FLOYO uses Floyd''s algorithm to find the'
      write ( *, '(a)' ) 
     &  '  shortest distance between all pairs of nodes'
      write ( *, '(a)' ) 
     &  '  in a directed graph, starting from the initial array'
      write ( *, '(a)' ) '  of direct node-to-node distances.'

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  In the initial direct distance array, if'
      write ( *, '(a)' ) '    A(I,J) = -1,'
      write ( *, '(a)' ) 
     &  '  this indicates there is NO directed link from'
      write ( *, '(a)' ) 
     &  '  node I to node J.  In that case, the value of'
      write ( *, '(a)' ) '  of A(I,J) is essentially "infinity".'

      call i4mat_print ( n, n, a, '  Initial direct distance array:' )

      huge = i4_huge ( ) / 2

      do i = 1, n
        do j = 1, n
          if ( a(i,j) .eq. - 1 ) then
            a(i,j) = huge
          end if
        end do
      end do

      call i4mat_floyd ( n, a )

      do i = 1, n
        do j = 1, n
          if ( huge .le. a(i,j) ) then
            a(i,j) = - 1
          end if
        end do
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  In the final shortest distance array, if'
      write ( *, '(a)' ) '    A(I,J) = -1,'
      write ( *, '(a)' ) 
     &  '  this indicates there is NO directed path from'
      write ( *, '(a)' ) '  node I to node J.'

      call i4mat_print ( n, n, a, '  Final shortest distance array:' )

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 tests R8MAT_FLOYD.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 July 2011
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 6 )

      double precision a(6,6)
      integer i
      integer j
      double precision r8_huge

      save a

      data a /
     &   0.0D+00, -1.0D+00, -1.0D+00, -1.0D+00, -1.0D+00, -1.0D+00, 
     &   2.0D+00,  0.0D+00, -1.0D+00, -1.0D+00, -1.0D+00,  5.0D+00, 
     &   5.0D+00,  7.0D+00,  0.0D+00, -1.0D+00,  2.0D+00, -1.0D+00, 
     &  -1.0D+00,  1.0D+00,  4.0D+00,  0.0D+00, -1.0D+00,  2.0D+00, 
     &  -1.0D+00, -1.0D+00, -1.0D+00,  3.0D+00,  0.0D+00,  4.0D+00, 
     &  -1.0D+00,  8.0D+00, -1.0D+00, -1.0D+00,  3.0D+00,  0.0D+00  /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) 
     &  '  R8MAT_FLOYO uses Floyd''s algorithm to find the'
      write ( *, '(a)' ) 
     &  '  shortest distance between all pairs of nodes'
      write ( *, '(a)' ) 
     &  '  in a directed graph, starting from the initial array'
      write ( *, '(a)' ) '  of direct node-to-node distances.'

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  In the initial direct distance array, if'
      write ( *, '(a)' ) '    A(I,J) = -1,'
      write ( *, '(a)' ) 
     &  '  this indicates there is NO directed link from'
      write ( *, '(a)' ) 
     &  '  node I to node J.  In that case, the value of'
      write ( *, '(a)' ) '  of A(I,J) is essentially "infinity".'

      call r8mat_print ( n, n, a, '  Initial direct distance array:' )

      do i = 1, n
        do j = 1, n
          if ( a(i,j) .eq. - 1.0D+00 ) then
            a(i,j) = r8_huge ( )
          end if
        end do
      end do

      call r8mat_floyd ( n, a )

      do i = 1, n
        do j = 1, n
          if ( r8_huge ( ) .le. a(i,j) ) then
            a(i,j) = - 1.0D+00
          end if
        end do
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  In the final shortest distance array, if'
      write ( *, '(a)' ) '    A(I,J) = -1,'
      write ( *, '(a)' ) 
     &  '  this indicates there is NO directed path from'
      write ( *, '(a)' ) '  node I to node J.'

      call r8mat_print ( n, n, a, '  Final shortest distance array:' )

      return
      end
      subroutine test03 ( )

c*********************************************************************72
c
cc TEST03 applies Floyd's algorithm to matrices of increasing size.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 March 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      double precision wtime

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST03'
      write ( *, '(a)' ) '  Test I4MAT_FLOYD on the MOD(I,J) matrix.'
      write ( *, '(a)' ) '  The work is roughly N^3.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '         N   Time (seconds)  Time/N^3'
      write ( *, '(a)' ) ' '

      n = 1

10    continue

      if ( n .le. 1024 ) then
        call test03_sub ( n, wtime )
        write ( *, '(2x,i8,2x,g14.6,2x,g14.6)' ) 
     &    n, wtime, 1000000.0D+00 * wtime / dble ( n**3 )
        n = n * 2
        go to 10

      end if

      return
      end
      subroutine test03_sub ( n, wtime )

c*********************************************************************72
c
cc TEST03_SUB tests I4MAT_FLOYD.
c
c  Discussion:
c
c    The matrix size is input by the user.
c
c    The matrix A has the property that
c
c      A(I,J) = 1 if I is divisible by J.
c
c  Example:
c
c    N = 6
c
c    1 0 0 0 0 0
c    1 1 0 0 0 0
c    1 0 1 0 0 0
c    1 1 0 1 0 0
c    1 0 0 0 1 0
c    1 1 1 0 0 1
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    20 July 2011
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the size of the matrix.
c
c    Output, double precision WTIME, the CPU  time required by I4MAT_FLOYD.
c
      implicit none

      integer n

      integer a(n,n)
      integer huge
      integer i
      integer i4_huge
      integer j
      double precision time1
      double precision time2
      double precision wtime

      huge = i4_huge ( ) / 2

      do j = 1, n
        do i = 1, n
          if ( mod ( i, j ) .eq. 0 ) then
            a(i,j) = 1
          else
            a(i,j) = huge
          end if
        end do
      end do

      call cpu_time ( time1 )

      call i4mat_floyd ( n, a )

      call cpu_time ( time2 )

      wtime = time2 - time1

      return
      end
      subroutine test04 ( )

c*********************************************************************72
c
cc TEST04 uses Floyd's method for a triangulation.
c
c  Discussion:
c
c     8  41--42--43--44  45--46--47--48
c     |   | \ | \ | \ |   | \ | \ | \ |
c     7  33--34--35--36  37--38--39--40
c     |   | \ |                   | \ |
c     6  29--30                  31--32
c     |   | \ |                   | \ |
c     5  25--26                  27--28
c     |   | \ |                   | \ |
c     4  21--22                  23--24
c     |   | \ |                   | \ |
c     3  17--18                  19--20
c     |   | \ |                   | \ |
c     2   9--10--11--12--13--14--15--16
c     |   | \ | \ | \ | \ | \ | \ | \ |
c     1   1---2---3---4---5---6---7---8
c     |    
c     +---1---2---3---4---5---6---7---8
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 March 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer element_num
      parameter ( element_num = 46 )
      integer node_num
      parameter ( node_num = 48 )
  
      double precision d(node_num,node_num)
      integer element
      integer element_node(3,element_num)
      integer i
      integer j
      integer n1
      integer n2
      double precision r8_huge
      double precision r8vec_diff_norm
      double precision xy(2,node_num) 

      save element_node
      save xy

      data element_node /
     &   1,  2,  9, 
     &   2, 10,  9, 
     &   2,  3, 10, 
     &   3, 11, 10, 
     &   3,  4, 11, 
     &   4, 12, 11, 
     &   4,  5, 12, 
     &   5, 13, 12, 
     &   5,  6, 13, 
     &   6, 14, 13, 
     &   6,  7, 14, 
     &   7, 15, 14, 
     &   7,  8, 15, 
     &   8, 16, 15, 
     &   9, 10, 17, 
     &  10, 18, 17, 
     &  15, 16, 19, 
     &  16, 20, 19, 
     &  17, 18, 21, 
     &  18, 22, 21, 
     &  19, 20, 23, 
     &  20, 24, 23, 
     &  21, 22, 25, 
     &  22, 26, 25, 
     &  23, 24, 27, 
     &  24, 28, 27, 
     &  25, 26, 29, 
     &  26, 30, 29, 
     &  27, 28, 31, 
     &  28, 32, 31, 
     &  29, 30, 33, 
     &  30, 34, 33, 
     &  31, 32, 39, 
     &  32, 40, 39, 
     &  33, 34, 41, 
     &  34, 42, 41, 
     &  34, 35, 42, 
     &  35, 43, 42, 
     &  35, 36, 43, 
     &  36, 44, 43, 
     &  37, 38, 45, 
     &  38, 46, 45, 
     &  38, 39, 46, 
     &  39, 47, 46, 
     &  39, 40, 47, 
     &  40, 48, 47 /
      data xy /
     &  1.0, 1.0, 
     &  2.0, 1.0, 
     &  3.0, 1.0, 
     &  4.0, 1.0, 
     &  5.0, 1.0, 
     &  6.0, 1.0, 
     &  7.0, 1.0, 
     &  8.0, 1.0, 
     &  1.0, 2.0, 
     &  2.0, 2.0, 
     &  3.0, 2.0, 
     &  4.0, 2.0, 
     &  5.0, 2.0, 
     &  6.0, 2.0, 
     &  7.0, 2.0, 
     &  8.0, 2.0, 
     &  1.0, 3.0,  
     &  2.0, 3.0, 
     &  7.0, 3.0, 
     &  8.0, 3.0, 
     &  1.0, 4.0, 
     &  2.0, 4.0, 
     &  7.0, 4.0, 
     &  8.0, 4.0, 
     &  1.0, 5.0, 
     &  2.0, 5.0, 
     &  7.0, 5.0, 
     &  8.0, 5.0, 
     &  1.0, 6.0, 
     &  2.0, 6.0, 
     &  7.0, 6.0, 
     &  8.0, 6.0, 
     &  1.0, 7.0, 
     &  2.0, 7.0, 
     &  3.0, 7.0, 
     &  4.0, 7.0, 
     &  5.0, 7.0, 
     &  6.0, 7.0, 
     &  7.0, 7.0, 
     &  8.0, 7.0, 
     &  1.0, 8.0,  
     &  2.0, 8.0, 
     &  3.0, 8.0, 
     &  4.0, 8.0, 
     &  5.0, 8.0, 
     &  6.0, 8.0, 
     &  7.0, 8.0, 
     &  8.0, 8.0 /

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST04'
      write ( *, '(a)' ) '  Start with a triangulation, use R8_FLOYD'
      write ( *, '(a)' ) '  to determine the pairwise distance matrix.'
c
c  Must initialize distances to -1c
c
      do j = 1, node_num
        do i = 1, node_num
          d(i,j) = -1.0D+00
        end do
      end do
c
c  Diagonals are 0.
c
      do i = 1, node_num
        d(i,i) = 0.0D+00
      end do
c
c  Initialize D to the one-step distance.
c
      do element = 1, element_num
        n1 = element_node(3,element)
        do i = 1, 3
          n2 = element_node(i,element)
          d(n1,n2) = r8vec_diff_norm ( 2, xy(1:2,n1), xy(1:2,n2) )
          d(n2,n1) = d(n1,n2)
          n1 = n2
        end do
      end do
c
c  Reset -1 values to R8_HUGE.
c
      do j = 1, node_num
        do i = 1, node_num
          if ( d(i,j) == - 1.0D+00 ) then
            d(i,j) = r8_huge ( )
          end if
        end do
      end do
c
c  Update D to the N-1 step distance.
c
      call r8mat_floyd ( node_num, d )

      call r8mat_print ( node_num, node_num, d, '  Distance matrix' )

      return
      end

