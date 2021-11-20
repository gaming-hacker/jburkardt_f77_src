      program main

c*********************************************************************72
c
cc MAIN is the main program for TOMS097_PRB.
c
c  Discussion:
c
c    TOMS097_PRB tests the TOMS097 library.
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
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TOMS097_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the TOMS097 library.'

      call test01 ( )
      call test02 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TOMS097_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
c! TEST01 tests I4MAT_SHORTEST_PATH.
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
      implicit none

      integer n
      parameter ( n = 6 )

      integer a(n,n)
      integer i
      integer i4_huge
      integer j

      save a

      data a /  
     &  0, -1, -1, -1, -1, -1,
     &  2,  0, -1, -1, -1,  5,
     &  5,  7,  0, -1,  2, -1,
     & -1,  1,  4,  0, -1,  2,
     & -1, -1, -1,  3,  0,  4,
     & -1,  8, -1, -1,  3,  0 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) 
     &  '  I4MAT_SHORTEST_PATH uses Floyd''s algorithm to find the'
      write ( *, '(a)' ) 
     &  '  shortest distance between all pairs of nodes'
      write ( *, '(a)' ) 
     &  '  in a directed graph, starting from the initial array'
      write ( *, '(a)' ) '  of direct node-to-node distances.'

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  In the initial direct distance array, if'
      write ( *, '(a)' ) '    A(I,J) = HUGE,'
      write ( *, '(a)' ) 
     &  '  this indicates there is NO directed link from'
      write ( *, '(a)' ) 
     &  '  node I to node J.  In that case, the value of'
      write ( *, '(a)' ) '  of A(I,J) is essentially "infinity".'

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Initial direct-link distance matrix:'
      write ( *, '(a)' ) ' '

      do i = 1,  n
        write ( *, '(6i8)' ) ( a(i,j), j = 1, n )
      end do

      do j = 1, n
        do i = 1, n
          if ( a(i,j) .eq. -1 ) then
            a(i,j) = i4_huge ( )
          end if
        end do
      end do

      call i4mat_shortest_path ( n, a )

      do j = 1, n
        do i = 1, n
          if ( a(i,j) == i4_huge ( ) ) then
            a(i,j) = -1
          end if
        end do
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  In the final shortest distance array, if'
      write ( *, '(a)' ) '    A(I,J) = -1,'
      write ( *, '(a)' ) 
     &  '  this indicates there is NO directed path from'
      write ( *, '(a)' ) '  node I to node J.'

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Final distance matrix:'
      write ( *, '(a)' ) ' '

      do i = 1,  n
        write ( *, '(6i8)' ) ( a(i,j), j = 1, n )
      end do

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
c! TEST02 tests R8MAT_SHORTEST_PATH.
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
      implicit none

      integer n
      parameter ( n = 6 )

      double precision a(n,n)
      integer i
      integer j
      double precision r8_huge

      save a

      data a /
     &  0.0D+00, -1.0D+00, -1.0D+00, -1.0D+00, -1.0D+00, -1.0D+00,
     &  2.0D+00,  0.0D+00, -1.0D+00, -1.0D+00, -1.0D+00,  5.0D+00,
     &  5.0D+00,  7.0D+00,  0.0D+00, -1.0D+00,  2.0D+00, -1.0D+00,
     & -1.0D+00,  1.0D+00,  4.0D+00,  0.0D+00, -1.0D+00,  2.0D+00,
     & -1.0D+00, -1.0D+00, -1.0D+00,  3.0D+00,  0.0D+00,  4.0D+00,
     & -1.0D+00,  8.0D+00, -1.0D+00, -1.0D+00,  3.0D+00,  0.0D+00  /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) 
     &  '  R8MAT_SHORTEST_PATH uses Floyd''s algorithm to find the'
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

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Initial direct-link distance matrix:'
      write ( *, '(a)' ) ' '

      do i = 1,  n
        write ( *, '(6f10.4)' ) ( a(i,j), j = 1, n )
      end do

      do j = 1, n
        do i = 1, n
          if ( a(i,j) .eq. -1.0D+00 ) then
            a(i,j) = r8_huge ( )
          end if
        end do
      end do

      call r8mat_shortest_path ( n, a )

      do j = 1, n
        do i = 1, n
          if ( a(i,j) .eq. r8_huge ( ) ) then
            a(i,j) = -1.0D+00
          end if
        end do
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  In the final shortest distance array, if'
      write ( *, '(a)' ) '    A(I,J) = -1,'
      write ( *, '(a)' ) 
     &  '  this indicates there is NO directed path from'
      write ( *, '(a)' ) '  node I to node J.'

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Final distance matrix:'
      write ( *, '(a)' ) ' '

      do i = 1,  n
        write ( *, '(6f10.4)' ) ( a(i,j), j = 1, n )
      end do

      return
      end
