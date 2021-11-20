      program main

c*********************************************************************72
c
cc MAIN is the main program for BELLMAN_FORD_PRB.
c
c  Discussion:
c
c    BELLMAN_FORD_PRB tests the BELLMAN_FORD library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 November 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'BELLMAN_FORD_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the BELLMAN_FORD library.'

      call test01 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'BELLMAN_FORD_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 runs a simple test.
c
c  Discussion:
c
c    The correct distances are { 0, -6, -2, 3, 0, 0 }.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 November 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer e_num
      parameter ( e_num = 10 )
      integer v_num
      parameter ( v_num = 6 )

      integer e(2,e_num)
      double precision e_weight(e_num)
      integer predecessor(v_num)
      integer source
      double precision v_weight(v_num)
  
      save e
      save e_weight

      data e /
     &  2, 1, 
     &  5, 2,
     &  2, 3, 
     &  3, 5, 
     &  5, 1, 
     &  3, 6, 
     &  6, 1, 
     &  4, 3, 
     &  6, 4, 
     &  4, 1 /
      data e_weight /
     &  -3.0, 
     &   6.0, 
     &  -4.0, 
     &  -1.0, 
     &   4.0, 
     &  -2.0, 
     &   2.0, 
     &   8.0, 
     &  -3.0, 
     &   3.0 /

      source = 1

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST01:'
      write ( *, '(a)' ) '  Bellman-Ford shortest path algorithm.'

      write ( *, '(a)' ) ''
      write ( *, '(a,i4)' ) '  Number of vertices = ', v_num
      write ( *, '(a,i4)' ) '  Number of edges = ', e_num
      write ( *, '(a,i4)' ) '  The reference vertex is ', source

      call i4mat_transpose_print ( 2, e_num, e, '  The edge array:' )
      call r8vec_print ( e_num, e_weight, '  The edge weights:' )

      call bellman_ford ( v_num, e_num, source, e, e_weight, 
     &  v_weight, predecessor )

      call r8vec_print ( v_num, v_weight, "  The shortest distances:" )

      call i4vec_print ( v_num, predecessor, 
     &  '  The vertex predecessor parents for the shortest paths:' )

      return
      end
