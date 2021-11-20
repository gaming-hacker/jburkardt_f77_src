program main

!*****************************************************************************80
!
!! MAIN is the main program for TOMS097_PRB.
!
!  Discussion:
!
!    TOMS097_PRB tests the TOMS097 library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 March 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TOMS097_PRB'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the TOMS097 library.'

  call test01 ( )
  call test02 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TOMS097_PRB'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 tests I4_SHORTEST_PATH.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 March 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 6

  integer ( kind = 4 ), dimension ( n, n ) :: a = reshape ( (/ &
     0, -1, -1, -1, -1, -1, &
     2,  0, -1, -1, -1,  5, &
     5,  7,  0, -1,  2, -1, &
    -1,  1,  4,  0, -1,  2, &
    -1, -1, -1,  3,  0,  4, &
    -1,  8, -1, -1,  3,  0  &
    /), (/ n, n /) )
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  I4_SHORTEST_PATH uses Floyd''s algorithm to find the'
  write ( *, '(a)' ) '  shortest distance between all pairs of nodes'
  write ( *, '(a)' ) '  in a directed graph, starting from the initial array'
  write ( *, '(a)' ) '  of direct node-to-node distances.'

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  In the initial direct distance array, if'
  write ( *, '(a)' ) '    A(I,J) = HUGE,'
  write ( *, '(a)' ) '  this indicates there is NO directed link from'
  write ( *, '(a)' ) '  node I to node J.  In that case, the value of'
  write ( *, '(a)' ) '  of A(I,J) is essentially "infinity".'

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Initial direct-link distance matrix:'
  write ( *, '(a)' ) ' '

  do i = 1,  n
    write ( *, '(6i8)' ) a(i,1:n)
  end do

  do j = 1, n
    do i = 1, n
      if ( a(i,j) == -1 ) then
        a(i,j) = huge ( a(i,j) )
      end if
    end do
  end do 

  call i4_shortest_path ( n, a )

  do j = 1, n
    do i = 1, n
      if ( a(i,j) == huge ( a(i,j) ) ) then
        a(i,j) = -1
      end if
    end do
  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  In the final shortest distance array, if'
  write ( *, '(a)' ) '    A(I,J) = -1,'
  write ( *, '(a)' ) '  this indicates there is NO directed path from'
  write ( *, '(a)' ) '  node I to node J.'

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Final distance matrix:'
  write ( *, '(a)' ) ' '

  do i = 1,  n
    write ( *, '(6i8)' ) a(i,1:n)
  end do

  return
end
subroutine test02 ( )

!*****************************************************************************80
!
!! TEST02 tests R8_SHORTEST_PATH.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 March 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 6

  real ( kind = 8 ), dimension ( n, n ) :: a = reshape ( (/ &
     0.0D+00, -1.0D+00, -1.0D+00, -1.0D+00, -1.0D+00, -1.0D+00, &
     2.0D+00,  0.0D+00, -1.0D+00, -1.0D+00, -1.0D+00,  5.0D+00, &
     5.0D+00,  7.0D+00,  0.0D+00, -1.0D+00,  2.0D+00, -1.0D+00, &
    -1.0D+00,  1.0D+00,  4.0D+00,  0.0D+00, -1.0D+00,  2.0D+00, &
    -1.0D+00, -1.0D+00, -1.0D+00,  3.0D+00,  0.0D+00,  4.0D+00, &
    -1.0D+00,  8.0D+00, -1.0D+00, -1.0D+00,  3.0D+00,  0.0D+00  &
    /), (/ n, n /) )
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST02'
  write ( *, '(a)' ) '  R8MAT_FLOYO uses Floyd''s algorithm to find the'
  write ( *, '(a)' ) '  shortest distance between all pairs of nodes'
  write ( *, '(a)' ) '  in a directed graph, starting from the initial array'
  write ( *, '(a)' ) '  of direct node-to-node distances.'

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  In the initial direct distance array, if'
  write ( *, '(a)' ) '    A(I,J) = -1,'
  write ( *, '(a)' ) '  this indicates there is NO directed link from'
  write ( *, '(a)' ) '  node I to node J.  In that case, the value of'
  write ( *, '(a)' ) '  of A(I,J) is essentially "infinity".'

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Initial direct-link distance matrix:'
  write ( *, '(a)' ) ' '

  do i = 1,  n
    write ( *, '(6f10.4)' ) a(i,1:n)
  end do

  do j = 1, n
    do i = 1, n
      if ( a(i,j) == -1.0D+00 ) then
        a(i,j) = huge ( a(i,j) )
      end if
    end do
  end do 

  call r8_shortest_path ( n, a )

  do j = 1, n
    do i = 1, n
      if ( a(i,j) == huge ( a(i,j) ) ) then
        a(i,j) = -1.0D+00
      end if
    end do
  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  In the final shortest distance array, if'
  write ( *, '(a)' ) '    A(I,J) = -1,'
  write ( *, '(a)' ) '  this indicates there is NO directed path from'
  write ( *, '(a)' ) '  node I to node J.'

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Final distance matrix:'
  write ( *, '(a)' ) ' '

  do i = 1,  n
    write ( *, '(6f10.4)' ) a(i,1:n)
  end do

  return
end
