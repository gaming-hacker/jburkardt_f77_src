      program main

c*********************************************************************72
c
cc MAIN is the main program for FEM1D_PACK_PRB.
c
c  Discussion:
c
c    FEM1D_PACK_PRB tests the FEM1D_PACK library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 July 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FEM1D_PACK_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the FEM1D_PACK library.'

      call test01 ( )
      call test02 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FEM1D_PACK_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 verifies LOCAL_BASIS_1D.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 July 2013
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

      integer node_num
      parameter ( node_num = 4 )

      integer i
      integer j
      double precision node_x(node_num)
      double precision phi(node_num)
      double precision phi_matrix(node_num,node_num)
      double precision r8_uniform_ab
      double precision r8vec_sum
      integer seed
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01:'
      write ( *, '(a)' ) 
     &  '  LOCAL_BASIS_1D evaluates the local basis functions'
      write ( *, '(a)' ) '  for a 1D element.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Test that the basis functions, evaluated at the nodes,'
      write ( *, '(a)' ) '  form the identity matrix.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Number of nodes = ', node_num

      node_x(1) = 1.0D+00
      node_x(2) = 2.0D+00
      node_x(3) = 4.0D+00
      node_x(4) = 4.5D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Node coordinates:'
      write ( *, '(a)' ) ' '
      do j = 1, node_num
        write ( *, '(2x,i8,2x,f7.3,2x,f7.3)' ) j, node_x(j)
      end do

      do j = 1, node_num
        x = node_x(j)
        call local_basis_1d ( node_num, node_x, x, phi_matrix(1,j) )
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  A(I,J) = PHI(I) at node (J):'
      write ( *, '(a)' ) ' '
      do i = 1, node_num
        write ( *, '(2x,10f7.3)' ) phi_matrix(i,1:node_num)
      end do

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  The PHI functions should sum to 1 at random X values:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       X        Sum ( PHI(:)(X) )'
      write ( *, '(a)' ) ' '

      do j = 1, 5
        x = r8_uniform_ab ( 1.0D+00, 4.5D+00, seed )
        call local_basis_1d ( node_num, node_x, x, phi )
        write ( *, '(2x,g14.6,2x,g14.6)' ) 
     &    x, r8vec_sum ( node_num, phi )
      end do

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 verifies LOCAL_BASIS_PRIME_1D.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 July 2013
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

      integer node_num
      parameter ( node_num = 4 )

      double precision dphidx(node_num)
      double precision dphidx_matrix(node_num,node_num)
      integer i
      integer j
      double precision node_x(node_num)
      double precision r8_uniform_ab
      double precision r8vec_sum
      integer seed
      double precision x

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST02:'
      write ( *, '(a)' ) 
     &  '  LOCAL_BASIS_PRIME_1D evaluates the local basis function'
      write ( *, '(a)' ) '  derivatives for a 1D element.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Number of nodes = ', node_num

      node_x(1) = 1.0D+00
      node_x(2) = 2.0D+00
      node_x(3) = 4.0D+00
      node_x(4) = 4.5D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Node coordinates:'
      write ( *, '(a)' ) ' '
      do j = 1, node_num
        write ( *, '(2x,i8,2x,f7.3,2x,f7.3)' ) j, node_x(j)
      end do

      do j = 1, node_num
        x = node_x(j)
        call local_basis_prime_1d ( node_num, node_x, x, 
     &    dphidx_matrix(1,j) )
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  A(I,J) = dPHIdx(I) at node(J):'
      write ( *, '(a)' ) '  The diagonal should be 0.'
      write ( *, '(a)' ) '  Columns should sum to 0.'
      write ( *, '(a)' ) ' '
      do i = 1, node_num
        write ( *, '(2x,10f7.3)' ) dphidx_matrix(i,1:node_num)
      end do

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  The dPHIdx functions should sum to 0 at random X values:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       X        Sum ( dPHIdx(:)(X) )'
      write ( *, '(a)' ) ' '

      do j = 1, 5
        x = r8_uniform_ab ( 1.0D+00, 4.5D+00, seed )
        call local_basis_prime_1d ( node_num, node_x, x, dphidx )
        write ( *, '(2x,g14.6,2x,g14.6)' ) 
     &    x, r8vec_sum ( node_num, dphidx )
      end do

      return
      end
