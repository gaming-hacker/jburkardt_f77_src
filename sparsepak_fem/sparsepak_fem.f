      program main

c*********************************************************************72
c
cc MAIN is the main program for SPARSEPAK_FEM.
c
c  Discussion:
c
c    SPARSEPAK_FEM sets up a linear system for SPARSEPAK to store and solve.
c
c    The linear system is derived from a 2D finite element grid on a square,
c    using quadratic triangular elements:
c
c      O-O-O-O-O-O-O-O-O
c      |  /|  /|  /|  /|
c      O O O O O O O O O
c      |/  |/  |/  |/  |
c      O-O-O-O-O-O-O-O-O
c      |  /|  /|  /|  /|
c      O O O O O O O O O
c      |/  |/  |/  |/  |
c      O-O-O-O-O-O-O-O-O
c
c  Modified:
c
c    29 November 2008
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SPARSEPAK_FLOW'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Set up a finite element problem.'
      write ( *, '(a)' ) '  Solve with SPARSPAK,'
      write ( *, '(a)' ) '  a direct sparse solver.'
c
c  TEST01 is failing, claiming the matrix is not positive definite.
c
      call test01 ( )
c
c  TEST02 is failing, claiming the matrix is not positive definite.
c
      call test02 ( )
c
c  TEST03 is failing, claiming lack of storage.
c
      call test03 ( )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SPARSEPAK_FLOW'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 uses the RCM method.
c
c  Modified:
c
c    31 December 2008
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer, parameter :: maxadj = 3000
      integer, parameter :: maxenv = 1200
      integer, parameter :: nx = 5
      integer, parameter :: ny = 5
      integer, parameter :: np = ( 2 * nx - 1 ) * ( 2 * ny - 1 )
      integer, parameter :: neqn = np

      double precision diag(neqn)
      double precision env(maxenv)
      integer i
      integer iadj(maxadj)
      integer iband
      integer ienv
      integer ierror
      integer invprm(neqn)
      integer jadj(neqn+1)
      integer mask(neqn)
      integer nadj
      integer perm(neqn)
      double precision rhs(neqn)
      integer xenv(neqn+1)
      integer xls(neqn)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  Use the RCM method.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Finite element grid uses '
      write ( *, '(a,i8)' ) '  NX = ', nx
      write ( *, '(a,i8)' ) '  NY = ', ny
      write ( *, '(a,i8)' ) '  NP = ', np
c
c  Initialize the permutation vectors.
c
      do i = 1, neqn
        perm(i) = i
      end do

      do i = 1, neqn
        invprm(i) = i
      end do
c
c  Store the adjacency information.
c
      call adjset ( iadj, jadj, maxadj, nadj, neqn, np, nx, ny )
c
c  Display adjacency information.
c
      call adj_print ( neqn, nadj, jadj, iadj )
c
c  Get a picture of the matrix.
c
      call shomat ( iadj, iband, ienv, invprm, nadj, neqn, perm, jadj )
c
c  Generate the RCM ordering.
c
      call genrcm ( neqn, jadj, iadj, perm, mask, xls )
c
c  Compute the inverse ordering.
c
      call perm_inverse ( neqn, perm, invprm )
c
c  Get a picture of the matrix.
c
      call shomat ( iadj, iband, ienv, invprm, nadj, neqn, perm, jadj )
c
c  Compute the envelope.
c
      call fnenv ( neqn, jadj, iadj, perm, invprm, xenv, ienv, iband )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  The envelope size is ', ienv
      write ( *, '(a,i8)' ) '  The bandwidth is     ', iband
c
c  Set RHS, DIAG, ENV
c
      call setsy1 ( diag, env, iadj, invprm, jadj, maxadj, maxenv,
     &  neqn, rhs, xenv )
c
c  Factor the matrix.
c
      write ( *, '(a)' ) 'DEBUG: TEST01 calls ESFCT'
      call esfct ( neqn, xenv, env, diag, ierror )

      if ( ierror /= 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'TEST01 - Fatal error!'
        write ( *, '(a)' ) '  The matrix is not positive definite.'
        return
      end if
c
c  Solve the system.
c
      call elslv ( neqn, xenv, env, diag, rhs )

      call euslv ( neqn, xenv, env, diag, rhs )
c
c  Unpermute the solution.
c
      call permrv ( neqn, rhs, perm )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'Solution:'
      write ( *, '(5g14.6)' ) ( rhs(i), i = 1, neqn )

      return
      end
      subroutine setsy1 ( diag, env, iadj, invprm, jadj, maxadj,
     &  maxenv, neqn, rhs, xenv )

c*********************************************************************72
c
cc SETSY1 stores the numerical values defining problem 1.
c
c  Discussion:
c
c    There is only one nonzero right hand side entry.
c    The matrix diagonal entries are all 2.
c    The nonzero offdiagonal entries are all -1.
c
c  Modified:
c
c    31 December 2008
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer maxadj
      integer maxenv
      integer neqn

      double precision diag(neqn)
      double precision env(maxenv)
      integer i
      integer ierror
      integer iadj(maxadj)
      integer invprm(neqn)
      integer isub
      integer j
      integer jadj(neqn+1)
      integer jsub
      double precision rhs(neqn)
      double precision value
      integer xenv(neqn+1)
c
c  First zero out storage
c
      do i = 1, neqn
        rhs(i) = 0.0D+00
      end do

      do i = 1, neqn
        diag(i) = 0.0D+00
      end do

      do i = 1, maxenv
        env(i) = 0.0D+00
      end do
c
c  Set the nonzero elements of the right hand side vector.
c
      isub = 6
      value = 11.0D+00

      call addrhs ( invprm, isub, neqn, rhs, value )
c
c  Set the diagonal entries of the matrix.
c
      do i = 1, neqn
        diag(i) = 50.0D+00
      end do
c
c  Set the off diagonal terms of the matrix.
c
      do i = 1, neqn

        isub = i

        do j = jadj(i), jadj(i+1) - 1

          jsub = iadj(j)
          value = - 1.0D+00

          call addrcm ( isub, jsub, value, invprm, diag, xenv, env, 
     &      ierror, neqn )

        end do

      end do

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 uses the RQT method.
c
c  Modified:
c
c    29 November 2008
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer, parameter :: maxadj = 3000
      integer, parameter :: maxblk = 10
      integer, parameter :: maxenv = 600
      integer, parameter :: maxnon = 600
      integer, parameter :: nx = 5
      integer, parameter :: ny = 5
      integer, parameter :: np = ( 2 * nx - 1 ) * ( 2 * ny - 1 )
      integer, parameter :: neqn = np

      integer bnum(neqn)
      double precision diag(neqn)
      double precision env(maxenv)
      integer father(neqn)
      integer first(neqn)
      integer i
      integer iadj(maxadj)
      integer iband
      integer ienv
      integer ierror
      integer invprm(neqn)
      integer jadj(neqn+1)
      integer ls(neqn)
      integer mask(neqn)
      integer nadj
      integer nblks
      integer nodlvl(neqn)
      integer nofnz
      double precision nonz(maxnon)
      integer nzsubs(maxnon)
      integer perm(neqn)
      double precision rhs(neqn)
      integer subg(neqn)
      double precision temp(neqn)
      integer xblk(maxblk+1)
      integer xenv(neqn+1)
      integer xls(neqn)
      integer xnonz(neqn+1)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) '  Use the RQT method.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Finite element grid uses '
      write ( *, '(a,i8)' ) '  NX = ', nx
      write ( *, '(a,i8)' ) '  NY = ', ny
      write ( *, '(a,i8)' ) '  NP = ', np
c
c  Initialize the permutation vectors.
c
      do i = 1, neqn
        perm(i) = i
      end do

      do i = 1, neqn
        invprm(i) = i
      end do
c
c  Store the adjacency information.
c
      call adjset ( iadj, jadj, maxadj, nadj, neqn, np, nx, ny )
c
c  Display adjacency information.
c
      call adj_print ( neqn, nadj, jadj, iadj )
c
c  Get a picture of the matrix.
c
      call shomat ( iadj, iband, ienv, invprm, nadj, neqn, perm, jadj )
c
c  Generate the RQT ordering.
c
      call genrqt ( neqn, jadj, iadj, nblks, xblk, perm, xls, ls, 
     &  nodlvl )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) ' Number of blocks is ',nblks
c
c  Get a picture of the matrix.
c
      call shomat ( iadj, iband, ienv, invprm, nadj, neqn, perm, jadj )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  The envelope size is ', ienv

      call bshufl ( jadj, iadj, perm, nblks, xblk, bnum, mask, subg, 
     &  xls, neqn )
c
c  Compute the inverse ordering.
c
      call perm_inverse ( neqn, perm, invprm )
c
c  Get a picture of the matrix.
c
      call shomat ( iadj, iband, ienv, invprm, nadj, neqn, perm, jadj )
c
c  Determine the quotient tree adjacency structure.
c
      call fntadj ( jadj, iadj, perm, nblks, xblk, father, mask, neqn )
c
c  Determine the envelope index vector for the diagonal blocks.
c
      call fntenv ( jadj, iadj, perm, invprm, nblks, xblk, xenv, ienv, 
     &  neqn )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST02 - Returned from FNTENV'
      write ( *, '(a,i8)' ) '  Number of diagonal blocks =', nblks
      write ( *, '(a,i8)' ) '  Envelope size IENV=', ienv
c
c  Get number of off-block-diagonal nonzeros.
c
      nofnz = maxnon

      call fnofnz ( jadj, iadj, perm, invprm, nblks, xblk, xnonz, 
     &  nzsubs, nofnz, neqn )

      write ( *, '(a,i8)' ) 
     &  '  FNOFNZ returns number of off-block-diagonal nonzeros = ',
     &  nofnz
c
c  Set RHS, DIAG, ENV.
c
      call setsy2 ( diag, env, iadj, invprm, jadj, maxadj, maxenv,
     &  neqn, nonz, nzsubs, rhs, xenv, xnonz )
c
c  Factor the system.
c
      call tsfct ( nblks, xblk, father, diag, xenv, env, xnonz,
     &  nonz, nzsubs, temp, first, ierror )

      if ( ierror /= 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'TEST02 - Fatal error!'
        write ( *, '(a)' ) '  The matrix is not positive definite.'
        return
      end if
c
c  Solve the system.
c
      call tsslv ( nblks, xblk, diag, xenv, env, xnonz, nonz, nzsubs,
     &  rhs, temp, neqn )
c
c  Unpermute the solution stored in RHS.
c
      call permrv ( neqn, rhs, perm )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Solution:'
      write ( *, '(5g14.6)' ) ( rhs(i), i = 1, neqn )

      return
      end
      subroutine setsy2 ( diag, env, iadj, invprm, jadj, maxadj,
     &  maxenv, neqn, nonz, nzsubs, rhs, xenv, xnonz )

c*********************************************************************72
c
cc SETSY2 stores the numerical values defining problem 2.
c
c  Discussion:
c
c    There is only one nonzero right hand side entry.
c    The matrix diagonal entries are all 2.
c    The nonzero offdiagonal entries are all -1.
c
c  Modified:
c
c    29 November 2008
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer maxadj
      integer maxenv
      integer neqn

      double precision diag(neqn)
      double precision env(maxenv)
      integer iadj(maxadj)
      integer i
      integer invprm(neqn)
      integer isub
      integer j
      integer jadj(neqn+1)
      integer jsub
      double precision nonz(*)
      integer nzsubs(*)
      double precision rhs(neqn)
      double precision value
      integer xenv(neqn+1)
      integer xnonz(neqn+1)
c
c  First zero out storage.
c
      do i = 1, neqn
        rhs(i) = 0.0D+00
      end do

      do i = 1, neqn
        diag(i) = 0.0D+00
      end do

      do i = 1, maxenv
        env(i) = 0.0D+00
      end do
c
c  Set the nonzero elements of the right hand side vector.
c
      isub = 6
      value = 11.0D+00

      call addrhs ( invprm, isub, neqn, rhs, value )
c
c  Set the diagonal entries of the matrix.
c
      do i = 1, neqn
        diag(i) = 50.0D+00
      end do
c
c  Set the off diagonal terms of the matrix.
c
      do i = 1, neqn

        isub = i

        do j = jadj(i), jadj(i+1)-1

          jsub = iadj(j)
          value = -1.0D+00

          call addrqt ( isub, jsub, value, invprm, diag, xenv, env,
     &      xnonz, nonz, nzsubs, neqn )

        end do

      end do

      return
      end
      subroutine adjset ( iadj, jadj, maxadj, nadj, neqn, np, nx, ny )

c*********************************************************************72
c
cc ADJSET sets up the adjacency information.
c
c  Modified:
c
c    29 November 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, integer IADJ(MAXADJ), the adjacency indices.
c
c    Output, integer JADJ(NEQN+1), the adjacency pointers.
c
c    Input, integer MAXADJ, the allocated storage for IADJ.
c
c    Output, integer NADJ, the number of adjaceny indices stored.
c
c    Output, integer NEQN, the number of equations.
c
c    Input, integer NP, the number of nodes.
c
c    Input, integer NX, the number of elements in the X direction.
c
c    Input, integer NY, the number of elements in the Y direction.
c
      implicit none

      integer maxadj
      integer np

      integer i
      integer icol
      integer ip
      integer irow
      integer iadj(maxadj)
      integer j
      integer jadj(neqn+1)
      integer k
      integer l
      integer nab(6)
      integer nadj
      integer neqn
      integer nx
      integer ny

      i = 0
      j = 0
      call setadj ( iadj, i, j, maxadj, nadj, neqn, jadj )

      do ip = 1, np

        icol = ((ip-1)/(2*ny-1))+1
        irow = mod((ip-1),2*ny-1)+1

        if ((mod(irow,2)==1.and.mod(icol,2)==1).and.
     &     (icol/=2*nx-1).and.(irow/=2*ny-1))then
c
c           3
c          /|
c         / |
c        /  |
c       6   5
c      /    |
c     /     |
c    /      |
c   1---4---2
c
          nab(1) = ip
          nab(2) = ip + 2
          nab(3) = ip + 2*(2*ny-1)+2
          nab(4) = ip + 1
          nab(5) = ip + (2*ny-1)+2
          nab(6) = ip + (2*ny-1)+1

          do k = 1, 6
            i = nab(k)
            do l = 1, 6
              j = nab(l)
              call setadj ( iadj, i, j, maxadj, nadj, neqn, jadj )
            end do
          end do
c
c
c  3---5---2
c  |      /
c  |     /
c  |    /
c  6   4
c  |  /
c  | /
c  |/
c  1
c
          nab(1) = ip
          nab(2) = ip + 2*(2*ny-1)+2
          nab(3) = ip + 2*(2*ny-1)
          nab(4) = ip +   (2*ny-1)+1
          nab(5) = ip + 2*(2*ny-1)+1
          nab(6) = ip +   (2*ny-1)

          do k = 1, 6
            i = nab(k)
            do l = 1, 6
              j = nab(l)
              call setadj ( iadj, i, j, maxadj, nadj, neqn, jadj )
            end do
          end do

        endif

      end do

      return
      end
      subroutine test03 ( )

c*********************************************************************72
c
cc TEST03 tests the 1WD method.
c
c  Modified:
c
c    29 November 2008
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer, parameter :: maxadj = 2000
      integer, parameter :: maxblk = 10
      integer, parameter :: maxenv = 600
      integer, parameter :: maxnon = 2000
      integer, parameter :: nx = 5
      integer, parameter :: ny = 5
      integer, parameter :: np = ( 2 * nx - 1 ) * ( 2 * ny - 1 )
      integer, parameter :: neqn = np

      double precision diag(neqn)
      double precision env(maxenv)
      integer father(neqn)
      integer first(neqn)
      integer i
      integer iadj(maxadj)
      integer iband
      integer ienv
      integer ierror
      integer invprm(neqn)
      integer jadj(neqn+1)
      integer ls(neqn)
      integer marker(neqn)
      integer mask(neqn)
      integer nadj
      integer nblks
      double precision nonz(maxnon)
      integer nzsubs(maxnon)
      integer perm(neqn)
      integer rchset(neqn)
      double precision rhs(neqn)
      double precision temp(neqn)
      integer xblk(maxblk+1)
      integer xenv(neqn+1)
      integer xnonz(neqn+1)
      integer xls(neqn)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST03'
      write ( *, '(a)' ) '  Use the 1WD method.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Finite element grid uses '
      write ( *, '(a,i8)' ) '  NX = ',nx
      write ( *, '(a,i8)' ) '  NY = ',ny
      write ( *, '(a,i8)' ) '  NP = ',np
c
c  Initialize the permutation vectors.
c
      do i = 1, neqn
        perm(i) = i
      end do

      do i = 1, neqn
        invprm(i) = i
      end do
c
c  Store the adjacency information.
c
      call adjset ( iadj, jadj, maxadj, nadj, neqn, np, nx, ny )
c
c  Display adjacency information.
c
      call adj_print ( neqn, nadj, jadj, iadj )
c
c  Get a picture of the matrix.
c
      call shomat ( iadj, iband, ienv, invprm, nadj, neqn, perm, jadj )
c
c  Reorder the variables and equations.
c
      call gen1wd ( neqn, jadj, iadj, mask, nblks, xblk, perm, xls, ls )
c
c  Compute the inverse ordering.
c
      call perm_inverse ( neqn, perm, invprm )

      call fnbenv ( jadj, iadj, perm, invprm, nblks, xblk, xenv, ienv,
     &  mask, marker, rchset, neqn )
c
c  Get a picture of the matrix.
c
      call shomat ( iadj, iband, ienv, invprm, nadj, neqn, perm, jadj )
c
c  Set RHS, DIAG, ENV.
c
      call setsy2 ( diag, env, iadj, invprm, jadj, maxadj, maxenv,
     &  neqn, nonz, nzsubs, rhs, xenv, xnonz )
c
c  Factor the system.
c
      call tsfct ( nblks, xblk, father, diag, xenv, env, xnonz,
     &  nonz, nzsubs, temp, first, ierror )

      if ( ierror /= 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'TEST03 - Fatal error!'
        write ( *, '(a)' ) '  The matrix is not positive definite.'
        return
      end if
c
c  Solve the system.
c
      call tsslv ( nblks, xblk, diag, xenv, env, xnonz, nonz, nzsubs,
     &  rhs, temp )
c
c  Unpermute solution stored in RHS.
c
      call permrv ( neqn, rhs, perm )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'Solution:'
      write ( *, '(5g14.6)' ) ( rhs(i), i = 1, neqn )

      return
      end
