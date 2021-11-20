      program main

c*********************************************************************72
c
cc MAIN is the main program for SPARSE_DISPLAY_PRB.
c
c  Discussion:
c
c    SPARSE_DISPLAY_PRB tests the SPARSE_DISPLAY library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 September 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SPARSE_DISPLAY_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the SPARSE_DISPLAY library.'

      call test01 ( )
      call test02 ( )
      call test03 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SPARSE_DISPLAY_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 tests SPY_GE for a general storage matrix.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 June 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      real ( kind = 8 ), allocatable :: a(:,:)
      character ( len = 255 ) header
      integer ( kind = 4 ) n
      integer ( kind = 4 ) nx
      integer ( kind = 4 ) ny
      integer ( kind = 4 ) seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) 
     &  '  SPY_GE generates a sparsity plot for a matrix stored'
      write ( *, '(a)' ) '  in general (GE) format.'

      nx = 5
      ny = 5
      call wathen_order ( nx, ny, n )
      seed = 123456789
      allocate ( a(1:n,1:n) )
      call wathen_ge ( nx, ny, n, seed, a )
      header = 'wathen_ge'

      call spy_ge ( n, n, a, header )

      deallocate ( a )

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 tests SPY_FILE in cases where the indices are in a file.
c
c  Discussion:
c
c    The files used in this example actually use negative column indices
c    because they were output by DEAL.II and intended to be passed directly
c    into GNUPLOT without any clever commands.
c
c    So my own "SPY_FILE" is currently set up to deal exactly with such
c    files, and hence, given sensible data will actually show a spy plot
c    that is transposed - still readable and all, but wrong way round.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 June 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      character * ( 255 ) filename
      character * ( 255 ) header

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) 
     &  '  SPY_FILE generates a sparsity plot for a matrix for'
      write ( *, '(a)' ) 
     &  '  which there exists a file containing all the pairs'
      write ( *, '(a)' ) '  (I,J) corresponding to nonzero entries.'

      header = 'before'
      filename = 'before_data.txt'

      call spy_file ( header, filename )

      header = 'after'
      filename = 'after_data.txt'

      call spy_file ( header, filename )

      return
      end
      subroutine test03 ( )

c*********************************************************************72
c
cc TEST03 tests SPY_GE for a general storage matrix.
c
c  Discussion:
c
c   It is not clear to me whether the plot being created is correctly
c   oriented.  I might be seeing the transpose of the matrix.
c    One way to check is to set up a moderate sized, highly asymmetric matrix.
c    In this case, I will create a certain upper triangular matrix.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 September 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 20 )
      integer n
      parameter ( n = 30 )

      double precision a(m,n)
      character * ( 255 ) header
      integer i
      integer j

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST03'
      write ( *, '(a)' ) 
     &  '  SPY_GE generates a sparsity plot for a matrix stored'
      write ( *, '(a)' ) '  in general (GE) format.'
      write ( *, '(a)' ) 
     &  '  To orient ourselves, generate an upper triangular matrix.'

      do i = 1, m
        do j = 1, n
          if ( j .lt. i .or. mod ( i - j, i ) .ne. 0 ) then
            a(i,j) = 0.0
          else
            a(i,j) = 1.0
          end if
        end do
      end do

      header = '20x30'

      call spy_ge ( m, n, a, header )

      return
      end
      subroutine r8mat_uniform_01 ( m, n, seed, r )

c*********************************************************************72
c
cc R8MAT_UNIFORM_01 returns a unit pseudorandom R8MAT.
c
c  Discussion:
c
c    An R8MAT is an array of R8's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 August 2004
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Paul Bratley, Bennett Fox, Linus Schrage,
c    A Guide to Simulation,
c    Springer Verlag, pages 201-202, 1983.
c
c    Bennett Fox,
c    Algorithm 647:
c    Implementation and Relative Efficiency of Quasirandom
c    Sequence Generators,
c    ACM Transactions on Mathematical Software,
c    Volume 12, Number 4, pages 362-376, 1986.
c
c    Peter Lewis, Allen Goodman, James Miller,
c    A Pseudo-Random Number Generator for the System/360,
c    IBM Systems Journal,
c    Volume 8, pages 136-143, 1969.
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns in the array.
c
c    Input/output, integer SEED, the "seed" value, which should NOT be 0.
c    On output, SEED has been updated.
c
c    Output, double precision R(M,N), the array of pseudorandom values.
c
      implicit none

      integer m
      integer n

      integer i
      integer j
      integer k
      integer seed
      double precision r(m,n)

      if ( seed .eq. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8MAT_UNIFORM_01 - Fatal error!'
        write ( *, '(a)' ) '  Input value of SEED = 0.'
        stop 1
      end if

      do j = 1, n

        do i = 1, m

          k = seed / 127773

          seed = 16807 * ( seed - k * 127773 ) - k * 2836

          if ( seed .lt. 0 ) then
            seed = seed + 2147483647
          end if

          r(i,j) = dble ( seed ) * 4.656612875D-10

        end do
      end do

      return
      end
      subroutine wathen_ge ( nx, ny, n, seed, a )

c*********************************************************************72
c
cc WATHEN_GE returns the Wathen matrix as a general storage (GE) matrix.
c
c  Discussion:
c
c    The Wathen matrix is a finite element matrix which is sparse.
c
c    The entries of the matrix depend in part on a physical quantity
c    related to density.  That density is here assigned random values between
c    0 and 100.
c
c    The matrix order N is determined by the input quantities NX and NY,
c    which would usually be the number of elements in the X and Y directions.
c    The value of N is
c
c      N = 3*NX*NY + 2*NX + 2*NY + 1,
c
c    The matrix is the consistent mass matrix for a regular NX by NY grid
c    of 8 node serendipity elements.
c
c    Here is an illustration for NX = 3, NY = 2:
c
c     23-24-25-26-27-28-29
c      |     |     |     |
c     19    20    21    22
c      |     |     |     |
c     12-13-14-15-16-17-18
c      |     |     |     |
c      8     9    10    11
c      |     |     |     |
c      1--2--3--4--5--6--7
c
c    For this example, the total number of nodes is, as expected,
c
c      N = 3 * 3 * 2 + 2 * 2 + 2 * 3 + 1 = 29
c
c    The matrix is symmetric positive definite for any positive values of the
c    density RHO(X,Y).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Nicholas Higham,
c    Algorithm 694: A Collection of Test Matrices in MATLAB,
c    ACM Transactions on Mathematical Software,
c    Volume 17, Number 3, September 1991, pages 289-305.
c
c    Andrew Wathen,
c    Realistic eigenvalue bounds for the Galerkin mass matrix,
c    IMA Journal of Numerical Analysis,
c    Volume 7, Number 4, October 1987, pages 449-457.
c
c  Parameters:
c
c    Input, integer NX, NY, values which determine the size
c    of the matrix.
c
c    Input, integer N, the number of rows and columns.
c
c    Input/output, integer SEED, the random number seed.
c
c    Output, double precision A(N,N), the matrix.
c
      implicit none

      integer n
      integer nx
      integer ny

      double precision a(n,n)
      double precision em(8,8)
      integer i
      integer j
      integer kcol
      integer krow
      integer node(8)
      double precision rho(nx,ny)
      integer seed

      save em

      data em /
     &   6.0, -6.0,  2.0, -8.0,  3.0, -8.0,  2.0, -6.0,
     &  -6.0, 32.0, -6.0, 20.0, -8.0, 16.0, -8.0, 20.0,
     &   2.0, -6.0,  6.0, -6.0,  2.0, -8.0,  3.0, -8.0,
     &  -8.0, 20.0, -6.0, 32.0, -6.0, 20.0, -8.0, 16.0,
     &   3.0, -8.0,  2.0, -6.0,  6.0, -6.0,  2.0, -8.0,     
     &  -8.0, 16.0, -8.0, 20.0, -6.0, 32.0, -6.0, 20.0,
     &   2.0, -8.0,  3.0, -8.0,  2.0, -6.0,  6.0, -6.0,
     &  -6.0, 20.0, -8.0, 16.0, -8.0, 20.0, -6.0, 32.0 /

      do j = 1, n
        do i = 1, n
          a(i,j) = 0.0D+00
        end do
      end do

      call r8mat_uniform_01 ( nx, ny, seed, rho )

      do j = 1, ny
        do i = 1, nx
          rho(i,j) = 100.0D+00 * rho(i,j)
        end do
      end do

      do j = 1, ny
        do i = 1, nx

          node(1) = 3 * j * nx + 2 * j + 2 * i + 1
          node(2) = node(1) - 1
          node(3) = node(1) - 2
          node(4) = ( 3 * j - 1 ) * nx + 2 * j + i - 1
          node(5) = ( 3 * j - 3 ) * nx + 2 * j + 2 * i - 3
          node(6) = node(5) + 1
          node(7) = node(5) + 2
          node(8) = node(4) + 1

          do krow = 1, 8
            do kcol = 1, 8
              a(node(krow),node(kcol)) = a(node(krow),node(kcol))       
     &      + rho(i,j) * em(krow,kcol)
            end do
          end do

        end do
      end do

      return
      end
      subroutine wathen_order ( nx, ny, n )

c*********************************************************************72
c
cc WATHEN_ORDER returns the order of the WATHEN matrix.
c
c  Discussion:
c
c    N = 3 * 3 * 2 + 2 * 2 + 2 * 3 + 1 = 29
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Nicholas Higham,
c    Algorithm 694: A Collection of Test Matrices in MATLAB,
c    ACM Transactions on Mathematical Software,
c    Volume 17, Number 3, September 1991, pages 289-305.
c
c    Andrew Wathen,
c    Realistic eigenvalue bounds for the Galerkin mass matrix,
c    IMA Journal of Numerical Analysis,
c    Volume 7, 1987, pages 449-457.
c
c  Parameters:
c
c    Input, integer NX, NY, values which determine the size of A.
c
c    Output, integer N, the order of the matrix,
c    as determined by NX and NY.
c
      implicit none

      integer n
      integer nx
      integer ny

      n = 3 * nx * ny + 2 * nx + 2 * ny + 1

      return
      end

