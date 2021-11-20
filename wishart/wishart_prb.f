      program main

c*********************************************************************72
c
cc MAIN is the main program for WISHART_PRB.
c
c  Discussion:
c
c    WISHART_PRB tests the WISHART library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 October 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'WISHART_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the WISHART library.'

      call test01 ( )
      call test02 ( )
      call test03 ( )
      call test04 ( )
      call test05 ( )
      call test06 ( )
      call test07 ( )
      call test08 ( )
      call test09 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'WISHART_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 demonstrates the unit Wishart sampling function.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 August 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 5 )

      integer df
      integer it_max
      integer it_num
      double precision lambda(n_max)
      integer n
      integer rot_num
      double precision v(n_max,n_max)
      double precision w(n_max,n_max)
c
c  Initialize the RNGLIB library.
c
      call initialize ( )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST01:'
      write ( *, '(a)' ) 
     &  '  We can compute sample unit Wishart matrices by:'
      write ( *, '(a)' ) '    W = wishart_unit_sample ( n, df )'
c
c  Set the parameters and call.
c
      n = 5
      df = 8
      call wishart_unit_sample ( n, df, w )
      call r8mat_print ( n, n, w, '  wishart_unit_sample ( 5, 8 ):' )
c
c  Calling again yields a new matrix.
c
      call wishart_unit_sample ( n, df, w )
      call r8mat_print ( n, n, w, '  wishart_unit_sample ( 5, 8 ):' )
c
c  Reduce DF
c
      n = 5
      df = 5
      call wishart_unit_sample ( n, df, w )
      call r8mat_print ( n, n, w, '  wishart_unit_sample ( 5, 5 ):' )
c
c  Try a smaller matrix.
c
      n = 3
      df = 5
      call wishart_unit_sample ( n, df, w )
      call r8mat_print ( n, n, w, '  wishart_unit_sample ( 3, 5 ):' )
c
c  What is the eigendecomposition of the matrix?
c
      it_max = 50

      call jacobi_eigenvalue ( n, w, it_max, v, lambda, it_num, 
     &  rot_num )
      call r8mat_print ( n, n, v, '  Eigenvectors of previous matrix:' )
      call r8vec_print ( n, lambda, 
     &  '  Eigenvalues of previous matrix:' )

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 demonstrates the unit Bartlett sampling function.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 August 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 5 )

      integer df
      integer it_max
      integer it_num
      double precision lambda(n_max)
      integer n
      integer rot_num
      double precision t(n_max,n_max)
      double precision v(n_max,n_max)
      double precision w(n_max,n_max)
c
c   Initialize the RNGLIB library.
c
      call initialize ( )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST02:'
      write ( *, '(a)' ) 
     &  '  We can compute sample unit Bartlett matrices by:'
      write ( *, '(a)' ) '    call bartlett_unit_sample ( n, df, t )'
c
c   Set the parameters and call.
c
      n = 5
      df = 8
      call bartlett_unit_sample ( n, df, t )
      call r8mat_print ( n, n, t, 
     &  '  bartlett_unit_sample ( 5, 8, t ):' )
c
c   Calling again yields a new matrix.
c
      call bartlett_unit_sample ( n, df, t )
      call r8mat_print ( n, n, t, 
     &  '  bartlett_unit_sample ( 5, 8, t ):' )
c
c   Reduce DF.
c
      n = 5
      df = 5
      call bartlett_unit_sample ( n, df, t )
      call r8mat_print ( n, n, t, 
     &  '  bartlett_unit_sample ( 5, 5, t ):' )
c
c   Try a smaller matrix.
c
      n = 3
      df = 5
      call bartlett_unit_sample ( n, df, t )
      call r8mat_print ( n, n, t, 
     &  '  bartlett_unit_sample ( 3, 5, t ):' )
c
c   What is the eigendecomposition of the matrix T' * T?
c
      call r8mat_mtm ( n, n, n, t, t, w )

      it_max = 50

      call jacobi_eigenvalue ( n, w, it_max, v, lambda, it_num, 
     &  rot_num )
      call r8mat_print ( n, n, v, '  Eigenvectors of previous matrix:' )
      call r8vec_print ( n, lambda, 
     &  '  Eigenvalues of previous matrix:' )

      return
      end
      subroutine test03 ( )

c*********************************************************************72
c
cc TEST03 compares the unit Wishart and Bartlett sample matrices.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 August 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 5 )

      integer df
      double precision diff
      integer n
      double precision r8mat_norm_fro_affine
      double precision t(n_max,n_max)
      double precision tt(n_max,n_max)
      double precision w(n_max,n_max)
c
c   Initialize the RNGLIB library.
c   Normally, we would do this just once, here at the beginning.
c   In this example, however, we really want to do it just before
c   we call each of the sampling routines, so that they both access
c   the same set of random numbers...
c
      call initialize ( )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST03:'
      write ( *, '(a)' ) 
     &  '  Verify that, if using the same set of random numbers,'
      write ( *, '(a)' ) '    W = T'' * T,'
      write ( *, '(a)' ) '  where'
      write ( *, '(a)' ) '    W = wishart_unit_sample ( n, df )'
      write ( *, '(a)' ) '    T = bartlett_unit_sample ( n, df )'
c
c   Set the parameters.
c
      n = 5
      df = 8
c
c   Initialize the random number package and compute W.
c
      call initialize ( )
      call wishart_unit_sample ( n, df, w )
c
c   Initialize the random number package again, and compute T.
c
      call initialize ( )
      call bartlett_unit_sample ( n, df, t )
c
c   Compute T' * T.
c
      call r8mat_mtm ( n, n, n, t, t, tt )
c
c   Compare T'T to W.
c
      diff = r8mat_norm_fro_affine ( n, n, w, tt )
      write ( *, '(a)' ) ''
      write ( *, '(a,g14.6)' ) '  Frobenius norm of error is ', diff

      return
      end
      subroutine test04 ( )

c*********************************************************************72
c
cc TEST04 demonstrates the Wishart sampling function.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 August 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 5 )

      integer df
      integer i
      integer it_max
      integer it_num
      integer j
      double precision lambda(n_max)
      integer n
      integer rot_num
c
c  Note that R is an upper triangular matrix,
c  whose entries here are listed in column major order.
c
      double precision r(3,3)
      double precision sigma(n_max,n_max)
      double precision sigma_diag(5)
      double precision v(n_max,n_max)
      double precision w(n_max,n_max)

      save r
      save sigma_diag

      data r /
     &  5.0D+00, 0.0D+00, 0.0D+00, 
     &  1.0D+00, 4.0D+00, 0.0D+00, 
     &  3.0D+00, 2.0D+00, 6.0D+00 /
      data sigma_diag /
     &  1.0D+00, 2.0D+00, 3.0D+00, 4.0D+00, 5.0D+00 /
c
c  Initialize the RNGLIB library.
c
      call initialize ( )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST04:'
      write ( *, '(a)' ) '  We can compute sample Wishart matrices by:'
      write ( *, '(a)' ) '    W = wishart_sample ( n, df, sigma )'
c
c   Set the parameters and call.
c
      n = 5
      df = 8
      call r8mat_identity ( n, sigma )

      call wishart_sample ( n, df, sigma, w )
      call r8mat_print ( n, n, w, 
     &  '  wishart_sample ( 5, 8, Identity ):' )
c
c   Calling again yields a new matrix.
c
      call wishart_sample ( n, df, sigma, w )
      call r8mat_print ( n, n, w, 
     &  '  wishart_sample ( 5, 8, Identity ):' )
c
c   Try a diagonal matrix.
c
      call r8mat_diagonal ( n, sigma_diag, sigma )

      call wishart_sample ( n, df, sigma, w )
      call r8mat_print ( n, n, w, 
     &  '  wishart_sample ( 5, 8, diag(1,2,3,4,5) ):' )
c
c   Try a smaller matrix.  Sigma must be positive definite symmetric.
c
      n = 3
      df = 3
      call r8mat_mtm ( n, n, n, r, r, sigma )
      call r8mat_print ( n, n, sigma, '  Set covariance SIGMA:' )

      call wishart_sample ( n, df, sigma, w )
      call r8mat_print ( n, n, w, '  wishart_sample ( 3, 3, sigma ):' )
c
c   What is the eigendecomposition of this matrix?
c
      it_max = 50

      call jacobi_eigenvalue ( n, w, it_max, v, lambda, it_num, 
     &  rot_num )
      call r8mat_print ( n, n, v, '  Eigenvectors of previous matrix:' )
      call r8vec_print ( n, lambda, 
     &  '  Eigenvalues of previous matrix:' )

      return
      end
      subroutine test05 ( )

c*********************************************************************72
c
cc TEST05 demonstrates the Bartlett sampling function.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 August 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 5 )

      integer df
      integer it_max
      integer it_num
      double precision lambda(n_max)
      integer n
c
c  Note that R is an upper triangular matrix,
c  whose entries here are listed in column major order.
c
      double precision r(3,3)
      integer rot_num
      double precision sigma(n_max,n_max)
      double precision sigma_diag(5)
      double precision t(n_max,n_max)
      double precision v(n_max,n_max)
      double precision w(n_max,n_max)

      save r
      save sigma_diag

      data r /
     &  5.0D+00, 0.0D+00, 0.0D+00, 
     &  1.0D+00, 4.0D+00, 0.0D+00, 
     &  3.0D+00, 2.0D+00, 6.0D+00 /
      data sigma_diag /
     &  1.0D+00, 2.0D+00, 3.0D+00, 4.0D+00, 5.0D+00 /
c
c   Initialize the RNGLIB library.
c
      call initialize ( )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST05:'
      write ( *, '(a)' ) '  We can compute sample Bartlett matrices by:'
      write ( *, '(a)' ) '    T = bartlett_sample ( n, df, sigma )'
c
c   Set the parameters and call.
c
      n = 5
      df = 8
      call r8mat_identity ( n, sigma )

      call bartlett_sample ( n, df, sigma, t )
      call r8mat_print ( n, n, t, 
     &  '  bartlett_sample ( 5, 8, Identity ):' )
c
c   Calling again yields a new matrix.
c
      call bartlett_sample ( n, df, sigma, t )
      call r8mat_print ( n, n, t, 
     &  '  bartlett_sample ( 5, 8, Identity ):' )
c
c   Try a diagonal matrix.
c
      call r8mat_diagonal ( n, sigma_diag, sigma )

      call bartlett_sample ( n, df, sigma, t )
      call r8mat_print ( n, n, t, 
     &  '  bartlett_sample ( 5, 8, diag(1,2,3,4,5) ):' )
c
c   Try a smaller matrix.
c
      n = 3
      df = 3
      call r8mat_mtm ( n, n, n, r, r, sigma )
      call r8mat_print ( n, n, sigma, '  Set covariance SIGMA:' )

      call bartlett_sample ( n, df, sigma, t )
      call r8mat_print ( n, n, t, '  bartlett_sample ( 3, 3, sigma ):' )
c
c   What is the eigendecomposition of T' * T?
c
      call r8mat_mtm ( n, n, n, t, t, w )
      it_max = 50

      call jacobi_eigenvalue ( n, w, it_max, v, lambda, it_num, 
     &  rot_num )
      call r8mat_print ( n, n, v, '  Eigenvectors of previous matrix:' )
      call r8vec_print ( n, lambda, 
     &  '  Eigenvalues of previous matrix:' )

      return
      end
      subroutine test06 ( )

c*********************************************************************72
c
cc TEST06 compares the Wishart and Bartlett sample matrices.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 August 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 5 )

      integer df
      double precision diff
      integer n
c
c  Note that R is an upper triangular matrix,
c  whose entries here are listed in column major order.
c
      double precision r(3,3)
      double precision r8mat_norm_fro_affine
      double precision sigma(n_max,n_max)
      double precision t(n_max,n_max)
      double precision tt(n_max,n_max)
      double precision w(n_max,n_max)

      save r

      data r /
     &  5.0D+00, 0.0D+00, 0.0D+00, 
     &  1.0D+00, 4.0D+00, 0.0D+00, 
     &  3.0D+00, 2.0D+00, 6.0D+00 /
c
c   Initialize the RNGLIB library.
c   Normally, we would do this just once, here at the beginning.
c   In this example, however, we really want to do it just before
c   we call each of the sampling routines, so that they both access
c   the same set of random numbers...
c
      call initialize ( )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST06:'
      write ( *, '(a)' ) 
     &  '  Verify that, if using the same set of random numbers,'
      write ( *, '(a)' ) '    W = T'' * T,'
      write ( *, '(a)' ) '  where'
      write ( *, '(a)' ) '    W = wishart_sample ( n, df, sigma )'
      write ( *, '(a)' ) '    T = bartlett_sample ( n, df, sigma )'
c
c   Set the parameters.
c
      n = 3
      df = 5
      call r8mat_mtm ( n, n, n, r, r, sigma )
      call r8mat_print ( n, n, sigma, "  Covariance SIGMA:" )
c
c   Initialize the random number package and compute W.
c
      call initialize ( )
      call wishart_sample ( n, df, sigma, w )
c
c   Initialize the random number package again, and compute T.
c
      call initialize ( )
      call bartlett_sample ( n, df, sigma, t )
c
c   Compute T' * T.
c
      call r8mat_mtm ( n, n, n, t, t, tt )
c
c   Compare T'T to W.
c
      diff = r8mat_norm_fro_affine ( n, n, w, tt )
      write ( *, '(a)' ) ''
      write ( *, '(a,g14.6)' ) '  Frobenius norm of error is ', diff

      return
      end
      subroutine test07 ( )

c*********************************************************************72
c
cc TEST07 demonstrates a property of the Wishart distribution.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 August 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 3 )

      integer df
      double precision diff
      integer i
      integer j
      integer k
      integer n
c
c  Note that R is an upper triangular matrix,
c  whose entries here are listed in column major order.
c
      double precision r(3,3)
      double precision r8mat_norm_fro_affine
      integer sample_num
      double precision sigma(n_max,n_max)
      double precision w(n_max,n_max)
      double precision w_average(n_max,n_max)

      save r

      data r /
     &  5.0D+00, 0.0D+00, 0.0D+00, 
     &  1.0D+00, 4.0D+00, 0.0D+00, 
     &  3.0D+00, 2.0D+00, 6.0D+00 /
c
c   Initialize the RNGLIB library.
c
      call initialize ( )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST07:'
      write ( *, '(a)' ) 
     &  '  For given values of N, DF, SIGMA, the random'
      write ( *, '(a)' ) '  matrices from the Wishart distribution:'
      write ( *, '(a)' ) '    W = wishart_sample ( n, df, sigma )'
      write ( *, '(a)' ) '  should have mean DF * SIGMA.'
c
c   Set the parameters.
c
      n = 3
      write ( *, '(a,i4)' ) '  Fix N = ', n
      df = 5
      write ( *, '(a,i4)' ) '  Fix DF = ', df

      call r8mat_mtm ( n, n, n, r, r, sigma )
      call r8mat_print ( n, n, sigma, '  Fix covariance SIGMA:' )
c
c   Sample many times and average.
c
      sample_num = 1000

      do j = 1, n
        do i = 1, n
          w_average(i,j) = 0.0D+00
        end do
      end do

      do k = 1, sample_num
        call wishart_sample ( n, df, sigma, w )
        do j = 1, n
          do i = 1, n
            w_average(i,j) = w_average(i,j) + w(i,j)
          end do
        end do
      end do

      do j = 1, n
        do i = 1, n
          w_average(i,j) = w_average(i,j) / dble ( sample_num )
        end do
      end do
c
c   Compare SIGMA and W_SAMPLE / DF.
c
      do j = 1, n
        do i = 1, n
          w_average(i,j) = w_average(i,j) / dble ( df )
        end do
      end do

      call r8mat_print ( n, n, w_average, '  W_Average / DF: ' )

      diff = r8mat_norm_fro_affine ( n, n, sigma, w_average )
      write ( *, '(a)' ) ''
      write ( *, '(a,g14.6)' ) 
     &  '  Frobenius norm of SIGMA-W_average/DF = ', diff

      return
      end
      subroutine test08 ( )

c*********************************************************************72
c
cc TEST08 samples the unit Wishart and unit Wishart inverse matrices.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 October 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 )

      integer df
      double precision diff
      integer i
      double precision ident(n,n)
      integer j
      double precision m(n,n)
      double precision r8mat_norm_fro_affine
      double precision w(n,n)
      double precision wm(n,n)
c
c   Initialize the RNGLIB library.
c   Normally, we would do this just once, here at the beginning.
c   In this example, however, we really want to do it just before
c   we call each of the sampling routines, so that they both access
c   the same set of random numbers...
c
      call initialize ( )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST08:'
      write ( *, '(a)' ) 
     &  '  Verify that, if using the same set of random numbers,'
      write ( *, '(a)' ) '    inverse(W) = M,'
      write ( *, '(a)' ) '  where'
      write ( *, '(a)' ) '    W = wishart_unit_sample ( n, df )'
      write ( *, '(a)' ) '    M = wishart_unit_sample_inverse ( n, df )'
c
c   Set the parameters.
c
      df = 8
c
c   Initialize the random number package and compute W.
c
      call initialize ( )
      call wishart_unit_sample ( n, df, w )
c
c   Initialize the random number package again, and compute T.
c
      call initialize ( )
      call wishart_unit_sample_inverse ( n, df, m )
c
c   Compute W * M
c
      call r8mat_mm ( n, n, n, w, m, wm )
c
c   Compare WM to I
c
      do j = 1, n
        do i = 1, n
          ident(i,j) = 0.0D+00
        end do
        ident(j,j) = 1.0D+00
      end do

      diff = r8mat_norm_fro_affine ( n, n, wm, ident )
      write ( *, '(a)' ) ''
      write ( *, '(a,g14.6)' ) '  Frobenius norm of error is ', diff

      return
      end
      subroutine test09 ( )

c*********************************************************************72
c
cc TEST09 samples the Wishart and unit Wishart inverse matrices.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 October 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5 )

      integer df
      double precision diff
      integer i
      double precision ident(n,n)
      integer j
      double precision m(n,n)
      double precision r(n,n)
      double precision r8mat_norm_fro_affine
      double precision sigma(n,n)
      double precision w(n,n)
      double precision wm(n,n)

      save r

      data r /
     &  3.0D+00, 0.0D+00, 0.0D+00, 0.0D+00, 0.0D+00, 
     &  1.0D+00, 7.0D+00, 0.0D+00, 0.0D+00, 0.0D+00, 
     &  1.0D+00, 1.0D+00, 5.0D+00, 0.0D+00, 0.0D+00, 
     &  1.0D+00, 2.0D+00, 1.0D+00, 4.0D+00, 0.0D+00, 
     &  1.0D+00, 3.0D+00, 3.0D+00, 2.0D+00, 6.0D+00 /
c
c   Initialize the RNGLIB library.
c   Normally, we would do this just once, here at the beginning.
c   In this example, however, we really want to do it just before
c   we call each of the sampling routines, so that they both access
c   the same set of random numbers...
c
      call initialize ( )

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST09:'
      write ( *, '(a)' ) 
     &  '  Verify that, if using the same set of random numbers,'
      write ( *, '(a)' ) '    inverse(W) = M,'
      write ( *, '(a)' ) '  where'
      write ( *, '(a)' ) '    W = wishart_sample ( n, df, sigma )'
      write ( *, '(a)' ) 
     &  '    M = wishart_sample_inverse ( n, df, sigma )'
c
c   Set the parameters.
c
      df = 8
      call r8mat_mtm ( n, n, n, r, r, sigma )
c
c   Initialize the random number package and compute W.
c
      call initialize ( )
      call wishart_sample ( n, df, sigma, w )
c
c   Initialize the random number package again, and compute T.
c
      call initialize ( )
      call wishart_sample_inverse ( n, df, sigma, m )
c
c   Compute W * M
c
      call r8mat_mm ( n, n, n, w, m, wm )
c
c   Compare WM to I
c
      do j = 1, n
        do i = 1, n
          ident(i,j) = 0.0D+00
        end do
        ident(j,j) = 1.0D+00
      end do

      diff = r8mat_norm_fro_affine ( n, n, wm, ident )
      write ( *, '(a)' ) ''
      write ( *, '(a,g14.6)' ) '  Frobenius norm of error is ', diff

      return
      end