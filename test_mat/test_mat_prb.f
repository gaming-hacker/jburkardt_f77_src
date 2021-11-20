      program main

c*********************************************************************72
c
cc MAIN is the main program for TEST_MAT_PRB.
c
c  Discussion:
c
c    TEST_MAT_PRB tests the TEST_MAT library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    15 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST_MAT_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the TEST_MAT library.'
c
c  Utilities.
c
      call bvec_next_grlex_test ( )
      call legendre_zeros_test ( )
      call mertens_test ( )
      call moebius_test ( )
      call r8mat_is_eigen_left_test ( )
      call r8mat_is_eigen_right_test ( )
      call r8mat_is_llt_test ( )
      call r8mat_is_null_left_test ( )
      call r8mat_is_null_right_test ( )
      call r8mat_is_solution_test ( )
      call r8mat_norm_fro_test ( )
c
c  Important things.
c
      call test_condition ( )
      call test_determinant ( )
      call test_eigen_left ( )
      call test_eigen_right ( )
      call test_inverse ( )
      call test_llt ( )
      call test_null_left ( )
      call test_null_right ( )
      call test_plu ( )
      call test_solution ( )
      call test_type ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST_MAT_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine bvec_next_grlex_test ( )

c*********************************************************************72
c
cc BVEC_NEXT_GRLEX_TEST tests BVEC_NEXT_GRLEX.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    13 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 4 )
 
      integer b(n)
      integer i
      integer j

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'BVEC_NEXT_GRLEX_TEST'
      write ( *, '(a)' ) 
     &  '  BVEC_NEXT_GRLEX computes binary vectors in GRLEX order.'
      write ( *, '(a)' ) ''

      b(1:n) = 0

      do i = 0, 16
        write ( *, '(2x,i2,a)', advance = 'no' ) i, ':  '
        do j = 1, n
          write ( *, '(i1)', advance = 'no' ) b(j)
        end do
        write ( *, '(a)' ) ''
        call bvec_next_grlex ( n, b )
      end do

      return
      end
      subroutine legendre_zeros_test ( )

c*********************************************************************72
c
cc LEGENDRE_ZEROS_TEST tests LEGENDRE_ZEROS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 February 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 7 )

      double precision l(n_max)
      integer n

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'LEGENDRE_ZEROS_TEST:'
      write ( *, '(a)' ) '  LEGENDRE_ZEROS computes the zeros of the'
      write ( *, '(a)' ) '  N-th Legendre polynomial.'

      do n = 1, 7
        call legendre_zeros ( n, l )
        call r8vec_print ( n, l, '  Legendre zeros' )
      end do

      return
      end
      subroutine mertens_test ( )

c*********************************************************************72
c
cc MERTENS_TEST tests MERTENS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    22 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer c
      integer c2
      integer mertens
      integer n
      integer n_data

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'MERTENS_TEST'
      write ( *, '(a)' ) '  MERTENS computes the Mertens function.'
      write ( *, '(a)' ) ' ' 
      write ( *, '(a)' ) '         N     Exact   MERTENS(N)'
      write ( *, '(a)' ) ' '
     
      n_data = 0

10    continue

        call mertens_values ( n_data, n, c )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        c2 = mertens ( n )

        write ( *, '(2x,i8,2x,i10,2x,i10)' ) n, c, c2

      go to 10

20    continue
     
      return
      end
      subroutine moebius_test ( )

c*********************************************************************72
c
cc MOEBIUS_TEST tests MOEBIUS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    22 March 2009
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer c
      integer c2
      integer n
      integer n_data

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'MOEBIUS_TEST'
      write ( *, '(a)' ) '  MOEBIUS computes the Moebius function.'
      write ( *, '(a)' ) ' ' 
      write ( *, '(a)' ) '         N     Exact   MOEBIUS(N)'
      write ( *, '(a)' ) ' '
     
      n_data = 0

10    continue

        call moebius_values ( n_data, n, c )

        if ( n_data .eq. 0 ) then
          go to 20
        end if

        call moebius ( n, c2 )

        write ( *, '(2x,i8,2x,i10,2x,i10)' ) n, c, c2

      go to 10

20    continue
     
      return
      end
      subroutine r8mat_is_eigen_left_test ( )

c*********************************************************************72
c
cc R8MAT_IS_EIGEN_LEFT_TEST tests R8MAT_IS_EIGEN_LEFT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 4 )
      integer k
      parameter ( k = 4 )
c
c  This is the CARRY ( 4.0, 4 ) matrix.
c
      double precision a(n,n)
      double precision lam(n)
      double precision value
      double precision x(n,k)

      save a
      save lam
      save x

      data a /
     &   0.13671875D+00,   0.05859375D+00,
     &   0.01953125D+00,   0.00390625D+00, 
     &   0.60546875D+00,   0.52734375D+00,
     &   0.39453125D+00,   0.25390625D+00, 
     &   0.25390625D+00,   0.39453125D+00,
     &   0.52734375D+00,   0.60546875D+00, 
     &   0.00390625D+00,   0.01953125D+00,
     &   0.05859375D+00,   0.13671875D+00 /
      data lam /
     &   1.000000000000000D+00, 
     &   0.250000000000000D+00, 
     &   0.062500000000000D+00, 
     &   0.015625000000000D+00 /
      data x /
     &   1.0D+00, 11.0D+00, 11.0D+00,  1.0D+00, 
     &   1.0D+00,  3.0D+00, -3.0D+00, -1.0D+00, 
     &   1.0D+00, -1.0D+00, -1.0D+00,  1.0D+00, 
     &   1.0D+00, -3.0D+00,  3.0D+00, -1.0D+00 /

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'R8MAT_IS_EIGEN_LEFT_TEST:'
      write ( *, '(a)' ) 
     &  '  R8MAT_IS_EIGEN_LEFT tests the error in the left eigensystem'
      write ( *, '(a)' ) '    A'' * X - X * LAMBDA = 0'

      call r8mat_print ( n, n, a, '  Matrix A:' )
      call r8mat_print ( n, k, x, '  Eigenmatrix X:' )
      call r8vec_print ( n, lam, '  Eigenvalues LAM:' )

      call r8mat_is_eigen_left ( n, k, a, x, lam, value )

      write ( *, '(a)' ) ''
      write ( *, '(a,g14.6)' ) 
     &  '  Frobenius norm of A''*X-X*LAMBDA is ', value

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'R8MAT_IS_EIGEN_LEFT_TEST'
      write ( *, '(a)' ) '  Normal end of execution.'

      return
      end
      subroutine r8mat_is_eigen_right_test ( )

c*********************************************************************72
c
cc R8MAT_IS_EIGEN_RIGHT_TEST tests R8MAT_IS_EIGEN_RIGHT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 4 )
      integer k
      parameter ( k = 4 )
c
c  This is the CARRY ( 4.0, 4 ) matrix.
c
      double precision a(n,n)
      double precision lam(n)
      double precision value
      double precision x(n,k)

      save a
      save lam
      save x

      data a /
     &   0.13671875D+00,   0.05859375D+00,
     &   0.01953125D+00,   0.00390625D+00, 
     &   0.60546875D+00,   0.52734375D+00,
     &   0.39453125D+00,   0.25390625D+00, 
     &   0.25390625D+00,   0.39453125D+00,
     &   0.52734375D+00,   0.60546875D+00, 
     &   0.00390625D+00,   0.01953125D+00,
     &   0.05859375D+00,   0.13671875D+00 /
      data lam /
     &   1.000000000000000D+00, 
     &   0.250000000000000D+00, 
     &   0.062500000000000D+00, 
     &   0.015625000000000D+00 /
      data x /
     &  1.0D+00,  1.0D+00,  1.0D+00,  1.0D+00, 
     &  6.0D+00,  2.0D+00, -2.0D+00, -6.0D+00, 
     & 11.0D+00, -1.0D+00, -1.0D+00, 11.0D+00, 
     &  6.0D+00, -2.0D+00,  2.0D+00, -6.0D+00 /

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'R8MAT_IS_EIGEN_RIGHT_TEST:'
      write ( *, '(a)' ) 
     &  '  R8MAT_IS_EIGEN_RIGHT tests error in the right eigensystem'
      write ( *, '(a)' ) '    A * X - X * LAMBDA = 0'

      call r8mat_print ( n, n, a, '  Matrix A:' )
      call r8mat_print ( n, k, x, '  Eigenmatrix X:' )
      call r8vec_print ( n, lam, '  Eigenvalues LAM:' )

      call r8mat_is_eigen_right ( n, k, a, x, lam, value )

      write ( *, '(a)' ) ''
      write ( *, '(a,g14.6)' ) 
     &  '  Frobenius norm of A*X-X*LAMBDA is ', value

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'R8MAT_IS_EIGEN_RIGHT_TEST'
      write ( *, '(a)' ) '  Normal end of execution.'

      return
      end
      subroutine r8mat_is_llt_test ( )

c*********************************************************************72
c
cc R8MAT_IS_LLT_TEST tests R8MAT_IS_LLT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    15 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 4 )
      integer n
      parameter ( n = 4 )

      double precision a(4,4)
      double precision enorm
      double precision l(4,4)

      save a
      save l

      data a /
     &  2.0D+00, 1.0D+00, 0.0D+00, 0.0D+00, 
     &  1.0D+00, 2.0D+00, 1.0D+00, 0.0D+00, 
     &  0.0D+00, 1.0D+00, 2.0D+00, 1.0D+00, 
     &  0.0D+00, 0.0D+00, 1.0D+00, 2.0D+00 /
      data l /
     &  1.414213562373095D+00, 0.707106781186547D+00, 
     &  0.0D+00,               0.0D+00,               
     &  0.0D+00,               1.224744871391589D+00, 
     &  0.816496580927726D+00, 0.0D+00,               
     &  0.0D+00,               0.0D+00,               
     &  1.154700538379251D+00, 0.866025403784439D+00, 
     &  0.0D+00,               0.0D+00,               
     &  0.0D+00,               1.118033988749895D+00 /

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'R8MAT_IS_LLT_TEST:'
      write ( *, '(a)' ) 
     &  '  R8MAT_IS_LLT tests the error in a lower triangular'
      write ( *, '(a)' ) 
     &  '  Cholesky factorization A = L * L'' by looking at'
      write ( *, '(a)' ) '    A - L * L'''

      call r8mat_print ( m, m, a, '  Matrix A:' );
      call r8mat_print ( m, n, l, '  Factor L:' );

      call r8mat_is_llt ( m, n, a, l, enorm )

      write ( *, '(a)' ) ''
      write ( *, '(a,g14.6)' ) '  Frobenius norm of A-L*L'' is ', enorm

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'R8MAT_IS_LLT_TEST'
      write ( *, '(a)' ) '  Normal end of execution.'

      return
      end
      subroutine r8mat_is_null_left_test ( )

c*********************************************************************72
c
cc R8MAT_IS_NULL_LEFT_TEST tests R8MAT_IS_NULL_LEFT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 3 )
      integer n
      parameter ( n = 3 )

      double precision a(m,n)
      double precision enorm
      double precision x(m)

      save a
      save x

      data a /
     &  1.0D+00, 4.0D+00, 7.0D+00, 
     &  2.0D+00, 5.0D+00, 8.0D+00, 
     &  3.0D+00, 6.0D+00, 9.0D+00 /
      data x /
     &  1.0D+00, -2.0D+00, 1.0D+00 /

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'R8MAT_IS_NULL_LEFT_TEST:'
      write ( *, '(a)' ) 
     &  '  R8MAT_IS_NULL_LEFT tests whether the M vector X'
      write ( *, '(a)' ) 
     &  '  is a left null vector of A, that is, x''*A=0.'

      call r8mat_print ( m, n, a, '  Matrix A:' )
      call r8vec_print ( m, x, '  Vector X:' )

      call r8mat_is_null_left ( m, n, a, x, enorm )

      write ( *, '(a)' ) ''
      write ( *, '(a,g14.6)' ) '  Frobenius norm of X''*A is ', enorm

      return
      end
      subroutine r8mat_is_null_right_test ( )

c*********************************************************************72
c
cc R8MAT_IS_NULL_RIGHT_TEST tests R8MAT_IS_NULL_RIGHT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 3 )
      integer n
      parameter ( n = 3 )

      double precision a(m,n)
      double precision enorm
      double precision x(n)

      save a
      save x

      data a /
     &  1.0D+00, 4.0D+00, 7.0D+00, 
     &  2.0D+00, 5.0D+00, 8.0D+00, 
     &  3.0D+00, 6.0D+00, 9.0D+00 /
      data x /
     &  1.0D+00, -2.0D+00, 1.0D+00 /

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'R8MAT_IS_NULL_RIGHT_TEST:'
      write ( *, '(a)' ) 
     &  '  R8MAT_IS_NULL_RIGHT tests whether the N vector X'
      write ( *, '(a)' ) 
     &  '  is a right null vector of A, that is, A*X=0.'

      call r8mat_print ( m, n, a, '  Matrix A:' )
      call r8vec_print ( n, x, '  Vector X:' )

      call r8mat_is_null_right ( m, n, a, x, enorm )

      write ( *, '(a)' ) ''
      write ( *, '(a,g14.6)' ) '  Frobenius norm of A*X is ', enorm

      return
      end
      subroutine r8mat_is_solution_test ( )

c*********************************************************************72
c
cc R8MAT_IS_SOLUTION_TEST tests R8MAT_IS_SOLUTION.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 February 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a(10,10)
      double precision b(10,10)
      double precision enorm
      integer i4_hi
      integer i4_lo
      integer i4_uniform_ab
      integer k
      integer m
      integer n
      double precision r8_hi
      double precision r8_lo
      integer seed
      double precision x(10,10)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'R8MAT_IS_SOLUTION_TEST:'
      write ( *, '(a)' ) 
     &  '  R8MAT_IS_SOLUTION tests whether X is the solution of'
      write ( *, '(a)' ) 
     &  '  A*X=B by computing the Frobenius norm of the residual.'
c
c  Get random shapes.
c
      i4_lo = 1
      i4_hi = 10
      seed = 123456789
      m = i4_uniform_ab ( i4_lo, i4_hi, seed )
      n = i4_uniform_ab ( i4_lo, i4_hi, seed )
      k = i4_uniform_ab ( i4_lo, i4_hi, seed )
c
c  Get a random A.
c
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      call r8mat_uniform_ab ( m, n, r8_lo, r8_hi, seed, a )
c
c  Get a random X.
c
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      call r8mat_uniform_ab ( n, k, r8_lo, r8_hi, seed, x )
c
c  Compute B = A * X.
c
      call r8mat_mm ( m, n, k, a, x, b )
c
c  Compute || A*X-B||
c
      call r8mat_is_solution ( m, n, k, a, x, b, enorm )
  
      write ( *, '(a)' ) ''
      write ( *, '(a,i2,a,i2)' ) '  A is ', m, ' by ', n
      write ( *, '(a,i2,a,i2)' ) '  X is ', n, ' by ', k
      write ( *, '(a,i2,a,i2)' ) '  B is ', m, ' by ', k
      write ( *, '(a,g14.6)' ) '  Frobenius error in A*X-B is ', enorm

      return
      end
      subroutine r8mat_norm_fro_test ( )

c*********************************************************************72
c
cc R8MAT_NORM_FRO_TEST tests R8MAT_NORM_FRO.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 December 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 5 )
      integer n
      parameter ( n = 4 )

      double precision a(m,n)
      integer i
      integer j
      integer k
      double precision r8mat_norm_fro
      double precision t1
      double precision t2

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_NORM_FRO_TEST'
      write ( *, '(a)' ) 
     &  '  R8MAT_NORM_FRO computes the Frobenius norm of an R8MAT;'

      t1 = 0.0D+00
      k = 0
      do i = 1, m
        do j = 1, n
          k = k + 1
          a(i,j) = dble ( k )
          t1 = t1 + dble ( k * k )
        end do
      end do

      t1 = sqrt ( t1 )

      call r8mat_print ( m, n, a, '  A:' )

      t2 = r8mat_norm_fro ( m, n, a )

      write ( *, '(a)' ) ''
      write ( *, '(a,g14.6)' ) '  Expected norm = ', t1
      write ( *, '(a,g14.6)' ) '  Computed norm = ', t2

      return
      end
      subroutine test_condition ( )

c*********************************************************************72
c
cc TEST_CONDITION tests the condition number computations.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 April 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 100 )

      double precision a(n_max,n_max)
      double precision a_norm
      double precision alpha
      double precision b(n_max,n_max)
      double precision b_norm
      double precision beta
      double precision cond1
      double precision cond2
      integer i
      integer n
      double precision r8_hi
      double precision r8_lo
      double precision r8_uniform_ab
      double precision r8mat_norm_l1
      integer seed
      character * ( 20 ) title
      double precision x(n_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST_CONDITION'
      write ( *, '(a)' ) '  Compute the L1 condition number of an'
      write ( *, '(a)' ) '  example of each test matrix'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Title                    N      COND            COND'
      write ( *, '(a)' ) ' '
c
c  AEGERTER
c
      title = 'AEGERTER'
      n = 5
      call aegerter_condition ( n, cond1 )

      call aegerter ( n, a )
      call aegerter_inverse ( n, b )
      a_norm = r8mat_norm_l1 ( n, n, a )
      b_norm = r8mat_norm_l1 ( n, n, b )
      cond2 = a_norm * b_norm

      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, cond1, cond2
c
c  BAB
c
      title = 'BAB'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      beta = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call bab_condition ( n, alpha, beta, cond1 )

      call bab ( n, alpha, beta, a )
      call bab_inverse ( n, alpha, beta, b )
      a_norm = r8mat_norm_l1 ( n, n, a )
      b_norm = r8mat_norm_l1 ( n, n, b )
      cond2 = a_norm * b_norm

      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, cond1, cond2
c
c  BAUER
c
      title = 'BAUER'
      n = 6
      call bauer_condition ( cond1 )

      call bauer ( a )
      call bauer_inverse ( b )
      a_norm = r8mat_norm_l1 ( n, n, a )
      b_norm = r8mat_norm_l1 ( n, n, b )
      cond2 = a_norm * b_norm

      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, cond1, cond2
c
c  BIS
c
      title = 'BIS'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      beta = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call bis_condition ( alpha, beta, n, cond1 )

      call bis ( alpha, beta, n, n, a );
      call bis_inverse ( alpha, beta, n, b )
      a_norm = r8mat_norm_l1 ( n, n, a )
      b_norm = r8mat_norm_l1 ( n, n, b )
      cond2 = a_norm * b_norm

      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, cond1, cond2
c
c  BIW
c
      title = 'BIW'
      n = 5
      call biw_condition ( n, cond1 )

      call biw ( n, a );
      call biw_inverse ( n, b )
      a_norm = r8mat_norm_l1 ( n, n, a )
      b_norm = r8mat_norm_l1 ( n, n, b )
      cond2 = a_norm * b_norm

      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, cond1, cond2
c
c  BODEWIG
c
      title = 'BODEWIG'
      n = 4
      call bodewig_condition ( cond1 )

      call bodewig ( a )
      call bodewig_inverse ( b )
      a_norm = r8mat_norm_l1 ( n, n, a )
      b_norm = r8mat_norm_l1 ( n, n, b )
      cond2 = a_norm * b_norm

      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, cond1, cond2
c
c  BOOTHROYD
c
      title = 'BOOTHROYD'
      n = 5
      call boothroyd_condition ( n, cond1 )

      call boothroyd ( n, a )
      call boothroyd_inverse ( n, b )
      a_norm = r8mat_norm_l1 ( n, n, a )
      b_norm = r8mat_norm_l1 ( n, n, b )
      cond2 = a_norm * b_norm

      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, cond1, cond2
c
c  COMBIN
c
      title = 'COMBIN'
      n = 3
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      beta = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call combin_condition ( alpha, beta, n, cond1 )

      call combin ( alpha, beta, n, a )
      call combin_inverse ( alpha, beta, n, b )
      a_norm = r8mat_norm_l1 ( n, n, a )
      b_norm = r8mat_norm_l1 ( n, n, b )
      cond2 = a_norm * b_norm

      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, cond1, cond2
c
c  COMPANION
c
      title = 'COMPANION'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
      call companion_condition ( n, x, cond1 )

      call companion ( n, x, a )
      call companion_inverse ( n, x, b )
      a_norm = r8mat_norm_l1 ( n, n, a )
      b_norm = r8mat_norm_l1 ( n, n, b )
      cond2 = a_norm * b_norm

      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, cond1, cond2
c
c  CONEX1
c
      title = 'CONEX1'
      n = 4
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call conex1_condition ( alpha, cond1 )

      call conex1 ( alpha, a )
      call conex1_inverse ( alpha, b )
      a_norm = r8mat_norm_l1 ( n, n, a )
      b_norm = r8mat_norm_l1 ( n, n, b )
      cond2 = a_norm * b_norm

      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, cond1, cond2
c
c  CONEX2
c
      title = 'CONEX2'
      n = 3
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call conex2_condition ( alpha, cond1 )

      call conex2 ( alpha, a )
      call conex2_inverse ( alpha, b )
      a_norm = r8mat_norm_l1 ( n, n, a )
      b_norm = r8mat_norm_l1 ( n, n, b )
      cond2 = a_norm * b_norm

      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, cond1, cond2
c
c  CONEX3
c
      title = 'CONEX3'
      n = 5
      call conex3_condition ( n, cond1 )

      call conex3 ( n, a )
      call conex3_inverse ( n, b )
      a_norm = r8mat_norm_l1 ( n, n, a )
      b_norm = r8mat_norm_l1 ( n, n, b )
      cond2 = a_norm * b_norm

      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, cond1, cond2
c
c  CONEX4
c
      title = 'CONEX4'
      n = 4
      call conex4_condition ( cond1 )

      call conex4 ( a )
      call conex4_inverse ( b )
      a_norm = r8mat_norm_l1 ( n, n, a )
      b_norm = r8mat_norm_l1 ( n, n, b )
      cond2 = a_norm * b_norm

      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, cond1, cond2
c
c  DAUB2
c
      title = 'DAUB2'
      n = 4
      call daub2_condition ( n, cond1 )

      call daub2 ( n, a )
      call daub2_inverse ( n, b )
      a_norm = r8mat_norm_l1 ( n, n, a )
      b_norm = r8mat_norm_l1 ( n, n, b )
      cond2 = a_norm * b_norm

      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, cond1, cond2
c
c  DAUB4
c
      title = 'DAUB4'
      n = 8
      call daub4_condition ( n, cond1 )

      call daub4 ( n, a )
      call daub4_inverse ( n, b )
      a_norm = r8mat_norm_l1 ( n, n, a )
      b_norm = r8mat_norm_l1 ( n, n, b )
      cond2 = a_norm * b_norm

      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, cond1, cond2
c
c  DAUB6
c
      title = 'DAUB6'
      n = 12
      call daub6_condition ( n, cond1 )

      call daub6 ( n, a )
      call daub6_inverse ( n, b )
      a_norm = r8mat_norm_l1 ( n, n, a )
      b_norm = r8mat_norm_l1 ( n, n, b )
      cond2 = a_norm * b_norm

      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, cond1, cond2
c
c  DAUB8
c
      title = 'DAUB8'
      n = 16
      call daub2_condition ( n, cond1 )

      call daub8 ( n, a )
      call daub8_inverse ( n, b )
      a_norm = r8mat_norm_l1 ( n, n, a )
      b_norm = r8mat_norm_l1 ( n, n, b )
      cond2 = a_norm * b_norm

      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, cond1, cond2
c
c  DAUB10
c
      title = 'DAUB10'
      n = 20
      call daub10_condition ( n, cond1 )

      call daub10 ( n, a )
      call daub10_inverse ( n, b )
      a_norm = r8mat_norm_l1 ( n, n, a )
      b_norm = r8mat_norm_l1 ( n, n, b )
      cond2 = a_norm * b_norm

      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, cond1, cond2
c
c  DAUB12
c
      title = 'DAUB12'
      n = 24
      call daub12_condition ( n, cond1 )

      call daub12 ( n, a )
      call daub12_inverse ( n, b )
      a_norm = r8mat_norm_l1 ( n, n, a )
      b_norm = r8mat_norm_l1 ( n, n, b )
      cond2 = a_norm * b_norm

      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, cond1, cond2
c
c  DIAGONAL
c
      title = 'DIAGONAL'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
      call diagonal_condition ( n, x, cond1 )

      call diagonal ( n, n, x, a )
      call diagonal_inverse ( n, x, b )
      a_norm = r8mat_norm_l1 ( n, n, a )
      b_norm = r8mat_norm_l1 ( n, n, b )
      cond2 = a_norm * b_norm

      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, cond1, cond2
c
c  DIF2
c
      title = 'DIF2'
      n = 5
      call dif2_condition ( n, cond1 )

      call dif2 ( n, n, a )
      call dif2_inverse ( n, b )
      a_norm = r8mat_norm_l1 ( n, n, a )
      b_norm = r8mat_norm_l1 ( n, n, b )
      cond2 = a_norm * b_norm

      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, cond1, cond2
c
c  DOWNSHIFT
c
      title = 'DOWNSHIFT'
      n = 5
      call downshift_condition ( n, cond1 )

      call downshift ( n, a )
      call downshift_inverse ( n, b )
      a_norm = r8mat_norm_l1 ( n, n, a )
      b_norm = r8mat_norm_l1 ( n, n, b )
      cond2 = a_norm * b_norm

      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, cond1, cond2
c
c  EXCHANGE
c
      title = 'EXCHANGE'
      n = 5
      call exchange_condition ( n, cond1 )

      call exchange ( n, n, a )
      call exchange_inverse ( n, b )
      a_norm = r8mat_norm_l1 ( n, n, a )
      b_norm = r8mat_norm_l1 ( n, n, b )
      cond2 = a_norm * b_norm

      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, cond1, cond2
c
c  FIBONACCI2
c
      title = 'FIBONACCI2'
      n = 5
      call fibonacci2_condition ( n, cond1 )

      call fibonacci2 ( n, a )
      call fibonacci2_inverse ( n, b )
      a_norm = r8mat_norm_l1 ( n, n, a )
      b_norm = r8mat_norm_l1 ( n, n, b )
      cond2 = a_norm * b_norm

      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, cond1, cond2
c
c  GFPP
c
      title = 'GFPP'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call gfpp_condition ( n, alpha, cond1 )

      call gfpp ( n, alpha, a )
      call gfpp_inverse ( n, alpha, b )
      a_norm = r8mat_norm_l1 ( n, n, a )
      b_norm = r8mat_norm_l1 ( n, n, b )
      cond2 = a_norm * b_norm

      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, cond1, cond2
c
c  GIVENS
c
      title = 'GIVENS'
      n = 5
      call givens_condition ( n, cond1 )

      call givens ( n, n, a )
      call givens_inverse ( n, b )
      a_norm = r8mat_norm_l1 ( n, n, a )
      b_norm = r8mat_norm_l1 ( n, n, b )
      cond2 = a_norm * b_norm

      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, cond1, cond2
c
c  HANKEL_N
c
      title = 'HANKEL_N'
      n = 5
      call hankel_n_condition ( n, cond1 )

      call hankel_n ( n, a )
      call hankel_n_inverse ( n, b )
      a_norm = r8mat_norm_l1 ( n, n, a )
      b_norm = r8mat_norm_l1 ( n, n, b )
      cond2 = a_norm * b_norm

      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, cond1, cond2
c
c  HARMAN
c
      title = 'HARMAN'
      n = 8
      call harman_condition ( cond1 )

      call harman ( a )
      call harman_inverse ( b )
      a_norm = r8mat_norm_l1 ( n, n, a )
      b_norm = r8mat_norm_l1 ( n, n, b )
      cond2 = a_norm * b_norm

      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, cond1, cond2
c
c  HARTLEY
c
      title = 'HARTLEY'
      n = 5
      call hartley_condition ( n, cond1 )

      call hartley ( n, a )
      call hartley_inverse ( n, b )
      a_norm = r8mat_norm_l1 ( n, n, a )
      b_norm = r8mat_norm_l1 ( n, n, b )
      cond2 = a_norm * b_norm

      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, cond1, cond2
c
c  IDENTITY
c
      title = 'IDENTITY'
      n = 5
      call identity_condition ( n, cond1 )

      call identity ( n, n, a )
      call identity_inverse ( n, b )
      a_norm = r8mat_norm_l1 ( n, n, a )
      b_norm = r8mat_norm_l1 ( n, n, b )
      cond2 = a_norm * b_norm

      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, cond1, cond2
c
c  ILL3
c
      title = 'ILL3'
      n = 3
      call ill3_condition ( cond1 )

      call ill3 ( a )
      call ill3_inverse ( b )
      a_norm = r8mat_norm_l1 ( n, n, a )
      b_norm = r8mat_norm_l1 ( n, n, b )
      cond2 = a_norm * b_norm

      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, cond1, cond2
c
c  JORDAN
c
      title = 'JORDAN'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call jordan_condition ( n, alpha, cond1 )

      call jordan ( n, n, alpha, a )
      call jordan_inverse ( n, alpha, b )
      a_norm = r8mat_norm_l1 ( n, n, a )
      b_norm = r8mat_norm_l1 ( n, n, b )
      cond2 = a_norm * b_norm

      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, cond1, cond2
c
c  KERSHAW
c
      title = 'KERSHAW'
      n = 4
      call kershaw_condition ( cond1 )

      call kershaw ( a )
      call kershaw_inverse ( b )
      a_norm = r8mat_norm_l1 ( n, n, a )
      b_norm = r8mat_norm_l1 ( n, n, b )
      cond2 = a_norm * b_norm

      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, cond1, cond2
c
c  LIETZKE
c
      title = 'LIETZKE'
      n = 5
      call lietzke_condition ( n, cond1 )

      call lietzke ( n, a )
      call lietzke_inverse ( n, b )
      a_norm = r8mat_norm_l1 ( n, n, a )
      b_norm = r8mat_norm_l1 ( n, n, b )
      cond2 = a_norm * b_norm

      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, cond1, cond2
c
c  MAXIJ
c
      title = 'MAXIJ'
      n = 5
      call maxij_condition ( n, cond1 )

      call maxij ( n, n, a )
      call maxij_inverse ( n, b )
      a_norm = r8mat_norm_l1 ( n, n, a )
      b_norm = r8mat_norm_l1 ( n, n, b )
      cond2 = a_norm * b_norm

      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, cond1, cond2
c
c  MINIJ
c
      title = 'MINIJ'
      n = 5
      call minij_condition ( n, cond1 )

      call minij ( n, n, a )
      call minij_inverse ( n, b )
      a_norm = r8mat_norm_l1 ( n, n, a )
      b_norm = r8mat_norm_l1 ( n, n, b )
      cond2 = a_norm * b_norm

      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, cond1, cond2
c
c  ORTH_SYMM
c
      title = 'ORTH_SYMM'
      n = 5
      call orth_symm_condition ( n, cond1 )

      call orth_symm ( n, a )
      call orth_symm_inverse ( n, b )
      a_norm = r8mat_norm_l1 ( n, n, a )
      b_norm = r8mat_norm_l1 ( n, n, b )
      cond2 = a_norm * b_norm

      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, cond1, cond2
c
c  OTO
c
      title = 'OTO'
      n = 5
      call oto_condition ( n, cond1 )

      call oto ( n, n, a )
      call oto_inverse ( n, b )
      a_norm = r8mat_norm_l1 ( n, n, a )
      b_norm = r8mat_norm_l1 ( n, n, b )
      cond2 = a_norm * b_norm

      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, cond1, cond2
c
c  PASCAL1
c
      title = 'PASCAL1'
      n = 5
      call pascal1_condition ( n, cond1 )

      call pascal1 ( n, a )
      call pascal1_inverse ( n, b )
      a_norm = r8mat_norm_l1 ( n, n, a )
      b_norm = r8mat_norm_l1 ( n, n, b )
      cond2 = a_norm * b_norm

      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, cond1, cond2
c
c  PASCAL3
c
      title = 'PASCAL3'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call pascal3_condition ( n, alpha, cond1 )

      call pascal3 ( n, alpha, a )
      call pascal3_inverse ( n, alpha, b )
      a_norm = r8mat_norm_l1 ( n, n, a )
      b_norm = r8mat_norm_l1 ( n, n, b )
      cond2 = a_norm * b_norm

      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, cond1, cond2
c
c  PEI
c
      title = 'PEI'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call pei_condition ( alpha, n, cond1 )

      call pei ( alpha, n, a )
      call pei_inverse ( alpha, n, b )
      a_norm = r8mat_norm_l1 ( n, n, a )
      b_norm = r8mat_norm_l1 ( n, n, b )
      cond2 = a_norm * b_norm

      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, cond1, cond2
c
c  RODMAN
c
      title = 'RODMAN'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call rodman_condition ( n, alpha, cond1 )

      call rodman ( n, n, alpha, a )
      call rodman_inverse ( n, alpha, b )
      a_norm = r8mat_norm_l1 ( n, n, a )
      b_norm = r8mat_norm_l1 ( n, n, b )
      cond2 = a_norm * b_norm

      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, cond1, cond2
c
c  RUTIS1
c
      title = 'RUTIS1'
      n = 4
      call rutis1_condition ( cond1 )

      call rutis1 ( a )
      call rutis1_inverse ( b )
      a_norm = r8mat_norm_l1 ( n, n, a )
      b_norm = r8mat_norm_l1 ( n, n, b )
      cond2 = a_norm * b_norm

      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, cond1, cond2
c
c  RUTIS2
c
      title = 'RUTIS2'
      n = 4
      call rutis2_condition ( cond1 )

      call rutis2 ( a )
      call rutis2_inverse ( b )
      a_norm = r8mat_norm_l1 ( n, n, a )
      b_norm = r8mat_norm_l1 ( n, n, b )
      cond2 = a_norm * b_norm

      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, cond1, cond2
c
c  RUTIS3
c
      title = 'RUTIS3'
      n = 4
      call rutis3_condition ( cond1 )

      call rutis3 ( a )
      call rutis3_inverse ( b )
      a_norm = r8mat_norm_l1 ( n, n, a )
      b_norm = r8mat_norm_l1 ( n, n, b )
      cond2 = a_norm * b_norm

      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, cond1, cond2
c
c  RUTIS5
c
      title = 'RUTIS5'
      n = 4
      call rutis5_condition ( cond1 )

      call rutis5 ( a )
      call rutis5_inverse ( b )
      a_norm = r8mat_norm_l1 ( n, n, a )
      b_norm = r8mat_norm_l1 ( n, n, b )
      cond2 = a_norm * b_norm

      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, cond1, cond2
c
c  SUMMATION
c
      title = 'SUMMATION'
      n = 5
      call summation_condition ( n, cond1 )

      call summation ( n, n, a )
      call summation_inverse ( n, b )
      a_norm = r8mat_norm_l1 ( n, n, a )
      b_norm = r8mat_norm_l1 ( n, n, b )
      cond2 = a_norm * b_norm

      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, cond1, cond2
c
c  SWEET1
c
      title = 'SWEET1'
      n = 6
      call sweet1_condition ( cond1 )

      call sweet1 ( a )
      call sweet1_inverse ( b )
      a_norm = r8mat_norm_l1 ( n, n, a )
      b_norm = r8mat_norm_l1 ( n, n, b )
      cond2 = a_norm * b_norm

      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, cond1, cond2
c
c  SWEET2
c
      title = 'SWEET2'
      n = 6
      call sweet2_condition ( cond1 )

      call sweet2 ( a )
      call sweet2_inverse ( b )
      a_norm = r8mat_norm_l1 ( n, n, a )
      b_norm = r8mat_norm_l1 ( n, n, b )
      cond2 = a_norm * b_norm

      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, cond1, cond2
c
c  SWEET3
c
      title = 'SWEET3'
      n = 6
      call sweet3_condition ( cond1 )

      call sweet3 ( a )
      call sweet3_inverse ( b )
      a_norm = r8mat_norm_l1 ( n, n, a )
      b_norm = r8mat_norm_l1 ( n, n, b )
      cond2 = a_norm * b_norm

      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, cond1, cond2
c
c  SWEET4
c
      title = 'SWEET4'
      n = 13
      call sweet4_condition ( cond1 )

      call sweet4 ( a )
      call sweet4_inverse ( b )
      a_norm = r8mat_norm_l1 ( n, n, a )
      b_norm = r8mat_norm_l1 ( n, n, b )
      cond2 = a_norm * b_norm

      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, cond1, cond2
c
c  TRI_UPPER
c
      title = 'TRI_UPPER'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call tri_upper_condition ( alpha, n, cond1 )

      call tri_upper ( alpha, n, a )
      call tri_upper_inverse ( alpha, n, b )
      a_norm = r8mat_norm_l1 ( n, n, a )
      b_norm = r8mat_norm_l1 ( n, n, b )
      cond2 = a_norm * b_norm

      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, cond1, cond2
c
c  UPSHIFT
c
      title = 'UPSHIFT'
      n = 5
      call upshift_condition ( n, cond1 )

      call upshift ( n, a )
      call upshift_inverse ( n, b )
      a_norm = r8mat_norm_l1 ( n, n, a )
      b_norm = r8mat_norm_l1 ( n, n, b )
      cond2 = a_norm * b_norm

      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, cond1, cond2
c
c  WILK03
c
      title = 'WILK03'
      n = 3
      call wilk03_condition ( cond1 )

      call wilk03 ( a )
      call wilk03_inverse ( b )
      a_norm = r8mat_norm_l1 ( n, n, a )
      b_norm = r8mat_norm_l1 ( n, n, b )
      cond2 = a_norm * b_norm

      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, cond1, cond2
c
c  WILK04
c
      title = 'WILK04'
      n = 4
      call wilk04_condition ( cond1 )

      call wilk04 ( a )
      call wilk04_inverse ( b )
      a_norm = r8mat_norm_l1 ( n, n, a )
      b_norm = r8mat_norm_l1 ( n, n, b )
      cond2 = a_norm * b_norm

      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, cond1, cond2
c
c  WILK05
c
      title = 'WILK05'
      n = 5
      call wilk05_condition ( cond1 )

      call wilk05 ( a )
      call wilk05_inverse ( b )
      a_norm = r8mat_norm_l1 ( n, n, a )
      b_norm = r8mat_norm_l1 ( n, n, b )
      cond2 = a_norm * b_norm

      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, cond1, cond2
c
c  WILSON
c
      title = 'WILSON'
      n = 4
      call wilson_condition ( cond1 )

      call wilson ( a )
      call wilson_inverse ( b )
      a_norm = r8mat_norm_l1 ( n, n, a )
      b_norm = r8mat_norm_l1 ( n, n, b )
      cond2 = a_norm * b_norm

      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, cond1, cond2
 
      return
      end
      subroutine test_determinant ( )

c*********************************************************************72
c
cc TEST_DETERMINANT tests the determinant computations.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 April 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 100 )

      double precision a(n_max,n_max)
      double precision alpha
      double precision b
      double precision beta
      integer col_num
      double precision d(n_max)
      double precision d1
      double precision d2
      double precision d3
      double precision d4
      double precision d5
      double precision da
      double precision determ1
      double precision determ2
      double precision di
      double precision gam
      integer i
      integer i4_hi
      integer i4_lo
      integer i4_uniform_ab
      integer ii
      integer jj
      integer k
      integer key
      double precision l(n_max,n_max)
      integer m
      integer n
      double precision norm_frobenius
      double precision p(n_max,n_max)
      integer pivot(n_max)
      double precision prob
      double precision r8_hi
      double precision r8_lo
      double precision r8_uniform_ab
      double precision r8mat_norm_fro
      double precision r8vec_sum
      integer rank
      integer row_num
      integer seed
      character*80 title
      double precision u(n_max,n_max)
      double precision v1(n_max)
      double precision v2(n_max)
      double precision v3(n_max)
      double precision w(n_max)
      double precision x(2*n_max-1)
      double precision x_hi
      double precision x_lo
      integer x_n
      double precision x1
      double precision x2
      double precision y(n_max)
      integer y_n
      double precision y_sum
      double precision z(n_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST_DETERMINANT'
      write ( *, '(a)' ) 
     &  '  Compute the determinants of an example of each'
      write ( *, '(a)' ) 
     &  '  test matrix; compare with the determinant routine,'
      write ( *, '(a)' ) 
     &  '  if available.  Print the matrix Frobenius norm'
      write ( *, '(a)' ) '  for an estimate of magnitude.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,a)' ) '  Title                    N      ',
     &               'Determ          Determ           ||A||'
      write ( *, '(a)' ) ' '
c
c  A123
c
      title = 'A123'
      n = 3
      call a123 ( a )
      call a123_determinant ( determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  AEGERTER
c
      title = 'AEGERTER'
      n = 5
      call aegerter ( n, a )
      call aegerter_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  ANTICIRCULANT
c
      title = 'ANTICIRCULANT'
      n = 3
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
      call anticirculant ( n, n, x, a )
      call anticirculant_determinant ( n, x, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  ANTICIRCULANT
c
      title = 'ANTICIRCULANT'
      n = 4
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
      call anticirculant ( n, n, x, a )
      call anticirculant_determinant ( n, x, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  ANTICIRCULANT
c
      title = 'ANTICIRCULANT'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
      call anticirculant ( n, n, x, a )
      call anticirculant_determinant ( n, x, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  ANTIHADAMARD
c
      title = 'ANTIHADAMARD'
      n = 5
      call antihadamard ( n, a )
      call antihadamard_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  ANTISYMM_RANDOM
c
      title = 'ANTISYMM_RANDOM'
      n = 5
      key = 123456789
      call antisymm_random ( n, key, a )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,14x,2x,g14.6,2x,g14.6)' )
     &  title, n,          determ2, norm_frobenius
c
c  ANTISYMM_RANDOM
c
      title = 'ANTISYMM_RANDOM'
      n = 6
      key = 123456789
      call antisymm_random ( n, key, a )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,14x,2x,g14.6,2x,g14.6)' )
     &  title, n,          determ2, norm_frobenius
c
c  BAB
c
      title = 'BAB'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      beta = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call bab ( n, alpha, beta, a )
      call bab_determinant ( n, alpha, beta, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  BAUER
c
      title = 'BAUER'
      n = 6
      call bauer ( a )
      call bauer_determinant ( determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  BERNSTEIN
c
      title = 'BERNSTEIN'
      n = 5
      call bernstein ( n, a )
      call bernstein_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  BIMARKOV_RANDOM
c
      title = 'BIMARKOV_RANDOM'
      n = 5
      key = 123456789
      call bimarkov_random ( n, key, a )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,14x,2x,g14.6,2x,g14.6)' )
     &  title, n,          determ2, norm_frobenius
c
c  BIS
c
      title = 'BIS'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      beta = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call bis ( alpha, beta, n, n, a )
      call bis_determinant ( alpha, beta, n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  BIW
c
      title = 'BIW'
      n = 5
      call biw ( n, a )
      call biw_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  BODEWIG
c
      title = 'BODEWIG'
      n = 4
      call bodewig ( a )
      call bodewig_determinant ( determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  BOOTHROYD
c
      title = 'BOOTHROYD'
      n = 5
      call boothroyd ( n, a )
      call boothroyd_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  BORDERBAND
c
      title = 'BORDERBAND'
      n = 5
      call borderband ( n, a )
      call borderband_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  CARRY
c
      title = 'CARRY'
      n = 5
      i4_lo = 2
      i4_hi = 20
      seed = 123456789
      k = i4_uniform_ab ( i4_lo, i4_hi, seed )
      call carry ( n, k, a )
      call carry_determinant ( n, k, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  CAUCHY
c
      title = 'CAUCHY'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, y )
      call cauchy ( n, x, y, a )
      call cauchy_determinant ( n, x, y, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  CHEBY_DIFF1
c
      title = 'CHEBY_DIFF1'
      n = 5
      call cheby_diff1 ( n, a )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,14x,2x,g14.6,2x,g14.6)' )
     &  title, n,          determ2, norm_frobenius
c
c  CHEBY_DIFF1
c
      title = 'CHEBY_DIFF1'
      n = 6
      call cheby_diff1 ( n, a )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,14x,2x,g14.6,2x,g14.6)' )
     &  title, n,          determ2, norm_frobenius
c
c  CHEBY_T
c
      title = 'CHEBY_T'
      n = 5
      call cheby_t ( n, a )
      call cheby_t_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  CHEBY_U
c
      title = 'CHEBY_U'
      n = 5
      call cheby_u ( n, a )
      call cheby_u_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  CHEBY_VAN1
c
      title = 'CHEBY_VAN1'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      call r8vec_linspace ( n, r8_lo, r8_hi, x )
      call cheby_van1 ( n, r8_lo, r8_hi, n, x, a )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,14x,2x,g14.6,2x,g14.6)' ) 
     &  title, n,          determ2, norm_frobenius
c
c  CHEBY_VAN2
c
      do n = 2, 10
        title = 'CHEBY_VAN2'
        call cheby_van2 ( n, a )
        call cheby_van2_determinant ( n, determ1 )
        call r8mat_determinant ( n, a, determ2 )
        norm_frobenius = r8mat_norm_fro ( n, n, a )
        write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &    title, n, determ1, determ2, norm_frobenius
      end do
c
c  CHEBY_VAN3
c
      title = 'CHEBY_VAN3'
      n = 5
      call cheby_van3 ( n, a )
      call cheby_van3_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  CHOW
c
      title = 'CHOW'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      beta = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call chow ( alpha, beta, n, n, a )
      call chow_determinant ( alpha, beta, n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  CIRCULANT
c
      title = 'CIRCULANT'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789;
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
      call circulant ( n, n, x, a )
      call circulant_determinant ( n, x, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  CIRCULANT2
c
      title = 'CIRCULANT2'
      n = 3
      call circulant2 ( n, a )
      call circulant2_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  CIRCULANT2
c
      title = 'CIRCULANT2'
      n = 4
      call circulant2 ( n, a )
      call circulant2_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  CIRCULANT2
c
      title = 'CIRCULANT2'
      n = 5
      call circulant2 ( n, a )
      call circulant2_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  CLEMENT1
c
      title = 'CLEMENT1'
      n = 5
      call clement1 ( n, a )
      call clement1_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  CLEMENT1
c
      title = 'CLEMENT1'
      n = 6
      call clement1 ( n, a )
      call clement1_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  CLEMENT2
c
      title = 'CLEMENT2'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      call r8vec_uniform_ab ( n - 1, r8_lo, r8_hi, seed, x )
      call r8vec_uniform_ab ( n - 1, r8_lo, r8_hi, seed, y )
      call clement2 ( n, x, y, a )
      call clement2_determinant ( n, x, y, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  CLEMENT2
c
      title = 'CLEMENT2'
      n = 6
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      call r8vec_uniform_ab ( n - 1, r8_lo, r8_hi, seed, x )
      call r8vec_uniform_ab ( n - 1, r8_lo, r8_hi, seed, y )
      call clement2 ( n, x, y, a )
      call clement2_determinant ( n, x, y, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  COMBIN
c
      title = 'COMBIN'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      beta = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call combin ( alpha, beta, n, a )
      call combin_determinant ( alpha, beta, n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  COMPANION
c
      title = 'COMPANION'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
      call companion ( n, x, a )
      call companion_determinant ( n, x, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  COMPLEX_I
c
      title = 'COMPLEX_I'
      n = 2
      call complex_i ( a )
      call complex_i_determinant ( determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  CONEX1
c
      title = 'CONEX1'
      n = 4
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call conex1 ( alpha, a )
      call conex1_determinant ( alpha, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  CONEX2
c
      title = 'CONEX2'
      n = 3
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call conex2 ( alpha, a )
      call conex2_determinant ( alpha, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  CONEX3
c
      title = 'CONEX3'
      n = 5
      call conex3 ( n, a )
      call conex3_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  CONEX4
c
      title = 'CONEX4'
      n = 4
      call conex4 ( a )
      call conex4_determinant ( determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  CONFERENCE
c
      title = 'CONFERENCE'
      n = 6
      call conference ( n, a )
      call conference_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  CREATION
c
      title = 'CREATION'
      n = 5
      call creation ( n, n, a )
      call creation_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  DAUB2
c
      title = 'DAUB2'
      n = 4
      call daub2 ( n, a )
      call daub2_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  DAUB4
c
      title = 'DAUB4'
      n = 8
      call daub4 ( n, a )
      call daub4_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  DAUB6
c
      title = 'DAUB6'
      n = 12
      call daub6 ( n, a )
      call daub6_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  DAUB8
c
      title = 'DAUB8'
      n = 16
      call daub8 ( n, a )
      call daub8_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  DAUB10
c
      title = 'DAUB10'
      n = 20
      call daub10 ( n, a )
      call daub10_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  DAUB12
c
      title = 'DAUB12'
      n = 24
      call daub12 ( n, a )
      call daub12_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  DIAGONAL
c
      title = 'DIAGONAL'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
      call diagonal ( n, n, x, a )
      call diagonal_determinant ( n, x, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  DIF1
c
      title = 'DIF1'
      n = 5
      call dif1 ( n, n, a )
      call dif1_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  DIF1
c
      title = 'DIF1'
      n = 6
      call dif1 ( n, n, a )
      call dif1_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  DIF1CYCLIC
c
      title = 'DIF1CYCLIC'
      n = 5
      call dif1cyclic ( n, a )
      call dif1cyclic_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  DIF2
c
      title = 'DIF2'
      n = 5
      call dif2 ( n, n, a )
      call dif2_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  DIF2CYCLIC
c
      title = 'DIF2CYCLIC'
      n = 5
      call dif2cyclic ( n, a )
      call dif2cyclic_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  DORR
c
      title = 'DORR'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call dorr ( alpha, n, a )
      call dorr_determinant ( alpha, n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  DOWNSHIFT
c
      title = 'DOWNSHIFT'
      n = 5
      call downshift ( n, a )
      call downshift_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  EBERLEIN
c
      title = 'EBERLEIN'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call eberlein ( alpha, n, a )
      call eberlein_determinant ( alpha, n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  EULERIAN
c
      title = 'EULERIAN'
      n = 5
      call eulerian ( n, n, a )
      call eulerian_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  EXCHANGE
c
      title = 'EXCHANGE'
      n = 5
      call exchange ( n, n, a )
      call exchange_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  FIBONACCI1
c
      title = 'FIBONACCI1'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      beta = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call fibonacci1 ( n, alpha, beta, a )
      call fibonacci1_determinant ( n, alpha, beta, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  FIBONACCI2
c
      title = 'FIBONACCI2'
      n = 5
      call fibonacci2 ( n, a )
      call fibonacci2_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  FIBONACCI3
c
      title = 'FIBONACCI3'
      n = 5
      call fibonacci3 ( n, a )
      call fibonacci3_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  FIEDLER
c
      title = 'FIEDLER'
      n = 7
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
      call fiedler ( n, n, x, a )
      call fiedler_determinant ( n, x, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  FORSYTHE
c
      title = 'FORSYTHE'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      beta = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call forsythe ( alpha, beta, n, a )
      call forsythe_determinant ( alpha, beta, n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  FORSYTHE
c
      title = 'FORSYTHE'
      n = 6
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      beta = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call forsythe ( alpha, beta, n, a )
      call forsythe_determinant ( alpha, beta, n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  FOURIER_COSINE
c
      title = 'FOURIER_COSINE'
      n = 5
      call fourier_cosine ( n, a )
      call fourier_cosine_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  FOURIER_SINE
c
      title = 'FOURIER_SINE'
      n = 5
      call fourier_sine ( n, a )
      call fourier_sine_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  FRANK
c
      title = 'FRANK'
      n = 5
      call frank ( n, a )
      call frank_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  GEAR
c
      do n = 4, 8
        title = 'GEAR'
        i4_lo = -n
        i4_hi = +n
        seed = 123456789
        ii = i4_uniform_ab ( i4_lo, i4_hi, seed )
        jj = i4_uniform_ab ( i4_lo, i4_hi, seed )
        call gear ( ii, jj, n, a )
        call gear_determinant ( ii, jj, n, determ1 )
        call r8mat_determinant ( n, a, determ2 )
        norm_frobenius = r8mat_norm_fro ( n, n, a )
        write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &    title, n, determ1, determ2, norm_frobenius
      end do
c
c  GFPP
c
      title = 'GFPP'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call gfpp ( n, alpha, a )
      call gfpp_determinant ( n, alpha, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  GIVENS
c
      title = 'GIVENS'
      n = 5
      call givens ( n, n, a )
      call givens_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  GK316
c
      title = 'GK316'
      n = 5
      call gk316 ( n, a )
      call gk316_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  GK323
c
      title = 'GK323'
      n = 5
      call gk323 ( n, n, a )
      call gk323_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  GK324
c
      title = 'GK324'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      call r8vec_uniform_ab ( n - 1, r8_lo, r8_hi, seed, x )
      call gk324 ( n, n, x, a )
      call gk324_determinant ( n, x, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  GRCAR
c
      title = 'GRCAR'
      n = 5
      i4_lo = 1
      i4_hi = n - 1
      seed = 123456789
      k = i4_uniform_ab ( i4_lo, i4_hi, seed )
      call grcar ( n, n, k, a )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,14x,2x,g14.6,2x,g14.6)' ) 
     &  title, n,          determ2, norm_frobenius
c
c  HADAMARD
c
      title = 'HADAMARD'
      n = 5
      call hadamard ( n, n, a )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,14x,2x,g14.6,2x,g14.6)' ) 
     &  title, n,          determ2, norm_frobenius
c
c  HANKEL
c
      title = 'HANKEL'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      call r8vec_uniform_ab ( 2 * n - 1, r8_lo, r8_hi, seed, x )
      call hankel ( n, x, a )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,14x,2x,g14.6,2x,g14.6)' ) 
     &  title, n,          determ2, norm_frobenius
c
c  HANKEL_N
c
      title = 'HANKEL_N'
      n = 5
      call hankel_n ( n, a )
      call hankel_n_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  HANOWA
c
      title = 'HANOWA'
      n = 6
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call hanowa ( alpha, n, a )
      call hanowa_determinant ( alpha, n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  HARMAN
c
      title = 'HARMAN'
      n = 8
      call harman ( a )
      call harman_determinant ( determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  HARTLEY
c
      title = 'HARTLEY'
      do n = 5, 8
        call hartley ( n, a )
        call hartley_determinant ( n, determ1 )
        call r8mat_determinant ( n, a, determ2 )
        norm_frobenius = r8mat_norm_fro ( n, n, a )
        write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &    title, n, determ1, determ2, norm_frobenius
      end do
c
c  HELMERT
c
      title = 'HELMERT'
      n = 5
      call helmert ( n, a )
      call helmert_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  HELMERT2
c
      title = 'HELMERT2'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
      call helmert2 ( n, x, a )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  HERMITE
c
      title = 'HERMITE'
      n = 5
      call hermite ( n, a )
      call hermite_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  HERNDON
c
      title = 'HERNDON'
      n = 5
      call herndon ( n, a )
      call herndon_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  HILBERT
c
      title = 'HILBERT'
      n = 5
      call hilbert ( n, n, a )
      call hilbert_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  HOUSEHOLDER
c
      title = 'HOUSEHOLDER'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
      call householder ( n, x, a )
      call householder_determinant ( n, x, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  IDEM_RANDOM
c
      title = 'IDEM_RANDOM'
      n = 5
      i4_lo = 0
      i4_hi = n
      seed = 123456789
      rank = i4_uniform_ab ( i4_lo, i4_hi, seed )
      key = 123456789
      call idem_random ( n, rank, key, a )
      call idem_random_determinant ( n, rank, key, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  IDENTITY
c
      title = 'IDENTITY'
      n = 5
      call identity ( n, n, a )
      call identity_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  IJFACT1
c
      title = 'IJFACT1'
      n = 5
      call ijfact1 ( n, a )
      call ijfact1_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  IJFACT2
c
      title = 'IJFACT2'
      n = 5
      call ijfact2 ( n, a )
      call ijfact2_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  ILL3
c
      title = 'ILL3'
      n = 3
      call ill3 ( a )
      call ill3_determinant ( determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  INTEGRATION
c
      title = 'INTEGRATION'
      n = 6
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call integration ( alpha, n, a )
      call integration_determinant ( alpha, n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  INVOL
c
      title = 'INVOL'
      n = 5
      call invol ( n, a )
      call invol_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  INVOL_RANDOM
c
      title = 'INVOL_RANDOM'
      n = 5
      i4_lo = 0
      i4_hi = n
      seed = 123456789
      rank = i4_uniform_ab ( i4_lo, i4_hi, seed )
      key = 123456789
      call invol_random ( n, rank, key, a )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,14x,2x,g14.6,2x,g14.6)' ) 
     &  title, n,          determ2, norm_frobenius
c
c  JACOBI
c
      title = 'JACOBI'
      n = 5
      call jacobi ( n, n, a )
      call jacobi_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  JACOBI
c
      title = 'JACOBI'
      n = 6
      call jacobi ( n, n, a )
      call jacobi_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  JORDAN
c
      title = 'JORDAN'
      n = 6
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call jordan ( n, n, alpha, a )
      call jordan_determinant ( n, alpha, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  KAHAN
c
      title = 'KAHAN'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call kahan ( alpha, n, n, a )
      call kahan_determinant ( alpha, n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  KERSHAW
c
      title = 'KERSHAW'
      n = 4
      call kershaw ( a )
      call kershaw_determinant ( determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  KERSHAWTRI
c
      title = 'KERSHAWTRI'
      n = 5
      x_n = ( n + 1 ) / 2
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      call r8vec_uniform_ab ( x_n, r8_lo, r8_hi, seed, x )
      call kershawtri ( n, x, a )
      call kershawtri_determinant ( n, x, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  KMS
c
      title = 'KMS'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call kms ( alpha, n, n, a )
      call kms_determinant ( alpha, n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  LAGUERRE
c
      title = 'LAGUERRE'
      n = 5
      call laguerre ( n, a )
      call laguerre_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  LEGENDRE
c
      title = 'LEGENDRE'
      n = 5
      call legendre ( n, a )
      call legendre_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  LEHMER
c
      title = 'LEHMER'
      n = 5
      call lehmer ( n, n, a )
      call lehmer_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  LESLIE
c
      title = 'LESLIE'
      n = 4
      b =  0.025D+00
      di = 0.010D+00
      da = 0.100D+00
      call leslie ( b, di, da, a )
      call leslie_determinant ( b, di, da, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  LESP
c
      title = 'LESP'
      n = 5
      call lesp ( n, n, a )
      call lesp_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  LIETZKE
c
      title = 'LIETZKE'
      n = 5
      call lietzke ( n, a )
      call lietzke_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  LIGHTS_OUT
c
      title = 'LIGHTS_OUT'
      row_num = 5
      col_num = 5
      n = row_num * col_num;
      call lights_out ( row_num, col_num, n, a )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,14x,2x,g14.6,2x,g14.6)' ) 
     &  title, n,          determ2, norm_frobenius
c
c  LINE_ADJ
c
      title = 'LINE_ADJ'
      n = 5
      call line_adj ( n, a )
      call line_adj_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  LINE_ADJ
c
      title = 'LINE_ADJ'
      n = 6
      call line_adj ( n, a )
      call line_adj_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  LINE_LOOP_ADJ
c
      title = 'LINE_LOOP_ADJ'
      n = 5
      call line_loop_adj ( n, a )
      call line_loop_adj_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  LOEWNER
c
      title = 'LOEWNER'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, w )
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, y )
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, z )
      call loewner ( w, x, y, z, n, a )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,14x,2x,g14.6,2x,g14.6)' ) 
     &  title, n,          determ2, norm_frobenius
c
c  LOTKIN
c
      title = 'LOTKIN'
      n = 5
      call lotkin ( n, n, a )
      call lotkin_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  MARKOV_RANDOM
c
      title = 'MARKOV_RANDOM'
      n = 5
      key = 123456789
      call markov_random ( n, key, a )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,14x,2x,g14.6,2x,g14.6)' ) 
     &  title, n,          determ2, norm_frobenius
c
c  MAXIJ
c
      title = 'MAXIJ'
      n = 5
      call maxij ( n, n, a )
      call maxij_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  MILNES
c
      title = 'MILNES'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
      call milnes ( n, n, x, a )
      call milnes_determinant ( n, x, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  MINIJ
c
      title = 'MINIJ'
      n = 5
      call minij ( n, n, a )
      call minij_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  MOLER1
c
      title = 'MOLER1'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call moler1 ( alpha, n, n, a )
      call moler1_determinant ( alpha, n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  MOLER2
c
      title = 'MOLER2'
      n = 5
      call moler2 ( a )
      call moler2_determinant ( determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  MOLER3
c
      title = 'MOLER3'
      n = 5
      call moler3 ( n, n, a )
      call moler3_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  MOLER4
c
      title = 'MOLER4'
      n = 4
      call moler4 ( a )
      call moler4_determinant ( determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  NEUMANN
c
      title = 'NEUMANN'
      row_num = 5
      col_num = 5
      n = row_num * col_num
      call neumann ( row_num, col_num, a )
      call neumann_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  ONE
c
      title = 'ONE'
      n = 5
      call one ( n, n, a )
      call one_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  ORTEGA
c
      title = 'ORTEGA'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, v1 )
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, v2 )
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, v3 )
      call ortega ( n, v1, v2, v3, a )
      call ortega_determinant ( n, v1, v2, v3, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  ORTH_RANDOM
c
      title = 'ORTH_RANDOM'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      key = 123456789
      call orth_random ( n, key, a )
      call orth_random_determinant ( n, key, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  ORTH_SYMM
c
      title = 'ORTH_SYMM'
      n = 5
      call orth_symm ( n, a )
      call orth_symm_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  OTO
c
      title = 'OTO'
      n = 5
      call oto ( n, n, a )
      call oto_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  PARTER
c
      title = 'PARTER'
      n = 5
      call parter ( n, n, a )
      call parter_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  PASCAL1
c
      title = 'PASCAL1'
      n = 5
      call pascal1 ( n, a )
      call pascal1_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  PASCAL2
c
      title = 'PASCAL2'
      n = 5
      call pascal2 ( n, a )
      call pascal2_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  PASCAL3
c
      title = 'PASCAL3'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call pascal3 ( n, alpha, a )
      call pascal3_determinant ( n, alpha, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  PDS_RANDOM
c
      title = 'PDS_RANDOM'
      n = 5
      key = 123456789
      call pds_random ( n, key, a )
      call pds_random_determinant ( n, key, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  PEI
c
      title = 'PEI'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call pei ( alpha, n, a )
      call pei_determinant ( alpha, n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  PERMUTATION_RANDOM
c
      title = 'PERMUTATION_RANDOM'
      n = 5
      key = 123456789
      call permutation_random ( n, key, a )
      call permutation_random_determinant ( n, key, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  PLU
c
      title = 'PLU'
      n = 5
      seed = 123456789
      do i = 1, n
        i4_lo = i
        i4_hi = n
        pivot(i) = i4_uniform_ab ( i4_lo, i4_hi, seed )
      end do
      call plu ( n, pivot, a )
      call plu_determinant ( n, pivot, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  POISSON
c
      title = 'POISSON'
      row_num = 5
      col_num = 5
      n = row_num * col_num
      call poisson ( row_num, col_num, a )
      call poisson_determinant ( row_num, col_num, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  PROLATE
c
      title = 'PROLATE'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call prolate ( alpha, n, a )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,14x,2x,g14.6,2x,g14.6)' ) 
     &  title, n,          determ2, norm_frobenius
c
c  RECTANGLE_ADJ
c
      title = 'RECTANGLE_ADJ'
      row_num = 5
      col_num = 5
      n = row_num * col_num
      call rectangle_adj ( row_num, col_num, n, a )
      call rectangle_adj_determinant ( row_num, col_num, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  REDHEFFER
c
      title = 'REDHEFFER'
      n = 5
      call redheffer ( n, a )
      call redheffer_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  REF_RANDOM
c
      title = 'REF_RANDOM'
      n = 5
      prob = 0.65D+00
      key = 123456789
      call ref_random ( n, n, prob, key, a )
      call ref_random_determinant ( n, prob, key, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  REF_RANDOM
c
      title = 'REF_RANDOM'
      n = 5
      prob = 0.85D+00
      key = 123456789
      call ref_random ( n, n, prob, key, a )
      call ref_random_determinant ( n, prob, key, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  RIEMANN
c
      title = 'RIEMANN'
      n = 5
      call riemann ( n, n, a )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,14x,2x,g14.6,2x,g14.6)' ) 
     &  title, n,          determ2, norm_frobenius
c
c  RING_ADJ
c
      do n = 1, 8
        title = 'RING_ADJ'
        call ring_adj ( n, a )
        call ring_adj_determinant ( n, determ1 )
        call r8mat_determinant ( n, a, determ2 )
        norm_frobenius = r8mat_norm_fro ( n, n, a )
        write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &    title, n, determ1, determ2, norm_frobenius
      end do
c
c  RIS
c
      title = 'RIS'
      n = 5
      call ris ( n, a )
      call ris_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  RODMAN
c
      title = 'RODMAN'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call rodman ( n, n, alpha, a )
      call rodman_determinant ( n, alpha, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  ROSSER1
c
c  Note that while the correct determinant of this matrix is 0,
c  that value is very difficult to calculate correctly.  MATLAB
c  returns det ( A ) = -10611, for instance.
c
      title = 'ROSSER1'
      n = 8
      call rosser1 ( a )
      call rosser1_determinant ( determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  ROUTH
c
      title = 'ROUTH'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
      call routh ( n, x, a )
      call routh_determinant ( n, x, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  RUTIS1
c
      title = 'RUTIS1'
      n = 4
      call rutis1 ( a )
      call rutis1_determinant ( determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  RUTIS2
c
      title = 'RUTIS2'
      n = 4
      call rutis2 ( a )
      call rutis2_determinant ( determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  RUTIS3
c
      title = 'RUTIS3'
      n = 4
      call rutis3 ( a )
      call rutis3_determinant ( determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  RUTIS4
c
      title = 'RUTIS4'
      n = 5
      call rutis4 ( n, a )
      call rutis4_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  RUTIS5
c
      title = 'RUTIS5'
      n = 4
      call rutis5 ( a )
      call rutis5_determinant ( determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  SCHUR_BLOCK
c
      title = 'SCHUR_BLOCK'
      n = 5
      x_n = ( n + 1 ) / 2
      y_n = n / 2
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      call r8vec_uniform_ab ( x_n, r8_lo, r8_hi, seed, x )
      call r8vec_uniform_ab ( y_n, r8_lo, r8_hi, seed, y )
      call schur_block ( n, x, y, a )
      call schur_block_determinant ( n, x, y, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  SKEW_CIRCULANT
c
      title = 'SKEW_CIRCULANT'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
      call skew_circulant ( n, n, x, a )
      call skew_circulant_determinant ( n, x, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  SPLINE
c
      title = 'SPLINE'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
      call spline ( n, x, a )
      call spline_determinant ( n, x, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  STIRLING
c
      title = 'STIRLING'
      n = 5
      call stirling ( n, n, a )
      call stirling_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  STRIPE
c
      title = 'STRIPE'
      n = 5
      call stripe ( n, a )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,14x,2x,g14.6,2x,g14.6)' ) 
     &  title, n,          determ2, norm_frobenius
c
c  SUMMATION
c
      title = 'SUMMATION'
      n = 5
      call summation ( n, n, a )
      call summation_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  SWEET1
c
      title = 'SWEET1'
      n = 6
      call sweet1_determinant ( determ1 )
      call sweet1 ( a )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  SWEET2
c
      title = 'SWEET2'
      n = 6
      call sweet2_determinant ( determ1 )
      call sweet2 ( a )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  SWEET3
c
      title = 'SWEET3'
      n = 6
      call sweet3_determinant ( determ1 )
      call sweet3 ( a )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  SWEET4
c
      title = 'SWEET4'
      n = 13
      call sweet4_determinant ( determ1 )
      call sweet4 ( a )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  SYLVESTER
c
      title = 'SYLVESTER'
      n = 5
      x_n = 3 + 1
      y_n = 2 + 1
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      call r8vec_uniform_ab ( x_n, r8_lo, r8_hi, seed, x )
      call r8vec_uniform_ab ( y_n, r8_lo, r8_hi, seed, y )
      call sylvester ( n, x_n - 1, x, y_n - 1, y, a )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,14x,2x,g14.6,2x,g14.6)' ) 
     &  title, n,          determ2, norm_frobenius
c
c  SYLVESTER_KAC
c
      title = 'SYLVESTER_KAC'
      n = 5
      call r8vec_uniform_ab ( x_n, r8_lo, r8_hi, seed, x )
      call r8vec_uniform_ab ( y_n, r8_lo, r8_hi, seed, y )
      call sylvester_kac ( n, a )
      call sylvester_kac_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  SYLVESTER_KAC
c
      title = 'SYLVESTER_KAC'
      n = 6
      call r8vec_uniform_ab ( x_n, r8_lo, r8_hi, seed, x )
      call r8vec_uniform_ab ( y_n, r8_lo, r8_hi, seed, y )
      call sylvester_kac ( n, a )
      call sylvester_kac_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  SYMM_RANDOM
c
      title = 'SYMM_RANDOM'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, d )
      key = 123456789
      call symm_random ( n, d, key, a )
      call symm_random_determinant ( n, d, key, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  TOEPLITZ
c
      title = 'TOEPLITZ'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      call r8vec_uniform_ab ( 2 * n - 1, r8_lo, r8_hi, seed, x )
      call toeplitz ( n, x, a )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,14x,2x,g14.6,2x,g14.6)' ) 
     &  title, n,          determ2, norm_frobenius
c
c  TOEPLITZ_5DIAG
c
      title = 'TOEPLITZ_5DIAG'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      d1 = r8_uniform_ab ( r8_lo, r8_hi, seed )
      d2 = r8_uniform_ab ( r8_lo, r8_hi, seed )
      d3 = r8_uniform_ab ( r8_lo, r8_hi, seed )
      d4 = r8_uniform_ab ( r8_lo, r8_hi, seed )
      d5 = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call toeplitz_5diag ( n, d1, d2, d3, d4, d5, a )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,14x,2x,g14.6,2x,g14.6)' ) 
     &  title, n,          determ2, norm_frobenius
c
c  TOEPLITZ_5S
c
      title = 'TOEPLITZ_5S'
      row_num = 5
      col_num = 5
      n = row_num * col_num
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      beta = r8_uniform_ab ( r8_lo, r8_hi, seed )
      gam = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call toeplitz_5s ( row_num, col_num, alpha, beta, gam, n, a )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,14x,2x,g14.6,2x,g14.6)' ) 
     &  title, n,          determ2, norm_frobenius
c
c  TOEPLITZ_PDS
c
      title = 'TOEPLITZ_PDS'
      m = 3
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      call r8vec_uniform_ab ( m, r8_lo, r8_hi, seed, x )
      call r8vec_uniform_ab ( m, r8_lo, r8_hi, seed, y )
      y_sum = r8vec_sum ( m, y )
      do i = 1, m
        y(i) = y(i) / y_sum
      end do
      call toeplitz_pds ( m, n, x, y, a )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,14x,2x,g14.6,2x,g14.6)' ) 
     &  title, n,          determ2, norm_frobenius
c
c  TOURNAMENT_RANDOM
c
      title = 'TOURNAMENT_RANDOM'
      n = 5
      key = 123456789
      call tournament_random ( n, key, a )
      call tournament_random_determinant ( n, key, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  TRANSITION_RANDOM
c
      title = 'TRANSITION_RANDOM'
      n = 5
      key = 123456789
      call transition_random ( n, key, a )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,14x,2x,g14.6,2x,g14.6)' ) 
     &  title, n,          determ2, norm_frobenius
c
c  TRENCH
c
      title = 'TRENCH'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call trench ( alpha, n, n, a )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,14x,2x,g14.6,2x,g14.6)' ) 
     &  title, n,          determ2, norm_frobenius
c
c  TRI_UPPER
c
      title = 'TRI_UPPER'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call tri_upper ( alpha, n, a )
      call tri_upper_determinant ( alpha, n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  TRIS
c
      title = 'TRIS'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      beta = r8_uniform_ab ( r8_lo, r8_hi, seed )
      gam = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call tris ( n, n, alpha, beta, gam, a )
      call tris_determinant ( n, alpha, beta, gam, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  TRIV
c
      title = 'TRIV'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      call r8vec_uniform_ab ( n - 1, r8_lo, r8_hi, seed, x )
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, y )
      call r8vec_uniform_ab ( n - 1, r8_lo, r8_hi, seed, z )
      call triv ( n, x, y, z, a )
      call triv_determinant ( n, x, y, z, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  TRIW
c
      title = 'TRIW'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      i4_lo = 0
      i4_hi = n - 1
      seed = 123456789
      k = i4_uniform_ab ( i4_lo, i4_hi, seed )
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call triw ( alpha, k, n, a )
      call triw_determinant ( alpha, k, n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  UPSHIFT
c
      title = 'UPSHIFT'
      n = 5
      call upshift ( n, a )
      call upshift_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  VAND1
c
      title = 'VAND1'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
      call vand1 ( n, x, a )
      call vand1_determinant ( n, x, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  VAND2
c
      title = 'VAND2'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
      call vand2 ( n, x, a )
      call vand2_determinant ( n, x, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  WATHEN
c
      title = 'WATHEN'
      row_num = 5
      col_num = 5
      call wathen_order ( row_num, col_num, n )
      call wathen ( row_num, col_num, n, a )
      call r8mat_determinant ( n, a, determ2 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,14x,2x,g14.6,2x,g14.6)' ) 
     &  title, n,          determ2, norm_frobenius
c
c  WILK03
c
      title = 'WILK03'
      n = 3
      call wilk03 ( a )
      call wilk03_determinant ( determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  WILK04
c
      title = 'WILK04'
      n = 4
      call wilk04 ( a )
      call wilk04_determinant ( determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  WILK05
c
      title = 'WILK05'
      n = 5
      call wilk05 ( a )
      call wilk05_determinant ( determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  WILK12
c
      title = 'WILK12'
      n = 12
      call wilk12 ( a )
      call wilk12_determinant ( determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  WILK20
c
      title = 'WILK20'
      n = 20
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call wilk20 ( alpha, a )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,14x,2x,g14.6,2x,g14.6)' )
     &  title, n,          determ2, norm_frobenius
c
c  WILK21
c
      title = 'WILK21'
      n = 21
      call wilk21 ( n, a )
      call wilk21_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  WILSON
c
      title = 'WILSON'
      n = 4
      call wilson ( a )
      call wilson_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  ZERO
c
      title = 'ZERO'
      n = 5
      call zero ( n, n, a )
      call zero_determinant ( n, determ1 )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, determ1, determ2, norm_frobenius
c
c  ZIELKE
c
      title = 'ZIELKE'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      d1 = r8_uniform_ab ( r8_lo, r8_hi, seed )
      d2 = r8_uniform_ab ( r8_lo, r8_hi, seed )
      d3 = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call zielke ( n, d1, d2, d3, a )
      call r8mat_determinant ( n, a, determ2 )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,14x,2x,g14.6,2x,g14.6)' )
     &  title, n,          determ2, norm_frobenius

      return
      end
      subroutine test_eigen_left ( )

c*********************************************************************72
c
cc TEST_EIGEN_LEFT tests left eigensystems.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    15 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 25 )

      double precision a(n_max,n_max)
      double precision alpha
      double precision beta
      double precision d(n_max)
      double precision error_frobenius
      double precision gam
      integer i
      integer i1
      integer i4_hi
      integer i4_lo
      integer i4_uniform_ab
      integer k
      integer key
      double precision lambda(n_max)
      integer n
      double precision norm_frobenius
      double precision r8_hi
      double precision r8_lo
      double precision r8_uniform_ab
      double precision r8mat_norm_fro
      integer rank
      integer seed
      character * ( 20 ) title
      double precision v1(n_max)
      double precision v2(n_max)
      double precision v3(n_max)
      double precision x(n_max,n_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST_EIGEN_LEFT'
      write ( *, '(a)' ) 
     &  '  Compute the Frobenius norm of the eigenvalue error:'
      write ( *, '(a)' ) '    X * A - LAMBDA * X'
      write ( *, '(a)' ) 
     &  '  given K left eigenvectors X and eigenvalues LAMBDA.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Title                    N     K      ||A||' 
     &  // '          ||X*A-Lambda*X||'
      write ( *, '(a)' ) ' '
c
c  A123
c
      title = 'A123'
      n = 3
      k = 3
      call a123 ( a )
      call a123_eigenvalues ( lambda )
      call a123_eigen_left ( x )
      call r8mat_is_eigen_left ( n, k, a, x, lambda, error_frobenius )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, k, norm_frobenius, error_frobenius
c
c  CARRY
c
      title = 'CARRY'
      n = 5
      k = 5
      i4_lo = 2
      i4_hi = 20
      seed = 123456789
      i1 = i4_uniform_ab ( i4_lo, i4_hi, seed )
      call carry ( n, i1, a )
      call carry_eigenvalues ( n, i1, lambda )
      call carry_eigen_left ( n, i1, x )
      call r8mat_is_eigen_left ( n, k, a, x, lambda, error_frobenius )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, k, norm_frobenius, error_frobenius
c
c  CHOW
c
      title = 'CHOW'
      n = 5
      k = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      beta = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call chow ( alpha, beta, n, n, a )
      call chow_eigenvalues ( alpha, beta, n, lambda )
      call chow_eigen_left ( alpha, beta, n, x )
      call r8mat_is_eigen_left ( n, k, a, x, lambda, error_frobenius )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, k, norm_frobenius, error_frobenius
c
c  DIAGONAL
c
      title = 'DIAGONAL'
      n = 5
      k = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, d )
      call diagonal ( n, n, d, a )
      call diagonal_eigenvalues ( n, d, lambda )
      call diagonal_eigen_left ( n, d, x )
      call r8mat_is_eigen_left ( n, k, a, x, lambda, error_frobenius )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, k, norm_frobenius, error_frobenius
c
c  ROSSER1
c
      title = 'ROSSER1'
      n = 8
      k = 8
      call rosser1 ( a )
      call rosser1_eigenvalues ( lambda )
      call rosser1_eigen_left ( x )
      call r8mat_is_eigen_left ( n, k, a, x, lambda, error_frobenius )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, k, norm_frobenius, error_frobenius
c
c  SYMM_RANDOM
c
      title = 'SYMM_RANDOM'
      n = 5
      k = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, d )
      key = 123456789
      call symm_random ( n, d, key, a )
      call symm_random_eigenvalues ( n, d, key, lambda )
      call symm_random_eigen_left ( n, d, key, x )
      call r8mat_is_eigen_left ( n, k, a, x, lambda, error_frobenius )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, k, norm_frobenius, error_frobenius

      return
      end
      subroutine test_eigen_right ( )

c*********************************************************************72
c
cc TEST_EIGEN_RIGHT tests right eigensystems.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 April 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 25 )

      double precision a(n_max,n_max)
      double precision alpha
      double precision beta
      double precision d(n_max)
      double precision error_frobenius
      double precision gam
      integer i
      integer i1
      integer i4_hi
      integer i4_lo
      integer i4_uniform_ab
      integer k
      integer key
      double precision lambda(n_max)
      integer n
      double precision norm_frobenius
      double precision r8_hi
      double precision r8_lo
      double precision r8_uniform_ab
      double precision r8mat_norm_fro
      integer rank
      integer seed
      character * ( 20 ) title
      double precision v1(n_max)
      double precision v2(n_max)
      double precision v3(n_max)
      double precision x(n_max,n_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST_EIGEN_RIGHT'
      write ( *, '(a)' ) 
     &  '  Compute the Frobenius norm of the eigenvalue error:'
      write ( *, '(a)' ) '    A * X - X * LAMBDA'
      write ( *, '(a)' ) 
     &  '  given K right eigenvectors X and eigenvalues LAMBDA.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Title                    N     K      ||A||' 
     &  // '          ||A*X-X*Lambda||'
      write ( *, '(a)' ) ' '
c
c  A123
c
      title = 'A123'
      n = 3
      k = 3
      call a123 ( a )
      call a123_eigenvalues ( lambda )
      call a123_eigen_right ( x )
      call r8mat_is_eigen_right ( n, k, a, x, lambda, error_frobenius )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, k, norm_frobenius, error_frobenius
c
c  BAB
c
      title = 'BAB'
      n = 5
      k = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      beta = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call bab ( n, alpha, beta, a )
      call bab_eigenvalues ( n, alpha, beta, lambda )
      call bab_eigen_right ( n, alpha, beta, x )
      call r8mat_is_eigen_right ( n, k, a, x, lambda, error_frobenius )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, k, norm_frobenius, error_frobenius
c
c  BODEWIG
c
      title = 'BODEWIG'
      n = 4
      k = 4
      call bodewig ( a )
      call bodewig_eigenvalues ( lambda )
      call bodewig_eigen_right ( x )
      call r8mat_is_eigen_right ( n, k, a, x, lambda, error_frobenius )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, k, norm_frobenius, error_frobenius
c
c  CARRY
c
      title = 'CARRY'
      n = 5
      k = 5
      i4_lo = 2
      i4_hi = 20
      seed = 123456789
      i1 = i4_uniform_ab ( i4_lo, i4_hi, seed )
      call carry ( n, i1, a )
      call carry_eigenvalues ( n, i1, lambda )
      call carry_eigen_right ( n, i1, x )
      call r8mat_is_eigen_right ( n, k, a, x, lambda, error_frobenius )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, k, norm_frobenius, error_frobenius
c
c  CHOW
c
      title = 'CHOW'
      n = 5
      k = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      beta = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call chow ( alpha, beta, n, n, a )
      call chow_eigenvalues ( alpha, beta, n, lambda )
      call chow_eigen_right ( alpha, beta, n, x )
      call r8mat_is_eigen_right ( n, k, a, x, lambda, error_frobenius )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, k, norm_frobenius, error_frobenius
c
c  COMBIN
c
      title = 'COMBIN'
      n = 5
      k = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      beta = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call combin ( alpha, beta, n, a )
      call combin_eigenvalues ( alpha, beta, n, lambda )
      call combin_eigen_right ( alpha, beta, n, x )
      call r8mat_is_eigen_right ( n, k, a, x, lambda, error_frobenius )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, k, norm_frobenius, error_frobenius
c
c  DIF2
c
      title = 'DIF2'
      n = 5
      k = 5
      call dif2 ( n, n, a )
      call dif2_eigenvalues ( n, lambda )
      call dif2_eigen_right ( n, x )
      call r8mat_is_eigen_right ( n, k, a, x, lambda, error_frobenius )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, k, norm_frobenius, error_frobenius
c
c  EXCHANGE
c
      title = 'EXCHANGE'
      n = 5
      k = 5
      call exchange ( n, n, a )
      call exchange_eigenvalues ( n, lambda )
      call exchange_eigen_right ( n, x )
      call r8mat_is_eigen_right ( n, k, a, x, lambda, error_frobenius )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, k, norm_frobenius, error_frobenius
c
c  IDEM_RANDOM
c
      title = 'IDEM_RANDOM'
      n = 5
      k = 5
      rank = 3
      key = 123456789
      call idem_random ( n, rank, key, a )
      call idem_random_eigenvalues ( n, rank, key, lambda )
      call idem_random_eigen_right ( n, rank, key, x )
      call r8mat_is_eigen_right ( n, k, a, x, lambda, error_frobenius )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, k, norm_frobenius, error_frobenius
c
c  IDENTITY
c
      title = 'IDENTITY'
      n = 5
      k = 5
      call identity ( n, n, a )
      call identity_eigenvalues ( n, lambda )
      call identity_eigen_right ( n, x )
      call r8mat_is_eigen_right ( n, k, a, x, lambda, error_frobenius )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, k, norm_frobenius, error_frobenius
c
c  ILL3
c
      title = 'ILL3'
      n = 3
      k = 3
      call ill3 ( a )
      call ill3_eigenvalues ( lambda )
      call ill3_eigen_right ( x )
      call r8mat_is_eigen_right ( n, k, a, x, lambda, error_frobenius )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, k, norm_frobenius, error_frobenius
c
c  KERSHAW
c
      title = 'KERSHAW'
      n = 4
      k = 4
      call kershaw ( a )
      call kershaw_eigenvalues ( lambda )
      call kershaw_eigen_right ( x )
      call r8mat_is_eigen_right ( n, k, a, x, lambda, error_frobenius )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, k, norm_frobenius, error_frobenius
c
c  KMS
c  Eigenvalue information requires 0 <= ALPHA <= 1.
c
      title = 'KMS'
      n = 5
      k = 5
      r8_lo = 0.0D+00
      r8_hi = 1.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call kms ( alpha, n, n, a )
      call kms_eigenvalues ( alpha, n, lambda )
      call kms_eigen_right ( alpha, n, x )
      call r8mat_is_eigen_right ( n, k, a, x, lambda, error_frobenius )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, k, norm_frobenius, error_frobenius
c
c  LINE_ADJ
c
      title = 'LINE_ADJ'
      n = 5
      k = 5
      call line_adj ( n, a )
      call line_adj_eigenvalues ( n, lambda )
      call line_adj_eigen_right ( n, x )
      call r8mat_is_eigen_right ( n, k, a, x, lambda, error_frobenius )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, k, norm_frobenius, error_frobenius
c
c  LINE_LOOP_ADJ
c
      title = 'LINE_LOOP_ADJ'
      n = 5
      k = 5
      call line_loop_adj ( n, a )
      call line_loop_adj_eigenvalues ( n, lambda )
      call line_loop_adj_eigen_right ( n, x )
      call r8mat_is_eigen_right ( n, k, a, x, lambda, error_frobenius )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, k, norm_frobenius, error_frobenius
c
c  ONE
c
      title = 'ONE'
      n = 5
      k = 5
      call one ( n, n, a )
      call one_eigenvalues ( n, lambda )
      call one_eigen_right ( n, x )
      call r8mat_is_eigen_right ( n, k, a, x, lambda, error_frobenius )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, k, norm_frobenius, error_frobenius
c
c  ORTEGA
c
      title = 'ORTEGA'
      n = 5
      k = n
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, v1 )
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, v2 )
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, v3 )
      call ortega ( n, v1, v2, v3, a )
      call ortega_eigenvalues ( n, v1, v2, v3, lambda )
      call ortega_eigen_right ( n, v1, v2, v3, x )
      call r8mat_is_eigen_right ( n, k, a, x, lambda, error_frobenius )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, k, norm_frobenius, error_frobenius
c
c  OTO
c
      title = 'OTO'
      n = 5
      k = 5
      call oto ( n, n, a )
      call oto_eigenvalues ( n, lambda )
      call oto_eigen_right ( n, x )
      call r8mat_is_eigen_right ( n, k, a, x, lambda, error_frobenius )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, k, norm_frobenius, error_frobenius
c
c  PDS_RANDOM
c
      title = 'PDS_RANDOM'
      n = 5
      k = 5
      key = 123456789
      call pds_random ( n, key, a )
      call pds_random_eigenvalues ( n, key, lambda )
      call pds_random_eigen_right ( n, key, x )
      call r8mat_is_eigen_right ( n, k, a, x, lambda, error_frobenius )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, k, norm_frobenius, error_frobenius
c
c  PEI
c
      title = 'PEI'
      n = 5
      k = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call pei ( alpha, n, a )
      call pei_eigenvalues ( alpha, n, lambda )
      call pei_eigen_right ( alpha, n, x )
      call r8mat_is_eigen_right ( n, k, a, x, lambda, error_frobenius )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, k, norm_frobenius, error_frobenius
c
c  RODMAN
c
      title = 'RODMAN'
      n = 5
      k = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call rodman ( n, n, alpha, a )
      call rodman_eigenvalues ( n, alpha, lambda )
      call rodman_eigen_right ( n, alpha, x )
      call r8mat_is_eigen_right ( n, k, a, x, lambda, error_frobenius )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, k, norm_frobenius, error_frobenius
c
c  ROSSER1
c
      title = 'ROSSER1'
      n = 8
      k = 8
      call rosser1 ( a )
      call rosser1_eigenvalues ( lambda )
      call rosser1_eigen_right ( x )
      call r8mat_is_eigen_right ( n, k, a, x, lambda, error_frobenius )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, k, norm_frobenius, error_frobenius
c
c  RUTIS1
c
      title = 'RUTIS1'
      n = 4
      k = 4
      call rutis1 ( a )
      call rutis1_eigenvalues ( lambda )
      call rutis1_eigen_right ( x )
      call r8mat_is_eigen_right ( n, k, a, x, lambda, error_frobenius )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, k, norm_frobenius, error_frobenius
c
c  RUTIS2
c
      title = 'RUTIS2'
      n = 4
      k = 4
      call rutis2 ( a )
      call rutis2_eigenvalues ( lambda )
      call rutis2_eigen_right ( x )
      call r8mat_is_eigen_right ( n, k, a, x, lambda, error_frobenius )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, k, norm_frobenius, error_frobenius
c
c  RUTIS3
c  COMPLEX eigenvalues cannot be handled yetc
c
      if ( .false. ) then
        title = 'RUTIS3'
        n = 4
        k = 4
        call rutis3 ( a )
        call rutis3_eigenvalues ( lambda )
        call rutis3_eigen_right ( x )
c       call r8mat_is_eigen_right ( n, k, a, x, lambda, error_frobenius )
        norm_frobenius = r8mat_norm_fro ( n, n, a )
        write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &    title, n, k, norm_frobenius, error_frobenius

      end if
c
c  RUTIS5
c
      title = 'RUTIS5'
      n = 4
      k = 4
      call rutis5 ( a )
      call rutis5_eigenvalues ( lambda )
      call rutis5_eigen_right ( x )
      call r8mat_is_eigen_right ( n, k, a, x, lambda, error_frobenius )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, k, norm_frobenius, error_frobenius
c
c  SYLVESTER_KAC
c
      title = 'SYLVESTER_KAC'
      n = 5
      k = 5
      call sylvester_kac ( n, a )
      call sylvester_kac_eigenvalues ( n, lambda )
      call sylvester_kac_eigen_right ( n, x )
      call r8mat_is_eigen_right ( n, k, a, x, lambda, error_frobenius )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, k, norm_frobenius, error_frobenius
c
c  SYMM_RANDOM
c
      title = 'SYMM_RANDOM'
      n = 5
      k = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, d )
      key = 123456789
      call symm_random ( n, d, key, a )
      call symm_random_eigenvalues ( n, d, key, lambda )
      call symm_random_eigen_right ( n, d, key, x )
      call r8mat_is_eigen_right ( n, k, a, x, lambda, error_frobenius )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, k, norm_frobenius, error_frobenius
c
c  WILK12
c
      title = 'WILK12'
      n = 12
      k = 12
      call wilk12 ( a )
      call wilk12_eigenvalues ( lambda )
      call wilk12_eigen_right ( x )
      call r8mat_is_eigen_right ( n, k, a, x, lambda, error_frobenius )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, k, norm_frobenius, error_frobenius
c
c  WILSON
c
      title = 'WILSON'
      n = 4
      k = 4
      call wilson ( a )
      call wilson_eigenvalues ( lambda )
      call wilson_eigen_right ( x )
      call r8mat_is_eigen_right ( n, k, a, x, lambda, error_frobenius )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, k, norm_frobenius, error_frobenius
c
c  ZERO
c
      title = 'ZERO'
      n = 5
      k = 5
      call zero ( n, n, a )
      call zero_eigenvalues ( n, lambda )
      call zero_eigen_right ( n, x )
      call r8mat_is_eigen_right ( n, k, a, x, lambda, error_frobenius )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, n, k, norm_frobenius, error_frobenius

      return
      end
      subroutine test_inverse ( )

c*********************************************************************72
c
cc TEST_INVERSE tests the inverse computations.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 April 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 25 )

      double precision a(n_max,n_max)
      double precision alpha
      double precision b(n_max,n_max)
      double precision beta
      double precision c(n_max,n_max)
      double precision d(n_max)
      double precision error_ab
      double precision error_ac
      double precision gam
      integer i
      integer i4_hi
      integer i4_lo
      integer i4_uniform_ab
      integer k
      integer key
      double precision l(n_max,n_max)
      integer n
      double precision norm_frobenius
      double precision p(n_max,n_max)
      integer pivot(n_max)
      double precision r8_hi
      double precision r8_lo
      double precision r8_uniform_ab
      double precision r8mat_norm_fro
      integer seed
      character * ( 20 ) title
      double precision u(n_max,n_max)
      double precision v1(n_max)
      double precision v2(n_max)
      double precision v3(n_max)
      double precision x(n_max)
      integer x_n
      double precision y(n_max)
      integer y_n
      double precision z(n_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST_INVERSE'
      write ( *, '(a)' ) '  A = a test matrix of order N;'
      write ( *, '(a)' ) '  B = inverse as computed by a routine.'
      write ( *, '(a)' ) '  C = inverse as computed by R8MAT_INVERSE.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  ||A||    = Frobenius norm of A.'
      write ( *, '(a)' ) '  ||I-AB|| = Frobenius norm of I-A*B.'
      write ( *, '(a)' ) '  ||I-AC|| = Frobenius norm of I-A*C.'

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Title                    N        ' // 
     &               '||A||        ||I-AB||        ||I-AC||'
      write ( *, '(a)' ) ' '
c
c  AEGERTER
c
      title = 'AEGERTER'
      n = 5
      call aegerter ( n, a )
      call aegerter_inverse ( n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  BAB
c
      title = 'BAB'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      beta = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call bab ( n, alpha, beta, a )
      call bab_inverse ( n, alpha, beta, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  BAUER
c
      title = 'BAUER'
      n = 6
      call bauer ( a )
      call bauer_inverse ( b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  BERNSTEIN
c
      title = 'BERNSTEIN'
      n = 5
      call bernstein ( n, a )
      call bernstein_inverse ( n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  BIS
c
      title = 'BIS'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      beta = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call bis ( alpha, beta, n, n, a );
      call bis_inverse ( alpha, beta, n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  BIW
c
      title = 'BIW'
      n = 5
      call biw ( n, a );
      call biw_inverse ( n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  BODEWIG
c
      title = 'BODEWIG'
      n = 4
      call bodewig ( a )
      call bodewig_inverse ( b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  BOOTHROYD
c
      title = 'BOOTHROYD'
      n = 5
      call boothroyd ( n, a )
      call boothroyd_inverse ( n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  BORDERBAND
c
      title = 'BORDERBAND'
      n = 5
      call borderband ( n, a )
      call borderband_inverse ( n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  CARRY
c
      title = 'CARRY'
      n = 5
      i4_lo = 2
      i4_hi = 20
      seed = 123456789
      k = i4_uniform_ab ( i4_lo, i4_hi, seed )
      call carry ( n, k, a )
      call carry_inverse ( n, k, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  CAUCHY
c
      title = 'CAUCHY'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, y )
      call cauchy ( n, x, y, a )
      call cauchy_inverse ( n, x, y, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  CHEBY_T
c
      title = 'CHEBY_T'
      n = 5
      call cheby_t ( n, a )
      call cheby_t_inverse ( n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  CHEBY_U
c
      title = 'CHEBY_U'
      n = 5
      call cheby_u ( n, a )
      call cheby_u_inverse ( n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  CHEBY_VAN2
c
      title = 'CHEBY_VAN2'
      n = 5
      call cheby_van2 ( n, a )
      call cheby_van2_inverse ( n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  CHEBY_VAN3
c
      title = 'CHEBY_VAN3'
      n = 5
      call cheby_van3 ( n, a )
      call cheby_van3_inverse ( n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  CHOW
c
      title = 'CHOW'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      beta = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call chow ( alpha, beta, n, n, a )
      call chow_inverse ( alpha, beta, n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  CIRCULANT
c
      if ( .false. ) then
      title = 'CIRCULANT'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
      call circulant ( n, n, x, a )
c     call circulant_inverse ( n, x, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
      else
      write ( *, '(2x,a)' ) 'CIRCULANT --- NOT READY'
      end if
c
c  CIRCULANT2
c
      title = 'CIRCULANT2'
      n = 5
      call circulant2 ( n, a )
      call circulant2_inverse ( n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  CLEMENT1
c
      title = 'CLEMENT1'
      n = 6
      call clement1 ( n, a )
      call clement1_inverse ( n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  CLEMENT2
c
      title = 'CLEMENT2'
      n = 6
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      call r8vec_uniform_ab ( n - 1, r8_lo, r8_hi, seed, x )
      call r8vec_uniform_ab ( n - 1, r8_lo, r8_hi, seed, y )
      call clement2 ( n, x, y, a )
      call clement2_inverse ( n, x, y, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  COMBIN
c
      title = 'COMBIN'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      beta = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call combin ( alpha, beta, n, a )
      call combin_inverse ( alpha, beta, n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  COMPANION
c
      title = 'COMPANION'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
      call companion ( n, x, a )
      call companion_inverse ( n, x, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  COMPLEX_I
c
      title = 'COMPLEX_I'
      n = 2
      call complex_i ( a )
      call complex_i_inverse ( b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  CONEX1
c
      title = 'CONEX1'
      n = 4
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call conex1 ( alpha, a )
      call conex1_inverse ( alpha, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  CONEX2
c
      title = 'CONEX2'
      n = 3
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call conex2 ( alpha, a )
      call conex2_inverse ( alpha, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  CONEX3
c
      title = 'CONEX3'
      n = 5
      call conex3 ( n, a )
      call conex3_inverse ( n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  CONFERENCE
c
      title = 'CONFERENCE'
      n = 6
      call conference ( n, a )
      call conference_inverse ( n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  DAUB2
c
      title = 'DAUB2'
      n = 4
      call daub2 ( n, a )
      call daub2_inverse ( n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  DAUB4
c
      title = 'DAUB4'
      n = 8
      call daub4 ( n, a )
      call daub4_inverse ( n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  DAUB6
c
      title = 'DAUB6'
      n = 12
      call daub6 ( n, a )
      call daub6_inverse ( n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  DAUB8
c
      title = 'DAUB8'
      n = 16
      call daub8 ( n, a )
      call daub8_inverse ( n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  DAUB10
c
      title = 'DAUB10'
      n = 20
      call daub10 ( n, a )
      call daub10_inverse ( n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  DAUB12
c
      title = 'DAUB12'
      n = 24
      call daub12 ( n, a )
      call daub12_inverse ( n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  DIAGONAL
c
      title = 'DIAGONAL'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
      call diagonal ( n, n, x, a )
      call diagonal_inverse ( n, x, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  DIF1
c  N must be even.
c
      title = 'DIF1'
      n = 6
      call dif1 ( n, n, a )
      call dif1_inverse ( n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  DIF2
c
      title = 'DIF2'
      n = 5
      call dif2 ( n, n, a )
      call dif2_inverse ( n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  DORR
c
      title = 'DORR'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call dorr ( alpha, n, a )
      call dorr_inverse ( alpha, n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  DOWNSHIFT
c
      title = 'DOWNSHIFT'
      n = 5
      call downshift ( n, a )
      call downshift_inverse ( n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  EULERIAN
c
      title = 'EULERIAN'
      n = 5
      call eulerian ( n, n, a )
      call eulerian_inverse ( n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  EXCHANGE
c
      title = 'EXCHANGE'
      n = 5
      call exchange ( n, n, a )
      call exchange_inverse ( n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  FIBONACCI2
c
      title = 'FIBONACCI2'
      n = 5
      call fibonacci2 ( n, a )
      call fibonacci2_inverse ( n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  FIBONACCI3
c
      title = 'FIBONACCI3'
      n = 5
      call fibonacci3 ( n, a )
      call fibonacci3_inverse ( n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  FIEDLER
c  The FIEDLER_INVERSE routine assumes the X vector is sorted.
c
      title = 'FIEDLER'
      n = 7
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
      call r8vec_sort_bubble_a ( n, x )
      call fiedler ( n, n, x, a )
      call fiedler_inverse ( n, x, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  FORSYTHE
c
      title = 'FORSYTHE'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      beta = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call forsythe ( alpha, beta, n, a )
      call forsythe_inverse ( alpha, beta, n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  FOURIER_COSINE
c
      title = 'FOURIER_COSINE'
      n = 5
      call fourier_cosine ( n, a )
      call fourier_cosine_inverse ( n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  FOURIER_SINE
c
      title = 'FOURIER_SINE'
      n = 5
      call fourier_sine ( n, a )
      call fourier_sine_inverse ( n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  FRANK
c
      title = 'FRANK'
      n = 5
      call frank ( n, a )
      call frank_inverse ( n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  GFPP
c
      title = 'GFPP'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call gfpp ( n, alpha, a )
      call gfpp_inverse ( n, alpha, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  GIVENS
c
      title = 'GIVENS'
      n = 5
      call givens ( n, n, a )
      call givens_inverse ( n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  GK316
c
      title = 'GK316'
      n = 5
      call gk316 ( n, a )
      call gk316_inverse ( n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  GK323
c
      title = 'GK323'
      n = 5
      call gk323 ( n, n, a )
      call gk323_inverse ( n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  GK324
c
      title = 'GK324'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      call r8vec_uniform_ab ( n - 1, r8_lo, r8_hi, seed, x )
      call gk324 ( n, n, x, a )
      call gk324_inverse ( n, x, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  HANKEL_N
c
      title = 'HANKEL_N'
      n = 6
      call hankel_n ( n, a )
      call hankel_n_inverse ( n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  HANOWA
c
      title = 'HANOWA'
      n = 8
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call hanowa ( alpha, n, a )
      call hanowa_inverse ( alpha, n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  HARMAN
c
      title = 'HARMAN'
      n = 8
      call harman ( a )
      call harman_inverse ( b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  HARTLEY
c
      title = 'HARTLEY'
      n = 5
      call hartley ( n, a )
      call hartley_inverse ( n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  HELMERT
c
      title = 'HELMERT'
      n = 5
      call helmert ( n, a )
      call helmert_inverse ( n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  HELMERT2
c
      title = 'HELMERT2'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
      call helmert2 ( n, x, a )
      call helmert2_inverse ( n, x, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  HERMITE
c
      title = 'HERMITE'
      n = 5
      call hermite ( n, a )
      call hermite_inverse ( n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  HERNDON
c
      title = 'HERNDON'
      n = 5
      call herndon ( n, a )
      call herndon_inverse ( n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  HILBERT
c
      title = 'HILBERT'
      n = 5
      call hilbert ( n, n, a )
      call hilbert_inverse ( n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  HOUSEHOLDER
c
      title = 'HOUSEHOLDER'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
      call householder ( n, x, a )
      call householder_inverse ( n, x, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  IDENTITY
c
      title = 'IDENTITY'
      n = 5
      call identity ( n, n, a )
      call identity_inverse ( n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  ILL3
c
      title = 'ILL3'
      n = 3
      call ill3 ( a )
      call ill3_inverse ( b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  INTEGRATION
c
      title = 'INTEGRATION'
      n = 6
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call integration ( alpha, n, a )
      call integration_inverse ( alpha, n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  INVOL
c
      title = 'INVOL'
      n = 5
      call invol ( n, a )
      call invol_inverse ( n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  JACOBI
c  N must be even.
c
      title = 'JACOBI'
      n = 6
      call jacobi ( n, n, a )
      call jacobi_inverse ( n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  JORDAN
c
      title = 'JORDAN'
      n = 6
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call jordan ( n, n, alpha, a )
      call jordan_inverse ( n, alpha, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  KAHAN
c
      title = 'KAHAN'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call kahan ( alpha, n, n, a )
      call kahan_inverse ( alpha, n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  KERSHAW
c
      title = 'KERSHAW'
      n = 4
      call kershaw ( a )
      call kershaw_inverse ( b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  KERSHAWTRI
c
      title = 'KERSHAWTRI'
      n = 5
      x_n = ( n + 1 ) / 2
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      call r8vec_uniform_ab ( x_n, r8_lo, r8_hi, seed, x )
      call kershawtri ( n, x, a )
      call kershawtri_inverse ( n, x, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  KMS
c
      title = 'KMS'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call kms ( alpha, n, n, a )
      call kms_inverse ( alpha, n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  LAGUERRE
c
      title = 'LAGUERRE'
      n = 5
      call laguerre ( n, a )
      call laguerre_inverse ( n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  LEGENDRE
c
      title = 'LEGENDRE'
      n = 5
      call legendre ( n, a )
      call legendre_inverse ( n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  LEHMER
c
      title = 'LEHMER'
      n = 5
      call lehmer ( n, n, a )
      call lehmer_inverse ( n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  LESP
c
      title = 'LESP'
      n = 5
      call lesp ( n, n, a )
      call lesp_inverse ( n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  LIETZKE
c
      title = 'LIETZKE'
      n = 5
      call lietzke ( n, a )
      call lietzke_inverse ( n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  LINE_ADJ
c  N must be even.
c
      title = 'LINE_ADJ'
      n = 6
      call line_adj ( n, a )
      call line_adj_inverse ( n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  LOTKIN
c
      title = 'LOTKIN'
      n = 5
      call lotkin ( n, n, a )
      call lotkin_inverse ( n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  MAXIJ
c
      title = 'MAXIJ'
      n = 5
      call maxij ( n, n, a )
      call maxij_inverse ( n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  MILNES
c
      title = 'MILNES'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
      call milnes ( n, n, x, a )
      call milnes_inverse ( n, x, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  MINIJ
c
      title = 'MINIJ'
      n = 5
      call minij ( n, n, a )
      call minij_inverse ( n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  MOLER1
c
      title = 'MOLER1'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call moler1 ( alpha, n, n, a )
      call moler1_inverse ( alpha, n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  MOLER3
c
      title = 'MOLER3'
      n = 5
      call moler3 ( n, n, a )
      call moler3_inverse ( n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  ORTEGA
c
      title = 'ORTEGA'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, v1 )
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, v2 )
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, v3 )
      call ortega ( n, v1, v2, v3, a )
      call ortega_inverse ( n, v1, v2, v3, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  ORTH_SYMM
c
      title = 'ORTH_SYMM'
      n = 5
      call orth_symm ( n, a )
      call orth_symm_inverse ( n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  OTO
c
      title = 'OTO'
      n = 5
      call oto ( n, n, a )
      call oto_inverse ( n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  PARTER
c
      title = 'PARTER'
      n = 5
      call parter ( n, n, a )
      call parter_inverse ( n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  PASCAL1
c
      title = 'PASCAL1'
      n = 5
      call pascal1 ( n, a )
      call pascal1_inverse ( n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  PASCAL2
c
      title = 'PASCAL2'
      n = 5
      call pascal2 ( n, a )
      call pascal2_inverse ( n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  PASCAL3
c
      title = 'PASCAL3'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call pascal3 ( n, alpha, a )
      call pascal3_inverse ( n, alpha, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  PDS_RANDOM
c
      title = 'PDS_RANDOM'
      n = 5
      key = 123456789
      call pds_random ( n, key, a )
      call pds_random_inverse ( n, key, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  PEI
c
      title = 'PEI'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call pei ( alpha, n, a )
      call pei_inverse ( alpha, n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  PERMUTATION_RANDOM
c
      title = 'PERMUTATION_RANDOM'
      n = 5
      key = 123456789
      call permutation_random ( n, key, a )
      call permutation_random_inverse ( n, key, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  PLU
c
      title = 'PLU'
      n = 5
      seed = 123456789
      do i = 1, n
        i4_lo = i
        i4_hi = n
        pivot(i) = i4_uniform_ab ( i4_lo, i4_hi, seed )
      end do
      call plu ( n, pivot, a )
      call plu_inverse ( n, pivot, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  RIS
c
      title = 'RIS'
      n = 5
      call ris ( n, a )
      call ris_inverse ( n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  RODMAN
c
      title = 'RODMAN'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call rodman ( n, n, alpha, a )
      call rodman_inverse ( n, alpha, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  RUTIS1
c
      title = 'RUTIS1'
      n = 4
      call rutis1 ( a )
      call rutis1_inverse ( b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  RUTIS2
c
      title = 'RUTIS2'
      n = 4
      call rutis2 ( a )
      call rutis2_inverse ( b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  RUTIS3
c
      title = 'RUTIS3'
      n = 4
      call rutis3 ( a )
      call rutis3_inverse ( b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  RUTIS4
c
      title = 'RUTIS4'
      n = 5
      call rutis4 ( n, a )
      call rutis4_inverse ( n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  RUTIS5
c
      title = 'RUTIS5'
      n = 4
      call rutis5 ( a )
      call rutis5_inverse ( b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  SCHUR_BLOCK
c
      title = 'SCHUR_BLOCK'
      n = 5
      x_n = ( n + 1 ) / 2
      y_n = n / 2
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      call r8vec_uniform_ab ( x_n, r8_lo, r8_hi, seed, x )
      call r8vec_uniform_ab ( y_n, r8_lo, r8_hi, seed, y )
      call schur_block ( n, x, y, a )
      call schur_block_inverse ( n, x, y, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  SPLINE
c
      title = 'SPLINE'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      call r8vec_uniform_ab ( n - 1, r8_lo, r8_hi, seed, x )
      call spline ( n, x, a )
      call spline_inverse ( n, x, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  STIRLING
c
      title = 'STIRLING'
      n = 5
      call stirling ( n, n, a )
      call stirling_inverse ( n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  SWEET1
c
      title = 'SWEET1'
      n = 6
      call sweet1 ( a )
      call sweet1_inverse ( b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  SWEET2
c
      title = 'SWEET2'
      n = 6
      call sweet2 ( a )
      call sweet2_inverse ( b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  SWEET3
c
      title = 'SWEET3'
      n = 6
      call sweet3 ( a )
      call sweet3_inverse ( b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  SWEET4
c
      title = 'SWEET4'
      n = 13
      call sweet4 ( a )
      call sweet4_inverse ( b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  SUMMATION
c
      title = 'SUMMATION'
      n = 5
      call summation ( n, n, a )
      call summation_inverse ( n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  SYLVESTER_KAC
c  N must be even.
c
      title = 'SYLVESTER_KAC'
      n = 6
      call sylvester_kac ( n, a )
      call sylvester_kac_inverse ( n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  SYMM_RANDOM
c
      title = 'SYMM_RANDOM'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, d )
      key = 123456789
      call symm_random ( n, d, key, a )
      call symm_random_inverse ( n, d, key, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  TRI_UPPER
c
      title = 'TRI_UPPER'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call tri_upper ( alpha, n, a )
      call tri_upper_inverse ( alpha, n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  TRIS
c
      title = 'TRIS'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      beta = r8_uniform_ab ( r8_lo, r8_hi, seed )
      gam = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call tris ( n, n, alpha, beta, gam, a )
      call tris_inverse ( n, alpha, beta, gam, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  TRIV
c
      title = 'TRIV'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      call r8vec_uniform_ab ( n - 1, r8_lo, r8_hi, seed, x )
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, y )
      call r8vec_uniform_ab ( n - 1, r8_lo, r8_hi, seed, z )
      call triv ( n, x, y, z, a )
      call triv_inverse ( n, x, y, z, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  TRIW
c
      title = 'TRIW'
      n = 5
      i4_lo = 0
      i4_hi = n - 1
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      k = i4_uniform_ab ( i4_lo, i4_hi, seed )
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call triw ( alpha, k, n, a )
      call triw_inverse ( alpha, k, n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  UPSHIFT
c
      title = 'UPSHIFT'
      n = 5
      call upshift ( n, a )
      call upshift_inverse ( n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  VAND1
c
      title = 'VAND1'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
      call vand1 ( n, x, a )
      call vand1_inverse ( n, x, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  VAND2
c
      title = 'VAND2'
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      call r8vec_uniform_ab ( n, r8_lo, r8_hi, seed, x )
      call vand2 ( n, x, a )
      call vand2_inverse ( n, x, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  WILK03
c
      title = 'WILK03'
      n = 3
      call wilk03 ( a )
      call wilk03_inverse ( b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  WILK04
c
      title = 'WILK04'
      n = 4
      call wilk04 ( a )
      call wilk04_inverse ( b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  WILK05
c
      title = 'WILK05'
      n = 5
      call wilk05 ( a )
      call wilk05_inverse ( b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  WILK21
c
      title = 'WILK21'
      n = 21
      call wilk21 ( n, a )
      call wilk21_inverse ( n, b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac
c
c  WILSON
c
      title = 'WILSON'
      n = 4
      call wilson ( a )
      call wilson_inverse ( b )
      call r8mat_inverse ( n, a, c )
      call r8mat_is_inverse ( n, a, b, error_ab )
      call r8mat_is_inverse ( n, a, c, error_ac )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, n, norm_frobenius, error_ab, error_ac

      return
      end
      subroutine test_llt ( )

c*********************************************************************72
c
cc TEST_LLT tests LLT factors.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 April 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 25 )

      double precision a(n_max,n_max)
      double precision alpha
      double precision error_frobenius
      double precision l(n_max,n_max)
      integer m
      integer n
      double precision norm_a_frobenius
      double precision r8_hi
      double precision r8_lo
      double precision r8_uniform_ab
      double precision r8mat_norm_fro
      integer seed
      character * ( 20 ) title

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST_LLT'
      write ( *, '(a)' ) '  A = a test matrix of order M by M'
      write ( *, '(a)' ) 
     &  '  L is an M by N lower triangular Cholesky factor.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  ||A|| = Frobenius norm of A.'
      write ( *, '(a)' ) '  ||A-LLT|| = Frobenius norm of A-L*L''.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Title                    M     N' //
     &  '      ||A||            ||A-LLT||'
      write ( *, '(a)' ) ''
c
c  DIF2
c
      title = 'DIF2'
      m = 5
      n = 5
      call dif2 ( m, n, a )
      call dif2_llt ( n, l )
      call r8mat_is_llt ( m, n, a, l, error_frobenius )
      norm_a_frobenius = r8mat_norm_fro ( m, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, norm_a_frobenius, error_frobenius
c
c  GIVENS
c
      title = 'GIVENS'
      m = 5
      n = 5
      call givens ( m, n, a )
      call givens_llt ( n, l )
      call r8mat_is_llt ( m, n, a, l, error_frobenius )
      norm_a_frobenius = r8mat_norm_fro ( m, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, norm_a_frobenius, error_frobenius
c
c  KERSHAW
c
      title = 'KERSHAW'
      m = 4
      n = 4
      call kershaw ( a )
      call kershaw_llt ( l )
      call r8mat_is_llt ( m, n, a, l, error_frobenius )
      norm_a_frobenius = r8mat_norm_fro ( m, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, norm_a_frobenius, error_frobenius
c
c  LEHMER
c
      title = 'LEHMER'
      m = 5
      n = 5
      call lehmer ( n, n, a )
      call lehmer_llt ( n, l )
      call r8mat_is_llt ( m, n, a, l, error_frobenius )
      norm_a_frobenius = r8mat_norm_fro ( m, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, norm_a_frobenius, error_frobenius
c
c  MINIJ
c
      title = 'MINIJ'
      m = 5
      n = 5
      call minij ( n, n, a )
      call minij_llt ( n, l )
      call r8mat_is_llt ( m, n, a, l, error_frobenius )
      norm_a_frobenius = r8mat_norm_fro ( m, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, norm_a_frobenius, error_frobenius
c
c  MOLER1
c
      title = 'MOLER1'
      m = 5
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call moler1 ( alpha, m, n, a )
      call moler1_llt ( alpha, n, l )
      call r8mat_is_llt ( m, n, a, l, error_frobenius )
      norm_a_frobenius = r8mat_norm_fro ( m, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, norm_a_frobenius, error_frobenius
c
c  MOLER3
c
      title = 'MOLER3'
      m = 5
      n = 5
      call moler3 ( m, n, a )
      call moler3_llt ( n, l )
      call r8mat_is_llt ( m, n, a, l, error_frobenius )
      norm_a_frobenius = r8mat_norm_fro ( m, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, norm_a_frobenius, error_frobenius
c
c  OTO
c
      title = 'OTO'
      m = 5
      n = 5
      call oto ( m, n, a )
      call oto_llt ( n, l )
      call r8mat_is_llt ( m, n, a, l, error_frobenius )
      norm_a_frobenius = r8mat_norm_fro ( m, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, norm_a_frobenius, error_frobenius
c
c  PASCAL2
c
      title = 'PASCAL2'
      m = 5
      n = 5
      call pascal2 ( n, a )
      call pascal2_llt ( n, l )
      call r8mat_is_llt ( m, n, a, l, error_frobenius )
      norm_a_frobenius = r8mat_norm_fro ( m, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, norm_a_frobenius, error_frobenius
c
c  WILSON
c
      title = 'WILSON'
      m = 4
      n = 4
      call wilson ( a )
      call wilson_llt ( l )
      call r8mat_is_llt ( m, n, a, l, error_frobenius )
      norm_a_frobenius = r8mat_norm_fro ( m, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, norm_a_frobenius, error_frobenius

      return
      end
      subroutine test_null_left ( )

c*********************************************************************72
c
cc TEST_NULL_LEFT tests left null vectors.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 25 )

      double precision a(n_max,n_max)
      double precision alpha
      double precision error_l2
      double precision f1
      double precision f2
      integer m
      integer n
      double precision norm_a_frobenius
      double precision norm_x_l2
      double precision r8_hi
      double precision r8_lo
      double precision r8_uniform_ab
      double precision r8mat_norm_fro
      double precision r8vec_norm_l2
      integer seed
      character*20 title
      double precision x(n_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST_NULL_LEFT'
      write ( *, '(a)' ) '  A = a test matrix of order M by N'
      write ( *, '(a)' ) 
     &  '  x = an M vector, candidate for a left null vector.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  ||A|| = Frobenius norm of A.'
      write ( *, '(a)' ) '  ||x|| = L2 norm of x.'
      write ( *, '(a)' ) 
     &  '  ||A''*x||/||x|| = L2 norm of A''*x over L2 norm of x.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Title       	           M     N      ' //
     &         '||A||            ||x||        ||A''*x||/||x||'
      write ( *, '(a)' ) ' '
c
c  A123
c
      title = 'A123'
      m = 3
      n = 3
      call a123 ( a )
      call a123_null_left ( x )
      call r8mat_is_null_left ( m, n, a, x, error_l2 )
      norm_a_frobenius = r8mat_norm_fro ( m, n, a )
      norm_x_l2 = r8vec_norm_l2 ( m, x )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, norm_a_frobenius, norm_x_l2, error_l2
c
c  CHEBY_DIFf1
c
      title = 'CHEBY_DIFF1'
      m = 5
      n = m
      call cheby_diff1 ( n, a )
      call cheby_diff1_null_left ( m, n, x )
      call r8mat_is_null_left ( m, n, a, x, error_l2 )
      norm_a_frobenius = r8mat_norm_fro ( m, n, a )
      norm_x_l2 = r8vec_norm_l2 ( m, x )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, norm_a_frobenius, norm_x_l2, error_l2
c
c  CREATION
c
      title = 'CREATION'
      m = 5
      n = m
      call creation ( m, n, a )
      call creation_null_left ( m, n, x )
      call r8mat_is_null_left ( m, n, a, x, error_l2 )
      norm_a_frobenius = r8mat_norm_fro ( m, n, a )
      norm_x_l2 = r8vec_norm_l2 ( m, x )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, norm_a_frobenius, norm_x_l2, error_l2 
c
c  DIF1
c  Only has null vectors for M odd.
c
      title = 'DIF1'
      m = 5
      n = m
      call dif1 ( m, n, a )
      call dif1_null_left ( m, n, x )
      call r8mat_is_null_left ( m, n, a, x, error_l2 )
      norm_a_frobenius = r8mat_norm_fro ( m, n, a )
      norm_x_l2 = r8vec_norm_l2 ( m, x )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, norm_a_frobenius, norm_x_l2, error_l2 
c
c  DIF1CYCLIC
c
      title = 'DIF1CYCLIC'
      m = 5
      n = m
      call dif1cyclic ( n, a )
      call dif1cyclic_null_left ( m, n, x )
      call r8mat_is_null_left ( m, n, a, x, error_l2 )
      norm_a_frobenius = r8mat_norm_fro ( m, n, a )
      norm_x_l2 = r8vec_norm_l2 ( m, x )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, norm_a_frobenius, norm_x_l2, error_l2 
c
c  DIF2CYCLIC
c
      title = 'DIF2CYCLIC'
      m = 5
      n = m
      call dif2cyclic ( n, a )
      call dif2cyclic_null_left ( m, n, x )
      call r8mat_is_null_left ( m, n, a, x, error_l2 )
      norm_a_frobenius = r8mat_norm_fro ( m, n, a )
      norm_x_l2 = r8vec_norm_l2 ( m, x )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, norm_a_frobenius, norm_x_l2, error_l2 
c
c  EBERLEIN
c
      title = 'EBERLEIN'
      m = 5
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call eberlein ( alpha, n, a )
      call eberlein_null_left ( m, n, x )
      call r8mat_is_null_left ( m, n, a, x, error_l2 )
      norm_a_frobenius = r8mat_norm_fro ( m, n, a )
      norm_x_l2 = r8vec_norm_l2 ( m, x )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, norm_a_frobenius, norm_x_l2, error_l2 
c
c  FIBONACCI1
c
      title = 'FIBONACCI1'
      m = 5
      n = m
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      f1 = r8_uniform_ab ( r8_lo, r8_hi, seed )
      f2 = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call fibonacci1 ( n, f1, f2, a )
      call fibonacci1_null_left ( m, n, f1, f2, x )
      call r8mat_is_null_left ( m, n, a, x, error_l2 )
      norm_a_frobenius = r8mat_norm_fro ( m, n, a )
      norm_x_l2 = r8vec_norm_l2 ( m, x )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, norm_a_frobenius, norm_x_l2, error_l2 
c
c  LAUCHLI
c
      title = 'LAUCHLI'
      m = 6
      n = m - 1
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call lauchli ( alpha, m, n, a )
      call lauchli_null_left ( alpha, m, n, x )
      call r8mat_is_null_left ( m, n, a, x, error_l2 )
      norm_a_frobenius = r8mat_norm_fro ( m, n, a )
      norm_x_l2 = r8vec_norm_l2 ( m, x )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, norm_a_frobenius, norm_x_l2, error_l2 
c
c  LINE_ADJ
c
      title = 'LINE_ADJ'
      m = 7
      n = m
      call line_adj ( n, a )
      call line_adj_null_left ( m, n, x )
      call r8mat_is_null_left ( m, n, a, x, error_l2 )
      norm_a_frobenius = r8mat_norm_fro ( m, n, a )
      norm_x_l2 = r8vec_norm_l2 ( m, x )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, norm_a_frobenius, norm_x_l2, error_l2 
c
c  MOLER2
c
      title = 'MOLER2'
      m = 5
      n = 5
      call moler2 ( a )
      call moler2_null_left ( x )
      call r8mat_is_null_left ( m, n, a, x, error_l2 )
      norm_a_frobenius = r8mat_norm_fro ( m, n, a )
      norm_x_l2 = r8vec_norm_l2 ( m, x )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, norm_a_frobenius, norm_x_l2, error_l2 
c
c  ONE
c
      title = 'ONE'
      m = 5
      n = 5
      call one ( m, n, a )
      call one_null_left ( m, n, x )
      call r8mat_is_null_left ( m, n, a, x, error_l2 )
      norm_a_frobenius = r8mat_norm_fro ( m, n, a )
      norm_x_l2 = r8vec_norm_l2 ( m, x )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, norm_a_frobenius, norm_x_l2, error_l2 
c
c  RING_ADJ
c  M must be a multiple of 4 for there to be a null vector.
c
      title = 'RING_ADJ'
      m = 12
      n = 12
      call ring_adj ( n, a )
      call ring_adj_null_left ( m, n, x )
      call r8mat_is_null_left ( m, n, a, x, error_l2 )
      norm_a_frobenius = r8mat_norm_fro ( m, n, a )
      norm_x_l2 = r8vec_norm_l2 ( m, x )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, norm_a_frobenius, norm_x_l2, error_l2
c
c  ROSSER1
c
      title = 'ROSSER1'
      m = 8
      n = 8
      call rosser1 ( a )
      call rosser1_null_left ( x )
      call r8mat_is_null_left ( m, n, a, x, error_l2 )
      norm_a_frobenius = r8mat_norm_fro ( m, n, a )
      norm_x_l2 = r8vec_norm_l2 ( m, x )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, norm_a_frobenius, norm_x_l2, error_l2 
c
c  ZERO
c
      title = 'ZERO'
      m = 5
      n = 5
      call zero ( m, n, a )
      call zero_null_left ( m, n, x )
      call r8mat_is_null_left ( m, n, a, x, error_l2 )
      norm_a_frobenius = r8mat_norm_fro ( m, n, a )
      norm_x_l2 = r8vec_norm_l2 ( m, x )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, norm_a_frobenius, norm_x_l2, error_l2 

      return
      end
      subroutine test_null_right ( )

c*********************************************************************72
c
cc TEST_NULL_RIGHT tests right null vectors.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 100 )

      double precision a(n_max,n_max)
      double precision alpha
      integer col_num
      double precision error_l2
      double precision f1
      double precision f2
      integer m
      integer n
      double precision norm_a_frobenius
      double precision norm_x_l2
      double precision r8_hi
      double precision r8_lo
      double precision r8_uniform_ab
      double precision r8mat_norm_fro
      double precision r8vec_norm_l2
      integer row_num
      integer seed
      character*20 title
      double precision x(n_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST_NULL_RIGHT'
      write ( *, '(a)' ) '  A = a test matrix of order M by N'
      write ( *, '(a)' ) 
     &  '  x = an N vector, candidate for a right null vector.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  ||A|| = Frobenius norm of A.'
      write ( *, '(a)' ) '  ||x|| = L2 norm of x.'
      write ( *, '(a)' ) 
     &  '  ||A*x||/||x|| = L2 norm of A*x over L2 norm of x.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Title       	           M     N      ' //
     &         '||A||            ||x||        ||A*x||/||x||'
      write ( *, '(a)' ) ' '
c
c  A123
c
      title = 'A123'
      m = 3
      n = 3
      call a123 ( a )
      call a123_null_right ( x )
      call r8mat_is_null_right ( m, n, a, x, error_l2 )
      norm_a_frobenius = r8mat_norm_fro ( m, n, a )
      norm_x_l2 = r8vec_norm_l2 ( n, x )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, norm_a_frobenius, norm_x_l2, error_l2 
c
c  ARCHIMEDES
c
      title = 'ARCHIMEDES'
      m = 7
      n = 8
      call archimedes ( a )
      call archimedes_null_right ( x )
      call r8mat_is_null_right ( m, n, a, x, error_l2 )
      norm_a_frobenius = r8mat_norm_fro ( m, n, a )
      norm_x_l2 = r8vec_norm_l2 ( n, x )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, norm_a_frobenius, norm_x_l2, error_l2 
c
c  CHEBY_DIFf1
c
      title = 'CHEBY_DIFF1'
      m = 5
      n = m
      call cheby_diff1 ( n, a )
      call cheby_diff1_null_right ( m, n, x )
      call r8mat_is_null_right ( m, n, a, x, error_l2 )
      norm_a_frobenius = r8mat_norm_fro ( m, n, a )
      norm_x_l2 = r8vec_norm_l2 ( n, x )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, norm_a_frobenius, norm_x_l2, error_l2
c
c  CREATION
c
      title = 'CREATION'
      m = 5
      n = m
      call creation ( m, n, a )
      call creation_null_right ( m, n, x )
      call r8mat_is_null_right ( m, n, a, x, error_l2 )
      norm_a_frobenius = r8mat_norm_fro ( m, n, a )
      norm_x_l2 = r8vec_norm_l2 ( n, x )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, norm_a_frobenius, norm_x_l2, error_l2 
c
c  DIF1
c  Only has null vectors for N odd.
c
      title = 'DIF1'
      m = 5
      n = m
      call dif1 ( m, n, a )
      call dif1_null_right ( m, n, x )
      call r8mat_is_null_right ( m, n, a, x, error_l2 )
      norm_a_frobenius = r8mat_norm_fro ( m, n, a )
      norm_x_l2 = r8vec_norm_l2 ( n, x )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, norm_a_frobenius, norm_x_l2, error_l2 
c
c  DIF1CYCLIC
c
      title = 'DIF1CYCLIC'
      m = 5
      n = m
      call dif1cyclic ( n, a )
      call dif1cyclic_null_right ( m, n, x )
      call r8mat_is_null_right ( m, n, a, x, error_l2 )
      norm_a_frobenius = r8mat_norm_fro ( m, n, a )
      norm_x_l2 = r8vec_norm_l2 ( n, x )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, norm_a_frobenius, norm_x_l2, error_l2 
c
c  DIF2CYCLIC
c
      title = 'DIF2CYCLIC'
      m = 5
      n = m
      call dif2cyclic ( n, a )
      call dif2cyclic_null_right ( m, n, x )
      call r8mat_is_null_right ( m, n, a, x, error_l2 )
      norm_a_frobenius = r8mat_norm_fro ( m, n, a )
      norm_x_l2 = r8vec_norm_l2 ( n, x )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, norm_a_frobenius, norm_x_l2, error_l2 
c
c  FIBONACCI1
c
      title = 'FIBONACCI1'
      m = 5
      n = m
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      f1 = r8_uniform_ab ( r8_lo, r8_hi, seed )
      f2 = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call fibonacci1 ( n, f1, f2, a )
      call fibonacci1_null_right ( m, n, f1, f2, x )
      call r8mat_is_null_right ( m, n, a, x, error_l2 )
      norm_a_frobenius = r8mat_norm_fro ( m, n, a )
      norm_x_l2 = r8vec_norm_l2 ( n, x )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, norm_a_frobenius, norm_x_l2, error_l2 
c
c  HAMMING
c
      title = 'HAMMING'
      m = 5
      n = ( 2 ** m ) - 1
      call hamming ( m, n, a )
      call hamming_null_right ( m, n, x )
      call r8mat_is_null_right ( m, n, a, x, error_l2 )
      norm_a_frobenius = r8mat_norm_fro ( m, n, a )
      norm_x_l2 = r8vec_norm_l2 ( n, x )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, norm_a_frobenius, norm_x_l2, error_l2 
c
c  LINE_ADJ
c
      title = 'LINE_ADJ'
      m = 7
      n = m
      call line_adj ( n, a )
      call line_adj_null_right ( m, n, x )
      call r8mat_is_null_right ( m, n, a, x, error_l2 )
      norm_a_frobenius = r8mat_norm_fro ( m, n, a )
      norm_x_l2 = r8vec_norm_l2 ( n, x )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, norm_a_frobenius, norm_x_l2, error_l2 
c
c  MOLER2
c
      title = 'MOLER2'
      m = 5
      n = 5
      call moler2 ( a )
      call moler2_null_right ( x )
      call r8mat_is_null_right ( m, n, a, x, error_l2 )
      norm_a_frobenius = r8mat_norm_fro ( m, n, a )
      norm_x_l2 = r8vec_norm_l2 ( n, x )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, norm_a_frobenius, norm_x_l2, error_l2 
c
c  NEUMANN
c
      title = 'NEUMANN'
      row_num = 5
      col_num = 5
      m = row_num * col_num
      n = row_num * col_num
      call neumann ( row_num, col_num, a )
      call neumann_null_right ( row_num, col_num, x )
      call r8mat_is_null_right ( m, n, a, x, error_l2 )
      norm_a_frobenius = r8mat_norm_fro ( m, n, a )
      norm_x_l2 = r8vec_norm_l2 ( n, x )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, norm_a_frobenius, norm_x_l2, error_l2 
c
c  ONE
c
      title = 'ONE'
      m = 5
      n = 5
      call one ( m, n, a )
      call one_null_right ( m, n, x )
      call r8mat_is_null_right ( m, n, a, x, error_l2 )
      norm_a_frobenius = r8mat_norm_fro ( m, n, a )
      norm_x_l2 = r8vec_norm_l2 ( n, x )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, norm_a_frobenius, norm_x_l2, error_l2 
c
c  RING_ADJ
c  N must be a multiple of 4 for there to be a null vector.
c
      title = 'RING_ADJ'
      m = 12
      n = 12
      call ring_adj ( n, a )
      call ring_adj_null_right ( m, n, x )
      call r8mat_is_null_right ( m, n, a, x, error_l2 )
      norm_a_frobenius = r8mat_norm_fro ( m, n, a )
      norm_x_l2 = r8vec_norm_l2 ( n, x )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, norm_a_frobenius, norm_x_l2, error_l2 
c
c  ROSSER1
c
      title = 'ROSSER1'
      m = 8
      n = 8
      call rosser1 ( a )
      call rosser1_null_right ( x )
      call r8mat_is_null_right ( m, n, a, x, error_l2 )
      norm_a_frobenius = r8mat_norm_fro ( m, n, a )
      norm_x_l2 = r8vec_norm_l2 ( n, x )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, norm_a_frobenius, norm_x_l2, error_l2 
c
c  ZERO
c
      title = 'ZERO'
      m = 5
      n = 5
      call zero ( m, n, a )
      call zero_null_right ( m, n, x )
      call r8mat_is_null_right ( m, n, a, x, error_l2 )
      norm_a_frobenius = r8mat_norm_fro ( m, n, a )
      norm_x_l2 = r8vec_norm_l2 ( n, x )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, norm_a_frobenius, norm_x_l2, error_l2 

      return
      end
      subroutine test_plu ( )

c*********************************************************************72
c
cc TEST_PLU tests the PLU factors.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 April 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m_max
      parameter ( m_max = 25 )
      integer n_max
      parameter ( n_max = 25 )

      double precision a(m_max,n_max)
      double precision alpha
      double precision error_frobenius
      integer i
      integer i4_hi
      integer i4_lo
      integer i4_uniform_ab
      double precision l(m_max,n_max)
      integer m
      integer n
      double precision norm_a_frobenius
      double precision p(m_max,n_max)
      integer pivot(n_max)
      double precision r8_hi
      double precision r8_lo
      double precision r8_uniform_ab
      double precision r8mat_norm_fro
      integer seed
      character * ( 20 ) title
      double precision u(m_max,n_max)
      double precision x(n_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST_PLU'
      write ( *, '(a)' ) '  A = a test matrix of order M by N'
      write ( *, '(a)' ) '  P, L, U are the PLU factors.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  ||A|| = Frobenius norm of A.'
      write ( *, '(a)' ) '  ||A-PLU|| = Frobenius norm of A-P*L*U.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Title       	           M     N      ' 
     &  // '||A||            ||A-PLU||'
      write ( *, '(a)' ) ' '
c
c  A123
c
      title = 'A123'
      m = 3
      n = 3
      call a123 ( a )
      call a123_plu ( p, l, u )
      call r8mat_is_plu ( m, n, a, p, l, u, error_frobenius )
      norm_a_frobenius = r8mat_norm_fro ( m, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, norm_a_frobenius, error_frobenius
c
c  BODEWIG
c
      title = 'BODEWIG'
      m = 4
      n = 4
      call bodewig ( a )
      call bodewig_plu ( p, l, u )
      call r8mat_is_plu ( m, n, a, p, l, u, error_frobenius )
      norm_a_frobenius = r8mat_norm_fro ( m, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, norm_a_frobenius, error_frobenius
c
c  BORDERBAND
c
      title = 'BORDERBAND'
      m = 5
      n = 5
      call borderband ( n, a )
      call borderband_plu ( n, p, l, u )
      call r8mat_is_plu ( m, n, a, p, l, u, error_frobenius )
      norm_a_frobenius = r8mat_norm_fro ( m, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, norm_a_frobenius, error_frobenius
c
c  DIF2
c
      title = 'DIF2'
      m = 5
      n = 5
      call dif2 ( m, n, a )
      call dif2_plu ( n, p, l, u )
      call r8mat_is_plu ( m, n, a, p, l, u, error_frobenius )
      norm_a_frobenius = r8mat_norm_fro ( m, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, norm_a_frobenius, error_frobenius
c
c  GFPP
c
      title = 'GFPP'
      m = 5
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call gfpp ( n, alpha, a )
      call gfpp_plu ( n, alpha, p, l, u )
      call r8mat_is_plu ( m, n, a, p, l, u, error_frobenius )
      norm_a_frobenius = r8mat_norm_fro ( m, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, norm_a_frobenius, error_frobenius
c
c  GIVENS
c
      title = 'GIVENS'
      m = 5
      n = 5
      call givens ( n, n, a )
      call givens_plu ( n, p, l, u  )
      call r8mat_is_plu ( m, n, a, p, l, u, error_frobenius )
      norm_a_frobenius = r8mat_norm_fro ( m, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, norm_a_frobenius, error_frobenius
c
c  KMS
c
      title = 'KMS'
      m = 5
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call kms ( alpha, m, n, a )
      call kms_plu ( alpha, n, p, l, u  )
      call r8mat_is_plu ( m, n, a, p, l, u, error_frobenius )
      norm_a_frobenius = r8mat_norm_fro ( m, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, norm_a_frobenius, error_frobenius
c
c  LEHMER
c
      title = 'LEHMER'
      m = 5
      n = 5
      call lehmer ( n, n, a )
      call lehmer_plu ( n, p, l, u  )
      call r8mat_is_plu ( m, n, a, p, l, u, error_frobenius )
      norm_a_frobenius = r8mat_norm_fro ( m, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, norm_a_frobenius, error_frobenius
c
c  MAXIJ
c
      title = 'MAXIJ'
      m = 5
      n = 5
      call maxij ( n, n, a )
      call maxij_plu ( n, p, l, u  )
      call r8mat_is_plu ( m, n, a, p, l, u, error_frobenius )
      norm_a_frobenius = r8mat_norm_fro ( m, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, norm_a_frobenius, error_frobenius
c
c  MINIJ
c
      title = 'MINIJ'
      m = 5
      n = 5
      call minij ( n, n, a )
      call minij_plu ( n, p, l, u  )
      call r8mat_is_plu ( m, n, a, p, l, u, error_frobenius )
      norm_a_frobenius = r8mat_norm_fro ( m, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, norm_a_frobenius, error_frobenius
c
c  MOLER1
c
      title = 'MOLER1'
      m = 5
      n = 5
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      alpha = r8_uniform_ab ( r8_lo, r8_hi, seed )
      call moler1 ( alpha, n, n, a )
      call moler1_plu ( alpha, n, p, l, u  )
      call r8mat_is_plu ( m, n, a, p, l, u, error_frobenius )
      norm_a_frobenius = r8mat_norm_fro ( m, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, norm_a_frobenius, error_frobenius
c
c  MOLER3
c
      title = 'MOLER3'
      m = 5
      n = 5
      call moler3 ( m, n, a )
      call moler3_plu ( n, p, l, u  )
      call r8mat_is_plu ( m, n, a, p, l, u, error_frobenius )
      norm_a_frobenius = r8mat_norm_fro ( m, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, norm_a_frobenius, error_frobenius
c
c  OTO
c
      title = 'OTO'
      m = 5
      n = 5
      call oto ( m, n, a )
      call oto_plu ( n, p, l, u  )
      call r8mat_is_plu ( m, n, a, p, l, u, error_frobenius )
      norm_a_frobenius = r8mat_norm_fro ( m, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, norm_a_frobenius, error_frobenius
c
c  PASCAL2
c
      title = 'PASCAL2'
      m = 5
      n = 5
      call pascal2 ( n, a )
      call pascal2_plu ( n, p, l, u  )
      call r8mat_is_plu ( m, n, a, p, l, u, error_frobenius )
      norm_a_frobenius = r8mat_norm_fro ( m, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, norm_a_frobenius, error_frobenius
c
c  PLU
c
      title = 'PLU'
      n = 5
      seed = 123456789
      do i = 1, n
        i4_lo = i
        i4_hi = n
        pivot(i) = i4_uniform_ab ( i4_lo, i4_hi, seed )
      end do
      call plu ( n, pivot, a )
      call plu_plu ( n, pivot, p, l, u  )
      call r8mat_is_plu ( m, n, a, p, l, u, error_frobenius )
      norm_a_frobenius = r8mat_norm_fro ( m, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, norm_a_frobenius, error_frobenius
c
c  VAND2
c
      title = 'VAND2'
      m = 4
      n = 4
      r8_lo = -5.0D+00
      r8_hi = +5.0D+00
      seed = 123456789
      call r8vec_uniform_ab ( m, r8_lo, r8_hi, seed, x )
      call vand2 ( m, x, a )
      call vand2_plu ( m, x, p, l, u  )
      call r8mat_is_plu ( m, n, a, p, l, u, error_frobenius )
      norm_a_frobenius = r8mat_norm_fro ( m, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, norm_a_frobenius, error_frobenius
c
c  WILSON
c
      title = 'WILSON'
      m = 4
      n = 4
      call wilson ( a )
      call wilson_plu ( p, l, u  )
      call r8mat_is_plu ( m, n, a, p, l, u, error_frobenius )
      norm_a_frobenius = r8mat_norm_fro ( m, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, norm_a_frobenius, error_frobenius

      return
      end
      subroutine test_solution ( )

c*********************************************************************72
c
cc TEST_SOLUTION tests the linear solution computations.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 20 )

      double precision a(n_max,n_max)
      double precision alpha
      double precision b(n_max,n_max)
      double precision beta
      double precision error_frobenius
      double precision gam
      integer i1
      integer i4_hi
      integer i4_lo
      integer i4_uniform_ab
      integer k
      integer m
      integer n
      integer ncol
      double precision norm_frobenius
      integer nrow
      double precision r8_hi
      double precision r8_lo
      double precision r8_uniform_ab
      double precision r8mat_norm_fro
      integer seed
      character*20 title
      double precision x(n_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST_SOLUTION'
      write ( *, '(a)' ) 
     &  '  Compute the Frobenius norm of the solution error:'
      write ( *, '(a)' ) '    A * X - B'
      write ( *, '(a)' ) 
     &  '  given MxN matrix A, NxK solution X, MxK right hand side B.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Title                    M     N' // 
     &  '     K      ||A||         ||A*X-B||'
      write ( *, '(a)' ) ' '
c
c  A123
c
      title = 'A123'
      m = 3
      n = 3
      k = 1
      call a123 ( a )
      call a123_rhs ( b )
      call a123_solution ( x )
      call r8mat_is_solution ( m, n, k, a, x, b, error_frobenius )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, k, norm_frobenius, error_frobenius
c
c  BODEWIG
c
      title = 'BODEWIG'
      m = 4
      n = 4
      k = 1
      call bodewig ( a )
      call bodewig_rhs ( b )
      call bodewig_solution ( x )
      call r8mat_is_solution ( m, n, k, a, x, b, error_frobenius )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, k, norm_frobenius, error_frobenius
c
c  DIF2
c
      title = 'DIF2'
      m = 10
      n = 10
      k = 2
      call dif2 ( m, n, a )
      call dif2_rhs ( m, k, b )
      call dif2_solution ( n, k, x )
      call r8mat_is_solution ( m, n, k, a, x, b, error_frobenius )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, k, norm_frobenius, error_frobenius
c
c  FRANK
c
      title = 'FRANK'
      m = 10
      n = 10
      k = 2
      call frank ( n, a )
      call frank_rhs ( m, k, b )
      call frank_solution ( n, k, x )
      call r8mat_is_solution ( m, n, k, a, x, b, error_frobenius )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, k, norm_frobenius, error_frobenius
c
c  POISSON
c
      title = 'POISSON'
      nrow = 4
      ncol = 5
      m = nrow * ncol
      n = nrow * ncol
      k = 1
      call poisson ( nrow, ncol, a )
      call poisson_rhs ( nrow, ncol, b )
      call poisson_solution ( nrow, ncol, x )
      call r8mat_is_solution ( m, n, k, a, x, b, error_frobenius )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, k, norm_frobenius, error_frobenius
c
c  WILK03
c
      title = 'WILK03'
      m = 3
      n = 3
      k = 1
      call wilk03 ( a )
      call wilk03_rhs ( b )
      call wilk03_solution ( x )
      call r8mat_is_solution ( m, n, k, a, x, b, error_frobenius )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, k, norm_frobenius, error_frobenius
c
c  WILK04
c
      title = 'WILK04'
      m = 4
      n = 4
      k = 1
      call wilk04 ( a )
      call wilk04_rhs ( b )
      call wilk04_solution ( x )
      call r8mat_is_solution ( m, n, k, a, x, b, error_frobenius )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, k, norm_frobenius, error_frobenius
c
c  WILSON
c
      title = 'WILSON'
      m = 4
      n = 4
      k = 1
      call wilson ( a )
      call wilson_rhs ( b )
      call wilson_solution ( x )
      call r8mat_is_solution ( m, n, k, a, x, b, error_frobenius )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, k, norm_frobenius, error_frobenius

      return
      end
      subroutine test_type ( )

c*********************************************************************72
c
cc TEST_TYPE tests functions which test the type of a matrix.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 101 )

      double precision a(n_max,n_max)
      double precision error_frobenius
      integer key
      integer m
      integer n
      double precision norm_frobenius
      double precision r8mat_norm_fro
      integer seed
      character * ( 20 ) title

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST_TYPE'
      write ( *, '(a)' ) 
     &  '  Demonstrate functions which test the type of a matrix.'
c
c  TRANSITION
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Title                  M     N     ||A||'
     &  // '            ||Transition Error||'
      write ( *, '(a)' ) ''

      title = 'BODEWIG'
      m = 4
      n = 4
      call bodewig ( a )
      call r8mat_is_transition ( m, n, a, error_frobenius )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, norm_frobenius, error_frobenius

      title = 'SNAKES'
      m = 101
      n = 101
      call snakes ( a )
      call r8mat_is_transition ( m, n, a, error_frobenius )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, norm_frobenius, error_frobenius

      title = 'TRANSITION_RANDOM';
      m = 5
      n = 5
      key = 123456789
      call transition_random ( n, key, a )
      call r8mat_is_transition ( m, n, a, error_frobenius )
      norm_frobenius = r8mat_norm_fro ( n, n, a )
      write ( *, '(2x,a20,2x,i4,2x,i4,2x,g14.6,2x,g14.6)' ) 
     &  title, m, n, norm_frobenius, error_frobenius

      return
      end
