      subroutine ccs_sparse_test ( )

c*********************************************************************72
c
cc CCS_SPARSE_TEST uses the CCS function to build a sparse grid.
c
c  Discussion:
c
c    We use CCS_ORDER to provide orders corresponding to slow growth.
c    We can still use the CCU routine to compute the corresponding rule.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 December 2012
c
c  Author:
c
c    John Burkardt
c
c  Local parameters:
c
c    Local, integer D, the spatial dimension.
c
c    Local, integer MAXK, the maximum level to check.
c
      implicit none

      external ccu
      external ccs_order
      integer d
      double precision error_mc
      double precision error_sg
      double precision estimate
      double precision fu_integral
      double precision, allocatable :: fx(:)
      integer k
      integer maxk
      integer n
      integer n2
      integer r
      double precision r8vec_dot_product
      double precision, allocatable :: s(:)
      integer seed
      double precision trueval
      double precision, allocatable :: w(:)
      double precision, allocatable :: x(:,:)

      d = 10
      maxk = 7

      trueval = fu_integral ( d );

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'CCS_SPARSE_TEST:'
      write ( *, '(a)' ) '  CCS sparse grid:'
      write ( *, '(a)' ) '  Sparse Gaussian unweighted quadrature over [0,1].'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '   D  Level   Nodes    SG error    MC error'
      write ( *, '(a)' ) ''

      do k = 2, maxk
c
c  Compute sparse grid estimate.
c
        call nwspgr_size ( ccs_order, d, k, n )
        allocate ( x(1:d,1:n) )
        allocate ( w(1:n) )
        call nwspgr ( ccu, ccs_order, d, k, n, n2, x, w )
        allocate ( fx(1:n2) )
        call fu_value ( d, n2, x, fx )
        estimate = r8vec_dot_product ( n2, w, fx )

        error_sg = abs ( ( estimate - trueval ) / trueval )
c
c  Compute 1000 Monte Carlo estimates with same number of points, and average.
c
        allocate ( s(1000) )
        seed = 123456789

        do r = 1, 1000
          call r8mat_uniform_01 ( d, n2, seed, x )
          call fu_value ( d, n2, x, fx )
          s(r) = r8vec_sum ( n2, fx ) / dble ( n2 )
        end do
        error_mc = sqrt ( sum ( ( s(1:1000) - trueval ) ** 2 ) / 1000.0D+00 ) / trueval

        write ( *, '(2x,i2,5x,i2,2x,i6,2x,g10.5,2x,g10.5)' ) d, k, n2, error_sg, error_mc

        deallocate ( fx )
        deallocate ( s )
        deallocate ( w )
        deallocate ( x )

      end do

      return
      end
      subroutine ccu_sparse_test ( )

c*********************************************************************72
c
cc CCU_SPARSE_TEST uses the CCU function to build a sparse grid.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 December 2012
c
c  Author:
c
c    John Burkardt
c
c  Local parameters:
c
c    Local, integer D, the spatial dimension.
c
c    Local, integer MAXK, the maximum level to check.
c
      implicit none

      external ccu
      external ccu_order
      integer d
      double precision error_mc
      double precision error_sg
      double precision estimate
      double precision fu_integral
      double precision, allocatable :: fx(:)
      integer k
      integer maxk
      integer n
      integer n2
      integer r
      double precision r8vec_dot_product
      double precision, allocatable :: s(:)
      integer seed
      double precision trueval
      double precision, allocatable :: w(:)
      double precision, allocatable :: x(:,:)

      d = 10
      maxk = 7

      trueval = fu_integral ( d );

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'CCU_SPARSE_TEST:'
      write ( *, '(a)' ) '  CCU sparse grid:'
      write ( *, '(a)' ) '  Sparse Gaussian unweighted quadrature over [0,1].'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '   D  Level   Nodes    SG error    MC error'
      write ( *, '(a)' ) ''

      do k = 2, maxk
c
c  Compute sparse grid estimate.
c
        call nwspgr_size ( ccu_order, d, k, n )
        allocate ( x(1:d,1:n) )
        allocate ( w(1:n) )
        call nwspgr ( ccu, ccu_order, d, k, n, n2, x, w )
        allocate ( fx(1:n2) )
        call fu_value ( d, n2, x, fx )
        estimate = r8vec_dot_product ( n2, w, fx )

        error_sg = abs ( ( estimate - trueval ) / trueval )
c
c  Compute 1000 Monte Carlo estimates with same number of points, and average.
c
        allocate ( s(1000) )
        seed = 123456789

        do r = 1, 1000
          call r8mat_uniform_01 ( d, n2, seed, x )
          call fu_value ( d, n2, x, fx )
          s(r) = r8vec_sum ( n2, fx ) / dble ( n2 )
        end do
        error_mc = sqrt ( sum ( ( s(1:1000) - trueval ) ** 2 ) / 1000.0D+00 ) / trueval

        write ( *, '(2x,i2,5x,i2,2x,i6,2x,g10.5,2x,g10.5)' ) d, k, n2, error_sg, error_mc

        deallocate ( fx )
        deallocate ( s )
        deallocate ( w )
        deallocate ( x )

      end do

      return
      end

      subroutine gqn_sparse_test ( )

c*********************************************************************72
c
cc GQN_SPARSE_TEST uses the GQN function to build a sparse grid.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 December 2012
c
c  Author:
c
c    John Burkardt
c
c  Local parameters:
c
c    Local, integer D, the spatial dimension.
c
c    Local, integer MAXK, the maximum level to check.
c
      implicit none

      integer d
      double precision error_mc
      double precision error_sg
      double precision estimate
      double precision fn_integral
      double precision, allocatable :: fx(:)
      external gqn
      external gqn_order
      integer k
      integer maxk
      integer n
      integer n2
      integer r
      double precision r8vec_dot_product
      double precision, allocatable :: s(:)
      integer seed
      double precision trueval
      double precision, allocatable :: w(:)
      double precision, allocatable :: x(:,:)

      d = 10
      maxk = 7

      trueval = fn_integral ( d );

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'GQN_SPARSE_TEST:'
      write ( *, '(a)' ) '  GQN sparse grid:'
      write ( *, '(a)' ) '  Sparse Gaussian quadrature with Hermite weight over (-oo,+oo).'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '   D  Level   Nodes    SG error    MC error'
      write ( *, '(a)' ) ''

      do k = 2, maxk
c
c  Compute sparse grid estimate.
c
        call nwspgr_size ( gqn_order, d, k, n )
        allocate ( x(1:d,1:n) )
        allocate ( w(1:n) )
        call nwspgr ( gqn, gqn_order, d, k, n, n2, x, w )
        allocate ( fx(1:n2) )
        call fn_value ( d, n2, x, fx )
        estimate = r8vec_dot_product ( n2, w, fx )

        error_sg = abs ( ( estimate - trueval ) / trueval )
c
c  Compute 1000 Monte Carlo estimates with same number of points, and average.
c
        allocate ( s(1000) )
        seed = 123456789

        do r = 1, 1000
          call r8mat_normal_01 ( d, n2, seed, x )
          call fn_value ( d, n2, x, fx )
          s(r) = r8vec_sum ( n2, fx ) / dble ( n2 )
        end do
        error_mc = sqrt ( sum ( ( s(1:1000) - trueval ) ** 2 ) / 1000.0D+00 ) / trueval

        write ( *, '(2x,i2,5x,i2,2x,i6,2x,g10.5,2x,g10.5)' ) d, k, n2, error_sg, error_mc

        deallocate ( fx )
        deallocate ( s )
        deallocate ( w )
        deallocate ( x )

      end do

      return
      end
      subroutine gqu_sparse_test ( )

c*********************************************************************72
c
cc GQU_SPARSE_TEST uses the GQU function to build a sparse grid.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 December 2012
c
c  Author:
c
c    John Burkardt
c
c  Local parameters:
c
c    Local, integer D, the spatial dimension.
c
c    Local, integer MAXK, the maximum level to check.
c
      implicit none

      integer d
      double precision error_mc
      double precision error_sg
      double precision estimate
      double precision fu_integral
      double precision, allocatable :: fx(:)
      external gqu
      external gqu_order
      integer k
      integer maxk
      integer n
      integer n2
      integer r
      double precision r8vec_dot_product
      double precision, allocatable :: s(:)
      integer seed
      double precision trueval
      double precision, allocatable :: w(:)
      double precision, allocatable :: x(:,:)

      d = 10
      maxk = 7

      trueval = fu_integral ( d );

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'GQU_SPARSE_TEST:'
      write ( *, '(a)' ) '  GQU sparse grid:'
      write ( *, '(a)' ) '  Sparse Gauss-Legendre quadrature with unit weight over [0,1].'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '   D  Level   Nodes    SG error    MC error'
      write ( *, '(a)' ) ''

      do k = 2, maxk
c
c  Compute sparse grid estimate.
c
        call nwspgr_size ( gqu_order, d, k, n )
        allocate ( x(1:d,1:n) )
        allocate ( w(1:n) )
        call nwspgr ( gqu, gqu_order, d, k, n, n2, x, w )
        allocate ( fx(1:n2) )
        call fu_value ( d, n2, x, fx )
        estimate = r8vec_dot_product ( n2, w, fx )

        error_sg = abs ( ( estimate - trueval ) / trueval )
c
c  Compute 1000 Monte Carlo estimates with same number of points, and average.
c
        allocate ( s(1000) )
        seed = 123456789

        do r = 1, 1000
          call r8mat_uniform_01 ( d, n2, seed, x )
          call fu_value ( d, n2, x, fx )
          s(r) = r8vec_sum ( n2, fx ) / dble ( n2 )
        end do
        error_mc = sqrt ( sum ( ( s(1:1000) - trueval ) ** 2 ) / 1000.0D+00 ) / trueval

        write ( *, '(2x,i2,5x,i2,2x,i6,2x,g10.5,2x,g10.5)' ) d, k, n2, error_sg, error_mc

        deallocate ( fx )
        deallocate ( s )
        deallocate ( w )
        deallocate ( x )

      end do

      return
      end
      subroutine kpn_sparse_test ( )

c*********************************************************************72
c
cc KPN_SPARSE_TEST uses the KPN function to build a sparse grid.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 December 2012
c
c  Author:
c
c    John Burkardt
c
c  Local parameters:
c
c    Local, integer D, the spatial dimension.
c
c    Local, integer MAXK, the maximum level to check.
c
      implicit none

      integer d
      double precision error_mc
      double precision error_sg
      double precision estimate
      double precision fn_integral
      double precision, allocatable :: fx(:)
      integer k
      external kpn
      external kpn_order
      integer maxk
      integer n
      integer n2
      integer r
      double precision r8vec_dot_product
      double precision, allocatable :: s(:)
      integer seed
      double precision trueval
      double precision, allocatable :: w(:)
      double precision, allocatable :: x(:,:)

      d = 10
      maxk = 7

      trueval = fn_integral ( d );

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'KPN_SPARSE_TEST:'
      write ( *, '(a)' ) '  KPN sparse grid:'
      write ( *, '(a)' ) '  Sparse Kronrod quadrature with Hermite weight over (-oo,+oo).'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '   D  Level   Nodes    SG error    MC error'
      write ( *, '(a)' ) ''

      do k = 2, maxk
c
c  Compute sparse grid estimate.
c
        call nwspgr_size ( kpn_order, d, k, n )
        allocate ( x(1:d,1:n) )
        allocate ( w(1:n) )
        call nwspgr ( kpn, kpn_order, d, k, n, n2, x, w )
        allocate ( fx(1:n2) )
        call fn_value ( d, n2, x, fx )
        estimate = r8vec_dot_product ( n2, w, fx )

        error_sg = abs ( ( estimate - trueval ) / trueval )
c
c  Compute 1000 Monte Carlo estimates with same number of points, and average.
c
        allocate ( s(1000) )
        seed = 123456789

        do r = 1, 1000
          call r8mat_normal_01 ( d, n2, seed, x )
          call fn_value ( d, n2, x, fx )
          s(r) = r8vec_sum ( n2, fx ) / dble ( n2 )
        end do
        error_mc = sqrt ( sum ( ( s(1:1000) - trueval ) ** 2 ) / 1000.0D+00 ) / trueval

        write ( *, '(2x,i2,5x,i2,2x,i6,2x,g10.5,2x,g10.5)' ) d, k, n2, error_sg, error_mc

        deallocate ( fx )
        deallocate ( s )
        deallocate ( w )
        deallocate ( x )

      end do

      return
      end
      subroutine kpu_sparse_test ( )

c*********************************************************************72
c
cc KPU_SPARSE_TEST uses the KPU function to build a sparse grid.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 December 2012
c
c  Author:
c
c    John Burkardt
c
c  Local parameters:
c
c    Local, integer D, the spatial dimension.
c
c    Local, integer MAXK, the maximum level to check.
c
      implicit none

      integer d
      double precision error_mc
      double precision error_sg
      double precision estimate
      double precision fu_integral
      double precision, allocatable :: fx(:)
      integer k
      external kpu
      external kpu_order
      integer maxk
      integer n
      integer n2
      integer r
      double precision r8vec_dot_product
      double precision, allocatable :: s(:)
      integer seed
      double precision trueval
      double precision, allocatable :: w(:)
      double precision, allocatable :: x(:,:)

      d = 10
      maxk = 7

      trueval = fu_integral ( d );

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'KPU_SPARSE_TEST:'
      write ( *, '(a)' ) '  KPU sparse grid:'
      write ( *, '(a)' ) '  Sparse Kronrod quadrature with unit weight over [0,1].'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '   D  Level   Nodes    SG error    MC error'
      write ( *, '(a)' ) ''

      do k = 2, maxk
c
c  Compute sparse grid estimate.
c
        call nwspgr_size ( kpu_order, d, k, n )
        allocate ( x(1:d,1:n) )
        allocate ( w(1:n) )
        call nwspgr ( kpu, kpu_order, d, k, n, n2, x, w )
        allocate ( fx(1:n2) )
        call fu_value ( d, n2, x, fx )
        estimate = r8vec_dot_product ( n2, w, fx )

        error_sg = abs ( ( estimate - trueval ) / trueval )
c
c  Compute 1000 Monte Carlo estimates with same number of points, and average.
c
        allocate ( s(1000) )
        seed = 123456789

        do r = 1, 1000
          call r8mat_uniform_01 ( d, n2, seed, x )
          call fu_value ( d, n2, x, fx )
          s(r) = r8vec_sum ( n2, fx ) / dble ( n2 )
        end do
        error_mc = sqrt ( sum ( ( s(1:1000) - trueval ) ** 2 ) / 1000.0D+00 ) / trueval

        write ( *, '(2x,i2,5x,i2,2x,i6,2x,g10.5,2x,g10.5)' ) d, k, n2, error_sg, error_mc

        deallocate ( fx )
        deallocate ( s )
        deallocate ( w )
        deallocate ( x )

      end do

      return
      end

      subroutine nwspgr_test ( )

c*********************************************************************72
c
cc NWSPGR_TEST tests NWSPGR.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 December 2012
c
c  Author:
c
c    John Burkardt.
c
      implicit none

      external ccu
      external ccu_order
      integer dim
      external gqn
      external gqn_order
      external gqu
      external gqu_order
      integer k
      external kpn
      external kpn_order
      external kpu
      external kpu_order
      double precision, allocatable :: nodes(:,:)
      integer r_size
      integer s_size
      integer seq_max
      double precision, allocatable :: weights(:)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'NWSPGR_TEST:'
      write ( *, '(a)' ) '  NWSPGR generates a sparse grid, based on either:'
      write ( *, '(a)' ) '  one of the built-in 1D rules, or a family of 1D rules'
      write ( *, '(a)' ) '  supplied by the user.'

      dim = 2
      k = 3
      call num_seq ( k - 1, dim, seq_max )
      call nwspgr_size ( kpu_order, dim, k, seq_max, r_size )
      allocate ( nodes(dim,r_size) )
      allocate ( weights(r_size) )
      call nwspgr ( kpu, kpu_order, dim, k, r_size, s_size, nodes, weights )
      call quad_rule_print ( dim, s_size, nodes, weights, '  Kronrod-Patterson, [0,1], Dim 2, Level 3' )
      deallocate ( nodes )
      deallocate ( weights )

      dim = 2
      k = 3
      call nwspgr_size ( kpn_order, dim, k, seq_max, r_size )
      allocate ( nodes(dim,r_size) )
      allocate ( weights(r_size) )
      call nwspgr ( kpn, kpn_order, dim, k, r_size, s_size, nodes, weights )
      call quad_rule_print ( dim, s_size, nodes, weights, '  Kronrod-Patterson, (-oo,+oo), Dim 2, Level 3' )
      deallocate ( nodes )
      deallocate ( weights )

      dim = 2
      k = 3
      call nwspgr_size ( gqu_order, dim, k, seq_max, r_size )
      allocate ( nodes(dim,r_size) )
      allocate ( weights(r_size) )
      call nwspgr ( gqu, gqu_order, dim, k, r_size, s_size, nodes, weights )
      call quad_rule_print ( dim, s_size, nodes, weights, '  Gauss-Legendre, [0,1], Dim 2, Level 3' )
      deallocate ( nodes )
      deallocate ( weights )

      dim = 2
      k = 3
      call nwspgr_size ( gqn_order, dim, k, seq_max, r_size )
      allocate ( nodes(dim,r_size) )
      allocate ( weights(r_size) )
      call nwspgr ( gqn, gqn_order, dim, k, r_size, s_size, nodes, weights )
      call quad_rule_print ( dim, s_size, nodes, weights, '  Gauss Hermite, (-oo,+oo), Dim 2, Level 3' )
      deallocate ( nodes )
      deallocate ( weights )

      dim = 2
      k = 3
      call nwspgr_size ( ccu_order, dim, k, seq_max, r_size )
      allocate ( nodes(dim,r_size) )
      allocate ( weights(r_size) )
      call nwspgr ( ccu, ccu_order, dim, k, r_size, s_size, nodes, weights )
      call quad_rule_print ( dim, s_size, nodes, weights, '  Clenshaw Curtis, [-1,+1], Dim 2, Level 3' )
      deallocate ( nodes )
      deallocate ( weights )

      return
      end
      subroutine tensor_product_test ( )

c*********************************************************************72
c
cc TENSOR_PRODUCT_TEST tests TENSOR_PRODUCT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 May 2012
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer d_max
      parameter ( d_max = 3 )
      integer n1d_max
      parameter ( n1d_max = 7 )
      integer order1
      parameter ( order1 = 2 )
      integer order2
      parameter ( order2 = 3 )
      integer order3 
      parameter ( order3 = 2 )

      integer d
      integer i1
      integer i2
      integer i4vec_product
      integer i4vec_sum
      integer n
      integer n1d
      integer order1d(d_max)
      double precision w1d(n1d_max)
      double precision, allocatable :: wnd(:)
      double precision w1_1d(order1)
      double precision w2_1d(order2)
      double precision w3_1d(order3)
      double precision x1_1d(order1)
      double precision x2_1d(order2)
      double precision x3_1d(order3)
      double precision x1d(n1d_max)
      double precision, allocatable :: xnd(:,:)

      data w1_1d / 1.0D+00, 1.0D+00 /
      data w2_1d / 0.25D+00, 0.50D+00, 0.25D+00 /
      data w3_1d / 2.50D+00, 2.50D+00 /
      data x1_1d / -1.0D+00, +1.0D+00 /
      data x2_1d / 2.0D+00, 2.5D+00, 3.0D+00 /
      data x3_1d( / 10.0D+00, 15.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TENSOR_PRODUCT_TEST:'
      write ( *, '(a)' ) '  Given a sequence of 1D quadrature rules, construct the'
      write ( *, '(a)' ) '  tensor product rule.'
c
c  1D rule.
c
      d = 1

      order1d(1) = order1

      n1d = i4vec_sum ( d, order1d )

      n = i4vec_product ( d, order1d )
      allocate ( xnd(1:d,1:n) )
      allocate ( wnd(1:n) )

      i1 = 1
      i2 = order1
      x1d(i1:i2) = x1_1d(1:order1)
      w1d(i1:i2) = w1_1d(1:order1)
     
      call tensor_product ( d, order1d,n1d, x1d, w1d, n, xnd, wnd )
      
      call quad_rule_print ( d, n, xnd, wnd, 
     &  '  A 1D rule over [-1,+1]:' )

      deallocate ( wnd )
      deallocate ( xnd )
c
c  2D rule.
c
      d = 2

      order1d(1) = order1
      order1d(2) = order2

      n1d = i4vec_sum ( d, order1d )

      n = i4vec_product ( d, order1d )
      allocate ( xnd(1:d,1:n) )
      allocate ( wnd(1:n) )

      i1 = 1
      i2 = order1
      x1d(i1:i2) = x1_1d(1:order1)
      w1d(i1:i2) = w1_1d(1:order1)
      i1 = i2 + 1
      i2 = i2 + order2
      x1d(i1:i2) = x2_1d(1:order2)
      w1d(i1:i2) = w2_1d(1:order2)

      call tensor_product ( d, order1d, n1d, x1d, w1d, n, xnd, wnd )
      
      call quad_rule_print ( d, n, xnd, wnd, 
     &  '  A 2D rule over [-1,+1] x [2.0,3.0]:' )

      deallocate ( wnd )
      deallocate ( xnd )
c
c  3D rule.
c
      d = 3

      order1d(1) = order1
      order1d(2) = order2
      order1d(3) = order3

      n1d = i4vec_sum ( d, order1d )

      n = i4vec_product ( d, order1d )
      allocate ( xnd(1:d,1:n) )
      allocate ( wnd(1:n) )

      i1 = 1
      i2 = order1
      x1d(i1:i2) = x1_1d(1:order1)
      w1d(i1:i2) = w1_1d(1:order1)
      i1 = i2 + 1
      i2 = i2 + order2
      x1d(i1:i2) = x2_1d(1:order2)
      w1d(i1:i2) = w2_1d(1:order2)
      i1 = i2 + 1
      i2 = i2 + order3
      x1d(i1:i2) = x3_1d(1:order3)
      w1d(i1:i2) = w3_1d(1:order3)

      call tensor_product ( d, order1d, n1d, x1d, w1d, n, xnd, wnd )

      call quad_rule_print ( d, n, xnd, wnd, 
     &  '  A 3D rule over [-1,+1] x [2.0,3.0] x [10.0,15.0]:' )

      deallocate ( wnd )
      deallocate ( xnd )

      return
      end
      subroutine tensor_product_cell_test ( )

c*********************************************************************72
c
cc TENSOR_PRODUCT_CELL_TEST tests TENSOR_PRODUCT_CELL.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 December 2012
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer order1
      parameter ( order1 = 2 )
      integer order2
      parameter ( order2 = 3 )
      integer order3 
      parameter ( order3 = 2 )

      integer d
      integer i1
      integer i2
      integer i4vec_product
      integer i4vec_sum
      integer n1d
      integer nc
      integer np
      integer :: nr(3) = (/ 2, 3, 2 /)
      integer, allocatable :: order1d(:)
      integer roff(4)
      double precision, allocatable :: w1d(:)
      double precision, allocatable :: wc(:)
      double precision, allocatable :: wp(:)
      double precision :: w1_1d(order1) = (/ 1.0D+00, 1.0D+00 /)
      double precision :: w2_1d(order2) = (/ 0.25D+00, 0.50D+00, 0.25D+00 /)
      double precision :: w3_1d(order3) = (/ 2.50D+00, 2.50D+00 /)
      double precision :: x1_1d(order1) = (/ -1.0D+00, +1.0D+00 /)
      double precision :: x2_1d(order2) = (/ 2.0D+00, 2.5D+00, 3.0D+00 /)
      double precision :: x3_1d(order3) = (/ 10.0D+00, 15.0D+00 /)
      double precision, allocatable :: x1d(:)
      double precision, allocatable :: xc(:)
      double precision, allocatable :: xp(:,:)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TENSOR_PRODUCT_TEST_CELL:'
      write ( *, '(a)' ) '  Given a set of 1D quadrature rules stored in a cell array,'
      write ( *, '(a)' ) '  construct the tensor product rule.'
c
c  We can construct ROFF once and for all.
c
      call r8cvv_offset ( 3, nr, roff )
c
c  1D rule.
c
      d = 1
      nc = i4vec_sum ( d, nr )
      allocate ( xc(1:nc) )
      call r8cvv_rset ( nc, xc, d, roff, 1, x1_1d )
      allocate ( wc(1:nc) )
      call r8cvv_rset ( nc, wc, d, roff, 1, w1_1d )
      np = i4vec_product ( d, nr )
      allocate( xp(1:d,1:np) )
      allocate( wp(1:np) )

      call tensor_product_cell ( nc, xc, wc, d, nr, roff, np, xp, wp )

      call quad_rule_print ( d, np, xp, wp, '  A 1D rule over [-1,+1]:' )

      deallocate ( wc )
      deallocate ( wp )
      deallocate ( xc )
      deallocate ( xp )
c
c  2D rule.
c
      d = 2
      nc = i4vec_sum ( d, nr )
      allocate ( xc(1:nc) )
      call r8cvv_rset ( nc, xc, d, roff, 1, x1_1d )
      call r8cvv_rset ( nc, xc, d, roff, 2, x2_1d )
      allocate ( wc(1:nc) )
      call r8cvv_rset ( nc, wc, d, roff, 1, w1_1d )
      call r8cvv_rset ( nc, wc, d, roff, 2, w2_1d )
      np = i4vec_product ( d, nr )
      allocate( xp(1:d,1:np) )
      allocate( wp(1:np) )

      call tensor_product_cell ( nc, xc, wc, d, nr, roff, np, xp, wp )

      call quad_rule_print ( d, np, xp, wp, '  A 1D rule over [-1,+1]:' )

      deallocate ( wc )
      deallocate ( wp )
      deallocate ( xc )
      deallocate ( xp )
c
c  3D rule.
c
      d = 3
      nc = i4vec_sum ( d, nr )
      allocate ( xc(1:nc) )
      call r8cvv_rset ( nc, xc, d, roff, 1, x1_1d )
      call r8cvv_rset ( nc, xc, d, roff, 2, x2_1d )
      call r8cvv_rset ( nc, xc, d, roff, 3, x3_1d )
      allocate ( wc(1:nc) )
      call r8cvv_rset ( nc, wc, d, roff, 1, w1_1d )
      call r8cvv_rset ( nc, wc, d, roff, 2, w2_1d )
      call r8cvv_rset ( nc, wc, d, roff, 3, w3_1d )
      np = i4vec_product ( d, nr )
      allocate( xp(1:d,1:np) )
      allocate( wp(1:np) )

      call tensor_product_cell ( nc, xc, wc, d, nr, roff, np, xp, wp )

      call quad_rule_print ( d, np, xp, wp, '  A 1D rule over [-1,+1]:' )

      deallocate ( wc )
      deallocate ( wp )
      deallocate ( xc )
      deallocate ( xp )

      return
      end

