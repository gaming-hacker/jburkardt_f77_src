      subroutine nwspgr ( rule, rule_order, dim, k, r_size, s_size, 
     &  nodes, weights )

c*********************************************************************72
c
cc NWSPGR generates nodes and weights for sparse grid integration.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 December 2012
c
c  Author:
c
c    Original MATLAB version by Florian Heiss, Viktor Winschel.
c    FORTRAN90 version by John Burkardt.
c
c  Reference:
c
c    Florian Heiss, Viktor Winschel,
c    Likelihood approximation by numerical integration on sparse grids,
c    Journal of Econometrics,
c    Volume 144, 2008, pages 62-80.
c
c  Parameters:
c 
c    Input, external RULE ( n, x, w ), the name of a subroutine which
c    is given the order N and returns the points X and weights W of the
c    corresponding 1D quadrature rule.
c
c    Input, external RULE_ORDER ( l, n ), the name of a subroutine which
c    is given the level L and returns the order N of the corresponding 1D rule.
c
c    Input, integer DIM, the spatial dimension.
c
c    Input, integer K, the level of the sparse rule.
c
c    Input, integer R_SIZE, the "size" of the sparse rule.
c
c    Output, double precision NODES(DIM,R_SIZE), the nodes of the sparse rule.
c
c    Output, double precision WEIGHTS(R_SIZE), the weights of the sparse rule.
c
      implicit none

      integer dim
      integer r_size

      integer bq
      integer i
      integer i4_choose
      integer i4_mop
      integer, allocatable :: is(:,:)
      integer j
      integer k
      integer, allocatable :: keep(:)
      integer level
      integer lr(dim)
      integer maxq
      integer minq
      integer n
      integer n1d(k)
      integer n1d_total
      integer nc
      double precision nodes(dim,r_size)
      integer np
      integer nr(dim)
      integer q
      integer r
      integer roff(dim+1)
      integer, allocatable :: rq(:)
      integer s_size
      integer seq_num
      double precision t
      double precision, allocatable :: w(:)
      double precision, allocatable :: w1d(:)
      integer w1d_off(k+1)
      double precision, allocatable :: wc(:)
      double precision weights(r_size)
      double precision, allocatable :: wp(:)
      double precision, allocatable :: wr(:)
      double precision, allocatable :: x(:)
      double precision, allocatable :: x1d(:)
      integer x1d_off(k+1)
      double precision, allocatable :: xc(:)
      double precision, allocatable :: xp(:,:)
      double precision, allocatable :: xr(:)

      nodes(1:dim,1:r_size) = 0.0D+00
      weights(1:r_size) = 0.0D+00
c
c  Create cell arrays that will contain the points and weights 
c  for levels 1 through K.
c
      x1d_off(1) = 0
      w1d_off(1) = 0

      do level = 1, k

        call rule_order ( level, n )
        n1d(level) = n
        x1d_off(level+1) = x1d_off(level) + n
        w1d_off(level+1) = w1d_off(level) + n

      end do

      n1d_total = x1d_off(k+1)

      allocate ( x1d(n1d_total) )
      allocate ( w1d(n1d_total) )

      do level = 1, k

        n = n1d(level)

        allocate ( x(1:n) )
        allocate ( w(1:n) )

        call rule ( n, x, w )
        call r8cvv_rset ( n1d_total, x1d, k, x1d_off, level, x )
        call r8cvv_rset ( n1d_total, w1d, k, w1d_off, level, w )

        deallocate ( x )
        deallocate ( w )

      end do
c
c  Initialization.
c
      minq = max ( 0, k - dim )
      maxq = k - 1
c
c  The outer loop is over Q.
c
      r = 0

      do q = minq, maxq
c
c  BQ is the combinatorial coefficient applied to the component
c  product rules which have level Q.
c
        bq = i4_mop ( maxq - q ) * i4_choose ( dim - 1, dim + q - k )
c
c  Compute the D-dimensional row vectors that sum to DIM+Q.
c
        call num_seq ( q, dim, seq_num )

        allocate ( is(1:seq_num,1:dim) )

        call get_seq ( dim, q + dim, seq_num, is )
c
c  Allocate new rows for nodes and weights.
c
        allocate ( rq(1:seq_num) )

        do j = 1, seq_num
          rq(j) = product ( n1d(is(j,1:dim)) )
        end do
c
c  Generate each of the product rules indicated by IS, and
c  insert them into NODES and WEIGHTS.
c
        do j = 1, seq_num

          lr(1:dim) = is(j,1:dim)

          do i = 1, dim
            call rule_order ( lr(i), nr(i) )
          end do

          call r8cvv_offset ( dim, nr, roff )

          nc = i4vec_sum ( dim, nr )
          allocate ( wc(nc) )
          allocate ( xc(nc) )

          do i = 1, dim
            allocate ( wr(1:nr(i)) )
            allocate ( xr(1:nr(i)) ) 
            call rule ( nr(i), xr, wr )
            call r8cvv_rset ( nc, xc, dim, roff, i, xr )
            call r8cvv_rset ( nc, wc, dim, roff, i, wr )
            deallocate ( wr )
            deallocate ( xr )
          end do

          np = rq(j)
          allocate ( wp(1:np) )
          allocate ( xp(1:dim,1:np) )

          call tensor_product_cell ( nc, xc, wc, dim, nr, roff, np, xp, wp )
c
c  Append the new nodes and weights to the arrays.
c
          nodes(1:dim,r+1:r+np) = xp(1:dim,1:np)
          weights(r+1:r+np) = bq * wp(1:np)
          deallocate ( wc )
          deallocate ( wp )
          deallocate ( xc )
          deallocate ( xp )
c
c  Increase R.
c
          r = r + rq(j)
        end do

        deallocate ( is )
        deallocate ( rq )

      end do
c
c  Reorder the rule so the points are in ascending lexicographic order.
c
      call rule_sort ( dim, r_size, nodes, weights )
c
c  Now suppress duplicate points and merge weights.
c
      r = 1
      do j = 2, r_size
        if ( all ( nodes(1:dim,r) .eq. nodes(1:dim,j) ) ) then
          weights(r) = weights(r) + weights(j)
        else
          r = r + 1
          weights(r) = weights(j)
          nodes(1:dim,r) = nodes(1:dim,j)
        end if
      end do

      s_size = r
c
c  Zero out unneeded entries.
c
      nodes(1:dim,s_size+1:r_size) = 0.0D+00
      weights(s_size+1:r_size) = 0.0D+00
c  
c  Normalize the weights to sum to 1.
c
      t = 0.0D+00
      do i = 1, r
        t = t + weights(i)
      end do

      do i = 1, r
        weights(i) = weights(i) / t
      end do

      return
      end

