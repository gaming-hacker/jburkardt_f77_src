      program main

c*********************************************************************72
c
cc MAIN is the main program for SPARSE_GRID_HW_PRB.
c
c  Discussion:
c
c    SPARSE_GRID_HW_PRB tests the SPARSE_GRID_HW library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 February 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SPARSE_GRID_HW_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the SPARSE_GRID_HW library.'

      call ccl_test ( )
c     call ccl_sparse_test ( )

      call ccs_test ( )
c     call ccs_sparse_test ( )

      call cce_test ( )
c     call cce_sparse_test ( )

      call get_seq_test ( )

      call gqn_test ( )
c     call gqn_sparse_test ( )
c     call gqn2_sparse_test ( )

      call gqu_test ( )
c     call gqu_sparse_test ( )

      call kpn_test ( )
c     call kpn_sparse_test ( )

      call kpu_test ( )
c     call kpu_sparse_test ( )

      call nwspgr_size_test ( )
c     call nwspgr_test ( )

      call order_report ( )

      call symmetric_sparse_size_test ( )

c     call tensor_product_test ( )
c     call tensor_product_cell_test ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SPARSE_GRID_HW_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'

      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine ccl_test ( )

c*********************************************************************72
c
cc CCL_TEST uses CCL_ORDER + CC for 1D quadrature over [0,1].
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 February 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 9 )

      integer d
      double precision e
      double precision exact
      double precision fu_integral
      double precision fx(n_max)
      integer l
      integer n
      double precision q
      double precision r8vec_dot_product
      double precision w(n_max)
      double precision x(n_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CCL_TEST:'
      write ( *, '(a)' ) '  Use CCL_ORDER + CC.'
      write ( *, '(a)' ) 
     &  '  Clenshaw Curtis Linear (CCL) quadrature over [0,1]:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   Level   Nodes    Estimate  Error'
      write ( *, '(a)' ) ' '

      d = 1
      exact = fu_integral ( d )

      do l = 1, 5

        call ccl_order ( l, n )

        call cc ( n, x, w )

        call fu_value ( d, n, x, fx )

        q = r8vec_dot_product ( n, w, fx )

        e = sqrt ( ( q - exact )**2 ) / exact

        write ( *, '(2x,i2,4x,i6,2x,g14.6,2x,g14.6)' ) l, n, q, e

      end do

      return
      end
      subroutine ccs_test ( )

c*********************************************************************72
c
cc CCS_TEST uses CCS_ORDER + CC for 1D quadrature over [0,1].
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 February 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 9 )

      integer d
      double precision e
      double precision exact
      double precision fu_integral
      double precision fx(n_max)
      integer l
      integer n
      double precision q
      double precision r8vec_dot_product
      double precision w(n_max)
      double precision x(n_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CCS_TEST:'
      write ( *, '(a)' ) '  Use CCS_ORDER + CC.'
      write ( *, '(a)' ) '  Clenshaw Curtis Slow quadrature over [0,1]:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   Level   Nodes    Estimate  Error'
      write ( *, '(a)' ) ' '

      d = 1
      exact = fu_integral ( d )

      do l = 1, 5

        call ccs_order ( l, n )

        call cc ( n, x, w )

        call fu_value ( d, n, x, fx )

        q = r8vec_dot_product ( n, w, fx )

        e = sqrt ( ( q - exact )**2 ) / exact

        write ( *, '(2x,i2,4x,i6,2x,g14.6,2x,g14.6)' ) l, n, q, e

      end do

      return
      end
      subroutine cce_test ( )

c*********************************************************************72
c
cc CCE_TEST uses CCE_ORDER + CC for 1D quadrature over [0,1].
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 February 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 17 )

      integer d
      double precision e
      double precision exact
      double precision fu_integral
      double precision fx(n_max)
      integer l
      integer n
      double precision q
      double precision r8vec_dot_product
      double precision w(n_max)
      double precision x(n_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CCE_TEST:'
      write ( *, '(a)' ) '  Use CCE_ORDER + CC.'
      write ( *, '(a)' ) '  Clenshaw Curtis Exponential 1D quadrature:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   Level   Nodes    Estimate  Error'
      write ( *, '(a)' ) ' '

      d = 1
      exact = fu_integral ( d );

      do l = 1, 5

        call cce_order ( l, n )

        call cc ( n, x, w )

        call fu_value ( d, n, x, fx )

        q = r8vec_dot_product ( n, w, fx )

        e = sqrt ( ( q - exact )**2 ) / exact

        write ( *, '(2x,i2,4x,i6,2x,g14.6,2x,g14.6)' ) l, n, q, e

      end do

      return
      end
      subroutine get_seq_test ( )

c*********************************************************************72
c
cc GET_SEQ_TEST tests GET_SEQ.
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

      integer d
      parameter ( d = 3 )

      integer fs(10,d)
      integer norm
      integer seq_num

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'GET_SEQ_TEST'
      write ( *, '(a)' ) 
     &  '  GET_SEQ returns all D-dimensional vectors that sum to NORM.'

      norm = 6

      write ( *, '(a)' ) ' '
      write ( *, '(a,i4)' ) '  D = ', d
      write ( *, '(a,i4)' ) '  NORM = ', norm

      call num_seq ( norm - d, d, seq_num )

      call get_seq ( d, norm, seq_num, fs )

      call i4mat_print ( seq_num, d, fs, '  The compositions' )

      return
      end
      subroutine gqn_test ( )

c*********************************************************************72
c
cc GQN_TEST uses the GQN function for 1D quadrature over (-oo,+oo).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 January 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 5 )

      integer d
      double precision e
      double precision exact
      double precision fn_integral
      double precision fx(n_max)
      integer i
      integer l
      integer n
      double precision q
      double precision r8vec_dot_product
      double precision w(n_max)
      double precision x(n_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'GQN_TEST:'
      write ( *, '(a)' ) '  Gauss-Hermite quadrature over (-oo,+oo):'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   Level   Nodes    Estimate  Error'
      write ( *, '(a)' ) ' '

      d = 1
      exact = fn_integral ( d );

      do l = 1, 5

        n = l

        call gqn ( n, x, w )

        call fn_value ( d, n, x, fx )

        q = r8vec_dot_product ( n, w, fx )

        e = sqrt ( ( q - exact )**2 ) / exact

        write ( *, '(2x,i2,4x,i6,2x,g14.6,2x,g14.6)' ) l, n, q, e

      end do

      return
      end
      subroutine gqu_test ( )

c*********************************************************************72
c
cc GQU_TEST uses the GQU function for 1D quadrature over [0,1].
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 January 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 5 )

      integer d
      double precision e
      double precision exact
      double precision fu_integral
      double precision fx(n_max)
      integer i
      integer l
      integer n
      double precision q
      double precision r8vec_dot_product
      double precision w(n_max)
      double precision x(n_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'GQU_TEST:'
      write ( *, '(a)' ) '  Gauss-Legendre quadrature over [0,1]:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   Level   Nodes    Estimate  Error'
      write ( *, '(a)' ) ' '

      d = 1
      exact = fu_integral ( d );

      do l = 1, 5

        n = l

        call gqu ( n, x, w )

        call fu_value ( d, n, x, fx )

        q = r8vec_dot_product ( n, w, fx )

        e = sqrt ( ( q - exact )**2 ) / exact

        write ( *, '(2x,i2,4x,i6,2x,g14.6,2x,g14.6)' ) l, n, q, e

      end do

      return
      end
      subroutine kpn_test ( )

c*********************************************************************72
c
cc KPN_TEST uses the KPN function for 1D quadrature over (-oo,+oo).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 January 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 9 )

      integer d
      double precision e
      double precision exact
      double precision fn_integral
      double precision fx(n_max)
      integer i
      integer l
      integer n
      double precision q
      double precision r8vec_dot_product
      double precision w(n_max)
      double precision x(n_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'KPN_TEST:'
      write ( *, '(a)' ) 
     &  '  Kronrod-Patterson-Hermite quadrature over (-oo,+oo):'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   Level   Nodes    Estimate  Error'
      write ( *, '(a)' ) ' '

      d = 1
      exact = fn_integral ( d );

      do l = 1, 5

        call kpn_order ( l, n )

        call kpn ( n, x, w )

        call fn_value ( d, n, x, fx )

        q = r8vec_dot_product ( n, w, fx )

        e = sqrt ( ( q - exact )**2 ) / exact

        write ( *, '(2x,i2,4x,i6,2x,g14.6,2x,g14.6)' ) l, n, q, e

      end do

      return
      end
      subroutine kpu_test ( )

c*********************************************************************72
c
cc KPU_TEST uses the KPU function for 1D quadrature over [0,1].
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 January 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 7 )

      integer d
      double precision e
      double precision exact
      double precision fu_integral
      double precision fx(n_max)
      integer i
      integer l
      integer n
      double precision q
      double precision r8vec_dot_product
      double precision w(n_max)
      double precision x(n_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'KPU_TEST:'
      write ( *, '(a)' ) '  Kronrod-Patterson quadrature over [0,1]:'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   Level   Nodes    Estimate  Error'
      write ( *, '(a)' ) ' '

      d = 1
      exact = fu_integral ( d );

      do l = 1, 5

        call kpu_order ( l, n )

        call kpu ( n, x, w )

        call fu_value ( d, n, x, fx )

        q = r8vec_dot_product ( n, w, fx )

        e = sqrt ( ( q - exact )**2 ) / exact

        write ( *, '(2x,i2,4x,i6,2x,g14.6,2x,g14.6)' ) l, n, q, e

      end do

      return
      end
      subroutine nwspgr_size_test ( )

c*********************************************************************72
c
cc NWSPGR_SIZE_TEST tests NWSPGR_SIZE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 January 2013
c
c  Author:
c
c    John Burkardt.
c
      implicit none

      external cce_order
      integer dim
      external gqn_order
      external gqu_order
      integer k
      external kpn_order
      external kpu_order
      integer r_size
      integer seq_max

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'NWSPGR_SIZE_TEST:'
      write ( *, '(a)' ) 
     &  '  NWSPGR_SIZE: size of a sparse grid, based on either:'
      write ( *, '(a)' ) 
     &  '  one of the built-in 1D rules, or a family of 1D rules'
      write ( *, '(a)' ) '  supplied by the user.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  Kronrod-Patterson, [0,1], Dim 2, Level 3, Symmetric'
      write ( *, '(a)' ) ''
      k = 2
      dim = 3
      call num_seq ( k - 1, dim, seq_max )
      call nwspgr_size ( kpu_order, dim, k, seq_max, r_size )
      write ( *, '(a,i6)' ) '  Full          ', r_size

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  Kronrod-Patterson, (-oo,+oo), Dim 2, Level 3, Symmetric'
      write ( *, '(a)' ) ''
      k = 2
      dim = 3
      call num_seq ( k - 1, dim, seq_max )
      call nwspgr_size ( kpn_order, dim, k, seq_max, r_size )
      write ( *, '(a,i6)' ) '  Full          ', r_size

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  Gauss-Legendre, [0,1], Dim 2, Level 3, Symmetric'
      write ( *, '(a)' ) ''
      k = 2
      dim = 3
      call num_seq ( k - 1, dim, seq_max )
      call nwspgr_size ( gqu_order, dim, k, seq_max, r_size )
      write ( *, '(a,i6)' ) '  Full          ', r_size

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  Gauss Hermite, (-oo,+oo), [0,1], Dim 2, Level 3, Symmetric'
      write ( *, '(a)' ) ''
      k = 2
      dim = 3
      call num_seq ( k - 1, dim, seq_max )
      call nwspgr_size ( gqn_order, dim, k, seq_max, r_size )
      write ( *, '(a,i6)' ) '  Full          ', r_size

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  Clenshaw Curtis Exponential, ' //
     &  '[-1,+1], [0,1], Dim 2, Level 3, Unsymmetric'
      write ( *, '(a)' ) ''
      k = 2
      dim = 3
      call num_seq ( k - 1, dim, seq_max )
      call nwspgr_size ( cce_order, dim, k, seq_max, r_size )
      write ( *, '(a,i6)' ) '  Full          ', r_size
c
c  Do a table.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '  Dimension / Level table for Clenshaw Curtis Exponential'
      write ( *, '(a)' ) ''
      write ( *, '(a)', advance = 'no' ) ' Dim: '
      do dim = 1, 10
        write ( *, '(2x,i6)', advance = 'no' ) dim
      end do
      write ( *, '(a)', advance = 'yes' ) ''
      write ( *, '(a)' ) 'Level'
      do k = 1, 5
        write ( *, '(2x,i2,2x)', advance = 'no' ) k
        do dim = 1, 10
          call num_seq ( k - 1, dim, seq_max )
          call nwspgr_size ( cce_order, dim, k, seq_max, r_size )
          write ( *, '(2x,i6)', advance = 'no' ) r_size
        end do
        write ( *, '(a)', advance = 'yes' ) ''
      end do

      return
      end
      subroutine order_report ( )

c*********************************************************************72
c
cc ORDER_REPORT reports on the order of each family of rules.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 January 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer ap
      integer k
      integer kpn_order(5)
      integer l
      integer o
      integer rp

      data kpn_order / 1, 3, 9, 19, 35 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'ORDER_REPORT'
      write ( *, '(a)' ) '  For each family of rules, report:'
      write ( *, '(a)' ) '  L,  the level index,'
      write ( *, '(a)' ) '  RP, the required polynomial precision,'
      write ( *, '(a)' ) '  AP, the actual polynomial precision,'
      write ( *, '(a)' ) '  O,  the rule order (number of points).'

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  GQN family'
      write ( *, '(a)' ) 
     &  '  Gauss quadrature, exponential weight, (-oo,+oo)'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   L  RP  AP   O'
      write ( *, '(a)' ) ' '

      do l = 1, 25
        rp = 2 * l - 1
        o = l
        ap = 2 * o - 1
        write ( *, '(2x,i2,2x,i2,2x,i2,2x,i2)' ) l, rp, ap, o
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  GQU family'
      write ( *, '(a)' ) '  Gauss quadrature, unit weight, [0,1]'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   L  RP  AP   O'
      write ( *, '(a)' ) ' '

      do l = 1, 25
        rp = 2 * l - 1
        o = l
        ap = 2 * o - 1
        write ( *, '(2x,i2,2x,i2,2x,i2,2x,i2)' ) l, rp, ap, o
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  KPN family'
      write ( *, '(a)' ) 
     &  '  Gauss-Kronrod-Patterson quadrature, ' //
     &  'exponential weight, (-oo,+oo)'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   L  RP  AP   O'
      write ( *, '(a)' ) ' '

      k = 1
      o = 1
      ap = 1

      do l = 1, 25

        rp = 2 * l - 1

10      continue

        if ( ap .lt. rp ) then

          if ( k .eq. 5 ) then
            write ( *, '(a)' ) ' '
            write ( *, '(a)' ) '  No higher order rule is available!'
            go to 20
          end if
c
c  Can we use a simple rule?
c
          if ( rp .lt. kpn_order(k+1) ) then
            o = rp
            ap = rp
c
c  Otherwise, move to next higher rule.
c
          else
            k = k + 1
            ap = 2 * kpn_order(k) - kpn_order(k-1)
            o = kpn_order(k)
          end if

          go to 10

        end if

        write ( *, '(2x,i2,2x,i2,2x,i2,2x,i2)' ) l, rp, ap, o

      end do

20    continue

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  KPU family'
      write ( *, '(a)' ) 
     &  '  Gauss-Kronrod-Patterson quadrature, unit weight, [0,1]'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   L  RP  AP   O'
      write ( *, '(a)' ) ' '

      do l = 1, 25
        rp = 2 * l - 1
        o = 1
        ap = 1
30      continue
        if ( ap .lt. rp ) then
          o = 2 * ( o + 1 ) - 1
          ap = ( 3 * o + 1 ) / 2
          go to 30
        end if
        write ( *, '(2x,i2,2x,i2,2x,i2,2x,i2)' ) l, rp, ap, o
      end do

      return
      end
      subroutine symmetric_sparse_size_test ( )

c*********************************************************************72
c
cc SYMMETRIC_SPARSE_SIZE_TEST tests SYMMETRIC_SPARSE_SIZE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 January 2013
c
c  Author:
c
c    Original MATLAB version by Florian Heiss, Viktor Winschel.
c    FORTRAN77 version by John Burkardt.
c
c  Local parameters:
c
c    Local, integer D, the spatial dimension.
c
c    Local, integer MAXK, the maximum level to check.
c
      implicit none

      integer test_num
      parameter ( test_num = 3 )

      integer dim
      integer dim_test(test_num)
      double precision nodes1(6,5)
      double precision nodes2(21,5)
      double precision nodes3(23,3)
      integer r
      integer r_test(test_num)
      integer r2
      integer test
      double precision x0

      data dim_test / 5, 5, 3 /
      data nodes1 /
     & 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 
     & 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 
     & 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 
     & 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 
     & 0.0, 1.0, 0.0, 0.0, 0.0, 0.0 /
      data nodes2 /
     &  0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 
     &  0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.73205, 
     &  0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 
     &  1.0, 1.73205, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 
     &  0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.73205, 0.0, 0.0, 
     &  0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 
     &  0.0, 0.0, 0.0, 1.0, 1.0, 1.73205, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 
     &  1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 
     &  0.0, 1.0, 1.73205, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 
     &  0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0 /
      data nodes3 /
     &  0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 
     &  0.741964, 1.0, 
     &  1.0, 1.0, 1.0, 1.0, 1.0, 1.73205, 1.73205, 1.73205, 2.33441, 
     &  0.0, 0.0, 0.0, 0.0, 0.0, 0.741964, 1.0, 1.0, 1.0, 1.73205, 
     &  1.73205, 2.33441, 
     &  0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.73205, 0.0, 0.0, 1.0, 0.0, 
     &  0.0, 0.741964, 1.0, 1.73205, 2.33441, 0.0, 0.0, 1.0, 1.73205, 
     &  0.0, 1.0, 0.0, 
     &  0.0, 0.0, 1.0, 1.73205, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0 /
      data r_test / 6, 21, 23 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SYMMETRIC_SPARSE_SIZE_TEST'
      write ( *, '(a)' ) 
     &  '  Given a symmetric sparse grid rule represented only by'
      write ( *, '(a)' ) 
     &  '  the points with positive values, determine the total number'
      write ( *, '(a)' ) '  of points in the grid.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  For dimension DIM, we report '
      write ( *, '(a)' ) 
     &  '  R, the number of points in the positive orthant, and '
      write ( *, '(a)' ) '  R2, the total number of points.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '       DIM         R        R2'
      write ( *, '(a)' ) ' '

      x0 = 0.0

      do test = 1, test_num

        r = r_test(test)
        dim = dim_test(test)
        if ( test .eq. 1 ) then
          call symmetric_sparse_size ( r, dim, nodes1, x0, r2 )
        else if ( test .eq. 2 ) then
          call symmetric_sparse_size ( r, dim, nodes2, x0, r2 )
        else if ( test .eq. 3 ) then
          call symmetric_sparse_size ( r, dim, nodes3, x0, r2 )
        end if

        write ( *, '(2x,i8,2x,i8,2x,i8)' ) dim, r, r2

      end do

      return
      end
