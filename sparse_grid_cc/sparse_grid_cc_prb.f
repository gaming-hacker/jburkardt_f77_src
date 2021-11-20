      program main

c********************************************************************72
c
cc MAIN is the main program for SPARSE_GRID_CC_PRB.
c
c  Discussion:
c
c    SPARSE_GRID_CC_PRB tests the SPARSE_GRID_CC routines.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 March 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer dim_max
      integer dim_min
      integer dim_num
      integer level_max
      integer level_max_max
      integer level_max_min

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SPARSE_GRID_CC_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the SPARSE_GRID_CC library.'
c
c  Count number of points in sparse rule from DIM_MIN to DIM_MAX, LEVEL_MAX_MAX.
c
      dim_min = 1
      dim_max = 5
      level_max_min = 0
      level_max_max = 10
      call test01 ( dim_min, dim_max, level_max_min, level_max_max )

      write ( *, '(a)' ) ' '
      call timestamp ( )

      dim_min = 6
      dim_max = 10
      level_max_min = 0
      level_max_max = 10
      call test01 ( dim_min, dim_max, level_max_min, level_max_max )

      write ( *, '(a)' ) ' '
      call timestamp ( )

      dim_min = 11
      dim_max = 15
      level_max_min = 0
      level_max_max = 5
      call test01 ( dim_min, dim_max, level_max_min, level_max_max )

      write ( *, '(a)' ) ' '
      call timestamp ( )

      dim_min = 100
      dim_max = 100
      level_max_min = 0
      level_max_max = 4
      call test01 ( dim_min, dim_max, level_max_min, level_max_max )

      write ( *, '(a)' ) ' '
      call timestamp ( )
c
c  Count number of points in sparse rule from DIM_MIN to DIM_MAX, LEVEL_MAX_MAX.
c
      dim_min = 1
      dim_max = 5
      level_max_min = 0
      level_max_max = 10
      call test015 ( dim_min, dim_max, level_max_min, level_max_max )

      write ( *, '(a)' ) ' '
      call timestamp ( )

      dim_min = 6
      dim_max = 10
      level_max_min = 0
      level_max_max = 10
      call test015 ( dim_min, dim_max, level_max_min, level_max_max )

      write ( *, '(a)' ) ' '
      call timestamp ( )

      dim_min = 100
      dim_max = 100
      level_max_min = 0
      level_max_max = 4
      call test015 ( dim_min, dim_max, level_max_min, level_max_max )

      write ( *, '(a)' ) ' '
      call timestamp ( )
c
c  Compute abstract grid indices of sparse grid points as selected from product grid
c  for DIMENSION, LEVEL_MAX.
c
      call test02 ( 2, 3 )
      call test02 ( 2, 4 )
      call test02 ( 3, 0 )
      call test02 ( 3, 2 )
      call test02 ( 6, 2 )
c
c  Compute sparse Clenshaw-Curtis rule for DIMENSION, LEVEL_MAX.
c
      call test03 ( 2, 3 )
      call test03 ( 3, 0 )
      call test03 ( 3, 1 )
c
c  Test sum of weights for DIMENSION, LEVEL_MAX.
c
      call test04 ( 2, 4 )
      call test04 ( 3, 0 )
      call test04 ( 3, 1 )
      call test04 ( 3, 6 )
      call test04 ( 10, 3 )
c
c  Test monomial exactness for DIMENSION, LEVEL_MAX, DEGREE_MAX.
c
      call test05 ( 2, 0, 3 )
      call test05 ( 2, 1, 5 )
      call test05 ( 2, 2, 7 )
      call test05 ( 2, 3, 9 )
      call test05 ( 2, 4, 11 )
      call test05 ( 2, 5, 13 )

      call test05 ( 3, 0, 2 )
      call test05 ( 3, 1, 4 )
      call test05 ( 3, 2, 6 )
      call test05 ( 3, 3, 8 )
c
c  Show how to write a rule to a file.
c
      dim_num = 4
      level_max = 2

      call test06 ( dim_num, level_max )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SPARSE_GRID_CC_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'

      write ( *, '(a)' ) ' '
      call timestamp ( )

      return
      end
      subroutine test01 ( dim_min, dim_max, level_max_min,
     &  level_max_max )

c********************************************************************72
c
cc TEST01 tests SPARSE_GRID_CFN_SIZE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 March 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer DIM_MIN, the minimum spatial dimension.
c
c    Input, integer DIM_MAX, the maximum spatial dimension.
c
c    Input, integer LEVEL_MAX_MIN, the minimum value of LEVEL_MAX.
c
c    Input, integer LEVEL_MAX_MAX, the maximum value of LEVEL_MAX.
c
      implicit none

      integer dim_max
      integer dim_min

      integer dim
      integer dim_num
      integer level_max
      integer level_max_max
      integer level_max_min
      integer point_num(dim_min:dim_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) 
     &  '  SPARSE_GRID_CFN_SIZE returns the number of distinct'
      write ( *, '(a)' ) 
     &  '  points in asparse grid of Closed Fully Nested rules.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Each sparse grid is of spatial dimension DIM,'
      write ( *, '(a)' ) 
     &  '  and is made up of all product grids of levels'
      write ( *, '(a)' ) '  up to LEVEL_MAX.'
      write ( *, '(a)' ) ' '

      do dim_num = dim_min, dim_max
        point_num(dim_num) = dim_num
      end do

      write ( *, '(a8,6(2x,i8))' ) 
     &  '   DIM: ', ( point_num(dim), dim = dim_min, dim_max )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   LEVEL_MAX'
      write ( *, '(a)' ) ' '

      do level_max = level_max_min, level_max_max

        do dim_num = dim_min, dim_max
          call sparse_grid_cfn_size ( dim_num, level_max, 
     &      point_num(dim_num) )
        end do

        write ( *, '(a4,i4,6(2x,i8))' ) '    ', level_max, 
     &    ( point_num(dim), dim = dim_min, dim_max )

      end do

      return
      end
      subroutine test015 ( dim_min, dim_max, level_max_min, 
     &  level_max_max )

c********************************************************************72
c
cc TEST015 tests SPARSE_GRID_CCS_SIZE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 March 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer DIM_MIN, the minimum spatial dimension.
c
c    Input, integer DIM_MAX, the maximum spatial dimension.
c
c    Input, integer LEVEL_MAX_MIN, the minimum value of LEVEL_MAX.
c
c    Input, integer LEVEL_MAX_MAX, the maximum value of LEVEL_MAX.
c
      implicit none

      integer dim_max
      integer dim_min

      integer dim
      integer dim_num
      integer level_max
      integer level_max_max
      integer level_max_min
      integer point_num(dim_min:dim_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST015'
      write ( *, '(a)' ) 
     &  '  SPARSE_GRID_CCS_SIZE returns the number of distinct'
      write ( *, '(a)' ) 
     &  '  points in a Clenshaw Curtis Slow-Growth sparse grid.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Each sparse grid is of spatial dimension DIM, and'
      write ( *, '(a)' ) 
     &  '  is made up of all product grids of levels up to LEVEL_MAX.'
      write ( *, '(a)' ) ' '

      do dim_num = dim_min, dim_max
        point_num(dim_num) = dim_num
      end do

      write ( *, '(a8,6(2x,i8))' ) '   DIM: ', 
     &  ( point_num(dim), dim = dim_min, dim_max)
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   LEVEL_MAX'
      write ( *, '(a)' ) ' '

      do level_max = level_max_min, level_max_max

        do dim_num = dim_min, dim_max
          call sparse_grid_ccs_size ( dim_num, level_max, 
     &      point_num(dim_num) )
        end do

        write ( *, '(a4,i4,6(2x,i8))' ) '    ', 
     &    level_max, ( point_num(dim), dim = dim_min, dim_max )

      end do

      return
      end
      subroutine test02 ( dim_num, level_max )

c********************************************************************72
c
cc TEST02 tests SPARSE_GRID_CC_INDEX.
c
c  Discussion:
c
c    The routine computes the indices of the unique points used in a sparse
c    multidimensional grid whose size is controlled by a parameter LEVEL_MAX.
c
c    Once these indices are returned, they can be converted into
c    Clenshaw Curtis points.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 March 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer DIM_NUM, the spatial dimension.
c
c    Input, integer LEVEL_MAX, the level.
c
      implicit none

      integer point_max
      parameter ( point_max = 85 )

      integer dim
      integer dim_num
      integer grid_index(dim_num,point_max)
      integer level_max
      integer point
      integer point_num

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST02:'
      write ( *, '(a)' ) 
     &  '  SPARSE_GRID_CC_INDEX returns all grid indexes'
      write ( *, '(a)' ) '  whose level value satisfies'
      write ( *, '(a)' ) '    0 <= LEVEL <= LEVEL_MAX.'
      write ( *, '(a)' ) 
     &  '  Here, LEVEL is the sum of the levels of the 1D rules,'
      write ( *, '(a)' ) '  and the order of the rule is 2^LEVEL + 1.'

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  LEVEL_MAX = ', level_max
      write ( *, '(a,i8)' ) '  Spatial dimension DIM_NUM = ', dim_num

      call sparse_grid_cfn_size ( dim_num, level_max, point_num )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) 
     &  '  Number of unique points in the grid = ', point_num
c
c  Compute the orders and points.
c
      call sparse_grid_cc_index ( dim_num, level_max, point_num,
     &  grid_index )
c
c  Now we're done.  Print the merged grid data.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Grid index:'
      write ( *, '(a)' ) ' '
      do point = 1, point_num
        write ( *, '(a2,i4,a2,10i6)' ) '  ', point, ' ', 
     &    ( grid_index(dim,point), dim = 1, dim_num )
      end do

      return
      end
      subroutine test03 ( dim_num, level_max )

c********************************************************************72
c
cc TEST03 calls SPARSE_GRID_CC to create a Clenshaw Curtis grid.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 March 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer DIM_NUM, the spatial dimension.
c
c    Input, integer LEVEL_MAX, the level.
c
      implicit none

      integer point_max
      parameter ( point_max = 65 )

      integer dim
      integer dim_num
      double precision grid_point(dim_num,point_max)
      double precision grid_weight(point_max)
      integer level_max
      integer point
      integer point_num

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST03:'
      write ( *, '(a)' ) 
     &  '  SPARSE_GRID_CC makes a sparse Clenshaw Curtis grid.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  LEVEL_MAX = ', level_max
      write ( *, '(a,i8)' ) 
     &  '  Spatial dimension DIM_NUM = ', dim_num
c
c  Determine the number of points.
c
      call sparse_grid_cfn_size ( dim_num, level_max, point_num )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) 
     &  '  Number of unique points in the grid = ', point_num
c
c  Compute the weights and points.
c
      call sparse_grid_cc ( dim_num, level_max, point_num, grid_weight, 
     &  grid_point )
c
c  Print them out.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Grid weights:'
      write ( *, '(a)' ) ' '
      do point = 1, point_num
        write ( *, '(2x,i4,2x,f10.6)' ) point, grid_weight(point)
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Grid points:'
      write ( *, '(a)' ) ' '
      do point = 1, point_num
        write ( *, '(2x,i4,2x,5f10.6)' ) 
     &    point, ( grid_point(dim,point), dim = 1, dim_num )
      end do

      return
      end
      subroutine test04 ( dim_num, level_max )

c********************************************************************72
c
cc TEST04 sums the weights and compares them to 2^DIM_NUM.
c
c  Discussion:
c
c    This routine gets the sparse grid indices and determines the
c    corresponding sparse grid abscissas.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 March 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer DIM_NUM, the spatial dimension.
c
c    Input, integer LEVEL_MAX, the level.
c
      implicit none

      integer point_max
      parameter ( point_max = 1581 )
      integer dim_num
      double precision grid_point(dim_num,point_max)
      double precision grid_weight(point_max)
      integer level_max
      integer point
      integer point_num
      double precision r8vec_sum
      double precision weight_sum
      double precision weight_sum_error
      double precision weight_sum_exact

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST04:'
      write ( *, '(a)' ) 
     &  '  Compute the weights of a Clenshaw Curtis sparse grid .'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  As a simple test, sum these weights.'
      write ( *, '(a)' ) '  They should sum to exactly 2^DIM_NUM.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  LEVEL_MAX = ', level_max
      write ( *, '(a,i8)' ) '  Spatial dimension DIM_NUM = ', dim_num
c
c  Determine the number of points.
c
      call sparse_grid_cfn_size ( dim_num, level_max, point_num )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) 
     &  '  Number of unique points in the grid = ', point_num
c
c  Compute the weights and points.
c
      call sparse_grid_cc ( dim_num, level_max, point_num, grid_weight, 
     &  grid_point )
c
c  Sum the weights.
c
      weight_sum = r8vec_sum ( point_num, grid_weight )

      weight_sum_exact = 2.0D+00 ** dim_num

      weight_sum_error = abs ( weight_sum - weight_sum_exact )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '    Weight sum     Exact sum    Difference'
      write ( *, '(a)' ) ' '
      write ( *, '(a2,g14.6,a2,g14.6,a2,g14.6)' ) 
     &  '  ', weight_sum, '  ', weight_sum_exact, 
     &  '  ', weight_sum_error

      return
      end
      subroutine test05 ( dim_num, level_max, degree_max )

c********************************************************************72
c
cc TEST05 tests a Clenshaw Curtis sparse grid rule for monomial exactness.
c
c  Discussion:
c
c    This test is going to check EVERY monomial of total degree DEGREE_MAX
c    or less.  Even for a moderately high dimension of DIM_NUM = 10, you
c    do NOT want to use a large value of DEGREE_MAX, since there are
c
c      1         monomials of total degree 0,
c      DIM_NUM   monomials of total degree 1,
c      DIM_NUM^2 monomials of total degree 2,
c      DIM_NUM^3 monomials of total degree 3, and so on.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 March 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer DIM_NUM, the spatial dimension.
c
c    Input, integer LEVEL_MAX, the level.
c
c    Input, integer DEGREE_MAX, the maximum monomial total degree to check.
c
      implicit none

      integer dim_num
      integer point_max
      parameter ( point_max = 145 )

      integer degree
      integer degree_max
      integer dim
      integer expon(dim_num)
      double precision grid_point(dim_num,point_max)
      double precision grid_weight(point_max)
      integer h
      integer i
      integer j
      integer last
      integer level_max
      logical more
      integer point
      integer point_num
      double precision quad_error
      integer t
      double precision volume
    
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST05'
      write ( *, '(a)' ) 
     &  '  Check the exactness of a CC sparse grid quadrature rule,'
      write ( *, '(a)' ) 
     &  '  applied to all monomials of orders 0 to DEGREE_MAX.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  LEVEL_MAX = ', level_max
      write ( *, '(a,i8)' ) '  Spatial dimension DIM_NUM = ', dim_num
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  The maximum total degree to be '
     &  // 'checked is DEGREE_MAX = ', degree_max
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) 
     &  '  We expect accuracy up to and including total degree ', 
     &  2 * level_max + 1
c
c  Determine the number of points in the rule.
c
      call sparse_grid_cfn_size ( dim_num, level_max, point_num )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) 
     &  '  Number of unique points in the grid = ', point_num
c
c  Compute the weights and points.
c
      call sparse_grid_cc ( dim_num, level_max, point_num,
     &  grid_weight, grid_point )
c
c  Rescale the weights, and translate the abscissas.
c
      volume = 2.0D+00 ** dim_num

      do j = 1, point_num
        grid_weight(j) = grid_weight(j) / volume
      end do

      do j = 1, point_num
        do i = 1, dim_num
          grid_point(i,j) = ( grid_point(i,j) + 1.0D+00 ) / 2.0D+00
        end do
      end do
c
c  Explore the monomials.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '      Error      Total   Monomial'
      write ( *, '(a)' ) '                 Degree  Exponents'
      write ( *, '(a)' ) ' '

      do degree = 0, degree_max

        more = .false.

10      continue

          call comp_next ( degree, dim_num, expon, more, h, t )

          call monomial_quadrature ( dim_num, expon, point_num, 
     &      grid_weight, grid_point, quad_error )

          write ( *, '(a,g14.6,a,i2,a,10i2)' ) '  ', quad_error, '  ', 
     &      degree, '    ', ( expon(dim), dim = 1, dim_num)

          if ( .not. more ) then
            go to 20
          end if

        go to 10

20      continue

        write ( *, '(a)' ) ' '

      end do

      return
      end
      subroutine test06 ( dim_num, level_max )

c********************************************************************72
c
cc TEST06 creates a Clenshaw Curtis grid and writes it to a file.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 March 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer DIM_NUM, the spatial dimension.
c
c    Input, integer LEVEL_MAX, the level.
c
      implicit none

      integer point_max
      parameter ( point_max = 41 )

      integer dim
      integer dim_num
      integer level_max
      integer point
      integer point_num
      double precision r(dim_num,2)
      character * ( 80 ) r_filename
      double precision w(point_max)
      character * ( 80 ) w_filename
      double precision x(dim_num,point_max)
      character * ( 80 ) x_filename

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST06:'
      write ( *, '(a)' ) 
     &  '  Call SPARSE_GRID_CC to make a sparse Clenshaw Curtis grid.'
      write ( *, '(a)' ) 
     &  '  Write the data to a set of quadrature files.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  LEVEL_MAX = ', level_max
      write ( *, '(a,i8)' ) '  Spatial dimension DIM_NUM = ', dim_num
c
c  Determine the number of points.
c
      call sparse_grid_cfn_size ( dim_num, level_max, point_num )
c
c  Compute the weights and points.
c
      do dim = 1, dim_num
        r(dim,1) = -1.0D+00
        r(dim,2) = +1.0D+00
      end do

      call sparse_grid_cc ( dim_num, level_max, point_num, w, x )
c
c  Write the data out.
c
      write ( r_filename, '(a,i2,a,i3,a)' ) 
     &  'cc_d', dim_num, '_level', level_max, '_r.txt'
      write ( w_filename, '(a,i2,a,i3,a)' ) 
     &  'cc_d', dim_num, '_level', level_max, '_w.txt'
      write ( x_filename, '(a,i2,a,i3,a)' ) 
     &  'cc_d', dim_num, '_level', level_max, '_x.txt'

      call s_blank_delete ( r_filename )
      call s_blank_delete ( w_filename )
      call s_blank_delete ( x_filename )

      call r8mat_write ( r_filename, dim_num, 2,         r )
      call r8mat_write ( w_filename, 1,       point_num, w )
      call r8mat_write ( x_filename, dim_num, point_num, x )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  R data written to "' // trim ( r_filename ) // '".'
      write ( *, '(a)' ) 
     &  '  W data written to "' // trim ( w_filename ) // '".'
      write ( *, '(a)' ) 
     &  '  X data written to "' // trim ( x_filename ) // '".'

      return
      end
