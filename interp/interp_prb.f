      program main

c*********************************************************************72
c
cc MAIN is the main program for INTERP_PRB.
c
c  Discussion:
c
c    INTERP_PRB tests the INTERP library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 February 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer data_num

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'INTERP_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version:'
      write ( *, '(a)' ) '  Test the INTERP library.'

      call test01 ( )

      call test02 ( )

      data_num = 6
      call test03 ( data_num )

      data_num = 11
      call test03 ( data_num )

      data_num = 6
      call test04 ( data_num )

      data_num = 11
      call test04 ( data_num )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'INTERP_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 tests INTERP_NEAREST on 1-dimensional data.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 July 2012
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer data_num
      parameter ( data_num = 11 )
      integer m
      parameter ( m = 1 )
      integer before
      parameter ( before = 4 )
      integer fat
      parameter ( fat = 3 )
      integer after
      parameter ( after = 2 )

      integer i
      integer interp
      integer interp_num
      double precision p
      double precision p_data(m,data_num)
      double precision p_interp(m,before+1+(data_num-1)*(fat+1)+after)
      double precision p_value(before+1+(data_num-1)*(fat+1)+after)
      double precision t
      double precision t_data(data_num)
      double precision t_interp(before+1+(data_num-1)*(fat+1)+after)
      double precision t_max
      double precision t_min

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) 
     &  '  INTERP_NEAREST evaluates a nearest-neighbor interpolant.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  In this example, the function we are interpolating is'
      write ( *, '(a)' ) '  Runge''s function, with Chebyshev knots.'

      t_min = -1.0D+00
      t_max = +1.0D+00

      call cc_abscissas_ab ( t_min, t_max, data_num, t_data )

      call f_runge ( m, data_num, t_data, p_data )

      write ( *, '(a)'    ) ' '
      write ( *, '(a)'    ) '  The data to be interpolated:'
      write ( *, '(a)'    ) ' '
      write ( *, '(a,i8)' ) '  Spatial dimension =     ', m
      write ( *, '(a,i8)' ) '  Number of data values = ', data_num
      write ( *, '(a)'    ) ' '
      write ( *, '(a)'    ) '       T_data        P_data'
      write ( *, '(a)'    ) ' '
      do i = 1, data_num
        write ( *, '(2x,2g14.6)' ) t_data(i), p_data(1,i)
      end do
c
c  Our interpolation values will include the original T values, plus
c  3 new values in between each pair of original values.
c
      interp_num = before + 1 + ( data_num - 1 ) * ( fat + 1 ) + after

      call r8vec_expand_linear2 ( data_num, t_data, before, fat, after, 
     &  t_interp )

      call interp_nearest ( m, data_num, t_data, p_data, interp_num,    
     &  t_interp, p_interp )

      call f_runge ( m, interp_num, t_interp, p_value )

      write ( *, '(a)'    ) ' '
      write ( *, '(a)'    ) '  Interpolation:'
      write ( *, '(a)'    ) ' '
      write ( *, '(a)'    ) 
     &  '    T_interp      P_interp        P_exact        Error'
      write ( *, '(a)'    ) ' '

      do interp = 1, interp_num

        write ( *, '(2x,f10.4,2x,g14.6,2x,g14.6,2x,g10.2)' )
     &    t_interp(interp), p_interp(1,interp), p_value(interp),
     &    p_interp(1,interp) - p_value(interp)

      end do

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 tests INTERP_LINEAR on 1-dimensional data.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 December 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer data_num
      parameter ( data_num = 11 )
      integer, parameter :: m = 1
      integer before
      parameter ( before = 4 )
      integer fat
      parameter ( fat = 3 )
      integer after
      parameter ( after = 2 )

      integer i
      integer interp
      integer interp_num
      double precision p
      double precision p_data(m,data_num)
      double precision p_interp(m,before+1+(data_num-1)*(fat+1)+after)
      double precision p_value(before+1+(data_num-1)*(fat+1)+after)
      double precision t
      double precision t_data(data_num)
      double precision t_interp(before+1+(data_num-1)*(fat+1)+after)
      double precision t_max
      double precision t_min

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) 
     &  '  INTERP_LINEAR evaluates a piecewise linear spline.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  In this example, the function we are interpolating is'
      write ( *, '(a)' ) 
     &  '  Runge''s function, with evenly spaced knots.'

      t_min = -1.0D+00
      t_max = +1.0D+00

      call ncc_abscissas_ab ( t_min, t_max, data_num, t_data )

      call f_runge ( m, data_num, t_data, p_data )

      write ( *, '(a)'    ) ' '
      write ( *, '(a)'    ) '  The data to be interpolated:'
      write ( *, '(a)'    ) ' '
      write ( *, '(a,i8)' ) '  Spatial dimension =     ', m
      write ( *, '(a,i8)' ) '  Number of data values = ', data_num
      write ( *, '(a)'    ) ' '
      write ( *, '(a)'    ) '       T_data        P_data'
      write ( *, '(a)'    ) ' '
      do i = 1, data_num
        write ( *, '(2x,2g14.6)' ) t_data(i), p_data(1,i)
      end do
c
c  Our interpolation values will include the original T values, plus
c  3 new values in between each pair of original values.
c
      interp_num = before + 1 + ( data_num - 1 ) * ( fat + 1 ) + after

      call r8vec_expand_linear2 ( data_num, t_data, before, fat, after, 
     &  t_interp )

      call interp_linear ( m, data_num, t_data, p_data, interp_num,     
     &  t_interp, p_interp )

      call f_runge ( m, interp_num, t_interp, p_value )

      write ( *, '(a)'    ) ' '
      write ( *, '(a)'    ) '  Interpolation:'
      write ( *, '(a)'    ) ' '
      write ( *, '(a)'    ) 
     &  '    T_interp      P_interp        P_exact        Error'
      write ( *, '(a)'    ) ' '

      do interp = 1, interp_num

        write ( *, '(2x,f10.4,2x,g14.6,2x,g14.6,2x,g10.2)' )
     &    t_interp(interp), p_interp(1,interp), p_value(interp),
     &    p_interp(1,interp) - p_value(interp)

      end do

      return
      end
      subroutine test03 ( data_num )

c*********************************************************************72
c
cc TEST03 tests INTERP_LAGRANGE on 1-dimensional data, equally spaced data.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 December 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer DATA_NUM, the number of data values.
c
      implicit none

      integer data_num
      integer m
      parameter ( m = 1 )
      integer before
      parameter ( before = 4 )
      integer fat
      parameter ( fat = 3 )
      integer after
      parameter ( after = 2 )

      integer i
      integer interp
      integer interp_num
      double precision p
      double precision p_data(m,data_num)
      double precision p_interp(m,before+1+(data_num-1)*(fat+1)+after)
      double precision p_value(before+1+(data_num-1)*(fat+1)+after)
      double precision t
      double precision t_data(data_num)
      double precision t_interp(before+1+(data_num-1)*(fat+1)+after)
      double precision t_max
      double precision t_min

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST03'
      write ( *, '(a)' ) 
     &  '  INTERP_LAGRANGE evaluates a polynomial interpolant.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  In this example, the function we are interpolating is'
      write ( *, '(a)' ) 
     &  '  Runge''s function, with evenly spaced knots.'

      t_min = -1.0D+00
      t_max = +1.0D+00

      call ncc_abscissas_ab ( t_min, t_max, data_num, t_data )

      call f_runge ( m, data_num, t_data, p_data )

      write ( *, '(a)'    ) ' '
      write ( *, '(a)'    ) '  The data to be interpolated:'
      write ( *, '(a)'    ) ' '
      write ( *, '(a,i8)' ) '  Spatial dimension =     ', m
      write ( *, '(a,i8)' ) '  Number of data values = ', data_num
      write ( *, '(a)'    ) ' '
      write ( *, '(a)'    ) '       T_data        P_data'
      write ( *, '(a)'    ) ' '
      do i = 1, data_num
        write ( *, '(2x,2g14.6)' ) t_data(i), p_data(1,i)
      end do
c
c  Our interpolation values will include the original T values, plus
c  3 new values in between each pair of original values.
c
      interp_num = before + 1 + ( data_num - 1 ) * ( fat + 1 ) + after

      call r8vec_expand_linear2 ( data_num, t_data, before, fat, after, 
     &  t_interp )

      call interp_lagrange ( m, data_num, t_data, p_data, interp_num,   
     &  t_interp, p_interp )

      call f_runge ( m, interp_num, t_interp, p_value )

      write ( *, '(a)'    ) ' '
      write ( *, '(a)'    ) '  Interpolation:'
      write ( *, '(a)'    ) ' '
      write ( *, '(a)'    ) 
     &  '    T_interp      P_interp        P_exact        Error'
      write ( *, '(a)'    ) ' '

      do interp = 1, interp_num

        write ( *, '(2x,f10.4,2x,g14.6,2x,g14.6,2x,g10.2)' )
     &    t_interp(interp), p_interp(1,interp), p_value(interp),
     &    p_interp(1,interp) - p_value(interp)

      end do

      return
      end
      subroutine test04 ( data_num )

c*********************************************************************72
c
cc TEST04 tests INTERP_LAGRANGE on 1-dimensional data, Clenshaw-Curtis data.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 December 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer DATA_NUM, the number of data values.
c
      implicit none

      integer data_num

      integer before
      parameter ( before = 4 )
      integer fat
      parameter ( fat = 3 )
      integer after
      parameter ( after = 2 )
      integer m
      parameter ( m = 1 )

      integer i
      integer interp
      integer interp_num
      double precision p
      double precision p_data(data_num)
      double precision p_interp(m,before+1+(data_num-1)*(fat+1)+after)
      double precision p_value(before+1+(data_num-1)*(fat+1)+after)
      double precision t
      double precision t_data(data_num)
      double precision t_interp(before+1+(data_num-1)*(fat+1)+after)
      double precision t_max
      double precision t_min

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST04'
      write ( *, '(a)' ) 
     &  '  INTERP_LAGRANGE evaluates a polynomial interpolant.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  In this example, the function we are interpolating is'
      write ( *, '(a)' ) 
     &  '  Runge''s function, with Clenshaw Curtis knots.'

      t_min = -1.0D+00
      t_max = +1.0D+00

      call cc_abscissas_ab ( t_min, t_max, data_num, t_data )

      call f_runge ( m, data_num, t_data, p_data )

      write ( *, '(a)'    ) ' '
      write ( *, '(a)'    ) '  The data to be interpolated:'
      write ( *, '(a)'    ) ' '
      write ( *, '(a,i8)' ) '  Spatial dimension =     ', m
      write ( *, '(a,i8)' ) '  Number of data values = ', data_num
      write ( *, '(a)'    ) ' '
      write ( *, '(a)'    ) '       T_data        P_data'
      write ( *, '(a)'    ) ' '
      do i = 1, data_num
        write ( *, '(2x,2g14.6)' ) t_data(i), p_data(i)
      end do
c
c  Our interpolation values will include the original T values, plus
c  3 new values in between each pair of original values.
c
      interp_num = before + 1 + ( data_num - 1 ) * ( fat + 1 ) + after

      call r8vec_expand_linear2 ( data_num, t_data, before, fat, after, 
     &  t_interp )

      call interp_lagrange ( m, data_num, t_data, p_data, interp_num,   
     &  t_interp, p_interp )

      call f_runge ( m, interp_num, t_interp, p_value )

      write ( *, '(a)'    ) ' '
      write ( *, '(a)'    ) '  Interpolation:'
      write ( *, '(a)'    ) ' '
      write ( *, '(a)'    ) 
     &  '    T_interp      P_interp        P_exact        Error'
      write ( *, '(a)'    ) ' '

      do interp = 1, interp_num

        write ( *, '(2x,f10.4,2x,g14.6,2x,g14.6,2x,g10.2)' )
     &    t_interp(interp), p_interp(1,interp), p_value(interp),
     &    p_interp(1,interp) - p_value(interp)

      end do

      return
      end
      subroutine f_runge ( m, n, x, f )

c*********************************************************************72
c
cc F_RUNGE evaluates the Runge function.
c
c  Discussion:
c
c    Interpolation of the Runge function at evenly spaced points in [-1,1]
c    is a common test.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 July 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer N, the number of evaluation points.
c
c    Input, double precision X(M,N), the evaluation points.
c
c    Output, double precision F(N), the function values.
c
      implicit none

      integer m
      integer n

      double precision f(n)
      integer i
      integer j
      double precision s
      double precision x(m,n)

      do j = 1, n
        s = 0.0D+00
        do i = 1, m
          s = s + x(i,j) ** 2
        end do
        f(j) = 1.0D+00 / ( 1.0D+00 + 25.0D+00 * s )
      end do

      return
      end
