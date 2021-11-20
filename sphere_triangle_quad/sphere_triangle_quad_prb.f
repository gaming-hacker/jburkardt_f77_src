      program main

c*********************************************************************72
c
cc MAIN is the main program for SPHERE_TRIANGLE_QUAD_PRB.
c
c  Discussion:
c
c    SPHERE_TRIANGLE_QUAD_PRB tests the SPHERE_TRIANGLE_QUAD library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    21 April 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SPHERE_TRIANGLE_QUAD_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the SPHERE_TRIANGLE_QUAD library.'

      call test01 ( )
      call test02 ( )
      call test03 ( )
      call test04 ( )
      call test05 ( )
      call test06 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'SPHERE_TRIANGLE_QUAD_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 tests SPHERE01_TRIANGLE_QUAD_01, 02, 03.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    21 April 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer test_num
      parameter ( test_num = 17 )

      integer e(3)
      integer e_test(3,test_num)
      double precision polyterm_value_3d
      external polyterm_value_3d
      integer i
      integer j
      double precision result_01
      double precision result_02
      double precision result_03
      integer seed
      double precision v1(3)
      double precision v2(3)
      double precision v3(3)

      save e_test

      data e_test /
     &  0, 0, 0,
     &  1, 0, 0,
     &  0, 1, 0,
     &  0, 0, 1,
     &  2, 0, 0,
     &  0, 2, 2,
     &  2, 2, 2,
     &  0, 2, 4,
     &  0, 0, 6,
     &  1, 2, 4,
     &  2, 4, 2,
     &  6, 2, 0,
     &  0, 0, 8,
     &  6, 0, 4,
     &  4, 6, 2,
     &  2, 4, 8,
     & 16, 0, 0 /

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) 
     &  '  Approximate an integral on a random spherical triangle.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  QUAD_01 uses centroids of spherical triangles.'
      write ( *, '(a)' ) 
     &  '  QUAD_02 uses vertices of spherical triangles.'
      write ( *, '(a)' ) 
     &  '  QUAD_03 uses midsides of spherical triangles.'
c
c  Choose three points at random to define a spherical triangle.
c
      call sphere01_sample ( 1, seed, v1 )
      call sphere01_sample ( 1, seed, v2 )
      call sphere01_sample ( 1, seed, v3 )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Vertices of random spherical triangle:'
      write ( *, '(a)' ) ' '
      call r8vec_transpose_print ( 3, v1, '  V1:' )
      call r8vec_transpose_print ( 3, v2, '  V2:' )
      call r8vec_transpose_print ( 3, v3, '  V3:' )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'QUAD_01      QUAD_02      QUAD_03'

      do j = 1, test_num

        do i = 1, 3
          e(i) = e_test(i,j)
        end do

        call polyterm_exponent ( 'SET', e )

        call polyterm_exponent ( 'PRINT', e )

        call sphere01_triangle_quad_01 ( v1, v2, v3, polyterm_value_3d, 
     &    result_01 )

        call sphere01_triangle_quad_02 ( v1, v2, v3, polyterm_value_3d, 
     &    result_02 )

        call sphere01_triangle_quad_03 ( v1, v2, v3, polyterm_value_3d, 
     &    result_03 )

        write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &    result_01, result_02, result_03

      end do

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 tests SPHERE01_TRIANGLE_QUAD_00.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    21 April 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer test_num
      parameter ( test_num = 17 )

      integer e(3)
      integer e_test(3,test_num)
      double precision polyterm_value_3d
      external polyterm_value_3d
      integer i
      integer j
      integer n_mc1
      parameter ( n_mc1 = 1000 )
      integer n_mc2
      parameter ( n_mc2 = 10000 )
      integer n_mc3
      parameter ( n_mc3 = 100000 )
      double precision result_mc1
      double precision result_mc2
      double precision result_mc3
      double precision result_01
      double precision result_02
      double precision result_03
      integer seed
      double precision v1(3)
      double precision v2(3)
      double precision v3(3)

      save e_test

      data e_test /
     &  0, 0, 0,
     &  1, 0, 0,
     &  0, 1, 0,
     &  0, 0, 1,
     &  2, 0, 0,
     &  0, 2, 2,
     &  2, 2, 2,
     &  0, 2, 4,
     &  0, 0, 6,
     &  1, 2, 4,
     &  2, 4, 2,
     &  6, 2, 0,
     &  0, 0, 8,
     &  6, 0, 4,
     &  4, 6, 2,
     &  2, 4, 8,
     & 16, 0, 0 /

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) 
     &  '  Approximate an integral on a random spherical triangle.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i10,a)' ) 
     &  '  QUAD_MC1 uses a Monte Carlo method with', n_mc1, ' points.'
      write ( *, '(a,i10,a)' ) 
     &  '  QUAD_MC2 uses a Monte Carlo method with', n_mc2, ' points.'
      write ( *, '(a,i10,a)' ) 
     &  '  QUAD_MC3 uses a Monte Carlo method with', n_mc3, ' points.'
c
c  Choose three points at random to define a spherical triangle.
c
      call sphere01_sample ( 1, seed, v1 )
      call sphere01_sample ( 1, seed, v2 )
      call sphere01_sample ( 1, seed, v3 )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Vertices of random spherical triangle:'
      write ( *, '(a)' ) ' '
      call r8vec_transpose_print ( 3, v1, '  V1:' )
      call r8vec_transpose_print ( 3, v2, '  V2:' )
      call r8vec_transpose_print ( 3, v3, '  V3:' )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  QUAD_MC1      QUAD_MC2      QUAD_MC3'

      do j = 1, test_num

        do i = 1, 3
          e(i) = e_test(i,j)
        end do

        call polyterm_exponent ( 'SET', e )

        call polyterm_exponent ( 'PRINT', e )

        call sphere01_triangle_quad_00 ( n_mc1, v1, v2, v3, 
     &    polyterm_value_3d, seed, result_mc1 )
        call sphere01_triangle_quad_00 ( n_mc2, v1, v2, v3, 
     &    polyterm_value_3d, seed, result_mc2 )
        call sphere01_triangle_quad_00 ( n_mc3, v1, v2, v3, 
     &    polyterm_value_3d, seed, result_mc3 )

        write ( *, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &    result_mc1, result_mc2, result_mc3

      end do

      return
      end
      subroutine test03 ( )

c*********************************************************************72
c
cc TEST03 tests SPHERE01_TRIANGLE_QUAD_ICOS1C.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    21 April 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer test_num
      parameter ( test_num = 17 )

      double precision best
      integer e(3)
      integer e_test(3,test_num)
      double precision error
      integer factor
      integer factor_log
      double precision polyterm_value_3d
      external polyterm_value_3d
      integer i
      integer j
      integer node_num
      double precision result
      integer seed
      double precision v1(3)
      double precision v2(3)
      double precision v3(3)

      save e_test

      data e_test /
     &  0, 0, 0,
     &  1, 0, 0,
     &  0, 1, 0,
     &  0, 0, 1,
     &  2, 0, 0,
     &  0, 2, 2,
     &  2, 2, 2,
     &  0, 2, 4,
     &  0, 0, 6,
     &  1, 2, 4,
     &  2, 4, 2,
     &  6, 2, 0,
     &  0, 0, 8,
     &  6, 0, 4,
     &  4, 6, 2,
     &  2, 4, 8,
     & 16, 0, 0 /

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST03'
      write ( *, '(a)' ) 
     &  '  SPHERE01_TRIANGLE_QUAD_ICOS1C approximate the'
      write ( *, '(a)' ) 
     &  '  integral of a function over a spherical triangle on'
      write ( *, '(a)' ) 
     &  '  the surface of the unit sphere using a centroid rule.'
      write ( *, '(a)' ) ' ' 
      write ( *, '(a)' ) 
     &  '  We do not have an exact result, so we compare each'
      write ( *, '(a)' ) '  estimate to the final one.'
c
c  Choose three points at random to define a spherical triangle.
c 
      call sphere01_sample ( 1, seed, v1 )
      call sphere01_sample ( 1, seed, v2 )
      call sphere01_sample ( 1, seed, v3 )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Vertices of random spherical triangle:'
      write ( *, '(a)' ) ' '
      call r8vec_transpose_print ( 3, v1, '  V1:' )
      call r8vec_transpose_print ( 3, v2, '  V2:' )
      call r8vec_transpose_print ( 3, v3, '  V3:' )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FACTOR   N   RESULT'

      do j = 1, test_num

        do i = 1, 3
          e(i) = e_test(i,j)
        end do

        call polyterm_exponent ( 'SET', e )

        call polyterm_exponent ( 'PRINT', e )

        factor = 2**11
        call sphere01_triangle_quad_icos1c ( v1, v2, v3, factor, 
     &    polyterm_value_3d, node_num, best )

        factor = 1
        do factor_log = 0, 9

          call sphere01_triangle_quad_icos1c ( v1, v2, v3, factor, 
     &      polyterm_value_3d, node_num, result )

          error = abs ( result - best )

          write ( *, '(2x,i4,2x,i8,2x,g16.8,2x,g10.2)' ) 
     &      factor, node_num, result, error

          factor = factor * 2

        end do

      end do

      return
      end
      subroutine test04 ( )

c*********************************************************************72
c
cc TEST04 tests SPHERE01_TRIANGLE_QUAD_ICOS1M.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    21 April 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer test_num
      parameter ( test_num = 17 )

      double precision best
      integer e(3)
      integer e_test(3,test_num)
      double precision error
      integer factor
      integer factor_log
      double precision polyterm_value_3d
      external polyterm_value_3d
      integer i
      integer j
      integer node_num
      double precision result
      integer seed
      double precision v1(3)
      double precision v2(3)
      double precision v3(3)

      save e_test

      data e_test /
     &  0, 0, 0,
     &  1, 0, 0,
     &  0, 1, 0,
     &  0, 0, 1,
     &  2, 0, 0,
     &  0, 2, 2,
     &  2, 2, 2,
     &  0, 2, 4,
     &  0, 0, 6,
     &  1, 2, 4,
     &  2, 4, 2,
     &  6, 2, 0,
     &  0, 0, 8,
     &  6, 0, 4,
     &  4, 6, 2,
     &  2, 4, 8,
     & 16, 0, 0 /

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST04'
      write ( *, '(a)' ) 
     &  '  SPHERE01_TRIANGLE_QUAD_ICOS1M approximate the'
      write ( *, '(a)' ) 
     &  '  integral of a function over a spherical triangle'
      write ( *, '(a)' ) '  on the surface of the unit sphere using a'
      write ( *, '(a)' ) '  midpoint rule.'
      write ( *, '(a)' ) ' ' 
      write ( *, '(a)' ) 
     &  '  We do not have an exact result, so we compare each'
      write ( *, '(a)' ) '  estimate to the final one.'
c
c  Choose three points at random to define a spherical triangle.
c
      call sphere01_sample ( 1, seed, v1 )
      call sphere01_sample ( 1, seed, v2 )
      call sphere01_sample ( 1, seed, v3 )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Vertices of random spherical triangle:'
      write ( *, '(a)' ) ' '
      call r8vec_transpose_print ( 3, v1, '  V1:' )
      call r8vec_transpose_print ( 3, v2, '  V2:' )
      call r8vec_transpose_print ( 3, v3, '  V3:' )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FACTOR   N   RESULT'

      do j = 1, test_num

        do i = 1, 3
          e(i) = e_test(i,j)
        end do

        call polyterm_exponent ( 'SET', e )

        call polyterm_exponent ( 'PRINT', e )

        factor = 2**11
        call sphere01_triangle_quad_icos1m ( v1, v2, v3, factor, 
     &    polyterm_value_3d, node_num, best )

        factor = 1
        do factor_log = 0, 9

          call sphere01_triangle_quad_icos1m ( v1, v2, v3, factor, 
     &      polyterm_value_3d, node_num, result )

          error = abs ( result - best )

          write ( *, '(2x,i4,2x,i8,2x,g16.8,2x,g10.2)' ) 
     &      factor, node_num, result, error

          factor = factor * 2

        end do

      end do

      return
      end
      subroutine test05 ( )

c*********************************************************************72
c
cc TEST05 tests SPHERE01_TRIANGLE_QUAD_ICOS1V.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    21 April 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer test_num
      parameter ( test_num = 17 )

      double precision best
      integer e(3)
      integer e_test(3,test_num)
      double precision error
      integer factor
      integer factor_log
      double precision polyterm_value_3d
      external polyterm_value_3d
      integer i
      integer j
      integer node_num
      double precision result
      integer seed
      double precision v1(3)
      double precision v2(3)
      double precision v3(3)

      save e_test

      data e_test /
     &  0, 0, 0,
     &  1, 0, 0,
     &  0, 1, 0,
     &  0, 0, 1,
     &  2, 0, 0,
     &  0, 2, 2,
     &  2, 2, 2,
     &  0, 2, 4,
     &  0, 0, 6,
     &  1, 2, 4,
     &  2, 4, 2,
     &  6, 2, 0,
     &  0, 0, 8,
     &  6, 0, 4,
     &  4, 6, 2,
     &  2, 4, 8,
     & 16, 0, 0 /

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST05'
      write ( *, '(a)' ) 
     &  '  SPHERE01_TRIANGLE_QUAD_ICOS1V approximates the'
      write ( *, '(a)' ) 
     &  '  integral of a function over a spherical triangle'
      write ( *, '(a)' ) 
     &  '  on the surface of the unit sphere using a vertex rule.'
      write ( *, '(a)' ) ' ' 
      write ( *, '(a)' ) 
     &  '  We do not have an exact result, so we compare each'
      write ( *, '(a)' ) '  estimate to the final one.'
c
c  Choose three points at random to define a spherical triangle.
c
      call sphere01_sample ( 1, seed, v1 )
      call sphere01_sample ( 1, seed, v2 )
      call sphere01_sample ( 1, seed, v3 )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Vertices of random spherical triangle:'
      write ( *, '(a)' ) ' '
      call r8vec_transpose_print ( 3, v1, '  V1:' )
      call r8vec_transpose_print ( 3, v2, '  V2:' )
      call r8vec_transpose_print ( 3, v3, '  V3:' )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FACTOR   N   RESULT'

      do j = 1, test_num

        do i = 1, 3
          e(i) = e_test(i,j)
        end do

        call polyterm_exponent ( 'SET', e )

        call polyterm_exponent ( 'PRINT', e )

        factor = 2**11
        call sphere01_triangle_quad_icos1v ( v1, v2, v3, factor, 
     &    polyterm_value_3d, node_num, best )

        factor = 1
        do factor_log = 0, 9

          call sphere01_triangle_quad_icos1v ( v1, v2, v3, factor, 
     &      polyterm_value_3d, node_num, result )

          error = abs ( result - best )

          write ( *, '(2x,i4,2x,i8,2x,g16.8,2x,g10.2)' ) 
     &      factor, node_num, result, error

          factor = factor * 2

        end do

      end do

      return
      end
      subroutine test06 ( )

c*********************************************************************72
c
cc TEST06 tests SPHERE01_TRIANGLE_QUAD_ICOS2V.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    21 April 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer test_num
      parameter ( test_num = 17 )

      double precision best
      integer e(3)
      integer e_test(3,test_num)
      double precision error
      integer factor
      integer factor_log
      double precision polyterm_value_3d
      external polyterm_value_3d
      integer i
      integer j
      integer node_num
      double precision result
      integer seed
      double precision v1(3)
      double precision v2(3)
      double precision v3(3)

      save e_test

      data e_test /
     &  0, 0, 0,
     &  1, 0, 0,
     &  0, 1, 0,
     &  0, 0, 1,
     &  2, 0, 0,
     &  0, 2, 2,
     &  2, 2, 2,
     &  0, 2, 4,
     &  0, 0, 6,
     &  1, 2, 4,
     &  2, 4, 2,
     &  6, 2, 0,
     &  0, 0, 8,
     &  6, 0, 4,
     &  4, 6, 2,
     &  2, 4, 8,
     & 16, 0, 0 /

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST06'
      write ( *, '(a)' ) 
     &  '  SPHERE01_TRIANGLE_QUAD_ICOS2V approximates the'
      write ( *, '(a)' ) 
     &  '  integral of a function over a spherical triangle'
      write ( *, '(a)' ) 
     &  '  on the surface of the unit sphere using a vertex rule.'
      write ( *, '(a)' ) ' ' 
      write ( *, '(a)' ) 
     &  '  We do not have an exact result, so we compare each'
      write ( *, '(a)' ) '  estimate to the final one.'
c
c  Choose three points at random to define a spherical triangle.
c
      call sphere01_sample ( 1, seed, v1 )
      call sphere01_sample ( 1, seed, v2 )
      call sphere01_sample ( 1, seed, v3 )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Vertices of random spherical triangle:'
      write ( *, '(a)' ) ' '
      call r8vec_transpose_print ( 3, v1, '  V1:' )
      call r8vec_transpose_print ( 3, v2, '  V2:' )
      call r8vec_transpose_print ( 3, v3, '  V3:' )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'FACTOR   N   RESULT'

      do j = 1, test_num

        do i = 1, 3
          e(i) = e_test(i,j)
        end do

        call polyterm_exponent ( 'SET', e )

        call polyterm_exponent ( 'PRINT', e )

        factor = 2**11
        call sphere01_triangle_quad_icos2v ( v1, v2, v3, factor, 
     &    polyterm_value_3d, node_num, best )
      
        factor = 1
        do factor_log = 0, 9

          call sphere01_triangle_quad_icos2v ( v1, v2, v3, factor, 
     &      polyterm_value_3d, node_num, result )

          error = abs ( result - best )

          write ( *, '(2x,i4,2x,i8,2x,g16.8,2x,g10.2)' ) 
     &      factor, node_num, result, error

          factor = factor * 2

        end do

      end do

      return
      end
      subroutine polyterm_exponent ( action, e )

c*********************************************************************72
c
cc POLYTERM_EXPONENT gets or sets the exponents for the polynomial term.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    21 April 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, character ( len = 3 ) ACTION.
c    'GET' asks the routine to return the current values in E.
c    'SET' asks the routine to set the current values to E.
c
c    Input/output, integer E(3), storage used to set or get values.
c
      implicit none

      character ( len = * )  action
      integer e(3)
      integer, save, dimension ( 3 ) :: e_save = (/ 0, 0, 0 /)
      character ( len = 80 ) text
      character ( len = 80 ) text2

      if ( action(1:1) .eq. 'G' ) then

        e(1:3) = e_save(1:3)

      else if ( action(1:1) .eq. 'P' ) then

        write ( *, * ) ' '

        if ( e_save(1) .eq. 0 .and. 
     &       e_save(2) .eq. 0 .and. 
     &       e_save(3) .eq. 0 ) then

          text = 'P(X,Y,Z) = 1'

        else

          text = 'P(X,Y,Z) = '

          if ( e_save(1) .eq. 0 ) then

          else if ( e_save(1) .eq. 1 ) then

            call s_cat ( text, ' X', text )

          else

            call s_cat ( text, ' X^', text )

            write ( text2, '(i2)' ) e_save(1)
            text2 = adjustl ( text2 )
            call s_cat ( text, text2, text )

          end if

          if ( e_save(2) .eq. 0 ) then

          else if ( e_save(2) .eq. 1 ) then

            call s_cat ( text, ' Y', text )

          else

            call s_cat ( text, ' Y^', text )

            write ( text2, '(i2)' ) e_save(2)
            text2 = adjustl ( text2 )
            call s_cat ( text, text2, text )

          end if
       
          if ( e_save(3) .eq. 0 ) then

          else if ( e_save(3) .eq. 1 ) then

            call s_cat ( text, ' Z', text )

          else

            call s_cat ( text, ' Z^', text )

            write ( text2, '(i2)' ) e_save(3)
            text2 = adjustl ( text2 )
            call s_cat ( text, text2, text )

          end if
 
        end if

        write ( *, '(a)' ) trim ( text )
    
      else if ( action(1:1) .eq. 'S' ) then

        e_save(1:3) = e(1:3)

      end if

      return
      end
      function polyterm_value_3d ( x )

c*********************************************************************72
c
cc POLYTERM_VALUE_3D evaluates a single polynomial term in 3D.
c
c  Discussion:
c
c    The polynomial term has the form:
c
c      X(1)^E(1) * X(2)^E(2) * X(3)^E(3)
c
c    The exponents E(1:3) are set by calling POLYTERM_EXPONENT_SET.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    21 April 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision X(3), the point where the polynomial term 
c    is to be evaluated.
c
c    Output, double precision POLYTERM_VALUE_3D, the value of the 
c    polynomial term.
c
      implicit none

      integer e(3)
      double precision factor
      integer i
      double precision polyterm_value_3d
      double precision value
      double precision x(3)

      call polyterm_exponent ( 'GET', e )

      value = 1.0D+00

      do i = 1, 3

        if ( e(i) .eq. 0 ) then
          factor = 1.0D+00
        else if ( e(i) .eq. 1 ) then
          factor = x(i)
        else if ( x(i) .eq. 0.0D+00 ) then
          factor = 0.0D+00
        else
          factor = x(i)**e(i)
        end if

        value = value * factor

      end do

      polyterm_value_3d = value
  
      return
      end
