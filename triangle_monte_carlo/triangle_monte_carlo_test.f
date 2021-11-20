      program main

c*********************************************************************72
c
cc MAIN is the main program for TRIANGLE_MONTE_CARLO_PRB.
c
c  Discussion:
c
c    TRIANGLE_MONTE_CARLO_PRB tests the TRIANGLE_MONTE_CARLO library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 January 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TRIANGLE_MONTE_CARLO_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the TRIANGLE_MONTE_CARLO library.'
c
c  Try each sampler on the unit triangle, integrating X^2, X*Y, Y^2.
c
      call test01 ( )
      call test02 ( )
      call test03 ( )
      call test04 ( )
c
c  Try each sampler on a general triangle, integrating a selection of functions.
c
      call test05 ( )
      call test06 ( )
      call test07 ( )
      call test08 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TRIANGLE_MONTE_CARLO_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 uses TRIANGLE_SAMPLE_01 with an increasing number of points.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 January 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer f_num
      parameter ( f_num = 3 )

      integer p_num
      double precision result(f_num)
      integer seed
      double precision t(2,3)
      external triangle_integrand_03
      external triangle_unit_sample_01

      save t

      data t /
     &  1.0D+00, 0.0D+00, 
     &  0.0D+00, 1.0D+00, 
     &  0.0D+00, 0.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  Sample using TRIANGLE_UNIT_SAMPLE_01'
      write ( *, '(a)' ) '  Integrate TRIANGLE_UNIT_INTEGRAND_03'
      write ( *, '(a)' ) '  Integration region is the unit triangle.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Use an increasing number of points P_NUM.'
      write ( *, '(a)' ) 
     &  '  Note that the sample routine is a "bad" sampler.'

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '     P_NUM      X^2             X*Y             Y^2'
      write ( *, '(a)' ) ' '

      p_num = 1

10    continue

      if ( p_num .le. 65536 ) then

        call triangle_monte_carlo ( t, p_num, f_num, 
     &    triangle_unit_sample_01, triangle_integrand_03, seed,
     &    result )

        write ( *, '(2x,i8,3(2x,g14.6))' ) p_num, result(1:f_num)

        p_num = 2 * p_num

        go to 10

      end if

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 uses TRIANGLE_SAMPLE_02 with an increasing number of points.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 January 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer f_num
      parameter ( f_num = 3 )

      integer p_num
      double precision result(f_num)
      integer seed
      double precision t(2,3)
      external triangle_integrand_03
      external triangle_unit_sample_02

      save t

      data t /
     &  1.0D+00, 0.0D+00, 
     &  0.0D+00, 1.0D+00, 
     &  0.0D+00, 0.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) '  Sample using TRIANGLE_UNIT_SAMPLE_02'
      write ( *, '(a)' ) '  Integrate TRIANGLE_UNIT_INTEGRAND_03'
      write ( *, '(a)' ) '  Integration region is the unit triangle.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Use an increasing number of points P_NUM.'
      write ( *, '(a)' ) 
     &  '  Note that the sample routine is a good" sampler.'

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '     P_NUM      X^2             X*Y             Y^2'
      write ( *, '(a)' ) ' '

      p_num = 1

10    continue

      if ( p_num .le. 65536 ) then

        call triangle_monte_carlo ( t, p_num, f_num, 
     &    triangle_unit_sample_02, triangle_integrand_03, seed, 
     &    result )

        write ( *, '(2x,i8,3(2x,g14.6))' ) p_num, result(1:f_num)

        p_num = 2 * p_num

        go to 10

      end if

      return
      end
      subroutine test03 ( )

c*********************************************************************72
c
cc TEST03 uses TRIANGLE_SAMPLE_03 with an increasing number of points.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 January 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer f_num
      parameter ( f_num = 3 )

      integer p_num
      double precision result(f_num)
      integer seed
      double precision t(2,3)
      external triangle_integrand_03
      external triangle_unit_sample_03

      save t

      data t /
     &  1.0D+00, 0.0D+00, 
     &  0.0D+00, 1.0D+00, 
     &  0.0D+00, 0.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST03'
      write ( *, '(a)' ) '  Sample using TRIANGLE_UNIT_SAMPLE_03'
      write ( *, '(a)' ) '  Integrate TRIANGLE_UNIT_INTEGRAND_03'
      write ( *, '(a)' ) '  Integration region is the unit triangle.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Use an increasing number of points P_NUM.'
      write ( *, '(a)' ) 
     &  '  Note that the sample routine is a good" sampler.'

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '     P_NUM      X^2             X*Y             Y^2'
      write ( *, '(a)' ) ' '

      p_num = 1

10    continue

      if ( p_num .le. 65536 ) then

        call triangle_monte_carlo ( t, p_num, f_num, 
     &    triangle_unit_sample_03, triangle_integrand_03, seed, 
     &    result )

        write ( *, '(2x,i8,3(2x,g14.6))' ) p_num, result(1:f_num)

        p_num = 2 * p_num

        go to 10

      end if

      return
      end
      subroutine test04 ( )

c*********************************************************************72
c
cc TEST04 uses TRIANGLE_SAMPLE_04 with an increasing number of points.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 January 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer f_num
      parameter ( f_num = 3 )

      integer p_num
      double precision result(f_num)
      integer seed
      double precision t(2,3)
      external triangle_integrand_03
      external triangle_unit_sample_04

      save t

      data t /
     &  1.0D+00, 0.0D+00, 
     &  0.0D+00, 1.0D+00, 
     &  0.0D+00, 0.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST04'
      write ( *, '(a)' ) '  Sample using TRIANGLE_UNIT_SAMPLE_04'
      write ( *, '(a)' ) '  Integrate TRIANGLE_UNIT_INTEGRAND_03'
      write ( *, '(a)' ) '  Integration region is the unit triangle.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Use an increasing number of points P_NUM.'
      write ( *, '(a)' ) 
     &  '  Note that the sample routine is a good" sampler.'

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '     P_NUM      X^2             X*Y             Y^2'
      write ( *, '(a)' ) ' '

      p_num = 1

10    continue

      if ( p_num .le. 65536 ) then

        call triangle_monte_carlo ( t, p_num, f_num, 
     &    triangle_unit_sample_04, triangle_integrand_03, seed, 
     &    result )

        write ( *, '(2x,i8,3(2x,g14.6))' ) p_num, result(1:f_num)

        p_num = 2 * p_num

        go to 10

      end if

      return
      end
      subroutine test05 ( )

c*********************************************************************72
c
cc TEST05 uses TRIANGLE_SAMPLE_01 on a general triangle.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 January 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer f_num
      parameter ( f_num = 8 )

      integer p_num
      double precision result(f_num)
      integer seed
      double precision t(2,3)
      external triangle_integrand_user
      external triangle_unit_sample_01

      save t

      data t /
     &  4.0D+00, 1.0D+00, 
     &  8.0D+00, 3.0D+00, 
     &  0.0D+00, 9.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST05'
      write ( *, '(a)' ) '  Sample using TRIANGLE_UNIT_SAMPLE_01'
      write ( *, '(a)' ) '  Integrate TRIANGLE_UNIT_INTEGRAND_USER'
      write ( *, '(a)' ) 
     &  '  Integration region is over a general triangle.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Use an increasing number of points P_NUM.'
      write ( *, '(a)' ) 
     &  '  Note that the sample routine is a "bad" sampler.'

      seed = 123456789

      call r8mat_transpose_print ( 2, 3, t, '  Triangle vertices:' )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     P_NUM'
      write ( *, '(a)' ) ' '

      p_num = 1

10    continue

      if ( p_num .le. 65536 ) then

        call triangle_monte_carlo ( t, p_num, f_num, 
     &    triangle_unit_sample_01, triangle_integrand_user, seed, 
     &    result )

        write ( *, '(2x,i8,8(2x,f12.6))' ) p_num, result(1:f_num)

        p_num = 2 * p_num

        go to 10

      end if

      return
      end
      subroutine test06 ( )

c*********************************************************************72
c
cc TEST06 uses TRIANGLE_SAMPLE_02 on a general triangle.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 January 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer f_num
      parameter ( f_num = 8 )

      integer p_num
      double precision result(f_num)
      integer seed
      double precision t(2,3)
      external triangle_integrand_user
      external triangle_unit_sample_02

      save t

      data t /
     &  4.0D+00, 1.0D+00, 
     &  8.0D+00, 3.0D+00, 
     &  0.0D+00, 9.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST06'
      write ( *, '(a)' ) '  Sample using TRIANGLE_UNIT_SAMPLE_02'
      write ( *, '(a)' ) '  Integrate TRIANGLE_UNIT_INTEGRAND_USER'
      write ( *, '(a)' ) 
     &  '  Integration region is over a general triangle.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Use an increasing number of points P_NUM.'
      write ( *, '(a)' ) 
     &  '  Note that the sample routine is a "good" sampler.'

      seed = 123456789

      call r8mat_transpose_print ( 2, 3, t, '  Triangle vertices:' )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     P_NUM'
      write ( *, '(a)' ) ' '

      p_num = 1

10    continue

      if ( p_num .le. 65536 ) then

        call triangle_monte_carlo ( t, p_num, f_num, 
     &    triangle_unit_sample_02, triangle_integrand_user, seed, 
     &    result )

        write ( *, '(2x,i8,8(2x,f12.6))' ) p_num, result(1:f_num)

        p_num = 2 * p_num

        go to 10

      end if

      return
      end
      subroutine test07 ( )

c*********************************************************************72
c
cc TEST07 uses TRIANGLE_SAMPLE_03 on a general triangle.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 January 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer f_num
      parameter ( f_num = 8 )

      integer p_num
      double precision result(f_num)
      integer seed
      double precision t(2,3)
      external triangle_integrand_user
      external triangle_unit_sample_03

      save t

      data t /
     &  4.0D+00, 1.0D+00, 
     &  8.0D+00, 3.0D+00, 
     &  0.0D+00, 9.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST07'
      write ( *, '(a)' ) '  Sample using TRIANGLE_UNIT_SAMPLE_03'
      write ( *, '(a)' ) '  Integrate TRIANGLE_UNIT_INTEGRAND_USER'
      write ( *, '(a)' ) 
     &  '  Integration region is over a general triangle.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Use an increasing number of points P_NUM.'
      write ( *, '(a)' ) 
     &  '  Note that the sample routine is a "good" sampler.'

      seed = 123456789

      call r8mat_transpose_print ( 2, 3, t, '  Triangle vertices:' )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     P_NUM'
      write ( *, '(a)' ) ' '

      p_num = 1

10    continue

      if ( p_num .le. 65536 ) then

        call triangle_monte_carlo ( t, p_num, f_num, 
     &    triangle_unit_sample_03, triangle_integrand_user, seed, 
     &    result )

        write ( *, '(2x,i8,8(2x,f12.6))' ) p_num, result(1:f_num)

        p_num = 2 * p_num

        go to 10

      end if

      return
      end
      subroutine test08 ( )

c*********************************************************************72
c
cc TEST08 uses TRIANGLE_SAMPLE_04 on a general triangle.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 January 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer f_num
      parameter ( f_num = 8 )

      integer p_num
      double precision result(f_num)
      integer seed
      double precision t(2,3)
      external triangle_integrand_user
      external triangle_unit_sample_04

      save t

      data t /
     &  4.0D+00, 1.0D+00, 
     &  8.0D+00, 3.0D+00, 
     &  0.0D+00, 9.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST08'
      write ( *, '(a)' ) '  Sample using TRIANGLE_UNIT_SAMPLE_04'
      write ( *, '(a)' ) '  Integrate TRIANGLE_UNIT_INTEGRAND_USER'
      write ( *, '(a)' ) 
     &  '  Integration region is over a general triangle.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Use an increasing number of points P_NUM.'
      write ( *, '(a)' ) 
     &  '  Note that the sample routine is a "good" sampler.'

      seed = 123456789

      call r8mat_transpose_print ( 2, 3, t, '  Triangle vertices:' )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     P_NUM'
      write ( *, '(a)' ) ' '

      p_num = 1

10    continue

      if ( p_num .le. 65536 ) then

        call triangle_monte_carlo ( t, p_num, f_num, 
     &    triangle_unit_sample_04, triangle_integrand_user, seed, 
     &    result )

        write ( *, '(2x,i8,8(2x,f12.6))' ) p_num, result(1:f_num)

        p_num = 2 * p_num

        go to 10

      end if

      return
      end
      subroutine triangle_integrand_user ( p_num, p, f_num, fp )

c*********************************************************************72
c
cc TRIANGLE_INTEGRAND_USER evaluates 8 integrand functions defined by the user.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 January 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer P_NUM, the number of points.
c
c    Input, double precision P(2,P_NUM), the evaluation points.
c
c    Input, integer F_NUM, the number of integrands.
c
c    Output, double precision FP(F_NUM,P_NUM), the integrand values.
c
      implicit none

      integer f_num
      integer p_num

      double precision fp(f_num,p_num)
      integer j
      double precision p(2,p_num)

      do j = 1, p_num
        fp(1,j) = 1.0D+00
        fp(2,j) = p(1,j)
        fp(3,j) =             p(2,j)
        fp(4,j) = p(1,j)**2
        fp(5,j) = p(1,j)    * p(2,j)
        fp(6,j) =             p(2,j)**2
        fp(7,j) = p(1,j)**2 * p(2,j)
        fp(8,j) = p(1,j)**2 * p(2,j)**2
      end do

      return
      end

