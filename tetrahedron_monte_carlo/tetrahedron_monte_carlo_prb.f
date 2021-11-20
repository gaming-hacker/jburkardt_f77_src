      program main

c*********************************************************************72
c
cc MAIN is the main program for TETRAHEDRON_MONTE_CARLO_PRB.
c
c  Discussion:
c
c    TETRAHEDRON_MONTE_CARLO_PRB tests the TETRAHEDRON_MONTE_CARLO library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 January 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TETRAHEDRON_MONTE_CARLO_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the TETRAHEDRON_MONTE_CARLO library.'
c
c  Try each sampler on the unit tetrahedron, integrating quadratics.
c
      call test01 ( )
      call test02 ( )
      call test03 ( )
      call test04 ( )
c
c  Try each sampler on a general tetrahedron, integrating a selection of functions.
c
      call test05 ( )
      call test06 ( )
      call test07 ( )
      call test08 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TETRAHEDRON_MONTE_CARLO_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 uses TETRAHEDRON_SAMPLE_01 with an increasing number of points.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 January 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer f_num
      parameter ( f_num = 6 )

      integer p_num
      double precision result(f_num)
      integer seed
      double precision t(3,4)
      external tetrahedron_integrand_03
      external tetrahedron_unit_sample_01

      save t

      data t /
     &  1.0D+00, 0.0D+00, 0.0D+00,
     &  0.0D+00, 1.0D+00, 0.0D+00,
     &  0.0D+00, 0.0D+00, 1.0D+00,
     &  0.0D+00, 0.0D+00, 0.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  Sample using TETRAHEDRON_UNIT_SAMPLE_01'
      write ( *, '(a)' ) '  Integrate TETRAHEDRON_UNIT_INTEGRAND_03'
      write ( *, '(a)' ) '  Integration region is the unit tetrahedron.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Use an increasing number of points P_NUM.'
      write ( *, '(a)' ) '  This sample routine is bad.'

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '     P_NUM      X^2             X*Y             X*Z' //
     &  '             Y^2             Y*Z             Z^2'
      write ( *, '(a)' ) ' '

      p_num = 1

10    continue

      if ( p_num .le. 65536 ) then

        call tetrahedron_monte_carlo ( t, p_num, f_num, 
     &    tetrahedron_unit_sample_01, tetrahedron_integrand_03, 
     &    seed, result )

        write ( *, '(2x,i8,6(2x,g14.6))' ) p_num, result(1:f_num)

        p_num = 2 * p_num

        go to 10

      end if

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 uses TETRAHEDRON_SAMPLE_02 with an increasing number of points.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 January 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer f_num
      parameter ( f_num = 6 )

      integer p_num
      double precision result(f_num)
      integer seed
      double precision t(3,4)
      external tetrahedron_integrand_03
      external tetrahedron_unit_sample_02

      save t

      data t /
     &  1.0D+00, 0.0D+00, 0.0D+00,
     &  0.0D+00, 1.0D+00, 0.0D+00,
     &  0.0D+00, 0.0D+00, 1.0D+00,
     &  0.0D+00, 0.0D+00, 0.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) '  Sample using TETRAHEDRON_UNIT_SAMPLE_02'
      write ( *, '(a)' ) '  Integrate TETRAHEDRON_UNIT_INTEGRAND_03'
      write ( *, '(a)' ) '  Integration region is the unit tetrahedron.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Use an increasing number of points P_NUM.'
      write ( *, '(a)' ) '  The sample routine is good.'

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '     P_NUM      X^2             X*Y             X*Z' //
     &  '             Y^2             Y*Z             Z^2'
      write ( *, '(a)' ) ' '

      p_num = 1

10    continue

      if ( p_num .le. 65536 ) then

        call tetrahedron_monte_carlo ( t, p_num, f_num, 
     &    tetrahedron_unit_sample_02, tetrahedron_integrand_03, 
     &    seed, result )

        write ( *, '(2x,i8,6(2x,g14.6))' ) p_num, result(1:f_num)

        p_num = 2 * p_num

        go to 10

      end if

      return
      end
      subroutine test03 ( )

c*********************************************************************72
c
cc TEST03 uses TETRAHEDRON_SAMPLE_03 with an increasing number of points.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 January 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer f_num
      parameter ( f_num = 6 )

      integer p_num
      double precision result(f_num)
      integer seed
      double precision t(3,4)
      external tetrahedron_integrand_03
      external tetrahedron_unit_sample_03

      save t

      data t /
     &  1.0D+00, 0.0D+00, 0.0D+00,
     &  0.0D+00, 1.0D+00, 0.0D+00,
     &  0.0D+00, 0.0D+00, 1.0D+00,
     &  0.0D+00, 0.0D+00, 0.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST03'
      write ( *, '(a)' ) '  Sample using TETRAHEDRON_UNIT_SAMPLE_03'
      write ( *, '(a)' ) '  Integrate TETRAHEDRON_UNIT_INTEGRAND_03'
      write ( *, '(a)' ) '  Integration region is the unit tetrahedron.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Use an increasing number of points P_NUM.'
      write ( *, '(a)' ) '  The sample routine is good.'

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '     P_NUM      X^2             X*Y             X*Z' //
     &  '             Y^2             Y*Z             Z^2'
      write ( *, '(a)' ) ' '

      p_num = 1

10    continue

      if ( p_num .le. 65536 ) then

        call tetrahedron_monte_carlo ( t, p_num, f_num, 
     &    tetrahedron_unit_sample_03, tetrahedron_integrand_03, 
     &    seed, result )

        write ( *, '(2x,i8,6(2x,g14.6))' ) p_num, result(1:f_num)

        p_num = 2 * p_num

        go to 10

      end if

      return
      end
      subroutine test04 ( )

c*********************************************************************72
c
cc TEST04 uses TETRAHEDRON_SAMPLE_04 with an increasing number of points.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 January 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer f_num
      parameter ( f_num = 6 )

      integer p_num
      double precision result(f_num)
      integer seed
      double precision t(3,4)
      external tetrahedron_integrand_03
      external tetrahedron_unit_sample_04

      save t

      data t /
     &  1.0D+00, 0.0D+00, 0.0D+00,
     &  0.0D+00, 1.0D+00, 0.0D+00,
     &  0.0D+00, 0.0D+00, 1.0D+00,
     &  0.0D+00, 0.0D+00, 0.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST04'
      write ( *, '(a)' ) '  Sample using TETRAHEDRON_UNIT_SAMPLE_04'
      write ( *, '(a)' ) '  Integrate TETRAHEDRON_UNIT_INTEGRAND_03'
      write ( *, '(a)' ) '  Integration region is the unit tetrahedron.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Use an increasing number of points P_NUM.'
      write ( *, '(a)' ) '  The sample routine is good.'

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '     P_NUM      X^2             X*Y             X*Z' //
     &  '             Y^2             Y*Z             Z^2'
      write ( *, '(a)' ) ' '

      p_num = 1

10    continue

      if ( p_num .le. 65536 ) then

        call tetrahedron_monte_carlo ( t, p_num, f_num, 
     &    tetrahedron_unit_sample_04, tetrahedron_integrand_03, 
     &    seed, result )

        write ( *, '(2x,i8,6(2x,g14.6))' ) p_num, result(1:f_num)

        p_num = 2 * p_num

        go to 10

      end if

      return
      end
      subroutine test05 ( )

c*********************************************************************72
c
cc TEST05 uses TETRAHEDRON_SAMPLE_01 on a general tetrahedron.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 January 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer f_num
      parameter ( f_num = 6 )

      integer p_num
      double precision result(f_num)
      integer seed
      double precision t(3,4)
      external tetrahedron_integrand_user
      external tetrahedron_unit_sample_01

      save t

      data t /
     &  1.0D+00, 2.0D+00, 3.0D+00,
     &  4.0D+00, 1.0D+00, 2.0D+00,
     &  2.0D+00, 4.0D+00, 4.0D+00,
     &  3.0D+00, 2.0D+00, 5.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST05'
      write ( *, '(a)' ) '  Sample using TETRAHEDRON_UNIT_SAMPLE_01'
      write ( *, '(a)' ) '  Integrate TETRAHEDRON_UNIT_INTEGRAND_USER'
      write ( *, '(a)' ) 
     &  '  Integration region is over a general tetrahedron.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Use an increasing number of points P_NUM.'
      write ( *, '(a)' ) '  The sample routine is bad.'

      seed = 123456789

      call r8mat_transpose_print ( 3, 4, t, '  Tetrahedron vertices:' )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     P_NUM'
      write ( *, '(a)' ) ' '

      p_num = 1

10    continue

      if ( p_num .le. 65536 ) then

        call tetrahedron_monte_carlo ( t, p_num, f_num, 
     &    tetrahedron_unit_sample_01, tetrahedron_integrand_user, 
     &    seed, result )

        write ( *, '(2x,i8,6(2x,f12.6))' ) p_num, result(1:f_num)

        p_num = 2 * p_num

        go to 10

      end if

      return
      end
      subroutine test06 ( )

c*********************************************************************72
c
cc TEST06 uses TETRAHEDRON_SAMPLE_02 on a general tetrahedron.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 January 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer f_num
      parameter ( f_num = 6 )

      integer p_num
      double precision result(f_num)
      integer seed
      double precision t(3,4)
      external tetrahedron_integrand_user
      external tetrahedron_unit_sample_02

      save t

      data t /
     &  1.0D+00, 2.0D+00, 3.0D+00,
     &  4.0D+00, 1.0D+00, 2.0D+00,
     &  2.0D+00, 4.0D+00, 4.0D+00,
     &  3.0D+00, 2.0D+00, 5.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST06'
      write ( *, '(a)' ) '  Sample using TETRAHEDRON_UNIT_SAMPLE_02'
      write ( *, '(a)' ) '  Integrate TETRAHEDRON_UNIT_INTEGRAND_USER'
      write ( *, '(a)' ) 
     &  '  Integration region is over a general tetrahedron.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Use an increasing number of points P_NUM.'
      write ( *, '(a)' ) '  The sample routine is good.'

      seed = 123456789

      call r8mat_transpose_print ( 3, 4, t, '  Tetrahedron vertices:' )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     P_NUM'
      write ( *, '(a)' ) ' '

      p_num = 1

10    continue

      if ( p_num .le. 65536 ) then

        call tetrahedron_monte_carlo ( t, p_num, f_num, 
     &    tetrahedron_unit_sample_02, tetrahedron_integrand_user, 
     &    seed, result )

        write ( *, '(2x,i8,6(2x,f12.6))' ) p_num, result(1:f_num)

        p_num = 2 * p_num

        go to 10

      end if

      return
      end
      subroutine test07 ( )

c*********************************************************************72
c
cc TEST07 uses TETRAHEDRON_SAMPLE_03 on a general tetrahedron.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 January 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer f_num
      parameter ( f_num = 6 )

      integer p_num
      double precision result(f_num)
      integer seed
      double precision t(3,4)
      external tetrahedron_integrand_user
      external tetrahedron_unit_sample_03

      save t

      data t /
     &  1.0D+00, 2.0D+00, 3.0D+00,
     &  4.0D+00, 1.0D+00, 2.0D+00,
     &  2.0D+00, 4.0D+00, 4.0D+00,
     &  3.0D+00, 2.0D+00, 5.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST07'
      write ( *, '(a)' ) '  Sample using TETRAHEDRON_UNIT_SAMPLE_03'
      write ( *, '(a)' ) '  Integrate TETRAHEDRON_UNIT_INTEGRAND_USER'
      write ( *, '(a)' ) 
     &  '  Integration region is over a general tetrahedron.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Use an increasing number of points P_NUM.'
      write ( *, '(a)' ) '  The sample routine is good.'

      seed = 123456789

      call r8mat_transpose_print ( 3, 4, t, '  Tetrahedron vertices:' )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     P_NUM'
      write ( *, '(a)' ) ' '

      p_num = 1

10    continue

      if ( p_num .le. 65536 ) then

        call tetrahedron_monte_carlo ( t, p_num, f_num, 
     &    tetrahedron_unit_sample_03, tetrahedron_integrand_user, 
     &    seed, result )

        write ( *, '(2x,i8,6(2x,f12.6))' ) p_num, result(1:f_num)

        p_num = 2 * p_num

        go to 10

      end if

      return
      end
      subroutine test08 ( )

c*********************************************************************72
c
cc TEST08 uses TETRAHEDRON_SAMPLE_04 on a general tetrahedron.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 January 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer f_num
      parameter ( f_num = 6 )

      integer p_num
      double precision result(f_num)
      integer seed
      double precision t(3,4)
      external tetrahedron_integrand_user
      external tetrahedron_unit_sample_04

      save t

      data t /
     &  1.0D+00, 2.0D+00, 3.0D+00,
     &  4.0D+00, 1.0D+00, 2.0D+00,
     &  2.0D+00, 4.0D+00, 4.0D+00,
     &  3.0D+00, 2.0D+00, 5.0D+00 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST08'
      write ( *, '(a)' ) '  Sample using TETRAHEDRON_UNIT_SAMPLE_04'
      write ( *, '(a)' ) '  Integrate TETRAHEDRON_UNIT_INTEGRAND_USER'
      write ( *, '(a)' ) 
     &  '  Integration region is over a general tetrahedron.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Use an increasing number of points P_NUM.'
      write ( *, '(a)' ) '  The sample routine is good.'

      seed = 123456789

      call r8mat_transpose_print ( 3, 4, t, '  Tetrahedron vertices:' )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     P_NUM'
      write ( *, '(a)' ) ' '

      p_num = 1

10    continue

      if ( p_num .le. 65536 ) then

        call tetrahedron_monte_carlo ( t, p_num, f_num, 
     &    tetrahedron_unit_sample_04, tetrahedron_integrand_user, 
     &    seed, result )

        write ( *, '(2x,i8,6(2x,f12.6))' ) p_num, result(1:f_num)

        p_num = 2 * p_num

        go to 10

      end if

      return
      end
      subroutine tetrahedron_integrand_user ( p_num, p, f_num, fp )

c*********************************************************************72
c
cc TETRAHEDRON_INTEGRAND_USER evaluates 6 integrand functions defined by the user.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 January 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer P_NUM, the number of points.
c
c    Input, double precision P(3,P_NUM), the evaluation points.
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
      double precision p(3,p_num)

      do j = 1, p_num
        fp(1,j) = 1.0D+00
        fp(2,j) = p(1,j)
        fp(3,j) =             p(2,j)**2
        fp(4,j) =                         p(3,j)**3
        fp(5,j) = p(1,j)    * p(2,j)    * p(3,j)**2
        fp(6,j) = p(1,j)**2 * p(2,j)**2 * p(3,j)**2
      end do

      return
      end

