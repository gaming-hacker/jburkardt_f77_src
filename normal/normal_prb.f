      program main

c*********************************************************************72
c
cc MAIN is the main program for NORMAL_PRB.
c
c  Discussion:
c
c    NORMAL_PRB tests the NORMAL library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'NORMAL_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version.'
      write ( *, '(a)' ) '  Test the NORMAL library.'

      call c4_normal_01_test ( )
      call c8_normal_01_test ( )
      call i4_normal_ab_test ( )
      call r4_normal_01_test ( )
      call r4_normal_ab_test ( )
      call r4_uniform_01_test ( )
      call r8_normal_01_test ( )
      call r8_normal_ab_test ( )
      call r8_uniform_01_test ( )
      call r8mat_normal_01_test ( )
      call r8mat_normal_ab_test ( )
      call r8vec_normal_01_test ( )
      call r8vec_normal_ab_test ( )
      call r8vec_uniform_01_test ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'NORMAL_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine c4_normal_01_test ( )

c*********************************************************************72
c
cc C4_NORMAL_01_TEST tests C4_NORMAL_01.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      complex c4_normal_01
      integer i
      complex r
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'C4_NORMAL_01_TEST'
      write ( *, '(a)' ) '  C4_NORMAL_01 computes pseudorandom'
      write ( *, '(a)' ) '  complex values normally distributed '
      write ( *, '(a)' ) '  in the unit circle.'

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a,i12)' ) '  SEED = ', seed
      write ( *, '(a)' ) ' '

      do i = 1, 10
        r = c4_normal_01 ( seed )
        write ( *, '(2x,i8,2x,2g14.6)' ) i, r
      end do

      return
      end
      subroutine c8_normal_01_test ( )

c*********************************************************************72
c
cc C8_NORMAL_01_TEST tests C8_NORMAL_01.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double complex c8_normal_01
      integer i
      double complex r
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'C8_NORMAL_01_TEST'
      write ( *, '(a)' ) '  C8_NORMAL_01 computes pseudorandom '
      write ( *, '(a)' ) '  double precision complex values '
      write ( *, '(a)' ) '  normally distributed in the unit circle.'

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a,i12)' ) '  SEED = ', seed
      write ( *, '(a)' ) ' '

      do i = 1, 10
        r = c8_normal_01 ( seed )
        write ( *, '(2x,i6,2x,2g14.6)' ) i, r
      end do

      return
      end
      subroutine i4_normal_ab_test ( )

c*********************************************************************72
c
cc I4_NORMAL_AB_TEST tests I4_NORMAL_AB.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer i
      integer i4_normal_ab
      real mu
      integer r
      integer seed
      real sigma

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'I4_NORMAL_AB_TEST'
      write ( *, '(a)' ) '  I4_NORMAL_AB computes integer pseudorandom'
      write ( *, '(a)' ) '  values in an interval [A,B].'

      mu = 70.0E+00
      sigma = 10.0E+00
      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  MU = ', mu
      write ( *, '(a,g14.6)' ) '  SIGMA = ', sigma
      write ( *, '(a,i12)' ) '  SEED = ', seed
      write ( *, '(a)' ) ' '

      do i = 1, 10
        r = i4_normal_ab ( mu, sigma, seed )
        write ( *, '(2x,i8,2x,i8)' ) i, r
      end do

      return
      end
      subroutine r4_normal_01_test ( )

c*********************************************************************72
c
cc R4_NORMAL_01_TEST tests R4_NORMAL_01.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer i
      real r
      real r4_normal_01
      integer seed
 
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R4_NORMAL_01_TEST'
      write ( *, '(a)' ) '  R4_NORMAL_01 computes normal pseudorandom'
      write ( *, '(a)' ) '  values in the interval [0,1].'

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a,i12)' ) '  SEED = ', seed
      write ( *, '(a)' ) ' '

      do i = 1, 10
        r = r4_normal_01 ( seed )
        write ( *, '(2x,i6,2x,g14.6)' ) i, r
      end do

      return
      end
      subroutine r4_normal_ab_test ( )

c*********************************************************************72
c
cc R4_NORMAL_AB_TEST tests R4_NORMAL_AB.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 August 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer i
      real mu
      real r
      real r4_normal_ab
      integer seed
      real sigma

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R4_NORMAL_AB_TEST'
      write ( *, '(a)' ) '  R4_NORMAL_AB computes real pseudorandom'
      write ( *, '(a)' ) '  values with mean MU and standard '
      write ( *, '(a)' ) '  deviation SIGMA.'

      mu = 10.0E+00
      sigma = 2.0E+00
      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  MU = ', mu
      write ( *, '(a,g14.6)' ) '  SIGMA = ', sigma
      write ( *, '(a,i12)' ) '  SEED = ', seed
      write ( *, '(a)' ) ' '

      do i = 1, 10
        r = r4_normal_ab ( mu, sigma, seed )
        write ( *, '(2x,i6,2x,g14.6)' ) i, r
      end do

      return
      end
      subroutine r4_uniform_01_test ( )

c*********************************************************************72
c
cc R4_UNIFORM_01_TEST tests R4_UNIFORM_01
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer i
      real r
      real r4_uniform_01
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R4_UNIFORM_01_TEST'
      write ( *, '(a)' ) '  R4_UNIFORM_01 samples a uniform random'
      write ( *, '(a)' ) '  distribution in [0,1].'

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a,i12)' ) '  SEED = ', seed
      write ( *, '(a)' ) ''

      do i = 1, 10
        r = r4_uniform_01 ( seed )
        write ( *, '(2x,i2,2x,g14.6)' ) i, r
      end do

      return
      end
      subroutine r8_normal_01_test ( )

c*********************************************************************72
c
cc R8_NORMAL_01_TEST tests R8_NORMAL_01.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer i
      double precision r
      double precision r8_normal_01
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_NORMAL_01_TEST'
      write ( *, '(a)' ) '  R8_NORMAL_01 computes pseudonormal values '
      write ( *, '(a)' ) '  with mean 0.0 and standard deviation 1.0.'

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a,i12)' ) '  SEED = ', seed
      write ( *, '(a)' ) ' '

      do i = 1, 10
        r = r8_normal_01 ( seed )
        write ( *, '(2x,i6,2x,g14.6)' ) i, r
      end do

      return
      end
      subroutine r8_normal_ab_test ( )

c*********************************************************************72
c
cc R8_NORMAL_AB_TEST tests R8_NORMAL_AB.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer i
      double precision mu
      double precision r
      double precision r8_normal_ab
      integer seed
      double precision sigma

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_NORMAL_AB_TEST'
      write ( *, '(a)' ) '  R8_NORMAL_AB computes pseudonormal values '
      write ( *, '(a)' ) '  with mean MU and standard deviation SIGMA.'

      mu = 10.0D+00
      sigma = 2.0D+00
      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  MU = ', mu
      write ( *, '(a,g14.6)' ) '  SIGMA = ', sigma
      write ( *, '(a,i12)' ) '  SEED = ', seed
      write ( *, '(a)' ) ' '

      do i = 1, 10
        r = r8_normal_ab ( mu, sigma, seed )
        write ( *, '(2x,i6,2x,g14.6)' ) i, r
      end do

      return
      end
      subroutine r8_uniform_01_test ( )

c*********************************************************************72
c
cc R8_UNIFORM_01_TEST tests R8_UNIFORM_01
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer i
      double precision r
      double precision r8_uniform_01
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8_UNIFORM_01_TEST'
      write ( *, '(a)' ) '  R8_UNIFORM_01 samples a uniform random'
      write ( *, '(a)' ) '  distribution in [0,1].'

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a,i12)' ) '  SEED = ', seed
      write ( *, '(a)' ) ''

      do i = 1, 10
        r = r8_uniform_01 ( seed )
        write ( *, '(2x,i2,2x,g14.6)' ) i, r
      end do

      return
      end
      subroutine r8mat_normal_01_test ( )

c*********************************************************************72
c
cc R8MAT_NORMAL_01_TEST tests R8MAT_NORMAL_01.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 June 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      integer n

      parameter ( m = 5 )
      parameter ( n = 4 )

      double precision r(m,n)
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_NORMAL_01_TEST'
      write ( *, '(a)' ) 
     &  '  R8MAT_NORMAL_01 computes a matrix of values.'
 
      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a,i12)' ) '  SEED = ', seed

      call r8mat_normal_01 ( m, n, seed, r )

      call r8mat_print ( m, n, r, '  Matrix:' )
  
      return
      end
      subroutine r8mat_normal_ab_test ( )

c*********************************************************************72
c
cc R8MAT_NORMAL_AB_TEST tests R8MAT_NORMAL_AB.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 June 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      integer n

      parameter ( m = 5 )
      parameter ( n = 4 )

      double precision mu
      double precision r(m,n)
      integer seed
      double precision sigma

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8MAT_NORMAL_AB_TEST'
      write ( *, '(a)' ) 
     &  '  R8MAT_NORMAL_AB computes a matrix of NORMAL AB values.'
 
      mu = 100.0D+00
      sigma = 5.0D+00
      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  MU = ', mu
      write ( *, '(a,g14.6)' ) '  SIGMA = ', sigma
      write ( *, '(a,i12)' ) '  SEED = ', seed

      call r8mat_normal_ab ( m, n, mu, sigma, seed, r )

      call r8mat_print ( m, n, r, '  Matrix:' )
  
      return
      end
      subroutine r8vec_normal_01_test ( )

c*********************************************************************72
c
cc R8VEC_NORMAL_01_TEST tests R8VEC_NORMAL_01.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 June 2006
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )

      double precision r(n)
      double precision r8_normal_01
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_NORMAL_01_TEST'
      write ( *, '(a)' ) 
     &  '  R8VEC_NORMAL_01 computes a vector of values.'
      write ( *, '(a)' ) ' '

      seed = 123456789
      write ( *, '(a)' ) ' '
      write ( *, '(a,i12)' ) '  SEED = ', seed

      call r8vec_normal_01 ( n, seed, r )

      call r8vec_print ( n, r, '  Random vector:' )
  
      return
      end
      subroutine r8vec_normal_ab_test ( )

c*********************************************************************72
c
cc R8VEC_NORMAL_AB_TEST tests R8VEC_NORMAL_AB.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )

      double precision mu
      double precision r(n)
      double precision r8_normal_01
      integer seed
      double precision sigma

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_NORMAL_AB_TEST'
      write ( *, '(a)' ) 
     &  '  R8VEC_NORMAL_AB computes a vector of values.'
      write ( *, '(a)' ) ' '

      mu = 15.0D+00
      sigma = 0.25D+00
      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  MU = ', mu
      write ( *, '(a,g14.6)' ) '  SIGMA = ', sigma
      write ( *, '(a,i12)' ) '  SEED = ', seed

      call r8vec_normal_ab ( n, mu, sigma, seed, r )

      call r8vec_print ( n, r, '  Random vector:' )
  
      return
      end
      subroutine r8vec_uniform_01_test ( )

c*********************************************************************72
c
cc R8VEC_UNIFORM_01_TEST tests R8VEC_UNIFORM_01.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 March 2015
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10 )

      double precision r(n)
      integer seed

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'R8VEC_UNIFORM_01_TEST'
      write ( *, '(a)' ) '  R8VEC_UNIFORM_01 returns a random R8VEC '
      write ( *, '(a)' ) '  with entries in [0,1].'

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a,i12)' ) '  SEED = ', seed

      call r8vec_uniform_01 ( n, seed, r )

      call r8vec_print ( n, r, '  Random R8VEC:' )

      return
      end
