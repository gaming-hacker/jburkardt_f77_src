      program main

c*********************************************************************72
c
cc MAIN is the main program for WAVELET_PRB.
c
c  Discussion:
c
c    WAVELET_PRB tests the WAVELET library.
c
c  Modified:
c
c    09 February 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'WAVELET_PRB:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the WAVELET library.'
c
c  Test transforms.
c
      call test01 ( )
      call test02 ( )
      call test03 ( )
      call test04 ( )
      call test05 ( )
      call test06 ( )
      call test07 ( )
      call test08 ( )
      call test09 ( )

      call test10 ( )
      call test11 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'WAVELET_PRB:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 tests DAUB2_TRANSFORM and DAUB2_TRANSFORM_INVERSE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 February 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 16 )

      double precision a_first
      double precision a_last
      integer i
      integer n
      integer seed
      double precision u(n_max)
      double precision v(n_max)
      double precision w(n_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) 
     &  '  DAUB2_TRANSFORM computes the DAUB2 transform of a vector.'
      write ( *, '(a)' ) '  DAUB2_TRANSFORM_INVERSE inverts it.'
c
c  Random data.
c
      n = 16
      seed = 123456789
      call r8vec_uniform_01 ( n, seed, u )

      call daub2_transform ( n, u, v )

      call daub2_transform_inverse ( n, v, w )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   i      U          D2(U)    D2inv(D2(U))'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i2,2x,f10.4,2x,f10.4,2x,f10.4)' )
     &    i, u(i), v(i), w(i)
      end do
c
c  Constant signal.
c
      n = 8
      do i = 1, n
        u(i) = 1.0D+00
      end do

      call daub2_transform ( n, u, v )

      call daub2_transform_inverse ( n, v, w )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   i      U          D2(U)    D2inv(D2(U))'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i2,2x,f10.4,2x,f10.4,2x,f10.4)' )
     &    i, u(i), v(i), w(i)
      end do
c
c  Linear signal.
c
      n = 16
      a_first = 1.0D+00
      a_last = dble ( n )
      call r8vec_linspace ( n, a_first, a_last, u )

      call daub2_transform ( n, u, v )

      call daub2_transform_inverse ( n, v, w )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   i      U          D2(U)    D2inv(D2(U))'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i2,2x,f10.4,2x,f10.4,2x,f10.4)' )
     &    i, u(i), v(i), w(i)
      end do
c
c  Quadratic data.
c
      n = 8
      u(1) = 25.0D+00
      u(2) = 16.0D+00
      u(3) = 9.0D+00
      u(4) = 4.0D+00
      u(5) = 1.0D+00
      u(6) = 0.0D+00
      u(7) = 1.0D+00
      u(8) = 4.0D+00

      call daub2_transform ( n, u, v )

      call daub2_transform_inverse ( n, v, w )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   i      U          D2(U)    D2inv(D2(U))'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i2,2x,f10.4,2x,f10.4,2x,f10.4)' )
     &    i, u(i), v(i), w(i)
      end do

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 tests DAUB4_TRANSFORM and DAUB4_TRANSFORM_INVERSE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 February 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 16 )

      double precision a_first
      double precision a_last
      integer i
      integer n
      integer seed
      double precision u(n_max)
      double precision v(n_max)
      double precision w(n_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) 
     &  '  DAUB4_TRANSFORM computes the DAUB4 transform of a vector.'
      write ( *, '(a)' ) '  DAUB4_TRANSFORM_INVERSE inverts it.'
c
c  Random data.
c
      n = 16
      seed = 123456789
      call r8vec_uniform_01 ( n, seed, u )

      call daub4_transform ( n, u, v )

      call daub4_transform_inverse ( n, v, w )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   i      U          D4(U)    D4inv(D4(U))'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i2,2x,f10.4,2x,f10.4,2x,f10.4)' )
     &    i, u(i), v(i), w(i)
      end do
c
c  Constant signal.
c
      n = 8
      do i = 1, n
        u(i) = 1.0D+00
      end do
      call daub4_transform ( n, u, v )

      call daub4_transform_inverse ( n, v, w )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   i      U          D4(U)    D4inv(D4(U))'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i2,2x,f10.4,2x,f10.4,2x,f10.4)' )
     &    i, u(i), v(i), w(i)
      end do
c
c  Linear signal.
c
      n = 16
      a_first = 1.0D+00
      a_last = dble ( n )
      call r8vec_linspace ( n, a_first, a_last, u )

      call daub4_transform ( n, u, v )

      call daub4_transform_inverse ( n, v, w )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   i      U          D4(U)    D4inv(D4(U))'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i2,2x,f10.4,2x,f10.4,2x,f10.4)' )
     &    i, u(i), v(i), w(i)
      end do
c
c  Quadratic data.
c
      n = 8
      u(1) = 25.0D+00
      u(2) = 16.0D+00
      u(3) = 9.0D+00
      u(4) = 4.0D+00
      u(5) = 1.0D+00
      u(6) = 0.0D+00
      u(7) = 1.0D+00
      u(8) = 4.0D+00

      call daub4_transform ( n, u, v )

      call daub4_transform_inverse ( n, v, w )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   i      U          D4(U)    D4inv(D4(U))'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i2,2x,f10.4,2x,f10.4,2x,f10.4)' )
     &    i, u(i), v(i), w(i)
      end do

      return
      end
      subroutine test03 ( )

c*********************************************************************72
c
cc TEST03 tests DAUB6_TRANSFORM and DAUB6_TRANSFORM_INVERSE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 February 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 16 )

      double precision a_first
      double precision a_last
      integer i
      integer n
      integer seed
      double precision u(n_max)
      double precision v(n_max)
      double precision w(n_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST03'
      write ( *, '(a)' ) 
     &  '  DAUB6_TRANSFORM computes the DAUB6 transform of a vector.'
      write ( *, '(a)' ) '  DAUB6_TRANSFORM_INVERSE inverts it.'
c
c  Random data.
c
      n = 16
      seed = 123456789
      call r8vec_uniform_01 ( n, seed, u )

      call daub6_transform ( n, u, v )

      call daub6_transform_inverse ( n, v, w )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   i      U          D6(U)    D6inv(D6(U))'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i2,2x,f10.4,2x,f10.4,2x,f10.4)' )
     &    i, u(i), v(i), w(i)
      end do
c
c  Constant signal.
c
      n = 8
      do i = 1, n
        u(i) = 1.0D+00
      end do

      call daub6_transform ( n, u, v )

      call daub6_transform_inverse ( n, v, w )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   i      U          D6(U)    D6inv(D6(U))'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i2,2x,f10.4,2x,f10.4,2x,f10.4)' )
     &    i, u(i), v(i), w(i)
      end do
c
c  Linear signal.
c
      n = 16
      a_first = 1.0D+00
      a_last = dble ( n )
      call r8vec_linspace ( n, a_first, a_last, u )

      call daub6_transform ( n, u, v )

      call daub6_transform_inverse ( n, v, w )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   i      U          D6(U)    D6inv(D6(U))'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i2,2x,f10.4,2x,f10.4,2x,f10.4)' )
     &    i, u(i), v(i), w(i)
      end do
c
c  Quadratic data.
c
      n = 8
      u(1) = 25.0D+00
      u(2) = 16.0D+00
      u(3) = 9.0D+00
      u(4) = 4.0D+00
      u(5) = 1.0D+00
      u(6) = 0.0D+00
      u(7) = 1.0D+00
      u(8) = 4.0D+00

      call daub6_transform ( n, u, v )

      call daub6_transform_inverse ( n, v, w )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   i      U          D6(U)    D6inv(D6(U))'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i2,2x,f10.4,2x,f10.4,2x,f10.4)' )
     &    i, u(i), v(i), w(i)
      end do

      return
      end
      subroutine test04 ( )

c*********************************************************************72
c
cc TEST04 tests DAUB8_TRANSFORM and DAUB8_TRANSFORM_INVERSE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 February 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 16 )

      double precision a_first
      double precision a_last
      integer i
      integer n
      integer seed
      double precision u(n_max)
      double precision v(n_max)
      double precision w(n_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST04'
      write ( *, '(a)' ) 
     &  '  DAUB8_TRANSFORM computes the DAUB8 transform of a vector.'
      write ( *, '(a)' ) '  DAUB8_TRANSFORM_INVERSE inverts it.'
c
c  Random data.
c
      n = 16
      seed = 123456789
      call r8vec_uniform_01 ( n, seed, u )

      call daub8_transform ( n, u, v )

      call daub8_transform_inverse ( n, v, w )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   i      U          D8(U)    D8inv(D8(U))'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i2,2x,f10.4,2x,f10.4,2x,f10.4)' )
     &    i, u(i), v(i), w(i)
      end do
c
c  Constant signal.
c
      n = 8
      do i = 1, n
        u(i) = 1.0D+00
      end do
      call daub8_transform ( n, u, v )

      call daub8_transform_inverse ( n, v, w )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   i      U          D8(U)    D8inv(D8(U))'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i2,2x,f10.4,2x,f10.4,2x,f10.4)' )
     &    i, u(i), v(i), w(i)
      end do
c
c  Linear signal.
c
      n = 16
      a_first = 1.0D+00
      a_last = dble ( n )
      call r8vec_linspace ( n, a_first, a_last, u )

      call daub8_transform ( n, u, v )

      call daub8_transform_inverse ( n, v, w )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   i      U          D8(U)    D8inv(D8(U))'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i2,2x,f10.4,2x,f10.4,2x,f10.4)' )
     &    i, u(i), v(i), w(i)
      end do
c
c  Quadratic data.
c
      n = 8
      u(1) = 25.0D+00
      u(2) = 16.0D+00
      u(3) = 9.0D+00
      u(4) = 4.0D+00
      u(5) = 1.0D+00
      u(6) = 0.0D+00
      u(7) = 1.0D+00
      u(8) = 4.0D+00

      call daub8_transform ( n, u, v )

      call daub8_transform_inverse ( n, v, w )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   i      U          D8(U)    D8inv(D8(U))'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i2,2x,f10.4,2x,f10.4,2x,f10.4)' )
     &    i, u(i), v(i), w(i)
      end do

      return
      end
      subroutine test05 ( )

c*********************************************************************72
c
cc TEST05 tests DAUB10_TRANSFORM and DAUB10_TRANSFORM_INVERSE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 February 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 16 )

      double precision a_first
      double precision a_last
      integer i
      integer n
      integer seed
      double precision u(n_max)
      double precision v(n_max)
      double precision w(n_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST05'
      write ( *, '(a)' ) 
     &  '  DAUB10_TRANSFORM computes the DAUB10 transform of a vector.'
      write ( *, '(a)' ) '  DAUB10_TRANSFORM_INVERSE inverts it.'
c
c  Random data.
c
      n = 16
      seed = 123456789
      call r8vec_uniform_01 ( n, seed, u )

      call daub10_transform ( n, u, v )

      call daub10_transform_inverse ( n, v, w )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   i      U         D10(U)   D10inv(D10(U))'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i2,2x,f10.4,2x,f10.4,2x,f10.4)' )
     &    i, u(i), v(i), w(i)
      end do
c
c  Constant signal.
c
      n = 8

      do i = 1, n
        u(i) = 1.0D+00
      end do

      call daub10_transform ( n, u, v )

      call daub10_transform_inverse ( n, v, w )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   i      U         D10(U)   D10inv(D10(U))'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i2,2x,f10.4,2x,f10.4,2x,f10.4)' )
     &    i, u(i), v(i), w(i)
      end do
c
c  Linear signal.
c
      n = 16
      a_first = 1.0D+00
      a_last = dble ( n )
      call r8vec_linspace ( n, a_first, a_last, u )

      call daub10_transform ( n, u, v )

      call daub10_transform_inverse ( n, v, w )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   i      U         D10(U)   D10inv(D10(U))'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i2,2x,f10.4,2x,f10.4,2x,f10.4)' )
     &    i, u(i), v(i), w(i)
      end do
c
c  Quadratic data.
c
      n = 8
      u(1) = 25.0D+00
      u(2) = 16.0D+00
      u(3) = 9.0D+00
      u(4) = 4.0D+00
      u(5) = 1.0D+00
      u(6) = 0.0D+00
      u(7) = 1.0D+00
      u(8) = 4.0D+00

      call daub10_transform ( n, u, v )

      call daub10_transform_inverse ( n, v, w )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   i      U         D10(U)   D10inv(D10(U))'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i2,2x,f10.4,2x,f10.4,2x,f10.4)' )
     &    i, u(i), v(i), w(i)
      end do

      return
      end
      subroutine test06 ( )

c*********************************************************************72
c
cc TEST06 tests DAUB12_TRANSFORM and DAUB12_TRANSFORM_INVERSE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 February 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 16 )

      double precision a_first
      double precision a_last
      integer i
      integer n
      integer seed
      double precision u(n_max)
      double precision v(n_max)
      double precision w(n_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST06'
      write ( *, '(a)' ) 
     &  '  DAUB12_TRANSFORM computes the DAUB12 transform of a vector.'
      write ( *, '(a)' ) '  DAUB12_TRANSFORM_INVERSE inverts it.'
c
c  Random data.
c
      n = 16
      seed = 123456789
      call r8vec_uniform_01 ( n, seed, u )

      call daub12_transform ( n, u, v )

      call daub12_transform_inverse ( n, v, w )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   i      U         D12(U)   D12inv(D12(U))'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i2,2x,f10.4,2x,f10.4,2x,f10.4)' )
     &    i, u(i), v(i), w(i)
      end do
c
c  Constant signal.
c
      n = 8
      do i = 1, n
        u(i) = 1.0D+00
      end do

      call daub12_transform ( n, u, v )

      call daub12_transform_inverse ( n, v, w )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   i      U         D12(U)   D12inv(D12(U))'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i2,2x,f10.4,2x,f10.4,2x,f10.4)' )
     &    i, u(i), v(i), w(i)
      end do
c
c  Linear signal.
c
      n = 16
      a_first = 1.0D+00
      a_last = dble ( n )
      call r8vec_linspace ( n, a_first, a_last, u )

      call daub12_transform ( n, u, v )

      call daub12_transform_inverse ( n, v, w )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   i      U         D12(U)   D12inv(D12(U))'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i2,2x,f10.4,2x,f10.4,2x,f10.4)' )
     &    i, u(i), v(i), w(i)
      end do
c
c  Quadratic data.
c
      n = 8
      u(1) = 25.0D+00
      u(2) = 16.0D+00
      u(3) = 9.0D+00
      u(4) = 4.0D+00
      u(5) = 1.0D+00
      u(6) = 0.0D+00
      u(7) = 1.0D+00
      u(8) = 4.0D+00

      call daub12_transform ( n, u, v )

      call daub12_transform_inverse ( n, v, w )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   i      U         D12(U)   D12inv(D12(U))'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i2,2x,f10.4,2x,f10.4,2x,f10.4)' )
     &    i, u(i), v(i), w(i)
      end do

      return
      end
      subroutine test07 ( )

c*********************************************************************72
c
cc TEST07 tests DAUB14_TRANSFORM and DAUB14_TRANSFORM_INVERSE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 February 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 16 )

      double precision a_first
      double precision a_last
      integer i
      integer n
      integer seed
      double precision u(n_max)
      double precision v(n_max)
      double precision w(n_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST07'
      write ( *, '(a)' ) 
     &  '  DAUB14_TRANSFORM computes the DAUB14 transform of a vector.'
      write ( *, '(a)' ) '  DAUB14_TRANSFORM_INVERSE inverts it.'
c
c  Random data.
c
      n = 16
      seed = 123456789
      call r8vec_uniform_01 ( n, seed, u )

      call daub14_transform ( n, u, v )

      call daub14_transform_inverse ( n, v, w )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   i      U         D14(U)   D14inv(D14(U))'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i2,2x,f10.4,2x,f10.4,2x,f10.4)' )
     &    i, u(i), v(i), w(i)
      end do
c
c  Constant signal.
c
      n = 8
      do i = 1, n
        u(i) = 1.0D+00
      end do
      call daub14_transform ( n, u, v )

      call daub14_transform_inverse ( n, v, w )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   i      U         D14(U)   D14inv(D14(U))'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i2,2x,f10.4,2x,f10.4,2x,f10.4)' )
     &    i, u(i), v(i), w(i)
      end do
c
c  Linear signal.
c
      n = 16
      a_first = 1.0D+00
      a_last = dble ( n )
      call r8vec_linspace ( n, a_first, a_last, u )

      call daub14_transform ( n, u, v )

      call daub14_transform_inverse ( n, v, w )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   i      U         D14(U)   D14inv(D14(U))'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i2,2x,f10.4,2x,f10.4,2x,f10.4)' )
     &    i, u(i), v(i), w(i)
      end do
c
c  Quadratic data.
c
      n = 8
      u(1) = 25.0D+00
      u(2) = 16.0D+00
      u(3) = 9.0D+00
      u(4) = 4.0D+00
      u(5) = 1.0D+00
      u(6) = 0.0D+00
      u(7) = 1.0D+00
      u(8) = 4.0D+00

      call daub14_transform ( n, u, v )

      call daub14_transform_inverse ( n, v, w )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   i      U         D14(U)   D14inv(D14(U))'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i2,2x,f10.4,2x,f10.4,2x,f10.4)' )
     &    i, u(i), v(i), w(i)
      end do

      return
      end
      subroutine test08 ( )

c*********************************************************************72
c
cc TEST08 tests DAUB16_TRANSFORM and DAUB16_TRANSFORM_INVERSE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 February 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 16 )

      double precision a_first
      double precision a_last
      integer i
      integer n
      integer seed
      double precision u(n_max)
      double precision v(n_max)
      double precision w(n_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST08'
      write ( *, '(a)' ) 
     &  '  DAUB16_TRANSFORM computes the DAUB16 transform of a vector.'
      write ( *, '(a)' ) '  DAUB16_TRANSFORM_INVERSE inverts it.'
c
c  Random data.
c
      n = 16
      seed = 123456789
      call r8vec_uniform_01 ( n, seed, u )

      call daub16_transform ( n, u, v )

      call daub16_transform_inverse ( n, v, w )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   i      U         D16(U)   D16inv(D16(U))'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i2,2x,f10.4,2x,f10.4,2x,f10.4)' )
     &    i, u(i), v(i), w(i)
      end do
c
c  Constant signal.
c
      n = 8
      do i = 1, n
        u(i) = 1.0D+00
      end do

      call daub16_transform ( n, u, v )

      call daub16_transform_inverse ( n, v, w )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   i      U         D16(U)   D16inv(D16(U))'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i2,2x,f10.4,2x,f10.4,2x,f10.4)' )
     &    i, u(i), v(i), w(i)
      end do
c
c  Linear signal.
c
      n = 16
      a_first = 1.0D+00
      a_last = dble ( n )
      call r8vec_linspace ( n, a_first, a_last, u )

      call daub16_transform ( n, u, v )

      call daub16_transform_inverse ( n, v, w )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   i      U         D16(U)   D16inv(D16(U))'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i2,2x,f10.4,2x,f10.4,2x,f10.4)' )
     &    i, u(i), v(i), w(i)
      end do
c
c  Quadratic data.
c
      n = 8
      u(1) = 25.0D+00
      u(2) = 16.0D+00
      u(3) = 9.0D+00
      u(4) = 4.0D+00
      u(5) = 1.0D+00
      u(6) = 0.0D+00
      u(7) = 1.0D+00
      u(8) = 4.0D+00

      call daub16_transform ( n, u, v )

      call daub16_transform_inverse ( n, v, w )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   i      U         D16(U)   D16inv(D16(U))'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i2,2x,f10.4,2x,f10.4,2x,f10.4)' )
     &    i, u(i), v(i), w(i)
      end do

      return
      end
      subroutine test09 ( )

c*********************************************************************72
c
cc TEST09 tests DAUB18_TRANSFORM and DAUB18_TRANSFORM_INVERSE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 February 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 16 )

      double precision a_first
      double precision a_last
      integer i
      integer n
      integer seed
      double precision u(n_max)
      double precision v(n_max)
      double precision w(n_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST09'
      write ( *, '(a)' ) 
     &  '  DAUB18_TRANSFORM computes the DAUB18 transform of a vector.'
      write ( *, '(a)' ) '  DAUB18_TRANSFORM_INVERSE inverts it.'
c
c  Random data.
c
      n = 16
      seed = 123456789
      call r8vec_uniform_01 ( n, seed, u )

      call daub18_transform ( n, u, v )

      call daub18_transform_inverse ( n, v, w )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   i      U         D18(U)   D18inv(D18(U))'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i2,2x,f10.4,2x,f10.4,2x,f10.4)' )
     &    i, u(i), v(i), w(i)
      end do
c
c  Constant signal.
c
      n = 8
      do i = 1, n
        u(i) = 1.0D+00
      end do

      call daub18_transform ( n, u, v )

      call daub18_transform_inverse ( n, v, w )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   i      U         D18(U)   D18inv(D18(U))'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i2,2x,f10.4,2x,f10.4,2x,f10.4)' )
     &    i, u(i), v(i), w(i)
      end do
c
c  Linear signal.
c
      n = 16
      a_first = 1.0D+00
      a_last = dble ( n )
      call r8vec_linspace ( n, a_first, a_last, u )

      call daub18_transform ( n, u, v )

      call daub18_transform_inverse ( n, v, w )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   i      U         D18(U)   D18inv(D18(U))'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i2,2x,f10.4,2x,f10.4,2x,f10.4)' )
     &    i, u(i), v(i), w(i)
      end do
c
c  Quadratic data.
c
      n = 8
      u(1) = 25.0D+00
      u(2) = 16.0D+00
      u(3) = 9.0D+00
      u(4) = 4.0D+00
      u(5) = 1.0D+00
      u(6) = 0.0D+00
      u(7) = 1.0D+00
      u(8) = 4.0D+00

      call daub18_transform ( n, u, v )

      call daub18_transform_inverse ( n, v, w )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   i      U         D18(U)   D18inv(D18(U))'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i2,2x,f10.4,2x,f10.4,2x,f10.4)' )
     &    i, u(i), v(i), w(i)
      end do

      return
      end
      subroutine test10 ( )

c*********************************************************************72
c
cc TEST10 tests DAUB20_TRANSFORM and DAUB20_TRANSFORM_INVERSE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 February 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 16 )

      double precision a_first
      double precision a_last
      integer i
      integer n
      integer seed
      double precision u(n_max)
      double precision v(n_max)
      double precision w(n_max)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST10'
      write ( *, '(a)' ) 
     &  '  DAUB20_TRANSFORM computes the DAUB20 transform of a vector.'
      write ( *, '(a)' ) '  DAUB20_TRANSFORM_INVERSE inverts it.'
c
c  Random data.
c
      n = 16
      seed = 123456789
      call r8vec_uniform_01 ( n, seed, u )

      call daub20_transform ( n, u, v )

      call daub20_transform_inverse ( n, v, w )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   i      U         D20(U)   D20inv(D20(U))'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i2,2x,f10.4,2x,f10.4,2x,f10.4)' )
     &    i, u(i), v(i), w(i)
      end do
c
c  Constant signal.
c
      n = 8
      do i = 1, n
        u(i) = 1.0D+00
      end do

      call daub20_transform ( n, u, v )

      call daub20_transform_inverse ( n, v, w )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   i      U         D20(U)   D20inv(D20(U))'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i2,2x,f10.4,2x,f10.4,2x,f10.4)' )
     &    i, u(i), v(i), w(i)
      end do
c
c  Linear signal.
c
      n = 16
      a_first = 1.0D+00
      a_last = dble ( n )
      call r8vec_linspace ( n, a_first, a_last, u )

      call daub20_transform ( n, u, v )

      call daub20_transform_inverse ( n, v, w )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   i      U         D20(U)   D20inv(D20(U))'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i2,2x,f10.4,2x,f10.4,2x,f10.4)' )
     &    i, u(i), v(i), w(i)
      end do
c
c  Quadratic data.
c
      n = 8
      u(1) = 25.0D+00
      u(2) = 16.0D+00
      u(3) = 9.0D+00
      u(4) = 4.0D+00
      u(5) = 1.0D+00
      u(6) = 0.0D+00
      u(7) = 1.0D+00
      u(8) = 4.0D+00

      call daub20_transform ( n, u, v )

      call daub20_transform_inverse ( n, v, w )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '   i      U         D20(U)   D20inv(D20(U))'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i2,2x,f10.4,2x,f10.4,2x,f10.4)' )
     &    i, u(i), v(i), w(i)
      end do

      return
      end
      subroutine test11 ( )

c*********************************************************************72
c
cc TEST11 tests DAUB*_MATRIX.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 February 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 20 )

      double precision a(n,n)
      double precision b(n,n)
      double precision c(n,n)
      double precision error_frobenius

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST11'
      write ( *, '(a)' ) '  DAUB*_MATRIX computes the DAUB* matrix.'
      write ( *, '(a)' ) '  Verify that each matrix is orthogonal.'

      call daub2_matrix ( n, a )

      call r8mat_transpose ( n, n, a, b )
      call r8mat_mm ( n, n, n, a, b, c )
      call r8mat_is_identity ( n, c, error_frobenius )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i4,a,g14.6)' ) 
     &  '  DAUB2,  N = ', n, '  || A*A'' - I|| = ', error_frobenius

      call daub4_matrix ( n, a )

      call r8mat_transpose ( n, n, a, b )
      call r8mat_mm ( n, n, n, a, b, c )
      call r8mat_is_identity ( n, c, error_frobenius )

      write ( *, '(a,i4,a,g14.6)' ) 
     &  '  DAUB4,  N = ', n, '  || A*A'' - I|| = ', error_frobenius

      call daub6_matrix ( n, a )

      call r8mat_transpose ( n, n, a, b )
      call r8mat_mm ( n, n, n, a, b, c )
      call r8mat_is_identity ( n, c, error_frobenius )

      write ( *, '(a,i4,a,g14.6)' ) 
     &  '  DAUB6,  N = ', n, '  || A*A'' - I|| = ', error_frobenius

      call daub8_matrix ( n, a )

      call r8mat_transpose ( n, n, a, b )
      call r8mat_mm ( n, n, n, a, b, c )
      call r8mat_is_identity ( n, c, error_frobenius )

      write ( *, '(a,i4,a,g14.6)' ) 
     &  '  DAUB8,  N = ', n, '  || A*A'' - I|| = ', error_frobenius

      call daub10_matrix ( n, a )

      call r8mat_transpose ( n, n, a, b )
      call r8mat_mm ( n, n, n, a, b, c )
      call r8mat_is_identity ( n, c, error_frobenius )

      write ( *, '(a,i4,a,g14.6)' ) 
     &  '  DAUB10, N = ', n, '  || A*A'' - I|| = ', error_frobenius

      call daub12_matrix ( n, a )

      call r8mat_transpose ( n, n, a, b )
      call r8mat_mm ( n, n, n, a, b, c )
      call r8mat_is_identity ( n, c, error_frobenius )

      write ( *, '(a,i4,a,g14.6)' ) 
     &  '  DAUB12, N = ', n, '  || A*A'' - I|| = ', error_frobenius

      return
      end
