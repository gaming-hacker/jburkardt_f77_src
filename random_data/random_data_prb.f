      program main

c*********************************************************************72
c
cc MAIN is the main program for RANDOM_DATA_PRB.
c
c  Discussion:
c
c    RANDOM_DATA_PRB tests the RANDOM_DATA library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    10 December 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'RANDOM_DATA_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the RANDOM_DATA library.'

      call test005 ( )
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
      call test115 ( )
      call test12 ( )
      call test125 ( )
      call test13 ( )
      call test14 ( )
      call test15 ( )
      call test16 ( )
      call test17 ( )
      call test18 ( )
      call test19 ( )

      call test20 ( )
      call test205 ( )
      call test21 ( )
      call test22 ( )
      call test23 ( )
      call test235 ( )
      call test24 ( )
      call test245 ( )
      call test25 ( )
      call test26 ( )
      call test264 ( )
      call test265 ( )
      call test267 ( )
      call test27 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'RANDOM_DATA_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test005 ( )

c*********************************************************************72
c
cc TEST005 tests BAD_IN_SIMPLEX01.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    10 December 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer dim_max
      parameter ( dim_max = 3 )
      integer n
      parameter ( n = 10000 )

      integer dim_num
      character * ( 255 ) output_filename
      integer seed
      double precision x(dim_max,n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST005:'
      write ( *, '(a)' ) 
     &  '  BAD_IN_SIMPLEX01 is a "bad" sampling technique'
      write ( *, '(a)' ) '  for the unit simplex.'

      do dim_num = 2, dim_max

        seed = 123456789

        if ( dim_num .eq. 2 ) then
          output_filename = 'bad_in_triangle.txt'
        else if ( dim_num .eq. 3 ) then
          output_filename = 'bad_in_tetrahedron.txt'
        end if

        write ( *, '(a)' ) ' '
        write ( *, '(a,i8)'  ) 
     &    '  Spatial dimension DIM_NUM =   ', dim_num
        write ( *, '(a,i8)'  ) '  Number of points N =          ', n
        write ( *, '(a,i12)' ) 
     &    '  Initial random number SEED =  ', seed

        call bad_in_simplex01 ( dim_num, n, seed, x )

        call r8mat_write ( output_filename, dim_num, n, x )

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  Data written to "' 
     &    // trim ( output_filename ) // '".'

      end do

      return
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 tests BROWNIAN.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    10 December 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer dim_num
      parameter ( dim_num = 2 )
      integer n
      parameter ( n = 100 )

      character * ( 255 ) output_filename
      integer seed
      double precision x(dim_num,n)

      output_filename = 'brownian.txt'
      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01:'
      write ( *, '(a)' ) '  BROWNIAN generates Brownian motion points.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)'  ) '  Spatial dimension DIM_NUM =   ', dim_num
      write ( *, '(a,i8)'  ) '  Number of points N =          ', n
      write ( *, '(a,i12)' ) '  Initial random number SEED =  ', seed

      call brownian ( dim_num, n, seed, x )

      write ( *, '(a,i12)' ) '  Final random number SEED =    ', seed

      call scale_to_block01 ( dim_num, n, x )

      call r8mat_write ( output_filename, dim_num, n, x )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Data written to "' 
     &  // trim ( output_filename ) // '".'

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 tests R8_NORMAL_01
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    10 December 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer i
      double precision r8_normal_01
      integer seed
      integer seed_in
      double precision x

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST02:'
      write ( *, '(a)' ) '  R8_NORMAL_01 generates a single normal'
      write ( *, '(a)' ) '  pseudorandom value.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     Seed          Seed       R8_NORMAL_01'
      write ( *, '(a)' ) '    (Input)       (Output)'
      write ( *, '(a)' ) ' '

      do i = 1, 10

        seed_in = seed
        x = r8_normal_01 ( seed )

        write ( *, '(2x,i12,2x,i12,2x,f12.8)' ) seed_in, seed, x

      end do

      return
      end
      subroutine test03 ( )

c*********************************************************************72
c
cc TEST03 tests R8_UNIFORM_01
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    10 December 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer i
      double precision r8_uniform_01
      integer seed
      integer seed_in
      double precision x

      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST03:'
      write ( *, '(a)' ) '  R8_UNIFORM_01 generates a single uniform'
      write ( *, '(a)' ) '  pseudorandom value.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '     Seed          Seed       R8_UNIFORM_01'
      write ( *, '(a)' ) '    (Input)       (Output)'
      write ( *, '(a)' ) ' '

      do i = 1, 10

        seed_in = seed
        x = r8_uniform_01 ( seed )

        write ( *, '(2x,i12,2x,i12,2x,f12.8)' ) seed_in, seed, x

      end do

      return
      end
      subroutine test04 ( )

c*********************************************************************72
c
cc TEST04 tests GRID_IN_CUBE01.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    10 December 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer dim_num
      parameter ( dim_num = 2 )
      integer n
      parameter ( n = 85 )

      integer center
      character * ( 255 ) output_filename
      integer seed
      double precision x(dim_num,n)

      center = 1
      output_filename = 'grid_in_cube01.txt'
      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST04'
      write ( *, '(a)' ) '  GRID_IN_CUBE01 generates grid points'
      write ( *, '(a)' ) '  in the unit hypercube.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)'  ) '  Spatial dimension DIM_NUM =   ', dim_num
      write ( *, '(a,i8)'  ) '  Number of points N =          ', n
      write ( *, '(a,i8)'  ) '  CENTER option =               ', center
      write ( *, '(a,i12)' ) '  Initial random number SEED =  ', seed

      call grid_in_cube01 ( dim_num, n, center, seed, x )

      write ( *, '(a,i12)' ) '  Final random number SEED =    ', seed

      call r8mat_write ( output_filename, dim_num, n, x )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Data written to file "' 
     &  // trim ( output_filename ) // '".'

      return
      end
      subroutine test05 ( )

c*********************************************************************72
c
cc TEST05 tests HALTON_IN_CIRCLE01_ACCEPT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    10 December 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer dim_num
      parameter ( dim_num = 2 )
      integer n
      parameter ( n = 400 )

      character * ( 255 ) output_filename
      integer seed
      double precision x(dim_num,n)

      output_filename = 'halton_in_circle01_accept.txt'
      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST05'
      write ( *, '(a)' ) '  HALTON_IN_CIRCLE01_ACCEPT generates'
      write ( *, '(a)' ) 
     &  '  Halton points in a unit circle by acceptance/rejection.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)'  ) '  Spatial dimension DIM_NUM =   ', dim_num
      write ( *, '(a,i8)'  ) '  Number of points N =          ', n
      write ( *, '(a,i12)' ) '  Initial random number SEED =  ', seed

      call halton_in_circle01_accept ( dim_num, n, seed, x )

      write ( *, '(a,i12)' ) '  Final random number SEED =    ', seed

      call r8mat_write ( output_filename, dim_num, n, x )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Data written to file "' 
     &  // trim ( output_filename ) // '".'

      return
      end
      subroutine test06 ( )

c*********************************************************************72
c
cc TEST06 tests HALTON_IN_CIRCLE01_MAP.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    10 December 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer dim_num
      parameter ( dim_num = 2 )
      integer n
      parameter ( n = 400 )

      character * ( 255 ) output_filename
      integer seed
      double precision x(dim_num,n)

      output_filename = 'halton_in_circle01_map.txt'
      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST06'
      write ( *, '(a)' ) '  HALTON_IN_CIRCLE01_MAP maps'
      write ( *, '(a)' ) '  Halton points into a unit circle.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)'  ) '  Spatial dimension DIM_NUM =   ', dim_num
      write ( *, '(a,i8)'  ) '  Number of points N =          ', n
      write ( *, '(a,i12)' ) '  Initial random number SEED =  ', seed

      call halton_in_circle01_map ( dim_num, n, seed, x )

      write ( *, '(a,i12)' ) '  Final random number SEED =    ', seed

      call r8mat_write ( output_filename, dim_num, n, x )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Data written to file "' 
     &  // trim ( output_filename ) // '".'

      return
      end
      subroutine test07 ( )

c*********************************************************************72
c
cc TEST07 tests HALTON_IN_CUBE01.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    10 December 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer dim_num
      parameter ( dim_num = 2 )
      integer n
      parameter ( n = 510 )

      character * ( 255 ) output_filename
      integer seed
      double precision x(dim_num,n)

      output_filename = 'halton_in_cube01.txt'
      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST07'
      write ( *, '(a)' ) '  HALTON_IN_CUBE01 generates Halton points'
      write ( *, '(a)' ) '  in the unit hypercube.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)'  ) '  Spatial dimension DIM_NUM =   ', dim_num
      write ( *, '(a,i8)'  ) '  Number of points N =          ', n
      write ( *, '(a,i12)' ) '  Initial random number SEED =  ', seed

      call halton_in_cube01 ( dim_num, n, seed, x )

      write ( *, '(a,i12)' ) '  Final random number SEED =    ', seed

      call r8mat_write ( output_filename, dim_num, n, x )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Data written to file "' 
     &  // trim ( output_filename ) // '".'

      return
      end
      subroutine test08 ( )

c*********************************************************************72
c
cc TEST08 tests HAMMERSLEY_IN_CUBE01.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    10 December 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer dim_num
      parameter ( dim_num = 2 )
      integer n
      parameter ( n = 100 )

      character * ( 255 ) output_filename
      integer seed
      double precision x(dim_num,n)

      output_filename = 'hammersley_in_cube01.txt'
      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST08'
      write ( *, '(a)' ) 
     &  '  HAMMERSLEY_IN_CUBE01 generates Hammersley points'
      write ( *, '(a)' ) '  in the unit hypercube.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)'  ) '  Spatial dimension DIM_NUM =   ', dim_num
      write ( *, '(a,i8)'  ) '  Number of points N =          ', n
      write ( *, '(a,i12)' ) '  Initial random number SEED =  ', seed

      call hammersley_in_cube01 ( dim_num, n, seed, x )

      write ( *, '(a,i12)' ) '  Final random number SEED =    ', seed

      call r8mat_write ( output_filename, dim_num, n, x )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Data written to file "' 
     &  // trim ( output_filename ) // '".'

      return
      end
      subroutine test09 ( )

c*********************************************************************72
c
cc TEST09 tests NORMAL.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    10 December 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer dim_num
      parameter ( dim_num = 2 )
      integer n
      parameter ( n = 1000 )

      character * ( 255 ) output_filename
      integer i
      integer info
      integer j
      double precision mu(dim_num)
      double precision r(dim_num,dim_num)
      integer seed
      double precision v(dim_num,dim_num)
      double precision x(dim_num,n)

      mu(1) = 6.0D+00
      mu(2) = 100.0D+00
      output_filename = 'normal.txt'
      seed = 123456789
      v(1,1) = 1.0D+00
      v(2,1) = 0.3D+00
      v(1,2) = 0.3D+00
      v(2,2) = 1.0D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST09'
      write ( *, '(a)' ) '  NORMAL generates normal points'
      write ( *, '(a)' ) 
     &  '    in M dimensions, using a nonzero mean, and with'
      write ( *, '(a)' ) 
     &  '    user-specified variance-covariance matrix.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)'  ) '  Spatial dimension DIM_NUM =   ', dim_num
      write ( *, '(a,i8)'  ) '  Number of points N =          ', n
      write ( *, '(a,i12)' ) '  Initial random number SEED =  ', seed

      call r8vec_print ( dim_num, mu, '  Mean vector MU:' )

      call r8mat_print ( dim_num, dim_num, v, 
     &  '  Variance-covariance matrix V:' )
     
      do j = 1, dim_num
        do i = 1, dim_num
          r(i,j) = v(i,j)
        end do
      end do

      call r8po_fa ( dim_num, r, info )

      if ( info .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'TEST04 - Fatal error!'
        write ( *, '(a)' ) 
     &    '  Variance-covariance matrix factorization failed.'
        write ( *, '(a,i8)' ) '  INFO = ', info
        stop
      end if

      call r8mat_print ( dim_num, dim_num, r, '  Cholesky factor R:' )

      call normal ( dim_num, n, r, mu, seed, x )

      write ( *, '(a,i12)' ) '  Final random number SEED =    ', seed

      call scale_to_block01 ( dim_num, n, x )

      call r8mat_write ( output_filename, dim_num, n, x )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Data written to file "' 
     &  // trim ( output_filename ) // '".'

      return
      end
      subroutine test10 ( )

c*********************************************************************72
c
cc TEST10 tests NORMAL_CIRCULAR.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    10 December 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer dim_num
      parameter ( dim_num = 2 )
      integer n
      parameter ( n = 2000 )

      character * ( 255 ) output_filename
      integer seed
      double precision x(dim_num,n)

      output_filename = 'normal_circular.txt'
      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST10'
      write ( *, '(a)' ) '  NORMAL_CIRCULAR generates points in 2D'
      write ( *, '(a)' ) 
     &  '    distributed according to a circular normal.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)'  ) '  Spatial dimension DIM_NUM =   ', dim_num
      write ( *, '(a,i8)'  ) '  Number of points N =          ', n
      write ( *, '(a,i12)' ) '  Initial random number SEED =  ', seed

      call normal_circular ( dim_num, n, seed, x )

      write ( *, '(a,i12)' ) '  Final random number SEED =    ', seed

      call scale_to_block01 ( dim_num, n, x )

      call r8mat_write ( output_filename, dim_num, n, x )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Data written to file "' 
     &  // trim ( output_filename ) // '".'

      return
      end
      subroutine test11 ( )

c*********************************************************************72
c
cc TEST11 tests NORMAL_SIMPLE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    08 July 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer dim_num
      parameter ( dim_num = 2 )
      integer n
      parameter ( n = 1000 )

      character * ( 255 ) output_filename
      integer seed
      double precision x(dim_num,n)

      output_filename = 'normal_simple.txt'
      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST11'
      write ( *, '(a)' ) '  NORMAL_SIMPLE generates normal points'
      write ( *, '(a)' ) 
     &  '    in M dimensions, using a zero mean, and with'
      write ( *, '(a)' ) 
     &  '    the identity as the variance-covariance matrix.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)'  ) '  Spatial dimension DIM_NUM =   ', dim_num
      write ( *, '(a,i8)'  ) '  Number of points N =          ', n
      write ( *, '(a,i12)' ) '  Initial random number SEED =  ', seed

      call normal_simple ( dim_num, n, seed, x )

      write ( *, '(a,i12)' ) '  Final random number SEED =    ', seed

      call scale_to_block01 ( dim_num, n, x )

      call r8mat_write ( output_filename, dim_num, n, x )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Data written to file "' 
     &  // trim ( output_filename ) // '".'

      return
      end
      subroutine test115 ( )

c*********************************************************************72
c
cc TEST115 tests UNIFORM_IN_ANNULUS.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    08 July 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer dim_num
      parameter ( dim_num = 2 )
      integer n
      parameter ( n = 400 )

      character * ( 255 ) output_filename
      double precision pc(2)
      double precision r1
      double precision r2
      integer seed
      double precision x(dim_num,n)

      output_filename = 'uniform_in_annulus.txt'
      pc(1) = 10.0D+00
      pc(2) = 5.0D+00
      r1 = 1.0D+00
      r2 = 3.0D+00
      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST115'
      write ( *, '(a)' ) '  UNIFORM_IN_ANNULUS generates uniform '
      write ( *, '(a)' ) '  points in an annulus by mapping.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)'     ) 
     &  '  Spatial dimension DIM_NUM =   ', dim_num
      write ( *, '(a,i8)'     ) '  Number of points N =          ', n
      write ( *, '(a,2g14.6)' ) 
     &  '  Center PC(1:2) =              ', pc(1:2)
      write ( *, '(a,g14.6)'  ) '  Inner radius is R1 =          ', r1
      write ( *, '(a,g14.6)'  ) '  Outer radius is R2 =          ', r2
      write ( *, '(a,i12)'    ) '  Initial random number SEED =  ', seed
     
      call uniform_in_annulus ( pc, r1, r2, n, seed, x )

      write ( *, '(a,i12)' ) '  Final random number SEED =    ', seed

      call r8mat_write ( output_filename, dim_num, n, x )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Data written to file "' 
     &  // trim ( output_filename ) // '".'

      return
      end
      subroutine test12 ( )

c*********************************************************************72
c
cc TEST12 tests UNIFORM_IN_ANNULUS_ACCEPT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    08 July 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer dim_num
      parameter ( dim_num = 2 )
      integer n
      parameter ( n = 400 )

      character * ( 255 ) output_filename
      double precision pc(2)
      double precision r1
      double precision r2
      integer seed
      double precision x(dim_num,n)

      output_filename = 'uniform_in_annulus_accept.txt'
      pc(1) = 10.0D+00
      pc(2) = 5.0D+00
      r1 = 1.0D+00
      r2 = 3.0D+00
      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST12'
      write ( *, '(a)' ) 
     &  '  UNIFORM_IN_ANNULUS_ACCEPT generates uniform '
      write ( *, '(a)' ) 
     &  '  points in an annulus by acceptance/rejection.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)'     ) 
     &  '  Spatial dimension DIM_NUM =   ', dim_num
      write ( *, '(a,i8)'     ) '  Number of points N =          ', n
      write ( *, '(a,2g14.6)' ) 
     &  '  Center PC(1:2) =              ', pc(1:2)
      write ( *, '(a,g14.6)'  ) '  Inner radius is R1 =          ', r1
      write ( *, '(a,g14.6)'  ) '  Outer radius is R2 =          ', r2
      write ( *, '(a,i12)'    ) '  Initial random number SEED =  ', seed
     
      call uniform_in_annulus_accept ( pc, r1, r2, n, seed, x )

      write ( *, '(a,i12)' ) '  Final random number SEED =    ', seed

      call r8mat_write ( output_filename, dim_num, n, x )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Data written to file "' 
     &  // trim ( output_filename ) // '".'

      return
      end
      subroutine test125 ( )

c*********************************************************************72
c
cc TEST125 tests UNIFORM_IN_ANNULUS_SECTOR.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    08 July 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer dim_num
      parameter ( dim_num = 2 )
      integer n
      parameter ( n = 400 )

      character * ( 255 ) output_filename
      double precision pc(2)
      double precision r1
      double precision r2
      integer seed
      double precision theta1
      double precision theta2
      double precision x(dim_num,n)

      output_filename = 'uniform_in_annulus_sector.txt'
      pc(1) = 10.0D+00
      pc(2) = 5.0D+00
      r1 = 1.0D+00
      r2 = 3.0D+00
      seed = 123456789
      theta1 = 0.0D+00
      theta2 = 1.5707964D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST125'
      write ( *, '(a)' ) 
     &  '  UNIFORM_IN_ANNULUS_SECTOR generates uniform '
      write ( *, '(a)' ) '  points in an annular sector by mapping.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)'     ) 
     &  '  Spatial dimension DIM_NUM =   ', dim_num
      write ( *, '(a,i8)'     ) '  Number of points N =          ', n
      write ( *, '(a,2g14.6)' ) 
     &  '  Center PC(1:2) =              ', pc(1:2)
      write ( *, '(a,g14.6)'  ) '  Inner radius is R1 =          ', r1
      write ( *, '(a,g14.6)'  ) '  Outer radius is R2 =          ', r2
      write ( *, '(a,g14.6)'  ) 
     &  '  THETA1 =                      ', theta1
      write ( *, '(a,g14.6)'  ) 
     &  '  THETA2 =                      ', theta2
      write ( *, '(a,i12)'    ) 
     &  '  Initial random number SEED =  ', seed
     
      call uniform_in_annulus_sector ( pc, r1, r2, theta1, theta2, 
     &  n, seed, x )

      write ( *, '(a,i12)' ) '  Final random number SEED =    ', seed

      call r8mat_write ( output_filename, dim_num, n, x )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Data written to file "' 
     &  // trim ( output_filename ) // '".'

      return
      end
      subroutine test13 ( )

c*********************************************************************72
c
cc TEST13 tests UNIFORM_IN_CIRCLE01_MAP.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    07 August 2005
c
c  Modified:
c
c    08 July 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer dim_num
      parameter ( dim_num = 2 )
      integer n
      parameter ( n = 400 )

      character * ( 255 ) output_filename
      integer seed
      double precision x(dim_num,n)

      output_filename = 'uniform_in_circle01_map.txt'
      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST13'
      write ( *, '(a)' ) '  UNIFORM_IN_CIRCLE01_MAP maps uniform '
      write ( *, '(a)' ) '  points into a unit circle.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)'  ) '  Spatial dimension DIM_NUM =   ', dim_num
      write ( *, '(a,i8)'  ) '  Number of points N =          ', n
      write ( *, '(a,i12)' ) '  Initial random number SEED =  ', seed

      call uniform_in_circle01_map ( n, seed, x )

      write ( *, '(a,i12)' ) '  Final random number SEED =    ', seed

      call r8mat_write ( output_filename, dim_num, n, x )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Data written to file "' 
     &  // trim ( output_filename ) // '".'

      return
      end
      subroutine test14 ( )

c*********************************************************************72
c
cc TEST14 tests UNIFORM_IN_CUBE01.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    08 July 2007
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer dim_num
      parameter ( dim_num = 2 )
      integer n
      parameter ( n = 1000 )

      character * ( 255 ) output_filename
      integer seed
      double precision x(dim_num,n)

      output_filename = 'uniform_in_cube01.txt'
      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST14'
      write ( *, '(a)' ) '  UNIFORM_IN_CUBE01 generates uniform '
      write ( *, '(a)' ) '  points in the unit hypercube.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)'  ) '  Spatial dimension DIM_NUM =   ', dim_num
      write ( *, '(a,i8)'  ) '  Number of points N =          ', n
      write ( *, '(a,i12)' ) '  Initial random number SEED =  ', seed

      call uniform_in_cube01 ( dim_num, n, seed, x )

      write ( *, '(a,i12)' ) '  Final random number SEED =    ', seed

      call r8mat_write ( output_filename, dim_num, n, x )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Data written to file "' 
     &  // trim ( output_filename ) // '".'

      return
      end
      subroutine test15 ( )

c*********************************************************************72
c
cc TEST15 tests UNIFORM_IN_ELLIPSOID_MAP.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    10 December 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer dim_num
      parameter ( dim_num = 2 )
      integer n
      parameter ( n = 1000 )

      double precision a(dim_num,dim_num)
      double precision ax(dim_num)
      integer fail_num
      character * ( 255 ) output_filename
      integer j
      double precision r
      double precision r2
      double precision r8vec_dot_product
      integer seed
      integer success_num
      double precision x(dim_num,n)

      a(1,1) = 3.0D+00
      a(2,1) = 1.0D+00
      a(1,2) = 1.0D+00
      a(2,2) = 2.0D+00
      output_filename = 'uniform_in_ellipsoid_map.txt'
      r = 1.0D+00
      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST15'
      write ( *, '(a)' ) '  UNIFORM_IN_ELLIPSOID_MAP maps uniform '
      write ( *, '(a)' ) '  points into an ellipsoid.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)'  ) '  Spatial dimension DIM_NUM =   ', dim_num
      write ( *, '(a,i8)'  ) '  Number of points N =          ', n
      write ( *, '(a,i12)' ) '  Initial random number SEED =  ', seed

      call uniform_in_ellipsoid_map ( dim_num, n, a, r, seed, x )

      write ( *, '(a,i12)' ) '  Final random number SEED =    ', seed

      call r8mat_write ( output_filename, dim_num, n, x )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Data written to file "' // trim ( output_filename ) // '".'
c
c  Test the data.
c
      fail_num = 0
      success_num = 0

      do j = 1, n

        call r8mat_mv ( dim_num, dim_num, a, x, ax )
        r2 = sqrt ( r8vec_dot_product ( dim_num, x(1,j), ax ) )

        if ( r .lt. r2 ) then
          fail_num = fail_num + 1
        else
          success_num = success_num + 1
        end if

      end do
      
      write ( *, '(a)' ) ' '
      write ( *, '(2x,i8,a)' ) 
     &  fail_num, '  points failed the ellipsoid test.'
      write ( *, '(2x,i8,a)' ) 
     &  success_num, ' points satisfy the ellipsoid test.'

      return
      end
      subroutine test16 ( )

c*********************************************************************72
c
cc TEST16 tests UNIFORM_IN_PARALLELOGRAM_MAP.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    10 December 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer dim_num
      parameter ( dim_num = 2 )
      integer n
      parameter ( n = 1000 )

      character * ( 255 ) output_filename
      integer seed
      double precision v1(dim_num)
      double precision v2(dim_num)
      double precision v3(dim_num)
      double precision x(dim_num,n)

      output_filename = 'uniform_in_parallelogram_map.txt'
      seed = 123456789
      v1(1) = 0.75D+00
      v1(2) = 0.90D+00
      v2(1) = 0.0D+00
      v2(2) = 0.2D+00
      v3(1) = 1.10D+00
      v3(2) = 0.65D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST16'
      write ( *, '(a)' ) '  UNIFORM_IN_PARALLELOGRAM_MAP maps uniform'
      write ( *, '(a)' ) '  points into an arbitrary parallelogram.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)'  ) '  Spatial dimension DIM_NUM =   ', dim_num
      write ( *, '(a,i8)'  ) '  Number of points N =          ', n
      write ( *, '(a,i12)' ) '  Initial random number SEED =  ', seed

      write ( *, '(a)' ) ' '
      write ( *, '(a,2f8.2)' ) '  V1 = ', v1(1:2)
      write ( *, '(a,2f8.2)' ) '  V2 = ', v2(1:2)
      write ( *, '(a,2f8.2)' ) '  V3 = ', v3(1:2)
      write ( *, '(a,2f8.2)' ) '  V4 = ', v3(1:2)+v2(1:2)-v1(1:2)

      call uniform_in_parallelogram_map ( v1, v2, v3, n, seed, x )

      write ( *, '(a,i12)' ) '  Final random number SEED =    ', seed

      call scale_to_block01 ( dim_num, n, x )

      call r8mat_write ( output_filename, dim_num, n, x )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Data written to file "' 
     &  // trim ( output_filename ) // '".'

      return
      end
      subroutine test17 ( )

c*********************************************************************72
c
cc TEST17 tests UNIFORM_IN_POLYGON_MAP.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    10 December 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer dim_num
      parameter ( dim_num = 2 )
      integer n
      parameter ( n = 1000 )
      integer nv
      parameter ( nv = 10 )

      character * ( 255 ) output_filename
      integer seed
      double precision v(dim_num,nv)
      double precision x(dim_num,n)

      save v
 
      data v /
     &  0.0D+00, 0.0D+00, 
     &  0.5D+00, 0.3D+00, 
     &  1.0D+00, 0.0D+00, 
     &  0.7D+00, 0.4D+00, 
     &  1.0D+00, 0.6D+00, 
     &  0.6D+00, 0.6D+00, 
     &  0.5D+00, 1.0D+00, 
     &  0.4D+00, 0.6D+00, 
     &  0.0D+00, 0.6D+00, 
     &  0.3D+00, 0.4D+00 /

      output_filename = 'uniform_in_polygon_map.txt'
      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST17'
      write ( *, '(a)' ) '  UNIFORM_IN_POLYGON_MAP maps uniform '
      write ( *, '(a)' ) '  points into a polygon.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)'  ) '  Spatial dimension DIM_NUM =   ', dim_num
      write ( *, '(a,i8)'  ) '  Number of points N =          ', n
      write ( *, '(a,i12)' ) '  Initial random number SEED =  ', seed

      call r8mat_print ( dim_num, nv, v, '  Polygonal vertices:' )

      call uniform_in_polygon_map ( nv, v, n, seed, x )

      write ( *, '(a,i12)' ) '  Final random number SEED =    ', seed

      call r8mat_write ( 'polygon_vertices.txt', dim_num, nv, v )

      call r8mat_write ( output_filename, dim_num, n, x )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Data written to file "' 
     &  // trim ( output_filename ) // '".'

      return
      end
      subroutine test18 ( )

c*********************************************************************72
c
cc TEST18 tests UNIFORM_IN_SECTOR_MAP.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    10 December 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer dim_num
      parameter ( dim_num = 2 )
      integer n
      parameter ( n = 300 )

      character * ( 255 ) output_filename
      double precision r1
      double precision r2
      integer seed
      double precision t1
      double precision t2
      double precision x(dim_num,n)

      output_filename = 'uniform_in_sector_map.txt'
      r1 = 1.0D+00
      r2 = 2.0D+00
      seed = 123456789
      t1 = 0.78D+00
      t2 = 2.35D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST18'
      write ( *, '(a)' ) '  UNIFORM_IN_SECTOR_MAP maps uniform '
      write ( *, '(a)' ) '  points into a circular sector.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  R1 = ', r1
      write ( *, '(a,g14.6)' ) '  R2 = ', r2
      write ( *, '(a,g14.6)' ) '  T1 = ', t1
      write ( *, '(a,g14.6)' ) '  T2 = ', t2
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)'  ) '  Spatial dimension DIM_NUM =   ', dim_num
      write ( *, '(a,i8)'  ) '  Number of points N =          ', n
      write ( *, '(a,i12)' ) '  Initial random number SEED =  ', seed

      call uniform_in_sector_map ( r1, r2, t1, t2, n, seed, x )

      write ( *, '(a,i12)' ) '  Final random number SEED =    ', seed

      call r8mat_write ( output_filename, dim_num, n, x )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Data written to file "' 
     &  // trim ( output_filename ) // '".'

      return
      end
      subroutine test19 ( )

c*********************************************************************72
c
cc TEST19 tests UNIFORM_IN_SIMPLEX01_MAP.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    10 December 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer dim_num
      parameter ( dim_num = 2 )
      integer n
      parameter ( n = 10000 )

      character * ( 255 ) output_filename
      integer seed
      double precision x(dim_num,n)

      output_filename = 'uniform_in_simplex01_map.txt'
      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST19'
      write ( *, '(a)' ) '  UNIFORM_IN_SIMPLEX01_MAP maps uniform '
      write ( *, '(a)' ) '  points into the unit simplex.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)'  ) '  Spatial dimension DIM_NUM =   ', dim_num
      write ( *, '(a,i8)'  ) '  Number of points N =          ', n
      write ( *, '(a,i12)' ) '  Initial random number SEED =  ', seed

      call uniform_in_simplex01_map ( dim_num, n, seed, x )

      write ( *, '(a,i12)' ) '  Final random number SEED =    ', seed

      call r8mat_write ( output_filename, dim_num, n, x )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Data written to "' 
     &  // trim ( output_filename ) // '".'

      return
      end
      subroutine test20 ( )

c*********************************************************************72
c
cc TEST20 tests UNIFORM_IN_SPHERE01_MAP.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    10 December 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer dim_num
      parameter ( dim_num = 2 )
      integer n
      parameter ( n = 1000 )

      character * ( 255 ) output_filename
      integer seed
      double precision x(dim_num,n)

      output_filename = 'uniform_in_sphere01_map.txt'
      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST20'
      write ( *, '(a)' ) '  UNIFORM_IN_SPHERE01_MAP maps uniform '
      write ( *, '(a)' ) '  points into the unit sphere.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)'  ) '  Spatial dimension DIM_NUM =   ', dim_num
      write ( *, '(a,i8)'  ) '  Number of points N =          ', n
      write ( *, '(a,i12)' ) '  Initial random number SEED =  ', seed

      call uniform_in_sphere01_map ( dim_num, n, seed, x )

      write ( *, '(a,i12)' ) '  Final random number SEED =    ', seed

      call r8mat_write ( output_filename, dim_num, n, x )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Data written to file "' 
     &  // trim ( output_filename ) // '".'

      return
      end
      subroutine test205 ( )

c*********************************************************************72
c
cc TEST205 tests UNIFORM_IN_TETRAHEDRON.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    10 December 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 1000 )

      character * ( 255 ) output_filename
      integer seed
      double precision v(3,4)
      double precision x(3,n)

      save v

      data v /
     &  1.0D+00,  2.0D+00,  3.0D+00, 
     &  4.0D+00,  1.0D+00,  2.0D+00, 
     &  2.0D+00,  4.0D+00,  4.0D+00, 
     &  3.0D+00,  2.0D+00,  5.0D+00 /

      output_filename = 'uniform_in_tetrahedron.txt'
      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST205'
      write ( *, '(a)' ) '  UNIFORM_IN_TETRAHEDRON returns uniform '
      write ( *, '(a)' ) '  points from a tetrahedron.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)'  ) '  Spatial dimension DIM_NUM =   ', 3
      write ( *, '(a,i8)'  ) '  Number of points N =          ', n
      write ( *, '(a,i12)' ) 
     &  '  Initial random number SEED =  ', seed

      call r8mat_print ( 3, 4, v, '  Tetrahedron vertices:' )

      call uniform_in_tetrahedron ( v, n, seed, x )

      write ( *, '(a,i12)' ) '  Final random number SEED =    ', seed

      call r8mat_write ( output_filename, 3, n, x )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Data written to file "' 
     &  // trim ( output_filename ) // '".'

      return
      end
      subroutine test21 ( )

c*********************************************************************72
c
cc TEST21 tests UNIFORM_IN_TRIANGLE_MAP1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    10 December 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer dim_num
      parameter ( dim_num = 2 )
      integer n
      parameter ( n = 1000 )

      character * ( 255 ) output_filename
      integer seed
      double precision v1(dim_num)
      double precision v2(dim_num)
      double precision v3(dim_num)
      double precision x(dim_num,n)

      output_filename = 'uniform_in_triangle_map1.txt'
      seed = 123456789
      v1(1) = 0.75D+00
      v1(2) = 0.90D+00
      v2(1) = 0.00D+00
      v2(2) = 0.20D+00
      v3(1) = 0.95D+00
      v3(2) = 0.65D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST21'
      write ( *, '(a)' ) '  UNIFORM_IN_TRIANGLE_MAP1 maps uniform '
      write ( *, '(a)' ) '  points into a triangle.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)'  ) '  Spatial dimension DIM_NUM =   ', dim_num
      write ( *, '(a,i8)'  ) '  Number of points N =          ', n
      write ( *, '(a,i12)' ) '  Initial random number SEED =  ', seed

      write ( *, '(a)' ) ' '
      write ( *, '(a,2f8.2)' ) '  V1 = ', v1(1:2)
      write ( *, '(a,2f8.2)' ) '  V2 = ', v2(1:2)
      write ( *, '(a,2f8.2)' ) '  V3 = ', v3(1:2)

      call uniform_in_triangle_map1 ( v1, v2, v3, n, seed, x )

      write ( *, '(a,i12)' ) '  Final random number SEED =    ', seed

      call r8mat_write ( output_filename, dim_num, n, x )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Data written to file "' 
     &  // trim ( output_filename ) // '".'

      return
      end
      subroutine test22 ( )

c*********************************************************************72
c
cc TEST22 tests UNIFORM_IN_TRIANGLE_MAP2.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    10 December 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer dim_num
      parameter ( dim_num = 2 )
      integer n
      parameter ( n = 1000 )

      character * ( 255 ) output_filename
      integer seed
      double precision v1(dim_num)
      double precision v2(dim_num)
      double precision v3(dim_num)
      double precision x(dim_num,n)

      output_filename = 'uniform_in_triangle_map2.txt'
      seed = 123456789
      v1(1) = 0.75D+00
      v1(2) = 0.90D+00
      v2(1) = 0.00D+00
      v2(2) = 0.20D+00
      v3(1) = 0.95D+00
      v3(2) = 0.65D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST22'
      write ( *, '(a)' ) '  UNIFORM_IN_TRIANGLE_MAP maps uniform '
      write ( *, '(a)' ) '  points into a triangle.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)'  ) '  Spatial dimension DIM_NUM =   ', dim_num
      write ( *, '(a,i8)'  ) '  Number of points N =          ', n
      write ( *, '(a,i12)' ) '  Initial random number SEED =  ', seed

      write ( *, '(a)' ) ' '
      write ( *, '(a,2f8.2)' ) '  V1 = ', v1(1:2)
      write ( *, '(a,2f8.2)' ) '  V2 = ', v2(1:2)
      write ( *, '(a,2f8.2)' ) '  V3 = ', v3(1:2)

      call uniform_in_triangle_map2 ( v1, v2, v3, n, seed, x )

      write ( *, '(a,i12)' ) '  Final random number SEED =    ', seed

      call r8mat_write ( output_filename, dim_num, n, x )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Data written to file "' 
     &  // trim ( output_filename ) // '".'

      return
      end
      subroutine test23 ( )

c*********************************************************************72
c
cc TEST23 tests UNIFORM_IN_TRIANGLE01_MAP.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    10 December 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer dim_num
      parameter ( dim_num = 2 )
      integer n
      parameter ( n = 2000 )

      character * ( 255 ) output_filename
      integer seed
      double precision x(dim_num,n)

      output_filename = 'uniform_in_triangle01_map.txt'
      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST23'
      write ( *, '(a)' ) '  UNIFORM_IN_TRIANGLE01_MAP maps uniform '
      write ( *, '(a)' ) '  points into the unit triangle.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)'  ) '  Spatial dimension DIM_NUM =   ', dim_num
      write ( *, '(a,i8)'  ) '  Number of points N =          ', n
      write ( *, '(a,i12)' ) '  Initial random number SEED =  ', seed

      call uniform_in_triangle01_map ( n, seed, x )

      write ( *, '(a,i12)' ) '  Final random number SEED =    ', seed

      call r8mat_write ( output_filename, dim_num, n, x )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Data written to file "' 
     &  // trim ( output_filename ) // '".'

      return
      end
      subroutine test235 ( )

c*********************************************************************72
c
cc TEST235 tests UNIFORM_ON_CUBE01.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    10 December 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer m
      parameter ( m = 3 )
      integer n
      parameter ( n = 200 )

      character * ( 255 ) output_filename
      integer seed
      double precision x(m,n)

      output_filename = 'uniform_on_cube01.txt'
      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST235'
      write ( *, '(a)' ) 
     &  '  UNIFORM_ON_CUBE01 samples N uniform points on'
      write ( *, '(a)' ) '  the surface of the unit M-dimensional cube.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)'  ) '  Spatial dimension DIM_NUM =   ', m
      write ( *, '(a,i8)'  ) '  Number of points N =          ', n
      write ( *, '(a,i12)' ) '  Initial random number SEED =  ', seed

      call uniform_on_cube01 ( m, n, seed, x )

      write ( *, '(a,i12)' ) '  Final random number SEED =    ', seed

      call r8mat_write ( output_filename, m, n, x )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Data written to file "' 
     &  // trim ( output_filename ) // '".'

      return
      end
      subroutine test24 ( )

c*********************************************************************72
c
cc TEST24 tests UNIFORM_ON_ELLIPSOID_MAP.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    10 December 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer dim_num
      parameter ( dim_num = 2 )
      integer n
      parameter ( n = 200 )

      character * ( 255 ) output_filename
      double precision a(dim_num,dim_num)
      double precision r
      integer seed
      double precision x(dim_num,n)

      save a

      data a /
     &  3.0D+00, 1.0D+00, 
     &  1.0D+00, 2.0D+00 /

      output_filename = 'uniform_on_ellipsoid_map.txt'
      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST24'
      write ( *, '(a)' ) '  UNIFORM_ON_ELLIPSOID_MAP maps uniform '
      write ( *, '(a)' ) '  points onto an ellipsoid.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)'  ) '  Spatial dimension DIM_NUM =   ', dim_num
      write ( *, '(a,i8)'  ) '  Number of points N =          ', n
      write ( *, '(a,i12)' ) '  Initial random number SEED =  ', seed

      r = 1.0D+00

      call uniform_on_ellipsoid_map ( dim_num, n, a, r, seed, x )

      write ( *, '(a,i12)' ) '  Final random number SEED =    ', seed

      call r8mat_write ( output_filename, dim_num, n, x )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Data written to file "' 
     &  // trim ( output_filename ) // '".'

      return
      end
      subroutine test245 ( )

c*********************************************************************72
c
cc TEST245 tests UNIFORM_ON_HEMISPHERE01_PHONG.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    10 December 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer dim_num
      parameter ( dim_num = 3 )
      integer n
      parameter ( n = 50 )

      character * ( 255 ) output_filename
      integer m
      parameter ( m = 2 )
      integer seed
      double precision x(dim_num,n)

      output_filename = 'uniform_on_hemisphere01_phong.txt'
      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST245'
      write ( *, '(a)' ) '  UNIFORM_ON_HEMISPHERE01_PHONG maps uniform '
      write ( *, '(a)' ) 
     &  '  points onto the unit hemisphere with Phong density.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)'  ) '  Spatial dimension DIM_NUM =   ', dim_num
      write ( *, '(a,i8)'  ) '  Number of points N =          ', n
      write ( *, '(a,i8)'  ) '  Phong exponent M =            ', m
      write ( *, '(a,i12)' ) '  Initial random number SEED =  ', seed

      call uniform_on_hemisphere01_phong ( n, m, seed, x )

      write ( *, '(a,i12)' ) '  Final random number SEED =    ', seed

      call r8mat_write ( output_filename, dim_num, n, x )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Data written to file "' 
     &  // trim ( output_filename ) // '".'

      return
      end
      subroutine test25 ( )

c*********************************************************************72
c
cc TEST25 tests UNIFORM_ON_SIMPLEX01_MAP.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    10 December 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer dim_num
      parameter ( dim_num = 2 )
      integer n
      parameter ( n = 50 )

      character * ( 255 ) output_filename
      integer seed
      double precision x(dim_num,n)

      output_filename = 'uniform_on_simplex01_map.txt'
      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST25'
      write ( *, '(a)' ) '  UNIFORM_ON_SIMPLEX01_MAP maps uniform '
      write ( *, '(a)' ) '  points onto the unit simplex.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)'  ) '  Spatial dimension DIM_NUM =   ', dim_num
      write ( *, '(a,i8)'  ) '  Number of points N =          ', n
      write ( *, '(a,i12)' ) '  Initial random number SEED =  ', seed

      call uniform_on_simplex01_map ( dim_num, n, seed, x )

      write ( *, '(a,i12)' ) '  Final random number SEED =    ', seed

      call r8mat_write ( output_filename, dim_num, n, x )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Data written to file "' 
     &  // trim ( output_filename ) // '".'

      return
      end
      subroutine test26 ( )

c*********************************************************************72
c
cc TEST26 tests UNIFORM_ON_SPHERE01_MAP.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    10 December 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer dim_num
      parameter ( dim_num = 2 )
      integer n
      parameter ( n = 50 )

      character * ( 255 ) output_filename
      integer seed
      double precision x(dim_num,n)

      output_filename = 'uniform_on_sphere01_map.txt'
      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST26'
      write ( *, '(a)' ) '  UNIFORM_ON_SPHERE01_MAP maps uniform '
      write ( *, '(a)' ) 
     &  '  points onto the unit sphere, in any dimension.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)'  ) '  Spatial dimension DIM_NUM =   ', dim_num
      write ( *, '(a,i8)'  ) '  Number of points N =          ', n
      write ( *, '(a,i12)' ) '  Initial random number SEED =  ', seed

      call uniform_on_sphere01_map ( dim_num, n, seed, x )

      write ( *, '(a,i12)' ) '  Final random number SEED =    ', seed

      call r8mat_write ( output_filename, dim_num, n, x )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Data written to file "' 
     &  // trim ( output_filename ) // '".'

      return
      end
      subroutine test264 ( )

c*********************************************************************72
c
cc TEST264 tests UNIFORM_ON_SPHERE01_PATCH_TP.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    10 December 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 5000 )

      character * ( 255 ) output_filename
      double precision phi1
      double precision phi2
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      integer seed
      double precision theta1
      double precision theta2
      double precision tp(2,n)

      output_filename = 'uniform_on_sphere01_patch_tp.txt'
      seed = 123456789

      phi1 = 0.0D+00 * ( pi / 180.0D+00 )
      phi2 = 180.0D+00 * ( pi / 180.0D+00 )
      theta1 =  0.0D+00 * ( pi / 360.0D+00 )
      theta2 = 30.0D+00 * ( pi / 360.0D+00 )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST264'
      write ( *, '(a)' ) '  UNIFORM_ON_SPHERE01_PATCH_TP maps uniform '
      write ( *, '(a)' ) 
     &  '  points onto a TP (THETA,PHI) patch of the unit sphere.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)'    ) '  Spatial dimension DIM_NUM =   ', 3
      write ( *, '(a,i8)'    ) '  Data dimension =              ', 2
      write ( *, '(a,i8)'    ) '  Number of points N =          ', n
      write ( *, '(a,g14.6)' ) 
     &  '  Latitudinal angle PHI1 =      ', phi1
      write ( *, '(a,g14.6)' ) 
     &  '  Latitudinal angle PHI2 =      ', phi2
      write ( *, '(a,g14.6)' ) 
     &  '  Longitudinal angle THETA1 =   ', theta1
      write ( *, '(a,g14.6)' ) 
     &  '  Longitudinal angle THETA2 =   ', theta2
      write ( *, '(a,i12)'   ) '  Initial random number SEED =  ', seed

      call uniform_on_sphere01_patch_tp ( n, phi1, phi2, theta1, 
     &  theta2, seed, tp )

      write ( *, '(a,i12)' ) '  Final random number SEED =    ', seed

      call r8mat_write ( output_filename, 2, n, tp )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Data written to file "' 
     &  // trim ( output_filename ) // '".'

      return
      end
      subroutine test265 ( )

c*********************************************************************72
c
cc TEST265 tests UNIFORM_ON_SPHERE01_PATCH_XYZ.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    10 December 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer dim_num
      parameter ( dim_num = 3 )
      integer n
      parameter ( n = 50 )

      character * ( 255 ) output_filename
      double precision phi1
      double precision phi2
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      integer seed
      double precision theta1
      double precision theta2
      double precision x(dim_num,n)

      output_filename = 'uniform_on_sphere01_patch_xyz.txt'
      seed = 123456789

      phi1 = 75.0D+00 * ( pi / 180.0D+00 )
      phi2 = 90.0D+00 * ( pi / 180.0D+00 )
      theta1 =  0.0D+00 * ( pi / 360.0D+00 )
      theta2 = 30.0D+00 * ( pi / 360.0D+00 )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST265'
      write ( *, '(a)' ) '  UNIFORM_ON_SPHERE01_PATCH_XYZ maps uniform '
      write ( *, '(a)' ) 
     &  '  points onto an XYZ patch of the unit sphere.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)'    ) 
     &  '  Spatial dimension DIM_NUM =   ', dim_num
      write ( *, '(a,i8)'    ) '  Number of points N =          ', n
      write ( *, '(a,g14.6)' ) 
     &  '  Latitudinal angle PHI1 =      ', phi1
      write ( *, '(a,g14.6)' ) 
     &  '  Latitudinal angle PHI2 =      ', phi2
      write ( *, '(a,g14.6)' ) 
     &  '  Longitudinal angle THETA1 =   ', theta1
      write ( *, '(a,g14.6)' ) 
     &  '  Longitudinal angle THETA2 =   ', theta2
      write ( *, '(a,i12)'   ) 
     &  '  Initial random number SEED =  ', seed

      call uniform_on_sphere01_patch_xyz ( n, phi1, phi2, theta1, 
     &  theta2, seed, x )

      write ( *, '(a,i12)' ) '  Final random number SEED =    ', seed

      call r8mat_write ( output_filename, dim_num, n, x )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Data written to file "' 
     &  // trim ( output_filename ) // '".'

      return
      end
      subroutine test267 ( )

c*********************************************************************72
c
cc TEST267 tests UNIFORM_ON_SPHERE01_TRIANGLE_XYZ.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    10 December 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 500 )

      character * ( 255 ) output_filename
      integer seed
      double precision v1(3)
      double precision v2(3)
      double precision v3(3)
      double precision x(3,n)

      output_filename = 'uniform_on_sphere01_triangle_xyz.txt'
      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST267'
      write ( *, '(a)' ) 
     &  '  UNIFORM_ON_SPHERE01_TRIANGLE_XYZ maps uniform '
      write ( *, '(a)' ) 
     &  '  points onto a spherical triangle using XYZ coordinates.'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)'    ) '  Spatial dimension DIM_NUM =   ', 3
      write ( *, '(a,i8)'    ) '  Number of points N =          ', n
      write ( *, '(a,i12)'   ) '  Initial random number SEED =  ', seed

      if ( .true. ) then

        call uniform_on_sphere01_map ( 3, 1, seed, v1 )
        call uniform_on_sphere01_map ( 3, 1, seed, v2 )
        call uniform_on_sphere01_map ( 3, 1, seed, v3 )

      else

        v1(1) = 1.0D+00
        v1(2) = 0.0D+00
        v1(2) = 0.0D+00
        v2(1) = 0.0D+00
        v2(2) = 1.0D+00
        v2(2) = 0.0D+00
        v3(1) = 0.0D+00
        v3(2) = 0.0D+00
        v3(2) = 1.0D+00

      end if

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Vertices of spherical triangle:'
      write ( *, '(a)' ) ' '
      write ( *, '(a,3g14.6)' ) '  V1:', v1(1:3)
      write ( *, '(a,3g14.6)' ) '  V2:', v2(1:3)
      write ( *, '(a,3g14.6)' ) '  V3:', v3(1:3)

      call uniform_on_sphere01_triangle_xyz ( n, v1, v2, v3, seed, x )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i12)' ) '  Final random number SEED =    ', seed

      call r8mat_write ( output_filename, 3, n, x )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Data written to file "' 
     &  // trim ( output_filename ) // '".'

      return
      end
      subroutine test27 ( )

c*********************************************************************72
c
cc TEST27 tests UNIFORM_WALK.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    10 December 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer dim_num
      parameter ( dim_num = 2 )
      integer n 
      parameter ( n = 400 )

      character * ( 255 ) output_filename
      integer seed
      double precision x(dim_num,n)

      output_filename = 'uniform_walk.txt'
      seed = 123456789

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST27:'
      write ( *, '(a)' ) 
     &  '  UNIFORM_WALK generates points on a uniform random walk'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)'  ) '  Spatial dimension DIM_NUM =   ', dim_num
      write ( *, '(a,i8)'  ) '  Number of points N =          ', n
      write ( *, '(a,i12)' ) '  Initial random number SEED =  ', seed

      call uniform_walk ( dim_num, n, seed, x )

      write ( *, '(a,i12)' ) '  Final random number SEED =    ', seed

      call scale_to_block01 ( dim_num, n, x )

      call r8mat_write ( output_filename, dim_num, n, x )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Data written to file "' 
     &  // trim ( output_filename ) // '".'

      return
      end
