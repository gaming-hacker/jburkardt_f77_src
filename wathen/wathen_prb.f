      program main

c*********************************************************************72
c
cc MAIN is the main program for WATHEN_PRB.
c
c  Discussion:
c
c    WATHEN_PRB tests the WATHEN library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 June 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'WATHEN_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the WATHEN library.'

      call test01 ( )
      call test02 ( )
      call test05 ( )
      call test06 ( )
      call test07 ( )
      call test08 ( )
      call test10 ( )
      call test11 ( )
      call test115 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'WATHEN_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 assembles, factor and solve using WATHEN_GE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 June 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 65 )

      double precision a(n_max,n_max)
      double precision b(n_max)
      double precision e
      integer i
      integer info
      integer ipvt(n_max)
      integer job
      integer n
      integer nx
      integer ny
      integer seed
      double precision x1(n_max)
      double precision x2(n_max)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  Assemble, factor and solve a Wathen system'
      write ( *, '(a)' ) '  defined by WATHEN_GE.'
      write ( *, '(a)' ) ''

      nx = 4
      ny = 4
      write ( *, '(a,i6)' ) '  Elements in X direction NX = ', nx
      write ( *, '(a,i6)' ) '  Elements in Y direction NY = ', ny
      write ( *, '(a,i6)' ) '  Number of elements = ', nx * ny
c
c  Compute the number of unknowns.
c
      call wathen_order ( nx, ny, n )
      write ( *, '(a,i6)' ) '  Number of nodes N = ', n
c
c  Set up a random solution X.
c
      seed = 123456789
      call r8vec_uniform_01 ( n, seed, x1 )
c
c  Compute the matrix.
c
      seed = 123456789
      call wathen_ge ( nx, ny, n, seed, a )
c
c  Compute the corresponding right hand side B.
c
      call mv_ge ( n, n, a, x1, b )
c
c  Solve the linear system.
c
      call dgefa ( a, n, n, ipvt, info )

      do i = 1, n
        x2(i) = b(i)
      end do
      job = 0

      call dgesl ( a, n, n, ipvt, x2, job )
c
c  Compute the maximum solution error.
c
      e = 0.0D+00
      do i = 1, n
        e = max ( e, abs ( x1(i) - x2(i) ) )
      end do
      write ( *, '(a,g14.6)' ) '  Maximum solution error is ', e

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 assembles, factors and solves using WATHEN_GB.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 June 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 65 )

      double precision a(49,n_max)
      double precision b(n_max)
      double precision e
      integer i
      integer info
      integer ipvt(n_max)
      integer j
      integer jhi
      integer jlo
      integer job
      integer lda
      integer md
      integer ml
      integer mu
      integer n
      integer nx
      integer ny
      integer seed
      double precision x1(n_max)
      double precision x2(n_max)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) '  Assemble, factor and solve a Wathen system'
      write ( *, '(a)' ) '  using WATHEN_GB.'
      write ( *, '(a)' ) ''

      nx = 4
      ny = 4
      write ( *, '(a,i6)' ) '  Elements in X direction NX = ', nx
      write ( *, '(a,i6)' ) '  Elements in Y direction NY = ', ny
      write ( *, '(a,i6)' ) '  Number of elements = ', nx * ny
c
c  Compute the number of unknowns.
c
      call wathen_order ( nx, ny, n )
      write ( *, '(a,i6)' ) '  Number of nodes N = ', n
c
c  Compute the bandwidth.
c
      call wathen_bandwidth ( nx, ny, ml, md, mu )
      write ( *, '(a,i6)' ) '  Lower bandwidth ML = ', ml
      write ( *, '(a,i6)' ) '  Upper bandwidth MU = ', mu
c
c  Set up a random solution X1.
c
      seed = 123456789
      call r8vec_uniform_01 ( n, seed, x1 )
c
c  Compute the matrix.
c
      seed = 123456789
      call wathen_gb ( nx, ny, n, seed, a )
c
c  Compute the corresponding right hand side B.
c
      call mv_gb ( n, n, ml, mu, a, x1, b )
c
c  Solve the linear system.
c
      lda = 2 * ml + mu + 1
      call dgbfa ( a, lda, n, ml, mu, ipvt, info )

      do i = 1, n
        x2(i) = b(i)
      end do
      job = 0
      call dgbsl ( a, lda, n, ml, mu, ipvt, x2, job )
c
c  Compute the maximum solution error.
c
      e = 0.0D+00
      do i = 1, n
        e = max ( e, abs ( x1(i) - x2(i) ) )
      end do
      write ( *, '(a,g14.6)' ) '  Maximum solution error is ', e

      return
      end
      subroutine test05 ( )

c*********************************************************************72
c
cc TEST05 measures the storage needed for the Wathen system.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 June 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 3201 )

      double precision a(n_max,n_max)
      integer bd1
      integer bd2
      integer bl1
      integer bl2
      integer bu1
      integer bu2
      integer bw1
      integer bw2
      integer n
      integer nx
      integer ny
      integer seed
      integer storage_gb
      integer storage_ge
      integer storage_sparse
      integer test

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST05'
      write ( *, '(a)' ) 
     &  '  For various problem sizes and storage schemes,'
      write ( *, '(a)' ) 
     &  '  measure the storage used for the Wathen system.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '                                   Predicted  Observed'
      write ( *, '(a)' ) 
     &  '                              GE        ' //
     &  'Band      Band      Band    Sparse'
      write ( *, '(a)' ) 
     &  '    NX  Elements   Nodes   storage     width  ' //
     &  '   width   storage   storage'
      write ( *, '(a)' ) ''

      nx = 1
      ny = 1

      do test = 1, 6
c
c  Compute the number of unknowns.
c
        call wathen_order ( nx, ny, n )
c
c  Predict the bandwidth.
c
        call wathen_bandwidth ( nx, ny, bl1, bd1, bu1 )
        bw1 = bl1 + bd1 + bu1
c
c  Compute the matrix.
c
        seed = 123456789
        call wathen_ge ( nx, ny, n, seed, a )

        storage_ge = n * n

        call bandwidth ( n, n, a, bw2, bl2, bd2, bu2 )
        storage_gb = ( 2 * bl2 + 1 + bu2 ) * n

        call nonzeros ( n, n, a, storage_sparse )
c
c  Report.
c
        write ( *, '(2x,i4,6x,i4,2x,i6,2x,i8,2x,i8,2x,i8,2x,i8,2x,i8)' )
     &    nx, nx * ny, n, storage_ge, bw1, bw2, storage_gb, 
     &    storage_sparse
c
c  Ready for next iteration.
c
        nx = nx * 2
        ny = ny * 2

      end do

      return
      end
      subroutine test06 ( )

c*********************************************************************72
c
cc TEST06 times WATHEN_GE assembly and solution.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 June 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 3201 )

      double precision a(n_max,n_max)
      double precision b(n_max)
      double precision e
      integer i
      integer info
      integer ipvt(n_max)
      integer job
      integer n
      integer nx
      integer ny
      integer seed
      integer storage_ge
      double precision t0
      double precision t1
      double precision t2
      integer test
      double precision x1(n_max)
      double precision x2(n_max)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST06'
      write ( *, '(a)' ) '  For various problem sizes,'
      write ( *, '(a)' ) 
     &  '  time the assembly and factorization of a Wathen system'
      write ( *, '(a)' ) '  using the WATHEN_GE function.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '    NX  Elements   Nodes   Storage    Assembly' //
     &  '      Factor      Error'
      write ( *, '(a)' ) ''

      nx = 1
      ny = 1

      do test = 1, 6
c
c  Compute the number of unknowns.
c
        call wathen_order ( nx, ny, n )
        storage_ge = n * n
c
c  Set up a random solution X1.
c
        seed = 123456789
        call r8vec_uniform_01 ( n, seed, x1 )
c
c  Compute the matrix, and measure the storage required.
c
        seed = 123456789

        call cpu_time ( t0 )
        call wathen_ge ( nx, ny, n, seed, a )
        call cpu_time ( t1 )
        t1 = t1 - t0
c
c  Compute the corresponding right hand side B.
c
        call mv_ge ( n, n, a, x1, b )
c
c  Solve the system.
c
        do i = 1, n
          x2(i) = b(i)
        end do
        job = 0

        call cpu_time ( t0 )
        call dgefa ( a, n, n, ipvt, info )
        call dgesl ( a, n, n, ipvt, x2, job )
        call cpu_time ( t2 )
        t2 = t2 - t0
c
c  Compute the maximum solution error.
c
        e = 0.0D+00
        do i = 1, n
          e = max ( e, abs ( x1(i) - x2(i) ) )
        end do
c
c  Report.
c
        write ( *, 
     &    '(2x,i4,6x,i4,2x,i6,2x,i8,2x,e10.2,2x,e10.2,2x,e10.2)' )
     &    nx, nx * ny, n, storage_ge, t1, t2, e
c
c  Ready for next iteration.
c
        nx = nx * 2
        ny = ny * 2

      end do

      return
      end
      subroutine test07 ( )

c*********************************************************************72
c
cc  TEST07 times WATHEN_GB assembly and solution.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 June 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 3201 )

      double precision a(49,n_max)
      double precision b(n_max)
      double precision e
      integer i
      integer info
      integer ipvt(n_max)
      integer j
      integer jhi
      integer jlo
      integer job
      integer lda
      integer md
      integer ml
      integer mu
      integer n
      integer nx
      integer ny
      integer seed
      integer storage_gb
      double precision t0
      double precision t1
      double precision t2
      integer test
      double precision x1(n_max)
      double precision x2(n_max)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST07'
      write ( *, '(a)' ) '  For various problem sizes,'
      write ( *, '(a)' ) 
     &  '  time the assembly and factorization of a Wathen system'
      write ( *, '(a)' ) '  using the WATHEN_GB function.'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 
     &  '    NX  Elements   Nodes   Storage    Assembly' //
     &  '      Factor      Error'
      write ( *, '(a)' ) ''

      nx = 1
      ny = 1

      do test = 1, 6
c
c  Compute the number of unknowns.
c
        call wathen_order ( nx, ny, n )
c
c  Compute the bandwidth.
c
        call wathen_bandwidth ( nx, ny, ml, md, mu )
        storage_gb = ( 2 * ml + mu + 1 ) * n
c
c  Set up a random solution X1.
c
        seed = 123456789
        call r8vec_uniform_01 ( n, seed, x1 )
c
c  Compute the matrix.
c
        seed = 123456789
        call cpu_time ( t0 )
        call wathen_gb ( nx, ny, n, seed, a )
        call cpu_time ( t1 )
        t1 = t1 - t0
c
c  Compute the corresponding right hand side B.
c
        call mv_gb ( n, n, ml, mu, a, x1, b )
c
c  Solve the system.
c
        lda = 2 * ml + mu + 1
        do i = 1, n
          x2(i) = b(i)
        end do
        job = 0

        call cpu_time ( t0 )
        call dgbfa ( a, lda, n, ml, mu, ipvt, info )
        call dgbsl ( a, lda, n, ml, mu, ipvt, x2, job )
        call cpu_time ( t2 )
        t2 = t2 - t0
c
c  Compute the maximum solution error.
c
        e = 0.0D+00
        do i = 1, n
          e = max ( e, abs ( x1(i) - x2(i) ) )
        end do
c
c  Report.
c
        write ( *, 
     &    '(2x,i4,6x,i4,2x,i6,2x,i8,2x,e10.2,2x,e10.2,2x,e10.2)' )
     &    nx, nx * ny, n, storage_gb, t1, t2, e
c
c  Ready for next iteration.
c
        nx = nx * 2
        ny = ny * 2

      end do

      return
      end
      subroutine test08 ( )

c*********************************************************************72
c
cc TEST08 times WATHEN_GE/WATHEN_GB.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 June 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 3201 )

      double precision a(n_max,n_max)
      double precision b(n_max)
      double precision e
      integer i
      integer info
      integer ipvt(n_max)
      integer j
      integer jhi
      integer jlo
      integer job
      integer lda
      integer md
      integer ml
      integer mu
      integer n
      integer nx
      integer ny
      integer seed
      integer storage_gb
      integer storage_ge
      double precision t0
      double precision t1
      double precision t2
      integer test
      double precision x1(n_max)
      double precision x2(n_max)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST08'
      write ( *, '(a)' ) '  For various problem sizes,'
      write ( *, '(a)' ) 
     &  '  time the assembly and factorization of a Wathen system'
      write ( *, '(a)' ) '  WATHEN_GE/WATHEN_GB'
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '                   NX  Elements   Nodes   ' //
     &  'Storage    Assembly      Factor      Error'

      nx = 1
      ny = 1

      do test = 1, 6
c
c  Compute the number of unknowns.
c
        call wathen_order ( nx, ny, n )
        storage_ge = n * n
c
c  Set up a random solution X1.
c
        seed = 123456789
        call r8vec_uniform_01 ( n, seed, x1 )
c
c  Compute the matrix.
c
        seed = 123456789
        call cpu_time ( t0 )
        call wathen_ge ( nx, ny, n, seed, a )
        call cpu_time ( t1 )
        t1 = t1 - t0
c
c  Compute the corresponding right hand side B.
c
        call mv_ge ( n, n, a, x1, b )
c
c  Solve the system.
c
        do i = 1, n
          x2(i) = b(i)
        end do
        job = 0

        call cpu_time ( t0 )
        call dgefa ( a, n, n, ipvt, info )
        call dgesl ( a, n, n, ipvt, x2, job )
        call cpu_time ( t2 )
        t2 = t2 - t0
c
c  Compute the maximum solution error.
c
        e = 0.0D+00
        do i = 1, n
          e = max ( e, abs ( x1(i) - x2(i) ) )
        end do
c
c  Report.
c
        write ( *, '(a)' ) ''
        write ( *, 
     &    '(2x,a,2x,i4,6x,i4,2x,i6,2x,i8,2x,e10.2,2x,e10.2,2x,e10.2)' )
     &    'WATHEN_GE    ', nx, nx * ny, n, storage_ge, t1, t2, e
c
c  Compute the bandwidth.
c
        call wathen_bandwidth ( nx, ny, ml, md, mu )
        storage_gb = ( 2 * ml + mu + 1 ) * n
c
c  Set up a random solution X1.
c
        seed = 123456789
        call r8vec_uniform_01 ( n, seed, x1 )
c
c  Compute the matrix.
c
        seed = 123456789
        call cpu_time ( t0 )
        call wathen_gb ( nx, ny, n, seed, a )
        call cpu_time ( t1 )
        t1 = t1 - t0
c
c  Compute the corresponding right hand side B.
c
        call mv_gb ( n, n, ml, mu, a, x1, b )
c
c  Solve the system.
c
        lda = 2 * ml + mu + 1
        do i = 1, n
          x2(i) = b(i)
        end do
        job = 0

        call cpu_time ( t0 )
        call dgbfa ( a, lda, n, ml, mu, ipvt, info )
        call dgbsl ( a, lda, n, ml, mu, ipvt, x2, job )
        call cpu_time ( t2 )
        t2 = t2 - t0
c
c  Compute the maximum solution error.
c
        e = 0.0D+00
        do i = 1, n
          e = max ( e, abs ( x1(i) - x2(i) ) )
        end do
c
c  Report.
c
        write ( *, 
     &    '(2x,a,2x,i4,6x,i4,2x,i6,2x,i8,2x,e10.2,2x,e10.2,2x,e10.2)' )
     &    'WATHEN_GB    ', nx, nx * ny, n, storage_gb, t1, t2, e
c
c  Ready for next iteration.
c
        nx = nx * 2
        ny = ny * 2

      end do

      return
      end
      subroutine test10 ( )

c*********************************************************************72
c
cc TEST10 assembles, factor and solve using WATHEN_GE and CG_GE.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 June 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 8 )

      double precision a(n_max,n_max)
      double precision b(n_max)
      double precision e
      integer i
      integer n
      integer nx
      integer ny
      integer seed
      double precision x1(n_max)
      double precision x2(n_max)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST10'
      write ( *, '(a)' ) '  Assemble, factor and solve a Wathen system'
      write ( *, '(a)' ) '  defined by WATHEN_GE and CG_GE.'
      write ( *, '(a)' ) ''

      nx = 1
      ny = 1
      write ( *, '(a,i6)' ) '  Elements in X direction NX = ', nx
      write ( *, '(a,i6)' ) '  Elements in Y direction NY = ', ny
      write ( *, '(a,i6)' ) '  Number of elements = ', nx * ny
c
c  Compute the number of unknowns.
c
      call wathen_order ( nx, ny, n )
      write ( *, '(a,i6)' ) '  Number of nodes N = ', n
c
c  Set up a random solution X.
c
      seed = 123456789
      call r8vec_uniform_01 ( n, seed, x1 )
c
c  Compute the matrix.
c
      seed = 123456789
      call wathen_ge ( nx, ny, n, seed, a )
c
c  Compute the corresponding right hand side B.
c
      call mv_ge ( n, n, a, x1, b )
c
c  Solve the linear system.
c
      do i = 1, n
        x2(i) = 1.0D+00
      end do
      call cg_ge ( n, a, b, x2 )
c
c  Compute the maximum solution error.
c
      e = 0.0D+00
      do i = 1, n
        e = max ( e, abs ( x1(i) - x2(i) ) )
      end do
      write ( *, '(a,g14.6)' ) '  Maximum solution error is ', e

      return
      end
      subroutine test11 ( )

c*********************************************************************72
c
cc TEST11 assemble, factor and solve using WATHEN_ST + CG_ST.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 June 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 8 )
      integer nz_num_max
      parameter ( nz_num_max = 64 )

      double precision a(nz_num_max)
      double precision b(n_max)
      integer col(nz_num_max)
      double precision e
      integer i
      integer n
      integer nx
      integer ny
      integer nz_num
      integer row(nz_num_max)
      integer seed
      double precision x1(n_max)
      double precision x2(n_max)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST11'
      write ( *, '(a)' ) '  Assemble, factor and solve a Wathen system'
      write ( *, '(a)' ) '  defined by WATHEN_ST and CG_ST.'
      write ( *, '(a)' ) ''

      nx = 1
      ny = 1
      write ( *, '(a,i6)' ) '  Elements in X direction NX = ', nx
      write ( *, '(a,i6)' ) '  Elements in Y direction NY = ', ny
      write ( *, '(a,i6)' ) '  Number of elements = ', nx * ny
c
c  Compute the number of unknowns.
c
      call wathen_order ( nx, ny, n )
      write ( *, '(a,i6)' ) '  Number of nodes N = ', n
c
c  Set up a random solution X1.
c
      seed = 123456789
      call r8vec_uniform_01 ( n, seed, x1 )
c
c  Compute the matrix size.
c
      call wathen_st_size ( nx, ny, nz_num )
      write ( *, '(a,i6)' ) '  Number of nonzeros NZ_NUM = ', nz_num
c
c  Compute the matrix.
c
      seed = 123456789
      call wathen_st ( nx, ny, nz_num, seed, row, col, a )
c
c  Compute the corresponding right hand side B.
c
      call mv_st ( n, n, nz_num, row, col, a, x1, b )
c
c  Solve the linear system.
c
      do i = 1, n
        x2(i) = 1.0D+00
      end do
      call cg_st ( n, nz_num, row, col, a, b, x2 )
c
c  Compute the maximum solution error.
c
      e = 0.0D+00
      do i = 1, n
        e = max ( e, abs ( x1(i) - x2(i) ) )
      end do
      write ( *, '(a,g14.6)' ) '  Maximum solution error is ', e

      return
      end
      subroutine test115 ( )

c*********************************************************************72
c
cc TEST115 assembles, factors and solves using WATHEN_GB and CG_GB.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 June 2014
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n_max
      parameter ( n_max = 65 )
 
      double precision a(49,n_max)
      double precision b(n_max)
      double precision e
      integer i
      integer j
      integer jhi
      integer jlo
      integer lda
      integer md
      integer ml
      integer mu
      integer n
      integer nx
      integer ny
      integer seed
      double precision x1(n_max)
      double precision x2(n_max)

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TEST115'
      write ( *, '(a)' ) '  Assemble, factor and solve a Wathen system'
      write ( *, '(a)' ) '  using WATHEN_GB and CG_GB.'
      write ( *, '(a)' ) ''

      nx = 4
      ny = 4
      write ( *, '(a,i6)' ) '  Elements in X direction NX = ', nx
      write ( *, '(a,i6)' ) '  Elements in Y direction NY = ', ny
      write ( *, '(a,i6)' ) '  Number of elements = ', nx * ny
c
c  Compute the number of unknowns.
c
      call wathen_order ( nx, ny, n )
      write ( *, '(a,i6)' ) '  Number of nodes N = ', n
c
c  Compute the bandwidth.
c
      call wathen_bandwidth ( nx, ny, ml, md, mu )
      write ( *, '(a,i6)' ) '  Lower bandwidth ML = ', ml
      write ( *, '(a,i6)' ) '  Upper bandwidth MU = ', mu
c
c  Set up a random solution X1.
c
      seed = 123456789
      call r8vec_uniform_01 ( n, seed, x1 )
c
c  Compute the matrix.
c
      seed = 123456789
      call wathen_gb ( nx, ny, n, seed, a )
c
c  Compute the corresponding right hand side B.
c
      call mv_gb ( n, n, ml, mu, a, x1, b )
c
c  Solve the linear system.
c
      do i = 1, n
        x2(i) = 1.0D+00
      end do

      call cg_gb ( n, ml, mu, a, b, x2 )
c
c  Compute the maximum solution error.
c
      e = 0.0D+00
      do i = 1, n
        e = max ( e, abs ( x1(i) - x2(i) ) )
      end do
      write ( *, '(a,g14.6)' ) '  Maximum solution error is ', e

      return
      end
