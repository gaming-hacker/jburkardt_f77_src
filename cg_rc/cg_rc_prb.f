      program main

c*********************************************************************72
c
cc MAIN is the main program for CG_RC_PRB.
c
c  Discussion:
c
c    CG_RC_PRB tests the CG_RC library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 January 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CG_RC_PRB:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the CG_RC library.'

      call test01 ( )
      call test02 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CG_RC_PRB:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 uses CG_RC for the simple 1, -2, 1 matrix.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 January 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 21 )

      double precision angle
      double precision b(n)
      double precision bnrm2
      double precision err
      integer i
      integer it
      integer it_max
      integer job
      double precision p(n)
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision q(n)
      double precision r(n)
      double precision rnrm2
      double precision tol
      double precision x(n)
      double precision x_exact(n)
      double precision z(n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) '  Use CG_RC on the 1, -2, 1 matrix.'
c
c  In order to specify the right hand side, pick an exact solution,
c  and multiply by the matrix.
c
      do i = 1, n
        angle = 2.0D+00 * pi * dble ( i - 1 ) / dble ( n - 1 )
        x_exact(i) = sin ( angle )
      end do

      do i = 1, n
        b(i) = - 2.0D+00 * x_exact(i)
      end do
      do i = 1, n - 1
        b(i) = b(i) + x_exact(i+1)
      end do
      do i = 2, n
        b(i) = b(i) + x_exact(i-1)
      end do
c
c  Here is the initial guess for the solution.
c
      do i = 1, n
        x(i) = 0.0D+00
      end do
c
c  Two parameters we need for the stopping test.
c
      it = 0
      it_max = 30
      tol = 1.0D-05
      bnrm2 = 0.0D+00
      do i = 1, n
        bnrm2 = bnrm2 + b(i)**2
      end do
      bnrm2 = sqrt ( bnrm2 )
c
c  Set parameters for CG_RC.
c
      job = 1
c
c  Repeatedly call CG_RC, and on return, do what JOB tells you.
c
10    continue

        call cg_rc ( n, b, x, r, z, p, q, job )
c
c  Compute q = A * p
c
        if ( job .eq. 1 ) then

          do i = 1, n
            q(i) = - 2.0D+00 * p(i)
          end do
          do i = 1, n - 1
            q(i) = q(i) + p(i+1)
          end do
          do i = 2, n
            q(i) = q(i) + p(i-1)
          end do
c
c  Solve M * z = r.
c
        else if ( job .eq. 2 ) then

          do i = 1, n
            z(i) = r(i) / ( - 2.0D+00 )
          end do
c
c  Compute r = r - A * x.
c
        else if ( job .eq. 3 ) then

          do i = 1, n
            r(i) = r(i) - 2.0D+00 * x(i)
          end do
          do i = 1, n - 1
            r(i) = r(i) + x(i+1)
          end do
          do i = 2, n
            r(i) = r(i) + x(i-1)
          end do
c
c  Stopping test.
c
        else if ( job .eq. 4 ) then

          rnrm2 = 0.0D+00
          do i = 1, n
            rnrm2 = rnrm2 + r(i)**2
          end do
          rnrm2 = sqrt ( rnrm2 )

          if ( bnrm2 .eq. 0.0D+00 ) then
            if ( rnrm2 .le. tol ) then
              go to 20
            end if
          else
            if ( rnrm2 .le. tol * bnrm2 ) then
              go to 20
            end if
          end if

          it = it + 1

          if ( it_max .le. it ) then
            write ( *, '(a)' ) ' '
            write ( *, '(a)' ) '  Iteration limit exceeded.'
            write ( *, '(a)' ) '  Terminating early.'
            go to 20
          end if

        end if

        job = 2

      go to 10

20    continue
      
      write ( *, '(a)' ) ' '
      write ( *, '(a,i5)' ) '  Number of iterations was ', it
      write ( *, '(a,g14.6)' ) '  Estimated error is ', rnrm2
      err = 0.0D+00
      do i = 1, n
        err = max ( err, abs ( x_exact(i) - x(i) ) )
      end do
      write ( *, '(a,g14.6)' ) '  Loo error is ', err

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '     I      X(I)         X_EXACT(I)        B(I)'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &    i, x(i), x_exact(i), b(i)
      end do

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 tests CG_RC with the Wathen matrix.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 January 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer n
      parameter ( n = 79 )

      double precision a(n,n)
      double precision ax(n)
      double precision b(n)
      double precision bnrm2
      double precision err
      integer i
      integer ii
      integer it
      integer it_max
      integer job
      integer nx
      integer ny
      double precision p(n)
      double precision q(n)
      double precision r(n)
      double precision rnrm2
      integer seed
      double precision tol
      double precision x(n)
      double precision x_exact(n)
      double precision z(n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) '  Use CG_RC to solve a linear system'
      write ( *, '(a)' ) '  involving the Wathen matrix.'

      nx = 5
      ny = 4
 
      write ( *, '(a)' ) ' '
      write ( *, '(a,i6)' ) '  NX = ', nx
      write ( *, '(a,i6)' ) '  NY = ', ny
      write ( *, '(a,i6)' ) '  N  = ', n

      call wathen ( nx, ny, n, a )

      seed = 123456789
      call r8vec_uniform_01 ( n, seed, x_exact )

      call r8mat_mv ( n, n, a, x_exact, b )

      do i = 1, n
        x(i) = 0.0D+00
      end do
c
c  Parameters for the stopping test.
c
      it = 0
      it_max = 30
      tol = 1.0D-05
      bnrm2 = 0.0D+00
      do i = 1, n
        bnrm2 = bnrm2 + b(i)**2
      end do
      bnrm2 = sqrt ( bnrm2 )
c
c  Set parameters for the CG_RC code.
c
      job = 1
c
c  Repeatedly call the CG_RC code, and on return, do what JOB tells you.
c
10    continue

        call cg_rc ( n, b, x, r, z, p, q, job )
c
c  Compute q = A * p.
c
        if ( job .eq. 1 ) then

          call r8mat_mv ( n, n, a, p, q )
c
c  Solve M * z = r.
c
        else if ( job .eq. 2 ) then

          do i = 1, n
            z(i) = r(i) / a(i,i)
          end do
c
c  Compute r = r - A * x.
c
        else if ( job .eq. 3 ) then

          call r8mat_mv ( n, n, a, x, ax )

          do i = 1, n
            r(i) = r(i) - ax(i)
          end do
c
c  Stopping test.
c
        else if ( job .eq. 4 ) then

          rnrm2 = 0.0D+00
          do i = 1, n
            rnrm2 = rnrm2 + r(i)**2
          end do
          rnrm2 = sqrt ( rnrm2 )

          if ( bnrm2 .eq. 0.0D+00 ) then
            if ( rnrm2 .le. tol ) then
              go to 20
            end if
          else
            if ( rnrm2 .le. tol * bnrm2 ) then
              go to 20
            end if
          end if

          it = it + 1

          if ( it_max .le. it ) then
            write ( *, '(a)' ) ' '
            write ( *, '(a)' ) '  Iteration limit exceeded.'
            write ( *, '(a)' ) '  Terminating early.'
            go to 20
          end if

        end if

        job = 2

      go to 10

20    continue
      
      write ( *, '(a)' ) ' '
      write ( *, '(a,i5)' ) '  Number of iterations was ', it
      write ( *, '(a,g14.6)' ) '  Estimated error is ', rnrm2
      err = 0.0D+00
      do i = 1, n
        err = max ( err, abs ( x_exact(i) - x(i) ) )
      end do
      write ( *, '(a,g14.6)' ) '  Loo error is ', err

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '     I      X(I)         X_EXACT(I)        B(I)'
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) 
     &    i, x(i), x_exact(i), b(i)
      end do

      return
      end

