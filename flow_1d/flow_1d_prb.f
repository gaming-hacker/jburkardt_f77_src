      program main

c*********************************************************************72
c
cc MAIN is the main program for FLOW_1D_PRB.
c
c  Discussion:
c
c    FLOW_1D_PRB calls some FLOW_1D tests.
c
      implicit none

      call timestamp ( )
      write ( *, * ) ' '
      write ( *, * ) 'FLOW_1D_PRB'
      write ( *, * ) '  Tests for the FLOW_1D routines.'

      call test01

      call test02

      call test03

      write ( *, * ) ' '
      write ( *, * ) 'FLOW_1D_PRB'
      write ( *, * ) '  Normal end of FLOW_1D tests.'
      write ( *, * ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01

c*********************************************************************72
c
cc TEST01 compares the exact and finite difference routines.
c
      implicit none

      integer n
      integer ntest

      parameter ( n = 10 )
      parameter ( ntest = 4 )

      real c(0:n)
      integer i
      integer itest
      real r
      real rhs(0:n)
      real resid(0:n)
      real rtest(ntest)
      real u(0:n)
      real ux(0:n)
      real uxx(0:n)
      real x(0:n)

      write ( *, * ) ' '
      write ( *, * ) 'TEST01'
      write ( *, * ) ' '

      rtest(1) = 0.0
      rtest(2) = 0.1
      rtest(3) = 1.0
      rtest(4) = 10.0

      do itest = 1, ntest

        r = rtest(itest)

        write ( *, * ) ' '
        write ( *, * ) '  R = ', r

        do i = 0, n
          x(i) = real ( i ) / real ( n )
        end do

        write ( *, * ) ' '
        write ( *, * ) 'Exact solution of continuous PDE:'
        write ( *, * ) ' '
        write ( *, * ) ' ____X____ ____U____ __Ux_____ ___Uxx___' //
     &    ' ___RHS___ __Resid__'
        write ( *, * ) ' '

        do i = 0, n
          call u_exact_1d ( x(i), u(i) )
          call ux_exact_1d ( x(i), ux(i) )
          call uxx_exact_1d ( x(i), uxx(i) )
        end do

        call bc_exact_1d ( x(0), r, rhs(0) )

        do i = 1, n-1
          call rhs_exact_1d ( x(i), r, rhs(i) )
        end do

        call bc_exact_1d ( x(n), r, rhs(n) )

        do i = 0, n
          call resid_exact_1d ( x(i), u(i), ux(i), uxx(i), r, resid(i) )
        end do

        do i = 0, n
          write ( *, '(6f10.4)' ) x(i), u(i), ux(i), uxx(i), rhs(i), 
     &      resid(i)
        end do
c
c  Linear finite difference approximation.
c
        call u_linear_1d ( n, x, c )
  
        call u_fd_1d ( n, x, c, u )
        call ux_fd_1d ( n, x, c, ux )
        call uxx_fd_1d ( n, x, c, uxx )

        call resid_fd_1d ( n, x, c, r, resid )

        write ( *, * ) ' '
        write ( *, * ) 'Linear finite difference approximation:'
        write ( *, * ) ' '
        write ( *, * ) ' '
        write ( *, * ) ' ____X____ ____U____ __Ux_____ ___Uxx___' //
     &  ' ___RHS___ __Resid__'
        write ( *, * ) ' '

        call bc_exact_1d ( x(0), r, rhs(0) )

        do i = 1, n-1
          call rhs_exact_1d ( x(i), r, rhs(i) )
        end do

        call bc_exact_1d ( x(n), r, rhs(n) )

        do i = 0, n
          write ( *, '(6f10.4)' ) x(i), u(i), ux(i), uxx(i), rhs(i), 
     &      resid(i)
        end do
c
c  Finite difference interpolant
c
        do i = 0, n
          call u_exact_1d ( x(i), c(i) )
        end do
  
        call u_fd_1d ( n, x, c, u )
        call ux_fd_1d ( n, x, c, ux )
        call uxx_fd_1d ( n, x, c, uxx )

        call resid_fd_1d ( n, x, c, r, resid )

        write ( *, * ) ' '
        write ( *, * ) 'Finite difference interpolant:'
        write ( *, * ) ' '
        write ( *, * ) ' '
        write ( *, * ) ' ____X____ ____U____ __Ux_____ ___Uxx___' //
     &  ' ___RHS___ __Resid__'
        write ( *, * ) ' '

        call bc_exact_1d ( x(0), r, rhs(0) )

        do i = 1, n-1
          call rhs_exact_1d ( x(i), r, rhs(i) )
        end do

        call bc_exact_1d ( x(n), r, rhs(n) )

        do i = 0, n
          write ( *, '(6f10.4)' ) x(i), u(i), ux(i), uxx(i), rhs(i), 
     &      resid(i)
        end do

      end do

      return
      end
      subroutine test02

c*********************************************************************72
c
cc TEST02 applies Newton's method to the 1D problem, with SGE jacobian.
c
      implicit none

      integer lda
      integer n
      integer ntest

      parameter ( n = 10 )
      parameter ( ntest = 4 )
      parameter ( lda = n )

      real a(0:lda,0:n)
      real c(0:n)
      integer i
      integer info
      integer ipivot(0:n)
      integer istep
      integer itest
      integer j
      integer job
      real r
      real rhs(0:n)
      real resid(0:n)
      real rtest(ntest)
      real u(0:n)
      real ux(0:n)
      real uxx(0:n)
      real x(0:n)

      write ( *, * ) ' '
      write ( *, * ) 'TEST02'
      write ( *, * ) ' '

      rtest(1) = 0.0
      rtest(2) = 0.1
      rtest(3) = 1.0
      rtest(4) = 10.0

      do itest = 1, ntest

        r = rtest(itest)

        write ( *, * ) ' '
        write ( *, * ) '  R = ', r

        do i = 0, n
          x(i) = real ( i ) / real ( n )
        end do
c
c  Finite difference approximation.
c
        call u_linear_1d ( n, x, c )
  
        do istep = 1, 3

          call u_fd_1d ( n, x, c, u )
          call ux_fd_1d ( n, x, c, ux )
          call uxx_fd_1d ( n, x, c, uxx )

          call resid_fd_1d ( n, x, c, r, resid )

          write ( *, * ) ' '
          write ( *, * ) 'Finite difference approximation:'
          write ( *, * ) ' '
          write ( *, * ) ' '
          write ( *, * ) ' ____X____ ____U____ __Ux_____ ___Uxx___' //
     &      ' ___RHS___ __Resid__'
          write ( *, * ) ' '

          call bc_exact_1d ( x(0), r, rhs(0) )

          do i = 1, n-1
            call rhs_exact_1d ( x(i), r, rhs(i) )
          end do

          call bc_exact_1d ( x(n), r, rhs(n) )

          do i = 0, n
            write ( *, '(6f10.4)' ) x(i), u(i), ux(i), uxx(i), rhs(i), 
     &        resid(i)
          end do
c
c  Compute the jacobian.
c
          call jac_sge_fd_1d ( n, lda, x, c, r, a )
c
c  Factor jacobian.
c
          call sge_fa ( a, lda+1, n+1, ipivot, info )

          if ( info .ne. 0 ) then
            write ( *, * ) ' '
            write ( *, * ) 'TEST02 - Fatal errorc'
            write ( *, * ) '  INFO = ', info 
            return
          end if
c
c  Solve system.
c
          job = 0
          call sge_sl ( a, lda+1, n+1, ipivot, resid, job )
c
c  Update solution.
c
          do i = 0, n
            c(i) = c(i) - resid(i)
          end do

        end do

      end do

      return
      end
      subroutine test03

c*********************************************************************72
c
cc TEST03 applies Newton's method to the 1D problem, with S3 jacobian.
c
      implicit none

      integer n
      integer ntest

      parameter ( n = 10 )
      parameter ( ntest = 4 )

      real a1(1:n)
      real a2(0:n)
      real a3(0:n-1)
      real c(0:n)
      integer i
      integer info
      integer ipivot(0:n)
      integer istep
      integer itest
      integer j
      integer job
      integer lda
      real r
      real rhs(0:n)
      real resid(0:n)
      real rtest(ntest)
      real u(0:n)
      real ux(0:n)
      real uxx(0:n)
      real x(0:n)

      write ( *, * ) ' '
      write ( *, * ) 'TEST03'
      write ( *, * ) ' '

      rtest(1) = 0.0
      rtest(2) = 0.1
      rtest(3) = 1.0
      rtest(4) = 10.0

      do itest = 1, ntest

        r = rtest(itest)

        write ( *, * ) ' '
        write ( *, * ) '  R = ', r

        do i = 0, n
          x(i) = real ( i ) / real ( n )
        end do
c
c  Finite difference approximation.
c
        call u_linear_1d ( n, x, c )
  
        do istep = 1, 3

          call u_fd_1d ( n, x, c, u )
          call ux_fd_1d ( n, x, c, ux )
          call uxx_fd_1d ( n, x, c, uxx )

          call resid_fd_1d ( n, x, c, r, resid )

          write ( *, * ) ' '
          write ( *, * ) 'Finite difference approximation:'
          write ( *, * ) ' '
          write ( *, * ) ' '
          write ( *, * ) ' ____X____ ____U____ __Ux_____ ___Uxx___' //
     &      ' ___RHS___ __Resid__'
          write ( *, * ) ' '

          call bc_exact_1d ( x(0), r, rhs(0) )

          do i = 1, n-1
            call rhs_exact_1d ( x(i), r, rhs(i) )
          end do

          call bc_exact_1d ( x(n), r, rhs(n) )

          do i = 0, n
            write ( *, '(6f10.4)' ) x(i), u(i), ux(i), uxx(i), rhs(i), 
     &        resid(i)
          end do
c
c  Compute the jacobian.
c
          call jac_s3_fd_1d ( n, x, c, r, a1, a2, a3 )
c
c  Factor jacobian.
c
          call s3_np_fa ( a1, a2, a3, n+1, info )

          if ( info .ne. 0 ) then
            write ( *, * ) ' '
            write ( *, * ) 'TEST03 - Fatal errorc'
            write ( *, * ) '  INFO = ', info 
            return
          end if
c
c  Solve system.
c
          job = 0
          call s3_np_sl ( a1, a2, a3, n+1, resid, job )
c
c  Update solution.
c
          do i = 0, n
            c(i) = c(i) - resid(i)
          end do

        end do

      end do

      return
      end
