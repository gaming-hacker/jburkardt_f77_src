      program main

c*********************************************************************72
c
cc MAIN is the main program for OU_PRB.
c
c  Discussion:
c
c    OU_PRB tests the OU library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 January 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)') ' '
      write ( *, '(a)' ) 'OU_PRB:'
      write ( *, '(a)' ) '  FORTRAN77 version.'
      write ( *, '(a)' ) '  Test the OU library.'

      call ou_euler_test ( )
      call ou_euler_maruyama_test ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'OU_PRB:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      stop
      end
      subroutine ou_euler_test ( )

c*********************************************************************72
c
cc OU_EULER_TEST tests OU_EULER.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 January 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision mu
      integer n
      integer seed
      double precision sigma
      double precision theta
      double precision tmax
      double precision x0

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'OU_EULER_TEST:'
      write ( *, '(a)' ) 
     &  '  Estimate a solution to the Ornstein-Uhlenbeck equation with'
      write ( *, '(a)' ) 
     &  '  the Euler method for stochastic differential equations.'
      write ( *, '(a)' ) ''

      theta = 2.0D+00
      write ( *, '(a,g14.6)' ) '  Using decay rate THETA = ', theta
      mu = 1.0D+00
      write ( *, '(a,g14.6)' ) '  Using mean MU = ', mu
      sigma = 0.15D+00
      write ( *, '(a,g14.6)' ) '  Using variance SIGMA = ', sigma
      x0 = 2.0D+00
      write ( *, '(a,g14.6)' ) '  Using initial value X0 = ', x0
      tmax = 3.0D+00
      write ( *, '(a,g14.6)' ) '  Using final time TMAX = ', tmax
      n = 10000
      write ( *, '(a,i8)' ) '  Using number of timesteps N = ', n
      seed = 123456789
      write ( *, '(a,i12)' ) '  Using value of random SEED = ', seed

      call ou_euler ( theta, mu, sigma, x0, tmax, n, seed )

      return
      end
      subroutine ou_euler_maruyama_test ( )

c*********************************************************************72
c
cc OU_EULER_MARUYAMA_TEST tests OU_EULER_MARUYAMA.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 January 2013
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision mu
      integer n
      integer r
      integer seed
      double precision sigma
      double precision theta
      double precision tmax
      double precision x0

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'OU_EULER_MARUYAMA_TEST:'
      write ( *, '(a)' ) 
     &  '  Estimate a solution to the Ornstein-Uhlenbeck equation'
      write ( *, '(a)' ) 
     &  '  using the Euler-Maruyama method for stochastic '
      write ( *, '(a)' ) '  differential equations.'
      write ( *, '(a)' ) ''

      theta = 2.0D+00
      write ( *, '(a,g14.6)' ) '  Using decay rate THETA = ', theta
      mu = 1.0D+00
      write ( *, '(a,g14.6)' ) '  Using mean MU = ', mu
      sigma = 0.15D+00
      write ( *, '(a,g14.6)' ) '  Using variance SIGMA = ', sigma
      x0 = 2.0D+00
      write ( *, '(a,g14.6)' ) '  Using initial value X0 = ', x0
      tmax = 3.0D+00
      write ( *, '(a,g14.6)' ) '  Using final time TMAX = ', tmax
      n = 10000
      write ( *, '(a,i6)' ) '  Using number of large timesteps N = ', n
      r = 16
      write ( *, '(a,i6)' ) 
     &  '  Using small time steps per one large time step R = ', r
      seed = 123456789
      write ( *, '(a,i12)' ) '  Using value of random SEED = ', seed

      call ou_euler_maruyama ( theta, mu, sigma, x0, tmax, n, r, seed )

      return
      end

