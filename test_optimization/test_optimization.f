      subroutine p00_ab ( problem, m, a, b )

c*********************************************************************72
c
cc P00_AB evaluates the limits of the optimization region for any problem.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Marcin Molga, Czeslaw Smutnicki,
c    Test functions for optimization needs.
c
c  Parameters:
c
c    Input, integer PROBLEM, the problem number.
c
c    Input, integer M, the number of variables.
c
c    Output, double precision A(M), B(M), the lower and upper bounds.
c
      implicit none

      integer m

      double precision a(m)
      double precision b(m)
      integer problem

      if ( problem .eq. 1 ) then
        call p01_ab ( m, a, b )
      else if ( problem .eq. 2 ) then
        call p02_ab ( m, a, b )
      else if ( problem .eq. 3 ) then
        call p03_ab ( m, a, b )
      else if ( problem .eq. 4 ) then
        call p04_ab ( m, a, b )
      else if ( problem .eq. 5 ) then
        call p05_ab ( m, a, b )
      else if ( problem .eq. 6 ) then
        call p06_ab ( m, a, b )
      else if ( problem .eq. 7 ) then
        call p07_ab ( m, a, b )
      else if ( problem .eq. 8 ) then
        call p08_ab ( m, a, b )
      else if ( problem .eq. 9 ) then
        call p09_ab ( m, a, b )
      else if ( problem .eq. 10 ) then
        call p10_ab ( m, a, b )
      else if ( problem .eq. 11 ) then
        call p11_ab ( m, a, b )
      else if ( problem .eq. 12 ) then
        call p12_ab ( m, a, b )
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'P00_AB - Fatal error!'
        write ( *, '(a,i8)' ) 
     &    '  Illegal problem number PROBLEM = ', problem
        stop
      end if

      return
      end
      subroutine p00_compass_search ( problem, m, x0, delta_tol, 
     &  delta_init, k_max, x, fx, k )

c*********************************************************************72
c
cc P00_COMPASS_SEARCH carries out a direct search minimization algorithm.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Tamara Kolda, Robert Michael Lewis, Virginia Torczon,
c    Optimization by Direct Search: New Perspectives on Some Classical 
c    and Modern Methods,
c    SIAM Review,
c    Volume 45, Number 3, 2003, pages 385-482. 
c
c  Parameters:
c
c    Input, integer PROBLEM, the problem number.
c
c    Input, integer M, the number of variables.
c
c    Input, double precision X0(M), a starting estimate for the minimizer.
c
c    Input, double precision DELTA_TOL, the smallest step size that is allowed.
c
c    Input, double precision DELTA_INIT, the starting stepsize.  
c
c    Input, integer K_MAX, the maximum number of steps allowed.
c
c    Output, double precision X(M), the estimated minimizer.
c
c    Output, double precision FX, the function value at X.
c
c    Output, integer K, the number of steps taken.
c
      implicit none

      integer m
      integer n
      parameter ( n = 1 )

      logical decrease
      double precision delta
      double precision delta_init
      double precision delta_tol
      double precision fx
      double precision fxd
      integer i
      integer ii
      integer k
      integer k_max
      integer problem
      double precision s
      double precision x(m)
      double precision x0(m)
      double precision xd(m)

      k = 0
      do i = 1, m
        x(i) = x0(i)
      end do
      call p00_f ( problem, m, n, x, fx )

      if ( delta_tol .le. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'P00_COMPASS_SEARCH - Fatal error!'
        write ( *, '(a)' ) '  DELTA_TOL <= 0.0.'
        write ( *, '(a,g14.6)' ) '  DELTA_TOL = ', delta_tol
        stop
      end if

      if ( delta_init .le. delta_tol ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'P00_COMPASS_SEARCH - Fatal error!'
        write ( *, '(a)' ) '  DELTA_INIT < DELTA_TOL.'
        write ( *, '(a,g14.6)' ) '  DELTA_INIT = ', delta_init
        write ( *, '(a,g14.6)' ) '  DELTA_TOL = ', delta_tol
        stop
      end if

      delta = delta_init

10    continue

      if ( k .lt. k_max ) then

        k = k + 1
c
c  For each coordinate direction I, seek a lower function value
c  by increasing or decreasing X(I) by DELTA.
c
        decrease = .false.
        s = + 1.0D+00
        i = 1

        do ii = 1, 2 * m

          xd = x
          xd(i) = xd(i) + s * delta
          call p00_f ( problem, m, n, xd, fxd )
c
c  As soon as a decrease is noticed, accept the new point.
c
          if ( fxd .lt. fx ) then
            x = xd
            fx = fxd
            decrease = .true.
            go to 20
          end if

          s = - s
          if ( s .eq. + 1.0D+00 ) then
            i = i + 1
          end if

        end do

20      continue
c
c  If no decrease occurred, reduce DELTA.
c
        if ( .not. decrease ) then
          delta = delta / 2.0D+00
          if ( delta .lt. delta_tol ) then
            go to 30
          end if
        end if

        go to 10

      end if

30    continue

      return
      end
      subroutine p00_f ( problem, m, n, x, f )

c*********************************************************************72
c
cc P00_F evaluates the objective function for any problem.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Marcin Molga, Czeslaw Smutnicki,
c    Test functions for optimization needs.
c
c  Parameters:
c
c    Input, integer PROBLEM, the problem number.
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer N, the number of arguments.
c
c    Input, double precision X(M,N), the argument of the objective function.
c
c    Output, double precision F(N), the objective function evaluated at
c    each argument.
c
      implicit none

      integer m
      integer n

      double precision f(n)
      integer problem
      double precision x(m,n)

      if ( problem .eq. 1 ) then
        call p01_f ( m, n, x, f )
      else if ( problem .eq. 2 ) then
        call p02_f ( m, n, x, f )
      else if ( problem .eq. 3 ) then
        call p03_f ( m, n, x, f )
      else if ( problem .eq. 4 ) then
        call p04_f ( m, n, x, f )
      else if ( problem .eq. 5 ) then
        call p05_f ( m, n, x, f )
      else if ( problem .eq. 6 ) then
        call p06_f ( m, n, x, f )
      else if ( problem .eq. 7 ) then
        call p07_f ( m, n, x, f )
      else if ( problem .eq. 8 ) then
        call p08_f ( m, n, x, f )
      else if ( problem .eq. 9 ) then
        call p09_f ( m, n, x, f )
      else if ( problem .eq. 10 ) then
        call p10_f ( m, n, x, f )
      else if ( problem .eq. 11 ) then
        call p11_f ( m, n, x, f )
      else if ( problem .eq. 12 ) then
        call p12_f ( m, n, x, f )
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'P00_F - Fatal error!'
        write ( *, '(a,i8)' ) 
     &    '  Illegal problem number PROBLEM = ', problem
        stop
      end if

      return
      end
      subroutine p00_problem_num ( problem_num )

c*********************************************************************72
c
cc P00_PROBLEM_NUM returns the number of problems available.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c   Output, integer PROBLEM_NUM, the number of problems available.
c
      implicit none

      integer problem_num

      problem_num = 12

      return
      end
      subroutine p00_sol ( problem, m, know, x )

c*********************************************************************72
c
cc P00_SOL returns the solution for any problem.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer PROBLEM, the problem number.
c
c    Input, integer M, the spatial dimension.
c
c    Input/output, integer KNOW.
c    On input, KNOW is 0, or the index of the previously returned solution.
c    On output, KNOW is 0 if there are no more solutions, or it is the
c    index of the next solution.
c
c    Output, double precision X(M), the solution, if known.
c
      implicit none

      integer m

      integer know
      integer problem
      double precision x(m)

      if ( problem .eq. 1 ) then
        call p01_sol ( m, know, x )
      else if ( problem .eq. 2 ) then
        call p02_sol ( m, know, x )
      else if ( problem .eq. 3 ) then
        call p03_sol ( m, know, x )
      else if ( problem .eq. 4 ) then
        call p04_sol ( m, know, x )
      else if ( problem .eq. 5 ) then
        call p05_sol ( m, know, x )
      else if ( problem .eq. 6 ) then
        call p06_sol ( m, know, x )
      else if ( problem .eq. 7 ) then
        call p07_sol ( m, know, x )
      else if ( problem .eq. 8 ) then
        call p08_sol ( m, know, x )
      else if ( problem .eq. 9 ) then
        call p09_sol ( m, know, x )
      else if ( problem .eq. 10 ) then
        call p10_sol ( m, know, x )
      else if ( problem .eq. 11 ) then
        call p11_sol ( m, know, x )
      else if ( problem .eq. 12 ) then
        call p12_sol ( m, know, x )
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'P00_SOL - Fatal error!'
        write ( *, '(a,i8)' ) 
     &    '  Illegal value of PROBLEM = ', problem
        stop
      end if

      return
      end
      subroutine p00_title ( problem, title )

c*********************************************************************72
c
cc P00_TITLE returns a title for any problem.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer PROBLEM, the number of the problem.
c
c    Output, character ( len = * ) TITLE, a title for the problem.
c
      implicit none

      integer problem
      character ( len = * ) title

      if ( problem .eq. 1 ) then
        call p01_title ( title )
      else if ( problem .eq. 2 ) then
        call p02_title ( title )
      else if ( problem .eq. 3 ) then
        call p03_title ( title )
      else if ( problem .eq. 4 ) then
        call p04_title ( title )
      else if ( problem .eq. 5 ) then
        call p05_title ( title )
      else if ( problem .eq. 6 ) then
        call p06_title ( title )
      else if ( problem .eq. 7 ) then
        call p07_title ( title )
      else if ( problem .eq. 8 ) then
        call p08_title ( title )
      else if ( problem .eq. 9 ) then
        call p09_title ( title )
      else if ( problem .eq. 10 ) then
        call p10_title ( title )
      else if ( problem .eq. 11 ) then
        call p11_title ( title )
      else if ( problem .eq. 12 ) then
        call p12_title ( title )
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'P00_TITLE - Fatal error!'
        write ( *, '(a,i8)' ) 
     &    '  Illegal value of PROBLEM = ', problem
        stop
      end if

      return
      end
      subroutine p01_ab ( m, a, b )

c*********************************************************************72
c
cc P01_AB evaluates the limits of the optimization region for problem 01.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Output, double precision A(M), B(M), the lower and upper bounds.
c
      implicit none

      integer m

      double precision a(m)
      double precision b(m)
      integer i

      do i = 1, m
        a(i) = - 5.0D+00
        b(i) = + 5.0D+00
      end do

      return
      end
      subroutine p01_f ( m, n, x, f )

c*********************************************************************72
c
cc P01_F evaluates the objective function for problem 01.
c
c  Discussion:
c
c    The function is continuous, convex, and unimodal.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Hugues Bersini, Marco Dorigo, Stefan Langerman, Gregory Seront, 
c    Luca Gambardella,
c    Results of the first international contest on evolutionary optimisation,
c    In Proceedings of 1996 IEEE International Conference on Evolutionary 
c    Computation,
c    IEEE Press, pages 611-615, 1996.
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer N, the number of arguments.
c
c    Input, double precision X(M,N), the arguments.
c
c    Output, double precision F(N), the function evaluated at the arguments.
c
      implicit none

      integer m
      integer n

      double precision f(n)
      integer i
      integer j
      double precision x(m,n)

      do j = 1, n
        f(j) = 0.0D+00
        do i = 1, m
          f(j) = f(j) + ( x(i,j) - 1.0D+00 ) ** 2
        end do
      end do

      return
      end
      subroutine p01_sol ( m, know, x )

c*********************************************************************72
c
cc P01_SOL returns the solution for problem 01.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input/output, integer KNOW.
c    On input, KNOW is 0, or the index of the previously returned solution.
c    On output, KNOW is 0 if there are no more solutions, or it is the
c    index of the next solution.
c
c    Output, double precision X(M), the solution, if known.
c
      implicit none

      integer m

      integer know
      integer i
      double precision x(m)

      if ( know .eq. 0 ) then
        know = 1
        do i = 1, m
          x(i) = 1.0D+00
        end do
      else
        know = 0
      end if

      return
      end
      subroutine p01_title ( title )

c*********************************************************************72
c
cc P01_TITLE returns a title for problem 01.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, character ( len = * ) TITLE, a title for the problem.
c
      implicit none

      character ( len = * ) title

      title = 'The sphere model.'

      return
      end
      subroutine p02_ab ( m, a, b )

c*********************************************************************72
c
cc P02_AB evaluates the limits of the optimization region for problem 02.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Marcin Molga, Czeslaw Smutnicki,
c    Test functions for optimization needs.
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Output, double precision A(M), B(M), the lower and upper bounds.
c
      implicit none

      integer m

      double precision a(m)
      double precision b(m)
      integer i

      do i = 1, m
        a(i) = - 5.12D+00
        b(i) = + 5.12D+00
      end do

      return
      end
      subroutine p02_f ( m, n, x, f )

c*********************************************************************72
c
cc P02_F evaluates the objective function for problem 02.
c
c  Discussion:
c
c    This function is also known as the weighted sphere model.
c
c    The function is continuous, convex, and unimodal.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Marcin Molga, Czeslaw Smutnicki,
c    Test functions for optimization needs.
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer N, the number of arguments.
c
c    Input, double precision X(M,N), the arguments.
c
c    Output, double precision F(N), the function evaluated at the arguments.
c
      implicit none

      integer m
      integer n

      double precision f(n)
      integer i
      integer j
      double precision x(m,m)
      double precision y(m)

      call r8vec_indicator ( m, y )

      do j = 1, n
        f(j) = 0.0
        do i = 1, m
          f(j) = f(j) + y(i) * x(i,j) ** 2
        end do
      end do

      return
      end
      subroutine p02_sol ( m, know, x )

c*********************************************************************72
c
cc P02_SOL returns the solution for problem 02.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input/output, integer KNOW.
c    On input, KNOW is 0, or the index of the previously returned solution.
c    On output, KNOW is 0 if there are no more solutions, or it is the
c    index of the next solution.
c
c    Output, double precision X(M), the solution, if known.
c
      implicit none

      integer m

      integer i
      integer know
      double precision x(m)

      if ( know .eq. 0 ) then
        know = 1
        do i = 1, m
          x(i) = 0.0D+00
        end do
      else
        know = 0
      end if

      return
      end
      subroutine p02_title ( title )

c*********************************************************************72
c
cc P02_TITLE returns a title for problem 02.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, character ( len = * ) TITLE, a title for the problem.
c
      implicit none

      character ( len = * ) title

      title = 'The axis-parallel hyper-ellipsoid function.'

      return
      end
      subroutine p03_ab ( m, a, b )

c*********************************************************************72
c
cc P03_AB evaluates the limits of the optimization region for problem 03.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Marcin Molga, Czeslaw Smutnicki,
c    Test functions for optimization needs.
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Output, double precision A(M), B(M), the lower and upper bounds.
c
      implicit none

      integer m

      double precision a(m)
      double precision b(m)
      integer i

      do i = 1, m
        a(i) = - 65.536D+00
        b(i) = + 65.536D+00
      end do

      return
      end
      subroutine p03_f ( m, n, x, f )

c*********************************************************************72
c
cc P03_F evaluates the objective function for problem 03.
c
c  Discussion:
c
c    This function is also known as the weighted sphere model.
c
c    The function is continuous, convex, and unimodal.
c
c     There is a typographical error in Molga and Smutnicki, so that the
c     formula for this function is given incorrectly.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Marcin Molga, Czeslaw Smutnicki,
c    Test functions for optimization needs.
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer N, the number of arguments.
c
c    Input, double precision X(M,N), the arguments.
c
c    Output, double precision F(N), the function evaluated at the arguments.
c
      implicit none

      integer m
      integer n

      double precision f(n)
      integer i
      integer j
      double precision x(m,n)
      double precision x_sum

      do j = 1, n

        f(j) = 0.0D+00
        x_sum = 0.0D+00

        do i = 1, m
          x_sum = x_sum + x(i,j)
          f(j) = f(j) + x_sum**2
        end do

      end do

      return
      end
      subroutine p03_sol ( m, know, x )

c*********************************************************************72
c
cc P03_SOL returns the solution for problem 03.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input/output, integer KNOW.
c    On input, KNOW is 0, or the index of the previously returned solution.
c    On output, KNOW is 0 if there are no more solutions, or it is the
c    index of the next solution.
c
c    Output, double precision X(M), the solution, if known.
c
      implicit none

      integer m

      integer i
      integer know
      double precision x(m)

      if ( know .eq. 0 ) then
        know = 1
        do i = 1, m
          x(i) = 0.0D+00
        end do
      else
        know = 0
      end if

      return
      end
      subroutine p03_title ( title )

c*********************************************************************72
c
cc P03_TITLE returns a title for problem 03.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, character ( len = * ) TITLE, a title for the problem.
c
      implicit none

      character ( len = * ) title

      title = 'The rotated hyper-ellipsoid function.'

      return
      end
      subroutine p04_ab ( m, a, b )

c*********************************************************************72
c
cc P04_AB evaluates the limits of the optimization region for problem 04.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Marcin Molga, Czeslaw Smutnicki,
c    Test functions for optimization needs.
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Output, double precision A(M), B(M), the lower and upper bounds.
c
      implicit none

      integer m

      double precision a(m)
      double precision b(m)
      integer i

      do i = 1, m
        a(i) = - 2.048D+00
        b(i) = + 2.048D+00
      end do

      return
      end
      subroutine p04_f ( m, n, x, f )

c*********************************************************************72
c
cc P04_F evaluates the objective function for problem 04.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Howard Rosenbrock,
c    An Automatic Method for Finding the Greatest or Least Value of a Function,
c    Computer Journal,
c    Volume 3, 1960, pages 175-184.
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer N, the number of arguments.
c
c    Input, double precision X(M,N), the arguments.
c
c    Output, double precision F(N), the function evaluated at the arguments.
c
      implicit none

      integer m
      integer n

      double precision f(n)
      integer i
      integer j
      double precision x(m,n)

      do j = 1, n
        f(j) = 0.0D+00
        do i = 1, m
          f(j) = f(j) + ( 1.0D+00 - x(i,j) )**2
        end do
        do i = 2, m
          f(j) = f(j) + ( x(i,j) - x(i-1,j) )**2
        end do
      end do

      return
      end
      subroutine p04_sol ( m, know, x )

c*********************************************************************72
c
cc P04_SOL returns the solution for problem 04.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input/output, integer KNOW.
c    On input, KNOW is 0, or the index of the previously returned solution.
c    On output, KNOW is 0 if there are no more solutions, or it is the
c    index of the next solution.
c
c    Output, double precision X(M), the solution, if known.
c
      implicit none

      integer m

      integer i
      integer know
      double precision x(m)

      if ( know .eq. 0 ) then
        know = 1
        do i = 1, m
          x(i) = 1.0D+00
        end do
      else
        know = 0
      end if

      return
      end
      subroutine p04_title ( title )

c*********************************************************************72
c
cc P04_TITLE returns a title for problem 04.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, character ( len = * ) TITLE, a title for the problem.
c
      implicit none

      character ( len = * ) title

      title = 'Rosenbrock''s valley.'

      return
      end
      subroutine p05_ab ( m, a, b )

c*********************************************************************72
c
cc P05_AB evaluates the limits of the optimization region for problem 05.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Marcin Molga, Czeslaw Smutnicki,
c    Test functions for optimization needs.
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Output, double precision A(M), B(M), the lower and upper bounds.
c
      implicit none

      integer m

      double precision a(m)
      double precision b(m)
      integer i

      do i = 1, m
        a(i) = - 5.12D+00
        b(i) = + 5.12D+00
      end do

      return
      end
      subroutine p05_f ( m, n, x, f )

c*********************************************************************72
c
cc P05_F evaluates the objective function for problem 05.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Marcin Molga, Czeslaw Smutnicki,
c    Test functions for optimization needs.
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer N, the number of arguments.
c
c    Input, double precision X(M,N), the arguments.
c
c    Output, double precision F(N), the function evaluated at the arguments.
c
      implicit none

      integer m
      integer n

      double precision f(n)
      integer i
      integer j
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision x(m,n)

      do j = 1, n

        f(j) = dble ( 10 * m )

        do i = 1, m
          f(j) = f(j) + x(i,j) ** 2 
     &      - 10.0D+00 * cos ( 2.0D+00 * pi * x(i,j) )
        end do

      end do

      return
      end
      subroutine p05_sol ( m, know, x )

c*********************************************************************72
c
cc P05_SOL returns the solution for problem 05.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input/output, integer KNOW.
c    On input, KNOW is 0, or the index of the previously returned solution.
c    On output, KNOW is 0 if there are no more solutions, or it is the
c    index of the next solution.
c
c    Output, double precision X(M), the solution, if known.
c
      implicit none

      integer m

      integer i
      integer know
      double precision x(m)

      if ( know .eq. 0 ) then
        know = 1
        do i = 1, m
          x(i) = 0.0D+00
        end do
      else
        know = 0
      end if

      return
      end
      subroutine p05_title ( title )

c*********************************************************************72
c
cc P05_TITLE returns a title for problem 05.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, character ( len = * ) TITLE, a title for the problem.
c
      implicit none

      character ( len = * ) title

      title = 'Rastrigin''s function.'

      return
      end
      subroutine p06_ab ( m, a, b )

c*********************************************************************72
c
cc P06_AB evaluates the limits of the optimization region for problem 06.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Marcin Molga, Czeslaw Smutnicki,
c    Test functions for optimization needs.
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Output, double precision A(M), B(M), the lower and upper bounds.
c
      implicit none

      integer m

      double precision a(m)
      double precision b(m)
      integer i

      do i = 1, m
        a(i) = - 500.0D+00
        b(i) = + 500.0D+00
      end do

      return
      end
      subroutine p06_f ( m, n, x, f )

c*********************************************************************72
c
cc P06_F evaluates the objective function for problem 06.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Hans-Paul Schwefel,
c    Numerical optimization of computer models,
c    Wiley, 1981,
c    ISBN13: 978-0471099888,
c    LC: QA402.5.S3813.
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer N, the number of arguments.
c
c    Input, double precision X(M,N), the arguments.
c
c    Output, double precision F(N), the function evaluated at the arguments.
c
      implicit none

      integer m
      integer n

      double precision f(n)
      integer i
      integer j
      double precision x(m,n)

      do j = 1, n
        f(j) = 0.0D+00
        do i = 1, m
          f(j) = f(j) - x(i,j) * sin ( sqrt ( abs ( x(i,j) ) ) )
        end do
      end do

      return
      end
      subroutine p06_sol ( m, know, x )

c*********************************************************************72
c
cc P06_SOL returns the solution for problem 06.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input/output, integer KNOW.
c    On input, KNOW is 0, or the index of the previously returned solution.
c    On output, KNOW is 0 if there are no more solutions, or it is the
c    index of the next solution.
c
c    Output, double precision X(M), the solution, if known.
c
      implicit none

      integer m

      integer i
      integer know
      double precision x(m)

      if ( know .eq. 0 ) then
        know = 1
        do i = 1, m
          x(i) = 420.9687D+00
        end do
      else
        know = 0
      end if

      return
      end
      subroutine p06_title ( title )

c*********************************************************************72
c
cc P06_TITLE returns a title for problem 06.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, character ( len = * ) TITLE, a title for the problem.
c
      implicit none

      character ( len = * ) title

      title = 'Schwefel''s function.'

      return
      end
      subroutine p07_ab ( m, a, b )

c*********************************************************************72
c
cc P07_AB evaluates the limits of the optimization region for problem 07.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Marcin Molga, Czeslaw Smutnicki,
c    Test functions for optimization needs.
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Output, double precision A(M), B(M), the lower and upper bounds.
c
      implicit none

      integer m

      double precision a(m)
      double precision b(m)
      integer i

      do i = 1, m
        a(i) = - 600.0D+00
        b(i) = + 600.0D+00
      end do

      return
      end
      subroutine p07_f ( m, n, x, f )

c*********************************************************************72
c
cc P07_F evaluates the objective function for problem 07.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Marcin Molga, Czeslaw Smutnicki,
c    Test functions for optimization needs.
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer N, the number of arguments.
c
c    Input, double precision X(M,N), the arguments.
c
c    Output, double precision F(N), the function evaluated at the arguments.
c
      implicit none

      integer m
      integer n

      double precision f(n)
      integer i
      integer j
      double precision p
      double precision x(m,n)
      double precision y(m)

      call r8vec_indicator ( m, y )

      do i = 1, m
        y(i) = sqrt ( y(i) )
      end do

      do j = 1, n
        f(j) = 1.0D+00
        do i = 1, m
          f(j) = f(j) + x(i,j) ** 2 / 4000.0D+00
        end do
        p = 1.0D+00
        do i = 1, m
          p = p * cos ( x(i,j) / y(i) )
        end do
        f(j) = f(j) - p
      end do

      return
      end
      subroutine p07_sol ( m, know, x )

c*********************************************************************72
c
cc P07_SOL returns the solution for problem 07.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input/output, integer KNOW.
c    On input, KNOW is 0, or the index of the previously returned solution.
c    On output, KNOW is 0 if there are no more solutions, or it is the
c    index of the next solution.
c
c    Output, double precision X(M), the solution, if known.
c
      implicit none

      integer m

      integer i
      integer know
      double precision x(m)

      if ( know .eq. 0 ) then
        know = 1
        do i = 1, m
          x(i) = 0.0D+00
        end do
      else
        know = 0
      end if

      return
      end
      subroutine p07_title ( title )

c*********************************************************************72
c
cc P07_TITLE returns a title for problem 07.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, character ( len = * ) TITLE, a title for the problem.
c
      implicit none

      character ( len = * ) title

      title = 'Griewank''s function.'

      return
      end
      subroutine p08_ab ( m, a, b )

c*********************************************************************72
c
cc P08_AB evaluates the limits of the optimization region for problem 08.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Marcin Molga, Czeslaw Smutnicki,
c    Test functions for optimization needs.
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Output, double precision A(M), B(M), the lower and upper bounds.
c
      implicit none

      integer m

      double precision a(m)
      double precision b(m)
      integer i

      do i = 1, m
        a(i) = - 1.0D+00
        b(i) = + 1.0D+00
      end do

      return
      end
      subroutine p08_f ( m, n, x, f )

c*********************************************************************72
c
cc P08_F evaluates the objective function for problem 08.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Marcin Molga, Czeslaw Smutnicki,
c    Test functions for optimization needs.
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer N, the number of arguments.
c
c    Input, double precision X(M,N), the arguments.
c
c    Output, double precision F(N), the function evaluated at the arguments.
c
      implicit none

      integer m
      integer n

      double precision f(n)
      integer i
      integer j
      double precision x(m,n)
      double precision y(m)

      call r8vec_indicator ( m, y )

      do i = 1, m
        y(i) = y(i) + 1.0D+00
      end do

      do j = 1, n
        f(j) = 0.0D+00
        do i = 1, m
          f(j) = abs ( x(i,j) ) ** y(i)
        end do
      end do

      return
      end
      subroutine p08_sol ( m, know, x )

c*********************************************************************72
c
cc P08_SOL returns the solution for problem 08.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input/output, integer KNOW.
c    On input, KNOW is 0, or the index of the previously returned solution.
c    On output, KNOW is 0 if there are no more solutions, or it is the
c    index of the next solution.
c
c    Output, double precision X(M), the solution, if known.
c
      implicit none

      integer m

      integer i
      integer know
      double precision x(m)

      if ( know .eq. 0 ) then
        know = 1
        do i = 1, m
          x(i) = 0.0D+00
        end do
      else
        know = 0
      end if

      return
      end
      subroutine p08_title ( title )

c*********************************************************************72
c
cc P08_TITLE returns a title for problem 08.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, character ( len = * ) TITLE, a title for the problem.
c
      implicit none

      character ( len = * ) title

      title = 'The power sum function.'

      return
      end
      subroutine p09_ab ( m, a, b )

c*********************************************************************72
c
cc P09_AB evaluates the limits of the optimization region for problem 09.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Marcin Molga, Czeslaw Smutnicki,
c    Test functions for optimization needs.
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Output, double precision A(M), B(M), the lower and upper bounds.
c
      implicit none

      integer m

      double precision a(m)
      double precision b(m)
      integer i

      do i = 1, m
        a(i) = - 32.768D+00
        b(i) = + 32.768D+00
      end do

      return
      end
      subroutine p09_f ( m, n, x, f )

c*********************************************************************72
c
cc P09_F evaluates the objective function for problem 09.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Marcin Molga, Czeslaw Smutnicki,
c    Test functions for optimization needs.
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer N, the number of arguments.
c
c    Input, double precision X(M,N), the arguments.
c
c    Output, double precision F(N), the function evaluated at the arguments.
c
      implicit none

      integer m
      integer n

      double precision a
      parameter ( a = 20.0D+00 )
      double precision b
      parameter ( b = 0.2D+00 )
      double precision c
      parameter ( c = 0.2D+00 )
      double precision f(n)
      integer i
      integer j
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision s1
      double precision s2
      double precision x(m,n)

      do j = 1, n
        s1 = 0.0D+00
        s2 = 0.0D+00
        do i = 1, m
          s1 = s1 + x(i,j)**2
          s2 = s2 + cos ( c * pi * x(i,j) )
        end do
        f(j) = - a * exp ( - b * sqrt ( s1 / dble ( m ) ) ) 
     &    - exp ( s2 / dble ( m ) ) + a + exp ( 1.0D+00 )
      end do

      return
      end
      subroutine p09_sol ( m, know, x )

c*********************************************************************72
c
cc P09_SOL returns the solution for problem 09.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input/output, integer KNOW.
c    On input, KNOW is 0, or the index of the previously returned solution.
c    On output, KNOW is 0 if there are no more solutions, or it is the
c    index of the next solution.
c
c    Output, double precision X(M), the solution, if known.
c
      implicit none

      integer m

      integer i
      integer know
      double precision x(m)

      if ( know .eq. 0 ) then
        know = 1
        do i = 1, m
          x(i) = 0.0D+00
        end do
      else
        know = 0
      end if

      return
      end
      subroutine p09_title ( title )

c*********************************************************************72
c
cc P09_TITLE returns a title for problem 09.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, character ( len = * ) TITLE, a title for the problem.
c
      implicit none

      character ( len = * ) title

      title = 'Ackley''s function.'

      return
      end
      subroutine p10_ab ( m, a, b )

c*********************************************************************72
c
cc P10_AB evaluates the limits of the optimization region for problem 10.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Marcin Molga, Czeslaw Smutnicki,
c    Test functions for optimization needs.
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Output, double precision A(M), B(M), the lower and upper bounds.
c
      implicit none

      integer m

      double precision a(m)
      double precision b(m)
      integer i
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )

      do i = 1, m
        a(i) = 0.0D+00
        b(i) = pi
      end do

      return
      end
      subroutine p10_f ( m, n, x, f )

c*********************************************************************72
c
cc P10_F evaluates the objective function for problem 10.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Marcin Molga, Czeslaw Smutnicki,
c    Test functions for optimization needs.
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer N, the number of arguments.
c
c    Input, double precision X(M,N), the arguments.
c
c    Output, double precision F(N), the function evaluated at the arguments.
c
      implicit none

      integer m
      integer n

      double precision f(n)
      integer i
      integer j
      integer p 
      parameter ( p = 10 )
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision x(m,n)
      double precision y(m)

      call r8vec_indicator ( m, y )

      do j = 1, n
        f(j) = 0.0D+00
        do i = 1, m
          f(j) = f(j) - sin ( x(i,j) ) 
     &      * ( sin ( x(i,j)**2 * y(i) / pi ) ) ** ( 2 * p ) 
        end do
      end do

      return
      end
      subroutine p10_sol ( m, know, x )

c*********************************************************************72
c
cc P10_SOL returns the solution for problem 10.
c
c  Discussion:
c
c    The minimum value is - 0.966 * M.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input/output, integer KNOW.
c    On input, KNOW is 0, or the index of the previously returned solution.
c    On output, KNOW is 0 if there are no more solutions, or it is the
c    index of the next solution.
c
c    Output, double precision X(M), the solution, if known.
c
      implicit none

      integer m

      integer i
      integer know
      double precision x(m)

      know = 0
      do i = 1, m
        x(i) = 0.0D+00
      end do
        
      return
      end
      subroutine p10_title ( title )

c*********************************************************************72
c
cc P10_TITLE returns a title for problem 10.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, character ( len = * ) TITLE, a title for the problem.
c
      implicit none

      character ( len = * ) title

      title = 'Michalewicz''s function.'

      return
      end
      subroutine p11_ab ( m, a, b )

c*********************************************************************72
c
cc P11_AB evaluates the limits of the optimization region for problem 11.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Marcin Molga, Czeslaw Smutnicki,
c    Test functions for optimization needs.
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Output, double precision A(M), B(M), the lower and upper bounds.
c
      implicit none

      integer m

      double precision a(m)
      double precision b(m)
      integer i

      do i = 1, m
        a(i) = - 5.12D+00
        b(i) = + 5.12D+00
      end do

      return
      end
      subroutine p11_f ( m, n, x, f )

c*********************************************************************72
c
cc P11_F evaluates the objective function for problem 11.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Marcin Molga, Czeslaw Smutnicki,
c    Test functions for optimization needs.
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer N, the number of arguments.
c
c    Input, double precision X(M,N), the arguments.
c
c    Output, double precision F(N), the function evaluated at the arguments.
c
      implicit none

      integer m
      integer n

      double precision f(n)
      integer i
      integer j
      double precision rsq
      double precision x(m,n)

      do j = 1, n

        rsq = 0.0D+00
        do i = 1, m
          rsq = rsq + x(i,j)**2
        end do

        f(j) = - ( 1.0D+00 + cos ( 12.0D+00 * sqrt ( rsq ) ) ) 
     &    / ( 0.5D+00 * rsq + 2.0D+00 )

      end do

      return
      end
      subroutine p11_sol ( m, know, x )

c*********************************************************************72
c
cc P11_SOL returns the solution for problem 11.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input/output, integer KNOW.
c    On input, KNOW is 0, or the index of the previously returned solution.
c    On output, KNOW is 0 if there are no more solutions, or it is the
c    index of the next solution.
c
c    Output, double precision X(M), the solution, if known.
c
      implicit none

      integer m

      integer i
      integer know
      double precision x(m)

      if ( know .eq. 0 ) then
        do i = 1, m
          x(i) = 0.0D+00
        end do
        know = 1
      else
        know = 0
      end if

      return
      end
      subroutine p11_title ( title )

c*********************************************************************72
c
cc P11_TITLE returns a title for problem 11.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, character ( len = * ) TITLE, a title for the problem.
c
      implicit none

      character ( len = * ) title

      title = 'Drop wave function.'

      return
      end
      subroutine p12_ab ( m, a, b )

c*********************************************************************72
c
cc P12_AB evaluates the limits of the optimization region for problem 12.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Marcin Molga, Czeslaw Smutnicki,
c    Test functions for optimization needs.
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Output, double precision A(M), B(M), the lower and upper bounds.
c
      implicit none

      integer m

      double precision a(m)
      double precision b(m)
      integer i

      do i = 1, m
        a(i) = 0.0D+00
        b(i) = 1.0D+00
      end do

      return
      end
      subroutine p12_f ( m, n, x, f )

c*********************************************************************72
c
cc P12_F evaluates the objective function for problem 12.
c
c  Discussion:
c
c    In dimension I, the function is a piecewise linear function with
c    local minima at 0 and 1.0, and a global minimum at ALPHA(I) = I/(M+1).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Marcin Molga, Czeslaw Smutnicki,
c    Test functions for optimization needs.
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer N, the number of arguments.
c
c    Input, double precision X(M,N), the arguments.
c
c    Output, double precision F(N), the function evaluated at the arguments.
c
      implicit none

      integer m
      integer n

      double precision alpha(m)
      double precision beta
      parameter ( beta = 2.0D+00 )
      double precision f(n)
      double precision g
      integer i
      integer j
      double precision x(m,n)
c
c  I'm just choosing ALPHA in [0,1] arbitrarily.
c
      do i = 1, m
        alpha(i) = dble ( i ) / dble ( m + 1 )
      end do

      do j = 1, n

        f(j) = 0.0D+00

        do i = 1, m

          if ( x(i,j) .le. 0.0D+00 ) then
            g = x(i,j)
          else if ( x(i,j) .le. 0.8D+00 * alpha(i) ) then
            g = 0.8D+00 - x(i,j) / alpha(i)
          else if ( x(i,j) .le. alpha(i) ) then
            g = 5.0D+00 * x(i,j) / alpha(i) - 4.0D+00
          else if ( x(i,j) .le. 
     &      ( 1.0D+00 + 4.0D+00 * alpha(i) ) / 5.0D+00 ) then
            g = 1.0D+00 + 5.0D+00 * ( x(i,j) - alpha(i) ) 
     &        / ( alpha(i) - 1.0D+00 )
          else if ( x(i,j) .le. 1.0D+00 ) then
            g = 0.8D+00 + ( x(i,j) - 1.0D+00 ) / ( 1.0D+00 - alpha(i) )
          else
            g = x(i,j) - 1.0D+00
          end if

          f(j) = f(j) + g

        end do

        f(j) = f(j) / dble ( m )
        f(j) = - ( f(j) ** beta )

      end do

      return
      end
      subroutine p12_sol ( m, know, x )

c*********************************************************************72
c
cc P12_SOL returns the solution for problem 12.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input/output, integer KNOW.
c    On input, KNOW is 0, or the index of the previously returned solution.
c    On output, KNOW is 0 if there are no more solutions, or it is the
c    index of the next solution.
c
c    Output, double precision X(M), the solution, if known.
c
      implicit none

      integer m

      double precision alpha(m)
      integer i
      integer know
      double precision x(m)

      do i = 1, m
        alpha(i) = dble ( i ) / dble ( m + 1 )
      end do

      if ( know .eq. 0 ) then
        know = 1
        do i = 1, m
          x(i) = alpha(i)
        end do
      else
        know = 0
      end if

      return
      end
      subroutine p12_title ( title )

c*********************************************************************72
c
cc P12_TITLE returns a title for problem 12.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, character ( len = * ) TITLE, a title for the problem.
c
      implicit none

      character ( len = * ) title

      title = 'The deceptive function.'

      return
      end
      subroutine r8col_uniform ( m, n, a, b, seed, r )

c*********************************************************************72
c
cc R8COL_UNIFORM fills an R8COL with scaled pseudorandom numbers.
c
c  Discussion:
c
c    An R8COL is an array of R8 values, regarded as a set of column vectors.
c
c    The user specifies a minimum and maximum value for each row.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 December 2011
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Paul Bratley, Bennett Fox, Linus Schrage,
c    A Guide to Simulation,
c    Springer Verlag, pages 201-202, 1983.
c
c    Bennett Fox,
c    Algorithm 647:
c    Implementation and Relative Efficiency of Quasirandom
c    Sequence Generators,
c    ACM Transactions on Mathematical Software,
c    Volume 12, Number 4, pages 362-376, 1986.
c
c    Peter Lewis, Allen Goodman, James Miller,
c    A Pseudo-Random Number Generator for the System/360,
c    IBM Systems Journal,
c    Volume 8, pages 136-143, 1969.
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns in
c    the array.
c
c    Input, double precision A(M), B(M), the lower and upper limits.
c
c    Input/output, integer SEED, the "seed" value, which
c    should NOT be 0.  On output, SEED has been updated.
c
c    Output, double precision R(M,N), the array of pseudorandom values.
c
      implicit none

      integer m
      integer n

      double precision a(m)
      double precision b(m)
      integer i
      integer i4_huge
      parameter ( i4_huge = 2147483647 )
      integer j
      integer k
      integer seed
      double precision r(m,n)

      do j = 1, n

        do i = 1, m

          k = seed / 127773

          seed = 16807 * ( seed - k * 127773 ) - k * 2836

          if ( seed .lt. 0 ) then
            seed = seed + i4_huge
          end if

          r(i,j) = a(i) 
     &      + ( b(i) - a(i) ) * dble ( seed ) * 4.656612875D-10

        end do
      end do

      return
      end
      subroutine r8vec_indicator ( n, a )

c*********************************************************************72
c
cc R8VEC_INDICATOR sets an R8VEC to the indicator vector.
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of elements of A.
c
c    Output, double precision A(N), the array to be initialized.
c
      implicit none

      integer n

      double precision a(n)
      integer i

      do i = 1, n
        a(i) = dble ( i )
      end do

      return
      end
      subroutine r8vec_max ( n, a, amax )

c*********************************************************************72
c
cc R8VEC_MAX returns the maximum value in an R8VEC.
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 May 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in the array.
c
c    Input, double precision A(N), the array.
c
c    Output, double precision AMAX, the value of the largest entry.
c
      implicit none

      integer n

      double precision a(n)
      double precision amax
      integer i

      amax = a(1)
      do i = 2, n
        amax = max ( amax, a(i) )
      end do

      return
      end
      subroutine r8vec_min ( n, a, amin )

c*********************************************************************72
c
cc R8VEC_MIN returns the minimum value in an R8VEC.
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 May 2009
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in the array.
c
c    Input, double precision A(N), the array.
c
c    Output, double precision AMIN, the value of the smallest entry.
c
      implicit none

      integer n

      double precision a(n)
      double precision amin
      integer i

      amin = a(1)
      do i = 2, n
        amin = min ( amin, a(i) )
      end do

      return
      end
      subroutine timestamp ( )

c*********************************************************************72
c
cc TIMESTAMP prints out the current YMDHMS date as a timestamp.
c
c  Discussion:
c
c    This FORTRAN77 version is made available for cases where the
c    FORTRAN90 version cannot be used.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    None
c
      implicit none

      character * ( 8 ) ampm
      integer d
      character * ( 8 ) date
      integer h
      integer m
      integer mm
      character * ( 9 ) month(12)
      integer n
      integer s
      character * ( 10 ) time
      integer y

      save month

      data month /
     &  'January  ', 'February ', 'March    ', 'April    ',
     &  'May      ', 'June     ', 'July     ', 'August   ',
     &  'September', 'October  ', 'November ', 'December ' /

      call date_and_time ( date, time )

      read ( date, '(i4,i2,i2)' ) y, m, d
      read ( time, '(i2,i2,i2,1x,i3)' ) h, n, s, mm

      if ( h .lt. 12 ) then
        ampm = 'AM'
      else if ( h .eq. 12 ) then
        if ( n .eq. 0 .and. s .eq. 0 ) then
          ampm = 'Noon'
        else
          ampm = 'PM'
        end if
      else
        h = h - 12
        if ( h .lt. 12 ) then
          ampm = 'PM'
        else if ( h .eq. 12 ) then
          if ( n .eq. 0 .and. s .eq. 0 ) then
            ampm = 'Midnight'
          else
            ampm = 'AM'
          end if
        end if
      end if

      write ( *,
     &  '(i2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' )
     &  d, month(m), y, h, ':', n, ':', s, '.', mm, ampm

      return
      end
