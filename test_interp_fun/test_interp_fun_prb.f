      program main

c*********************************************************************72
c
cc MAIN tests TEST_INTERP_FUN.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 February 2012
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp (  )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST_INTERP_FUN_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the TEST_INTERP_FUN library.'

      call test01 ( )
      call test02 ( )
      call test03 ( )
      call test04 ( )
      call test05 ( )
      call test06 ( )
      call test07 ( )
      call test08 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST_INTERP_FUN_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'

      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test01 ( )

c*********************************************************************72
c
cc TEST01 shows how P00_TITLE can be called.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 February 2012
c
c  Author:
c
c    John Burkardt
c
      implicit none

      double precision a
      double precision b
      integer prob
      integer prob_num
      character * ( 80 ) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST01'
      write ( *, '(a)' ) 
     &  '  Demonstrate some of the bookkeeping routines.'
      write ( *, '(a)' ) 
     &  '  P00_PROB_NUM returns the number of problems.'
      write ( *, '(a)' ) '  P00_TITLE returns the problem title.'
      write ( *, '(a)' ) '  P00_LIMIT returns the problem limits.'

      call p00_prob_num ( prob_num )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  Number of problems = ', prob_num

      do prob = 1, prob_num

        write ( *, '(a)' ) ' '
        write ( *, '(a,i8)' ) '  Problem ', prob
        call p00_title ( prob, title )
        write ( *, '(a)' ) 
     &    '  Problem TITLE = "' // trim ( title ) // '".'
        call p00_lim ( prob, a, b )
        write ( *, '(a,g14.6)' ) '  Problem lower limit A = ', a
        write ( *, '(a,g14.6)' ) '  Problem upper limit B = ', b

      end do

      return
      end
      subroutine test02 ( )

c*********************************************************************72
c
cc TEST02 shows how P00_STORY can be called.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 February 2012
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer prob
      integer prob_num

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST02'
      write ( *, '(a)' ) '  P00_STORY prints the problem "story".'

      call p00_prob_num ( prob_num )

      do prob = 1, prob_num

        write ( *, '(a)' ) ' '
        write ( *, '(a,i8)' ) '  Problem ', prob

        call p00_story ( prob )

      end do

      return
      end
      subroutine test03 ( )

c*********************************************************************72
c
cc TEST03 uses equally spaced polynomial interpolation.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 February 2012
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer max_tab
      parameter ( max_tab = 21 )

      double precision a
      double precision b
      double precision diftab(max_tab)
      double precision error_max
      integer i
      integer imax
      integer n
      integer prob
      integer prob_num
      double precision p00_fun
      character * ( 80 ) title
      integer type
      double precision x
      double precision xtab(max_tab)
      double precision yapprox
      double precision ytab(max_tab)
      double precision ytrue

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST03'
      write ( *, '(a)' ) '  Equally spaced polynomial interpolation.'
      write ( *, '(a)' ) 
     &  '  Evaluate the function at N equally spaced points.'
      write ( *, '(a)' ) 
     &  '  Determine the N-1 degre polynomial interpolant.'
      write ( *, '(a)' ) 
     &  '  Estimate the maximum difference between the function'
      write ( *, '(a)' ) '  and the interpolant.'

      call p00_prob_num ( prob_num )

      do prob = 1, prob_num

        call p00_title ( prob, title )
        call p00_lim ( prob, a, b )

        write ( *, '(a)' ) ' '
        write ( *, '(a,i8)' ) '  Problem ', prob
        write ( *, '(2x,a)' ) trim ( title )
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '     N   Max ||Error||'
        write ( *, '(a)' ) ' '

        do n = 1, max_tab, 4
c
c  Evaluate the function at N equally spaced points.
c
          do i = 1, n

            if ( n .eq. 1 ) then
              xtab(i) = 0.5D+00 * ( a + b )
            else
              xtab(i) = ( dble ( n - i     ) * a 
     &                  + dble (     i - 1 ) * b 
     &                ) / dble ( n     - 1 )
            end if

            ytab(i) = p00_fun ( prob, xtab(i) )

          end do
c
c  Construct the interpolating polynomial via finite differences.
c
          call data_to_dif ( n, xtab, ytab, diftab )
c
c  Now examine the approximation error.
c
          error_max = 0.0D+00
          imax = 100

          do i = 0, imax

            if ( imax .eq. 0 ) then
              x = 0.5D+00 * ( a + b )
            else
              x = ( dble ( imax - i ) * a    
     &            + dble (        i ) * b  ) 
     &            / dble ( imax     )
            end if

            ytrue = p00_fun ( prob, x )
            call dif_val ( n, xtab, diftab, x, yapprox )
            error_max = max ( error_max, abs ( ytrue - yapprox ) )

          end do

          write ( *, '(2x,i4,2x,g14.6)' ) n, error_max

        end do

      end do

      return
      end
      subroutine test04 ( )

c*********************************************************************72
c
cc TEST04 uses Bernstein polynomial approximation on functional problems.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 February 2012
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer max_tab
      parameter ( max_tab = 21 )

      double precision a
      double precision b
      double precision error_max
      integer i
      integer imax
      integer ndata
      integer prob
      integer prob_num
      double precision p00_fun
      character * ( 80 ) title
      double precision xdata(max_tab)
      double precision xval
      double precision ydata(max_tab)
      double precision ytrue
      double precision yval

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST04'
      write ( *, '(a)' ) '  Bernstein polynomial approximation.'
      write ( *, '(a)' ) 
     &  '  Evaluate the function at N equally spaced points.'
      write ( *, '(a)' ) 
     &  '  Determine the N-1 degree Bernstein polynomial approximant.'
      write ( *, '(a)' ) 
     &  '  Estimate the maximum difference between the function'
      write ( *, '(a)' ) '  and the approximant.'

      call p00_prob_num ( prob_num )

      do prob = 1, prob_num

        call p00_title ( prob, title )
        call p00_lim ( prob, a, b )

        write ( *, '(a)' ) ' '
        write ( *, '(a,i8)' ) '  Problem ', prob
        write ( *, '(2x,a)' ) trim ( title )
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '     N   Max ||Error||'
        write ( *, '(a)' ) ' '

        do ndata = 1, max_tab, 4
c
c  Evaluate the function at NDATA equally spaced points.
c
          do i = 1, ndata

            if ( ndata .eq. 1 ) then
              xdata(i) = 0.5D+00 * ( a + b )
            else
              xdata(i) = ( dble ( ndata - i     ) * a    
     &                   + dble (         i - 1 ) * b  ) 
     &                   / dble ( ndata     - 1 )
            end if

            ydata(i) = p00_fun ( prob, xdata(i) )

          end do
c
c  Now examine the approximation error.
c
          error_max = 0.0D+00
          imax = 100

          do i = 0, imax

            if ( imax .eq. 0 ) then
              xval = 0.5D+00 * ( a + b )
            else
              xval = ( dble ( imax - i ) * a    
     &               + dble (        i ) * b  ) 
     &               / dble ( imax     )
            end if

            ytrue = p00_fun ( prob, xval )

            call bpab_approx ( ndata - 1, a, b, ydata, xval, yval )

            error_max = max ( error_max, abs ( ytrue - yval ) )

          end do

          write ( *, '(2x,i4,2x,g14.6)' ) ndata, error_max

        end do

      end do

      return
      end
      subroutine test05 ( )

c*********************************************************************72
c
cc TEST05 uses linear spline interpolation on all problems.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 February 2012
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer max_data
      parameter ( max_data = 21 )

      double precision a
      double precision b
      double precision error_max
      integer i
      integer imax
      character mark
      integer ndata
      integer prob
      integer prob_num
      double precision p00_fun
      character * ( 80 ) title
      double precision xdata(max_data)
      double precision xval
      double precision ydata(max_data)
      double precision ypval
      double precision ytrue
      double precision yval

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST05'
      write ( *, '(a)' ) '  Linear spline interpolation.'

      call p00_prob_num ( prob_num )

      do prob = 1, prob_num

        call p00_title ( prob, title )
        call p00_lim ( prob, a, b )

        write ( *, '(a)' ) ' '
        write ( *, '(a,i8)' ) '  Problem ', prob
        write ( *, '(2x,a)' ) trim ( title )
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '     N   Max ||Error||'
        write ( *, '(a)' ) ' '

        do ndata = 2, max_data, 4
c
c  Evaluate the function at NDATA equally spaced points.
c
          do i = 1, ndata

            if ( ndata .eq. 1 ) then
              xdata(i) = 0.5D+00 * ( a + b )
            else
              xdata(i) = ( dble ( ndata - i     ) * a    
     &                   + dble (         i - 1 ) * b  ) 
     &                   / dble ( ndata     - 1 )
            end if

            ydata(i) = p00_fun ( prob, xdata(i) )

          end do
c
c  Evaluate the interpolation function.
c
          error_max = 0.0D+00
          imax = 100

          do i = 0, imax

            if ( imax .eq. 0 ) then
              xval = 0.5D+00 * ( a + b )
            else
              xval = ( dble ( imax - i ) * a    
     &               + dble (        i ) * b  ) 
     &               / dble ( imax     )
            end if

            call spline_linear_val ( ndata, xdata, ydata, xval, 
     &        yval, ypval )

            ytrue = p00_fun ( prob, xval )

            error_max = max ( error_max, abs ( yval - ytrue ) );

          end do

          write ( *, '(2x,i4,2x,g14.6)' ) ndata, error_max

        end do

      end do

      return
      end
      subroutine test06 ( )

c*********************************************************************72
c
cc TEST06 uses Overhauser spline interpolation on all problems.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 February 2012
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer ndata
      parameter ( ndata = 11 )
      integer num_dim
      parameter ( num_dim = 1 )

      double precision a
      double precision b
      integer i
      integer j
      integer jhi
      integer jmax
      character mark
      integer prob
      integer prob_num
      double precision p00_fun
      character * ( 80 ) title
      double precision xdata(ndata)
      double precision xval
      double precision ydata(ndata)
      double precision yval

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST06'
      write ( *, '(a)' ) '  Overhauser spline interpolation.'

      call p00_prob_num ( prob_num )

      do prob = 1, prob_num

        call p00_title ( prob, title )

        call p00_lim ( prob, a, b )

        do i = 1, ndata
          xdata(i) = ( dble ( ndata - i     ) * a   
     &               + dble (         i - 1 ) * b ) 
     &               / dble ( ndata     - 1 )
          ydata(i) = p00_fun ( prob, xdata(i) )
        end do

        write ( *, '(a)' ) ' '
        write ( *, '(a,i8)' ) '  Problem ', prob
        write ( *, '(a)' ) trim ( title )
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  X   Y'
        write ( *, '(a)' ) ' '
c
c  Evaluate the interpolation function.
c
        do i = 1, ndata - 1

          jmax = 3

          if ( i .eq. ndata - 1 ) then
            jhi = jmax
          else
            jhi = jmax - 1
          end if

          do j = 1, jhi

            xval = ( dble ( jmax - j     ) * xdata(i)     
     &             + dble (        j - 1 ) * xdata(i+1) ) 
     &             / dble ( jmax     - 1 )

            call spline_overhauser_val ( num_dim, ndata, xdata, 
     &        ydata, xval, yval )

            if ( j .eq. 1 .or. j .eq. 3 ) then
              mark = '*'
            else
              mark = ' '
            end if

            write ( *, '(2x,a,2g14.6)' ) mark, xval, yval

          end do

        end do

      end do

      return
      end
      subroutine test07 ( )

c*********************************************************************72
c
cc TEST07 uses cubic spline interpolation on all problems.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 February 2012
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer ndata
      parameter ( ndata = 11 )

      double precision a
      double precision b
      integer i
      integer ibcbeg
      integer ibcend
      integer j
      integer jhi
      integer jmax
      character mark
      integer prob
      integer prob_num
      double precision p00_fun
      character * ( 80 ) title
      double precision xdata(ndata)
      double precision xval
      double precision ybcbeg
      double precision ybcend
      double precision ydata(ndata)
      double precision ypp(ndata)
      double precision yppval
      double precision ypval
      double precision yval

      ibcbeg = 0
      ibcend = 0
      ybcbeg = 0.0D+00
      ybcend = 0.0D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST07'
      write ( *, '(a)' ) '  Cubic spline interpolation.'

      call p00_prob_num ( prob_num )

      do prob = 1, prob_num

        call p00_title ( prob, title )
        call p00_lim ( prob, a, b )

        do i = 1, ndata
          xdata(i) = ( dble ( ndata - i     ) * a   
     &               + dble (         i - 1 ) * b ) 
     &               / dble ( ndata     - 1 )
          ydata(i) = p00_fun ( prob, xdata(i) )
        end do
c
c  Set up the interpolation function.
c
        call spline_cubic_set ( ndata, xdata, ydata, ibcbeg, ybcbeg, 
     &    ibcend, ybcend, ypp )

        write ( *, '(a)' ) ' '
        write ( *, '(a,i8)' ) '  Problem ', prob
        write ( *, '(2x,a)' ) trim ( title )
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '    X   Y'
        write ( *, '(a)' ) ' '
c
c  Evaluate the interpolation function.
c
        do i = 1, ndata - 1

          jmax = 3

          if ( i .eq. ndata - 1 ) then
            jhi = jmax
          else
            jhi = jmax - 1
          end if

          do j = 1, jhi

            xval = ( dble ( jmax - j     ) * xdata(i)     
     &             + dble (        j - 1 ) * xdata(i+1) ) 
     &             / dble ( jmax     - 1 )

            call spline_cubic_val ( ndata, xdata, ydata, ypp, xval, 
     &        yval, ypval, yppval )

            if ( j .eq. 1 .or. j .eq. 3 ) then
              mark = '*'
            else
              mark = ' '
            end if

            write ( *, '(2x,a,2g14.6)' ) mark, xval, yval

          end do

        end do

      end do

      return
      end
      subroutine test08 ( )

c*********************************************************************72
c
cc TEST08 uses B spline approximation on all problems.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 February 2012
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer ndata
      parameter ( ndata = 11 )

      double precision a
      double precision b
      integer i
      integer j
      integer jhi
      integer jmax
      character mark
      integer prob
      integer prob_num
      double precision p00_fun
      character * ( 80 ) title
      double precision xdata(ndata)
      double precision xval
      double precision ydata(ndata)
      double precision yval

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST08'
      write ( *, '(a)' ) '  B spline approximation.'

      call p00_prob_num ( prob_num )

      do prob = 1, prob_num

        call p00_title ( prob, title )
        call p00_lim ( prob, a, b )

        do i = 1, ndata
          xdata(i) = ( dble ( ndata - i     ) * a   
     &               + dble (         i - 1 ) * b ) 
     &               / dble ( ndata     - 1 )
          ydata(i) = p00_fun ( prob, xdata(i) )
        end do

        write ( *, '(a)' ) ' '
        write ( *, '(a,i8)' ) '  Problem ', prob
        write ( *, '(2x,a)' ) trim ( title )
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '       X        Y'
        write ( *, '(a)' ) ' '
c
c  Evaluate the interpolation function.
c
        do i = 1, ndata - 1

          jmax = 3

          if ( i .eq. ndata - 1 ) then
            jhi = jmax
          else
            jhi = jmax - 1
          end if

          do j = 1, jhi

            xval = ( dble ( jmax - j     ) * xdata(i)     
     &             + dble (        j - 1 ) * xdata(i+1) ) 
     &             / dble ( jmax     - 1 )

            call spline_b_val ( ndata, xdata, ydata, xval, yval )

            if ( j .eq. 1 .or. j .eq. 3 ) then
              mark = '*'
            else
              mark = ' '
            end if

            write ( *, '(2x,a,2g14.6)' ) mark, xval, yval

          end do

        end do

      end do

      return
      end
