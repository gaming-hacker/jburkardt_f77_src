      program main

c*********************************************************************72
c
cc MAIN is the main program for ASA266_PRB.
c
c  Discussion:
c
c    ASA266_PRB tests the ASA266 library.
c
c  Modified:
c
c    13 January 2008
c
c  Author:
c
c    John Burkardt
c
      implicit none

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'ASA266_PRB:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the ASA266 library.'

      call test08 ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'ASA266_PRB:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test08 ( )

c*********************************************************************72
c
c! TEST08 tests DIRICH, DIRICHLET_MEAN, DIRICHLET_VARIANCE.
c
c  Discussion:
c
c    Canned data is used.
c
c  Modified:
c
c    13 January 2008
c
c  Author:
c
c    John Burkardt
c
      implicit none

      integer elem_num
      parameter ( elem_num = 3 )
      integer sample_num
      parameter ( sample_num = 23 )

      double precision alpha(elem_num)
      double precision alpha_sum
      double precision aminus
      double precision aplus
      integer elem_i
      double precision eps
      double precision g(elem_num)
      integer i
      integer ifault
      integer init
      integer j
      double precision mean(elem_num)
      integer niter
      double precision rlogl
      double precision s
      integer sample_i
      double precision v((elem_num*(elem_num+1))/2)
      double precision vari
      double precision variance(elem_num)
      double precision work(2*elem_num)
      double precision x(sample_num,elem_num)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST08'
      write ( *, '(a)' ) '  For samples of a Dirichlet PDF,'
      write ( *, '(a)' ) '  DIRICH estimates the parameters.'
      write ( *, '(a)' ) '  DIRICHLET_MEAN finds the means;'
      write ( *, '(a)' ) '  DIRICHLET_VARIANCE finds the variances;'
c
c  Set the data
c
      x(1,1) = 0.178D+00
      x(1,2) = 0.346D+00
      x(1,3) = 0.476D+00

      x(2,1) = 0.162D+00
      x(2,2) = 0.307D+00
      x(2,3) = 0.531D+00

      x(3,1) = 0.083D+00
      x(3,2) = 0.448D+00
      x(3,3) = 0.469D+00

      x(4,1) = 0.087D+00
      x(4,2) = 0.474D+00
      x(4,3) = 0.439D+00

      x(5,1) = 0.078D+00
      x(5,2) = 0.503D+00
      x(5,3) = 0.419D+00

      x(6,1) = 0.040D+00
      x(6,2) = 0.456D+00
      x(6,3) = 0.504D+00

      x(7,1) = 0.049D+00
      x(7,2) = 0.363D+00
      x(7,3) = 0.588D+00

      x(8,1) = 0.100D+00
      x(8,2) = 0.317D+00
      x(8,3) = 0.583D+00

      x(9,1) = 0.075D+00
      x(9,2) = 0.394D+00
      x(9,3) = 0.531D+00

      x(10,1) = 0.084D+00
      x(10,2) = 0.445D+00
      x(10,3) = 0.471D+00

      x(11,1) = 0.060D+00
      x(11,2) = 0.435D+00
      x(11,3) = 0.505D+00

      x(12,1) = 0.089D+00
      x(12,2) = 0.418D+00
      x(12,3) = 0.493D+00

      x(13,1) = 0.050D+00
      x(13,2) = 0.485D+00
      x(13,3) = 0.465D+00

      x(14,1) = 0.073D+00
      x(14,2) = 0.378D+00
      x(14,3) = 0.549D+00

      x(15,1) = 0.064D+00
      x(15,2) = 0.562D+00
      x(15,3) = 0.374D+00

      x(16,1) = 0.085D+00
      x(16,2) = 0.465D+00
      x(16,3) = 0.450D+00

      x(17,1) = 0.094D+00
      x(17,2) = 0.388D+00
      x(17,3) = 0.518D+00

      x(18,1) = 0.014D+00
      x(18,2) = 0.449D+00
      x(18,3) = 0.537D+00

      x(19,1) = 0.060D+00
      x(19,2) = 0.544D+00
      x(19,3) = 0.396D+00

      x(20,1) = 0.031D+00
      x(20,2) = 0.569D+00
      x(20,3) = 0.400D+00

      x(21,1) = 0.025D+00
      x(21,2) = 0.491D+00
      x(21,3) = 0.484D+00

      x(22,1) = 0.045D+00
      x(22,2) = 0.613D+00
      x(22,3) = 0.342D+00

      x(23,1) = 0.0195D+00
      x(23,2) = 0.526D+00
      x(23,3) = 0.4545D+00

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Sampled data:'
      write ( *, '(a)' ) ' '

      do sample_i = 1, sample_num
        write ( *, '(i8,3g14.6)' )
     &  sample_i, ( x(sample_i,j), j = 1, elem_num )
      end do
c
c  Compute the observed averages.
c
      call r8col_mean ( sample_num, elem_num, x, mean )

      call r8col_variance ( sample_num, elem_num, x, variance )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Observed means, variances are:'
      write ( *, '(a)' ) ' '
      do elem_i = 1, elem_num
        write ( *, '(i6,2g14.6)' )
     &  elem_i, mean(elem_i), variance(elem_i)
      end do

      init = 1

      call dirich ( elem_num, sample_num, x, sample_num,
     &  init, alpha, rlogl, v, g, niter, s, eps, work, ifault )

      if ( ifault .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'WARNING!'
        write ( *, '(a)' ) '  DIRICHLET_ESTIMATE error code:'
        write ( *, '(a,i8)' ) '  IFAULT = ', ifault
      end if

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Index, Estimate, Lower Limit, Upper Limit:'
      write ( *, '(a)' ) ' '

      do elem_i = 1, elem_num
        vari = v((elem_i*(elem_i-1))/2+elem_i)
        aminus = alpha(elem_i) - 1.96D+00 * dsqrt ( vari )
        aplus = alpha(elem_i) + 1.96D+00 * dsqrt ( vari )
        write ( *, '(i8,3g14.6)' )
     &  elem_i, alpha(elem_i), aminus, aplus
      end do

      call dirichlet_mean ( elem_num, alpha, mean )

      call dirichlet_variance ( elem_num, alpha, variance )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'Expected means, variances are:'
      write ( *, '(a)' ) ' '
      do elem_i = 1, elem_num
        write ( *, '(i8,2g14.6)' )
     &  elem_i, mean(elem_i), variance(elem_i)
      end do

      alpha_sum = 0.0D+00
      do elem_i = 1, elem_num
        alpha_sum = alpha_sum + alpha(elem_i)
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  Alpha sum is ', alpha_sum
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  NORMALIZED VALUES:'
      write ( *, '(a)' ) '  Index, Estimate, Lower Limit, Upper Limit:'
      write ( *, '(a)' ) ' '

      do elem_i = 1, elem_num
        vari = v((elem_i*(elem_i-1))/2+elem_i)
        aminus = ( alpha(elem_i) - 1.96D+00 * dsqrt ( vari ) )
     &  / alpha_sum
        aplus = ( alpha(elem_i) + 1.96D+00 * dsqrt ( vari ) )
     &  / alpha_sum
        write ( *, '(i8,3g14.6)' )
     &  elem_i, alpha(elem_i)/alpha_sum, aminus, aplus
      end do

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) 'Log likelikhood function = ', rlogl

      return
      end
