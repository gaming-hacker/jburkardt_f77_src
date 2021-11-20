      program main

c*********************************************************************72
c
cc MAIN is the main program for TOMS725_PRB.
c
c  Discussion:
c
c    TOMS725_PRB tests the TOMS725 library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 May 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Zvi Drezner,
c    Algorithm 725: Computation of the Multivariate Normal Integral,
c    Transactions on Mathematical Software,
c    Volume 18, Number 4, December 1992, pages 470-480.
c
      implicit none

      double precision diff
      double precision err
      double precision h(20)
      integer ier
      integer ih
      integer ir
      integer j
      integer k
      integer l
      integer m
      integer number
      double precision prob
      double precision r(20,20)
      double precision result(12)

      save result

      data result /
     &  0.72437D+00, 0.74520D+00, 0.95618D+00, 0.95855D+00, 0.63439D+00,
     &  0.67778D+00, 0.93656D+00, 0.94253D+00, 0.56299D+00, 0.62670D+00,
     &  0.91819D+00, 0.92845D+00 /

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TOMS725_PRB:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the TOMS725 library.'

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '   M      Calculated       True          Diff.    IER'
      write ( *, '(a)' ) ' '

      number = 0

      k = 0

      do m = 2, 4
        do ih = 1, 2
          do ir = 1, 2

            do l = 1, m
              h(l) = ih
              do j = 1, m
                r(l,j) = 0.25D+00 * ir
              end do
              r(l,l) = 1.0D+00
            end do

            number = number + 1
            call dmv ( m, k, h, r, prob, 1.0D-05, ier, err, 15.0D+00 )
            diff = prob - result(number)
            write ( *, '(i4,2f16.8,1p,e12.2,i5)' ) 
     &        m, prob, result(number), diff, ier

          end do
        end do
      end do
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'TOMS725_PRB:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ''
      call timestamp ( )

      stop
      end
