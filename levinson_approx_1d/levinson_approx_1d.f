      subroutine levinson_approx_1d ( n )

c*********************************************************************72
c
cc TIMESTAMP prints out the current YMDHMS date as a timestamp.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 June 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Milton Jose Porsani, Tadeusz Ulrych,
c    Levinson-type algorithms for polynomial fitting and for Cholesky
c    and Q factors of Hankel and Vandermonde Matrices,
c    IEEE Transactions on Signal Processing,
c    Volume 43, Number 1, January 1995, pages 63-70.
c
c  Parameters:
c
c    Input, integer N, the number of polynomial coefficients,
c    or more precisely, the degree of the fitting polynomial plus 1.
c
c    Input, double precision R(2*N-1), the values that define the
c    Hankel matrix.  R(I) is the value of all elements on the
c    I-th anti-diagaonal.
c
c    Input, double precision YX(?), the cross correlation elements.
c
c    Input, double precision YY, the energy of the observations.
c
c    Output, double precision F(N), the backward modeling error operator MEO.
c
c    Output, double precision G(N), the first line of the inverse of
c    the coefficien matrix, which will be a Hankel matrix.
c
c    Output, double precision H(N), the coefficients of the least
c    squares polynomial.
c
c    Output, double precision EH, the error energy associated to the fitting.
c
c    Output, double precision RXY(N), the correlation coefficients.
c
      implicit none

      integer n

      double precision alf
      double precision beta
      double precision deltg
      double precision delth
      double precision ef
      double precision eh
      double precision eh0
      double precision f(n)
      double precision g(n)
      double precision gam
      double precision h(n)
      integer i
      integer j
      double precision r(2*n-1)
      double precision rxy(n)
      double precision yx(n)
      double precision yy

      eh0 = yy
      eh = eh0
      h(1) = - yx(1) / r(1)
      eh = eh0 + h(1) * yx(1)
      f(1) = 1.0D+00
      g(1) = 1.0D+00 / r(1)
      ef = r(1)

      do j = 1, n - 1

        deltg = r(j+1) * g(1)
        do i = 2, j
          deltg = deltg + g(i) * r(j+i)
        end do

        beta = - ef / deltg
        f(j+1) = f(j)

        do i = 1, j - 1
          f(j+1-i) = f(j-1) + beta * g(i)
        end do
        f(1) = beta * g(j)

        ef = r(j+1)
        do i = 1,j
          ef = ef + f(i) * r(2*j+2-i)
        end do

        gam = - deltg / ef
        do i = 1, j + 1
          g(i) = g(i) + gam * f(j-i+2)
        end do

        delth = yx(j+1)
        do i = 1, j
          delth = delth + h(i) * r(j+i)
        end do

        alf = - delth / ef
        do i = 1,j + 1
          h(i) = h(i) + alf * f(j-i+2)
        end do

        eh = eh + alf * delth * f(1)
        rxy(j) = 1.0D+00 - eh / eh0

      end do

      do i = 1, n
        h(i) = - h(i)
      end do

      return
      end
      subroutine timestamp ( )

c*********************************************************************72
c
cc TIMESTAMP prints out the current YMDHMS date as a timestamp.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 June 2014
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
     &  d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, 
     &  trim ( ampm )

      return
      end
