      program main

c*********************************************************************72
c
cc EX14 demonstrates the use of TeX instructions for math formulas.
c
c  Discussion:
c
c    On Unix systems, the backslash character is interpreted as a special
c    symbol BEFORE THE COMPILER SEES IT, unless it is inside a double-quote
c    string.
c
c    If you know the program will be used on a UNIX system, you can 'simply'
c    type TWO backslash characters whenever you mean one.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 February 2014
c
c  Author:
c
c    This FORTRAN77 version by John Burkardt
c
c  Reference:
c
c    Helmut Michels,
c    The Data Plotting Software DISLIN - version 10.4,
c    Shaker Media GmbH, January 2010,
c    ISBN13: 978-3-86858-517-9.
c
      implicit none

      character*80 cstr
      integer nl
      integer nlmess

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'EX14:'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Demonstrate the use of TeX instructions to'
      write ( *, '(a)' ) '  create mathematical formulas.'
c
c  Specify the format of the output file.
c
      call metafl ( 'png' )
c
c  Indicate that new data overwrites old data.
c
      call filmod ( 'delete' )
c
c  Specify the name of the output graphics file.
c
      call setfil ( 'ex14.png' )
c
c  Choose the page size and orientation.
c
      call setpag ( 'usap' )
c
c  For PNG output, reverse the default black background to white.
c
      call scrmod ( 'reverse' )
c
c  Open DISLIN.
c
      call disini ( )
c
c  Plot a border around the page.
c
      call pagera ( )
c
c  Use the COMPLEX font.
c
      call complx ( )
      call height ( 40 )

      cstr = 'TeX Instructions for Mathematical Formulas'
      nl = nlmess ( cstr )
      call messag ( cstr, (2100 - nl)/2, 100 ) 
  
      call texmod ( 'ON' )
      call messag ( '$\frac{1}{x+y}$', 150, 400 )
      call messag ( '$\frac{a^2 - b^2}{a+b} = a - b$', 1200, 400 )
  
      call messag ( '$r = \red{\sqrt{x^2 + y^2}}', 150, 700 )
      call messag ( '$\cos \phi = \frac{x}{\sqrt{x^2 + y^2}}$', 
     &  1200, 700 )

      call messag ( '$\Gamma(x) = \int_0^\infty e^{-t}t^{x-1}dt$', 
     &  150, 1000 )
      call messag ( '$\lim_{x \to \infty} (1 + \frac{1}{x})^x = e$', 
     &  1200, 1000 )

      call messag ( '$\mu = \sum_{i = 1}^n x_i p_i$', 150, 1300 )
      call messag ( '$\mu = \int_{-\infty}^ \infty x f(x) dx$', 
     &  1200, 1300 )

      call messag ( 
     &  '$\overline{x} = \frac{1}{n} \sum_{i = 1}^n x_i$', 
     &  150, 1600 )

      call messag ( '$s^2 = \frac{1}{n-1} \sum_{i = 1}^n' //
     &  '(x_i - \overline{x})^2$', 1200, 1600 )

      call messag ( '$\sqrt[n]{\frac{x^n - y^n}{1 + u^{2n}}}$', 
     &  150, 1900 )  
      call messag ( '$\sqrt[3]{-q + \sqrt{q^2 + p^3}}$', 1200, 1900 )

      call messag ( '$\int \frac{dx}{1+x^2} = \arctan x + C$', 
     &  150, 2200 )

      call messag ( 
     &  '$\int \frac{dx}{\sqrt{1+x^2}} = {\rm arcsinh} x + C$',
     &  1200, 2200 )

      call messag ( '$\overline{P_1P_2} = \sqrt{(x_2-x_1)^2 + ' //
     &  '(y_2-y_1)^2}$', 150, 2500 )
      call messag ( '$x = \frac{x_1 + \lambda x_2}{1 + \lambda}$', 
     &  1200, 2500 )
c
c  Close DISLIN.
c
      call disfin ( )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'EX14:'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
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
