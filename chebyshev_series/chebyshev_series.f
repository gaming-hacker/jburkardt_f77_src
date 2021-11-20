      subroutine echebser0 ( x, coef, nc, y0 )

c*********************************************************************72
c
cc ECHEBSER0 evaluates a Chebyshev series.
c
c  Discussion:
c
c    This function implements a modification and extension of 
c    Maess's algorithm.  Table 6.5.1 on page 164 of the reference 
c    gives an example for treating the first derivative.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    21 April 2014
c
c  Author:
c
c    Manfred Zimmer
c
c  Reference:
c
c    Charles Clenshaw,
c    Mathematical Tables, Volume 5,
c    Chebyshev series for mathematical functions,
c    London, 1962.
c
c    Gerhard Maess,
c    Vorlesungen ueber Numerische Mathematik II, Analysis,
c    Berlin, Akademie_Verlag, 1984-1988,
c    ISBN: 978-3764318840,
c    LC: QA297.M325.  
c
c  Parameters:
c
c    Input, double precision X, the evaluation point.
c    -1 <= X <= +1.
c
c    Input, double precision COEF(NC), the Chebyshev series.
c
c    Input, integer NC, the number of terms in the series.
c    0 .lt. NC.
c
c    Output, double precision Y0, the value of the Chebyshev series at X.
c
      implicit none

      integer nc

      double precision b0
      double precision b1
      double precision b2
      double precision coef(nc)
      integer i
      double precision x
      double precision x2
      double precision y0

      b0 = coef(nc)
      b1 = 0.0D+00
      b2 = 0.0D+00

      x2 = 2.0D+00 * x

      do i = nc - 1, 1, -1
        b2 = b1
        b1 = b0
        b0 = coef(i) - b2 + x2 * b1
      end do

      y0 = 0.5D+00 * ( b0 - b2 )

      return
      end
      subroutine echebser1 ( x, coef, nc, y0, y1 )

c*********************************************************************72
c
cc ECHEBSER1 evaluates a Chebyshev series and first derivative.
c
c  Discussion:
c
c    This function implements a modification and extension of 
c    Maess's algorithm.  Table 6.5.1 on page 164 of the reference 
c    gives an example for treating the first derivative.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    21 April 2014
c
c  Author:
c
c    Manfred Zimmer
c
c  Reference:
c
c    Charles Clenshaw,
c    Mathematical Tables, Volume 5,
c    Chebyshev series for mathematical functions,
c    London, 1962.
c
c    Gerhard Maess,
c    Vorlesungen ueber Numerische Mathematik II, Analysis,
c    Berlin, Akademie_Verlag, 1984-1988,
c    ISBN: 978-3764318840,
c    LC: QA297.M325.  
c
c  Parameters:
c
c    Input, double precision X, the evaluation point.
c    -1 <= X <= +1.
c
c    Input, double precision COEF(NC), the Chebyshev series.
c
c    Input, integer NC, the number of terms in the series.
c    0 .lt. NC.
c
c    Output, double precision Y0, the value of the Chebyshev series at X.
c
c    Output, double precision Y1, the value of the 1st derivative of the
c    Chebyshev series at X.
c
      implicit none

      integer nc

      double precision b0
      double precision b1
      double precision b2
      double precision c0
      double precision c1
      double precision c2
      double precision coef(nc)
      integer i
      double precision x
      double precision x2
      double precision y0
      double precision y1

      b0 = coef(nc)
      b1 = 0.0D+00
      b2 = 0.0D+00

      c0 = coef(nc)
      c1 = 0.0D+00
      c2 = 0.0D+00

      x2 = 2.0D+00 * x

      do i = nc - 1, 1, -1

        b2 = b1
        b1 = b0
        b0 = coef(i) - b2 + x2 * b1

        if ( 1 .lt. i ) then
          c2 = c1
          c1 = c0
          c0 = b0 - c2 + x2 * c1
        end if

      end do

      y0 = 0.5D+00 * ( b0 - b2 )
      y1 = c0 - c2

      return
      end
      subroutine echebser2 ( x, coef, nc, y0, y1, y2 )

c*********************************************************************72
c
cc ECHEBSER2 evaluates a Chebyshev series and two derivatives.
c
c  Discussion:
c
c    This function implements a modification and extension of 
c    Maess's algorithm.  Table 6.5.1 on page 164 of the reference 
c    gives an example for treating the first derivative.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    21 April 2014
c
c  Author:
c
c    Manfred Zimmer
c
c  Reference:
c
c    Charles Clenshaw,
c    Mathematical Tables, Volume 5,
c    Chebyshev series for mathematical functions,
c    London, 1962.
c
c    Gerhard Maess,
c    Vorlesungen ueber Numerische Mathematik II, Analysis,
c    Berlin, Akademie_Verlag, 1984-1988,
c    ISBN: 978-3764318840,
c    LC: QA297.M325.  
c
c  Parameters:
c
c    Input, double precision X, the evaluation point.
c    -1 <= X <= +1.
c
c    Input, double precision COEF(NC), the Chebyshev series.
c
c    Input, integer NC, the number of terms in the series.
c    0 .lt. NC.
c
c    Output, double precision Y0, the value of the Chebyshev series at X.
c
c    Output, double precision Y1, the value of the 1st derivative of the
c    Chebyshev series at X.
c
c    Output, double precision Y2, the value of the 2nd derivative of the
c    Chebyshev series at X.
c
      implicit none

      integer nc

      double precision b0
      double precision b1
      double precision b2
      double precision c0
      double precision c1
      double precision c2
      double precision coef(nc)
      double precision d0
      double precision d1
      double precision d2
      integer i
      double precision x
      double precision x2
      double precision y0
      double precision y1
      double precision y2

      b0 = coef(nc)
      b1 = 0.0D+00
      b2 = 0.0D+00
      c0 = coef(nc)
      c1 = 0.0D+00
      c2 = 0.0D+00
      d0 = coef(nc)
      d1 = 0.0D+00
      d2 = 0.0D+00

      x2 = 2.0D+00 * x

      do i = nc - 1, 1, -1

        b2 = b1
        b1 = b0
        b0 = coef(i) - b2 + x2 * b1

        if ( 1 .lt. i ) then
          c2 = c1
          c1 = c0
          c0 = b0 - c2 + x2 * c1
        end if

        if ( 2 .lt. i ) then
          d2 = d1
          d1 = d0
          d0 = c0 - d2 + x2 * d1
        end if

      end do

      y0 = 0.5D+00 * ( b0 - b2 )
      y1 = c0 - c2
      y2 = ( d0 - d2 ) * 4.0D+00

      return
      end
      subroutine echebser3 ( x, coef, nc, y0, y1, y2, y3 )

c*********************************************************************72
c
cc ECHEBSER3 evaluates a Chebyshev series and three derivatives.
c
c  Discussion:
c
c    This function implements a modification and extension of 
c    Maess's algorithm.  Table 6.5.1 on page 164 of the reference 
c    gives an example for treating the first derivative.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    21 April 2014
c
c  Author:
c
c    Manfred Zimmer
c
c  Reference:
c
c    Charles Clenshaw,
c    Mathematical Tables, Volume 5,
c    Chebyshev series for mathematical functions,
c    London, 1962.
c
c    Gerhard Maess,
c    Vorlesungen ueber Numerische Mathematik II, Analysis,
c    Berlin, Akademie_Verlag, 1984-1988,
c    ISBN: 978-3764318840,
c    LC: QA297.M325.  
c
c  Parameters:
c
c    Input, double precision X, the evaluation point.
c    -1 <= X <= +1.
c
c    Input, double precision COEF(NC), the Chebyshev series.
c
c    Input, integer NC, the number of terms in the series.
c    0 .lt. NC.
c
c    Output, double precision Y0, the value of the Chebyshev series at X.
c
c    Output, double precision Y1, the value of the 1st derivative of the
c    Chebyshev series at X.
c
c    Output, double precision Y2, the value of the 2nd derivative of the
c    Chebyshev series at X.
c
c    Output, double precision Y3, the value of the 3rd derivative of the
c    Chebyshev series at X.
c
      implicit none

      integer nc

      double precision b0
      double precision b1
      double precision b2
      double precision c0
      double precision c1
      double precision c2
      double precision coef(nc)
      double precision d0
      double precision d1
      double precision d2
      double precision e0
      double precision e1
      double precision e2
      integer i
      double precision x
      double precision x2
      double precision y0
      double precision y1
      double precision y2
      double precision y3

      b0 = coef(nc)
      b1 = 0.0D+00
      b2 = 0.0D+00
      c0 = coef(nc)
      c1 = 0.0D+00
      c2 = 0.0D+00
      d0 = coef(nc)
      d1 = 0.0D+00
      d2 = 0.0D+00
      e0 = coef(nc)
      e1 = 0.0D+00
      e2 = 0.0D+00

      x2 = 2.0D+00 * x

      do i = nc - 1, 1, -1

        b2 = b1
        b1 = b0
        b0 = coef(i) - b2 + x2 * b1

        if ( 1 .lt. i ) then
          c2 = c1
          c1 = c0
          c0 = b0 - c2 + x2 * c1
        end if

        if ( 2 .lt. i ) then
          d2 = d1
          d1 = d0
          d0 = c0 - d2 + x2 * d1
        end if

        if ( 3 .lt. i ) then
          e2 = e1
          e1 = e0
          e0 = d0 - e2 + x2 * e1
        end if

      end do

      y0 = 0.5D+00 * ( b0 - b2 )
      y1 = c0 - c2
      y2 = ( d0 - d2 ) * 4.0D+00
      y3 = ( e0 - e2 ) * 24.0D+00

      return
      end
      subroutine echebser4 ( x, coef, nc, y0, y1, y2, y3, y4 )

c*********************************************************************72
c
cc ECHEBSER4 evaluates a Chebyshev series and four derivatives.
c
c  Discussion:
c
c    This function implements a modification and extension of 
c    Maess's algorithm.  Table 6.5.1 on page 164 of the reference 
c    gives an example for treating the first derivative.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    29 April 2014
c
c  Author:
c
c    Manfred Zimmer
c
c  Reference:
c
c    Charles Clenshaw,
c    Mathematical Tables, Volume 5,
c    Chebyshev series for mathematical functions,
c    London, 1962.
c
c    Gerhard Maess,
c    Vorlesungen ueber Numerische Mathematik II, Analysis,
c    Berlin, Akademie_Verlag, 1984-1988,
c    ISBN: 978-3764318840,
c    LC: QA297.M325.  
c
c  Parameters:
c
c    Input, double precision X, the evaluation point.
c    -1 <= X <= +1.
c
c    Input, double precision COEF(NC), the Chebyshev series.
c
c    Input, integer NC, the number of terms in the series.
c    0 < NC.
c
c    Output, double precision Y0, Y1, Y2, Y3, Y4, the value of the 
c    Chebyshev series and its first four derivatives at X.
c
      implicit none

      integer nc

      double precision b0
      double precision b1
      double precision b2
      double precision c0
      double precision c1
      double precision c2
      double precision coef(nc)
      double precision d0
      double precision d1
      double precision d2
      double precision e0
      double precision e1
      double precision e2
      double precision f0
      double precision f1
      double precision f2
      integer i
      double precision x
      double precision y0
      double precision y1
      double precision y2
      double precision y3
      double precision y4

      b0 = coef(nc)
      b1 = 0.0D+00
      b2 = 0.0D+00
      c0 = coef(nc)
      c1 = 0.0D+00
      c2 = 0.0D+00
      d0 = coef(nc)
      d1 = 0.0D+00
      d2 = 0.0D+00
      e0 = coef(nc)
      e1 = 0.0D+00
      e2 = 0.0D+00
      f0 = coef(nc)
      f1 = 0.0D+00
      f2 = 0.0D+00

      do i = nc - 1, 1, -1

        b2 = b1
       b1 = b0
        b0 = coef(i) - b2 + 2.0D+00 * x * b1

        if ( 1 .lt. i ) then
          c2 = c1
          c1 = c0
          c0 = b0 - c2 + 2.0D+00 * x * c1
        end if

        if ( 2 .lt. i ) then
          d2 = d1
          d1 = d0
          d0 = c0 - d2 + 2.0D+00 * x * d1
        end if

        if ( 3 .lt. i ) then
          e2 = e1
          e1 = e0
          e0 = d0 - e2 + 2.0D+00 * x * e1
        end if

        if ( 4 .lt. i ) then
          f2 = f1
          f1 = f0
          f0 = e0 - f2 + 2.0D+00 * x * f1
        end if

      end do

      y0 = ( b0 - b2 )            / 2.0D+00
      y1 =   c0 - c2
      y2 = ( d0 - d2 ) *  2.0D+00 * 2.0D+00
      y3 = ( e0 - e2 ) *  6.0D+00 * 4.0D+00
      y4 = ( f0 - f2 ) * 24.0D+00 * 8.0D+00

      return
      end
      subroutine evenchebser0 ( x, coef, nc, y0 )

c*********************************************************************72
c
cc EVENCHEBSER0 evaluates an even Chebyshev series.
c
c  Discussion:
c
c    This function implements Clenshaw's modification of his
c    algorithm for even series.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    21 April 2014
c
c  Author:
c
c    Manfred Zimmer
c
c  Reference:
c
c    Charles Clenshaw,
c    Mathematical Tables, Volume 5,
c    Chebyshev series for mathematical functions,
c    London, 1962.
c
c    Gerhard Maess,
c    Vorlesungen ueber Numerische Mathematik II, Analysis,
c    Berlin, Akademie_Verlag, 1984-1988,
c    ISBN: 978-3764318840,
c    LC: QA297.M325.  
c
c  Parameters:
c
c    Input, double precision X, the evaluation point.
c    -1 <= X <= +1.
c
c    Input, double precision COEF(NC), the Chebyshev series.
c
c    Input, integer NC, the number of terms in the series.
c    0 .lt. NC.
c
c    Output, double precision Y0, the value of the Chebyshev series at X.
c
      implicit none

      integer nc

      double precision b0
      double precision b1
      double precision b2
      double precision coef(nc)
      integer i
      double precision x
      double precision x2
      double precision y0

      b0 = coef(nc)
      b1 = 0.0D+00
      b2 = 0.0D+00

      x2 = 4.0D+00 * x * x - 2.0D+00

      do i = nc - 1, 1, -1

        b2 = b1
        b1 = b0
        b0 = coef(i) - b2 + x2 * b1

      end do

      y0 = 0.5D+00 * ( b0 - b2 )

      return
      end
      subroutine evenchebser1 ( x, coef, nc, y0, y1 )

c*********************************************************************72
c
cc EVENCHEBSER1 evaluates an even Chebyshev series and first derivative.
c
c  Discussion:
c
c    This function implements a modification and extension of 
c    Maess's algorithm.  Table 6.5.1 on page 164 of the reference 
c    gives an example for treating the first derivative.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    21 April 2014
c
c  Author:
c
c    Manfred Zimmer
c
c  Reference:
c
c    Charles Clenshaw,
c    Mathematical Tables, Volume 5,
c    Chebyshev series for mathematical functions,
c    London, 1962.
c
c    Gerhard Maess,
c    Vorlesungen ueber Numerische Mathematik II, Analysis,
c    Berlin, Akademie_Verlag, 1984-1988,
c    ISBN: 978-3764318840,
c    LC: QA297.M325.  
c
c  Parameters:
c
c    Input, double precision X, the evaluation point.
c    -1 <= X <= +1.
c
c    Input, double precision COEF(NC), the Chebyshev series.
c
c    Input, integer NC, the number of terms in the series.
c    0 .lt. NC.
c
c    Output, double precision Y0, the value of the Chebyshev series at X.
c
c    Output, double precision Y1, the value of the 1st derivative of the
c    Chebyshev series at X.
c
      implicit none

      integer nc

      double precision b0
      double precision b1
      double precision b2
      double precision c0
      double precision c1
      double precision c2
      double precision coef(nc)
      integer i
      double precision x
      double precision x2
      double precision y0
      double precision y1

      b0 = coef(nc)
      b1 = 0.0D+00
      b2 = 0.0D+00
      c0 = coef(nc)
      c1 = 0.0D+00
      c2 = 0.0D+00

      x2 = 4.0D+00 * x * x - 2.0D+00

      do i = nc - 1, 1, -1

        b2 = b1
        b1 = b0
        b0 = coef(i) - b2 + x2 * b1

        if ( 1 .lt. i ) then
          c2 = c1
          c1 = c0
          c0 = b0 - c2 + x2 * c1
        end if

      end do

      y0 = 0.5D+00 * ( b0 - b2 )
      y1 = ( c0 - c2 ) * 4.0D+00 * x

      return
      end
      subroutine evenchebser2 ( x, coef, nc, y0, y1, y2 )

c*********************************************************************72
c
cc EVENCHEBSER2 evaluates an even Chebyshev series and first two derivatives.
c
c  Discussion:
c
c    This function implements a modification and extension of
c    Maess's algorithm.  Table 6.5.1 on page 164 of the reference
c    gives an example for treating the first derivative.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 April 2014
c
c  Author:
c
c    Manfred Zimmer
c
c  Reference:
c
c    Charles Clenshaw,
c    Mathematical Tables, Volume 5,
c    Chebyshev series for mathematical functions,
c    London, 1962.
c
c    Gerhard Maess,
c    Vorlesungen ueber Numerische Mathematik II, Analysis,
c    Berlin, Akademie_Verlag, 1984-1988,
c    ISBN: 978-3764318840,
c    LC: QA297.M325.  
c
c  Parameters:
c
c    Input, double precision X, the evaluation point.
c    -1 <= X <= +1.
c
c    Input, double precision COEF(NC), the Chebyshev series.
c
c    Input, integer NC, the number of terms in the series.
c    0 .lt. NC.
c
c    Output, double precision Y0, the value of the Chebyshev series at X.
c
c    Output, double precision Y1, the value of the 1st derivative of the
c    Chebyshev series at X.
c
c    Output, double precision Y2, the value of the 2nd derivative of the
c    Chebyshev series at X.
c
      implicit none

      integer nc

      double precision b0
      double precision b1
      double precision b2
      double precision c0
      double precision c1
      double precision c2
      double precision coef(nc)
      double precision d0
      double precision d1
      double precision d2
      integer i
      double precision x
      double precision x2
      double precision y0
      double precision y1
      double precision y2

      b0 = coef(nc)
      b1 = 0.0D+00
      b2 = 0.0D+00
      c0 = coef(nc)
      c1 = 0.0D+00
      c2 = 0.0D+00
      d0 = coef(nc)
      d1 = 0.0D+00
      d2 = 0.0D+00

      x2 = 4.0D+00 * x * x - 2.0D+00

      do i = nc - 1, 1, -1

        b2 = b1
        b1 = b0
        b0 = coef(i) - b2 + x2 * b1

        if ( 1 .lt. i ) then
          c2 = c1
          c1 = c0
          c0 = b0 - c2 + x2 * c1
        end if

        if ( 2 .lt. i ) then
          d2 = d1
          d1 = d0
          d0 = c0 - d2 + x2 * d1
        end if

      end do

      y0 = 0.5D+00 * ( b0 - b2 )
      y1 = ( c0 - c2 ) * 4.0D+00 * x
      y2 = ( d0 - d2 ) * 64.0D+00 * x * x + ( c0 - c2 ) * 4.0D+00

      return
      end
      subroutine oddchebser0 ( x, coef, nc, y0 )

c*********************************************************************72
c
cc ODDCHEBSER0 evaluates an odd Chebyshev series.
c
c  Discussion:
c
c    This function implements Clenshaw's modification of  his algorithm
c    for odd series.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    21 April 2014
c
c  Author:
c
c    Manfred Zimmer
c
c  Reference:
c
c    Charles Clenshaw,
c    Mathematical Tables, Volume 5,
c    Chebyshev series for mathematical functions,
c    London, 1962.
c
c  Parameters:
c
c    Input, double precision X, the evaluation point.
c    -1 <= X <= +1.
c
c    Input, double precision COEF(NC), the Chebyshev series.
c
c    Input, integer NC, the number of terms in the series.
c    0 .lt. NC.
c
c    Output, double precision Y0, the value of the Chebyshev series at X.
c
      implicit none

      integer nc

      double precision b0
      double precision b1
      double precision b2
      double precision coef(nc)
      integer i
      double precision x
      double precision x2
      double precision y0

      b0 = coef(nc)
      b1 = 0.0D+00
      b2 = 0.0D+00

      x2 = 4.0D+00 * x * x - 2.0D+00

      do i = nc - 1, 1, -1

        b2 = b1
        b1 = b0
        b0 = coef(i) - b2 + x2 * b1

      end do

      y0 = ( b0 - b1 ) * x

      return
      end
      subroutine oddchebser1 ( x, coef, nc, y0, y1 )

c*********************************************************************72
c
cc ODDCHEBSER1 evaluates an odd Chebyshev series and the first derivative.
c
c  Discussion:
c
c    This function implements a modification and extension of
c    Clenshaw's algorithm. 
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 April 2014
c
c  Author:
c
c    Manfred Zimmer
c
c  Reference:
c
c    Charles Clenshaw,
c    Mathematical Tables, Volume 5,
c    Chebyshev series for mathematical functions,
c    London, 1962.
c
c    Gerhard Maess,
c    Vorlesungen ueber Numerische Mathematik II, Analysis,
c    Berlin, Akademie_Verlag, 1984-1988,
c    ISBN: 978-3764318840,
c    LC: QA297.M325.  
c
c  Parameters:
c
c    Input, double precision X, the evaluation point.
c    -1 <= X <= +1.
c
c    Input, double precision COEF(NC), the Chebyshev series.
c
c    Input, integer NC, the number of terms in the series.
c    0 .lt. NC.
c
c    Output, double precision Y0, the value of the Chebyshev series at X.
c
c    Output, double precision Y1, the value of the 1st derivative of the
c    Chebyshev series at X.
c
      implicit none

      integer nc

      double precision b0
      double precision b1
      double precision b2
      double precision c0
      double precision c1
      double precision c2
      double precision coef(nc)
      double precision coefi
      integer i
      double precision x
      double precision x2
      double precision y0
      double precision y1

      coefi = 2.0D+00 * coef(nc)
      b0 = coefi
      b1 = 0.0D+00
      b2 = 0.0D+00
      c0 = coefi
      c1 = 0.0D+00
      c2 = 0.0D+00

      x2 = 4.0D+00 * x * x - 2.0D+00

      do i = nc - 1, 1, -1

        b2 = b1
        b1 = b0
        coefi = 2.0D+00 * coef(i) - coefi
        b0 = coefi - b2 + x2 * b1

        if ( 1 .lt. i ) then
          c2 = c1
          c1 = c0
          c0 = b0 - c2 + x2 * c1
        end if

      end do

      y0 = ( b0 - b2 ) * 0.5D+00 * x
      y1 = ( c0 - c2 ) * 4.0D+00 * x * x + ( b0 - b2 ) * 0.5D+00

      return
      end
      subroutine oddchebser2 ( x, coef, nc, y0, y1, y2 )

c*********************************************************************72
c
cc ODDCHEBSER2 evaluates an odd Chebyshev series and first two derivatives.
c
c  Discussion:
c
c    This function implements a modification and extension of
c    Clenshaw's algorithm.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 April 2014
c
c  Author:
c
c    Manfred Zimmer
c
c  Reference:
c
c    Charles Clenshaw,
c    Mathematical Tables, Volume 5,
c    Chebyshev series for mathematical functions,
c    London, 1962.
c
c    Gerhard Maess,
c    Vorlesungen ueber Numerische Mathematik II, Analysis,
c    Berlin, Akademie_Verlag, 1984-1988,
c    ISBN: 978-3764318840,
c    LC: QA297.M325.  
c
c  Parameters:
c
c    Input, double precision X, the evaluation point.
c    -1 <= X <= +1.
c
c    Input, double precision COEF(NC), the Chebyshev series.
c
c    Input, integer NC, the number of terms in the series.
c    0 .lt. NC.
c
c    Output, double precision Y0, the value of the Chebyshev series at X.
c
c    Output, double precision Y1, the value of the 1st derivative of the
c    Chebyshev series at X.
c
c    Output, double precision Y2, the value of the 2nd derivative of the
c    Chebyshev series at X.
c
      implicit none

      integer nc

      double precision b0
      double precision b1
      double precision b2
      double precision c0
      double precision c1
      double precision c2
      double precision coef(nc)
      double precision coefi
      double precision d0
      double precision d1
      double precision d2
      integer i
      double precision x
      double precision x2
      double precision y0
      double precision y1
      double precision y2

      coefi = 2.0D+00 * coef(nc)
      b0 = coefi
      b1 = 0.0D+00
      b2 = 0.0D+00
      c0 = coefi
      c1 = 0.0D+00
      c2 = 0.0D+00
      d0 = coefi
      d1 = 0.0D+00
      d2 = 0.0D+00

      x2 = 4.0D+00 * x * x - 2.0D+00

      do i = nc - 1, 1, -1

        b2 = b1
        b1 = b0
        coefi = 2.0D+00 * coef(i) - coefi
        b0 = coefi - b2 + x2 * b1

        if ( 1 .lt. i ) then
          c2 = c1
          c1 = c0
          c0 = b0 - c2 + x2 * c1
        end if

        if ( 2 .lt. i ) then
          d2 = d1
          d1 = d0
          d0 = c0 - d2 + x2 * d1
        end if

      end do

      x2 = x * x

      y0 = ( b0 - b2 ) * 0.5D+00 * x
      y1 = ( c0 - c2 ) * 4.0D+00 * x2 + ( b0 - b2 ) * 0.5D+00
      y2 = ( ( d0 - d2 ) * 64.0D+00 * x2 + ( c0 - c2 ) * 12.0D+00 ) * x

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
