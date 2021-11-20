      subroutine cce_order ( l, n )

c*********************************************************************72
c
cc CCE_ORDER: order of a Clenshaw Curtis Exponential rule from the level.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 June 2012
c
c  Author:
c
c    John Burkardt.
c
c  Parameters:
c
c    Input, integer L, the level of the rule.  
c    1 .le. L.
c
c    Output, integer N, the order of the rule.
c
      implicit none

      integer l
      integer n

      if ( l .lt. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CCE_ORDER - Fatal error!'
        write ( *, '(a)' ) '  1 .le. L required.'
        write ( *, '(a,i4)' ) '  Input L = ', l
        stop 1
      else if ( l .eq. 1 ) then
        n = 1
      else
        n = 2 ** ( l - 1 ) + 1
      end if

      return
      end
      subroutine ccl_order ( l, n )

c*********************************************************************72
c
cc CCL_ORDER: order of a Clenshaw Curtis Linear (CCL) quadrature rule.
c
c  Discussion:
c
c    Our convention is that the abscissas are numbered from left to right.
c
c    The rule is defined on [0,1].
c
c    The integral to approximate:
c
c      Integral ( 0 .le. X .le. 1 ) F(X) dX
c
c    The quadrature rule:
c
c      Sum ( 1 .le. I .le. N ) W(I) * F ( X(I) )
c
c     L  2*L-1   N
c    --  -----  --
c     1      1   1
c     2      3   3
c     3      5   5
c     4      7   7
c     5      9   9
c     6     11  11
c     7     13  13
c     8     15  15
c     9     17  17
c    10     19  19
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    19 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer L, the level of the rule.
c    1 .le. L.
c
c    Output, integer N, the appropriate order.
c
      implicit none

      integer l
      integer n

      if ( l .lt. 1 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'CCL_ORDER - Fatal error!'
        write ( *, '(a,i4)' ) '  Illegal value of L = ', l
        stop 1
      end if

      n = 2 * l - 1

      return
      end
      subroutine ccs_order ( l, n )

c*********************************************************************72
c
cc CCS_ORDER: order of a "slow growth" Clenshaw Curtis quadrature rule.
c
c  Discussion:
c
c    Our convention is that the abscissas are numbered from left to right.
c
c    The rule is defined on [0,1].
c
c    The integral to approximate:
c
c      Integral ( 0 .le. X .le. 1 ) F(X) dX
c
c    The quadrature rule:
c
c      Sum ( 1 .le. I .le. N ) W(I) * F ( X(I) )
c
c    The input value L requests a rule of precision at least 2*L-1.
c
c    In order to preserve nestedness, this function returns the order
c    of a rule which is the smallest value of the form 1+2^E which
c    is greater than or equal to 2*L-1.
c
c     L  2*L-1   N
c    --  -----  --
c     1      1   1
c     2      3   3
c     3      5   5
c     4      7   9
c     5      9   9
c     6     11  17
c     7     13  17
c     8     15  17
c     9     17  17
c    10     19  33
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 May 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer L, the level of the rule.
c    1 .le. L.
c
c    Output, integer N, the appropriate order.
c
      implicit none

      integer l
      integer n

      if ( l .lt. 1 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'CCS_ORDER - Fatal error!'
        write ( *, '(a,i4)' ) '  Illegal value of L = ', l
        stop 1
      end if
c
c  Find the order N that satisfies the precision requirement.
c
      if ( l .eq. 1 ) then
        n = 1
      else
        n = 3
10      continue
        if ( n .lt. 2 * l - 1 ) then
          n = 2 * n - 1
          go to 10
        end if
      end if

      return
      end
      subroutine cc ( n, x, w )

c*********************************************************************72
c
cc CC computes a Clenshaw Curtis quadrature rule based on order.
c
c  Discussion:
c
c    Our convention is that the abscissas are numbered from left to right.
c
c    The rule is defined on [0,1].
c
c    The integral to approximate:
c
c      Integral ( 0 .le. X .le. 1 ) F(X) dX
c
c    The quadrature rule:
c
c      Sum ( 1 .le. I .le. N ) W(I) * F ( X(I) )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    10 May 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the order of the rule.
c    1 .le. N.
c
c    Output, double precision X(N), the abscissas.
c
c    Output, double precision W(N), the weights.
c
      implicit none

      integer n

      double precision a
      double precision b
      double precision c
      double precision d
      double precision e
      integer i
      integer j
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision theta
      double precision w(n)
      double precision x(n)

      if ( n .lt. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CC - Fatal error!'
        write ( *, '(a,i8)' ) '  Illegal value of N = ', n
        stop 1
      end if

      if ( n .eq. 1 ) then

        x(1) = 0.0D+00
        w(1) = 2.0D+00

      else

        do i = 1, n
          x(i) = cos ( dble ( n - i ) * pi 
     &               / dble ( n - 1 ) )
        end do

        x(1) = -1.0D+00
        if ( mod ( n, 2 ) .eq. 1 ) then
          x((n+1)/2) = 0.0D+00
        end if
        x(n) = +1.0D+00

        do i = 1, n

          theta = dble ( i - 1 ) * pi 
     &          / dble ( n - 1 )

          w(i) = 1.0D+00

          do j = 1, ( n - 1 ) / 2

            if ( 2 * j .eq. ( n - 1 ) ) then
              e = 1.0D+00
            else
              e = 2.0D+00
            end if

            w(i) = w(i) - e * cos ( 2.0D+00 * dble ( j ) * theta ) 
     &           / dble ( 4 * j * j - 1 )

          end do

        end do

        w(1)     =         w(1) / dble ( n - 1 )
        do i = 2, n - 1
          w(i) = 2.0D+00 * w(i) / dble ( n - 1 )
        end do
        w(n)     =         w(n) / dble ( n - 1 )

      end if
c
c  Transform from [-1,+1] to [0,1].
c
      a = -1.0D+00
      b = +1.0D+00
      c =  0.0D+00
      d = +1.0D+00
      call rule_adjust ( a, b, c, d, n, x, w )

      return
      end
      function fn_integral ( d )

c*********************************************************************72
c
cc FN_INTEGRAL is the integral of the Hermite test function.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 May 2012
c
c  Author:
c
c    John Burkardt.
c
c  Parameters:
c
c    Input, integer D, the spatial dimension.
c
c    Output, double precision FN_INTEGRAL, the integral value.
c
      implicit none

      integer d
      integer exponent
      parameter ( exponent = 6 )
      double precision fn_integral
      integer i4_factorial2

      fn_integral = dble ( i4_factorial2 ( exponent - 1 ) )

      return
      end
      subroutine fn_value ( d, n, x, fx )

c*********************************************************************72
c
cc FN_VALUE is a Hermite test function.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 May 2012
c
c  Author:
c
c    John Burkardt.
c
c  Parameters:
c
c    Input, integer D, the spatial dimension.
c
c    Input, integer N, the number of points.
c
c    Input, double precision X(D,N), the points.
c
c    Output, double precision FX(N), the function values.
c
      implicit none

      integer d
      integer n

      integer exponent
      parameter ( exponent = 6 )
      double precision fx(n)
      integer i
      double precision x(d,n)

      do i = 1, n
        fx(i) = x(1,i) ** exponent
      end do

      return
      end
      function fu_integral ( d )

c*********************************************************************72
c
cc FU_INTEGRAL is the integral of the test function for the [0,1]^D interval.
c
c  Discussion:
c
c    The same function, integrated over [-1,+1]^D, has an integral
c    which is 2^D times larger.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 May 2012
c
c  Author:
c
c    John Burkardt.
c
c  Parameters:
c
c    Input, integer D, the spatial dimension.
c
c    Output, double precision FU_INTEGRAL, the integral value.
c
      implicit none

      integer d
      double precision fu_integral

      fu_integral = ( 0.5D+00 * erf ( 0.5D+00 / sqrt ( 2.0D+00 ) ) ) 
     &  ** d

      return
      end
      subroutine fu_value ( d, n, x, fx )

c*********************************************************************72
c
cc FU_VALUE is a sample function for the [0,1]^D interval.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 May 2012
c
c  Author:
c
c    John Burkardt.
c
c  Parameters:
c
c    Input, integer D, the spatial dimension.
c
c    Input, integer N, the number of points.
c
c    Input, double precision X(D,N), the points.
c
c    Output, double precision FX(N), the function values.
c
      implicit none

      integer d
      integer n

      double precision fx(n)
      integer i
      integer j
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision x(d,n)

      do j = 1, n

        fx(j) = 1.0D+00

        do i = 1, d
          fx(j) = fx(j) * exp ( - ( x(i,j) / 2.0D+00 )**2 / 2.0D+00 ) 
     &      / 2.0D+00 / sqrt ( 2.0D+00 * pi )
        end do

      end do

      return
      end
      subroutine get_seq ( d, norm, seq_num, fs )

c*********************************************************************72
c
cc GET_SEQ generates all positive integer D-vectors that sum to NORM.
c
c  Discussion:
c
c    This function computes a list, in reverse dictionary order, of
c    all D-vectors of positive values that sum to NORM.
c
c    For example, call get_seq ( 3, 5, 6, fs ) returns
c
c      3  1  1
c      2  2  1
c      2  1  2
c      1  3  1
c      1  2  2
c      1  1  3
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 May 2010
c
c  Author:
c
c    Original MATLAB version by Florian Heiss, Viktor Winschel.
c    FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Florian Heiss, Viktor Winschel,
c    Likelihood approximation by numerical integration on sparse grids,
c    Journal of Econometrics,
c    Volume 144, 2008, pages 62-80.
c
c  Parameters:
c
c    Input, integer D, the dimension.
c    1 .le. D.
c
c    Input, integer NORM, the value that each row must sum to.
c    D .le. NORM.
c
c    Input, integer SEQ_NUM, the number of rows of FS.
c
c    Output, integer FS(SEQ_NUM,D).  Each row of FS represents 
c    one vector with all elements positive and summing to NORM.
c
      implicit none

      integer d
      integer seq_num

      integer a
      integer c
      integer fs(seq_num,d)
      integer i
      integer norm
      integer row
      integer seq(d)

      if ( norm .lt. d ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'GET_SEQ - Fatal error!'
        write ( *, '(a,i4,a,i4)' ) '  NORM = ', norm, ' .lt. D = ', d
        stop 1
      end if

      do i = 1, d
        seq(i) = 0
      end do
c
c  The algorithm is written to work with vectors whose minimum value is
c  allowed to be zero.  So we subtract D from NORM at the beginning and
c  then increment the result vectors by 1 at the end!
c
      a = norm - d
      seq(1) = a

      row = 1
      do i = 1, d
        fs(row,i) = seq(i) + 1
      end do
      c = 1

10    continue

      if ( seq ( d ) .lt. a ) then

        if ( c .eq. d ) then
          do i = c - 1, 1, -1
            c = i
            if ( seq(i) .ne. 0 ) then
              go to 20
            end if
          end do
20        continue
        end if

        seq(c) = seq(c) - 1
        c = c + 1
        seq(c) = a
        do i = 1, c - 1
          seq(c) = seq(c) - seq(i)
        end do

        if ( c .lt. d ) then
          do i = c + 1, d
            seq(i) = 0
          end do
        end if

        row = row + 1
        do i = 1, d
          fs(row,i) = seq(i) + 1
        end do

        go to 10

      end if

      return
      end
      subroutine gqn ( n, x, w )

c*********************************************************************72
c
cc GQN provides data for Gauss quadrature with a normal weight.
c
c  Discussion:
c
c    This data assumes integration over the interval (-oo,+oo) with 
c    weight function w(x) = exp(-x*x/2)/sqrt(2*pi).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 January 2013
c
c  Author:
c
c    Original MATLAB version by Florian Heiss, Viktor Winschel.
c    FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Florian Heiss, Viktor Winschel,
c    Likelihood approximation by numerical integration on sparse grids,
c    Journal of Econometrics,
c    Volume 144, 2008, pages 62-80.
c
c  Parameters:
c
c    Input, integer N, the number of points and weights.
c    1 .le. N .le. 25.
c
c    Output, double precision X(N), the nodes.
c
c    Output, double precision W(N), the weights.
c
      implicit none

      integer n

      integer nhalf
      double precision w(n)
      double precision wh(( n + 1 ) / 2)
      double precision x(n)
      double precision xh(( n + 1 ) / 2)
      double precision x01(1)
      double precision w01(1)
      double precision x02(1)
      double precision w02(1)
      double precision x03(2)
      double precision w03(2)
      double precision x04(2)
      double precision w04(2)
      double precision x05(3)
      double precision w05(3)
      double precision x06(3)
      double precision w06(3)
      double precision x07(4)
      double precision w07(4)
      double precision x08(4)
      double precision w08(4)
      double precision x09(5)
      double precision w09(5)
      double precision x10(5)
      double precision w10(5)
      double precision x11(6)
      double precision w11(6)
      double precision x12(6)
      double precision w12(6)
      double precision x13(7)
      double precision w13(7)
      double precision x14(7)
      double precision w14(7)
      double precision x15(8)
      double precision w15(8)
      double precision x16(8)
      double precision w16(8)
      double precision x17(9)
      double precision w17(9)
      double precision x18(9)
      double precision w18(9)
      double precision x19(10)
      double precision w19(10)
      double precision x20(10)
      double precision w20(10)
      double precision x21(11)
      double precision w21(11)
      double precision x22(11)
      double precision w22(11)
      double precision x23(12)
      double precision w23(12)
      double precision x24(12)
      double precision w24(12)
      double precision x25(13)
      double precision w25(13)

      data x01 /
     &   0.0000000000000000D+00 /
      data w01 /
     &  1.0000000000000000D+00 /
      data x02 /
     &  1.0000000000000002D+00 /
      data w02 /
     &  5.0000000000000000D-01 /
      data x03 /
     &  0.0000000000000000D+00, 1.7320508075688772D+00 /
      data w03 /
     &  6.6666666666666663D-01, 1.6666666666666674D-01 /
      data x04 /
     &  7.4196378430272591D-01, 2.3344142183389773D+00 /
      data w04 /
     &  4.5412414523193145D-01, 4.5875854768068498D-02 /
      data x05 /
     &  0.0000000000000000D+00, 1.3556261799742659D+00, 
     &  2.8569700138728056D+00 /
      data w05 /
     &  5.3333333333333344D-01, 2.2207592200561263D-01, 
     &  1.1257411327720691D-02 /
      data x06 /
     &  6.1670659019259422D-01, 1.8891758777537109D+00, 
     &  3.3242574335521193D+00 /
      data w06 /
     &  4.0882846955602919D-01, 8.8615746041914523D-02, 
     &  2.5557844020562431D-03 /
      data x07 /
     &  0.0000000000000000D+00, 1.1544053947399682D+00, 
     &  2.3667594107345415D+00, 3.7504397177257425D+00 /
      data w07 /
     &  4.5714285714285757D-01, 2.4012317860501250D-01, 
     &  3.0757123967586491D-02, 5.4826885597221875D-04 /
      data x08 /
     &  5.3907981135137517D-01, 1.6365190424351082D+00, 
     &  2.8024858612875416D+00, 4.1445471861258945D+00 /
      data w08 /
     &  3.7301225767907736D-01, 1.1723990766175897D-01,
     &  9.6352201207882630D-03, 1.1261453837536784D-04 /
      data x09 /
     &  0.0000000000000000D+00, 1.0232556637891326D+00,
     &  2.0768479786778302D+00, 3.2054290028564703D+00, 
     &  4.5127458633997826D+00 /
      data w09 /
     &  4.0634920634920685D-01, 2.4409750289493909D-01,
     &  4.9916406765217969D-02, 2.7891413212317675D-03, 
     &  2.2345844007746563D-05 /
      data x10 /
     &  4.8493570751549764D-01, 1.4659890943911582D+00, 
     &  2.4843258416389546D+00, 3.5818234835519269D+00, 
     &  4.8594628283323127D+00 /
      data w10 /
     &  3.4464233493201940D-01, 1.3548370298026730D-01, 
     &  1.9111580500770317D-02, 7.5807093431221972D-04, 
     &  4.3106526307183106D-06 /
      data x11 /
     &  0.0000000000000000D+00, 9.2886899738106388D-01, 
     &  1.8760350201548459D+00, 2.8651231606436447D+00, 
     &  3.9361666071299775D+00, 5.1880012243748714D+00 /
      data w11 /
     &  3.6940836940836957D-01, 2.4224029987397003D-01,
     &  6.6138746071057644D-02, 6.7202852355372697D-03,
     &  1.9567193027122324D-04, 8.1218497902149036D-07 /
      data x12 /
     &  4.4440300194413901D-01, 1.3403751971516167D+00, 
     &  2.2594644510007993D+00, 3.2237098287700974D+00, 
     &  4.2718258479322815D+00, 5.5009017044677480D+00 /
      data w12 /
     &  3.2166436151283007D-01, 1.4696704804532995D-01, 
     &  2.9116687912364138D-02, 2.2033806875331849D-03, 
     &  4.8371849225906076D-05, 1.4999271676371597D-07 /
      data x13 /
     &  0.0000000000000000D+00, 8.5667949351945005D-01, 
     &  1.7254183795882394D+00, 2.6206899734322149D+00, 
     &  3.5634443802816347D+00, 4.5913984489365207D+00, 
     &  5.8001672523865011D+00 /
      data w13 /
     &  3.4099234099234149D-01, 2.3787152296413588D-01, 
     &  7.9168955860450141D-02, 1.1770560505996543D-02, 
     &  6.8123635044292619D-04, 1.1526596527333885D-05, 
     &  2.7226276428059039D-08 /
      data x14 /
     &  4.1259045795460181D-01, 1.2426889554854643D+00, 
     &  2.0883447457019444D+00, 2.9630365798386675D+00, 
     &  3.8869245750597696D+00, 4.8969363973455646D+00, 
     &  6.0874095469012914D+00 /
      data w14 /
     &  3.0263462681301945D-01, 1.5408333984251366D-01, 
     &  3.8650108824253432D-02, 4.4289191069474066D-03, 
     &  2.0033955376074381D-04, 2.6609913440676334D-06, 
     &  4.8681612577483872D-09 /
      data x15 /
     &  0.0000000000000000D+00, 7.9912906832454811D-01, 
     &  1.6067100690287301D+00, 2.4324368270097581D+00, 
     &  3.2890824243987664D+00, 4.1962077112690155D+00, 
     &  5.1900935913047821D+00, 6.3639478888298378D+00 /
      data w15 /
     &  3.1825951825951820D-01, 2.3246229360973222D-01, 
     &  8.9417795399844444D-02, 1.7365774492137616D-02, 
     &  1.5673575035499571D-03, 5.6421464051890157D-05, 
     &  5.9754195979205961D-07, 8.5896498996331805D-10 /
      data x16 /
     &  3.8676060450055738D-01, 1.1638291005549648D+00, 
     &  1.9519803457163336D+00, 2.7602450476307019D+00, 
     &  3.6008736241715487D+00, 4.4929553025200120D+00, 
     &  5.4722257059493433D+00, 6.6308781983931295D+00 /
      data w16 /
     &  2.8656852123801241D-01, 1.5833837275094925D-01, 
     &  4.7284752354014067D-02, 7.2669376011847411D-03, 
     &  5.2598492657390979D-04, 1.5300032162487286D-05, 
     &  1.3094732162868203D-07, 1.4978147231618314D-10 /
      data x17 /
     &  0.0000000000000000D+00, 7.5184260070389630D-01, 
     &  1.5098833077967408D+00, 2.2810194402529889D+00, 
     &  3.0737971753281941D+00, 3.9000657171980104D+00, 
     &  4.7785315896299840D+00, 5.7444600786594071D+00, 
     &  6.8891224398953330D+00 /
      data w17 /
     &  2.9953837012660756D-01, 2.2670630846897877D-01,
     &  9.7406371162718081D-02, 2.3086657025711152D-02, 
     &  2.8589460622846499D-03, 1.6849143155133945D-04, 
     &  4.0126794479798725D-06, 2.8080161179305783D-08, 
     &  2.5843149193749151D-11 /
      data x18 /
     &  3.6524575550769767D-01, 1.0983955180915013D+00,
     &  1.8397799215086457D+00, 2.5958336889112403D+00, 
     &  3.3747365357780907D+00, 4.1880202316294044D+00, 
     &  5.0540726854427405D+00, 6.0077459113595975D+00, 
     &  7.1394648491464796D+00 /
      data w18 /
     &  2.7278323465428789D-01, 1.6068530389351263D-01,
     &  5.4896632480222654D-02, 1.0516517751941352D-02, 
     &  1.0654847962916496D-03, 5.1798961441161962D-05, 
     &  1.0215523976369816D-06, 5.9054884788365484D-09, 
     &  4.4165887693587078D-12 /
      data x19 /
     &  0.0000000000000000D+00, 7.1208504404237993D-01,
     &  1.4288766760783731D+00, 2.1555027613169351D+00, 
     &  2.8980512765157536D+00, 3.6644165474506383D+00, 
     &  4.4658726268310316D+00, 5.3205363773360386D+00, 
     &  6.2628911565132519D+00, 7.3825790240304316D+00 /
      data w19 /
     &  2.8377319275152108D-01, 2.2094171219914366D-01,
     &  1.0360365727614400D-01, 2.8666691030118496D-02, 
     &  4.5072354203420355D-03, 3.7850210941426759D-04, 
     &  1.5351145954666744D-05, 2.5322200320928681D-07, 
     &  1.2203708484474786D-09, 7.4828300540572308D-13 /
      data x20 /
     &  3.4696415708135592D-01, 1.0429453488027509D+00,
     &  1.7452473208141270D+00, 2.4586636111723679D+00,
     &  3.1890148165533900D+00, 3.9439673506573163D+00, 
     &  4.7345813340460552D+00, 5.5787388058932015D+00,
     &  6.5105901570136551D+00, 7.6190485416797591D+00 /
      data w20 /
     &  2.6079306344955544D-01, 1.6173933398400026D-01,
     &  6.1506372063976029D-02, 1.3997837447101043D-02,
     &  1.8301031310804918D-03, 1.2882627996192898D-04, 
     &  4.4021210902308646D-06, 6.1274902599829597D-08,
     &  2.4820623623151838D-10, 1.2578006724379305D-13 /
      data x21 /
     &  0.0000000000000000D+00, 6.7804569244064405D-01,
     &  1.3597658232112304D+00, 2.0491024682571628D+00,
     &  2.7505929810523733D+00, 3.4698466904753764D+00, 
     &  4.2143439816884216D+00, 4.9949639447820253D+00,
     &  5.8293820073044706D+00, 6.7514447187174609D+00,
     &  7.8493828951138225D+00 /
      data w21 /
     &  2.7026018357287707D-01, 2.1533371569505982D-01,
     &  1.0839228562641938D-01, 3.3952729786542839D-02,
     &  6.4396970514087768D-03, 7.0804779548153736D-04, 
     &  4.2192347425515866D-05, 1.2253548361482522D-06,
     &  1.4506612844930740D-08, 4.9753686041217464D-11,
     &  2.0989912195656652D-14 /
      data x22 /
     &  3.3117931571527381D-01, 9.9516242227121554D-01,
     &  1.6641248391179071D+00, 2.3417599962877080D+00,
     &  3.0324042278316763D+00, 3.7414963502665177D+00, 
     &  4.4763619773108685D+00, 5.2477244337144251D+00,
     &  6.0730749511228979D+00, 6.9859804240188152D+00,
     &  8.0740299840217116D+00 /
      data w22 /
     &  2.5024359658693501D-01, 1.6190629341367538D-01,
     &  6.7196311428889891D-02, 1.7569072880805774D-02,
     &  2.8087610475772107D-03, 2.6228330325596416D-04, 
     &  1.3345977126808712D-05, 3.3198537498140043D-07,
     &  3.3665141594582109D-09, 9.8413789823460105D-12,
     &  3.4794606478771428D-15 /
      data x23 /
     &  0.0000000000000000D+00, 6.4847115353449580D-01,
     &  1.2998764683039790D+00, 1.9573275529334242D+00,
     &  2.6243236340591820D+00, 3.3050400217529652D+00, 
     &  4.0047753217333044D+00, 4.7307241974514733D+00,
     &  5.4934739864717947D+00, 6.3103498544483996D+00,
     &  7.2146594350518622D+00, 8.2933860274173536D+00 /
      data w23 /
     &  2.5850974080883904D-01, 2.0995966957754261D-01,
     &  1.1207338260262091D-01, 3.8867183703480947D-02,
     &  8.5796783914656640D-03, 1.1676286374978613D-03, 
     &  9.3408186090312983D-05, 4.0899772449921549D-06,
     &  8.7750624838617161D-08, 7.6708888623999076D-10,
     &  1.9229353115677913D-12, 5.7323831678020873D-16 /
      data x24 /
     &  3.1737009662945231D-01, 9.5342192293210926D-01,
     &  1.5934804298164202D+00, 2.2404678516917524D+00,
     &  2.8977286432233140D+00, 3.5693067640735610D+00, 
     &  4.2603836050199053D+00, 4.9780413746391208D+00,
     &  5.7327471752512009D+00, 6.5416750050986341D+00,
     &  7.4378906660216630D+00, 8.5078035191952583D+00 /
      data w24 /
     &  2.4087011554664056D-01, 1.6145951286700025D-01,
     &  7.2069364017178436D-02, 2.1126344408967029D-02,
     &  3.9766089291813113D-03, 4.6471871877939763D-04, 
     &  3.2095005652745989D-05, 1.2176597454425830D-06,
     &  2.2674616734804651D-08, 1.7186649279648690D-10,
     &  3.7149741527624159D-13, 9.3901936890419202D-17 /
      data x25 /
     &  0.0000000000000000D+00, 6.2246227918607611D-01, 
     &  1.2473119756167892D+00, 1.8770583699478387D+00,
     &  2.5144733039522058D+00, 3.1627756793881927D+00, 
     &  3.8259005699724917D+00, 4.5089299229672850D+00,
     &  5.2188480936442794D+00, 5.9660146906067020D+00,
     &  6.7674649638097168D+00, 7.6560379553930762D+00, 
     &  8.7175976783995885D+00 /
      data w25 /
     &  2.4816935117648548D-01, 2.0485102565034041D-01, 
     &  1.1488092430395164D-01, 4.3379970167644971D-02,
     &  1.0856755991462316D-02, 1.7578504052637961D-03, 
     &  1.7776690692652660D-04, 1.0672194905202536D-05,
     &  3.5301525602454978D-07, 5.7380238688993763D-09,
     &  3.7911500004771871D-11, 7.1021030370039253D-14, 
     &  1.5300389979986825D-17 /

      nhalf = ( n + 1 ) / 2

      if ( n .eq. 1 ) then
        call r8vec_copy ( nhalf, x01, xh )
        call r8vec_copy ( nhalf, w01, wh )
      else if ( n .eq. 2 ) then
        call r8vec_copy ( nhalf, x02, xh )
        call r8vec_copy ( nhalf, w02, wh )
      else if ( n .eq. 3 ) then
        call r8vec_copy ( nhalf, x03, xh )
        call r8vec_copy ( nhalf, w03, wh )
      else if ( n .eq. 4 ) then
        call r8vec_copy ( nhalf, x04, xh )
        call r8vec_copy ( nhalf, w04, wh )
      else if ( n .eq. 5 ) then
        call r8vec_copy ( nhalf, x05, xh )
        call r8vec_copy ( nhalf, w05, wh )
      else if ( n .eq. 6 ) then
        call r8vec_copy ( nhalf, x06, xh )
        call r8vec_copy ( nhalf, w06, wh )
      else if ( n .eq. 7 ) then
        call r8vec_copy ( nhalf, x07, xh )
        call r8vec_copy ( nhalf, w07, wh )
      else if ( n .eq. 8 ) then
        call r8vec_copy ( nhalf, x08, xh )
        call r8vec_copy ( nhalf, w08, wh )
      else if ( n .eq. 9 ) then
        call r8vec_copy ( nhalf, x09, xh )
        call r8vec_copy ( nhalf, w09, wh )
      else if ( n .eq. 10 ) then
        call r8vec_copy ( nhalf, x10, xh )
        call r8vec_copy ( nhalf, w10, wh )
      else if ( n .eq. 11 ) then
        call r8vec_copy ( nhalf, x11, xh )
        call r8vec_copy ( nhalf, w11, wh )
      else if ( n .eq. 12 ) then
        call r8vec_copy ( nhalf, x12, xh )
        call r8vec_copy ( nhalf, w12, wh )
      else if ( n .eq. 13 ) then
        call r8vec_copy ( nhalf, x13, xh )
        call r8vec_copy ( nhalf, w13, wh )
      else if ( n .eq. 14 ) then
        call r8vec_copy ( nhalf, x14, xh )
        call r8vec_copy ( nhalf, w14, wh )
      else if ( n .eq. 15 ) then
        call r8vec_copy ( nhalf, x15, xh )
        call r8vec_copy ( nhalf, w15, wh )
      else if ( n .eq. 16 ) then
        call r8vec_copy ( nhalf, x16, xh )
        call r8vec_copy ( nhalf, w16, wh )
      else if ( n .eq. 17 ) then
        call r8vec_copy ( nhalf, x17, xh )
        call r8vec_copy ( nhalf, w17, wh )
      else if ( n .eq. 18 ) then
        call r8vec_copy ( nhalf, x18, xh )
        call r8vec_copy ( nhalf, w18, wh )
      else if ( n .eq. 19 ) then
        call r8vec_copy ( nhalf, x19, xh )
        call r8vec_copy ( nhalf, w19, wh )
      else if ( n .eq. 20 ) then
        call r8vec_copy ( nhalf, x20, xh )
        call r8vec_copy ( nhalf, w20, wh )
      else if ( n .eq. 21 ) then
        call r8vec_copy ( nhalf, x21, xh )
        call r8vec_copy ( nhalf, w21, wh )
      else if ( n .eq. 22 ) then
        call r8vec_copy ( nhalf, x22, xh )
        call r8vec_copy ( nhalf, w22, wh )
      else if ( n .eq. 23 ) then
        call r8vec_copy ( nhalf, x23, xh )
        call r8vec_copy ( nhalf, w23, wh )
      else if ( n .eq. 24 ) then
        call r8vec_copy ( nhalf, x24, xh )
        call r8vec_copy ( nhalf, w24, wh )
      else if ( n .eq. 25 ) then
        call r8vec_copy ( nhalf, x25, xh )
        call r8vec_copy ( nhalf, w25, wh )
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'GQN - Fatal error!'
        write ( *, '(a)' ) '  Value of N must be between 1 and 25.'
        stop 1
      end if

      if ( mod ( n, 2 ) .eq. 1 ) then
        x(1:nhalf-1) = - xh(nhalf:2:-1)
        x(nhalf:n) = xh(1:nhalf)
        w(1:nhalf-1) = wh(nhalf:2:-1)
        w(nhalf:n) = wh(1:nhalf)
      else
        x(1:nhalf) = - xh(nhalf:1:-1)
        x(nhalf+1:n) = xh(1:nhalf)
        w(1:nhalf) = wh(nhalf:1:-1)
        w(nhalf+1:n) = wh(1:nhalf)
      end if

      return
      end
      subroutine gqn_order ( l, n )

c*********************************************************************72
c
cc GQN_ORDER computes the order of a GQN rule from the level.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 June 2012
c
c  Author:
c
c    John Burkardt.
c
c  Parameters:
c
c    Input, integer L, the level of the rule.  
c    1 .le. L.
c
c    Output, integer N, the order of the rule.
c
      implicit none

      integer l
      integer n

      if ( l .lt. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'GQN_ORDER - Fatal error!'
        write ( *, '(a)' ) '  1 .le. L required.'
        write ( *, '(a,i4)' ) '  Input L = ', l
        stop 1
      else if ( l .le. 25 ) then
        n = l
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'GQN_ORDER - Fatal error!'
        write ( *, '(a)' ) '  L .le. 25 required.'
        write ( *, '(a,i4)' ) '  Input L = ', l
        stop 1
      end if

      return
      end
      subroutine gqn2_order ( l, n )

c*********************************************************************72
c
cc GQN2_ORDER computes the order of a GQN rule from the level.
c
c  Discussion:
c
c    For this version of the order routine, we have
c
c      n = 2 * l - 1
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 February 2014
c
c  Author:
c
c    John Burkardt.
c
c  Parameters:
c
c    Input, integer L, the level of the rule.  
c    1 .le. L.
c
c    Output, integer N, the order of the rule.
c
      implicit none

      integer l
      integer n

      if ( l .lt. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'GQN2_ORDER - Fatal error!'
        write ( *, '(a)' ) '  1 .le. L required.'
        write ( *, '(a,i4)' ) '  Input L = ', l
        stop 1
      else if ( l .le. 13 ) then
        n = 2 * l - 1
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'GQN2_ORDER - Fatal error!'
        write ( *, '(a)' ) '  L .le. 13 required.'
        write ( *, '(a,i4)' ) '  Input L = ', l
        stop 1
      end if

      return
      end
      subroutine gqu ( n, x, w )

c*********************************************************************72
c
cc GQU provides data for Gauss quadrature with a uniform weight.
c
c  Discussion:
c
c    This data assumes integration over the interval [0,1] with 
c    weight function w(x) = 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 January 2013
c
c  Author:
c
c    Original MATLAB version by Florian Heiss, Viktor Winschel.
c    FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Florian Heiss, Viktor Winschel,
c    Likelihood approximation by numerical integration on sparse grids,
c    Journal of Econometrics,
c    Volume 144, 2008, pages 62-80.
c
c  Parameters:
c
c    Input, integer N, the number of points and weights.
c    1 .le. N .le. 25.
c
c    Output, double precision X(N), the nodes.
c
c    Output, double precision W(N), the weights.
c
      implicit none

      integer n

      integer nhalf
      double precision w(n)
      double precision wh((n+1)/2)
      double precision x(n)
      double precision xh((n+1)/2)
      double precision x01(1)
      double precision w01(1)
      double precision x02(1)
      double precision w02(1)
      double precision x03(2)
      double precision w03(2)
      double precision x04(2)
      double precision w04(2)
      double precision x05(3)
      double precision w05(3)
      double precision x06(3)
      double precision w06(3)
      double precision x07(4)
      double precision w07(4)
      double precision x08(4)
      double precision w08(4)
      double precision x09(5)
      double precision w09(5)
      double precision x10(5)
      double precision w10(5)
      double precision x11(6)
      double precision w11(6)
      double precision x12(6)
      double precision w12(6)
      double precision x13(7)
      double precision w13(7)
      double precision x14(7)
      double precision w14(7)
      double precision x15(8)
      double precision w15(8)
      double precision x16(8)
      double precision w16(8)
      double precision x17(9)
      double precision w17(9)
      double precision x18(9)
      double precision w18(9)
      double precision x19(10)
      double precision w19(10)
      double precision x20(10)
      double precision w20(10)
      double precision x21(11)
      double precision w21(11)
      double precision x22(11)
      double precision w22(11)
      double precision x23(12)
      double precision w23(12)
      double precision x24(12)
      double precision w24(12)
      double precision x25(13)
      double precision w25(13)

      data x01 /
     &  5.0000000000000000D-01 /
      data w01 /
     &  1.0000000000000000D+00 /
      data x02 /
     &  7.8867513459481287D-01 /
      data w02 /
     &  5.0000000000000000D-01 /
      data x03 /
     &  5.0000000000000000D-01, 8.8729833462074170D-01 /
      data w03 /
     &  4.4444444444444570D-01, 2.7777777777777712D-01 /
      data x04 /
     &  6.6999052179242813D-01, 9.3056815579702623D-01 /
      data w04 /
     &  3.2607257743127516D-01, 1.7392742256872484D-01 /
      data x05 /
     &  5.0000000000000000D-01, 7.6923465505284150D-01, 
     &  9.5308992296933193D-01 /
      data w05 /
     &  2.8444444444444655D-01, 2.3931433524968501D-01, 
     &  1.1846344252809174D-01 /
      data x06 /
     &  6.1930959304159849D-01, 8.3060469323313235D-01, 
     &  9.6623475710157603D-01 /
      data w06 /
     &  2.3395696728634746D-01, 1.8038078652407072D-01, 
     &  8.5662246189581834D-02 /
      data x07 /
     &  5.0000000000000000D-01, 7.0292257568869854D-01, 
     &  8.7076559279969723D-01, 9.7455395617137919D-01 /
      data w07 /
     &  2.0897959183673620D-01, 1.9091502525256090D-01, 
     &  1.3985269574463935D-01, 6.4742483084431701D-02 /
      data x08 /
     &  5.9171732124782495D-01, 7.6276620495816450D-01, 
     &  8.9833323870681348D-01, 9.8014492824876809D-01 /
      data w08 /
     &  1.8134189168918213D-01, 1.5685332293894469D-01, 
     &  1.1119051722668793D-01, 5.0614268145185180D-02 /
      data x09 /
     &  5.0000000000000000D-01, 6.6212671170190451D-01, 
     &  8.0668571635029518D-01, 9.1801555366331788D-01,
     &  9.8408011975381304D-01 /
      data w09 /
     &  1.6511967750063075D-01, 1.5617353852000226D-01, 
     &  1.3030534820146844D-01, 9.0324080347429253D-02,
     &  4.0637194180784583D-02 /
      data x10 /
     &  5.7443716949081558D-01, 7.1669769706462361D-01, 
     &  8.3970478414951222D-01, 9.3253168334449232D-01,
     &  9.8695326425858587D-01 /
      data w10 /
     &  1.4776211235737713D-01, 1.3463335965499873D-01, 
     &  1.0954318125799158D-01, 7.4725674575290599D-02,
     &  3.3335672154342001D-02 /
      data x11 /
     &  5.0000000000000000D-01, 6.3477157797617245D-01, 
     &  7.5954806460340585D-01, 8.6507600278702468D-01,
     &  9.4353129988404771D-01, 9.8911432907302843D-01 /
      data w11 /
     &  1.3646254338895086D-01, 1.3140227225512388D-01, 
     &  1.1659688229599563D-01, 9.3145105463867520D-02,
     &  6.2790184732452625D-02, 2.7834283558084916D-02 /
      data x12 /
     &  5.6261670425573451D-01, 6.8391574949909006D-01, 
     &  7.9365897714330869D-01, 8.8495133709715235D-01,
     &  9.5205862818523745D-01, 9.9078031712335957D-01 /
      data w12 /
     &  1.2457352290670189D-01, 1.1674626826917781D-01, 
     &  1.0158371336153328D-01, 8.0039164271673444D-02,
     &  5.3469662997659276D-02, 2.3587668193254314D-02 /
      data x13 /
     &  5.0000000000000000D-01, 6.1522915797756739D-01, 
     &  7.2424637551822335D-01, 8.2117466972017006D-01,
     &  9.0078904536665494D-01, 9.5879919961148907D-01, 
     &  9.9209152735929407D-01 /
      data w13 /
     &  1.1627577661543741D-01, 1.1314159013144903D-01, 
     &  1.0390802376844462D-01, 8.9072990380973202D-02,
     &  6.9436755109893875D-02, 4.6060749918864378D-02, 
     &  2.0242002382656228D-02 /
      data x14 /
     &  5.5402747435367183D-01, 6.5955618446394482D-01, 
     &  7.5762431817907705D-01, 8.4364645240584268D-01,
     &  9.1360065753488251D-01, 9.6421744183178681D-01, 
     &  9.9314190434840621D-01 /
      data w14 /
     &  1.0763192673157916D-01, 1.0259923186064811D-01, 
     &  9.2769198738969161D-02, 7.8601583579096995D-02,
     &  6.0759285343951711D-02, 4.0079043579880291D-02, 
     &  1.7559730165874574D-02 /
      data x15 /
     &  5.0000000000000000D-01, 6.0059704699871730D-01, 
     &  6.9707567353878175D-01, 7.8548608630426942D-01,
     &  8.6220886568008503D-01, 9.2410329170521366D-01, 
     &  9.6863669620035298D-01, 9.9399625901024269D-01 /
      data w15 /
     &  1.0128912096278091D-01, 9.9215742663556039D-02, 
     &  9.3080500007781286D-02, 8.3134602908497196D-02,
     &  6.9785338963077315D-02, 5.3579610233586157D-02, 
     &  3.5183023744054159D-02, 1.5376620998057434D-02 /
      data x16 /
     &  5.4750625491881877D-01, 6.4080177538962946D-01, 
     &  7.2900838882861363D-01, 8.0893812220132189D-01,
     &  8.7770220417750155D-01, 9.3281560119391593D-01, 
     &  9.7228751153661630D-01, 9.9470046749582497D-01 /
      data w16 /
     &  9.4725305227534431D-02, 9.1301707522462000D-02, 
     &  8.4578259697501462D-02, 7.4797994408288562D-02,
     &  6.2314485627767105D-02, 4.7579255841246545D-02, 
     &  3.1126761969323954D-02, 1.3576229705875955D-02 /
      data x17 /
     &  5.0000000000000000D-01, 5.8924209074792389D-01, 
     &  6.7561588172693821D-01, 7.5634526854323847D-01,
     &  8.2883557960834531D-01, 8.9075700194840068D-01, 
     &  9.4011957686349290D-01, 9.7533776088438384D-01,
     &  9.9528773765720868D-01 /
      data w17 /
     &  8.9723235178103419D-02, 8.8281352683496447D-02, 
     &  8.4002051078225143D-02, 7.7022880538405308D-02,
     &  6.7568184234262890D-02, 5.5941923596702053D-02, 
     &  4.2518074158589644D-02, 2.7729764686993612D-02,
     &  1.2074151434273140D-02 /
      data x18 /
     &  5.4238750652086765D-01, 6.2594311284575277D-01, 
     &  7.0587558073142131D-01, 7.7988541553697377D-01,
     &  8.4584352153017661D-01, 9.0185247948626157D-01, 
     &  9.4630123324877791D-01, 9.7791197478569880D-01,
     &  9.9578258421046550D-01 /
      data w18 /
     &  8.4571191481571939D-02, 8.2138241872916504D-02, 
     &  7.7342337563132801D-02, 7.0321457335325452D-02,
     &  6.1277603355739306D-02, 5.0471022053143716D-02, 
     &  3.8212865127444665D-02, 2.4857274447484968D-02,
     &  1.0808006763240719D-02 /
      data x19 /
     &  5.0000000000000000D-01, 5.8017932282011264D-01, 
     &  6.5828204998181494D-01, 7.3228537068798050D-01,
     &  8.0027265233084055D-01, 8.6048308866761469D-01, 
     &  9.1135732826857141D-01, 9.5157795180740901D-01,
     &  9.8010407606741501D-01, 9.9620342192179212D-01 /
      data w19 /
     &  8.0527224924391946D-02, 7.9484421696977337D-02, 
     &  7.6383021032929960D-02, 7.1303351086803413D-02,
     &  6.4376981269668232D-02, 5.5783322773667113D-02, 
     &  4.5745010811225124D-02, 3.4522271368820669D-02,
     &  2.2407113382849821D-02, 9.7308941148624341D-03 /
      data x20 /
     &  5.3826326056674867D-01, 6.1389292557082253D-01, 
     &  6.8685304435770977D-01, 7.5543350097541362D-01,
     &  8.1802684036325757D-01, 8.7316595323007540D-01, 
     &  9.1955848591110945D-01, 9.5611721412566297D-01,
     &  9.8198596363895696D-01, 9.9656429959254744D-01 /
      data w20 /
     &  7.6376693565363113D-02, 7.4586493236301996D-02, 
     &  7.1048054659191187D-02, 6.5844319224588346D-02,
     &  5.9097265980759248D-02, 5.0965059908620318D-02, 
     &  4.1638370788352433D-02, 3.1336024167054569D-02,
     &  2.0300714900193556D-02, 8.8070035695753026D-03 /
      data x21 /
     &  5.0000000000000000D-01, 5.7278092708044759D-01, 
     &  6.4401065840120053D-01, 7.1217106010371944D-01,
     &  7.7580941794360991D-01, 8.3356940209870611D-01, 
     &  8.8421998173783889D-01, 9.2668168229165859D-01,
     &  9.6004966707520034D-01, 9.8361341928315316D-01,
     &  9.9687608531019478D-01 /
      data w21 /
     &  7.3040566824845346D-02, 7.2262201994985134D-02, 
     &  6.9943697395536658D-02, 6.6134469316668845D-02,
     &  6.0915708026864350D-02, 5.4398649583574356D-02, 
     &  4.6722211728016994D-02, 3.8050056814189707D-02,
     &  2.8567212713428641D-02, 1.8476894885426285D-02,
     &  8.0086141288864491D-03 /
      data x22 /
     &  5.3486963665986109D-01, 6.0393021334411068D-01, 
     &  6.7096791044604209D-01, 7.3467791899337853D-01,
     &  7.9382020175345580D-01, 8.4724363159334137D-01, 
     &  8.9390840298960406D-01, 9.3290628886015003D-01,
     &  9.6347838609358694D-01, 9.8503024891771429D-01,
     &  9.9714729274119962D-01 /
      data w22 /
     &  6.9625936427816129D-02, 6.8270749173007697D-02, 
     &  6.5586752393531317D-02, 6.1626188405256251D-02,
     &  5.6466148040269712D-02, 5.0207072221440600D-02, 
     &  4.2970803108533975D-02, 3.4898234212260300D-02,
     &  2.6146667576341692D-02, 1.6887450792407110D-02,
     &  7.3139976491353280D-03 /
      data x23 /
     &  5.0000000000000000D-01, 5.6662841214923310D-01, 
     &  6.3206784048517251D-01, 6.9515051901514546D-01,
     &  7.5475073892300371D-01, 8.0980493788182306D-01, 
     &  8.5933068156597514D-01, 9.0244420080942001D-01,
     &  9.3837617913522076D-01, 9.6648554341300807D-01,
     &  9.8627123560905761D-01, 9.9738466749877608D-01 /
      data w23 /
     &  6.6827286093053176D-02, 6.6231019702348404D-02, 
     &  6.4452861094041150D-02, 6.1524542153364815D-02,
     &  5.7498320111205814D-02, 5.2446045732270824D-02, 
     &  4.6457883030017563D-02, 3.9640705888359551D-02,
     &  3.2116210704262994D-02, 2.4018835865542369D-02,
     &  1.5494002928489686D-02, 6.7059297435702412D-03 /
      data x24 /
     &  5.3202844643130276D-01, 5.9555943373680820D-01, 
     &  6.5752133984808170D-01, 7.1689675381302254D-01,
     &  7.7271073569441984D-01, 8.2404682596848777D-01, 
     &  8.7006209578927718D-01, 9.1000099298695147D-01,
     &  9.4320776350220048D-01, 9.6913727600136634D-01,
     &  9.8736427798565474D-01, 9.9759360999851066D-01 /
      data w24 /
     &  6.3969097673376246D-02, 6.2918728173414318D-02, 
     &  6.0835236463901793D-02, 5.7752834026862883D-02,
     &  5.3722135057982914D-02, 4.8809326052057039D-02, 
     &  4.3095080765976693D-02, 3.6673240705540205D-02,
     &  2.9649292457718385D-02, 2.2138719408709880D-02,
     &  1.4265694314466934D-02, 6.1706148999928351D-03 /
      data x25 /
     &  5.0000000000000000D-01, 5.6143234630535521D-01, 
     &  6.2193344186049426D-01, 6.8058615290469393D-01,
     &  7.3650136572285752D-01, 7.8883146512061142D-01, 
     &  8.3678318423673415D-01, 8.7962963151867890D-01,
     &  9.1672131438041693D-01, 9.4749599893913761D-01,
     &  9.7148728561448716D-01, 9.8833196072975871D-01, 
     &  9.9777848489524912D-01 /
      data w25 /
     &  6.1588026863357799D-02, 6.1121221495155122D-02, 
     &  5.9727881767892461D-02, 5.7429129572855862D-02,
     &  5.4259812237131867D-02, 5.0267974533525363D-02, 
     &  4.5514130991481903D-02, 4.0070350167500532D-02,
     &  3.4019166906178545D-02, 2.7452347987917691D-02,
     &  2.0469578350653148D-02, 1.3177493307516108D-02, 
     &  5.6968992505125535D-03 /

      nhalf = ( n + 1 ) / 2

      if ( n .eq. 1 ) then
        call r8vec_copy ( nhalf, x01, xh )
        call r8vec_copy ( nhalf, w01, wh )
      else if ( n .eq. 2 ) then
        call r8vec_copy ( nhalf, x02, xh )
        call r8vec_copy ( nhalf, w02, wh )
      else if ( n .eq. 3 ) then
        call r8vec_copy ( nhalf, x03, xh )
        call r8vec_copy ( nhalf, w03, wh )
      else if ( n .eq. 4 ) then
        call r8vec_copy ( nhalf, x04, xh )
        call r8vec_copy ( nhalf, w04, wh )
      else if ( n .eq. 5 ) then
        call r8vec_copy ( nhalf, x05, xh )
        call r8vec_copy ( nhalf, w05, wh )
      else if ( n .eq. 6 ) then
        call r8vec_copy ( nhalf, x06, xh )
        call r8vec_copy ( nhalf, w06, wh )
      else if ( n .eq. 7 ) then
        call r8vec_copy ( nhalf, x07, xh )
        call r8vec_copy ( nhalf, w07, wh )
      else if ( n .eq. 8 ) then
        call r8vec_copy ( nhalf, x08, xh )
        call r8vec_copy ( nhalf, w08, wh )
      else if ( n .eq. 9 ) then
        call r8vec_copy ( nhalf, x09, xh )
        call r8vec_copy ( nhalf, w09, wh )
      else if ( n .eq. 10 ) then
        call r8vec_copy ( nhalf, x10, xh )
        call r8vec_copy ( nhalf, w10, wh )
      else if ( n .eq. 11 ) then
        call r8vec_copy ( nhalf, x11, xh )
        call r8vec_copy ( nhalf, w11, wh )
      else if ( n .eq. 12 ) then
        call r8vec_copy ( nhalf, x12, xh )
        call r8vec_copy ( nhalf, w12, wh )
      else if ( n .eq. 13 ) then
        call r8vec_copy ( nhalf, x13, xh )
        call r8vec_copy ( nhalf, w13, wh )
      else if ( n .eq. 14 ) then
        call r8vec_copy ( nhalf, x14, xh )
        call r8vec_copy ( nhalf, w14, wh )
      else if ( n .eq. 15 ) then
        call r8vec_copy ( nhalf, x15, xh )
        call r8vec_copy ( nhalf, w15, wh )
      else if ( n .eq. 16 ) then
        call r8vec_copy ( nhalf, x16, xh )
        call r8vec_copy ( nhalf, w16, wh )
      else if ( n .eq. 17 ) then
        call r8vec_copy ( nhalf, x17, xh )
        call r8vec_copy ( nhalf, w17, wh )
      else if ( n .eq. 18 ) then
        call r8vec_copy ( nhalf, x18, xh )
        call r8vec_copy ( nhalf, w18, wh )
      else if ( n .eq. 19 ) then
        call r8vec_copy ( nhalf, x19, xh )
        call r8vec_copy ( nhalf, w19, wh )
      else if ( n .eq. 20 ) then
        call r8vec_copy ( nhalf, x20, xh )
        call r8vec_copy ( nhalf, w20, wh )
      else if ( n .eq. 21 ) then
        call r8vec_copy ( nhalf, x21, xh )
        call r8vec_copy ( nhalf, w21, wh )
      else if ( n .eq. 22 ) then
        call r8vec_copy ( nhalf, x22, xh )
        call r8vec_copy ( nhalf, w22, wh )
      else if ( n .eq. 23 ) then
        call r8vec_copy ( nhalf, x23, xh )
        call r8vec_copy ( nhalf, w23, wh )
      else if ( n .eq. 24 ) then
        call r8vec_copy ( nhalf, x24, xh )
        call r8vec_copy ( nhalf, w24, wh )
      else if ( n .eq. 25 ) then
        call r8vec_copy ( nhalf, x25, xh )
        call r8vec_copy ( nhalf, w25, wh )
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'GQU - Fatal error!'
        write ( *, '(a)' ) '  Value of N must be between 1 and 25.'
        stop 1
      end if

      if ( mod ( n, 2 ) .eq. 1 ) then
        x(1:nhalf-1) = 1.0D+00 - xh(nhalf:2:-1)
        x(nhalf:n) = xh(1:nhalf)
        w(1:nhalf-1) = wh(nhalf:2:-1)
        w(nhalf:n) = wh(1:nhalf)
      else
        x(1:nhalf) = 1.0D+00 - xh(nhalf:1:-1)
        x(nhalf+1:n) = xh(1:nhalf)
        w(1:nhalf) = wh(nhalf:1:-1)
        w(nhalf+1:n) = wh(1:nhalf)
      end if

      return
      end
      subroutine gqu_order ( l, n )

c*********************************************************************72
c
cc GQU_ORDER computes the order of a GQU rule from the level.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 June 2012
c
c  Author:
c
c    John Burkardt.
c
c  Parameters:
c
c    Input, integer L, the level of the rule.  
c    1 .le. L .le. 25.
c
c    Output, integer N, the order of the rule.
c
      implicit none

      integer l
      integer n

      if ( l .lt. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'GQU_ORDER - Fatal error!'
        write ( *, '(a)' ) '  1 .le. L required.'
        write ( *, '(a,i4)' ) '  Input L = ', l
        stop 1
      else if ( l .le. 25 ) then
        n = l
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'GQU_ORDER - Fatal error!'
        write ( *, '(a)' ) '  L .le. 25 required.'
        write ( *, '(a,i4)' ) '  Input L = ', l
        stop 1
      end if

      return
      end
      function i4_choose ( n, k )

c*********************************************************************72
c
cc I4_CHOOSE computes the binomial coefficient C(N,K).
c
c  Discussion:
c
c    The value is calculated in such a way as to avoid overflow and
c    roundoff.  The calculation is done in integer arithmetic.
c
c    The formula used is:
c
c      C(N,K) = N! / ( K! * (N-K)! )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 June 2007
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    ML Wolfson, HV Wright,
c    Algorithm 160:
c    Combinatorial of M Things Taken N at a Time,
c    Communications of the ACM,
c    Volume 6, Number 4, April 1963, page 161.
c
c  Parameters:
c
c    Input, integer N, K, are the values of N and K.
c
c    Output, integer I4_CHOOSE, the number of combinations of N
c    things taken K at a time.
c
      implicit none

      integer i
      integer i4_choose
      integer k
      integer mn
      integer mx
      integer n
      integer value

      mn = min ( k, n - k )

      if ( mn .lt. 0 ) then

        value = 0

      else if ( mn .eq. 0 ) then

        value = 1

      else

        mx = max ( k, n - k )
        value = mx + 1

        do i = 2, mn
          value = ( value * ( mx + i ) ) / i
        end do

      end if

      i4_choose = value

      return
      end
      function i4_factorial2 ( n )

c*********************************************************************72
c
cc I4_FACTORIAL2 computes the double factorial function.
c
c  Discussion:
c
c    FACTORIAL2( N ) = Product ( N * (N-2) * (N-4) * ... * 2 )  (N even)
c                    = Product ( N * (N-2) * (N-4) * ... * 1 )  (N odd)
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    01 June 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the argument of the double factorial 
c    function.  If N is less than 1, I4_FACTORIAL2 is returned as 1.
c
c    Output, integer I4_FACTORIAL2, the value of N!!.
c
      implicit none

      integer i4_factorial2
      integer n
      integer n_copy

      if ( n .lt. 1 ) then
        i4_factorial2 = 1
        return
      end if

      n_copy = n
      i4_factorial2 = 1

10    continue

      if ( 1 .lt. n_copy ) then
        i4_factorial2 = i4_factorial2 * n_copy
        n_copy = n_copy - 2
        go to 10
      end if

      return
      end
      function i4_mop ( i )

c*********************************************************************72
c
cc I4_MOP returns the I-th power of -1 as an I4 value.
c
c  Discussion:
c
c    An I4 is an integer value.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer I, the power of -1.
c
c    Output, integer I4_MOP, the I-th power of -1.
c
      implicit none

      integer i
      integer i4_mop

      if ( mod ( i, 2 ) .eq. 0 ) then
        i4_mop = 1
      else
        i4_mop = -1
      end if

      return
      end
      subroutine i4mat_print ( m, n, a, title )

c*********************************************************************72
c
cc I4MAT_PRINT prints an I4MAT.
c
c  Discussion:
c
c    An I4MAT is an array of I4's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 June 2003
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the number of rows in A.
c
c    Input, integer N, the number of columns in A.
c
c    Input, integer A(M,N), the matrix to be printed.
c
c    Input, character*(*) TITLE, a title.
c
      implicit none

      integer m
      integer n

      integer a(m,n)
      integer ihi
      integer ilo
      integer jhi
      integer jlo
      character*(*) title

      ilo = 1
      ihi = m
      jlo = 1
      jhi = n

      call i4mat_print_some ( m, n, a, ilo, jlo, ihi, jhi, title )

      return
      end
      subroutine i4mat_print_some ( m, n, a, ilo, jlo, ihi, jhi, title )

c*********************************************************************72
c
cc I4MAT_PRINT_SOME prints some of an I4MAT.
c
c  Discussion:
c
c    An I4MAT is an array of I4's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    04 November 2003
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns.
c
c    Input, integer A(M,N), an M by N matrix to be printed.
c
c    Input, integer ILO, JLO, the first row and column to print.
c
c    Input, integer IHI, JHI, the last row and column to print.
c
c    Input, character*(*) TITLE, a title.
c
      implicit none

      integer incx
      parameter ( incx = 10 )
      integer m
      integer n

      integer a(m,n)
      character*(8) ctemp(incx)
      integer i
      integer i2hi
      integer i2lo
      integer ihi
      integer ilo
      integer inc
      integer j
      integer j2
      integer j2hi
      integer j2lo
      integer jhi
      integer jlo
      character*(*) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )

      if ( m .le. 0 .or. n .le. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  (None)'
        return
      end if

      do j2lo = max ( jlo, 1 ), min ( jhi, n ), incx

        j2hi = j2lo + incx - 1
        j2hi = min ( j2hi, n )
        j2hi = min ( j2hi, jhi )

        inc = j2hi + 1 - j2lo

        write ( *, '(a)' ) ' '

        do j = j2lo, j2hi
          j2 = j + 1 - j2lo
          write ( ctemp(j2), '(i8)' ) j
        end do

        write ( *, '(''  Col '',10a8)' ) ( ctemp(j), j = 1, inc )
        write ( *, '(a)' ) '  Row'
        write ( *, '(a)' ) ' '

        i2lo = max ( ilo, 1 )
        i2hi = min ( ihi, m )

        do i = i2lo, i2hi

          do j2 = 1, inc

            j = j2lo - 1 + j2

            write ( ctemp(j2), '(i8)' ) a(i,j)

          end do

          write ( *, '(i5,a,10a8)' ) i, ':', ( ctemp(j), j = 1, inc )

        end do

      end do

      return
      end
      subroutine i4vec_cum0 ( n, a, a_cum )

c*********************************************************************72
c
cc I4VEC_CUM0 computes the cumulutive sum of the entries of an I4VEC.
c
c  Discussion:
c
c    An I4VEC is a vector of I4's.
c
c    This routine returns a vector of length N+1, with the first value
c    being 0.
c
c  Example:
c
c    Input:
c
c      A = (/ 1, 2, 3, 4 /)
c
c    Output:
c
c      A_CUM = (/ 0, 1, 3, 6, 10 /)
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 December 2010
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in the vector.
c
c    Input, integer A(N), the vector to be summed.
c
c    Output, integer A_CUM(0:N), the cumulative sum of the
c    entries of A.
c
      implicit none

      integer n

      integer a(n)
      integer a_cum(0:n)
      integer i

      a_cum(0) = 0

      do i = 1, n
        a_cum(i) = a_cum(i-1) + a(i)
      end do

      return
      end
      subroutine i4vec_print ( n, a, title )

c*********************************************************************72
c
cc I4VEC_PRINT prints an I4VEC.
c
c  Discussion:
c
c    An I4VEC is a vector of integer values.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 November 2006
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of components of the vector.
c
c    Input, integer A(N), the vector to be printed.
c
c    Input, character*(*) TITLE, a title.
c
      implicit none

      integer n

      integer a(n)
      integer i
      character*(*) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i8,a,1x,i12)' ) i, ':', a(i)
      end do

      return
      end
      function i4vec_product ( n, a )

c*********************************************************************72
c
cc I4VEC_PRODUCT returns the product of the entries of an I4VEC.
c
c  Discussion:
c
c    An I4VEC is a vector of I4's.
c
c    In FORTRAN90, this facility is offered by the built in
c    PRODUCT function:
c
c      I4VEC_PRODUCT ( N, A ) = PRODUCT ( A(1:N) )
c
c    In MATLAB, this facility is offered by the built in
c    PROD function:
c
c      I4VEC_PRODUCT ( N, A ) = PROD ( A(1:N) )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in the array.
c
c    Input, integer A(N), the array.
c
c    Output, integer I4VEC_PRODUCT, the product of the entries.
c
      implicit none

      integer n

      integer a(n)
      integer i
      integer i4vec_product

      i4vec_product = 1
      do i = 1, n
        i4vec_product = i4vec_product * a(i)
      end do

      return
      end
      function i4vec_sum ( n, a )

c*********************************************************************72
c
cc I4VEC_SUM returns the sum of the entries of an I4VEC.
c
c  Discussion:
c
c    An I4VEC is a vector of I4's.
c
c    In FORTRAN90, this facility is offered by the built in
c    SUM function:
c
c      I4VEC_SUM ( N, A ) = SUM ( A(1:N) )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in the array.
c
c    Input, integer A(N), the array.
c
c    Output, integer I4VEC_SUM, the sum of the entries.
c
      implicit none

      integer n

      integer a(n)
      integer i
      integer i4vec_sum

      i4vec_sum = 0

      do i = 1, n
        i4vec_sum = i4vec_sum + a(i)
      end do

      return
      end
      subroutine i4vec_transpose_print ( n, a, title )

c*********************************************************************72
c
cc I4VEC_TRANSPOSE_PRINT prints an I4VEC "transposed".
c
c  Discussion:
c
c    An I4VEC is a vector of I4's.
c
c  Example:
c
c    A = (/ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 /)
c    TITLE = 'My vector:  '
c
c    My vector:
c
c        1    2    3    4    5
c        6    7    8    9   10
c       11
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of components of the vector.
c
c    Input, integer A(N), the vector to be printed.
c
c    Input, character ( len = * ) TITLE, a title.
c
      implicit none

      integer n

      integer a(n)
      integer i
      integer ihi
      integer ilo
      character ( len = 11 ) string
      character ( len = * ) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
      write ( *, '(a)' ) ' '
      do ilo = 1, n, 5
        ihi = min ( ilo + 5 - 1, n )
        write ( *, '(5i12)' ) ( a(i), i = ilo, ihi)
      end do

      return
      end
      subroutine kpn ( n, x, w )

c*********************************************************************72
c
cc KPN provides data for Kronrod-Patterson quadrature with a normal weight.
c
c  Discussion:
c
c    This data assumes integration over the interval (-oo,+oo) with 
c    weight function w(x) = exp(-x*x/2)/sqrt(2*pi).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 January 2013
c
c  Author:
c
c    Original MATLAB version by Florian Heiss, Viktor Winschel.
c    FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Florian Heiss, Viktor Winschel,
c    Likelihood approximation by numerical integration on sparse grids,
c    Journal of Econometrics,
c    Volume 144, 2008, pages 62-80.
c
c    Alan Genz, Bradley Keister,
c    Fully symmetric interpolatory rules for multiple integrals
c    over infinite regions with Gaussian weight,
c    Journal of Computational and Applied Mathematics,
c    Volume 71, 1996, pages 299-309.
c
c    Thomas Patterson,
c    The optimal addition of points to quadrature formulae,
c    Mathematics of Computation,
c    Volume 22, Number 104, October 1968, pages 847-856.
c
c  Parameters:
c
c    Input, integer N, the order of the rule.
c
c    Output, double precision X(N), the nodes.
c
c    Output, double precision W(N), the weights.
c
      implicit none

      integer n

      integer nhalf
      double precision w(n)
      double precision wh((n+1)/2)
      double precision x(n)
      double precision xh((n+1)/2)
      double precision x01(1)
      double precision w01(1)
      double precision x03(2)
      double precision w03(2)
      double precision x07(4)
      double precision w07(4)
      double precision x09(5)
      double precision w09(5)
      double precision x17(9)
      double precision w17(9)
      double precision x19(10)
      double precision w19(10)
      double precision x31(16)
      double precision w31(16)
      double precision x33(17)
      double precision w33(17)
      double precision x35(18)
      double precision w35(18)

      data x01 /
     &  0.0000000000000000D+00 /
      data w01 /
     &  1.0000000000000000D+00/
      data x03 /
     &  0.0000000000000000D+00, 1.7320508075688772D+00 /
      data w03 /
     &  6.6666666666666663D-01, 1.6666666666666666D-01/
      data x07 /
     &  0.0000000000000000D+00, 7.4109534999454085D-01,
     &  1.7320508075688772D+00, 4.1849560176727323D+00 /
      data w07 /
     &  4.5874486825749189D-01, 1.3137860698313561D-01,
     &  1.3855327472974924D-01, 6.9568415836913987D-04 /
      data x09 /
     &  0.0000000000000000D+00, 7.4109534999454085D-01,
     &  1.7320508075688772D+00, 2.8612795760570582D+00,
     &  4.1849560176727323D+00 /
      data w09 /
     &  2.5396825396825407D-01, 2.7007432957793776D-01,
     &  9.4850948509485125D-02, 7.9963254708935293D-03,
     &  9.4269457556517470D-05 /
      data x17 /
     &  0.0000000000000000D+00, 7.4109534999454085D-01,
     &  1.2304236340273060D+00, 1.7320508075688772D+00,
     &  2.5960831150492023D+00, 2.8612795760570582D+00, 
     &  4.1849560176727323D+00, 5.1870160399136562D+00,
     &  6.3633944943363696D+00 /
      data w17 /
     &  2.6692223033505302D-01, 2.5456123204171222D-01,
     &  1.4192654826449365D-02, 8.8681002152028010D-02,
     &  1.9656770938777492D-03, 7.0334802378279075D-03, 
     &  1.0563783615416941D-04, -8.2049207541509217D-07,
     &  2.1136499505424257D-08 /
      data x19 /
     &  0.0000000000000000D+00, 7.4109534999454085D-01,
     &  1.2304236340273060D+00, 1.7320508075688772D+00,
     &  2.5960831150492023D+00, 2.8612795760570582D+00, 
     &  3.2053337944991944D+00, 4.1849560176727323D+00,
     &  5.1870160399136562D+00, 6.3633944943363696D+00 /
      data w19 /
     &  3.0346719985420623D-01,  2.0832499164960877D-01,
     &  6.1151730125247716D-02,  6.4096054686807610D-02,
     &  1.8085234254798462D-02, -6.3372247933737571D-03, 
     &  2.8848804365067559D-03,  6.0123369459847997D-05,
     &  6.0948087314689840D-07,  8.6296846022298632D-10 /
      data x31 /
     &  0.0000000000000000D+00, 2.4899229757996061D-01,
     &  7.4109534999454085D-01, 1.2304236340273060D+00,
     &  1.7320508075688772D+00, 2.2336260616769419D+00, 
     &  2.5960831150492023D+00, 2.8612795760570582D+00,
     &  3.2053337944991944D+00, 3.6353185190372783D+00,
     &  4.1849560176727323D+00, 5.1870160399136562D+00, 
     &  6.3633944943363696D+00, 7.1221067008046166D+00,
     &  7.9807717985905606D+00, 9.0169397898903032D+00 /
      data w31 /
     &  2.5890005324151566D-01,  2.8128101540033167D-02,
     &  1.9968863511734550D-01,  6.5417392836092561D-02, 
     &  6.1718532565867179D-02,  1.7608475581318002D-03, 
     &  1.6592492698936010D-02, -5.5610063068358157D-03,
     &  2.7298430467334002D-03,  1.5044205390914219D-05,
     &  5.9474961163931621D-05,  6.1435843232617913D-07, 
     &  7.9298267864869338D-10,  5.1158053105504208D-12,
     & -1.4840835740298868D-13,  1.2618464280815118D-15 /
      data x33 /
     &  0.0000000000000000D+00, 2.4899229757996061D-01,
     &  7.4109534999454085D-01, 1.2304236340273060D+00,
     &  1.7320508075688772D+00, 2.2336260616769419D+00, 
     &  2.5960831150492023D+00, 2.8612795760570582D+00,
     &  3.2053337944991944D+00, 3.6353185190372783D+00,
     &  4.1849560176727323D+00, 5.1870160399136562D+00, 
     &  5.6981777684881099D+00, 6.3633944943363696D+00,
     &  7.1221067008046166D+00, 7.9807717985905606D+00,
     &  9.0169397898903032D+00 /
      data w33 /
     &  1.3911022236338039D-01,  1.0387687125574284D-01,
     &  1.7607598741571459D-01,  7.7443602746299481D-02,
     &  5.4677556143463042D-02,  7.3530110204955076D-03, 
     &  1.1529247065398790D-02, -2.7712189007789243D-03,
     &  2.1202259559596325D-03,  8.3236045295766745D-05,
     &  5.5691158981081479D-05,  6.9086261179113738D-07, 
     & -1.3486017348542930D-08,  1.5542195992782658D-09,
     & -1.9341305000880955D-11,  2.6640625166231651D-13,
     & -9.9313913286822465D-16 /
      data x35 /
     &  0.0000000000000000D+00, 2.4899229757996061D-01,
     &  7.4109534999454085D-01, 1.2304236340273060D+00,
     &  1.7320508075688772D+00, 2.2336260616769419D+00, 
     &  2.5960831150492023D+00, 2.8612795760570582D+00,
     &  3.2053337944991944D+00, 3.6353185190372783D+00,
     &  4.1849560176727323D+00, 4.7364330859522967D+00, 
     &  5.1870160399136562D+00, 5.6981777684881099D+00,
     &  6.3633944943363696D+00, 7.1221067008046166D+00,
     &  7.9807717985905606D+00, 9.0169397898903032D+00 /
      data w35 /
     &  5.1489450806921377D-04, 1.9176011588804434D-01,
     &  1.4807083115521585D-01, 9.2364726716986353D-02,
     &  4.5273685465150391D-02, 1.5673473751851151D-02, 
     &  3.1554462691875513D-03, 2.3113452403522071D-03,
     &  8.1895392750226735D-04, 2.7524214116785131D-04,
     &  3.5729348198975332D-05, 2.7342206801187888D-06, 
     &  2.4676421345798140D-07, 2.1394194479561062D-08,
     &  4.6011760348655917D-10, 3.0972223576062995D-12,
     &  5.4500412650638128D-15, 1.0541326582334014D-18 /

      nhalf = ( n + 1 ) / 2

      if ( n .eq. 1 ) then
        call r8vec_copy ( nhalf, x01, xh )
        call r8vec_copy ( nhalf, w01, wh )
      else if ( n .eq. 3 ) then
        call r8vec_copy ( nhalf, x03, xh )
        call r8vec_copy ( nhalf, w03, wh )
      else if ( n .eq. 7 ) then
        call r8vec_copy ( nhalf, x07, xh )
        call r8vec_copy ( nhalf, w07, wh )
      else if ( n .eq. 9 ) then
        call r8vec_copy ( nhalf, x09, xh )
        call r8vec_copy ( nhalf, w09, wh )
      else if ( n .eq. 17 ) then
        call r8vec_copy ( nhalf, x17, xh )
        call r8vec_copy ( nhalf, w17, wh )
      else if ( n .eq. 19 ) then
        call r8vec_copy ( nhalf, x19, xh )
        call r8vec_copy ( nhalf, w19, wh )
      else if ( n .eq. 31 ) then
        call r8vec_copy ( nhalf, x31, xh )
        call r8vec_copy ( nhalf, w31, wh )
      else if ( n .eq. 33 ) then
        call r8vec_copy ( nhalf, x33, xh )
        call r8vec_copy ( nhalf, w33, wh )
      else if ( n .eq. 35 ) then
        call r8vec_copy ( nhalf, x35, xh )
        call r8vec_copy ( nhalf, w35, wh )
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'KPN - Fatal error!'
        write ( *, '(a)' ) '  Illegal value of N.'
        stop 1
      end if

      if ( mod ( n, 2 ) .eq. 1 ) then
        x(1:nhalf-1) = - xh(nhalf:2:-1)
        x(nhalf:n) = xh(1:nhalf)
        w(1:nhalf-1) = wh(nhalf:2:-1)
        w(nhalf:n) = wh(1:nhalf)
      else
        x(1:nhalf) = - xh(nhalf:1:-1)
        x(nhalf+1:n) = xh(1:nhalf)
        w(1:nhalf) = wh(nhalf:1:-1)
        w(nhalf+1:n) = wh(1:nhalf)
      end if

      return
      end
      subroutine kpn_order ( l, n )

c*********************************************************************72
c
cc KPN_ORDER computes the order of a KPN rule from the level.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 May 2012
c
c  Author:
c
c    John Burkardt.
c
c  Parameters:
c
c    Input, integer L, the level of the rule.  
c    1 .le. L .le. 25
c
c    Output, integer N, the order of the rule.
c
      implicit none

      integer l
      integer n

      if ( l .lt. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'KPN_ORDER - Fatal error!'
        write ( *, '(a)' ) '  1 .le. L .le. 25 required.'
        write ( *, '(a,i4)' ) '  Input L = ', l
        stop 1
      else if ( l .eq. 1 ) then
        n = 1
      else if ( l .le. 3 ) then
        n = 3
      else if ( l .eq. 4 ) then
        n = 7
      else if ( l .le. 8 ) then
        n = 9
      else if ( l .eq. 9 ) then
        n = 17
      else if ( l .le. 15 ) then
        n = 19
      else if ( l .eq. 16 ) then
        n = 31
      else if ( l .eq. 17 ) then
        n = 33
      else if ( l .le. 25 ) then
        n = 35
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'KPN_ORDER - Fatal error!'
        write ( *, '(a)' ) '  1 .le. L .le. 25 required.'
        write ( *, '(a,i4)' ) '  Input L = ', l
        stop 1
      end if

      return
      end
      subroutine kpu ( n, x, w )

c**********************************************************************72
c
cc KPU provides data for Kronrod-Patterson quadrature with a uniform weight.
c
c  Discussion:
c
c    This data assumes integration over the interval [0,1] with 
c    weight function w(x) = 1.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    06 January 2013
c
c  Author:
c
c    John Burkardt.
c
c  Reference:
c
c    Florian Heiss, Viktor Winschel,
c    Likelihood approximation by numerical integration on sparse grids,
c    Journal of Econometrics,
c    Volume 144, 2008, pages 62-80.
c
c    Alan Genz, Bradley Keister,
c    Fully symmetric interpolatory rules for multiple integrals
c    over infinite regions with Gaussian weight,
c    Journal of Computational and Applied Mathematics,
c    Volume 71, 1996, pages 299-309.
c
c    Thomas Patterson,
c    The optimal addition of points to quadrature formulae,
c    Mathematics of Computation,
c    Volume 22, Number 104, October 1968, pages 847-856.
c
c  Parameters:
c
c    Input, integer N, the order of the rule.
c    Only 1, 3, 7, 15, 31 and 63 are legal input values for N.
c
c    Output, double precision X(N), the nodes.
c
c    Output, double precision W(N), the weights.
c
      implicit none

      integer n

      double precision w(n)
      double precision w01(1)
      double precision w03(3)
      double precision w07(7)
      double precision w15(15)
      double precision w31(31)
      double precision w63(63)
      double precision x(n)
      double precision x01(1)
      double precision x03(3)
      double precision x07(7)
      double precision x15(15)
      double precision x31(31)
      double precision x63(63)

      data x01 /
     & 0.5000000D+00 /
      data w01 / 
     & 1.0000000D+00 /
      data x03 /
     & 0.1127017D+00,  0.5000000D+00,  0.8872983D+00 /
      data w03 / 
     & 0.2777778D+00,  0.4444444D+00,  0.2777778D+00 /
      data x07 / 
     & 0.0197544D+00,  0.1127017D+00,  0.2828781D+00, 
     & 0.5000000D+00,  0.7171219D+00,  0.8872983D+00, 
     & 0.9802456D+00 /
      data w07 / 
     & 0.052328105232811D+00,   0.134244013424401D+00,
     & 0.200698720069872D+00,   0.225458322545832D+00,
     & 0.200698720069872D+00,   0.134244013424401D+00, 
     & 0.052328105232811D+00 /
      data x15 / 
     & 0.0030840D+00,  0.0197544D+00,  0.0557704D+00, 
     & 0.1127017D+00,  0.1894485D+00,  0.2828781D+00, 
     & 0.3883067D+00,  0.5000000D+00,  0.6116933D+00,  
     & 0.7171219D+00,  0.8105515D+00,  0.8872983D+00, 
     & 0.9442296D+00,  0.9802456D+00,  0.9969160D+00 /
      data w15 / 
     & 0.0085009D+00,  0.0258016D+00,  0.0464636D+00, 
     & 0.0672076D+00,  0.0857560D+00,  0.1003143D+00, 
     & 0.1095784D+00,  0.1127552D+00,  0.1095784D+00, 
     & 0.1003143D+00,  0.0857560D+00,  0.0672076D+00, 
     & 0.0464636D+00,  0.0258016D+00,  0.0085009D+00 /
      data x31 / 
     & 0.0004509D+00,  0.0030840D+00,  0.0092344D+00, 
     & 0.0197544D+00,  0.0351726D+00,  0.0557704D+00, 
     & 0.0816370D+00,  0.1127017D+00,  0.1487519D+00, 
     & 0.1894485D+00,  0.2343401D+00,  0.2828781D+00, 
     & 0.3344323D+00,  0.3883067D+00,  0.4437555D+00, 
     & 0.5000000D+00,  0.5562445D+00,  0.6116933D+00, 
     & 0.6655677D+00,  0.7171219D+00,  0.7656599D+00, 
     & 0.8105515D+00,  0.8512481D+00,  0.8872983D+00, 
     & 0.9183630D+00,  0.9442296D+00,  0.9648274D+00, 
     & 0.9802456D+00,  0.9907656D+00,  0.9969160D+00, 
     & 0.9995491D+00 /
      data w31 / 
     & 0.0012724D+00,  0.0042173D+00,  0.0082230D+00, 
     & 0.0129038D+00,  0.0179786D+00,  0.0232314D+00, 
     & 0.0284898D+00,  0.0336039D+00,  0.0384398D+00, 
     & 0.0428780D+00,  0.0468136D+00,  0.0501571D+00, 
     & 0.0528349D+00,  0.0547892D+00,  0.0559784D+00, 
     & 0.0563776D+00,  0.0559784D+00,  0.0547892D+00, 
     & 0.0528349D+00,  0.0501571D+00,  0.0468136D+00, 
     & 0.0428780D+00,  0.0384398D+00,  0.0336039D+00, 
     & 0.0284898D+00,  0.0232314D+00,  0.0179786D+00, 
     & 0.0129038D+00,  0.0082230D+00,  0.0042173D+00, 
     & 0.0012724D+00 /
      data x63 / 
     & 0.0000635D+00,  0.0004509D+00,  0.0013969D+00, 
     & 0.0030840D+00,  0.0056576D+00,  0.0092344D+00, 
     & 0.0139086D+00,  0.0197544D+00,  0.0268286D+00, 
     & 0.0351726D+00,  0.0448144D+00,  0.0557704D+00, 
     & 0.0680460D+00,  0.0816370D+00,  0.0965297D+00, 
     & 0.1127017D+00,  0.1301220D+00,  0.1487519D+00, 
     & 0.1685452D+00,  0.1894485D+00,  0.2114021D+00, 
     & 0.2343401D+00,  0.2581910D+00,  0.2828781D+00, 
     & 0.3083203D+00,  0.3344323D+00,  0.3611251D+00, 
     & 0.3883067D+00,  0.4158823D+00,  0.4437555D+00, 
     & 0.4718279D+00,  0.5000000D+00,  0.5281721D+00, 
     & 0.5562445D+00,  0.5841177D+00,  0.6116933D+00, 
     & 0.6388749D+00,  0.6655677D+00,  0.6916797D+00, 
     & 0.7171219D+00,  0.7418090D+00,  0.7656599D+00, 
     & 0.7885979D+00,  0.8105515D+00,  0.8314548D+00, 
     & 0.8512481D+00,  0.8698780D+00,  0.8872983D+00, 
     & 0.9034703D+00,  0.9183630D+00,  0.9319540D+00, 
     & 0.9442296D+00,  0.9551856D+00,  0.9648274D+00, 
     & 0.9731714D+00,  0.9802456D+00,  0.9860914D+00, 
     & 0.9907656D+00,  0.9943424D+00,  0.9969160D+00, 
     & 0.9986031D+00,  0.9995491D+00,  0.9999365D+00 /
      data w63 / 
     & 0.0001816D+00,  0.0006326D+00,  0.0012895D+00, 
     & 0.0021088D+00,  0.0030578D+00,  0.0041115D+00, 
     & 0.0052491D+00,  0.0064519D+00,  0.0077034D+00, 
     & 0.0089893D+00,  0.0102971D+00,  0.0116157D+00, 
     & 0.0129348D+00,  0.0142449D+00,  0.0155368D+00, 
     & 0.0168019D+00,  0.0180322D+00,  0.0192199D+00, 
     & 0.0203578D+00,  0.0214390D+00,  0.0224573D+00, 
     & 0.0234068D+00,  0.0242822D+00,  0.0250786D+00, 
     & 0.0257916D+00,  0.0264175D+00,  0.0269527D+00, 
     & 0.0273946D+00,  0.0277407D+00,  0.0279892D+00, 
     & 0.0281388D+00,  0.0281888D+00,  0.0281388D+00, 
     & 0.0279892D+00,  0.0277407D+00,  0.0273946D+00, 
     & 0.0269527D+00,  0.0264175D+00,  0.0257916D+00, 
     & 0.0250786D+00,  0.0242822D+00,  0.0234068D+00, 
     & 0.0224573D+00,  0.0214390D+00,  0.0203578D+00, 
     & 0.0192199D+00,  0.0180322D+00,  0.0168019D+00, 
     & 0.0155368D+00,  0.0142449D+00,  0.0129348D+00, 
     & 0.0116157D+00,  0.0102971D+00,  0.0089893D+00, 
     & 0.0077034D+00,  0.0064519D+00,  0.0052491D+00, 
     & 0.0041115D+00,  0.0030578D+00,  0.0021088D+00, 
     & 0.0012895D+00,  0.0006326D+00,  0.0001816D+00 /

      if ( n .eq. 1 ) then
        call r8vec_copy ( n, x01, x )
        call r8vec_copy ( n, w01, w )
      else if ( n .eq. 3 ) then
        call r8vec_copy ( n, x03, x )
        call r8vec_copy ( n, w03, w )
      else if ( n .eq. 7 ) then
        call r8vec_copy ( n, x07, x )
        call r8vec_copy ( n, w07, w )
      else if ( n .eq. 15 ) then
        call r8vec_copy ( n, x15, x )
        call r8vec_copy ( n, w15, w )
      else if ( n .eq. 31 ) then
        call r8vec_copy ( n, x31, x )
        call r8vec_copy ( n, w31, w )
      else if ( n .eq. 63 ) then
        call r8vec_copy ( n, x63, x )
        call r8vec_copy ( n, w63, w )
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'KPU - Fatal error!'
        write ( *, '(a)' ) '  Illegal value of N.'
        stop 1
      end if

      return
      end
      subroutine kpu_order ( l, n )

c*********************************************************************72
c
cc KPU_ORDER computes the order of a KPU rule from the level.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 May 2012
c
c  Author:
c
c    John Burkardt.
c
c  Parameters:
c
c    Input, integer L, the level of the rule.  
c    1 .le. L .le. 25
c
c    Output, integer N, the order of the rule.
c
      implicit none

      integer l
      integer n

      if ( l .lt. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'KPU_ORDER - Fatal error!'
        write ( *, '(a)' ) '  1 .le. L .le. 25 required.'
        write ( *, '(a,i4)' ) '  Input L = ', l
        stop 1
      else if ( l .eq. 1 ) then
        n = 1
      else if ( l .le. 3 ) then
        n = 3
      else if ( l .le. 6 ) then
        n = 7
      else if ( l .le. 12 ) then
        n = 15
      else if ( l .le. 24 ) then
        n = 31
      else if ( l .le. 25 ) then
        n = 63
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'KPU_ORDER - Fatal error!'
        write ( *, '(a)' ) '  1 .le. L .le. 25 required.'
        write ( *, '(a,i4)' ) '  Input L = ', l
        stop 1
      end if

      return
      end
      subroutine num_seq ( n, k, seq_num )

c*********************************************************************72
c
cc NUM_SEQ returns the number of compositions of the integer N into K parts.
c
c  Discussion:
c
c    A composition of the integer N into K parts is an ordered sequence
c    of K nonnegative integers which sum to N.  The compositions (1,2,1)
c    and (1,1,2) are considered to be distinct.
c
c    The 28 compositions of 6 into three parts are:
c
c      6 0 0,  5 1 0,  5 0 1,  4 2 0,  4 1 1,  4 0 2,
c      3 3 0,  3 2 1,  3 1 2,  3 0 3,  2 4 0,  2 3 1,
c      2 2 2,  2 1 3,  2 0 4,  1 5 0,  1 4 1,  1 3 2,
c      1 2 3,  1 1 4,  1 0 5,  0 6 0,  0 5 1,  0 4 2,
c      0 3 3,  0 2 4,  0 1 5,  0 0 6.
c
c    The formula for the number of compositions of N into K parts is
c
c      Number = ( N + K - 1 )! / ( N! * ( K - 1 )! )
c
c    Describe the composition using N '1's and K-1 dividing lines '|'.
c    The number of distinct permutations of these symbols is the number
c    of compositions.  This is equal to the number of permutations of
c    N+K-1 things, with N identical of one kind and K-1 identical of another.
c
c    Thus, for the above example, we have:
c
c      Number = ( 6 + 3 - 1 )! / ( 6! * (3-1)! ) = 28
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 June 2007
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Albert Nijenhuis, Herbert Wilf,
c    Combinatorial Algorithms for Computers and Calculators,
c    Second Edition,
c    Academic Press, 1978,
c    ISBN: 0-12-519260-6,
c    LC: QA164.N54.
c
c  Parameters:
c
c    Input, integer N, the integer whose compositions are desired.
c
c    Input, integer K, the number of parts in the composition.
c
c    Output, integer SEQ_NUM, the number of compositions of N
c    into K parts.
c
      implicit none

      integer i4_choose
      integer k
      integer n
      integer seq_num

      seq_num = i4_choose ( n + k - 1, n )

      return
      end
      subroutine nwspgr_size ( rule_order, dim, k, seq_max, r_size )

c*********************************************************************72
c
cc NWSPGR_SIZE determines the size of a sparse grid rule.
c
c  Discussion:
c
c    This routine does a "raw" count, that is, it does not notice that many
c    points may be repeated, in which case, the size of the rule could be
c    reduced by merging repeated points and combining the corresponding weights.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 December 2012
c
c  Author:
c
c    John Burkardt.
c
c  Reference:
c
c    Florian Heiss, Viktor Winschel,
c    Likelihood approximation by numerical integration on sparse grids,
c    Journal of Econometrics,
c    Volume 144, 2008, pages 62-80.
c
c  Parameters:
c
c    Input, external RULE_ORDER ( l, n ), the name of a subroutine which
c    is given the level L and returns the order N of the corresponding rule.
c
c    Input, integer DIM, the dimension of the integration problem.
c
c    Input, integer K, the level.  When using the built in 1D 
c    rules, the resulting sparse grid will be exact for polynomials up to total
c    order 2*K-1.  When using the built-in quadrature rules, the maximum value 
c    of K that is available is 25.
c
c    Input, integer SEQ_MAX, the maximum number of sequences necessary
c    for the calculation.  SEQ_MAX can be computed by the call
c      call num_seq ( k - 1, dim, seq_max )
c    and is only needed by FORTRAN77 for preallocating arrays.
c
c    Output, integer R_SIZE, the "size" of the rule, that is,
c    the number of weights and multidimensional quadrature points that will
c    be needed.  The size of the rule will be reduced when duplicate points
c    are merged.
c
      implicit none

      integer dim
      integer k
      integer seq_max

      integer i
      integer i4vec_sum
      integer is(seq_max*dim)
      integer j
      integer level
      integer maxq
      integer minq
      integer n
      integer n1d(k)
      integer n1d_total
      integer q
      integer r_size
      integer rq(seq_max)
      external rule_order
      integer seq_num
c
c  Determine the size of each 1D rule.
c
      do level = 1, k

        call rule_order ( level, n )
        n1d(level) = n

      end do

      n1d_total = i4vec_sum ( k, n1d )
c
c  Go through the motions of generating the rules.
c
      minq = max ( 0, k - dim )
      maxq = k - 1
      r_size = 0

      do q = minq, maxq
c
c  Compute the D-dimensional vectors that sum to Q+DIM.
c
        call num_seq ( q, dim, seq_num )

        call get_seq ( dim, q + dim, seq_num, is )
c
c  Determine the size of each rule.
c
        do i = 1, seq_num
          rq(i) = 1
          do j = 1, dim
            rq(i) = rq(i) * n1d(is(i+(j-1)*seq_num))
          end do
        end do
c
c  Add the sizes to the total.
c
        r_size = r_size + i4vec_sum ( seq_num, rq )

      end do

      return
      end
      subroutine quad_rule_print ( m, n, x, w, title )

c*********************************************************************72
c
cc QUAD_RULE_PRINT prints a multidimensional quadrature rule.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    07 May 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the spatial dimension.
c
c    Input, integer N, the number of points.
c
c    Input, double precision X(M,N), the abscissas.
c
c    Input, double precision W(N), the weights.
c
c    Input, character ( len = * ) TITLE, a title for the rule.
c
      implicit none

      integer m
      integer n

      integer i
      integer j
      character ( len = * ) title
      double precision w(n)
      double precision x(m,n)

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
      write ( *, '(a)' ) ' '

      do j = 1, n
        write ( *, '(2x,i2,2x,f10.6,a)', advance = 'no' ) 
     &    j, w(j), ' * f ('
        do i = 1, m
          write ( *, '(f10.6)', advance = 'no' ) x(i,j)
          if ( i .lt. m ) then
            write ( *, '('','')', advance = 'no' )
          else
            write ( *, '('' )'')', advance = 'yes' )
          end if
        end do
      end do

      return
      end
      function r8_uniform_01 ( seed )

c*********************************************************************72
c
cc R8_UNIFORM_01 returns a pseudorandom R8 scaled to [0,1].
c
c  Discussion:
c
c    This routine implements the recursion
c
c      seed = 16807 * seed mod ( 2^31 - 1 )
c      r8_uniform_01 = seed / ( 2^31 - 1 )
c
c    The integer arithmetic never requires more than 32 bits,
c    including a sign bit.
c
c    If the initial seed is 12345, then the first three computations are
c
c      Input     Output      R8_UNIFORM_01
c      SEED      SEED
c
c         12345   207482415  0.096616
c     207482415  1790989824  0.833995
c    1790989824  2035175616  0.947702
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 August 2004
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
c    Pierre L'Ecuyer,
c    Random Number Generation,
c    in Handbook of Simulation,
c    edited by Jerry Banks,
c    Wiley Interscience, page 95, 1998.
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
c    Input/output, integer SEED, the "seed" value, which should NOT be 0.
c    On output, SEED has been updated.
c
c    Output, double precision R8_UNIFORM_01, a new pseudorandom variate,
c    strictly between 0 and 1.
c
      implicit none

      double precision r8_uniform_01
      integer k
      integer seed

      if ( seed .eq. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8_UNIFORM_01 - Fatal error!'
        write ( *, '(a)' ) '  Input value of SEED = 0.'
        stop 1
      end if

      k = seed / 127773

      seed = 16807 * ( seed - k * 127773 ) - k * 2836

      if ( seed .lt. 0 ) then
        seed = seed + 2147483647
      end if

      r8_uniform_01 = dble ( seed ) * 4.656612875D-10

      return
      end
      subroutine r8cvv_offset ( m, nr, roff )

c*********************************************************************72
c
cc R8CVV_OFFSET determines the row offsets of an R8CVV.
c
c  Discussion:
c
c    An R8CVV is a "vector of vectors" of R8's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 November 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the number of rows in the array.
c
c    Input, integer NR(M), the row sizes.
c
c    Output, integer ROFF(M+1), the row offsets.
c
      implicit none

      integer m

      integer i
      integer roff(m+1)
      integer nr(m)

      roff(1) = 0
      do i = 1, m
        roff(i+1) = roff(i) + nr(i)
      end do

      return
      end
      subroutine r8cvv_print ( mn, a, m, roff, title )

c*********************************************************************72
c
cc R8CVV_PRINT prints an R8CVV.
c
c  Discussion:
c
c    An R8CVV is a "vector of vectors" of R8's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 November 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer MN, the size of the cell array.
c
c    Input, double precision A(MN), the cell array.
c
c    Input, integer M, the number of rows in the array.
c
c    Input, integer ROFF(M+1), the row offsets.
c
c    Input, character ( len = * ) TITLE, a title.
c
      implicit none

      integer m
      integer mn

      double precision a(mn)
      integer i
      integer k1
      integer k2
      integer khi
      integer klo
      integer roff(m+1)
      character ( len = * ) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
      write ( *, '(a)' ) ' '

      do i = 1, m

        k1 = roff(i) + 1
        k2 = roff(i+1)

        do klo = k1, k2, 5
          khi = min ( klo + 5 - 1, k2 )
          if ( klo .eq. k1 ) then
            write ( *, '(i5,2x, 5g14.6)' ) i, a(klo:khi)
          else
            write ( *, '(5x,2x, 5g14.6)' )    a(klo:khi)
          end if
        end do

      end do

      return
      end
      subroutine r8cvv_rget ( mn, a, m, roff, i, ai )

c*********************************************************************72
c
cc R8CVV_RGET gets row I from an R8CVV.
c
c  Discussion:
c
c    An R8CVV is a "vector of vectors" of R8's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    16 November 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer MN, the size of the cell array.
c
c    Input, double precision A(MN), the cell array.
c
c    Input, integer M, the number of rows in the array.
c
c    Input, integer ROFF(M+1), the row offsets.
c
c    Input, integer I, the row.
c    1 .le. I .le. M.
c
c    Output, double precision AI(NR(I)), the value of A(I,*).
c
      implicit none

      integer m
      integer mn

      double precision a(mn)
      double precision ai(*)
      integer i
      integer k1
      integer k2
      integer nv
      integer roff(m+1)

      k1 = roff(i) + 1
      k2 = roff(i+1)
      nv = k2 + 1 - k1
      ai(1:nv) = a(k1:k2)

      return
      end
      subroutine r8cvv_rset ( mn, a, m, roff, i, ai )

c*********************************************************************72
c
cc R8CVV_RSET sets row I from an R8CVV.
c
c  Discussion:
c
c    An R8CVV is a "vector of vectors" of R8's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 November 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer MN, the size of the cell array.
c
c    Input/output, double precision A(MN), the cell array.
c
c    Input, integer M, the number of rows in the array.
c
c    Input, integer ROFF(M+1), the row offsets.
c
c    Input, integer I, the row.
c    1 .le. I .le. M.
c
c    Input, double precision AI(NR(I)), the new value of A(I,*).
c
      implicit none

      integer m
      integer mn

      double precision a(mn)
      double precision ai(*)
      integer i
      integer k1
      integer k2
      integer nv
      integer roff(m+1)

      k1 = roff(i) + 1
      k2 = roff(i+1)
      nv = k2 + 1 - k1
      a(k1:k2) = ai(1:nv)

      return
      end
      subroutine r8mat_normal_01 ( m, n, seed, r )

c*********************************************************************72
c
cc R8MAT_NORMAL_01 returns a unit pseudonormal R8MAT.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 November 2010
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Paul Bratley, Bennett Fox, Linus Schrage,
c    A Guide to Simulation,
c    Second Edition,
c    Springer, 1987,
c    ISBN: 0387964673,
c    LC: QA76.9.C65.B73.
c
c    Bennett Fox,
c    Algorithm 647:
c    Implementation and Relative Efficiency of Quasirandom
c    Sequence Generators,
c    ACM Transactions on Mathematical Software,
c    Volume 12, Number 4, December 1986, pages 362-376.
c
c    Pierre L'Ecuyer,
c    Random Number Generation,
c    in Handbook of Simulation,
c    edited by Jerry Banks,
c    Wiley, 1998,
c    ISBN: 0471134031,
c    LC: T57.62.H37.
c
c    Peter Lewis, Allen Goodman, James Miller,
c    A Pseudo-Random Number Generator for the System/360,
c    IBM Systems Journal,
c    Volume 8, 1969, pages 136-143.
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns
c    in the array.
c
c    Input/output, integer SEED, the "seed" value, which
c    should NOT be 0.  On output, SEED has been updated.
c
c    Output, double precision R(M,N), the array of pseudonormal values.
c
      implicit none

      integer m
      integer n

      integer seed
      double precision r(m,n)

      call r8vec_normal_01 ( m * n, seed, r )

      return
      end
      subroutine r8mat_transpose_print ( m, n, a, title )

c*********************************************************************72
c
cc R8MAT_TRANSPOSE_PRINT prints an R8MAT, transposed.
c
c  Discussion:
c
c    An R8MAT is an array of R8's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 April 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns.
c
c    Input, double precision A(M,N), an M by N matrix to be printed.
c
c    Input, character*(*) TITLE, a title.
c
      implicit none

      integer m
      integer n

      double precision a(m,n)
      character*(*) title

      call r8mat_transpose_print_some ( m, n, a, 1, 1, m, n, title )

      return
      end
      subroutine r8mat_transpose_print_some ( m, n, a, ilo, jlo, ihi,
     &  jhi, title )

c*********************************************************************72
c
cc R8MAT_TRANSPOSE_PRINT_SOME prints some of an R8MAT transposed.
c
c  Discussion:
c
c    An R8MAT is an array of R8's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 April 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns.
c
c    Input, double precision A(M,N), an M by N matrix to be printed.
c
c    Input, integer ILO, JLO, the first row and column to print.
c
c    Input, integer IHI, JHI, the last row and column to print.
c
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer incx
      parameter ( incx = 5 )
      integer m
      integer n

      double precision a(m,n)
      character * ( 14 ) ctemp(incx)
      integer i
      integer i2
      integer i2hi
      integer i2lo
      integer ihi
      integer ilo
      integer inc
      integer j
      integer j2hi
      integer j2lo
      integer jhi
      integer jlo
      character * ( * ) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )

      if ( m .le. 0 .or. n .le. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  (None)'
        return
      end if

      do i2lo = max ( ilo, 1 ), min ( ihi, m ), incx

        i2hi = i2lo + incx - 1
        i2hi = min ( i2hi, m )
        i2hi = min ( i2hi, ihi )

        inc = i2hi + 1 - i2lo

        write ( *, '(a)' ) ' '

        do i = i2lo, i2hi
          i2 = i + 1 - i2lo
          write ( ctemp(i2), '(i8,6x)') i
        end do

        write ( *, '(''       Row'',5a14)' ) ctemp(1:inc)
        write ( *, '(a)' ) '       Col'

        j2lo = max ( jlo, 1 )
        j2hi = min ( jhi, n )

        do j = j2lo, j2hi

          do i2 = 1, inc
            i = i2lo - 1 + i2
            write ( ctemp(i2), '(g14.6)' ) a(i,j)
          end do

          write ( *, '(2x,i8,a,5a14)' ) j, ':', ( ctemp(i), i = 1, inc )

        end do

      end do

      return
      end
      subroutine r8mat_uniform_01 ( m, n, seed, r )

c*********************************************************************72
c
cc R8MAT_UNIFORM_01 returns a unit pseudorandom R8MAT.
c
c  Discussion:
c
c    An R8MAT is an array of R8's.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    11 August 2004
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
c    Input, integer M, N, the number of rows and columns in the array.
c
c    Input/output, integer SEED, the "seed" value, which should NOT be 0.
c    On output, SEED has been updated.
c
c    Output, double precision R(M,N), the array of pseudorandom values.
c
      implicit none

      integer m
      integer n

      integer i
      integer j
      integer k
      integer seed
      double precision r(m,n)

      if ( seed .eq. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8MAT_UNIFORM_01 - Fatal error!'
        write ( *, '(a)' ) '  Input value of SEED = 0.'
        stop 1
      end if

      do j = 1, n

        do i = 1, m

          k = seed / 127773

          seed = 16807 * ( seed - k * 127773 ) - k * 2836

          if ( seed .lt. 0 ) then
            seed = seed + 2147483647
          end if

          r(i,j) = dble ( seed ) * 4.656612875D-10

        end do
      end do

      return
      end
      subroutine r8vec_copy ( n, a1, a2 )

c*********************************************************************72
c
cc R8VEC_COPY copies an R8VEC.
c
c  Discussion:
c
c    An R8VEC is a vector of R8 values.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    13 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the length of the vectors.
c
c    Input, double precision A1(N), the vector to be copied.
c
c    Output, double precision A2(N), a copy of A1.
c
      implicit none

      integer n

      double precision a1(n)
      double precision a2(n)
      integer i

      do i = 1, n
        a2(i) = a1(i)
      end do

      return
      end
      subroutine r8vec_direct_product ( factor_index, factor_order,
     &  factor_value, factor_num, point_num, x )

c*********************************************************************72
c
cc R8VEC_DIRECT_PRODUCT creates a direct product of R8VEC's.
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
c
c    To explain what is going on here, suppose we had to construct
c    a multidimensional quadrature rule as the product of K rules
c    for 1D quadrature.
c
c    The product rule will be represented as a list of points and weights.
c
c    The J-th item in the product rule will be associated with
c      item J1 of 1D rule 1,
c      item J2 of 1D rule 2,
c      ...,
c      item JK of 1D rule K.
c
c    In particular,
c      X(J) = ( X(1,J1), X(2,J2), ..., X(K,JK))
c    and
c      W(J) = W(1,J1) * W(2,J2) * ... * W(K,JK)
c
c    So we can construct the quadrature rule if we can properly
c    distribute the information in the 1D quadrature rules.
c
c    This routine carries out that task for the abscissas X.
c
c    Another way to do this would be to compute, one by one, the
c    set of all possible indices (J1,J2,...,JK), and then index
c    the appropriate information.  An advantage of the method shown
c    here is that you can process the K-th set of information and
c    then discard it.
c
c  Example:
c
c    Rule 1:
c      Order = 4
c      X(1:4) = ( 1, 2, 3, 4 )
c
c    Rule 2:
c      Order = 3
c      X(1:3) = ( 10, 20, 30 )
c
c    Rule 3:
c      Order = 2
c      X(1:2) = ( 100, 200 )
c
c    Product Rule:
c      Order = 24
c      X(1:24) =
c        ( 1, 10, 100 )
c        ( 2, 10, 100 )
c        ( 3, 10, 100 )
c        ( 4, 10, 100 )
c        ( 1, 20, 100 )
c        ( 2, 20, 100 )
c        ( 3, 20, 100 )
c        ( 4, 20, 100 )
c        ( 1, 30, 100 )
c        ( 2, 30, 100 )
c        ( 3, 30, 100 )
c        ( 4, 30, 100 )
c        ( 1, 10, 200 )
c        ( 2, 10, 200 )
c        ( 3, 10, 200 )
c        ( 4, 10, 200 )
c        ( 1, 20, 200 )
c        ( 2, 20, 200 )
c        ( 3, 20, 200 )
c        ( 4, 20, 200 )
c        ( 1, 30, 200 )
c        ( 2, 30, 200 )
c        ( 3, 30, 200 )
c        ( 4, 30, 200 )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer FACTOR_INDEX, the index of the factor being processed.
c    The first factor processed must be factor 1!
c
c    Input, integer FACTOR_ORDER, the order of the factor.
c
c    Input, double precision FACTOR_VALUE(FACTOR_ORDER), the factor values
c    for factor FACTOR_INDEX.
c
c    Input, integer FACTOR_NUM, the number of factors.
c
c    Input, integer POINT_NUM, the number of elements in the direct product.
c
c    Input/output, double precision X(FACTOR_NUM,POINT_NUM), the elements of the
c    direct product, which are built up gradually.  Before the first call,
c    X might be set to 0.  After each factor has been input, X should
c    have the correct value.
c
c  Local Parameters:
c
c    Local, integer START, the first location of a block of values to set.
c
c    Local, integer CONTIG, the number of consecutive values to set.
c
c    Local, integer SKIP, the distance from the current value of START
c    to the next location of a block of values to set.
c
c    Local, integer REP, the number of blocks of values to set.
c
      implicit none

      integer factor_num
      integer factor_order
      integer point_num

      integer contig
      integer factor_index
      double precision factor_value(factor_order)
      integer i
      integer j
      integer k
      integer rep
      integer skip
      integer start
      double precision x(factor_num,point_num)

      save contig
      save rep
      save skip

      data contig / 0 /
      data rep / 0 /
      data skip / 0 /

      if ( factor_index .eq. 1 ) then
        contig = 1
        skip = 1
        rep = point_num
      end if

      rep = rep / factor_order
      skip = skip * factor_order

      do j = 1, factor_order

        start = 1 + ( j - 1 ) * contig

        do k = 1, rep
          do i = start, start+contig-1
            x(factor_index,i) = factor_value(j)
          end do
          start = start + skip
        end do

      end do

      contig = contig * factor_order

      return
      end
      subroutine r8vec_direct_product2 ( factor_index, factor_order,
     &  factor_value, factor_num, point_num, w )

c*********************************************************************72
c
cc R8VEC_DIRECT_PRODUCT2 creates a direct product of R8VEC's.
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
c
c    To explain what is going on here, suppose we had to construct
c    a multidimensional quadrature rule as the product of K rules
c    for 1D quadrature.
c
c    The product rule will be represented as a list of points and weights.
c
c    The J-th item in the product rule will be associated with
c      item J1 of 1D rule 1,
c      item J2 of 1D rule 2,
c      ...,
c      item JK of 1D rule K.
c
c    In particular,
c      X(J) = ( X(1,J1), X(2,J2), ..., X(K,JK))
c    and
c      W(J) = W(1,J1) * W(2,J2) * ... * W(K,JK)
c
c    So we can construct the quadrature rule if we can properly
c    distribute the information in the 1D quadrature rules.
c
c    This routine carries out the task involving the weights W.
c
c    Another way to do this would be to compute, one by one, the
c    set of all possible indices (J1,J2,...,JK), and then index
c    the appropriate information.  An advantage of the method shown
c    here is that you can process the K-th set of information and
c    then discard it.
c
c  Example:
c
c    Rule 1:
c      Order = 4
c      W(1:4) = ( 2, 3, 5, 7 )
c
c    Rule 2:
c      Order = 3
c      W(1:3) = ( 11, 13, 17 )
c
c    Rule 3:
c      Order = 2
c      W(1:2) = ( 19, 23 )
c
c    Product Rule:
c      Order = 24
c      W(1:24) =
c        ( 2 * 11 * 19 )
c        ( 3 * 11 * 19 )
c        ( 4 * 11 * 19 )
c        ( 7 * 11 * 19 )
c        ( 2 * 13 * 19 )
c        ( 3 * 13 * 19 )
c        ( 5 * 13 * 19 )
c        ( 7 * 13 * 19 )
c        ( 2 * 17 * 19 )
c        ( 3 * 17 * 19 )
c        ( 5 * 17 * 19 )
c        ( 7 * 17 * 19 )
c        ( 2 * 11 * 23 )
c        ( 3 * 11 * 23 )
c        ( 5 * 11 * 23 )
c        ( 7 * 11 * 23 )
c        ( 2 * 13 * 23 )
c        ( 3 * 13 * 23 )
c        ( 5 * 13 * 23 )
c        ( 7 * 13 * 23 )
c        ( 2 * 17 * 23 )
c        ( 3 * 17 * 23 )
c        ( 5 * 17 * 23 )
c        ( 7 * 17 * 23 )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 August 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer FACTOR_INDEX, the index of the factor being processed.
c    The first factor processed must be factor 1!
c
c    Input, integer FACTOR_ORDER, the order of the factor.
c
c    Input, double precision FACTOR_VALUE(FACTOR_ORDER), the factor values
c    for factor FACTOR_INDEX.
c
c    Input, integer FACTOR_NUM, the number of factors.
c
c    Input, integer POINT_NUM, the number of elements in the direct product.
c
c    Input/output, double precision W(POINT_NUM), the elements of the
c    direct product, which are built up gradually.  Before the first call,
c    W should be set to 1.
c
c  Local Parameters:
c
c    Local, integer START, the first location of a block of values to set.
c
c    Local, integer CONTIG, the number of consecutive values to set.
c
c    Local, integer SKIP, the distance from the current value of START
c    to the next location of a block of values to set.
c
c    Local, integer REP, the number of blocks of values to set.
c
      implicit none

      integer factor_num
      integer factor_order
      integer point_num

      integer contig
      integer factor_index
      double precision factor_value(factor_order)
      integer i
      integer j
      integer k
      integer rep
      integer skip
      integer start
      double precision w(point_num)

      save contig
      save rep
      save skip

      data contig / 0 /
      data rep / 0 /
      data skip / 0 /

      if ( factor_index .eq. 1 ) then
        contig = 1
        skip = 1
        rep = point_num
      end if

      rep = rep / factor_order
      skip = skip * factor_order

      do j = 1, factor_order

        start = 1 + ( j - 1 ) * contig

        do k = 1, rep
          do i = start, start+contig-1
            w(i) = w(i) * factor_value(j)
          end do
          start = start + skip
        end do

      end do

      contig = contig * factor_order

      return
      end
      function r8vec_dot_product ( n, v1, v2 )

c*********************************************************************72
c
cc R8VEC_DOT_PRODUCT finds the dot product of a pair of R8VEC's.
c
c  Discussion:
c
c    An R8VEC is a vector of R8 values.
c
c    In FORTRAN90, the system routine DOT_PRODUCT should be called
c    directly.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    27 May 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the dimension of the vectors.
c
c    Input, double precision V1(N), V2(N), the vectors.
c
c    Output, double precision R8VEC_DOT_PRODUCT, the dot product.
c
      implicit none

      integer n

      integer i
      double precision r8vec_dot_product
      double precision v1(n)
      double precision v2(n)
      double precision value

      value = 0.0D+00
      do i = 1, n
        value = value + v1(i) * v2(i)
      end do

      r8vec_dot_product = value

      return
      end
      subroutine r8vec_normal_01 ( n, seed, x )

c*********************************************************************72
c
cc R8VEC_NORMAL_01 returns a unit pseudonormal R8VEC.
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
c
c    The standard normal probability distribution function (PDF) has
c    mean 0 and standard deviation 1.
c
c    This routine can generate a vector of values on one call.  It
c    has the feature that it should provide the same results
c    in the same order no matter how we break up the task.
c
c    The Box-Muller method is used, which is efficient, but
c    generates an even number of values each time.  On any call
c    to this routine, an even number of new values are generated.
c    Depending on the situation, one value may be left over.
c    In that case, it is saved for the next call.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 July 2006
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of values desired.  If N is negative,
c    then the code will flush its internal memory; in particular,
c    if there is a saved value to be used on the next call, it is
c    instead discarded.  This is useful if the user has reset the
c    random number seed, for instance.
c
c    Input/output, integer SEED, a seed for the random number generator.
c
c    Output, double precision X(N), a sample of the standard normal PDF.
c
c  Local parameters:
c
c    Local, integer MADE, records the number of values that have
c    been computed.  On input with negative N, this value overwrites
c    the return value of N, so the user can get an accounting of
c    how much work has been done.
c
c    Local, integer SAVED, is 0 or 1 depending on whether there is a
c    single saved value left over from the previous call.
c
c    Local, integer X_LO_INDEX, X_HI_INDEX, records the range of entries of
c    X that we need to compute.  This starts off as 1:N, but is adjusted
c    if we have a saved value that can be immediately stored in X(1),
c    and so on.
c
c    Local, double precision Y, the value saved from the previous call, if
c    SAVED is 1.
c
      implicit none

      integer n

      integer i
      integer m
      integer made
      double precision pi
      parameter ( pi = 3.141592653589793D+00 )
      double precision r(2)
      double precision r8_uniform_01
      integer saved
      integer seed
      double precision x(n)
      integer x_hi_index
      integer x_lo_index
      double precision y

      save made
      save saved
      save y

      data made / 0 /
      data saved / 0 /
      data y / 0.0D+00 /
c
c  I'd like to allow the user to reset the internal data.
c  But this won't work properly if we have a saved value Y.
c  I'm making a crock option that allows the user to signal
c  explicitly that any internal memory should be flushed,
c  by passing in a negative value for N.
c
      if ( n .lt. 0 ) then
        n = made
        made = 0
        saved = 0
        y = 0.0D+00
        return
      else if ( n .eq. 0 ) then
        return
      end if
c
c  Record the range of X we need to fill in.
c
      x_lo_index = 1
      x_hi_index = n
c
c  Use up the old value, if we have it.
c
      if ( saved .eq. 1 ) then
        x(1) = y
        saved = 0
        x_lo_index = 2
      end if
c
c  Maybe we don't need any more values.
c
      if ( x_hi_index - x_lo_index + 1 .eq. 0 ) then
c
c  If we need just one new value, do that here to avoid null arrays.
c
      else if ( x_hi_index - x_lo_index + 1 .eq. 1 ) then

        r(1) = r8_uniform_01 ( seed )

        if ( r(1) .eq. 0.0D+00 ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'R8VEC_NORMAL_01 - Fatal errorc'
          write ( *, '(a)' ) '  R8_UNIFORM_01 returned a value of 0.'
          stop 1
        end if

        r(2) = r8_uniform_01 ( seed )

        x(x_hi_index) =
     &           sqrt ( -2.0D+00 * log ( r(1) ) )
     &           * cos ( 2.0D+00 * pi * r(2) )
        y =      sqrt ( -2.0D+00 * log ( r(1) ) )
     &           * sin ( 2.0D+00 * pi * r(2) )

        saved = 1

        made = made + 2
c
c  If we require an even number of values, that's easy.
c
      else if ( mod ( x_hi_index - x_lo_index + 1, 2 ) .eq. 0 ) then

        do i = x_lo_index, x_hi_index, 2

          call r8vec_uniform_01 ( 2, seed, r )

          x(i) =
     &      sqrt ( -2.0D+00 * log ( r(1) ) )
     &      * cos ( 2.0D+00 * pi * r(2) )

          x(i+1) =
     &      sqrt ( -2.0D+00 * log ( r(1) ) )
     &      * sin ( 2.0D+00 * pi * r(2) )

        end do

        made = made + x_hi_index - x_lo_index + 1
c
c  If we require an odd number of values, we generate an even number,
c  and handle the last pair specially, storing one in X(N), and
c  saving the other for later.
c
      else

        do i = x_lo_index, x_hi_index - 1, 2

          call r8vec_uniform_01 ( 2, seed, r )

          x(i) =
     &      sqrt ( -2.0D+00 * log ( r(1) ) )
     &      * cos ( 2.0D+00 * pi * r(2) )

          x(i+1) =
     &      sqrt ( -2.0D+00 * log ( r(1) ) )
     &      * sin ( 2.0D+00 * pi * r(2) )

        end do

        call r8vec_uniform_01 ( 2, seed, r )

        x(n) = sqrt ( -2.0D+00 * log ( r(1) ) )
     &    * cos ( 2.0D+00 * pi * r(1) )

        y = sqrt ( -2.0D+00 * log ( r(2) ) )
     &    * sin ( 2.0D+00 * pi * r(2) )

        saved = 1

        made = made + x_hi_index - x_lo_index + 2

      end if

      return
      end
      subroutine r8vec_print ( n, a, title )

c*********************************************************************72
c
cc R8VEC_PRINT prints an R8VEC.
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
c    12 January 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of components of the vector.
c
c    Input, double precision A(N), the vector to be printed.
c
c    Input, character * ( * ) TITLE, a title.
c
      implicit none

      integer n

      double precision a(n)
      integer i
      character ( len = * ) title

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
      write ( *, '(a)' ) ' '
      do i = 1, n
        write ( *, '(2x,i8,a,1x,g16.8)' ) i, ':', a(i)
      end do

      return
      end
      function r8vec_sum ( n, v1 )

c*********************************************************************72
c
cc R8VEC_SUM sums the entries of an R8VEC.
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
c
c    In FORTRAN90, the system routine SUM should be called
c    directly.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    22 July 2008
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the dimension of the vectors.
c
c    Input, double precision V1(N), the vector.
c
c    Output, double precision R8VEC_SUM, the sum of the entries.
c
      implicit none

      integer n

      integer i
      double precision r8vec_sum
      double precision v1(n)
      double precision value

      value = 0.0D+00
      do i = 1, n
        value = value + v1(i)
      end do

      r8vec_sum = value

      return
      end
      subroutine r8vec_uniform_01 ( n, seed, r )

c*********************************************************************72
c
cc R8VEC_UNIFORM_01 returns a unit pseudorandom R8VEC.
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
c    17 July 2006
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
c    Input, integer N, the number of entries in the vector.
c
c    Input/output, integer SEED, the "seed" value, which should NOT be 0.
c    On output, SEED has been updated.
c
c    Output, double precision R(N), the vector of pseudorandom values.
c
      implicit none

      integer n

      integer i
      integer k
      integer seed
      double precision r(n)

      do i = 1, n

        k = seed / 127773

        seed = 16807 * ( seed - k * 127773 ) - k * 2836

        if ( seed .lt. 0 ) then
          seed = seed + 2147483647
        end if

        r(i) = dble ( seed ) * 4.656612875D-10

      end do

      return
      end
      subroutine rule_adjust ( a, b, c, d, n, x, w )

c*********************************************************************72
c
cc RULE_ADJUST adjusts a 1D quadrature rule from [A,B] to [C,D].
c
c  Discussion:
c
c    This function is only appropriate for cases involving finite intervals
c    and a uniform weighting function.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    21 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, double precision A, B, the left and right endpoints of the
c    original interval.
c
c    Input, double precision C, D, the left and right endpoints of the
c    new interval.
c
c    Input, integer N, the order of the rule.
c    1 <= N.
c
c    Input/output, double precision X(N), the abscissas.
c
c    Input/output, double precision W(N), the weights.
c
      implicit none

      integer n

      double precision a
      double precision b
      double precision c
      double precision d
      integer i
      double precision s
      double precision w(n)
      double precision x(n)

      do i = 1, n
        x(i) = ( ( b - x(i)     ) * c   
     &         + (     x(i) - a ) * d ) 
     &         / ( b        - a )
      end do

      s = ( d - c ) / ( b - a )

      do i = 1, n
        w(i) = s * w(i)
      end do

      return
      end
      subroutine rule_sort ( m, n, x, w )

c*********************************************************************72
c
cc RULE_SORT sorts a multidimensional quadrature rule.
c
c  Discussion:
c
c    This routine simply reindexes the items in the rule so that the points
c    occur in increasing lexicographic order.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 December 2004
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns.
c
c    Input/output, double precision X(M,N), W(N).
c    The points and weights.
c
      implicit none

      integer m
      integer n

      integer i
      integer indx
      integer isgn
      integer j1
      integer j2
      double precision t
      double precision w(n)
      double precision x(m,n)

      if ( m .le. 0 ) then
        return
      end if

      if ( n .le. 1 ) then
        return
      end if
c
c  Initialize.
c
      indx = 0
      isgn = 0
      j1 = 0
      j2 = 0
c
c  Call the external heap sorter.
c
10    continue

        call sort_heap_external ( n, indx, j1, j2, isgn )
c
c  Interchange columns J1 and J2.
c
        if ( 0 .lt. indx ) then

          do i = 1, m
            t       = x(i,j1)
            x(i,j1) = x(i,j2)
            x(i,j2) = t
          end do

          t     = w(j1)
          w(j1) = w(j2)
          w(j2) = t     
c
c  Compare columns J1 and J2.
c
        else if ( indx .lt. 0 ) then

          isgn = 0

          do i = 1, m 
     
            if ( x(i,j1) .lt. x(i,j2) ) then
              isgn = -1
              go to 20
            else if ( x(i,j2) .lt. x(i,j1) ) then
              isgn = +1
              go to 20
            end if

          end do

20        continue
c
c  The columns are sorted.
c
        else if ( indx .eq. 0 ) then

          go to 30

        end if

      go to 10

30    continue

      return
      end
      subroutine sort_heap_external ( n, indx, i, j, isgn )

c*********************************************************************72
c
cc SORT_HEAP_EXTERNAL externally sorts a list of items into ascending order.
c
c  Discussion:
c
c    The actual list of data is not passed to the routine.  Hence this
c    routine may be used to sort integers, reals, numbers, names,
c    dates, shoe sizes, and so on.  After each call, the routine asks
c    the user to compare or interchange two items, until a special
c    return value signals that the sorting is completed.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 February 2004
c
c  Author:
c
c    Original FORTRAN77 version by Albert Nijenhuis, Herbert Wilf.
c    FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Albert Nijenhuis, Herbert Wilf,
c    Combinatorial Algorithms for Computers and Calculators,
c    Academic Press, 1978,
c    ISBN: 0-12-519260-6,
c    LC: QA164.N54.
c
c  Parameters:
c
c    Input, integer N, the number of items to be sorted.
c
c    Input/output, integer INDX, the main communication signal.
c    The user must set INDX to 0 before the first call.
c    Thereafter, the user should not change the value of INDX until
c    the sorting is done.
c    On return, if INDX is
c    * greater than 0,
c      > interchange items I and J;
c      > call again.
c    * less than 0,
c      > compare items I and J;
c      > set ISGN = -1 if I .lt. J, ISGN = +1 if J .lt. I;
c      > call again.
c    * equal to 0, the sorting is done.
c
c    Output, integer I, J, the indices of two items.
c    On return with INDX positive, elements I and J should be interchanged.
c    On return with INDX negative, elements I and J should be compared, and
c    the result reported in ISGN on the next call.
c
c    Input, integer ISGN, results of comparison of elements
c    I and J. (Used only when the previous call returned INDX less than 0).
c    ISGN .le. 0 means I is less than or equal to J;
c    0 .le. ISGN means I is greater than or equal to J.
c
      implicit none

      integer i
      integer, save :: i_save = 0
      integer indx
      integer isgn
      integer j
      integer, save :: j_save = 0
      integer, save :: k = 0
      integer, save :: k1 = 0
      integer n
      integer, save :: n1 = 0
c
c  INDX = 0: This is the first call.
c
      if ( indx .eq. 0 ) then

        i_save = 0
        j_save = 0
        k = n / 2
        k1 = k
        n1 = n
c
c  INDX .lt. 0: The user is returning the results of a comparison.
c
      else if ( indx .lt. 0 ) then

        if ( indx .eq. -2 ) then

          if ( isgn .lt. 0 ) then
            i_save = i_save + 1
          end if

          j_save = k1
          k1 = i_save
          indx = -1
          i = i_save
          j = j_save
          return

        end if

        if ( 0 .lt. isgn ) then
          indx = 2
          i = i_save
          j = j_save
          return
        end if

        if ( k .le. 1 ) then

          if ( n1 .eq. 1 ) then
            i_save = 0
            j_save = 0
            indx = 0
          else
            i_save = n1
            n1 = n1 - 1
            j_save = 1
            indx = 1
          end if

          i = i_save
          j = j_save
          return

        end if

        k = k - 1
        k1 = k
c
c  0 .lt. INDX, the user was asked to make an interchange.
c
      else if ( indx .eq. 1 ) then

        k1 = k

      end if

      do

        i_save = 2 * k1

        if ( i_save .eq. n1 ) then
          j_save = k1
          k1 = i_save
          indx = -1
          i = i_save
          j = j_save
          return
        else if ( i_save .le. n1 ) then
          j_save = i_save + 1
          indx = -2
          i = i_save
          j = j_save
          return
        end if

        if ( k .le. 1 ) then
          exit
        end if

        k = k - 1
        k1 = k

      end do

      if ( n1 .eq. 1 ) then
        i_save = 0
        j_save = 0
        indx = 0
        i = i_save
        j = j_save
      else
        i_save = n1
        n1 = n1 - 1
        j_save = 1
        indx = 1
        i = i_save
        j = j_save
      end if

      return
      end
      subroutine symmetric_sparse_size ( nr, dim, nodes, x0, nr2 )

c*********************************************************************72
c
cc SYMMETRIC_SPARSE_SIZE sizes a symmetric sparse rule.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 May 2012
c
c  Author:
c
c    John Burkardt.
c
c  Reference:
c
c    Florian Heiss, Viktor Winschel,
c    Likelihood approximation by numerical integration on sparse grids,
c    Journal of Econometrics,
c    Volume 144, 2008, pages 62-80.
c
c  Parameters:
c 
c    Input, integer DIM, the dimension.
c
c    Input, integer NR, the dimension of the rule in the 
c    positive orthant.
c
c    Input, double precision NODES(NR,DIM), the nodes for the positive orthant.
c
c    Input, double precision X0, the point of symmetry for the 1D rule, 
c    typically 0.
c
c    Output, integer NR2, the dimension of the rule when "unfolded" to the 
c    full space.
c
      integer dim
      integer nr

      integer count
      integer j
      double precision nodes(nr,dim)
      integer nr2
      integer r
      double precision x0
c
c  Count the size of the full rule.
c
        nr2 = 0

        do r = 1, nr
          count = 1
          do j = 1, dim
            if ( nodes(r,j) .ne. x0 ) then
              count = 2 * count
            end if
          end do
          nr2 = nr2 + count
        end do

      return
      end
      subroutine tensor_product ( d, order1d, n1d, x1d, w1d, n, xnd, 
     &  wnd )

c*********************************************************************72
c
cc TENSOR_PRODUCT generates a tensor product quadrature rule.
c
c  Discussion:
c
c    The Kronecker product of an K by L matrix A and an M by N matrix B
c    is the K*M by L*N matrix formed by
c
c      a(1,1) * B,  a(1,2) * B,  ..., a(1,l) * B
c      a(2,1) * B,  a(2,2) * B,  ..., a(2,l) * B
c      ..........   ..........   .... ..........
c      a(k,1) * B,  a(k,2) * B,  ..., a(k,l) * B
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 April 2012
c
c  Author:
c
c    Original MATLAB version by Florian Heiss, Viktor Winschel.
c    FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Florian Heiss, Viktor Winschel,
c    Likelihood approximation by numerical integration on sparse grids,
c    Journal of Econometrics,
c    Volume 144, 2008, pages 62-80.
c
c  Parameters:
c
c    Input, integer D, the spatial dimension.
c
c    Input, integer ORDER1D(D), the order of each 1D rule.
c
c    Input, integer N1D, the number of 1D items.
c
c    Input, double precision X1D(N1D), the 1D nodes.
c
c    Input, double precision W1D(N1D), the 1D weights.
c
c    Input, integer N, the number of N-dimensional items.
c
c    Output, double precision XND(D,N), the nodes.
c
c    Output, double precision WND(N), the weights.
c
      implicit none

      integer d
      integer n
      integer n1d

      integer i
      integer i1
      integer i2
      integer order1d(d)
      double precision w1d(n1d)
      double precision wnd(n)
      double precision x1d(n1d)
      double precision xnd(d,n)
c
c  Compute the weights.
c
      i2 = 0
      do i = 1, d
        i1 = i2 + 1
        i2 = i2 + order1d(i)
        call r8vec_direct_product2 ( i, order1d(i), w1d(i1:i2), d, 
     &    n, wnd )
      end do
c
c  Compute the points.
c
      i2 = 0
      do i = 1, d
        i1 = i2 + 1
        i2 = i2 + order1d(i)
        call r8vec_direct_product ( i, order1d(i), x1d(i1:i2), d, 
     &    n, xnd )
      end do

      return
      end
      subroutine tensor_product_cell ( nc, xc, wc, dim, nr, roff, np, 
     &  xp, wp )

c*********************************************************************72
c
cc TENSOR_PRODUCT_CELL generates a tensor product quadrature rule.
c
c  Discussion:
c
c    The Kronecker product of an K by L matrix A and an M by N matrix B
c    is the K*M by L*N matrix formed by
c
c      a(1,1) * B,  a(1,2) * B,  ..., a(1,l) * B
c      a(2,1) * B,  a(2,2) * B,  ..., a(2,l) * B
c      ..........   ..........   .... ..........
c      a(k,1) * B,  a(k,2) * B,  ..., a(k,l) * B
c
c    The 1D factors are stored in a kind of cell array structure.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    01 December 2012
c
c  Author:
c
c    John Burkardt.
c
c  Parameters:
c
c    Input, integer NC, the number of items in the cell arrays.
c
c    Input, double precision XC(NC), a cell array containing points for 
c    1D rules.
c
c    Input, double precision WC(NC), a cell array containing weights for
c    1D rules.
c
c    Input, integer DIM, the spatial dimension.
c
c    Input, integer NR(DIM), the length of each row of the 
c    cell array.
c
c    Input, integer ROFF(DIM+1), offsets for the cell arrays.
c
c    Input, integer NP, the number of points in the product rule.
c
c    Output, double precision XP(DIM,NP), the nodes.
c
c    Output, double precision WP(NP), the weights.
c
      implicit none

      integer n1d_max
      parameter ( n1d_max = 100 )

      integer dim
      integer nc
      integer np

      integer i
      integer n1d
      integer nr(dim)
      integer roff(dim+1)
      double precision w1d(n1d_max)
      double precision wc(nc)
      double precision wp(np)
      double precision x1d(n1d_max)
      double precision xc(nc)
      double precision xp(dim,np)
c
c  Compute the weights.
c
      do i = 1, dim
        n1d = nr(i)
        call r8cvv_rget ( nc, wc, dim, roff, i, w1d )
        call r8vec_direct_product2 ( i, n1d, w1d, dim, np, wp )
      end do
c
c  Compute the points.
c
      do i = 1, dim
        n1d = nr(i)
        call r8cvv_rget ( nc, xc, dim, roff, i, x1d )
        call r8vec_direct_product ( i, n1d, x1d, dim, np, xp )
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
