      subroutine daub_coefficients ( n, c )

c*********************************************************************72
c
cc DAUB_COEFFICIENTS returns a set of Daubechies coefficients.
c
c  Discussion:
c
c    Often, the uses to which these coefficients are applied require that they
c    be rescaled, by being multiplied by sqrt ( 2 ).
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the order of the coefficient set.
c    2 <= N <= 20, and N must be even.
c
c    Output, double precision C(N), the coefficients.
c
      implicit none

      integer n

      double precision c(n)
      double precision c02(2)
      double precision c04(4)
      double precision c06(6)
      double precision c08(8)
      double precision c10(10)
      double precision c12(12)
      double precision c14(14)
      double precision c16(16)
      double precision c18(18)
      double precision c20(20)

      save c02
      save c04
      save c06
      save c08
      save c10
      save c12
      save c14
      save c16
      save c18
      save c20

      data c02 /
     &  7.071067811865475D-01, 
     &  7.071067811865475D-01 /
      data c04 /
     &   0.4829629131445341D+00, 
     &   0.8365163037378079D+00, 
     &   0.2241438680420133D+00, 
     & - 0.1294095225512603D+00 /
      data c06 /
     &   0.3326705529500826D+00, 
     &   0.8068915093110925D+00, 
     &   0.4598775021184915D+00, 
     & - 0.1350110200102545D+00, 
     & - 0.8544127388202666D-01, 
     &   0.3522629188570953D-01 /
      data c08 /
     &  0.2303778133088965D+00, 
     &  0.7148465705529156D+00, 
     &  0.6308807679298589D+00, 
     & -0.2798376941685985D-01, 
     & -0.1870348117190930D+00, 
     &  0.3084138183556076D-01, 
     &  0.3288301166688519D-01, 
     & -0.1059740178506903D-01 /
      data c10 /
     &  0.1601023979741929D+00, 
     &  0.6038292697971896D+00, 
     &  0.7243085284377729D+00, 
     &  0.1384281459013207D+00, 
     & -0.2422948870663820D+00, 
     & -0.3224486958463837D-01, 
     &  0.7757149384004571D-01, 
     & -0.6241490212798274D-02, 
     & -0.1258075199908199D-01, 
     &  0.3335725285473771D-02 /
      data c12 /
     &  0.1115407433501094D+00, 
     &  0.4946238903984530D+00, 
     &  0.7511339080210953D+00, 
     &  0.3152503517091976D+00, 
     & -0.2262646939654398D+00, 
     & -0.1297668675672619D+00, 
     &  0.9750160558732304D-01, 
     &  0.2752286553030572D-01, 
     & -0.3158203931748602D-01, 
     &  0.5538422011614961D-03, 
     &  0.4777257510945510D-02, 
     & -0.1077301085308479D-02 /
      data c14 /
     &   7.785205408500917D-02, 
     &   3.965393194819173D-01, 
     &   7.291320908462351D-01, 
     &   4.697822874051931D-01, 
     &  -1.439060039285649D-01, 
     &  -2.240361849938749D-01, 
     &   7.130921926683026D-02, 
     &   8.061260915108307D-02, 
     &  -3.802993693501441D-02, 
     &  -1.657454163066688D-02, 
     &   1.255099855609984D-02, 
     &   4.295779729213665D-04, 
     &  -1.801640704047490D-03, 
     &   3.537137999745202D-04 /
      data c16 /
     &   5.441584224310400D-02, 
     &   3.128715909142999D-01, 
     &   6.756307362972898D-01, 
     &   5.853546836542067D-01, 
     &  -1.582910525634930D-02, 
     &  -2.840155429615469D-01, 
     &   4.724845739132827D-04, 
     &   1.287474266204784D-01, 
     &  -1.736930100180754D-02, 
     &  -4.408825393079475D-02, 
     &   1.398102791739828D-02, 
     &   8.746094047405776D-03, 
     &  -4.870352993451574D-03, 
     &  -3.917403733769470D-04, 
     &   6.754494064505693D-04, 
     &  -1.174767841247695D-04 /
      data c18 /
     &   3.807794736387834D-02, 
     &   2.438346746125903D-01, 
     &   6.048231236901111D-01, 
     &   6.572880780513005D-01, 
     &   1.331973858250075D-01, 
     &  -2.932737832791749D-01, 
     &  -9.684078322297646D-02, 
     &   1.485407493381063D-01, 
     &   3.072568147933337D-02, 
     &  -6.763282906132997D-02, 
     &   2.509471148314519D-04, 
     &   2.236166212367909D-02, 
     &  -4.723204757751397D-03, 
     &  -4.281503682463429D-03, 
     &   1.847646883056226D-03, 
     &   2.303857635231959D-04, 
     &  -2.519631889427101D-04, 
     &   3.934732031627159D-05 /
      data c20 /
     &   2.667005790055555D-02, 
     &   1.881768000776914D-01, 
     &   5.272011889317255D-01, 
     &   6.884590394536035D-01, 
     &   2.811723436605774D-01, 
     &  -2.498464243273153D-01, 
     &  -1.959462743773770D-01, 
     &   1.273693403357932D-01, 
     &   9.305736460357235D-02, 
     &  -7.139414716639708D-02, 
     &  -2.945753682187581D-02, 
     &   3.321267405934100D-02, 
     &   3.606553566956169D-03, 
     &  -1.073317548333057D-02, 
     &   1.395351747052901D-03, 
     &   1.992405295185056D-03, 
     &  -6.858566949597116D-04, 
     &  -1.164668551292854D-04, 
     &   9.358867032006959D-05, 
     &  -1.326420289452124D-05 /

      if ( n .eq. 2 ) then

        call r8vec_copy ( n, c02, c )

      else if ( n .eq. 4 ) then

        call r8vec_copy ( n, c04, c )

      else if ( n .eq. 6 ) then

        call r8vec_copy ( n, c06, c )

      else if ( n .eq. 8 ) then

        call r8vec_copy ( n, c08, c )

      else if ( n .eq. 10 ) then

        call r8vec_copy ( n, c10, c )

      else if ( n .eq. 12 ) then

        call r8vec_copy ( n, c12, c )

      else if ( n .eq. 14 ) then

        call r8vec_copy ( n, c14, c )

      else if ( n .eq. 16 ) then

        call r8vec_copy ( n, c16, c )

      else if ( n .eq. 18 ) then

        call r8vec_copy ( n, c18, c )

      else if ( n .eq. 20 ) then

        call r8vec_copy ( n, c20, c )

      else

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'DAUB_COEFFICIENTS - Fatal error!'
        write ( *, '(a,i8)' ) '  Value of N = ', n
        write ( *, '(a)' ) 
     &    '  Legal values are 2, 4, 6, 8, 10, 12, 14, 16, 18, 20.'
        stop 1

      end if

      return
      end
      subroutine daub2_matrix ( n, a )

c*********************************************************************72
c
cc DAUB2_MATRIX returns the DAUB2 matrix.
c
c  Discussion:
c
c    The DAUB2 matrix is the Daubechies wavelet transformation matrix
c    with 2 coefficients.
c
c    The DAUB2 matrix is also known as the Haar matrix.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the order of the matrix.
c    N must be at least 2 and a multiple of 2.
c
c    Output, double precision A(N,N), the matrix.
c
      implicit none

      integer n
      integer p
      parameter ( p = 1 )

      double precision a(n,n)
      double precision c(0:p)
      integer i
      integer j
      integer i4_wrap
      integer m

      save c

      data c /
     &  7.071067811865475D-01,     7.071067811865475D-01 /

      if ( n .lt. 2 .or. mod ( n, 2 ) .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'DAUB2_MATRIX - Fatal error!'
        write ( *, '(a)' ) 
     &    '  Order N must be at least 2 and a multiple of 2.'
        stop 1
      end if

      do j = 1, n
        do i = 1, n
          a(i,j) = 0.0D+00
        end do
      end do

      m = n / 2

      do i = 1, n - 1, 2

        a(i,i)     =   c(0)
        a(i,i+1)   =   c(1)

        a(i+1,i)   =   c(1)
        a(i+1,i+1) = - c(0)

      end do

      return
      end
      subroutine daub2_transform ( n, x, y )

c*********************************************************************72
c
cc DAUB2_TRANSFORM computes the DAUB2 transform of a vector.
c
c  Discussion:
c
c    DAUB2 is better known as the Haar transform.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the dimension of the vector.
c    N must be a power of 2.
c
c    Input, double precision X(N), the vector to be transformed.
c
c    Output, double precision Y(N), the transformed vector.
c
      implicit none

      integer n
      integer p
      parameter ( p = 1 )

      double precision c(0:p)
      integer i
      integer m
      double precision x(n)
      double precision y(n)
      double precision z(n)

      save c

      data c /
     &  7.071067811865475D-01, 
     &  7.071067811865475D-01 /

      do i = 1, n
        y(i) = x(i)
      end do

      do i = 1, n
        z(i) = 0.0D+00
      end do

      m = n

10    continue

      if ( 2 .le. m ) then

        m = m / 2

        do i = 1, m
          z(i)   = c(0) * ( y(2*i-1) + y(2*i) )
          z(i+m) = c(1) * ( y(2*i-1) - y(2*i) )
        end do

        do i = 1, 2 * m
          y(i) = z(i)
        end do

        go to 10

      end if

      return
      end
      subroutine daub2_transform_inverse ( n, y, x )

c*********************************************************************72
c
cc DAUB2_TRANSFORM_INVERSE inverts the DAUB2 transform of a vector.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the dimension of the vector.
c    N must be a power of 2.
c
c    Input, double precision Y(N), the transformed vector.
c
c    Output, double precision X(N), the original vector.
c
      implicit none

      integer n
      integer p
      parameter ( p = 1 )

      double precision c(0:p)
      integer i
      integer m
      double precision x(n)
      double precision y(n)
      double precision z(n)

      save c

      data c /
     &  7.071067811865475D-01, 
     &  7.071067811865475D-01 /

      do i = 1, n
        x(i) = y(i)
      end do

      do i = 1, n
        z(i) = 0.0D+00
      end do

      m = 1

10    continue

      if ( m * 2 .le. n ) then

        do i = 1, m
          z(2*i-1) = c(0) * ( x(i) + x(i+m) )
          z(2*i)   = c(1) * ( x(i) - x(i+m) )
        end do

        do i = 1, 2 * m
          x(i) = z(i)
        end do

        m = m * 2

        go to 10

      end if

      return
      end
      subroutine daub4_matrix ( n, a )

c*********************************************************************72
c
cc DAUB4_MATRIX returns the DAUB4 matrix.
c
c  Discussion:
c
c    The DAUB4 matrix is the Daubechies wavelet transformation matrix
c    with 4 coefficients.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the order of the matrix.
c    N must be at least 4 and a multiple of 2.
c
c    Output, double precision A(N,N), the matrix.
c
      implicit none

      integer n
      integer p
      parameter ( p = 3 )

      double precision a(n,n)
      double precision c(0:p)
      integer i
      integer i4_wrap
      integer j
      integer m

      save c

      data c /
     &  0.4829629131445341D+00, 
     &  0.8365163037378079D+00, 
     &  0.2241438680420133D+00, 
     & -0.1294095225512603D+00 /

      if ( n .lt. 4 .or. mod ( n, 2 ) .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'DAUB4_MATRIX - Fatal error!'
        write ( *, '(a)' ) 
     &    '  Order N must be at least 4 and a multiple of 2.'
        stop 1
      end if

       do j = 1, n
        do i = 1, n
          a(i,j) = 0.0D+00
        end do
      end do

      do i = 1, n - 1, 2

        a(i,i)                  =   c(0)
        a(i,i+1)                =   c(1)
        a(i,i4_wrap(i+2,1,n))   =   c(2)
        a(i,i4_wrap(i+3,1,n))   =   c(3)

        a(i+1,i)                =   c(3)
        a(i+1,i+1)              = - c(2)
        a(i+1,i4_wrap(i+2,1,n)) =   c(1)
        a(i+1,i4_wrap(i+3,1,n)) = - c(0)

      end do

      return
      end
      subroutine daub4_transform ( n, x, y )

c*********************************************************************72
c
cc DAUB4_TRANSFORM computes the DAUB4 transform of a vector.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the dimension of the vector.
c    N must be a power of 2 and at least 4.
c
c    Input, double precision X(N), the vector to be transformed.
c
c    Output, double precision Y(N), the transformed vector.
c
      implicit none

      integer n
      integer p
      parameter ( p = 3 )

      double precision c(0:p)
      integer i
      integer i4_wrap
      integer j
      integer j0
      integer j1
      integer j2
      integer j3
      integer m
      double precision x(n)
      double precision y(n)
      double precision z(n)

      save c

      data c /
     &  0.4829629131445341D+00, 
     &  0.8365163037378079D+00, 
     &  0.2241438680420133D+00, 
     & -0.1294095225512603D+00 /

      do i = 1, n
        y(i) = x(i)
      end do

      do i = 1, n
        z(i) = 0.0D+00
      end do

      m = n

10    continue

      if ( 4 .le. m ) then

        i = 1

        do j = 1, m - 1, 2

          j0 = i4_wrap ( j,     1, m )
          j1 = i4_wrap ( j + 1, 1, m )
          j2 = i4_wrap ( j + 2, 1, m )
          j3 = i4_wrap ( j + 3, 1, m )

          z(i)     = c(0) * y(j0) + c(1) * y(j1)
     &             + c(2) * y(j2) + c(3) * y(j3)

          z(i+m/2) = c(3) * y(j0) - c(2) * y(j1)
     &             + c(1) * y(j2) - c(0) * y(j3)

          i = i + 1

        end do

        do i = 1, m
          y(i) = z(i)
        end do

        m = m / 2

        go to 10

      end if

      return
      end
      subroutine daub4_transform_inverse ( n, y, x )

c*********************************************************************72
c
cc DAUB4_TRANSFORM_INVERSE inverts the DAUB4 transform of a vector.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the dimension of the vector.
c    N must be a power of 2 and at least 4.
c
c    Input, double precision Y(N), the transformed vector.
c
c    Output, double precision X(N), the original vector.
c
      implicit none

      integer n
      integer p
      parameter ( p = 3 )

      double precision c(0:p)
      integer i
      integer i0
      integer i1
      integer i2
      integer i3
      integer i4_wrap
      integer j
      integer m
      double precision x(n)
      double precision y(n)
      double precision z(n)

      save c

      data c /
     &  0.4829629131445341D+00, 
     &  0.8365163037378079D+00, 
     &  0.2241438680420133D+00, 
     & -0.1294095225512603D+00 /

      do i = 1, n
        x(i) = y(i)
      end do

      do i = 1, n
        z(i) = 0.0D+00
      end do

      m = 4

10    continue

      if ( m .le. n ) then

        j = 1

        do i = 0, m / 2 - 1

          i0 = i4_wrap ( i,                 1,         m / 2 )
          i2 = i4_wrap ( i + 1,             1,         m / 2 )

          i1 = i4_wrap ( i + m / 2,         m / 2 + 1, m     )
          i3 = i4_wrap ( i + m / 2 + 1,     m / 2 + 1, m     )

          z(j)   = c(2) * x(i0) + c(1) * x(i1)
     &           + c(0) * x(i2) + c(3) * x(i3)

          z(j+1) = c(3) * x(i0) - c(0) * x(i1)
     &           + c(1) * x(i2) - c(2) * x(i3)

          j = j + 2

        end do

        do i = 1, m
          x(i) = z(i)
        end do

        m = m * 2

        go to 10

      end if

      return
      end
      subroutine daub6_matrix ( n, a )

c*********************************************************************72
c
cc DAUB6_MATRIX returns the DAUB6 matrix.
c
c  Discussion:
c
c    The DAUB6 matrix is the Daubechies wavelet transformation matrix
c    with 6 coefficients.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the order of the matrix.
c    N must be at least 6, and a multiple of 2.
c
c    Output, double precision A(N,N), the matrix.
c
      implicit none

      integer n
      integer p
      parameter ( p = 5 )

      double precision a(n,n)
      double precision c(0:p)
      integer i
      integer i4_wrap
      integer j
      integer m

      save c

      data c /
     &   0.3326705529500826D+00, 
     &   0.8068915093110925D+00, 
     &   0.4598775021184915D+00, 
     & - 0.1350110200102545D+00, 
     & - 0.08544127388202666D+00, 
     &   0.03522629188570953D+00 /

      if ( n .lt. 6 .or. mod ( n, 2 ) .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'DAUB6_MATRIX - Fatal error!'
        write ( *, '(a)' ) '  N must be at least 6 and a multiple of 2.'
        stop 1
      end if

      do j = 1, n
        do i = 1, n
          a(i,j) = 0.0D+00
        end do
      end do

      do i = 1, n - 1, 2

        a(i,i)                  =   c(0)
        a(i,i+1)                =   c(1)
        a(i,i4_wrap(i+2,1,n))   =   c(2)
        a(i,i4_wrap(i+3,1,n))   =   c(3)
        a(i,i4_wrap(i+4,1,n))   =   c(4)
        a(i,i4_wrap(i+5,1,n))   =   c(5)

        a(i+1,i)                =   c(5)
        a(i+1,i+1)              = - c(4)
        a(i+1,i4_wrap(i+2,1,n)) =   c(3)
        a(i+1,i4_wrap(i+3,1,n)) = - c(2)
        a(i+1,i4_wrap(i+4,1,n)) =   c(1)
        a(i+1,i4_wrap(i+5,1,n)) = - c(0)

      end do

      return
      end
      subroutine daub6_transform ( n, x, y )

c*********************************************************************72
c
cc DAUB6_TRANSFORM computes the DAUB6 transform of a vector.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the dimension of the vector.
c    N must be a power of 2 and at least 4.
c
c    Input, double precision X(N), the vector to be transformed.
c
c    Output, double precision Y(N), the transformed vector.
c
      implicit none

      integer n
      integer p
      parameter ( p = 5 )

      double precision c(0:p)
      integer i
      integer i4_wrap
      integer j
      integer j0
      integer j1
      integer k
      integer m
      integer q
      double precision x(n)
      double precision y(n)
      double precision z(n)

      save c

      data c /
     &   0.3326705529500826D+00, 
     &   0.8068915093110925D+00, 
     &   0.4598775021184915D+00, 
     & - 0.1350110200102545D+00, 
     & - 0.08544127388202666D+00, 
     &   0.03522629188570953D+00 /

      do i = 1, n
        y(i) = x(i)
      end do

      m = n
      q = ( p - 1 ) / 2

10    continue

      if ( 4 .le. m ) then

        i = 1
        do j = 1, m
          z(j) = 0.0D+00
        end do

        do j = 1, m - 1, 2

          do k = 0, p - 1, 2
            j0 = i4_wrap ( j + k,     1, m )
            j1 = i4_wrap ( j + k + 1, 1, m )
            z(i)     = z(i)     + c(  k) * y(j0) + c(  k+1) * y(j1)
            z(i+m/2) = z(i+m/2) + c(p-k) * y(j0) - c(p-k-1) * y(j1)
          end do

          i = i + 1

        end do

        do j = 1, m
          y(j) = z(j)
        end do

        m = m / 2

        go to 10

      end if

      return
      end
      subroutine daub6_transform_inverse ( n, y, x )

c*********************************************************************72
c
cc DAUB6_TRANSFORM_INVERSE inverts the DAUB6 transform of a vector.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the dimension of the vector.
c    N must be a power of 2 and at least 4.
c
c    Input, double precision Y(N), the transformed vector.
c
c    Output, double precision X(N), the original vector.
c
      implicit none

      integer n
      integer p
      parameter ( p = 5 )

      double precision c(0:p)
      integer i
      integer i0
      integer i1
      integer i4_wrap
      integer j
      integer k
      integer m
      integer q
      double precision x(n)
      double precision y(n)
      double precision z(n)

      save c

      data c /
     &   0.3326705529500826D+00, 
     &   0.8068915093110925D+00, 
     &   0.4598775021184915D+00, 
     & - 0.1350110200102545D+00, 
     & - 0.08544127388202666D+00, 
     &   0.03522629188570953D+00 /

      do i = 1, n
        x(i) = y(i)
      end do

      m = 4
      q = ( p - 1 ) / 2

10    continue

      if ( m .le. n ) then

        do i = 1, m
          z(i) = 0.0D+00
        end do

        j = 1

        do i = - q + 1, m / 2 - q

          do k = 0, p - 1, 2
            i0 = i4_wrap ( i         + k / 2,     1,         m / 2 )
            i1 = i4_wrap ( i + m / 2 + k / 2,     m / 2 + 1, m     )
            z(j)   = z(j)   + c(p-k-1) * x(i0) + c(k+1) * x(i1)
            z(j+1) = z(j+1) + c(p-k)   * x(i0) - c(k)   * x(i1)
          end do

          j = j + 2

        end do

        do i = 1, m
          x(i) = z(i)
        end do

        m = m * 2

        go to 10

      end if

      return
      end
      subroutine daub8_matrix ( n, a )

c*********************************************************************72
c
cc DAUB8_MATRIX returns the DAUB8 matrix.
c
c  Discussion:
c
c    The DAUB8 matrix is the Daubechies wavelet transformation matrix
c    with 8 coefficients.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Gilbert Strang, Truong Nguyen,
c    Wavelets and Filter Banks,
c    Wellesley-Cambridge Press, 1997,
c    ISBN: 0-9614088-7-1,
c    LC: TK7872.F5S79 / QA403.3.S87
c
c  Parameters:
c
c    Input, integer N, the order of the matrix.
c    N must be at least 8, and a multiple of 2.
c
c    Output, double precision A(N,N), the matrix.
c
      implicit none

      integer n

      double precision a(n,n)
      double precision c(0:7)
      integer i
      integer i4_wrap
      integer j
      integer m

      save c

      data c /
     &  0.2303778133088964D+00, 
     &  0.7148465705529154D+00, 
     &  0.6308807679298587D+00, 
     & -0.0279837694168599D+00, 
     & -0.1870348117190931D+00, 
     &  0.0308413818355607D+00, 
     &  0.0328830116668852D+00, 
     & -0.0105974017850690D+00 /

      if ( n .lt. 8 .or. mod ( n, 2 ) .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'DAUB8_MATRIX - Fatal error!'
        write ( *, '(a)' ) '  N must be at least 8 and a multiple of 2.'
        stop 1
      end if

      do j = 1, n
        do i = 1, n
          a(i,j) = 0.0D+00
        end do
      end do

      do i = 1, n - 1, 2

        a(i,i)                  =   c(0)
        a(i,i+1)                =   c(1)
        a(i,i4_wrap(i+2,1,n))   =   c(2)
        a(i,i4_wrap(i+3,1,n))   =   c(3)
        a(i,i4_wrap(i+4,1,n))   =   c(4)
        a(i,i4_wrap(i+5,1,n))   =   c(5)
        a(i,i4_wrap(i+6,1,n))   =   c(6)
        a(i,i4_wrap(i+7,1,n))   =   c(7)

        a(i+1,i)                =   c(7)
        a(i+1,i+1)              = - c(6)
        a(i+1,i4_wrap(i+2,1,n)) =   c(5)
        a(i+1,i4_wrap(i+3,1,n)) = - c(4)
        a(i+1,i4_wrap(i+4,1,n)) =   c(3)
        a(i+1,i4_wrap(i+5,1,n)) = - c(2)
        a(i+1,i4_wrap(i+6,1,n)) =   c(1)
        a(i+1,i4_wrap(i+7,1,n)) = - c(0)

      end do

      return
      end
      subroutine daub8_transform ( n, x, y )

c*********************************************************************72
c
cc DAUB8_TRANSFORM computes the DAUB8 transform of a vector.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the dimension of the vector.
c    N must be a power of 2 and at least 4.
c
c    Input, double precision X(N), the vector to be transformed.
c
c    Output, double precision Y(N), the transformed vector.
c
      implicit none

      integer n
      integer p
      parameter ( p = 7 )

      double precision c(0:7)
      integer i
      integer i4_wrap
      integer j
      integer j0
      integer j1
      integer k
      integer m
      integer q
      double precision x(n)
      double precision y(n)
      double precision z(n)

      save c

      data c /
     &  0.2303778133088964D+00, 
     &  0.7148465705529154D+00, 
     &  0.6308807679298587D+00, 
     & -0.0279837694168599D+00, 
     & -0.1870348117190931D+00, 
     &  0.0308413818355607D+00, 
     &  0.0328830116668852D+00, 
     & -0.0105974017850690D+00 /

      do i = 1, n
        y(i) = x(i)
      end do

      m = n
      q = ( p - 1 ) / 2

10    continue

      if ( 4 .le. m ) then

        i = 1
        do j = 1, m
          z(j) = 0.0D+00
        end do

        do j = 1, m - 1, 2

          do k = 0, p - 1, 2
            j0 = i4_wrap ( j + k,     1, m )
            j1 = i4_wrap ( j + k + 1, 1, m )
            z(i)     = z(i)     + c(  k) * y(j0) + c(  k+1) * y(j1)
            z(i+m/2) = z(i+m/2) + c(p-k) * y(j0) - c(p-k-1) * y(j1)
          end do

          i = i + 1

        end do

        do i = 1, m
          y(i) = z(i)
        end do

        m = m / 2

        go to 10

      end if

      return
      end
      subroutine daub8_transform_inverse ( n, y, x )

c*********************************************************************72
c
cc DAUB8_TRANSFORM_INVERSE inverts the DAUB8 transform of a vector.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the dimension of the vector.
c    N must be a power of 2 and at least 4.
c
c    Input, double precision Y(N), the transformed vector.
c
c    Output, double precision X(N), the original vector.
c
      implicit none

      integer n
      integer p
      parameter ( p = 7 )

      double precision c(0:7)
      integer i
      integer i0
      integer i1
      integer i4_wrap
      integer j
      integer k
      integer m
      integer q
      double precision x(n)
      double precision y(n)
      double precision z(n)

      save c

      data c /
     &  0.2303778133088964D+00, 
     &  0.7148465705529154D+00, 
     &  0.6308807679298587D+00, 
     & -0.0279837694168599D+00, 
     & -0.1870348117190931D+00, 
     &  0.0308413818355607D+00, 
     &  0.0328830116668852D+00, 
     & -0.0105974017850690D+00 /

      do i = 1, n
        x(i) = y(i)
      end do

      m = 4
      q = ( p - 1 ) / 2

10    continue

      if ( m .le. n ) then

        do i = 1, m
          z(i) = 0.0D+00
        end do

        j = 1

        do i = - q + 1, m / 2 - q

          do k = 0, p - 1, 2
            i0 = i4_wrap ( i         + k / 2,     1,         m / 2 )
            i1 = i4_wrap ( i + m / 2 + k / 2,     m / 2 + 1, m     )
            z(j)   = z(j)   + c(p-k-1) * x(i0) + c(k+1) * x(i1)
            z(j+1) = z(j+1) + c(p-k)   * x(i0) - c(k)   * x(i1)
          end do

          j = j + 2

        end do

        do i = 1, m
          x(i) = z(i)
        end do

        m = m * 2

        go to 10

      end if

      return
      end
      subroutine daub10_matrix ( n, a )

c*********************************************************************72
c
cc DAUB10_MATRIX returns the DAUB10 matrix.
c
c  Discussion:
c
c    The DAUB10 matrix is the Daubechies wavelet transformation matrix
c    with 10 coefficients.
c
c    Note that in the reference, the coefficient 0.0775714938400459D+00
c    is given incorrectly, with the "8" misrepresented as a "0".
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Gilbert Strang, Truong Nguyen,
c    Wavelets and Filter Banks,
c    Wellesley-Cambridge Press, 1997,
c    ISBN: 0-9614088-7-1,
c    LC: TK7872.F5S79 / QA403.3.S87
c
c  Parameters:
c
c    Input, integer N, the order of the matrix.
c    N must be at least 10, and a multiple of 2.
c
c    Output, double precision A(N,N), the matrix.
c
      implicit none

      integer n
      integer p
      parameter ( p = 9 )

      double precision a(n,n)
      double precision c(0:p)
      integer i
      integer i4_wrap
      integer j
      integer m

      save c

      data c /
     &  0.1601023979741929D+00, 
     &  0.6038292697971895D+00, 
     &  0.7243085284377726D+00, 
     &  0.1384281459013203D+00, 
     & -0.2422948870663823D+00, 
     & -0.0322448695846381D+00, 
     &  0.0775714938400459D+00, 
     & -0.0062414902127983D+00, 
     & -0.0125807519990820D+00, 
     &  0.0033357252854738D+00 /

      if ( n .lt. 10 .or. mod ( n, 2 ) .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'DAUB10_MATRIX - Fatal error!'
        write ( *, '(a)' ) 
     &    '  N must be at least 10 and a multiple of 2.'
        stop 1
      end if

      do j = 1, n
        do i = 1, n
          a(i,j) = 0.0D+00
        end do
      end do

      do i = 1, n - 1, 2

        a(i,i)                  =   c(0)
        a(i,i+1)                =   c(1)
        a(i,i4_wrap(i+2,1,n))   =   c(2)
        a(i,i4_wrap(i+3,1,n))   =   c(3)
        a(i,i4_wrap(i+4,1,n))   =   c(4)
        a(i,i4_wrap(i+5,1,n))   =   c(5)
        a(i,i4_wrap(i+6,1,n))   =   c(6)
        a(i,i4_wrap(i+7,1,n))   =   c(7)
        a(i,i4_wrap(i+8,1,n))   =   c(8)
        a(i,i4_wrap(i+9,1,n))   =   c(9)

        a(i+1,i)                =   c(9)
        a(i+1,i+1)              = - c(8)
        a(i+1,i4_wrap(i+2,1,n)) =   c(7)
        a(i+1,i4_wrap(i+3,1,n)) = - c(6)
        a(i+1,i4_wrap(i+4,1,n)) =   c(5)
        a(i+1,i4_wrap(i+5,1,n)) = - c(4)
        a(i+1,i4_wrap(i+6,1,n)) =   c(3)
        a(i+1,i4_wrap(i+7,1,n)) = - c(2)
        a(i+1,i4_wrap(i+8,1,n)) =   c(1)
        a(i+1,i4_wrap(i+9,1,n)) = - c(0)

      end do

      return
      end
      subroutine daub10_transform ( n, x, y )

c*********************************************************************72
c
cc DAUB10_TRANSFORM computes the DAUB10 transform of a vector.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the dimension of the vector.
c    N must be a power of 2 and at least 4.
c
c    Input, double precision X(N), the vector to be transformed.
c
c    Output, double precision Y(N), the transformed vector.
c
      implicit none

      integer n
      integer p
      parameter ( p = 9 )

      double precision c(0:p)
      integer i
      integer i4_wrap
      integer j
      integer j0
      integer j1
      integer k
      integer m
      integer q
      double precision x(n)
      double precision y(n)
      double precision z(n)

      save c

      data c /
     &  0.1601023979741929D+00, 
     &  0.6038292697971895D+00, 
     &  0.7243085284377726D+00, 
     &  0.1384281459013203D+00, 
     & -0.2422948870663823D+00, 
     & -0.0322448695846381D+00, 
     &  0.0775714938400459D+00, 
     & -0.0062414902127983D+00, 
     & -0.0125807519990820D+00, 
     &  0.0033357252854738D+00 /

      do i = 1, n
        y(i) = x(i)
      end do

      m = n
      q = ( p - 1 ) / 2

10    continue

      if ( 4 .le. m ) then

        i = 1
        do j = 1, m
          z(j) = 0.0D+00
        end do

        do j = 1, m - 1, 2

          do k = 0, p - 1, 2
            j0 = i4_wrap ( j + k,     1, m )
            j1 = i4_wrap ( j + k + 1, 1, m )
            z(i)     = z(i)     + c(  k) * y(j0) + c(  k+1) * y(j1)
            z(i+m/2) = z(i+m/2) + c(p-k) * y(j0) - c(p-k-1) * y(j1)
          end do

          i = i + 1

        end do

        do i = 1, m
          y(i) = z(i)
        end do

        m = m / 2

        go to 10

      end if

      return
      end
      subroutine daub10_transform_inverse ( n, y, x )

c*********************************************************************72
c
cc DAUB10_TRANSFORM_INVERSE inverts the DAUB10 transform of a vector.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the dimension of the vector.
c    N must be a power of 2 and at least 4.
c
c    Input, double precision Y(N), the transformed vector.
c
c    Output, double precision X(N), the original vector.
c
      implicit none

      integer n
      integer p
      parameter ( p = 9 )

      double precision c(0:p)
      integer i
      integer i0
      integer i1
      integer i4_wrap
      integer j
      integer k
      integer m
      integer q
      double precision x(n)
      double precision y(n)
      double precision z(n)

      save c

      data c /
     &  0.1601023979741929D+00, 
     &  0.6038292697971895D+00, 
     &  0.7243085284377726D+00, 
     &  0.1384281459013203D+00, 
     & -0.2422948870663823D+00, 
     & -0.0322448695846381D+00, 
     &  0.0775714938400459D+00, 
     & -0.0062414902127983D+00, 
     & -0.0125807519990820D+00, 
     &  0.0033357252854738D+00 /

      do i = 1, n
        x(i) = y(i)
      end do

      m = 4
      q = ( p - 1 ) / 2

10    continue

      if ( m .le. n ) then

        do i = 1, m
          z(i) = 0.0D+00
        end do

        j = 1

        do i = - q + 1, m / 2 - q

          do k = 0, p - 1, 2
            i0 = i4_wrap ( i         + k / 2,     1,         m / 2 )
            i1 = i4_wrap ( i + m / 2 + k / 2,     m / 2 + 1, m     )
            z(j)   = z(j)   + c(p-k-1) * x(i0) + c(k+1) * x(i1)
            z(j+1) = z(j+1) + c(p-k)   * x(i0) - c(k)   * x(i1)
          end do

          j = j + 2

        end do

        do i = 1, m
          x(i) = z(i)
        end do

        m = m * 2

        go to 10

      end if

      return
      end
      subroutine daub12_matrix ( n, a )

c*********************************************************************72
c
cc DAUB12_MATRIX returns the DAUB12 matrix.
c
c  Discussion:
c
c    The DAUB12 matrix is the Daubechies wavelet transformation matrix
c    with 12 coefficients.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Gilbert Strang, Truong Nguyen,
c    Wavelets and Filter Banks,
c    Wellesley-Cambridge Press, 1997,
c    ISBN: 0-9614088-7-1,
c    LC: TK7872.F5S79 / QA403.3.S87
c
c  Parameters:
c
c    Input, integer N, the order of the matrix.
c    N must be at least 12, and a multiple of 2.
c
c    Output, double precision A(N,N), the matrix.
c
      implicit none

      integer n
      integer p
      parameter ( p = 11 )

      double precision a(n,n)
      double precision c(0:p)
      integer i
      integer i4_wrap
      integer j
      integer q

      save c

      data c /
     &  0.1115407433501095D+00, 
     &  0.4946238903984533D+00, 
     &  0.7511339080210959D+00, 
     &  0.3152503517091982D+00, 
     & -0.2262646939654400D+00, 
     & -0.1297668675672625D+00, 
     &  0.0975016055873225D+00, 
     &  0.0275228655303053D+00, 
     & -0.0315820393174862D+00, 
     &  0.0005538422011614D+00, 
     &  0.0047772575109455D+00, 
     & -0.0010773010853085D+00 /

      if ( n .lt. 12 .or. mod ( n, 2 ) .ne. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'DAUB12_MATRIX - Fatal error!'
        write ( *, '(a)' ) 
     &    '  N must be at least 12 and a multiple of 2.'
        stop 1
      end if

      q = ( p - 1 ) / 2

      do j = 1, n
        do i = 1, n
          a(i,j) = 0.0D+00
        end do
      end do

      do i = 1, n - 1, 2

        a(i,i)                   =    c(0)
        a(i,i+1)                 =    c(1)
        a(i,i4_wrap(i+ 2,1,n))   =    c(2)
        a(i,i4_wrap(i+ 3,1,n))   =    c(3)
        a(i,i4_wrap(i+ 4,1,n))   =    c(4)
        a(i,i4_wrap(i+ 5,1,n))   =    c(5)
        a(i,i4_wrap(i+ 6,1,n))   =    c(6)
        a(i,i4_wrap(i+ 7,1,n))   =    c(7)
        a(i,i4_wrap(i+ 8,1,n))   =    c(8)
        a(i,i4_wrap(i+ 9,1,n))   =    c(9)
        a(i,i4_wrap(i+10,1,n))   =   c(10)
        a(i,i4_wrap(i+11,1,n))   =   c(11)

        a(i+1,i)                 =   c(11)
        a(i+1,i+1)               = - c(10)
        a(i+1,i4_wrap(i+ 2,1,n)) =    c(9)
        a(i+1,i4_wrap(i+ 3,1,n)) =  - c(8)
        a(i+1,i4_wrap(i+ 4,1,n)) =    c(7)
        a(i+1,i4_wrap(i+ 5,1,n)) =  - c(6)
        a(i+1,i4_wrap(i+ 6,1,n)) =    c(5)
        a(i+1,i4_wrap(i+ 7,1,n)) =  - c(4)
        a(i+1,i4_wrap(i+ 8,1,n)) =    c(3)
        a(i+1,i4_wrap(i+ 9,1,n)) =  - c(2)
        a(i+1,i4_wrap(i+10,1,n)) =    c(1)
        a(i+1,i4_wrap(i+11,1,n)) =  - c(0)

      end do

      return
      end
      subroutine daub12_transform ( n, x, y )

c*********************************************************************72
c
cc DAUB12_TRANSFORM computes the DAUB12 transform of a vector.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the dimension of the vector.
c    N must be a power of 2 and at least 4.
c
c    Input, double precision X(N), the vector to be transformed.
c
c    Output, double precision Y(N), the transformed vector.
c
      implicit none

      integer n
      integer p
      parameter ( p = 11 )

      double precision c(0:p)
      integer i
      integer i4_wrap
      integer j
      integer j0
      integer j1
      integer k
      integer m
      integer q
      double precision x(n)
      double precision y(n)
      double precision z(n)

      save c

      data c /
     &  0.1115407433501095D+00, 
     &  0.4946238903984533D+00, 
     &  0.7511339080210959D+00, 
     &  0.3152503517091982D+00, 
     & -0.2262646939654400D+00, 
     & -0.1297668675672625D+00, 
     &  0.0975016055873225D+00, 
     &  0.0275228655303053D+00, 
     & -0.0315820393174862D+00, 
     &  0.0005538422011614D+00, 
     &  0.0047772575109455D+00, 
     & -0.0010773010853085D+00 /

      do i = 1, n
        y(i) = x(i)
      end do

      m = n
      q = ( p - 1 ) / 2

10    continue

      if ( 4 .le. m ) then

        i = 1
        do j = 1, m
          z(j) = 0.0D+00
        end do

        do j = 1, m - 1, 2

          do k = 0, p - 1, 2
            j0 = i4_wrap ( j + k,     1, m )
            j1 = i4_wrap ( j + k + 1, 1, m )
            z(i)     = z(i)     + c(  k) * y(j0) + c(  k+1) * y(j1)
            z(i+m/2) = z(i+m/2) + c(p-k) * y(j0) - c(p-k-1) * y(j1)
          end do

          i = i + 1

        end do

        do i = 1, m
          y(i) = z(i)
        end do

        m = m / 2

        go to 10

      end if

      return
      end
      subroutine daub12_transform_inverse ( n, y, x )

c*********************************************************************72
c
cc DAUB12_TRANSFORM_INVERSE inverts the DAUB12 transform of a vector.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the dimension of the vector.
c    N must be a power of 2 and at least 4.
c
c    Input, double precision Y(N), the transformed vector.
c
c    Output, double precision X(N), the original vector.
c
      implicit none

      integer n
      integer p
      parameter ( p = 11 )

      double precision c(0:p)
      integer i
      integer i0
      integer i1
      integer i4_wrap
      integer j
      integer k
      integer m
      integer q
      double precision x(n)
      double precision y(n)
      double precision z(n)

      save c

      data c /
     &  0.1115407433501095D+00, 
     &  0.4946238903984533D+00, 
     &  0.7511339080210959D+00, 
     &  0.3152503517091982D+00, 
     & -0.2262646939654400D+00, 
     & -0.1297668675672625D+00, 
     &  0.0975016055873225D+00, 
     &  0.0275228655303053D+00, 
     & -0.0315820393174862D+00, 
     &  0.0005538422011614D+00, 
     &  0.0047772575109455D+00, 
     & -0.0010773010853085D+00 /

      do i = 1, n
        x(i) = y(i)
      end do

      m = 4
      q = ( p - 1 ) / 2

10    continue

      if ( m .le. n ) then

        do i = 1, m
          z(i) = 0.0D+00
        end do

        j = 1

        do i = - q + 1, m / 2 - q

          do k = 0, p - 1, 2
            i0 = i4_wrap ( i         + k / 2,     1,         m / 2 )
            i1 = i4_wrap ( i + m / 2 + k / 2,     m / 2 + 1, m     )
            z(j)   = z(j)   + c(p-k-1) * x(i0) + c(k+1) * x(i1)
            z(j+1) = z(j+1) + c(p-k)   * x(i0) - c(k)   * x(i1)
          end do

          j = j + 2

        end do

        do i = 1, m
          x(i) = z(i)
        end do

        m = m * 2

        go to 10

      end if

      return
      end
      subroutine daub14_transform ( n, x, y )

c*********************************************************************72
c
cc DAUB14_TRANSFORM computes the DAUB14 transform of a vector.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the dimension of the vector.
c    N must be a power of 2 and at least 4.
c
c    Input, double precision X(N), the vector to be transformed.
c
c    Output, double precision Y(N), the transformed vector.
c
      implicit none

      integer n
      integer p
      parameter ( p = 13 )

      double precision c(0:p)
      integer i
      integer i4_wrap
      integer j
      integer j0
      integer j1
      integer k
      integer m
      integer q
      double precision x(n)
      double precision y(n)
      double precision z(n)

      save c

      data c /
     &  7.785205408500917D-02, 
     &  3.965393194819173D-01, 
     &  7.291320908462351D-01, 
     &  4.697822874051931D-01, 
     & -1.439060039285649D-01, 
     & -2.240361849938749D-01, 
     &  7.130921926683026D-02, 
     &  8.061260915108307D-02, 
     & -3.802993693501441D-02, 
     & -1.657454163066688D-02, 
     &  1.255099855609984D-02, 
     &  4.295779729213665D-04, 
     & -1.801640704047490D-03, 
     &  3.537137999745202D-04 /

      do i = 1, n
        y(i) = x(i)
      end do

      m = n
      q = ( p - 1 ) / 2

10    continue

      if ( 4 .le. m ) then

        i = 1
        z(1:m) = 0.0D+00

        do j = 1, m - 1, 2

          do k = 0, p - 1, 2
            j0 = i4_wrap ( j + k,     1, m )
            j1 = i4_wrap ( j + k + 1, 1, m )
            z(i)     = z(i)     + c(  k) * y(j0) + c(  k+1) * y(j1)
            z(i+m/2) = z(i+m/2) + c(p-k) * y(j0) - c(p-k-1) * y(j1)
          end do

          i = i + 1

        end do

        do i = 1, m
          y(i) = z(i)
        end do

        m = m / 2

        go to 10

      end if

      return
      end
      subroutine daub14_transform_inverse ( n, y, x )

c*********************************************************************72
c
cc DAUB14_TRANSFORM_INVERSE inverts the DAUB14 transform of a vector.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the dimension of the vector.
c    N must be a power of 2 and at least 4.
c
c    Input, double precision Y(N), the transformed vector.
c
c    Output, double precision X(N), the original vector.
c
      implicit none

      integer n
      integer p
      parameter ( p = 13 )

      double precision c(0:p)
      integer i
      integer i0
      integer i1
      integer i4_wrap
      integer j
      integer k
      integer m
      integer q
      double precision x(n)
      double precision y(n)
      double precision z(n)

      save c

      data c /
     &  7.785205408500917D-02, 
     &  3.965393194819173D-01, 
     &  7.291320908462351D-01, 
     &  4.697822874051931D-01, 
     & -1.439060039285649D-01, 
     & -2.240361849938749D-01, 
     &  7.130921926683026D-02, 
     &  8.061260915108307D-02, 
     & -3.802993693501441D-02, 
     & -1.657454163066688D-02, 
     &  1.255099855609984D-02, 
     &  4.295779729213665D-04, 
     & -1.801640704047490D-03, 
     &  3.537137999745202D-04 /

      do i = 1, n
        x(i) = y(i)
      end do

      m = 4
      q = ( p - 1 ) / 2

10    continue

      if ( m .le. n ) then

        do i = 1, m
          z(i) = 0.0D+00
        end do

        j = 1

        do i = - q + 1, m / 2 - q

          do k = 0, p - 1, 2
            i0 = i4_wrap ( i         + k / 2,     1,         m / 2 )
            i1 = i4_wrap ( i + m / 2 + k / 2,     m / 2 + 1, m     )
            z(j)   = z(j)   + c(p-k-1) * x(i0) + c(k+1) * x(i1)
            z(j+1) = z(j+1) + c(p-k)   * x(i0) - c(k)   * x(i1)
          end do

          j = j + 2

        end do

        x(1:m) = z(1:m)

        m = m * 2

        go to 10

      end if

      return
      end
      subroutine daub16_transform ( n, x, y )

c*********************************************************************72
c
cc DAUB16_TRANSFORM computes the DAUB16 transform of a vector.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the dimension of the vector.
c    N must be a power of 2 and at least 4.
c
c    Input, double precision X(N), the vector to be transformed.
c
c    Output, double precision Y(N), the transformed vector.
c
      implicit none

      integer n
      integer p
      parameter ( p = 15 )

      double precision c(0:p)
      integer i
      integer i4_wrap
      integer j
      integer j0
      integer j1
      integer k
      integer m
      integer q
      double precision x(n)
      double precision y(n)
      double precision z(n)

      save c

      data c /
     &  5.441584224310400D-02,
     &  3.128715909142999D-01, 
     &  6.756307362972898D-01, 
     &  5.853546836542067D-01, 
     & -1.582910525634930D-02, 
     & -2.840155429615469D-01, 
     &  4.724845739132827D-04, 
     &  1.287474266204784D-01, 
     & -1.736930100180754D-02, 
     & -4.408825393079475D-02, 
     &  1.398102791739828D-02, 
     &  8.746094047405776D-03, 
     & -4.870352993451574D-03, 
     & -3.917403733769470D-04, 
     &  6.754494064505693D-04, 
     & -1.174767841247695D-04 /

      do i = 1, n
        y(i) = x(i)
      end do

      m = n
      q = ( p - 1 ) / 2

10    continue

      if ( 4 .le. m ) then

        i = 1

        do j = 1, m
          z(j) = 0.0D+00
        end do

        do j = 1, m - 1, 2

          do k = 0, p - 1, 2
            j0 = i4_wrap ( j + k,     1, m )
            j1 = i4_wrap ( j + k + 1, 1, m )
            z(i)     = z(i)     + c(  k) * y(j0) + c(  k+1) * y(j1)
            z(i+m/2) = z(i+m/2) + c(p-k) * y(j0) - c(p-k-1) * y(j1)
          end do

          i = i + 1

        end do

        do i = 1, m
          y(i) = z(i)
        end do

        m = m / 2

        go to 10

      end if

      return
      end
      subroutine daub16_transform_inverse ( n, y, x )

c*********************************************************************72
c
cc DAUB16_TRANSFORM_INVERSE inverts the DAUB16 transform of a vector.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the dimension of the vector.
c    N must be a power of 2 and at least 4.
c
c    Input, double precision Y(N), the transformed vector.
c
c    Output, double precision X(N), the original vector.
c
      implicit none

      integer n
      integer p
      parameter ( p = 15 )

      double precision c(0:p)
      integer i
      integer i0
      integer i1
      integer i4_wrap
      integer j
      integer k
      integer m
      integer q
      double precision x(n)
      double precision y(n)
      double precision z(n)

      save c

      data c /
     &  5.441584224310400D-02,
     &  3.128715909142999D-01, 
     &  6.756307362972898D-01, 
     &  5.853546836542067D-01, 
     & -1.582910525634930D-02, 
     & -2.840155429615469D-01, 
     &  4.724845739132827D-04, 
     &  1.287474266204784D-01, 
     & -1.736930100180754D-02, 
     & -4.408825393079475D-02, 
     &  1.398102791739828D-02, 
     &  8.746094047405776D-03, 
     & -4.870352993451574D-03, 
     & -3.917403733769470D-04, 
     &  6.754494064505693D-04, 
     & -1.174767841247695D-04 /

      do i = 1, n
        x(i) = y(i)
      end do

      m = 4
      q = ( p - 1 ) / 2

10    continue

      if ( m .le. n ) then

        do i = 1, m
          z(i) = 0.0D+00
        end do

        j = 1

        do i = - q + 1, m / 2 - q

          do k = 0, p - 1, 2
            i0 = i4_wrap ( i         + k / 2,     1,         m / 2 )
            i1 = i4_wrap ( i + m / 2 + k / 2,     m / 2 + 1, m     )
            z(j)   = z(j)   + c(p-k-1) * x(i0) + c(k+1) * x(i1)
            z(j+1) = z(j+1) + c(p-k)   * x(i0) - c(k)   * x(i1)
          end do

          j = j + 2

        end do

        do i = 1, m
          x(i) = z(i)
        end do

        m = m * 2

        go to 10

      end if

      return
      end
      subroutine daub18_transform ( n, x, y )

c*********************************************************************72
c
cc DAUB18_TRANSFORM computes the DAUB18 transform of a vector.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the dimension of the vector.
c    N must be a power of 2 and at least 4.
c
c    Input, double precision X(N), the vector to be transformed.
c
c    Output, double precision Y(N), the transformed vector.
c
      implicit none

      integer n
      integer p
      parameter ( p = 17 )

      double precision c(0:p)
      integer i
      integer i4_wrap
      integer j
      integer j0
      integer j1
      integer k
      integer m
      integer q
      double precision x(n)
      double precision y(n)
      double precision z(n)

      save c

      data c /
     &  3.807794736387834D-02, 
     &  2.438346746125903D-01, 
     &  6.048231236901111D-01, 
     &  6.572880780513005D-01, 
     &  1.331973858250075D-01, 
     & -2.932737832791749D-01, 
     & -9.684078322297646D-02, 
     &  1.485407493381063D-01, 
     &  3.072568147933337D-02, 
     & -6.763282906132997D-02, 
     &  2.509471148314519D-04, 
     &  2.236166212367909D-02, 
     & -4.723204757751397D-03, 
     & -4.281503682463429D-03, 
     &  1.847646883056226D-03, 
     &  2.303857635231959D-04, 
     & -2.519631889427101D-04, 
     &  3.934732031627159D-05 /

      do i = 1, n
        y(i) = x(i)
      end do

      m = n
      q = ( p - 1 ) / 2

10    continue

      if ( 4 .le. m ) then

        i = 1

        do j = 1, m
          z(j) = 0.0D+00
        end do

        do j = 1, m - 1, 2

          do k = 0, p - 1, 2
            j0 = i4_wrap ( j + k,     1, m )
            j1 = i4_wrap ( j + k + 1, 1, m )
            z(i)     = z(i)     + c(  k) * y(j0) + c(  k+1) * y(j1)
            z(i+m/2) = z(i+m/2) + c(p-k) * y(j0) - c(p-k-1) * y(j1)
          end do

          i = i + 1

        end do

        do i = 1, m
          y(i) = z(i)
        end do

        m = m / 2

        go to 10

      end if

      return
      end
      subroutine daub18_transform_inverse ( n, y, x )

c*********************************************************************72
c
cc DAUB18_TRANSFORM_INVERSE inverts the DAUB18 transform of a vector.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the dimension of the vector.
c    N must be a power of 2 and at least 4.
c
c    Input, double precision Y(N), the transformed vector.
c
c    Output, double precision X(N), the original vector.
c
      implicit none

      integer n
      integer p
      parameter ( p = 17 )

      double precision c(0:p)
      integer i
      integer i0
      integer i1
      integer i4_wrap
      integer j
      integer k
      integer m
      integer q
      double precision x(n)
      double precision y(n)
      double precision z(n)

      save c

      data c /
     &  3.807794736387834D-02, 
     &  2.438346746125903D-01, 
     &  6.048231236901111D-01, 
     &  6.572880780513005D-01, 
     &  1.331973858250075D-01, 
     & -2.932737832791749D-01, 
     & -9.684078322297646D-02, 
     &  1.485407493381063D-01, 
     &  3.072568147933337D-02, 
     & -6.763282906132997D-02, 
     &  2.509471148314519D-04, 
     &  2.236166212367909D-02, 
     & -4.723204757751397D-03, 
     & -4.281503682463429D-03, 
     &  1.847646883056226D-03, 
     &  2.303857635231959D-04, 
     & -2.519631889427101D-04, 
     &  3.934732031627159D-05 /

      do i = 1, n
        x(i) = y(i)
      end do

      m = 4
      q = ( p - 1 ) / 2

10    continue

      if ( m .le. n ) then

        do i = 1, m
          z(i) = 0.0D+00
        end do

        j = 1

        do i = - q + 1, m / 2 - q

          do k = 0, p - 1, 2
            i0 = i4_wrap ( i         + k / 2,     1,         m / 2 )
            i1 = i4_wrap ( i + m / 2 + k / 2,     m / 2 + 1, m     )
            z(j)   = z(j)   + c(p-k-1) * x(i0) + c(k+1) * x(i1)
            z(j+1) = z(j+1) + c(p-k)   * x(i0) - c(k)   * x(i1)
          end do

          j = j + 2

        end do

        do i = 1, m
          x(i) = z(i)
        end do

        m = m * 2

        go to 10

      end if

      return
      end
      subroutine daub20_transform ( n, x, y )

c*********************************************************************72
c
cc DAUB20_TRANSFORM computes the DAUB20 transform of a vector.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the dimension of the vector.
c    N must be a power of 2 and at least 4.
c
c    Input, double precision X(N), the vector to be transformed.
c
c    Output, double precision Y(N), the transformed vector.
c
      implicit none

      integer n
      integer p
      parameter ( p = 19 )

      double precision c(0:p)
      integer i
      integer i4_wrap
      integer j
      integer j0
      integer j1
      integer k
      integer m
      integer q
      double precision x(n)
      double precision y(n)
      double precision z(n)

      save c

      data c /
     &  2.667005790055555D-02, 
     &  1.881768000776914D-01, 
     &  5.272011889317255D-01, 
     &  6.884590394536035D-01, 
     &  2.811723436605774D-01, 
     & -2.498464243273153D-01, 
     & -1.959462743773770D-01, 
     &  1.273693403357932D-01, 
     &  9.305736460357235D-02, 
     & -7.139414716639708D-02, 
     & -2.945753682187581D-02, 
     &  3.321267405934100D-02, 
     &  3.606553566956169D-03, 
     & -1.073317548333057D-02, 
     &  1.395351747052901D-03, 
     &  1.992405295185056D-03, 
     & -6.858566949597116D-04, 
     & -1.164668551292854D-04, 
     &  9.358867032006959D-05, 
     & -1.326420289452124D-05 /

      do i = 1, n
        y(i) = x(i)
      end do

      m = n
      q = ( p - 1 ) / 2

10    continue

      if ( 4 .le. m ) then

        i = 1

        do j = 1, m
          z(j) = 0. 0D+00
        end do

        do j = 1, m - 1, 2

          do k = 0, p - 1, 2
            j0 = i4_wrap ( j + k,     1, m )
            j1 = i4_wrap ( j + k + 1, 1, m )
            z(i)     = z(i)     + c(  k) * y(j0) + c(  k+1) * y(j1)
            z(i+m/2) = z(i+m/2) + c(p-k) * y(j0) - c(p-k-1) * y(j1)
          end do

          i = i + 1

        end do

        do i = 1, m
          y(i) = z(i)
        end do

        m = m / 2

        go to 10

      end if

      return
      end
      subroutine daub20_transform_inverse ( n, y, x )

c*********************************************************************72
c
cc DAUB20_TRANSFORM_INVERSE inverts the DAUB20 transform of a vector.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    09 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the dimension of the vector.
c    N must be a power of 2 and at least 4.
c
c    Input, double precision Y(N), the transformed vector.
c
c    Output, double precision X(N), the original vector.
c
      implicit none

      integer n
      integer p
      parameter ( p = 19 )

      double precision c(0:p)
      integer i
      integer i0
      integer i1
      integer i4_wrap
      integer j
      integer k
      integer m
      integer q
      double precision x(n)
      double precision y(n)
      double precision z(n)

      save c

      data c /
     &  2.667005790055555D-02, 
     &  1.881768000776914D-01, 
     &  5.272011889317255D-01, 
     &  6.884590394536035D-01, 
     &  2.811723436605774D-01, 
     & -2.498464243273153D-01, 
     & -1.959462743773770D-01, 
     &  1.273693403357932D-01, 
     &  9.305736460357235D-02, 
     & -7.139414716639708D-02, 
     & -2.945753682187581D-02, 
     &  3.321267405934100D-02, 
     &  3.606553566956169D-03, 
     & -1.073317548333057D-02, 
     &  1.395351747052901D-03, 
     &  1.992405295185056D-03, 
     & -6.858566949597116D-04, 
     & -1.164668551292854D-04, 
     &  9.358867032006959D-05, 
     & -1.326420289452124D-05 /

      do i = 1, n
        x(i) = y(i)
      end do

      m = 4
      q = ( p - 1 ) / 2

10    continue

      if ( m .le. n ) then

        do i = 1, m
          z(i) = 0.0D+00
        end do

        j = 1

        do i = - q + 1, m / 2 - q

          do k = 0, p - 1, 2
            i0 = i4_wrap ( i         + k / 2,     1,         m / 2 )
            i1 = i4_wrap ( i + m / 2 + k / 2,     m / 2 + 1, m     )
            z(j)   = z(j)   + c(p-k-1) * x(i0) + c(k+1) * x(i1)
            z(j+1) = z(j+1) + c(p-k)   * x(i0) - c(k)   * x(i1)
          end do

          j = j + 2

        end do

        do i = 1, m
          x(i) = z(i)
        end do

        m = m * 2

        go to 10

      end if

      return
      end
      function i4_is_power_of_2 ( n )

c*********************************************************************72
c
cc I4_IS_POWER_OF_2 reports whether an I4 is a power of 2.
c
c  Discussion:
c
c    The powers of 2 are 1, 2, 4, 8, 16, and so on.
c
c    An I4 is an integer value.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    24 October 2007
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the integer to be tested.
c
c    Output, logical I4_IS_POWER_OF_2, is TRUE if N is a power of 2.
c
      implicit none

      logical i4_is_power_of_2
      integer n
      integer n_copy

      n_copy = n
      i4_is_power_of_2 = .false.

      if ( n_copy .le. 0 ) then
        return
      end if

10    continue

      if ( n_copy .ne. 1 ) then

        if ( mod ( n_copy, 2 ) .eq. 1 ) then
          return
        end if

        n_copy = n_copy / 2

        go to 10

      end if

      i4_is_power_of_2 = .true.

      return
      end
      function i4_modp ( i, j )

c*********************************************************************72
c
cc I4_MODP returns the nonnegative remainder of integer division.
c
c  Discussion:
c
c    If
c      NREM = I4_MODP ( I, J )
c      NMULT = ( I - NREM ) / J
c    then
c      I = J * NMULT + NREM
c    where NREM is always nonnegative.
c
c    The MOD function computes a result with the same sign as the
c    quantity being divided.  Thus, suppose you had an angle A,
c    and you wanted to ensure that it was between 0 and 360.
c    Then mod(A,360) would do, if A was positive, but if A
c    was negative, your result would be between -360 and 0.
c
c    On the other hand, I4_MODP(A,360) is between 0 and 360, always.
c
c  Example:
c
c        I     J     MOD I4_MODP    Factorization
c
c      107    50       7       7    107 =  2 *  50 + 7
c      107   -50       7       7    107 = -2 * -50 + 7
c     -107    50      -7      43   -107 = -3 *  50 + 43
c     -107   -50      -7      43   -107 =  3 * -50 + 43
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 December 2006
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer I, the number to be divided.
c
c    Input, integer J, the number that divides I.
c
c    Output, integer I4_MODP, the nonnegative remainder when I is
c    divided by J.
c
      implicit none

      integer i
      integer i4_modp
      integer j
      integer value

      if ( j .eq. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'I4_MODP - Fatal error!'
        write ( *, '(a,i8)' ) '  Illegal divisor J = ', j
        stop
      end if

      value = mod ( i, j )

      if ( value .lt. 0 ) then
        value = value + abs ( j )
      end if

      i4_modp = value

      return
      end
      function i4_wrap ( ival, ilo, ihi )

c*********************************************************************72
c
cc I4_WRAP forces an I4 to lie between given limits by wrapping.
c
c  Example:
c
c    ILO = 4, IHI = 8
c
c    I  Value
c
c    -2     8
c    -1     4
c     0     5
c     1     6
c     2     7
c     3     8
c     4     4
c     5     5
c     6     6
c     7     7
c     8     8
c     9     4
c    10     5
c    11     6
c    12     7
c    13     8
c    14     4
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    31 December 2006
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer IVAL, an integer value.
c
c    Input, integer ILO, IHI, the desired bounds for the integer value.
c
c    Output, integer I4_WRAP, a "wrapped" version of IVAL.
c
      implicit none

      integer i4_modp
      integer i4_wrap
      integer ihi
      integer ilo
      integer ival
      integer jhi
      integer jlo
      integer value
      integer wide

      jlo = min ( ilo, ihi )
      jhi = max ( ilo, ihi )

      wide = jhi - jlo + 1

      if ( wide .eq. 1 ) then
        value = jlo
      else
        value = jlo + i4_modp ( ival - jlo, wide )
      end if

      i4_wrap = value

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
      subroutine r8mat_is_identity ( n, a, error_frobenius )

c*********************************************************************72
c
cc R8MAT_IS_IDENTITY determines if an R8MAT is the identity.
c
c  Discussion:
c
c    An R8MAT is a matrix of double precision values.
c
c    The routine returns the Frobenius norm of A - I.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    10 February 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the order of the matrix.
c
c    Input, double precision A(N,N), the matrix.
c
c    Output, double precision ERROR_FROBENIUS, the Frobenius norm
c    of the difference matrix A - I, which would be exactly zero
c    if A were the identity matrix.
c
      implicit none

      integer n

      double precision a(n,n)
      double precision error_frobenius
      integer i
      integer j
      double precision value

      error_frobenius = 0.0D+00

      do i = 1, n
        do j = 1, n
          if ( i .eq. j ) then
            error_frobenius = error_frobenius + ( a(i,j) - 1.0D+00 )**2
          else
            error_frobenius = error_frobenius + a(i,j)**2
          end if
        end do 
      end do

      error_frobenius = sqrt ( error_frobenius )

      return
      end
      subroutine r8mat_mm ( n1, n2, n3, a, b, c )

c*********************************************************************72
c
cc R8MAT_MM multiplies two R8MAT's.
c
c  Discussion:
c
c    An R8MAT is an array of R8 values.
c
c    In FORTRAN90, this operation is more efficiently done by the
c    command:
c
c      C(1:N1,1:N3) = MATMUL ( A(1:N1,1;N2), B(1:N2,1:N3) )
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    15 July 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N1, N2, N3, the order of the matrices.
c
c    Input, double precision A(N1,N2), B(N2,N3), the matrices to multiply.
c
c    Output, double precision C(N1,N3), the product matrix C = A * B.
c
      implicit none

      integer n1
      integer n2
      integer n3

      double precision a(n1,n2)
      double precision b(n2,n3)
      double precision c(n1,n3)
      double precision c1(n1,n3)
      integer i
      integer j
      integer k

      do i = 1, n1
        do j = 1, n3
          c1(i,j) = 0.0D+00
          do k = 1, n2
            c1(i,j) = c1(i,j) + a(i,k) * b(k,j)
          end do
        end do
      end do

      do j = 1, n3
        do i = 1, n1
          c(i,j) = c1(i,j)
        end do
      end do

      return
      end
      subroutine r8mat_transpose ( m, n, a, at )

c*********************************************************************72
c
cc R8MAT_TRANSPOSE makes a transposed copy of a matrix.
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
c    13 June 2011
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, N, the number of rows and columns of the matrix A.
c
c    Input, double precision A(N,N), the matrix to be transposed.
c
c    Output, double precision AT(N,M), the matrix to be transposed.
c
      implicit none

      integer m
      integer n

      double precision a(m,n)
      double precision at(n,m)
      integer i
      integer j

      do j = 1, m
        do i = 1, n
          at(i,j) = a(j,i)
        end do
      end do

      return
      end
      subroutine r8vec_conjugate ( n, c, d )

c*********************************************************************72
c
cc R8VEC_CONJUGATE reverses a vector and negates even-indexed entries.
c
c  Discussion:
c
c    There are many times in wavelet computations when such an operation
c    is invoked.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    08 February 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the dimension of the vector.
c
c    Input, double precision C(N), the input vector.
c
c    Output, double precision D(N), the "conjugated" vector.
c
      implicit none

      integer ( kind = 4 ) n

      double precision c(n)
      double precision d(n)
      double precision e(n)
      integer i
c
c  Using a temporary array means C and D could have the same memory.
c
      do i = 1, n
        e(n+1-i)= c(i)
      end do

      do i = 1, n
        d(i) = e(i)
      end do

      do i = 2, n, 2
        d(i) = - d(i)
      end do

      return
      end
      subroutine r8vec_convolution ( m, x, n, y, z )

c*********************************************************************72
c
cc R8VEC_CONVOLUTION returns the convolution of two R8VEC's.
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
c
c    The I-th entry of the convolution can be formed by summing the products 
c    that lie along the I-th diagonal of the following table:
c
c    Y3 | 3   4   5   6   7
c    Y2 | 2   3   4   5   6
c    Y1 | 1   2   3   4   5
c       +------------------
c        X1  X2  X3  X4  X5
c
c    which will result in:
c
c    Z = ( X1 * Y1,
c          X1 * Y2 + X2 * Y1,
c          X1 * Y3 + X2 * Y2 + X3 * Y1,
c                    X2 * Y3 + X3 * Y2 + X4 * Y1,
c                              X3 * Y3 + X4 * Y2 + X5 * Y1,
c                                        X4 * Y3 + X5 * Y2,
c                                                  X5 * Y3 )
c            
c  Example:
c
c    Input:
c
c      X = (/ 1, 2, 3, 4 /)
c      Y = (/ -1, 5, 3 /)
c
c    Output:
c
c      Z = (/ -1, 3, 10, 17, 29, 12 /)
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 May 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer M, the dimension of X.
c
c    Input, double precision X(M), the first vector to be convolved.
c
c    Input, integer N, the dimension of Y.
c
c    Input, double precision Y(N), the second vector to be convolved.
c
c    Output, double precision Z(M+N-1), the convolution of X and Y.
c
      implicit none

      integer m
      integer n

      integer i
      integer j
      double precision x(m)
      double precision y(n)
      double precision z(m+n-1)

      do i = 1, m + n - 1
        z(i) = 0.0D+00
      end do

      do j = 1, n
        do i = 0, m - 1
          z(j+i) = z(j+i) + x(i+1) * y(j)
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
      subroutine r8vec_linspace ( n, a_first, a_last, a )

c*********************************************************************72
c
cc R8VEC_LINSPACE creates a vector of linearly spaced values.
c
c  Discussion:
c
c    An R8VEC is a vector of R8's.
c
c    4 points evenly spaced between 0 and 12 will yield 0, 4, 8, 12.
c
c    In other words, the interval is divided into N-1 even subintervals,
c    and the endpoints of intervals are used as the points.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 March 2011
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the number of entries in the vector.
c
c    Input, double precision A_FIRST, A_LAST, the first and last entries.
c
c    Output, double precision A(N), a vector of linearly spaced data.
c
      implicit none

      integer n

      double precision a(n)
      double precision a_first
      double precision a_last
      integer i

      if ( n .eq. 1 ) then

        a(1) = ( a_first + a_last ) / 2.0D+00

      else

        do i = 1, n
          a(i) = ( dble ( n - i     ) * a_first 
     &           + dble (     i - 1 ) * a_last )
     &           / dble ( n     - 1 )
        end do

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
