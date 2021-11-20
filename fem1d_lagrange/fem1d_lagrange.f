      subroutine fem1d_lagrange_stiffness ( x_num, x, q_num, f, 
     &  a, m, b )

c*********************************************************************72
c
cc FEM1D_LAGRANGE_STIFFNESS evaluates the Lagrange polynomial stiffness matrix.
c
c  Discussion:
c
c    The finite element method is to be applied over a given interval that
c    has been meshed with X_NUM points X.
c
c    The finite element basis functions are to be the X_NUM Lagrange
c    basis polynomials L(i)(X), such that
c      L(i)(X(j)) = delta(i,j).
c
c    The following items are computed:
c    * A, the stiffness matrix, with A(I,J) = integral L'(i)(x) L'(j)(x)
c    * M, the mass matrix, with M(I,J) = integral L(i)(x) L(j)(x)
c    * B, the load matrix, with B(I) = integral L(i)(x) F(x)
c
c    The integrals are approximated by quadrature.
c
c    Boundary conditions are not handled by this routine.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 November 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer X_NUM, the number of nodes.
c
c    Input, double precision X(X_NUM), the coordinates of the nodes.
c
c    Input, integer Q_NUM, the number of quadrature points to use.
c
c    Input, external, double precision F(X), the right hand side function.
c
c    Output, double precision A(X_NUM,X_NUM), the stiffness matrix.
c
c    Output, double precision M(X_NUM,X_NUM), the mass matrix.
c
c    Output, double precision B(X_NUM), the right hand side vector.
c
      implicit none

      integer x_num

      double precision a(x_num,x_num)
      double precision b(x_num)
      double precision f
      external f
      double precision l(q_num,x_num)
      double precision li
      double precision lj
      double precision lp(q_num,x_num)
      double precision lpi
      double precision lpj
      double precision m(x_num,x_num)
      integer q_i
      integer q_num
      double precision q_w(q_num)
      double precision q_x(q_num)
      double precision x(x_num)
      integer x_i
      integer x_j
c
c  Get the quadrature rule for [-1,+1].
c
      call legendre_set ( q_num, q_x, q_w )
c
c  Adjust the quadrature rule to the interval [ x(1), x(x_num) }
c
      do q_i = 1, q_num
        q_x(q_i) =  ( ( 1.0D+00 - q_x(q_i) ) * x(1) 
     &              + ( 1.0D+00 + q_x(q_i) ) * x(x_num) ) 
     &              /   2.0D+00
      end do

      do q_i = 1, q_num
        q_w(q_i) = q_w(q_i) * ( x(x_num) - x(1) ) / 2.0D+00
      end do
c
c  Evaluate all the Lagrange basis polynomials at all the quadrature points.
c
      call lagrange_value ( x_num, x, q_num, q_x, l )
c
c  Evaluate all the Lagrange basis polynomial derivatives at all 
c  the quadrature points.
c
      call lagrange_derivative ( x_num, x, q_num, q_x, lp )
c
c  Assemble the matrix and right hand side.
c
      do x_j = 1, x_num
        do x_i = 1, x_num
          a(x_i,x_j) = 0.0D+00
        end do
      end do

      do x_j = 1, x_num
        do x_i = 1, x_num
          m(x_i,x_j) = 0.0D+00
        end do
      end do

      do x_i = 1, x_num
        b(x_i) = 0.0D+00
      end do

      do x_i = 1, x_num
        do q_i = 1, q_num
          li = l(q_i,x_i)
          lpi = lp(q_i,x_i)
          do x_j = 1, x_num
            lj = l(q_i,x_j)
            lpj = lp(q_i,x_j)
            a(x_i,x_j) = a(x_i,x_j) + q_w(q_i) * lpi * lpj
            m(x_i,x_j) = m(x_i,x_j) + q_w(q_i) * li * lj
          end do
          b(x_i) = b(x_i) + q_w(q_i) * li * f ( q_x(q_i) )
        end do
      end do

      return
      end
      subroutine lagrange_derivative ( nd, xd, ni, xi, lpi ) 

c*********************************************************************72
c
cc LAGRANGE_DERIVATIVE evaluates the Lagrange basis derivative.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    18 November 2014
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer ND, the number of data points.
c
c    Input, double precision XD(ND), the interpolation nodes.
c
c    Input, integer NI, the number of evaluation points.
c
c    Input, double precision XI(NI), the evaluation points.
c
c    Output, double precision LPI(NI,ND), the value, at the I-th point XI, 
c    of the Jth basis function derivative.
c
      implicit none

      integer nd
      integer ni

      integer i
      integer j
      integer j1
      integer j2
      double precision lpi(ni,nd)
      double precision p
      double precision xd(nd)
      double precision xi(ni)
  
      do i = 1, ni
        do j = 1, nd

          lpi(i,j) = 0.0D+00

          do j1 = 1, nd

            if ( j1 .ne. j ) then
              p = 1.0D+00
              do j2 = 1, nd
                if ( j2 .eq. j1 ) then
                  p = p / ( xd(j) - xd(j2) )
                else if ( j2 .ne. j ) then
                  p = p * ( xi(i) - xd(j2) ) / ( xd(j) - xd(j2) )
                end if
              end do
              lpi(i,j) = lpi(i,j) + p
            end if
 
          end do

        end do
      end do

      return
      end
      subroutine lagrange_value ( nd, xd, ni, xi, li )

c*********************************************************************72
c
cc LAGRANGE_VALUE evaluates the Lagrange basis polynomials.
c
c  Discussion:
c
c    Given ND distinct abscissas, XD(1:ND),
c    the I-th Lagrange basis polynomial LB(I)(T) is defined as the polynomial of
c    degree ND - 1 which is 1 at XD(I) and 0 at the ND - 1
c    other abscissas.
c
c    A formal representation is:
c
c      LI(I)(T) = Product ( 1 <= J <= ND, I /= J )
c       ( T - T(J) ) / ( T(I) - T(J) )
c
c    This routine accepts a set of NI values at which all the Lagrange
c    basis polynomials should be evaluated.
c
c    Given data values YD at each of the abscissas, the value of the
c    Lagrange interpolating polynomial at each of the interpolation points
c    is then simple to compute by matrix multiplication:
c
c      YI(1:NI) = LI(1:NI,1:ND) * YD(1:ND)
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    12 September 2012
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer ND, the number of data points.
c    ND must be at least 1.
c
c    Input, double precision XD(ND), the data points.
c
c    Input, integer NI, the number of interpolation points.
c
c    Input, double precision XI(NI), the interpolation points.
c
c    Output, double precision LI(NI,ND), the values
c    of the Lagrange basis polynomials at the interpolation points.
c
      implicit none

      integer nd
      integer ni

      integer i
      integer j
      integer k
      double precision li(ni,nd)
      double precision xd(nd)
      double precision xi(ni)
c
c  Evaluate the polynomial.
c
      do j = 1, nd
        do i = 1, ni
          li(i,j) = 1.0D+00
        end do
      end do

      do k = 1, nd

        do j = 1, nd

          if ( j .ne. k ) then

            do i = 1, ni
              li(i,k) = li(i,k) * ( xi(i) - xd(j) ) / ( xd(k) - xd(j) )
            end do

          end if

        end do

      end do

      return
      end
      subroutine legendre_set ( n, x, w )

c*********************************************************************72
c
cc LEGENDRE_SET sets abscissas and weights for Gauss-Legendre quadrature.
c
c  Discussion:
c
c    The integral:
c
c      integral ( -1 <= x <= 1 ) f(x) dx
c
c    The quadrature rule:
c
c      sum ( 1 <= i <= n ) w(i) * f ( x(i) )
c
c    The quadrature rule is exact for polynomials through degree 2*N-1.
c
c    The abscissas are the zeroes of the Legendre polynomial P(N)(X).
c
c    Mathematica can compute the abscissas and weights of a Gauss-Legendre
c    rule of order N for the interval [A,B] with P digits of precision
c    by the commands:
c
c    Needs["NumericalDifferentialEquationAnalysis`"]
c    GaussianQuadratureWeights [ n, a, b, p ]
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    17 June 2011
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    Milton Abramowitz, Irene Stegun,
c    Handbook of Mathematical Functions,
c    National Bureau of Standards, 1964,
c    ISBN: 0-486-61272-4,
c    LC: QA47.A34.
c
c    Vladimir Krylov,
c    Approximate Calculation of Integrals,
c    Dover, 2006,
c    ISBN: 0486445798,
c    LC: QA311.K713.
c
c    Arthur Stroud, Don Secrest,
c    Gaussian Quadrature Formulas,
c    Prentice Hall, 1966,
c    LC: QA299.4G3S7.
c
c    Stephen Wolfram,
c    The Mathematica Book,
c    Fourth Edition,
c    Cambridge University Press, 1999,
c    ISBN: 0-521-64314-7,
c    LC: QA76.95.W65.
c
c    Daniel Zwillinger, editor,
c    CRC Standard Mathematical Tables and Formulae,
c    30th Edition,
c    CRC Press, 1996,
c    ISBN: 0-8493-2479-3,
c    LC: QA47.M315.
c
c  Parameters:
c
c    Input, integer N, the order.
c    N must be between 1 and 16.
c
c    Output, double precision X(N), the abscissas.
c
c    Output, double precision W(N), the weights.
c
      implicit none

      integer n

      double precision w(n)
      double precision x(n)

      if ( n .eq. 1 ) then

        x(1) = 0.000000000000000000000000000000D+00

        w(1) = 2.000000000000000000000000000000D+00

      else if ( n .eq. 2 ) then

        x(1) = -0.577350269189625764509148780502D+00
        x(2) = 0.577350269189625764509148780502D+00

        w(1) = 1.000000000000000000000000000000D+00
        w(2) = 1.000000000000000000000000000000D+00

      else if ( n .eq. 3 ) then

        x(1) = -0.774596669241483377035853079956D+00
        x(2) = 0.000000000000000000000000000000D+00
        x(3) = 0.774596669241483377035853079956D+00

        w(1) = 0.555555555555555555555555555556D+00
        w(2) = 0.888888888888888888888888888889D+00
        w(3) = 0.555555555555555555555555555556D+00

      else if ( n .eq. 4 ) then

        x(1) = -0.861136311594052575223946488893D+00
        x(2) = -0.339981043584856264802665759103D+00
        x(3) = 0.339981043584856264802665759103D+00
        x(4) = 0.861136311594052575223946488893D+00

        w(1) = 0.347854845137453857373063949222D+00
        w(2) = 0.652145154862546142626936050778D+00
        w(3) = 0.652145154862546142626936050778D+00
        w(4) = 0.347854845137453857373063949222D+00

      else if ( n .eq. 5 ) then

        x(1) = -0.906179845938663992797626878299D+00
        x(2) = -0.538469310105683091036314420700D+00
        x(3) = 0.000000000000000000000000000000D+00
        x(4) = 0.538469310105683091036314420700D+00
        x(5) = 0.906179845938663992797626878299D+00

        w(1) = 0.236926885056189087514264040720D+00
        w(2) = 0.478628670499366468041291514836D+00
        w(3) = 0.568888888888888888888888888889D+00
        w(4) = 0.478628670499366468041291514836D+00
        w(5) = 0.236926885056189087514264040720D+00

      else if ( n .eq. 6 ) then

        x(1) = -0.932469514203152027812301554494D+00
        x(2) = -0.661209386466264513661399595020D+00
        x(3) = -0.238619186083196908630501721681D+00
        x(4) = 0.238619186083196908630501721681D+00
        x(5) = 0.661209386466264513661399595020D+00
        x(6) = 0.932469514203152027812301554494D+00

        w(1) = 0.171324492379170345040296142173D+00
        w(2) = 0.360761573048138607569833513838D+00
        w(3) = 0.467913934572691047389870343990D+00
        w(4) = 0.467913934572691047389870343990D+00
        w(5) = 0.360761573048138607569833513838D+00
        w(6) = 0.171324492379170345040296142173D+00

      else if ( n .eq. 7 ) then

        x(1) = -0.949107912342758524526189684048D+00
        x(2) = -0.741531185599394439863864773281D+00
        x(3) = -0.405845151377397166906606412077D+00
        x(4) = 0.000000000000000000000000000000D+00
        x(5) = 0.405845151377397166906606412077D+00
        x(6) = 0.741531185599394439863864773281D+00
        x(7) = 0.949107912342758524526189684048D+00

        w(1) = 0.129484966168869693270611432679D+00
        w(2) = 0.279705391489276667901467771424D+00
        w(3) = 0.381830050505118944950369775489D+00
        w(4) = 0.417959183673469387755102040816D+00
        w(5) = 0.381830050505118944950369775489D+00
        w(6) = 0.279705391489276667901467771424D+00
        w(7) = 0.129484966168869693270611432679D+00

      else if ( n .eq. 8 ) then

        x(1) = -0.960289856497536231683560868569D+00
        x(2) = -0.796666477413626739591553936476D+00
        x(3) = -0.525532409916328985817739049189D+00
        x(4) = -0.183434642495649804939476142360D+00
        x(5) = 0.183434642495649804939476142360D+00
        x(6) = 0.525532409916328985817739049189D+00
        x(7) = 0.796666477413626739591553936476D+00
        x(8) = 0.960289856497536231683560868569D+00

        w(1) = 0.101228536290376259152531354310D+00
        w(2) = 0.222381034453374470544355994426D+00
        w(3) = 0.313706645877887287337962201987D+00
        w(4) = 0.362683783378361982965150449277D+00
        w(5) = 0.362683783378361982965150449277D+00
        w(6) = 0.313706645877887287337962201987D+00
        w(7) = 0.222381034453374470544355994426D+00
        w(8) = 0.101228536290376259152531354310D+00

      else if ( n .eq. 9 ) then

        x(1) = -0.968160239507626089835576203D+00
        x(2) = -0.836031107326635794299429788D+00
        x(3) = -0.613371432700590397308702039D+00
        x(4) = -0.324253423403808929038538015D+00
        x(5) = 0.000000000000000000000000000D+00
        x(6) = 0.324253423403808929038538015D+00
        x(7) = 0.613371432700590397308702039D+00
        x(8) = 0.836031107326635794299429788D+00
        x(9) = 0.968160239507626089835576203D+00

        w(1) = 0.081274388361574411971892158111D+00
        w(2) = 0.18064816069485740405847203124D+00
        w(3) = 0.26061069640293546231874286942D+00
        w(4) = 0.31234707704000284006863040658D+00
        w(5) = 0.33023935500125976316452506929D+00
        w(6) = 0.31234707704000284006863040658D+00
        w(7) = 0.26061069640293546231874286942D+00
        w(8) = 0.18064816069485740405847203124D+00
        w(9) = 0.081274388361574411971892158111D+00

      else if ( n .eq. 10 ) then

        x(1) = -0.973906528517171720077964012D+00
        x(2) = -0.865063366688984510732096688D+00
        x(3) = -0.679409568299024406234327365D+00
        x(4) = -0.433395394129247190799265943D+00
        x(5) = -0.148874338981631210884826001D+00
        x(6) = 0.148874338981631210884826001D+00
        x(7) = 0.433395394129247190799265943D+00
        x(8) = 0.679409568299024406234327365D+00
        x(9) = 0.865063366688984510732096688D+00
        x(10) = 0.973906528517171720077964012D+00

        w(1) = 0.066671344308688137593568809893D+00
        w(2) = 0.14945134915058059314577633966D+00
        w(3) = 0.21908636251598204399553493423D+00
        w(4) = 0.26926671930999635509122692157D+00
        w(5) = 0.29552422471475287017389299465D+00
        w(6) = 0.29552422471475287017389299465D+00
        w(7) = 0.26926671930999635509122692157D+00
        w(8) = 0.21908636251598204399553493423D+00
        w(9) = 0.14945134915058059314577633966D+00
        w(10) = 0.066671344308688137593568809893D+00

      else if ( n .eq. 11 ) then

        x(1) = -0.978228658146056992803938001D+00
        x(2) = -0.887062599768095299075157769D+00
        x(3) = -0.730152005574049324093416252D+00
        x(4) = -0.519096129206811815925725669D+00
        x(5) = -0.269543155952344972331531985D+00
        x(6) = 0.000000000000000000000000000D+00
        x(7) = 0.269543155952344972331531985D+00
        x(8) = 0.519096129206811815925725669D+00
        x(9) = 0.730152005574049324093416252D+00
        x(10) = 0.887062599768095299075157769D+00
        x(11) = 0.978228658146056992803938001D+00

        w(1) = 0.055668567116173666482753720443D+00
        w(2) = 0.12558036946490462463469429922D+00
        w(3) = 0.18629021092773425142609764143D+00
        w(4) = 0.23319376459199047991852370484D+00
        w(5) = 0.26280454451024666218068886989D+00
        w(6) = 0.27292508677790063071448352834D+00
        w(7) = 0.26280454451024666218068886989D+00
        w(8) = 0.23319376459199047991852370484D+00
        w(9) = 0.18629021092773425142609764143D+00
        w(10) = 0.12558036946490462463469429922D+00
        w(11) = 0.055668567116173666482753720443D+00

      else if ( n .eq. 12 ) then

        x(1) = -0.981560634246719250690549090D+00
        x(2) = -0.904117256370474856678465866D+00
        x(3) = -0.769902674194304687036893833D+00
        x(4) = -0.587317954286617447296702419D+00
        x(5) = -0.367831498998180193752691537D+00
        x(6) = -0.125233408511468915472441369D+00
        x(7) = 0.125233408511468915472441369D+00
        x(8) = 0.367831498998180193752691537D+00
        x(9) = 0.587317954286617447296702419D+00
        x(10) = 0.769902674194304687036893833D+00
        x(11) = 0.904117256370474856678465866D+00
        x(12) = 0.981560634246719250690549090D+00

        w(1) = 0.047175336386511827194615961485D+00
        w(2) = 0.10693932599531843096025471819D+00
        w(3) = 0.16007832854334622633465252954D+00
        w(4) = 0.20316742672306592174906445581D+00
        w(5) = 0.23349253653835480876084989892D+00
        w(6) = 0.24914704581340278500056243604D+00
        w(7) = 0.24914704581340278500056243604D+00
        w(8) = 0.23349253653835480876084989892D+00
        w(9) = 0.20316742672306592174906445581D+00
        w(10) = 0.16007832854334622633465252954D+00
        w(11) = 0.10693932599531843096025471819D+00
        w(12) = 0.047175336386511827194615961485D+00

      else if ( n .eq. 13 ) then

        x(1) = -0.984183054718588149472829449D+00
        x(2) = -0.917598399222977965206547837D+00
        x(3) = -0.801578090733309912794206490D+00
        x(4) = -0.642349339440340220643984607D+00
        x(5) = -0.448492751036446852877912852D+00
        x(6) = -0.230458315955134794065528121D+00
        x(7) = 0.000000000000000000000000000D+00
        x(8) = 0.230458315955134794065528121D+00
        x(9) = 0.448492751036446852877912852D+00
        x(10) = 0.642349339440340220643984607D+00
        x(11) = 0.80157809073330991279420649D+00
        x(12) = 0.91759839922297796520654784D+00
        x(13) = 0.98418305471858814947282945D+00

        w(1) = 0.040484004765315879520021592201D+00
        w(2) = 0.092121499837728447914421775954D+00
        w(3) = 0.13887351021978723846360177687D+00
        w(4) = 0.17814598076194573828004669200D+00
        w(5) = 0.20781604753688850231252321931D+00
        w(6) = 0.22628318026289723841209018604D+00
        w(7) = 0.23255155323087391019458951527D+00
        w(8) = 0.22628318026289723841209018604D+00
        w(9) = 0.20781604753688850231252321931D+00
        w(10) = 0.17814598076194573828004669200D+00
        w(11) = 0.13887351021978723846360177687D+00
        w(12) = 0.092121499837728447914421775954D+00
        w(13) = 0.040484004765315879520021592201D+00

      else if ( n .eq. 14 ) then

        x(1) = -0.986283808696812338841597267D+00
        x(2) = -0.928434883663573517336391139D+00
        x(3) = -0.827201315069764993189794743D+00
        x(4) = -0.687292904811685470148019803D+00
        x(5) = -0.515248636358154091965290719D+00
        x(6) = -0.319112368927889760435671824D+00
        x(7) = -0.108054948707343662066244650D+00
        x(8) = 0.108054948707343662066244650D+00
        x(9) = 0.31911236892788976043567182D+00
        x(10) = 0.51524863635815409196529072D+00
        x(11) = 0.68729290481168547014801980D+00
        x(12) = 0.82720131506976499318979474D+00
        x(13) = 0.92843488366357351733639114D+00
        x(14) = 0.98628380869681233884159727D+00

        w(1) = 0.035119460331751863031832876138D+00
        w(2) = 0.08015808715976020980563327706D+00
        w(3) = 0.12151857068790318468941480907D+00
        w(4) = 0.15720316715819353456960193862D+00
        w(5) = 0.18553839747793781374171659013D+00
        w(6) = 0.20519846372129560396592406566D+00
        w(7) = 0.21526385346315779019587644332D+00
        w(8) = 0.21526385346315779019587644332D+00
        w(9) = 0.20519846372129560396592406566D+00
        w(10) = 0.18553839747793781374171659013D+00
        w(11) = 0.15720316715819353456960193862D+00
        w(12) = 0.12151857068790318468941480907D+00
        w(13) = 0.08015808715976020980563327706D+00
        w(14) = 0.035119460331751863031832876138D+00

      else if ( n .eq. 15 ) then

        x(1) = -0.987992518020485428489565719D+00
        x(2) = -0.937273392400705904307758948D+00
        x(3) = -0.848206583410427216200648321D+00
        x(4) = -0.724417731360170047416186055D+00
        x(5) = -0.570972172608538847537226737D+00
        x(6) = -0.394151347077563369897207371D+00
        x(7) = -0.201194093997434522300628303D+00
        x(8) = 0.00000000000000000000000000D+00
        x(9) = 0.20119409399743452230062830D+00
        x(10) = 0.39415134707756336989720737D+00
        x(11) = 0.57097217260853884753722674D+00
        x(12) = 0.72441773136017004741618605D+00
        x(13) = 0.84820658341042721620064832D+00
        x(14) = 0.93727339240070590430775895D+00
        x(15) = 0.98799251802048542848956572D+00

        w(1) = 0.030753241996117268354628393577D+00
        w(2) = 0.070366047488108124709267416451D+00
        w(3) = 0.107159220467171935011869546686D+00
        w(4) = 0.13957067792615431444780479451D+00
        w(5) = 0.16626920581699393355320086048D+00
        w(6) = 0.18616100001556221102680056187D+00
        w(7) = 0.19843148532711157645611832644D+00
        w(8) = 0.20257824192556127288062019997D+00
        w(9) = 0.19843148532711157645611832644D+00
        w(10) = 0.18616100001556221102680056187D+00
        w(11) = 0.16626920581699393355320086048D+00
        w(12) = 0.13957067792615431444780479451D+00
        w(13) = 0.107159220467171935011869546686D+00
        w(14) = 0.070366047488108124709267416451D+00
        w(15) = 0.030753241996117268354628393577D+00

      else if ( n .eq. 16 ) then

        x(1) = -0.989400934991649932596154173D+00
        x(2) = -0.944575023073232576077988416D+00
        x(3) = -0.865631202387831743880467898D+00
        x(4) = -0.755404408355003033895101195D+00
        x(5) = -0.617876244402643748446671764D+00
        x(6) = -0.458016777657227386342419443D+00
        x(7) = -0.281603550779258913230460501D+00
        x(8) = -0.09501250983763744018531934D+00
        x(9) = 0.09501250983763744018531934D+00
        x(10) = 0.28160355077925891323046050D+00
        x(11) = 0.45801677765722738634241944D+00
        x(12) = 0.61787624440264374844667176D+00
        x(13) = 0.75540440835500303389510119D+00
        x(14) = 0.86563120238783174388046790D+00
        x(15) = 0.94457502307323257607798842D+00
        x(16) = 0.98940093499164993259615417D+00

        w(1) = 0.027152459411754094851780572456D+00
        w(2) = 0.062253523938647892862843836994D+00
        w(3) = 0.09515851168249278480992510760D+00
        w(4) = 0.12462897125553387205247628219D+00
        w(5) = 0.14959598881657673208150173055D+00
        w(6) = 0.16915651939500253818931207903D+00
        w(7) = 0.18260341504492358886676366797D+00
        w(8) = 0.18945061045506849628539672321D+00
        w(9) = 0.18945061045506849628539672321D+00
        w(10) = 0.18260341504492358886676366797D+00
        w(11) = 0.16915651939500253818931207903D+00
        w(12) = 0.14959598881657673208150173055D+00
        w(13) = 0.12462897125553387205247628219D+00
        w(14) = 0.09515851168249278480992510760D+00
        w(15) = 0.062253523938647892862843836994D+00
        w(16) = 0.027152459411754094851780572456D+00

      else

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'LEGENDRE_SET - Fatal error!'
        write ( *, '(a,i8)' ) '  Illegal value of N = ', n
        write ( *, '(a)' ) '  Legal values are 1 through 16.'
        stop 1

      end if

      return
      end
      subroutine r8mat_fs ( n, a, b, info )

c*********************************************************************72
c
cc R8MAT_FS factors and solves a system with one right hand side.
c
c  Discussion:
c
c    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
c
c    This routine differs from R8MAT_FSS in two ways:
c    * only one right hand side is allowed;
c    * the input matrix A is not modified.
c
c    This routine uses partial pivoting, but no pivot vector is required.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 January 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer N, the order of the matrix.
c    N must be positive.
c
c    Input, double precision A(N,N), the coefficient matrix.
c
c    Input/output, double precision B(N).
c    On input, B is the right hand side of the linear system.
c    On output, B is the solution of the linear system.
c
c    Output, integer INFO, singularity flag.
c    0, no singularity detected.
c    nonzero, the factorization failed on the INFO-th step.
c
      implicit none

      integer n
      integer nb

      double precision a(n,n)
      double precision a2(n,n)
      double precision b(n)
      integer i
      integer info
      integer ipiv
      integer j
      integer jcol
      integer k
      double precision piv
      double precision temp
c
c  Copy the matrix.
c
      do j = 1, n
        do i = 1, n
          a2(i,j) = a(i,j)
        end do
      end do

      info = 0

      do jcol = 1, n
c
c  Find the maximum element in column I.
c
        piv = abs ( a2(jcol,jcol) )
        ipiv = jcol
        do i = jcol + 1, n
          if ( piv .lt. abs ( a2(i,jcol) ) ) then
            piv = abs ( a2(i,jcol) )
            ipiv = i
          end if
        end do

        if ( piv .eq. 0.0D+00 ) then
          info = jcol
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'R8MAT_FS - Fatal error!'
          write ( *, '(a,i8)' ) '  Zero pivot on step ', info
          return
        end if
c
c  Switch rows JCOL and IPIV, and B.
c
        if ( jcol .ne. ipiv ) then

          do j = 1, n
            temp = a2(jcol,j)
            a2(jcol,j) = a2(ipiv,j)
            a2(ipiv,j) = temp
          end do

          temp = b(jcol)
          b(jcol) = b(ipiv)
          b(ipiv) = temp

        end if
c
c  Scale the pivot row.
c
        do j = jcol + 1, n
          a2(jcol,j) = a2(jcol,j) / a2(jcol,jcol)
        end do
        b(jcol) = b(jcol) / a2(jcol,jcol)
        a2(jcol,jcol) = 1.0D+00
c
c  Use the pivot row to eliminate lower entries in that column.
c
        do i = jcol + 1, n
          if ( a2(i,jcol) .ne. 0.0D+00 ) then
            temp = - a2(i,jcol)
            a2(i,jcol) = 0.0D+00
            do j = jcol + 1, n
              a2(i,j) = a2(i,j) + temp * a2(jcol,j)
            end do
            b(i) = b(i) + temp * b(jcol)
          end if
        end do

      end do
c
c  Back solve.
c
      do k = n, 2, -1
        do i = 1, k - 1
          b(i) = b(i) - a2(i,k) * b(k)
        end do
      end do

      return
      end
      subroutine r8vec_linspace ( n, a, b, x )

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
c    Input, double precision A, B, the first and last entries.
c
c    Output, double precision X(N), a vector of linearly spaced data.
c
      implicit none

      integer n

      double precision a
      double precision b
      integer i
      double precision x(n)

      if ( n .eq. 1 ) then

        x(1) = ( a + b ) / 2.0D+00

      else

        do i = 1, n
          x(i) = ( dble ( n - i     ) * a
     &           + dble (     i - 1 ) * b )
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
      character * ( * ) title

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
