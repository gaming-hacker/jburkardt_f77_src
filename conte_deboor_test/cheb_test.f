      program cheb_test

c*********************************************************************72
c
cc cheb_test test cheb.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    14 February 2003
c
c  Author:
c
c    John Burkardt
c
      real c(15)
      data c /
     & 1.266065877752008,
     & 1.130318207984970,
     & 0.271495339534077,
     & 0.044336849848664,
     & 0.005474240442094,
     & 0.000542926311914,
     & 0.000044977322954,
     & 0.000003198436462,
     & 0.000000199212481,
     & 0.000000011036772,
     & 0.000000000550590,
     & 0.000000000024980,
     & 0.000000000001039,
     & 0.000000000000040,
     & 0.000000000000001 /
      real d(30)
      integer i
      integer nterms
      integer nx
      real value_cheb

      common /poly/ nterms, d

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'cheb_test:'
      write ( *, '(a)' ) '  Test cheb.'

      nterms = 15
      do i = 1, nterms
        d(i) = c(i)
      end do

      nx = 11

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  X   cheb(exp)(x)  exp(x)'
      write ( *, '(a)' ) ''

      do i = 1, nx
        x = ( ( nx - i ) * 0.0 + ( i - 1 ) * 5.0 ) / ( nx - 1 )
        value_cheb = cheb ( x )
        write ( *, '(2x,f8.4,f14.8,f14.8)' ) x, value_cheb, exp(x)
      end do
c
c  Terminate.
c
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'cheb_test:'
      write ( *, '(a)' ) '  Normal end of execution.'

      stop 0
      end
  
