      program bisect_test

c*********************************************************************72
c
cc bisect_test tests bisect
c
      real a
      real b
      real error
      real ff
      external ff
      integer iflag
      real xi
      real xtol
      
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) 'bisect_test'
      write ( *, '(a)' ) '  Test bisect()'

      a = 1.0
      b = 2.0
      xtol = 1.0e-6

      call bisect ( ff, a, b, xtol, iflag )

      if ( iflag < 0 ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) '  bisect() returned error flag.'
        stop 1
      end if

      xi = ( a + b ) / 2.0
      error = ( b - a ) / 2.0
      write ( *, '(a)' ) ''
      write ( *, '(a,f12.8)' ) '  Estimated zero is x = ', xi
      write ( *, '(a,e14.6)' ) '  with error bound ', error

      stop 0
      end
      function ff ( x )
      real ff
      real x
      ff = - 1.0 - x * ( 1.0 - x * x )
      write ( *, '(a,f12.8,a,e14.6)' ) '  X = ', x, '  f(x) = ', ff
      return
      end

      
