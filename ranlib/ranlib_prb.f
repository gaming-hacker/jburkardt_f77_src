      program main

c*********************************************************************72
c
cc MAIN is the main program for RANLIB_PRB.
c
c  Discussion:
c
c    RANLIB_PRB tests the RANLIB library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 April 2013
c
c  Author:
c
c    Original FORTRAN77 version by Barry Brown, James Lovato.
c    Modifications by John Burkardt
c
      implicit none

      character * ( 100 ) phrase

      call timestamp ( )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'RANLIB_PRB'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  Test the RANLIB library.'

      phrase = 'randomizer'

      call test_phrtsd ( phrase )

      call test_bot ( )

      call test_genbet ( phrase )
      call test_ignbin ( phrase )
      call test_genchi ( phrase )
      call test_genexp ( phrase )
      call test_genf ( phrase )
      call test_gengam ( phrase )
      call test_ignnbn ( phrase )
      call test_gennch ( phrase )
      call test_gennf ( phrase )
      call test_gennor ( phrase )
      call test_ignpoi ( phrase )
      call test_genunf ( phrase )
c
c  Terminate.
c
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'RANLIB_PRB'
      write ( *, '(a)' ) '  Normal end of execution.'
      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine test_phrtsd ( phrase )

c*********************************************************************72
c
cc TEST_PHRTSD tests PHRTSD, which computes two seeds from a random phrase.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    02 April 2013
c
c  Author:
c
c    Original FORTRAN77 version by Barry Brown, James Lovato.
c    Modifications by John Burkardt
c
      implicit none

      character * ( * ) phrase
      integer seed1
      integer seed2

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST_PHRTSD'
      write ( *, '(a)' ) '  Test PHRTSD,'
      write ( *, '(a)' ) 
     &  '  which generates two seeds from a random phrase.'

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  The phrase is "' // trim ( phrase ) // '".'

      call phrtsd ( phrase, seed1, seed2 )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i12)' ) '  SEED1 = ', seed1
      write ( *, '(a,i12)' ) '  SEED2 = ', seed2

      return
      end
      subroutine test_bot ( )

c*********************************************************************72
c
cc TEST_BOT is a test program for the bottom level routines
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    03 April 2013
c
c  Author:
c
c    Original FORTRAN77 version by Barry Brown, James Lovato.
c    Modifications by John Burkardt
c
      implicit none

      integer answer(10000)
      integer genlst(5)
      integer i4_uni
      integer ians
      integer iblock
      integer igen
      integer itmp
      integer ix
      integer ixgen
      integer nbad
      integer seed1
      integer seed2

      save genlst

      data genlst / 1, 5, 10, 20, 32 /

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST_BOT'
      write ( *, '(a)' ) 
     &  '  Test the lower level random number generators.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Five of the 32 generators will be tested.'
      write ( *, '(a)' ) '  We generate 100000 numbers, reset the block'
      write ( *, '(a)' ) 
     &  '  and do it again.  No disagreements should occur.'
      write ( *, '(a)' ) ' '
c
c  Initialize the generators.
c
      call initialize ( )
c
c  Set up all generators.
c
      seed1 = 12345
      seed2 = 54321
      call set_initial_seed ( seed1, seed2 )
c
c  For a selected set of generators
c
      nbad = 0

      do ixgen = 1, 5

        igen = genlst(ixgen)
        call cgn_set ( igen )
        write ( *, '(a,i2)' ) '  Testing generator ', igen
c
c  Use 10 blocks, and generate 1000 numbers per block
c
        call init_generator ( 0 )

        do iblock = 1, 10
          do ians = 1, 1000
            ix = ians + ( iblock - 1 ) * 1000
            answer(ix) = i4_uni ( )
          end do
          call init_generator ( 2 )
        end do
c
c  Do it again and compare answers
c  Use 10 blocks, and generate 1000 numbers.
c
        call init_generator ( 0 )

        do iblock = 1, 10
          do ians = 1, 1000
            ix = ians + ( iblock - 1 ) * 1000
            itmp = i4_uni ( )

            if ( itmp .ne. answer(ix) ) then

              write ( *, '(a)' ) ' '
              write ( *, '(a)' ) 'TEST_BOT - Warning!'
              write ( *, '(a)' ) '  Data disagreement:'
              write ( *, '(a,i6)' ) '  Block = ', iblock
              write ( *, '(a,i6)' ) '  N within block = ', ians
              write ( *, '(a,i6)' ) '  Index in ANSWER = ', ix
              write ( *, '(a,i10)' ) '  First value =  ', answer(ix)
              write ( *, '(a,i10)' ) '  Second value = ', itmp

              nbad = nbad + 1

              if ( 10 .lt. nbad ) then
                write ( *, '(a)' ) ' '
                write ( *, '(a)' ) 'TEST_BOT - Warning!'
                write ( *, '(a)' ) '  More than 10 mismatches!'
                write ( *, '(a)' ) '  Tests terminated early.'
                return
              end if

            end if

          end do

          call init_generator ( 2 )

        end do

      end do

      return
      end
      subroutine test_genbet ( phrase )

c*********************************************************************72
c
cc TEST_GENBET tests GENBET, which generates Beta deviates.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 March 2013
c
c  Author:
c
c    Original FORTRAN77 version by Barry Brown, James Lovato.
c    Modifications by John Burkardt
c
      implicit none

      integer n
      parameter ( n = 1000 )

      real a
      real array(n)
      real av
      real avtr
      real b
      real genbet
      real genunf
      real high
      integer i
      real low
      real param(2)
      character * ( 4 ) pdf
      character * ( * ) phrase
      integer seed1
      integer seed2
      real var
      real vartr
      real xmax
      real xmin

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST_GENBET'
      write ( *, '(a)' ) '  Test GENBET,'
      write ( *, '(a)' ) '  which generates Beta deviates.'
c
c  Initialize the generators.
c
      call initialize ( )
c
c  Set the seeds based on the phrase.
c
      call phrtsd ( phrase, seed1, seed2 )
c
c  Initialize all generators.
c
      call set_initial_seed ( seed1, seed2 )
c
c  Select the parameters at random within a given range.
c
      low = 1.0E+00
      high = 10.0E+00
      a = genunf ( low, high )

      low = 1.0E+00
      high = 10.0E+00
      b = genunf ( low, high )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i6)' ) '  N = ', n
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Parameters:'
      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  A = ', a
      write ( *, '(a,g14.6)' ) '  B = ', b
c
c  Generate N samples.
c
      do i = 1, n
        array(i) = genbet ( a, b )
      end do
c
c  Compute statistics on the samples.
c
      call stats ( array, n, av, var, xmin, xmax )
c
c  Request expected value of statistics for this distribution.
c
      pdf = 'bet'
      param(1) = a
      param(2) = b
      call trstat ( pdf, param, avtr, vartr )

      write ( *, '(a)' ) ' '
      write ( *, '(a,2g14.6)' ) 
     &  '  Sample data range:          ', xmin, xmax
      write ( *, '(a,2g14.6)' ) 
     &  '  Sample mean, variance:      ', av,   var
      write ( *, '(a,2g14.6)' ) 
     &  '  Distribution mean, variance ', avtr, vartr

      return
      end
      subroutine test_ignbin ( phrase )

c*********************************************************************72
c
cc TEST_IGNBIN tests IGNBIN, which generates Binomial deviates.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 March 2013
c
c  Author:
c
c    Original FORTRAN77 version by Barry Brown, James Lovato.
c    Modifications by John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10000 )

      real array(n)
      real av
      real avtr
      real genunf
      real high
      integer i
      integer ignbin
      real low
      integer nn
      real param(2)
      character * ( 4 ) pdf
      character * ( * ) phrase
      real pp
      integer seed1
      integer seed2
      real var
      real vartr
      real xmax
      real xmin

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST_IGNBIN'
      write ( *, '(a)' ) '  Test IGNBIN,'
      write ( *, '(a)' ) '  which generates binomial deviates.'
c
c  Initialize the generators.
c
      call initialize ( )
c
c  Set the seeds based on the phrase.
c
      call phrtsd ( phrase, seed1, seed2 )
c
c  Initialize all generators.
c
      call set_initial_seed ( seed1, seed2 )
c
c  Select the parameters at random within a given range.
c
      low = 0.5E+00
      high = 20.0E+00
      nn = int ( genunf ( low, high ) )

      low = 0.0E+00
      high = 1.0E+00
      pp = genunf ( low, high )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i6)' ) '  N = ', n
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Parameters:'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i6)' ) '  NN = ', nn
      write ( *, '(a,g14.6)' ) '  PP = ', pp
c
c  Generate N samples.
c
      do i = 1, n
        array(i) = real ( ignbin ( nn, pp ) )
      end do
c
c  Compute statistics on the samples.
c
      call stats ( array, n, av, var, xmin, xmax )
c
c  Request expected value of statistics for this distribution.
c
      pdf = 'bin'
      param(1) = real ( nn )
      param(2) = pp
      call trstat ( pdf, param, avtr, vartr )

      write ( *, '(a)' ) ' '
      write ( *, '(a,2g14.6)' ) 
     &  '  Sample data range:          ', xmin, xmax
      write ( *, '(a,2g14.6)' ) 
     &  '  Sample mean, variance:      ', av,   var
      write ( *, '(a,2g14.6)' ) 
     &  '  Distribution mean, variance ', avtr, vartr

      return
      end
      subroutine test_genchi ( phrase )

c*********************************************************************72
c
cc TEST_GENCHI tests GENCHI, which generates Chi-Square deviates.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 March 2013
c
c  Author:
c
c    Original FORTRAN77 version by Barry Brown, James Lovato.
c    Modifications by John Burkardt
c
      implicit none

      integer n
      parameter ( n = 1000 )

      real array(n)
      real av
      real avtr
      real df
      real genchi
      real genunf
      real high
      integer i
      real low
      real param(1)
      character * ( 4 ) pdf
      character * ( * ) phrase
      integer seed1
      integer seed2
      real var
      real vartr
      real xmax
      real xmin

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST_GENCHI'
      write ( *, '(a)' ) '  Test GENCHI,'
      write ( *, '(a)' ) '  which generates Chi-square deviates.'
c
c  Initialize the generators.
c
      call initialize ( )
c
c  Set the seeds based on the phrase.
c
      call phrtsd ( phrase, seed1, seed2 )
c
c  Initialize all generators.
c
      call set_initial_seed ( seed1, seed2 )
c
c  Select the parameters at random within a given range.
c
      low = 1.0E+00
      high = 10.0E+00
      df = genunf ( low, high )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i6)' ) '  N = ', n
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Parameters:'
      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  DF = ', df
c
c  Generate N samples.
c
      do i = 1, n
        array(i) = genchi ( df )
      end do
c
c  Compute statistics on the samples.
c
      call stats ( array, n, av, var, xmin, xmax )
c
c  Request expected value of statistics for this distribution.
c
      pdf = 'chi'
      param(1) = df
      call trstat ( pdf, param, avtr, vartr )

      write ( *, '(a)' ) ' '
      write ( *, '(a,2g14.6)' ) 
     &  '  Sample data range:          ', xmin, xmax
      write ( *, '(a,2g14.6)' ) 
     &  '  Sample mean, variance:      ', av,   var
      write ( *, '(a,2g14.6)' ) 
     &  '  Distribution mean, variance ', avtr, vartr

      return
      end
      subroutine test_genexp ( phrase )

c*********************************************************************72
c
cc TEST_GENEXP tests GENEXP, which generates exponential deviates.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 March 2013
c
c  Author:
c
c    Original FORTRAN77 version by Barry Brown, James Lovato.
c    Modifications by John Burkardt
c
      implicit none

      integer n
      parameter ( n = 1000 )

      real array(n)
      real av
      real avtr
      real genexp
      real genunf
      real high
      integer i
      real low
      real mu
      real param(2)
      character * ( 4 ) pdf
      character * ( * ) phrase
      integer seed1
      integer seed2
      real var
      real vartr
      real xmax
      real xmin

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST_GENEXP'
      write ( *, '(a)' ) '  Test GENEXP,'
      write ( *, '(a)' ) '  which generates exponential deviates.'
c
c  Initialize the generators.
c
      call initialize ( )
c
c  Set the seeds based on the phrase.
c
      call phrtsd ( phrase, seed1, seed2 )
c
c  Initialize all generators.
c
      call set_initial_seed ( seed1, seed2 )
c
c  Select the parameters at random within a given range.
c
      low =  0.5E+00
      high = 10.0E+00
      mu = genunf ( low, high )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i6)' ) '  N = ', n
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Parameters:'
      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  MU =   ', mu
c
c  Generate N samples.
c
      do i = 1, n
        array(i) = genexp ( mu )
      end do
c
c  Compute statistics on the samples.
c
      call stats ( array, n, av, var, xmin, xmax )
c
c  Request expected value of statistics for this distribution.
c
      pdf = 'exp'
      param(1) = mu

      call trstat ( pdf, param, avtr, vartr )

      write ( *, '(a)' ) ' '
      write ( *, '(a,2g14.6)' ) 
     &  '  Sample data range:          ', xmin, xmax
      write ( *, '(a,2g14.6)' ) 
     &  '  Sample mean, variance:      ', av,   var
      write ( *, '(a,2g14.6)' ) 
     &  '  Distribution mean, variance ', avtr, vartr

      return
      end
      subroutine test_genf ( phrase )

c*********************************************************************72
c
cc TEST_GENF tests GENF, which generates F deviates.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 March 2013
c
c  Author:
c
c    Original FORTRAN77 version by Barry Brown, James Lovato.
c    Modifications by John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10000 )

      real array(n)
      real av
      real avtr
      real dfd
      real dfn
      real genf
      real genunf
      real high
      integer i
      real low
      real param(2)
      character * ( 4 ) pdf
      character * ( * ) phrase
      integer seed1
      integer seed2
      real var
      real vartr
      real xmax
      real xmin

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST_GENF'
      write ( *, '(a)' ) '  Test GENF,'
      write ( *, '(a)' ) '  which generates F deviates.'
c
c  Initialize the generators.
c
      call initialize ( )
c
c  Set the seeds based on the phrase.
c
      call phrtsd ( phrase, seed1, seed2 )
c
c  Initialize all generators.
c
      call set_initial_seed ( seed1, seed2 )
c
c  Select the parameters at random within a given range.
c
      low = 3.0E+00
      high = 10.0E+00
      dfn = genunf ( low, high )

      low = 5.0E+00
      high = 10.0E+00
      dfd = genunf ( low, high )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i6)' ) '  N = ', n
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Parameters:'
      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  DFN =   ', dfn
      write ( *, '(a,g14.6)' ) '  DFD =   ', dfd
c
c  Generate N samples.
c
      do i = 1, n
        array(i) = genf ( dfn, dfd )
      end do
c
c  Compute statistics on the samples.
c
      call stats ( array, n, av, var, xmin, xmax )
c
c  Request expected value of statistics for this distribution.
c
      pdf = 'f'
      param(1) = dfn
      param(2) = dfd

      call trstat ( pdf, param, avtr, vartr )

      write ( *, '(a)' ) ' '
      write ( *, '(a,2g14.6)' ) 
     &  '  Sample data range:          ', xmin, xmax
      write ( *, '(a,2g14.6)' ) 
     &  '  Sample mean, variance:      ', av,   var
      write ( *, '(a,2g14.6)' ) 
     &  '  Distribution mean, variance ', avtr, vartr

      return
      end
      subroutine test_gengam ( phrase )

c*********************************************************************72
c
cc TEST_GENGAM tests GENGAM, which generates Gamma deviates.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 March 2013
c
c  Author:
c
c    Original FORTRAN77 version by Barry Brown, James Lovato.
c    Modifications by John Burkardt
c
      implicit none

      integer n
      parameter ( n = 1000 )

      real a
      real array(n)
      real av
      real avtr
      real gengam
      real genunf
      real high
      integer i
      real low
      real param(2)
      character * ( 4 ) pdf
      character * ( * ) phrase
      real r
      integer seed1
      integer seed2
      real var
      real vartr
      real xmax
      real xmin

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST_GENGAM'
      write ( *, '(a)' ) '  Test GENGAM,'
      write ( *, '(a)' ) '  which generates Gamma deviates.'
c
c  Initialize the generators.
c
      call initialize ( )
c
c  Set the seeds based on the phrase.
c
      call phrtsd ( phrase, seed1, seed2 )
c
c  Initialize all generators.
c
      call set_initial_seed ( seed1, seed2 )
c
c  Select the parameters at random within a given range.
c
      low = 1.0E+00
      high = 10.0E+00
      a = genunf ( low, high )

      low = 1.0E+00
      high = 10.0E+00
      r = genunf ( low, high )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i6)' ) '  N = ', n
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Parameters:'
      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  A = ', a
      write ( *, '(a,g14.6)' ) '  R = ', r
c
c  Generate N samples.
c
      do i = 1, n
        array(i) = gengam ( a, r )
      end do
c
c  Compute statistics on the samples.
c
      call stats ( array, n, av, var, xmin, xmax )
c
c  Request expected value of statistics for this distribution.
c
      pdf = 'gam'
      param(1) = a
      param(2) = r
      call trstat ( pdf, param, avtr, vartr )

      write ( *, '(a)' ) ' '
      write ( *, '(a,2g14.6)' ) 
     &  '  Sample data range:          ', xmin, xmax
      write ( *, '(a,2g14.6)' ) 
     &  '  Sample mean, variance:      ', av,   var
      write ( *, '(a,2g14.6)' ) 
     &  '  Distribution mean, variance ', avtr, vartr

      return
      end
      subroutine test_ignnbn ( phrase )

c*********************************************************************72
c
cc TEST_IGNNBN tests IGNNBN, which generates Negative Binomial deviates.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 March 2013
c
c  Author:
c
c    Original FORTRAN77 version by Barry Brown, James Lovato.
c    Modifications by John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10000 )

      real array(n)
      real av
      real avtr
      real genunf
      real high
      integer i
      integer ignnbn
      real low
      integer nn
      real param(2)
      character * ( 4 ) pdf
      character * ( * ) phrase
      real pp
      integer seed1
      integer seed2
      real var
      real vartr
      real xmax
      real xmin

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST_IGNNBN'
      write ( *, '(a)' ) '  Test IGNNBN,'
      write ( *, '(a)' ) '  which generates negative binomial deviates.'
c
c  Initialize the generators.
c
      call initialize ( )
c
c  Set the seeds based on the phrase.
c
      call phrtsd ( phrase, seed1, seed2 )
c
c  Initialize all generators.
c
      call set_initial_seed ( seed1, seed2 )
c
c  Select the parameters at random within a given range.
c
      low = 3.0E+00
      high = 20.0E+00
      nn = int ( genunf ( low, high ) )

      low = 0.0E+00
      high = 1.0E+00
      pp = genunf ( low, high )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i6)' ) '  N = ', n
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Parameters:'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i6)' ) '  NN = ', nn
      write ( *, '(a,g14.6)' ) '  PP = ', pp
c
c  Generate N samples.
c
      do i = 1, n
        array(i) = real ( ignnbn ( nn, pp ) )
      end do
c
c  Compute statistics on the samples.
c
      call stats ( array, n, av, var, xmin, xmax )
c
c  Request expected value of statistics for this distribution.
c
      pdf = 'nbn'
      param(1) = real ( nn )
      param(2) = pp
      call trstat ( pdf, param, avtr, vartr )

      write ( *, '(a)' ) ' '
      write ( *, '(a,2g14.6)' ) 
     &  '  Sample data range:          ', xmin, xmax
      write ( *, '(a,2g14.6)' ) 
     &  '  Sample mean, variance:      ', av,   var
      write ( *, '(a,2g14.6)' ) 
     &  '  Distribution mean, variance ', avtr, vartr

      return
      end
      subroutine test_gennch ( phrase )

c*********************************************************************72
c
cc TEST_GENNCH tests GENNCH, which generates noncentral Chi-Square deviates.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    21 March 2013
c
c  Author:
c
c    Original FORTRAN77 version by Barry Brown, James Lovato.
c    Modifications by John Burkardt
c
      implicit none

      integer n
      parameter ( n = 1000 )

      real array(n)
      real av
      real avtr
      real df
      real gennch
      real genunf
      real high
      integer i
      real low
      real param(2)
      character * ( 4 ) pdf
      character * ( * ) phrase
      integer seed1
      integer seed2
      real var
      real vartr
      real xmax
      real xmin
      real xnonc

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST_GENNCH'
      write ( *, '(a)' ) '  Test GENNCH,'
      write ( *, '(a)' ) 
     &  '  which generates noncentral Chi-square deviates.'
c
c  Initialize the generators.
c
      call initialize ( )
c
c  Set the seeds based on the phrase.
c
      call phrtsd ( phrase, seed1, seed2 )
c
c  Initialize all generators.
c
      call set_initial_seed ( seed1, seed2 )
c
c  Select the parameters at random within a given range.
c
      low = 2.0E+00
      high = 10.0E+00
      df = genunf ( low, high )

      low = 0.0E+00
      high = 2.0E+00
      xnonc = genunf ( low, high )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i6)' ) '  N = ', n
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Parameters:'
      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  DF =    ', df
      write ( *, '(a,g14.6)' ) '  XNONC = ', xnonc
c
c  Generate N samples.
c
      do i = 1, n
        array(i) = gennch ( df, xnonc )
      end do
c
c  Compute statistics on the samples.
c
      call stats ( array, n, av, var, xmin, xmax )
c
c  Request expected value of statistics for this distribution.
c
      pdf = 'nch'
      param(1) = df
      param(2) = xnonc
      call trstat ( pdf, param, avtr, vartr )

      write ( *, '(a)' ) ' '
      write ( *, '(a,2g14.6)' ) 
     &  '  Sample data range:          ', xmin, xmax
      write ( *, '(a,2g14.6)' ) 
     &  '  Sample mean, variance:      ', av,   var
      write ( *, '(a,2g14.6)' ) 
     &  '  Distribution mean, variance ', avtr, vartr

      return
      end
      subroutine test_gennf ( phrase )

c*********************************************************************72
c
cc TEST_GENNF tests GENNF, which generates noncentral F deviates.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 March 2013
c
c  Author:
c
c    Original FORTRAN77 version by Barry Brown, James Lovato.
c    Modifications by John Burkardt
c
      implicit none

      integer n
      parameter ( n = 10000 )

      real array(n)
      real av
      real avtr
      real dfd
      real dfn
      real gennf
      real genunf
      real high
      integer i
      real low
      real param(3)
      character * ( 4 ) pdf
      character * ( * ) phrase
      integer seed1
      integer seed2
      real var
      real vartr
      real xmax
      real xmin
      real xnonc

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST_GENNF'
      write ( *, '(a)' ) '  Test GENNF,'
      write ( *, '(a)' ) '  which generates noncentral F deviates.'
c
c  Initialize the generators.
c
      call initialize ( )
c
c  Set the seeds based on the phrase.
c
      call phrtsd ( phrase, seed1, seed2 )
c
c  Initialize all generators.
c
      call set_initial_seed ( seed1, seed2 )
c
c  Select the parameters at random within a given range.
c
      low = 3.0E+00
      high = 10.0E+00
      dfn = genunf ( low, high )

      low = 5.0E+00
      high = 10.0E+00
      dfd = genunf ( low, high )

      low = 0.0E+00
      high = 2.0E+00
      xnonc = genunf ( low, high )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i6)' ) '  N = ', n
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Parameters:'
      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  DFN =   ', dfn
      write ( *, '(a,g14.6)' ) '  DFD =   ', dfd
      write ( *, '(a,g14.6)' ) '  XNONC = ', xnonc
c
c  Generate N samples.
c
      do i = 1, n
        array(i) = gennf ( dfn, dfd, xnonc )
      end do
c
c  Compute statistics on the samples.
c
      call stats ( array, n, av, var, xmin, xmax )
c
c  Request expected value of statistics for this distribution.
c
      pdf = 'nf'
      param(1) = dfn
      param(2) = dfd
      param(3) = xnonc
      call trstat ( pdf, param, avtr, vartr )

      write ( *, '(a)' ) ' '
      write ( *, '(a,2g14.6)' ) 
     &  '  Sample data range:          ', xmin, xmax
      write ( *, '(a,2g14.6)' ) 
     &  '  Sample mean, variance:      ', av,   var
      write ( *, '(a,2g14.6)' ) 
     &  '  Distribution mean, variance ', avtr, vartr

      return
      end
      subroutine test_gennor ( phrase )

c*********************************************************************72
c
cc TEST_GENNOR tests GENNOR, which generates normal deviates.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 March 2013
c
c  Author:
c
c    Original FORTRAN77 version by Barry Brown, James Lovato.
c    Modifications by John Burkardt
c
      implicit none

      integer n
      parameter ( n = 1000 )

      real array(n)
      real av
      real avtr
      real gennor
      real genunf
      real high
      integer i
      real low
      real mu
      real param(2)
      character * ( 4 ) pdf
      character * ( * ) phrase
      real sd
      integer seed1
      integer seed2
      real var
      real vartr
      real xmax
      real xmin

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST_GENNOR'
      write ( *, '(a)' ) '  Test GENNOR,'
      write ( *, '(a)' ) '  which generates normal deviates.'
c
c  Initialize the generators.
c
      call initialize ( )
c
c  Set the seeds based on the phrase.
c
      call phrtsd ( phrase, seed1, seed2 )
c
c  Initialize all generators.
c
      call set_initial_seed ( seed1, seed2 )
c
c  Select the parameters at random within a given range.
c
      low = -10.0E+00
      high = 10.0E+00
      mu = genunf ( low, high )

      low = 0.25E+00
      high = 4.0E+00
      sd = genunf ( low, high )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i6)' ) '  N = ', n
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Parameters:'
      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  MU =   ', mu
      write ( *, '(a,g14.6)' ) '  SD =   ', sd
c
c  Generate N samples.
c
      do i = 1, n
        array(i) = gennor ( mu, sd )
      end do
c
c  Compute statistics on the samples.
c
      call stats ( array, n, av, var, xmin, xmax )
c
c  Request expected value of statistics for this distribution.
c
      pdf = 'nor'
      param(1) = mu
      param(2) = sd

      call trstat ( pdf, param, avtr, vartr )

      write ( *, '(a)' ) ' '
      write ( *, '(a,2g14.6)' ) 
     &  '  Sample data range:          ', xmin, xmax
      write ( *, '(a,2g14.6)' ) 
     &  '  Sample mean, variance:      ', av,   var
      write ( *, '(a,2g14.6)' ) 
     &  '  Distribution mean, variance ', avtr, vartr

      return
      end
      subroutine test_ignpoi ( phrase )

c*********************************************************************72
c
cc TEST_IGNPOI tests IGNPOI, which generates Poisson deviates.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    29 March 2013
c
c  Author:
c
c    Original FORTRAN77 version by Barry Brown, James Lovato.
c    Modifications by John Burkardt
c
      implicit none

      integer n
      parameter ( n = 1000 )

      real array(n)
      real av
      real avtr
      real genunf
      real high
      integer i
      integer ignpoi
      real low
      real mu
      real param(1)
      character * ( 4 ) pdf
      character * ( * ) phrase
      integer seed1
      integer seed2
      real var
      real vartr
      real xmax
      real xmin

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST_IGNPOI'
      write ( *, '(a)' ) '  Test IGNPOI,'
      write ( *, '(a)' ) '  which generates Poisson deviates.'
c
c  Initialize the generators.
c
      call initialize ( )
c
c  Set the seeds based on the phrase.
c
      call phrtsd ( phrase, seed1, seed2 )
c
c  Initialize all generators.
c
      call set_initial_seed ( seed1, seed2 )
c
c  Select the parameters at random within a given range.
c
      low = 0.5E+00
      high = 20.0E+00
      mu = genunf ( low, high )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i6)' ) '  N = ', n
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Parameters:'
      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  MU = ', mu
c
c  Generate N samples.
c
      do i = 1, n
        array(i) = real ( ignpoi ( mu ) )
      end do
c
c  Compute statistics on the samples.
c
      call stats ( array, n, av, var, xmin, xmax )
c
c  Request expected value of statistics for this distribution.
c
      pdf = 'poi'
      param(1) = mu
      call trstat ( pdf, param, avtr, vartr )

      write ( *, '(a)' ) ' '
      write ( *, '(a,2g14.6)' ) 
     &  '  Sample data range:          ', xmin, xmax
      write ( *, '(a,2g14.6)' ) 
     &  '  Sample mean, variance:      ', av,   var
      write ( *, '(a,2g14.6)' ) 
     &  '  Distribution mean, variance ', avtr, vartr

      return
      end
      subroutine test_genunf ( phrase )

c*********************************************************************72
c
cc TEST_GENUNF tests GENUNF, which generates uniform deviates.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    28 March 2013
c
c  Author:
c
c    Original FORTRAN77 version by Barry Brown, James Lovato.
c    Modifications by John Burkardt
c
      implicit none

      integer n
      parameter ( n = 1000 )

      real a
      real array(n)
      real av
      real avtr
      real b
      real genunf
      real high
      integer i
      real low
      real param(2)
      character * ( 4 ) pdf
      character * ( * ) phrase
      integer seed1
      integer seed2
      real var
      real vartr
      real xmax
      real xmin

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'TEST_GENUNF'
      write ( *, '(a)' ) '  Test GENUNF,'
      write ( *, '(a)' ) '  which generates uniform deviates.'
c
c  Initialize the generators.
c
      call initialize ( )
c
c  Set the seeds based on the phrase.
c
      call phrtsd ( phrase, seed1, seed2 )
c
c  Initialize all generators.
c
      call set_initial_seed ( seed1, seed2 )
c
c  Select the parameters at random within a given range.
c
      low = 1.0E+00
      high = 10.0E+00
      a = genunf ( low, high )

      low = a + 1.0E+00
      high = a + 10.0E+00
      b = genunf ( low, high )

      write ( *, '(a)' ) ' '
      write ( *, '(a,i6)' ) '  N = ', n
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Parameters:'
      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6)' ) '  A = ', a
      write ( *, '(a,g14.6)' ) '  B = ', b
c
c  Generate N samples.
c
      do i = 1, n
        array(i) = genunf ( a, b )
      end do
c
c  Compute statistics on the samples.
c
      call stats ( array, n, av, var, xmin, xmax )
c
c  Request expected value of statistics for this distribution.
c
      pdf = 'unf'
      param(1) = a
      param(2) = b
      call trstat ( pdf, param, avtr, vartr )

      write ( *, '(a)' ) ' '
      write ( *, '(a,2g14.6)' ) 
     &  '  Sample data range:          ', xmin, xmax
      write ( *, '(a,2g14.6)' ) 
     &  '  Sample mean, variance:      ', av,   var
      write ( *, '(a,2g14.6)' ) 
     &  '  Distribution mean, variance ', avtr, vartr

      return
      end
