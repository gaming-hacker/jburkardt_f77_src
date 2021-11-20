      subroutine advance_state ( k )

c*********************************************************************72
c
cc ADVANCE_STATE advances the state of the current generator.
c
c  Discussion:
c
c    This procedure advances the state of the current generator by 2^K 
c    values and resets the initial seed to that value.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 March 2013
c
c  Author:
c
c    Original Pascal version by Pierre L'Ecuyer, Serge Cote.
c    FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Pierre LEcuyer, Serge Cote,
c    Implementing a Random Number Package with Splitting Facilities,
c    ACM Transactions on Mathematical Software,
c    Volume 17, Number 1, March 1991, pages 98-111.
c
c  Parameters:
c
c    Input, integer K, indicates that the generator is to be 
c    advanced by 2^K values.
c    0 <= K.
c
      implicit none

      integer a1
      parameter ( a1 = 40014 )
      integer a2
      parameter ( a2 = 40692 )
      integer b1
      integer b2
      integer cg1
      integer cg2
      integer cgn_get
      integer g
      integer i
      logical initialized_get
      integer k
      integer m1
      parameter ( m1 = 2147483563 )
      integer m2
      parameter ( m2 = 2147483399 )
      integer multmod

      if ( k .lt. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'ADVANCE_STATE - Fatal error!'
        write ( *, '(a)' ) '  Input exponent K is out of bounds.'
        stop
      end if
c
c  Check whether the package must be initialized.
c
      if ( .not. initialized_get ( ) ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'ADVANCE_STATE - Note:'
        write ( *, '(a)' ) '  Initializing RNGLIB package.'
        call initialize ( )
      end if
c
c  Get the current generator index.
c
      g = cgn_get ( )

      b1 = a1
      b2 = a2

      do i = 1, k
        b1 = multmod ( b1, b1, m1 )
        b2 = multmod ( b2, b2, m2 )
      end do

      call cg_get ( g, cg1, cg2 )
      cg1 = multmod ( b1, cg1, m1 )
      cg2 = multmod ( b2, cg2, m2 )
      call cg_set ( g, cg1, cg2 )

      return
      end
      function antithetic_get ( )

c*********************************************************************72
c
cc ANTITHETIC_GET queries the antithetic value for the current generator.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 March 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, logical ANTITHETIC_GET, is TRUE if the current generator 
c    is antithetic.
c
      implicit none

      logical antithetic_get
      integer i
      logical value

      i = -1
      call antithetic_memory ( i, value )

      antithetic_get = value

      return
      end
      subroutine antithetic_memory ( i, value )

c*********************************************************************72
c
cc ANTITHETIC_MEMORY stores the antithetic value for the current generator.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 March 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer I, the desired action.
c    -1, get a value.
c    0, initialize all values.
c    1, set a value.
c
c    Input/output, logical VALUE.  For I = -1, VALUE is an output
c    quantity, for I = +1 it is an input quantity.
c
      implicit none

      integer g_max
      parameter ( g_max = 32 )

      logical a_save(g_max)
      integer cgn_get
      integer g
      integer i
      logical value

      save a_save

      data a_save / 32 * .false. /

      if ( i .lt. 0 ) then
        g = cgn_get ( )
        value = a_save(g)
      else if ( i .eq. 0 ) then
        a_save(1:g_max) = .false.
      else if ( 0 .lt. i ) then
        g = cgn_get ( )
        a_save(g) = value
      end if

      return
      end
      subroutine antithetic_set ( value )

c*********************************************************************72
c
cc ANTITHETIC_SET sets the antithetic value for the current generator.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 March 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, logical VALUE, is TRUE if the generator is to be antithetic.
c
      implicit none

      integer i
      logical value

      i = +1
      call antithetic_memory ( i, value )

      return
      end
      subroutine cg_get ( g, cg1, cg2 )

c*********************************************************************72
c
cc CG_GET queries the CG values for a given generator.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 March 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer G, the index of the generator.
c    1 <= G <= 32.
c
c    Output, integer CG1, CG2, the CG values for generator G.
c
      implicit none

      integer cg1
      integer cg2
      integer g
      integer i

      i = -1
      call cg_memory ( i, g, cg1, cg2 )

      return
      end
      subroutine cg_memory ( i, g, cg1, cg2 )

c*********************************************************************72
c
cc CG_MEMORY stores the CG values for all generators.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 March 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer I, the desired action.
c    -1, get a value.
c    0, initialize all values.
c    1, set a value.
c
c    Input, integer G, for I = -1 or +1, the index of 
c    the generator, with 1 <= G <= 32.
c
c    Input/output, integer CG1, CG2.  For I = -1, 
c    these are output, for I = +1, these are input, for I = 0,
c    these arguments are ignored.  When used, the arguments are
c    old or new values of the CG parameter for generator G.
c
      implicit none

      integer g_max
      parameter ( g_max = 32 )

      integer cg1
      integer cg1_save(g_max)
      integer cg2
      integer cg2_save(g_max)
      integer g
      integer i
      integer j

      save cg1_save
      save cg2_save

      data cg1_save / 32 * 0 /
      data cg2_save / 32 * 0 /

      if ( g .lt. 1 .or. g_max .lt. g ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'CG_MEMORY - Fatal error!'
        write ( *, '(a)' ) '  Input generator index G is out of bounds.'
        stop
      end if

      if ( i .lt. 0 ) then
        cg1 = cg1_save(g)
        cg2 = cg2_save(g)
      else if ( i .eq. 0 ) then
        do j = 1, g_max
          cg1_save(j) = 0
          cg2_save(j) = 0
        end do
      else if ( 0 .lt. i ) then
        cg1_save(g) = cg1
        cg2_save(g) = cg2
      end if

      return
      end
      subroutine cg_set ( g, cg1, cg2 )

c*********************************************************************72
c
cc CG_SET sets the CG values for a given generator.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 March 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer G, the index of the generator.
c    1 <= G <= 32.
c
c    Input, integer CG1, CG2, the CG values for generator G.
c
      implicit none

      integer cg1
      integer cg2
      integer g
      integer i

      i = +1
      call cg_memory ( i, g, cg1, cg2 )

      return
      end
      function cgn_get ( )

c*********************************************************************72
c
cc CGN_GET gets the current generator index.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 March 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Output, integer CGN_GET, the current generator index.
c    1 <= CGN_GET <= 32.
c
      implicit none

      integer cgn_get
      integer g
      integer i

      i = -1
      call cgn_memory ( i, g )

      cgn_get = g

      return
      end
      subroutine cgn_memory ( i, g )

c*********************************************************************72
c
cc CGN_MEMORY stores the current generator index.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 March 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer I, the desired action.
c    -1, get the value.
c    0, initialize the value.
c    1, set the value.
c
c    Input/output, integer G.  For I = -1 or 0,
c    this is output, for I = +1, this is input.
c
      implicit none

      integer g_max
      parameter ( g_max = 32 )

      integer g
      integer g_save
      integer i

      save g_save

      data g_save / 1 /

      if ( i .lt. 0 ) then

        g = g_save

      else if ( i .eq. 0 ) then

        g_save = 1
        g = g_save

      else if ( 0 .lt. i ) then

        if ( g .lt. 1 .or. g_max .lt. g ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'CGN_MEMORY - Fatal error!'
          write ( *, '(a)' ) '  Generator index G is out of bounds.'
          stop
        end if

        g_save = g

      end if

      return
      end
      subroutine cgn_set ( g )

c*********************************************************************72
c
cc CGN_SET sets the current generator index.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 March 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer G, the index of the generator.
c    1 <= G <= 32.
c
      implicit none

      integer g
      integer i

      i = +1
      call cgn_memory ( i, g )

      return
      end
      subroutine get_state ( cg1, cg2 )

c*********************************************************************72
c
cc GET_STATE returns the state of the current generator.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 March 2013
c
c  Author:
c
c    Original Pascal version by Pierre L'Ecuyer, Serge Cote.
c    FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Pierre LEcuyer, Serge Cote,
c    Implementing a Random Number Package with Splitting Facilities,
c    ACM Transactions on Mathematical Software,
c    Volume 17, Number 1, March 1991, pages 98-111.
c
c  Parameters:
c
c    Output, integer CG1, CG2, the CG values for the current generator.
c
      implicit none

      integer cg1
      integer cg2
      integer cgn_get
      integer g
      logical initialized_get
c
c  Check whether the package must be initialized.
c
      if ( .not. initialized_get ( ) ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'GET_STATE - Note:'
        write ( *, '(a)' ) '  Initializing RNGLIB package.'
        call initialize ( )
      end if
c
c  Get the current generator index.
c
      g = cgn_get ( )
c
c  Retrieve the seed values for this generator.
c
      call cg_get ( g, cg1, cg2 )

      return
      end
      function i4_uni ( )

c*********************************************************************72
c
cc I4_UNI generates a random positive integer.
c
c  Discussion:
c
c    This procedure returns a random integer following a uniform distribution 
c    over (1, 2147483562) using the current generator.
c
c    The original name of this function was "random()", but this conflicts
c    with a standard library function name in C.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 August 2013
c
c  Author:
c
c    Original Pascal version by Pierre L'Ecuyer, Serge Cote.
c    FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Pierre LEcuyer, Serge Cote,
c    Implementing a Random Number Package with Splitting Facilities,
c    ACM Transactions on Mathematical Software,
c    Volume 17, Number 1, March 1991, pages 98-111.
c
c  Parameters:
c
c    Output, integer I4_UNI, the random integer.
c
      implicit none

      integer a1
      parameter ( a1 = 40014 )
      integer a2
      parameter ( a2 = 40692 )
      logical antithetic_get
      integer cg1
      integer cg2
      integer cgn_get
      integer g
      integer i4_uni
      logical initialized_get
      integer k
      integer m1
      parameter ( m1 = 2147483563 )
      integer m2
      parameter ( m2 = 2147483399 )
      logical value
      integer z
c
c  Check whether the package must be initialized.
c
      if ( .not. initialized_get ( ) ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'I4_UNI - Note:'
        write ( *, '(a)' ) '  Initializing RNGLIB package.'
        call initialize ( )
      end if
c
c  Get the current generator index.
c
      g = cgn_get ( )
c
c  Retrieve the seeds for the current generator.
c
      call cg_get ( g, cg1, cg2 )
c
c  Update the seeds.
c
      k = cg1 / 53668
      cg1 = a1 * ( cg1 - k * 53668 ) - k * 12211

      if ( cg1 .lt. 0 ) then
        cg1 = cg1 + m1
      end if

      k = cg2 / 52774
      cg2 = a2 * ( cg2 - k * 52774 ) - k * 3791

      if ( cg2 .lt. 0 ) then
        cg2 = cg2 + m2
      end if
c
c  Store the new seeds.
c
      call cg_set ( g, cg1, cg2 )
c
c  Construct the random integer from the seeds.
c
      z = cg1 - cg2

      if ( z .lt. 1 ) then
        z = z + m1 - 1
      end if
c
c  If the generator is in antithetic mode, we must reflect the value.
c
      value = antithetic_get ( )

      if ( value ) then
        z = m1 - z
      end if
c
c  Return the value.
c
      i4_uni = z

      return
      end
      subroutine ig_get ( g, ig1, ig2 )

c*********************************************************************72
c
cc IG_GET queries the IG values for a given generator.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 March 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer G, the index of the generator.
c    1 <= G <= 32.
c
c    Output, integer IG1, IG2, the IG values for generator G.
c
      implicit none

      integer g
      integer i
      integer ig1
      integer ig2

      i = -1
      call ig_memory ( i, g, ig1, ig2 )

      return
      end
      subroutine ig_memory ( i, g, ig1, ig2 )

c*********************************************************************72
c
cc IG_MEMORY stores the IG values for all generators.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 March 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer I, the desired action.
c    -1, get a value.
c    0, initialize all values.
c    1, set a value.
c
c    Input, integer G, for I = -1 or +1, the index of 
c    the generator, with 1 <= G <= 32.
c
c    Input/output, integer IG1, IG2.  For I = -1, 
c    these are output, for I = +1, these are input, for I = 0,
c    these arguments are ignored.  When used, the arguments are
c    old or new values of the IG parameter for generator G.
c
      implicit none

      integer g_max
      parameter ( g_max = 32 )

      integer g
      integer i
      integer ig1
      integer ig1_save(g_max)
      integer ig2
      integer ig2_save(g_max)
      integer j

      save ig1_save
      save ig2_save

      data ig1_save / 32 * 0 /
      data ig2_save / 32 * 0 /

      if ( g .lt. 1 .or. g_max .lt. g ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'IG_MEMORY - Fatal error!'
        write ( *, '(a)' ) '  Input generator index G is out of bounds.'
        stop
      end if

      if ( i .lt. 0 ) then
        ig1 = ig1_save(g)
        ig2 = ig2_save(g)
      else if ( i .eq. 0 ) then
        do j = 1, g_max
          ig1_save(j) = 0
          ig2_save(j) = 0
        end do
      else if ( 0 .lt. i ) then
        ig1_save(g) = ig1
        ig2_save(g) = ig2
      end if

      return
      end
      subroutine ig_set ( g, ig1, ig2 )

c*********************************************************************72
c
cc IG_SET sets the IG values for a given generator.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 March 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer G, the index of the generator.
c    1 <= G <= 32.
c
c    Input, integer IG1, IG2, the IG values for generator G.
c
      implicit none

      integer g
      integer i
      integer ig1
      integer ig2

      i = +1
      call ig_memory ( i, g, ig1, ig2 )

      return
      end
      subroutine init_generator ( t )

c*********************************************************************72
c
cc INIT_GENERATOR sets the current generator G to initial, last or new seed.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 March 2013
c
c  Author:
c
c    Original Pascal version by Pierre L'Ecuyer, Serge Cote.
c    FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Pierre LEcuyer, Serge Cote,
c    Implementing a Random Number Package with Splitting Facilities,
c    ACM Transactions on Mathematical Software,
c    Volume 17, Number 1, March 1991, pages 98-111.
c
c  Parameters:
c
c    Input, integer T, the seed type:
c    0, use the seed chosen at initialization time.
c    1, use the last seed.
c    2, use a new seed set 2^30 values away.
c
      implicit none

      integer a1_w
      parameter ( a1_w = 1033780774 )
      integer a2_w 
      parameter ( a2_w = 1494757890 )
      integer cg1
      integer cg2
      integer cgn_get
      integer g
      integer ig1
      integer ig2
      logical initialized_get
      integer lg1
      integer lg2
      integer m1
      parameter ( m1 = 2147483563 )
      integer m2
      parameter ( m2 = 2147483399 )
      integer multmod
      integer t
c
c  Check whether the package must be initialized.
c
      if ( .not. initialized_get ( ) ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'INIT_GENERATOR - Note:'
        write ( *, '(a)' ) '  Initializing RNGLIB package.'
        call initialize ( )
      end if
c
c  Get the current generator index.
c
      g = cgn_get ( )
c
c  0: restore the initial seed.
c
      if ( t .eq. 0 ) then

        call ig_get ( g, ig1, ig2 )
        lg1 = ig1
        lg2 = ig2
        call lg_set ( g, lg1, lg2 )
c
c  1: restore the last seed.
c
      else if ( t .eq. 1 ) then

        call lg_get ( g, lg1, lg2 )
c
c  2: advance to a new seed.
c
      else if ( t .eq. 2 ) then

        call lg_get ( g, lg1, lg2 )
        lg1 = multmod ( a1_w, lg1, m1 )
        lg2 = multmod ( a2_w, lg2, m2 )
        call lg_set ( g, lg1, lg2 )

      else

        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'INIT_GENERATOR - Fatal error!'
        write ( *, '(a)' ) '  Input parameter T out of bounds.'
        stop

      end if
c
c  Store the new seed.
c
      cg1 = lg1
      cg2 = lg2
      call cg_set ( g, cg1, cg2 )

      return
      end
      subroutine initialize ( )

c*********************************************************************72
c
cc INITIALIZE initializes the random number generator library.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 March 2013
c
c  Author:
c
c    Original Pascal version by Pierre L'Ecuyer, Serge Cote.
c    FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Pierre LEcuyer, Serge Cote,
c    Implementing a Random Number Package with Splitting Facilities,
c    ACM Transactions on Mathematical Software,
c    Volume 17, Number 1, March 1991, pages 98-111.
c
c  Parameters:
c
c    None
c
      implicit none

      integer g
      integer g_max
      parameter ( g_max = 32 )
      integer ig1
      integer ig2
      logical value
c
c  Remember that we have called INITIALIZE().
c
      call initialized_set ( )
c
c  Initialize all generators to have FALSE antithetic value.
c
      value = .false.
      do g = 1, g_max
        call cgn_set ( g )
        call antithetic_set ( value )
      end do
c
c  Set the initial seeds.
c
      ig1 = 1234567890
      ig2 = 123456789
      call set_initial_seed ( ig1, ig2 )
c
c  Initialize the current generator index to the first one.
c
      g = 1
      call cgn_set ( g )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'INITIALIZE - Note:'
      write ( *, '(a)' ) '  The RNGLIB package has been initialized.'

      return
      end
      function initialized_get ( )

c*********************************************************************72
c
cc INITIALIZED_GET queries the INITIALIZED value.
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
c    John Burkardt
c
c  Parameters:
c
c    Output, logical INITIALIZED_GET, is TRUE if the package has 
c    been initialized.
c
      implicit none

      integer i
      logical initialized
      logical initialized_get

      i = -1
      call initialized_memory ( i, initialized )

      initialized_get = initialized

      return
      end
      subroutine initialized_memory ( i, initialized )

c*********************************************************************72
c
cc INITIALIZED_MEMORY stores the INITIALIZED value for the package.
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
c    John Burkardt
c
c  Parameters:
c
c    Input, integer I, the desired action.
c    -1, get the value.
c    0, initialize the value.
c    1, set the value.
c
c    Input/output, logical INITIALIZED.  For I = -1, 
c    this is output, for I = +1, this is input, for I = 0,
c    this argument is ignored.  
c  
      implicit none

      integer i
      logical initialized
      logical initialized_save

      save initialized_save

      data initialized_save / .false. /

      if ( i .lt. 0 ) then
        initialized = initialized_save
      else if ( i .eq. 0 ) then
        initialized_save = .false.
      else if ( 0 .lt. i ) then
        initialized_save = initialized
      end if

      return
      end
      subroutine initialized_set ( )

c*********************************************************************72
c
cc INITIALIZED_SET sets the INITIALIZED value true.
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
c    John Burkardt
c
c  Parameters:
c
c    None
c
      implicit none

      integer i
      logical initialized

      i = +1
      initialized = .true.
      call initialized_memory ( i, initialized )
 
      return
      end
      subroutine lg_get ( g, lg1, lg2 )

c*********************************************************************72
c
cc LG_GET queries the LG values for a given generator.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 March 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer G, the index of the generator.
c    1 <= G <= 32.
c
c    Output, integer LG1, LG2, the LG values for generator G.
c
      implicit none

      integer g
      integer i
      integer lg1
      integer lg2

      i = -1
      call lg_memory ( i, g, lg1, lg2 )

      return
      end
      subroutine lg_memory ( i, g, lg1, lg2 )

c*********************************************************************72
c
cc LG_MEMORY stores the LG values for all generators.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 March 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer I, the desired action.
c    -1, get a value.
c    0, initialize all values.
c    1, set a value.
c
c    Input, integer G, for I = -1 or +1, the index of 
c    the generator, with 1 <= G <= 32.
c
c    Input/output, integer LG1, LG2.  For I = -1, 
c    these are output, for I = +1, these are input, for I = 0,
c    these arguments are ignored.  When used, the arguments are
c    old or new values of the LG parameter for generator G.
c
      implicit none

      integer g_max
      parameter ( g_max = 32 )

      integer g
      integer i
      integer j
      integer lg1
      integer lg1_save(g_max)
      integer lg2
      integer lg2_save(g_max)

      save lg1_save
      save lg2_save

      data lg1_save / 32 * 0 /
      data lg2_save / 32 * 0 /

      if ( g .lt. 1 .or. g_max .lt. g ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'LG_MEMORY - Fatal error!'
        write ( *, '(a)' ) '  Input generator index G is out of bounds.'
        stop
      end if

      if ( i .lt. 0 ) then
        lg1 = lg1_save(g)
        lg2 = lg2_save(g)
      else if ( i .eq. 0 ) then
        do j = 1, g_max
          lg1_save(1:g_max) = 0
          lg2_save(1:g_max) = 0
        end do
      else if ( 0 .lt. i ) then
        lg1_save(g) = lg1
        lg2_save(g) = lg2
      end if

      return
      end
      subroutine lg_set ( g, lg1, lg2 )

c*********************************************************************72
c
cc LG_SET sets the LG values for a given generator.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 March 2013
c
c  Author:
c
c    John Burkardt
c
c  Parameters:
c
c    Input, integer G, the index of the generator.
c    1 <= G <= 32.
c
c    Input, integer LG1, LG2, the LG values for generator G.
c
      implicit none

      integer g
      integer i
      integer lg1
      integer lg2

      i = +1
      call lg_memory ( i, g, lg1, lg2 )

      return
      end
      function multmod ( a, s, m )

c*********************************************************************72
c
cc MULTMOD carries out modular multiplication.
c
c  Discussion:
c
c    This procedure returns 
c
c      ( A * S ) mod M
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 March 2013
c
c  Author:
c
c    Original Pascal version by Pierre L'Ecuyer, Serge Cote.
c    FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Pierre LEcuyer, Serge Cote,
c    Implementing a Random Number Package with Splitting Facilities,
c    ACM Transactions on Mathematical Software,
c    Volume 17, Number 1, March 1991, pages 98-111.
c
c  Parameters:
c
c    Input, integer A, S, M, the arguments.
c
c    Output, integer MULTMOD, the value of the product of A and S, 
c    modulo M.
c
      implicit none

      integer a
      integer a0
      integer a1
      integer h
      parameter ( h = 32768 )
      integer k
      integer m
      integer multmod
      integer p
      integer q
      integer qh
      integer rh
      integer s

      if ( a .le. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'MULTMOD - Fatal error!'
        write ( *, '(a)' ) '  A <= 0.'
        stop
      end if

      if ( m .le. a ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'MULTMOD - Fatal error!'
        write ( *, '(a)' ) '  M <= A.'
        stop
      end if

      if ( s .le. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'MULTMOD - Fatal error!'
        write ( *, '(a)' ) '  S <= 0.'
        stop
      end if

      if ( m .le. s ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'MULTMOD - Fatal error!'
        write ( *, '(a)' ) '  M <= S.'
        stop
      end if

      if ( a .lt. h ) then

        a0 = a
        p = 0

      else

        a1 = a / h
        a0 = a - h * a1
        qh = m / h
        rh = m - h * qh

        if ( h .le. a1 ) then
   
          a1 = a1 - h
          k = s / qh
          p = h * ( s - k * qh ) - k * rh

10        continue

          if ( p .lt. 0 ) then
            p = p + m
            go to 10
          end if

        else

          p = 0

        end if

        if ( a1 .ne. 0 ) then

          q = m / a1
          k = s / q
          p = p - k * ( m - a1 * q )
          if ( 0 .lt. p ) then
            p = p - m
          end if
          p = p + a1 * ( s - k * q )

20        continue

          if ( p .lt. 0 ) then
            p = p + m
            go to 20
          end if

        end if

        k = p / qh
        p = h * ( p - k * qh ) - k * rh

30      continue

        if ( p .lt. 0 ) then
          p = p + m
          go to 30
        end if

      end if

      if ( a0 .ne. 0 ) then

        q = m / a0
        k = s / q
        p = p - k * ( m - a0 * q )

        if ( 0 .lt. p ) then
          p = p - m
        end if

        p = p + a0 * ( s - k * q )

40      continue

        if ( p .lt. 0 ) then
          p = p + m
          go to 40
        end if

      end if

      multmod = p

      return
      end
      function r4_uni_01 ( )

c*********************************************************************72
c
cc R4_UNI_01 returns a real uniform random number.
c
c  Discussion:
c
c    This procedure returns a random floating point number from a uniform 
c    distribution over (0,1), not including the endpoint values, using the
c    current generator.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 August 2013
c
c  Author:
c
c    Original Pascal version by Pierre L'Ecuyer, Serge Cote.
c    FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Pierre LEcuyer, Serge Cote,
c    Implementing a Random Number Package with Splitting Facilities,
c    ACM Transactions on Mathematical Software,
c    Volume 17, Number 1, March 1991, pages 98-111.
c
c  Parameters:
c
c    Output, real R4_UNI_01, a uniform random value in [0,1].
c
      implicit none

      integer i
      integer i4_uni
      logical initialized_get
      real r4_uni_01
c
c  Check whether the package must be initialized.
c
      if ( .not. initialized_get ( ) ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R4_UNI_01 - Note:'
        write ( *, '(a)' ) '  Initializing RNGLIB package.'
        call initialize ( )
      end if
c
c  Get a random positive integer.
c
      i = i4_uni ( )
c
c  Scale it to get a random real number in [0,1].
c
      r4_uni_01 = real ( i ) * 4.656613057E-10

      return
      end
      function r8_uni_01 ( )

c*********************************************************************72
c
cc R8_UNI_01 returns a double precision uniform random number.
c
c  Discussion:
c
c    This procedure returns a random floating point number from a uniform 
c    distribution over (0,1), not including the endpoint values, using the
c    current generator.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    05 August 2013
c
c  Author:
c
c    Original Pascal version by Pierre L'Ecuyer, Serge Cote.
c    FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Pierre LEcuyer, Serge Cote,
c    Implementing a Random Number Package with Splitting Facilities,
c    ACM Transactions on Mathematical Software,
c    Volume 17, Number 1, March 1991, pages 98-111.
c
c  Parameters:
c
c    Output, double precision R8_UNI_01, a uniform random value in [0,1].
c
      implicit none

      integer i
      integer i4_uni
      logical initialized_get
      double precision r8_uni_01
c
c  Check whether the package must be initialized.
c
      if ( .not. initialized_get ( ) ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'R8_UNI_01 - Note:'
        write ( *, '(a)' ) '  Initializing RNGLIB package.'
        call initialize ( )
      end if
c
c  Get a random positive integer.
c
      i = i4_uni ( )
c
c  Scale it to get a random real number in [0,1].
c
      r8_uni_01 = dble ( i ) * 4.656613057D-10

      return
      end
      subroutine set_initial_seed ( ig1, ig2 )

c*********************************************************************72
c
cc SET_INITIAL_SEED resets the initial seed and state for all generators.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    26 March 2013
c
c  Author:
c
c    Original Pascal version by Pierre L'Ecuyer, Serge Cote.
c    FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Pierre LEcuyer, Serge Cote,
c    Implementing a Random Number Package with Splitting Facilities,
c    ACM Transactions on Mathematical Software,
c    Volume 17, Number 1, March 1991, pages 98-111.
c
c  Parameters:
c
c    Input, integer IG1, IG2, the initial seed values 
c    for the first generator.
c    1 <= IG1 < 2147483563
c    1 <= IG2 < 2147483399
c
      implicit none

      integer a1_vw
      parameter ( a1_vw = 2082007225 )
      integer a2_vw
      parameter ( a2_vw = 784306273 )
      integer g
      integer g_max
      parameter ( g_max = 32 )
      integer i
      integer ig1
      integer ig2
      logical initialized_get
      integer m1
      parameter ( m1 = 2147483563 )
      integer m2
      parameter ( m2 = 2147483399 )
      integer multmod
      integer t

      if ( ig1 .lt. 1 .or. m1 .le. ig1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'SET_INITIAL_SEED - Fatal error!'
        write ( *, '(a)' ) '  Input parameter IG1 out of bounds.'
        stop
      end if

      if ( ig2 .lt. 1 .or. m2 .le. ig2 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'SET_INITIAL_SEED - Fatal error!'
        write ( *, '(a)' ) '  Input parameter IG2 out of bounds.'
        stop
      end if
c
c  Because INITIALIZE calls SET_INITIAL_SEED, it's not easy to correct
c  the error that arises if SET_INITIAL_SEED is called before INITIALIZE.
c  So don't bother trying.
c
      if ( .not. initialized_get ( ) ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'SET_INITIAL_SEED - Fatal error!'
        write ( *, '(a)' ) '  RNGLIB was not initialized.'
        stop
      end if
c
c  Set the initial seed, then initialize the first generator.
c
      g = 1
      call cgn_set ( g )

      call ig_set ( g, ig1, ig2 )

      t = 0
      call init_generator ( t )
c
c  Now do similar operations for the other generators.
c
      do g = 2, g_max

        call cgn_set ( g )
        ig1 = multmod ( a1_vw, ig1, m1 )
        ig2 = multmod ( a2_vw, ig2, m2 )
        call ig_set ( g, ig1, ig2 )
        call init_generator ( t )

      end do
c
c  Now choose the first generator.
c
      g = 1
      call cgn_set ( g )

      return
      end
      subroutine set_seed ( cg1, cg2 )

c*********************************************************************72
c
cc SET_SEED resets the initial seed and state of the current generator.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license.
c
c  Modified:
c
c    30 March 2013
c
c  Author:
c
c    Original Pascal version by Pierre L'Ecuyer, Serge Cote.
c    FORTRAN77 version by John Burkardt.
c
c  Reference:
c
c    Pierre LEcuyer, Serge Cote,
c    Implementing a Random Number Package with Splitting Facilities,
c    ACM Transactions on Mathematical Software,
c    Volume 17, Number 1, March 1991, pages 98-111.
c
c  Parameters:
c
c    Input, integer CG1, CG2, the CG values for generator G.
c    1 <= CG1 < 2147483563
c    1 <= CG2 < 2147483399
c
      implicit none

      integer cg1
      integer cg2
      integer cgn_get
      integer g
      integer i
      logical initialized_get
      integer m1
      parameter ( m1 = 2147483563 )
      integer m2
      parameter ( m2 = 2147483399 )
      integer t

      if ( cg1 .lt. 1 .or. m1 .le. cg1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'SET_SEED - Fatal error!'
        write ( *, '(a)' ) '  Input parameter CG1 out of bounds.'
        stop
      end if

      if ( cg2 .lt. 1 .or. m2 .le. cg2 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'SET_SEED - Fatal error!'
        write ( *, '(a)' ) '  Input parameter CG2 out of bounds.'
        stop
      end if
c
c  Check whether the package must be initialized.
c
      if ( .not. initialized_get ( ) ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'SET_SEED - Note:'
        write ( *, '(a)' ) '  Initializing RNGLIB package.'
        call initialize ( )
      end if
c
c  Retrieve the current generator index.
c
      g = cgn_get ( )
c
c  Set the seeds.
c
      call cg_set ( g, cg1, cg2 )
c
c  Initialize the generator.
c
      t = 0
      call init_generator ( t )

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
