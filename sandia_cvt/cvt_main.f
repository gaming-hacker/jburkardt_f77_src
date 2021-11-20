      program main

c*********************************************************************72
c
cc MAIN is the main program for CVT_MAIN.
c
c  Discussion:
c
c    CVT_MAIN runs a test of CVT, the centroidal Voronoi tessellation library.
c
c    Typical parameters for this code are:
c
c      n = 1024
c      nbin = (/ 25, 25, 5 /)
c      ns_cvt = 5000
c      ns_mom = 5000
c
c    A "big" version of this code was run with:
c
c      n = 5000
c      nbin = (/ 25, 25, 5 /)
c      ns_cvt = 100
c      ns_mom = 100
c
c    A "huge" version of this code was run with:
c
c      n = 50000
c      nbin = (/ 50, 50, 10 /)
c      ns_cvt = 100
c      ns_mom = 100
c
c    A "monster" version of this code was run with:
c
c      n = 500000
c      nbin = (/ 100, 100, 20 /)
c      ns_cvt = 100
c      ns_mom = 100
c
c  Variables:
c
c    Note that floating point variables are declared as
c    of type "double precision".  This is meant to be equivalent to
c    "double precision".  If your compiler does not recognize
c    the first kind of declaration, the second should work.
c
c    Geometry Variables:
c    ------------------
c
c    Input, integer NDIM, the spatial dimension, which should be 2 or 3.
c
c    double precision BOX_MIN(NDIM), BOX_MAX(NDIM), the minimum and maximum 
c    coordinates of the bounding box.  
c
c    logical USE_DIATOM, is TRUE if DIATOM is to be called to
c    determine whether a point lies in the physical region; if it is
c    FALSE than a much simplified routine is used.
c
c    double precision DR, a tolerance used by DIATOM when testing whether
c    a point is within, outside of, or on the boundary of the physical region.
c    This is typically a very small value, of the order of 0.00001 or smaller.
c
c
c    CVT Algorithm Variables:
c    -----------------------
c
c    integer N, the number of Voronoi cells to generate.
c    A typical value is 256.
c
c    integer MAXIT, the maximum number of correction iterations used in the
c    Voronoi calculation.  A typical value is 10.
c
c    integer NS_CVT, the average number of sampling points tested per 
c    Voronoi cell, on each step of the correction iteration.  A typical 
c    value is 5000.
c
c    Input, integer RANDOM_GENERATOR, specifies how the Voronoi cell 
c    generators are to be initialized.
c    0, use the F90 RANDOM_NUMBER routine;
c    1, use the Halton sequence. (PREFERRED)
c
c    double precision CELL_GENERATOR(NDIM,N), the Voronoi cell generators 
c    of the Voronoi tessellation, as approximated by the CVT algorithm.  This
c    is the output quantity of most interest.
c
c
c    Moment Calculation Variables:
c    ----------------------------
c
c    integer NS_MOM, the average number of sampling points tested per 
c    Voronoi cell, for the moment calculation.  A typical value is 5000.
c
c    logical REGION_VOLUME_GIVEN, 
c    0, the area or volume is already available in REGION.
c    nonzero, the area or volume needs to be calculated.
c
c    double precision REGION_VOLUME.
c    If REGION_VOLUME_GIVEN, then REGION_VOLUME must be input by the user.
c    Otherwise, REGION_VOLUME is approximated computationally.
c
c    double precision CELL_VOLUME(N), the volume of the Voronoi cells.
c
c    double precision CELL_CENTROID(NDIM,N), the centroids of the 
c    Voronoi cells.
c
c    double precision CELL_MOMENT(NDIM,NDIM,N), the second moments of the
c    Voronoi cells.
c
c
c    The Nearest Neighbor Search Variables:
c    -------------------------------------
c
c    logical USE_BINS, is TRUE if the bounding box is to be divided up
c    into bins to speed up the nearest neighbor search;
c    FALSE if the nearest neighbor seach is to be done naively.
c
c    integer NBIN(3) is the number of bins to use in each direction.
c    For 2D problems, set NBIN(3) = 1.
c    For efficiency, these values should be set in such a way that the bins 
c    are nearly square or cubical.
c
c    integer BIN_START(NBIN(1),NBIN(2),NBIN(3)), the index of the first
c    cell center in the bin, or -1 if none.  
c
c    integer BIN_LAST(NBIN(1),NBIN(2),NBIN(3)), the index of the last 
c    cell center in the bin, or -1 if none.
c
c    integer BIN_NEXT(N), the index of the next cell center in the bin
c    containing this cell center.
c
c  Miscellaneous Variables:
c  -----------------------
c
c    integer SEED, determines how to initialize the RANDOM_NUMBER routine.
c    If SEED is zero on input, then RANDOM_INITIALIZE will make up a seed
c    from the current real time clock reading.
c    If SEED is nonzero, then a reproducible sequence of random numbers
c    defined by SEED will be chosen.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    23 July 2008
c   
c  Author:
c
c    John Burkardt, Max Gunzburger, Janet Peterson
c
c  Reference:
c
c    John Burkardt, Max Gunzburger, Janet Peterson and Rebecca Brannon,
c    User Manual and Supporting Information for Library of Codes
c    for Centroidal Voronoi Placement and Associated Zeroth,
c    First, and Second Moment Determination,
c    Sandia National Laboratories Technical Report SAND2002-0099,
c    February 2002.
c
      implicit none

      integer n
      parameter ( n = 1024 )
      integer ndim
      parameter ( ndim = 3 )
      integer nbin1
      parameter ( nbin1 = 25 )
      integer nbin2
      parameter ( nbin2 = 25 )
      integer nbin3
      parameter ( nbin3 = 5 )

      integer bin_last( nbin1,nbin2,nbin3 )
      integer bin_next(n)
      integer bin_start( nbin1,nbin2,nbin3 )
      double precision box_min(ndim)
      double precision box_max(ndim)
      double precision cell_centroid(ndim,n)
      double precision cell_generator(ndim,n)
      double precision cell_moment(ndim,ndim,n)
      double precision cell_volume(n)
      integer clock0
      integer clock1
      integer clock_max
      integer clock_rate
      double precision dr
      parameter ( dr = 0.00001D+00 )
      integer i
      integer ios
      integer it
      integer j
      integer k
      integer maxit
      parameter ( maxit = 10 )
      integer nbin(ndim)
      integer ns_cvt
      parameter ( ns_cvt = 5000 )
      integer ns_mom
      parameter ( ns_mom = 5000 )
      logical quality_checks
      parameter ( quality_checks = .true. )
      integer random_generator
      parameter ( random_generator = 1 )
      double precision region_volume
      parameter ( region_volume = 20.0D+00 * 1700.0D+00 )
      logical region_volume_given
      parameter ( region_volume_given = .true. )
      integer seed
      real tarray(2)
      real time0
      real time1
      integer updates(n)
      logical use_bins
      parameter ( use_bins = .true. )
      logical use_diatom
      parameter ( use_diatom = .false. )
      logical write_output
      parameter ( write_output = .false. )

      box_min(1) = 0.0
      box_min(2) = 0.0
      box_min(3) = 0.0

      box_max(1) = 100.0
      box_max(2) = 100.0
      box_max(3) =  20.0

      nbin(1) = nbin1
      nbin(2) = nbin2
      nbin(3) = nbin3

      seed = 0
c
c  Print introduction and options.
c
      call timestamp ( )

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CVT_MAIN'
      write ( *, '(a)' ) '  FORTRAN77 version'
      write ( *, '(a)' ) '  A sample problem for the probabilistic'
      write ( *, '(a)' ) '  Centroidal Voronoi Tessellation algorithm.'
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 
     &  '  Given a region in 2D or 3D, the problem is to determine'
      write ( *, '(a)' ) 
     &  '  GENERATORS, a set of points which define a division'
      write ( *, '(a)' ) 
     &  '  of the region into Voronoi cells, which are also'
      write ( *, '(a)' ) '  CENTROIDS of the Voronoi cells.'

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'Geometry parameters:'
      write ( *, '(a)' ) '-------------------'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) '  The spatial dimension is NDIM = ', ndim
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  The minimum corner of the bounding box is:'
      write ( *, '(5g14.6)' ) ( box_min(i), i = 1, ndim )
      write ( *, '(a)' ) '  The maximum corner of the bounding box is:'
      write ( *, '(5g14.6)' ) ( box_max(i), i = 1, ndim )
      if ( use_diatom ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  DIATOM is called to determine the region.'
        write ( *, '(a,g14.6)' ) '  The DIATOM tolerance DR = ', dr
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  DIATOM is not called;'
        write ( *, '(a)' ) '  a simple routine determines the region.'
      end if

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CVT Algorithm parameters:'
      write ( *, '(a)' ) '-------------------------'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) 
     &  '  The number of Voronoi cells to generate: ', n
      write ( *, '(a,i8)' ) 
     &  '  Number of iterations to determine CVT: ', maxit
      write ( *, '(a,i8)' ) 
     &  '  Number of sampling points per Voronoi cell: ', ns_cvt

      if ( random_generator .eq. 0 ) then
        write ( *, '(a)' ) 
     &    '  Voronoi cell generators are initialized by RANDOM_NUMBER.'
      else if ( random_generator == 1 ) then
        write ( *, '(a)' ) 
     &  '  Voronoi cell generators are initialized by Halton.'
      end if

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'Moment parameters:'
      write ( *, '(a)' ) '------------------'
      write ( *, '(a)' ) ' '
      write ( *, '(a,i8)' ) 
     &  '  Number of sampling points per Voronoi cell: ', ns_mom
      if ( region_volume_given ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  The volume of the region is given.'
        write ( *, '(a,g14.6)' ) 
     &    '  It is specified as REGION_VOLUME = ', region_volume
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  The volume of the region is not given.'
        write ( *, '(a)' ) '  It will be estimated.'
      end if

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'Nearest Neighbor Search parameters:'
      write ( *, '(a)' ) '-----------------------------------'
      write ( *, '(a)' ) ' '
      if ( use_bins ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 
     &    '  The nearest neighbor search is speeded up by using bins.'
        write ( *, '(a)' ) 
     &    '  The bounding box is to be divided up into bins.'
        write ( *, '(a)' ) '  The number of bins is :'
        write ( *, '(5i8)' ) nbin(1:ndim)
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 
     &  '  The nearest neighbor search is not speeded up.'
        write ( *, '(a)' ) 
     &    '  The nearest neighbor search is done by exhaustion.'
      end if

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'Miscellaneous parameters:'
      write ( *, '(a)' ) '------------------------'
      write ( *, '(a)' ) ' '
      if ( write_output ) then
        write ( *, '(a)' ) 
     &  '  Generator and moment output files WILL be written.'
      else
        write ( *, '(a)' ) 
     &    '  Generator and moment output files will NOT be written.'
      end if

      write ( *, '(a)' ) ' '
c
c  If DIATOM is to be used, the DIATOM setup routine must be called now.
c
      if ( use_diatom ) then
        call diatom_setup ( )
      end if
c
c  Begin timing.
c
c  The F90 CPU_TIME routine, at least on the DEC_ALPHA, can "wrap around"
c  after a relatively short time, giving negative timings.  So we also
c  call ETIME, which should be measuring the same thing, for checking.
c
c  The ETIME routine is called with an appended underscore, because
c  A) We're compiling with NOUNDERSCORE in order to interface with C;
c  B) but the UFOR library containing ETIME expects a reference to ETIME_.
c
c  The F90 SYSTEM_CLOCK routine should measure real execution time.
c
      call system_clock ( clock0, clock_rate, clock_max )
      call cpu_time ( time0 )
c
c  Initialize the Voronoi cell generators.
c
      call generator_init ( ndim, box_min, box_max, n, cell_generator, 
     &  use_diatom, dr, random_generator )
c
c  Carry out the CVT iteration, which drives the Voronoi generators 
c  and Voronoi centroids closer and closer.
c
c  If bins are used, they must be calculated before the first call,
c  and recalculated each time the cell centers are changed.
c
      do i = 1, n
        updates(i) = 1
      end do

      do it = 1, maxit + 1

        if ( use_bins ) then

          call bin_preprocess ( ndim, box_min, box_max, n, 
     &      cell_generator, nbin, bin_start, bin_last, bin_next )

        end if

        if ( it .le. maxit ) then

          call cvt_iteration ( ndim, box_min, box_max, n, 
     &      cell_generator, ns_cvt, use_diatom, use_bins, dr, updates, 
     &      nbin, bin_start, bin_last, bin_next )

        end if

      end do
c
c  Calculate cell moments.
c
      call vcm ( ndim, box_min, box_max, n, cell_generator, ns_mom, 
     &  use_diatom,  use_bins, region_volume_given, region_volume, 
     &  dr, nbin, bin_start, bin_last, bin_next, cell_volume, 
     &  cell_centroid, cell_moment )
c
c  End timing the "interesting" part.
c
      call cpu_time ( time1 )
      call system_clock ( clock1, clock_rate, clock_max )

      write ( *, '(a)' ) ' '
      write ( *, '(a,g14.6,a)' ) 'Elapsed CPU time, CPU_TIME: ', 
     &  time1-time0, ' seconds.'
      write ( *, '(a,g14.6,a)' ) 'Elapsed time, SYSTEM_CLOCK: ', 
     &  real ( clock1 - clock0 ) / real ( clock_rate ), ' seconds.'
c
c  Compute quality checks.
c
      if ( quality_checks ) then
        call quality ( ndim, n, cell_moment, cell_volume,
     &    region_volume )
      end if
c
c  Write generators and moments to files.
c
      if ( write_output ) then

        open ( unit = 1, file = 'cvt_generators.dat', 
     &    status = 'replace', iostat = ios )
        do j = 1, n
          write ( 1, '(3d15.6)' ) ( cell_generator(i,j), i = 1, ndim )
        end do
        close ( unit = 1 )

        open ( unit = 1, file = 'cvt_volume.dat', status = 'replace', 
     &    iostat = ios )
        write ( 1, '(d15.6)' ) cell_volume(1:n)
        close ( unit = 1 )

        open ( unit = 1, file = 'cvt_centroid.dat', 
     &    status = 'replace', iostat = ios )
        do j = 1, n
          write ( 1,'(3d15.6)' ) ( cell_centroid(i,j), i = 1, ndim )
        end do
        close ( unit = 1 )

        open ( unit = 1, file = 'cvt_moment.dat', status = 'replace', 
     &    iostat = ios )
        do k = 1, n
          do j = 1, ndim
            write ( 1, '(3d15.6)' ) ( cell_moment(i,j,k), i = 1, ndim )
          end do
        end do
        close ( unit = 1 )

      end if

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'CVT_MAIN'
      write ( *, '(a)' ) '  Normal end of execution.'

      write ( *, '(a)' ) ' '
      call timestamp ( )

      stop
      end
      subroutine diatom_setup ( )

c*********************************************************************72
c
cc DIATOM_SETUP is a dummy version of the DIATOM_SETUP routine.
c
c  Discussion:
c
c    If the DIATOM library is not available, or not needed, then this 
c    routine may be used to satisfy the compiler.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    23 July 2008
c
c  Author:
c
c    John Burkardt, Max Gunzburger, Janet Peterson
c
      implicit none

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'DIATOM_SETUP:'
      write ( *, '(a)' ) '  Dummy version of DIATOM setup.'

      return
      end
      subroutine diatom_point_test2 ( x, y, z, dr, mdens, ival )

c*********************************************************************72
c
cc DIATOM_POINT_TEST2 is a dummy version of the DIATOM_POINT_TEST2 routine.
c
c  Discussion:
c
c    If the DIATOM library is not available, then this routine may be used
c    to satisfy the compiler.
c
c  Modified:
c
c    23 July 2008
c
c  Author:
c
c    John Burkardt, Max Gunzburger, Janet Peterson
c
      implicit none

      double precision dr
      integer ival
      double precision mdens
      double precision x
      double precision y
      double precision z

      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'DIATOM_POINT_TEST2 - Fatal error!'
      write ( *, '(a)' ) 
     &  '  The dummy version of DIATOM_POINT_TEST2 was called.'

      stop
      end
      subroutine test_region ( x, ndim, ival )

c*********************************************************************72
c
cc TEST_REGION determines if a point is within the physical region.
c
c  Discussion:
c
c    The previous version of this routine had a flaw, because it
c    described the region as a rectangle sitting on top of half
c    an annulus.  Of course, such a rectangle only touches the
c    annulus at one point, and on either side of this contact there
c    is a small area, which should have been included in the overall
c    region, but was omitted.  This problem has been corrected.
c
c    Also, apparently, the region should extend no higher than
c    Y = 90, and no lower than Y = 0.  These constraints have
c    also been included now.
c
c    This routine should be functionally the same as the interface to
c    DIATOM, but it is much simpler, and a lot faster.  To signal that
c    the problem geometry will be determined by this routine, set
c    USE_DIATOM to FALSE in the calling program.
c
c    Using a simple routine like this is only appropriate for a simple 
c    region that can be easily defined by user formulas.  This version of 
c    the routine is a demonstration that implements the 2D and 3D versions 
c    of the "tuning fork" test region.
c
c    Computation of the "on-the-boundary" case is not considered important.
c    Only "inside" or "outside" is essential.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    23 July 2008
c
c  Author:
c
c    John Burkardt, Max Gunzburger, Janet Peterson
c
c  Parameters:
c
c    Input, double precision X(NDIM), the point to be checked.
c
c    Input, integer NDIM, the dimension of the space.
c
c    Output, integer IVAL, indicates the status of the point:
c    -1: the point is on the boundary of the region.
c     0: the point is outside the region.
c    +1: the point is inside the region.
c
      implicit none

      integer ndim

      double precision c
      integer ival
      double precision x(ndim)
c
c  The only adjustment needed for 3D is that we require 0 <= Z <= 20.
c
      if ( ndim .eq. 3 ) then
        if ( x(3) .lt. 0.0D+00 .or. 20.0D+00 .lt. x(3) ) then
          ival = 0
          return
        end if
      end if
c
c  Is the point inside the rectangular region?
c
      if ( 30.0D+00 .le. x(2)               .and. 
     &                   x(2) .le. 90.0D+00 .and. 
     &     45.0D+00 .le. x(1)               .and. 
     &                   x(1) .le. 55.0D+00 ) then

          ival = 1
c
c  Is the point in the annulus?
c
      else if ( 0.0D+00 .le. x(2) .and. x(2) .le. 40.0D+00 ) then

        c = sqrt ( ( x(1) - 50.0D+00 ) 
     &    * ( x(1) - 50.0D+00 ) + x(2) * x(2) )

        if ( 30.0D+00 .le. c .and. c .le. 40.0D+00 ) then
          ival = 1
        else
          ival = 0
        end if

      else

        ival = 0

      end if

      return
      end
