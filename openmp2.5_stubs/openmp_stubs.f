      subroutine omp_destroy_lock ( lock )

c*********************************************************************72
c
cc OMP_DESTROY_LOCK destroys a simple lock.
c
c  Discussion:
c
c    The routine is intended to return the state of the lock to the 
c    uninitialized state.
c
c    The OpenMP standard binding for FORTRAN requires that LOCK be
c    a variable of type "OMP_LOCK_KIND" which is taken to be a 
c    pointer to an integer, which may be thought of as simply the
c    address of an integer variable.  Pointers are not available in
c    the FORTRAN77 standard, so we treat LOCK as an integer itself.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 November 2007
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    OpenMP Application Program Interface,
c    Version 2.5,
c    May 2005.
c
c  Parameters:
c
c    Output, integer LOCK, the simple lock.
c
      implicit none

      integer lock

      lock = 0

      return
      end
      subroutine omp_destroy_nest_lock ( nlock )

c*********************************************************************72
c
cc OMP_DESTROY_NEST_LOCK destroys a nestable lock.
c
c  Discussion:
c
c    The routine is intended to return the state of the lock to the 
c    uninitialized state.
c
c    The OpenMP standard binding for FORTRAN requires that NLOCK be
c    a variable of type "OMP_LOCK_KIND" which is taken to be a 
c    pointer to an integer, which may be thought of as simply the
c    address of an integer variable.  Pointers are not available in
c    the FORTRAN77 standard, so we treat NLOCK as an integer itself.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 November 2007
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    OpenMP Application Program Interface,
c    Version 2.5,
c    May 2005.
c
c  Parameters:
c
c    Output, integer NLOCK, the nestable lock.
c
      implicit none

      integer nlock

      nlock = 0

      return
      end
      function omp_get_dynamic ( )

c*********************************************************************72
c
cc OMP_GET_DYNAMIC reports if dynamic adjustment of thread number is allowed.
c
c  Discussion:
c
c    The user can request dynamic thread adjustment by calling OMP_SET_DYNAMIC.
c
c    For this stub library, the value FALSE is always returned.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 November 2007
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    OpenMP Application Program Interface,
c    Version 2.5,
c    May 2005.
c
c  Parameters:
c
c    Output, logical OMP_GET_DYNAMIC, is TRUE if dynamic adjustment of thread
c    number has been enable, by default or by a user call.
c
      implicit none

      logical omp_get_dynamic

      omp_get_dynamic = .false.

      return
      end
      function omp_get_max_threads ( )

c*********************************************************************72
c
cc OMP_GET_MAX_THREADS returns the default number of threads.
c
c  Discussion:
c
c    If a parallel region is reached, and no number of threads has been
c    specified explicitly, there is a default number of threads that will
c    be used to form the new team.  That value is returned by this function.
c
c    For this stub library, the value 1 is always returned.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 November 2007
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    OpenMP Application Program Interface,
c    Version 2.5,
c    May 2005.
c
c  Parameters:
c
c    Output, integer OMP_GET_MAX_THREADS, the default number of threads.
c
      implicit none

      integer omp_get_max_threads

      omp_get_max_threads = 1

      return
      end
      function omp_get_nested ( )

c*********************************************************************72
c
cc OMP_GET_NESTED reports if nested parallelism has been enabled.
c
c  Discussion:
c
c    The user can request nested parallelism by calling OMP_SET_NESTED.
c
c    For this stub library, the value FALSE is always returned.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 November 2007
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    OpenMP Application Program Interface,
c    Version 2.5,
c    May 2005.
c
c  Parameters:
c
c    Output, logical OMP_GET_NESTED, is TRUE if nested parallelism has been
c    enable by default or by a user call.
c
      implicit none

      logical omp_get_nested

      omp_get_nested = .false.

      return
      end
      function omp_get_num_procs ( )

c*********************************************************************72
c
cc OMP_GET_NUM_PROCS returns the number of processors available to the program.
c
c  Discussion:
c
c    For this stub library, the value 1 is always returned.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 November 2007
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    OpenMP Application Program Interface,
c    Version 2.5,
c    May 2005.
c
c  Parameters:
c
c    Output, integer GET_NUM_PROCS, the number of processors available.
c
      implicit none

      integer omp_get_num_procs

      omp_get_num_procs = 1

      return
      end
      function omp_get_num_threads ( )

c*********************************************************************72
c
cc OMP_GET_NUM_THREADS returns the number of threads in the current team.
c
c  Discussion:
c
c    For this stub library, the value 1 is always returned.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 November 2007
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    OpenMP Application Program Interface,
c    Version 2.5,
c    May 2005.
c
c  Parameters:
c
c    Output, integer OMP_GET_NUM_THREADS, the number of threads in the 
c    current team.
c
      implicit none

      integer omp_get_num_threads

      omp_get_num_threads = 1

      return
      end
      function omp_get_thread_num ( )

c*********************************************************************72
c
cc OMP_GET_THREAD_NUM returns the thread number of a thread in a team.
c
c  Discussion:
c
c    Thread numbers start at 0.
c
c    If this function is not called from a parallel region, then only one
c    thread is executing, so the value returned is 0.
c
c    For this stub library, the value 0 is always returned.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 November 2007
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    OpenMP Application Program Interface,
c    Version 2.5,
c    May 2005.
c
c  Parameters:
c
c    Output, integer OMP_GET_THREAD_NUM, the thread number.
c
      implicit none

      integer omp_get_thread_num

      omp_get_thread_num = 0

      return
      end
      function omp_get_wtick ( )

c*********************************************************************72
c
cc OMP_GET_WTICK returns the precision of the timer used by OMP_GET_WTIME.
c
c  Discussion:
c
c    The call here to SYSTEM_CLOCK is not portable.
c
c    SYSTEM_CLOCK is defined as a FORTRAN90 intrinsic.  GFORTRAN makes
c    this intrinsic available to FORTRAN77 programs as well.  
c
c    If another FORTRAN77 compiler is used, then this call to 
c    SYSTEM_CLOCK would probably need to be replaced.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 November 2007
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    OpenMP Application Program Interface,
c    Version 2.5,
c    May 2005.
c
c  Parameters:
c
c    Output, double precision OMP_GET_WTICK, the number of seconds between
c    successive "ticks" of the wall clock timer.
c
      implicit none

      integer count
      integer count_max
      integer count_rate
      double precision omp_get_wtick

      call system_clock ( count, count_rate, count_max )

      omp_get_wtick = 1.0D+00 / dble ( count_rate )

      return
      end
      function omp_get_wtime ( )

c*********************************************************************72
c
cc OMP_GET_WTIME returns elapsed wall clock time in seconds.
c
c  Discussion:
c
c    The call here to SYSTEM_CLOCK is not portable.
c
c    SYSTEM_CLOCK is defined as a FORTRAN90 intrinsic.  GFORTRAN makes
c    this intrinsic available to FORTRAN77 programs as well.  
c
c    If another FORTRAN77 compiler is used, then this call to 
c    SYSTEM_CLOCK would probably need to be replaced.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 November 2007
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    OpenMP Application Program Interface,
c    Version 2.5,
c    May 2005.
c
c  Parameters:
c
c    Output, double precision OMP_GET_WTIME, the current reading of the
c    wall clock timer.
c
      implicit none

      integer count
      integer count_max
      integer count_rate
      double precision omp_get_wtime

      call system_clock ( count, count_rate, count_max )

      omp_get_wtime = dble ( count ) / dble ( count_rate )

      return
      end
      function omp_in_parallel ( )

c*********************************************************************72
c
cc OMP_IN_PARALLEL returns TRUE if the call is made from a parallel region.
c
c  Discussion:
c
c    For this stub library, the value FALSE is always returned.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 November 2007
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    OpenMP Application Program Interface,
c    Version 2.5,
c    May 2005.
c
c  Parameters:
c
c    Output, logical OMP_IN_PARALLEL, is TRUE if the call to this
c    routine was made from a parallel region.
c
      implicit none

      logical omp_in_parallel

      omp_in_parallel = .false.

      return
      end
      subroutine omp_init_lock ( lock )

c*********************************************************************72
c
cc OMP_INIT_LOCK initializes a simple lock.
c
c  Discussion:
c
c    This routine is intended to initialize the lock to the unlocked state.
c
c    For this stub library, the lock points to -1.
c
c    The OpenMP standard binding for FORTRAN requires that LOCK be
c    a variable of type "OMP_LOCK_KIND" which is taken to be a 
c    pointer to an integer, which may be thought of as simply the
c    address of an integer variable.  Pointers are not available in
c    the FORTRAN77 standard, so we treat LOCK as an integer itself.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 November 2007
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    OpenMP Application Program Interface,
c    Version 2.5,
c    May 2005.
c
c  Parameters:
c
c    Output, integer LOCK, the lock.
c
      implicit none

      integer lock

      lock = -1

      return
      end
      subroutine omp_init_nest_lock ( nlock )

c*********************************************************************72
c
cc OMP_INIT_NEST_LOCK initializes a nestable lock.
c
c  Discussion:
c
c    This routine is intended to initialize the lock to the unlocked state.
c
c    For this stub library, the lock points to -1.
c
c    The OpenMP standard binding for FORTRAN requires that NLOCK be
c    a variable of type "OMP_LOCK_KIND" which is taken to be a 
c    pointer to an integer, which may be thought of as simply the
c    address of an integer variable.  Pointers are not available in
c    the FORTRAN77 standard, so we treat NLOCK as an integer itself.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 November 2007
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    OpenMP Application Program Interface,
c    Version 2.5,
c    May 2005.
c
c  Parameters:
c
c    Output, integer NLOCK, the lock.
c
      implicit none

      integer nlock

      nlock = -1

      return
      end
      subroutine omp_set_dynamic ( dynamic_threads )

c*********************************************************************72
c
cc OMP_SET_DYNAMIC enables dynamic adjustment of the number of threads.
c
c  Discussion:
c
c    If DYNAMIC_THREADS is TRUE, then the number of threads available for
c    execution in a parallel region may be dynamically adjusted.
c
c    For this stub library, the input value is ignored.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 November 2007
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    OpenMP Application Program Interface,
c    Version 2.5,
c    May 2005.
c
c  Parameters:
c
c    Input, logical DYNAMIC_THREADS, is TRUE if the user wishes to allow
c    dynamic adjustment of the number of threads available for execution
c    in any parallel region.
c
      implicit none

      logical dynamic_threads

      return
      end
      subroutine omp_set_lock ( lock )

c*********************************************************************72
c
cc OMP_SET_LOCK sets a simple lock.
c
c  Discussion:
c
c    The lock must already have been initialized.
c
c    The OpenMP standard binding for FORTRAN requires that LOCK be
c    a variable of type "OMP_LOCK_KIND" which is taken to be a 
c    pointer to an integer, which may be thought of as simply the
c    address of an integer variable.  Pointers are not available in
c    the FORTRAN77 standard, so we treat LOCK as an integer itself.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 November 2007
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    OpenMP Application Program Interface,
c    Version 2.5,
c    May 2005.
c
c  Parameters:
c
c    Input/output, integer LOCK, the simple lock.
c
      implicit none

      integer lock

      if ( lock .eq. -1 ) then
        lock = 1
      else if ( lock .eq. 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'OMP_SET_LOCK - Fatal error!'
        write ( *, '(a)' ) '  Deadlock using lock variable.'
        stop
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'OMP_SET_LOCK - Fatal error!'
        write ( *, '(a)' ) '  Lock not initialized.'
        stop
      end if

      return
      end
      subroutine omp_set_nest_lock ( nlock )

c*********************************************************************72
c
cc OMP_SET_NEST_LOCK sets a nestable lock.
c
c  Discussion:
c
c    The lock must already have been initialized.
c
c    The OpenMP standard binding for FORTRAN requires that NLOCK be
c    a variable of type "OMP_LOCK_KIND" which is taken to be a 
c    pointer to an integer, which may be thought of as simply the
c    address of an integer variable.  Pointers are not available in
c    the FORTRAN77 standard, so we treat NLOCK as an integer itself.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 November 2007
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    OpenMP Application Program Interface,
c    Version 2.5,
c    May 2005.
c
c  Parameters:
c
c    Input/output, integer NLOCK, the nestable lock.
c
      implicit none

      integer nlock

      if ( nlock .eq. -1 ) then
        nlock = 1
      else if ( nlock .eq. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'OMP_SET_NEST_LOCK - Fatal error!'
        write ( *, '(a)' ) '  Nested lock not initialized.'
        stop
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'OMP_SET_NEST_LOCK - Fatal error!'
        write ( *, '(a)' ) '  Deadlock using nested lock variable.'
        stop
      end if

      return
      end
      subroutine omp_set_nested ( nested )

c*********************************************************************72
c
cc OMP_SET_NESTED controls the use of nested parallelism.
c
c  Discussion:
c
c    For this stub library, the input value is ignored.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 November 2007
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    OpenMP Application Program Interface,
c    Version 2.5,
c    May 2005.
c
c  Parameters:
c
c    Input, logical NESTED, is TRUE if nested parallelism is to be enabled.
c
      implicit none

      logical nested

      return
      end
      subroutine omp_set_num_threads ( num_threads )

c*********************************************************************72
c
cc OMP_SET_NUM_THREADS sets the number of threads.
c
c  Discussion:
c
c    This routine sets the number of threads to be used in all subsequent
c    parallel regions for which an explicit number of threads is not
c    specified.
c
c    For this stub library, the input value is ignored.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 November 2007
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    OpenMP Application Program Interface,
c    Version 2.5,
c    May 2005.
c
c  Parameters:
c
c    Input, integer NUM_THREADS, the number of threads to be used in all
c    subsequent parallel regions for which an explicit number of threads
c    is not specified.  0 < NUM_THREADS.
c
      implicit none

      integer num_threads

      if ( num_threads <= 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'OMP_SET_NUM_THREADS - Fatal error!'
        write ( *, '(a)' ) '  Number of threads must be at least 1.'
        stop
      end if

      return
      end
      function omp_test_lock ( lock )

c*********************************************************************72
c
cc OMP_TEST_LOCK tests a simple lock.
c
c  Discussion:
c
c    Calling this routine with an uninitialized lock causes an error.
c
c    The OpenMP standard binding for FORTRAN requires that LOCK be
c    a variable of type "OMP_LOCK_KIND" which is taken to be a 
c    pointer to an integer, which may be thought of as simply the
c    address of an integer variable.  Pointers are not available in
c    the FORTRAN77 standard, so we treat LOCK as an integer itself.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 November 2007
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    OpenMP Application Program Interface,
c    Version 2.5,
c    May 2005.
c
c  Parameters:
c
c    Input/output, integer LOCK, the simple lock.
c    If the lock was initialized but not set, it is set by this call.
c
c    Output, logical OMP_TEST_LOCK:
c    TRUE, on input, the lock was initialized and not set;
c    FALSE, on input the lock was initialized, and set.
c
      implicit none

      integer lock
      logical omp_test_lock

      if ( lock .eq. -1 ) then
        lock = 1
        omp_test_lock = .true.
      else if ( lock .eq. 1 ) then
        omp_test_lock = .false.
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'OMP_TEST_LOCK - Fatal error!'
        write ( *, '(a)' ) '  Lock not initialized.'
        stop
      end if

      return
      end
      function omp_test_nest_lock ( nlock )

c*********************************************************************72
c
cc OMP_TEST_NEST_LOCK tests a nestable lock.
c
c  Discussion:
c
c    Calling this routine with an uninitialized lock causes an error.
c
c    The OpenMP standard binding for FORTRAN requires that NLOCK be
c    a variable of type "OMP_LOCK_KIND" which is taken to be a 
c    pointer to an integer, which may be thought of as simply the
c    address of an integer variable.  Pointers are not available in
c    the FORTRAN77 standard, so we treat NLOCK as an integer itself.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 November 2007
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    OpenMP Application Program Interface,
c    Version 2.5,
c    May 2005.
c
c  Parameters:
c
c    Input/output, integer NLOCK, the nestable lock.
c    If the lock was initialized but not set, it is set by this call.
c
c    Output, integer OMP_TEST_NEST_LOCK, returns the new nesting count,
c    if the call was successful.  Otherwise, the value 0 is returned.
c
      implicit none

      integer nlock
      integer omp_test_nest_lock

      if ( nlock .eq. -1 ) then
        nlock = 1
        omp_test_nest_lock = 1
      else if ( nlock .eq. 1 ) then
        omp_test_nest_lock = 0
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'OMP_TEST_NEST_LOCK - Fatal error!'
        write ( *, '(a)' ) '  Nested lock not initialized.'
        stop
      end if

      return
      end
      subroutine omp_unset_lock ( lock )

c*********************************************************************72
c
cc OMP_UNSET_LOCK unsets a simple lock.
c
c  Discussion:
c
c    The OpenMP standard binding for FORTRAN requires that LOCK be
c    a variable of type "OMP_LOCK_KIND" which is taken to be a 
c    pointer to an integer, which may be thought of as simply the
c    address of an integer variable.  Pointers are not available in
c    the FORTRAN77 standard, so we treat LOCK as an integer itself.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 November 2007
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    OpenMP Application Program Interface,
c    Version 2.5,
c    May 2005.
c
c  Parameters:
c
c    Input/output, integer LOCK, the simple lock.
c
      implicit none

      integer lock

      if ( lock .eq. 1 ) then
        lock = -1
      else if ( lock .eq. -1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'OMP_UNSET_LOCK - Fatal error!'
        write ( *, '(a)' ) '  Attempt to unset lock, '
        write ( *, '(a)' ) '  but lock was not set first.'
        stop
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'OMP_UNSET_LOCK - Fatal error!'
        write ( *, '(a)' ) '  Lock not initialized.'
        stop
      end if

      return
      end
      subroutine omp_unset_nest_lock ( nlock )

c*********************************************************************72
c
cc OMP_UNSET_NEST_LOCK unsets a nestable lock.
c
c  Discussion:
c
c    The OpenMP standard binding for FORTRAN requires that NLOCK be
c    a variable of type "OMP_LOCK_KIND" which is taken to be a 
c    pointer to an integer, which may be thought of as simply the
c    address of an integer variable.  Pointers are not available in
c    the FORTRAN77 standard, so we treat NLOCK as an integer itself.
c
c  Licensing:
c
c    This code is distributed under the GNU LGPL license. 
c
c  Modified:
c
c    20 November 2007
c
c  Author:
c
c    John Burkardt
c
c  Reference:
c
c    OpenMP Application Program Interface,
c    Version 2.5,
c    May 2005.
c
c  Parameters:
c
c    Input/output, integer NLOCK, the nestable lock.
c
      implicit none

      integer nlock

      if ( nlock .eq. 1 ) then
        nlock = -1
      else if ( nlock .eq. 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'OMP_UNSET_NEST_LOCK - Fatal error!'
        write ( *, '(a)' ) '  Nested lock not initialized.'
        stop
      else
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'OMP_UNSET_NEST_LOCK - Fatal error!'
        write ( *, '(a)' ) '  Nested lock not set.'
        stop
      end if

      return
      end
