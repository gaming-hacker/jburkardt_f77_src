function second ( )

!*******************************************************************************
!
!! SECOND returns the current elapsed CPU time in seconds.
!
!  Modified:
!
!    27 October 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, real ( kind = 8 ) SECOND, the current CPU time in seconds.
!
  implicit none

  real ( kind = 8 ) second
  real ( kind = 8 ) time

  call cpu_time ( time )

  second = time

  return
end
