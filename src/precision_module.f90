module precision_module
  implicit none
! 
! Updated with the release of Nag 7 which
! supports 16 bit reals.
! 
! single, double, quad naming used by lapack.
! hence sp, dp, qp
!
! we have used hp as half precision
!
  integer, parameter :: hp = selected_real_kind( 3,   4)
  integer, parameter :: sp = selected_real_kind( 6,  37)
  integer, parameter :: dp = selected_real_kind(15, 307)
  integer, parameter :: qp = selected_real_kind(30, 291)
end module
