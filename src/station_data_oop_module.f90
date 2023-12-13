module station_data_oop_module
    implicit none

    type station_data_oop_type
        integer :: year
        integer :: month
        real :: max_temp     ! centigrade
        real :: min_temp     ! centigrade   
        integer :: af_days
        real :: rainfall     ! centimeter 
        real :: sunshine
    contains
        procedure, pass(this) :: printdata
    end type station_data_oop_type

contains
    subroutine printdata(this)
        class(station_data_oop_type), intent(inout) ::this
        print *,"Year: " ,this%year
        print *, "Month: ", this%month
        print *, "Max Temperature: ", this%max_temp
        print *, "Min Temperature: ", this%min_temp
        print *, "AF Days: ", this%af_days
        print *, "Rainfall: ", this%rainfall
        print * , "Sunshine: ", this%sunshine
    end subroutine

end module station_data_oop_module