module global_module
    implicit none

    integer,parameter :: UNIT_NO =100
    character *16,parameter, public :: LIST_OF_STATIONS = 'station_list.txt'
    integer, parameter :: n_months = 12
    character *9, dimension (1:n_months),parameter, public :: month_names = (/ 'January  ', 'February ', 'March    ', &
    'April    ', 'May      ', 'June     ', 'July     ', 'August   ', 'September', 'October  ', 'November ', 'December ' /)
end module global_module