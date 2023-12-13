program main

  use process_oop_module

  implicit none

  !call build_station_list()
 ! print *, "total number of stations: ", total_no_stations
  !call load_station_data()
  call buildStations()
  call readStationFiles()

end program main
