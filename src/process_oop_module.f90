module process_oop_module
    use station_oop_module
    use linked_list_module
    use global_module
    implicit none

    type(LinkedList) :: stationsList

    !stationsList = LinkedList()


contains
    subroutine skip_header_lines(header_lines, z)

        implicit none
        integer, intent (in) :: header_lines
        integer :: i
        integer, intent(in) :: z

    !   Skip header lines

        do i = 1, header_lines
        read (unit= z, fmt='(a)')
        end do
        print *, "Skipped ", header_lines, " lines"

    end subroutine
    subroutine buildStations()
        type(station_oop_type) :: stationptr
        class(*), pointer :: general_ptr
        integer :: stat

        stationsList = LinkedList()

        open (unit=UNIT_NO, file=LIST_OF_STATIONS, status= 'old')
101     format (i3,2x,a15,2x,a23,2x,i3)  ! station_number,site_name, station_data_file_name,file_header_size

        stat = 0
        do while (stat == 0)
            stationptr = station_oop_type()
            read(unit=UNIT_NO,fmt=101,iostat=stat)stationptr%station_number, &
                                              stationptr%site_name, &
                                              stationptr%station_data_file_name, &
                                              stationptr%file_header_size
            if (stat < 0)  then
                cycle
            else if (stationptr%station_number > 0) then
                allocate(general_ptr, source = stationptr)
                call stationsList%append(general_ptr)
            end if
        end do
        close(UNIT_NO)
        call stationsList%traverse(printValues)
    end subroutine buildStations

    subroutine readStationFiles() 
        call stationsList%traverse(tranverseStationList)
    end subroutine readStationFiles

    subroutine tranverseStationList(node) 
        type(LinkedListNode), pointer, intent(inout) :: node
        select type (p => node%value)
                class is (station_oop_type)
                    call readDataFile(p)
        end select 
    end subroutine tranverseStationList

    subroutine readDataFile(station)
        implicit none
        class(station_oop_type),pointer,intent(inout) :: station
        class(station_data_oop_type), pointer :: lineOfData
        class(*), pointer :: general_ptr  
        integer :: stat 
        integer :: y

        open (unit=UNIT_NO, file=station%station_data_file_name, status='old')
        call skip_header_lines(station%file_header_size,UNIT_NO)
        stat = 0 
        do while (stat == 0)
            allocate(lineOfData)

            read (unit=UNIT_NO, fmt=100,iostat=stat) lineOfData%year, &
                                                    lineOfData%month, &
                                                    lineOfData%max_temp, &
                                                    lineOfData%min_temp, &
                                                    lineOfData%af_days, &
                                                    lineOfData%rainfall, &
                                                    lineOfData%sunshine

100       format (3x, i4, 2x, i2, 2x, f5.1, 3x, f5.1, 3x, i5, 2x, f6.1, 1x, f6.1)
            if (stat < 0)  then
                cycle   
            else if (lineOfData%year > 0) then
               allocate(general_ptr, source = lineOfData)
               call station%station_data%append(general_ptr)
            end if
        end do
        close(UNIT_NO)
        call station%loadArray()
        do y = 1,4
            call station%arraydata(y)%printdata()
        end do
    end subroutine 

    subroutine printValues(node)
        type(LinkedListNode), pointer, intent(inout) :: node
        integer :: k = 1
        select type(p => node%value)
            type is (station_oop_type)
               print *, "Node Number: ", k
               k = k + 1
               call p%printList()
               print *, ""
            class is (station_data_oop_type)
               call p%printdata() 
        end select
    end subroutine printValues  
end module process_oop_module