module station_oop_module
    use linked_list_module
    use station_data_oop_module
    implicit none 

    type station_oop_type
        integer :: station_number = 0
        character *15 :: site_name
        character *23 :: station_data_file_name 
        integer :: file_header_size = 0
        integer  :: lines_read = 0 
        type(LinkedList) :: station_data
        type(station_data_oop_type), dimension(:), allocatable ::  arraydata
    contains
        procedure, pass(this)  :: setLinesRead
        procedure, pass(this) :: loadArray
        procedure, pass(this)  :: printList
    end type station_oop_type
    interface station_oop_type
        module procedure MetStation
    end interface station_oop_type
contains
        function MetStation() result(newstation) 
            type(station_oop_type) :: newstation
            newstation%station_number = 0
            newstation%file_header_size = 0
            newstation%lines_read = 0
            newstation%station_data = LinkedList()
        end function Metstation 

        subroutine setLinesRead(this)
            class(station_oop_type), intent(inout) :: this
            this%lines_read = this%station_data%size
        end subroutine setLinesRead 
        
        subroutine loadArray(this)
            class(station_oop_type), intent(inout) :: this
            if (this%station_data%size > 0) then
                call this%setLinesRead()
                allocate(this%arraydata(this%lines_read))
                call this%station_data%traverse(loadStationData)
            else 
                print *, "No data read into linked list"  
            end if      
        contains
            subroutine loadStationData(node)
                type(LinkedListNode), pointer, intent(inout) :: node
                integer :: i = 1 
                select type(p => node%value)
                    type is (station_data_oop_type)
                       this%arraydata(i) = p
                       if (i >= this%lines_read ) then
                            i = 1
                       else
                            i = i + 1                           
                       end if
                end select
            end subroutine loadStationData    
        end subroutine loadArray

        subroutine printList(this)
            class(station_oop_type), intent(inout) ::this
            print *,"Station Number: " ,this%station_number
            print *, "Site Name: ", this%site_name
            print *, "Station data file: ", this%station_data_file_name
            print *, "File header length: ", this%file_header_size
            print *, "Lines read: ", this%lines_read
        end subroutine

end module station_oop_module 