module linked_list_module
    implicit none

    type, public :: LinkedListNode
        class(*), pointer :: value => null()
        type(LinkedListNode), pointer :: next => null() 
    contains
        final :: nodefinalize
    end type LinkedListNode
    
    type, public :: LinkedList 
        integer :: size = 0
        type(LinkedListNode), pointer :: head => null()
        type(LinkedListNode), pointer :: tail => null()
    contains
        procedure :: append
        procedure :: first
        procedure :: last
        procedure :: atindex
        procedure :: reset 
        procedure :: length
        procedure :: traverse
        procedure, private :: cleanup
            final :: lastfinalize
    end type LinkedList
    interface LinkedList
        module procedure Constructor
    end interface

    contains
        function Constructor() result(list)
            type(LinkedList) list 
            list%size = 0
        end function
        subroutine nodefinalize(this)
            type(LinkedListNode), intent(inout) :: this

            if (associated(this%value)) then
                deallocate(this%value)
                nullify(this%value)
                nullify(this%next)
            end if
        end subroutine nodefinalize
        
        pure function length(this) result(size)
            class(LinkedList), intent(in) :: this
            integer :: size
            size = this%size
        end function length

        function first(this) result(firstnode)
            class(LinkedList), intent(in) :: this
            type(LinkedListNode), pointer :: firstnode
            firstnode => this%head
        end function first
        
        function last(this) result(lastnode)
            class(LinkedList), intent(in) :: this
            type(LinkedListnode), pointer :: lastnode

            lastnode => this%tail
        end function last

        subroutine append(this, value)
            class(LinkedList), intent(inout) :: this
            class(*), intent(in),  pointer :: value
            
            type(LinkedListNode), pointer :: node_ptr
            type(LinkedListNode), pointer :: current_ptr

            allocate(node_ptr)
            node_ptr%value => value
            node_ptr%next => null()

            this%size = this%size + 1

            if (.not. associated(this%head)) then
                this%head => node_ptr
                this%tail => node_ptr
            else 
                this%tail%next => node_ptr
                this%tail => node_ptr
            end if
        end subroutine append

        function atindex(this,index) result(indexnode)
            class(LinkedList), intent(in) :: this
            integer, intent(in) :: index
            type(LinkedListNode), pointer :: indexnode

            integer :: i

            nullify(indexnode) 
            if (index > 0 .and. index <= this%size) then 
                indexnode => this%head 
                do i = 1, index - 1
                    indexnode => indexnode%next
                end do
            end if
        end function atindex
        
        subroutine traverse(this, iterator_func)
            class(LinkedList), intent(inout) :: this
            interface
                subroutine iterator_func(node)
                    import LinkedListNode
                    type(LinkedListNode), pointer, intent(inout) :: node
                end subroutine iterator_func
            end interface

            type(LinkedListNode), pointer :: current_ptr, temp_ptr

            current_ptr => this%head
            do while(associated(current_ptr))
                nullify(temp_ptr)
                temp_ptr => current_ptr%next
                call iterator_func(current_ptr)
                current_ptr => temp_ptr
            end do
        end subroutine traverse

        subroutine cleanup(this)
            class(LinkedList), intent(inout) :: this
            type(LinkedListNode), pointer :: current_ptr

            call this%traverse(destroyall)
            nullify(this%head)
            nullify(this%tail)

        contains
            subroutine destroyall(node)
                type(LinkedListNode), pointer, intent(inout) :: node

                this%head => node%next
                deallocate(node)
                nullify(node)

                this%size = this%size - 1
            end subroutine destroyall
        end subroutine cleanup

        subroutine reset(this)
            class(LinkedList), intent(inout) :: this

            call this%cleanup()
        end subroutine reset

        subroutine lastfinalize(this)
            type(LinkedList) :: this
            call this%cleanup()
        end subroutine lastfinalize        
end module linked_list_module