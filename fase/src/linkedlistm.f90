module linkedlistm
    implicit none

    type :: node
        integer :: data
        type(node), pointer :: next => null()
        type(node), pointer :: prev => null()
    end type node

    type :: linkedlist
        integer :: size = 0
        type(node), pointer :: head => null()
        type(node), pointer :: tail => null()
    contains
        procedure :: add
        procedure :: remove
        procedure :: search
        procedure :: print
    end type linkedlist
contains

    subroutine remove(this, value)
        class(linkedlist), intent(inout) :: this
        integer, intent(in) :: value
        type(node), pointer :: current_node

        current_node => this%head
        do while (associated(current_node))
            if (current_node%data == value) then
                if (associated(current_node%prev)) then
                    current_node%prev%next => current_node%next
                else
                    this%head => current_node%next
                end if

                if (associated(current_node%next)) then
                    current_node%next%prev => current_node%prev
                else
                    this%tail => current_node%prev
                end if

                deallocate(current_node)
                this%size = this%size - 1
                return
            end if
            current_node => current_node%next
        end do
    end subroutine remove


    subroutine add(this, value)
        class(linkedlist), intent(inout) :: this
        integer, intent(in) :: value
        type(node), pointer :: new_node

        if ( this%search(value) ) then
            return
        end if

        allocate(new_node)
        new_node%data = value
        new_node%next => null()
        new_node%prev => this%tail

        if (associated(this%tail)) then
            this%tail%next => new_node
        else
            this%head => new_node
        end if

        this%tail => new_node
        this%size = this%size + 1
    end subroutine add
    
    subroutine print(this)
        class(linkedlist), intent(in) :: this
        type(node), pointer :: current_node

        current_node => this%head
        do while (associated(current_node))
            write(*,'(I0)', advance="no") current_node%data
            if ( associated(current_node%next) ) then
                write(*,'(A)', advance ='no') ' , '
            end if
            current_node => current_node%next
        end do
    end subroutine print

    function search(this, value) result(found)
        class(linkedlist), intent(in) :: this
        integer, intent(in) :: value
        type(node), pointer :: current_node
        logical :: found

        current_node => this%head
        do while (associated(current_node))
            if (current_node%data == value) then
                found = .true.
                return
            end if
            current_node => current_node%next
        end do
        found = .false.
    end function search

end module linkedlistm