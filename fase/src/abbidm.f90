module abbidm
    
    use linkedlistm, only: linkedlist

    implicit none

    integer :: id = 1
    type :: node
        integer :: value
        integer :: uid
        type(node), pointer :: left => null()
        type(node), pointer :: right => null()
    end type

    type :: node_queue
        class(node), pointer :: data
        type(node_queue), pointer :: next => null()
    end type
    type :: queue
        type(node_queue), pointer :: head => null()
        type(node_queue), pointer :: tail => null()
    contains
        procedure :: enqueue
        procedure :: dequeue
        procedure :: isempty
    end type

    type :: abbid
        type(linkedlist) :: list
        type(node), pointer :: root => null()
        contains
        procedure :: add
        procedure :: add_rec
        procedure :: preorder
        procedure :: inorder
        procedure :: postorder
        procedure :: dotgen
        procedure :: dotgen2
        procedure :: dotgen_rec
        procedure :: delete
        procedure :: delete_rec
        procedure :: search
        procedure :: search_rec
        procedure :: amplitude
    end type

    contains

        subroutine enqueue(this, value)
            class(queue), intent(inout) :: this
            type(node_queue), pointer :: tmp
            type(node), target, intent(in) :: value
            allocate(tmp)
            tmp%data => value
            if (.not. associated(this%head)) then
                this%head => tmp
                this%tail => tmp
            else
                this%tail%next => tmp
                this%tail => tmp
            end if
        end subroutine enqueue

        function dequeue(this) result(res)
            class(queue), intent(inout) :: this
            type(node_queue), pointer :: tmp
            class(node), pointer :: res
            if (.not. associated(this%head)) then
                res => null()
                return
            end if
            res => this%head%data
            tmp => this%head
            this%head => this%head%next
            deallocate(tmp)
        end function dequeue

        function isempty(this) result(res)
            class(queue), intent(in) :: this
            logical :: res
            res = .not. associated(this%head)
        end function isempty


        subroutine add(this, value)
            class(abbid), intent(inout) :: this
            integer, intent(in) :: value
            type(node), pointer :: tmp
            if(associated(this%root)) then
                call this%add_rec(value, this%root)
            else
                allocate(tmp)
                tmp%value = value
                tmp%uid = id
                id = id + 1
                this%root => tmp
            end if
        end subroutine add

        subroutine add_rec(this, value, tmp)
            class(abbid), intent(inout) :: this
            integer, intent(in) :: value
            class(node), intent(inout) :: tmp
            if (value < tmp%value) then
                if (associated(tmp%left)) then
                    call this%add_rec(value, tmp%left)
                else
                    allocate(tmp%left)
                    tmp%left%value = value
                    tmp%left%uid = id
                    id = id + 1
                end if
            else
                if (associated(tmp%right)) then
                    call this%add_rec(value, tmp%right)
                else
                    allocate(tmp%right)
                    tmp%right%value = value
                    tmp%right%uid = id
                    id = id + 1
                end if
            end if
        end subroutine add_rec

        subroutine preorder(this, tmp)
            class(abbid), intent(inout) :: this
            class(node), intent(in), pointer :: tmp
            if( .not. associated(tmp)) then
                return
            end if
            call this%list%add(tmp%value)
            call this%preorder(tmp%left)
            call this%preorder(tmp%right)
        end subroutine preorder

        subroutine inorder(this, tmp)
            class(abbid), intent(inout) :: this
            class(node), intent(in), pointer :: tmp
            if( .not. associated(tmp)) then
                return
            end if
            call this%inorder(tmp%left)

            call this%list%add(tmp%value)
            call this%inorder(tmp%right)
        end subroutine inorder

        subroutine postorder(this, tmp)
            class(abbid), intent(inout) :: this
            class(node), intent(in), pointer :: tmp
            if( .not. associated(tmp)) then
                return
            end if
            call this%postorder(tmp%left)
            call this%postorder(tmp%right)

            call this%list%add(tmp%value)
        end subroutine postorder

        subroutine dotgen2(this)
            class(abbid), intent(inout) :: this
            integer :: unit
            character(len=100) :: filename = "img/abbtree.dot"
            character(len=:), allocatable :: temp

            temp = "Preorder: "
            call this%preorder(this%root)
            do while (associated(this%list%head))
                temp = trim(temp) // ' ' // int_to_string(this%list%head%data)
                call this%list%remove(this%list%head%data)
            end do
            temp = trim(temp) // '\nInorder: '
            call this%inorder(this%root)
            do while (associated(this%list%head))
                temp = trim(temp) // ' ' // int_to_string(this%list%head%data)
                call this%list%remove(this%list%head%data)
            end do
            temp = trim(temp) // '\nPostorder: '
            call this%postorder(this%root)
            do while (associated(this%list%head))
                temp = trim(temp) // ' ' // int_to_string(this%list%head%data)
                call this%list%remove(this%list%head%data)
            end do
            open(unit, file=filename, status='replace')
            write(unit, '(A)') 'graph{'
            write(unit, '(A)') 'info [label="' // temp // '"];'
            write(unit, '(A)') 'info -- ' // int_to_string(this%root%uid) // ';'
            call this%dotgen_rec(this%root, unit)
            write(unit, '(A)') '}'
            close(unit)
            call execute_command_line('dot -Tsvg img/abbtree.dot > img/abbtree.svg')
            call execute_command_line('eog img/abbtree.svg')
        end subroutine dotgen2

        subroutine dotgen(this)
            class(abbid), intent(in) :: this
            integer :: unit
            character(len=100) :: filename = "img/abbtree.dot"
            open(unit, file=filename, status='replace')
            write(unit, '(A)') 'graph{'
            call this%dotgen_rec(this%root, unit)
            write(unit, '(A)') '}'
            close(unit)
            call execute_command_line('dot -Tsvg img/abbtree.dot > img/abbtree.svg')
            call execute_command_line('eog img/abbtree.svg')
        end subroutine dotgen

        subroutine dotgen_rec(this, tmp, unit)
            class(abbid), intent(in) :: this
            class(node), intent(in), pointer :: tmp
                integer, intent(in) :: unit
            if (.not. associated(tmp)) then
                    return
                end if
            write (unit, '(A,I0,A,I0,A)') ' l', tmp%uid, ' [label="', tmp%value, '"];'
            if (associated(tmp%left)) then
                    write (unit, '(A,I0,A,I0,A)') ' l', tmp%uid, ' -> l', tmp%left%uid, ';'
                end if
            if (associated(tmp%right)) then
                    write (unit, '(A,I0,A,I0,A)') ' l', tmp%uid, ' -> l', tmp%right%uid, ';'
                end if
            call this%dotgen_rec(tmp%left, unit)
                call this%dotgen_rec(tmp%right, unit)
        end subroutine dotgen_rec


        subroutine delete(this, idd)
            class(abbid), intent(inout) :: this
            integer, intent(in) :: idd
            call this%delete_rec(idd, this%root)
        end subroutine delete

        subroutine delete_rec(this, value, tmp)
            class(abbid), intent(inout) :: this
            integer, intent(in) :: value
            type(node), pointer ,intent(inout) :: tmp
            type(node), pointer :: tmp2
            if (.not. associated(tmp)) then
                return
            end if
            if (value < tmp%value) then
                call this%delete_rec(value, tmp%left)
            else if (value > tmp%value) then
                call this%delete_rec(value, tmp%right)
            else
                if (.not. associated(tmp%left) .and. .not. associated(tmp%right)) then
                    deallocate(tmp)
                    tmp => null()
                else if (.not. associated(tmp%left)) then
                    tmp2 => tmp
                    tmp => tmp%right
                    deallocate(tmp2)
                else if (.not. associated(tmp%right)) then
                    tmp2 => tmp
                    tmp => tmp%left
                    deallocate(tmp2)
                else
                    tmp2 => tmp%left
                    do while (associated(tmp2%right))
                        tmp2 => tmp2%right
                    end do
                    tmp%value = tmp2%value
                    call this%delete_rec(tmp2%value, tmp%left)
                end if
            end if
        end subroutine delete_rec

        function search(this, idd) result(res)
            class(abbid), intent(in) :: this
            integer, intent(in) :: idd
            integer, pointer :: res
            res => this%search_rec(idd, this%root)
        end function search

        recursive function search_rec(this, value, tmp) result(res)
            class(abbid), intent(in) :: this
            integer, intent(in) :: value
            class(node), intent(in), pointer :: tmp
            integer, pointer :: res
            if (.not. associated(tmp)) then
                write (*, '(A)') 'Not found'
                res => null()
                return
            end if
            if (value < tmp%value) then
                res => this%search_rec(value, tmp%left)
            else if (value > tmp%value) then
                res => this%search_rec(value, tmp%right)
            else
                res => tmp%value
            end if
        end function search_rec

        function int_to_string(i) result(s)
            integer, intent(in) :: i
            character(len=32) :: s
            write(s, '(I0)') i
        end function int_to_string

        subroutine amplitude(this)
            class(abbid), intent(inout) :: this
            type(queue) :: q
            class(node), pointer :: tmp
            if (.not. associated(this%root)) then
                return
            end if
            call q%enqueue(this%root)
            do while (.not. q%isempty())
                tmp => q%dequeue()
                if ( associated(tmp) ) then
                    call this%list%add(tmp%value)
                    if ( associated(tmp%left) ) then
                        call q%enqueue(tmp%left)
                    end if
                    if ( associated(tmp%right) ) then
                        call q%enqueue(tmp%right)
                    end if
                end if
            end do
        end subroutine amplitude
end module abbidm