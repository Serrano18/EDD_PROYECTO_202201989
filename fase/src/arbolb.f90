module arbolb
    use usuario, only: User
    implicit none
    
      	! Order 5
    integer, parameter :: MAXI = 4, MINI = 2 

    type nodeptr
        type (BTreeNode), pointer :: ptr => null()
    end type nodeptr

    type BTreeNode
        type(User) :: val(0:MAXI+1) !arr de usuarios
        integer :: num = 0
        type(nodeptr) :: link(0:MAXI+1)
        type(BTreeNode), pointer :: root => null() !PUNTERO PADRE
        contains
        procedure :: insert
        procedure :: printTree
        procedure :: createNode
        procedure :: removeUser
        procedure :: graphTree
        procedure :: searchUser
        procedure :: searchUserByNameAndPassword
    end type BTreeNode

    
contains

subroutine graphTree(this)
    class(BTreeNode), intent(in) :: this
    integer :: unit, count = 0
    open(unit, file='btree.dot', status='replace')
    write(unit, '(A)', advance='no') 'digraph G {'
    call graphBtree(this%root, unit, count)
    write(unit, '(A)', advance='no') '}'
    close(unit)
    call execute_command_line('dot -Tsvg btree.dot -o btree.svg')
    call execute_command_line('start btree.svg')

end subroutine graphTree

recursive subroutine graphBtree(myNode, unit, count)
    type(BTreeNode), pointer, intent(in) :: myNode
    integer, intent(in) :: unit
    integer, intent(inout) :: count
    integer :: i, temp
    if (associated(myNode)) then
        count = count + 1
        write(unit, '(A,I0,A)', advance='no') 'node', count, ' [label="'
        do i = 1, myNode%num
            write(unit, '(I0,A,A)', advance='no') myNode%val(i)%dpi, ',', trim(myNode%val(i)%nombre)
            if (i < myNode%num) then
                write(unit, '(A)', advance='no') '|'
            end if
        end do
        write(unit, '(A)', advance='no') '"];'

        temp = count
        do i = 0, myNode%num
            if (associated(myNode%link(i)%ptr)) then
                write(unit, '(A, I0, A, I0, A)', advance='no') 'node', temp, ' -> node', count + 1, ';'
                call graphBtree(myNode%link(i)%ptr, unit, count)
            end if
        end do
    end if
end subroutine graphBtree

subroutine insert(this,users)
    class(BTreeNode), intent(inout) :: this
    type(User), intent(in) :: users
    type(User) :: i
    type(BTreeNode), pointer :: child
    allocate(child)
    if (setValue(users, i, this%root, child)) then
            this%root => this%createNode(i, child)
    end if
    
end subroutine insert

recursive function setValue(users,pusers, node, child) result(res)
    type(User),  intent(in) :: users
    type(User),  intent(inout) :: pusers
    type(BTreeNode), pointer, intent(inout) :: node
    type(BTreeNode), pointer, intent(inout) :: child
    type(BTreeNode), pointer :: newnode        
    integer :: pos
    logical :: res
    allocate(newnode)
    if (.not. associated(node)) then            
            pusers = users
            child => null()
            res = .true.
            return
    end if
    if (users%dpi < node%val(1)%dpi) then
            pos = 0
    else
            pos = node%num
            do while (users%dpi < node%val(pos)%dpi .and. pos > 1) 
            pos = pos - 1
            end do
            if (users%dpi == node%val(pos)%dpi) then
                print *, "Duplicates are not permitted"
                res = .false.
                return
            end if
    end if
    if (setValue(users, pusers, node%link(pos)%ptr, child)) then
            if (node%num < MAXI) then
                call insertNode(pusers, pos, node, child)
            else
                call splitNode(pusers, pusers, pos, node, child, newnode)
                child => newnode
                res = .true.
            return
        end if
    end if
    res = .false.
end function setValue

subroutine insertNode(users, pos, node, child)
    type(User), intent(in) :: users
    integer, intent(in) :: pos
    type(BTreeNode), pointer, intent(inout) :: node
    type(BTreeNode), pointer, intent(in) :: child
    integer :: j
    j = node%num
    do while (j > pos)
            node%val(j + 1) = node%val(j)
            node%link(j + 1)%ptr => node%link(j)%ptr
            j = j - 1
    end do
    node%val(j + 1) = users
    node%link(j + 1)%ptr => child
    node%num = node%num + 1
end subroutine insertNode

subroutine splitNode(users, pusers, pos, node, child, newnode)
    type(User), intent(in) :: users
    integer, intent(in) :: pos
    type(User), intent(inout) :: pusers
    type(BTreeNode), pointer, intent(inout) :: node,  newnode
    type(BTreeNode), pointer, intent(in) ::  child
    integer :: median, i, j
    if (pos > MINI) then
            median = MINI + 1
    else
            median = MINI
    end if
    if (.not. associated(newnode)) then
        allocate(newnode)
    do i = 0, MAXI
                newnode%link(i)%ptr => null()
        enddo
    end if
    j = median + 1
    do while (j <= MAXI)
            newnode%val(j - median) = node%val(j)
            newnode%link(j - median)%ptr => node%link(j)%ptr
            j = j + 1
    end do
    node%num = median
    newnode%num = MAXI - median
    if (pos <= MINI) then
            call insertNode(users, pos, node, child)
    else
            call insertNode(users, pos - median, newnode, child)
    end if        
    pusers = node%val(node%num)        
    newnode%link(0)%ptr => node%link(node%num)%ptr
    node%num = node%num - 1
end subroutine splitNode

function createNode(this,users, child) result(newNode)
    class(BTreeNode), intent(inout) :: this
    type(User), intent(in) :: users
    type(BTreeNode), pointer, intent(in) :: child
    type(BTreeNode), pointer :: newNode
    integer :: i
    allocate(newNode)
    newNode%val(1) = users
    newNode%num = 1
    newNode%link(0)%ptr => this%root
    newNode%link(1)%ptr => child
    do i = 2, MAXI
            newNode%link(i)%ptr => null()
    end do
end function createNode

subroutine printTree(this)
    class(BTreeNode), intent(in) :: this
    call traversal(this%root)
end subroutine printTree

recursive subroutine traversal(myNode)
    type(BTreeNode), pointer, intent(in) :: myNode
    integer :: i
    if (associated(myNode)) then
            write (*, '(A)', advance='no') ' [ '
            i = 0
            do while (i < myNode%num)
                write (*,'(1I3)', advance='no') myNode%val(i+1)%dpi
                i = i + 1
            end do
            do i = 0, myNode%num
                call traversal(myNode%link(i)%ptr)    
            end do
            write (*, '(A)', advance='no') ' ] '
    end if
end subroutine traversal

function searchUser(this, dpi) result(users)
    class(BTreeNode), intent(inout) :: this
    integer(kind=8), intent(in) :: dpi
    type(User), pointer :: users
    users => search(this%root, dpi)
end function searchUser

recursive function search(myNode, dpi) result(users)
    type(BTreeNode), pointer, intent(inout) :: myNode
    integer(kind=8), intent(in) :: dpi
    type(User), pointer :: users
    integer :: i
    if (associated(myNode)) then
            i = 1
            do while (i <= myNode%num .and. dpi > myNode%val(i)%dpi)
                i = i + 1
            end do
            if (i <= myNode%num .and. dpi == myNode%val(i)%dpi) then
                print *, "Usuario encontrado : ", myNode%val(i)%nombre
                users => myNode%val(i)
            else
                users => search(myNode%link(i-1)%ptr, dpi)
            end if
    else
            print *, "Usuario no encontrado"
            users => null()
    end if
end function search
function searchUserByNameAndPassword(this, nombre, pass) result(users)
    class(BTreeNode), intent(inout) :: this
    character(len=*), intent(in) :: nombre
    character(len=*), intent(in) :: pass
    type(User), pointer :: users
    users => searchByNameAndPassword(this%root, nombre, pass)
end function searchUserByNameAndPassword

recursive function searchByNameAndPassword(myNode, nombre, pass) result(users)
    type(BTreeNode), pointer, intent(inout) :: myNode
    character(len=*), intent(in) :: nombre
    character(len=*), intent(in) :: pass
    type(User), pointer :: users
    integer :: i
    if (associated(myNode)) then
        i = 1
        do while (i <= myNode%num .and. nombre /= myNode%val(i)%nombre .or. pass /= myNode%val(i)%password)
            i = i + 1
        end do
        if (i <= myNode%num .and. nombre == myNode%val(i)%nombre .and. pass == myNode%val(i)%password) then
            print *, "Usuario encontrado: ", myNode%val(i)%nombre
            users => myNode%val(i)
        else
            users => searchByNameAndPassword(myNode%link(i-1)%ptr, nombre, pass)
        end if
    else
        print *, "Usuario no encontrado"
        users => null()
    end if
end function searchByNameAndPassword

subroutine removeUser(this, dpi)
    class(BTreeNode), intent(inout) :: this
    integer(kind=8), intent(in) :: dpi
    call remove(this%root, dpi)
end subroutine removeUser

recursive subroutine remove(nodo, dpi)
    type(BTreeNode), pointer, intent(inout) :: nodo
    integer(kind=8), intent(in) :: dpi
    integer :: i, j
    if (associated(nodo)) then
            i = 1
            do while (i <= nodo%num .and. dpi > nodo%val(i)%dpi)
                i = i + 1
            end do
            if (i <= nodo%num .and. dpi == nodo%val(i)%dpi) then
                if (.not. associated(nodo%link(i-1)%ptr)) then
                    do j = i + 1, nodo%num
                        nodo%val(j-1) = nodo%val(j)
                        nodo%link(j-1)%ptr => nodo%link(j)%ptr
                    end do
                    nodo%num = nodo%num - 1
                else
                    call restore(nodo, i)
                end if
            else
                call remove(nodo%link(i-1)%ptr, dpi)
            end if
    else
            print *, "Client not found"
    end if
end subroutine remove

subroutine restore(nodo, pos)
    type(BTreeNode), pointer, intent(inout) ::nodo
    integer, intent(in) :: pos
    type(BTreeNode), pointer :: q
    q => nodo%link(pos-1)%ptr
    if (q%num > MINI) then
            do while (associated(q%link(q%num)%ptr))
                q => q%link(q%num)%ptr
            end do
            nodo%val(pos) = q%val(q%num)
            q%num = q%num - 1
    else
            call combine(nodo, pos)
    end if
end subroutine restore

subroutine combine(nodo, pos)
    type(BTreeNode), pointer, intent(inout) :: nodo
    integer, intent(in) :: pos
    type(BTreeNode), pointer :: q, r
    integer :: i
    q => nodo%link(pos-1)%ptr
    r => nodo%link(pos)%ptr
    q%num = q%num + 1
    q%val(q%num) = nodo%val(pos)
    q%link(q%num)%ptr => r%link(0)%ptr
    do i = 1, r%num
            q%num = q%num + 1
            q%val(q%num) = r%val(i)
            q%link(q%num)%ptr => r%link(i)%ptr
    end do
    do i = pos, nodo%num-1
            nodo%val(i) = nodo%val(i+1)
            nodo%link(i)%ptr => nodo%link(i+1)%ptr
    end do
    nodo%num = nodo%num - 1
    deallocate(r)
end subroutine combine

end module arbolb