module arbolb

    implicit none
    
      	! Order 4
    integer, parameter :: MAXI = 3, MINI = 1 

    type nodeptr
        type (BTreeNode), pointer :: ptr => null()
    end type nodeptr

    type BTreeNode
        integer :: val(0:MAXI+1)
        integer :: num = 0
        type(nodeptr) :: link(0:MAXI+1)
        type(BTreeNode), pointer :: root => null() !PUNTERO PADRE
        contains
        procedure :: insert
        procedure :: printTree
        procedure :: createNode
        procedure :: remove
        procedure :: graphTree
        
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
            write(unit, '(I0)', advance='no') myNode%val(i)
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

subroutine insert(this,val)
    class(BTreeNode), intent(inout) :: this
    integer, intent(in) :: val
    integer :: i
    type(BTreeNode), pointer :: child
    allocate(child)
    if (setValue(val, i, this%root, child)) then
            this%root => this%createNode(i, child)
    end if
end subroutine insert

recursive function setValue(val, pval, node, child) result(res)
    integer, intent(in) :: val
    integer, intent(inout) :: pval
    type(BTreeNode), pointer, intent(inout) :: node
    type(BTreeNode), pointer, intent(inout) :: child
    type(BTreeNode), pointer :: newnode        
    integer :: pos
    logical :: res
    allocate(newnode)
    if (.not. associated(node)) then            
            pval = val
            child => null()
            res = .true.
            return
    end if
    if (val < node%val(1)) then
            pos = 0
    else
            pos = node%num
            do while (val < node%val(pos) .and. pos > 1) 
            pos = pos - 1
            end do
            if (val == node%val(pos)) then
                print *, "Duplicates are not permitted"
                res = .false.
                return
            end if
    end if
    if (setValue(val, pval, node%link(pos)%ptr, child)) then
            if (node%num < MAXI) then
                call insertNode(pval, pos, node, child)
            else
                call splitNode(pval, pval, pos, node, child, newnode)
                child => newnode
                res = .true.
            return
        end if
    end if
    res = .false.
end function setValue

subroutine insertNode(val, pos, node, child)
    integer, intent(in) :: val, pos
    type(BTreeNode), pointer, intent(inout) :: node
    type(BTreeNode), pointer, intent(in) :: child
    integer :: j
    j = node%num
    do while (j > pos)
            node%val(j + 1) = node%val(j)
            node%link(j + 1)%ptr => node%link(j)%ptr
            j = j - 1
    end do
    node%val(j + 1) = val
    node%link(j + 1)%ptr => child
    node%num = node%num + 1
end subroutine insertNode

subroutine splitNode(val, pval, pos, node, child, newnode)
    integer, intent(in) :: val, pos
    integer, intent(inout) :: pval
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
            call insertNode(val, pos, node, child)
    else
            call insertNode(val, pos - median, newnode, child)
    end if        
    pval = node%val(node%num)        
    newnode%link(0)%ptr => node%link(node%num)%ptr
    node%num = node%num - 1
end subroutine splitNode

function createNode(this,valor, child) result(newNode)
    class(BTreeNode), intent(inout) :: this
    integer, intent(in) :: valor
    type(BTreeNode), pointer, intent(in) :: child
    type(BTreeNode), pointer :: newNode
    integer :: i
    allocate(newNode)
    newNode%val(1) = valor
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
                write (*,'(1I3)', advance='no') myNode%val(i+1)
                i = i + 1
            end do
            do i = 0, myNode%num
                call traversal(myNode%link(i)%ptr)    
            end do
            write (*, '(A)', advance='no') ' ] '
    end if
end subroutine traversal

subroutine remove(this,val)
    class(BTreeNode), intent(inout) :: this
    integer, intent(in) :: val
    logical :: isDeleted
    if (.not. associated(this%root)) then
        print *, "Empty tree"
        return
    end if
    call deleteValue(val, this%root, isDeleted)
    if (isDeleted .and. this%root%num == 0) then
        if (associated(this%root%link(0)%ptr)) then
            this%root => this%root%link(0)%ptr
        end if
    end if
end subroutine remove

recursive subroutine deleteValue(val, node, isDeleted)
    integer, intent(in) :: val
    type(BTreeNode), pointer, intent(inout) :: node
    logical, intent(out) :: isDeleted
    integer :: pos, successorVal

    isDeleted = .false.

    if (.not. associated(node)) then
        return
    end if

    pos = findPosition(val, node)

    if (pos > 0) then
        if (associated(node%link(pos-1)%ptr)) then
            call getPredecessor(node%link(pos-1)%ptr, successorVal)
            node%val(pos) = successorVal
            call deleteValue(successorVal, node%link(pos-1)%ptr, isDeleted)
            if (.not. isDeleted) then
                return
            end if
        end if
    else
        pos = -pos
        call deleteValue(val, node%link(pos)%ptr, isDeleted)
        if (.not. isDeleted) then
            return
        end if
    end if

    if (isDeleted) then
        if (pos > 0) then
            call removeEntry(pos, node)
        else
            pos = -pos
            call getPredecessor(node%link(pos)%ptr, successorVal)
            node%val(pos) = successorVal
            call deleteValue(successorVal, node%link(pos)%ptr, isDeleted)
            if (.not. isDeleted) then
                return
            end if
        end if
    end if

    isDeleted = .true.
end subroutine deleteValue

! Función para encontrar la posición de un valor en un nodo
integer function findPosition(val, node)
    integer, intent(in) :: val
    type(BTreeNode), intent(in) :: node
    integer :: i

    findPosition = 0
    do i = 1, node%num
        if (val == node%val(i)) then
            findPosition = i
            return
        else if (val < node%val(i)) then
            findPosition = -i
            return
        end if
    end do
end function findPosition

! Subrutina para obtener el predecesor de un nodo
subroutine getPredecessor(node, predecessorVal)
    type(BTreeNode), pointer :: node
    integer, intent(out) :: predecessorVal
    do while (associated(node%link(node%num)%ptr))
        node => node%link(node%num)%ptr
    end do
    predecessorVal = node%val(node%num)
end subroutine getPredecessor

! Subrutina para eliminar una entrada de un nodo
subroutine removeEntry(pos, node)
    integer, intent(in) :: pos
    type(BTreeNode), pointer, intent(inout) :: node
    integer :: i
    do i = pos, node%num-1
        node%val(i) = node%val(i+1)
        node%link(i)%ptr => node%link(i+1)%ptr
    end do
    node%link(node%num)%ptr => null()
    node%num = node%num - 1
end subroutine removeEntry

end module arbolb