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
    end type BTreeNode

    
contains

subroutine insert(this,val)
    class(BTreeNode), intent(inout) :: this
    integer, intent(in) :: val
    integer :: i
    type(BTreeNode), pointer :: child
    allocate(child)
    if (setValue(val, i, this%root, child)) then
            this%root => createNode(i, child)
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

function createNode(this,val, child) result(newNode)
    class(BTreeNode), intent(inout) :: this
    integer, intent(in) :: val
    type(BTreeNode), pointer, intent(in) :: child
    type(BTreeNode), pointer :: newNode
    integer :: i
    allocate(newNode)
    newNode%val(1) = val
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

subroutine remove(this, val )
    class(BTreeNode), intent(inout) :: this
    integer , intent(in) :: val
    call deleteValue(this%root, val)
end subroutine remove

recursive subroutine deleteValue(node, val)
    type(BTreeNode), pointer, intent(inout) :: node
    integer, intent(in) :: val
    integer :: i, j
    if (associated(node)) then
            i = 1
            do while (i <= node%num .and. val > node%val(i))
                i = i + 1
            end do
            if (i <= node%num .and. val == node%val(i)) then
                if (.not. associated(node%link(i-1)%ptr)) then
                    do j = i + 1, node%num
                        node%val(j-1) = node%val(j)
                        node%link(j-1)%ptr => node%link(j)%ptr
                    end do
                    node%num = node%num - 1
                else
                    call restore(node, i)
                end if
            else
                call delete(node%link(i-1)%ptr, val)
            end if
    else
            print *, "Client not found"
    end if
end subroutine deleteValue

subroutine restore(myNode, pos)
    type(BTreeNode), pointer, intent(inout) :: myNode
    integer, intent(in) :: pos
    type(BTreeNode), pointer :: q
    q => myNode%link(pos-1)%ptr
    if (q%num > MINI) then
            do while (associated(q%link(q%num)%ptr))
                q => q%link(q%num)%ptr
            end do
            myNode%val(pos) = q%val(q%num)
            q%num = q%num - 1
    else
            call combine(myNode, pos)
    end if
end subroutine restore

subroutine combine(myNode, pos)
    type(BTreeNode), pointer, intent(inout) :: myNode
    integer, intent(in) :: pos
    type(BTreeNode), pointer :: q, r
    integer :: i
    q => myNode%link(pos-1)%ptr
    r => myNode%link(pos)%ptr
    q%num = q%num + 1
    q%val(q%num) = myNode%val(pos)
    q%link(q%num)%ptr => r%link(0)%ptr
    do i = 1, r%num
            q%num = q%num + 1
            q%val(q%num) = r%val(i)
            q%link(q%num)%ptr => r%link(i)%ptr
    end do
    do i = pos, myNode%num-1
            myNode%val(i) = myNode%val(i+1)
            myNode%link(i)%ptr => myNode%link(i+1)%ptr
    end do
    myNode%num = myNode%num - 1
    deallocate(r)
end subroutine combine

subroutine generateDotFile(filename, node)
    character(len=*), intent(in) :: filename
    type(BTreeNode), intent(in) :: node
    integer :: unit, count=0
    ! Abrir el archivo DOT para escribir
    open(newunit=unit, file=filename, status='replace', action='write')

    ! Escribir el encabezado del archivo DOT
    write(unit, '(A)') 'digraph G {'

    ! Llamar a la función de generación del contenido del árbol
    call generateDotContent(node%root,unit,count)

    ! Escribir el cierre del archivo DOT
    write(unit, '(A)') '}'

    ! Cerrar el archivo DOT
    close(unit)

    
    ! Convertir el archivo DOT a SVG
    call execute_command_line('dot -Tsvg ' // trim(filename) // ' > img/btree.svg')
    ! Abrir el archivo SVG en un visor de imágenes
    call execute_command_line('eog img/btree.svg')
end subroutine generateDotFile

recursive subroutine generateDotContent(node,unit, count)
    integer, intent(in) :: unit
    integer, intent(inout) :: count
    type(BTreeNode), pointer, intent(in) :: node
    integer :: i, temp

    ! Si el nodo es nulo, no hay nada que graficar
    if (associated(node)) return
        count = count + 1
        write(unit,'(A,I0,A)') 'node',count,'[label="'
        do i = 1, node%num
            write(unit, '(I0)') node%val(i)
            if (i < node%num) then
                write(unit, '(A)') '|'
            end if
        end do
        write(unit, '(A)') '"];'
    ! Escribir el nodo actual
    temp = count
    ! Llamar a la función recursivamente para los nodos hijos
    do i = 0, node%num
        if (associated(node)) then
            write(unit,'(A,I0,A,I0,A)') 'node', temp, ' -> node', count + 1,';'
            call generateDotContent (node,unit,count)
        end if
    end do
end subroutine generateDotContent


end module arbolb