module listadoImagenes
    use Imagens, only:Imagen
    implicit none

    type :: nodoImagen
        type(Imagen) :: imag
        type(nodoImagen), pointer :: next => null()
    end type nodoImagen

    type :: linkedlistImagen
        integer :: size = 0
        type(nodoImagen), pointer :: head => null()
        type(nodoImagen), pointer :: tail => null()
    contains
        procedure :: addlistimag
        procedure :: printimag
        procedure :: clearimag
        procedure :: iniciarimag
        procedure :: ordenarporcapa
    end type linkedlistImagen
contains
        subroutine iniciarimag(this)
            class(linkedlistImagen), intent(inout) :: this
            this%size = 0
            this%head => null()
            this%tail => null()
        end subroutine iniciarimag

    subroutine addlistimag(this, val)
        class(linkedlistImagen), intent(inout) :: this
        class(Imagen), intent(in) :: val
        type(nodoImagen), pointer :: new_node

        if ( associated(this%head) ) then
            new_node => this%head
            do while (associated(new_node%next))
                new_node => new_node%next
            end do
            allocate(new_node%next)
            new_node => new_node%next
        else
            allocate(this%head)
            new_node => this%head
        end if
        new_node%imag = val
        this%size = this%size + 1
    end subroutine addlistimag
    
    subroutine ordenarporcapa(this)
        class(linkedlistImagen), intent(inout) :: this
        type(nodoImagen), pointer :: tempNode, nextNode
        type(Imagen) :: temp
        integer :: i, j

        tempNode => this%head
        do i = 1, this%size
            nextNode => tempNode%next
            do j = i + 1, this%size
                if (tempNode%imag%capa%size < nextNode%imag%capa%size) then
                    temp = tempNode%imag
                    tempNode%imag = nextNode%imag
                    nextNode%imag = temp
                end if
                nextNode => nextNode%next
            end do
            tempNode => tempNode%next
        end do
    end subroutine ordenarporcapa

    subroutine printimag(this)
        class(linkedlistImagen), intent(in) :: this
        type(nodoImagen), pointer :: tempNode
        integer :: contador 
        tempNode => this%head
        contador = 0
        do while (associated(tempNode).and. contador < 5)
            call tempNode%imag%infoImagen
            tempNode => tempNode%next
            contador = contador + 1
        end do
    end subroutine printimag

    subroutine clearimag(this)
        class(linkedlistImagen), intent(inout) :: this
        type(nodoImagen), pointer :: tempNode, nextNode

        tempNode => this%head
        do while (associated(tempNode))
            nextNode => tempNode%next
            deallocate(tempNode)
            tempNode => nextNode
        end do
        this%head => null()
        this%tail => null()
        this%size = 0
    end subroutine clearimag

end module listadoImagenes