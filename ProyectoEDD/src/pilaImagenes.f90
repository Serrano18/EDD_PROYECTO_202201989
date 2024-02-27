module pilaImagenes
    implicit none
    type, public :: Imagen
        integer :: id, img_g, img_p
        character :: nombreC
        integer :: vent
    end type Imagen       
    type, public :: nodoI
        type(Imagen) :: valor
        type(nodoI),pointer :: next => null()
    end type nodoI
    type, public :: pilaImagen
        type(nodoI), pointer :: head => null()
        contains
        procedure :: Push
        procedure :: Pop
        procedure :: Iprint

    end type pilaImagen     
    contains
    subroutine Push(this, imag)
        class(pilaImagen), intent(inout) :: this
        type(Imagen), intent(in) :: imag

        type(nodoI), pointer :: temp
        allocate(temp)
        temp%valor = imag
        temp%next => this%head
        this%head => temp
    end subroutine Push

    subroutine Pop(this)
        class(pilaImagen), intent(inout) :: this

        type(nodoI), pointer :: temp
        if (associated(this%head)) then
            temp => this%head
            this%head => this%head%next
            deallocate(temp)
        end if
    end subroutine Pop

    subroutine Iprint(this)
        class(pilaImagen), intent(in) :: this

        type(nodoI), pointer :: current
        current => this%head
        print *, '|-----------------------------------------------|'
        print *, '|              Contenido de la Pila             |'
        print *, '|-----------------------------------------------|'
        do while (associated(current))
            print *, 'ID:', current%valor%id
            current => current%next
        end do
    end subroutine Iprint

    subroutine VaciarPila(this)
        class(pilaImagen), intent(inout) :: this
        type(nodoI), pointer :: current, nextNode
        current => this%head
        do while (associated(current))
            nextNode => current%next
            deallocate(current)
            current => nextNode
        end do
        this%head => null()
    end subroutine VaciarPila
end module pilaImagenes