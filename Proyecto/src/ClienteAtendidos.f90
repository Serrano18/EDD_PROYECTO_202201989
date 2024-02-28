module clienteAtendidos
    implicit none
    type, public :: ClienteA 
        character(len=100) :: clientName
        integer :: nVentanilla
        integer :: nImagenImpresa
        integer :: cantPasos
    end type ClienteA

    type, public :: nodoCA
    private
        type(ClienteA) :: value
        type(nodoCA), pointer :: next => null()
    end type nodoCA

    type, public :: listaClientesA
        private
        type(nodoCA), pointer :: head => null()
        contains
        procedure :: insertarCliente
        procedure :: printLista

    end type listaClientesA

    contains

    subroutine insertarCliente(this, nombre, ventanilla, numImagenes, totalPasos)
        class(listaClientesA), intent(inout) :: this
        character(len=100), intent(in)::nombre
        integer, intent(in) :: ventanilla,numImagenes,totalPasos
        type(nodoCA), pointer :: current, newClient

        allocate(newClient)
        newClient%value%clientName = nombre
        newClient%value%nVentanilla = ventanilla
        newClient%value%nImagenImpresa = numImagenes
        newClient%value%cantPasos = totalPasos
        newClient%next => null()


        if (.not. associated(this%head)) then
            this%head => newClient
        else
            current => this%head
            do while (associated(current%next))
                current => current%next
            end do
            current%next => newClient
        end if
        call this%printLista()
    end subroutine insertarCliente

    subroutine printLista(this)
        class(listaClientesA), intent(in) :: this
        type(nodoCA), pointer :: current

        current => this%head
        print *, 'Lista de clientes atendidos:'
        print *, '-------------------------------------'
        do while (associated(current))
            print *, 'Nombre: ', current%value%clientName
            print *, 'Ventanilla: ', current%value%nVentanilla
            print *, 'Número de imágenes impresas: ', current%value%nImagenImpresa
            print *, 'Total de pasos en el sistema: ', current%value%cantPasos
            print *, '-------------------------------------'
            current => current%next
        end do
    end subroutine printLista
end module clienteAtendidos