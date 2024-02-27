module clienteAtendidos
    implicit none
    type, public :: ClienteA 
        character :: clientName
        integer :: nVentanilla
        integer :: nImagenImpresa
        integer :: cantPasos
    end type ClienteA

    type, public :: nodoCA
    private
        type(ClienteA) :: value
        type(nodoCA), pointer :: next => null()
        type(nodoCA), pointer :: after => null()
    end type nodoCA

    type, public :: listaVentanas
        private
        type(nodoCA), pointer :: head => null()
        contains
        !procedure :: Vinsertar
        !procedure :: Vprint
        !procedure :: Vordenar
        !procedure :: Veliminar
        !procedure :: Vdisponible
        !procedure :: GraficarV
    end type listaVentanas

    contains
end module clienteAtendidos