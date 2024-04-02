module Imagens
    use abbidm   
    implicit none
    type :: Imagen
    
        integer :: id
        type(abbid) :: arbolIdCapas
        type(linkedlist) :: capa
        contains
        procedure :: agregarcapa
        procedure :: eliminarCapa
        procedure :: setIdImg
        procedure :: getId
        procedure :: agregarNodoIdCapas
        procedure :: infoImagen
    end type Imagen
    contains
    
    subroutine infoImagen(this)
        class(Imagen), intent(in) :: this
        print *, "--------------------------------------"
        write (*, '(A, I0)') " ID: ", this%id
        write(*, '(A)')  " Capas: "
        call this%capa%print()
        print *, ''
        print *, "-------------------------------------"
    end subroutine infoImagen

    subroutine agregarcapa(this, id)
        class(Imagen), intent(inout) :: this
        integer :: id
        call this%capa%addlist(id)
    end subroutine agregarcapa
    

    subroutine eliminarCapa(this, id)
        class(Imagen), intent(inout) :: this
        integer :: id
        call this%capa%removelist(id)
    end subroutine eliminarCapa

    subroutine agregarNodoIdCapas(this, value)
        class(Imagen), intent(inout) :: this
        integer, intent(in) :: value
        call this%arbolIdCapas%add(value)
    end subroutine agregarNodoIdCapas

    subroutine setIdImg(this, newId)
        class(Imagen), intent(inout) :: this
        integer, intent(in) :: newId
        this%id = newId
    end subroutine setIdImg

    function getId(this) result(id)
        class(Imagen), intent(in) :: this
        integer :: id
        id = this%id
    end function getId

end module Imagens