module usuario
    use arbolbb
    use arbolavl
    implicit none
    type, public :: User
        character(len=100) :: nombre   
        integer (kind=8):: dpi
        character (len=20) :: password
        type(abb) :: arbolDeCapas
        type(avl) :: arbolDeImagenes
        contains
        procedure :: setNombre
        procedure :: getNombre
        procedure :: setDpi
        procedure :: getDpi
        procedure :: setPassword
        procedure :: getPassword
        procedure :: imprimirDatos
    end type User

    contains
        function createUser(nombre, dpi, password) result(cliente)
            character(len=*), intent(in) :: nombre
            integer(kind=8), intent(in) :: dpi
            character(len=*), intent(in) :: password
            type(User) :: cliente
        
            cliente%nombre = nombre
            cliente%dpi = dpi
            cliente%password = password
        end function createUser
        
        subroutine setNombre(this, nombre)
            class(User), intent(inout) :: this
            character(len=*), intent(in) :: nombre
            this%nombre = nombre
        end subroutine setNombre

        function getNombre(this) result(nombre)
            class(User), intent(in) :: this
            character(len=100) :: nombre
            nombre = this%nombre
        end function getNombre

        subroutine setDpi(this, dpi)
            class(User), intent(inout) :: this
            integer (kind=8), intent(in) :: dpi
            this%dpi = dpi
        end subroutine setDpi

        function getDpi(this) result(dpi)
            class(User), intent(in) :: this
            integer (kind=8) :: dpi
            dpi = this%dpi
        end function getDpi

        subroutine setPassword(this, password)
            class(User), intent(inout) :: this
            character (len=*), intent(in) :: password
            this%password = password
        end subroutine setPassword

        function getPassword(this) result(password)
            class(User), intent(in) :: this
            character (len=20) :: password
            password = this%password
        end function getPassword

        subroutine imprimirDatos(this)
            class(User), intent(in) :: this
            print *, "Nombre: ", this%nombre
            print *, "DPI: ", this%dpi
            print *, "Password: ", this%password
        end subroutine imprimirDatos

end module usuario