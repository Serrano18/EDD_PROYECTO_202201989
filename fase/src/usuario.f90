module usuario
    implicit none
    type, public :: User 

        character(len=100) :: nombre   
        character(len=15) :: dpi, password
    end type User

    type, public :: nodoU
    private
        type(user) :: value
        type(nodoU), pointer :: next => null()
    end type nodoU

    type, public :: listaUsuarios
        private
        type(nodoU), pointer :: head => null()
        contains
        procedure :: Agregar
        procedure  :: Eliminar
        procedure :: ActualizarDatos
        procedure :: Imprimir
        procedure :: IniciarSesion

    end type listaUsuarios

    contains
    subroutine Agregar (this, name, cui, pass)
        class(listaUsuarios), intent(inout) :: this
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: cui
        character(len=*), intent(in) :: pass
        type(nodoU), pointer :: current
        logical :: existe
        
        ! Verificar si el usuario ya existe
        current => this%head
        existe = .false.
        do while (associated(current))
            if (current%value%dpi == cui) then
                existe = .true.
                exit
            end if
            current => current%next
        end do
        
        if (existe) then
            !Usuario existente
            write(*, '(A,A,A)') "El usuario con DPI ", trim(cui)," y nombre ", trim(name), " ya existe."
        else
            ! Agregar nuevo usuario
            allocate(current)
            current%value%nombre = name
            current%value%dpi = cui
            current%value%password = pass
            current%next => this%head
            this%head => current
            write(*, '(A,A,A)') "El usuario con DPI ", trim(cui)," y nombre ", trim(name), " ha sido registrado exitosamente."
        end if
    end subroutine Agregar

    subroutine Eliminar (this,cui)
        class(listaUsuarios), intent(inout) :: this
        character(len=*), intent(in) :: cui
        type(nodoU), pointer :: current, previous
        current => this%head
        previous => null()
        do while (associated(current))
            if (current%value%dpi == cui) then
                if (associated(previous)) then
                    previous%next => current%next
                else
                    this%head => current%next
                end if
                deallocate(current)
                exit
            end if
            previous => current
            current => current%next
        end do
    end subroutine Eliminar

    subroutine ActualizarDatos (this, name, cui, pass)
        class(listaUsuarios), intent(inout) :: this 
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: cui, pass
        type(nodoU), pointer :: current
        
        current => this%head
        do while (associated(current))
            if (current%value%dpi == cui) then
                current%value%nombre = name
                current%value%password = pass
                exit
            end if
            current => current%next
        end do
    end subroutine ActualizarDatos

    subroutine Imprimir (this)
        class(listaUsuarios), intent(inout) :: this
        type(nodoU), pointer :: current
    
        current => this%head
        do while (associated(current))
            write(*, '(A, A, A)') "Nombre: ", trim(current%value%nombre), ", DPI: ", trim(current%value%dpi)
            current => current%next
        end do
    end subroutine Imprimir

    function IniciarSesion(this, name, pass) result(esValido)
        class(listaUsuarios), intent(in) :: this
        character(len=*), intent(in) :: name, pass
        integer :: esValido
        type(nodoU), pointer :: current
        ! Verificar si los datos corresponden al usuario "admin"
        if (trim(name) == "admin" .and. trim(pass) == "EDD2024") then
            ! Usuario es admin, redirigir a un menú de administrador
            esValido = 1
        else
            ! Verificar si los datos corresponden a un usuario normal
            current => this%head
            do while (associated(current))
                if (current%value%nombre == name .and. current%value%password == pass) then
                    ! Usuario es válido, redirigir a un menú de usuario normal
                    esValido = -1
                    exit
                end if
                current => current%next
            end do
            if (esValido /= 1 .and. esValido /= -1) then
                esValido = 0
            end if

        end if
    end function IniciarSesion

end module usuario