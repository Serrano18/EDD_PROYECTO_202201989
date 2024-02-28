module colaImpresion
    use ListaDeEspera
    implicit none
    integer :: ConteoIP = 0
    type, public :: imag
        integer :: id
        character (len=5) :: tipo
        character (len=100) :: nombreCl
        integer :: idClie
    end type imag

    type, public :: nodoCI
        type(imag):: value
        type(nodoCI), pointer :: next => null()
    end type nodoCI

    type, public :: colaCI
        integer :: llamadas = 0
        type(nodoCI), pointer :: head => null()
        type(nodoCI), pointer :: tail => null()
        contains
        procedure :: appendcl
        procedure :: deletecl
        procedure :: printcl
        procedure :: eliminarNodoAntiguo
    end type colaCI

    contains

    subroutine appendcl(this,cantIG,nombrel,tip,idCle)
        class(colaCI), intent(inout) :: this
        integer, intent (in) :: cantIG,idCle
        character (len=5), intent (in) :: tip
        character (len=100), intent (in) :: nombrel
        type(nodoCI), pointer :: temp
        integer :: i

        do i = 1, cantIG
            
            allocate(temp)
            temp%value%tipo = tip
            temp%value%id = ConteoIP
            temp%value%nombreCl = nombrel
            temp%value%idClie = idCle
            ConteoIP = ConteoIP + 1
            temp%next => null()
            if (.not. associated(this%head)) then
                this%head => temp
                this%tail => temp
            else
                this%tail%next => temp
                this%tail => temp
            end if
        end do

        !call this%printcl(tip)
    end subroutine appendcl

    subroutine deletecl(this)
        class(colaCI), intent(inout) :: this
        type(nodoCI), pointer :: temp

        if (.not. associated(this%head)) then
            print *, 'Cola Imagenes Pequeñas Vacia'
            return
        end if

        print *, 'Delete ', this%head%value
        temp => this%head
        this%head => this%head%next
        deallocate(temp)
    end subroutine deletecl

    subroutine printcl(this,tipos)
        class(colaCI), intent(in) :: this
        type(nodoCI), pointer :: current
         character (len=5):: tipos
        current => this%head

        print *, '|--------------------------------------------------------|'
        print *, '|                   Listado de ',tipos,"                     |"
        print *, '|--------------------------------------------------------|'
        do while (associated(current))
            write(*,*) current%value%nombreCl
            current => current%next
        end do 
    end subroutine printcl

    subroutine eliminarNodoAntiguo(this,esperal)
        class(colaCI), intent(inout) :: this
        type (listaespera), intent(inout) :: esperal
        type(nodoCI), pointer :: current, previous, temp
        integer :: pasos
        if (.not. associated(this%head)) then
            !print *, 'La cola está vacía.'
            return
        end if

        this%llamadas = this%llamadas + 1
        current => this%head
        previous => null()
        do while (associated(current))
            if (current%value%tipo ==  "IMG_G") then
                pasos = this%llamadas
                if (pasos == 2) then
                    write(*, '(A, A, A, A)') "Se entrego una imagen al cliente ", trim(current%value%nombreCl)  
                    call esperal%incrementarImagenRecibida(current%value%nombreCl, &
                    current%value%idClie,&
                    current%value%tipo)
                    ! Eliminar el nodo
                    if (associated(previous)) then
                        previous%next => current%next
                    else
                        this%head => current%next
                    end if
                    temp => current
                    current => current%next
                    deallocate(temp)
                    this%llamadas = 0
                    
                else
                    exit ! No es necesario seguir buscando
                end if
            elseif (current%value%tipo  == "IMG_P") then
                if (this%llamadas == 1) then
                    write(*, '(A, A, A, A)') "Se entrego una imagen al cliente ", trim(current%value%nombreCl) 
                    call esperal%incrementarImagenRecibida(current%value%nombreCl, &
                    current%value%idClie,&
                    current%value%tipo)
                    ! Eliminar el nodo
                    if (associated(previous)) then
                        previous%next => current%next
                    else
                        this%head => current%next
                    end if
                    temp => current
                    current => current%next
                    deallocate(temp)
                    this%llamadas = 0
                   
                else
                    exit ! No es necesario seguir buscando
                end if
            else
                previous => current
                current => current%next
            end if
        end do
        !call this%printcl("image")
    end subroutine eliminarNodoAntiguo
end module colaImpresion
