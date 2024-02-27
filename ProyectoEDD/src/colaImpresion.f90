module colaImpresion
    implicit none
    integer :: ConteoIP = 0
    type, public :: imag
        integer :: id
        character (len=5) :: tipo
        character (len=100) :: nombreCl
    end type imag

    type, public :: nodoCI
        private
        type(imag):: value
        type(nodoCI), pointer :: next => null()
    end type nodoCI

    type, public :: colaCI
    private 
        integer :: llamadas = 0
        type(nodoCI), pointer :: head => null()
        type(nodoCI), pointer :: tail => null()
        contains
        procedure :: append
        procedure :: delete
        procedure :: print
        procedure :: eliminarNodoAntiguo
    end type colaCI

    contains

    subroutine append(this,cantIG,nombrel,tip)
        class(colaCI), intent(inout) :: this
        integer, intent (in) :: cantIG
        character (len=5), intent (in) :: tip
        character (len=100), intent (in) :: nombrel
        type(nodoCI), pointer :: temp
        integer :: i

        do i = 1, cantIG
            
            allocate(temp)
            temp%value%tipo = tip
            temp%value%id = ConteoIP
            temp%value%nombreCl = nombrel
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

        !call this%print(tip)
    end subroutine append

    subroutine delete(this)
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
    end subroutine delete

    subroutine print(this,tipos)
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
    end subroutine print

    subroutine eliminarNodoAntiguo(this)
        class(colaCI), intent(inout) :: this
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
        !call this%print("image")
    end subroutine eliminarNodoAntiguo
end module colaImpresion
