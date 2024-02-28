module listaimagenes
    type, public :: ile
        integer :: idClientes
        character (len=5) :: tipo
        character (len=100) :: nombreCl
    end type ile
    
    type, public :: nodoILE
        
        type(ile):: value
        type(nodoILE), pointer :: next => null()
    end type nodoILE

    type, public :: ListaICE
    
        integer :: llamadas = 0
        type(nodoILE), pointer :: head => null()
        type(nodoILE), pointer :: tail => null()
        contains
        procedure :: append
        procedure :: delete
        procedure :: print
    end type ListaICE
    contains
    subroutine append(this,nombrel,tip,idClies)
        class(ListaICE), intent(inout) :: this
        integer, intent (in) :: idClies
        character (len=5), intent (in) :: tip
        character (len=100), intent (in) :: nombrel
        type(nodoILE), pointer :: temp
            allocate(temp)
            temp%value%tipo = tip
            temp%value%idClientes = idClies
            temp%value%nombreCl = nombrel
            temp%next => null()
            if (.not. associated(this%head)) then
                this%head => temp
                this%tail => temp
            else
                this%tail%next => temp
                this%tail => temp
            end if
        call this%print(tip)
    end subroutine append

    subroutine delete(this)
        class(ListaICE), intent(inout) :: this
        type(nodoILE), pointer :: temp

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
        class(ListaICE), intent(in) :: this
        type(nodoILE), pointer :: current
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

end module listaimagenes