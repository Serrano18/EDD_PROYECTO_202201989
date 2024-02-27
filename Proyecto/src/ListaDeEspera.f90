module ListaDeEspera
    use cliente
    implicit none
    type, public :: clienteEspera
         
        integer :: id, img_g, img_p
        character (len=100) :: nombre
        integer :: igrecibida
        integer :: iprecibida 

    end type clienteEspera

    type, public :: nodoclie
        type(clienteEspera) :: value               
        type(nodoclie), pointer :: next => null()
        type(nodoclie), pointer :: prev => null()
    end type nodoclie

    type, public :: listaespera
        type(nodoclie), pointer :: head => null()
        type(nodoclie), pointer :: tail => null()
        contains
            procedure :: appendle
    end type listaespera


    contains
        subroutine appendle(this,  ui,ig,ip,name,igr,ipr)
            class(listaespera), intent(inout) :: this
            integer,intent(in) :: ui,ig,ip,igr,ipr
            character (len=100), intent (in) ::name
            type(nodoclie), pointer :: temp

            allocate(temp)
            temp%value%id = ui
            temp%value%img_g = ig
            temp%value%img_p = ip
            temp%value%nombre = name
            temp%value%igrecibida = igr            
            temp%value%iprecibida = ipr
            if (.not. associated(this%head)) then
                this%head => temp
                this%tail => temp
                temp%next => temp
                temp%prev => temp
            else
                this%tail%next => temp
                temp%prev => this%tail
                temp%next => this%head
                this%head%prev => temp
                this%tail => temp
            end if

        end subroutine appendle

        
    subroutine delete(this)
        class(listaespera), intent(inout) :: this
        type(nodoclie), pointer :: temp

        if (.not. associated(this%head)) then
            print *, 'Lista de espera vacía'
            return
        end if

        print *, 'Delete ', this%head%value%nombre ! Ejemplo de acceso a datos del cliente
        temp => this%head
        this%head => this%head%next
        if (associated(this%head)) then
            this%head%prev => this%tail
            this%tail%next => this%head
        else
            this%tail => null() ! La lista está vacía
        end if
        deallocate(temp)
    end subroutine delete
            
end module ListaDeEspera