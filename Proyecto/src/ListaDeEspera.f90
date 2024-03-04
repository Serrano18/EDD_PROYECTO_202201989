module ListaDeEspera
    use cliente
    use listaimagenes
    
    implicit none
    type, public :: clienteEspera
         
    integer :: idc=-1, ceimg_g, ceimg_p,idv,pasosre
    character (len=100) :: cenombre
    integer :: igrecibida = 0
    integer :: iprecibida = 0

    end type clienteEspera

    type, public :: nodoclie
        type(clienteEspera) :: value  
        type(ListaICE) :: listimag             
        type(nodoclie), pointer :: next => null()
        type(nodoclie), pointer :: prev => null()
    end type nodoclie

    type, public :: listaespera
        integer :: size = 0
        type(nodoclie), pointer :: head => null()
        type(nodoclie), pointer :: tail => null()
        contains
            procedure :: appendle
            procedure :: deletele
            procedure :: incrementarImagenRecibida
            procedure :: verificarAtendidos
            procedure :: printListaEspera
    end type listaespera


    contains
        subroutine appendle(this, vi, ui,ig,ip,name,igr,ipr,pasor)
            
            class(listaespera), intent(inout) :: this
            integer,intent(in) :: ui,ig,ip,igr,ipr,vi,pasor
            character (len=100), intent (in) ::name
            type(nodoclie), pointer :: temp
            this%size = this%size+1
            !print *, "se agrego "
            allocate(temp)
            temp%value%idc = ui
            temp%value%idv = vi
            temp%value%ceimg_g = ig
            temp%value%ceimg_p = ip
            temp%value%cenombre = name
            temp%value%igrecibida = igr            
            temp%value%iprecibida = ipr
            temp%value%pasosre = pasor
           
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
            !call this%printListaEspera()
        end subroutine appendle

        
    subroutine deletele(this)
        class(listaespera), intent(inout) :: this
        type(nodoclie), pointer :: temp

        if (.not. associated(this%head)) then
            print *, 'Lista de espera vacia'
            return
        end if

        print *, 'Delete ', this%head%value%cenombre! Ejemplo de acceso a datos del cliente
        temp => this%head
        this%head => this%head%next
        if (associated(this%head)) then
            this%head%prev => this%tail
            this%tail%next => this%head
        else
            this%tail => null() ! La lista está vacía
        end if
        deallocate(temp)
    end subroutine deletele

    subroutine incrementarImagenRecibida(this, nombreCliente,idClien, tipoImagen)
        class(listaespera), intent(inout) :: this
        character(len=100), intent(in) :: nombreCliente
        character(len=5), intent(in) :: tipoImagen
        integer, intent(in) :: idClien
        type(nodoclie), pointer :: current
        !call this%printListaEspera()
        current => this%head
        do while (associated(current))
            if (current%value%cenombre == nombreCliente .and. current%value%idc == idClien) then
                if (tipoImagen == "IMG_G") then
                    current%value%igrecibida = current%value%igrecibida + 1
                    call current%listimag%append(nombreCliente,tipoImagen,idClien)

                elseif (tipoImagen == "IMG_P") then
                    current%value%iprecibida = current%value%iprecibida + 1
                    call current%listimag%append(nombreCliente,tipoImagen,idClien)
                end if
                exit ! Salir del bucle una vez que se ha encontrado el cliente
            end if
            current => current%next
        end do
        
    end subroutine incrementarImagenRecibida
    

    subroutine verificarAtendidos(this, listaAtendidos, pasoActual)
        use clienteAtendidos
        class(listaespera), intent(inout) :: this
        class(listaClientesA), intent(inout) :: listaAtendidos
        integer, intent(in) :: pasoActual
        type(nodoclie), pointer :: current, temp 
        integer :: i
        current => this%head
        do i=0, this%size-1
            if (current%value%igrecibida == current%value%ceimg_g .and. &
                current%value%iprecibida == current%value%ceimg_p) then
                ! Eliminar el nodo y enviar a lista de atendidos
                temp => current
                call listaAtendidos%insertarCliente(temp%value%cenombre, &
                    temp%value%idv, temp%value%igrecibida + temp%value%iprecibida, &
                    pasoActual-temp%value%pasosre, temp%value%ceimg_g, temp%value%ceimg_p)
                    write(*, '(A, A, A, A)') "El cliente " ,trim(temp%value%cenombre) ," salio de la lista de espera"
                if (i==0) then
                    this%head => current%next
                    current%next%prev=>this%tail
                    this%tail%next => this%head
                else if (i==this%size-1) then
                    this%head => current%next
                    current%next%prev=>this%tail
                    this%tail%next => this%head
                else if(i/=0 .or. i/=this%size-1) then
                    current%prev%next => current%next
                    current%next%prev => current%prev
                end if
                deallocate(temp)
                this%size = this%size-1
            end if 
            current=> current%next
        end do
    end subroutine verificarAtendidos      

    subroutine printListaEspera(this)
        class(listaespera), intent(in) :: this
        type(nodoclie), pointer :: current
        integer :: i
        current => this%head
        print *, 'Lista de espera:'
        print *, '-------------------------------------'
        do i=0, this%size-1
            if (current%value%idc/=-1) then
                print *, 'ID Cliente: ', current%value%idc
                print *, 'ID Ventanilla: ', current%value%idv
                print *, 'Nombre: ', current%value%cenombre
                print *, 'Imagenes G recibidas: ', current%value%igrecibida
                print *, 'Imagenes P recibidas: ', current%value%iprecibida
                print *, '-------------------------------------'
            end if
            current => current%next
        end do
    end subroutine printListaEspera        
end module ListaDeEspera