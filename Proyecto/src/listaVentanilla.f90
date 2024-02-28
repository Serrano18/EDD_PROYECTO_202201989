module listaVentanilla

    use pilaImagenes
    use Cliente
    use colaImpresion
    use ListaDeEspera
    implicit none
    type, public :: Ventana 
        integer :: idve
        integer contimg 
        type(pilaImagen):: pilaImagens
        type(clientes) :: clienteActual
        logical :: confirmacion = .false.
        
    end type Ventana
    type, public :: nodoV
    private
        type(Ventana) :: value
        type(nodoV), pointer :: next => null()
    end type nodoV
    
    type, public :: listaVentanas
        integer :: numImagenestot
        type(nodoV), pointer :: head => null()
        contains
        procedure :: Vinsertar
        procedure :: Vprint
        procedure :: Vordenar
        procedure :: ObtenerVentanillaSinConfirmar
        procedure :: agregarClienteActual
        procedure :: AgregarImagenAPila
        procedure :: ConteoImgagen
    end type listaVentanas
    
    type(Imagen) :: nuevaImagen
    contains
    subroutine Vinsertar(this, cant)
        class(listaVentanas), intent(inout) :: this
        integer, intent(in) :: cant
        
        type(nodoV), pointer :: temp
        integer :: i 
        do i=1, cant
            allocate(temp)
            temp%value%idve = i
            temp%value%contimg = 0
            temp%next => null()
            if (.not. associated(this%head)) then
                this%head => temp
            else
                temp%next => this%head
                this%head => temp
            end if
        end do  
        call this%Vordenar()
    end subroutine Vinsertar

    subroutine Vprint(this)
        class(listaVentanas), intent(in) :: this
        type(nodoV), pointer :: current
        current => this%head
        print *, '|-------------------------------------------------------------------------|'
        print *, '|                          Listado de Ventanillas:                           |'
        print *, '|-------------------------------------------------------------------------|'
        do while (associated(current))
            write(*,*)  "Ventanilla ",current%value%idve
            current => current%next
        end do 
    end subroutine Vprint

    subroutine Vordenar(this)
        class(listaVentanas), intent(inout) :: this
        type(nodoV), pointer :: current, nextNode
        integer :: tempId
        current => this%head
        do while (associated(current))
            nextNode => current%next
            do while (associated(nextNode))
                if (current%value%idve > nextNode%value%idve) then
                    ! Intercambiar los valores de id
                    tempId = current%value%idve
                    current%value%idve = nextNode%value%idve
                    nextNode%value%idve = tempId
                end if
                nextNode => nextNode%next
            end do
            current => current%next
        end do
        call this%Vprint()

    end subroutine Vordenar

    ! Método para agregar una imagen a la pila de imágenes de la ventana en uso
    subroutine AgregarImagenAPila(this, idVentana, idImagen, img_g, img_p, nombreC,idcliente)
        class(listaVentanas), intent(inout) :: this
        integer, intent(in) :: idVentana, idImagen, img_g, img_p,idcliente
        character, intent(in) :: nombreC
        type(nodoV), pointer :: current
        current => this%head
        do while (associated(current) .and. current%value%idve /= idVentana)
            current => current%next
        end do
        if (associated(current)) then
            nuevaImagen%vent = idVentana
            nuevaImagen%id = idImagen
            nuevaImagen%img_g = img_g
            nuevaImagen%img_p = img_p
            nuevaImagen%nombreC = nombreC
            nuevaImagen%idclient = idcliente
            call current%value%pilaImagens%Push(nuevaImagen)
        else
            print *, 'Error: No se encontró la ventana con el ID especificado.'
        end if
    end subroutine AgregarImagenAPila

    subroutine agregarClienteActual(this, idVent, clien)
        class(listaVentanas), intent(inout) :: this
        integer, intent(in) ::idVent
        type(clientes), intent(in) :: clien
        type(nodoV), pointer :: current

        current => this%head
        do while (associated(current) .and. current%value%idve /= idVent)
            current => current%next
        end do
        if (associated(current)) then
            current%value%clienteActual = clien
            write(*,'(A, I0, A, I0)') "EL CLIENTE ", clien%id, " INGRESA A VENTANILLA ", idVent
        else
            print *, 'Error: No se encontró la ventana con el ID especificado.'
        end if
    end subroutine agregarClienteActual

    function ObtenerVentanillaSinConfirmar(this) result(idVentanilla)
        class(listaVentanas), intent(inout) :: this
        integer :: idVentanilla
        type(nodoV), pointer :: current
        logical :: ventanaEncontrada
    
        current => this%head
        ventanaEncontrada = .false.
        do while (associated(current) .and. .not. ventanaEncontrada)
            if (.not. current%value%confirmacion) then
                current%value%confirmacion = .true.
                idVentanilla = current%value%idve
                ventanaEncontrada = .true.
            else
                current => current%next
            end if
        end do
    
        if (.not. ventanaEncontrada) then
            print *, 'No hay ventanillas Disponibles'
            idVentanilla = -1
        end if
    end function ObtenerVentanillaSinConfirmar
    
    subroutine ConteoImgagen(this,colaImagenG,colaImagenP,espera)
        class(listaVentanas), intent(inout) :: this
        type(colaCI), intent(inout) :: colaImagenG, colaImagenP     
        type(listaespera), intent(inout) :: espera  
        type(nodoV), pointer :: current
        integer :: suma_img 
        current => this%head
        do while (associated(current))
            if (current%value%confirmacion) then
                suma_img = current%value%clienteActual%img_g + current%value%clienteActual%img_p
                if (suma_img /= current%value%contimg) then
                    write(*, '(A, I0, A)') "La ventanilla ", current%value%idve, " recibe una imagen."
                    call this%AgregarImagenAPila( current%value%idve, &
                     this%numImagenestot, current%value%clienteActual%img_g, &
                      current%value%clienteActual%img_p, &
                      current%value%clienteActual%nombre, &
                      current%value%clienteActual%id)
                    current%value%contimg = current%value%contimg + 1
                    this%numImagenestot = this%numImagenestot + 1
                else
                    !Aqui es cuando el ventanilla ya recibio todas las imagenes y las envia a las colas
                    !Aqui se mandan las imagenes a la cola
                    call colaImagenG%appendcl(current%value%clienteActual%img_g, &
                    current%value%clienteActual%nombre, "IMG_G", & 
                    current%value%clienteActual%id)
                    call colaImagenP%appendcl(current%value%clienteActual%img_p, &
                    current%value%clienteActual%nombre, "IMG_P", & 
                    current%value%clienteActual%id)
                    current%value%confirmacion = .false.
                    !Aqui se manda el cliente a la lista de espera
                    call espera%appendle(current%value%idve,&
                    current%value%clienteActual%id, &
                    current%value%clienteActual%img_g, &
                    current%value%clienteActual%img_p, &
                    current%value%clienteActual%nombre, 0,0)
                    !call espera%printListaEspera()
                    !write(*, '(A, A, A)')  "El clinte " , current%value%clienteActual%nombre ," pasa a la lista de espera"
                    write(*, '(A, A, A, A)') "El cliente ", trim(current%value%clienteActual%nombre), " pasa a la lista de espera"
                    write(*, '(A, I0, A)') "La ventanlla ", current%value%idve, " envia imagenes a la cola de Impresion"

                end if
            end if
            current => current%next
        end do
    end subroutine ConteoImgagen
    
end module listaVentanilla