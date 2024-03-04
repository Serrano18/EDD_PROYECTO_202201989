module listaVentanilla

    
    
    use pilaImagenes
    use Cliente
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
        procedure :: graficarventanillas
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
            write(*,'(A,I0)')  "Ventanilla ",current%value%idve
            if (current%value%clienteActual%id/=-1) then
                write(*,'(A,A)') "Cliente: ",trim(current%value%clienteActual%nombre)
            else
                print *, "Cliente: Vacio"
            end if
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
        use pilaImagenes
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
            print *, 'Error: No se encontro la ventana con el ID especificado.'
        end if
    end subroutine AgregarImagenAPila

    subroutine agregarClienteActual(this, idVent, clien,pa)
        use Cliente
        class(listaVentanas), intent(inout) :: this
        integer, intent(in) ::idVent,pa
        type(clientes), intent(inout) :: clien
        type(nodoV), pointer :: current
       
        current => this%head
        do while (associated(current) .and. current%value%idve /= idVent)
            current => current%next
        end do
        if (associated(current)) then
            clien%pasoInicio = pa
            current%value%clienteActual = clien
            
            if (clien%id/=-1) then
                write(*,'(A, A, A, I0)') "EL CLIENTE ", trim(clien%nombre), " INGRESA A VENTANILLA ", idVent
            else
                print *, "Ya no hay clientes"
            end if
        else
            print *, 'Error: No se encontro la ventana con el ID especificado.'
        end if
        call this%Vprint()
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
        use colaImpresion
        use ListaDeEspera
        use Cliente
        class(listaVentanas), intent(inout) :: this
        type(colaCI), intent(inout) :: colaImagenG, colaImagenP     
        type(listaespera), intent(inout) :: espera  
        type(nodoV), pointer :: current
        integer :: suma_img 
        current => this%head
        do while (associated(current))
            if (current%value%confirmacion .and. current%value%clienteActual%id/=-1) then
                suma_img = current%value%clienteActual%img_g + current%value%clienteActual%img_p
                if (suma_img /= current%value%contimg) then
                    current%value%contimg = current%value%contimg + 1
                    write(*, '(A, I0, A)') "La ventanilla ", current%value%idve, " recibe una imagen."
                    call this%AgregarImagenAPila( current%value%idve, &
                     this%numImagenestot, current%value%clienteActual%img_g, &
                      current%value%clienteActual%img_p, &
                      current%value%clienteActual%nombre, &
                      current%value%clienteActual%id)
                    
                    this%numImagenestot = this%numImagenestot + 1
                else
                    !Aqui es cuando el ventanilla ya recibio todas las imagenes y las envia a las colas
                    !Aqui se mandan las imagenes a la cola
                    call colaImagenG%appendcl(current%value%clienteActual%img_g, current%value%clienteActual%nombre, &
                    "IMG_G", current%value%clienteActual%id)
                    call colaImagenP%appendcl(current%value%clienteActual%img_p, &
                    current%value%clienteActual%nombre, "IMG_P", & 
                    current%value%clienteActual%id)
                    current%value%confirmacion = .false.
                    !Aqui se manda el cliente a la lista de espera
                    call espera%appendle(current%value%idve,&
                    current%value%clienteActual%id, &
                    current%value%clienteActual%img_g, &
                    current%value%clienteActual%img_p, &
                    current%value%clienteActual%nombre,0,0,&
                    current%value%clienteActual%pasoInicio)
                    !call espera%printListaEspera()      
                    this%numImagenestot = 0
                    write(*, '(A, A, A)') "El cliente ", trim(current%value%clienteActual%nombre), " pasa a la lista de espera"
                    write(*, '(A, I0, A)') "La ventanlla ", current%value%idve, " envia imagenes a la cola de Impresion"
                    call current%value%pilaImagens%VaciarPila()

                end if
            end if
            current => current%next
        end do
    end subroutine ConteoImgagen
    

    subroutine graficarventanillas(this,filename)
        class(listaVentanas), intent(in) :: this
        character(len=*),intent(in) :: filename
        integer :: unit
        type(nodoV),pointer :: current
        integer :: count

        open(unit, file=filename, status='replace')
        write(unit, *) 'digraph pilasImagenes {'
        write(unit, *) '    node [shape=box, style=filled];' ! Aplicar atributos a todos los nodos
        write(unit, *) 'rankdir = LR;'
        
        current => this%head
        count = 0
        do while (associated(current))
            count = count + 1
            write(unit, *) '     "Ventanilla', count, '" [label="Ventanilla ', & 
            current%value%idve, '\nCantidad de imágenes: ', &
             current%value%contimg, '", shape=box];'
            if (current%value%contimg > 0) then
                write(unit, '(A, I0)') '     subgraph cluster_', count, ' {'
                write(unit, *) '        label="Pila de Imágenes";'
                write(unit, *) '        color=blue;'
                call current%value%pilaImagens%graficarpilaimagen(unit)
                write(unit, *) '     }'
            end if
            if (associated(current%next)) then
                write(unit, *) '    "Ventanilla', count, '" -> "Ventanilla', count+1, '";'
            end if
            current => current%next
        end do 

        ! Cerrar el archivo DOT
        write(unit, *) '}'
        close(unit)
        ! Generar el archivo PNG utilizando Graphviz
        call system('dot -Tpng ' // trim(filename) // ' -o ' // trim(adjustl(filename)) // '.png')
        
        print *, 'Graphviz file generated: ', trim(adjustl(filename)) // '.png'

    end subroutine graficarventanillas
end module listaVentanilla