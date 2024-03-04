module clienteAtendidos
    implicit none
    type, public :: ClienteA 
        character(len=100) :: clientName
        integer :: nVentanilla = -1
        integer :: nImagenImpresa
        integer :: cantPasos
        integer :: cant_imgG
        integer :: cant_imgP
    end type ClienteA

    type, public :: nodoCA
    private
        type(ClienteA) :: value
        type(nodoCA), pointer :: next => null()
    end type nodoCA

    type, public :: listaClientesA
        private
        type(nodoCA), pointer :: head => null()
        contains
        procedure :: insertarCliente
        procedure :: printLista
        procedure :: graficaratendidos
        procedure :: ordenarPorImgP
        procedure :: ordenarPorImgG
        procedure :: clienteConMasPasos
        procedure :: graficarClienteMasPasos
        procedure :: copia
        procedure :: graficarCliente
        procedure :: graficarTop
        procedure :: imprimirListaNombres


    end type listaClientesA

    contains

    subroutine insertarCliente(this, nombre, ventanilla, numImagenes, totalPasos,cantG,cantP)
        class(listaClientesA), intent(inout) :: this
        character(len=100), intent(in)::nombre
        integer, intent(in) :: ventanilla,numImagenes,totalPasos,cantG,cantP
        type(nodoCA), pointer :: current, newClient

        allocate(newClient)
        newClient%value%clientName = nombre
        newClient%value%nVentanilla = ventanilla
        newClient%value%nImagenImpresa = numImagenes
        newClient%value%cantPasos = totalPasos
        newClient%value%cant_imgG = cantG
        newClient%value%cant_imgP = cantP
        newClient%next => null()


        if (.not. associated(this%head)) then
            this%head => newClient
        else
            current => this%head
            do while (associated(current%next))
                current => current%next
            end do
            current%next => newClient
        end if
        !call this%printLista()
    end subroutine insertarCliente

    subroutine printLista(this)
        class(listaClientesA), intent(in) :: this
        type(nodoCA), pointer :: current

        current => this%head
        print *, 'Lista de clientes atendidos:'
        print *, '-------------------------------------'
        do while (associated(current))
            print *, 'Nombre: ', current%value%clientName
            print *, 'Ventanilla: ', current%value%nVentanilla
            print *, 'IMG_G: ',current%value%cant_imgG
            print *, 'IMG_P: ', current%value%cant_imgP
            print *, 'Número de imágenes impresas: ', current%value%nImagenImpresa
            print *, 'Total de pasos en el sistema: ', current%value%cantPasos
            print *, '-------------------------------------'
            current => current%next
        end do
    end subroutine printLista

    subroutine graficaratendidos(this,filename)
        class(listaClientesA),intent(in) :: this
        character(len=*) :: filename
        integer :: unit
        type(nodoCA),pointer :: current
        integer :: count
    
        open(unit,file = filename,status='replace')
        write(unit, *) 'digraph colac {'
        write(unit, *) '    node [shape=box, style=filled];' ! Aplicar atributos a todos los nodos
        write(unit,*) 'rankdir = LR;'
        ! Escribir nodos y conexiones
        current => this%head
        count = 0
        do while (associated(current))
            count = count + 1
            write(unit, *) '     "Node', count, '" [label="', trim(current%value%clientName), '\nN#_IMG: ', &
            current%value%nImagenImpresa, '\nIMG_G: ', &
            current%value%cant_imgG,'\nIMG_P: ', &
            current%value%cant_imgP,'\nN#Ventanilla:', current%value%nVentanilla, &
            '\n N#Pasos:', current%value%cantPasos,'", shape=box];'
            if (associated(current%next)) then
                write(unit, *) '    "Node', count, '" -> "Node', count+1, '";'
            end if
            current => current%next
        end do 
    
        ! Cerrar el archivo DOT
        write(unit, *) '}'
        close(unit)
            ! Generar el archivo PNG utilizando Graphviz
        call system('dot -Tpng ' // trim(filename) // ' -o ' // trim(adjustl(filename)) // '.png')
        
        print *, 'Graphviz file generated: ', trim(adjustl(filename)) // '.png'
    end subroutine graficaratendidos

    subroutine graficarTop(this,filename)
        class(listaClientesA),intent(in) :: this
        character(len=*) :: filename
        integer :: unit
        type(nodoCA),pointer :: current
        integer :: count,maxClientes
    
        open(unit,file = filename,status='replace')
        write(unit, *) 'digraph colac {'
        write(unit, *) '    node [shape=box, style=filled];' ! Aplicar atributos a todos los nodos
        write(unit,*) 'rankdir = LR;'
        ! Escribir nodos y conexiones
        current => this%head
        count = 0
        maxClientes = 5
        do while (associated(current) .and. count<maxClientes )
            count = count + 1
            write(unit, *) '     "Node', count, '" [label="', trim(current%value%clientName),'\nIMG_G: ', &
            current%value%cant_imgG, '\nIMG_P: ', &
            current%value%cant_imgP, '\nN#_IMG: ', &
            current%value%nImagenImpresa, '\nN#Ventanilla:', current%value%nVentanilla, &
            '\n N#Pasos:', current%value%cantPasos,'", shape=box];'
            if (associated(current%next)) then
                write(unit, *) '    "Node', count, '" -> "Node', count+1, '";'
            end if
            current => current%next
        end do 
    
        ! Cerrar el archivo DOT
        write(unit, *) '}'
        close(unit)
            ! Generar el archivo PNG utilizando Graphviz
        call system('dot -Tpng ' // trim(filename) // ' -o ' // trim(adjustl(filename)) // '.png')
        
        print *, 'Graphviz file generated: ', trim(adjustl(filename)) // '.png'

    end subroutine graficarTop

    subroutine ordenarPorImgG(this)
        class(listaClientesA), intent(inout) :: this
        type(nodoCA), pointer :: current, nextNode
        type(ClienteA) :: tempCliente
        logical :: swapped
    
        current => this%head
        swapped = .true.
        do while (associated(current) .and. swapped)
            swapped = .false.
            nextNode => current%next
            do while (associated(nextNode))
                if (current%value%nVentanilla/=-1) then
                    if (current%value%cant_imgG < nextNode%value%cant_imgG) then
                        ! Intercambiar los clientes
                        tempCliente = current%value
                        current%value = nextNode%value
                        nextNode%value = tempCliente
                        swapped = .true.
                    end if
                end if
                nextNode => nextNode%next
            end do
            current => current%next
        end do
    end subroutine ordenarPorImgG
    
    subroutine ordenarPorImgP(this)
        class(listaClientesA), intent(inout) :: this
        type(nodoCA), pointer :: current, nextNode
        type(ClienteA) :: tempCliente
        logical :: swapped
    
        current => this%head
        swapped = .true.
        do while (associated(current) .and. swapped)
            swapped = .false.
            nextNode => current%next
            do while (associated(nextNode))
                if (current%value%nVentanilla /=-1) then
                    if (current%value%cant_imgP > nextNode%value%cant_imgP) then
                        ! Intercambiar los clientes
                        tempCliente = current%value
                        current%value = nextNode%value
                        nextNode%value = tempCliente
                        swapped = .true.
                    end if
                end if
                nextNode => nextNode%next
            end do
            current => current%next
        end do
    end subroutine ordenarPorImgP
    
    subroutine clienteConMasPasos(this, clienteMasPasos)
        class(listaClientesA), intent(in) :: this
        type(ClienteA), intent(out) :: clienteMasPasos
        type(nodoCA), pointer :: current
        integer :: maxPasos
    
        current => this%head
        maxPasos = 0
        do while (associated(current))
            if (current%value%nVentanilla/=-1) then
                if (current%value%cantPasos > maxPasos) then
                    maxPasos = current%value%cantPasos
                    clienteMasPasos = current%value
                end if
            end if
            current => current%next
        end do
    end subroutine clienteConMasPasos
    
    subroutine graficarClienteMasPasos(this, filename)
        class(listaClientesA), intent(in) :: this
        character(len=*) :: filename
        integer :: unit
        type(ClienteA) :: clienteMasPasos
    
        ! Obtener al cliente con más pasos
        call this%clienteConMasPasos(clienteMasPasos)
    
        ! Graficar solo al cliente con más pasos
        open(unit, file=filename, status='replace')
        write(unit, *) 'digraph colac {'
        write(unit, *) '    node [shape=box, style=filled];' ! Aplicar atributos a todos los nodos
        write(unit, *) '    "ClienteMasPasos" [label="', trim(clienteMasPasos%clientName), '\nN#_IMG: ', &
            clienteMasPasos%nImagenImpresa, '\nN#Ventanilla:', clienteMasPasos%nVentanilla, &
            '\nN#Pasos:', clienteMasPasos%cantPasos, '", shape=box];'
        write(unit, *) '}'
        close(unit)
    
        ! Generar el archivo PNG utilizando Graphviz
        call system('dot -Tpng ' // trim(filename) // ' -o ' // trim(adjustl(filename)) // '.png')
        print *, 'Graphviz file generated: ', trim(adjustl(filename)) // '.png'
    end subroutine graficarClienteMasPasos
    
    subroutine imprimirListaNombres(this)
        class(listaClientesA), intent(in) :: this
        type(nodoCA), pointer :: current
    
        current => this%head
        print *, 'Lista de nombres de clientes atendidos:'
        print *, '-------------------------------------'
        do while (associated(current))
            if (current%value%nVentanilla /= -1) then
                print *, current%value%clientName
            end if
            current => current%next
        end do
    end subroutine imprimirListaNombres
    
    subroutine graficarCliente(this, nombre, filename)
        class(listaClientesA),intent(in) :: this
        character(len=100) :: nombre
        character(len=*) :: filename
        type(nodoCA), pointer :: current
        logical :: clienteEncontrado
        integer :: count,unit
        
        open(unit,file = filename,status='replace')
        write(unit, *) 'digraph colac {'
        write(unit, *) '    node [shape=box, style=filled];' ! Aplicar atributos a todos los nodos
        write(unit,*) 'rankdir = LR;'
        
        current => this%head
        count = 0
        clienteEncontrado = .false.
        do while (associated(current))
            count = count + 1
            if (trim(current%value%clientName) == trim(nombre)) then
                write(unit, *) '     "Node', count, '" [label="', trim(current%value%clientName), '\nIMG_T: ', &
                    current%value%nImagenImpresa, '\nVentanilla:', current%value%nVentanilla, &
                    '\n Pasos:', current%value%cantPasos,'\nImg_G:', current%value%cant_imgG, &
                    '\nImg_P:', current%value%cant_imgP, '", shape=box];'
                clienteEncontrado = .true.
                exit
            end if
            current => current%next
        end do 
        
        if (.not. clienteEncontrado) then
            print *, 'El cliente "', nombre, '" no se encuentra en la lista de clientes atendidos.'
        end if
        
        ! Cerrar el archivo DOT
        write(unit, *) '}'
        close(unit)
        
        ! Generar el archivo PNG utilizando Graphviz si se encontró el cliente
        if (clienteEncontrado) then
            call system('dot -Tpng ' // trim(filename) // ' -o ' // trim(adjustl(filename)) // '.png')
            print *, 'Graphviz file generated: ', trim(adjustl(filename)) // '.png'
        end if
    end subroutine graficarCliente
    
    subroutine copia(this,copy)
        class(listaClientesA), intent(in) :: this
        class(listaClientesA), intent(out) :: copy
        type(nodoCA), pointer :: nodo, newnodo, lastnodo
        nodo => this%head
        lastnodo => null()
        do while(associated(nodo))
            allocate(newnodo)
            newnodo%value%clientName = nodo%value%clientName
            newnodo%value%nVentanilla = nodo%value%nVentanilla
            newnodo%value%cantPasos = nodo%value%cantPasos
            newnodo%value%nImagenImpresa = nodo%value%nImagenImpresa
            newnodo%value%cant_imgG = nodo%value%cant_imgG
            newnodo%value%cant_imgP = nodo%value%cant_imgP
            newnodo%next => null()
    
            if(.not. associated(copy%head)) then
                copy%head => newnodo
                lastnodo => newnodo
            else
                lastnodo%next => newnodo
                lastnodo => newnodo
    
            end if
            nodo => nodo%next
        end do
    end subroutine copia
end module clienteAtendidos