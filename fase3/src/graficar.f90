module graficar
    use rutas, only:lista_rutas,ruta,nodor => nodoruta
    use resultados, only:listares,resultado
    implicit none
    type :: nodog
        integer :: id
        type(lista_rutas) :: rutas
        type(nodog),pointer :: next => null()
    end type nodog

    type :: grafica
        integer :: size
        type(nodog),pointer :: head => null()
        contains
        procedure :: insertarg
        procedure :: agregarnodo
        procedure :: agregarruta
        procedure :: obtenernodo
        procedure :: show
        procedure :: rutamascorta
    end type grafica
    contains

        subroutine insertarg(this)
            class(grafica) :: this
            type(nodog),pointer :: aux
            type(ruta) :: rut

            aux => this%obtenernodo(rut%sucursal1)

            if ( .not. associated(aux) ) then
                call this%agregarnodo(rut%sucursal1)
                aux => this%obtenernodo(rut%sucursal1)
                call this%agregarruta(rut, aux)
            else
                call this%agregarruta(rut, aux)
            end if
        end subroutine insertarg

        subroutine agregarnodo (this,id)
            class(grafica), intent(inout) :: this
            integer, intent(in) :: id
            type(nodog), pointer :: temp
            allocate(temp)
            temp%id = id
            if ( .not. associated(this%head) ) then
                this%head => temp
            else
                temp%next => this%head
                this%head => temp
            end if
        end subroutine agregarnodo

        subroutine agregarruta (this,rut,padre)
            class(grafica), intent(inout) :: this
            type(ruta), intent(in) :: rut
            type(nodog), pointer :: padre
            type(nodog), pointer :: temp
            type(ruta) ::tmpruta
            tmpruta%distancia = rut%distancia
            tmpruta%sucursal1 = rut%sucursal2
            tmpruta%sucursal2 = padre%id
            tmpruta%impresoras = rut%impresoras
            tmpruta%weight = rut%weight
            temp => this%obtenernodo(tmpruta%sucursal1)
        
            if ( .not. associated(temp) ) then
                call this%agregarnodo(tmpruta%sucursal1)
            end if
    
            call padre%rutas%addr(tmpruta, .true.)
    
            this%size = this%size + 1
        end subroutine agregarruta

        function obtenernodo(this,id) result(nodoObtenido)
            class(grafica), intent(in) :: this
            integer, intent(in) :: id
            type(nodog), pointer :: temp, nodoObtenido
            temp => this%head
            do while ( associated(temp) )
                if ( temp%id == id ) then
                    nodoObtenido => temp
                    return
                end if
                temp => temp%next
            end do
            nodoObtenido => null()
        end function obtenernodo

        
    subroutine show(this)
        class(grafica), intent(in) :: this
        type(nodog), pointer :: current
        type(nodor), pointer :: current_route

        current => this%head

        do while ( associated(current) )
            write(*,*) 'Node: ', current%id
            current_route => current%rutas%head
            do while ( associated(current_route) )
                write(*,*) 'Route: ', current_route%oruta%sucursal1, ' -> ', current_route%oruta%sucursal2, &
                ' = ', current_route%oruta%weight
                current_route => current_route%next
            end do
            current => current%next
        end do
    end subroutine show

    function rutamascorta(this, origin, destination) result(res)
        class(grafica), intent(in) :: this
        integer, intent(in) :: origin, destination
        type(listares), pointer :: res
        type(lista_rutas), pointer :: queue
        type(nodog), pointer :: current_node
        type(nodor), pointer :: current_rnode
        type(ruta) :: tmp
        integer :: sub_total = 0

        call tmp%inicializarruta()

        tmp%sucursal1 = origin
        tmp%weight = 0
        allocate(res)
        allocate(queue)
        res%total_weightr = 0

        current_node => this%obtenernodo(origin)

        if ( associated(current_node) ) then
            call queue%fusionar(current_node%rutas)
            call res%insertarResultado(tmp)
        end if

        do while( .not. queue%estavacia() )
            current_rnode => queue%remove()
            sub_total = current_rnode%oruta%weight
            current_node => this%obtenernodo(current_rnode%oruta%sucursal1)

            if ( .not. associated(current_node) ) then
                print *, 'El nodo no existe'
                exit
            end if

            if ( current_node%id == destination ) then
                print *, 'El destino no es posible'
                tmp = current_rnode%oruta
                call res%insertarResultado(tmp)
                exit
            end if

            call current_node%rutas%actualizar_weight(sub_total)
            call queue%fusionar(current_node%rutas)
            tmp = current_rnode%oruta
            call res%insertarResultado(tmp)

            current_node => current_node%next
        end do

    end function rutamascorta
    
    subroutine ruta_optima(self, origen, destino, arbol, merkle_t)
        class(ListaAdyacencia), intent(inout) :: self
        integer, intent(in) :: origen
        integer, intent(in) :: destino
        integer :: total
        class(ABB), intent(inout) :: arbol
        class(merkle), intent(inout) :: merkle_t
    
        if (ruta_max_impresoras(self, origen, destino, .false., arbol, merkle_t)&
        > ruta_corta_distancia(self, origen, destino, .false., arbol, merkle_t)) then
            print *, 'La ruta óptima es la que maximiza el número de impresoras'
            total =ruta_max_impresoras(self, origen, destino, .true.,arbol, merkle_t)
        else
            print *, 'La ruta óptima es la que minimiza la distancia recorrida'
            total = ruta_corta_distancia(self, origen, destino, .true., arbol, merkle_t)
        end if
    end subroutine ruta_optima
    
    function ruta_max_impresoras(self, origen, destino, optima, arbol, merkle_t) result(ganancia_neta)
        class(ListaAdyacencia), intent(inout) :: self
        integer, intent(in) :: origen
        integer, intent(in) :: destino
        logical, intent(in) :: optima
        class(ABB), intent(inout) :: arbol
        class(merkle), intent(inout) :: merkle_t
    
        type(v_nodo), pointer :: actual
        type(a_nodo), pointer :: aux
        integer, dimension(:), allocatable :: num_impresorass
        integer, dimension(:), allocatable :: ruta_critica
        logical, dimension(:), allocatable :: visitados
        integer :: i, j, u, max_dist
        integer :: suma_total
        type(BlockData) :: block
    
        integer :: ganancia_neta
    
        ! Inicialización
        ganancia_neta = 0
        allocate(num_impresorass(0:100)) ! Ajusta el tamaño máximo según tus necesidades 
        allocate(visitados(0:100))
        allocate(ruta_critica(100)) ! Tamaño inicial del array ruta_critica
        actual => self%raiz
        do while (associated(actual))
            num_impresorass(actual%val) = -HUGE(0) ! Inicializa todas las num_impresorass a menos infinito 
            visitados(actual%val) = .false. ! Ningún nodo ha sido visitado
            actual => actual%sig
        end do
        num_impresorass(origen) = 0 ! La num_impresoras al nodo de origen es 0
    
        ! Algoritmo modificado de Dijkstra para maximizar la ruta
        do i = 1, 100 ! Ajusta el límite superior según tus necesidades
            max_dist = -HUGE(0)
            u = -1
            ! Encuentra el nodo con la num_impresoras máxima no visitada
            do j = 1, 100
                if (.not. visitados(j) .and. num_impresorass(j) > max_dist) then
                    max_dist = num_impresorass(j)
                    u = j
                end if
            end do
            if (u == -1) exit ! Salir si todos los nodos están visitados
            visitados(u) = .true.
    
            ! Actualiza las num_impresorass de los nodos adyacentes al nodo actual
            actual => self%raiz
            do while (associated(actual))
                if (actual%val == u) then
                    aux => actual%raiz
                    do while (associated(aux))
                        if (.not. visitados(aux%destino) .and. num_impresorass(u)&
                        + aux%num_impresoras > num_impresorass(aux%destino)) then
                            num_impresorass(aux%destino) = num_impresorass(u) + aux%num_impresoras
                        end if
                        aux => aux%sig
                    end do
                    exit
                end if
                actual => actual%sig
            end do
        end do
    
        ! Calcula la suma total de las num_impresorass
        suma_total = num_impresorass(destino)
    
        ! Imprime la ruta crítica
        print *, 'La ruta crítica es:'
        u = destino
        i = 0
        do while (u /= origen)
            i = i + 1
            ruta_critica(i) = u
            actual => self%raiz
            do while (associated(actual))
                if (actual%val == u) then
                    aux => actual%raiz
                    do while (associated(aux))
                        if (num_impresorass(aux%destino) == num_impresorass(u) - aux%num_impresoras) then
                            u = aux%destino
                            print *, 'Origen ', actual%val, 'Destino ', aux%destino, 'Perdidas: ',&
                            70*aux%distancia, 'Ganancias ', 100*aux%num_impresoras
                            ganancia_neta = ganancia_neta + 100*aux%num_impresoras - 70*aux%distancia
                            if (optima .eqv. .true.) then
                                print *, "aqui agrega al blockchain"
                                block%origin_id =actual%val 
                                block%origin_address = arbol%buscar_direccion(actual%val)
                                block%destination_id = aux%destino
                                block%destination_address = arbol%buscar_direccion(aux%destino)
                                block%total_cost = (100*aux%num_impresoras - 70*aux%distancia)
                                call merkle_t%agregar(block)
                            end if
                        end if
                        aux => aux%sig
                    end do
                    exit
                end if
                actual => actual%sig
            end do
        end do
        i = i + 1
        ruta_critica(i) = origen
    
        ! Imprime la ruta crítica en orden inverso
        do j = i, 1, -1
            print *, 'Sucursal ', ruta_critica(j), ' con ', num_impresorass(ruta_critica(j)), ' impresoras'
        end do
    
        ! Imprime la suma total de las num_impresorass
        print *, 'La suma total de las num_impresorass desde el nodo ', origen, ' al nodo ', destino, ' es: ', suma_total
        print *, 'La ganancia neta es: ', ganancia_neta
        
        deallocate(num_impresorass)
        deallocate(visitados)
        deallocate(ruta_critica)
    end function ruta_max_impresoras
    
    
    function ruta_corta_distancia(self, origen, destino, optima, arbol, merkle_t) result(ganancia_neta)
        class(grafica), intent(inout) :: self
        integer, intent(in) :: origen
        integer, intent(in) :: destino
        logical, intent(in) :: optima
        class(sucursalesavl), intent(inout) :: arbol
        class(Arbolmerkle), intent(inout) :: merkle_t
        type(block) :: bloc
    
        type(v_nodo), pointer :: actual
        type(a_nodo), pointer :: aux
        integer, dimension(:), allocatable :: distancias
        integer, dimension(:), allocatable :: ruta_critica
        logical, dimension(:), allocatable :: visitados
        integer :: i, j, u, min_dist
        integer :: suma_total
        integer :: ganancia_neta
    
        ! Inicialización
        ganancia_neta = 0
        allocate(distancias(0:100)) ! Ajusta el tamaño máximo según tus necesidades
        allocate(visitados(0:100))
        allocate(ruta_critica(100)) ! Tamaño inicial del array ruta_critica
        actual => self%raiz
        do while (associated(actual))
            distancias(actual%val) = HUGE(0) ! Inicializa todas las distancias a infinito
            visitados(actual%val) = .false. ! Ningún nodo ha sido visitado
            actual => actual%sig
        end do
        distancias(origen) = 0 ! La distancia al nodo de origen es 0
    
        ! Algoritmo de Dijkstra
        do i = 1, 100 ! Ajusta el límite superior según tus necesidades
            min_dist = HUGE(0)
            u = -1
            ! Encuentra el nodo con la distancia mínima no visitada
            do j = 1, 100
                if (.not. visitados(j) .and. distancias(j) < min_dist) then
                    min_dist = distancias(j)
                    u = j
                end if
            end do
            if (u == -1) exit ! Salir si todos los nodos están visitados
            visitados(u) = .true.
    
            ! Actualiza las distancias de los nodos adyacentes al nodo actual
            actual => self%raiz
            do while (associated(actual))
                if (actual%val == u) then
                    aux => actual%raiz
                    do while (associated(aux))
                        if (.not. visitados(aux%destino) .and. distancias(u) + aux%distancia < distancias(aux%destino)) then
                            distancias(aux%destino) = distancias(u) + aux%distancia
                        end if
                        aux => aux%sig
                    end do
                    exit
                end if
                actual => actual%sig
            end do
        end do
    
        ! Calcula la suma total de las distancias
        suma_total = distancias(destino)
    
        ! Imprime la ruta crítica
        print *, 'La ruta crítica es:'
        u = destino
        i = 0
        do while (u /= origen)
            i = i + 1
            ruta_critica(i) = u
            actual => self%raiz
            do while (associated(actual))
                if (actual%val == u) then
                    aux => actual%raiz
                    do while (associated(aux))
                        if (distancias(aux%destino) == distancias(u) - aux%distancia) then
                            u = aux%destino
                            print *, 'Origen ', actual%val, 'Destino ', aux%destino, 'Perdidas: ', &
                            70*aux%distancia, 'Ganancias ', 100*aux%num_impresoras
                            ganancia_neta = ganancia_neta + 100*aux%num_impresoras - 70*aux%distancia
                            if (optima .eqv. .true.) then
                                print *, "aqui agrega al blockchain"
                                block%origin_id =actual%val 
                                block%origin_address = arbol%buscar_direccion(actual%val)
                                block%destination_id = aux%destino
                                block%destination_address = arbol%buscar_direccion(aux%destino)
                                block%total_cost = (100*aux%num_impresoras - 70*aux%distancia)
                                call merkle_t%agregar(block)
                                ! Aquí se agrega al blockchain
                            end if
                            exit
                        end if
                        aux => aux%sig
                    end do
                    exit
                end if
                actual => actual%sig
            end do
        end do
        i = i + 1
        ruta_critica(i) = origen
    
        ! Imprime la ruta crítica en orden inverso
        do j = i, 1, -1
            print *, 'Sucursal ', ruta_critica(j), ' a una distancia de ', distancias(ruta_critica(j)), ' kilometros'
            
        end do
    
        ! Imprime la suma total de las distancias
        print *, 'La suma total de las distancias desde el nodo ', origen, ' al nodo ', destino, ' es: ', suma_total
        print *, 'La ganancia neta es: ', ganancia_neta
        
        deallocate(distancias)
        deallocate(visitados)
        deallocate(ruta_critica)
    end function ruta_corta_distancia

end module graficar