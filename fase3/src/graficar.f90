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

end module graficar