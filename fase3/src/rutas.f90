module rutas
    implicit none
    type :: ruta
        integer :: sucursal1=-1,sucursal2=-1,distancia=-1
        integer :: impresoras=-1,weight=-1
    contains
        procedure :: set_sucursal1
        procedure :: set_sucursal2
        procedure :: set_distancia
        procedure :: set_impresoras
        procedure :: set_weight
        procedure :: imprimir_ruta
        procedure :: inicializarruta
    end type ruta

    type :: nodoruta
        type(ruta) :: oruta
        type(nodoruta), pointer :: next => null()
        type(nodoruta), pointer :: prev => null()
    end type nodoruta
    type :: lista_rutas
        type(nodoruta), pointer :: head => null()
        type(nodoruta), pointer :: tail => null()
        integer :: size = 0
        contains
            procedure :: addr
            procedure :: remove
            procedure :: actualizar_weight
            procedure :: estavacia
            procedure :: fusionar
    end type lista_rutas

    contains
    subroutine inicializarruta(this)
        class(ruta), intent(inout) :: this
        this%sucursal1 = -1
        this%sucursal2 = -1
        this%distancia = -1
        this%impresoras = -1
        this%weight = -1
    end subroutine inicializarruta
    
    subroutine addr(this, oruta, sort)
        class(lista_rutas), intent(inout) :: this
        type(ruta), intent(in) :: oruta
        logical, intent(in) :: sort
        type(nodoruta), pointer :: new_node, current, prev

        allocate(new_node)

        new_node%oruta = oruta

        if ( .not. associated(this%head) ) then
            this%head => new_node
            this%tail => new_node
        else
            current => this%head
            prev => null()

            if ( sort ) then
                do while ( associated( current ))
                    if ( current%oruta%sucursal1 > oruta%sucursal1 ) then
                        exit
                    end if
                    prev => current
                    current => current%next
                end do
            else
                do while ( associated( current ))
                    if ( current%oruta%weight > oruta%weight ) then
                        exit
                    end if
                    prev => current
                    current => current%next
                end do
            end if


            if ( .not. associated( prev ) ) then
                new_node%next => this%head
                this%head%prev => new_node
                this%head => new_node
            else if ( .not. associated( current ) ) then
                this%tail%next => new_node
                new_node%prev => this%tail
                this%tail => new_node
            else
                prev%next => new_node
                new_node%prev => prev
                new_node%next => current
                current%prev => new_node
            end if
        end if

        this%size = this%size + 1
    end subroutine addr

    function remove(this) result(oruta)
        class(lista_rutas), intent(inout) :: this
        type(nodoruta), pointer :: oruta

        if ( .not. associated( this%head ) ) then
            oruta => null()
            return
        end if

        oruta => this%head
        this%head => this%head%next
        if ( associated( this%head ) ) then
            this%head%prev => null()
        else
            this%tail => null()
        end if

        this%size = this%size - 1
    end function remove

    function estavacia(this) result(res)
        class(lista_rutas), intent(in) :: this
        logical :: res

        res = (.not. associated( this%head ))
    end function estavacia

    subroutine fusionar(this, list)
        class(lista_rutas), intent(inout) :: this
        class(lista_rutas), intent(in) :: list
        type(nodoruta), pointer :: current

        current => list%head

        do while ( associated( current ) )
            call this%addr( current%oruta, .false. )
            current => current%next
        end do
    end subroutine fusionar

    subroutine actualizar_weight(this, weight)
        class(lista_rutas), intent(inout) :: this
        integer, intent(in) :: weight
        type(nodoruta), pointer :: current

        current => this%head

        do while ( associated( current ) )
            current%oruta%weight = current%oruta%weight + weight
            current => current%next
        end do
    end subroutine actualizar_weight

        subroutine set_sucursal1(this, sucursal1)
            class(ruta), intent(inout) :: this
            integer, intent(in) :: sucursal1
            this%sucursal1 = sucursal1
        end subroutine set_sucursal1

        subroutine set_sucursal2(this, sucursal2)
            class(ruta), intent(inout) :: this
            integer, intent(in) :: sucursal2
            this%sucursal2 = sucursal2
        end subroutine set_sucursal2

        subroutine set_distancia(this, distancia)
            class(ruta), intent(inout) :: this
            integer, intent(in) :: distancia
            this%distancia = distancia
        end subroutine set_distancia

        subroutine set_impresoras(this, impresoras)
            class(ruta), intent(inout) :: this
            integer, intent(in) :: impresoras
            this%impresoras = impresoras
        end subroutine set_impresoras

        subroutine set_weight(this, weight)
            class(ruta), intent(inout) :: this
            integer, intent(in) :: weight
            this%weight = weight
        end subroutine set_weight
        subroutine imprimir_ruta(this)
            class(ruta), intent(in) :: this
            print *, "Sucursal 1: ", this%sucursal1
            print *, "Sucursal 2: ", this%sucursal2
            print *, "Distancia: ", this%distancia
            print *, "Impresoras: ", this%impresoras
        end subroutine imprimir_ruta
end module rutas