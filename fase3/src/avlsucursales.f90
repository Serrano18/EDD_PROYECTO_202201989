module avlsucursales
    use tecnicoshash, only: tecnicos_hash
    implicit none

    type :: sucursal
        integer :: id = -1
        character(:),allocatable :: departamento,direccion,contrasena
        type(tecnicos_hash) :: tecnicos
        integer :: impresoras = -1
        contains
        procedure :: set_id
        procedure :: set_departamento
        procedure :: set_direccion
        procedure   :: set_contrasena
        procedure :: imprimir_sucursal
        !procedure :: inicializar_tecnicos
        procedure :: inicializar_sucursal
        
    end type sucursal
    type :: nodo
        type(sucursal) :: tsucursal
        integer :: id1, height
        type(nodo), pointer :: left => null()
        type(nodo),pointer:: right => null()
        end type nodo
    type :: sucursalesavl
        integer :: uid = 1
        integer :: num = 0
        type(nodo), pointer :: root => null()
        contains
        procedure :: preorder
        procedure :: inorder
        procedure :: postorder
        procedure :: add
        procedure :: add_rec
        procedure :: srl
        procedure :: srr
        procedure :: drl
        procedure :: drr
        procedure :: getheight
        procedure :: getmax
        procedure :: dotgen
        procedure :: dotgen_rec
        procedure :: amplitud
        procedure :: amplitud_rec
        procedure :: eliminar
        procedure :: eliminar_rec
        procedure :: buscar
        procedure :: buscar_rec
    end type sucursalesavl

    contains
    subroutine eliminar(this, id)
        class(sucursalesavl), intent(inout) :: this
        integer, intent(in) :: id
        type(sucursal) :: stemp
        call stemp%set_id(id)
        call this%eliminar_rec(stemp, this%root)
    end subroutine eliminar

    subroutine eliminar_rec(this, stemp, ntemp)
        class(sucursalesavl), intent(inout) :: this
        type(sucursal), intent(in) :: stemp
        type(nodo), pointer, intent(inout) :: ntemp
        type(nodo), pointer :: aux
        integer :: r, l, m

        if ( .not. associated(ntemp) ) then
            write(*,'(A,I0,A)') 'La sucursal ', stemp%id, ' no existe'
            return
        end if

        if ( stemp%id < ntemp%tsucursal%id ) then
            call this%eliminar_rec(stemp, ntemp%left)
        else if ( stemp%id > ntemp%tsucursal%id ) then
            call this%eliminar_rec(stemp, ntemp%right)
        else
            if ( associated(ntemp%left) .and. associated(ntemp%right) ) then
                aux => ntemp%left
                do while ( associated(aux%right) )
                    aux => aux%right
                end do
                ntemp%tsucursal = aux%tsucursal
                call this%eliminar_rec(aux%tsucursal, ntemp%left)
            else
                if ( associated(ntemp%right) ) then
                    ntemp => ntemp%right
                else
                    ntemp => ntemp%left
                end if

                write(*,'(A,I0,A)') 'Sucursal ', stemp%id, ' eliminada'
                this%num = this%num - 1
            end if
        end if

        if ( associated(ntemp) ) then
            r = this%getheight(ntemp%right)
            l = this%getheight(ntemp%left)
            m = this%getmax(r, l)
            ntemp%height = m + 1

            if ( (r-l) == 2 ) then
                if ( this%getheight(ntemp%right%left) > this%getheight(ntemp%right%right) ) then
                    ntemp => this%drr(ntemp)
                else
                    ntemp => this%srr(ntemp)
                end if
            else if ( (l-r) == 2 ) then
                if ( this%getheight(ntemp%left%right) > this%getheight(ntemp%left%left) ) then
                    ntemp => this%drl(ntemp)
                else
                    ntemp => this%srl(ntemp)
                end if
            end if
        end if
    end subroutine eliminar_rec

    subroutine amplitud(this)
        class(sucursalesavl), intent(in) :: this
        integer :: amp

        do amp = 0, this%getheight(this%root)
            call this%amplitud_rec(this%root, amp)
        end do
    end subroutine amplitud

    subroutine amplitud_rec(this,ntemp, amp)
        class(sucursalesavl), intent(in) :: this
        type(nodo), pointer, intent(in) :: ntemp
        integer, intent(in) :: amp

        if ( .not. associated(ntemp) ) then
            return
        end if

        if ( amp == 0 ) then
            write(*,'(A,I0,A)') 'Sucursal  ', ntemp%tsucursal%id
        else
            call this%amplitud_rec(ntemp%left, amp-1)
            call this%amplitud_rec(ntemp%right, amp-1)
        end if
    end subroutine amplitud_rec

    subroutine add(this, temp)
        class(sucursalesavl), intent(inout) :: this
        type(sucursal), intent(in) :: temp
        type(nodo), pointer :: tmp

        if ( associated(this%buscar(temp%id)) ) then
            write(*,'(A,I0,A)') 'La sucursal con el id ', temp%id, ' ya existe'
        else
            if ( associated(this%root) ) then
                call this%add_rec(temp, this%root)
            else
                allocate(tmp)
                tmp%tsucursal = temp
                tmp%height = 0
                tmp%id1 = this%uid
                this%uid = this%uid + 1
                this%num = this%num + 1
                this%root => tmp
            end if
        end if
    end subroutine add

    subroutine add_rec(this, temp, tmp)
        class(sucursalesavl), intent(inout) :: this
        type(sucursal), intent(in) :: temp
        type(nodo), pointer :: tmp
        integer :: r, l, m

        if ( .not. associated(tmp) ) then
            allocate(tmp)
            tmp%tsucursal = temp
            tmp%height = 0
            tmp%id1 = this%uid
            this%uid = this%uid + 1
            this%num = this%num + 1
        else if( temp%id < tmp%tsucursal%id ) then
            call this%add_rec(temp, tmp%left)
            if ( (this%getheight(tmp%left) - this%getheight(tmp%right)) == 2 ) then
                if ( temp%id < tmp%left%tsucursal%id ) then
                    tmp => this%srl(tmp)
                else
                    tmp => this%drl(tmp)
                end if
            end if
        else
            call this%add_rec(temp, tmp%right)
            if ( (this%getheight(tmp%right) - this%getheight(tmp%left)) == 2 ) then
                if ( temp%id > tmp%right%tsucursal%id ) then
                    tmp => this%srr(tmp)
                else
                    tmp => this%drr(tmp)
                end if
            end if
        end if

        r = this%getheight(tmp%right)
        l = this%getheight(tmp%left)
        m = this%getmax(r, l)
        tmp%height = m + 1
    end subroutine add_rec

    function buscar(this, id2) result (temp)
        class(sucursalesavl), intent(in) :: this
        integer, intent(in) :: id2
        type(sucursal), pointer :: temp
        type(sucursal) :: sucur
        call sucur%set_id(id2)
        temp => this%buscar_rec(sucur,this%root)
    end function buscar

    recursive function buscar_rec(this, stemp, ntemp) result (resulta)
        class(sucursalesavl), intent(in) :: this
        type(sucursal), intent(in) :: stemp
        type(nodo), pointer,intent(in) :: ntemp
        type(sucursal), pointer :: resulta
        if ( .not. associated(ntemp) ) then
            resulta => null()
        else if ( stemp%id < ntemp%tsucursal%id ) then
            resulta => this%buscar_rec(stemp, ntemp%left)
        else if ( stemp%id > ntemp%tsucursal%id ) then
            resulta => this%buscar_rec(stemp, ntemp%right)
        else
            resulta => ntemp%tsucursal
        end if
    end function buscar_rec
subroutine dotgen(this)
        class(sucursalesavl), intent(in) :: this
        integer :: unit

        open(unit, file="img/avl_branches.dot", status="replace")
        write(unit, '(A)') 'graph AVL_Branches {'
        call this%dotgen_rec(this%root, unit)
        write(unit, '(A)') '}'
        close(unit)

        call execute_command_line('dot -Tsvg img/avl_branches.dot -o img/avl_branches.svg')
        call execute_command_line('eog img/avl_branches.svg')
    end subroutine dotgen

    subroutine dotgen_rec(this, tmp, unit)
        class(sucursalesavl), intent(in) :: this
        type(nodo), pointer, intent(in) :: tmp
        integer, intent(in) :: unit

        if ( .not. associated(tmp) ) then
            return
        end if

        write (unit, '(A,I5,A,I5,A)') ' ', tmp%id1, ' [label="', tmp%tsucursal%id, '"];'
        if ( associated(tmp%left) ) then
            write (unit, '(A,I5,A,I5,A)') ' ', tmp%id1, ' -- ', tmp%left%id1, ';'
        end if
        if ( associated(tmp%right) ) then
            write (unit, '(A,I5,A,I5,A)') ' ', tmp%id1, ' -- ', tmp%right%id1, ';'
        end if
        call this%dotgen_rec(tmp%left, unit)
        call this%dotgen_rec(tmp%right, unit)
    end subroutine dotgen_rec
    
    integer function getmax(this, val1, val2)
        class(sucursalesavl), intent(in) :: this
        integer, intent(in) :: val1, val2

        getmax = merge(val1, val2, val1 > val2)
    end function getmax

    integer function getheight(this, tmp)
        class(sucursalesavl), intent(in) :: this
        type(nodo), pointer, intent(in) :: tmp

        if ( .not. associated(tmp) ) then
            getheight = -1
        else
            getheight = tmp%height
        end if
    end function getheight

    function srl(this, tmp) result(res)
        class(sucursalesavl), intent(in) :: this
        type(nodo), pointer, intent(in) :: tmp
        type(nodo), pointer :: res

        res => tmp%left
        tmp%left => res%right
        res%right => tmp
        tmp%height = this%getmax(this%getheight(tmp%left), this%getheight(tmp%right)) + 1
        res%height = this%getmax(this%getheight(res%left), tmp%height) + 1
    end function srl

    function srr(this, tmp) result(res)
        class(sucursalesavl), intent(in) :: this
        type(nodo), pointer, intent(in) :: tmp
        type(nodo), pointer :: res

        res => tmp%right
        tmp%right => res%left
        res%left => tmp
        tmp%height = this%getmax(this%getheight(tmp%left), this%getheight(tmp%right)) + 1
        res%height = this%getmax(this%getheight(res%right), tmp%height) + 1
    end function srr

    function drl(this, tmp) result(res)
        class(sucursalesavl), intent(in) :: this
        type(nodo), pointer, intent(in) :: tmp
        type(nodo), pointer :: res

        tmp%left => this%srr(tmp%left)
        res => this%srl(tmp)
    end function drl

    function drr(this, tmp) result(res)
        class(sucursalesavl), intent(in) :: this
        type(nodo), pointer, intent(in) :: tmp
        type(nodo), pointer :: res

        tmp%right => this%srl(tmp%right)
        res => this%srr(tmp)
    end function drr

    subroutine preorder(this, tmp)
        class(sucursalesavl), intent(in) :: this
        type(nodo), pointer, intent(in) :: tmp

        if ( .not. associated(tmp) ) then
            return
        end if

        write(*,'(A,I0)') 'Sucursal: ', tmp%tsucursal%id
        call this%preorder(tmp%left)
        call this%preorder(tmp%right)
    end subroutine preorder

    subroutine inorder(this, tmp)
        class(sucursalesavl), intent(in) :: this
        type(nodo), pointer, intent(in) :: tmp

        if ( .not. associated(tmp) ) then
            return
        end if

        call this%inorder(tmp%left)
        write(*,'(A,I0)') 'Sucursal: ', tmp%tsucursal%id
        call this%inorder(tmp%right)
    end subroutine inorder

    subroutine postorder(this, tmp)
        class(sucursalesavl), intent(in) :: this
        type(nodo), pointer, intent(in) :: tmp

        if ( .not. associated(tmp) ) then
            return
        end if

        call this%postorder(tmp%left)
        call this%postorder(tmp%right)
        write(*,'(A,I0)') 'Sucursal: ', tmp%tsucursal%id
    end subroutine postorder

    subroutine set_id(this,id)
        class(sucursal), intent(inout) :: this
        integer, intent(in) :: id
        this%id = id
    end subroutine set_id
    subroutine set_departamento(this,departamento)
        class(sucursal), intent(inout) :: this
        character(len=*), intent(in) :: departamento
        this%departamento = departamento
    end subroutine set_departamento
    subroutine set_direccion(this,direccion)
        class(sucursal), intent(inout) :: this
        character(len=*), intent(in) :: direccion
        this%direccion = direccion
    end subroutine set_direccion    
    subroutine set_contrasena(this,contrasena)
        class(sucursal), intent(inout) :: this
        character(len=*), intent(in) :: contrasena
        this%contrasena = contrasena
    end subroutine set_contrasena

    subroutine imprimir_sucursal(this)
        class(sucursal), intent(inout) :: this
        print*,"------------------------------------------------------"
        write(*, '(A,I0)') 'ID: ', this%id
        write(*, '(A,A)') 'Departamento: ', this%departamento
        write(*, '(A,A)') 'Direccion: ', this%direccion
        write(*, '(A,A)') 'Contrasena: ', this%contrasena
        print*,"---------------------Tecnicos-------------------------"
        call this%tecnicos%listadotecnicos()
        print*,"------------------------------------------------------"

    end subroutine imprimir_sucursal

  ! subroutine inicializar_tecnicos(this)
    !    class(sucursal), intent(inout) :: this
     !   call this%tecnicos%init(7,30,70)
    !end subroutine inicializar_tecnicos

    subroutine inicializar_sucursal(this)
        class(sucursal), intent(inout) :: this
        this%id = -1
        this%departamento = ''
        this%direccion = ''
        this%contrasena = ''
        this%impresoras = -1
        call this%tecnicos%init(7,30,70)
    end subroutine inicializar_sucursal
end module avlsucursales