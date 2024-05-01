module merkledatos
    use sha256_module

    implicit none
    integer :: uid = 1
    type :: dato
        integer :: uid
        character(:), allocatable :: idOrigen, idDestino
        character(:), allocatable :: direccionOrigen, direccionDestino
        character(:), allocatable :: costo, valorHash
        type(dato), pointer :: next => null()
    contains
        procedure :: initdato
        procedure :: setIdOrigen
        procedure :: setIdDestino
        procedure :: setDireccionOrigen
        procedure :: setDireccionDestino
        procedure :: setCosto
        procedure :: setValorHash
        procedure :: relleno
        procedure :: imprimirDatos
    end type dato

    type :: nodohash
        integer :: uid
        type(dato), pointer :: tmpdato => null()   
        type(nodohash), pointer :: left => null()
        type(nodohash), pointer :: right => null()
        character(:), allocatable :: hash
    end type nodohash
    
    type :: Arbolmerkle
        type(nodohash), pointer :: tophash => null()
        type(dato), pointer :: datahead => null()
        type(dato), pointer :: datacoil => null()
        character (len = :), allocatable :: dot
        integer :: pos = 0
        contains
        procedure :: add_dato
        procedure :: generate_dato
        procedure :: tamano_datos
        procedure :: getdato 
        procedure :: createtree
        procedure :: genhash
        procedure :: dotgenm
        procedure :: dotgenrecm
        procedure :: getroot
    end type Arbolmerkle

contains
function getroot(this) result(hash)
    class(Arbolmerkle), intent(in) :: this
    character(:), allocatable :: hash
    hash = this%tophash%hash
end function getroot


subroutine generate_dato(this)
    class(Arbolmerkle), intent(inout) :: this
    integer :: i, exp = 1, pow

    do while(2 ** exp < this%tamano_datos())
        exp = exp + 1
    end do

    pow = 2 ** exp
    this%pos = pow
    i = this%tamano_datos()

    do while(i < pow)
        call this%add_dato(-1,-1,"","",-1)
        i = i + 1
    end do

    allocate(this%tophash)

    call this%createtree(this%tophash, exp)
    call this%genhash(this%tophash, pow)
end subroutine generate_dato


subroutine add_dato(this, ido, idd, direco, direcd, costof)
    class(Arbolmerkle), intent(inout) :: this
    integer, intent(in) :: ido, idd, costof
    character(*), intent(in) :: direco, direcd
    type(dato), pointer :: tdata

    allocate(tdata)
    call tdata%initdato(ido, idd, direco, direcd, costof)

    tdata%uid = uid
    uid = uid + 1

    if ( associated(this%datahead) ) then
        this%datacoil%next => tdata
        this%datacoil => tdata
    else
        this%datahead => tdata
        this%datacoil => tdata
    end if

end subroutine add_dato
function tamano_datos(this) result(len)
    class(Arbolmerkle), intent(inout) :: this
    type(dato), pointer :: tdata
    integer :: len
    len = 0
    tdata => this%datahead
    do while( associated(tdata) )
        len = len + 1
        tdata => tdata%next
    end do
end function tamano_datos
subroutine createtree(this, tnode, exp)
    class(Arbolmerkle), intent(inout) :: this
    type(nodohash), pointer, intent(inout) :: tnode
    integer, intent(in) :: exp

    tnode%uid = uid
    uid = uid + 1

    if (exp > 0) then
        allocate(tnode%left)
        allocate(tnode%right)
        call this%createtree(tnode%left, exp - 1)
        call this%createtree(tnode%right, exp - 1)
    end if
end subroutine createtree

subroutine genhash(this, tnode, pow)
    class(Arbolmerkle), intent(inout) :: this
    type(nodohash), pointer, intent(inout) :: tnode
    integer, intent(in) :: pow
    integer :: i

    if ( associated(tnode) ) then
        call this%genhash(tnode%left, pow)
        call this%genhash(tnode%right, pow)

        if ( .not. associated(tnode%left) .and. .not. associated(tnode%right) ) then
            i = pow - this%pos
            tnode%tmpdato => this%getdato(i)
            this%pos = this%pos - 1
            tnode%hash = tnode%tmpdato%valorHash
        else
            tnode%hash = sha256(tnode%left%hash // tnode%right%hash)
        end if
    end if

end subroutine genhash

function getdato(this, pos) result(tdata)
    class(Arbolmerkle), intent(inout) :: this
    integer, intent(inout) :: pos
    type(dato), pointer :: tdata

    tdata => this%datahead

    do while( associated(tdata) )
        if ( pos == 0 ) then
            return
        end if
        pos = pos - 1
        tdata => tdata%next
    end do
end function getdato



subroutine dotgenm(this)
    class(Arbolmerkle), intent(in) :: this
    integer :: unit

    open(unit, file='img/merkle_tree.dot', status='replace')
    write(unit, '(a)') 'digraph G {'
    call this%dotgenrec(this%tophash, unit)
    write(unit, '(a)') '}'
    close(unit)

    call execute_command_line('dot -Tpng img/merkle_tree.dot -o img/merkle_tree.png')
    call execute_command_line('eog img/merkle_tree.png')

end subroutine dotgenm

subroutine dotgenrecm(this, tmp, unit)
    class(Arbolmerkle), intent(in) :: this
    type(nodohash), pointer, intent(in) :: tmp
    integer, intent(in) :: unit

    if ( .not. associated(tmp) ) then
        return
    end if

    write(unit, '(I0,A,A,A)') tmp%uid, ' [label="', tmp%hash, '"];'
    if ( associated(tmp%left) ) then
        write(unit, '(I0,A,I0,A)') tmp%uid, ' -> ', tmp%left%uid, ';'
    end if

    if ( associated(tmp%right) ) then
        write(unit, '(I0,A,I0,A)') tmp%uid, ' -> ', tmp%right%uid, ';'
    end if


    call this%dotgenrecm(tmp%left, unit)
    call this%dotgenrecm(tmp%right, unit)

    if ( associated(tmp%tmpdato) ) then
        write(unit, '(I0,A)') tmp%tmpdato%uid, '[label=<<TABLE><TR>'
        write(unit, '(A,A,A)') '<TD>Id Origen: ', trim(tmp%tmpdato%idOrigen), '</TD>'
        write(unit, '(A,A,A)') '<TD>Direccion Origen: ', trim(tmp%tmpdato%direccionOrigen), '</TD></TR>'
        write(unit, '(A,A,A)') '<TR><TD>Id Destino: ', trim(tmp%tmpdato%idDestino), '</TD>'
        write(unit, '(A,A,A)') '<TD>Direccion Destino: ', trim(tmp%tmpdato%direccionDestino), '</TD></TR>'
        write(unit, '(A,A,A)') '<TR><TD>Costo: ', trim(tmp%tmpdato%costo), '</TD></TR>'
        write(unit, '(A)') '</TABLE>>];'
        write(unit, '(I0,A,I0,A)') tmp%uid, ' -> ', tmp%tmpdato%uid, ';'
    end if
end subroutine dotgenrecm

subroutine initdato(this, id1, id2, direccion1, direccion2, costo)
    class(dato), intent(inout) :: this
    integer, intent(in) :: id1, id2, costo
    character(len=*), intent(in) :: direccion1, direccion2

    call this%setIdOrigen(id1)
    call this%setIdDestino(id2)
    call this%setDireccionOrigen(direccion1)
    call this%setDireccionDestino(direccion2)
    call this%setCosto(costo)
    call this%setValorHash()
end subroutine initdato
subroutine setValorHash(this)
    class(dato), intent(inout) :: this
    character(:), allocatable :: valorh

    valorh = sha256(this%idOrigen // this%direccionOrigen // this%idDestino // this%direccionDestino // this%costo)

    this%valorHash = valorh
end subroutine setValorHash
subroutine relleno(this)
    class(dato), intent(inout) :: this

    this%idOrigen = "-1"
    this%idDestino = "-1"
    this%direccionDestino = "-1"
    this%direccionOrigen = "-1"
    this%costo = "-1"
    this%valorHash = "-1"
end subroutine relleno
    subroutine setIdOrigen(this, id)
        class(dato), intent(inout) :: this
        integer, intent(in) :: id
        character(len=256) :: int_str
        write(int_str, '(I10)') id
        this%idOrigen = trim(int_str)
    end subroutine setIdOrigen  
    subroutine setCosto(this, costo)
        class(dato), intent(inout) :: this
        integer, intent(in) :: costo
        character(len=256) :: int_str
        write(int_str, '(I10)') costo
        this%costo = trim(int_str)
    end subroutine setCosto
    subroutine setIdDestino(this, id)
        class(dato), intent(inout) :: this
        integer, intent(in) :: id
        character(len=256) :: int_str
        write(int_str, '(I10)') id
        this%idDestino = trim(int_str)
    end subroutine setIdDestino
    subroutine setDireccionOrigen(this, direccion)
        class(dato), intent(inout) :: this
        character(*), intent(in) :: direccion
        this%direccionOrigen = direccion
    end subroutine setDireccionOrigen
    subroutine setDireccionDestino(this, direccion)
        class(dato), intent(inout) :: this
        character(*), intent(in) :: direccion
        this%direccionDestino = direccion
    end subroutine setDireccionDestino
    subroutine imprimirDatos(this)
        class(dato), intent(in) :: this
        write(*, '(A, A)') "ID Origen: ", this%idOrigen
        write(*, '(A, A)') "ID Destino: ", this%idDestino
        write(*, '(A, A)') "Direccion Origen: ", this%direccionOrigen
        write(*, '(A, A)') "Direccion Destino: ", this%direccionDestino
        write(*, '(A, A)') "Costo: ", this%costo
        write(*, '(A, A)') "Valor Hash: ", this%valorHash
    end subroutine imprimirDatos
end module merkledatos