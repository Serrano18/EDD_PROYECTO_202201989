module blockchain
    use avlsucursales, only:sucursalesavl,sucursal
    use sha256_module, only: sha256
    use resultados, only: listares,nodores
    use merkledatos, only:Arbolmerkle
    implicit none
    integer :: blocks = 0
    type :: block
        integer :: index, nonce
        character(:), allocatable :: tiempos
        type(listares),pointer :: transacciones
        type(block), pointer :: next => null()
        type(sucursalesavl), pointer :: sucursales => null()
        character(len=256) :: prevh = '000'
        character(len=256) :: tophashm = '0000'
        character(len=256) ::  hash = '0000'
        contains
        procedure :: inicializarBlock
        procedure :: imprimirBlock
    end type block

    type :: blockchainl
        type(block), pointer :: head => null()
        type(block), pointer :: tail => null()
        integer :: size = 0
        contains
        procedure :: agregarBlock
        procedure :: imprimirBlockchain
    end type blockchainl
    contains
    subroutine inicializarBlock(this,datos,sucursa)
        class(block), intent(inout) :: this
        type(listares), pointer :: datos
        type(sucursalesavl), pointer :: sucursa
        type(nodores), pointer :: current  => null()
        type(sucursal), pointer :: origen => null()
        type(sucursal), pointer :: destino => null()
        type(Arbolmerkle) :: merkle 
        integer, dimension(8) :: valores
        character(len=20) :: tiempo
        this%index = blocks
        blocks = blocks + 1
        call date_and_time(values=valores)
        write(tiempo, '(I0,A,I0,A,I0,A,I0,A,I0,A,I0)') valores(3), '-', valores(2), '-', valores(1), ' ', &
        valores(5), ':', valores(6), ':', valores(7)
        this%nonce = 4560
        this%tiempos = trim(tiempo)
        this%transacciones => datos
        this%sucursales => sucursa
        current => datos%head
        do while(associated(current))
            origen => sucursa%buscar(current%res%idr)
            if ( associated(origen) .and. associated(current%next)) then
                destino => sucursa%buscar(current%next%res%idr)
                if ( associated(destino) ) then
                    call merkle%add_dato(origen%id, destino%id, origen%direccion, destino%direccion, &
                    current%next%res%weightr*80)
                end if
            end if
            current => current%next
        end do
        call merkle%generate_dato()
        call merkle%dotgen()
        this%tophashm = merkle%getroot()
    end subroutine inicializarBlock
    subroutine imprimirBlock (this)
        class(block), intent(in) :: this
        print *, '------------------------------------------'
        write(*, '(A,I0)') '--> Index: ', this%index
        write(*, '(A,A)') '--> Tiempos: ', this%tiempos
        write(*, '(A,I0)') '--> Nonce: ', this%nonce
        write(*, '(A,A)') '--> Prev Hash: ', this%prevh
        write(*, '(A,A)') '--> Cabeza Merkle: ', this%tophashm
        write(*, '(A,A)') '--> Hash: ', this%hash
        call this%transacciones%imprimirResultado()
        print *, '-------------------------------------------'
    end subroutine imprimirBlock

    subroutine agregarBlock(this,temp)
        class(blockchainl), intent(inout) :: this
        type(block), pointer,intent(inout) :: temp
        character(len=256) :: index,nonce
        write(index, '(I0)') temp%index
        write(nonce, '(I0)') temp%nonce

        if(.not. associated(this%head)) then 
            this%head => temp
            temp%hash = sha256(trim(index)//trim(temp%tiempos)// & 
            trim(nonce)// temp%prevh // temp%tophashm)
            this%tail => temp
        else
            this%tail%next => temp
            temp%prevh = this%tail%hash
            temp%hash = sha256(trim(index)//trim(temp%tiempos)// &
            trim(nonce)// temp%prevh // temp%tophashm)
            this%tail => temp
        end if
    end subroutine agregarBlock
    
    subroutine imprimirBlockchain(this)
        class(blockchainl), intent(in) :: this
        type(block), pointer :: current => null()
        current => this%head
        do while(associated(current))
            call current%imprimirBlock()
            current => current%next
        end do
    end subroutine imprimirBlockchain
end module blockchain