module tecnicoshash
    implicit none
    type :: tecnicos
        integer(kind=8)::dpi
        character(:), allocatable::nombre,apellido,genero,direc
        integer::telefono
        integer :: trabajoscumplidos
        contains
            procedure :: set_dpi
            procedure :: set_nombre
            procedure :: set_apellido
            procedure :: set_genero
            procedure :: set_direc
            procedure :: set_telefono
            procedure :: set_trabajoscumplidos
            procedure :: imprimir_tecnico
            procedure :: crear_tecnico
            procedure :: inicializador
    end type tecnicos
    type :: tecnicos_hash
        integer :: n,m,mini,maxi
        type(tecnicos), dimension(:), allocatable::h
        contains
            procedure :: init
            procedure :: division
            procedure :: linear
            procedure :: insert
            procedure :: rehashing
            procedure :: show
            procedure :: buscar
            procedure :: listadotecnicos
    end type tecnicos_hash
    contains

    subroutine init(this, m, mini, maxi)
        class(tecnicos_hash), intent(inout) :: this
        integer, intent(in) :: m, mini, maxi
        type(tecnicos) :: temporal

        this%m = m
        this%mini = mini
        this%maxi = maxi
        this%n = 0

        if ( allocated(this%h) ) then
            deallocate(this%h)
        end if

        allocate(this%h(m))

        call temporal%inicializador()

        this%h = temporal
    end subroutine init


    integer function division(this, k)
        class(tecnicos_hash), intent(inout) :: this
        integer(kind=8), intent(in) :: k

        division = int(mod(k, this%m),4)
    end function division
    integer function linear(this, k, i)
        class(tecnicos_hash), intent(inout) :: this
        integer(kind=8), intent(in) :: k
        integer, intent(in) :: i

        linear = int(mod(mod(k,7)+1*i, this%m),4)
    end function linear
    subroutine insert(this, temp)
        class(tecnicos_hash), intent(inout) :: this
        type(tecnicos), intent(in) :: temp
        integer :: l, i = 1

        l = this%division(temp%dpi)

        if ( l == 0) then
            l = 1
        end if

        do while(this%h(l)%dpi /= -1)
            l = this%linear(temp%dpi, i)
            i = i + 1
        end do

        this%h(l) = temp
        this%n = this%n + 1
        call this%rehashing()
    end subroutine insert


    subroutine rehashing(this)
        class(tecnicos_hash), intent(inout) :: this
        integer :: i, prev
        type(tecnicos), dimension(:), allocatable :: temp

        if ( this%n * 100 / this%m >= this%maxi ) then
            allocate(temp(this%m))
            temp = this%h
            prev = this%m
            this%m = this%m * 2
            call this%init(this%m, this%mini, this%maxi)
            do i = 1, prev
                if ( temp(i)%dpi /= -1 ) then
                    call this%insert(temp(i))
                end if
            end do
        else
            call this%show()
        end if
    end subroutine rehashing

    
    subroutine show(this)
        class(tecnicos_hash), intent(inout) :: this
        integer :: i

        write(*,'(A)', advance='no') '['
        do i = 1, this%m
            write(*,'(A,A)', advance='no') this%h(i)%nombre, ' '
        end do

        write(*,'(A)') ']'
    end subroutine show

    subroutine buscar(this,k)
        class(tecnicos_hash), intent(inout) :: this
        integer(kind=8), intent(in) :: k
        integer :: i = 0, d

        d = this%division(k)
        do while(this%h(d)%dpi /= k)
            d = this%linear(k, i)
            i = i + 1
        end do

        call this%h(d)%imprimir_tecnico()
    end subroutine buscar
    subroutine listadotecnicos(this)
        class(tecnicos_hash), intent(inout) :: this
        integer :: i

        do i = 1, this%m
            if ( this%h(i)%dpi /= -1 ) then
                call this%h(i)%imprimir_tecnico()
            end if
        end do
    end subroutine listadotecnicos

    !Subrutinas para la clase tecnicos
    subroutine inicializador(this)
        class(tecnicos), intent(inout) :: this
        this%dpi = -1_8
        this%nombre = ""
        this%apellido = ""
        this%genero = ""
        this%direc = ""
        this%telefono = -1
    end subroutine inicializador
    subroutine crear_tecnico(this,dpi,nombre,apellido,genero,direc,telefono,trabajoscumplidos)
        class(tecnicos), intent(inout)::this
        integer(kind=8), intent(in)::dpi
        character(len=*), intent(in)::nombre
        character(len=*), intent(in)::apellido
        character(len=*), intent(in)::genero
        character(len=*), intent(in)::direc
        integer, intent(in)::telefono
        integer, intent(in)::trabajoscumplidos
        this%dpi=dpi
        this%nombre=nombre
        this%apellido=apellido
        this%genero=genero
        this%direc=direc
        this%telefono=telefono
    end subroutine crear_tecnico
    subroutine set_dpi(this,dpi)
        class(tecnicos), intent(inout)::this
        integer(kind=8), intent(in)::dpi
        this%dpi=dpi
    end subroutine set_dpi
    subroutine set_nombre(this,nombre)
        class(tecnicos), intent(inout)::this
        character(len=*), intent(in)::nombre
        this%nombre=nombre
    end subroutine set_nombre
    subroutine set_apellido(this,apellido)
        class(tecnicos), intent(inout)::this
        character(len=*), intent(in)::apellido
        this%apellido=apellido
    end subroutine set_apellido
    subroutine set_genero(this,genero)
        class(tecnicos), intent(inout)::this
        character(len=*), intent(in)::genero
        this%genero=genero
    end subroutine set_genero
    subroutine set_direc(this,direc)
        class(tecnicos), intent(inout)::this
        character(len=*), intent(in)::direc
        this%direc=direc
    end subroutine set_direc
    subroutine set_telefono(this,telefono)
        class(tecnicos), intent(inout)::this
        integer, intent(in)::telefono
        this%telefono=telefono
    end subroutine set_telefono
    subroutine set_trabajoscumplidos(this,trabajoscumplidos)
        class(tecnicos), intent(inout)::this
        integer, intent(in)::trabajoscumplidos
        this%trabajoscumplidos=trabajoscumplidos
    end subroutine set_trabajoscumplidos
    subroutine imprimir_tecnico(this)
        class(tecnicos), intent(inout)::this
        print*, "------------------------------------------"
        print*, "DPI: ", this%dpi
        print*, "Nombre: ", this%nombre
        print*, "Apellido: ", this%apellido
        print*, "Genero: ", this%genero
        print*, "Direccion: ", this%direc
        print*, "Telefono: ", this%telefono
        print*,"--------------------------------------------"
    end subroutine imprimir_tecnico 
end module tecnicoshash