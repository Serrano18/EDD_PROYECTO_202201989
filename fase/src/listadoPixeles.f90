module listadoPixeles
        type, public :: pixel
            integer :: fila
            integer :: columna
            character (len=30) :: color
            contains
            procedure :: setFila
            procedure :: getFila
            procedure :: setColumna
            procedure :: getColumna
            procedure :: setColor
            procedure :: getColor
        end type pixel
        
        type, public :: nodop
            type(pixel):: value
            type(nodop), pointer :: next => null()
        end type nodop
    
        type, public :: Listapixel
            integer :: size = 0
            type(nodop), pointer :: head => null()
            type(nodop), pointer :: tail => null()
            contains
            procedure :: append
            procedure :: deletep
            procedure :: print
            procedure :: getSize
        end type Listapixel
        
        contains
        subroutine setFila(this, newFila)
            class(pixel), intent(inout) :: this
            integer, intent(in) :: newFila
            this%fila = newFila
        end subroutine setFila
    
        function getFila(this) result(fila)
            class(pixel), intent(in) :: this
            integer :: fila
            fila = this%fila
        end function getFila
    
        subroutine setColumna(this, newColumna)
            class(pixel), intent(inout) :: this
            integer, intent(in) :: newColumna
            this%columna = newColumna
        end subroutine setColumna
    
        function getColumna(this) result(columna)
            class(pixel), intent(in) :: this
            integer :: columna
            columna = this%columna
        end function getColumna
    
        subroutine setColor(this, newColor)
            class(pixel), intent(inout) :: this
            character(len=*), intent(in) :: newColor
            this%color = newColor
        end subroutine setColor
    
        function getColor(this) result(color)
            class(pixel), intent(in) :: this
            character(len=30) :: color
            color = this%color
        end function getColor
    
        subroutine append(this,pix)
            class(Listapixel), intent(inout) :: this
            type(pixel), intent(in) :: pix
            type(nodop), pointer :: temp
                allocate(temp)
                temp%value=pix
                temp%next => null()
                if (.not. associated(this%head)) then
                    this%head => temp
                    this%tail => temp
                else
                    this%tail%next => temp
                    this%tail => temp
                end if
            this%size = this%size + 1
        end subroutine append
    
        subroutine deletep(this)
            class(Listapixel), intent(inout) :: this
            type(nodop), pointer :: temp
    
            if (.not. associated(this%head)) then
                print *, 'Lista Vacia'
                return
            end if
            !print *, 'Delete ', this%head%value
            temp => this%head
            this%head => this%head%next
            deallocate(temp)
        end subroutine deletep
    
        subroutine print(this)
            class(Listapixel), intent(inout) :: this
            type(nodop), pointer :: current
             
            current => this%head
    
            print *, '|--------------------------------------------------------|'
            print *, '|                   Listado de Pixeles                   |'
            print *, '|--------------------------------------------------------|'
            do while (associated(current))
                write(*,'(A,I0,A,I0,A,A)') "Fila: ", current%value%fila, &
                " Columna: ", current%value%columna, " Color: ", &
                trim(current%value%color)
                current => current%next
            end do 
        end subroutine print
        
    function createPixel(fila, columna, color) result(pix)
        integer, intent(in) :: fila, columna
        character(len=30), intent(in) :: color
        type(pixel) :: pix

        pix%fila = fila
        pix%columna = columna
        pix%color = color
    end function createPixel

    function getSize(this) result(size)
        class(Listapixel), intent(in) :: this
        integer :: size
        size = this%size
    end function getSize
    
end module listadoPixeles