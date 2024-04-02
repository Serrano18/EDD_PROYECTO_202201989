module matriz
    implicit none
    private

    type :: node_val
        private
        logical :: exists = .false.
        character(len=7) :: value
    end type node_val

    type :: node
        private
        integer :: i, j
        character(len=7) :: value = "#FFFFFF" !inicia en blanco
        type(node), pointer :: up => null()
        type(node), pointer :: down => null()
        type(node), pointer :: right => null()
        type(node), pointer :: left => null()
    end type node

    type, public :: matrix
        private
        type(node), pointer :: root
        integer :: width = 0
        integer :: height = 0
    contains
        procedure :: insert
        procedure :: insertRowHeader
        procedure :: insertColumnHeader
        procedure :: insertInRow
        procedure :: insertInColumn
        procedure :: searchRow
        procedure :: searchColumn
        procedure :: nodeExists
        procedure :: print
        procedure :: printColumnHeaders
        procedure :: getValue
        procedure :: graphMatrix
        procedure :: graphTable
        procedure :: imprimirvalores
        procedure :: graficarNLogico
        ! procedure :: printRowHeaders
    end type

contains
subroutine imprimirvalores(self)
    class(matrix), intent(inout) :: self
    type(node), pointer :: aux, auxaux
    aux => self%root%down
    do while ( associated(aux) )
        print *, aux%i
        print *, aux%j
        print *, aux%value
        auxaux => aux%right
        do while ( associated(auxaux) )
            print *, auxaux%i
            print *, auxaux%j
            print *, auxaux%value
            auxaux => auxaux%right
        end do
        if ( associated(aux%down) ) then
            aux => aux%down
        else
            exit
        end if
    end do
end subroutine imprimirvalores

    subroutine insert(self, i, j, value) 
        class(matrix), intent(inout) :: self  
        integer, intent(in) :: i
        integer, intent(in) :: j
        character(len=7), intent(in) :: value

        type(node), pointer :: new
        type(node), pointer :: row
        type(node), pointer :: column

        allocate(new)
        new = node(i=i, j=j, value=value)

        if(.not. associated(self%root)) then
            allocate(self%root)
            self%root = node(i=-1, j=-1)
        end if

        row => self%searchRow(i)
        column => self%searchColumn(j)

        if(j > self%width) self%width = j
        if(i > self%height) self%height = i

        if(.not. self%nodeExists(new)) then
            if(.not. associated(column)) then
                column => self%insertColumnHeader(j)
            end if

            if(.not. associated(row)) then
                row => self%insertRowHeader(i)
            end if
            call self%insertInColumn(new, row)
            call self%insertInRow(new, column)
        end if
    end subroutine insert

    function searchColumn(self, j) result(actual)
        class(matrix), intent(in) :: self
        integer, intent(in) :: j

        type(node), pointer :: actual
        actual => self%root

        do while(associated(actual))
            if(actual%j == j) return
            actual => actual%right
        end do
    end function searchColumn

    function searchRow(self, i) result(actual)
        class(matrix), intent(in) :: self
        integer, intent(in) :: i

        type(node), pointer :: actual
        actual => self%root

        do while(associated(actual))
            if(actual%i == i) return
            actual => actual%down
        end do
    end function searchRow

    function nodeExists(self, new) result(exists)
        class(matrix), intent(inout) :: self  
        type(node), pointer :: new
        
        logical :: exists
        type(node), pointer :: rowHeader
        type(node), pointer :: column
        rowHeader => self%root
        exists = .false.

        do while(associated(rowHeader))
            if(rowHeader%i == new%i) then
                column => rowHeader
                do while(associated(column))
                    if(column%j == new%j) then
                        column%value = new%value
                        exists = .true.
                        return
                    end if
                    column => column%right
                end do
                return
            end if
            rowHeader => rowHeader%down
        end do
        return
    end function nodeExists

    function insertRowHeader(self, i) result(newRowHeader)
        class(matrix), intent(inout) :: self  
        integer, intent(in) :: i

        type(node), pointer :: newRowHeader
        allocate(newRowHeader)

        newRowHeader = node(i=i, j=-1)
        call self%insertInRow(newRowHeader, self%root)
    end function insertRowHeader

    subroutine insertInRow(self, new, rowHeader)
        class(matrix), intent(inout) :: self
        type(node), pointer :: new
        type(node), pointer :: rowHeader

        type(node), pointer :: actual
        actual => rowHeader

        do while(associated(actual%down))
            if(new%i < actual%down%i .and. new%i > actual%i) then
                new%down => actual%down
                new%up => actual
                actual%down%up => new
                actual%down => new
                exit
            end if
            actual => actual%down
        end do

        if(.not. associated(actual%down)) then
            actual%down => new
            new%up => actual
        end if
    end subroutine insertInRow

    function insertColumnHeader(self, j) result(newColumnHeader)
        class(matrix), intent(inout) :: self  
        integer, intent(in) :: j

        type(node), pointer :: newColumnHeader
        allocate(newColumnHeader)

        newColumnHeader = node(i=-1, j=j)
        call self%insertInColumn(newColumnHeader, self%root)
    end function insertColumnHeader

    subroutine insertInColumn(self, new, columnHeader)
        class(matrix), intent(inout) :: self
        type(node), pointer :: new
        type(node), pointer :: columnHeader
        
        type(node), pointer :: actual
        actual => columnHeader
        do while(associated(actual%right))
            if(new%j < actual%right%j .and. new%j > actual%j) then
                new%right => actual%right
                new%left => actual
                actual%right%left => new
                actual%right => new
                exit
            end if
            actual => actual%right
        end do
        
        if(.not. associated(actual%right)) then
            actual%right => new
            new%left => actual
        end if
    end subroutine insertInColumn

    subroutine print(self)
        class(matrix), intent(inout) :: self  
        integer :: i
        integer :: j
        type(node), pointer :: aux
        type(node_val) :: val
        aux => self%root%down

        call self%printColumnHeaders()

        do i = 0, self%height
            print *, ""
            write(*, fmt='(I3)', advance='no') i
            do j = 0, self%width
                val = self%getValue(i,j)
                if(.not. val%exists) then
                    write(*, fmt='(I3)', advance='no') 0
                else
                    write(*, fmt='(L3)', advance='no') val%value
                end if
            end do
        end do
    end subroutine print

    subroutine printColumnHeaders(self)
        class(matrix), intent(in) :: self
        integer :: j

        do j=-1, self%width
            write(*, fmt='(I3)', advance='no') j
        end do
    end subroutine printColumnHeaders

    function getValue(self, i, j) result(val)
        class(matrix), intent(in) :: self
        integer, intent(in) :: i
        integer, intent(in) :: j
        
        type(node), pointer :: rowHeader
        type(node), pointer :: column
        type(node_val) :: val
        rowHeader => self%root

        do while(associated(rowHeader))
            if(rowHeader%i == i) then
                column => rowHeader
                do while(associated(column))
                    if(column%j == j) then
                        val%value = column%value
                        val%exists = .true.
                        return
                    end if
                    column => column%right
                end do
                return
            end if
            rowHeader => rowHeader%down
        end do
    end function getValue

    subroutine graphMatrix(self)
        class(matrix), intent(in) :: self
        integer :: unit
        open(unit, file='matriz.dot', status='replace')
        write(unit, '(A)') 'digraph G {'
        write(unit, '(A)') '    node [shape=plaintext];'
        call self%graphTable(unit)
        write(unit, '(A)') '}'
        write(unit, '(A)') ''
        close(unit)
        call execute_command_line('dot -Gnslimit=2 -Tpng matriz.dot > matriz.png')
        call execute_command_line('start matriz.png')
    end subroutine graphMatrix

    function convertiraCadena(i) result(str)
        implicit none
        integer, intent(in) :: i
        character(len=32) :: str
        write (str, '(I0)') i
    end function convertiraCadena


    subroutine graphTable(self, unit)
        class(matrix), intent(in) :: self
        type(node), pointer :: aux, temp
        integer, intent(in) :: unit
        character(len=7) :: str = "#FFFFFF"
        integer :: l_i, l_j

        aux => self%root%down
        temp => aux%right
        write(unit, '(A)') '    table [label=<'
        write(unit, '(A)') '    <TABLE BORDER="1" CELLBORDER="1" CELLSPACING="0" >'
        do l_i = 0, self%height
            write(unit, '(A)') '        <TR>'
            do l_j = 0, self%width
                if ( temp%i == l_i .and. temp%j == l_j ) then
                    str = temp%value
                    if ( associated(temp%right) ) then
                        temp => temp%right
                    else
                        aux => aux%down
                        if ( associated(aux) ) then
                            temp => aux%right
                        end if
                    end if
                write(unit, '(A,A,A)') '            <TD BGCOLOR="',str,'" ></TD>'
                else
                    write(unit, '(A,A,A)') '            <TD BGCOLOR="#FFFFFF"></TD>'
                end if
            end do
            write(unit, '(A)') '        </TR>'
        end do
        write(unit, '(A)') '    </TABLE>>];'
    end subroutine graphTable

    subroutine graficarNLogico(self)
        class(matrix), intent(inout) :: self  
        integer :: i, j, unit, iostat
        type(node), pointer :: aux
        type(node_val) :: val
        
        aux => self%root%down



        print *,"Generando matriz dispersa..."
        open(newunit=unit, file="ImagenNLogico.dot", status='replace', iostat=iostat)
     

        write(unit, *) "graph matriz {"
        write(unit, *) "    node [shape=box];"

        write(unit,'(A)') '"Origen" [label="-1", group = 1]'

        !Generacion de encabezados de cada columna
        do j=0, self%width
            write(unit,'(A,I0,A,I0,A,I0,A)') '"Col', j, '"[label="', j, '", group =', j+2, ']'
            if (.not. j == 0) then
            if (j /= self%width) then
                write(unit,'(A,I0,A,I0,A)') '"Col', j, '" --  "Col', j+1, '"'
            end if
            else
                write(unit,'(A,I0,A,I0,A)') '"Origen" --  "Col', j, '"'
                write(unit,'(A,I0,A,I0,A)') '"Col', j, '" --  "Col', j+1, '"'
            end if
        end do

        write(unit, *) ' { rank=same; "Origen";'

        do j=0, self%width
            write(unit, '(A,I0,A)', advance='no') ' "Col', j, '";'
        end do

        write(unit, *) " }"

        ! Generacion de encabezados de cada Fila
        do i=0, self%height
            write(unit,'(A,I0,A,I0,A)') '"Fil', i, '"[label="', i, '", group = 1]'
            if (.not. i == 0) then
            if (i /= self%height) then
                write(unit,'(A,I0,A,I0,A)') '"Fil', i, '" --  "Fil', i+1, '"'
            end if
            else
                write(unit,'(A,I0,A,I0,A)') '"Origen" --  "Fil', i, '"'
                write(unit,'(A,I0,A,I0,A)') '"Fil', i, '" --  "Fil', i+1, '"'
            end if
        end do

        do i = 0, self%height
            do j = 0, self%width
                val = self%getValue(i,j)
                if(.not. val%exists) then
                    write(unit,'(A,I0,A,I0,A,I0,A)') '"F',i, 'C', j, '" [label=" ", group=',  j+2,']'
                else
                    write(unit, '(A,I0,A,I0,A,A,A)') '"F',i, 'C', j, '" [label=" ", style=filled, fillcolor="', &
                    val%value, '"]'
                end if
                if (i == 0 .and. j == 0) then
                    write(unit,'(A,I0,A,A,I0,A,I0,A)') '"Fil', i, '" -- ', '"F',i, 'C', j, '"'
                    write(unit,'(A,I0,A,A,I0,A,I0,A)') '"Col', j, '" -- ', '"F',i, 'C', j, '"'
                else if (i == 0) then
                    write(unit,'(A,I0,A,A,I0,A,I0,A)') '"Col', j, '" -- ', '"F',i, 'C', j, '"'
                else if (j == 0) then
                    write(unit,'(A,I0,A,A,I0,A,I0,A)') '"Fil', i, '" -- ', '"F',i, 'C', j, '"'
                end if 
                if (j /= self%width) write(unit,'(A,I0,A,I0,A,A,I0,A,I0,A)') '"F',i, 'C', j, '" -- ' , '"F',i, 'C', j+1, '"'
                if (i /= self%height) write(unit,'(A,I0,A,I0,A,A,I0,A,I0,A)') '"F',i, 'C', j, '" -- ', '"F',i+1, 'C', j, '"'
            end do

            write(unit, '(A,I0,A)') ' { rank=same; "Fil', i, '";'
            do j=0, self%width
                write(unit, '(A,I0,A,I0,A)', advance='no') '"F',i, 'C', j, '";'
            end do
            write(unit, *) "}"
        end do

        write(unit, *) "}"
        close(unit)

        call system('dot  -Tpng ImagenNLogico.dot -o ImagenNLogico.png')
        print *,"Matriz Dispersa graficada con exito"
        call system('start ImagenNLogico.png')
    end subroutine graficarNLogico
end module matriz