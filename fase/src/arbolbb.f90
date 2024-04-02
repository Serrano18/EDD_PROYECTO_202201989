module arbolbb
    use listadoPixeles
    use linkedlistm
    implicit none
    type, public :: capas
        integer :: id
        type(Listapixel) :: listadoDePixeles
        contains
            procedure :: setId
            procedure :: getId
            procedure :: agregarPixel
    end type capas

    type :: Node_t
        type(capas) :: value
        type(Node_t), pointer :: right => null()
        type(Node_t), pointer :: left => null()
    end type Node_t

    type, public :: abb
        type(linkedlist) :: listado
        type(Node_t), pointer :: root => null()
        integer :: sizeab = 0
    contains
        procedure :: insertabb
        procedure :: delete
        procedure :: preorder
        procedure :: inorder
        procedure :: posorder
        procedure :: graph
        procedure :: search
        procedure :: search_recabb
        procedure :: recorrido
        procedure :: print_leaf_layers
        procedure :: depthf
    end type abb

contains   
    
    subroutine recorrido(this, tmp)
        class(abb), intent(inout) :: this
        class(Node_t), intent(in), pointer :: tmp
        if( .not. associated(tmp)) then
            return
        end if
        call this%recorrido(tmp%left)
        call this%recorrido(tmp%right)
        call this%listado%addlist(tmp%value%id)
    end subroutine recorrido

    subroutine agregarPixel(this, pix)
        class(capas), intent(inout) :: this
        type(pixel), intent(in) :: pix

        call this%listadoDePixeles%append(pix)
    end subroutine agregarPixel 
    subroutine setId(this, newId)
        class(capas), intent(inout) :: this
        integer, intent(in) :: newId
        this%id = newId
    end subroutine setId
    function getId(this) result(id)
        class(capas), intent(in) :: this
        integer :: id
        id = this%id
    end function getId
    !Subrutinas del tipo abb
    subroutine insertabb(self, val)
        class(abb), intent(inout) :: self
        type(capas), intent(in) :: val

        if (.not. associated(self%root)) then
            allocate(self%root)
            self%root%value = val
            self%sizeab =  1
        else
            call insertRec(self%root, val,self)
        end if
    end subroutine insertabb
    recursive subroutine insertRec(root, val,self)
        type(Node_t), pointer, intent(inout) :: root
        type(capas) , intent(in) :: val
        class(abb), intent(inout) :: self
        if (val%id < root%value%id) then
            if (.not. associated(root%left)) then
                allocate(root%left)
                root%left%value = val
                self%sizeab = self%sizeab + 1
            else
                call insertRec(root%left, val,self)
            end if
        else if (val%id > root%value%id) then
            if (.not. associated(root%right)) then
                allocate(root%right)
                root%right%value = val
                self%sizeab = self%sizeab + 1
            else
                call insertRec(root%right, val,self)
            end if
        end if
    end subroutine insertRec

    subroutine delete(self, val)
        class(abb), intent(inout) :: self
        type(capas), intent(inout) :: val
    
        self%root => deleteRec(self%root, val)
    end subroutine delete
    recursive function deleteRec(root, value) result(res)
        type(Node_t), pointer :: root
        type(capas), intent(in) :: value
        type(Node_t), pointer :: res
        type(Node_t), pointer :: temp

        if (.not. associated(root)) then
            res => root
            return
        end if

        if (value%id < root%value%id) then
            root%left => deleteRec(root%left, value)
        else if (value%id > root%value%id) then
            root%right => deleteRec(root%right, value)
        else
            if (.not. associated(root%left)) then
                temp => root%right
                deallocate(root)
                res => temp
                return
            else if (.not. associated(root%right)) then
                temp => root%left
                deallocate(root)
                res => temp
                return
            else
                call getMajorOfMinors(root%left, temp)
                root%value = temp%value
                root%left => deleteRec(root%left, temp%value)
            end if
        end if

        res => root
    end function deleteRec
    recursive subroutine getMajorOfMinors(root, major)
        type(Node_t), pointer :: root, major
        if (associated(root%right)) then
            call getMajorOfMinors(root%right, major)
        else
            major => root
        end if
    end subroutine getMajorOfMinors

    subroutine preorder(self)
        class(abb), intent(in) :: self
        
        call preorderRec(self%root)
        write(*, '()')
    end subroutine preorder
    recursive subroutine preorderRec(root)
        type(Node_t), pointer, intent(in) :: root

        if(associated(root)) then
            ! RAIZ - IZQ - DER
            write(*, '(I0 A)', advance='no') root%value%id, " - "
            call preorderRec(root%left)
            call preorderRec(root%right)
        end if
    end subroutine preorderRec

    subroutine inorder(self)
        class(abb), intent(in) :: self
        
        call inordenRec(self%root)
        print *, ""
    end subroutine inorder
    recursive subroutine inordenRec(root)
        type(Node_t), pointer, intent(in) :: root

        if(associated(root)) then
            ! IZQ - RAIZ - DER
            call inordenRec(root%left)
            write(*, '(I0 A)', advance='no') root%value%id, " - "
            call inordenRec(root%right)
        end if
    end subroutine inordenRec

    subroutine posorder(self)
        class(abb), intent(in) :: self
        call posordenRec(self%root)
        print *, ""
    end subroutine posorder
    recursive subroutine posordenRec(root)
        type(Node_t), pointer, intent(in) :: root

        if(associated(root)) then
            ! IZQ - DER - RAIZ
            call posordenRec(root%left)
            call posordenRec(root%right)
            write(*, '(I0 A)', advance='no') root%value%id, " - "
        end if
    end subroutine posordenRec

    subroutine graph(self, filename)
        class(abb), intent(in) :: self
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: dotStructure
        character(len=:), allocatable :: createNodes
        character(len=:), allocatable :: linkNodes
        
        createNodes = ''
        linkNodes = ''

        dotStructure = "digraph G{" // new_line('a')
        dotStructure = dotStructure // "node [shape=circle];" // new_line('a')

        if (associated(self%root)) then
            call RoamTree(self%root, createNodes, linkNodes)
        end if
        
        dotStructure = dotStructure // trim(createNodes) // trim(linkNodes) // "}" // new_line('a')
        call write_dot(filename, dotStructure)
        print *, "Archivo actualizado existosamente."
       
    
    end subroutine graph
    recursive subroutine RoamTree(current, createNodes, linkNodes)
        type(Node_t), pointer :: current
        character(len=:), allocatable, intent(inout) :: createNodes, linkNodes
        character(len=20) :: address, str_value

        if (associated(current)) then
            ! SE OBTIENE INFORMACION DEL NODO ACTUAL
          address = get_address_memory(current)
          write(str_value, '(I0)') current%Value%id
          createNodes = createNodes // '"' // trim(address) // '"' // '[label="' // trim(str_value) // '"];' // new_line('a')
          ! VIAJAMOS A LA SUBRAMA IZQ
          if (associated(current%Left)) then
            linkNodes = linkNodes // '"' // trim(address) // '"' // " -> "
            address = get_address_memory(current%Left)
            linkNodes = linkNodes // '"' // trim(address) // '" ' &
                      // '[label = "L"];' // new_line('a')
    
          end if
          ! VIAJAMOS A LA SUBRAMA DER
          if (associated(current%Right)) then
            address = get_address_memory(current)
            linkNodes = linkNodes // '"' // trim(address) // '"' // " -> "
            address = get_address_memory(current%Right)
            linkNodes = linkNodes // '"' // trim(address) // '" ' &
                      // '[label = "R"];' // new_line('a')
          end if
    
          call RoamTree(current%Left, createNodes, linkNodes)
          call RoamTree(current%Right, createNodes, linkNodes)
        end if
    end subroutine RoamTree
    subroutine write_dot(filename, code)
        character(len=*), intent(in) :: code, filename
        character(len=:), allocatable :: dot_filename, png_filename
        
        ! Agregar extensiones
        dot_filename = trim(filename) // ".dot"
        png_filename = trim(filename) // ".png"
        
        open(10, file=dot_filename, status='replace', action='write')
        write(10, '(A)') trim(code)
        close(10)

        ! Genera la imagen PNG
        call system("dot -Tpng "// dot_filename //" -o " // png_filename)
        call execute_command_line('start '// png_filename) 
    end subroutine write_dot

    function get_address_memory(node) result(address)
        !class(matrix_t), intent(in) :: self
        type(Node_t), pointer :: node
        character(len=20) :: address
        ! integer 8
        integer*8 :: i
    
        i = loc(node) ! get the address of x
        ! convert the address to string
        write(address, 10) i 
        10 format(I0)
    
    end function get_address_memory

    function search(this, idcapa) result(res)
        class(abb), intent(in) :: this
        integer, intent(in) :: idcapa
        type(capas) :: value
        type(capas), pointer :: res
        call value%setId(idcapa)
        res => this%search_recabb(value, this%root)
    end function search

    recursive function search_recabb(this, value, tmp) result(res)
        class(abb), intent(in) :: this
        type(capas), intent(in) :: value
        type(capas), pointer :: res
        class(Node_t), intent(in), pointer :: tmp
        if (.not. associated(tmp)) then
            write (*, '(A)') 'No se encontro capa0'
            res => null()
            return
        end if
        if (value%id < tmp%value%id) then
            res => this%search_recabb(value, tmp%left)
        else if (value%id > tmp%value%id) then
            res => this%search_recabb(value, tmp%right)
        else
            res => tmp%value
        end if
    end function search_recabb

    subroutine print_leaf_layers(this)
        class(abb), intent(in) :: this
        type(Node_t), pointer :: current_node
    
        print*, "Capas hojas:"
        current_node => this%root
        call print_leaf_layers_rec(current_node)
    end subroutine print_leaf_layers
    
    recursive subroutine print_leaf_layers_rec(current_node)
        type(Node_t), pointer :: current_node
    
        if (associated(current_node)) then
            call print_leaf_layers_rec(current_node%left)
            if (.not. associated(current_node%left) .and. .not. associated(current_node%right)) then
                write(*, '(A, I0)') "Capa hoja: ", current_node%value%id
            end if
            call print_leaf_layers_rec(current_node%right)
        end if
    end subroutine print_leaf_layers_rec

    function depthf(this) result(depth)
        class(abb), intent(in) :: this
        integer :: depth
        depth = depth_rec(this%root)
    end function depthf
    
    recursive function depth_rec(current_node) result(depth)
        type(Node_t), pointer :: current_node
        integer :: depth
        integer :: left_depth, right_depth
    
        if (.not. associated(current_node)) then
            depth = 0
        else
            left_depth = depth_rec(current_node%left)
            right_depth = depth_rec(current_node%right)
            depth = max(left_depth, right_depth) + 1
        end if
    end function depth_rec
    

end module arbolbb