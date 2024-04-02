module listadoAlbums
    use linkedlistm, nodeIMG => node
    implicit none
    
    type :: album
        character(len=100) :: nombre
        type(linkedlist) :: imagenes
        contains
        procedure :: agregarImagen
        procedure :: eliminarImagen
        procedure :: ImprimirAlbum
        procedure :: SetName
    end type album


    type :: nodoA
        type(album) :: value
        type(nodoA), pointer :: next => null()
        type(nodoA), pointer :: prev => null()
    end type nodoA

    type :: listaAlbum !trabajamos la lista y sus funciones como punteros
        type(nodoA), pointer :: head => null()
        type(nodoA), pointer :: tail => null()
        integer :: size = 0
        contains
        procedure :: agregarA => agregaLA
        procedure :: eliminarA => eliminarLA
        procedure :: graficarA => graficarLA
        procedure :: getAlbum => getLAlbum
        procedure :: imprimeA => ImprimeLA
        procedure :: eliminarImagenAlbum
        procedure :: contarImagenes
    end type listaAlbum

    contains
    function getLAlbum(this, name) result(albumt)
        class(listaAlbum), intent(in) :: this
        character(len=*), intent(in) :: name
        type(nodoA), pointer :: current
        type(album), pointer ::  albumt
        current => this%head
        do while (associated(current))
            if (current%value%nombre == name) then
                albumt => current%value
                return
            end if
            current => current%next
        end do
    end function getLAlbum

    subroutine ImprimeLA(this)
        class(listaAlbum), intent(in) :: this
        type(nodoA), pointer :: current
        current => this%head
        do while (associated(current))
            call current%value%ImprimirAlbum()
            print *, ''
            current => current%next
        end do
    end subroutine ImprimeLA

    subroutine agregaLA(this, valor)
        class(listaAlbum), intent(inout) :: this
        type(album), intent(in) :: valor
        type(nodoA), pointer :: new_node
        type(nodoA), pointer :: current

        allocate(new_node)
        new_node%value = valor

        if (.not. associated(this%head)) then
            this%head => new_node
            this%tail => new_node
        else
            current => this%tail
            current%next => new_node
            new_node%prev => current
            this%tail => new_node
        end if
        this%size = this%size + 1
    end subroutine agregaLA

    
    subroutine eliminarLA(this, valor)
        class(listaAlbum), intent(inout) :: this
        type(album), intent(in) :: valor
        type(nodoA), pointer :: current
        type(nodoA), pointer :: previous
        type(nodoA), pointer :: next

        current => this%head
        do while (associated(current))
            if (current%value%nombre == valor%nombre) then
                if (associated(current%prev)) then
                    previous => current%prev
                    previous%next => current%next
                else
                    this%head => current%next
                end if
                if (associated(current%next)) then
                    next => current%next
                    next%prev => current%prev
                else
                    this%tail => current%prev
                end if
                deallocate(current)
                this%size = this%size - 1
                return
            end if
            current => current%next
        end do
    end subroutine eliminarLA

    
    subroutine graficarLA(this)
        class(listaAlbum), intent(in) :: this
        type(nodoA), pointer :: current
        type(nodoA), pointer :: next
        type(nodoA), pointer :: previous
        type(nodeIMG), pointer :: temp
        integer :: i = 0
        current => this%head
        open(1, file='album.dot', status='replace')
        write(1, '(a)') 'digraph G {'
        write(1, '(a)') 'node [shape=box, style="rounded,filled", fillcolor=lightblue, color=blue];'
        do while (associated(current))
            if (associated(current%next)) then
                next => current%next
                write(1, '(A,A,A,A,A)') '"',trim(current%value%nombre), '"->"', trim(next%value%nombre), '" [color=red]'
                write(1, '(A,A,A,A,A)') '{rank=same; "', trim(current%value%nombre),'"; "', trim(next%value%nombre),'";}'
            end if
            temp => current%value%imagenes%head
                if (associated(temp)) then
                    write(1, '(A,A,A,I0,I0)') '"' ,trim(current%value%nombre), '"->', temp%data ,i
                end if
                do while ( associated(temp) )
                    write(1, '(A,I0,I0,A,I0,A)') '"',temp%data, i ,'" [label="', temp%data, '"]'
                    if ( associated(temp%next) ) then
                        write(1, '(I0,I0,A,I0,I0)') temp%data, i, '->', temp%next%data, i
                    end if
                    temp => temp%next
                end do
            if (associated(current%prev)) then
                previous => current%prev
                write(1, '(A,A,A,A,A)') '"',trim(current%value%nombre), '"->"', trim(previous%value%nombre), '"[color=blue]'
            end if
            i = i + 1
            current => current%next
        end do
        write(1, '(a)') '}'
        close(1)
        call execute_command_line('dot -Tpng album.dot -o album.png')
        call execute_command_line('start album.png')
    end subroutine graficarLA

    
    subroutine eliminarImagenAlbum(this, id)
        class(listaAlbum), intent(inout) :: this
        type(nodoA), pointer :: current
        type(nodeIMG), pointer :: temp
        integer :: id
        current => this%head
        do while (associated(current))
            temp => current%value%imagenes%head
            do while (associated(temp))
                if (temp%data == id) then
                    call current%value%imagenes%removelist(temp%data)
                end if
                temp => temp%next
            end do
            current => current%next
        end do
    end subroutine eliminarImagenAlbum



    subroutine eliminarImagen(this, img)
            class(album) :: this
            integer, intent(in) :: img
            call this%imagenes%removelist(img)
    end subroutine eliminarImagen

    subroutine SetName(this, name)
            class(album) :: this
            character(len=*), intent(in) :: name
            this%nombre = name
    end subroutine SetName

    subroutine agregarImagen(this, img)
            class(album) :: this
            integer, intent(in) :: img
            call this%imagenes%addlist(img)
    end subroutine AgregarImagen

    subroutine ImprimirAlbum(this)
            class(album) :: this
            write(*,'(A,A)')  'Album: ', this%nombre
            write(*,'(A)')  'Imagenes: '
            call this%imagenes%print()
    end subroutine ImprimirAlbum

    function contarImagenes(this) result(num_imagenes)
        class(listaAlbum), intent(in) :: this
        type(nodoA), pointer :: current
        type(album), pointer :: current_album
        type(nodeIMG), pointer :: current_image

        type(linkedlist):: unique_ids ! Lista enlazada auxiliar para almacenar los identificadores únicos
        logical :: is_unique
        integer :: num_imagenes
    
        num_imagenes = unique_ids%size 
        current => this%head
        do while (associated(current))
            current_album => current%value
            current_image => current_album%imagenes%head
            do while (associated(current_image))
                is_unique = .true.
                is_unique = unique_ids%searchlist(current_image%data)
                if (.not. is_unique) then
                    call unique_ids%addlist(current_image%data)
                    num_imagenes = num_imagenes + 1
                end if
                current_image => current_image%next
            end do
            current => current%next
        end do
    
    end function contarImagenes
    
end module listadoAlbums