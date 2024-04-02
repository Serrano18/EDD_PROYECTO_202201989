module lecturajson
    use json_module
    use usuario
    use arbolb
    use arbolbb
    use listadoPixeles
    use arbolavl
    use listadoAlbums
    use json_module, ik => json_ik
    implicit none
        type(json_file) :: json
        type(json_core) :: jsonc
        type(json_value), pointer :: listP, tempP, caracP, sublistP
        logical :: found
        integer :: i, j, sizej, sizek
        character(:), allocatable :: tempStr
    
    contains
        subroutine load_json(file_path)
            character(len=*), intent(in) :: file_path
    
            call json%initialize()
            call json%load(filename=file_path)
            call json%info('', n_children=sizej)
            call json%get_core(jsonc)
            call json%get('', listP, found)
            call json%print()
        end subroutine load_json
    
        subroutine cargaMasivaUsuarios(file_path,arbol) 
            character(len=*), intent(in) :: file_path
            class(BTreeNode), intent(inout) :: arbol
            type(User) :: temp_users, new_users
            integer(kind=8) :: cui 
            call load_json(file_path)
            do i=1, sizej
                temp_users = new_users
                call jsonc%get_child(listP, i, tempP, found=found)
    
                call jsonc%get_child(tempP, 'dpi', caracP, found=found)
                if ( found ) then
                    call jsonc%get(caracP, tempStr)
                    cui = convert_to_integer(trim(tempStr))
                    call temp_users%setDpi(cui)
                end if
    
                call jsonc%get_child(tempP, 'nombre_cliente', caracP, found=found)
                if ( found ) then
                    call jsonc%get(caracP, tempStr)
                    call temp_users%setNombre(trim(tempStr))
                end if
    
                call jsonc%get_child(tempP, 'password', caracP, found=found)
                if ( found ) then
                    call jsonc%get(caracP, tempStr)
                    call temp_users%setPassword(trim(tempStr))
                end if
                call temp_users%imprimirDatos()
                call arbol%insert(temp_users)
            end do
    
            call destroy_json()
        end subroutine cargaMasivaUsuarios
        
        function convert_to_integer(str) result(num)
            character(len=*), intent(in) :: str
            integer(kind=8) :: num
            read(str, *) num
        end function convert_to_integer
    
        subroutine destroy_json()
            call json%destroy()
        end subroutine destroy_json

        subroutine cargaMasivaCapas(file_path, tree)
            character(len=*), intent(in) :: file_path
            type(abb),intent (inout) :: tree
            type(pixel) :: temp_pixel,new_pixel
            type(capas) :: temp_capa,new_Capa
            integer :: num
            call load_json(file_path)
            
            do i = 1, sizej
                temp_capa = new_capa
                call jsonc%get_child(listP, i, tempP, found=found)
                if ( found ) then

                    call jsonc%get_child(tempP, 'id_capa', caracP, found=found)
                    if ( found ) then
                        call jsonc%get(caracP, tempStr)
                        num = convert_to_integer2(tempStr)
                        call temp_capa%setId(num)
                    end if

                    call jsonc%get_child(tempP, 'pixeles', sublistP, found=found)
                    if ( found ) then
                        call jsonc%info(sublistP, n_children=sizek)
                        do j = 1, sizek
                            temp_pixel = new_pixel
                            call jsonc%get_child(sublistP, j, tempP, found=found)
                            if ( found ) then
                                call jsonc%get_child(tempP, 'fila', caracP, found=found)
                                if ( found ) then
                                    call jsonc%get(caracP, tempStr)
                                    num = convert_to_integer2(tempStr)
                                    call temp_pixel%setFila(num)
                                end if

                                call jsonc%get_child(tempP, 'columna', caracP, found=found)
                                if ( found ) then
                                    call jsonc%get(caracP, tempStr)
                                    num = convert_to_integer2(tempStr)
                                    call temp_pixel%setColumna(num)
                                end if

                                call jsonc%get_child(tempP, 'color', caracP, found=found)
                                if ( found ) then
                                    call jsonc%get(caracP, tempStr)
                                    call temp_pixel%setColor(tempStr)
                                end if
                            end if
                            call temp_capa%agregarPixel(temp_pixel)
                        end do
                    end if
                end if
                call tree%insertabb(temp_capa)
            end do
        end subroutine cargaMasivaCapas
        
        subroutine cargaMasivaImagenes(file_path,tree)
            character(len=*), intent(in) :: file_path
            integer(kind=ik), allocatable :: tempInt(:)
            type(avl), intent(inout):: tree
            type(Imagen) :: temp_image,new_image
            integer :: num
        
            call load_json(file_path)
        
            do i = 1, sizej
                temp_image = new_image  ! Suponiendo que `image` es tu tipo de datos Imagen
                call jsonc%get_child(listP, i, tempP, found=found)
                if (found) then
                    call jsonc%get_child(tempP, 'id', caracP, found=found)
                    if (found) then
                        call jsonc%get(caracP, tempStr)
                        num = convert_to_integer2(tempStr)
                        call temp_image%setIdImg(num)
                    end if
        
                    call jsonc%get_child(tempP, 'capas', caracP, found=found)
                    if (found) then
                        call jsonc%get(caracP, tempInt)
                        do j = 1, size(tempInt)
                            call temp_image%agregarNodoIdCapas(tempInt(j))
                            call temp_image%agregarcapa(tempInt(j))
                        end do
                    end if
                end if
        
                call tree%insertavl(temp_image)
            end do
        
            call destroy_json()
        end subroutine cargaMasivaImagenes

        
    subroutine cargaMasivaAlbums(file_path,listadoAlbum) 
        character(len=*), intent(in) :: file_path
        integer(kind=ik), allocatable :: tempInt(:)
        type(album) :: temp_album, new_album
        type(listaAlbum), intent(inout) :: listadoAlbum
        integer :: num

        call load_json(file_path)
        do i = 1, sizej
            temp_album = new_album
            call jsonc%get_child(listP, i, tempP, found=found)
            if ( found ) then

                call jsonc%get_child(tempP, 'nombre_album', caracP, found=found)
                if ( found ) then
                    call jsonc%get(caracP, tempStr)
                    call temp_album%SetName(tempStr)
                end if

                call jsonc%get_child(tempP, 'imgs', caracP, found=found)
                if ( found ) then
                    call jsonc%get(caracP, tempInt)
                    do num = 1, size(tempInt)
                        call temp_album%agregarImagen(tempInt(num))
                    end do
                end if
            end if
            call listadoAlbum%agregarA(temp_album)
        end do
    end subroutine cargaMasivaAlbums

        function convert_to_integer2(str) result(num)
            character(len=*), intent(in) :: str
            integer :: num
            read(str, *) num
        end function convert_to_integer2

end module lecturajson