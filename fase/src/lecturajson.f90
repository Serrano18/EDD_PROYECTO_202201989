module lecturajson
    use json_module
    use usuario, only: User
    use arbolb, only:BTreeNode
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

end module lecturajson