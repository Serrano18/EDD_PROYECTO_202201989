module cargasmasivas
    use json_module
    use avlsucursales
    use tecnicoshash
    use rutas
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

    subroutine cargaMasivaSucursales(file_path, tree)
        character(len=*), intent(in) :: file_path
        type(sucursalesavl), intent(inout) :: tree
        type(sucursal) :: temp_sucursal,new_sucursal
        integer :: num
        call load_json(file_path)
    
        do i = 1, sizej
            temp_sucursal = new_sucursal
            call temp_sucursal%inicializar_sucursal() 
            call jsonc%get_child(listP, i, tempP, found=found)
            if (found) then
                call jsonc%get_child(tempP, 'id', caracP, found=found)
                if (found) then
                    call jsonc%get(caracP, tempStr)
                    num = convert_to_integer2(tempStr)
                    call temp_sucursal%set_id(num)
                end if
                call jsonc%get_child(tempP, 'departamento', caracP, found=found)
                if (found) then
                    call jsonc%get(caracP, tempStr)
                    call temp_sucursal%set_departamento(tempStr)
                end if
    
                call jsonc%get_child(tempP, 'direccion', caracP, found=found)
                if (found) then
                    call jsonc%get(caracP, tempStr)
                    call temp_sucursal%set_direccion(tempStr)
                end if
    
                call jsonc%get_child(tempP, 'password', caracP, found=found)
                if (found) then
                    call jsonc%get(caracP, tempStr)
                    call temp_sucursal%set_contrasena(tempStr)
                end if
            end if
    
            call tree%add(temp_sucursal)
        end do
    
        call destroy_json()
    end subroutine cargaMasivaSucursales

    subroutine cargaMasivaTecnicos(file_path, hash_table)
        character(len=*), intent(in) :: file_path
        type(tecnicos_hash), intent(inout) :: hash_table
        type(tecnicos) :: temp_tecnico, new_tecnico
        integer :: num
        call load_json(file_path)
    
        do i = 1, sizej
            temp_tecnico = new_tecnico
            call jsonc%get_child(listP, i, tempP, found=found)
            if (found) then
                call jsonc%get_child(tempP, 'dpi', caracP, found=found)
                if ( found ) then
                    call jsonc%get(caracP, tempStr)
                    call temp_tecnico%set_dpi( convert_to_integer(tempStr))
                end if
    
                call jsonc%get_child(tempP, 'nombre', caracP, found=found)
                if (found) then
                    call jsonc%get(caracP, tempStr)
                    call temp_tecnico%set_nombre(tempStr)
                end if
    
                call jsonc%get_child(tempP, 'apellido', caracP, found=found)
                if (found) then
                    call jsonc%get(caracP, tempStr)
                    call temp_tecnico%set_apellido(tempStr)
                end if
    
                call jsonc%get_child(tempP, 'genero', caracP, found=found)
                if (found) then
                    call jsonc%get(caracP, tempStr)
                    call temp_tecnico%set_genero(tempStr)
                end if
    
                call jsonc%get_child(tempP, 'direccion', caracP, found=found)
                if (found) then
                    call jsonc%get(caracP, tempStr)
                    call temp_tecnico%set_direc(tempStr)
                end if
                call jsonc%get_child(tempP, 'telefono', caracP, found=found)
                if (found) then
                    call jsonc%get(caracP, tempStr)
                    num = convert_to_integer2(tempStr)
                    call temp_tecnico%set_telefono(num)
                end if
    
                call hash_table%insert(temp_tecnico)
            end if
        end do
    
        call destroy_json()
    end subroutine cargaMasivaTecnicos

    subroutine cargaMasivaRutas(file_path, rutas)
        character(len=*), intent(in) :: file_path
        type(lista_rutas), intent(inout) :: rutas
        type(ruta) :: temp_ruta, new_ruta
        integer :: num
        call load_json(file_path)
        do i = 1, sizej
            temp_ruta = new_ruta
            call jsonc%get_child(listP, i, tempP, found=found)
            if (found) then
                call jsonc%get_child(tempP, 's1', caracP, found=found)
                if ( found ) then
                    call jsonc%get(caracP, tempStr)
                    num = convert_to_integer2(tempStr)
                    call temp_ruta%set_sucursal1(num)
                end if
    
                call jsonc%get_child(tempP, 's2', caracP, found=found)
                if (found) then
                    call jsonc%get(caracP, tempStr)
                    num = convert_to_integer2(tempStr)
                    call temp_ruta%set_sucursal2(num)
                end if
    
                call jsonc%get_child(tempP, 'distancia', caracP, found=found)
                if (found) then
                    call jsonc%get(caracP, tempStr)
                    num = convert_to_integer2(tempStr)
                    call temp_ruta%set_distancia(num)
                end if
    
                call jsonc%get_child(tempP, 'imp_mantenimiento', caracP, found=found)
                if (found) then
                    call jsonc%get(caracP, tempStr)
                    num = convert_to_integer2(tempStr)
                    call temp_ruta%set_impresoras(num)
                end if
    
                call rutas%addr(temp_ruta,.true.)
            end if
        end do
        call destroy_json()
    end subroutine cargaMasivaRutas

    function convert_to_integer2(str) result(num)
        character(len=*), intent(in) :: str
        integer :: num
        read(str, *) num
    end function convert_to_integer2

    function convert_to_integer(str) result(num)
        character(len=*), intent(in) :: str
        integer(kind=8) :: num
        read(str, *) num
    end function convert_to_integer

    subroutine destroy_json()
        call json%destroy()
    end subroutine destroy_json


end module cargasmasivas