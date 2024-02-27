module Proyecto
  use json_module
  use cola_clientes
  implicit none

  type(json_file) :: json   ! Se declara una variable del tipo json_file
  type(json_value), pointer :: listPointer, personPointer, attributePointer  ! Se declaran punteros a variables del tipo json_value
  type(json_core) :: jsonc  ! Se declara una variable del tipo json_core para acceder a las funciones básicas de JSON
  character(:), allocatable :: nombre  ! Se declara una cadena de caracteres que se asignará dinámicamente
  type (cola) :: cola_c

  integer :: i, size        ! Se declaran variables enteras
  logical :: found
  contains 
      subroutine load_json(narchivo)
        character(len=256), intent(in):: narchivo
          call json%initialize()
          call json%load(filename=narchivo)
          call json%info('',n_children=size)
          call json%get_core(jsonc)
          call json%get('',listPointer,found)
          call json%print()
      end subroutine load_json

      subroutine guardar_json(narchivo)
        type(json_file) :: json   ! Se declara una variable del tipo json_file
        type(json_value), pointer :: listPointer, personPointer, attributePointer  ! Se declaran punteros a variables del tipo json_value
        type(json_core) :: jsonc  ! Se declara una variable del tipo json_core para acceder a las funciones básicas de JSON
        !character(len=100):: nombre  
        character(:), allocatable :: nombre  
        integer :: i, size,u_id ,img_1, img_2
        logical :: found
        character(len=256), intent(in):: narchivo
        call json%initialize()
        call json%load(filename=narchivo)
        call json%info('',n_children=size)
        call json%get_core(jsonc)
        call json%get('',listPointer,found)
        do i = 1, size
          call jsonc%get_child(listPointer,i,personPointer,found=found)
          call jsonc%get_child(personPointer,'id',attributePointer,found=found)
          if (found) then
            call jsonc%get(attributePointer,u_id)
            !print *, u_id
          end if
          call jsonc%get_child(personPointer,'nombre',attributePointer,found=found)
          if (found) then
            call jsonc%get(attributePointer,nombre)
            !print *, trim(nombre)
          end if
          call jsonc%get_child(personPointer,'img_g',attributePointer,found=found)
          if (found) then
            call jsonc%get(attributePointer,img_1)
            !print *, img_1
          end if
          call jsonc%get_child(personPointer,'img_p',attributePointer,found=found)
          if (found) then
            call jsonc%get(attributePointer,img_2)
            !print *, img_2
          end if 
          call cola_c%append(nombre, u_id, img_1, img_2)
        end do
        call cola_c%print()
        call json%destroy()
      end subroutine guardar_json
end module Proyecto
